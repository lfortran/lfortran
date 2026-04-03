#import <Metal/Metal.h>
#import <Foundation/Foundation.h>
#include "lfortran_gpu_runtime.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_ARGS 32

typedef struct scalar_arg {
    void *data;
    size_t size;
} scalar_arg;

struct lfortran_gpu_ctx {
    id<MTLDevice> device;
    id<MTLCommandQueue> queue;
    id<MTLCommandBuffer> last_command_buffer;
};

struct lfortran_gpu_kernel {
    id<MTLComputePipelineState> pipeline;
    lfortran_gpu_ctx *ctx;
    // Argument storage
    void *buffer_ptrs[MAX_ARGS];
    size_t buffer_sizes[MAX_ARGS];
    scalar_arg scalar_args[MAX_ARGS];
    int arg_is_buffer[MAX_ARGS]; // 1 = buffer, 0 = scalar
    int n_args;
};

lfortran_gpu_ctx* lfortran_gpu_init(void) {
    static lfortran_gpu_ctx *singleton = NULL;
    if (singleton) return singleton;
    singleton = (lfortran_gpu_ctx*)calloc(1, sizeof(lfortran_gpu_ctx));
    singleton->device = MTLCreateSystemDefaultDevice();
    if (!singleton->device) {
        fprintf(stderr, "lfortran_gpu_init: Metal is not supported on this device\n");
        free(singleton);
        singleton = NULL;
        return NULL;
    }
    singleton->queue = [singleton->device newCommandQueue];
    singleton->last_command_buffer = nil;
    return singleton;
}

void lfortran_gpu_shutdown(lfortran_gpu_ctx* ctx) {
    if (!ctx) return;
    ctx->device = nil;
    ctx->queue = nil;
    ctx->last_command_buffer = nil;
    free(ctx);
}

lfortran_gpu_kernel* lfortran_gpu_load_kernel(
    lfortran_gpu_ctx* ctx, const char* source, const char* entry_point)
{
    if (!ctx || !source || !entry_point) return NULL;

    NSError *error = nil;
    NSString *src = [NSString stringWithUTF8String:source];

    id<MTLLibrary> library = [ctx->device newLibraryWithSource:src
                                                       options:nil
                                                         error:&error];
    if (!library) {
        fprintf(stderr, "lfortran_gpu_load_kernel: Failed to compile Metal shader:\n%s\n",
                [[error localizedDescription] UTF8String]);
        exit(1);
    }

    NSString *name = [NSString stringWithUTF8String:entry_point];
    id<MTLFunction> function = [library newFunctionWithName:name];
    if (!function) {
        fprintf(stderr, "lfortran_gpu_load_kernel: Function '%s' not found in shader\n",
                entry_point);
        exit(1);
    }

    id<MTLComputePipelineState> pipeline =
        [ctx->device newComputePipelineStateWithFunction:function error:&error];
    if (!pipeline) {
        fprintf(stderr, "lfortran_gpu_load_kernel: Failed to create pipeline:\n%s\n",
                [[error localizedDescription] UTF8String]);
        exit(1);
    }

    lfortran_gpu_kernel *k = (lfortran_gpu_kernel*)calloc(1, sizeof(lfortran_gpu_kernel));
    k->pipeline = pipeline;
    k->ctx = ctx;
    k->n_args = 0;
    return k;
}

void lfortran_gpu_release_kernel(lfortran_gpu_kernel* k) {
    if (!k) return;
    for (int i = 0; i < k->n_args; i++) {
        if (!k->arg_is_buffer[i] && k->scalar_args[i].data) {
            free(k->scalar_args[i].data);
        }
    }
    k->pipeline = nil;
    free(k);
}

void lfortran_gpu_set_buffer_arg(lfortran_gpu_kernel* k, int idx,
    void* ptr, size_t size)
{
    if (!k || idx >= MAX_ARGS) return;
    k->buffer_ptrs[idx] = ptr;
    k->buffer_sizes[idx] = size;
    k->arg_is_buffer[idx] = 1;
    if (idx >= k->n_args) k->n_args = idx + 1;
}

void lfortran_gpu_set_scalar_arg(lfortran_gpu_kernel* k, int idx,
    const void* val, size_t size)
{
    if (!k || idx >= MAX_ARGS) return;
    if (k->scalar_args[idx].data) {
        free(k->scalar_args[idx].data);
    }
    k->scalar_args[idx].data = malloc(size);
    memcpy(k->scalar_args[idx].data, val, size);
    k->scalar_args[idx].size = size;
    k->arg_is_buffer[idx] = 0;
    if (idx >= k->n_args) k->n_args = idx + 1;
}

void lfortran_gpu_launch(lfortran_gpu_ctx* ctx, lfortran_gpu_kernel* k,
    int grid[3], int block[3])
{
    if (!ctx || !k) return;

    id<MTLCommandBuffer> commandBuffer = [ctx->queue commandBuffer];

    id<MTLComputeCommandEncoder> encoder = [commandBuffer computeCommandEncoder];
    [encoder setComputePipelineState:k->pipeline];

    // Set arguments - track MTLBuffers for copy-back
    id<MTLBuffer> buffers[MAX_ARGS];
    memset(buffers, 0, sizeof(buffers));

    for (int i = 0; i < k->n_args; i++) {
        if (k->arg_is_buffer[i]) {
            // Copy host data into a Metal buffer
            id<MTLBuffer> buffer = [ctx->device
                newBufferWithBytes:k->buffer_ptrs[i]
                            length:k->buffer_sizes[i]
                           options:MTLResourceStorageModeShared];
            buffers[i] = buffer;
            [encoder setBuffer:buffer offset:0 atIndex:i];
        } else {
            // Set scalar bytes directly
            [encoder setBytes:k->scalar_args[i].data
                       length:k->scalar_args[i].size
                      atIndex:i];
        }
    }

    // Dispatch threads
    NSUInteger total_threads = (NSUInteger)(grid[0] * block[0]);
    NSUInteger threads_per_group = (NSUInteger)block[0];

    // Clamp threads_per_group to pipeline maximum
    NSUInteger max_threads = [k->pipeline maxTotalThreadsPerThreadgroup];
    if (threads_per_group > max_threads) {
        threads_per_group = max_threads;
    }

    MTLSize gridSize = MTLSizeMake(total_threads, 1, 1);
    MTLSize threadGroupSize = MTLSizeMake(threads_per_group, 1, 1);
    [encoder dispatchThreads:gridSize threadsPerThreadgroup:threadGroupSize];

    [encoder endEncoding];
    [commandBuffer commit];
    [commandBuffer waitUntilCompleted];

    if ([commandBuffer error]) {
        fprintf(stderr, "lfortran_gpu_launch: GPU execution error: %s\n",
                [[[commandBuffer error] localizedDescription] UTF8String]);
    }

    // Copy results back from Metal buffers to host memory
    for (int i = 0; i < k->n_args; i++) {
        if (k->arg_is_buffer[i] && buffers[i]) {
            memcpy(k->buffer_ptrs[i], [buffers[i] contents], k->buffer_sizes[i]);
        }
    }

    ctx->last_command_buffer = commandBuffer;
}

void lfortran_gpu_sync(lfortran_gpu_ctx* ctx) {
    if (!ctx || !ctx->last_command_buffer) return;
    [ctx->last_command_buffer waitUntilCompleted];

    if ([ctx->last_command_buffer error]) {
        fprintf(stderr, "lfortran_gpu_sync: GPU execution error: %s\n",
                [[[ctx->last_command_buffer error] localizedDescription] UTF8String]);
    }

    ctx->last_command_buffer = nil;
}
