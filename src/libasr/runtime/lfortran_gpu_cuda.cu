#include <cuda_runtime.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// We implement the same C API as the Metal runtime
// (lfortran_gpu_runtime.h), but using CUDA.

extern "C" {

#include "lfortran_gpu_runtime.h"

} // extern "C" for the header

#define MAX_ARGS 32
#define MAX_KERNELS 64

typedef void (*kernel_func_t)(void);

// Registry for pre-compiled CUDA kernels
static struct {
    const char *name;
    kernel_func_t func;
} kernel_registry[MAX_KERNELS];
static int n_registered = 0;

extern "C" void lfortran_gpu_register_kernel(const char *name, kernel_func_t func) {
    if (n_registered >= MAX_KERNELS) {
        fprintf(stderr, "lfortran_gpu_register_kernel: too many kernels\n");
        exit(1);
    }
    kernel_registry[n_registered].name = name;
    kernel_registry[n_registered].func = func;
    n_registered++;
}

typedef struct scalar_arg {
    void *data;
    size_t size;
} scalar_arg;

struct lfortran_gpu_ctx {
    int device_id;
};

struct lfortran_gpu_kernel {
    kernel_func_t func;
    lfortran_gpu_ctx *ctx;
    // Argument storage
    void *buffer_host_ptrs[MAX_ARGS];
    void *buffer_device_ptrs[MAX_ARGS];
    size_t buffer_sizes[MAX_ARGS];
    scalar_arg scalar_args[MAX_ARGS];
    int arg_is_buffer[MAX_ARGS]; // 1 = buffer, 0 = scalar
    int n_args;
};

extern "C" {

lfortran_gpu_ctx* lfortran_gpu_init(void) {
    static lfortran_gpu_ctx *singleton = NULL;
    if (singleton) return singleton;
    singleton = (lfortran_gpu_ctx*)calloc(1, sizeof(lfortran_gpu_ctx));
    singleton->device_id = 0;
    cudaError_t err = cudaSetDevice(0);
    if (err != cudaSuccess) {
        fprintf(stderr, "lfortran_gpu_init: cudaSetDevice failed: %s\n",
            cudaGetErrorString(err));
        free(singleton);
        singleton = NULL;
        return NULL;
    }
    return singleton;
}

void lfortran_gpu_shutdown(lfortran_gpu_ctx* ctx) {
    if (!ctx) return;
    cudaDeviceReset();
    free(ctx);
}

lfortran_gpu_kernel* lfortran_gpu_load_kernel(
    lfortran_gpu_ctx* ctx, const char* source, const char* entry_point)
{
    (void)source; // CUDA kernels are pre-compiled, source is unused
    if (!ctx || !entry_point) return NULL;

    // Look up the kernel in the registry
    kernel_func_t func = NULL;
    for (int i = 0; i < n_registered; i++) {
        if (strcmp(kernel_registry[i].name, entry_point) == 0) {
            func = kernel_registry[i].func;
            break;
        }
    }
    if (!func) {
        fprintf(stderr, "lfortran_gpu_load_kernel: kernel '%s' not found in registry\n",
            entry_point);
        exit(1);
    }

    lfortran_gpu_kernel *k = (lfortran_gpu_kernel*)calloc(1, sizeof(lfortran_gpu_kernel));
    k->func = func;
    k->ctx = ctx;
    k->n_args = 0;
    return k;
}

void lfortran_gpu_release_kernel(lfortran_gpu_kernel* k) {
    if (!k) return;
    // Free device buffers
    for (int i = 0; i < k->n_args; i++) {
        if (k->arg_is_buffer[i] && k->buffer_device_ptrs[i]) {
            cudaFree(k->buffer_device_ptrs[i]);
        }
        if (!k->arg_is_buffer[i] && k->scalar_args[i].data) {
            free(k->scalar_args[i].data);
        }
    }
    free(k);
}

void lfortran_gpu_set_buffer_arg(lfortran_gpu_kernel* k, int idx,
    void* ptr, size_t size)
{
    if (!k || idx >= MAX_ARGS) return;
    k->buffer_host_ptrs[idx] = ptr;
    k->buffer_sizes[idx] = size;
    k->arg_is_buffer[idx] = 1;
    if (idx >= k->n_args) k->n_args = idx + 1;

    // Allocate device memory and copy host → device
    void *d_ptr = NULL;
    cudaError_t err = cudaMalloc(&d_ptr, size);
    if (err != cudaSuccess) {
        fprintf(stderr, "lfortran_gpu_set_buffer_arg: cudaMalloc failed: %s\n",
            cudaGetErrorString(err));
        exit(1);
    }
    err = cudaMemcpy(d_ptr, ptr, size, cudaMemcpyHostToDevice);
    if (err != cudaSuccess) {
        fprintf(stderr, "lfortran_gpu_set_buffer_arg: cudaMemcpy H2D failed: %s\n",
            cudaGetErrorString(err));
        exit(1);
    }
    k->buffer_device_ptrs[idx] = d_ptr;
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

    // Build the kernel argument array for cudaLaunchKernel.
    // Arguments are in order: buffer args first (device pointers),
    // then scalar args (values).
    void *args[MAX_ARGS];
    int arg_idx = 0;
    for (int i = 0; i < k->n_args; i++) {
        if (k->arg_is_buffer[i]) {
            args[arg_idx++] = &k->buffer_device_ptrs[i];
        } else {
            args[arg_idx++] = k->scalar_args[i].data;
        }
    }

    dim3 gridDim(grid[0], grid[1], grid[2]);
    dim3 blockDim(block[0], block[1], block[2]);

    cudaError_t err = cudaLaunchKernel(
        (const void*)k->func, gridDim, blockDim,
        args, 0, 0);
    if (err != cudaSuccess) {
        fprintf(stderr, "lfortran_gpu_launch: cudaLaunchKernel failed: %s\n",
            cudaGetErrorString(err));
        exit(1);
    }

    // Synchronize and copy results back
    err = cudaDeviceSynchronize();
    if (err != cudaSuccess) {
        fprintf(stderr, "lfortran_gpu_launch: cudaDeviceSynchronize failed: %s\n",
            cudaGetErrorString(err));
        exit(1);
    }

    // Copy device → host for all buffer args
    for (int i = 0; i < k->n_args; i++) {
        if (k->arg_is_buffer[i] && k->buffer_device_ptrs[i]) {
            err = cudaMemcpy(k->buffer_host_ptrs[i],
                k->buffer_device_ptrs[i],
                k->buffer_sizes[i],
                cudaMemcpyDeviceToHost);
            if (err != cudaSuccess) {
                fprintf(stderr, "lfortran_gpu_launch: cudaMemcpy D2H failed: %s\n",
                    cudaGetErrorString(err));
                exit(1);
            }
        }
    }
}

void lfortran_gpu_sync(lfortran_gpu_ctx* ctx) {
    (void)ctx;
    cudaDeviceSynchronize();
}

} // extern "C"
