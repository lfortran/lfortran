#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _OPENMP
#include <omp.h>
#endif

#include "cuda_cpu_device.h"
#include "lfortran_gpu_runtime.h"

// CPU implementation of the same C API as the Metal and CUDA GPU runtimes.
//
// The generated device code is compiled as ordinary host code, so there is no
// separate device address space: the host pointer *is* the device pointer and
// no copies are needed. Kernels are reached through the per-kernel thunks that
// the CUDA code generator emits in emulation mode, because there is no
// portable way to call a function pointer with a dynamically built argument
// list.

#define MAX_ARGS 32
#define MAX_KERNELS 64

// Built-in thread coordinates, written once per emulated thread.
__thread uint3 threadIdx = {0, 0, 0};
__thread uint3 blockIdx = {0, 0, 0};
__thread dim3 blockDim = {1, 1, 1};
__thread dim3 gridDim = {1, 1, 1};

typedef void (*lfortran_gpu_thunk_t)(void **);

static struct {
    const char *name;
    lfortran_gpu_thunk_t func;
} kernel_registry[MAX_KERNELS];
static int n_registered = 0;

void lfortran_gpu_register_kernel(const char *name, lfortran_gpu_thunk_t func) {
    if (n_registered >= MAX_KERNELS) {
        fprintf(stderr, "lfortran_gpu_register_kernel: too many kernels\n");
        exit(1);
    }
    kernel_registry[n_registered].name = name;
    kernel_registry[n_registered].func = func;
    n_registered++;
}

void lfortran_gpu_cpu_barrier_unsupported(void) {
    fprintf(stderr, "lfortran_gpu_cpu: __syncthreads() needs the cpu gpu "
        "runtime to be built with openmp, so that the threads of a block run "
        "concurrently\n");
    exit(1);
}

typedef struct scalar_arg {
    void *data;
    size_t size;
} scalar_arg;

struct lfortran_gpu_ctx {
    int device_id;
};

struct lfortran_gpu_kernel {
    lfortran_gpu_thunk_t func;
    lfortran_gpu_ctx *ctx;
    // Argument storage. Buffers are passed through unchanged, so the "device"
    // pointer is just the host pointer.
    void *buffer_device_ptrs[MAX_ARGS];
    size_t buffer_sizes[MAX_ARGS];
    scalar_arg scalar_args[MAX_ARGS];
    int arg_is_buffer[MAX_ARGS]; // 1 = buffer, 0 = scalar
    int n_args;
};

lfortran_gpu_ctx* lfortran_gpu_init(void) {
    static lfortran_gpu_ctx *singleton = NULL;
    if (singleton) return singleton;
    singleton = (lfortran_gpu_ctx*)calloc(1, sizeof(lfortran_gpu_ctx));
    singleton->device_id = 0;
    return singleton;
}

void lfortran_gpu_shutdown(lfortran_gpu_ctx* ctx) {
    if (!ctx) return;
    free(ctx);
}

lfortran_gpu_kernel* lfortran_gpu_load_kernel(
    lfortran_gpu_ctx* ctx, const char* entry_point)
{
    if (!ctx || !entry_point) return NULL;

    lfortran_gpu_thunk_t func = NULL;
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
    for (int i = 0; i < k->n_args; i++) {
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
    // Memory is unified, so there is nothing to allocate or copy.
    k->buffer_device_ptrs[idx] = ptr;
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

// Runs one emulated thread of a block: publishes its built-in coordinates
// and calls the kernel thunk.
static void run_thread(lfortran_gpu_kernel* k, void **args,
    dim3 grid_dim, dim3 block_dim, unsigned int bx, unsigned int by,
    unsigned int bz, long long t)
{
    blockIdx.x = bx;
    blockIdx.y = by;
    blockIdx.z = bz;
    threadIdx.x = (unsigned int)(t % block_dim.x);
    threadIdx.y = (unsigned int)((t / block_dim.x) % block_dim.y);
    threadIdx.z = (unsigned int)(t / ((long long)block_dim.x * block_dim.y));
    blockDim = block_dim;
    gridDim = grid_dim;
    k->func(args);
}

void lfortran_gpu_launch(lfortran_gpu_ctx* ctx, lfortran_gpu_kernel* k,
    int grid[3], int block[3])
{
    if (!ctx || !k) return;

    // Same argument array the CUDA runtime hands to cudaLaunchKernel: every
    // entry points at the argument value.
    void *args[MAX_ARGS];
    int arg_idx = 0;
    for (int i = 0; i < k->n_args; i++) {
        if (k->arg_is_buffer[i]) {
            args[arg_idx++] = &k->buffer_device_ptrs[i];
        } else {
            args[arg_idx++] = k->scalar_args[i].data;
        }
    }

    dim3 grid_dim, block_dim;
    grid_dim.x = grid[0]; grid_dim.y = grid[1]; grid_dim.z = grid[2];
    block_dim.x = block[0]; block_dim.y = block[1]; block_dim.z = block[2];

    long long total_blocks =
        (long long)grid_dim.x * grid_dim.y * grid_dim.z;
    long long threads_per_block =
        (long long)block_dim.x * block_dim.y * block_dim.z;

#ifdef _OPENMP
    // The num_threads clause has to be honoured exactly, or a barrier inside
    // the kernel would wait for threads that never arrive.
    omp_set_dynamic(0);
#endif

    // Blocks run serially, so the result is deterministic, and the threads of
    // one block run together. That nesting is what makes __syncthreads() (an
    // OpenMP barrier) synchronise exactly the threads of a single block.
    for (long long b = 0; b < total_blocks; b++) {
        unsigned int bx = (unsigned int)(b % grid_dim.x);
        unsigned int by = (unsigned int)((b / grid_dim.x) % grid_dim.y);
        unsigned int bz = (unsigned int)(b / ((long long)grid_dim.x * grid_dim.y));
#ifdef _OPENMP
        #pragma omp parallel num_threads(threads_per_block)
        run_thread(k, args, grid_dim, block_dim, bx, by, bz,
            omp_get_thread_num());
#else
        for (long long t = 0; t < threads_per_block; t++) {
            run_thread(k, args, grid_dim, block_dim, bx, by, bz, t);
        }
#endif
    }
}

void lfortran_gpu_sync(lfortran_gpu_ctx* ctx) {
    (void)ctx;
}
