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

// A buffer argument larger than this gets new device memory at every launch
// instead of kept memory (see buffer_cache). Compared with copying the
// argument, creating a buffer costs most for a small one, and a large one
// kept for a kernel launched once, as a setup loop is, would hold its memory
// until the program ends.
static const size_t CACHED_BUFFER_MAX = (size_t)1024 * 1024;

// The device memory of the arguments of one kernel, kept from one launch to
// the next instead of a cudaMalloc and cudaFree for every argument of every
// launch. Argument `i` of every launch uses buffers[i], grown when a launch
// needs more bytes; the host's bytes are copied in before each launch and
// back after it, so every launch sees the current host data. A kernel object
// owns the memory from its load to its release (`in_use`), so two launches
// of the same kernel never share it; one loaded while it is taken allocates
// memory of its own for its launch. Scratch buffers are not kept.
typedef struct buffer_cache {
    void *buffers[MAX_ARGS];
    size_t capacities[MAX_ARGS];
    int in_use;
} buffer_cache;

// Registry for pre-compiled CUDA kernels
static struct {
    const char *name;
    kernel_func_t func;
    buffer_cache cache;
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
    // A buffer only the device holds: nothing is copied in or back.
    int arg_is_scratch[MAX_ARGS];
    // A buffer in `cache`, which the kernel object does not free.
    int arg_is_cached[MAX_ARGS];
    int n_args;
    // The memory of the kernel's registry entry, or NULL when another
    // kernel object of the same kernel holds it.
    buffer_cache *cache;
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
    // The reset frees all device memory, the kept buffers included.
    for (int i = 0; i < n_registered; i++) {
        memset(&kernel_registry[i].cache, 0, sizeof(buffer_cache));
    }
    cudaDeviceReset();
    free(ctx);
}

lfortran_gpu_kernel* lfortran_gpu_load_kernel(
    lfortran_gpu_ctx* ctx, const char* entry_point, int entry_point_len)
{
    if (!ctx || !entry_point) return NULL;

    char name_buf[LFORTRAN_GPU_MAX_KERNEL_NAME];
    lfortran_gpu_copy_kernel_name(name_buf, entry_point, entry_point_len);

    // Look up the kernel in the registry
    int entry = -1;
    for (int i = 0; i < n_registered; i++) {
        if (strcmp(kernel_registry[i].name, name_buf) == 0) {
            entry = i;
            break;
        }
    }
    if (entry < 0) {
        fprintf(stderr, "lfortran_gpu_load_kernel: kernel '%s' not found in registry\n",
            name_buf);
        exit(1);
    }

    lfortran_gpu_kernel *k = (lfortran_gpu_kernel*)calloc(1, sizeof(lfortran_gpu_kernel));
    k->func = kernel_registry[entry].func;
    k->ctx = ctx;
    k->n_args = 0;
    buffer_cache *cache = &kernel_registry[entry].cache;
    if (!__atomic_exchange_n(&cache->in_use, 1, __ATOMIC_ACQUIRE)) {
        k->cache = cache;
    }
    return k;
}

void lfortran_gpu_release_kernel(lfortran_gpu_kernel* k) {
    if (!k) return;
    // Free device buffers
    for (int i = 0; i < k->n_args; i++) {
        if (k->arg_is_buffer[i] && !k->arg_is_cached[i]
                && k->buffer_device_ptrs[i]) {
            cudaFree(k->buffer_device_ptrs[i]);
        }
        if (!k->arg_is_buffer[i] && k->scalar_args[i].data) {
            free(k->scalar_args[i].data);
        }
    }
    if (k->cache) __atomic_store_n(&k->cache->in_use, 0, __ATOMIC_RELEASE);
    free(k);
}

// The kept memory of argument `idx`, with room for at least `size` bytes.
static void* cached_buffer(buffer_cache *cache, int idx, size_t size) {
    if (cache->buffers[idx] && cache->capacities[idx] >= size) {
        return cache->buffers[idx];
    }
    size_t length = size;
    if (cache->buffers[idx]) {
        // Grow geometrically, so that an argument that grows a little at
        // every launch does not get new memory at every launch.
        size_t doubled = 2 * cache->capacities[idx];
        if (doubled > length && doubled <= CACHED_BUFFER_MAX) {
            length = doubled;
        }
        cudaFree(cache->buffers[idx]);
        cache->buffers[idx] = NULL;
        cache->capacities[idx] = 0;
    }
    void *d_ptr = NULL;
    cudaError_t err = cudaMalloc(&d_ptr, length);
    if (err != cudaSuccess) {
        fprintf(stderr, "lfortran_gpu_set_buffer_arg: cudaMalloc failed: %s\n",
            cudaGetErrorString(err));
        exit(1);
    }
    cache->buffers[idx] = d_ptr;
    cache->capacities[idx] = length;
    return d_ptr;
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
    cudaError_t err;
    if (k->cache && size > 0 && size <= CACHED_BUFFER_MAX) {
        d_ptr = cached_buffer(k->cache, idx, size);
        k->arg_is_cached[idx] = 1;
    } else {
        err = cudaMalloc(&d_ptr, size);
        if (err != cudaSuccess) {
            fprintf(stderr, "lfortran_gpu_set_buffer_arg: cudaMalloc failed: %s\n",
                cudaGetErrorString(err));
            exit(1);
        }
    }
    err = cudaMemcpy(d_ptr, ptr, size, cudaMemcpyHostToDevice);
    if (err != cudaSuccess) {
        fprintf(stderr, "lfortran_gpu_set_buffer_arg: cudaMemcpy H2D failed: %s\n",
            cudaGetErrorString(err));
        exit(1);
    }
    k->buffer_device_ptrs[idx] = d_ptr;
}

void lfortran_gpu_set_scratch_arg(lfortran_gpu_kernel* k, int idx,
    size_t size)
{
    if (!k || idx >= MAX_ARGS) return;
    k->buffer_host_ptrs[idx] = NULL;
    k->buffer_sizes[idx] = size;
    k->arg_is_buffer[idx] = 1;
    k->arg_is_scratch[idx] = 1;
    if (idx >= k->n_args) k->n_args = idx + 1;
    void *d_ptr = NULL;
    if (size > 0) {
        cudaError_t err = cudaMalloc(&d_ptr, size);
        if (err != cudaSuccess) {
            fprintf(stderr, "lfortran_gpu_set_scratch_arg: cudaMalloc "
                "failed: %s\n", cudaGetErrorString(err));
            exit(1);
        }
    }
    k->buffer_device_ptrs[idx] = d_ptr;
}

void lfortran_gpu_read_scratch_arg(lfortran_gpu_kernel* k, int idx,
    void* dst, size_t size)
{
    if (!k || idx >= MAX_ARGS || size == 0) return;
    if (!k->arg_is_scratch[idx] || size > k->buffer_sizes[idx]) {
        fprintf(stderr, "lfortran_gpu_read_scratch_arg: no scratch buffer "
            "%d of %zu bytes\n", idx, size);
        exit(1);
    }
    cudaError_t err = cudaMemcpy(dst, k->buffer_device_ptrs[idx], size,
        cudaMemcpyDeviceToHost);
    if (err != cudaSuccess) {
        fprintf(stderr, "lfortran_gpu_read_scratch_arg: cudaMemcpy D2H "
            "failed: %s\n", cudaGetErrorString(err));
        exit(1);
    }
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
        if (k->arg_is_buffer[i] && !k->arg_is_scratch[i]
                && k->buffer_device_ptrs[i]) {
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

extern "C" long long lfortran_gpu_heap_size(void) {
    return lfortran_gpu_parse_heap_size();
}
