#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cuda_cpu_device.h"
#include "cuda_cpu_emulator.h"
#include "cuda_runtime.h"

// CPU emulation of the part of the CUDA runtime API that the LFortran CUDA GPU
// runtime (lfortran_gpu_cuda.c) uses, so that `--gpu=cuda_cpu` runs that same
// runtime on a machine without a GPU or a CUDA toolkit.
//
// Device memory is memory of its own: cudaMalloc makes a separate host
// allocation, and only the copies move bytes between it and host memory, so a
// copy the runtime leaves out, or one it makes too late, shows as it would on
// a device. New device memory is filled with garbage (0xff bytes), because
// what a kernel reads from memory nobody wrote is not zero on a device either.
//
// There is one device, 0, with one stream, the default one. The stream runs
// every copy and launch to completion before the call that issues it returns,
// in the order they are issued. That is the order CUDA keeps; CUDA too copies
// pageable host memory before cudaMemcpyAsync returns, and a launch running
// early cannot be told apart by a program that waits, as it has to, for the
// results with a synchronizing call before it reads them. What the emulation
// keeps track of is whether work issued to the stream would still be pending
// on a device, for the statistics in cuda_cpu_emulator.h.
//
// A kernel is the per-thread thunk `void f(void **args)` that the CUDA code
// generator emits for it in emulation, called with the argument array of the
// launch. The blocks of a launch run one after another, as do the threads of a
// block, each with its built-in coordinates set; the threads never run
// concurrently, so __syncthreads() cannot be honoured (see cuda_cpu_device.h).

// Built-in thread coordinates, written once per emulated thread.
#ifdef _MSC_VER
__declspec(thread) uint3 threadIdx = {0, 0, 0};
__declspec(thread) uint3 blockIdx = {0, 0, 0};
__declspec(thread) dim3 blockDim = {1, 1, 1};
__declspec(thread) dim3 gridDim = {1, 1, 1};
#else
__thread uint3 threadIdx = {0, 0, 0};
__thread uint3 blockIdx = {0, 0, 0};
__thread dim3 blockDim = {1, 1, 1};
__thread dim3 gridDim = {1, 1, 1};
#endif

void lfortran_cuda_cpu_barrier_unsupported(void) {
    fprintf(stderr, "lfortran_cuda_cpu: __syncthreads() is not supported by "
        "the cpu emulation of cuda, which runs the threads of a block one "
        "after another\n");
    exit(1);
}

// The device limits, those of a typical CUDA GPU. The L2 cache, which the
// loop transformations size their tiles for, is that of a large one; the
// kernels run on the host, whose caches hold that much as well.
enum {
    MAX_THREADS_PER_BLOCK = 1024,
    MAX_BLOCK_DIM_X = 1024,
    MAX_BLOCK_DIM_Y = 1024,
    MAX_BLOCK_DIM_Z = 64,
    MAX_GRID_DIM_X = 2147483647,
    MAX_GRID_DIM_Y = 65535,
    MAX_GRID_DIM_Z = 65535,
    WARP_SIZE = 32,
    L2_CACHE_SIZE = 64 * 1024 * 1024
};

typedef struct allocation {
    char *base;
    size_t size;
} allocation;

// The live device allocations, sorted by address.
static allocation *allocations = NULL;
static size_t n_allocations = 0, allocations_capacity = 0;

static lfortran_cuda_cpu_stats stats;
static void (*synchronize_hook)(void) = NULL;
// Whether a copy or a launch was issued after the last synchronizing call.
static int pending_work = 0;
static int current_device = 0;

static cudaError_t fail(cudaError_t error) {
    stats.errors++;
    return error;
}

// The index of the first allocation whose start is above `p`.
static size_t upper_bound(const char *p) {
    size_t lo = 0, hi = n_allocations;
    while (lo < hi) {
        size_t mid = lo + (hi - lo) / 2;
        if ((uintptr_t)allocations[mid].base <= (uintptr_t)p) lo = mid + 1;
        else hi = mid;
    }
    return lo;
}

// The allocation `p` points into, or NULL.
static allocation *find(const void *p) {
    const char *c = (const char *)p;
    size_t i = upper_bound(c);
    if (i == 0) return NULL;
    allocation *a = &allocations[i - 1];
    return (uintptr_t)c - (uintptr_t)a->base < a->size ? a : NULL;
}

// Whether the `count` bytes at `p` are all in one device allocation.
static int device_range(const void *p, size_t count) {
    allocation *a = find(p);
    return a && count <= a->size - (size_t)((const char *)p - a->base);
}

// Whether none of the `count` bytes at `p` is device memory.
static int host_range(const void *p, size_t count) {
    const char *c = (const char *)p;
    size_t i = upper_bound(c);
    if (i > 0 && find(p)) return 0;
    return i == n_allocations
        || (uintptr_t)allocations[i].base - (uintptr_t)c >= count;
}

int lfortran_cuda_cpu_find_allocation(const void *ptr, void **base,
    size_t *size)
{
    allocation *a = find(ptr);
    if (!a) return 0;
    *base = a->base;
    *size = a->size;
    return 1;
}

const lfortran_cuda_cpu_stats *lfortran_cuda_cpu_get_stats(void) {
    return &stats;
}

void lfortran_cuda_cpu_set_synchronize_hook(void (*hook)(void)) {
    synchronize_hook = hook;
}

cudaError_t cudaSetDevice(int device) {
    if (device != 0) return fail(cudaErrorInvalidDevice);
    current_device = device;
    return cudaSuccess;
}

cudaError_t cudaGetDevice(int *device) {
    if (!device) return fail(cudaErrorInvalidValue);
    *device = current_device;
    return cudaSuccess;
}

cudaError_t cudaDeviceGetAttribute(int *value, cudaDeviceAttr attr,
    int device)
{
    if (!value) return fail(cudaErrorInvalidValue);
    if (device != 0) return fail(cudaErrorInvalidDevice);
    switch (attr) {
        case cudaDevAttrMaxThreadsPerBlock:
            *value = MAX_THREADS_PER_BLOCK; break;
        case cudaDevAttrMaxBlockDimX: *value = MAX_BLOCK_DIM_X; break;
        case cudaDevAttrMaxBlockDimY: *value = MAX_BLOCK_DIM_Y; break;
        case cudaDevAttrMaxBlockDimZ: *value = MAX_BLOCK_DIM_Z; break;
        case cudaDevAttrMaxGridDimX: *value = MAX_GRID_DIM_X; break;
        case cudaDevAttrMaxGridDimY: *value = MAX_GRID_DIM_Y; break;
        case cudaDevAttrMaxGridDimZ: *value = MAX_GRID_DIM_Z; break;
        case cudaDevAttrWarpSize: *value = WARP_SIZE; break;
        case cudaDevAttrL2CacheSize: *value = L2_CACHE_SIZE; break;
        default: return fail(cudaErrorInvalidValue);
    }
    return cudaSuccess;
}

cudaError_t cudaDeviceSynchronize(void) {
    if (synchronize_hook) synchronize_hook();
    stats.synchronizations++;
    pending_work = 0;
    return cudaSuccess;
}

// Waits for the pending work, which a call that frees memory does in CUDA.
static void implicit_synchronize(void) {
    if (pending_work) stats.implicit_synchronizations++;
    pending_work = 0;
}

cudaError_t cudaDeviceReset(void) {
    implicit_synchronize();
    for (size_t i = 0; i < n_allocations; i++) free(allocations[i].base);
    free(allocations);
    allocations = NULL;
    n_allocations = allocations_capacity = 0;
    stats.live_allocations = 0;
    current_device = 0;
    return cudaSuccess;
}

cudaError_t cudaMalloc(void **ptr, size_t size) {
    if (!ptr) return fail(cudaErrorInvalidValue);
    *ptr = NULL;
    if (size == 0) {
        stats.empty_mallocs++;
        return cudaSuccess;
    }
    if (n_allocations == allocations_capacity) {
        size_t capacity = allocations_capacity ? 2 * allocations_capacity : 64;
        allocation *grown = (allocation *)realloc(allocations,
            capacity * sizeof(allocation));
        if (!grown) return fail(cudaErrorMemoryAllocation);
        allocations = grown;
        allocations_capacity = capacity;
    }
    char *base = (char *)malloc(size);
    if (!base) return fail(cudaErrorMemoryAllocation);
    memset(base, 0xff, size);
    size_t i = upper_bound(base);
    memmove(&allocations[i + 1], &allocations[i],
        (n_allocations - i) * sizeof(allocation));
    allocations[i].base = base;
    allocations[i].size = size;
    n_allocations++;
    stats.live_allocations = n_allocations;
    stats.mallocs++;
    *ptr = base;
    return cudaSuccess;
}

cudaError_t cudaFree(void *ptr) {
    if (!ptr) return cudaSuccess;
    allocation *a = find(ptr);
    if (!a || a->base != (char *)ptr) return fail(cudaErrorInvalidValue);
    implicit_synchronize();
    free(a->base);
    size_t i = (size_t)(a - allocations);
    memmove(&allocations[i], &allocations[i + 1],
        (n_allocations - i - 1) * sizeof(allocation));
    n_allocations--;
    stats.live_allocations = n_allocations;
    stats.frees++;
    return cudaSuccess;
}

// Checks the two ranges of a copy against its direction, and copies.
static cudaError_t copy(void *dst, const void *src, size_t count,
    cudaMemcpyKind kind)
{
    if ((unsigned)kind > cudaMemcpyDefault) {
        return fail(cudaErrorInvalidMemcpyDirection);
    }
    if (count == 0) return cudaSuccess;
    if (!dst || !src) return fail(cudaErrorInvalidValue);
    if (kind == cudaMemcpyDefault) {
        int to_device = device_range(dst, count);
        int from_device = device_range(src, count);
        kind = to_device
            ? (from_device ? cudaMemcpyDeviceToDevice : cudaMemcpyHostToDevice)
            : (from_device ? cudaMemcpyDeviceToHost : cudaMemcpyHostToHost);
    }
    int to_device = kind == cudaMemcpyHostToDevice
        || kind == cudaMemcpyDeviceToDevice;
    int from_device = kind == cudaMemcpyDeviceToHost
        || kind == cudaMemcpyDeviceToDevice;
    if (!(to_device ? device_range(dst, count) : host_range(dst, count))
            || !(from_device ? device_range(src, count)
                             : host_range(src, count))) {
        return fail(cudaErrorInvalidValue);
    }
    memmove(dst, src, count);
    return cudaSuccess;
}

cudaError_t cudaMemcpy(void *dst, const void *src, size_t count,
    cudaMemcpyKind kind)
{
    cudaError_t err = copy(dst, src, count, kind);
    if (err != cudaSuccess) return err;
    // The copy waits for the work issued before it.
    pending_work = 0;
    stats.copies[kind]++;
    return cudaSuccess;
}

cudaError_t cudaMemcpyAsync(void *dst, const void *src, size_t count,
    cudaMemcpyKind kind, cudaStream_t stream)
{
    if (stream) return fail(cudaErrorInvalidResourceHandle);
    cudaError_t err = copy(dst, src, count, kind);
    if (err != cudaSuccess) return err;
    pending_work = 1;
    stats.async_copies[kind]++;
    return cudaSuccess;
}

cudaError_t cudaMemsetAsync(void *ptr, int value, size_t count,
    cudaStream_t stream)
{
    if (stream) return fail(cudaErrorInvalidResourceHandle);
    if (count == 0) return cudaSuccess;
    if (!ptr || !device_range(ptr, count)) return fail(cudaErrorInvalidValue);
    memset(ptr, value, count);
    pending_work = 1;
    stats.async_memsets++;
    return cudaSuccess;
}

static int valid_geometry(dim3 grid, dim3 block) {
    unsigned long long threads =
        (unsigned long long)block.x * block.y * block.z;
    return grid.x >= 1 && grid.y >= 1 && grid.z >= 1
        && block.x >= 1 && block.y >= 1 && block.z >= 1
        && grid.x <= (unsigned)MAX_GRID_DIM_X
        && grid.y <= MAX_GRID_DIM_Y && grid.z <= MAX_GRID_DIM_Z
        && block.x <= MAX_BLOCK_DIM_X && block.y <= MAX_BLOCK_DIM_Y
        && block.z <= MAX_BLOCK_DIM_Z && threads <= MAX_THREADS_PER_BLOCK;
}

typedef void (*thunk_t)(void **);

cudaError_t cudaLaunchKernel(const void *func, dim3 grid, dim3 block,
    void **args, size_t shared_mem, cudaStream_t stream)
{
    if (stream) return fail(cudaErrorInvalidResourceHandle);
    if (!func) return fail(cudaErrorInvalidDeviceFunction);
    // The generated kernels use no dynamic shared memory.
    if (shared_mem != 0) return fail(cudaErrorInvalidValue);
    if (!valid_geometry(grid, block)) {
        return fail(cudaErrorInvalidConfiguration);
    }
    thunk_t thunk = (thunk_t)func;
    unsigned long long threads_per_block =
        (unsigned long long)block.x * block.y * block.z;
    for (unsigned int bz = 0; bz < grid.z; bz++)
    for (unsigned int by = 0; by < grid.y; by++)
    for (unsigned int bx = 0; bx < grid.x; bx++) {
        for (unsigned long long t = 0; t < threads_per_block; t++) {
            blockIdx.x = bx;
            blockIdx.y = by;
            blockIdx.z = bz;
            threadIdx.x = (unsigned int)(t % block.x);
            threadIdx.y = (unsigned int)((t / block.x) % block.y);
            threadIdx.z = (unsigned int)(t / ((unsigned long long)block.x
                * block.y));
            blockDim = block;
            gridDim = grid;
            thunk(args);
        }
    }
    pending_work = 1;
    stats.launches++;
    return cudaSuccess;
}

const char *cudaGetErrorString(cudaError_t error) {
    switch (error) {
        case cudaSuccess: return "no error";
        case cudaErrorInvalidValue: return "invalid argument";
        case cudaErrorMemoryAllocation: return "out of memory";
        case cudaErrorInvalidConfiguration:
            return "invalid configuration argument";
        case cudaErrorInvalidMemcpyDirection:
            return "invalid copy direction for memcpy";
        case cudaErrorInvalidDeviceFunction: return "invalid device function";
        case cudaErrorInvalidDevice: return "invalid device ordinal";
        case cudaErrorInvalidResourceHandle:
            return "invalid resource handle";
    }
    return "unrecognized error code";
}
