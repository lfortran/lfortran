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

/*
Launches are asynchronous, and the device keeps what a kernel writes; this is
the same model as the Metal runtime (see lfortran_gpu_metal.m), with the
default stream ordering every copy and launch:

* A buffer argument the kernel may write (LFORTRAN_GPU_WRITE) is resident: it
  has an entry in a table of host ranges, which owns device memory. The host
  bytes are copied in when the entry is not dirty; after the launch the
  entry is dirty, a later launch handed the same range uses the same memory
  without a copy, and lfortran_gpu_sync copies every dirty entry back.
* A buffer the kernel only reads is copied in, before the launch on the
  stream, into memory the argument slot of its kernel keeps; the stream
  orders that copy after every earlier launch that read the memory. When it
  is a dirty range, the kernel reads the resident memory instead.
* A range that overlaps a dirty one without being the same range makes the
  runtime sync first.
* A scratch buffer, which only the device holds, is the memory its argument
  slot keeps as well. What a launch leaves there is dead once it ends (a part
  the host reads back is read before the next launch is made), and the
  stream runs the launches one after another, so every launch of the kernel
  uses the same memory, however many of them are still pending.
* Memory a launch may still use is freed only at the next sync.
*/

enum { MAX_RESIDENT = 1024, MAX_DEFERRED = 4096 };

typedef struct resident {
    void *host;
    size_t size;
    void *device;
    size_t capacity;
    int dirty;
    int used;
} resident;

typedef struct slot_cache {
    void *buffers[MAX_ARGS];
    size_t capacities[MAX_ARGS];
} slot_cache;

// Registry for pre-compiled CUDA kernels
static struct {
    const char *name;
    kernel_func_t func;
    slot_cache cache;
} kernel_registry[MAX_KERNELS];
static int n_registered = 0;

static resident entries[MAX_RESIDENT];
static int n_entries = 0;
static void *deferred_free[MAX_DEFERRED];
static int n_deferred = 0;

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

enum { ARG_NONE = 0, ARG_BUFFER, ARG_SCALAR, ARG_SCRATCH };

struct lfortran_gpu_kernel {
    kernel_func_t func;
    lfortran_gpu_ctx *ctx;
    void *buffer_host_ptrs[MAX_ARGS];
    void *buffer_device_ptrs[MAX_ARGS];
    size_t buffer_sizes[MAX_ARGS];
    int access[MAX_ARGS];
    scalar_arg scalar_args[MAX_ARGS];
    int kind[MAX_ARGS];
    int n_args;
    slot_cache *cache;
};

static void check(cudaError_t err, const char *what) {
    if (err != cudaSuccess) {
        fprintf(stderr, "lfortran_gpu: %s failed: %s\n", what,
            cudaGetErrorString(err));
        exit(1);
    }
}

static void* device_alloc(size_t size) {
    void *d_ptr = NULL;
    check(cudaMalloc(&d_ptr, size), "cudaMalloc");
    return d_ptr;
}

static void sync_all(void);

// Frees device memory at the next sync, when no launch can use it anymore.
static void free_later(void *d_ptr) {
    if (!d_ptr) return;
    if (n_deferred == MAX_DEFERRED) sync_all();
    deferred_free[n_deferred++] = d_ptr;
}

static int overlaps(const void *a, size_t na, const void *b, size_t nb) {
    const char *pa = (const char *)a, *pb = (const char *)b;
    return na > 0 && nb > 0 && pa < pb + nb && pb < pa + na;
}

static int find_entry(void *host) {
    for (int i = 0; i < n_entries; i++) {
        if (entries[i].used && entries[i].host == host) return i;
    }
    return -1;
}

static void remove_entry(int i) {
    free_later(entries[i].device);
    memset(&entries[i], 0, sizeof(resident));
}

static void sync_all(void) {
    check(cudaDeviceSynchronize(), "cudaDeviceSynchronize");
    for (int i = 0; i < n_entries; i++) {
        if (!entries[i].used || !entries[i].dirty) continue;
        check(cudaMemcpy(entries[i].host, entries[i].device, entries[i].size,
            cudaMemcpyDeviceToHost), "cudaMemcpy D2H");
        entries[i].dirty = 0;
    }
    for (int i = 0; i < n_deferred; i++) cudaFree(deferred_free[i]);
    n_deferred = 0;
}

static int overlaps_dirty(void *host, size_t size) {
    for (int i = 0; i < n_entries; i++) {
        resident *e = &entries[i];
        if (!e->used || !e->dirty) continue;
        if (e->host == host && e->size == size) continue;
        if (overlaps(e->host, e->size, host, size)) return 1;
    }
    return 0;
}

static void* resident_for_write(void *host, size_t size) {
    int i = find_entry(host);
    if (i >= 0 && entries[i].size != size) {
        if (entries[i].dirty) sync_all();
        entries[i].size = size;
    }
    if (i < 0) {
        for (int j = 0; j < n_entries; j++) {
            if (!entries[j].used) { i = j; break; }
        }
        if (i < 0) {
            if (n_entries == MAX_RESIDENT) {
                sync_all();
                for (int j = 0; j < n_entries; j++) remove_entry(j);
                n_entries = 0;
            }
            i = n_entries++;
        }
        entries[i].host = host;
        entries[i].size = size;
        entries[i].used = 1;
    }
    resident *e = &entries[i];
    if (e->dirty) return e->device;
    if (!e->device || e->capacity < size) {
        free_later(e->device);
        e->device = device_alloc(size);
        e->capacity = size;
    }
    check(cudaMemcpyAsync(e->device, host, size, cudaMemcpyHostToDevice, 0),
        "cudaMemcpyAsync H2D");
    e->dirty = 1;
    return e->device;
}

static void* dirty_resident(void *host, size_t size) {
    int i = find_entry(host);
    if (i < 0 || !entries[i].dirty || entries[i].size != size) return NULL;
    return entries[i].device;
}

// The memory argument `idx` of a kernel keeps for what it only reads, or
// for its scratch buffer.
static void* slot_buffer(slot_cache *cache, int idx, size_t size) {
    if (cache->buffers[idx] && cache->capacities[idx] >= size) {
        return cache->buffers[idx];
    }
    free_later(cache->buffers[idx]);
    size_t length = size;
    if (cache->capacities[idx] * 2 > length) {
        length = cache->capacities[idx] * 2;
    }
    cache->buffers[idx] = device_alloc(length);
    cache->capacities[idx] = length;
    return cache->buffers[idx];
}

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
    sync_all();
    // The reset frees all device memory, the kept buffers included.
    for (int i = 0; i < n_registered; i++) {
        memset(&kernel_registry[i].cache, 0, sizeof(slot_cache));
    }
    memset(entries, 0, sizeof(entries));
    n_entries = 0;
    cudaDeviceReset();
    free(ctx);
}

lfortran_gpu_kernel* lfortran_gpu_load_kernel(
    lfortran_gpu_ctx* ctx, const char* entry_point, int entry_point_len)
{
    if (!ctx || !entry_point) return NULL;

    char name_buf[LFORTRAN_GPU_MAX_KERNEL_NAME];
    lfortran_gpu_copy_kernel_name(name_buf, entry_point, entry_point_len);

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
    k->cache = &kernel_registry[entry].cache;
    return k;
}

void lfortran_gpu_release_kernel(lfortran_gpu_kernel* k) {
    if (!k) return;
    for (int i = 0; i < k->n_args; i++) {
        if (k->kind[i] == ARG_SCALAR && k->scalar_args[i].data) {
            free(k->scalar_args[i].data);
        }
    }
    free(k);
}

void lfortran_gpu_set_buffer_arg(lfortran_gpu_kernel* k, int idx,
    void* ptr, size_t size, int access)
{
    if (!k || idx >= MAX_ARGS) return;
    k->buffer_host_ptrs[idx] = ptr;
    k->buffer_sizes[idx] = size;
    k->access[idx] = access;
    k->kind[idx] = ARG_BUFFER;
    if (idx >= k->n_args) k->n_args = idx + 1;
}

void lfortran_gpu_set_scratch_arg(lfortran_gpu_kernel* k, int idx,
    size_t size)
{
    if (!k || idx >= MAX_ARGS) return;
    k->buffer_host_ptrs[idx] = NULL;
    k->buffer_sizes[idx] = size;
    k->buffer_device_ptrs[idx] = NULL;
    k->kind[idx] = ARG_SCRATCH;
    if (idx >= k->n_args) k->n_args = idx + 1;
}

void lfortran_gpu_read_scratch_arg(lfortran_gpu_kernel* k, int idx,
    void* dst, size_t size)
{
    if (!k || idx >= MAX_ARGS || size == 0) return;
    if (k->kind[idx] != ARG_SCRATCH || !k->buffer_device_ptrs[idx]
            || size > k->buffer_sizes[idx]) {
        fprintf(stderr, "lfortran_gpu_read_scratch_arg: no scratch buffer "
            "%d of %zu bytes\n", idx, size);
        exit(1);
    }
    sync_all();
    check(cudaMemcpy(dst, k->buffer_device_ptrs[idx], size,
        cudaMemcpyDeviceToHost), "cudaMemcpy D2H");
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
    k->kind[idx] = ARG_SCALAR;
    if (idx >= k->n_args) k->n_args = idx + 1;
}

void lfortran_gpu_launch(lfortran_gpu_ctx* ctx, lfortran_gpu_kernel* k,
    int grid[3], int block[3])
{
    lfortran_gpu_validate_launch(grid, block);
    if (!ctx || !k) return;

    for (int i = 0; i < k->n_args; i++) {
        if (k->kind[i] == ARG_BUFFER
                && overlaps_dirty(k->buffer_host_ptrs[i],
                    k->buffer_sizes[i])) {
            sync_all();
            break;
        }
    }
    // The ranges the kernel may write first, so that one it is also handed
    // to read is read from the same memory.
    for (int i = 0; i < k->n_args; i++) {
        if (k->kind[i] != ARG_BUFFER || k->access[i] != LFORTRAN_GPU_WRITE)
            continue;
        k->buffer_device_ptrs[i] = k->buffer_sizes[i] == 0 ? NULL
            : resident_for_write(k->buffer_host_ptrs[i], k->buffer_sizes[i]);
    }
    for (int i = 0; i < k->n_args; i++) {
        if (k->kind[i] != ARG_BUFFER || k->access[i] == LFORTRAN_GPU_WRITE)
            continue;
        size_t size = k->buffer_sizes[i];
        void *host = k->buffer_host_ptrs[i];
        void *d_ptr = NULL;
        if (size > 0) {
            d_ptr = dirty_resident(host, size);
            if (!d_ptr) {
                d_ptr = slot_buffer(k->cache, i, size);
                check(cudaMemcpyAsync(d_ptr, host, size,
                    cudaMemcpyHostToDevice, 0), "cudaMemcpyAsync H2D");
            }
        }
        k->buffer_device_ptrs[i] = d_ptr;
    }
    for (int i = 0; i < k->n_args; i++) {
        if (k->kind[i] != ARG_SCRATCH) continue;
        k->buffer_device_ptrs[i] = k->buffer_sizes[i] == 0 ? NULL
            : slot_buffer(k->cache, i, k->buffer_sizes[i]);
    }

    // Build the kernel argument array for cudaLaunchKernel: a device
    // pointer for a buffer, the value for a scalar.
    void *args[MAX_ARGS];
    int arg_idx = 0;
    for (int i = 0; i < k->n_args; i++) {
        if (k->kind[i] == ARG_SCALAR) {
            args[arg_idx++] = k->scalar_args[i].data;
        } else {
            args[arg_idx++] = &k->buffer_device_ptrs[i];
        }
    }

    dim3 gridDim(grid[0], grid[1], grid[2]);
    dim3 blockDim(block[0], block[1], block[2]);
    check(cudaLaunchKernel((const void*)k->func, gridDim, blockDim, args, 0,
        0), "cudaLaunchKernel");
}

void lfortran_gpu_sync(lfortran_gpu_ctx* ctx) {
    if (!ctx) return;
    sync_all();
}

void lfortran_gpu_discard(lfortran_gpu_ctx* ctx, void* ptr, size_t size) {
    if (!ctx || size == 0) return;
    for (int i = 0; i < n_entries; i++) {
        if (entries[i].used
                && overlaps(entries[i].host, entries[i].size, ptr, size)) {
            remove_entry(i);
        }
    }
}

} // extern "C"

extern "C" long long lfortran_gpu_heap_size(void) {
    return lfortran_gpu_parse_heap_size();
}

extern "C" void lfortran_gpu_metal_telemetry_snapshot(lfortran_gpu_ctx* ctx,
    lfortran_gpu_metal_telemetry *out)
{
    (void)ctx;
    if (!out) return;
    memset(out, 0, sizeof(*out));
}

extern "C" void lfortran_gpu_metal_telemetry_snapshot_sync(lfortran_gpu_ctx* ctx,
    lfortran_gpu_metal_telemetry *out)
{
    if (ctx) lfortran_gpu_sync(ctx);
    lfortran_gpu_metal_telemetry_snapshot(ctx, out);
}
