#include <cuda_runtime.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef __GLIBC__
#include <malloc.h>
#endif

// We implement the same C API as the Metal runtime
// (lfortran_gpu_runtime.h), but using the CUDA runtime API: NVIDIA's own
// (`--gpu=cuda`), the CPU emulation of it (`--gpu=cuda_cpu`,
// cuda_cpu/cuda_runtime.h) or HIP for AMD GPUs (`--gpu=hip`,
// hip/cuda_runtime.h).

#include "lfortran_gpu_runtime.h"

#define MAX_ARGS 32

typedef void (*kernel_func_t)(void);

/*
Launches are asynchronous, and the device keeps what a kernel writes; this is
the same model as the Metal runtime (see lfortran_gpu_metal.m), with the
default stream ordering every copy and launch:

* A buffer argument the kernel may write (LFORTRAN_GPU_WRITE) is resident: it
  has an entry in a table of host ranges, which owns device memory. The host
  bytes are copied in when the entry is not dirty, unless the kernel writes
  all of them first (LFORTRAN_GPU_WRITE_DISCARD); after the launch the
  entry is dirty, a later launch handed the same range uses the same memory
  without a copy, and lfortran_gpu_sync copies every dirty entry back.
* A buffer the kernel only reads is copied in, before the launch on the
  stream, into memory the argument slot of its kernel keeps; the stream
  orders that copy after every earlier launch that read the memory. When it
  is a dirty range, the kernel reads the resident memory instead. A later
  launch of the same kernel handle that is not handed the buffer again (the
  next chunk of a launch that runs in chunks) reads what the first one
  copied, as long as no other handle has used that memory since.
* A range that overlaps a dirty one without being the same range makes the
  runtime sync first.
* A scratch buffer, which only the device holds, is the memory its argument
  slot keeps as well. What a launch leaves there is dead once it ends (a part
  the host reads back is read before the next launch is made), and the
  stream runs the launches one after another, so every launch of the kernel
  uses the same memory, however many of them are still pending.
* Memory a launch may still use is freed only at the next sync.

That is the copy memory model, which every device has. A device that shares
the host's memory and can access what the host allocated (an APU such as the
AMD MI300A with HSA_XNACK=1) has the unified memory model too, where a kernel
uses host memory in place and nothing is copied. It is a special case of the
copy model, with the same table and the same contract:

* A written range is a resident entry whose device memory is the host range
  itself (`in_place`): nothing is allocated, copied in or copied back, and
  lfortran_gpu_sync only waits. The host does not touch a written range
  before the sync, as in the copy model, so no other wait is needed.
* A small range a launch only reads is still copied; a larger one is used
  in place. The host does not change or free a range a pending launch uses
  in place before lfortran_gpu_wait (or lfortran_gpu_sync), which the
  compiler places where the host next does (gpu_launch_sync.cpp).
* lfortran_gpu_discard of a range a pending launch uses in place waits for
  the launch first, since the host frees the memory the kernel uses.

The model is chosen when the runtime starts: LFORTRAN_GPU_MEMORY=copy or
unified selects one, and auto (the default) selects unified on a device that
is integrated with the host and can access its pageable memory.
LFORTRAN_GPU_VERBOSE=1 prints the model chosen. In the unified memory model
the runtime makes glibc keep freed memory mapped (keep_freed_memory_mapped);
LFORTRAN_GPU_KEEP_MEMORY=0 leaves malloc as it is.
*/

enum { MAX_RESIDENT = 1024, MAX_DEFERRED = 4096 };

// In the unified memory model, a range a launch only reads is copied when it
// is at most this many bytes, and used in place otherwise.
static const size_t UNIFIED_COPY_MAX_BYTES = 64 * 1024;

typedef struct resident {
    void *host;
    size_t size;
    void *device;
    size_t capacity;
    int dirty;
    int used;
    // `device` is `host` itself: the unified memory model.
    int in_place;
} resident;

// Whether kernels use host memory in place (the unified memory model)
// rather than copies of it in device memory (the copy model).
static int unified_memory = 0;

typedef struct slot_cache {
    void *buffers[MAX_ARGS];
    size_t capacities[MAX_ARGS];
    // The kernel handle whose read-only buffer the memory holds a copy of,
    // or NULL.
    const void *filled_by[MAX_ARGS];
} slot_cache;

// Registry for pre-compiled CUDA kernels, one entry for each kernel a
// translation unit registers, however many there are. An entry is never
// moved, since a loaded kernel keeps a pointer to its cache.
typedef struct kernel_entry {
    const char *name;
    kernel_func_t func;
    slot_cache cache;
} kernel_entry;
static kernel_entry **kernel_registry = NULL;
static int n_registered = 0;
static int registry_capacity = 0;

static resident entries[MAX_RESIDENT];
static int n_entries = 0;
static void *deferred_free[MAX_DEFERRED];
static int n_deferred = 0;
static uint64_t n_syncs = 0;

// Kernel code declares this function with C linkage, and a unit test
// compiles this file as C++ next to such a declaration.
#ifdef __cplusplus
extern "C"
#endif
void lfortran_gpu_register_kernel(const char *name, kernel_func_t func) {
    if (n_registered == registry_capacity) {
        int capacity = registry_capacity ? 2 * registry_capacity : 64;
        kernel_entry **grown = (kernel_entry**)realloc(kernel_registry,
            capacity * sizeof(kernel_entry*));
        if (!grown) {
            fprintf(stderr, "lfortran_gpu_register_kernel: out of memory\n");
            exit(1);
        }
        kernel_registry = grown;
        registry_capacity = capacity;
    }
    kernel_entry *entry = (kernel_entry*)calloc(1, sizeof(kernel_entry));
    if (!entry) {
        fprintf(stderr, "lfortran_gpu_register_kernel: out of memory\n");
        exit(1);
    }
    entry->name = name;
    entry->func = func;
    kernel_registry[n_registered++] = entry;
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
    // Whether the read-only buffer argument has been copied into the memory
    // of its slot since it was set.
    int copied[MAX_ARGS];
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
// Whether a launch since the last wait reads host memory in place.
static int pending_in_place_reads = 0;

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
    if (!entries[i].in_place) free_later(entries[i].device);
    memset(&entries[i], 0, sizeof(resident));
}

static void sync_all(void) {
    n_syncs++;
    pending_in_place_reads = 0;
    check(cudaDeviceSynchronize(), "cudaDeviceSynchronize");
    for (int i = 0; i < n_entries; i++) {
        if (!entries[i].used || !entries[i].dirty) continue;
        if (!entries[i].in_place) {
            check(cudaMemcpy(entries[i].host, entries[i].device,
                entries[i].size, cudaMemcpyDeviceToHost), "cudaMemcpy D2H");
        }
        entries[i].dirty = 0;
    }
    for (int i = 0; i < n_deferred; i++) {
        check(cudaFree(deferred_free[i]), "cudaFree");
    }
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

// The entry of the host range `host` of `size` bytes, made if there is none.
static resident* entry_for(void *host, size_t size) {
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
    return &entries[i];
}

// In the unified memory model: the entry `e` uses its host range in place.
// No advice is given to the driver: on an MI300A,
// hipMemAdvise(SetPreferredLocation) made kernels that use host memory in
// place erratic, from full speed in one run to 1-3 GB/s in the next.
static void* use_in_place(resident *e) {
    e->device = e->host;
    e->capacity = e->size;
    e->in_place = 1;
    return e->device;
}

// `discard`: the kernel writes all of the range before it reads it, so the
// host bytes are not copied in.
static void* resident_for_write(void *host, size_t size, int discard) {
    resident *e = entry_for(host, size);
    if (unified_memory) {
        e->dirty = 1;
        return use_in_place(e);
    }
    if (e->dirty) return e->device;
    if (!e->device || e->capacity < size) {
        free_later(e->device);
        e->device = device_alloc(size);
        e->capacity = size;
    }
    if (!discard) {
        check(cudaMemcpyAsync(e->device, host, size, cudaMemcpyHostToDevice,
            0), "cudaMemcpyAsync H2D");
    }
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

// The memory model of the device (see the comment at the top of this file),
// from LFORTRAN_GPU_MEMORY and what the device reports.
static int choose_unified_memory(int device) {
    int integrated = 0, pageable = 0;
    if (cudaDeviceGetAttribute(&integrated, cudaDevAttrIntegrated, device)
            != cudaSuccess) {
        integrated = 0;
    }
    if (cudaDeviceGetAttribute(&pageable, cudaDevAttrPageableMemoryAccess,
            device) != cudaSuccess) {
        pageable = 0;
    }
    const char *value = getenv("LFORTRAN_GPU_MEMORY");
    int unified;
    if (value == NULL || value[0] == '\0' || strcmp(value, "auto") == 0) {
        unified = integrated && pageable;
    } else if (strcmp(value, "copy") == 0) {
        unified = 0;
    } else if (strcmp(value, "unified") == 0) {
        if (!pageable) {
            fprintf(stderr, "LFORTRAN_GPU_MEMORY=unified: the GPU cannot "
                "access host memory (on an AMD GPU, set HSA_XNACK=1)\n");
            exit(1);
        }
        unified = 1;
    } else {
        fprintf(stderr, "LFORTRAN_GPU_MEMORY=%s is not a memory model "
            "(copy, unified or auto)\n", value);
        exit(1);
    }
    const char *verbose = getenv("LFORTRAN_GPU_VERBOSE");
    if (verbose != NULL && strcmp(verbose, "1") == 0) {
        fprintf(stderr, "lfortran_gpu: %s memory model (the GPU is %s the "
            "host and %s access its pageable memory)\n",
            unified ? "unified" : "copy",
            integrated ? "integrated with" : "separate from",
            pageable ? "can" : "cannot");
    }
#ifdef LFORTRAN_GPU_HIP
    // An APU waits by blocking (lfortran_gpu_hip_block_while_waiting).
    if (unified) lfortran_gpu_hip_block_while_waiting();
#endif
    return unified;
}

// In the unified memory model a kernel touches host memory through the
// host's page tables, and the first touch of a page by the GPU is a page
// fault that the driver serves: about 2 GB/s on an MI300A, so a kernel
// writing a fresh 512 MB array takes 0.2-0.4 s instead of 0.2 ms. glibc
// serves a large allocation with a fresh mmap and returns it to the system
// when it is freed, so every temporary array a loop allocates again (and the
// accumulators of a reduction) is fresh memory at every launch. Serving every
// allocation from the heap, and not trimming the heap when it shrinks,
// makes freed memory stay mapped, for the host and the GPU, and be reused.
static void keep_freed_memory_mapped(void) {
#ifdef M_MMAP_MAX
    const char *value = getenv("LFORTRAN_GPU_KEEP_MEMORY");
    if (value != NULL && strcmp(value, "0") == 0) return;
    mallopt(M_MMAP_MAX, 0);
    mallopt(M_TRIM_THRESHOLD, -1);
#endif
}

lfortran_gpu_ctx* lfortran_gpu_init(void) {
    static lfortran_gpu_ctx *singleton = NULL;
    if (singleton) return singleton;
    singleton = (lfortran_gpu_ctx*)calloc(1, sizeof(lfortran_gpu_ctx));
    singleton->device_id = 0;
    // Without a device every launch would be skipped and the program would
    // continue with the values its loops never computed.
    check(cudaSetDevice(0), "cudaSetDevice");
    unified_memory = choose_unified_memory(0);
    if (unified_memory) keep_freed_memory_mapped();
    return singleton;
}

void lfortran_gpu_shutdown(lfortran_gpu_ctx* ctx) {
    if (!ctx) return;
    sync_all();
    // The reset frees all device memory, the kept buffers included.
    for (int i = 0; i < n_registered; i++) {
        memset(&kernel_registry[i]->cache, 0, sizeof(slot_cache));
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
        if (strcmp(kernel_registry[i]->name, name_buf) == 0) {
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
    k->func = kernel_registry[entry]->func;
    k->ctx = ctx;
    k->n_args = 0;
    k->cache = &kernel_registry[entry]->cache;
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
    k->copied[idx] = 0;
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

    int is_buffer[MAX_ARGS];
    for (int i = 0; i < k->n_args; i++) {
        is_buffer[i] = k->kind[i] == ARG_BUFFER;
    }
    // Same-address writable aliases need one covering size for this launch,
    // not alternating sizes of the same dirty resident. Nonempty reads within
    // that range must use its shared binding too. Keep the original byte counts.
    size_t resident_sizes[MAX_ARGS] = {0};
    for (int i = 0; i < k->n_args; i++) {
        if (k->kind[i] != ARG_BUFFER || k->buffer_sizes[i] == 0) continue;
        resident_sizes[i] = k->buffer_sizes[i];
        for (int j = 0; j < k->n_args; j++) {
            if (k->kind[j] == ARG_BUFFER && k->access[j] != LFORTRAN_GPU_READ
                    && k->buffer_host_ptrs[j] == k->buffer_host_ptrs[i]
                    && k->buffer_sizes[j] > resident_sizes[i]) {
                resident_sizes[i] = k->buffer_sizes[j];
            }
        }
    }
    for (int i = 0; i < k->n_args; i++) {
        if (k->kind[i] == ARG_BUFFER
                && overlaps_dirty(k->buffer_host_ptrs[i],
                    resident_sizes[i])) {
            sync_all();
            break;
        }
    }
    // Preparing any argument may sync to retire allocations or evict residents.
    // That clears dirty flags and can invalidate earlier bindings: prepare them
    // again before dispatch, as in the Metal runtime.
    uint64_t syncs;
    // Whether the kernel reads host memory in place, which the host does
    // not change before lfortran_gpu_wait.
    int reads_in_place;
    do {
        syncs = n_syncs;
        reads_in_place = 0;
        // Writable ranges first, so a read of the same range uses their memory.
        for (int i = 0; i < k->n_args; i++) {
            if (k->kind[i] != ARG_BUFFER || k->access[i] == LFORTRAN_GPU_READ)
                continue;
            int discard = k->access[i] == LFORTRAN_GPU_WRITE_DISCARD
                && lfortran_gpu_discard_allowed(i, k->buffer_host_ptrs,
                    k->buffer_sizes, is_buffer, k->n_args);
            k->buffer_device_ptrs[i] = k->buffer_sizes[i] == 0 ? NULL
                : resident_for_write(k->buffer_host_ptrs[i], resident_sizes[i],
                    discard);
        }
        for (int i = 0; i < k->n_args; i++) {
            if (k->kind[i] != ARG_BUFFER || k->access[i] != LFORTRAN_GPU_READ)
                continue;
            size_t size = k->buffer_sizes[i];
            void *host = k->buffer_host_ptrs[i];
            void *d_ptr = NULL;
            if (size > 0) {
                d_ptr = dirty_resident(host, resident_sizes[i]);
                if (!d_ptr && unified_memory
                        && size > UNIFIED_COPY_MAX_BYTES) {
                    d_ptr = use_in_place(entry_for(host, size));
                    reads_in_place = 1;
                }
                if (d_ptr) {
                    k->copied[i] = 0;
                } else {
                    // The chunks of a launch hand the buffer over once: the
                    // copy the first one made is still there, unless the
                    // memory was replaced or another handle filled it.
                    void *previous = k->cache->buffers[i];
                    d_ptr = slot_buffer(k->cache, i, size);
                    if (!k->copied[i] || d_ptr != previous
                            || k->cache->filled_by[i] != k) {
                        check(cudaMemcpyAsync(d_ptr, host, size,
                            cudaMemcpyHostToDevice, 0),
                            "cudaMemcpyAsync H2D");
                        k->copied[i] = 1;
                        k->cache->filled_by[i] = k;
                    }
                }
            }
            k->buffer_device_ptrs[i] = d_ptr;
        }
        for (int i = 0; i < k->n_args; i++) {
            if (k->kind[i] != ARG_SCRATCH) continue;
            k->buffer_device_ptrs[i] = k->buffer_sizes[i] == 0 ? NULL
                : slot_buffer(k->cache, i, k->buffer_sizes[i]);
            k->cache->filled_by[i] = NULL;
        }
    } while (syncs != n_syncs);

#ifdef LFORTRAN_CUDA_CPU_EMULATION
    // In the CPU emulation, a range the kernel discards holds garbage (0xff
    // bytes) at every launch, not only in fresh device memory: whatever an
    // earlier launch left in a reused buffer must not let a kernel that reads
    // the range before writing all of it pass. A device keeps the old bytes.
    // In the unified memory model the range is host memory, which the
    // emulated stream, running everything when it is issued, lets the host
    // fill itself.
    for (int i = 0; i < k->n_args; i++) {
        if (k->kind[i] == ARG_BUFFER && k->buffer_sizes[i] > 0
                && k->access[i] == LFORTRAN_GPU_WRITE_DISCARD
                && lfortran_gpu_discard_allowed(i, k->buffer_host_ptrs,
                    k->buffer_sizes, is_buffer, k->n_args)) {
            if (unified_memory) {
                memset(k->buffer_device_ptrs[i], 0xff, k->buffer_sizes[i]);
            } else {
                check(cudaMemsetAsync(k->buffer_device_ptrs[i], 0xff,
                    k->buffer_sizes[i], 0), "cudaMemsetAsync");
            }
        }
    }
#endif

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

    dim3 grid_dim = {(unsigned)grid[0], (unsigned)grid[1], (unsigned)grid[2]};
    dim3 block_dim = {(unsigned)block[0], (unsigned)block[1],
        (unsigned)block[2]};
    check(cudaLaunchKernel((const void*)k->func, grid_dim, block_dim, args, 0,
        0), "cudaLaunchKernel");
    if (reads_in_place) pending_in_place_reads = 1;
}

void lfortran_gpu_sync(lfortran_gpu_ctx* ctx) {
    if (!ctx) return;
    sync_all();
}

void lfortran_gpu_wait(lfortran_gpu_ctx* ctx) {
    if (!ctx || !pending_in_place_reads) return;
    pending_in_place_reads = 0;
    check(cudaDeviceSynchronize(), "cudaDeviceSynchronize");
}

void lfortran_gpu_discard(lfortran_gpu_ctx* ctx, void* ptr, size_t size) {
    if (!ctx || size == 0) return;
    // In the unified memory model a pending launch may still write the host
    // memory that is about to be freed.
    for (int i = 0; unified_memory && i < n_entries; i++) {
        if (entries[i].used && entries[i].dirty && entries[i].in_place
                && overlaps(entries[i].host, entries[i].size, ptr, size)) {
            sync_all();
            break;
        }
    }
    for (int i = 0; i < n_entries; i++) {
        if (entries[i].used
                && overlaps(entries[i].host, entries[i].size, ptr, size)) {
            remove_entry(i);
        }
    }
}

long long lfortran_gpu_cache_bytes(void) {
    static long long bytes = -1;
    if (bytes < 0) {
        int device = 0, size = 0;
        if (cudaGetDevice(&device) != cudaSuccess
                || cudaDeviceGetAttribute(&size, cudaDevAttrL2CacheSize,
                    device) != cudaSuccess) {
            size = 0;
        }
        bytes = size;
#ifdef LFORTRAN_GPU_HIP
        bytes = lfortran_gpu_hip_last_level_cache(device, bytes);
#endif
    }
    return bytes;
}

long long lfortran_gpu_heap_size(void) {
    return lfortran_gpu_parse_heap_size();
}

void lfortran_gpu_metal_telemetry_snapshot(lfortran_gpu_ctx* ctx,
    lfortran_gpu_metal_telemetry *out)
{
    (void)ctx;
    if (!out) return;
    memset(out, 0, sizeof(*out));
}

void lfortran_gpu_metal_telemetry_snapshot_sync(lfortran_gpu_ctx* ctx,
    lfortran_gpu_metal_telemetry *out)
{
    if (ctx) lfortran_gpu_sync(ctx);
    lfortran_gpu_metal_telemetry_snapshot(ctx, out);
}
