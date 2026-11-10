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
  stream; the stream orders that copy after every earlier launch that read
  the memory. When it is a dirty range, the kernel reads the resident memory
  instead. A range of at least KEEP_READ_MIN_BYTES gets an entry that keeps
  the copy (`fresh`) until the next lfortran_gpu_wait or lfortran_gpu_sync:
  the host does not change a range a launch since the last wait reads before
  it waits (gpu_launch_sync.cpp places the waits), so later launches that read
  the range, such as the iterations of a time loop, use the copy without
  copying again. A wait keeps the copies inside the ranges the compiler
  passed to lfortran_gpu_keep right before it, which the host does not change
  until the launches after the wait read them: a time loop whose host reads
  the result of a reduction at every iteration waits there. A smaller range is copied into memory the argument slot of
  its kernel keeps; a later launch of the same kernel handle that is not
  handed the buffer again (the next chunk of a launch that runs in chunks)
  reads what the first one copied, as long as no other handle has used that
  memory since.
* An entry that no launch has used since the wait before the last one, and
  that is not dirty, is forgotten at a wait, and its device memory freed.
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
AMD MI300A with XNACK on) has the unified memory model too, where a kernel
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

A device with memory of its own that the driver can share with the host page
by page (CUDA managed memory on an NVIDIA GPU that supports concurrent
managed access) has the managed memory model, a special case of the copy
model too. The runtime replaces the program's allocator (the default
allocator of lfortran_intrinsics.c), so that an allocation of at least
MANAGED_MIN_BYTES, such as an allocatable array, is managed memory:

* A range inside a managed allocation is used in place, as in the unified
  model, and has no entry: the driver moves its pages to the device when a
  kernel touches them, and back when the host does, one page at a time.
  Nothing is copied at a launch, and lfortran_gpu_sync only waits.
* The host does not change a range a pending launch reads before
  lfortran_gpu_wait, as in the unified model.
* Freeing managed memory waits for every launch first.
* Smaller allocations, and memory the allocator did not make (stack and
  static arrays), keep the copy model.

The model is chosen at the first allocation the allocator could make managed
or when the runtime starts, whichever is first: LFORTRAN_GPU_MEMORY=copy,
unified or managed selects one, and auto (the default) selects unified on a
device that is integrated with the host and can access its pageable memory,
managed on an NVIDIA GPU with concurrent managed access, and copy otherwise.
LFORTRAN_GPU_VERBOSE=1 prints the model chosen. In the unified memory model
the runtime makes glibc keep freed memory mapped (keep_freed_memory_mapped);
LFORTRAN_GPU_KEEP_MEMORY=0 leaves malloc as it is.
*/

enum { MAX_RESIDENT = 1024, MAX_DEFERRED = 4096 };

// In the copy model, a range a launch only reads keeps its copy in device
// memory until the next wait when it is at least this many bytes.
static const size_t KEEP_READ_MIN_BYTES = 64 * 1024;

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
    // `device` holds a copy of the host bytes made since the last wait, which
    // the host has not changed since (the copy model).
    int fresh;
    // lfortran_gpu_keep was called for a range that holds the entry since
    // the last wait: the copy stays fresh past the next one.
    int keep;
    // The wait period (n_waits) in which a launch last used the entry.
    uint64_t last_use;
} resident;

// Ends the program with a message when `err` is an error.
static void check(cudaError_t err, const char *what);

enum { MEMORY_UNCHOSEN = -1, MEMORY_COPY, MEMORY_UNIFIED, MEMORY_MANAGED };
static int memory_model = MEMORY_UNCHOSEN;
// Whether kernels use host memory in place (the unified memory model)
// rather than copies of it in device memory (the copy model).
static int unified_memory = 0;

// The smallest allocation the managed memory model makes managed memory:
// making managed memory costs much more than malloc (the cache of freed blocks
// below spares a procedure that allocates at every call most of it), and
// copying a smaller range at a launch costs little. But a smaller array that
// the host looks at between kernels (a call of a procedure of the program,
// which the compiler cannot look into) is copied back and forth around every
// kernel, and a decode step full of such arrays was idle on the device for a
// third of its time. So an array of 4 KB and more is managed memory; the
// allocations of less than 1 MiB prefer the device (see managed_alloc).
enum { MANAGED_MIN_BYTES = 4096 };

// The managed allocations, sorted by address.
typedef struct managed_range {
    char *base;
    size_t size;
    // No launch has used the allocation yet.
    int fresh;
} managed_range;
static managed_range *managed = NULL;
static size_t n_managed = 0, managed_capacity = 0;

// The allocations are made and freed by host code, which may run on several
// threads at once.
#if defined(__GNUC__)
static volatile int managed_lock_word = 0;
static void managed_lock(void) {
    while (__sync_lock_test_and_set(&managed_lock_word, 1)) {}
}
static void managed_unlock(void) {
    __sync_lock_release(&managed_lock_word);
}
#else
static void managed_lock(void) {}
static void managed_unlock(void) {}
#endif

// The index of the first managed allocation that starts after `p`.
static size_t managed_upper(const void *p) {
    size_t lo = 0, hi = n_managed;
    while (lo < hi) {
        size_t mid = lo + (hi - lo)/2;
        if ((const char *)p < managed[mid].base) hi = mid; else lo = mid + 1;
    }
    return lo;
}

// Whether the `size` bytes at `p` lie inside one managed allocation.
static int managed_contains(const void *p, size_t size) {
    if (n_managed == 0 || size == 0) return 0;
    managed_lock();
    size_t i = managed_upper(p);
    int inside = i > 0 && (const char *)p + size
        <= managed[i - 1].base + managed[i - 1].size;
    managed_unlock();
    return inside;
}

static int managed_record(void *p, size_t size, int fresh) {
    managed_lock();
    if (n_managed == managed_capacity) {
        size_t capacity = managed_capacity ? 2*managed_capacity : 64;
        managed_range *grown = (managed_range *)realloc(managed,
            capacity*sizeof(managed_range));
        if (!grown) {
            managed_unlock();
            return 0;
        }
        managed = grown;
        managed_capacity = capacity;
    }
    size_t i = managed_upper(p);
    memmove(&managed[i + 1], &managed[i], (n_managed - i)*sizeof(managed_range));
    managed[i].base = (char *)p;
    managed[i].size = size;
    managed[i].fresh = fresh;
    n_managed++;
    managed_unlock();
    return 1;
}

// Forgets the managed allocation that starts at `p` and returns its size, or
// returns 0 if there is none.
static size_t managed_forget(void *p) {
    if (n_managed == 0 || !p) return 0;
    managed_lock();
    size_t i = managed_upper(p);
    size_t size = 0;
    if (i > 0 && managed[i - 1].base == (char *)p) {
        size = managed[i - 1].size;
        memmove(&managed[i - 1], &managed[i],
            (n_managed - i)*sizeof(managed_range));
        n_managed--;
    }
    managed_unlock();
    return size;
}

// Whether work was issued to the device since the last time the host waited
// for it: a launch, an asynchronous copy or a prefetch. The host waits for the
// device only then, however often the program asks it to sync.
static int device_busy = 0;

// Prefetches the `size` bytes of managed memory at `p` to `device`, on the
// default stream. CUDA 13 names the destination by a location rather than a
// device number; HIP keeps the device number.
static cudaError_t prefetch_to_device(const void *p, size_t size, int device) {
#if defined(CUDART_VERSION) && CUDART_VERSION >= 13000
    struct cudaMemLocation location;
    memset(&location, 0, sizeof(location));
    location.type = cudaMemLocationTypeDevice;
    location.id = device;
    return cudaMemPrefetchAsync(p, size, location, 0, 0);
#else
    return cudaMemPrefetchAsync(p, size, device, 0);
#endif
}

// The allocations of less than this many bytes are not prefetched but have
// their pages given a home on the device.
enum { MANAGED_PREFETCH_MIN_BYTES = 1 << 20 };

// Makes the `size` bytes of managed memory at `p` live on `device`, and maps
// them for the host: the pages a kernel writes stay on the device while the
// host reads a few of them (the arrays it checks or samples), which are then
// read in place across the bus instead of moved to the memory of the host
// and faulted back by the next kernel. Without this, the small arrays of a
// program that the host looks at between kernels were migrated back and forth,
// and now and then a step of a program took 100 to 600 milliseconds instead of
// 44.
static void prefer_device(const void *p, size_t size, int device) {
#if defined(LFORTRAN_GPU_HIP)
    (void)p; (void)size; (void)device;
#elif defined(CUDART_VERSION) && CUDART_VERSION >= 13000
    struct cudaMemLocation on_device, on_host;
    memset(&on_device, 0, sizeof(on_device));
    memset(&on_host, 0, sizeof(on_host));
    on_device.type = cudaMemLocationTypeDevice;
    on_device.id = device;
    on_host.type = cudaMemLocationTypeHost;
    (void)cudaMemAdvise(p, size, cudaMemAdviseSetPreferredLocation, on_device);
    (void)cudaMemAdvise(p, size, cudaMemAdviseSetAccessedBy, on_host);
#elif defined(CUDART_VERSION)
    (void)cudaMemAdvise(p, size, cudaMemAdviseSetPreferredLocation, device);
    (void)cudaMemAdvise(p, size, cudaMemAdviseSetAccessedBy, cudaCpuDeviceId);
#else
    (void)p; (void)size; (void)device;
#endif
}

// A launch is about to use the `size` bytes at `p`, inside a managed
// allocation. The first launch that uses an allocation moves all of it to the
// device in one go, which is several times faster than the page faults of
// the kernel would (the host has usually just filled it). Later launches
// leave the pages where they are: the host touches few of them in between,
// and prefetching a range whose pages are on the device already costs more
// than the faults it saves.
static void managed_to_device(void *p, size_t size) {
    managed_lock();
    size_t i = managed_upper(p);
    int fresh = i > 0 && managed[i - 1].fresh;
    char *base = fresh ? managed[i - 1].base : NULL;
    size_t length = fresh ? managed[i - 1].size : 0;
    if (fresh) managed[i - 1].fresh = 0;
    managed_unlock();
    if (!fresh) return;
    int device = 0;
    check(cudaGetDevice(&device), "cudaGetDevice");
    check(prefetch_to_device(base, length, device), "cudaMemPrefetchAsync");
    device_busy = 1;
    (void)size;
}


// Fills the `length` bytes at `dst`, the start of or inside a managed
// allocation no launch has used yet, with `fill`, and puts them on the device.
// Filling such an array from the host makes the driver map every page of it on
// the host first (about 20 GB/s) and the first launch then move it all to the
// device. Here the allocation is placed on the device first, and `fill`
// (which reads at most `n` bytes of the data at `offset` into `buffer`, and
// returns the bytes it read) reads into page-locked buffers that are copied to
// the device while the next one is read: about 35 GB/s, and the array is on the
// device when the call returns. Returns the bytes filled, or -1 and nothing
// done when the memory is not such an array or the buffers cannot be made.
typedef int64_t (*lfortran_gpu_fill_t)(void *context, char *buffer,
    int64_t n, int64_t offset);

enum { STAGE_BUFFERS = 4, STAGE_BYTES = 32 << 20 };
static char *stage_buffer[STAGE_BUFFERS];
static cudaEvent_t stage_event[STAGE_BUFFERS];
static volatile int stage_lock_word = 0;
#if defined(__GNUC__)
static void stage_lock(void) {
    while (__sync_lock_test_and_set(&stage_lock_word, 1)) {}
}
static void stage_unlock(void) {
    __sync_lock_release(&stage_lock_word);
}
#else
static void stage_lock(void) {}
static void stage_unlock(void) {}
#endif

#ifdef __cplusplus
extern "C"
#endif
int64_t lfortran_gpu_fill_managed(void *dst, int64_t length,
        lfortran_gpu_fill_t fill, void *context) {
    if (n_managed == 0 || length <= 0) return -1;
    stage_lock();
    int64_t result = -1, done = 0;
    int chunks = 0, device = 0, usable = 0;
    size_t i = 0, size = 0;
    char *base = NULL;
    if (!stage_buffer[0]) {
        for (int i = 0; i < STAGE_BUFFERS; i++) {
            if (cudaMallocHost((void **)&stage_buffer[i], STAGE_BYTES)
                    != cudaSuccess || cudaEventCreate(&stage_event[i])
                    != cudaSuccess) {
                for (int j = 0; j <= i; j++) {
                    if (stage_buffer[j]) cudaFreeHost(stage_buffer[j]);
                    stage_buffer[j] = NULL;
                }
                goto done;
            }
        }
    }
    managed_lock();
    i = managed_upper(dst);
    usable = i > 0 && managed[i - 1].fresh
        && (char *)dst + length <= managed[i - 1].base + managed[i - 1].size;
    base = usable ? managed[i - 1].base : NULL;
    size = usable ? managed[i - 1].size : 0;
    if (usable) managed[i - 1].fresh = 0;
    managed_unlock();
    if (!usable) goto done;
    check(cudaGetDevice(&device), "cudaGetDevice");
    check(prefetch_to_device(base, size, device), "cudaMemPrefetchAsync");
    device_busy = 1;
    while (done < length) {
        int b = chunks % STAGE_BUFFERS;
        // The copy that used the buffer last is finished before it is reused.
        if (chunks >= STAGE_BUFFERS) {
            check(cudaEventSynchronize(stage_event[b]), "cudaEventSynchronize");
        }
        int64_t n = length - done;
        if (n > STAGE_BYTES) n = STAGE_BYTES;
        int64_t got = fill(context, stage_buffer[b], n, done);
        if (got <= 0) break;
        check(cudaMemcpyAsync((char *)dst + done, stage_buffer[b], (size_t)got,
            cudaMemcpyHostToDevice, 0), "cudaMemcpyAsync H2D");
        check(cudaEventRecord(stage_event[b], 0), "cudaEventRecord");
        done += got;
        chunks++;
        if (got < n) break;
    }
    // Every copy is finished before the buffers are used again or the caller
    // looks at the data.
    for (int j = 0; j < STAGE_BUFFERS && j < chunks; j++) {
        check(cudaEventSynchronize(stage_event[j]), "cudaEventSynchronize");
    }
    result = done;
done:
    stage_unlock();
    return result;
}

static size_t managed_size(void *p) {
    if (n_managed == 0 || !p) return 0;
    managed_lock();
    size_t i = managed_upper(p);
    size_t size = i > 0 && managed[i - 1].base == (char *)p
        ? managed[i - 1].size : 0;
    managed_unlock();
    return size;
}

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
// The waits and syncs so far: the host may change what launches read only
// between them.
static uint64_t n_waits = 0;

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

// How many times the host waited for the device: a freed block is reused once
// the host has waited since it was freed.
static uint64_t sync_epoch = 0;

static void device_synchronize(void) {
    if (!device_busy) return;
    device_busy = 0;
    check(cudaDeviceSynchronize(), "cudaDeviceSynchronize");
    sync_epoch++;
}

// Device memory comes from a cache of freed blocks, since cudaMalloc and
// cudaFree cost tens of microseconds and the device heap is rebuilt at every
// call of a procedure that has a temporary array. A block is rounded up to a
// size class (four to each power of two) and returned to its class when it is
// freed; it goes back to the driver only when the cache is full, when it is
// too large to cache, or when the driver is out of memory.
enum { POOL_MIN_BYTES = 512, POOL_CLASSES = 128 };
static const size_t POOL_MAX_BLOCK_BYTES = (size_t)256 << 20;
static const size_t POOL_MAX_CACHED_BYTES = (size_t)2048 << 20;

typedef struct device_block {
    void *ptr;
    size_t size;
} device_block;

// The blocks the driver handed out and that are not freed yet, in a hash table
// (open addressing, keyed by address): the size of a block is not known when
// it is freed.
static device_block *live_blocks = NULL;
static size_t live_capacity = 0;
static size_t n_live = 0;
static device_block free_blocks[POOL_CLASSES][1024];
static int n_free_blocks[POOL_CLASSES];
static size_t cached_bytes = 0;

static size_t block_slot(const void *p, size_t capacity) {
    uintptr_t a = (uintptr_t)p;
    a ^= a >> 17;
    a *= (uintptr_t)0x9E3779B97F4A7C15ull;
    return (size_t)(a >> 7) & (capacity - 1);
}

static void live_insert(void *p, size_t size) {
    if ((n_live + 1) * 2 > live_capacity) {
        size_t capacity = live_capacity ? 2 * live_capacity : 1024;
        device_block *grown = (device_block*)calloc(capacity,
            sizeof(device_block));
        if (!grown) {
            fprintf(stderr, "lfortran_gpu: out of memory\n");
            exit(1);
        }
        for (size_t i = 0; i < live_capacity; i++) {
            if (!live_blocks[i].ptr) continue;
            size_t j = block_slot(live_blocks[i].ptr, capacity);
            while (grown[j].ptr) j = (j + 1) & (capacity - 1);
            grown[j] = live_blocks[i];
        }
        free(live_blocks);
        live_blocks = grown;
        live_capacity = capacity;
    }
    size_t j = block_slot(p, live_capacity);
    while (live_blocks[j].ptr) j = (j + 1) & (live_capacity - 1);
    live_blocks[j].ptr = p;
    live_blocks[j].size = size;
    n_live++;
}

// Removes the block at `p` from the table and returns its size, or 0.
static size_t live_remove(void *p) {
    if (!live_capacity) return 0;
    size_t j = block_slot(p, live_capacity);
    while (live_blocks[j].ptr) {
        if (live_blocks[j].ptr == p) {
            size_t size = live_blocks[j].size;
            live_blocks[j].ptr = NULL;
            n_live--;
            // Shift back the entries that probed past the hole.
            size_t hole = j;
            for (size_t k = (j + 1) & (live_capacity - 1); live_blocks[k].ptr;
                    k = (k + 1) & (live_capacity - 1)) {
                size_t home = block_slot(live_blocks[k].ptr, live_capacity);
                if (((k - home) & (live_capacity - 1))
                        >= ((k - hole) & (live_capacity - 1))) {
                    live_blocks[hole] = live_blocks[k];
                    live_blocks[k].ptr = NULL;
                    hole = k;
                }
            }
            return size;
        }
        j = (j + 1) & (live_capacity - 1);
    }
    return 0;
}

// The size class of `size` and the size of the blocks in it.
static int pool_class(size_t size, size_t *block_size) {
    size_t n = POOL_MIN_BYTES;
    int c = 0;
    while (n < size) {
        // Four classes to each power of two: n, 5n/4, 3n/2, 7n/4.
        size_t step = n / 4 ? n / 4 : 1;
        for (int i = 0; i < 4 && n < size; i++) { n += step; c++; }
        if (n < size && n > ((size_t)1 << 62)) break;
    }
    *block_size = n;
    return c;
}

static void pool_flush(void) {
    for (int c = 0; c < POOL_CLASSES; c++) {
        for (int i = 0; i < n_free_blocks[c]; i++) {
            (void)cudaFree(free_blocks[c][i].ptr);
        }
        n_free_blocks[c] = 0;
    }
    cached_bytes = 0;
}

static void* device_alloc(size_t size) {
    if (size == 0) size = 1;
    size_t block_size = size;
    int c = -1;
    if (size <= POOL_MAX_BLOCK_BYTES) {
        c = pool_class(size, &block_size);
        if (c >= POOL_CLASSES) c = -1;
    }
    if (c >= 0 && n_free_blocks[c] > 0) {
        device_block b = free_blocks[c][--n_free_blocks[c]];
        cached_bytes -= b.size;
        live_insert(b.ptr, b.size);
        return b.ptr;
    }
    void *d_ptr = NULL;
    cudaError_t err = cudaMalloc(&d_ptr, block_size);
    if (err != cudaSuccess && cached_bytes > 0) {
        pool_flush();
        err = cudaMalloc(&d_ptr, block_size);
    }
    check(err, "cudaMalloc");
    live_insert(d_ptr, block_size);
    return d_ptr;
}

// Gives the block at `d_ptr` back: to the cache, or to the driver.
static void device_release(void *d_ptr) {
    size_t block_size = live_remove(d_ptr);
    if (block_size == 0) {
        check(cudaFree(d_ptr), "cudaFree");
        return;
    }
    size_t unused;
    int c = block_size <= POOL_MAX_BLOCK_BYTES
        ? pool_class(block_size, &unused) : -1;
    if (c < 0 || c >= POOL_CLASSES || n_free_blocks[c] == 1024
            || cached_bytes + block_size > POOL_MAX_CACHED_BYTES) {
        check(cudaFree(d_ptr), "cudaFree");
        return;
    }
    free_blocks[c][n_free_blocks[c]].ptr = d_ptr;
    free_blocks[c][n_free_blocks[c]].size = block_size;
    n_free_blocks[c]++;
    cached_bytes += block_size;
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

static void end_wait_period(void);

static void sync_all(void) {
    n_syncs++;
    pending_in_place_reads = 0;
    device_synchronize();
    end_wait_period();
    for (int i = 0; i < n_entries; i++) {
        if (!entries[i].used || !entries[i].dirty) continue;
        if (!entries[i].in_place) {
            check(cudaMemcpy(entries[i].host, entries[i].device,
                entries[i].size, cudaMemcpyDeviceToHost), "cudaMemcpy D2H");
        }
        entries[i].dirty = 0;
    }
    for (int i = 0; i < n_deferred; i++) {
        device_release(deferred_free[i]);
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
        entries[i].fresh = 0;
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
    entries[i].last_use = n_waits;
    return &entries[i];
}

// After a wait the host may change any range but those it was told to keep
// (lfortran_gpu_keep), so no other copy is fresh. An entry unused in the
// last two wait periods that holds nothing the host lacks is forgotten: the
// program may have freed its host range.
static void end_wait_period(void) {
    for (int i = 0; i < n_entries; i++) {
        resident *e = &entries[i];
        if (!e->used) continue;
        if (!e->keep) e->fresh = 0;
        e->keep = 0;
        if (!e->dirty && e->last_use + 1 < n_waits) {
            remove_entry(i);
        }
    }
    n_waits++;
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
        e->fresh = 0;
    }
    if (!discard && !e->fresh) {
        check(cudaMemcpyAsync(e->device, host, size, cudaMemcpyHostToDevice,
            0), "cudaMemcpyAsync H2D");
        device_busy = 1;
    }
    e->dirty = 1;
    e->fresh = 0;
    return e->device;
}

// In the copy model: device memory that holds the host range, which a
// launch only reads, copied in unless the copy made since the last wait is
// there already.
static void* kept_read(void *host, size_t size) {
    resident *e = entry_for(host, size);
    if (!e->device || e->capacity < size) {
        free_later(e->device);
        e->device = device_alloc(size);
        e->capacity = size;
        e->fresh = 0;
    }
    if (!e->fresh) {
        check(cudaMemcpyAsync(e->device, host, size, cudaMemcpyHostToDevice,
            0), "cudaMemcpyAsync H2D");
        device_busy = 1;
        e->fresh = 1;
    }
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
static int choose_memory_model(int device) {
    int integrated = 0, pageable = 0, concurrent = 0;
    if (cudaDeviceGetAttribute(&integrated, cudaDevAttrIntegrated, device)
            != cudaSuccess) {
        integrated = 0;
    }
    if (cudaDeviceGetAttribute(&pageable, cudaDevAttrPageableMemoryAccess,
            device) != cudaSuccess) {
        pageable = 0;
    }
    if (cudaDeviceGetAttribute(&concurrent,
            cudaDevAttrConcurrentManagedAccess, device) != cudaSuccess) {
        concurrent = 0;
    }
    const char *value = getenv("LFORTRAN_GPU_MEMORY");
    int model;
    if (value == NULL || value[0] == '\0' || strcmp(value, "auto") == 0) {
        if (integrated && pageable) {
            model = MEMORY_UNIFIED;
        } else {
#ifdef LFORTRAN_GPU_HIP
            // With XNACK on, managed memory on a separate AMD GPU moves pages
            // on demand, but its speed varied from 5 to 1,380 GB/s between
            // identical runs of a STREAM triad on an MI210; the copy model
            // runs it at about 1 TB/s. With XNACK off a kernel reads the
            // managed pages the host touched from host memory, at 7 GB/s.
            model = MEMORY_COPY;
#else
            model = concurrent ? MEMORY_MANAGED : MEMORY_COPY;
#endif
        }
    } else if (strcmp(value, "copy") == 0) {
        model = MEMORY_COPY;
    } else if (strcmp(value, "unified") == 0) {
        if (!pageable) {
            fprintf(stderr, "LFORTRAN_GPU_MEMORY=unified: the GPU cannot "
                "access host memory (on an AMD GPU, XNACK must be on: "
                "HSA_XNACK=1)\n");
            exit(1);
        }
        model = MEMORY_UNIFIED;
    } else if (strcmp(value, "managed") == 0) {
        model = MEMORY_MANAGED;
    } else {
        fprintf(stderr, "LFORTRAN_GPU_MEMORY=%s is not a memory model "
            "(copy, unified, managed or auto)\n", value);
        exit(1);
    }
    const char *verbose = getenv("LFORTRAN_GPU_VERBOSE");
    if (verbose != NULL && strcmp(verbose, "1") == 0) {
        static const char *names[] = {"copy", "unified", "managed"};
        fprintf(stderr, "lfortran_gpu: %s memory model (the GPU is %s the "
            "host, %s access its pageable memory and %s managed memory "
            "concurrently)\n", names[model],
            integrated ? "integrated with" : "separate from",
            pageable ? "can" : "cannot", concurrent ? "can access" :
            "cannot access");
    }
#ifdef LFORTRAN_GPU_HIP
    // An APU waits by blocking (lfortran_gpu_hip_block_while_waiting).
    if (model == MEMORY_UNIFIED) lfortran_gpu_hip_block_while_waiting();
#endif
    return model;
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

// The memory model, chosen on first use. Without a device it is the copy
// model, and lfortran_gpu_init reports the error.
static int current_memory_model(void) {
    if (memory_model == MEMORY_UNCHOSEN) {
        if (cudaSetDevice(0) != cudaSuccess) {
            return MEMORY_COPY;
        }
        memory_model = choose_memory_model(0);
        unified_memory = memory_model == MEMORY_UNIFIED;
        if (unified_memory) keep_freed_memory_mapped();
    }
    return memory_model;
}

// The allocator of the program (lfortran_allocator_t in
// lfortran_intrinsics.h, which the GPU runtime does not include: a program
// links both).
typedef struct host_allocator {
    void* (*alloc)(void* context, int64_t size);
    void* (*realloc_func)(void* context, void* ptr, int64_t new_size);
    void  (*dealloc)(void* context, void* ptr);
    void* context;
} host_allocator;

// The allocator the managed one replaced, which makes every allocation that
// is not managed.
static host_allocator previous_allocator;

// Managed memory comes from a cache of freed blocks too: cudaMallocManaged
// and cudaFree cost tens of microseconds, and a procedure with an automatic
// array (or one that allocates a temporary) pays them at every call, with the
// device heap rebuilt each time. A block is rounded up to a size class (the
// classes of the device memory cache) and returned to its class when it is
// freed. It goes back to the driver only when its class is full, when the
// cache holds too much, when it is too large to cache, or when the driver is
// out of memory. The recorded size of a block is the size of the whole block.
// A launch that is still pending may use a block when it is freed, so a block
// is handed out again only after the host has waited for the device since (or
// when the device has no work pending): the free does not wait for the device,
// and the host runs ahead of the device through the launches of a procedure
// that allocates temporaries.
typedef struct managed_free_block {
    void *ptr;
    size_t size;
    uint64_t epoch;
} managed_free_block;
enum { MANAGED_POOL_PER_CLASS = 1024 };
static managed_free_block managed_free_blocks[POOL_CLASSES][MANAGED_POOL_PER_CLASS];
static int n_managed_free_blocks[POOL_CLASSES];
static size_t managed_cached_bytes = 0;
static const size_t MANAGED_MAX_BLOCK_BYTES = (size_t)256 << 20;
static const size_t MANAGED_MAX_CACHED_BYTES = (size_t)1024 << 20;

// Gives every cached block back to the driver.
static void managed_cache_flush(void) {
    device_synchronize();
    managed_lock();
    for (int c = 0; c < POOL_CLASSES; c++) {
        for (int i = 0; i < n_managed_free_blocks[c]; i++) {
            (void)cudaFree(managed_free_blocks[c][i].ptr);
        }
        n_managed_free_blocks[c] = 0;
    }
    managed_cached_bytes = 0;
    managed_unlock();
}

static void* managed_alloc(void *context, int64_t size) {
    if (size >= MANAGED_MIN_BYTES
            && current_memory_model() == MEMORY_MANAGED) {
        size_t block_size = (size_t)size;
        int c = -1;
        if (block_size <= MANAGED_MAX_BLOCK_BYTES) {
            c = pool_class(block_size, &block_size);
            if (c >= POOL_CLASSES) { c = -1; block_size = (size_t)size; }
        }
        void *p = NULL;
        if (c >= 0) {
            managed_lock();
            for (int i = n_managed_free_blocks[c]; i-- > 0;) {
                managed_free_block *b = &managed_free_blocks[c][i];
                if (b->epoch < sync_epoch || !device_busy) {
                    p = b->ptr;
                    *b = managed_free_blocks[c][--n_managed_free_blocks[c]];
                    managed_cached_bytes -= block_size;
                    break;
                }
            }
            managed_unlock();
            if (p) {
                if (managed_record(p, block_size,
                        block_size >= MANAGED_PREFETCH_MIN_BYTES)) return p;
                (void)cudaFree(p);
                p = NULL;
            }
        }
        cudaError_t err = cudaMallocManaged(&p, block_size,
            cudaMemAttachGlobal);
        if (err != cudaSuccess && managed_cached_bytes > 0) {
            managed_cache_flush();
            err = cudaMallocManaged(&p, block_size, cudaMemAttachGlobal);
        }
        if (err == cudaSuccess && p) {
            int large = block_size >= MANAGED_PREFETCH_MIN_BYTES;
            if (!large) prefer_device(p, block_size, 0);
            if (managed_record(p, block_size, large)) return p;
            (void)cudaFree(p);
        }
    }
    return previous_allocator.alloc(context, size);
}

static void managed_dealloc(void *context, void *ptr) {
    size_t block_size = managed_forget(ptr);
    if (block_size) {
        size_t unused = 0;
        int c = block_size <= MANAGED_MAX_BLOCK_BYTES
            ? pool_class(block_size, &unused) : -1;
        if (c >= 0 && c < POOL_CLASSES && unused == block_size) {
            int cached = 0;
            managed_lock();
            if (n_managed_free_blocks[c] < MANAGED_POOL_PER_CLASS
                    && managed_cached_bytes + block_size
                        <= MANAGED_MAX_CACHED_BYTES) {
                managed_free_block *b
                    = &managed_free_blocks[c][n_managed_free_blocks[c]++];
                b->ptr = ptr;
                b->size = block_size;
                b->epoch = sync_epoch;
                managed_cached_bytes += block_size;
                cached = 1;
            }
            managed_unlock();
            if (cached) return;
        }
        // A pending launch may still use the memory.
        device_synchronize();
        (void)cudaFree(ptr);
        return;
    }
    previous_allocator.dealloc(context, ptr);
}

static void* managed_realloc(void *context, void *ptr, int64_t size) {
    size_t old = managed_size(ptr);
    if (!old) return previous_allocator.realloc_func(context, ptr, size);
    void *p = managed_alloc(context, size);
    if (!p) return NULL;
    memcpy(p, ptr, old < (size_t)size ? old : (size_t)size);
    managed_dealloc(context, ptr);
    return p;
}

#if defined(__GNUC__)
// The program's allocator, which a program links from the LFortran runtime
// library; a unit test of this file links none, so it is looked up rather
// than referenced. A Mach-O link does not accept an undefined weak
// reference, so on macOS the dynamic linker finds it.
#if defined(__APPLE__)
#include <dlfcn.h>
static host_allocator *program_allocator(void) {
    void *get = dlsym(RTLD_DEFAULT, "_lfortran_get_default_allocator");
    return get ? ((host_allocator *(*)(void))get)() : NULL;
}
#else
#ifdef __cplusplus
extern "C"
#endif
host_allocator *_lfortran_get_default_allocator(void) __attribute__((weak));
static host_allocator *program_allocator(void) {
    return _lfortran_get_default_allocator
        ? _lfortran_get_default_allocator() : NULL;
}
#endif

#ifdef __cplusplus
extern "C"
#endif
void _lfortran_set_gpu_fill(int64_t (*function)(void *, int64_t,
    int64_t (*)(void *, char *, int64_t, int64_t), void *))
    __attribute__((weak));

__attribute__((constructor)) static void install_managed_allocator(void) {
    host_allocator *a = program_allocator();
    if (!a || a->alloc == managed_alloc) return;
    if (_lfortran_set_gpu_fill) {
        _lfortran_set_gpu_fill(lfortran_gpu_fill_managed);
    }
    previous_allocator = *a;
    a->alloc = managed_alloc;
    a->realloc_func = managed_realloc;
    a->dealloc = managed_dealloc;
}
#endif

#ifdef LFORTRAN_GPU_HIP
// Before the program, and so before any HIP call (the HIP runtime starts at
// the first one): an APU such as the MI300A can then run in the unified
// model without the user setting HSA_XNACK=1.
__attribute__((constructor)) static void enable_xnack(void) {
    lfortran_gpu_hip_enable_xnack();
}
#endif

lfortran_gpu_ctx* lfortran_gpu_init(void) {
    static lfortran_gpu_ctx *singleton = NULL;
    if (singleton) return singleton;
    singleton = (lfortran_gpu_ctx*)calloc(1, sizeof(lfortran_gpu_ctx));
    singleton->device_id = 0;
    // Without a device every launch would be skipped and the program would
    // continue with the values its loops never computed.
    check(cudaSetDevice(0), "cudaSetDevice");
    current_memory_model();
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
    // The reset frees the managed allocations too.
    managed_lock();
    n_managed = 0;
    managed_unlock();
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
            if (managed_contains(k->buffer_host_ptrs[i], resident_sizes[i])) {
                managed_to_device(k->buffer_host_ptrs[i], resident_sizes[i]);
                k->buffer_device_ptrs[i] = k->buffer_host_ptrs[i];
                continue;
            }
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
            if (size > 0 && managed_contains(host, size)) {
                managed_to_device(host, size);
                d_ptr = host;
                reads_in_place = 1;
            } else if (size > 0) {
                d_ptr = dirty_resident(host, resident_sizes[i]);
                if (!d_ptr && unified_memory
                        && size > UNIFIED_COPY_MAX_BYTES) {
                    d_ptr = use_in_place(entry_for(host, size));
                    reads_in_place = 1;
                } else if (!d_ptr && !unified_memory
                        && size >= KEEP_READ_MIN_BYTES) {
                    d_ptr = kept_read(host, size);
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
        device_busy = 1;
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
                device_busy = 1;
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
    device_busy = 1;
    if (reads_in_place) pending_in_place_reads = 1;
}

void lfortran_gpu_sync(lfortran_gpu_ctx* ctx) {
    if (!ctx) return;
    sync_all();
}

void lfortran_gpu_wait(lfortran_gpu_ctx* ctx) {
    if (!ctx) return;
    end_wait_period();
    if (!pending_in_place_reads) return;
    pending_in_place_reads = 0;
    device_synchronize();
}

void lfortran_gpu_keep(lfortran_gpu_ctx* ctx, void* ptr, size_t size) {
    if (!ctx || !ptr || size == 0) return;
    const char *lo = (const char *)ptr, *hi = lo + size;
    for (int i = 0; i < n_entries; i++) {
        resident *e = &entries[i];
        const char *h = (const char *)e->host;
        if (e->used && e->fresh && !e->dirty && h >= lo
                && h + e->size <= hi) {
            e->keep = 1;
        }
    }
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
    // A pending launch may read the memory in place too.
    int read_in_place = 0;
    for (int i = 0; i < n_entries && pending_in_place_reads; i++) {
        if (entries[i].used && entries[i].in_place
                && overlaps(entries[i].host, entries[i].size, ptr, size)) {
            read_in_place = 1;
        }
    }
    if (read_in_place) lfortran_gpu_wait(ctx);
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
