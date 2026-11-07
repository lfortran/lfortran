#import <Metal/Metal.h>
#import <Foundation/Foundation.h>
#include "lfortran_gpu_runtime.h"
#include <dlfcn.h>
#include <malloc/malloc.h>
#include <mach/vm_page_size.h>
#include <os/lock.h>
#include <sched.h>
#include <stdatomic.h>
#include <mach/mach_time.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_ARGS 32
#define MAX_KERNELS 64

/*
Launches are asynchronous, and the device keeps what a kernel writes.

* A buffer argument the kernel may write (LFORTRAN_GPU_WRITE) is *resident*:
  it gets an entry in a table of host ranges, which owns a device buffer. The
  host bytes are copied in when the entry is not dirty, unless the kernel
  writes all of them first (LFORTRAN_GPU_WRITE_DISCARD), and after the launch
  the entry is dirty: the device holds the newest values. A later launch
  handed the same range binds the same buffer without copying anything, and
  lfortran_gpu_sync copies every dirty entry back and marks it clean. The
  compiler guarantees that the host does not touch a dirty range before then
  (lfortran_gpu_runtime.h).
* A buffer the kernel only reads (LFORTRAN_GPU_READ) is copied in when the
  launch is made, so the host may change it right after: into the encoder
  itself when it is small, into a pooled buffer otherwise. When it is a dirty
  range, the kernel reads the resident buffer instead. The kernel handle
  keeps the pooled buffer until the argument is set again or the handle is
  released, so a later launch of the handle that is not handed the argument
  again (the next chunk of a launch that runs in chunks) reads it without a
  copy.
* A range that overlaps a dirty one without being the same range makes the
  runtime sync first, so that the host bytes it copies are current.
* A device buffer is never written by the host while a launch that uses it
  may still run: a copy into a resident buffer still in use goes into a fresh
  buffer instead, and pooled buffers are reused only once the launches that
  used them have completed.
* Launches are encoded one after another into one compute encoder, and the
  command buffer is committed every
  LAUNCHES_PER_COMMIT launches and at a sync: encoding a dispatch costs a
  fraction of what committing a command buffer and its overhead on the GPU
  do. The encoder runs dispatches concurrently, with a barrier before a
  dispatch that uses a buffer an earlier one since the last barrier writes,
  or writes one it reads: so independent launches overlap, and the device
  runs the launches as if in order. At most MAX_PENDING_COMMITS committed
  command buffers are pending: a host that launches faster than the GPU
  runs waits for it there.

That is the copy memory model, the same as the CUDA runtime's
(lfortran_gpu_cuda.c). An Apple GPU shares the host's memory, but a kernel
can only use host memory that a Metal buffer covers, which must be whole
pages, and making a buffer over a large range is cheap while the first
launch that uses a new one maps its pages for the device (about 10 ms for
1.4 GB). So the Metal runtime has the managed memory model of the CUDA
runtime, a special case of the copy model: it replaces the program's
allocator (the default allocator of lfortran_intrinsics.c), so that an
allocation of at least MANAGED_MIN_BYTES, such as an allocatable array, is
made of whole pages (`managed`, sorted by address):

* A range inside such an allocation is used in place and has no entry: the
  first launch that uses the allocation makes a buffer over its pages
  without copying them, which the allocation keeps until it is freed, and
  the kernel is handed that buffer at the offset of the range. Nothing is
  copied at a launch, and lfortran_gpu_sync copies nothing back for it.
* The host does not change or free a range a pending launch reads before
  lfortran_gpu_wait (or lfortran_gpu_sync), which the compiler places where
  the host next may (gpu_launch_sync.cpp), so lfortran_gpu_wait waits for
  every launch when one that reads host memory in place may be pending. The
  host does not access a range a pending launch writes before the sync, as
  in the copy model.
* A range a launch only reads of at most MANAGED_COPY_READ_MAX bytes is
  still copied, so that the host may change it without waiting, unless a
  pending launch writes the allocation in place: the host bytes are not
  current then. A range a launch only reads that overlaps one it writes is
  copied too, so that the kernel reads it as the host holds it, as in the
  copy model; the runtime waits first for pending launches that write the
  allocation.
* Freeing an allocation waits for the launches that use it first.
* Smaller allocations, memory the allocator did not make (stack and static
  arrays) and an allocation the device cannot map keep the copy model.

The model is chosen at the first allocation the allocator could make
managed, or when the runtime starts, whichever is first:
LFORTRAN_GPU_MEMORY=copy or managed selects one, and auto (the default)
selects managed on a GPU that shares the host's memory (every Apple GPU) and
copy otherwise. LFORTRAN_GPU_VERBOSE=1 prints the model chosen.
*/

// A read-only argument up to this size is copied into the encoder.
static const size_t INLINE_BYTES_MAX = 4096;
// Buffers up to this size are kept in the pool for reuse; a larger one, such
// as the device heap, is released with the kernel handle that holds it.
static const size_t POOLED_BUFFER_MAX = (size_t)16 * 1024 * 1024;
enum { POOL_MAX = 128, MAX_RESIDENT = 1024, HASH_SIZE = 4096 };
// The smallest allocation the managed memory model makes of whole pages:
// copying a smaller range at a launch costs little.
enum { MANAGED_MIN_BYTES = 1 << 20 };
// In the managed memory model, a range a launch only reads is copied when it
// is at most this many bytes and no pending launch writes it.
static const size_t MANAGED_COPY_READ_MAX = 64 * 1024;
// Launches encoded into one command buffer before it is committed, and how
// many committed command buffers may be pending: a host that launches faster
// than the device runs waits for the oldest. Few pending launches keep the
// buffers they hold few, so that the pool can give them out again.
enum { LAUNCHES_PER_COMMIT = 64, MAX_PENDING_COMMITS = 3 };
// Buffers whose use since the last barrier is remembered.
enum { MAX_TRACKED = 256 };

typedef struct buffer_use {
    void *buffer;
    int write;
} buffer_use;

// Registry of the shader sources emitted for each kernel, and the pipeline
// compiled from each, once per run: a kernel is loaded at every launch, and
// compiling its source again every time is slow and exhausts the driver's
// cache of compiled shaders.
//
// Each kernel also keeps a scratch buffer per argument slot. The device runs
// the launches one after another, and what a launch leaves in a scratch
// buffer is dead once the launch statement ends (a part the host reads back
// is read before the next one is made), so every launch of the kernel can
// use the same one, however many of them are still pending. A launch that
// runs in chunks encodes each chunk with its one kernel handle, so its
// chunks share the buffer: what an earlier chunk leaves in it is live for
// the later chunks and for the host to read back after the last one.
//
// With profiling on, each kernel also counts its launches, the bytes they
// copy to the device and the host time they take until they are encoded;
// with LFORTRAN_GPU_PROFILE=2 also the GPU time of its launches.
static struct {
    const char *name;
    const char *source;
    id<MTLComputePipelineState> pipeline;
    id<MTLBuffer> scratch[MAX_ARGS];
    uint64_t launches;
    uint64_t h2d_bytes;
    double host_seconds;
    double gpu_seconds;
} source_registry[MAX_KERNELS];
static int n_registered = 0;

typedef struct resident {
    void *host;
    size_t size;
    id<MTLBuffer> buffer;
    // The serial of the last command buffer that uses `buffer`.
    uint64_t last_use;
    uint64_t lru;
    int dirty;
    int used;
} resident;

typedef struct pooled {
    id<MTLBuffer> buffer;
    size_t length;
    uint64_t last_use;
} pooled;

// LFORTRAN_GPU_PROFILE=1 collects the telemetry; =2 also commits every
// launch in a command buffer of its own, so that the GPU time of each
// command buffer is the time of one kernel. That runs the launches one after
// another, without the overlap of a concurrent encoder, and adds the cost of
// a commit to each. Either prints a report at exit, to stderr or to the file
// LFORTRAN_GPU_PROFILE_FILE names.
static int gpu_profile_level(void) {
    const char *value = getenv("LFORTRAN_GPU_PROFILE");
    if (value == NULL) return 0;
    if (strcmp(value, "1") == 0) return 1;
    if (strcmp(value, "2") == 0) return 2;
    return 0;
}

static double telemetry_now(void) {
    static mach_timebase_info_data_t info = {0, 0};
    if (info.denom == 0) mach_timebase_info(&info);
    uint64_t ticks = mach_absolute_time();
    double nanos = (double)ticks * (double)info.numer / (double)info.denom;
    return nanos * 1e-9;
}

struct lfortran_gpu_ctx {
    id<MTLDevice> device;
    id<MTLCommandQueue> queue;
    // The last command buffer committed and not yet waited for, and the
    // ones committed before it, by serial modulo MAX_PENDING_COMMITS.
    id<MTLCommandBuffer> last_command_buffer;
    id<MTLCommandBuffer> pending[MAX_PENDING_COMMITS];
    // The command buffer launches are encoded into, not committed yet, and
    // its encoder, with the number of launches in it.
    id<MTLCommandBuffer> open_command_buffer;
    id<MTLComputeCommandEncoder> encoder;
    int n_encoded;
    // Command buffers are numbered from 1 as they are committed; `completed`
    // is the highest number the device has finished.
    uint64_t serial;
    int profile_enabled;
    // LFORTRAN_GPU_PROFILE=2: one launch per command buffer, and the
    // registry entry of the kernel each pending one runs.
    int profile_kernels;
    int open_entry;
    int pending_entry[MAX_PENDING_COMMITS];
    _Atomic uint64_t completed;
    _Atomic uint64_t handled;
    _Atomic int failed;
    char error[512];
    struct {
        uint64_t applied_serial;
        uint64_t completed_command_buffers;
        uint64_t commits;
        uint64_t dispatches;
        uint64_t host_syncs;
        uint64_t host_waits;
        uint64_t memcpy_h2d_bytes;
        uint64_t memcpy_d2h_bytes;
        uint64_t buffer_allocations;
        uint64_t buffer_alloc_bytes;
        double gpu_elapsed_seconds;
        double gpu_busy_seconds;
        double gpu_busy_cursor;
        double jit_seconds;
        int gpu_timing_available;
    } telemetry;
    uint64_t lru_clock;
    // How many times the host has synced.
    uint64_t n_syncs;
    // The serial of the last command buffer with a launch that reads host
    // memory in place (the managed memory model).
    uint64_t in_place_read;
    // The device buffers the dispatches since the last barrier of the open
    // encoder read and write.
    void *reads[MAX_TRACKED];
    void *writes[MAX_TRACKED];
    int n_reads, n_writes;
};

static resident entries[MAX_RESIDENT];
static int n_entries = 0;
// Host address -> index + 1 into `entries`, by linear probing.
static int hash_slots[HASH_SIZE];
static int dirty_list[MAX_RESIDENT];
static int n_dirty = 0;
static pooled pool[POOL_MAX];
static int n_pool = 0;

enum { ARG_NONE = 0, ARG_BUFFER, ARG_SCALAR, ARG_SCRATCH };
// The bytes of the scalar arguments of a launch are kept in the kernel
// object up to this many, all of them together.
enum { SCALAR_BYTES = 1024 };

struct lfortran_gpu_kernel {
    id<MTLComputePipelineState> pipeline;
    lfortran_gpu_ctx *ctx;
    void *ptrs[MAX_ARGS];
    size_t sizes[MAX_ARGS];
    int access[MAX_ARGS];
    int kind[MAX_ARGS];
    unsigned char scalar_bytes[SCALAR_BYTES];
    size_t scalar_used;
    void *scalar_heap[MAX_ARGS];
    // The scratch buffer of the slot the last launch used, kept until the
    // kernel is released so that the host can read part of it back: the
    // chunks of one launch share it, as they share the device heap state
    // they are handed.
    id<MTLBuffer> scratch_buffers[MAX_ARGS];
    // The pooled copy of a read-only buffer argument the last launch read,
    // kept for the next launch until the argument is set again.
    id<MTLBuffer> read_buffers[MAX_ARGS];
    int n_args;
    uint64_t serial;
    // The kernel's entry in the registry.
    int entry;
};

static void fail(const char *what, size_t size) {
    fprintf(stderr, "lfortran_gpu_launch: cannot allocate a device buffer "
        "of %zu bytes (%s)\n", size, what);
    exit(1);
}

static int completed_by_device(lfortran_gpu_ctx *ctx, uint64_t serial) {
    return serial <= atomic_load_explicit(&ctx->completed,
        memory_order_acquire);
}

static void telemetry_collect_command_buffer(lfortran_gpu_ctx *ctx,
    uint64_t serial, id<MTLCommandBuffer> cb)
{
    if (!ctx->profile_enabled || !cb) return;
    if (ctx->telemetry.gpu_timing_available) {
        double start = [cb GPUStartTime];
        double end = [cb GPUEndTime];
        if (start <= 0.0 || end < start) {
            ctx->telemetry.gpu_timing_available = 0;
        } else {
            ctx->telemetry.gpu_elapsed_seconds += end - start;
            if (ctx->profile_kernels) {
                int entry = ctx->pending_entry[serial % MAX_PENDING_COMMITS];
                source_registry[entry].gpu_seconds += end - start;
            }
            double begin = start > ctx->telemetry.gpu_busy_cursor
                ? start : ctx->telemetry.gpu_busy_cursor;
            if (end > begin) {
                ctx->telemetry.gpu_busy_seconds += end - begin;
            }
            if (end > ctx->telemetry.gpu_busy_cursor) {
                ctx->telemetry.gpu_busy_cursor = end;
            }
        }
    }
    ctx->telemetry.completed_command_buffers++;
    ctx->telemetry.applied_serial = serial;
}

// ---------------------------------------------------------------- pool ---

static id<MTLBuffer> acquire_buffer(lfortran_gpu_ctx *ctx, size_t size) {
    int best = -1;
    for (int i = 0; i < n_pool; i++) {
        size_t length = pool[i].length;
        if (length < size || length > 4 * size + 4096) continue;
        if (!completed_by_device(ctx, pool[i].last_use)) continue;
        if (best < 0 || length < pool[best].length) best = i;
    }
    if (best >= 0) {
        id<MTLBuffer> buffer = pool[best].buffer;
        pool[best] = pool[n_pool - 1];
        pool[n_pool - 1].buffer = nil;
        n_pool--;
        return buffer;
    }
    size_t length = 256;
    while (length < size) length *= 2;
    if (length > POOLED_BUFFER_MAX) length = size;
    if (ctx->profile_enabled) {
        ctx->telemetry.buffer_allocations++;
        ctx->telemetry.buffer_alloc_bytes += length;
    }
    id<MTLBuffer> buffer = [ctx->device newBufferWithLength:length
        options:MTLResourceStorageModeShared];
    if (!buffer) fail("pool", length);
    return buffer;
}

// Gives `buffer` back for reuse once the command buffer `last_use` is done.
static void release_buffer(id<MTLBuffer> buffer, uint64_t last_use) {
    if (!buffer || [buffer length] > POOLED_BUFFER_MAX) return;
    if (n_pool == POOL_MAX) {
        // The buffer that was given back first goes.
        pool[0] = pool[n_pool - 1];
        pool[n_pool - 1].buffer = nil;
        n_pool--;
    }
    pool[n_pool].buffer = buffer;
    pool[n_pool].length = [buffer length];
    pool[n_pool].last_use = last_use;
    n_pool++;
}

// ------------------------------------------------------ resident table ---

static unsigned hash_of(void *host) {
    uintptr_t h = (uintptr_t)host;
    h ^= h >> 17;
    h *= 0x9E3779B97F4A7C15ull;
    return (unsigned)(h >> 40) & (HASH_SIZE - 1);
}

static int find_entry(void *host) {
    unsigned slot = hash_of(host);
    while (hash_slots[slot] != 0) {
        int i = hash_slots[slot] - 1;
        if (entries[i].host == host) return i;
        slot = (slot + 1) & (HASH_SIZE - 1);
    }
    return -1;
}

static void hash_remove(void *host) {
    unsigned slot = hash_of(host);
    while (hash_slots[slot] != 0
            && entries[hash_slots[slot] - 1].host != host) {
        slot = (slot + 1) & (HASH_SIZE - 1);
    }
    if (hash_slots[slot] == 0) return;
    hash_slots[slot] = 0;
    // Move back what probed past the freed slot.
    unsigned next = (slot + 1) & (HASH_SIZE - 1);
    while (hash_slots[next] != 0) {
        int i = hash_slots[next] - 1;
        hash_slots[next] = 0;
        unsigned home = hash_of(entries[i].host);
        while (hash_slots[home] != 0) home = (home + 1) & (HASH_SIZE - 1);
        hash_slots[home] = i + 1;
        next = (next + 1) & (HASH_SIZE - 1);
    }
}

static void set_clean(int i) {
    if (!entries[i].dirty) return;
    entries[i].dirty = 0;
    for (int d = 0; d < n_dirty; d++) {
        if (dirty_list[d] == i) {
            dirty_list[d] = dirty_list[--n_dirty];
            break;
        }
    }
}

static void set_dirty(int i) {
    if (entries[i].dirty) return;
    entries[i].dirty = 1;
    dirty_list[n_dirty++] = i;
}

static void remove_entry(int i) {
    set_clean(i);
    hash_remove(entries[i].host);
    release_buffer(entries[i].buffer, entries[i].last_use);
    entries[i].buffer = nil;
    entries[i].used = 0;
    entries[i].host = NULL;
    entries[i].size = 0;
}

static int overlaps(const void *a, size_t na, const void *b, size_t nb) {
    const char *pa = (const char *)a, *pb = (const char *)b;
    return na > 0 && nb > 0 && pa < pb + nb && pb < pa + na;
}

static void sync_all(lfortran_gpu_ctx *ctx);

// An entry for the host range, which may be a new one. The table holds
// MAX_RESIDENT ranges; past that, a clean entry used longest ago goes.
static int new_entry(lfortran_gpu_ctx *ctx, void *host, size_t size) {
    int i = -1;
    if (n_entries < MAX_RESIDENT) {
        i = n_entries++;
    } else {
        for (int j = 0; j < MAX_RESIDENT; j++) {
            if (!entries[j].used) { i = j; break; }
        }
        if (i < 0) {
            if (n_dirty == MAX_RESIDENT) sync_all(ctx);
            for (int j = 0; j < MAX_RESIDENT; j++) {
                if (entries[j].dirty) continue;
                if (i < 0 || entries[j].lru < entries[i].lru) i = j;
            }
            remove_entry(i);
        }
    }
    entries[i].host = host;
    entries[i].size = size;
    entries[i].buffer = nil;
    entries[i].last_use = 0;
    entries[i].dirty = 0;
    entries[i].used = 1;
    unsigned slot = hash_of(host);
    while (hash_slots[slot] != 0) slot = (slot + 1) & (HASH_SIZE - 1);
    hash_slots[slot] = i + 1;
    return i;
}

// The resident buffer of a range the kernel may write, with the current host
// bytes in it unless the device holds newer ones.
// `discard`: the kernel writes all of the range before it reads it, so the
// host bytes are not copied in.
static id<MTLBuffer> resident_for_write(lfortran_gpu_ctx *ctx, void *host,
    size_t size, int discard)
{
    int i = find_entry(host);
    if (i >= 0 && entries[i].size != size) {
        // The same address, another size. A dirty range that overlaps was
        // synced before the launch was set up, so a dirty entry here is one
        // this launch is handed in another size as well.
        if (entries[i].dirty) sync_all(ctx);
        entries[i].size = size;
    }
    if (i < 0) i = new_entry(ctx, host, size);
    resident *e = &entries[i];
    e->lru = ++ctx->lru_clock;
    if (e->dirty) return e->buffer;
    if (!e->buffer || [e->buffer length] < size
            || !completed_by_device(ctx, e->last_use)) {
        id<MTLBuffer> old = e->buffer;
        e->buffer = acquire_buffer(ctx, size);
        release_buffer(old, e->last_use);
    }
    if (!discard) {
        if (ctx->profile_enabled) {
            ctx->telemetry.memcpy_h2d_bytes += size;
        }
        memcpy([e->buffer contents], host, size);
    }
    set_dirty(i);
    return e->buffer;
}

// The dirty resident buffer holding exactly this range, or nil.
static id<MTLBuffer> dirty_resident(void *host, size_t size) {
    int i = find_entry(host);
    if (i < 0 || !entries[i].dirty || entries[i].size != size) return nil;
    return entries[i].buffer;
}

// Whether a range overlaps a dirty one without being the same range.
static int overlaps_dirty(void *host, size_t size) {
    for (int d = 0; d < n_dirty; d++) {
        resident *e = &entries[dirty_list[d]];
        if (e->host == host && e->size == size) continue;
        if (overlaps(e->host, e->size, host, size)) return 1;
    }
    return 0;
}

// -------------------------------------------------- managed memory ---

enum { MEMORY_UNCHOSEN = -1, MEMORY_COPY, MEMORY_MANAGED };
static int memory_model = MEMORY_UNCHOSEN;

// The context of the program, once lfortran_gpu_init has made it.
static lfortran_gpu_ctx *singleton = NULL;

static void wait_all(lfortran_gpu_ctx *ctx);

// An allocation made of whole pages (the managed memory model).
typedef struct managed_range {
    char *base;
    // The bytes the program asked for, and the whole pages they take.
    size_t size;
    size_t length;
    // The id<MTLBuffer> over the pages, retained, from the first launch
    // that uses the allocation on; NULL before.
    void *buffer;
    // The serials of the last command buffers that use the allocation and
    // that write it.
    uint64_t last_use;
    uint64_t last_write;
    // The device cannot make a buffer over it: the copy model.
    int unmapped;
} managed_range;

static managed_range *managed = NULL;
static size_t n_managed = 0, managed_capacity = 0;
// Threads of an OpenMP program allocate concurrently.
static os_unfair_lock managed_lock = OS_UNFAIR_LOCK_INIT;

static int memory_model_of_device(void) {
#if defined(__arm64__) || defined(__aarch64__)
    // Every GPU of an Apple silicon Mac shares the host's memory.
    return MEMORY_MANAGED;
#else
    id<MTLDevice> device = MTLCreateSystemDefaultDevice();
    return device && [device hasUnifiedMemory] ? MEMORY_MANAGED
        : MEMORY_COPY;
#endif
}

// The memory model, chosen on first use (with managed_lock held, or before
// any thread is started).
static int current_memory_model(void) {
    if (memory_model != MEMORY_UNCHOSEN) return memory_model;
    const char *value = getenv("LFORTRAN_GPU_MEMORY");
    int model;
    if (value == NULL || value[0] == '\0' || strcmp(value, "auto") == 0) {
        model = memory_model_of_device();
    } else if (strcmp(value, "copy") == 0) {
        model = MEMORY_COPY;
    } else if (strcmp(value, "managed") == 0) {
        model = MEMORY_MANAGED;
    } else {
        fprintf(stderr, "LFORTRAN_GPU_MEMORY=%s is not a memory model of "
            "the Metal runtime (copy, managed or auto)\n", value);
        exit(1);
    }
    const char *verbose = getenv("LFORTRAN_GPU_VERBOSE");
    if (verbose != NULL && strcmp(verbose, "1") == 0) {
        fprintf(stderr, "lfortran_gpu: %s memory model\n",
            model == MEMORY_MANAGED ? "managed" : "copy");
    }
    memory_model = model;
    return model;
}

// The index of the first managed allocation that starts after `p`.
static size_t managed_upper(const void *p) {
    size_t lo = 0, hi = n_managed;
    while (lo < hi) {
        size_t mid = lo + (hi - lo)/2;
        if ((const char *)p < managed[mid].base) hi = mid; else lo = mid + 1;
    }
    return lo;
}

// The managed allocation that holds the `size` bytes at `p`, or NULL.
static managed_range *managed_holding(const void *p, size_t size) {
    size_t i = managed_upper(p);
    if (i == 0) return NULL;
    managed_range *r = &managed[i - 1];
    if ((const char *)p + size > r->base + r->size) return NULL;
    return r;
}

static int managed_record(void *p, size_t size, size_t length) {
    os_unfair_lock_lock(&managed_lock);
    if (n_managed == managed_capacity) {
        size_t capacity = managed_capacity ? 2*managed_capacity : 64;
        managed_range *grown = (managed_range *)realloc(managed,
            capacity*sizeof(managed_range));
        if (!grown) {
            os_unfair_lock_unlock(&managed_lock);
            return 0;
        }
        managed = grown;
        managed_capacity = capacity;
    }
    size_t i = managed_upper(p);
    memmove(&managed[i + 1], &managed[i],
        (n_managed - i)*sizeof(managed_range));
    managed_range r = {(char *)p, size, length, NULL, 0, 0, 0};
    managed[i] = r;
    n_managed++;
    os_unfair_lock_unlock(&managed_lock);
    return 1;
}

// Removes the managed allocation at `p` from the table into `out`; 0 when
// there is none.
static int managed_forget(void *p, managed_range *out) {
    os_unfair_lock_lock(&managed_lock);
    size_t i = managed_upper(p);
    int found = i > 0 && managed[i - 1].base == (char *)p;
    if (found) {
        *out = managed[i - 1];
        memmove(&managed[i - 1], &managed[i],
            (n_managed - i)*sizeof(managed_range));
        n_managed--;
    }
    os_unfair_lock_unlock(&managed_lock);
    return found;
}

// The size of the managed allocation at `p`, or 0.
static size_t managed_size(void *p) {
    os_unfair_lock_lock(&managed_lock);
    size_t i = managed_upper(p);
    size_t size = i > 0 && managed[i - 1].base == (char *)p
        ? managed[i - 1].size : 0;
    os_unfair_lock_unlock(&managed_lock);
    return size;
}

// The buffer over the managed allocation that holds the `size` bytes at
// `host`, made at its first use, and the offset of the range in it, with the
// serial of the last command buffer that writes it; nil when the range is
// not in one the device can map. With `serial` nonzero the command buffer
// `serial` uses the allocation, and writes it when `write` is set.
static id<MTLBuffer> managed_buffer(lfortran_gpu_ctx *ctx, void *host,
    size_t size, uint64_t serial, int write, size_t *offset,
    uint64_t *last_write)
{
    if (n_managed == 0 || size == 0) return nil;
    id<MTLBuffer> buffer = nil;
    os_unfair_lock_lock(&managed_lock);
    managed_range *r = managed_holding(host, size);
    if (r && !r->unmapped && !r->buffer) {
        // The device maps the pages at the first launch that uses the
        // buffer, so it is kept as long as the allocation.
        id<MTLBuffer> made = [ctx->device newBufferWithBytesNoCopy:r->base
            length:r->length options:MTLResourceStorageModeShared
            deallocator:nil];
        if (made) {
            r->buffer = (__bridge_retained void *)made;
        } else {
            r->unmapped = 1;
        }
    }
    if (r && r->buffer) {
        buffer = (__bridge id<MTLBuffer>)r->buffer;
        *offset = (size_t)((char *)host - r->base);
        if (last_write) *last_write = r->last_write;
        if (serial != 0) {
            r->last_use = serial;
            if (write) r->last_write = serial;
        }
    }
    os_unfair_lock_unlock(&managed_lock);
    return buffer;
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

static void* managed_alloc(void *context, int64_t size) {
    if (size >= MANAGED_MIN_BYTES) {
        os_unfair_lock_lock(&managed_lock);
        int model = current_memory_model();
        os_unfair_lock_unlock(&managed_lock);
        if (model == MEMORY_MANAGED) {
            // Memory free() releases too, as memory from malloc, should C
            // code free it.
            size_t page = vm_page_size;
            size_t length = ((size_t)size + page - 1) / page * page;
            void *p = NULL;
            if (posix_memalign(&p, page, length) == 0 && p) {
                if (managed_record(p, (size_t)size, length)) return p;
                free(p);
            }
        }
    }
    return previous_allocator.alloc(context, size);
}

static void managed_dealloc(void *context, void *ptr) {
    managed_range r;
    if (ptr && managed_forget(ptr, &r)) {
        if (r.buffer) {
            // A pending launch may still use the memory.
            if (singleton && r.last_use > atomic_load_explicit(
                    &singleton->completed, memory_order_acquire)) {
                wait_all(singleton);
            }
            CFRelease(r.buffer);
        }
        free(ptr);
        return;
    }
    previous_allocator.dealloc(context, ptr);
}

static void* managed_realloc(void *context, void *ptr, int64_t size) {
    if (!ptr) return managed_alloc(context, size);
    size_t old = managed_size(ptr);
    if (!old) {
        // An allocation that grows past MANAGED_MIN_BYTES becomes managed
        // too: the previous allocator is malloc, whose blocks malloc_size
        // knows.
        size_t have = size >= MANAGED_MIN_BYTES ? malloc_size(ptr) : 0;
        if (have == 0) return previous_allocator.realloc_func(context, ptr,
            size);
        void *p = managed_alloc(context, size);
        if (!p) return NULL;
        memcpy(p, ptr, have < (size_t)size ? have : (size_t)size);
        previous_allocator.dealloc(context, ptr);
        return p;
    }
    void *p = managed_alloc(context, size);
    if (!p) return NULL;
    memcpy(p, ptr, old < (size_t)size ? old : (size_t)size);
    managed_dealloc(context, ptr);
    return p;
}

// The program's allocator, which a program links from the LFortran runtime
// library; a unit test of this file links none, so it is looked up rather
// than referenced (a Mach-O link does not accept an undefined weak
// reference).
__attribute__((constructor)) static void install_managed_allocator(void) {
    void *get = dlsym(RTLD_DEFAULT, "_lfortran_get_default_allocator");
    host_allocator *a = get ? ((host_allocator *(*)(void))get)() : NULL;
    if (!a || a->alloc == managed_alloc) return;
    previous_allocator = *a;
    a->alloc = managed_alloc;
    a->realloc_func = managed_realloc;
    a->dealloc = managed_dealloc;
}

// ------------------------------------------------------------ context ---

static lfortran_gpu_ctx *profiled_ctx = NULL;
static void retire_all(lfortran_gpu_ctx *ctx);

static void print_profile(lfortran_gpu_ctx *ctx) {
    // Only waits, and does not exit on a failed launch: the host arrays a
    // dirty range belongs to may be gone already at exit.
    retire_all(ctx);
    const char *path = getenv("LFORTRAN_GPU_PROFILE_FILE");
    FILE *out = path && *path ? fopen(path, "w") : NULL;
    if (!out) out = stderr;
    fprintf(out, "LFortran GPU profile (LFORTRAN_GPU_PROFILE=%d)\n",
        ctx->profile_kernels ? 2 : 1);
    if (ctx->telemetry.gpu_timing_available) {
        fprintf(out, "gpu elapsed %.6f s, gpu busy %.6f s\n",
            ctx->telemetry.gpu_elapsed_seconds,
            ctx->telemetry.gpu_busy_seconds);
    } else {
        fprintf(out, "gpu timing unavailable\n");
    }
    fprintf(out, "jit %.6f s, dispatches %llu, commits %llu, host syncs %llu, "
        "host waits %llu\n", ctx->telemetry.jit_seconds,
        (unsigned long long)ctx->telemetry.dispatches,
        (unsigned long long)ctx->telemetry.commits,
        (unsigned long long)ctx->telemetry.host_syncs,
        (unsigned long long)ctx->telemetry.host_waits);
    fprintf(out, "h2d %llu bytes, d2h %llu bytes, buffer allocations %llu "
        "(%llu bytes)\n",
        (unsigned long long)ctx->telemetry.memcpy_h2d_bytes,
        (unsigned long long)ctx->telemetry.memcpy_d2h_bytes,
        (unsigned long long)ctx->telemetry.buffer_allocations,
        (unsigned long long)ctx->telemetry.buffer_alloc_bytes);
    // The kernels by GPU time, then by host time.
    int order[MAX_KERNELS];
    int n = 0;
    for (int i = 0; i < n_registered; i++) {
        if (source_registry[i].launches == 0) continue;
        int j = n++;
        while (j > 0 && (source_registry[order[j - 1]].gpu_seconds
                    < source_registry[i].gpu_seconds
                || (source_registry[order[j - 1]].gpu_seconds
                        == source_registry[i].gpu_seconds
                    && source_registry[order[j - 1]].host_seconds
                        < source_registry[i].host_seconds))) {
            order[j] = order[j - 1];
            j--;
        }
        order[j] = i;
    }
    double total = 0.0;
    for (int i = 0; i < n; i++) total += source_registry[order[i]].gpu_seconds;
    fprintf(out, "%-12s %10s %12s %12s %7s %12s %14s  %s\n", "registry",
        "launches", "gpu s", "gpu us/call", "gpu %", "host s", "h2d bytes",
        "kernel");
    for (int i = 0; i < n; i++) {
        int e = order[i];
        double gpu = source_registry[e].gpu_seconds;
        fprintf(out, "%-12d %10llu %12.6f %12.3f %7.2f %12.6f %14llu  %s\n",
            e, (unsigned long long)source_registry[e].launches, gpu,
            gpu * 1e6 / (double)source_registry[e].launches,
            total > 0.0 ? 100.0 * gpu / total : 0.0,
            source_registry[e].host_seconds,
            (unsigned long long)source_registry[e].h2d_bytes,
            source_registry[e].name);
    }
    if (out != stderr) fclose(out);
}

static void print_profile_at_exit(void) {
    if (profiled_ctx) print_profile(profiled_ctx);
    profiled_ctx = NULL;
}

lfortran_gpu_ctx* lfortran_gpu_init(void) {
    if (singleton) return singleton;
    singleton = (lfortran_gpu_ctx*)calloc(1, sizeof(lfortran_gpu_ctx));
    singleton->device = MTLCreateSystemDefaultDevice();
    if (!singleton->device) {
        // Without a device every launch would be skipped and the program
        // would continue with the values its loops never computed.
        fprintf(stderr, "lfortran_gpu_init: Metal is not supported on this device\n");
        exit(1);
    }
    singleton->queue = [singleton->device newCommandQueue];
    singleton->last_command_buffer = nil;
    int profile_level = gpu_profile_level();
    singleton->profile_enabled = profile_level >= 1;
    singleton->profile_kernels = profile_level == 2;
    singleton->telemetry.gpu_timing_available = singleton->profile_enabled;
    if (singleton->profile_enabled) {
        profiled_ctx = singleton;
        atexit(print_profile_at_exit);
    }
    os_unfair_lock_lock(&managed_lock);
    current_memory_model();
    os_unfair_lock_unlock(&managed_lock);
    return singleton;
}

void lfortran_gpu_shutdown(lfortran_gpu_ctx* ctx) {
    if (!ctx) return;
    sync_all(ctx);
    if (ctx == profiled_ctx) print_profile_at_exit();
    for (int i = 0; i < n_entries; i++) {
        if (entries[i].used) remove_entry(i);
    }
    for (int i = 0; i < n_pool; i++) pool[i].buffer = nil;
    n_pool = 0;
    ctx->device = nil;
    ctx->queue = nil;
    ctx->last_command_buffer = nil;
    if (ctx == singleton) singleton = NULL;
    free(ctx);
}

// The Metal Shading Language source is registered by a constructor the host
// code generator emits, so that loading a kernel only needs its name, exactly
// like the CUDA and CPU runtimes.
void lfortran_gpu_register_metal_source(const char *name, const char *source) {
    if (n_registered >= MAX_KERNELS) {
        fprintf(stderr, "lfortran_gpu_register_metal_source: too many kernels\n");
        exit(1);
    }
    source_registry[n_registered].name = name;
    source_registry[n_registered].source = source;
    n_registered++;
}

static id<MTLComputePipelineState> compile_pipeline(lfortran_gpu_ctx* ctx,
    const char *name_buf, const char *source)
{
    double started = ctx->profile_enabled ? telemetry_now() : 0.0;
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

    NSString *name = [NSString stringWithUTF8String:name_buf];
    id<MTLFunction> function = [library newFunctionWithName:name];
    if (!function) {
        fprintf(stderr, "lfortran_gpu_load_kernel: Function '%s' not found in shader\n",
                name_buf);
        exit(1);
    }

    id<MTLComputePipelineState> pipeline =
        [ctx->device newComputePipelineStateWithFunction:function error:&error];
    if (!pipeline) {
        fprintf(stderr, "lfortran_gpu_load_kernel: Failed to create pipeline:\n%s\n",
                [[error localizedDescription] UTF8String]);
        exit(1);
    }
    if (ctx->profile_enabled) {
        ctx->telemetry.jit_seconds += telemetry_now() - started;
    }
    return pipeline;
}

lfortran_gpu_kernel* lfortran_gpu_load_kernel(
    lfortran_gpu_ctx* ctx, const char* entry_point, int entry_point_len)
{
    if (!ctx || !entry_point) return NULL;

    // A launch passes the same name, a constant, every time: the entry
    // found for the address of the name is remembered.
    enum { NAME_CACHE = 64 };
    static struct { const char *name; int len; int entry; }
        name_cache[NAME_CACHE];
    unsigned slot = (unsigned)(((uintptr_t)entry_point >> 3)
        * 0x9E3779B1u) % NAME_CACHE;
    int entry = -1;
    if (name_cache[slot].name == entry_point
            && name_cache[slot].len == entry_point_len) {
        entry = name_cache[slot].entry;
    } else {
        char name_buf[LFORTRAN_GPU_MAX_KERNEL_NAME];
        lfortran_gpu_copy_kernel_name(name_buf, entry_point, entry_point_len);
        for (int i = 0; i < n_registered; i++) {
            if (strcmp(source_registry[i].name, name_buf) == 0) {
                entry = i;
                break;
            }
        }
        if (entry < 0) {
            fprintf(stderr, "lfortran_gpu_load_kernel: kernel '%s' not found in registry\n",
                    name_buf);
            exit(1);
        }
        name_cache[slot].name = entry_point;
        name_cache[slot].len = entry_point_len;
        name_cache[slot].entry = entry;
    }
    if (!source_registry[entry].pipeline) {
        @autoreleasepool {
            source_registry[entry].pipeline = compile_pipeline(ctx,
                source_registry[entry].name, source_registry[entry].source);
        }
    }

    lfortran_gpu_kernel *k = (lfortran_gpu_kernel*)calloc(1, sizeof(lfortran_gpu_kernel));
    k->pipeline = source_registry[entry].pipeline;
    k->ctx = ctx;
    k->n_args = 0;
    k->entry = entry;
    return k;
}

// Gives the copy of read-only argument `idx` that `k` keeps back to the
// pool, once the launches that read it are done.
static void release_read_buffer(lfortran_gpu_kernel *k, int idx) {
    if (!k->read_buffers[idx]) return;
    release_buffer(k->read_buffers[idx], k->serial);
    k->read_buffers[idx] = nil;
}

void lfortran_gpu_release_kernel(lfortran_gpu_kernel* k) {
    if (!k) return;
    for (int i = 0; i < k->n_args; i++) {
        if (k->scalar_heap[i]) free(k->scalar_heap[i]);
        k->scratch_buffers[i] = nil;
        release_read_buffer(k, i);
    }
    k->pipeline = nil;
    free(k);
}

static void set_arg(lfortran_gpu_kernel *k, int idx, int kind) {
    release_read_buffer(k, idx);
    k->kind[idx] = kind;
    if (idx >= k->n_args) k->n_args = idx + 1;
}

static id<MTLBuffer> tracked_buffer_alloc(lfortran_gpu_ctx *ctx, size_t length) {
    if (ctx->profile_enabled) {
        ctx->telemetry.buffer_allocations++;
        ctx->telemetry.buffer_alloc_bytes += length;
    }
    id<MTLBuffer> buffer = [ctx->device newBufferWithLength:length
        options:MTLResourceStorageModeShared];
    if (!buffer) fail("buffer", length);
    return buffer;
}

void lfortran_gpu_set_scratch_arg(lfortran_gpu_kernel* k, int idx,
    size_t size)
{
    if (!k || idx >= MAX_ARGS) return;
    k->ptrs[idx] = NULL;
    k->sizes[idx] = size;
    set_arg(k, idx, ARG_SCRATCH);
}

void lfortran_gpu_read_scratch_arg(lfortran_gpu_kernel* k, int idx,
    void* dst, size_t size)
{
    if (!k || idx >= MAX_ARGS || size == 0) return;
    if (k->kind[idx] != ARG_SCRATCH || !k->scratch_buffers[idx]
            || size > k->sizes[idx]) {
        fprintf(stderr, "lfortran_gpu_read_scratch_arg: no scratch buffer "
            "%d of %zu bytes\n", idx, size);
        exit(1);
    }
    sync_all(k->ctx);
    if (k->ctx->profile_enabled) {
        k->ctx->telemetry.memcpy_d2h_bytes += size;
    }
    memcpy(dst, [k->scratch_buffers[idx] contents], size);
}

void lfortran_gpu_set_buffer_arg(lfortran_gpu_kernel* k, int idx,
    void* ptr, size_t size, int access)
{
    if (!k || idx >= MAX_ARGS) return;
    k->ptrs[idx] = ptr;
    k->sizes[idx] = size;
    k->access[idx] = access;
    set_arg(k, idx, ARG_BUFFER);
}

void lfortran_gpu_set_scalar_arg(lfortran_gpu_kernel* k, int idx,
    const void* val, size_t size)
{
    if (!k || idx >= MAX_ARGS) return;
    if (k->scalar_heap[idx]) {
        free(k->scalar_heap[idx]);
        k->scalar_heap[idx] = NULL;
    }
    void *dst;
    if (k->scalar_used + size <= SCALAR_BYTES) {
        dst = k->scalar_bytes + k->scalar_used;
        // Each argument starts aligned for any scalar.
        k->scalar_used += (size + 15) & ~(size_t)15;
    } else {
        dst = k->scalar_heap[idx] = malloc(size);
    }
    memcpy(dst, val, size);
    k->ptrs[idx] = dst;
    k->sizes[idx] = size;
    set_arg(k, idx, ARG_SCALAR);
}

static void commit_open(lfortran_gpu_ctx *ctx);

static int listed(void *const *list, int n, void *buffer) {
    for (int i = 0; i < n; i++) {
        if (list[i] == buffer) return 1;
    }
    return 0;
}

// The encoder runs its dispatches concurrently: a dispatch that reads what
// one since the last barrier writes, or writes what one reads or writes,
// gets a barrier before it, so that the device runs the launches as if one
// after another.
static void order_after_dependences(lfortran_gpu_ctx *ctx,
    id<MTLComputeCommandEncoder> encoder, const buffer_use *uses, int n)
{
    int barrier = ctx->n_reads + n > MAX_TRACKED
        || ctx->n_writes + n > MAX_TRACKED;
    for (int i = 0; i < n && !barrier; i++) {
        barrier = listed(ctx->writes, ctx->n_writes, uses[i].buffer)
            || (uses[i].write
                && listed(ctx->reads, ctx->n_reads, uses[i].buffer));
    }
    if (barrier) {
        [encoder memoryBarrierWithScope:MTLBarrierScopeBuffers];
        ctx->n_reads = ctx->n_writes = 0;
    }
    for (int i = 0; i < n; i++) {
        if (uses[i].write) {
            ctx->writes[ctx->n_writes++] = uses[i].buffer;
        } else {
            ctx->reads[ctx->n_reads++] = uses[i].buffer;
        }
    }
}

// The scratch buffer of argument `slot` of the kernel `k`, with room for
// `size` bytes: the one the kernel keeps for the slot, grown when it is too
// small, like the slot buffer of the CUDA runtime. A large one -- a device
// heap arena, or the workspaces of a kernel with many threads -- is kept
// too: making a new one at every launch costs the device more than the
// kernel (a new buffer's pages are mapped for the device at its first use).
static id<MTLBuffer> scratch_buffer(lfortran_gpu_kernel *k, int slot,
    size_t size)
{
    lfortran_gpu_ctx *ctx = k->ctx;
    id<MTLBuffer> buffer = source_registry[k->entry].scratch[slot];
    if (buffer && [buffer length] >= size) return buffer;
    size_t length = 256;
    while (length < size) length *= 2;
    if (length > POOLED_BUFFER_MAX) length = size;
    buffer = tracked_buffer_alloc(ctx, length);
    source_registry[k->entry].scratch[slot] = buffer;
    return buffer;
}

static void wait_for_completion(lfortran_gpu_ctx *ctx,
        id<MTLCommandBuffer> cb)
{
    if (!cb) return;
    if (ctx->profile_enabled) ctx->telemetry.host_waits++;
    [cb waitUntilCompleted];
}

// Waits for every launch made so far.
static void retire_all(lfortran_gpu_ctx *ctx) {
    commit_open(ctx);
    if (ctx->last_command_buffer) {
        wait_for_completion(ctx, ctx->last_command_buffer);
        ctx->last_command_buffer = nil;
    }
    while (atomic_load_explicit(&ctx->handled, memory_order_acquire)
            < ctx->serial) {
        sched_yield();
    }
    if (ctx->profile_enabled) {
        uint64_t first = ctx->telemetry.applied_serial + 1;
        uint64_t last = ctx->serial;
        uint64_t floor = last > (uint64_t)(MAX_PENDING_COMMITS - 1)
            ? last - (uint64_t)(MAX_PENDING_COMMITS - 1) : 1;
        if (first < floor) first = floor;
        for (uint64_t serial = first; serial <= last; serial++) {
            id<MTLCommandBuffer> cb = ctx->pending[serial % MAX_PENDING_COMMITS];
            if (!cb) continue;
            telemetry_collect_command_buffer(ctx, serial, cb);
            ctx->pending[serial % MAX_PENDING_COMMITS] = nil;
        }
    }
    for (int i = 0; i < MAX_PENDING_COMMITS; i++) {
        ctx->pending[i] = nil;
    }
}

// Waits for every launch made so far, and exits if one failed.
static void wait_all(lfortran_gpu_ctx *ctx) {
    retire_all(ctx);
    // A kernel that faulted or was stopped by the watchdog has not computed
    // its results, so the program cannot go on with what is in the buffers;
    // the CUDA runtime exits in this case too.
    if (atomic_load(&ctx->failed)) {
        fprintf(stderr, "lfortran_gpu_launch: GPU execution error: %s\n",
            ctx->error);
        exit(1);
    }
}

static void sync_all(lfortran_gpu_ctx *ctx) {
    ctx->n_syncs++;
    if (ctx->profile_enabled) ctx->telemetry.host_syncs++;
    wait_all(ctx);
    while (n_dirty > 0) {
        int i = dirty_list[n_dirty - 1];
        if (ctx->profile_enabled) {
            ctx->telemetry.memcpy_d2h_bytes += entries[i].size;
        }
        memcpy(entries[i].host, [entries[i].buffer contents],
            entries[i].size);
        set_clean(i);
    }
}

// Ends the open encoder and commits its command buffer.
static void commit_open(lfortran_gpu_ctx *ctx) {
    if (!ctx->open_command_buffer) return;
    [ctx->encoder endEncoding];
    ctx->encoder = nil;
    id<MTLCommandBuffer> cb = ctx->open_command_buffer;
    ctx->open_command_buffer = nil;
    if (ctx->profile_enabled) {
        ctx->telemetry.dispatches += (uint64_t)ctx->n_encoded;
        ctx->telemetry.commits++;
    }
    ctx->n_encoded = 0;
    uint64_t serial = ++ctx->serial;
    int slot = (int)(serial % MAX_PENDING_COMMITS);
    if (ctx->pending[slot]) {
        wait_for_completion(ctx, ctx->pending[slot]);
        telemetry_collect_command_buffer(ctx, serial - MAX_PENDING_COMMITS,
            ctx->pending[slot]);
        ctx->pending[slot] = nil;
    }
    ctx->pending[slot] = cb;
    ctx->pending_entry[slot] = ctx->open_entry;
    lfortran_gpu_ctx *c = ctx;
    [cb addCompletedHandler:^(id<MTLCommandBuffer> done) {
        NSError *error = [done error];
        if (error && !atomic_exchange(&c->failed, 1)) {
            snprintf(c->error, sizeof(c->error), "%s",
                [[error localizedDescription] UTF8String]);
        }
        uint64_t seen = atomic_load(&c->completed);
        while (seen < serial && !atomic_compare_exchange_weak(&c->completed,
                &seen, serial)) {
        }
        atomic_fetch_add_explicit(&c->handled, 1, memory_order_release);
    }];
    [cb commit];
    ctx->last_command_buffer = cb;
}

// Whether buffer argument `idx` of `k` overlaps one the kernel may write.
static int overlaps_written_arg(const lfortran_gpu_kernel *k, int idx) {
    for (int j = 0; j < k->n_args; j++) {
        if (j == idx || k->kind[j] != ARG_BUFFER
                || k->access[j] == LFORTRAN_GPU_READ) {
            continue;
        }
        if (overlaps(k->ptrs[idx], k->sizes[idx], k->ptrs[j], k->sizes[j])) {
            return 1;
        }
    }
    return 0;
}

void lfortran_gpu_launch(lfortran_gpu_ctx* ctx, lfortran_gpu_kernel* k,
    int grid[3], int block[3])
{
    lfortran_gpu_validate_launch(grid, block);
    if (!ctx || !k) return;
    double started = ctx->profile_enabled ? telemetry_now() : 0.0;
    uint64_t h2d_before = ctx->telemetry.memcpy_h2d_bytes;

    // The command buffer, its encoder and the buffers it holds on to are
    // autoreleased: without a pool of its own every launch would keep them
    // until the program ends.
    @autoreleasepool {
        int is_buffer[MAX_ARGS];
        for (int i = 0; i < k->n_args; i++) {
            is_buffer[i] = k->kind[i] == ARG_BUFFER;
        }
        // The ranges in managed allocations the kernel uses in place (the
        // managed memory model), and their offsets in the buffers.
        id<MTLBuffer> in_place[MAX_ARGS];
        size_t in_place_offset[MAX_ARGS];
        int copies_written = 0;
        for (int i = 0; i < k->n_args; i++) {
            in_place[i] = nil;
            if (!is_buffer[i] || k->sizes[i] == 0) continue;
            uint64_t last_write = 0;
            id<MTLBuffer> buffer = managed_buffer(ctx, k->ptrs[i],
                k->sizes[i], 0, 0, &in_place_offset[i], &last_write);
            if (!buffer) continue;
            int overlap = k->access[i] == LFORTRAN_GPU_READ
                && overlaps_written_arg(k, i);
            if (k->access[i] == LFORTRAN_GPU_READ
                    && (k->sizes[i] <= MANAGED_COPY_READ_MAX || overlap)) {
                // A copy, unless a pending launch writes the allocation
                // and the kernel need not read it as the host holds it.
                if (completed_by_device(ctx, last_write)) continue;
                if (overlap) {
                    copies_written = 1;
                    continue;
                }
            }
            in_place[i] = buffer;
        }
        // A copy of what a pending launch writes in place needs the launch
        // to be done.
        if (copies_written) wait_all(ctx);
        // The host bytes of a range that overlaps one the device holds
        // newer values for are not current: get them back first.
        for (int i = 0; i < k->n_args; i++) {
            if (is_buffer[i] && !in_place[i]
                    && overlaps_dirty(k->ptrs[i], k->sizes[i])) {
                sync_all(ctx);
                break;
            }
        }
        // The ranges the kernel may write first, so that one the kernel is
        // also handed to read is read from the same buffer. Making them
        // ready may have to sync, which leaves nothing dirty: then they are
        // made ready again.
        id<MTLBuffer> written[MAX_ARGS];
        for (uint64_t syncs = ctx->n_syncs - 1; syncs != ctx->n_syncs;) {
            syncs = ctx->n_syncs;
            for (int i = 0; i < k->n_args; i++) {
                written[i] = nil;
                if (k->kind[i] != ARG_BUFFER
                        || k->access[i] == LFORTRAN_GPU_READ
                        || k->sizes[i] == 0 || in_place[i]) {
                    continue;
                }
                int discard = k->access[i] == LFORTRAN_GPU_WRITE_DISCARD
                    && lfortran_gpu_discard_allowed(i, k->ptrs, k->sizes,
                        is_buffer, k->n_args);
                written[i] = resident_for_write(ctx, k->ptrs[i],
                    k->sizes[i], discard);
            }
        }
        // The launch goes into the open command buffer, which is
        // committed as the next serial.
        uint64_t serial = ctx->serial + 1;
        k->serial = serial;
        if (!ctx->open_command_buffer) {
            ctx->open_command_buffer = [ctx->queue commandBuffer];
            ctx->encoder = [ctx->open_command_buffer
                computeCommandEncoderWithDispatchType:
                    MTLDispatchTypeConcurrent];
            ctx->n_reads = ctx->n_writes = 0;
        }
        id<MTLComputeCommandEncoder> encoder = ctx->encoder;
        [encoder setComputePipelineState:k->pipeline];
        // The device buffers this dispatch reads and writes.
        buffer_use uses[MAX_ARGS];
        int n_uses = 0;

        for (int i = 0; i < k->n_args; i++) {
            if (!written[i]) continue;
            entries[find_entry(k->ptrs[i])].last_use = serial;
            [encoder setBuffer:written[i] offset:0 atIndex:i];
            uses[n_uses].buffer = (__bridge void *)written[i];
            uses[n_uses++].write = 1;
        }
        for (int i = 0; i < k->n_args; i++) {
            if (!in_place[i]) continue;
            int write = k->access[i] != LFORTRAN_GPU_READ;
            size_t offset;
            managed_buffer(ctx, k->ptrs[i], k->sizes[i], serial, write,
                &offset, NULL);
            release_read_buffer(k, i);
            [encoder setBuffer:in_place[i] offset:in_place_offset[i]
                atIndex:i];
            uses[n_uses].buffer = (__bridge void *)in_place[i];
            uses[n_uses++].write = write;
            if (!write) ctx->in_place_read = serial;
        }
        for (int i = 0; i < k->n_args; i++) {
            switch (k->kind[i]) {
                case ARG_BUFFER: {
                    if (k->sizes[i] == 0) {
                        // An array with no elements, or one that is not
                        // allocated and that the kernel does not touch:
                        // Metal has no buffer of no bytes.
                        [encoder setBuffer:nil offset:0 atIndex:i];
                        break;
                    }
                    if (k->access[i] != LFORTRAN_GPU_READ
                            || in_place[i]) {
                        break;
                    }
                    id<MTLBuffer> buffer = dirty_resident(k->ptrs[i],
                        k->sizes[i]);
                    if (buffer) {
                        release_read_buffer(k, i);
                        entries[find_entry(k->ptrs[i])].last_use = serial;
                        uses[n_uses].buffer = (__bridge void *)buffer;
                        uses[n_uses++].write = 0;
                        [encoder setBuffer:buffer offset:0 atIndex:i];
                    } else if (k->sizes[i] <= INLINE_BYTES_MAX) {
                        [encoder setBytes:k->ptrs[i] length:k->sizes[i]
                            atIndex:i];
                    } else {
                        // The chunks of a launch hand the argument over
                        // once: the copy the first one made is kept.
                        buffer = k->read_buffers[i];
                        if (!buffer) {
                            buffer = acquire_buffer(ctx, k->sizes[i]);
                            if (ctx->profile_enabled) {
                                ctx->telemetry.memcpy_h2d_bytes
                                    += k->sizes[i];
                            }
                            memcpy([buffer contents], k->ptrs[i],
                                k->sizes[i]);
                            k->read_buffers[i] = buffer;
                        }
                        [encoder setBuffer:buffer offset:0 atIndex:i];
                    }
                    break;
                }
                case ARG_SCRATCH: {
                    if (k->sizes[i] == 0) {
                        [encoder setBuffer:nil offset:0 atIndex:i];
                        break;
                    }
                    id<MTLBuffer> buffer = scratch_buffer(k, i,
                        k->sizes[i]);
                    k->scratch_buffers[i] = buffer;
                    uses[n_uses].buffer = (__bridge void *)buffer;
                    uses[n_uses++].write = 1;
                    [encoder setBuffer:buffer offset:0 atIndex:i];
                    break;
                }
                case ARG_SCALAR:
                    [encoder setBytes:k->ptrs[i] length:k->sizes[i]
                        atIndex:i];
                    break;
                default:
                    break;
            }
        }

        NSUInteger total_threads = (NSUInteger)grid[0] * (NSUInteger)block[0];
        NSUInteger threads_per_group = (NSUInteger)block[0];
        NSUInteger max_threads = [k->pipeline maxTotalThreadsPerThreadgroup];
        if (threads_per_group > max_threads) {
            threads_per_group = max_threads;
        }
        order_after_dependences(ctx, encoder, uses, n_uses);
        [encoder dispatchThreads:MTLSizeMake(total_threads, 1, 1)
            threadsPerThreadgroup:MTLSizeMake(threads_per_group, 1, 1)];
        ctx->n_encoded++;
        if (ctx->profile_enabled) {
            source_registry[k->entry].launches++;
            source_registry[k->entry].h2d_bytes
                += ctx->telemetry.memcpy_h2d_bytes - h2d_before;
            source_registry[k->entry].host_seconds
                += telemetry_now() - started;
        }
        if (ctx->profile_kernels) {
            ctx->open_entry = k->entry;
            commit_open(ctx);
        } else if (ctx->n_encoded >= LAUNCHES_PER_COMMIT) {
            commit_open(ctx);
        }
    }
}

void lfortran_gpu_sync(lfortran_gpu_ctx* ctx) {
    if (!ctx) return;
    sync_all(ctx);
}

// A launch copies what its kernel only reads, except a range in a managed
// allocation that it reads in place: the host may change that once the
// launches that read it are done.
void lfortran_gpu_wait(lfortran_gpu_ctx* ctx) {
    if (!ctx || completed_by_device(ctx, ctx->in_place_read)) return;
    wait_all(ctx);
}

// A launch copies what its kernel only reads at the launch itself, and keeps
// no copy for later launches, so there is none to keep.
void lfortran_gpu_keep(lfortran_gpu_ctx* ctx, void* ptr, size_t size) {
    (void)ctx;
    (void)ptr;
    (void)size;
}

void lfortran_gpu_discard(lfortran_gpu_ctx* ctx, void* ptr, size_t size) {
    if (!ctx || size == 0) return;
    for (int d = n_dirty - 1; d >= 0; d--) {
        int i = dirty_list[d];
        if (overlaps(entries[i].host, entries[i].size, ptr, size)) {
            remove_entry(i);
        }
    }
    int i = find_entry(ptr);
    if (i >= 0) remove_entry(i);
}

long long lfortran_gpu_cache_bytes(void) {
    return 0;
}

long long lfortran_gpu_heap_size(void) {
    return lfortran_gpu_parse_heap_size();
}

void lfortran_gpu_metal_telemetry_snapshot(lfortran_gpu_ctx* ctx,
    lfortran_gpu_metal_telemetry *out)
{
    if (!out) return;
    memset(out, 0, sizeof(*out));
    if (!ctx) return;
    out->gpu_timing_available = ctx->profile_enabled
        && ctx->telemetry.gpu_timing_available;
    if (!ctx->profile_enabled) return;
    out->completed_command_buffers = ctx->telemetry.completed_command_buffers;
    out->commits = ctx->telemetry.commits;
    out->dispatches = ctx->telemetry.dispatches;
    out->host_syncs = ctx->telemetry.host_syncs;
    out->host_waits = ctx->telemetry.host_waits;
    out->memcpy_h2d_bytes = ctx->telemetry.memcpy_h2d_bytes;
    out->memcpy_d2h_bytes = ctx->telemetry.memcpy_d2h_bytes;
    out->buffer_allocations = ctx->telemetry.buffer_allocations;
    out->buffer_alloc_bytes = ctx->telemetry.buffer_alloc_bytes;
    out->jit_seconds = ctx->telemetry.jit_seconds;
    if (ctx->telemetry.gpu_timing_available) {
        out->gpu_elapsed_seconds = ctx->telemetry.gpu_elapsed_seconds;
        out->gpu_busy_seconds = ctx->telemetry.gpu_busy_seconds;
    }
}

void lfortran_gpu_metal_telemetry_snapshot_sync(lfortran_gpu_ctx* ctx,
    lfortran_gpu_metal_telemetry *out)
{
    if (ctx) lfortran_gpu_sync(ctx);
    lfortran_gpu_metal_telemetry_snapshot(ctx, out);
}
