#import <Metal/Metal.h>
#import <Foundation/Foundation.h>
#include "lfortran_gpu_runtime.h"
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
  range, the kernel reads the resident buffer instead.
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
*/

// A read-only argument up to this size is copied into the encoder.
static const size_t INLINE_BYTES_MAX = 4096;
// Buffers up to this size are kept in the pool for reuse; a larger one, such
// as the device heap, is released when the launch is done with it.
static const size_t POOLED_BUFFER_MAX = (size_t)16 * 1024 * 1024;
enum { POOL_MAX = 128, MAX_RESIDENT = 1024, HASH_SIZE = 4096 };
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
// buffer is dead once it ends (a part the host reads back is read before the
// next launch is made), so every launch of the kernel can use the same one,
// however many of them are still pending.
static struct {
    const char *name;
    const char *source;
    id<MTLComputePipelineState> pipeline;
    id<MTLBuffer> scratch[MAX_ARGS];
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

static int gpu_profile_enabled(void) {
    const char *value = getenv("LFORTRAN_GPU_PROFILE");
    return value != NULL && strcmp(value, "1") == 0;
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
    // A buffer only the device holds, created at the launch and kept until
    // the kernel is released so that the host can read part of it back.
    id<MTLBuffer> scratch_buffers[MAX_ARGS];
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

// ------------------------------------------------------------ context ---

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
    singleton->profile_enabled = gpu_profile_enabled();
    singleton->telemetry.gpu_timing_available = singleton->profile_enabled;
    return singleton;
}

void lfortran_gpu_shutdown(lfortran_gpu_ctx* ctx) {
    if (!ctx) return;
    sync_all(ctx);
    for (int i = 0; i < n_entries; i++) {
        if (entries[i].used) remove_entry(i);
    }
    for (int i = 0; i < n_pool; i++) pool[i].buffer = nil;
    n_pool = 0;
    ctx->device = nil;
    ctx->queue = nil;
    ctx->last_command_buffer = nil;
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

void lfortran_gpu_release_kernel(lfortran_gpu_kernel* k) {
    if (!k) return;
    for (int i = 0; i < k->n_args; i++) {
        if (k->scalar_heap[i]) free(k->scalar_heap[i]);
        k->scratch_buffers[i] = nil;
    }
    k->pipeline = nil;
    free(k);
}

static void set_arg(lfortran_gpu_kernel *k, int idx, int kind) {
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

// The scratch buffer of argument `slot` of the kernel `entry`, with room for
// `size` bytes. One larger than the pool keeps -- the device heap -- is made
// for the launch.
static id<MTLBuffer> scratch_buffer(lfortran_gpu_ctx *ctx, int entry,
    int slot, size_t size)
{
    if (size > POOLED_BUFFER_MAX) {
        return tracked_buffer_alloc(ctx, size);
    }
    id<MTLBuffer> buffer = source_registry[entry].scratch[slot];
    if (buffer && [buffer length] >= size) return buffer;
    size_t length = 256;
    while (length < size) length *= 2;
    buffer = tracked_buffer_alloc(ctx, length);
    source_registry[entry].scratch[slot] = buffer;
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
static void wait_all(lfortran_gpu_ctx *ctx) {
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

void lfortran_gpu_launch(lfortran_gpu_ctx* ctx, lfortran_gpu_kernel* k,
    int grid[3], int block[3])
{
    lfortran_gpu_validate_launch(grid, block);
    if (!ctx || !k) return;

    // The command buffer, its encoder and the buffers it holds on to are
    // autoreleased: without a pool of its own every launch would keep them
    // until the program ends.
    @autoreleasepool {
        // The host bytes of a range that overlaps one the device holds
        // newer values for are not current: get them back first.
        for (int i = 0; i < k->n_args; i++) {
            if (k->kind[i] == ARG_BUFFER
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
        int is_buffer[MAX_ARGS];
        for (int i = 0; i < k->n_args; i++) {
            is_buffer[i] = k->kind[i] == ARG_BUFFER;
        }
        for (uint64_t syncs = ctx->n_syncs - 1; syncs != ctx->n_syncs;) {
            syncs = ctx->n_syncs;
            for (int i = 0; i < k->n_args; i++) {
                written[i] = nil;
                if (k->kind[i] != ARG_BUFFER
                        || k->access[i] == LFORTRAN_GPU_READ
                        || k->sizes[i] == 0) {
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
            switch (k->kind[i]) {
                case ARG_BUFFER: {
                    if (k->sizes[i] == 0) {
                        // An array with no elements, or one that is not
                        // allocated and that the kernel does not touch:
                        // Metal has no buffer of no bytes.
                        [encoder setBuffer:nil offset:0 atIndex:i];
                        break;
                    }
                    if (k->access[i] != LFORTRAN_GPU_READ) break;
                    id<MTLBuffer> buffer = dirty_resident(k->ptrs[i],
                        k->sizes[i]);
                    if (buffer) {
                        entries[find_entry(k->ptrs[i])].last_use = serial;
                        uses[n_uses].buffer = (__bridge void *)buffer;
                        uses[n_uses++].write = 0;
                        [encoder setBuffer:buffer offset:0 atIndex:i];
                    } else if (k->sizes[i] <= INLINE_BYTES_MAX) {
                        [encoder setBytes:k->ptrs[i] length:k->sizes[i]
                            atIndex:i];
                    } else {
                        buffer = acquire_buffer(ctx, k->sizes[i]);
                        if (ctx->profile_enabled) {
                            ctx->telemetry.memcpy_h2d_bytes += k->sizes[i];
                        }
                        memcpy([buffer contents], k->ptrs[i], k->sizes[i]);
                        [encoder setBuffer:buffer offset:0 atIndex:i];
                        release_buffer(buffer, serial);
                    }
                    break;
                }
                case ARG_SCRATCH: {
                    if (k->sizes[i] == 0) {
                        [encoder setBuffer:nil offset:0 atIndex:i];
                        break;
                    }
                    id<MTLBuffer> buffer = scratch_buffer(ctx, k->entry, i,
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

        NSUInteger total_threads = (NSUInteger)(grid[0] * block[0]);
        NSUInteger threads_per_group = (NSUInteger)block[0];
        NSUInteger max_threads = [k->pipeline maxTotalThreadsPerThreadgroup];
        if (threads_per_group > max_threads) {
            threads_per_group = max_threads;
        }
        order_after_dependences(ctx, encoder, uses, n_uses);
        [encoder dispatchThreads:MTLSizeMake(total_threads, 1, 1)
            threadsPerThreadgroup:MTLSizeMake(threads_per_group, 1, 1)];
        if (++ctx->n_encoded >= LAUNCHES_PER_COMMIT) commit_open(ctx);
    }
}

void lfortran_gpu_sync(lfortran_gpu_ctx* ctx) {
    if (!ctx) return;
    sync_all(ctx);
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
