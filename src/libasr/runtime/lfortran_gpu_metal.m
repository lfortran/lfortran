#import <Metal/Metal.h>
#import <Foundation/Foundation.h>
#include "lfortran_gpu_runtime.h"
#include <sched.h>
#include <stdatomic.h>
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
  host bytes are copied in when the entry is not dirty, and after the launch
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
*/

// A read-only argument up to this size is copied into the encoder.
static const size_t INLINE_BYTES_MAX = 4096;
// Buffers up to this size are kept in the pool for reuse; a larger one, such
// as the device heap, is released when the launch is done with it.
static const size_t POOLED_BUFFER_MAX = (size_t)16 * 1024 * 1024;
enum { POOL_MAX = 128, MAX_RESIDENT = 1024, HASH_SIZE = 4096 };

// Registry of the shader sources emitted for each kernel, and the pipeline
// compiled from each, once per run: a kernel is loaded at every launch, and
// compiling its source again every time is slow and exhausts the driver's
// cache of compiled shaders.
static struct {
    const char *name;
    const char *source;
    id<MTLComputePipelineState> pipeline;
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
    uint64_t last_use;
} pooled;

struct lfortran_gpu_ctx {
    id<MTLDevice> device;
    id<MTLCommandQueue> queue;
    // The last command buffer committed and not yet waited for.
    id<MTLCommandBuffer> last_command_buffer;
    // Command buffers are numbered from 1 as they are committed; `completed`
    // is the highest number the device has finished.
    uint64_t serial;
    _Atomic uint64_t completed;
    _Atomic uint64_t handled;
    _Atomic int failed;
    char error[512];
    uint64_t lru_clock;
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
enum { SCALAR_INLINE_MAX = 256 };

struct lfortran_gpu_kernel {
    id<MTLComputePipelineState> pipeline;
    lfortran_gpu_ctx *ctx;
    void *ptrs[MAX_ARGS];
    size_t sizes[MAX_ARGS];
    int access[MAX_ARGS];
    int kind[MAX_ARGS];
    unsigned char scalar_inline[MAX_ARGS][SCALAR_INLINE_MAX];
    void *scalar_heap[MAX_ARGS];
    // A buffer only the device holds, created at the launch and kept until
    // the kernel is released so that the host can read part of it back.
    id<MTLBuffer> scratch_buffers[MAX_ARGS];
    int n_args;
    uint64_t serial;
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

// ---------------------------------------------------------------- pool ---

static id<MTLBuffer> acquire_buffer(lfortran_gpu_ctx *ctx, size_t size) {
    int best = -1;
    for (int i = 0; i < n_pool; i++) {
        NSUInteger length = [pool[i].buffer length];
        if (length < size || length > 4 * size + 4096) continue;
        if (!completed_by_device(ctx, pool[i].last_use)) continue;
        if (best < 0 || length < [pool[best].buffer length]) best = i;
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
static id<MTLBuffer> resident_for_write(lfortran_gpu_ctx *ctx, void *host,
    size_t size)
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
    memcpy([e->buffer contents], host, size);
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
    return pipeline;
}

lfortran_gpu_kernel* lfortran_gpu_load_kernel(
    lfortran_gpu_ctx* ctx, const char* entry_point, int entry_point_len)
{
    if (!ctx || !entry_point) return NULL;

    // A launch passes the same name every time, so the entry found for it
    // last is looked at first.
    static const char *last_name = NULL;
    static int last_len = -1, last_entry = -1;
    int entry = -1;
    if (entry_point == last_name && entry_point_len == last_len) {
        entry = last_entry;
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
        last_name = entry_point;
        last_len = entry_point_len;
        last_entry = entry;
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
    return k;
}

void lfortran_gpu_release_kernel(lfortran_gpu_kernel* k) {
    if (!k) return;
    for (int i = 0; i < k->n_args; i++) {
        if (k->scalar_heap[i]) free(k->scalar_heap[i]);
        if (k->scratch_buffers[i]) {
            release_buffer(k->scratch_buffers[i], k->serial);
            k->scratch_buffers[i] = nil;
        }
    }
    k->pipeline = nil;
    free(k);
}

static void set_arg(lfortran_gpu_kernel *k, int idx, int kind) {
    k->kind[idx] = kind;
    if (idx >= k->n_args) k->n_args = idx + 1;
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
    void *dst = k->scalar_inline[idx];
    if (size > SCALAR_INLINE_MAX) {
        dst = k->scalar_heap[idx] = malloc(size);
    }
    memcpy(dst, val, size);
    k->ptrs[idx] = dst;
    k->sizes[idx] = size;
    set_arg(k, idx, ARG_SCALAR);
}

// Waits for every command buffer committed so far.
static void wait_all(lfortran_gpu_ctx *ctx) {
    if (ctx->last_command_buffer) {
        [ctx->last_command_buffer waitUntilCompleted];
        ctx->last_command_buffer = nil;
    }
    // The completion handlers run after the wait returns; every one of them
    // has to have looked at its command buffer's error.
    while (atomic_load_explicit(&ctx->handled, memory_order_acquire)
            < ctx->serial) {
        sched_yield();
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
    wait_all(ctx);
    while (n_dirty > 0) {
        int i = dirty_list[n_dirty - 1];
        memcpy(entries[i].host, [entries[i].buffer contents],
            entries[i].size);
        set_clean(i);
    }
}

static void commit(lfortran_gpu_ctx *ctx, id<MTLCommandBuffer> cb) {
    uint64_t serial = ++ctx->serial;
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
        uint64_t serial = ctx->serial + 1;
        k->serial = serial;
        id<MTLCommandBuffer> commandBuffer = [ctx->queue commandBuffer];
        id<MTLComputeCommandEncoder> encoder = [commandBuffer computeCommandEncoder];
        [encoder setComputePipelineState:k->pipeline];

        // The ranges the kernel may write first, so that one the kernel is
        // also handed to read is read from the same buffer.
        for (int i = 0; i < k->n_args; i++) {
            if (k->kind[i] != ARG_BUFFER || k->access[i] != LFORTRAN_GPU_WRITE
                    || k->sizes[i] == 0) {
                continue;
            }
            id<MTLBuffer> buffer = resident_for_write(ctx, k->ptrs[i],
                k->sizes[i]);
            entries[find_entry(k->ptrs[i])].last_use = serial;
            [encoder setBuffer:buffer offset:0 atIndex:i];
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
                    if (k->access[i] == LFORTRAN_GPU_WRITE) break;
                    id<MTLBuffer> buffer = dirty_resident(k->ptrs[i],
                        k->sizes[i]);
                    if (buffer) {
                        entries[find_entry(k->ptrs[i])].last_use = serial;
                        [encoder setBuffer:buffer offset:0 atIndex:i];
                    } else if (k->sizes[i] <= INLINE_BYTES_MAX) {
                        [encoder setBytes:k->ptrs[i] length:k->sizes[i]
                            atIndex:i];
                    } else {
                        buffer = acquire_buffer(ctx, k->sizes[i]);
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
                    id<MTLBuffer> buffer = acquire_buffer(ctx, k->sizes[i]);
                    k->scratch_buffers[i] = buffer;
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
        [encoder dispatchThreads:MTLSizeMake(total_threads, 1, 1)
            threadsPerThreadgroup:MTLSizeMake(threads_per_group, 1, 1)];
        [encoder endEncoding];
        commit(ctx, commandBuffer);
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

long long lfortran_gpu_heap_size(void) {
    return lfortran_gpu_parse_heap_size();
}
