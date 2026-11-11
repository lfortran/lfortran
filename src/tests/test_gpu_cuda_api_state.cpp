#include <array>
#include <vector>

#include "../libasr/runtime/lfortran_gpu_cuda.c"
#include "cuda_cpu_device.h"
#include "cuda_cpu_emulator.h"
#include "cuda_api_test_double/gpu_cuda_scratch_order.h"

// Compile the production runtime against the CPU emulation of the CUDA
// runtime API that `--gpu=cuda_cpu` uses, whose statistics show how the
// runtime uses the API. The emulated stream runs copies and launches when they
// are issued; work issued after the last synchronizing call is what would
// still be pending on a device. The kernels are thunks, as in the emulation.
// This tests runtime state/ownership, not CUDA kernels or GPU performance.
static std::vector<int> sync_launches;
static bool scratch = true;

static void require(bool condition, const char *message)
{
    if (!condition) {
        fprintf(stderr, "CUDA API-state assertion: %s\n", message);
        exit(1);
    }
}

static const lfortran_cuda_cpu_stats &stats()
{
    return *lfortran_cuda_cpu_get_stats();
}

static void live(const void *ptr, size_t size)
{
    void *base;
    size_t allocated;
    require(lfortran_cuda_cpu_find_allocation(ptr, &base, &allocated)
        && base == ptr && allocated >= size,
        "operation needs a live allocation of sufficient size");
}

static void on_synchronize()
{
    if (sync_launches.size() == 16) {
        fprintf(stderr, "API-state sync guard: 16 synchronizations, %llu "
            "launches\n", stats().launches);
        require(false, "argument preparation must converge");
    }
    sync_launches.push_back((int)stats().launches);
}

// Every launch has a unit geometry.
static void unit_launch()
{
    require(gridDim.x == 1 && gridDim.y == 1 && gridDim.z == 1
        && blockDim.x == 1 && blockDim.y == 1 && blockDim.z == 1,
        "positive unit launch geometry");
}

template <typename T>
static T arg(void **args, int i)
{
    T value;
    memcpy(&value, args[i], sizeof(value));
    return value;
}

static void device_entry(void **args)
{
    unit_launch();
    std::array<int *, 31> buffers;
    buffers[0] = arg<int *>(args, 0);
    for (int i = 1; i < 31; i++) buffers[i] = arg<int *>(args, i + 1);
    int sequence = arg<int>(args, 1);
    for (int *buffer : buffers) live(buffer, sizeof(int));
    *buffers[0] = sequence;
    for (int i = 1; i < 31; i++) {
        if (scratch) *buffers[i] = sequence;
        else require(*buffers[i] == 1, "read-only upload is current");
    }
}

static void write_aliases(int32_t *wide, int32_t *narrow, int32_t *reader,
    bool reverse)
{
    live(wide, 2 * sizeof(int32_t));
    wide[0] = 201;
    if (narrow) {
        live(narrow, sizeof(int32_t));
        narrow[0] += 1;
    }
    if (reverse) wide[1] = 83;
    if (reader) {
        live(reader, sizeof(int32_t));
        wide[1] = reader[0];
    }
}

static void alias_entry(void **args)
{
    unit_launch();
    write_aliases(arg<int32_t *>(args, 0), arg<int32_t *>(args, 1), nullptr,
        false);
}

static void alias_reverse_entry(void **args)
{
    unit_launch();
    write_aliases(arg<int32_t *>(args, 1), arg<int32_t *>(args, 0), nullptr,
        true);
}

static void alias_read_entry(void **args)
{
    unit_launch();
    write_aliases(arg<int32_t *>(args, 0), arg<int32_t *>(args, 1),
        arg<int32_t *>(args, 2), false);
}

static void alias_discard_entry(void **args)
{
    unit_launch();
    int32_t *wide = arg<int32_t *>(args, 0);
    int32_t *narrow = arg<int32_t *>(args, 1);
    live(wide, 2 * sizeof(int32_t));
    live(narrow, sizeof(int32_t));
    require(wide == narrow, "read alias shares the writable resident");
    int32_t value = narrow[0];
    wide[0] = value + 1;
    wide[1] = value + 2;
}

static void discard_fill_entry(void **args)
{
    unit_launch();
    int32_t *out = arg<int32_t *>(args, 0);
    int32_t value = arg<int32_t>(args, 1);
    live(out, 2 * sizeof(int32_t));
    require(out[0] == -1 && out[1] == -1,
        "a discarded range holds garbage at every launch");
    out[0] = value;
    out[1] = value + 1;
}

static void fill_entry(void **args)
{
    unit_launch();
    int32_t *out = arg<int32_t *>(args, 0);
    live(out, 1000 * sizeof(int32_t));
    out[0] = arg<int32_t>(args, 1);
}

// out[chunk] = chunk + the sum of the 64 read-only values.
static void read_once_entry(void **args)
{
    unit_launch();
    int32_t *out = arg<int32_t *>(args, 0);
    int32_t *in = arg<int32_t *>(args, 1);
    int32_t chunk = arg<int32_t>(args, 2);
    live(out, 8 * sizeof(int32_t));
    live(in, 64 * sizeof(int32_t));
    int32_t sum = 0;
    for (int i = 0; i < 64; i++) sum += in[i];
    out[chunk] = chunk + sum;
}

static kernel_func_t entry(void (*thunk)(void **))
{
    return (kernel_func_t)thunk;
}

// The runtime allocates only positive sizes, uploads host memory
// asynchronously and reads device memory back synchronously, naming those
// directions, all on the default stream (the emulation rejects other
// streams), never frees memory a pending launch may still use, and does not
// misuse the API.
static void check_api_use()
{
    const lfortran_cuda_cpu_stats &s = stats();
    require(s.empty_mallocs == 0, "positive allocation");
    for (int kind = 0; kind <= cudaMemcpyDefault; kind++) {
        require(kind == cudaMemcpyHostToDevice || s.async_copies[kind] == 0,
            "asynchronous host upload on stream 0");
        require(kind == cudaMemcpyDeviceToHost || s.copies[kind] == 0,
            "synchronous device readback");
    }
    require(s.implicit_synchronizations == 0,
        "no freeing memory before pending uses finish");
    require(s.errors == 0, "no failed CUDA API call");
}

static int resident_table_order(int output_access)
{
    auto *ctx = lfortran_gpu_init();
    require(ctx != nullptr, "runtime initialization");
    lfortran_gpu_register_kernel("resident_order", entry(device_entry));
    std::array<int, 1025> output{};
    int grid[3] = {1, 1, 1}, block[3] = {1, 1, 1};
    for (int sequence = 1; sequence <= 1025; sequence++) {
        auto *k = lfortran_gpu_load_kernel(ctx, "resident_order", 14);
        int index = sequence == 1025 ? 1023 : sequence - 1;
        lfortran_gpu_set_buffer_arg(k, 0, &output[index], sizeof(int),
            output_access);
        lfortran_gpu_set_scalar_arg(k, 1, &sequence, sizeof(sequence));
        for (int slot = 2; slot < 32; slot++) {
            if (sequence == 1025 && slot == 2) {
                // Adding this range evicts the full residency table after
                // slot 0 has already been bound to one of its old entries.
                lfortran_gpu_set_buffer_arg(k, slot, &output[1024],
                    sizeof(int), output_access);
            } else {
                lfortran_gpu_set_scratch_arg(k, slot, sizeof(int));
            }
        }
        lfortran_gpu_launch(ctx, k, grid, block);
        lfortran_gpu_release_kernel(k);
    }
    lfortran_gpu_sync(ctx);
    printf("resident table: output[1023]=%d, output[1024]=%d, expected=1025\n",
        output[1023], output[1024]);
    int result = output[1023] == 1025 && output[1024] == 1025 ? 0 : 1;
    for (int i = 0; i < 1023; i++) {
        require(output[i] == i + 1, "evicted earlier writes reach the host");
    }
    require(sync_launches.size() == 2 && sync_launches[0] == 1024
        && sync_launches[1] == 1025, "one residency-table flush");
    if (output_access == LFORTRAN_GPU_WRITE_DISCARD) {
        require(stats().async_copies[cudaMemcpyHostToDevice] == 0,
            "discarded residents need no host upload");
    }
    lfortran_gpu_shutdown(ctx);
    check_api_use();
    return result;
}

// A range the kernel discards holds 0xff bytes at every launch: in fresh
// device memory, in the resident a pending launch wrote, and in the buffer
// it keeps after a synchronization.
static int discard_fill()
{
    auto *ctx = lfortran_gpu_init();
    require(ctx != nullptr, "runtime initialization");
    lfortran_gpu_register_kernel("discard_fill", entry(discard_fill_entry));
    int32_t out[2] = {1, 2};
    int grid[3] = {1, 1, 1}, block[3] = {1, 1, 1};
    for (int32_t value = 10; value <= 30; value += 10) {
        auto *k = lfortran_gpu_load_kernel(ctx, "discard_fill", 12);
        lfortran_gpu_set_buffer_arg(k, 0, out, sizeof(out),
            LFORTRAN_GPU_WRITE_DISCARD);
        lfortran_gpu_set_scalar_arg(k, 1, &value, sizeof(value));
        lfortran_gpu_launch(ctx, k, grid, block);
        lfortran_gpu_release_kernel(k);
        if (value == 20) lfortran_gpu_sync(ctx);
    }
    lfortran_gpu_sync(ctx);
    printf("discard fill: out=%d %d, memsets=%llu\n", out[0], out[1],
        stats().async_memsets);
    require(out[0] == 30 && out[1] == 31, "the last launch's values return");
    require(stats().launches == 3 && stats().async_memsets == 3,
        "one fill per launch");
    require(stats().async_copies[cudaMemcpyHostToDevice] == 0,
        "discarded ranges need no host upload");
    lfortran_gpu_shutdown(ctx);
    check_api_use();
    return 0;
}

// The chunks of a launch that runs in chunks launch one kernel handle once
// each, handing over only their bounds again (device_launch_expand.cpp): a
// read-only buffer is copied in at the first chunk only, and again once it
// is set again, or once another handle of the kernel has filled the memory
// of its slot in between.
static int read_once()
{
    auto *ctx = lfortran_gpu_init();
    require(ctx != nullptr, "runtime initialization");
    lfortran_gpu_register_kernel("read_once", entry(read_once_entry));
    std::array<int32_t, 64> a, b;
    for (int i = 0; i < 64; i++) {
        a[i] = i + 1;
        b[i] = 1000;
    }
    const int32_t sum_a = 64 * 65 / 2, sum_b = 64 * 1000;
    std::array<int32_t, 8> out{}, other{};
    int grid[3] = {1, 1, 1}, block[3] = {1, 1, 1};
    auto h2d = []() { return stats().async_copies[cudaMemcpyHostToDevice]; };
    auto *k = lfortran_gpu_load_kernel(ctx, "read_once", 9);
    lfortran_gpu_set_buffer_arg(k, 0, out.data(), sizeof(out),
        LFORTRAN_GPU_WRITE);
    lfortran_gpu_set_buffer_arg(k, 1, a.data(), sizeof(a), LFORTRAN_GPU_READ);
    for (int32_t chunk = 0; chunk < 3; chunk++) {
        lfortran_gpu_set_scalar_arg(k, 2, &chunk, sizeof(chunk));
        lfortran_gpu_launch(ctx, k, grid, block);
    }
    printf("read once: %llu uploads for 3 launches\n", h2d());
    require(h2d() == 2, "the chunks upload each buffer once");
    lfortran_gpu_set_buffer_arg(k, 1, a.data(), sizeof(a), LFORTRAN_GPU_READ);
    int32_t chunk = 3;
    lfortran_gpu_set_scalar_arg(k, 2, &chunk, sizeof(chunk));
    lfortran_gpu_launch(ctx, k, grid, block);
    require(h2d() == 3, "a buffer set again is uploaded again");
    // Another handle of the kernel fills the slot memory with `b`.
    auto *k2 = lfortran_gpu_load_kernel(ctx, "read_once", 9);
    lfortran_gpu_set_buffer_arg(k2, 0, other.data(), sizeof(other),
        LFORTRAN_GPU_WRITE);
    lfortran_gpu_set_buffer_arg(k2, 1, b.data(), sizeof(b), LFORTRAN_GPU_READ);
    chunk = 0;
    lfortran_gpu_set_scalar_arg(k2, 2, &chunk, sizeof(chunk));
    lfortran_gpu_launch(ctx, k2, grid, block);
    require(h2d() == 5, "another handle uploads its own buffers");
    chunk = 4;
    lfortran_gpu_set_scalar_arg(k, 2, &chunk, sizeof(chunk));
    lfortran_gpu_launch(ctx, k, grid, block);
    require(h2d() == 6, "a slot another handle filled is uploaded again");
    lfortran_gpu_sync(ctx);
    for (int32_t i = 0; i < 5; i++) {
        require(out[i] == i + sum_a, "every launch reads its own buffer");
    }
    require(other[0] == sum_b, "the other handle reads its own buffer");
    lfortran_gpu_release_kernel(k);
    lfortran_gpu_release_kernel(k2);
    lfortran_gpu_shutdown(ctx);
    check_api_use();
    return 0;
}

// In the copy model a range of at least KEEP_READ_MIN_BYTES that launches
// only read keeps its copy in device memory until the next wait: the
// launches of a time loop, which hand the buffer over every time, copy it in
// once. After lfortran_gpu_wait or lfortran_gpu_sync the host may have
// changed it, and the next launch copies it again. An entry no launch used
// in two wait periods is forgotten and its memory freed.
static int keep_reads()
{
    auto *ctx = lfortran_gpu_init();
    require(ctx != nullptr, "runtime initialization");
    lfortran_gpu_register_kernel("keep_reads", entry(read_once_entry));
    std::vector<int32_t> a(KEEP_READ_MIN_BYTES / sizeof(int32_t) * 2);
    for (size_t i = 0; i < a.size(); i++) a[i] = 1;
    std::array<int32_t, 8> out{};
    int grid[3] = {1, 1, 1}, block[3] = {1, 1, 1};
    auto h2d = []() { return stats().async_copies[cudaMemcpyHostToDevice]; };
    auto launch = [&](int32_t chunk) {
        auto *k = lfortran_gpu_load_kernel(ctx, "keep_reads", 10);
        lfortran_gpu_set_buffer_arg(k, 0, out.data(), sizeof(out),
            LFORTRAN_GPU_WRITE);
        lfortran_gpu_set_buffer_arg(k, 1, a.data(), a.size() * sizeof(int32_t),
            LFORTRAN_GPU_READ);
        lfortran_gpu_set_scalar_arg(k, 2, &chunk, sizeof(chunk));
        lfortran_gpu_launch(ctx, k, grid, block);
        lfortran_gpu_release_kernel(k);
    };
    auto start = h2d();
    for (int32_t i = 0; i < 3; i++) launch(i);
    printf("keep reads: %llu uploads for 3 launches\n", h2d() - start);
    require(h2d() - start == 2, "a kept range is uploaded once");
    lfortran_gpu_wait(ctx);
    for (size_t i = 0; i < a.size(); i++) a[i] = 2;
    launch(3);
    require(h2d() - start == 3, "after a wait the range is uploaded again");
    lfortran_gpu_sync(ctx);
    require(out[0] == 64 && out[2] == 66 && out[3] == 131,
        "every launch reads the values the host last wrote");
    for (size_t i = 0; i < a.size(); i++) a[i] = 3;
    launch(4);
    lfortran_gpu_sync(ctx);
    require(out[4] == 196, "after a sync the range is uploaded again");
    auto allocations = stats().live_allocations;
    lfortran_gpu_wait(ctx);
    lfortran_gpu_wait(ctx);
    lfortran_gpu_sync(ctx);
    require(stats().live_allocations == allocations,
        "an unused entry gives its memory to the cache, not to the driver");
    lfortran_gpu_shutdown(ctx);
    check_api_use();
    return 0;
}

// lfortran_gpu_keep right before a wait keeps the copies inside the range it
// names fresh past that wait (the host does not change the range until the
// launches after it read it); a copy only partly inside, or one not named,
// is uploaded again. The next wait without it ends the copy as before.
static int keep_across_waits()
{
    auto *ctx = lfortran_gpu_init();
    require(ctx != nullptr, "runtime initialization");
    lfortran_gpu_register_kernel("keep_across", entry(read_once_entry));
    std::vector<int32_t> a(KEEP_READ_MIN_BYTES / sizeof(int32_t) * 2, 1);
    size_t bytes = a.size() * sizeof(int32_t);
    std::array<int32_t, 8> out{};
    int grid[3] = {1, 1, 1}, block[3] = {1, 1, 1};
    auto h2d = []() { return stats().async_copies[cudaMemcpyHostToDevice]; };
    // A launch uploads `out`, which it writes, unless it is still on the
    // device (no sync since the last launch): the uploads of `a` are those
    // beyond that.
    auto launch = [&](int32_t chunk, unsigned long long out_uploads = 1) {
        auto before = h2d();
        auto *k = lfortran_gpu_load_kernel(ctx, "keep_across", 11);
        lfortran_gpu_set_buffer_arg(k, 0, out.data(), sizeof(out),
            LFORTRAN_GPU_WRITE);
        lfortran_gpu_set_buffer_arg(k, 1, a.data(), bytes,
            LFORTRAN_GPU_READ);
        lfortran_gpu_set_scalar_arg(k, 2, &chunk, sizeof(chunk));
        lfortran_gpu_launch(ctx, k, grid, block);
        lfortran_gpu_release_kernel(k);
        return h2d() - before - out_uploads;
    };
    require(launch(0) == 1, "the first launch uploads the range");
    lfortran_gpu_sync(ctx);
    require(launch(1) == 1, "after a sync the range is uploaded again");
    lfortran_gpu_keep(ctx, a.data(), bytes);
    lfortran_gpu_sync(ctx);
    require(launch(2) == 0, "a kept range is not uploaded again");
    lfortran_gpu_keep(ctx, a.data() + 1, bytes - sizeof(int32_t));
    lfortran_gpu_sync(ctx);
    require(launch(3) == 1, "a copy partly outside the range is not kept");
    lfortran_gpu_keep(ctx, a.data(), bytes);
    lfortran_gpu_wait(ctx);
    require(launch(4, 0) == 0, "a wait keeps it as a sync does");
    lfortran_gpu_sync(ctx);
    for (size_t i = 0; i < a.size(); i++) a[i] = 2;
    require(launch(5) == 1, "the next sync ends the copy");
    lfortran_gpu_sync(ctx);
    require(out[2] == 66 && out[3] == 67 && out[4] == 68 && out[5] == 133,
        "every launch reads the values the host last wrote");
    lfortran_gpu_shutdown(ctx);
    check_api_use();
    return 0;
}

// A deferred free that fails stops the program with the CUDA error, here
// for memory that is not a device allocation.
static int failed_free()
{
    auto *ctx = lfortran_gpu_init();
    require(ctx != nullptr, "runtime initialization");
    static int not_device_memory;
    free_later(&not_device_memory);
    lfortran_gpu_sync(ctx);
    fprintf(stderr, "a failed cudaFree was ignored\n");
    return 1;
}

// Syncing with nothing issued since the last sync does not wait for the
// device, and the device memory of a host array that is gone is reused by the
// next one instead of being freed and allocated again.
static int idle_sync_and_pool()
{
    auto *ctx = lfortran_gpu_init();
    require(ctx != nullptr, "runtime initialization");
    lfortran_gpu_register_kernel("idle_sync", entry(fill_entry));
    int grid[3] = {1, 1, 1}, block[3] = {1, 1, 1};
    for (int i = 0; i < 10; i++) lfortran_gpu_sync(ctx);
    require(stats().synchronizations == 0, "no work, no waiting");
    unsigned long long mallocs_before = stats().mallocs;
    for (int32_t i = 0; i < 40; i++) {
        std::vector<int32_t> out(1000);
        auto *k = lfortran_gpu_load_kernel(ctx, "idle_sync", 9);
        lfortran_gpu_set_buffer_arg(k, 0, out.data(), out.size() * 4,
            LFORTRAN_GPU_WRITE);
        lfortran_gpu_set_scalar_arg(k, 1, &i, sizeof(i));
        lfortran_gpu_launch(ctx, k, grid, block);
        lfortran_gpu_release_kernel(k);
        lfortran_gpu_discard(ctx, out.data(), out.size() * 4);
        lfortran_gpu_sync(ctx);
        for (int j = 0; j < 5; j++) lfortran_gpu_sync(ctx);
        sync_launches.clear();
    }
    printf("idle sync: %llu synchronizations, %llu allocations, %llu frees "
        "for 40 launches\n", stats().synchronizations,
        stats().mallocs - mallocs_before, stats().frees);
    require(stats().synchronizations == 40, "one wait for each launch");
    require(stats().mallocs - mallocs_before <= 4,
        "the memory of a finished array serves the next one");
    require(stats().frees == 0, "freed memory stays in the cache");
    lfortran_gpu_shutdown(ctx);
    check_api_use();
    return 0;
}

// The unified memory model (LFORTRAN_GPU_MEMORY=unified): a kernel uses a
// range it writes, and a range it only reads that is larger than
// UNIFIED_COPY_MAX_BYTES, in place; a smaller range it only reads is still
// copied. No launch waits for its kernel: before the host changes a range a
// launch reads in place, lfortran_gpu_wait waits for it, and returns at
// once when no pending launch reads host memory in place. Discarding a
// range a pending launch writes in place waits for it, since the host frees
// that memory. Nothing is ever copied back.
static int32_t unified_out[2];
static int32_t unified_small[4];
static std::vector<int32_t> unified_large(64 * 1024);

static void unified_entry(void **args)
{
    unit_launch();
    int32_t *out = arg<int32_t *>(args, 0);
    int32_t *small = arg<int32_t *>(args, 1);
    int32_t *large = arg<int32_t *>(args, 2);
    int32_t sequence = arg<int32_t>(args, 3);
    require(out == unified_out, "a written range is used in place");
    require(small != unified_small, "a small read-only range is copied");
    live(small, sizeof(unified_small));
    require(large == nullptr || large == unified_large.data(),
        "a large read-only range is used in place");
    out[0] = small[0] + (large ? large[unified_large.size() - 1] : 0)
        + sequence;
}

// CTest runs this case with LFORTRAN_GPU_MEMORY=unified.
static int unified_memory_model()
{
    auto *ctx = lfortran_gpu_init();
    require(ctx != nullptr, "runtime initialization");
    require(unified_memory, "LFORTRAN_GPU_MEMORY=unified selects the model");
    lfortran_gpu_register_kernel("unified", entry(unified_entry));
    int grid[3] = {1, 1, 1}, block[3] = {1, 1, 1};
    unified_small[0] = 10;
    unified_large.back() = 100;
    for (int32_t sequence = 1; sequence <= 2; sequence++) {
        size_t syncs = sync_launches.size();
        auto *k = lfortran_gpu_load_kernel(ctx, "unified", 7);
        lfortran_gpu_set_buffer_arg(k, 0, unified_out, sizeof(unified_out),
            LFORTRAN_GPU_WRITE);
        lfortran_gpu_set_buffer_arg(k, 1, unified_small,
            sizeof(unified_small), LFORTRAN_GPU_READ);
        // Only the first launch reads the large range.
        lfortran_gpu_set_buffer_arg(k, 2, unified_large.data(),
            sequence == 1 ? unified_large.size() * sizeof(int32_t) : 0,
            LFORTRAN_GPU_READ);
        lfortran_gpu_set_scalar_arg(k, 3, &sequence, sizeof(sequence));
        lfortran_gpu_launch(ctx, k, grid, block);
        lfortran_gpu_release_kernel(k);
        require(sync_launches.size() == syncs, "a launch does not wait");
        lfortran_gpu_wait(ctx);
        require(sync_launches.size() == syncs + (sequence == 1 ? 1 : 0),
            "only a launch that reads host memory in place is waited for");
        lfortran_gpu_wait(ctx);
        require(sync_launches.size() == syncs + (sequence == 1 ? 1 : 0),
            "a second wait has nothing to wait for");
        // The host may now change what the launch only reads.
        unified_small[0] = 20;
        unified_large.back() = 200;
        if (sequence == 1) {
            lfortran_gpu_sync(ctx);
            require(unified_out[0] == 10 + 100 + 1,
                "the kernel read the large range before the host changed it");
        }
    }
    size_t syncs = sync_launches.size();
    lfortran_gpu_discard(ctx, unified_out, sizeof(unified_out));
    require(sync_launches.size() == syncs + 1,
        "discarding a range a pending launch writes in place waits");
    lfortran_gpu_sync(ctx);
    printf("unified memory: out=%d, uploads=%llu, readbacks=%llu, "
        "allocations=%llu\n", unified_out[0],
        stats().async_copies[cudaMemcpyHostToDevice],
        stats().copies[cudaMemcpyDeviceToHost], stats().mallocs);
    require(unified_out[0] == 20 + 2, "the last launch's value is in place");
    require(stats().async_copies[cudaMemcpyHostToDevice] == 2,
        "only the small read-only range is uploaded, at each launch");
    require(stats().copies[cudaMemcpyDeviceToHost] == 0,
        "nothing is copied back");
    require(stats().mallocs == 1, "device memory only for the small copy");
    lfortran_gpu_shutdown(ctx);
    check_api_use();
    return 0;
}

// The managed memory model (LFORTRAN_GPU_MEMORY=managed): an allocation of
// at least MANAGED_MIN_BYTES is managed memory, which a kernel uses in place,
// and the first launch that uses it prefetches all of it to the device; later
// launches do not. The prefetch is the cudaMemPrefetchAsync of the version of
// the CUDA runtime API the runtime is compiled against (CUDART_VERSION):
// CTest runs this case against the emulated CUDA 13 API and, in
// test_gpu_cuda_api_state_cuda12, against the CUDA 12 one.
static void *host_alloc(void *, int64_t size)
{
    return malloc((size_t)size);
}

static void *host_realloc(void *, void *ptr, int64_t size)
{
    return realloc(ptr, (size_t)size);
}

static void host_dealloc(void *, void *ptr)
{
    free(ptr);
}

static void managed_entry(void **args)
{
    unit_launch();
    int32_t *data = arg<int32_t *>(args, 0);
    int32_t sequence = arg<int32_t>(args, 1);
    data[0] += sequence;
}

// CTest runs this case with LFORTRAN_GPU_MEMORY=managed.
static int managed_memory_model()
{
    // A unit test links no program allocator for the managed one to replace.
    previous_allocator = {host_alloc, host_realloc, host_dealloc, nullptr};
    auto *ctx = lfortran_gpu_init();
    require(ctx != nullptr, "runtime initialization");
    require(memory_model == MEMORY_MANAGED,
        "LFORTRAN_GPU_MEMORY=managed selects the model");
    auto *data = (int32_t *)managed_alloc(nullptr,
        MANAGED_PREFETCH_MIN_BYTES);
    require(data != nullptr
        && managed_contains(data, MANAGED_PREFETCH_MIN_BYTES),
        "a large allocation is managed memory");
    require(stats().memory_advice == 0,
        "a large allocation is prefetched, not advised");
    data[0] = 10;
    lfortran_gpu_register_kernel("managed", entry(managed_entry));
    int grid[3] = {1, 1, 1}, block[3] = {1, 1, 1};
    for (int32_t sequence = 1; sequence <= 2; sequence++) {
        auto *k = lfortran_gpu_load_kernel(ctx, "managed", 7);
        lfortran_gpu_set_buffer_arg(k, 0, data, sizeof(int32_t),
            LFORTRAN_GPU_WRITE);
        lfortran_gpu_set_scalar_arg(k, 1, &sequence, sizeof(sequence));
        lfortran_gpu_launch(ctx, k, grid, block);
        lfortran_gpu_release_kernel(k);
        require(stats().device_prefetches == 1,
            "the first launch that uses the allocation prefetches it");
    }
    lfortran_gpu_sync(ctx);
    require(data[0] == 10 + 1 + 2, "the kernels wrote the memory in place");
    require(stats().host_prefetches == 0, "nothing is prefetched to the host");
    require(stats().async_copies[cudaMemcpyHostToDevice] == 0
        && stats().copies[cudaMemcpyDeviceToHost] == 0,
        "managed memory is not copied");
    // A freed block goes to a cache, and is handed out again once the host
    // has waited for the device since.
    unsigned long long frees = stats().frees;
    unsigned long long mallocs = stats().mallocs;
    managed_dealloc(nullptr, data);
    require(stats().frees == frees,
        "a freed managed block goes to the cache, not to the driver");
    // The device has no work pending, so the block is handed out again.
    auto *again = (int32_t *)managed_alloc(nullptr, MANAGED_PREFETCH_MIN_BYTES);
    require(again == data && stats().mallocs == mallocs,
        "an allocation of the size of a cached block reuses it");
    managed_dealloc(nullptr, again);
    auto *larger = (int32_t *)managed_alloc(nullptr, 2 * MANAGED_PREFETCH_MIN_BYTES);
    require(larger != nullptr && larger != data
        && managed_contains(larger, 2 * MANAGED_PREFETCH_MIN_BYTES),
        "a larger allocation does not reuse a smaller cached block");
    managed_dealloc(nullptr, larger);
    managed_cache_flush();
    require(stats().frees == frees + 2, "flushing the cache frees the blocks");
    // A small allocation is managed memory too, with the device as its
    // preferred location and mapped for the host, and no prefetch.
    auto *small = (int32_t *)managed_alloc(nullptr, MANAGED_MIN_BYTES);
    require(small != nullptr && managed_contains(small, MANAGED_MIN_BYTES),
        "a small allocation is managed memory");
    require(stats().memory_advice == 2,
        "a small allocation is advised: preferred on the device, mapped for the host");
    small[0] = 5;
    auto *k = lfortran_gpu_load_kernel(ctx, "managed", 7);
    int32_t one = 1;
    lfortran_gpu_set_buffer_arg(k, 0, small, sizeof(int32_t),
        LFORTRAN_GPU_WRITE);
    lfortran_gpu_set_scalar_arg(k, 1, &one, sizeof(one));
    lfortran_gpu_launch(ctx, k, grid, block);
    lfortran_gpu_release_kernel(k);
    lfortran_gpu_sync(ctx);
    require(small[0] == 6, "the kernel wrote the small memory in place");
    require(stats().device_prefetches == 1,
        "the launch that uses a small allocation does not prefetch it");
    managed_dealloc(nullptr, small);
    lfortran_gpu_shutdown(ctx);
    check_api_use();
    // Only once every check passed, since CTest matches this line.
    printf("managed memory passed on the CUDA runtime API %d: device "
        "prefetches=%llu\n", CUDART_VERSION, stats().device_prefetches);
    return 0;
}

// CTest runs this case with LFORTRAN_GPU_MEMORY=managed. A procedure that
// allocates many temporaries of one size (one for each layer of a model)
// frees them all without a cudaFree, which would wait for the device: the
// blocks are kept for the next call.
static int managed_pool_capacity()
{
    previous_allocator = {host_alloc, host_realloc, host_dealloc, nullptr};
    auto *ctx = lfortran_gpu_init();
    require(ctx != nullptr, "runtime initialization");
    require(memory_model == MEMORY_MANAGED,
        "LFORTRAN_GPU_MEMORY=managed selects the model");
    enum { TEMPORARIES = 300 };
    static void *temporaries[TEMPORARIES];
    for (int i = 0; i < TEMPORARIES; i++) {
        temporaries[i] = managed_alloc(nullptr, MANAGED_MIN_BYTES);
        require(temporaries[i] != nullptr, "temporary allocation");
    }
    unsigned long long frees = stats().frees;
    for (int i = 0; i < TEMPORARIES; i++) {
        managed_dealloc(nullptr, temporaries[i]);
    }
    require(stats().frees == frees, "freed temporaries are kept, not freed");
    require(stats().implicit_synchronizations == 0,
        "freeing temporaries does not wait for the device");
    lfortran_gpu_shutdown(ctx);
    check_api_use();
    printf("managed pool capacity passed on the CUDA runtime API %d\n",
        CUDART_VERSION);
    return 0;
}

// Without a device the runtime stops at initialization: it must not skip
// the launch and leave the program with the values never computed.
static int no_device()
{
    lfortran_cuda_cpu_set_device_count(0);
    auto *ctx = lfortran_gpu_init();
    fprintf(stderr, "the runtime initialization returned without a device\n");
    return ctx == nullptr ? 1 : 0;
}

int main(int argc, char **argv)
{
    lfortran_cuda_cpu_set_synchronize_hook(on_synchronize);
    bool discard = argc > 1 && strcmp(argv[1], "discard") == 0;
    if (discard) {
        --argc;
        ++argv;
    }
    int output_access = discard ? LFORTRAN_GPU_WRITE_DISCARD : LFORTRAN_GPU_WRITE;
    if (argc == 3 && strcmp(argv[1], "alias") == 0) {
        int result = gpu_cuda_alias_order(entry(alias_entry),
            entry(alias_reverse_entry), entry(alias_read_entry),
            entry(alias_discard_entry), argv[2]);
        int expected_launches = strcmp(argv[2], "repeat") == 0
            || strcmp(argv[2], "discard-repeat") == 0 ? 2 : 1;
        unsigned long long launches = stats().launches;
        printf("API-state aliases: launches=%llu, synchronizations=%zu\n",
            launches, sync_launches.size());
        require(launches == (unsigned long long)expected_launches,
            "each requested dispatch runs once");
        require(sync_launches.size() == 1
            && sync_launches[0] == expected_launches,
            "no synchronization during stable alias preparation, and none "
            "at shutdown when the device is idle");
        if (strncmp(argv[2], "discard-", 8) == 0) {
            require(stats().async_copies[cudaMemcpyHostToDevice] == 1,
                "aliased discard falls back to one upload");
        }
        require(stats().live_allocations == 0,
            "shutdown releases alias memory");
        check_api_use();
        return result;
    }
    if (argc == 2 && strcmp(argv[1], "discard-fill") == 0) {
        return discard_fill();
    }
    if (argc == 2 && strcmp(argv[1], "read-once") == 0) {
        return read_once();
    }
    if (argc == 2 && strcmp(argv[1], "keep-reads") == 0) {
        return keep_reads();
    }
    if (argc == 2 && strcmp(argv[1], "keep-across-waits") == 0) {
        return keep_across_waits();
    }
    if (argc == 2 && strcmp(argv[1], "idle-sync-pool") == 0) {
        return idle_sync_and_pool();
    }
    if (argc == 2 && strcmp(argv[1], "failed-free") == 0) {
        return failed_free();
    }
    if (argc == 2 && strcmp(argv[1], "no-device") == 0) {
        return no_device();
    }
    if (argc == 2 && strcmp(argv[1], "unified") == 0) {
        return unified_memory_model();
    }
    if (argc == 2 && strcmp(argv[1], "managed-pool") == 0) {
        return managed_pool_capacity();
    }
    if (argc == 2 && strcmp(argv[1], "managed") == 0) {
        return managed_memory_model();
    }
    if (argc == 2 && strcmp(argv[1], "resident") == 0) {
        return resident_table_order(output_access);
    }
    if (argc == 2 && strcmp(argv[1], "read-only") == 0) scratch = false;
    int result = gpu_cuda_scratch_order(entry(device_entry), scratch,
        output_access);
    const lfortran_cuda_cpu_stats &s = stats();
    printf("API-state only: launches=%llu, allocations=%llu, deferred "
        "frees=%llu, first sync after launch=%lld\n", s.launches, s.mallocs,
        s.frees, sync_launches.empty() ? -1LL
            : sync_launches.front());
    // With a scratch buffer the read-back syncs once more, and the entry of
    // `output`, which no launch has used in the two wait periods before, is
    // forgotten there and its memory freed.
    require(s.launches == 201 && s.mallocs == 6017
        && s.frees == (scratch ? 3073u : 3072u),
        "per-slot reuse and deferred replacement counts");
    // The syncs of the read-back and of the shutdown find the device idle.
    require(sync_launches.size() == 2
        && sync_launches.front() == 200 && sync_launches[1] == 201,
        "one capacity flush before launch 201, no eager launch synchronization");
    require(s.live_allocations == 0, "shutdown releases cached memory");
    if (discard && scratch) {
        require(s.async_copies[cudaMemcpyHostToDevice] == 0,
            "discard remains upload-free across a retry");
    }
    check_api_use();
    return result;
}
