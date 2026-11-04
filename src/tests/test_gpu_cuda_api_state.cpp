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
        require(sync_launches.size() == 2
            && sync_launches[0] == expected_launches
            && sync_launches[1] == expected_launches,
            "no synchronization during stable alias preparation");
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
    if (argc == 2 && strcmp(argv[1], "failed-free") == 0) {
        return failed_free();
    }
    if (argc == 2 && strcmp(argv[1], "no-device") == 0) {
        return no_device();
    }
    if (argc == 2 && strcmp(argv[1], "unified") == 0) {
        return unified_memory_model();
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
    require(s.launches == 201 && s.mallocs == 6031 && s.frees == 4110,
        "per-slot reuse and deferred replacement counts");
    require(sync_launches.size() == (scratch ? 4 : 3)
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
