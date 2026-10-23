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
    for (int kind = 0; kind < 5; kind++) {
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
