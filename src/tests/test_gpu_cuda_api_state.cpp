#include <array>
#include <functional>
#include <unordered_map>
#include <vector>

#include "../libasr/runtime/lfortran_gpu_cuda.cu"
#include "../../integration_tests/gpu_cuda_scratch_order.h"

// Compile the production runtime against a small, ordered host API double.
// Copies and launches wait in stream 0 until a synchronous CUDA call drains
// them. This tests runtime state/ownership, not CUDA kernels or GPU performance.
static std::unordered_map<void *, size_t> allocations;
static std::vector<std::function<void()>> pending;
static std::vector<int> sync_launches;
static int launches = 0, mallocs = 0, frees = 0;
static bool scratch = true;

static void require(bool condition, const char *message)
{
    if (!condition) {
        fprintf(stderr, "CUDA API-state assertion: %s\n", message);
        exit(1);
    }
}

static void live(const void *ptr, size_t size)
{
    auto it = allocations.find(const_cast<void *>(ptr));
    require(it != allocations.end() && it->second >= size,
        "operation needs a live allocation of sufficient size");
}

static void drain()
{
    for (auto &operation : pending) operation();
    pending.clear();
}

cudaError_t cudaSetDevice(int device)
{
    require(device == 0, "device 0");
    return cudaSuccess;
}

cudaError_t cudaMalloc(void **ptr, size_t size)
{
    require(size > 0, "positive allocation");
    *ptr = malloc(size);
    require(*ptr != nullptr, "host allocation");
    allocations[*ptr] = size;
    ++mallocs;
    return cudaSuccess;
}

cudaError_t cudaFree(void *ptr)
{
    require(pending.empty(), "no freeing memory before pending uses finish");
    require(allocations.erase(ptr) == 1, "no double free");
    free(ptr);
    ++frees;
    return cudaSuccess;
}

cudaError_t cudaDeviceSynchronize()
{
    sync_launches.push_back(launches);
    drain();
    return cudaSuccess;
}

cudaError_t cudaDeviceReset()
{
    require(pending.empty(), "reset after synchronization");
    for (const auto &allocation : allocations) free(allocation.first);
    allocations.clear();
    return cudaSuccess;
}

cudaError_t cudaMemcpyAsync(void *dst, const void *src, size_t size,
    cudaMemcpyKind kind, cudaStream_t stream)
{
    require(kind == cudaMemcpyHostToDevice && stream == nullptr,
        "asynchronous host upload on stream 0");
    std::vector<unsigned char> bytes(size);
    memcpy(bytes.data(), src, size);
    pending.push_back([dst, bytes]() {
        live(dst, bytes.size());
        memcpy(dst, bytes.data(), bytes.size());
    });
    return cudaSuccess;
}

cudaError_t cudaMemcpy(void *dst, const void *src, size_t size,
    cudaMemcpyKind kind)
{
    require(kind == cudaMemcpyDeviceToHost, "synchronous device readback");
    drain();
    live(src, size);
    memcpy(dst, src, size);
    return cudaSuccess;
}

static void device_entry() {}

cudaError_t cudaLaunchKernel(const void *entry, dim3 grid, dim3 block,
    void **args, size_t shared, cudaStream_t stream)
{
    require(entry == (const void *)device_entry, "registered device entry");
    require(grid.x == 1 && grid.y == 1 && grid.z == 1
        && block.x == 1 && block.y == 1 && block.z == 1,
        "positive unit launch geometry");
    require(shared == 0 && stream == nullptr, "default stream, no shared memory");
    // CUDA copies argument values before returning; the launch handle and
    // its scalar storage can be released while this operation is pending.
    std::array<int *, 31> buffers;
    memcpy(&buffers[0], args[0], sizeof(buffers[0]));
    for (int i = 1; i < 31; i++) {
        memcpy(&buffers[i], args[i + 1], sizeof(buffers[i]));
    }
    int sequence;
    memcpy(&sequence, args[1], sizeof(sequence));
    pending.push_back([buffers, sequence]() {
        for (int *buffer : buffers) live(buffer, sizeof(int));
        *buffers[0] = sequence;
        for (int i = 1; i < 31; i++) {
            if (scratch) *buffers[i] = sequence;
            else require(*buffers[i] == 1, "read-only upload is current");
        }
    });
    ++launches;
    return cudaSuccess;
}

const char *cudaGetErrorString(cudaError_t)
{
    return "CUDA API test double";
}

static int resident_table_order()
{
    auto *ctx = lfortran_gpu_init();
    require(ctx != nullptr, "runtime initialization");
    lfortran_gpu_register_kernel("resident_order", device_entry);
    std::array<int, 1025> output{};
    int grid[3] = {1, 1, 1}, block[3] = {1, 1, 1};
    for (int sequence = 1; sequence <= 1025; sequence++) {
        auto *k = lfortran_gpu_load_kernel(ctx, "resident_order", 14);
        int index = sequence == 1025 ? 1023 : sequence - 1;
        lfortran_gpu_set_buffer_arg(k, 0, &output[index], sizeof(int),
            LFORTRAN_GPU_WRITE);
        lfortran_gpu_set_scalar_arg(k, 1, &sequence, sizeof(sequence));
        for (int slot = 2; slot < 32; slot++) {
            if (sequence == 1025 && slot == 2) {
                // Adding this range evicts the full residency table after
                // slot 0 has already been bound to one of its old entries.
                lfortran_gpu_set_buffer_arg(k, slot, &output[1024],
                    sizeof(int), LFORTRAN_GPU_WRITE);
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
    lfortran_gpu_shutdown(ctx);
    return result;
}

int main(int argc, char **argv)
{
    if (argc == 2 && strcmp(argv[1], "resident") == 0) {
        return resident_table_order();
    }
    if (argc == 2 && strcmp(argv[1], "read-only") == 0) scratch = false;
    int result = gpu_cuda_scratch_order(device_entry, scratch);
    printf("API-state only: launches=%d, allocations=%d, deferred frees=%d, "
        "first sync after launch=%d\n", launches, mallocs, frees,
        sync_launches.empty() ? -1 : sync_launches.front());
    require(launches == 201 && mallocs == 6031 && frees == 4110,
        "per-slot reuse and deferred replacement counts");
    require(sync_launches.size() == (scratch ? 4 : 3)
        && sync_launches.front() == 200 && sync_launches[1] == 201,
        "one capacity flush before launch 201, no eager launch synchronization");
    require(allocations.empty(), "shutdown releases cached memory");
    return result;
}
