#ifndef LFORTRAN_HIP_CUDA_RUNTIME_H
#define LFORTRAN_HIP_CUDA_RUNTIME_H

// The CUDA runtime API that the LFortran GPU runtime (lfortran_gpu_cuda.c)
// uses, implemented with HIP, for `--gpu=hip` (AMD GPUs through ROCm).
//
// The GPU runtime is written against the CUDA runtime API once; this header
// adapts it to HIP, as cuda_cpu/cuda_runtime.h adapts it to the CPU
// emulation. Its directory comes first on the include path when the GPU
// runtime is compiled for HIP, so that `#include <cuda_runtime.h>` finds it.
// Only what the runtime calls is here. The header is C and C++.

#include <hip/hip_runtime_api.h>
#include <stdio.h>

// Set when the CUDA runtime API is this adapter.
#define LFORTRAN_GPU_HIP 1

#ifdef __cplusplus
extern "C" {
#endif

typedef hipError_t cudaError_t;
typedef hipMemcpyKind cudaMemcpyKind;
typedef hipDeviceAttribute_t cudaDeviceAttr;
typedef hipStream_t cudaStream_t;

static const cudaError_t cudaSuccess = hipSuccess;

static const cudaMemcpyKind cudaMemcpyHostToDevice = hipMemcpyHostToDevice;
static const cudaMemcpyKind cudaMemcpyDeviceToHost = hipMemcpyDeviceToHost;

static const cudaDeviceAttr cudaDevAttrL2CacheSize =
    hipDeviceAttributeL2CacheSize;
// Whether the GPU shares the host's physical memory (an APU such as the
// MI300A), and whether it can access memory the host allocated with malloc
// through the host's page tables (on AMD, only with HSA_XNACK=1).
static const cudaDeviceAttr cudaDevAttrIntegrated =
    hipDeviceAttributeIntegrated;
static const cudaDeviceAttr cudaDevAttrPageableMemoryAccess =
    hipDeviceAttributePageableMemoryAccess;

static inline cudaError_t cudaSetDevice(int device) {
    return hipSetDevice(device);
}
// The host waits for the GPU by sleeping until it is woken rather than by
// spinning (hipDeviceScheduleBlockingSync). On an APU such as the MI300A the
// CPU and the GPU share the power of their socket, and a host thread
// spinning while a kernel streams through memory made the kernel 3-5%
// slower. Flags are a hint, so a device that refuses them keeps its own.
static inline void lfortran_gpu_hip_block_while_waiting(void) {
    if (hipSetDeviceFlags(hipDeviceScheduleBlockingSync) != hipSuccess) {
        (void)hipGetLastError();
    }
}
static inline cudaError_t cudaGetDevice(int *device) {
    return hipGetDevice(device);
}
static inline cudaError_t cudaDeviceGetAttribute(int *value,
        cudaDeviceAttr attr, int device) {
    return hipDeviceGetAttribute(value, attr, device);
}
static inline cudaError_t cudaDeviceSynchronize(void) {
    return hipDeviceSynchronize();
}
static inline cudaError_t cudaDeviceReset(void) {
    return hipDeviceReset();
}
static inline cudaError_t cudaMalloc(void **ptr, size_t size) {
    return hipMalloc(ptr, size);
}
static inline cudaError_t cudaFree(void *ptr) {
    return hipFree(ptr);
}
static inline cudaError_t cudaMemcpy(void *dst, const void *src,
        size_t count, cudaMemcpyKind kind) {
    return hipMemcpy(dst, src, count, kind);
}
static inline cudaError_t cudaMemcpyAsync(void *dst, const void *src,
        size_t count, cudaMemcpyKind kind, cudaStream_t stream) {
    return hipMemcpyAsync(dst, src, count, kind, stream);
}
static inline cudaError_t cudaMemsetAsync(void *ptr, int value,
        size_t count, cudaStream_t stream) {
    return hipMemsetAsync(ptr, value, count, stream);
}

// The size in bytes of the GPU's last-level cache, or `l2_bytes` when no
// larger one is found. HIP reports only the L2 cache, but an MI300 has an
// Infinity Cache (MALL) of 256 MB behind its 4 MB L2 caches, one per XCD,
// and that is the cache a loop transformation has to fit what it reuses in.
// The kernel driver lists every cache of the GPU in its topology, in
// /sys/class/kfd/kfd/topology/nodes/<node>/caches/<cache>/properties, as
// lines "level <n>" and "size <KiB>"; the node of the device is the one
// whose properties give its PCI location ("location_id", bus << 8 | device
// << 3 | function, and "domain").
static inline long long lfortran_gpu_hip_last_level_cache(int device,
        long long l2_bytes) {
    long long best = l2_bytes;
    int bus = 0, dev = 0, domain = 0;
    if (hipDeviceGetAttribute(&bus, hipDeviceAttributePciBusId, device)
                != hipSuccess
            || hipDeviceGetAttribute(&dev, hipDeviceAttributePciDeviceId,
                device) != hipSuccess
            || hipDeviceGetAttribute(&domain, hipDeviceAttributePciDomainID,
                device) != hipSuccess) {
        (void)hipGetLastError();
        return best;
    }
    long long location = ((long long)bus << 8) | ((long long)dev << 3);
    const char *topology = "/sys/class/kfd/kfd/topology/nodes";
    char path[256], line[128];
    for (int node = 0; node < 256; node++) {
        snprintf(path, sizeof(path), "%s/%d/properties", topology, node);
        FILE *f = fopen(path, "r");
        if (!f) break;
        long long node_location = -1, node_domain = -1, value;
        while (fgets(line, sizeof(line), f)) {
            if (sscanf(line, "location_id %lld", &value) == 1) {
                node_location = value;
            } else if (sscanf(line, "domain %lld", &value) == 1) {
                node_domain = value;
            }
        }
        fclose(f);
        if (node_location != location || node_domain != domain) continue;
        for (int cache = 0; cache < 4096; cache++) {
            snprintf(path, sizeof(path), "%s/%d/caches/%d/properties",
                topology, node, cache);
            f = fopen(path, "r");
            if (!f) break;
            long long level = 0, size = 0;
            while (fgets(line, sizeof(line), f)) {
                if (sscanf(line, "level %lld", &value) == 1) level = value;
                else if (sscanf(line, "size %lld", &value) == 1) size = value;
            }
            fclose(f);
            if (level >= 2 && size * 1024 > best) best = size * 1024;
        }
        break;
    }
    return best;
}
static inline cudaError_t cudaLaunchKernel(const void *func, dim3 grid,
        dim3 block, void **args, size_t shared_mem, cudaStream_t stream) {
    return hipLaunchKernel(func, grid, block, args, shared_mem, stream);
}
static inline const char *cudaGetErrorString(cudaError_t error) {
    return hipGetErrorString(error);
}

#ifdef __cplusplus
}
#endif

#endif // LFORTRAN_HIP_CUDA_RUNTIME_H
