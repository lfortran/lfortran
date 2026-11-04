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
