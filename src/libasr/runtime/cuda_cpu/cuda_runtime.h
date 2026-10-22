#ifndef LFORTRAN_CUDA_CPU_RUNTIME_H
#define LFORTRAN_CUDA_CPU_RUNTIME_H

// The CUDA runtime API as the CPU emulation (`--gpu=cuda_cpu`) provides it.
//
// Only the part of the API that the LFortran CUDA GPU runtime
// (lfortran_gpu_cuda.c) uses is here, written for LFortran; it is not the
// header of a CUDA toolkit. Its directory comes first on the include path when
// the GPU runtime is compiled for the emulation, so that `#include
// <cuda_runtime.h>` finds it, and cuda_cpu_runtime.c implements it on the host.
// The header is C and C++.

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum cudaError {
    cudaSuccess = 0,
    cudaErrorInvalidValue = 1,
    cudaErrorMemoryAllocation = 2,
    cudaErrorInvalidConfiguration = 9,
    cudaErrorInvalidDevice = 101,
    cudaErrorInvalidResourceHandle = 400
} cudaError_t;

typedef enum cudaMemcpyKind {
    cudaMemcpyHostToHost = 0,
    cudaMemcpyHostToDevice = 1,
    cudaMemcpyDeviceToHost = 2,
    cudaMemcpyDeviceToDevice = 3,
    // The direction follows from where the two pointers are.
    cudaMemcpyDefault = 4
} cudaMemcpyKind;

typedef enum cudaDeviceAttr {
    cudaDevAttrMaxThreadsPerBlock = 1,
    cudaDevAttrMaxBlockDimX = 2,
    cudaDevAttrMaxBlockDimY = 3,
    cudaDevAttrMaxBlockDimZ = 4,
    cudaDevAttrMaxGridDimX = 5,
    cudaDevAttrMaxGridDimY = 6,
    cudaDevAttrMaxGridDimZ = 7,
    cudaDevAttrWarpSize = 10,
    cudaDevAttrL2CacheSize = 38
} cudaDeviceAttr;

// Only the default stream (0) exists.
typedef struct lfortran_cuda_cpu_stream *cudaStream_t;

typedef struct dim3 {
    unsigned int x, y, z;
} dim3;

typedef struct uint3 {
    unsigned int x, y, z;
} uint3;

cudaError_t cudaSetDevice(int device);
cudaError_t cudaGetDevice(int *device);
cudaError_t cudaDeviceGetAttribute(int *value, cudaDeviceAttr attr,
    int device);
cudaError_t cudaDeviceSynchronize(void);
cudaError_t cudaDeviceReset(void);
cudaError_t cudaMalloc(void **ptr, size_t size);
cudaError_t cudaFree(void *ptr);
cudaError_t cudaMemcpy(void *dst, const void *src, size_t count,
    cudaMemcpyKind kind);
cudaError_t cudaMemcpyAsync(void *dst, const void *src, size_t count,
    cudaMemcpyKind kind, cudaStream_t stream);
// `func` is the address of a function `void f(void **args)` that runs one
// thread of the kernel: the CUDA code generator emits such a thunk for every
// kernel in emulation, and registers it in place of the kernel.
cudaError_t cudaLaunchKernel(const void *func, dim3 grid, dim3 block,
    void **args, size_t shared_mem, cudaStream_t stream);
const char *cudaGetErrorString(cudaError_t error);

#ifdef __cplusplus
}
#endif

#endif // LFORTRAN_CUDA_CPU_RUNTIME_H
