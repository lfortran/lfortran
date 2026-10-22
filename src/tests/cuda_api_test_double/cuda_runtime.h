#pragma once

#include <cstddef>

// Only the host CUDA API used by lfortran_gpu_cuda.c, not a device emulator.
enum cudaError_t { cudaSuccess };
enum cudaMemcpyKind { cudaMemcpyHostToDevice, cudaMemcpyDeviceToHost };
enum cudaDeviceAttr { cudaDevAttrL2CacheSize };
using cudaStream_t = void *;

struct dim3 {
    unsigned x, y, z;
    dim3(unsigned x = 1, unsigned y = 1, unsigned z = 1) : x(x), y(y), z(z) {}
};

cudaError_t cudaSetDevice(int);
cudaError_t cudaGetDevice(int *);
cudaError_t cudaDeviceGetAttribute(int *, cudaDeviceAttr, int);
cudaError_t cudaMalloc(void **, size_t);
cudaError_t cudaFree(void *);
cudaError_t cudaDeviceSynchronize();
cudaError_t cudaDeviceReset();
cudaError_t cudaMemcpy(void *, const void *, size_t, cudaMemcpyKind);
cudaError_t cudaMemcpyAsync(void *, const void *, size_t, cudaMemcpyKind,
    cudaStream_t);
cudaError_t cudaLaunchKernel(const void *, dim3, dim3, void **, size_t,
    cudaStream_t);
const char *cudaGetErrorString(cudaError_t);
