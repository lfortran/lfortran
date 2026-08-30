#ifndef CUDA_CPU_RUNTIME_H
#define CUDA_CPU_RUNTIME_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <omp.h>
#include <math.h>

// dim3/uint3, the built-in thread coordinates and the execution space
// qualifiers are shared with the cuda_cpu GPU-offload emulation.
#include "cuda_cpu_device.h"

// CUDA Runtime API Emulation for CPU
typedef enum {
    cudaSuccess = 0,
    cudaErrorMemoryAllocation = 2,
    cudaErrorInvalidValue = 11
} cudaError_t;

// Memory management API
cudaError_t cudaMalloc(void **devPtr, size_t size);
cudaError_t cudaFree(void *devPtr);
cudaError_t cudaMemcpy(void *dst, const void *src, size_t count, int kind);
cudaError_t cudaDeviceSynchronize(void);

// Memory copy kinds
#define cudaMemcpyHostToDevice 1
#define cudaMemcpyDeviceToHost 2
#define cudaMemcpyDeviceToDevice 3

// Kernel launch emulation
cudaError_t cudaLaunchKernel(void *func, dim3 gridDim, dim3 blockDim, 
                            void **args, size_t sharedMem, void *stream);

// Error handling
const char* cudaGetErrorString(cudaError_t error);

// Memory allocation tracking structure
typedef struct {
    void *cpu_ptr;
    void *device_ptr;
    size_t size;
    int is_allocated;
} memory_tracker_t;


// Initialization function
void cpu_runtime_init(void);
void cpu_runtime_cleanup(void);

#endif // CUDA_CPU_RUNTIME_H
