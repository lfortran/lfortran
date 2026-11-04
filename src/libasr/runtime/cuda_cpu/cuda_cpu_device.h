#ifndef LFORTRAN_CUDA_CPU_DEVICE_H
#define LFORTRAN_CUDA_CPU_DEVICE_H

// Device-side CUDA emulation for the CPU.
//
// This header is what makes generated CUDA device code compile as ordinary
// host C/C++: the CUDA execution-space qualifiers become no-ops and the
// built-in thread coordinates become thread-local variables that the
// emulated cudaLaunchKernel sets before each emulated thread runs its kernel
// body.
//
// It is included by the generated device source, which the `cuda_cpu` mode
// compiles as host C++.

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// dim3 and uint3, from the emulation of the CUDA runtime API.
#include "cuda_runtime.h"

#ifdef __cplusplus
extern "C" {
#endif

// Built-in thread coordinates. The emulation of cudaLaunchKernel
// (cuda_cpu_runtime.c) writes these before calling into a kernel, once per
// emulated thread. MSVC spells thread-local storage its own way.
#ifdef _MSC_VER
extern __declspec(thread) uint3 threadIdx;
extern __declspec(thread) uint3 blockIdx;
extern __declspec(thread) dim3 blockDim;
extern __declspec(thread) dim3 gridDim;
#else
extern __thread uint3 threadIdx;
extern __thread uint3 blockIdx;
extern __thread dim3 blockDim;
extern __thread dim3 gridDim;
#endif

// Reports that a barrier cannot be honoured and aborts: the emulation runs
// the threads of a block one after another, so there is no second thread to
// synchronise with.
void lfortran_cuda_cpu_barrier_unsupported(void);

#ifdef __cplusplus
}
#endif

// The device heap takes memory with an atomic add (see GpuAtomicFetchAdd).
#ifdef _MSC_VER
#include <intrin.h>
static inline int atomicAdd(int *address, int value) {
    // long and int are both 32 bits wide on Windows.
    return (int)_InterlockedExchangeAdd((volatile long *)address, value);
}
#else
static inline int atomicAdd(int *address, int value) {
    return __atomic_fetch_add(address, value, __ATOMIC_RELAXED);
}
#endif

// The reciprocal square root, which a device computes in one operation.
// A C library may declare an `rsqrt` of its own (C23), so the emulation's
// has a name of its own, which `rsqrt` in the device code names.
static inline double lfortran_cuda_cpu_rsqrt(double x) {
    return 1.0/sqrt(x);
}
#ifdef __cplusplus
static inline float lfortran_cuda_cpu_rsqrt(float x) {
    return 1.0f/sqrtf(x);
}
#endif
#define rsqrt lfortran_cuda_cpu_rsqrt

// A scalar TRANSFER between an integer and a real of one width reads the
// bits of one as the other with these intrinsics.
static inline float __int_as_float(int value) {
    float result;
    memcpy(&result, &value, sizeof(result));
    return result;
}

static inline int __float_as_int(float value) {
    int result;
    memcpy(&result, &value, sizeof(result));
    return result;
}

static inline double __longlong_as_double(long long value) {
    double result;
    memcpy(&result, &value, sizeof(result));
    return result;
}

static inline long long __double_as_longlong(double value) {
    long long result;
    memcpy(&result, &value, sizeof(result));
    return result;
}

// Execution space qualifiers have no meaning on the host.
#define __global__
#define __device__
#define __host__
#define __constant__
#define __shared__ static
#define __forceinline__ inline

// A barrier is only correct when the threads of one block really do run
// concurrently, which they do not in the emulation.
#define __syncthreads() lfortran_cuda_cpu_barrier_unsupported()

#endif // LFORTRAN_CUDA_CPU_DEVICE_H
