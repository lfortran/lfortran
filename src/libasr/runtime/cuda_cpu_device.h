#ifndef LFORTRAN_CUDA_CPU_DEVICE_H
#define LFORTRAN_CUDA_CPU_DEVICE_H

// Device-side CUDA emulation for the CPU.
//
// This header is what makes generated CUDA device code compile as ordinary
// host C/C++: the CUDA execution-space qualifiers become no-ops and the
// built-in thread coordinates become thread-local variables that the host
// runtime sets before each emulated thread runs its kernel body.
//
// It is included both by the generated device source (compiled as host C++
// by the `cuda_cpu` mode) and by cuda_cpu_runtime.h.

#include <math.h>

// Device execution configuration
typedef struct {
    unsigned int x, y, z;
} dim3;

// Thread and block index emulation
typedef struct {
    unsigned int x, y, z;
} uint3;

#ifdef __cplusplus
extern "C" {
#endif

// Built-in thread coordinates. The host runtime writes these before calling
// into a kernel, once per emulated thread.
extern __thread uint3 threadIdx;
extern __thread uint3 blockIdx;
extern __thread dim3 blockDim;
extern __thread dim3 gridDim;

// Reports that a barrier cannot be honoured and aborts. Only reachable when
// the emulation was built without OpenMP, where there is no second thread to
// synchronise with.
void lfortran_gpu_cpu_barrier_unsupported(void);

#ifdef __cplusplus
}
#endif

// Execution space qualifiers have no meaning on the host.
#define __global__
#define __device__
#define __host__
#define __constant__
#define __shared__ static
#define __forceinline__ inline

// A barrier is only correct when the threads of one block really do run
// concurrently, which is the case exactly when the runtime was built with
// OpenMP (the runtime opens one parallel region per block).
#ifdef _OPENMP
#define __syncthreads() _Pragma("omp barrier")
#else
#define __syncthreads() lfortran_gpu_cpu_barrier_unsupported()
#endif

#endif // LFORTRAN_CUDA_CPU_DEVICE_H
