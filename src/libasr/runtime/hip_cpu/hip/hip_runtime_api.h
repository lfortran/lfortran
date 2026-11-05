#ifndef LFORTRAN_HIP_CPU_RUNTIME_API_H
#define LFORTRAN_HIP_CPU_RUNTIME_API_H

// The HIP runtime API as the CPU emulation of HIP (`--gpu=hip_cpu`) provides
// it: the part that the HIP adapter of the GPU runtime (hip/cuda_runtime.h)
// calls, written for LFortran; it is not ROCm's header. Its directory follows
// the adapter's on the include path when the GPU runtime is compiled for the
// emulation, so that the adapter's `#include <hip/hip_runtime_api.h>` finds
// it, and hip_cpu_runtime.c implements it with the CPU emulation of the CUDA
// runtime API (cuda_cpu/). The adapter is then compiled and run as it is for
// an AMD GPU. The enumerators have values of their own, unlike those of the
// CUDA emulation, so that the adapter has to name each one it maps. The
// header is C and C++.

#include <stddef.h>

// Set when the HIP runtime API is this emulation.
#define LFORTRAN_HIP_CPU_EMULATION 1

#ifdef __cplusplus
extern "C" {
#endif

typedef enum hipError_t {
    hipSuccess = 0,
    hipErrorInvalidValue = 1,
    hipErrorOutOfMemory = 2,
    hipErrorInvalidConfiguration = 9,
    hipErrorInvalidMemcpyDirection = 21,
    hipErrorInvalidDeviceFunction = 98,
    hipErrorNoDevice = 100,
    hipErrorInvalidDevice = 101,
    hipErrorInvalidResourceHandle = 400,
    hipErrorNotSupported = 801,
    hipErrorUnknown = 999
} hipError_t;

typedef enum hipMemcpyKind {
    hipMemcpyHostToHost = 0,
    hipMemcpyHostToDevice = 1,
    hipMemcpyDeviceToHost = 2,
    hipMemcpyDeviceToDevice = 3,
    hipMemcpyDefault = 4
} hipMemcpyKind;

typedef enum hipDeviceAttribute_t {
    hipDeviceAttributeIntegrated = 1001,
    hipDeviceAttributeL2CacheSize,
    hipDeviceAttributePageableMemoryAccess,
    hipDeviceAttributePciBusId,
    hipDeviceAttributePciDeviceId,
    hipDeviceAttributePciDomainID
} hipDeviceAttribute_t;

#define hipDeviceScheduleAuto 0x0
#define hipDeviceScheduleSpin 0x1
#define hipDeviceScheduleYield 0x2
#define hipDeviceScheduleBlockingSync 0x4

// Only the default stream (0) exists.
typedef struct ihipStream_t *hipStream_t;

// The CPU emulation of the CUDA runtime API defines the same dim3, which
// hip_cpu_runtime.c includes first.
#ifndef LFORTRAN_CUDA_CPU_RUNTIME_H
typedef struct dim3 {
    unsigned int x, y, z;
} dim3;
#endif

hipError_t hipSetDevice(int device);
hipError_t hipGetDevice(int *device);
hipError_t hipSetDeviceFlags(unsigned int flags);
hipError_t hipDeviceGetAttribute(int *value, hipDeviceAttribute_t attr,
    int device);
hipError_t hipDeviceSynchronize(void);
hipError_t hipDeviceReset(void);
hipError_t hipMalloc(void **ptr, size_t size);
hipError_t hipFree(void *ptr);
hipError_t hipMemcpy(void *dst, const void *src, size_t count,
    hipMemcpyKind kind);
hipError_t hipMemcpyAsync(void *dst, const void *src, size_t count,
    hipMemcpyKind kind, hipStream_t stream);
hipError_t hipMemsetAsync(void *ptr, int value, size_t count,
    hipStream_t stream);
hipError_t hipLaunchKernel(const void *func, dim3 grid, dim3 block,
    void **args, size_t shared_mem, hipStream_t stream);
// The error of the last call that failed, which it then forgets.
hipError_t hipGetLastError(void);
const char *hipGetErrorString(hipError_t error);

// The flags of the last successful hipSetDeviceFlags (0 before it), for
// tests of the code that calls the API. Not part of the HIP API.
unsigned int lfortran_hip_cpu_device_flags(void);

#ifdef __cplusplus
}
#endif

#endif // LFORTRAN_HIP_CPU_RUNTIME_API_H
