// The HIP runtime API of hip/hip_runtime_api.h, for the CPU emulation of HIP
// (`--gpu=hip_cpu`), implemented with the CPU emulation of the CUDA runtime
// API (cuda_cpu/cuda_cpu_runtime.c, which the program links too).
//
// Every enumerator is translated by name, so that the HIP adapter of the GPU
// runtime (hip/cuda_runtime.h) runs against a HIP that, as ROCm's, does not
// share values with the CUDA API it adapts. The emulated device is not on a
// PCI bus, so it reports no PCI location, and the adapter then finds no cache
// in the kernel driver's topology and keeps the size of the L2 cache.

#include "../cuda_cpu/cuda_runtime.h"
#include "hip/hip_runtime_api.h"

static hipError_t last_error = hipSuccess;
static unsigned int device_flags = 0;

static hipError_t from_cuda(cudaError_t error) {
    hipError_t result;
    switch (error) {
        case cudaSuccess: return hipSuccess;
        case cudaErrorInvalidValue: result = hipErrorInvalidValue; break;
        case cudaErrorMemoryAllocation: result = hipErrorOutOfMemory; break;
        case cudaErrorInvalidConfiguration:
            result = hipErrorInvalidConfiguration; break;
        case cudaErrorInvalidMemcpyDirection:
            result = hipErrorInvalidMemcpyDirection; break;
        case cudaErrorInvalidDeviceFunction:
            result = hipErrorInvalidDeviceFunction; break;
        case cudaErrorNoDevice: result = hipErrorNoDevice; break;
        case cudaErrorInvalidDevice: result = hipErrorInvalidDevice; break;
        case cudaErrorInvalidResourceHandle:
            result = hipErrorInvalidResourceHandle; break;
        default: result = hipErrorUnknown; break;
    }
    last_error = result;
    return result;
}

static hipError_t fail(hipError_t error) {
    last_error = error;
    return error;
}

static int to_cuda_kind(hipMemcpyKind kind, cudaMemcpyKind *out) {
    switch (kind) {
        case hipMemcpyHostToHost: *out = cudaMemcpyHostToHost; return 1;
        case hipMemcpyHostToDevice: *out = cudaMemcpyHostToDevice; return 1;
        case hipMemcpyDeviceToHost: *out = cudaMemcpyDeviceToHost; return 1;
        case hipMemcpyDeviceToDevice:
            *out = cudaMemcpyDeviceToDevice; return 1;
        case hipMemcpyDefault: *out = cudaMemcpyDefault; return 1;
    }
    return 0;
}

hipError_t hipSetDevice(int device) {
    return from_cuda(cudaSetDevice(device));
}

hipError_t hipGetDevice(int *device) {
    return from_cuda(cudaGetDevice(device));
}

hipError_t hipSetDeviceFlags(unsigned int flags) {
    unsigned int schedule = flags & 0x7;
    if (schedule != hipDeviceScheduleAuto && schedule != hipDeviceScheduleSpin
            && schedule != hipDeviceScheduleYield
            && schedule != hipDeviceScheduleBlockingSync) {
        return fail(hipErrorInvalidValue);
    }
    device_flags = flags;
    return hipSuccess;
}

unsigned int lfortran_hip_cpu_device_flags(void) {
    return device_flags;
}

hipError_t hipDeviceGetAttribute(int *value, hipDeviceAttribute_t attr,
        int device) {
    cudaDeviceAttr cuda_attr;
    switch (attr) {
        case hipDeviceAttributeIntegrated:
            cuda_attr = cudaDevAttrIntegrated; break;
        case hipDeviceAttributeL2CacheSize:
            cuda_attr = cudaDevAttrL2CacheSize; break;
        case hipDeviceAttributePageableMemoryAccess:
            cuda_attr = cudaDevAttrPageableMemoryAccess; break;
        case hipDeviceAttributeConcurrentManagedAccess:
            cuda_attr = cudaDevAttrConcurrentManagedAccess; break;
        default:
            // The PCI location: the emulated device has none.
            return fail(hipErrorInvalidValue);
    }
    return from_cuda(cudaDeviceGetAttribute(value, cuda_attr, device));
}

hipError_t hipDeviceSynchronize(void) {
    return from_cuda(cudaDeviceSynchronize());
}

hipError_t hipDeviceReset(void) {
    device_flags = 0;
    return from_cuda(cudaDeviceReset());
}

hipError_t hipMalloc(void **ptr, size_t size) {
    return from_cuda(cudaMalloc(ptr, size));
}

hipError_t hipFree(void *ptr) {
    return from_cuda(cudaFree(ptr));
}

hipError_t hipMallocManaged(void **ptr, size_t size, unsigned int flags) {
    if (flags != hipMemAttachGlobal) return fail(hipErrorInvalidValue);
    return from_cuda(cudaMallocManaged(ptr, size, cudaMemAttachGlobal));
}

hipError_t hipMemPrefetchAsync(const void *ptr, size_t count, int device,
        hipStream_t stream) {
    if (stream) return fail(hipErrorInvalidResourceHandle);
#if CUDART_VERSION >= 13000
    struct cudaMemLocation location;
    if (device == hipCpuDeviceId) {
        location.type = cudaMemLocationTypeHost;
        location.id = 0;
    } else {
        location.type = cudaMemLocationTypeDevice;
        location.id = device;
    }
    return from_cuda(cudaMemPrefetchAsync(ptr, count, location, 0, 0));
#else
    return from_cuda(cudaMemPrefetchAsync(ptr, count,
        device == hipCpuDeviceId ? cudaCpuDeviceId : device, 0));
#endif
}

hipError_t hipMemcpy(void *dst, const void *src, size_t count,
        hipMemcpyKind kind) {
    cudaMemcpyKind cuda_kind;
    if (!to_cuda_kind(kind, &cuda_kind)) {
        return fail(hipErrorInvalidMemcpyDirection);
    }
    return from_cuda(cudaMemcpy(dst, src, count, cuda_kind));
}

hipError_t hipMemcpyAsync(void *dst, const void *src, size_t count,
        hipMemcpyKind kind, hipStream_t stream) {
    cudaMemcpyKind cuda_kind;
    if (stream) return fail(hipErrorInvalidResourceHandle);
    if (!to_cuda_kind(kind, &cuda_kind)) {
        return fail(hipErrorInvalidMemcpyDirection);
    }
    return from_cuda(cudaMemcpyAsync(dst, src, count, cuda_kind, 0));
}

hipError_t hipMemsetAsync(void *ptr, int value, size_t count,
        hipStream_t stream) {
    if (stream) return fail(hipErrorInvalidResourceHandle);
    return from_cuda(cudaMemsetAsync(ptr, value, count, 0));
}

hipError_t hipLaunchKernel(const void *func, dim3 grid, dim3 block,
        void **args, size_t shared_mem, hipStream_t stream) {
    if (stream) return fail(hipErrorInvalidResourceHandle);
    return from_cuda(cudaLaunchKernel(func, grid, block, args, shared_mem,
        0));
}

hipError_t hipGetLastError(void) {
    hipError_t error = last_error;
    last_error = hipSuccess;
    return error;
}

const char *hipGetErrorString(hipError_t error) {
    switch (error) {
        case hipSuccess: return "no error";
        case hipErrorInvalidValue: return "invalid argument";
        case hipErrorOutOfMemory: return "out of memory";
        case hipErrorInvalidConfiguration:
            return "invalid configuration argument";
        case hipErrorInvalidMemcpyDirection:
            return "invalid copy direction for memcpy";
        case hipErrorInvalidDeviceFunction:
            return "invalid device function";
        case hipErrorNoDevice: return "no ROCm-capable device is detected";
        case hipErrorInvalidDevice: return "invalid device ordinal";
        case hipErrorInvalidResourceHandle:
            return "invalid resource handle";
        case hipErrorNotSupported: return "operation not supported";
        case hipErrorUnknown: return "unknown error";
    }
    return "unrecognized error code";
}
