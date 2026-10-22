#ifndef LFORTRAN_CUDA_CPU_EMULATOR_H
#define LFORTRAN_CUDA_CPU_EMULATOR_H

// What the CPU emulation of the CUDA runtime API (cuda_cpu_runtime.c) shows
// about its own state, for tests of the code that calls the API. None of this
// is part of the CUDA API.

#include "cuda_runtime.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct lfortran_cuda_cpu_stats {
    // Successful calls of cudaLaunchKernel, cudaMalloc (of a positive size),
    // cudaFree (of device memory) and cudaDeviceSynchronize.
    unsigned long long launches;
    unsigned long long mallocs;
    unsigned long long frees;
    unsigned long long synchronizations;
    // cudaFree and cudaDeviceReset calls made while a copy or a launch was
    // still pending in the stream (issued after the last synchronizing call),
    // which they have to wait for, as CUDA does.
    unsigned long long implicit_synchronizations;
    // Successful copies by cudaMemcpyKind, of cudaMemcpyAsync and cudaMemcpy.
    unsigned long long async_copies[5];
    unsigned long long copies[5];
    // Calls that returned an error.
    unsigned long long errors;
    // Device allocations that are live now.
    size_t live_allocations;
} lfortran_cuda_cpu_stats;

const lfortran_cuda_cpu_stats *lfortran_cuda_cpu_get_stats(void);

// Calls `hook` at the start of every cudaDeviceSynchronize; NULL removes it.
void lfortran_cuda_cpu_set_synchronize_hook(void (*hook)(void));

// Whether `ptr` points into a live device allocation; if so, its start and
// size are stored in `*base` and `*size`.
int lfortran_cuda_cpu_find_allocation(const void *ptr, void **base,
    size_t *size);

#ifdef __cplusplus
}
#endif

#endif // LFORTRAN_CUDA_CPU_EMULATOR_H
