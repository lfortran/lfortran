#ifndef LFORTRAN_GPU_RUNTIME_H
#define LFORTRAN_GPU_RUNTIME_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct lfortran_gpu_ctx    lfortran_gpu_ctx;
typedef struct lfortran_gpu_kernel lfortran_gpu_kernel;

lfortran_gpu_ctx*    lfortran_gpu_init(void);
void                 lfortran_gpu_shutdown(lfortran_gpu_ctx* ctx);

lfortran_gpu_kernel* lfortran_gpu_load_kernel(
    lfortran_gpu_ctx* ctx, const char* source, const char* entry_point);
void                 lfortran_gpu_release_kernel(lfortran_gpu_kernel* k);

void lfortran_gpu_set_buffer_arg(lfortran_gpu_kernel* k, int idx,
    void* ptr, size_t size);
void lfortran_gpu_set_scalar_arg(lfortran_gpu_kernel* k, int idx,
    const void* val, size_t size);

void lfortran_gpu_launch(lfortran_gpu_ctx* ctx, lfortran_gpu_kernel* k,
    int grid[3], int block[3]);
void lfortran_gpu_sync(lfortran_gpu_ctx* ctx);

#ifdef __cplusplus
}
#endif

#endif // LFORTRAN_GPU_RUNTIME_H
