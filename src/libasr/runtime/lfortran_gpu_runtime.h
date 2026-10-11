#ifndef LFORTRAN_GPU_RUNTIME_H
#define LFORTRAN_GPU_RUNTIME_H

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

enum { LFORTRAN_GPU_MAX_KERNEL_NAME = 256 };

// A kernel name arrives as a Fortran character constant, which carries its
// own length instead of a terminator, so make a terminated copy of it.
static inline void lfortran_gpu_copy_kernel_name(char *dest, const char *name,
    int name_len)
{
    if (name_len < 0 || name_len >= LFORTRAN_GPU_MAX_KERNEL_NAME) {
        fprintf(stderr, "lfortran_gpu_load_kernel: kernel name is too long\n");
        exit(1);
    }
    memcpy(dest, name, (size_t)name_len);
    dest[name_len] = '\0';
}

// The size in bytes of the device heap of a launch that allocates on the
// device: the value of the environment variable LFORTRAN_GPU_HEAP_SIZE, a
// number with an optional K, M or G suffix (powers of 1024), or 256 MiB when
// it is not set. A value that is not such a number ends the program.
enum { LFORTRAN_GPU_DEFAULT_HEAP_MIB = 256 };

static inline long long lfortran_gpu_parse_heap_size(void)
{
    const char *value = getenv("LFORTRAN_GPU_HEAP_SIZE");
    if (value == NULL || value[0] == '\0') {
        return (long long)LFORTRAN_GPU_DEFAULT_HEAP_MIB * 1024 * 1024;
    }
    char *end = NULL;
    long long size = strtoll(value, &end, 10);
    long long unit = 1;
    if (end != NULL && *end != '\0') {
        switch (*end) {
            case 'k': case 'K': unit = 1024LL; break;
            case 'm': case 'M': unit = 1024LL * 1024; break;
            case 'g': case 'G': unit = 1024LL * 1024 * 1024; break;
            default: unit = 0; break;
        }
        if (unit != 0 && end[1] != '\0') unit = 0;
    }
    if (end == value || size <= 0 || unit == 0) {
        fprintf(stderr, "LFORTRAN_GPU_HEAP_SIZE=%s is not a size in bytes "
            "(a positive number, optionally followed by K, M or G)\n", value);
        exit(1);
    }
    return size * unit;
}

typedef struct lfortran_gpu_ctx    lfortran_gpu_ctx;
typedef struct lfortran_gpu_kernel lfortran_gpu_kernel;

lfortran_gpu_ctx*    lfortran_gpu_init(void);
void                 lfortran_gpu_shutdown(lfortran_gpu_ctx* ctx);

// The device source is registered by the device code generator, so a kernel
// is looked up by name only. The name is not assumed to be null terminated,
// because a Fortran character constant is not.
lfortran_gpu_kernel* lfortran_gpu_load_kernel(
    lfortran_gpu_ctx* ctx, const char* entry_point, int entry_point_len);
void                 lfortran_gpu_release_kernel(lfortran_gpu_kernel* k);

void lfortran_gpu_set_buffer_arg(lfortran_gpu_kernel* k, int idx,
    void* ptr, size_t size);
void lfortran_gpu_set_scalar_arg(lfortran_gpu_kernel* k, int idx,
    const void* val, size_t size);

void lfortran_gpu_launch(lfortran_gpu_ctx* ctx, lfortran_gpu_kernel* k,
    int grid[3], int block[3]);
void lfortran_gpu_sync(lfortran_gpu_ctx* ctx);
long long lfortran_gpu_heap_size(void);

#ifdef __cplusplus
}
#endif

#endif // LFORTRAN_GPU_RUNTIME_H
