#ifndef LFORTRAN_GPU_RUNTIME_H
#define LFORTRAN_GPU_RUNTIME_H

#include <stddef.h>
#include <stdint.h>
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

static inline void lfortran_gpu_validate_launch(const int grid[3],
    const int block[3])
{
    for (int i = 0; i < 3; i++) {
        if (grid[i] <= 0) {
            fprintf(stderr, "lfortran_gpu_launch: grid[%d] must be positive "
                "(got %d)\n", i, grid[i]);
            exit(1);
        }
        if (block[i] <= 0) {
            fprintf(stderr, "lfortran_gpu_launch: block[%d] must be positive "
                "(got %d)\n", i, block[i]);
            exit(1);
        }
    }
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

// Optional Metal telemetry collected by the runtime when command buffers are
// retired on the host. It is opt-in via LFORTRAN_GPU_PROFILE=1. The snapshot
// is cumulative and non-blocking; timings include only retired command buffers.
// Use snapshot_sync at measurement boundaries before subtracting snapshots.
// Elapsed time sums command-buffer intervals; busy time is their union, not
// hardware occupancy. Copy counters exclude inline/scalar setBytes arguments.
// Other backends return zeros, but snapshot_sync still synchronizes the runtime.
typedef struct lfortran_gpu_metal_telemetry {
    int gpu_timing_available;
    uint64_t completed_command_buffers;
    uint64_t commits;
    uint64_t dispatches;
    uint64_t host_syncs;
    uint64_t host_waits;
    uint64_t memcpy_h2d_bytes;
    uint64_t memcpy_d2h_bytes;
    uint64_t buffer_allocations;
    uint64_t buffer_alloc_bytes;
    double gpu_elapsed_seconds;
    double gpu_busy_seconds;
    double jit_seconds;
} lfortran_gpu_metal_telemetry;

lfortran_gpu_ctx*    lfortran_gpu_init(void);
void                 lfortran_gpu_shutdown(lfortran_gpu_ctx* ctx);
void                 lfortran_gpu_register_metal_source(const char *name,
    const char *source);

// The device source is registered by the device code generator, so a kernel
// is looked up by name only. The name is not assumed to be null terminated,
// because a Fortran character constant is not.
lfortran_gpu_kernel* lfortran_gpu_load_kernel(
    lfortran_gpu_ctx* ctx, const char* entry_point, int entry_point_len);
void                 lfortran_gpu_release_kernel(lfortran_gpu_kernel* k);

// How a kernel uses a buffer argument.
enum {
    // The kernel only reads the buffer.
    LFORTRAN_GPU_READ = 0,
    // The kernel may write the buffer.
    LFORTRAN_GPU_WRITE = 1
};

// The kernel sees the `size` bytes at `ptr` as they are when the launch is
// made. A launch does not wait for the kernel: the host sees what the kernel
// wrote at the next lfortran_gpu_sync.
//
// A runtime with memory of its own keeps a buffer the kernel may write on
// the device: until the next sync, a later launch handed the same host range
// sees what the kernel wrote there, without copying anything, and the host
// gets it back at the sync. So between a launch and the next sync the host
// must not read or write a range a kernel may write, nor free it except
// after lfortran_gpu_discard; a range the kernel only reads it may change
// or free as soon as the launch returns. This is what the compiler's
// `gpu_launch_sync` pass arranges. A range handed over again that overlaps
// a kept one without being the same range makes the runtime sync first.
void lfortran_gpu_set_buffer_arg(lfortran_gpu_kernel* k, int idx,
    void* ptr, size_t size, int access);
void lfortran_gpu_set_scalar_arg(lfortran_gpu_kernel* k, int idx,
    const void* val, size_t size);
// A buffer of `size` bytes only the device holds: nothing is copied in or
// back, and the kernel object owns it until it is released.
void lfortran_gpu_set_scratch_arg(lfortran_gpu_kernel* k, int idx,
    size_t size);
// Copies the leading `size` bytes of the scratch buffer `idx` into `dst`,
// after the launch and before the kernel is released. It waits for the
// launch, as lfortran_gpu_sync does.
void lfortran_gpu_read_scratch_arg(lfortran_gpu_kernel* k, int idx,
    void* dst, size_t size);

// Starts the kernel and returns without waiting for it. Every component of
// grid and block must be positive; otherwise the program ends with an error
// before any launch work or buffer residency changes. This contract is the
// same for Metal, CUDA and CPU emulation. An empty kernel body is legal, but
// zero-work Fortran loops must be skipped by compiler lowering, not launched.
void lfortran_gpu_launch(lfortran_gpu_ctx* ctx, lfortran_gpu_kernel* k,
    int grid[3], int block[3]);
// Waits for every launch made so far, and copies what the kernels wrote back
// into the host ranges they were handed. A kernel that failed ends the
// program here.
void lfortran_gpu_sync(lfortran_gpu_ctx* ctx);
// The host is about to free the `size` bytes at `ptr`, and no longer needs
// what a launch since the last sync wrote there: the runtime forgets the
// range without copying anything back.
void lfortran_gpu_discard(lfortran_gpu_ctx* ctx, void* ptr, size_t size);
long long lfortran_gpu_heap_size(void);

void lfortran_gpu_metal_telemetry_snapshot(lfortran_gpu_ctx* ctx,
    lfortran_gpu_metal_telemetry *out);
void lfortran_gpu_metal_telemetry_snapshot_sync(lfortran_gpu_ctx* ctx,
    lfortran_gpu_metal_telemetry *out);

#ifdef __cplusplus
}
#endif

#endif // LFORTRAN_GPU_RUNTIME_H
