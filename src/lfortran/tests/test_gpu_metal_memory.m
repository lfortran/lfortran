#import <Foundation/Foundation.h>
#import <Metal/Metal.h>

#include "lfortran_gpu_runtime.h"

#include <mach/vm_page_size.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// The managed memory model of the Metal runtime (lfortran_gpu_metal.m): the
// runtime replaces the program's allocator, which this test defines as a
// program linked with the LFortran runtime library does, so that a large
// allocation is used in place by the kernels; and the scratch buffers it
// keeps for later launches. Run with LFORTRAN_GPU_PROFILE=1 and
// LFORTRAN_GPU_MEMORY=managed or copy.

typedef struct test_allocator {
    void* (*alloc)(void* context, int64_t size);
    void* (*realloc_func)(void* context, void* ptr, int64_t new_size);
    void  (*dealloc)(void* context, void* ptr);
    void* context;
} test_allocator;

static void* test_alloc(void *context, int64_t size) {
    (void)context;
    return malloc((size_t)size);
}

static void* test_realloc(void *context, void *ptr, int64_t size) {
    (void)context;
    return realloc(ptr, (size_t)size);
}

static void test_dealloc(void *context, void *ptr) {
    (void)context;
    free(ptr);
}

static test_allocator default_allocator = {
    test_alloc, test_realloc, test_dealloc, NULL
};

// The runtime finds the allocator by this name when the program starts.
test_allocator *_lfortran_get_default_allocator(void) {
    return &default_allocator;
}

enum {
    // Elements of the large arrays: 32 MiB of floats, more than the runtime
    // keeps in its pool of copies.
    BIG = 8 * 1024 * 1024,
    STRIDE = 4096,
    OUT_LEN = BIG / STRIDE,
};

static const char kernel_source[] =
"#include <metal_stdlib>\n"
"using namespace metal;\n"
"\n"
"kernel void gather(device float *out [[buffer(0)]],\n"
"    device const float *a [[buffer(1)]],\n"
"    constant uint &n [[buffer(2)]],\n"
"    uint gid [[thread_position_in_grid]])\n"
"{\n"
"    if (gid >= n) return;\n"
"    out[gid] = a[gid * 4096u];\n"
"}\n"
"\n"
"kernel void scale(device float *b [[buffer(0)]],\n"
"    constant uint &n [[buffer(1)]],\n"
"    uint gid [[thread_position_in_grid]])\n"
"{\n"
"    if (gid >= n) return;\n"
"    b[gid] = 2.0f*b[gid] + 1.0f;\n"
"}\n"
"\n"
"kernel void shift(device float *b [[buffer(0)]],\n"
"    device const float *a [[buffer(1)]],\n"
"    constant uint &n [[buffer(2)]],\n"
"    uint gid [[thread_position_in_grid]])\n"
"{\n"
"    if (gid >= n) return;\n"
"    b[gid] = 2.0f*a[gid];\n"
"}\n"
"\n"
"kernel void stage(device float *out [[buffer(0)]],\n"
"    device float *work [[buffer(1)]],\n"
"    constant uint &n [[buffer(2)]],\n"
"    uint gid [[thread_position_in_grid]])\n"
"{\n"
"    if (gid >= n) return;\n"
"    work[gid * 4096u] = 3.0f*(float)gid;\n"
"    out[gid] = work[gid * 4096u] + 1.0f;\n"
"}\n";

static void fail(const char *what) {
    fprintf(stderr, "%s\n", what);
    exit(1);
}

static void check_u64(const char *name, uint64_t got, uint64_t want) {
    if (got != want) {
        fprintf(stderr, "%s: got %llu, want %llu\n", name,
            (unsigned long long)got, (unsigned long long)want);
        exit(1);
    }
}

static lfortran_gpu_ctx *ctx;
static int managed;

static lfortran_gpu_metal_telemetry snapshot(void) {
    lfortran_gpu_metal_telemetry t;
    lfortran_gpu_metal_telemetry_snapshot(ctx, &t);
    return t;
}

// out(i) = a(1 + (i - 1)*STRIDE) for the `n` elements from `a`.
static void gather(float *out, const float *a, size_t n_a) {
    lfortran_gpu_kernel *k = lfortran_gpu_load_kernel(ctx, "gather",
        (int)strlen("gather"));
    int grid[3] = {OUT_LEN / 256, 1, 1};
    int block[3] = {256, 1, 1};
    uint32_t n = OUT_LEN;
    lfortran_gpu_set_buffer_arg(k, 0, out, OUT_LEN * sizeof(float),
        LFORTRAN_GPU_WRITE_DISCARD);
    lfortran_gpu_set_buffer_arg(k, 1, (void *)a, n_a * sizeof(float),
        LFORTRAN_GPU_READ);
    lfortran_gpu_set_scalar_arg(k, 2, &n, sizeof(n));
    lfortran_gpu_launch(ctx, k, grid, block);
    lfortran_gpu_release_kernel(k);
}

static void scale(float *b, size_t n_b) {
    lfortran_gpu_kernel *k = lfortran_gpu_load_kernel(ctx, "scale",
        (int)strlen("scale"));
    int grid[3] = {(int)(n_b / 256), 1, 1};
    int block[3] = {256, 1, 1};
    uint32_t n = (uint32_t)n_b;
    lfortran_gpu_set_buffer_arg(k, 0, b, n_b * sizeof(float),
        LFORTRAN_GPU_WRITE);
    lfortran_gpu_set_scalar_arg(k, 1, &n, sizeof(n));
    lfortran_gpu_launch(ctx, k, grid, block);
    lfortran_gpu_release_kernel(k);
}

// b(i) = 2*a(i) for the `n_b` elements of `b`, where `a` overlaps `b`.
static void shift(float *b, const float *a, size_t n_b) {
    lfortran_gpu_kernel *k = lfortran_gpu_load_kernel(ctx, "shift",
        (int)strlen("shift"));
    int grid[3] = {(int)((n_b + 255) / 256), 1, 1};
    int block[3] = {256, 1, 1};
    uint32_t n = (uint32_t)n_b;
    lfortran_gpu_set_buffer_arg(k, 0, b, n_b * sizeof(float),
        LFORTRAN_GPU_WRITE);
    lfortran_gpu_set_buffer_arg(k, 1, (void *)a, n_b * sizeof(float),
        LFORTRAN_GPU_READ);
    lfortran_gpu_set_scalar_arg(k, 2, &n, sizeof(n));
    lfortran_gpu_launch(ctx, k, grid, block);
    lfortran_gpu_release_kernel(k);
}

// out(i) = 3*(i - 1) + 1, through a scratch buffer of BIG floats.
static void stage(float *out) {
    lfortran_gpu_kernel *k = lfortran_gpu_load_kernel(ctx, "stage",
        (int)strlen("stage"));
    int grid[3] = {OUT_LEN / 256, 1, 1};
    int block[3] = {256, 1, 1};
    uint32_t n = OUT_LEN;
    lfortran_gpu_set_buffer_arg(k, 0, out, OUT_LEN * sizeof(float),
        LFORTRAN_GPU_WRITE_DISCARD);
    lfortran_gpu_set_scratch_arg(k, 1, (size_t)BIG * sizeof(float));
    lfortran_gpu_set_scalar_arg(k, 2, &n, sizeof(n));
    lfortran_gpu_launch(ctx, k, grid, block);
    lfortran_gpu_release_kernel(k);
}

static void check_gather(const char *what, const float *out, float scale_by,
    float offset_by)
{
    for (size_t i = 0; i < OUT_LEN; i++) {
        float want = scale_by*(float)(i*STRIDE) + offset_by;
        if (out[i] != want) {
            fprintf(stderr, "%s: out[%zu] = %g, want %g\n", what, i,
                out[i], want);
            exit(1);
        }
    }
}

int main(void) {
    @autoreleasepool {
        const char *model = getenv("LFORTRAN_GPU_MEMORY");
        managed = model == NULL || strcmp(model, "copy") != 0;
        lfortran_gpu_register_metal_source("gather", kernel_source);
        lfortran_gpu_register_metal_source("scale", kernel_source);
        lfortran_gpu_register_metal_source("shift", kernel_source);
        lfortran_gpu_register_metal_source("stage", kernel_source);
        ctx = lfortran_gpu_init();
        test_allocator *al = _lfortran_get_default_allocator();
        if (al->alloc == test_alloc) fail("the allocator was not replaced");

        const size_t big_bytes = (size_t)BIG * sizeof(float);
        float *a = (float *)al->alloc(al->context, (int64_t)big_bytes);
        float *out = (float *)malloc(OUT_LEN * sizeof(float));
        if (managed && (uintptr_t)a % vm_page_size != 0) {
            fail("a large allocation does not start a page");
        }
        for (size_t i = 0; i < BIG; i++) a[i] = (float)i;

        // A large array a launch reads is used in place: nothing is copied.
        lfortran_gpu_metal_telemetry t0 = snapshot();
        gather(out, a, BIG);
        lfortran_gpu_wait(ctx);
        // The host changes it once the launch is done; the next one sees it.
        for (size_t i = 0; i < BIG; i++) a[i] = 3.0f*(float)i;
        gather(out, a, BIG);
        lfortran_gpu_sync(ctx);
        lfortran_gpu_metal_telemetry t1 = snapshot();
        check_gather("after the host changed it", out, 3.0f, 0.0f);
        uint64_t copied = t1.memcpy_h2d_bytes - t0.memcpy_h2d_bytes;
        if (managed) {
            check_u64("bytes copied in", copied, 0);
            check_u64("bytes copied back",
                t1.memcpy_d2h_bytes - t0.memcpy_d2h_bytes,
                OUT_LEN * sizeof(float));
        } else {
            check_u64("bytes copied in", copied, 2 * big_bytes);
        }

        // A range that starts inside the allocation, at its offset there.
        gather(out, a + 7, BIG - 7);
        lfortran_gpu_sync(ctx);
        check_gather("inside the allocation", out, 3.0f, 21.0f);

        // What a kernel writes in place, which the next launch reads.
        float *b = (float *)al->alloc(al->context, (int64_t)big_bytes);
        for (size_t i = 0; i < BIG; i++) b[i] = (float)i;
        t0 = snapshot();
        scale(b, BIG);
        gather(out, b, BIG);
        lfortran_gpu_sync(ctx);
        t1 = snapshot();
        check_gather("written in place", out, 2.0f, 1.0f);
        if (b[BIG - 1] != 2.0f*(float)(BIG - 1) + 1.0f) fail("b written");
        if (managed) {
            check_u64("written bytes copied back",
                t1.memcpy_d2h_bytes - t0.memcpy_d2h_bytes,
                OUT_LEN * sizeof(float));
        }

        // A small range of an allocation a pending launch writes is read in
        // place: the host bytes are not current yet.
        scale(b, BIG);
        gather(out, b, 2 * STRIDE);
        lfortran_gpu_sync(ctx);
        if (out[1] != 2.0f*(2.0f*(float)STRIDE + 1.0f) + 1.0f) {
            fail("small read of a pending write");
        }

        // A range a launch reads that overlaps one it writes is read as the
        // host holds it, once the pending launch that writes it is done.
        for (size_t i = 0; i < BIG; i++) b[i] = (float)i;
        scale(b, BIG);
        shift(b, b + 1, BIG - 1);
        lfortran_gpu_sync(ctx);
        for (size_t i = 0; i < BIG - 1; i++) {
            if (b[i] != 2.0f*(2.0f*(float)(i + 1) + 1.0f)) {
                fprintf(stderr, "overlapping read: b[%zu] = %g\n", i, b[i]);
                exit(1);
            }
        }

        // Freed and allocated again, likely at the same address, with other
        // values: the runtime must not keep the old buffer.
        al->dealloc(al->context, a);
        float *c = (float *)al->alloc(al->context, (int64_t)big_bytes);
        for (size_t i = 0; i < BIG; i++) c[i] = 5.0f*(float)i;
        gather(out, c, BIG);
        lfortran_gpu_sync(ctx);
        check_gather("allocated again", out, 5.0f, 0.0f);

        // Grown by reallocation past the size from which it is managed.
        float *d = (float *)al->alloc(al->context, 1024 * sizeof(float));
        for (size_t i = 0; i < 1024; i++) d[i] = (float)i;
        d = (float *)al->realloc_func(al->context, d, (int64_t)big_bytes);
        for (size_t i = 1024; i < BIG; i++) d[i] = (float)i;
        t0 = snapshot();
        gather(out, d, BIG);
        lfortran_gpu_sync(ctx);
        t1 = snapshot();
        check_gather("grown", out, 1.0f, 0.0f);
        if (managed) {
            check_u64("grown bytes copied in",
                t1.memcpy_h2d_bytes - t0.memcpy_h2d_bytes, 0);
        }

        // Freed while a launch that reads it may be pending.
        gather(out, d, BIG);
        al->dealloc(al->context, d);
        lfortran_gpu_sync(ctx);
        check_gather("freed while pending", out, 1.0f, 0.0f);

        // A large scratch buffer is kept for the later launches of the
        // kernel, as a small one is.
        stage(out);
        lfortran_gpu_sync(ctx);
        t0 = snapshot();
        for (int it = 0; it < 3; it++) stage(out);
        lfortran_gpu_sync(ctx);
        t1 = snapshot();
        check_gather("staged", out, 3.0f / STRIDE, 1.0f);
        check_u64("scratch buffers made again",
            t1.buffer_allocations - t0.buffer_allocations, 0);

        al->dealloc(al->context, b);
        al->dealloc(al->context, c);
        free(out);
        printf("RESULT model=%s ok\n", managed ? "managed" : "copy");
        lfortran_gpu_shutdown(ctx);
    }
    return 0;
}
