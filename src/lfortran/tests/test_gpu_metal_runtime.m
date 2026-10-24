#import <Foundation/Foundation.h>
#import <Metal/Metal.h>

#include "lfortran_gpu_runtime.h"

#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum {
    PROBE_LEN = 256,
    WIDE_LEN = 2048,
    PHASE1_LAUNCHES = 193,
    PHASE2_LAUNCHES = 63,
    PHASE3_LAUNCHES = 61,
    TOTAL_LAUNCHES = PHASE1_LAUNCHES + PHASE2_LAUNCHES + PHASE3_LAUNCHES,
};

static const char telemetry_kernel_source[] =
"#include <metal_stdlib>\n"
"using namespace metal;\n"
"\n"
"kernel void telemetry_step(device float *out [[buffer(0)]],\n"
"    device const float *input [[buffer(1)]],\n"
"    device const float *wide [[buffer(2)]],\n"
"    constant uint &n [[buffer(3)]],\n"
"    uint gid [[thread_position_in_grid]])\n"
"{\n"
"    if (gid >= n) return;\n"
"    out[gid] = out[gid] + input[gid] + wide[gid % 2048u];\n"
"}\n";

static void fail(const char *message)
{
    fprintf(stderr, "%s\n", message);
    exit(1);
}

static void check_u64(const char *name, uint64_t got, uint64_t want)
{
    if (got != want) {
        fprintf(stderr, "%s: got %llu, want %llu\n", name,
            (unsigned long long)got, (unsigned long long)want);
        exit(1);
    }
}

static void check_true(const char *name, int condition)
{
    if (!condition) {
        fprintf(stderr, "%s: failed\n", name);
        exit(1);
    }
}

static void fill_inputs(float *out, float *input, float *wide)
{
    for (size_t i = 0; i < PROBE_LEN; i++) {
        out[i] = 0.0f;
        input[i] = 1.0f;
    }
    for (size_t i = 0; i < WIDE_LEN; i++) {
        wide[i] = (float)(i % 7) * 0.25f + 1.0f;
    }
}

static double checksum(const float *values, size_t n)
{
    double sum = 0.0;
    for (size_t i = 0; i < n; i++) sum += values[i];
    return sum;
}

static void expect_phase_counts(const lfortran_gpu_metal_telemetry *telemetry,
    uint64_t launches, uint64_t commits, uint64_t syncs, uint64_t waits,
    uint64_t h2d_bytes, uint64_t d2h_bytes, uint64_t buffer_allocations,
    uint64_t buffer_alloc_bytes)
{
    check_u64("dispatches", telemetry->dispatches, launches);
    check_u64("commits", telemetry->commits, commits);
    check_u64("completed_command_buffers", telemetry->completed_command_buffers,
        commits);
    check_u64("host_syncs", telemetry->host_syncs, syncs);
    check_u64("host_waits", telemetry->host_waits, waits);
    check_u64("memcpy_h2d_bytes", telemetry->memcpy_h2d_bytes, h2d_bytes);
    check_u64("memcpy_d2h_bytes", telemetry->memcpy_d2h_bytes, d2h_bytes);
    check_true("buffer allocation bounds",
        telemetry->buffer_allocations >= 2
        && telemetry->buffer_allocations <= buffer_allocations);
    check_true("buffer allocation byte bounds",
        telemetry->buffer_alloc_bytes >= PROBE_LEN * sizeof(float)
            + WIDE_LEN * sizeof(float)
        && telemetry->buffer_alloc_bytes <= buffer_alloc_bytes);
}

static void launch_kernel(lfortran_gpu_ctx *ctx, lfortran_gpu_kernel *kernel,
    float *out, float *input, float *wide)
{
    int grid[3] = {1, 1, 1};
    int block[3] = {PROBE_LEN, 1, 1};
    uint32_t n = PROBE_LEN;
    lfortran_gpu_set_buffer_arg(kernel, 0, out, PROBE_LEN * sizeof(float),
        LFORTRAN_GPU_WRITE);
    lfortran_gpu_set_buffer_arg(kernel, 1, input, PROBE_LEN * sizeof(float),
        LFORTRAN_GPU_READ);
    lfortran_gpu_set_buffer_arg(kernel, 2, wide, WIDE_LEN * sizeof(float),
        LFORTRAN_GPU_READ);
    lfortran_gpu_set_scalar_arg(kernel, 3, &n, sizeof(n));
    lfortran_gpu_launch(ctx, kernel, grid, block);
}

static void run_phase(lfortran_gpu_ctx *ctx, lfortran_gpu_kernel *kernel,
    float *out, float *input, float *wide, uint64_t launches,
    int profile_enabled,
    uint64_t expected_dispatches, uint64_t expected_commits,
    uint64_t expected_syncs, uint64_t expected_waits,
    uint64_t expected_h2d, uint64_t expected_d2h,
    uint64_t expected_buffer_allocations, uint64_t expected_buffer_alloc_bytes)
{
    for (uint64_t i = 0; i < launches; i++) {
        launch_kernel(ctx, kernel, out, input, wide);
    }
    lfortran_gpu_metal_telemetry synced;
    lfortran_gpu_metal_telemetry_snapshot_sync(ctx, &synced);
    lfortran_gpu_metal_telemetry snapshot;
    lfortran_gpu_metal_telemetry_snapshot(ctx, &snapshot);
    if (!profile_enabled) {
        check_true("profile disabled timing", !snapshot.gpu_timing_available);
        check_u64("disabled dispatches", snapshot.dispatches, 0);
        check_u64("disabled commits", snapshot.commits, 0);
        check_u64("disabled completed", snapshot.completed_command_buffers, 0);
        check_u64("disabled syncs", snapshot.host_syncs, 0);
        check_u64("disabled waits", snapshot.host_waits, 0);
        check_u64("disabled h2d", snapshot.memcpy_h2d_bytes, 0);
        check_u64("disabled d2h", snapshot.memcpy_d2h_bytes, 0);
        check_u64("disabled allocs", snapshot.buffer_allocations, 0);
        check_u64("disabled alloc bytes", snapshot.buffer_alloc_bytes, 0);
        check_true("disabled elapsed", snapshot.gpu_elapsed_seconds == 0.0);
        check_true("disabled busy", snapshot.gpu_busy_seconds == 0.0);
        check_true("disabled jit", snapshot.jit_seconds == 0.0);
        (void)synced;
        return;
    }
    if (!snapshot.gpu_timing_available) {
        fail("gpu timing unavailable during telemetry probe");
    }
    expect_phase_counts(&snapshot, expected_dispatches, expected_commits,
        expected_syncs, expected_waits, expected_h2d, expected_d2h,
        expected_buffer_allocations, expected_buffer_alloc_bytes);
    check_true("gpu elapsed", snapshot.gpu_elapsed_seconds > 0.0);
    check_true("gpu busy", snapshot.gpu_busy_seconds > 0.0);
    check_true("gpu busy <= elapsed",
        snapshot.gpu_busy_seconds <= snapshot.gpu_elapsed_seconds + 1e-9);
    check_true("jit time", snapshot.jit_seconds > 0.0);
    (void)synced;
}

int main(void)
{
    @autoreleasepool {
        float out[PROBE_LEN];
        float input[PROBE_LEN];
        float wide[WIDE_LEN];
        fill_inputs(out, input, wide);

        lfortran_gpu_register_metal_source("telemetry_step",
            telemetry_kernel_source);
        lfortran_gpu_ctx *ctx = lfortran_gpu_init();
        if (!ctx) {
            fail("lfortran_gpu_init failed");
        }

        const char *profile_env = getenv("LFORTRAN_GPU_PROFILE");
        const int profile_enabled =
            profile_env != NULL && strcmp(profile_env, "1") == 0;
        lfortran_gpu_metal_telemetry start = {0};
        lfortran_gpu_metal_telemetry_snapshot(ctx, &start);
        check_u64("start dispatches", start.dispatches, 0);
        check_u64("start commits", start.commits, 0);
        check_u64("start completed", start.completed_command_buffers, 0);
        check_u64("start syncs", start.host_syncs, 0);
        check_u64("start waits", start.host_waits, 0);
        check_u64("start h2d", start.memcpy_h2d_bytes, 0);
        check_u64("start d2h", start.memcpy_d2h_bytes, 0);
        check_u64("start allocs", start.buffer_allocations, 0);
        check_u64("start alloc bytes", start.buffer_alloc_bytes, 0);
        check_true("start elapsed", start.gpu_elapsed_seconds == 0.0);
        check_true("start busy", start.gpu_busy_seconds == 0.0);
        check_true("start jit", start.jit_seconds == 0.0);
        check_true("start timing availability",
            start.gpu_timing_available == profile_enabled);

        lfortran_gpu_kernel *kernel = lfortran_gpu_load_kernel(ctx,
            "telemetry_step", (int)strlen("telemetry_step"));
        if (!kernel) {
            fail("lfortran_gpu_load_kernel failed");
        }

        const uint64_t out_bytes = PROBE_LEN * sizeof(float);
        const uint64_t wide_bytes = WIDE_LEN * sizeof(float);
        const uint64_t phase1_h2d = (uint64_t)PHASE1_LAUNCHES * wide_bytes
            + out_bytes;
        const uint64_t phase2_h2d = (uint64_t)PHASE2_LAUNCHES * wide_bytes
            + out_bytes;
        const uint64_t phase3_h2d = (uint64_t)PHASE3_LAUNCHES * wide_bytes
            + out_bytes;
        const uint64_t phase1_d2h = out_bytes;
        const uint64_t phase2_d2h = out_bytes;
        const uint64_t phase3_d2h = out_bytes;
        const uint64_t phase1_allocs = 1 + PHASE1_LAUNCHES;
        const uint64_t phase1_alloc_bytes = out_bytes
            + PHASE1_LAUNCHES * wide_bytes;

        run_phase(ctx, kernel, out, input, wide, PHASE1_LAUNCHES,
            profile_enabled,
            PHASE1_LAUNCHES, 4, 1, 2, phase1_h2d, phase1_d2h,
            phase1_allocs, phase1_alloc_bytes);
        run_phase(ctx, kernel, out, input, wide, PHASE2_LAUNCHES,
            profile_enabled,
            PHASE1_LAUNCHES + PHASE2_LAUNCHES, 5, 2, 3,
            phase1_h2d + phase2_h2d, phase1_d2h + phase2_d2h,
            phase1_allocs, phase1_alloc_bytes);
        run_phase(ctx, kernel, out, input, wide, PHASE3_LAUNCHES,
            profile_enabled,
            TOTAL_LAUNCHES, 6, 3, 4,
            phase1_h2d + phase2_h2d + phase3_h2d,
            phase1_d2h + phase2_d2h + phase3_d2h,
            phase1_allocs, phase1_alloc_bytes);

        lfortran_gpu_metal_telemetry end = {0};
        lfortran_gpu_metal_telemetry_snapshot_sync(ctx, &end);
        if (profile_enabled) {
            check_u64("final dispatches", end.dispatches, TOTAL_LAUNCHES);
            check_u64("final commits", end.commits, 6);
            check_u64("final completed", end.completed_command_buffers, 6);
            check_u64("final host syncs", end.host_syncs, 4);
            check_u64("final host waits", end.host_waits, 4);
            check_u64("final memcpy h2d", end.memcpy_h2d_bytes,
                phase1_h2d + phase2_h2d + phase3_h2d);
            check_u64("final memcpy d2h", end.memcpy_d2h_bytes,
                phase1_d2h + phase2_d2h + phase3_d2h);
            check_true("final buffer allocation bounds",
                end.buffer_allocations >= 2
                && end.buffer_allocations <= phase1_allocs);
            check_true("final buffer byte bounds",
                end.buffer_alloc_bytes >= out_bytes + wide_bytes
                && end.buffer_alloc_bytes <= phase1_alloc_bytes);
            check_true("timing available", end.gpu_timing_available);
            check_true("gpu elapsed positive", end.gpu_elapsed_seconds > 0.0);
            check_true("gpu busy positive", end.gpu_busy_seconds > 0.0);
            check_true("gpu busy <= elapsed",
                end.gpu_busy_seconds <= end.gpu_elapsed_seconds + 1e-9);
            check_true("jit positive", end.jit_seconds > 0.0);
        } else {
            check_true("timing unavailable", !end.gpu_timing_available);
            check_u64("final dispatches", end.dispatches, 0);
            check_u64("final commits", end.commits, 0);
            check_u64("final completed", end.completed_command_buffers, 0);
            check_u64("final host syncs", end.host_syncs, 0);
            check_u64("final host waits", end.host_waits, 0);
            check_u64("final memcpy h2d", end.memcpy_h2d_bytes, 0);
            check_u64("final memcpy d2h", end.memcpy_d2h_bytes, 0);
            check_u64("final buffer allocations", end.buffer_allocations, 0);
            check_u64("final buffer bytes", end.buffer_alloc_bytes, 0);
            check_true("gpu elapsed zero", end.gpu_elapsed_seconds == 0.0);
            check_true("gpu busy zero", end.gpu_busy_seconds == 0.0);
            check_true("jit zero", end.jit_seconds == 0.0);
        }

        double expected_sum = 0.0;
        for (size_t i = 0; i < PROBE_LEN; i++) {
            float expected = (float)TOTAL_LAUNCHES * (input[i] + wide[i]);
            expected_sum += expected;
            if (!isfinite(out[i]) || out[i] != expected) {
                fprintf(stderr, "final out[%zu]=%.9g, want %.9g\n", i,
                    out[i], expected);
                return 1;
            }
        }

        const double sum = checksum(out, PROBE_LEN);
        if (!isfinite(sum) || sum != expected_sum) {
            fprintf(stderr, "final checksum %.17g is wrong\n", sum);
            return 1;
        }

        printf("RESULT profile=%s final_sum=%.0f dispatches=%llu commits=%llu completed=%llu "
               "syncs=%llu waits=%llu h2d=%llu d2h=%llu timing=%d gpu_elapsed=%.6f "
               "gpu_busy=%.6f jit=%.6f\n",
            profile_enabled ? "on" : "off", sum,
            (unsigned long long)end.dispatches,
            (unsigned long long)end.commits,
            (unsigned long long)end.completed_command_buffers,
            (unsigned long long)end.host_syncs,
            (unsigned long long)end.host_waits,
            (unsigned long long)end.memcpy_h2d_bytes,
            (unsigned long long)end.memcpy_d2h_bytes,
            end.gpu_timing_available, end.gpu_elapsed_seconds,
            end.gpu_busy_seconds, end.jit_seconds);

        lfortran_gpu_release_kernel(kernel);
        lfortran_gpu_shutdown(ctx);

        (void)start;
        return 0;
    }
}
