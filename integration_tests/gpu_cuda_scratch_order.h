#pragma once

#include <initializer_list>

#include "../src/libasr/runtime/lfortran_gpu_runtime.h"

extern "C" void lfortran_gpu_register_kernel(const char *, void (*)(void));

// Shared by the native CUDA regression and the host-only CUDA API-state unit.
// Each name has its own cache even though all names use the same device entry.
static int gpu_cuda_scratch_order(void (*entry)(void), bool scratch = true)
{
    static_assert(sizeof(int) == 4);
    lfortran_gpu_ctx *ctx = lfortran_gpu_init();
    if (!ctx) return 1;
    char names[64][32];
    for (int i = 0; i < 64; i++) {
        snprintf(names[i], sizeof(names[i]), "scratch_order_%d", i);
        lfortran_gpu_register_kernel(names[i], entry);
    }
    int output = 0, sequence = 0;
    int input[8] = {1};
    int grid[3] = {1, 1, 1}, block[3] = {1, 1, 1};
    lfortran_gpu_kernel *last = NULL;
    for (int size : {4, 8, 16, 32}) {
        int count = size == 32 ? 9 : 64;
        for (int i = 0; i < count; i++) {
            auto *k = lfortran_gpu_load_kernel(ctx, names[i],
                (int)strlen(names[i]));
            lfortran_gpu_set_buffer_arg(k, 0, &output, sizeof(output),
                LFORTRAN_GPU_WRITE);
            ++sequence;
            lfortran_gpu_set_scalar_arg(k, 1, &sequence, sizeof(sequence));
            for (int slot = 2; slot < 32; slot++) {
                if (scratch) {
                    lfortran_gpu_set_scratch_arg(k, slot, size);
                } else {
                    lfortran_gpu_set_buffer_arg(k, slot, input, size,
                        LFORTRAN_GPU_READ);
                }
            }
            lfortran_gpu_launch(ctx, k, grid, block);
            if (sequence == 201) last = k;
            else lfortran_gpu_release_kernel(k);
        }
    }
    // 3840 replacements after size 16, then 4080 after eight size-32
    // launches. The 17th replacement of launch 201 flushes the 4096 queue.
    lfortran_gpu_sync(ctx);
    int readback = 201;
    if (scratch) {
        lfortran_gpu_read_scratch_arg(last, 31, &readback, sizeof(readback));
    }
    lfortran_gpu_release_kernel(last);
    printf("%s: output=%d, expected=201\n",
        scratch ? "scratch" : "read-only", output);
    if (scratch) printf("scratch_readback=%d, expected=201\n", readback);
    int result = output == 201 && readback == 201 ? 0 : 1;
    lfortran_gpu_shutdown(ctx);
    return result;
}
