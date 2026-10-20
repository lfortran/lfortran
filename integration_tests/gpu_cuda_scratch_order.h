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

static int gpu_cuda_alias_order(void (*write_entry)(void),
    void (*reverse_entry)(void), void (*read_entry)(void),
    const char *selected = NULL)
{
    lfortran_gpu_ctx *ctx = lfortran_gpu_init();
    if (!ctx) return 1;
    const size_t wide = 2 * sizeof(int32_t), narrow = sizeof(int32_t);
    const struct {
        const char *name;
        void (*entry)(void);
        size_t sizes[2];
        bool separate;
        size_t read_size;
        int32_t expected[2];
        int dispatches = 1;
    } cases[] = {
        {"wide-first", write_entry, {wide, narrow}, false, 0, {202, 17}},
        {"narrow-first", reverse_entry, {narrow, wide}, false, 0, {202, 83}},
        {"same-size", write_entry, {wide, wide}, false, 0, {202, 17}},
        {"nonalias", write_entry, {wide, narrow}, true, 0, {201, 17}},
        {"read-prefix", read_entry, {wide, narrow}, false, narrow, {202, 202}},
        {"read-same", read_entry, {wide, wide}, false, wide, {202, 202}},
        {"zero-size", write_entry, {wide, 0}, false, 0, {201, 17}},
        {"repeat", write_entry, {wide, narrow}, false, 0, {202, 17}, 2}
    };
    int result = 0, checked = 0;
    int grid[3] = {1, 1, 1}, block[3] = {1, 1, 1};
    for (const auto &test : cases) {
        if (selected && strcmp(selected, test.name) != 0) continue;
        lfortran_gpu_register_kernel(test.name, test.entry);
        auto *k = lfortran_gpu_load_kernel(ctx, test.name,
            (int)strlen(test.name));
        int32_t host[2] = {0, 17}, other[2] = {1, 19};
        lfortran_gpu_set_buffer_arg(k, 0, host, test.sizes[0],
            LFORTRAN_GPU_WRITE);
        lfortran_gpu_set_buffer_arg(k, 1, test.separate ? other : host,
            test.sizes[1], LFORTRAN_GPU_WRITE);
        if (test.read_size) {
            lfortran_gpu_set_buffer_arg(k, 2, host, test.read_size,
                LFORTRAN_GPU_READ);
        }
        printf("alias %s: initial={0,17}, expected={%d,%d}, dispatches=%d\n",
            test.name, test.expected[0], test.expected[1], test.dispatches);
        fflush(stdout);
        for (int i = 0; i < test.dispatches; i++) {
            lfortran_gpu_launch(ctx, k, grid, block);
        }
        lfortran_gpu_sync(ctx);
        printf("alias %s: host={%d,%d}, other={%d,%d}\n",
            test.name, host[0], host[1], other[0], other[1]);
        if (host[0] != test.expected[0] || host[1] != test.expected[1]
                || other[0] != (test.separate ? 2 : 1) || other[1] != 19) {
            result = 1;
        }
        lfortran_gpu_release_kernel(k);
        lfortran_gpu_discard(ctx, host, sizeof(host));
        lfortran_gpu_discard(ctx, other, sizeof(other));
        ++checked;
    }
    lfortran_gpu_shutdown(ctx);
    return checked ? result : 1;
}
