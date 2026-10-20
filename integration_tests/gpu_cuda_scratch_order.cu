#include <cuda_runtime.h>

#include "gpu_cuda_scratch_order.h"

__global__ void write_sequence(int *output, int sequence,
    int *s0, int *s1, int *s2, int *s3, int *s4, int *s5,
    int *s6, int *s7, int *s8, int *s9, int *s10, int *s11,
    int *s12, int *s13, int *s14, int *s15, int *s16, int *s17,
    int *s18, int *s19, int *s20, int *s21, int *s22, int *s23,
    int *s24, int *s25, int *s26, int *s27, int *s28, int *s29)
{
    int *scratch[] = {s0, s1, s2, s3, s4, s5, s6, s7, s8, s9,
        s10, s11, s12, s13, s14, s15, s16, s17, s18, s19,
        s20, s21, s22, s23, s24, s25, s26, s27, s28, s29};
    for (int *buffer : scratch) *buffer = sequence;
    *output = sequence;
}

__global__ void write_aliases(int32_t *wide, int32_t *narrow)
{
    wide[0] = 201;
    if (narrow) narrow[0] += 1;
}

__global__ void write_aliases_reversed(int32_t *narrow, int32_t *wide)
{
    wide[0] = 201;
    narrow[0] += 1;
    wide[1] = 83;
}

__global__ void read_aliases(int32_t *wide, int32_t *narrow,
    const int32_t *reader)
{
    wide[0] = 201;
    narrow[0] += 1;
    wide[1] = reader[0];
}

int main(int argc, char **argv)
{
    if (argc == 2 && strcmp(argv[1], "aliases") == 0) {
        return gpu_cuda_alias_order((void (*)(void))write_aliases,
            (void (*)(void))write_aliases_reversed,
            (void (*)(void))read_aliases);
    }
    return gpu_cuda_scratch_order((void (*)(void))write_sequence);
}
