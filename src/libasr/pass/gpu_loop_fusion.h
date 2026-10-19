#ifndef LIBASR_PASS_GPU_LOOP_FUSION_H
#define LIBASR_PASS_GPU_LOOP_FUSION_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    // Fuses a parallel loop that computes arrays into the parallel loop
    // after it that reads them at constant offsets, recomputing each value
    // where it is read, for loops assigned to the device. See
    // gpu_loop_fusion.cpp.
    void pass_gpu_loop_fusion(Allocator &al, ASR::TranslationUnit_t &unit,
                              const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_LOOP_FUSION_H
