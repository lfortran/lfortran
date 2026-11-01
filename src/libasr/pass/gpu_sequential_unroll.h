#ifndef LIBASR_PASS_GPU_SEQUENTIAL_UNROLL_H
#define LIBASR_PASS_GPU_SEQUENTIAL_UNROLL_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    // Unrolls the short counted loops of device code by the device's
    // sequential_loop_unroll_factor (gpu_decline.h). See
    // gpu_sequential_unroll.cpp.
    void pass_gpu_sequential_unroll(Allocator &al,
        ASR::TranslationUnit_t &unit, const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_SEQUENTIAL_UNROLL_H
