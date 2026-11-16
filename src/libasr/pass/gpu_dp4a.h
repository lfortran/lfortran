#ifndef LIBASR_PASS_GPU_DP4A_H
#define LIBASR_PASS_GPU_DP4A_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    // Writes the portable integer dot product of four signed bytes in device
    // code as a call of the `__dp4a` instruction of an NVIDIA GPU. See
    // gpu_dp4a.cpp.
    void pass_gpu_dp4a(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_DP4A_H
