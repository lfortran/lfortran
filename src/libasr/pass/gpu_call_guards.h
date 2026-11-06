#ifndef LIBASR_PASS_GPU_CALL_GUARDS_H
#define LIBASR_PASS_GPU_CALL_GUARDS_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    // Tests, before a call, the condition on which the subroutine called
    // returns at once, when the call can skip the subroutine on it. See
    // gpu_call_guards.cpp.
    void pass_gpu_call_guards(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_CALL_GUARDS_H
