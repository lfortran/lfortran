#ifndef LIBASR_PASS_GPU_INLINE_LOOP_CALLS_H
#define LIBASR_PASS_GPU_INLINE_LOOP_CALLS_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    // Inlines a kernel's calls of small pure functions that run a loop,
    // such as a function returning the sum of the squares of its array
    // argument, so that the loop is one of the kernel's own, which the
    // lanes of an iteration can share (gpu_vector_lanes). See
    // gpu_inline_loop_calls.cpp.
    void pass_gpu_inline_loop_calls(Allocator &al,
        ASR::TranslationUnit_t &unit, const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_INLINE_LOOP_CALLS_H
