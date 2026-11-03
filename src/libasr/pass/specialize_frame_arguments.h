#ifndef LIBASR_PASS_SPECIALIZE_FRAME_ARGUMENTS_H
#define LIBASR_PASS_SPECIALIZE_FRAME_ARGUMENTS_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    // With --fast, a procedure of this file whose dummy procedures are only
    // called or passed on gets a copy that takes each of them as a code
    // pointer and the address of a frame, so that an internal procedure is
    // passed to it with the frame of its host instead of a thunk. See
    // specialize_frame_arguments.cpp.
    void pass_specialize_frame_arguments(Allocator &al,
        ASR::TranslationUnit_t &unit, const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_SPECIALIZE_FRAME_ARGUMENTS_H
