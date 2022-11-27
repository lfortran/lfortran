#ifndef LIBASR_PASS_INTRINSIC_FUNCTION_H
#define LIBASR_PASS_INTRINSIC_FUNCTION_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LFortran {

    void pass_replace_intrinsic_function(Allocator &al, ASR::TranslationUnit_t &unit,
                                const LCompilers::PassOptions& pass_options);

} // namespace LFortran

#endif // LIBASR_PASS_INTRINSIC_FUNCTION_H
