#ifndef LIBASR_PASS_FUNCTION_RESULT_SCOPE_H
#define LIBASR_PASS_FUNCTION_RESULT_SCOPE_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    void pass_function_result_scope(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_FUNCTION_RESULT_SCOPE_H
