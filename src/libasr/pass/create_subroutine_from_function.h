#ifndef LIBASR_PASS_CREATE_SUBROUTINE_FROM_FUNCTION_H
#define LIBASR_PASS_CREATE_SUBROUTINE_FROM_FUNCTION_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    enum class FunctionResultLowering {
        AllAggregates,
        ExplicitArraySnapshot
    };

    void pass_create_subroutine_from_function(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &pass_options);

    void pass_create_subroutine_from_function(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &pass_options,
                                FunctionResultLowering lowering);

} // namespace LCompilers

#endif // LIBASR_PASS_CREATE_SUBROUTINE_FROM_FUNCTION_H
