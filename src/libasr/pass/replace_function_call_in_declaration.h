#ifndef LIBASR_PASS_REPLACE_FUNCTION_CALL_IN_DECLARATION_H
#define LIBASR_PASS_REPLACE_FUNCTION_CALL_IN_DECLARATION_H

#include <libasr/asr.h>
#include <libasr/utils.h>

#include <set>

namespace LCompilers {

    void pass_replace_function_call_in_declaration(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &pass_options);

    // The scopes whose explicit-shape objects keep their bound expressions,
    // because the GPU offload passes may turn them into device code and size
    // those objects themselves (empty unless a device is selected).
    std::set<SymbolTable*> offloadable_scopes(ASR::TranslationUnit_t &unit,
                                const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_REPLACE_FUNCTION_CALL_IN_DECLARATION_H
