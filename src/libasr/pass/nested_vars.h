#ifndef LIBASR_PASS_NESTED_VARS_H
#define LIBASR_PASS_NESTED_VARS_H

#include <libasr/asr.h>
#include <libasr/utils.h>

#include <utility>
#include <vector>

namespace LCompilers {

    void pass_nested_vars(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &pass_options);

    // The variables of `program` that its internal procedures use through
    // host association, which pass_nested_vars moves into a module, each
    // with the location of its first such use, in source order.
    std::vector<std::pair<ASR::symbol_t*, Location>> host_associated_variables(
        Allocator &al, const ASR::Program_t &program);

} // namespace LCompilers

#endif // LIBASR_PASS_NESTED_VARS_H
