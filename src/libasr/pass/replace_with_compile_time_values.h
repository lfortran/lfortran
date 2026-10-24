#ifndef LIBASR_PASS_REPLACE_WITH_COMPILE_TIME_VALUES_H
#define LIBASR_PASS_REPLACE_WITH_COMPILE_TIME_VALUES_H

#include <set>

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    void pass_replace_with_compile_time_values(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &pass_options);

    // Adds to `replaceable` the expressions of `function`, and of the
    // procedures it contains, that pass_replace_with_compile_time_values
    // puts their compile-time values in place of, without changing them.
    void collect_compile_time_value_replacements(Allocator &al,
        ASR::Function_t &function, std::set<ASR::expr_t*> &replaceable);

} // namespace LCompilers

#endif // LIBASR_PASS_REPLACE_WITH_COMPILE_TIME_VALUES_H
