#ifndef LIBASR_PASS_FUNCTION_RESULT_SCOPE_H
#define LIBASR_PASS_FUNCTION_RESULT_SCOPE_H

#include <libasr/asr.h>
#include <libasr/utils.h>

#include <vector>

namespace LCompilers {

    void pass_function_result_scope(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &pass_options);

    // Whether `expr` references a function whose result is finalized after
    // the innermost executable construct containing the reference
    // (F2018 7.5.6.3 p5).
    bool references_function_results(ASR::expr_t* expr);

    // The BLOCK, a symbol of `scope`, that evaluates each value of `header`
    // that references such a function into a new variable of `scope`, which
    // replaces the value, and then executes `rest`. The results are
    // finalized when the BLOCK completes. The statement that executes the
    // BLOCK is returned, or nullptr if no value references such a function.
    ASR::stmt_t* make_function_result_header_block(Allocator &al,
        SymbolTable* scope, const Location &loc,
        const std::vector<ASR::expr_t**> &header,
        const std::vector<ASR::stmt_t*> &rest);

} // namespace LCompilers

#endif // LIBASR_PASS_FUNCTION_RESULT_SCOPE_H
