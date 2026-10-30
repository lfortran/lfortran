#ifndef LIBASR_PASS_REPLACE_IMPLIED_DO_LOOPS_H
#define LIBASR_PASS_REPLACE_IMPLIED_DO_LOOPS_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    void pass_replace_implied_do_loops(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &pass_options);

    void replace_implied_do_loops_in_body(Allocator &al,
        ASR::stmt_t **&body, size_t &n_body, SymbolTable *scope,
        const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_REPLACE_IMPLIED_DO_LOOPS_H
