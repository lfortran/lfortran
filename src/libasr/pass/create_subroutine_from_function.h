#ifndef LIBASR_PASS_CREATE_SUBROUTINE_FROM_FUNCTION_H
#define LIBASR_PASS_CREATE_SUBROUTINE_FROM_FUNCTION_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    void pass_create_subroutine_from_function(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &pass_options);

    // Whether a call of `name` with these arguments may call a procedure
    // contained in the procedure `routine`, which may then access its
    // variables by host association: the callee is contained in it, or a
    // procedure, or data that may carry one, is passed down to where it
    // may be called. Procedures passed as actual arguments are followed
    // through the calls that receive them.
    bool call_may_reach_contained_procedure(SymbolTable *routine,
        ASR::symbol_t *name, ASR::call_arg_t *args, size_t n_args,
        ASR::expr_t *dt);

} // namespace LCompilers

#endif // LIBASR_PASS_CREATE_SUBROUTINE_FROM_FUNCTION_H
