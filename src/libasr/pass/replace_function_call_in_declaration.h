#ifndef LIBASR_PASS_REPLACE_FUNCTION_CALL_IN_DECLARATION_H
#define LIBASR_PASS_REPLACE_FUNCTION_CALL_IN_DECLARATION_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    void pass_replace_function_call_in_declaration(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &pass_options);

    // Whether the value of the specification expression `e` of a procedure
    // or BLOCK can change while it executes (e.g. it references a variable
    // that the body can define).
    bool bound_may_change(ASR::expr_t* e);

} // namespace LCompilers

#endif // LIBASR_PASS_REPLACE_FUNCTION_CALL_IN_DECLARATION_H
