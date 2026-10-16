#ifndef LIBASR_PASS_ARRAY_STRUCT_TEMPORARY_H
#define LIBASR_PASS_ARRAY_STRUCT_TEMPORARY_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    ASR::expr_t* create_temporary_variable_for_array(Allocator& al,
        ASR::expr_t* value, SymbolTable* scope, std::string name_hint,
        bool is_pointer_required=false, bool override_physical_type=false);

    ASR::expr_t* create_temporary_variable_for_scalar(Allocator& al,
        ASR::expr_t* value, SymbolTable* scope, std::string name_hint, bool is_pointer_required=false);

    // Whether `value` is an array other than a variable -- a section, or
    // a value an expression computes -- which a statement that passes it
    // on may hold in a temporary.
    bool is_temporary_needed(ASR::expr_t* value);

    // Whether `x` is a variable or a value computed element by element
    // from its operands, which an elementwise operation using it reads in
    // place, without a temporary.
    bool is_vectorise_able(ASR::expr_t* x);

    // Whether this pass reads an operand of an operation of type `op` that
    // is elementwise itself (is_vectorise_able, or a scalar broadcast to a
    // shape) in place, holding in a temporary only one that is not; an
    // operand of any other expression it may hold in a temporary.
    bool reads_elementwise_operands_in_place(ASR::exprType op);

    // Whether `rhs` names an array that `lhs` names as well, in which case
    // a section of it is copied rather than read in place.
    bool is_common_symbol_present_in_lhs_and_rhs(Allocator &al,
        ASR::expr_t* lhs, ASR::expr_t* rhs);

    void insert_allocate_stmt_for_array(Allocator& al, ASR::expr_t* temporary_var,
        ASR::expr_t* value, Vec<ASR::stmt_t*>* current_body,
        ASR::expr_t* rank_source = nullptr);

    void pass_array_struct_temporary(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_ARRAY_STRUCT_TEMPORARY_H
