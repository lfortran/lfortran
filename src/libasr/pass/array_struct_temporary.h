#ifndef LIBASR_PASS_ARRAY_STRUCT_TEMPORARY_H
#define LIBASR_PASS_ARRAY_STRUCT_TEMPORARY_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    ASR::ttype_t* normalize_string_temporary_type(Allocator& al,
        const Location& loc, ASR::ttype_t* var_type, bool capture_length=false);

    ASR::expr_t* create_temporary_variable_for_array(Allocator& al,
        ASR::expr_t* value, SymbolTable* scope, std::string name_hint,
        bool is_pointer_required=false, bool override_physical_type=false);

    ASR::expr_t* create_temporary_variable_for_scalar(Allocator& al,
        ASR::expr_t* value, SymbolTable* scope, std::string name_hint, bool is_pointer_required=false);

    // Allocates `temporary_var` with the shape of `value`. An assignment
    // target, `is_target`, that is not polymorphic keeps its storage when it
    // is allocated with that shape already (F2018 10.2.1.3); a temporary is
    // allocated again.
    void insert_allocate_stmt_for_array(Allocator& al, ASR::expr_t* temporary_var,
        ASR::expr_t* value, Vec<ASR::stmt_t*>* current_body,
        ASR::expr_t* rank_source = nullptr, bool is_target = false);

    // The dispatch expression of a type-bound call after its passed-object
    // actual `original`, which is the same expression as `dt`, has been
    // replaced by `prepared`, so that the operand is evaluated only once.
    ASR::expr_t* passed_object_dispatch(ASR::expr_t* dt, ASR::expr_t* original,
        ASR::expr_t* prepared);

    void pass_array_struct_temporary(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_ARRAY_STRUCT_TEMPORARY_H
