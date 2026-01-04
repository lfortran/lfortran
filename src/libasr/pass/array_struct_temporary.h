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

    void insert_allocate_stmt_for_array(Allocator& al, ASR::expr_t* temporary_var,
        ASR::expr_t* value, Vec<ASR::stmt_t*>* current_body);

    void pass_array_struct_temporary(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_ARRAY_STRUCT_TEMPORARY_H
