#ifndef LFORTRAN_SEMANTICS_ELEMENTAL_CHECKS_H
#define LFORTRAN_SEMANTICS_ELEMENTAL_CHECKS_H

#include <libasr/asr.h>
#include <libasr/diagnostics.h>

#include <string>

namespace LCompilers::LFortran {

// F2018 15.8 rules for elemental procedures: one static function per rule.
class ElementalChecks {
public:
    ElementalChecks() = delete;

    static void check_procedure(const ASR::Function_t &f,
        const std::string &proc_name, diag::Diagnostics &diag,
        const CompilerOptions &compiler_options);

    // Uses of elemental procedures in the body and declarations of a unit;
    // contained procedures are checked on their own.
    static void check_body(const ASR::Function_t &f, diag::Diagnostics &diag,
        const CompilerOptions &compiler_options);
    static void check_body(const ASR::Program_t &p, diag::Diagnostics &diag,
        const CompilerOptions &compiler_options);
    static void check_body(const ASR::Module_t &m, diag::Diagnostics &diag,
        const CompilerOptions &compiler_options);

    // C1517
    static bool not_dummy_interface(const std::string &name,
        ASR::ttype_t *type, bool is_pointer, const Location &loc,
        diag::Diagnostics &diag);

private:
    class BodyWalk;

    static void check_scope(const SymbolTable &scope, ASR::stmt_t **body,
        size_t n_body, diag::Diagnostics &diag,
        const CompilerOptions &compiler_options);

    // C15100
    static bool dummy_not_allocatable(const ASR::Variable_t &v,
        const std::string &proc_name, diag::Diagnostics &diag);

    // C15100
    static bool dummy_scalar(const ASR::Variable_t &v,
        const std::string &proc_name, diag::Diagnostics &diag);
    // C15100
    static bool dummy_not_pointer(const ASR::Variable_t &v,
        const std::string &proc_name, diag::Diagnostics &diag);
    // C15100
    static bool dummy_not_coarray(const ASR::Variable_t &v,
        const std::string &proc_name, diag::Diagnostics &diag);
    // C15103
    static bool dummy_not_procedure(const ASR::symbol_t &sym,
        const std::string &proc_name, diag::Diagnostics &diag);
    // C15103
    static bool no_alternate_return(const ASR::Variable_t &v,
        const std::string &proc_name, diag::Diagnostics &diag);
    // C15102
    static bool dummy_intent_given(const ASR::Variable_t &v,
        const std::string &proc_name, diag::Diagnostics &diag);
    // C15101
    static bool result_scalar(const ASR::Variable_t &r,
        const std::string &proc_name, diag::Diagnostics &diag);

    // C15101
    static bool result_not_allocatable(const ASR::Variable_t &r,
        const std::string &proc_name, diag::Diagnostics &diag);

    // C15101
    static bool result_not_pointer(const ASR::Variable_t &r,
        const std::string &proc_name, diag::Diagnostics &diag);

    // C15104
    static bool result_type_params(const ASR::Function_t &f,
        const ASR::Variable_t &r, const std::string &proc_name,
        diag::Diagnostics &diag);

    // C15104, length parameters of a derived-type result
    static bool result_derived_type_params(const ASR::Function_t &f,
        const ASR::Variable_t &r, const std::string &proc_name,
        diag::Diagnostics &diag);
    static bool is_nonintrinsic_elemental(ASR::symbol_t *sym);
    // C1031, C1519
    static bool not_pointer_target(ASR::expr_t *value,
        diag::Diagnostics &diag);
    // C1533
    static bool not_actual_argument(ASR::expr_t *actual,
        diag::Diagnostics &diag);
    // 15.4.2.2
    static bool explicit_interface(const ASR::symbol_t &proc,
        const SymbolTable &scope, const Location &loc, diag::Diagnostics &diag);
    // 9.5.3.3.3 p3
    static bool output_no_repeated_vector_subscript(ASR::expr_t *actual,
        const ASR::Variable_t &dummy, diag::Diagnostics &diag);
    // 15.8.2
    static bool function_actuals_conform(const ASR::Function_t &f,
        ASR::call_arg_t *args, size_t n_args, diag::Diagnostics &diag);
    // 15.8.3
    static bool subroutine_outputs_arrays(const ASR::Function_t &f,
        ASR::call_arg_t *args, size_t n_args, diag::Diagnostics &diag);
    // 15.8.3
    static bool subroutine_inputs_conform(const ASR::Function_t &f,
        ASR::call_arg_t *args, size_t n_args, diag::Diagnostics &diag);

    static const ASR::Variable_t *dummy_variable(const ASR::Function_t &f,
        size_t i);
    static bool is_output(const ASR::Function_t &f, size_t i);
    static bool may_be_absent(const ASR::Function_t &f, size_t i,
        ASR::expr_t *actual);
    static ASR::expr_t *shape_actual(const ASR::Function_t &f,
        ASR::call_arg_t *args, size_t i);
    static bool same_shape(ASR::expr_t *actual, ASR::expr_t *ref,
        std::string &actual_label, std::string &ref_label);
    static void add_shape_error(const std::string &message,
        ASR::expr_t *actual, const std::string &actual_label,
        ASR::expr_t *ref, const std::string &ref_label,
        diag::Diagnostics &diag);
};

} // namespace LCompilers::LFortran

#endif // LFORTRAN_SEMANTICS_ELEMENTAL_CHECKS_H
