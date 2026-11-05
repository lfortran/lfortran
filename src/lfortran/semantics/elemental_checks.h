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

private:
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
};

} // namespace LCompilers::LFortran

#endif // LFORTRAN_SEMANTICS_ELEMENTAL_CHECKS_H
