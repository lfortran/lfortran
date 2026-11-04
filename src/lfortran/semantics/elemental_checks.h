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
};

} // namespace LCompilers::LFortran

#endif // LFORTRAN_SEMANTICS_ELEMENTAL_CHECKS_H
