#ifndef LFORTRAN_SEMANTICS_PURE_CHECKS_H
#define LFORTRAN_SEMANTICS_PURE_CHECKS_H

#include <libasr/asr.h>
#include <libasr/diagnostics.h>

#include <map>
#include <string>

namespace LCompilers::LFortran {

// F2018 15.7 rules for pure procedures: one static function per rule.
class PureChecks {
public:
    PureChecks() = delete;

    // Rules that need the body of `f`. `implicit_interface_procedures`: the
    // procedure variables that calls through an implicit interface are made
    // through, mapped to the called procedure.
    static void check_definition(const ASR::Function_t &f,
        diag::Diagnostics &diag, const CompilerOptions &compiler_options,
        const std::map<const ASR::symbol_t*, ASR::symbol_t*>
            &implicit_interface_procedures);

private:
    // No statement or call with a side effect
    static bool body_has_no_side_effects(const ASR::Function_t &f,
        diag::Diagnostics &diag,
        const std::map<const ASR::symbol_t*, ASR::symbol_t*>
            &implicit_interface_procedures);

    // C1583
    static bool dummy_intent_given(const ASR::Variable_t &v,
        diag::Diagnostics &diag);
};

} // namespace LCompilers::LFortran

#endif // LFORTRAN_SEMANTICS_PURE_CHECKS_H
