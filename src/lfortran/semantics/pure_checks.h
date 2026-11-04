#ifndef LFORTRAN_SEMANTICS_PURE_CHECKS_H
#define LFORTRAN_SEMANTICS_PURE_CHECKS_H

#include <libasr/asr.h>
#include <libasr/diagnostics.h>

#include <map>
#include <set>
#include <string>

namespace LCompilers::LFortran {

// F2018 15.7 rules for pure procedures: one static function per rule.
class PureChecks {
public:
    PureChecks() = delete;

    // Rules on the dummies of `f`, run when the procedure is declared.
    static void check_declaration(const ASR::Function_t &f,
        const std::string &proc_name, diag::Diagnostics &diag,
        const CompilerOptions &compiler_options);

    // Rules that need the body; `implicit_interface_procedures` maps call variables to procedures.
    static void check_definition(const ASR::Function_t &f,
        diag::Diagnostics &diag, const CompilerOptions &compiler_options,
        const std::map<const ASR::symbol_t*, ASR::symbol_t*>
            &implicit_interface_procedures);

    // Rules on an assignment in `scope` to a target with base symbol `base`.
    static void check_assignment_target(ASR::symbol_t *base,
        SymbolTable *scope, const Location &loc, diag::Diagnostics &diag,
        const CompilerOptions &compiler_options);

private:
    // 15.7: an intent(out) dummy is not polymorphic
    static bool intent_out_not_polymorphic(const ASR::Variable_t &v,
        const std::string &proc_name, diag::Diagnostics &diag);

    // 15.7: no statement or call with a side effect
    static bool body_has_no_side_effects(const ASR::Function_t &f,
        diag::Diagnostics &diag,
        const std::map<const ASR::symbol_t*, ASR::symbol_t*>
            &implicit_interface_procedures);

    // C1583
    static bool dummy_intent_given(const ASR::Variable_t &v,
        diag::Diagnostics &diag);

    // 15.7: no assignment to a variable from the host or a module
    static bool assignment_target_is_local(ASR::symbol_t &base,
        SymbolTable *procedure_scope, SymbolTable *scope, const Location &loc,
        diag::Diagnostics &diag);

    static bool has_polymorphic_allocatable_ultimate_component(
        ASR::symbol_t *struct_sym, std::set<ASR::symbol_t*> &visited);
};

} // namespace LCompilers::LFortran

#endif // LFORTRAN_SEMANTICS_PURE_CHECKS_H
