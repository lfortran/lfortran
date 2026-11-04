#include <lfortran/semantics/elemental_checks.h>
#include <lfortran/semantics/semantic_exception.h>
#include <libasr/asr_utils.h>
#include <libasr/utils.h>

namespace LCompilers::LFortran {

void ElementalChecks::check_procedure(const ASR::Function_t &f,
        const std::string &proc_name, diag::Diagnostics &diag,
        const CompilerOptions &compiler_options) {
    if (!ASRUtils::get_FunctionType(f)->m_elemental) return;
    bool ok = true;
    for (size_t i = 0; i < f.n_args; i++) {
        if (!ASR::is_a<ASR::Var_t>(*f.m_args[i])) continue;
        ASR::symbol_t *sym = ASR::down_cast<ASR::Var_t>(f.m_args[i])->m_v;
        if (!ASR::is_a<ASR::Variable_t>(*sym)) continue;
        const ASR::Variable_t &v = *ASR::down_cast<ASR::Variable_t>(sym);
        ok = dummy_not_allocatable(v, proc_name, diag) && ok;
    }
    if (!ok && !compiler_options.continue_compilation) {
        throw SemanticAbort();
    }
}

bool ElementalChecks::dummy_not_allocatable(const ASR::Variable_t &v,
        const std::string &proc_name, diag::Diagnostics &diag) {
    if (!ASRUtils::is_allocatable(v.m_type)) return true;
    diag.add(diag::Diagnostic(
        "dummy argument '" + std::string(v.m_name) +
        "' of elemental procedure '" + proc_name + "' cannot be allocatable",
        diag::Level::Error, diag::Stage::Semantic, {
            diag::Label("", {v.base.base.loc})}));
    return false;
}

} // namespace LCompilers::LFortran
