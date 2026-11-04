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
        if (ASR::is_a<ASR::Variable_t>(*sym)) {
            const ASR::Variable_t &v = *ASR::down_cast<ASR::Variable_t>(sym);
            ok = dummy_not_procedure(*sym, proc_name, diag) && ok;
            ok = dummy_scalar(v, proc_name, diag) && ok;
            ok = dummy_not_allocatable(v, proc_name, diag) && ok;
            ok = dummy_not_pointer(v, proc_name, diag) && ok;
            ok = dummy_not_coarray(v, proc_name, diag) && ok;
            ok = no_alternate_return(v, proc_name, diag) && ok;
        } else if (ASR::is_a<ASR::Function_t>(*sym)) {
            ok = dummy_not_procedure(*sym, proc_name, diag) && ok;
        } else {
            throw LCompilersException("unexpected kind of dummy argument '" +
                std::string(ASRUtils::symbol_name(sym)) +
                "' of elemental procedure '" + proc_name + "'");
        }
    }
    if (f.m_return_var && ASR::is_a<ASR::Var_t>(*f.m_return_var)) {
        ASR::symbol_t *sym = ASR::down_cast<ASR::Var_t>(f.m_return_var)->m_v;
        if (ASR::is_a<ASR::Variable_t>(*sym)) {
            const ASR::Variable_t &r = *ASR::down_cast<ASR::Variable_t>(sym);
            ok = result_scalar(r, proc_name, diag) && ok;
            ok = result_not_allocatable(r, proc_name, diag) && ok;
        }
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

bool ElementalChecks::dummy_scalar(const ASR::Variable_t &v,
        const std::string &proc_name, diag::Diagnostics &diag) {
    if (!ASRUtils::is_array(v.m_type)) return true;
    diag.add(diag::Diagnostic(
        "dummy argument '" + std::string(v.m_name) +
        "' of elemental procedure '" + proc_name + "' must be scalar",
        diag::Level::Error, diag::Stage::Semantic, {
            diag::Label("", {v.base.base.loc})}));
    return false;
}

bool ElementalChecks::dummy_not_pointer(const ASR::Variable_t &v,
        const std::string &proc_name, diag::Diagnostics &diag) {
    if (!ASRUtils::is_pointer(v.m_type)) return true;
    diag.add(diag::Diagnostic(
        "dummy argument '" + std::string(v.m_name) +
        "' of elemental procedure '" + proc_name + "' cannot be a pointer",
        diag::Level::Error, diag::Stage::Semantic, {
            diag::Label("", {v.base.base.loc})}));
    return false;
}

bool ElementalChecks::dummy_not_coarray(const ASR::Variable_t &v,
        const std::string &proc_name, diag::Diagnostics &diag) {
    if (v.n_codims == 0) return true;
    diag.add(diag::Diagnostic(
        "dummy argument '" + std::string(v.m_name) +
        "' of elemental procedure '" + proc_name + "' cannot be a coarray",
        diag::Level::Error, diag::Stage::Semantic, {
            diag::Label("", {v.base.base.loc})}));
    return false;
}

bool ElementalChecks::dummy_not_procedure(const ASR::symbol_t &sym,
        const std::string &proc_name, diag::Diagnostics &diag) {
    bool is_procedure = ASR::is_a<ASR::Function_t>(sym);
    if (ASR::is_a<ASR::Variable_t>(sym)) {
        ASR::ttype_t *type = ASR::down_cast<ASR::Variable_t>(&sym)->m_type;
        is_procedure = ASR::is_a<ASR::FunctionType_t>(
            *ASRUtils::type_get_past_pointer(type));
    }
    if (!is_procedure) return true;
    diag.add(diag::Diagnostic(
        "dummy argument '" + std::string(ASRUtils::symbol_name(&sym)) +
        "' of elemental procedure '" + proc_name + "' cannot be a procedure",
        diag::Level::Error, diag::Stage::Semantic, {
            diag::Label("", {sym.base.loc})}));
    return false;
}

bool ElementalChecks::no_alternate_return(const ASR::Variable_t &v,
        const std::string &proc_name, diag::Diagnostics &diag) {
    if (std::string(v.m_name) != "__lfortran_alt_ret") return true;
    diag.add(diag::Diagnostic(
        "elemental procedure '" + proc_name +
        "' cannot have an alternate return dummy argument '*'",
        diag::Level::Error, diag::Stage::Semantic, {
            diag::Label("", {v.base.base.loc})}));
    return false;
}

bool ElementalChecks::result_scalar(const ASR::Variable_t &r,
        const std::string &proc_name, diag::Diagnostics &diag) {
    if (!ASRUtils::is_array(r.m_type)) return true;
    diag.add(diag::Diagnostic(
        "result of elemental function '" + proc_name + "' must be scalar",
        diag::Level::Error, diag::Stage::Semantic, {
            diag::Label("", {r.base.base.loc})}));
    return false;
}

bool ElementalChecks::result_not_allocatable(const ASR::Variable_t &r,
        const std::string &proc_name, diag::Diagnostics &diag) {
    if (!ASRUtils::is_allocatable(r.m_type)) return true;
    diag.add(diag::Diagnostic(
        "result of elemental function '" + proc_name +
        "' cannot be allocatable",
        diag::Level::Error, diag::Stage::Semantic, {
            diag::Label("", {r.base.base.loc})}));
    return false;
}

} // namespace LCompilers::LFortran
