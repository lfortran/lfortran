#include <lfortran/semantics/pure_checks.h>
#include <lfortran/semantics/semantic_exception.h>
#include <libasr/asr_side_effect.h>
#include <libasr/asr_utils.h>
#include <libasr/utils.h>

namespace LCompilers::LFortran {

void PureChecks::check_declaration(const ASR::Function_t &f,
        const std::string &proc_name, diag::Diagnostics &diag,
        const CompilerOptions &compiler_options) {
    if (!ASRUtils::get_FunctionType(f)->m_pure) return;
    for (size_t i = 0; i < f.n_args; i++) {
        if (!ASR::is_a<ASR::Var_t>(*f.m_args[i])) continue;
        ASR::symbol_t *sym = ASR::down_cast<ASR::Var_t>(f.m_args[i])->m_v;
        if (!ASR::is_a<ASR::Variable_t>(*sym)) continue;
        const ASR::Variable_t &v = *ASR::down_cast<ASR::Variable_t>(sym);
        if (!intent_out_not_polymorphic(v, proc_name, diag)
                && !compiler_options.continue_compilation) {
            throw SemanticAbort();
        }
    }
}

void PureChecks::check_definition(const ASR::Function_t &f,
        diag::Diagnostics &diag, const CompilerOptions &compiler_options,
        const std::map<const ASR::symbol_t*, ASR::symbol_t*>
            &implicit_interface_procedures) {
    if (!ASRUtils::get_FunctionType(f)->m_pure) return;
    if (!body_has_no_side_effects(f, diag, implicit_interface_procedures)
            && !compiler_options.continue_compilation) {
        throw SemanticAbort();
    }
    for (size_t i = 0; i < f.n_args; i++) {
        if (!ASR::is_a<ASR::Var_t>(*f.m_args[i])) continue;
        ASR::symbol_t *sym = ASR::down_cast<ASR::Var_t>(f.m_args[i])->m_v;
        if (!ASR::is_a<ASR::Variable_t>(*sym)) continue;
        const ASR::Variable_t &v = *ASR::down_cast<ASR::Variable_t>(sym);
        // Pointer dummies and dummy procedures are exempt.
        if (ASRUtils::is_pointer(v.m_type) || ASR::is_a<ASR::FunctionType_t>(
                *ASRUtils::type_get_past_pointer(v.m_type))) {
            continue;
        }
        if (!dummy_intent_given(v, diag)
                && !compiler_options.continue_compilation) {
            throw SemanticAbort();
        }
    }
}

bool PureChecks::body_has_no_side_effects(const ASR::Function_t &f,
        diag::Diagnostics &diag,
        const std::map<const ASR::symbol_t*, ASR::symbol_t*>
            &implicit_interface_procedures) {
    ASR::SideEffectFinder finder;
    finder.implicit_interface_procedures = &implicit_interface_procedures;
    for (size_t i = 0; i < f.n_body && !finder.found; i++) {
        finder.visit_stmt(*f.m_body[i]);
    }
    if (!finder.found) return true;
    diag.add(diag::Diagnostic(
        finder.description + " is not allowed inside a PURE procedure",
        diag::Level::Error, diag::Stage::Semantic, {
            diag::Label("", {finder.loc})}));
    return false;
}

bool PureChecks::dummy_intent_given(const ASR::Variable_t &v,
        diag::Diagnostics &diag) {
    if (v.m_intent != ASR::intentType::Unspecified || v.m_value_attr) {
        return true;
    }
    diag.add(diag::Diagnostic(
        "Dummy argument '" + std::string(v.m_name) +
        "' of pure function must have INTENT(IN) or VALUE attribute",
        diag::Level::Error, diag::Stage::Semantic, {
            diag::Label("", {v.base.base.loc})}));
    return false;
}

bool PureChecks::intent_out_not_polymorphic(const ASR::Variable_t &v,
        const std::string &proc_name, diag::Diagnostics &diag) {
    if (v.m_intent != ASR::intentType::Out) return true;
    ASR::ttype_t *type = ASRUtils::extract_type(v.m_type);
    if (!ASR::is_a<ASR::StructType_t>(*type)) return true;
    std::string msg = "intent(out) dummy argument '" + std::string(v.m_name) +
        "' of pure procedure '" + proc_name + "' cannot ";
    if (ASRUtils::is_class_type(type)) {
        msg += "be polymorphic";
    } else {
        std::set<ASR::symbol_t*> visited;
        if (!has_polymorphic_allocatable_ultimate_component(
                v.m_type_declaration, visited)) {
            return true;
        }
        msg += "have a polymorphic allocatable ultimate component";
    }
    diag.add(diag::Diagnostic(msg,
        diag::Level::Error, diag::Stage::Semantic, {
            diag::Label("", {v.base.base.loc})}));
    return false;
}

// Ultimate components are reached through nonpointer, nonallocatable components and the parent.
bool PureChecks::has_polymorphic_allocatable_ultimate_component(
        ASR::symbol_t *struct_sym, std::set<ASR::symbol_t*> &visited) {
    if (!struct_sym) return false;
    struct_sym = ASRUtils::symbol_get_past_external(struct_sym);
    if (!ASR::is_a<ASR::Struct_t>(*struct_sym) ||
            !visited.insert(struct_sym).second) {
        return false;
    }
    ASR::Struct_t *st = ASR::down_cast<ASR::Struct_t>(struct_sym);
    if (has_polymorphic_allocatable_ultimate_component(st->m_parent, visited)) {
        return true;
    }
    for (auto &item: st->m_symtab->get_scope()) {
        if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
        ASR::Variable_t *comp = ASR::down_cast<ASR::Variable_t>(item.second);
        if (ASRUtils::is_pointer(comp->m_type)) continue;
        ASR::ttype_t *comp_type = ASRUtils::extract_type(comp->m_type);
        if (!ASR::is_a<ASR::StructType_t>(*comp_type)) continue;
        if (ASRUtils::is_allocatable(comp->m_type)) {
            if (ASRUtils::is_class_type(comp_type)) return true;
        } else if (has_polymorphic_allocatable_ultimate_component(
                comp->m_type_declaration, visited)) {
            return true;
        }
    }
    return false;
}

} // namespace LCompilers::LFortran
