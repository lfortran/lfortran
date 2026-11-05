#include <lfortran/semantics/elemental_checks.h>
#include <lfortran/semantics/semantic_exception.h>
#include <libasr/asr_utils.h>
#include <libasr/utils.h>

#include <set>

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
            ok = dummy_intent_given(v, proc_name, diag) && ok;
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
            ok = result_not_pointer(r, proc_name, diag) && ok;
            ok = result_type_params(f, r, proc_name, diag) && ok;
            ok = result_derived_type_params(f, r, proc_name, diag) && ok;
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

bool ElementalChecks::dummy_intent_given(const ASR::Variable_t &v,
        const std::string &proc_name, diag::Diagnostics &diag) {
    if (v.m_intent != ASR::intentType::Unspecified || v.m_value_attr
            || ASR::is_a<ASR::FunctionType_t>(
                *ASRUtils::type_get_past_pointer(v.m_type))) {
        return true;
    }
    diag.add(diag::Diagnostic(
        "dummy argument '" + std::string(v.m_name) +
        "' of elemental procedure '" + proc_name +
        "' must have an explicit intent or the value attribute",
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

bool ElementalChecks::result_not_pointer(const ASR::Variable_t &r,
        const std::string &proc_name, diag::Diagnostics &diag) {
    if (!ASRUtils::is_pointer(r.m_type)) return true;
    diag.add(diag::Diagnostic(
        "result of elemental function '" + proc_name +
        "' cannot be a pointer",
        diag::Level::Error, diag::Stage::Semantic, {
            diag::Label("", {r.base.base.loc})}));
    return false;
}

namespace {

// Finds a dummy of `f` used other than as the base object of an inquiry.
class DummyOutsideInquiry : public ASR::BaseWalkVisitor<DummyOutsideInquiry> {
public:
    const ASR::Function_t &f;
    std::set<const ASR::Var_t*> allowed;
    const ASR::Var_t *found = nullptr;

    explicit DummyOutsideInquiry(const ASR::Function_t &f) : f(f) {}

    void allow(const ASR::expr_t *subject) {
        if (const ASR::Var_t *v = base_var(subject)) allowed.insert(v);
    }

    void visit_ttype(const ASR::ttype_t &) {}

    void visit_Var(const ASR::Var_t &x) {
        if (!found && allowed.count(&x) == 0
                && ASRUtils::is_dummy_argument(f, x.m_v)) {
            found = &x;
        }
    }

    void visit_StringLen(const ASR::StringLen_t &x) {
        allow(x.m_arg);
        BaseWalkVisitor<DummyOutsideInquiry>::visit_StringLen(x);
    }

    void visit_ArraySize(const ASR::ArraySize_t &x) {
        allow(x.m_v);
        BaseWalkVisitor<DummyOutsideInquiry>::visit_ArraySize(x);
    }

    void visit_ArrayBound(const ASR::ArrayBound_t &x) {
        allow(x.m_v);
        BaseWalkVisitor<DummyOutsideInquiry>::visit_ArrayBound(x);
    }

    void visit_ArrayRank(const ASR::ArrayRank_t &x) {
        allow(x.m_v);
        BaseWalkVisitor<DummyOutsideInquiry>::visit_ArrayRank(x);
    }

    void visit_TypeInquiry(const ASR::TypeInquiry_t &x) {
        if (x.m_arg) allow(x.m_arg);
        BaseWalkVisitor<DummyOutsideInquiry>::visit_TypeInquiry(x);
    }

private:
    // The Var at the bottom of a designator such as x%a(i)(j:k), or nullptr.
    static const ASR::Var_t *base_var(const ASR::expr_t *e) {
        while (true) {
            switch (e->type) {
                case ASR::exprType::Var:
                    return ASR::down_cast<ASR::Var_t>(e);
                case ASR::exprType::StructInstanceMember:
                    e = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v;
                    break;
                case ASR::exprType::ArrayItem:
                    e = ASR::down_cast<ASR::ArrayItem_t>(e)->m_v;
                    break;
                case ASR::exprType::ArraySection:
                    e = ASR::down_cast<ASR::ArraySection_t>(e)->m_v;
                    break;
                case ASR::exprType::StringItem:
                    e = ASR::down_cast<ASR::StringItem_t>(e)->m_arg;
                    break;
                case ASR::exprType::StringSection:
                    e = ASR::down_cast<ASR::StringSection_t>(e)->m_arg;
                    break;
                default:
                    return nullptr;
            }
        }
    }
};

} // namespace

bool ElementalChecks::result_type_params(const ASR::Function_t &f,
        const ASR::Variable_t &r, const std::string &proc_name,
        diag::Diagnostics &diag) {
    ASR::ttype_t *type = ASRUtils::extract_type(r.m_type);
    if (!ASR::is_a<ASR::String_t>(*type)) return true;
    ASR::expr_t *len = ASR::down_cast<ASR::String_t>(type)->m_len;
    if (len == nullptr) return true;
    DummyOutsideInquiry v(f);
    v.visit_expr(*len);
    if (v.found == nullptr) return true;
    std::string dummy = ASRUtils::symbol_name(v.found->m_v);
    diag.add(diag::Diagnostic(
        "result length of elemental function '" + proc_name +
        "' can use dummy argument '" + dummy +
        "' only as the argument of an inquiry function such as len",
        diag::Level::Error, diag::Stage::Semantic, {
            diag::Label("", {v.found->base.base.loc})}));
    return false;
}

bool ElementalChecks::result_derived_type_params(const ASR::Function_t &,
        const ASR::Variable_t &, const std::string &, diag::Diagnostics &) {
    // TODO: check the length parameter expressions of a derived-type result
    return true;
}

} // namespace LCompilers::LFortran
