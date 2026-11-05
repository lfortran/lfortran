#include <lfortran/semantics/elemental_checks.h>
#include <lfortran/semantics/semantic_exception.h>
#include <libasr/asr_utils.h>
#include <libasr/utils.h>

#include <algorithm>
#include <set>

namespace LCompilers::LFortran {

void ElementalChecks::check_procedure(const ASR::Function_t &f,
        const std::string &proc_name, diag::Diagnostics &diag,
        const CompilerOptions &compiler_options) {
    bool elemental = ASRUtils::get_FunctionType(f)->m_elemental;
    bool ok = true;
    for (size_t i = 0; i < f.n_args; i++) {
        if (!ASR::is_a<ASR::Var_t>(*f.m_args[i])) continue;
        ASR::symbol_t *sym = ASR::down_cast<ASR::Var_t>(f.m_args[i])->m_v;
        if (ASR::is_a<ASR::Variable_t>(*sym)) {
            if (!elemental) continue;
            const ASR::Variable_t &v = *ASR::down_cast<ASR::Variable_t>(sym);
            ok = dummy_not_procedure(*sym, proc_name, diag) && ok;
            ok = dummy_scalar(v, proc_name, diag) && ok;
            ok = dummy_not_allocatable(v, proc_name, diag) && ok;
            ok = dummy_not_pointer(v, proc_name, diag) && ok;
            ok = dummy_not_coarray(v, proc_name, diag) && ok;
            ok = no_alternate_return(v, proc_name, diag) && ok;
            ok = dummy_intent_given(v, proc_name, diag) && ok;
        } else if (ASR::is_a<ASR::Function_t>(*sym)) {
            const ASR::Function_t &dummy = *ASR::down_cast<ASR::Function_t>(sym);
            ok = not_dummy_interface(dummy.m_name, dummy.m_function_signature,
                false, sym->base.loc, diag) && ok;
            if (!elemental) continue;
            ok = dummy_not_procedure(*sym, proc_name, diag) && ok;
        } else if (elemental) {
            throw LCompilersException("unexpected kind of dummy argument '" +
                std::string(ASRUtils::symbol_name(sym)) +
                "' of elemental procedure '" + proc_name + "'");
        }
    }
    if (elemental && f.m_return_var && ASR::is_a<ASR::Var_t>(*f.m_return_var)) {
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

class ElementalChecks::BodyWalk : public ASR::BaseWalkVisitor<BodyWalk> {
public:
    bool ok = true;

    BodyWalk(const SymbolTable &scope, diag::Diagnostics &diag,
            const CompilerOptions &compiler_options)
        : scope(scope), diag(diag), compiler_options(compiler_options) {
        visit_compile_time_value = false;
    }

    // Contained procedures and interface bodies are checked on their own.
    void visit_Function(const ASR::Function_t &) {}

    void visit_Variable(const ASR::Variable_t &x) {
        ok = not_pointer_target(x.m_symbolic_value, diag) && ok;
        BaseWalkVisitor<BodyWalk>::visit_Variable(x);
    }

    void visit_Associate(const ASR::Associate_t &x) {
        ok = not_pointer_target(x.m_value, diag) && ok;
        BaseWalkVisitor<BodyWalk>::visit_Associate(x);
    }

    void visit_StructConstructor(const ASR::StructConstructor_t &x) {
        check_component_values(x.m_args, x.n_args);
        BaseWalkVisitor<BodyWalk>::visit_StructConstructor(x);
    }

    void visit_StructConstant(const ASR::StructConstant_t &x) {
        check_component_values(x.m_args, x.n_args);
        BaseWalkVisitor<BodyWalk>::visit_StructConstant(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        check_call(*x.m_name, x.m_args, x.n_args, x.base.base.loc);
        BaseWalkVisitor<BodyWalk>::visit_FunctionCall(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        check_call(*x.m_name, x.m_args, x.n_args, x.base.base.loc);
        BaseWalkVisitor<BodyWalk>::visit_SubroutineCall(x);
    }

private:
    const SymbolTable &scope;
    diag::Diagnostics &diag;
    const CompilerOptions &compiler_options;

    void check_component_values(ASR::call_arg_t *args, size_t n_args) {
        for (size_t i = 0; i < n_args; i++) {
            ok = not_pointer_target(args[i].m_value, diag) && ok;
        }
    }

    void check_call(ASR::symbol_t &name, ASR::call_arg_t *args,
            size_t n_args, const Location &loc) {
        for (size_t i = 0; i < n_args; i++) {
            ok = not_actual_argument(args[i].m_value, diag) && ok;
        }
        // An interactive cell calls the procedures of earlier cells.
        if (!compiler_options.interactive) {
            ok = explicit_interface(name, scope, loc, diag) && ok;
        }
        const ASR::Function_t *f = elemental_procedure(name);
        if (f == nullptr) return;
        for (size_t i = 0; i < n_args && i < f->n_args; i++) {
            if (args[i].m_value == nullptr) continue;
            if (!ASR::is_a<ASR::Var_t>(*f->m_args[i])) continue;
            ASR::symbol_t *dummy = ASR::down_cast<ASR::Var_t>(f->m_args[i])->m_v;
            if (!ASR::is_a<ASR::Variable_t>(*dummy)) continue;
            const ASR::Variable_t &v = *ASR::down_cast<ASR::Variable_t>(dummy);
            if (v.m_intent != ASR::intentType::Out
                    && v.m_intent != ASR::intentType::InOut) continue;
            ok = output_no_repeated_vector_subscript(args[i].m_value, v, diag)
                && ok;
        }
        if (f->m_return_var != nullptr) {
            ok = function_actuals_conform(*f, args, n_args, diag) && ok;
        } else {
            ok = subroutine_outputs_arrays(*f, args, n_args, diag) && ok;
            ok = subroutine_inputs_conform(*f, args, n_args, diag) && ok;
        }
    }

    // The called procedure if it is elemental, else nullptr.
    static const ASR::Function_t *elemental_procedure(ASR::symbol_t &name) {
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(&name);
        if (ASR::is_a<ASR::StructMethodDeclaration_t>(*sym)) {
            sym = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::StructMethodDeclaration_t>(sym)->m_proc);
        }
        if (!ASR::is_a<ASR::Function_t>(*sym)) return nullptr;
        const ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(sym);
        return ASRUtils::get_FunctionType(*f)->m_elemental ? f : nullptr;
    }
};

void ElementalChecks::check_body(const ASR::Function_t &f,
        diag::Diagnostics &diag, const CompilerOptions &compiler_options) {
    check_scope(*f.m_symtab, f.m_body, f.n_body, diag, compiler_options);
}

void ElementalChecks::check_body(const ASR::Program_t &p,
        diag::Diagnostics &diag, const CompilerOptions &compiler_options) {
    check_scope(*p.m_symtab, p.m_body, p.n_body, diag, compiler_options);
}

void ElementalChecks::check_body(const ASR::Module_t &m,
        diag::Diagnostics &diag, const CompilerOptions &compiler_options) {
    check_scope(*m.m_symtab, nullptr, 0, diag, compiler_options);
}

void ElementalChecks::check_scope(const SymbolTable &scope,
        ASR::stmt_t **body, size_t n_body, diag::Diagnostics &diag,
        const CompilerOptions &compiler_options) {
    BodyWalk walk(scope, diag, compiler_options);
    for (auto &a : scope.get_scope()) {
        walk.visit_symbol(*a.second);
    }
    for (size_t i = 0; i < n_body; i++) {
        walk.visit_stmt(*body[i]);
    }
    if (!walk.ok && !compiler_options.continue_compilation) {
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

bool ElementalChecks::is_nonintrinsic_elemental(ASR::symbol_t *sym) {
    if (sym == nullptr) return false;
    sym = ASRUtils::symbol_get_past_external(sym);
    return ASR::is_a<ASR::Function_t>(*sym)
        && !ASRUtils::is_intrinsic_symbol(sym)
        && ASRUtils::get_FunctionType(
            ASR::down_cast<ASR::Function_t>(sym))->m_elemental;
}

bool ElementalChecks::not_pointer_target(ASR::expr_t *value,
        diag::Diagnostics &diag) {
    if (value == nullptr || !ASR::is_a<ASR::Var_t>(*value)) return true;
    ASR::symbol_t *target = ASR::down_cast<ASR::Var_t>(value)->m_v;
    if (!is_nonintrinsic_elemental(target)) return true;
    diag.add(diag::Diagnostic(
        "nonintrinsic elemental procedure '"
        + std::string(ASRUtils::symbol_name(target))
        + "' cannot be a procedure pointer target",
        diag::Level::Error, diag::Stage::Semantic, {
            diag::Label("a procedure pointer target shall not be a "
                "nonintrinsic elemental procedure", {value->base.loc})}));
    return false;
}

bool ElementalChecks::not_actual_argument(ASR::expr_t *actual,
        diag::Diagnostics &diag) {
    if (actual == nullptr || !ASR::is_a<ASR::Var_t>(*actual)) return true;
    ASR::symbol_t *proc = ASR::down_cast<ASR::Var_t>(actual)->m_v;
    if (!is_nonintrinsic_elemental(proc)) return true;
    diag.add(diag::Diagnostic(
        "nonintrinsic elemental procedure '"
        + std::string(ASRUtils::symbol_name(proc))
        + "' cannot be an actual argument",
        diag::Level::Error, diag::Stage::Semantic, {
            diag::Label("a nonintrinsic elemental procedure shall not be "
                "used as an actual argument", {actual->base.loc})}));
    return false;
}

bool ElementalChecks::explicit_interface(const ASR::symbol_t &proc,
        const SymbolTable &scope, const Location &loc, diag::Diagnostics &diag) {
    if (!ASR::is_a<ASR::Function_t>(proc)) return true;
    const ASR::Function_t &f = *ASR::down_cast<ASR::Function_t>(&proc);
    if (!ASRUtils::get_FunctionType(f)->m_elemental) return true;
    // Only a call through an implicit interface names an external procedure.
    if (!ASRUtils::is_tu_scope(f.m_symtab->parent)) return true;
    for (const SymbolTable *s = &scope; s != nullptr; s = s->parent) {
        if (s == f.m_symtab) return true;
    }
    diag.add(diag::Diagnostic(
        "elemental procedure '" + std::string(f.m_name)
        + "' needs an explicit interface",
        diag::Level::Error, diag::Stage::Semantic, {
            diag::Label("put '" + std::string(f.m_name) + "' in a module or "
                "declare it in an interface block", {loc})}));
    return false;
}

bool ElementalChecks::output_no_repeated_vector_subscript(
        ASR::expr_t *actual, const ASR::Variable_t &dummy,
        diag::Diagnostics &diag) {
    ASR::array_index_t *indices = nullptr;
    size_t n_indices = 0;
    if (ASR::is_a<ASR::ArrayItem_t>(*actual)) {
        ASR::ArrayItem_t *item = ASR::down_cast<ASR::ArrayItem_t>(actual);
        indices = item->m_args;
        n_indices = item->n_args;
    } else if (ASR::is_a<ASR::ArraySection_t>(*actual)) {
        ASR::ArraySection_t *section = ASR::down_cast<ASR::ArraySection_t>(actual);
        indices = section->m_args;
        n_indices = section->n_args;
    }
    for (size_t i = 0; i < n_indices; i++) {
        ASR::expr_t *subscript = indices[i].m_right;
        if (indices[i].m_left != nullptr || indices[i].m_step != nullptr
                || subscript == nullptr
                || !ASRUtils::is_array(ASRUtils::expr_type(subscript))) {
            continue;
        }
        ASR::expr_t *value = ASRUtils::expr_value(subscript);
        if (value == nullptr || !ASR::is_a<ASR::ArrayConstant_t>(*value)) {
            continue;
        }
        ASR::ArrayConstant_t *values = ASR::down_cast<ASR::ArrayConstant_t>(value);
        std::set<std::string> seen;
        int64_t n = ASRUtils::get_fixed_size_of_array(values->m_type);
        for (int64_t k = 0; k < n; k++) {
            if (seen.insert(ASRUtils::fetch_ArrayConstant_value(values, k)).second) {
                continue;
            }
            diag.add(diag::Diagnostic(
                "array section with a repeated vector subscript value cannot "
                "be associated with intent(" + std::string(
                    dummy.m_intent == ASR::intentType::Out ? "out" : "inout")
                + ") dummy argument '" + std::string(dummy.m_name)
                + "' of an elemental subroutine",
                diag::Level::Error, diag::Stage::Semantic, {
                    diag::Label("a vector subscript with repeated values "
                        "makes the section not definable", {subscript->base.loc})}));
            return false;
        }
    }
    return true;
}

bool ElementalChecks::function_actuals_conform(const ASR::Function_t &f,
        ASR::call_arg_t *args, size_t n_args, diag::Diagnostics &diag) {
    ASR::expr_t *ref = nullptr;
    for (size_t i = 0; i < n_args; i++) {
        ASR::expr_t *actual = shape_actual(f, args, i);
        if (actual == nullptr) continue;
        if (ref == nullptr) {
            ref = actual;
            continue;
        }
        std::string actual_label, ref_label;
        if (same_shape(actual, ref, actual_label, ref_label)) continue;
        add_shape_error("the actual argument for dummy '"
            + std::string(dummy_variable(f, i)->m_name)
            + "' of elemental function '" + std::string(f.m_name)
            + "' must be a scalar or an array of the same shape as the other "
            "array arguments", actual, actual_label, ref, ref_label, diag);
        return false;
    }
    return true;
}

bool ElementalChecks::subroutine_outputs_arrays(const ASR::Function_t &f,
        ASR::call_arg_t *args, size_t n_args, diag::Diagnostics &diag) {
    ASR::expr_t *array = nullptr;
    for (size_t i = 0; i < n_args && array == nullptr; i++) {
        array = shape_actual(f, args, i);
    }
    if (array == nullptr) return true;
    ASR::expr_t *ref = nullptr;
    for (size_t i = 0; i < n_args; i++) {
        ASR::expr_t *actual = args[i].m_value;
        if (actual == nullptr || !is_output(f, i)
                || may_be_absent(f, i, actual)) {
            continue;
        }
        std::string actual_label = "a scalar", ref_label = "an array";
        if (ASRUtils::is_array(ASRUtils::expr_type(actual))) {
            if (ref == nullptr) {
                ref = actual;
                continue;
            }
            if (same_shape(actual, ref, actual_label, ref_label)) continue;
        }
        const ASR::Variable_t &dummy = *dummy_variable(f, i);
        add_shape_error("the actual argument for intent("
            + std::string(dummy.m_intent == ASR::intentType::Out ? "out" : "inout")
            + ") dummy '" + std::string(dummy.m_name) + "' of elemental "
            "subroutine '" + std::string(f.m_name) + "' must be an array of "
            "the same shape as the other output arrays, because another "
            "actual argument is an array", actual, actual_label,
            ref != nullptr ? ref : array, ref_label, diag);
        return false;
    }
    return true;
}

bool ElementalChecks::subroutine_inputs_conform(const ASR::Function_t &f,
        ASR::call_arg_t *args, size_t n_args, diag::Diagnostics &diag) {
    ASR::expr_t *ref = nullptr;
    for (size_t i = 0; i < n_args && ref == nullptr; i++) {
        if (is_output(f, i)) ref = shape_actual(f, args, i);
    }
    for (size_t i = 0; i < n_args && ref == nullptr; i++) {
        ref = shape_actual(f, args, i);
    }
    for (size_t i = 0; i < n_args; i++) {
        ASR::expr_t *actual = shape_actual(f, args, i);
        if (actual == nullptr || actual == ref || is_output(f, i)) continue;
        std::string actual_label, ref_label;
        if (same_shape(actual, ref, actual_label, ref_label)) continue;
        add_shape_error("the actual argument for dummy '"
            + std::string(dummy_variable(f, i)->m_name)
            + "' of elemental subroutine '" + std::string(f.m_name)
            + "' must be a scalar or an array of the same shape as the other "
            "array arguments", actual, actual_label, ref, ref_label, diag);
        return false;
    }
    return true;
}

const ASR::Variable_t *ElementalChecks::dummy_variable(
        const ASR::Function_t &f, size_t i) {
    if (i >= f.n_args || !ASR::is_a<ASR::Var_t>(*f.m_args[i])) return nullptr;
    ASR::symbol_t *sym = ASR::down_cast<ASR::Var_t>(f.m_args[i])->m_v;
    if (!ASR::is_a<ASR::Variable_t>(*sym)) return nullptr;
    return ASR::down_cast<ASR::Variable_t>(sym);
}

bool ElementalChecks::is_output(const ASR::Function_t &f, size_t i) {
    const ASR::Variable_t *v = dummy_variable(f, i);
    return v != nullptr && (v->m_intent == ASR::intentType::Out
        || v->m_intent == ASR::intentType::InOut);
}

// 15.5.2.12
bool ElementalChecks::may_be_absent(const ASR::Function_t &f, size_t i,
        ASR::expr_t *actual) {
    const ASR::Variable_t *dummy = dummy_variable(f, i);
    if (dummy == nullptr
            || dummy->m_presence != ASR::presenceType::Optional) {
        return false;
    }
    ASR::Variable_t *v = ASRUtils::expr_to_variable_or_null(
        ASRUtils::get_past_array_physical_cast(actual));
    return ASRUtils::is_allocatable_or_pointer(ASRUtils::expr_type(actual))
        || (v != nullptr && v->m_presence == ASR::presenceType::Optional);
}

// The actual `i` if it is an array that is present, else nullptr.
ASR::expr_t *ElementalChecks::shape_actual(const ASR::Function_t &f,
        ASR::call_arg_t *args, size_t i) {
    ASR::expr_t *actual = args[i].m_value;
    if (actual == nullptr || dummy_variable(f, i) == nullptr
            || !ASRUtils::is_array(ASRUtils::expr_type(actual))
            || may_be_absent(f, i, actual)) {
        return nullptr;
    }
    return actual;
}

bool ElementalChecks::same_shape(ASR::expr_t *actual, ASR::expr_t *ref,
        std::string &actual_label, std::string &ref_label) {
    ASR::dimension_t *dims = nullptr, *ref_dims = nullptr;
    size_t rank = ASRUtils::extract_dimensions_from_ttype(
        ASRUtils::expr_type(actual), dims);
    size_t ref_rank = ASRUtils::extract_dimensions_from_ttype(
        ASRUtils::expr_type(ref), ref_dims);
    if (rank != ref_rank) {
        actual_label = "rank " + std::to_string(rank);
        ref_label = "rank " + std::to_string(ref_rank);
        return false;
    }
    for (size_t d = 0; d < rank; d++) {
        int64_t n = 0, ref_n = 0;
        if (dims[d].m_length == nullptr || ref_dims[d].m_length == nullptr
                || !ASRUtils::extract_value(
                    ASRUtils::expr_value(dims[d].m_length), n)
                || !ASRUtils::extract_value(
                    ASRUtils::expr_value(ref_dims[d].m_length), ref_n)) {
            continue;
        }
        n = std::max<int64_t>(n, 0);
        ref_n = std::max<int64_t>(ref_n, 0);
        if (n == ref_n) continue;
        std::string in_dim = " in dimension " + std::to_string(d + 1);
        actual_label = "extent " + std::to_string(n) + in_dim;
        ref_label = "extent " + std::to_string(ref_n) + in_dim;
        return false;
    }
    return true;
}

void ElementalChecks::add_shape_error(const std::string &message,
        ASR::expr_t *actual, const std::string &actual_label,
        ASR::expr_t *ref, const std::string &ref_label,
        diag::Diagnostics &diag) {
    diag.add(diag::Diagnostic(message,
        diag::Level::Error, diag::Stage::Semantic, {
            diag::Label(actual_label, {actual->base.loc}),
            diag::Label(ref_label, {ref->base.loc}, false)}));
}

bool ElementalChecks::not_dummy_interface(const std::string &name,
        ASR::ttype_t *type, bool is_pointer, const Location &loc,
        diag::Diagnostics &diag) {
    ASR::ttype_t *proc_type = ASRUtils::type_get_past_pointer(type);
    if (!ASR::is_a<ASR::FunctionType_t>(*proc_type)) return true;
    if (!ASR::down_cast<ASR::FunctionType_t>(proc_type)->m_elemental) {
        return true;
    }
    diag.add(diag::Diagnostic(
        std::string(is_pointer ? "procedure pointer '" : "dummy procedure '")
        + name + "' cannot have an elemental interface",
        diag::Level::Error, diag::Stage::Semantic, {
            diag::Label("", {loc})}));
    return false;
}

} // namespace LCompilers::LFortran
