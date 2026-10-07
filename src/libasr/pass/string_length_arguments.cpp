#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/string_length_arguments.h>

#include <map>
#include <set>
#include <vector>
#include <string>

/*
Character dummy arguments: data pointer plus hidden length
==========================================================

A nonallocatable, nonpointer character dummy argument (a scalar, or an
explicit-shape or assumed-size array) of a procedure without BIND(C) is passed
as a pointer to its data, and its length (the element length for an array) is
passed by value in a hidden argument after all the explicit arguments, in the
order of the character dummies. This is the convention of gfortran and the
other Unix Fortran compilers:

```fortran
subroutine show(a, n, b)
    character(len=*) :: a
    integer :: n
    character :: b
end subroutine

call show(s, n, x(2))       ! s is character(len=5), x is character :: x(2)
```

becomes

```fortran
subroutine show(a, n, b, __libasr_len_a, __libasr_len_b)
    integer(8), value, intent(in) :: __libasr_len_a, __libasr_len_b
    character(len=__libasr_len_a) :: a
    integer :: n
    character :: b
end subroutine

call show(s, n, x(2), 5_8, 1_8)
```

which is `void show(char *a, int *n, char *b, int64_t _a, int64_t _b)` called
as `show(&s, &n, &x[1], 5, 1)`.

ASRUtils::is_string_dummy_with_hidden_length() decides which dummies get a
hidden length, and the code generators use it to pass those dummies as their
data pointer. An assumed-length dummy takes its length from its hidden
argument. Any other keeps its declared length and ignores its hidden argument,
which every call passes all the same, as gfortran's do.

The length of an actual whose length is not a constant is taken from the
actual itself, so a call with a side effect in the actual (in a subscript, say)
is moved to a temporary before the statement, to be evaluated only once.

The pass runs after the other lowering passes, so that every call is in place,
and after the .mod files are written, so they keep the argument lists as
declared: every translation unit applies it to the procedures it reads from
them. ASR verification checks the result (ASRVerifyOptions::
string_length_arguments).

The pass adds the hidden arguments again each time it runs, so PassManager
runs it at most once on a translation unit. The LLVM and C code generators
rely on it, so they run it even if the passes selected (--pass, --skip-pass)
leave it out (PassManager::apply_string_length_arguments).
*/
namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

namespace {

// The procedures this pass gave hidden arguments, with their number of
// arguments before it did.
typedef std::map<ASR::symbol_t*, size_t> TransformedProcedures;

ASR::ttype_t* hidden_length_type(Allocator &al, const Location &loc) {
    return ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 8));
}

// The String inside a scalar or array character type.
ASR::String_t* string_of(ASR::ttype_t *t) {
    return down_cast<ASR::String_t>(ASRUtils::type_get_past_array(t));
}

// A copy of the type `t` of a dummy, with the same array physical type.
ASR::ttype_t* duplicate_dummy_type(Allocator &al, ASR::ttype_t *t) {
    if (ASR::is_a<ASR::Array_t>(*t)) {
        return ASRUtils::duplicate_type(al, t, nullptr,
            down_cast<ASR::Array_t>(t)->m_physical_type, true);
    }
    return ASRUtils::duplicate_type(al, t);
}

// Gives each character dummy of `fn` that is passed with a hidden length its
// hidden `integer(8), value, intent(in)` dummy, appended after all the other
// dummies, and makes it the length of an assumed-length dummy.
void add_hidden_length_dummies(Allocator &al, ASR::Function_t *fn,
        TransformedProcedures &transformed) {
    ASR::symbol_t *fn_sym = &fn->base;
    if (transformed.find(fn_sym) != transformed.end()) return;
    ASR::FunctionType_t *ft = ASRUtils::get_FunctionType(fn);
    if (!ASRUtils::has_hidden_string_lengths(*ft)) return;
    LCOMPILERS_ASSERT(ft->n_arg_types == fn->n_args);
    std::vector<size_t> strings;
    for (size_t i = 0; i < fn->n_args; i++) {
        if (ASRUtils::is_string_dummy_with_hidden_length(*fn, fn->m_args[i])) {
            strings.push_back(i);
        }
    }
    if (strings.empty()) return;

    size_t n = fn->n_args;
    Vec<ASR::expr_t*> args;
    args.reserve(al, n + strings.size());
    Vec<ASR::ttype_t*> arg_types;
    arg_types.reserve(al, n + strings.size());
    for (size_t i = 0; i < n; i++) {
        args.push_back(al, fn->m_args[i]);
        arg_types.push_back(al, ft->m_arg_types[i]);
    }
    for (size_t i : strings) {
        ASR::Variable_t *v = ASRUtils::EXPR2VAR(fn->m_args[i]);
        const Location &loc = v->base.base.loc;
        std::string name = fn->m_symtab->get_unique_name(
            "__libasr_len_" + std::string(v->m_name), false);
        ASR::symbol_t *hidden = down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(al, loc, fn->m_symtab,
                s2c(al, name), nullptr, 0, ASR::intentType::In, nullptr,
                nullptr, ASR::storage_typeType::Default,
                hidden_length_type(al, loc), nullptr, ASR::abiType::Source,
                ASR::accessType::Public, ASR::presenceType::Required,
                true));
        fn->m_symtab->add_symbol(name, hidden);
        size_t hidden_index = args.size();
        args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc, hidden)));
        arg_types.push_back(al, hidden_length_type(al, loc));

        if (string_of(v->m_type)->m_len_kind
                == ASR::string_length_kindType::AssumedLength) {
            // The length of an assumed-length dummy is its hidden argument.
            v->m_type = duplicate_dummy_type(al, v->m_type);
            ASR::String_t *str = string_of(v->m_type);
            str->m_len = ASRUtils::EXPR(ASR::make_Var_t(al, loc, hidden));
            str->m_len_kind = ASR::string_length_kindType::ExpressionLength;
            Vec<char*> deps;
            deps.reserve(al, v->n_dependencies + 1);
            for (size_t j = 0; j < v->n_dependencies; j++) {
                deps.push_back(al, v->m_dependencies[j]);
            }
            deps.push_back(al, s2c(al, name));
            v->m_dependencies = deps.p;
            v->n_dependencies = deps.size();

            ASR::ttype_t *signature_type = duplicate_dummy_type(al,
                ft->m_arg_types[i]);
            ASR::String_t *signature_str = string_of(signature_type);
            signature_str->m_len = ASRUtils::EXPR(ASR::make_FunctionParam_t(
                al, loc, hidden_index, hidden_length_type(al, loc), nullptr));
            signature_str->m_len_kind =
                ASR::string_length_kindType::ExpressionLength;
            arg_types.p[i] = signature_type;
        }
    }
    fn->m_args = args.p;
    fn->n_args = args.size();
    // The signature is changed in place: expressions such as a procedure
    // pointer component share it rather than holding a copy.
    ft->m_arg_types = arg_types.p;
    ft->n_arg_types = arg_types.size();
    transformed[fn_sym] = n;
}

// Every procedure that is not a template.
void collect_procedures(SymbolTable *symtab,
        std::vector<ASR::Function_t*> &procedures) {
    for (auto &item : symtab->get_scope()) {
        ASR::symbol_t *sym = item.second;
        switch (sym->type) {
            case ASR::symbolType::Function: {
                ASR::Function_t *fn = down_cast<ASR::Function_t>(sym);
                procedures.push_back(fn);
                collect_procedures(fn->m_symtab, procedures);
                break;
            }
            case ASR::symbolType::Program:
            case ASR::symbolType::Module:
            case ASR::symbolType::Struct:
            case ASR::symbolType::Block:
            case ASR::symbolType::AssociateBlock: {
                collect_procedures(ASRUtils::symbol_symtab(sym), procedures);
                break;
            }
            default: {
                break;
            }
        }
    }
}

ASR::symbol_t* transformed_procedure(ASR::symbol_t *s,
        const TransformedProcedures &transformed) {
    if (s == nullptr) return nullptr;
    s = ASRUtils::symbol_get_past_external(s);
    if (s != nullptr && is_a<ASR::Function_t>(*s) &&
            transformed.find(s) != transformed.end()) {
        return s;
    }
    return nullptr;
}

// A procedure entity (a procedure pointer, a dummy procedure, a procedure
// component) and a FunctionPointerCast hold their own copy of their
// interface's FunctionType, so they have to take its new signature. So does
// the signature of a procedure with such a dummy.
class UpdateProcedureTypes: public ASR::BaseWalkVisitor<UpdateProcedureTypes> {
public:
    Allocator &al;
    const TransformedProcedures &transformed;
    std::set<ASR::symbol_t*> retyped;

    UpdateProcedureTypes(Allocator &al_,
        const TransformedProcedures &transformed_):
        al{al_}, transformed{transformed_} {}

    void visit_Variable(const ASR::Variable_t &x) {
        ASR::BaseWalkVisitor<UpdateProcedureTypes>::visit_Variable(x);
        ASR::symbol_t *decl = transformed_procedure(x.m_type_declaration,
            transformed);
        if (decl == nullptr || !is_a<ASR::FunctionType_t>(
                *ASRUtils::type_get_past_pointer(x.m_type))) {
            return;
        }
        ASR::Variable_t &xx = const_cast<ASR::Variable_t&>(x);
        ASR::ttype_t *signature =
            down_cast<ASR::Function_t>(decl)->m_function_signature;
        if (ASRUtils::is_pointer(xx.m_type)) {
            xx.m_type = ASRUtils::TYPE(ASR::make_Pointer_t(al,
                xx.base.base.loc, signature));
        } else {
            xx.m_type = signature;
        }
        retyped.insert(&xx.base);
    }

    void visit_FunctionPointerCast(const ASR::FunctionPointerCast_t &x) {
        ASR::BaseWalkVisitor<UpdateProcedureTypes>::visit_FunctionPointerCast(x);
        ASR::symbol_t *to = transformed_procedure(x.m_to, transformed);
        if (to == nullptr) return;
        const_cast<ASR::FunctionPointerCast_t&>(x).m_type =
            down_cast<ASR::Function_t>(to)->m_function_signature;
    }

    void visit_Function(const ASR::Function_t &x) {
        ASR::BaseWalkVisitor<UpdateProcedureTypes>::visit_Function(x);
        ASR::FunctionType_t *ft = ASRUtils::get_FunctionType(x);
        if (ft->n_arg_types != x.n_args) return;
        for (size_t i = 0; i < x.n_args; i++) {
            if (!is_a<ASR::Var_t>(*x.m_args[i])) continue;
            ASR::symbol_t *arg = down_cast<ASR::Var_t>(x.m_args[i])->m_v;
            if (retyped.find(arg) != retyped.end()) {
                ft->m_arg_types[i] = down_cast<ASR::Variable_t>(arg)->m_type;
            } else if (ASR::symbol_t *proc = transformed_procedure(arg,
                    transformed)) {
                ft->m_arg_types[i] =
                    down_cast<ASR::Function_t>(proc)->m_function_signature;
            }
        }
    }
};

// True if evaluating the call `x` can have a side effect.
bool is_impure_call(const ASR::FunctionCall_t &x) {
    ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_name);
    if (!is_a<ASR::Function_t>(*s)) return true;
    ASR::FunctionType_t *ft = ASRUtils::get_FunctionType(
        down_cast<ASR::Function_t>(s));
    return !ft->m_pure && ft->m_abi != ASR::abiType::Intrinsic;
}

// Replaces each call with a side effect in an expression with a temporary
// assigned before the statement, so that the expression can be evaluated
// twice: once as the actual, once for its length.
class HoistImpureCalls: public ASR::BaseExprReplacer<HoistImpureCalls> {
public:
    Allocator &al;
    SymbolTable *scope;
    Vec<ASR::stmt_t*> &before;

    HoistImpureCalls(Allocator &al_, SymbolTable *scope_,
        Vec<ASR::stmt_t*> &before_): al{al_}, scope{scope_}, before{before_} {
        // A node's `m_value` is not evaluated, and it can be shared with
        // expressions outside this statement's scope.
        call_replacer_on_value = false;
    }

    // Only the value is evaluated; the type's expressions are not.
    void replace_ttype(ASR::ttype_t* /*t*/) {}

    void hoist(ASR::expr_t *value) {
        ASR::ttype_t *t = ASRUtils::expr_type(value);
        const Location &loc = value->base.loc;
        ASR::ttype_t *tmp_type = nullptr;
        bool realloc = false;
        switch (t->type) {
            case ASR::ttypeType::Integer:
            case ASR::ttypeType::UnsignedInteger:
            case ASR::ttypeType::Real:
            case ASR::ttypeType::Complex:
            case ASR::ttypeType::Logical: {
                tmp_type = ASRUtils::duplicate_type(al, t);
                break;
            }
            case ASR::ttypeType::String: {
                tmp_type = ASRUtils::TYPE(ASR::make_Allocatable_t(al, loc,
                    ASRUtils::TYPE(ASR::make_String_t(al, loc,
                        down_cast<ASR::String_t>(t)->m_kind, nullptr,
                        ASR::string_length_kindType::DeferredLength,
                        ASR::string_physical_typeType::DescriptorString))));
                realloc = true;
                break;
            }
            default: {
                // Not a scalar of an intrinsic type: it stays where it is.
                return;
            }
        }
        std::string name = scope->get_unique_name("__libasr_string_arg_", false);
        ASR::symbol_t *tmp = down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(al, loc, scope, s2c(al, name),
                nullptr, 0, ASR::intentType::Local, nullptr, nullptr,
                ASR::storage_typeType::Default, tmp_type, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Required, false));
        scope->add_symbol(name, tmp);
        ASR::expr_t *var = ASRUtils::EXPR(ASR::make_Var_t(al, loc, tmp));
        before.push_back(al, ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
            al, loc, var, value, nullptr, realloc, false)));
        *current_expr = var;
    }

    void replace_FunctionCall(ASR::FunctionCall_t *x) {
        if (is_impure_call(*x)) {
            hoist(&x->base);
        } else {
            ASR::BaseExprReplacer<HoistImpureCalls>::replace_FunctionCall(x);
        }
    }

    void replace_IntrinsicImpureFunction(ASR::IntrinsicImpureFunction_t *x) {
        hoist(&x->base);
    }
};

class FindImpureCall: public ASR::BaseWalkVisitor<FindImpureCall> {
public:
    bool found = false;

    FindImpureCall() {
        visit_compile_time_value = false;
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        if (is_impure_call(x)) found = true;
        ASR::BaseWalkVisitor<FindImpureCall>::visit_FunctionCall(x);
    }

    void visit_IntrinsicImpureFunction(const ASR::IntrinsicImpureFunction_t &/*x*/) {
        found = true;
    }

    // Only the value is evaluated; the type's expressions are not.
    void visit_ttype(const ASR::ttype_t &/*x*/) {}
};

// Appends the lengths of the character actuals to every call of a procedure
// that got hidden length dummies.
class AddHiddenLengthActuals: public ASR::ASRPassBaseWalkVisitor<AddHiddenLengthActuals> {
public:
    Allocator &al;
    const TransformedProcedures &transformed;
    // The statements to insert before the statement being visited, or null
    // where an expression cannot be moved before it (a loop condition).
    Vec<ASR::stmt_t*> *before = nullptr;

    AddHiddenLengthActuals(Allocator &al_,
        const TransformedProcedures &transformed_):
        al{al_}, transformed{transformed_} {}

    void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
        Vec<ASR::stmt_t*> *before_copy = before;
        Vec<ASR::stmt_t*> body;
        body.reserve(al, n_body);
        for (size_t i = 0; i < n_body; i++) {
            Vec<ASR::stmt_t*> stmt_before;
            stmt_before.reserve(al, 0);
            before = &stmt_before;
            visit_stmt(*m_body[i]);
            for (size_t j = 0; j < stmt_before.size(); j++) {
                body.push_back(al, stmt_before[j]);
            }
            body.push_back(al, m_body[i]);
        }
        before = before_copy;
        m_body = body.p;
        n_body = body.size();
    }

    // The condition of a loop is evaluated on every iteration, so what it
    // needs evaluated before it goes at the start of each iteration:
    // `do while (c)` becomes `do; if (.not. c) exit; ...`.
    void visit_WhileLoop(const ASR::WhileLoop_t &x) {
        ASR::WhileLoop_t &xx = const_cast<ASR::WhileLoop_t&>(x);
        Vec<ASR::stmt_t*> *before_copy = before;
        Vec<ASR::stmt_t*> test_before;
        test_before.reserve(al, 0);
        before = x.n_orelse == 0 ? &test_before : nullptr;
        visit_expr(*x.m_test);
        before = before_copy;
        transform_stmts(xx.m_body, xx.n_body);
        transform_stmts(xx.m_orelse, xx.n_orelse);
        if (test_before.size() == 0) return;
        const Location &loc = x.m_test->base.loc;
        ASR::ttype_t *logical = ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4));
        Vec<ASR::stmt_t*> exit;
        exit.reserve(al, 1);
        exit.push_back(al, ASRUtils::STMT(ASR::make_Exit_t(al, loc, x.m_name)));
        ASR::stmt_t *exit_if = ASRUtils::STMT(ASR::make_If_t(al, loc, nullptr,
            ASRUtils::EXPR(ASR::make_LogicalNot_t(al, loc, x.m_test, logical,
                nullptr)), exit.p, exit.size(), nullptr, 0));
        Vec<ASR::stmt_t*> body;
        body.reserve(al, test_before.size() + 1 + x.n_body);
        for (size_t i = 0; i < test_before.size(); i++) {
            body.push_back(al, test_before[i]);
        }
        body.push_back(al, exit_if);
        for (size_t i = 0; i < x.n_body; i++) {
            body.push_back(al, x.m_body[i]);
        }
        xx.m_test = ASRUtils::EXPR(ASR::make_LogicalConstant_t(al, loc, true,
            logical));
        xx.m_body = body.p;
        xx.n_body = body.size();
    }

    // The procedure a call goes to, if the pass transformed it.
    ASR::Function_t* callee(ASR::symbol_t *name) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(name);
        if (is_a<ASR::Variable_t>(*s)) {
            s = down_cast<ASR::Variable_t>(s)->m_type_declaration;
        } else if (is_a<ASR::StructMethodDeclaration_t>(*s)) {
            s = down_cast<ASR::StructMethodDeclaration_t>(s)->m_proc;
        }
        s = transformed_procedure(s, transformed);
        return s ? down_cast<ASR::Function_t>(s) : nullptr;
    }

    // The length of the character actual `actual` (its element length if it
    // is an array) as an `integer(8)`. Absent is length 0.
    ASR::expr_t* actual_length(ASR::expr_t *&actual, const Location &loc) {
        ASR::ttype_t *int8 = hidden_length_type(al, loc);
        if (actual == nullptr) {
            return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 0,
                int8, ASR::integerbozType::Decimal));
        }
        ASR::expr_t *str_expr = actual;
        while (is_a<ASR::ArrayPhysicalCast_t>(*str_expr)) {
            str_expr = down_cast<ASR::ArrayPhysicalCast_t>(str_expr)->m_arg;
        }
        ASR::String_t *str = down_cast<ASR::String_t>(
            ASRUtils::extract_type(ASRUtils::expr_type(str_expr)));
        int64_t len = 0;
        if (str->m_len_kind == ASR::string_length_kindType::ExpressionLength &&
                str->m_len && ASRUtils::extract_value(
                    ASRUtils::expr_value(str->m_len), len)) {
            return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, len,
                int8, ASR::integerbozType::Decimal));
        }
        // The length is taken from the actual itself, which is then
        // evaluated twice: a call with a side effect in it is evaluated
        // once, before the statement.
        FindImpureCall impure;
        impure.visit_expr(*actual);
        if (impure.found && before != nullptr) {
            HoistImpureCalls hoist(al, current_scope, *before);
            hoist.current_expr = &actual;
            hoist.replace_expr(actual);
            str_expr = actual;
            while (is_a<ASR::ArrayPhysicalCast_t>(*str_expr)) {
                str_expr = down_cast<ASR::ArrayPhysicalCast_t>(str_expr)->m_arg;
            }
        }
        ASRUtils::ExprStmtDuplicator duplicator(al);
        duplicator.allow_procedure_calls = true;
        ASR::expr_t *copy = duplicator.duplicate_expr(str_expr);
        return ASRUtils::EXPR(ASR::make_StringLen_t(al, loc, copy, int8,
            nullptr));
    }

    template <typename T>
    void add_actuals(T &x) {
        ASR::Function_t *fn = callee(x.m_name);
        if (fn == nullptr) return;
        size_t n = transformed.at(&fn->base);
        if (x.n_args > n) return;  // Already has them.
        Vec<ASR::call_arg_t> args;
        args.reserve(al, fn->n_args);
        for (size_t i = 0; i < n; i++) {
            ASR::call_arg_t arg;
            arg.loc = x.base.base.loc;
            arg.m_value = nullptr;
            if (i < x.n_args) arg = x.m_args[i];
            args.push_back(al, arg);
        }
        for (size_t i = 0; i < n; i++) {
            if (!ASRUtils::is_string_dummy_with_hidden_length(*fn,
                    fn->m_args[i])) {
                continue;
            }
            ASR::call_arg_t arg;
            arg.loc = args[i].loc;
            arg.m_value = actual_length(args.p[i].m_value, arg.loc);
            args.push_back(al, arg);
        }
        LCOMPILERS_ASSERT(args.size() == fn->n_args);
        x.m_args = args.p;
        x.n_args = args.size();
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        ASR::ASRPassBaseWalkVisitor<AddHiddenLengthActuals>::visit_SubroutineCall(x);
        add_actuals(const_cast<ASR::SubroutineCall_t&>(x));
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        ASR::ASRPassBaseWalkVisitor<AddHiddenLengthActuals>::visit_FunctionCall(x);
        add_actuals(const_cast<ASR::FunctionCall_t&>(x));
    }
};

} // namespace

void pass_string_length_arguments(Allocator &al, ASR::TranslationUnit_t &unit,
        const LCompilers::PassOptions &/*pass_options*/) {
    // In interactive mode each cell is a TranslationUnit chained to copies of
    // the earlier cells (see FortranEvaluator::snapshot_cell_scope), which no
    // pass has seen. Their procedures were compiled with hidden lengths, so
    // this cell's view of them gets them too.
    std::vector<SymbolTable*> scopes;
    for (SymbolTable *s = unit.m_symtab; s != nullptr; s = s->parent) {
        if (ASRUtils::is_tu_scope(s)) scopes.push_back(s);
    }
    std::vector<ASR::Function_t*> procedures;
    for (SymbolTable *s : scopes) {
        collect_procedures(s, procedures);
    }
    // A signature is changed in place, so a procedure that shares its
    // signature object with another one first gets a copy of its own.
    std::set<ASR::ttype_t*> signatures;
    for (ASR::Function_t *fn : procedures) {
        if (!signatures.insert(fn->m_function_signature).second) {
            ASR::FunctionType_t *ft = ASRUtils::get_FunctionType(fn);
            fn->m_function_signature = ASRUtils::TYPE(ASR::make_FunctionType_t(
                al, ft->base.base.loc, ft->m_arg_types, ft->n_arg_types,
                ft->m_return_var_type, ft->m_abi, ft->m_deftype,
                ft->m_bindc_name, ft->m_elemental, ft->m_pure, ft->m_module,
                ft->m_inline, ft->m_static, ft->m_restrictions,
                ft->n_restrictions, ft->m_is_restriction, ft->m_exec_space));
        }
    }
    TransformedProcedures transformed;
    for (ASR::Function_t *fn : procedures) {
        add_hidden_length_dummies(al, fn, transformed);
    }
    if (transformed.empty()) return;

    UpdateProcedureTypes u(al, transformed);
    for (SymbolTable *s : scopes) {
        for (auto &item : s->get_scope()) {
            u.visit_symbol(*item.second);
        }
    }
    AddHiddenLengthActuals a(al, transformed);
    a.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor d(al);
    d.visit_TranslationUnit(unit);
}

} // namespace LCompilers
