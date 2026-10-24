#include <algorithm>
#include <map>
#include <set>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/gpu_offload_host_association.h>
#include <libasr/pass/gpu_offload_rewrite.h>
#include <libasr/pass/intrinsic_function_registry.h>

namespace LCompilers {

namespace {

// The variables of `scope` an ASR subtree names, and the procedures of
// `procedures` it calls.
class KernelScopeReferences :
        public ASR::BaseWalkVisitor<KernelScopeReferences> {
public:
    SymbolTable *scope;
    const std::set<ASR::Function_t*> &procedures;
    std::set<ASR::symbol_t*> variables;
    std::set<ASR::Function_t*> callees;

    KernelScopeReferences(SymbolTable *scope_,
            const std::set<ASR::Function_t*> &procedures_)
        : scope(scope_), procedures(procedures_) {}

    void visit_Var(const ASR::Var_t &x) {
        if (ASR::is_a<ASR::Variable_t>(*x.m_v)
                && ASRUtils::symbol_parent_symtab(x.m_v) == scope) {
            variables.insert(x.m_v);
        }
    }

    void called(ASR::symbol_t *name) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(name);
        if (s != nullptr && ASR::is_a<ASR::Function_t>(*s)) {
            ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(s);
            if (procedures.count(fn) > 0) callees.insert(fn);
        }
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        called(x.m_name);
        ASR::BaseWalkVisitor<KernelScopeReferences>::visit_FunctionCall(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        called(x.m_name);
        ASR::BaseWalkVisitor<KernelScopeReferences>::visit_SubroutineCall(x);
    }
};

// Points every name of a procedure's host at the procedure's own symbol.
class HostNameRetargeter :
        public ASR::BaseWalkVisitor<HostNameRetargeter> {
public:
    const std::map<ASR::symbol_t*, ASR::symbol_t*> &own;

    explicit HostNameRetargeter(
            const std::map<ASR::symbol_t*, ASR::symbol_t*> &own_)
        : own(own_) {}

    void visit_Var(const ASR::Var_t &x) {
        auto it = own.find(x.m_v);
        if (it != own.end()) const_cast<ASR::Var_t&>(x).m_v = it->second;
    }
};

struct LiftedProcedure {
    // The variables of the kernel the procedure is handed, in the order of
    // the dummies they become.
    std::vector<ASR::symbol_t*> passed;
    // A variable of the kernel to the procedure's own symbol for it.
    std::map<ASR::symbol_t*, ASR::symbol_t*> own;
};

// Hands every call of a lifted procedure, in the kernel and in the
// procedures, the variables of the kernel the procedure is given, as the
// caller names them.
class LiftedCallExtender :
        public ASR::BaseWalkVisitor<LiftedCallExtender> {
public:
    Allocator &al;
    const std::map<ASR::Function_t*, LiftedProcedure> &lifted;
    ASR::Function_t *caller = nullptr;
    std::set<const void*> extended;

    LiftedCallExtender(Allocator &al_,
            const std::map<ASR::Function_t*, LiftedProcedure> &lifted_)
        : al(al_), lifted(lifted_) {}

    void visit_Function(const ASR::Function_t &x) {
        ASR::Function_t *saved = caller;
        caller = const_cast<ASR::Function_t*>(&x);
        ASR::BaseWalkVisitor<LiftedCallExtender>::visit_Function(x);
        caller = saved;
    }

    void extend(ASR::symbol_t *name, ASR::call_arg_t *&args, size_t &n_args,
            const void *call, const Location &loc) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(name);
        if (s == nullptr || !ASR::is_a<ASR::Function_t>(*s)) return;
        auto it = lifted.find(ASR::down_cast<ASR::Function_t>(s));
        if (it == lifted.end() || it->second.passed.empty()) return;
        if (!extended.insert(call).second) return;
        auto own = lifted.find(caller);
        Vec<ASR::call_arg_t> extended_args;
        extended_args.reserve(al, n_args + it->second.passed.size());
        for (size_t i = 0; i < n_args; i++) {
            extended_args.push_back(al, args[i]);
        }
        for (ASR::symbol_t *v : it->second.passed) {
            ASR::symbol_t *actual = v;
            if (own != lifted.end()) {
                auto o = own->second.own.find(v);
                LCOMPILERS_ASSERT(o != own->second.own.end());
                actual = o->second;
            }
            ASR::call_arg_t arg;
            arg.loc = loc;
            arg.m_value = ASRUtils::EXPR(ASR::make_Var_t(al, loc, actual));
            extended_args.push_back(al, arg);
        }
        args = extended_args.p;
        n_args = extended_args.n;
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        ASR::BaseWalkVisitor<LiftedCallExtender>::visit_FunctionCall(x);
        ASR::FunctionCall_t &xx = const_cast<ASR::FunctionCall_t&>(x);
        extend(xx.m_name, xx.m_args, xx.n_args, &x, x.base.base.loc);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        ASR::BaseWalkVisitor<LiftedCallExtender>::visit_SubroutineCall(x);
        ASR::SubroutineCall_t &xx = const_cast<ASR::SubroutineCall_t&>(x);
        extend(xx.m_name, xx.m_args, xx.n_args, &x, x.base.base.loc);
    }
};

bool is_parameter(ASR::symbol_t *s) {
    return ASR::down_cast<ASR::Variable_t>(s)->m_storage
        == ASR::storage_typeType::Parameter;
}

void append_dummy(Allocator &al, ASR::Function_t &fn, ASR::symbol_t *dummy,
        ASR::ttype_t *type) {
    const Location &loc = dummy->base.loc;
    Vec<ASR::expr_t*> args;
    args.reserve(al, fn.n_args + 1);
    for (size_t i = 0; i < fn.n_args; i++) args.push_back(al, fn.m_args[i]);
    args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc, dummy)));
    fn.m_args = args.p;
    fn.n_args = args.n;
    // The signature may be shared with the procedure this one was copied
    // from, so a new one is made rather than the old one changed.
    ASR::FunctionType_t *t = ASRUtils::get_FunctionType(fn);
    Vec<ASR::ttype_t*> types;
    types.reserve(al, t->n_arg_types + 1);
    for (size_t i = 0; i < t->n_arg_types; i++) {
        types.push_back(al, t->m_arg_types[i]);
    }
    types.push_back(al, gpu_signature_type(al, type));
    fn.m_function_signature = ASRUtils::TYPE(ASR::make_FunctionType_t(al,
        t->base.base.loc, types.p, types.n, t->m_return_var_type, t->m_abi,
        t->m_deftype, t->m_bindc_name, t->m_elemental, t->m_pure,
        t->m_module, t->m_inline, t->m_static, t->m_restrictions,
        t->n_restrictions, t->m_is_restriction, t->m_exec_space));
}

} // namespace

ASR::ttype_t* gpu_signature_type(Allocator &al, ASR::ttype_t *type) {
    if (!ASR::is_a<ASR::Array_t>(*type)) return type;
    ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
    auto constant_bound = [&](ASR::expr_t *bound) -> ASR::expr_t* {
        int64_t value = 0;
        if (bound == nullptr || !ASRUtils::extract_value(
                ASRUtils::expr_value(bound), value)) {
            return nullptr;
        }
        return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, bound->base.loc,
            value, ASRUtils::expr_type(bound)));
    };
    ASR::dimension_t *dims = al.allocate<ASR::dimension_t>(arr->n_dims);
    for (size_t d = 0; d < arr->n_dims; d++) {
        dims[d].loc = arr->m_dims[d].loc;
        dims[d].m_start = constant_bound(arr->m_dims[d].m_start);
        dims[d].m_length = constant_bound(arr->m_dims[d].m_length);
    }
    return ASRUtils::TYPE(ASR::make_Array_t(al, arr->base.base.loc,
        arr->m_type, dims, arr->n_dims, arr->m_physical_type,
        arr->m_memory_space));
}

namespace {

// Lifts what the procedures `kernel` contains name of `kernel`.
void lift_into_contained(Allocator &al, ASR::Function_t &kernel) {
    SymbolTable *scope = kernel.m_symtab;
    std::set<ASR::Function_t*> procedures;
    for (auto &item : scope->get_scope()) {
        if (ASR::is_a<ASR::Function_t>(*item.second)) {
            procedures.insert(ASR::down_cast<ASR::Function_t>(item.second));
        }
    }
    if (procedures.empty()) return;

    // What each procedure names of the kernel, and which procedures it
    // calls.
    std::map<ASR::Function_t*, std::set<ASR::symbol_t*>> needs;
    std::map<ASR::Function_t*, std::set<ASR::Function_t*>> callees;
    for (ASR::Function_t *fn : procedures) {
        KernelScopeReferences refs(scope, procedures);
        refs.visit_Function(*fn);
        needs[fn] = refs.variables;
        callees[fn] = refs.callees;
    }
    // A procedure is also handed what the procedures it calls are handed,
    // to pass it on, and the variables the shape of a dummy names.
    for (bool changed = true; changed;) {
        changed = false;
        for (ASR::Function_t *fn : procedures) {
            std::set<ASR::symbol_t*> &n = needs[fn];
            size_t before = n.size();
            for (ASR::Function_t *callee : callees[fn]) {
                for (ASR::symbol_t *v : needs[callee]) {
                    if (!is_parameter(v)) n.insert(v);
                }
            }
            std::set<ASR::Function_t*> none;
            KernelScopeReferences type_refs(scope, none);
            for (ASR::symbol_t *v : n) {
                type_refs.visit_ttype(
                    *ASR::down_cast<ASR::Variable_t>(v)->m_type);
            }
            n.insert(type_refs.variables.begin(), type_refs.variables.end());
            changed |= n.size() != before;
        }
    }

    std::map<ASR::Function_t*, LiftedProcedure> lifted;
    for (ASR::Function_t *fn : procedures) {
        if (needs[fn].empty()) continue;
        std::vector<ASR::symbol_t*> ordered(needs[fn].begin(),
            needs[fn].end());
        std::sort(ordered.begin(), ordered.end(),
            [](ASR::symbol_t *a, ASR::symbol_t *b) {
                return std::string(ASRUtils::symbol_name(a))
                    < std::string(ASRUtils::symbol_name(b));
            });
        LiftedProcedure &l = lifted[fn];
        for (ASR::symbol_t *v : ordered) {
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(v);
            std::string name = var->m_name;
            if (fn->m_symtab->get_symbol(name) != nullptr) {
                name = fn->m_symtab->get_unique_name(name);
            }
            ASR::ttype_t *type = ASRUtils::duplicate_type(al, var->m_type);
            if (is_parameter(v)) {
                // A named constant is the procedure's own.
                ASRUtils::ExprStmtDuplicator dup(al);
                ASR::symbol_t *own = ASR::down_cast<ASR::symbol_t>(
                    ASRUtils::make_Variable_t_util(al, v->base.loc,
                        fn->m_symtab, s2c(al, name), nullptr, 0,
                        ASR::intentType::Local,
                        var->m_symbolic_value
                            ? dup.duplicate_expr(var->m_symbolic_value)
                            : nullptr,
                        var->m_value ? dup.duplicate_expr(var->m_value)
                            : nullptr,
                        ASR::storage_typeType::Parameter, type,
                        var->m_type_declaration, var->m_abi, var->m_access,
                        ASR::presenceType::Required, false));
                fn->m_symtab->add_symbol(name, own);
                l.own[v] = own;
            } else {
                // A dummy of the kernel's host, such as an `intent(in)` one
                // of a procedure the kernel calls, keeps its intent.
                ASR::intentType intent =
                    var->m_intent == ASR::intentType::In
                        ? ASR::intentType::In : ASR::intentType::InOut;
                ASR::symbol_t *own = gpu_new_variable(al, v->base.loc,
                    fn->m_symtab, name, type, intent,
                    var->m_type_declaration);
                append_dummy(al, *fn, own, var->m_type);
                l.passed.push_back(v);
                l.own[v] = own;
            }
        }
    }
    if (lifted.empty()) return;

    for (auto &item : lifted) {
        HostNameRetargeter retarget(item.second.own);
        retarget.visit_Function(*item.first);
    }
    LiftedCallExtender extender(al, lifted);
    extender.visit_Function(kernel);
}

// The routine a call names, past external symbols and type-bound
// declarations.
ASR::Function_t* called_routine(ASR::symbol_t *name) {
    ASR::symbol_t *s = ASRUtils::symbol_get_past_external(name);
    if (s != nullptr && ASR::is_a<ASR::StructMethodDeclaration_t>(*s)) {
        s = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::StructMethodDeclaration_t>(s)->m_proc);
    }
    return s != nullptr && ASR::is_a<ASR::Function_t>(*s)
        ? ASR::down_cast<ASR::Function_t>(s) : nullptr;
}

// Whether the `i`th dummy of `fn` is a dummy procedure, declared by an
// interface block or with `procedure(iface)`. A procedure pointer is not:
// what it points to can change.
bool is_dummy_procedure_arg(const ASR::Function_t &fn, size_t i) {
    if (!ASR::is_a<ASR::Var_t>(*fn.m_args[i])) return false;
    ASR::symbol_t *dummy = ASR::down_cast<ASR::Var_t>(fn.m_args[i])->m_v;
    return ASR::is_a<ASR::Function_t>(*dummy) ||
        (ASR::is_a<ASR::Variable_t>(*dummy) && ASR::is_a<ASR::FunctionType_t>(
            *ASR::down_cast<ASR::Variable_t>(dummy)->m_type));
}

// Whether `fn` has a dummy procedure.
bool has_dummy_procedure(const ASR::Function_t &fn) {
    for (size_t i = 0; i < fn.n_args; i++) {
        if (is_dummy_procedure_arg(fn, i)) return true;
    }
    return false;
}

// Whether `scope` is `ancestor` or is nested in it.
bool is_within(SymbolTable *scope, SymbolTable *ancestor) {
    for (; scope != nullptr; scope = scope->parent) {
        if (scope == ancestor) return true;
    }
    return false;
}

// Every call of a statement list, so that it can be retargeted.
class RoutineCalls : public ASRUtils::BlockBodyWalkVisitor<RoutineCalls> {
public:
    std::vector<ASR::FunctionCall_t*> functions;
    std::vector<ASR::SubroutineCall_t*> subroutines;

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        functions.push_back(const_cast<ASR::FunctionCall_t*>(&x));
        ASR::BaseWalkVisitor<RoutineCalls>::visit_FunctionCall(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        subroutines.push_back(const_cast<ASR::SubroutineCall_t*>(&x));
        ASR::BaseWalkVisitor<RoutineCalls>::visit_SubroutineCall(x);
    }
};

// Names the routines a clone is specialized for wherever its dummy
// procedures were named: as the routine a call calls, and as an actual
// argument handed on.
class DummyProcedureRetargeter :
        public ASR::BaseWalkVisitor<DummyProcedureRetargeter> {
public:
    const std::map<ASR::symbol_t*, ASR::symbol_t*> &routine;

    explicit DummyProcedureRetargeter(
            const std::map<ASR::symbol_t*, ASR::symbol_t*> &routine_)
        : routine(routine_) {}

    template <typename T>
    void retarget(T &x) {
        auto it = routine.find(x.m_name);
        if (it == routine.end()) return;
        x.m_name = it->second;
        x.m_original_name = nullptr;
    }

    void visit_Var(const ASR::Var_t &x) {
        auto it = routine.find(x.m_v);
        if (it != routine.end()) const_cast<ASR::Var_t&>(x).m_v = it->second;
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        retarget(const_cast<ASR::FunctionCall_t&>(x));
        ASR::BaseWalkVisitor<DummyProcedureRetargeter>::visit_FunctionCall(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        retarget(const_cast<ASR::SubroutineCall_t&>(x));
        ASR::BaseWalkVisitor<DummyProcedureRetargeter>
            ::visit_SubroutineCall(x);
    }
};

// `present(g)` of a dummy procedure `g` a clone is specialized for, which
// is handed a routine and so is present.
class DummyProcedurePresent :
        public ASR::BaseExprReplacer<DummyProcedurePresent> {
public:
    Allocator &al;
    const std::map<ASR::symbol_t*, ASR::symbol_t*> &routine;

    DummyProcedurePresent(Allocator &al_,
            const std::map<ASR::symbol_t*, ASR::symbol_t*> &routine_)
        : al(al_), routine(routine_) {}

    void replace_IntrinsicElementalFunction(
            ASR::IntrinsicElementalFunction_t *x) {
        if (x->m_intrinsic_id == static_cast<int64_t>(
                    ASRUtils::IntrinsicElementalFunctions::Present)
                && x->n_args == 1 && ASR::is_a<ASR::Var_t>(*x->m_args[0])
                && routine.count(
                    ASR::down_cast<ASR::Var_t>(x->m_args[0])->m_v) > 0) {
            *current_expr = ASRUtils::EXPR(ASR::make_LogicalConstant_t(al,
                x->base.base.loc, true, x->m_type));
            return;
        }
        ASR::BaseExprReplacer<DummyProcedurePresent>::
            replace_IntrinsicElementalFunction(x);
    }
};

class DummyProcedurePresentVisitor : public ASR::CallReplacerOnExpressionsVisitor<
        DummyProcedurePresentVisitor> {
public:
    DummyProcedurePresent replacer;

    DummyProcedurePresentVisitor(Allocator &al,
            const std::map<ASR::symbol_t*, ASR::symbol_t*> &routine)
        : replacer(al, routine) {}

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }
};

// Device code calls no routine through a dummy procedure: a call that
// hands a routine to one calls a clone of the procedure written for that
// routine, which calls it directly. So the device needs no procedure
// pointers, every routine device code reaches is reached by a call the
// device passes see, and a clone takes the hidden arguments the routine is
// given -- the variables of its host, say -- as any other caller does.
class DummyProcedureSpecializer {
public:
    explicit DummyProcedureSpecializer(Allocator &al_) : al(al_) {}

    // The dummy procedure of the first call that could not be
    // specialized, or the empty string.
    std::string unresolved;

    void run(ASR::Function_t &kernel) {
        kernel_scope = kernel.m_symtab;
        std::vector<ASR::Function_t*> work;
        collect(kernel.m_symtab, work);
        work.push_back(&kernel);
        std::set<ASR::Function_t*> seen(work.begin(), work.end());
        while (!work.empty()) {
            ASR::Function_t *fn = work.back();
            work.pop_back();
            RoutineCalls calls;
            for (size_t i = 0; i < fn->n_body; i++) {
                calls.visit_stmt(*fn->m_body[i]);
            }
            std::vector<ASR::Function_t*> clones;
            for (ASR::FunctionCall_t *c : calls.functions) {
                specialize(*c, clones);
            }
            for (ASR::SubroutineCall_t *c : calls.subroutines) {
                specialize(*c, clones);
            }
            for (ASR::Function_t *callee : clones) {
                if (!seen.insert(callee).second) continue;
                work.push_back(callee);
                // What a clone contains is a copy of what the original
                // does, which nothing walked yet.
                std::vector<ASR::Function_t*> contained;
                collect(callee->m_symtab, contained);
                for (ASR::Function_t *c : contained) {
                    if (seen.insert(c).second) work.push_back(c);
                }
            }
        }
        // The clones are what every call reaches now, and what a procedure
        // with a dummy procedure was written for is in them.
        if (!unresolved.empty()) return;
        for (ASR::Function_t *fn : generic) {
            fn->m_symtab->parent->erase_symbol(fn->m_name);
        }
    }

private:
    Allocator &al;
    SymbolTable *kernel_scope = nullptr;
    // A procedure and the routines handed to its dummy procedures, to the
    // clone written for them.
    std::map<std::pair<ASR::Function_t*, std::vector<ASR::symbol_t*>>,
        ASR::Function_t*> clones;
    // The procedures with a dummy procedure.
    std::vector<ASR::Function_t*> generic;

    // The routines of `scope` and the scopes they hold, except those with
    // a dummy procedure, which go to `generic`: a call of one of those is
    // specialized, the body written for a dummy is not walked.
    void collect(SymbolTable *scope, std::vector<ASR::Function_t*> &out) {
        for (auto &item : scope->get_scope()) {
            if (!ASR::is_a<ASR::Function_t>(*item.second)) continue;
            ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(
                item.second);
            if (ASRUtils::is_dummy_procedure(*fn)) continue;
            if (has_dummy_procedure(*fn)) {
                generic.push_back(fn);
                continue;
            }
            out.push_back(fn);
            collect(fn->m_symtab, out);
        }
    }

    template <typename T>
    void specialize(T &call, std::vector<ASR::Function_t*> &reached) {
        ASR::Function_t *callee = called_routine(call.m_name);
        if (callee == nullptr || ASRUtils::is_dummy_procedure(*callee)) {
            return;
        }
        if (!has_dummy_procedure(*callee)) {
            reached.push_back(callee);
            return;
        }
        ASR::Function_t *clone = clone_for(call, *callee);
        if (clone == nullptr) {
            if (unresolved.empty()) {
                for (size_t i = 0; i < callee->n_args; i++) {
                    if (!is_dummy_procedure_arg(*callee, i)) continue;
                    unresolved = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(callee->m_args[i])->m_v);
                    break;
                }
            }
            return;
        }
        Vec<ASR::call_arg_t> args;
        args.reserve(al, call.n_args);
        for (size_t i = 0; i < call.n_args; i++) {
            if (i < callee->n_args && is_dummy_procedure_arg(*callee, i)) {
                continue;
            }
            args.push_back(al, call.m_args[i]);
        }
        call.m_args = args.p;
        call.n_args = args.n;
        call.m_name = &clone->base;
        call.m_original_name = nullptr;
        call.m_dt = nullptr;
        reached.push_back(clone);
    }

    // The clone of `callee` written for the routines `call` hands its
    // dummy procedures, or nullptr when the call does not name a routine
    // for each of them that the clone can name too.
    template <typename T>
    ASR::Function_t* clone_for(T &call, ASR::Function_t &callee) {
        // The clone goes where it can name both what the procedure names
        // and the routines: the caller names them all, so they are in
        // scopes the caller is in, and the innermost of those has them.
        SymbolTable *destination = callee.m_symtab->parent;
        if (ASR::is_a<ASR::ExternalSymbol_t>(*call.m_name) ||
                call.m_dt != nullptr || call.n_args != callee.n_args ||
                !is_within(destination, kernel_scope)) {
            return nullptr;
        }
        std::vector<ASR::symbol_t*> routines(callee.n_args, nullptr);
        for (size_t i = 0; i < callee.n_args; i++) {
            if (!is_dummy_procedure_arg(callee, i)) continue;
            ASR::expr_t *actual = call.m_args[i].m_value;
            if (actual == nullptr || !ASR::is_a<ASR::Var_t>(*actual)) {
                return nullptr;
            }
            ASR::symbol_t *routine = ASR::down_cast<ASR::Var_t>(actual)->m_v;
            if (!ASR::is_a<ASR::Function_t>(*routine) ||
                    ASRUtils::is_dummy_procedure(
                        *ASR::down_cast<ASR::Function_t>(routine))) {
                return nullptr;
            }
            SymbolTable *scope = ASRUtils::symbol_parent_symtab(routine);
            if (is_within(scope, destination)) {
                destination = scope;
            } else if (!is_within(destination, scope)) {
                return nullptr;
            }
            routines[i] = routine;
        }
        auto key = std::make_pair(&callee, routines);
        auto found = clones.find(key);
        if (found != clones.end()) return found->second;

        std::string name = callee.m_name;
        for (ASR::symbol_t *r : routines) {
            if (r != nullptr) name += "_" + std::string(ASRUtils::symbol_name(r));
        }
        name = destination->get_unique_name(name, false);
        ASRUtils::SymbolDuplicator duplicator(al);
        ASR::symbol_t *clone_sym = duplicator.duplicate_Function(&callee,
            destination);
        if (clone_sym == nullptr) return nullptr;
        ASR::Function_t *clone = ASR::down_cast<ASR::Function_t>(clone_sym);
        clone->m_name = s2c(al, name);
        destination->add_symbol(name, clone_sym);
        // The procedures the clone contains name the clone's entities, its
        // dummy procedures among them, rather than the original's.
        gpu_rebind_host_association(&callee, clone);

        std::map<ASR::symbol_t*, ASR::symbol_t*> routine;
        Vec<ASR::expr_t*> args;
        args.reserve(al, clone->n_args);
        for (size_t i = 0; i < clone->n_args; i++) {
            if (routines[i] == nullptr) {
                args.push_back(al, clone->m_args[i]);
                continue;
            }
            ASR::symbol_t *dummy =
                ASR::down_cast<ASR::Var_t>(clone->m_args[i])->m_v;
            routine[dummy] = routines[i];
        }
        DummyProcedurePresentVisitor present(al, routine);
        present.visit_Function(*clone);
        DummyProcedureRetargeter retarget(routine);
        retarget.visit_Function(*clone);
        for (auto &item : routine) {
            clone->m_symtab->erase_symbol(ASRUtils::symbol_name(item.first));
        }
        clone->m_args = args.p;
        clone->n_args = args.n;
        clone->m_function_signature = ASRUtils::TYPE(
            ASRUtils::make_FunctionType_t_util(al, clone->base.base.loc,
                clone->m_args, clone->n_args, clone->m_return_var,
                ASRUtils::get_FunctionType(*clone), clone->m_symtab));
        clones[key] = clone;
        return clone;
    }
};

} // namespace

std::string gpu_specialize_dummy_procedures(Allocator &al,
        ASR::Function_t &kernel) {
    DummyProcedureSpecializer specializer(al);
    specializer.run(kernel);
    return specializer.unresolved;
}

void gpu_lift_host_association(Allocator &al, ASR::Function_t &kernel) {
    lift_into_contained(al, kernel);
    // A procedure the kernel calls is a copy of the original, made along
    // with the procedures it contains, which name its variables in turn.
    for (auto &item : kernel.m_symtab->get_scope()) {
        if (ASR::is_a<ASR::Function_t>(*item.second)) {
            gpu_lift_host_association(al,
                *ASR::down_cast<ASR::Function_t>(item.second));
        }
    }
}

} // namespace LCompilers
