#include <algorithm>
#include <map>
#include <set>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/gpu_offload_host_association.h>
#include <libasr/pass/gpu_offload_rewrite.h>

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

// The type of a dummy in a procedure signature, which must not name the
// symbols of a scope.
ASR::ttype_t* signature_type(Allocator &al, ASR::ttype_t *type) {
    if (!ASR::is_a<ASR::Array_t>(*type)) return type;
    ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
    ASR::dimension_t *dims = al.allocate<ASR::dimension_t>(arr->n_dims);
    for (size_t d = 0; d < arr->n_dims; d++) {
        dims[d].loc = arr->m_dims[d].loc;
        dims[d].m_start = nullptr;
        dims[d].m_length = nullptr;
    }
    return ASRUtils::TYPE(ASR::make_Array_t(al, arr->base.base.loc,
        arr->m_type, dims, arr->n_dims, arr->m_physical_type,
        arr->m_memory_space));
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
    types.push_back(al, signature_type(al, type));
    fn.m_function_signature = ASRUtils::TYPE(ASR::make_FunctionType_t(al,
        t->base.base.loc, types.p, types.n, t->m_return_var_type, t->m_abi,
        t->m_deftype, t->m_bindc_name, t->m_elemental, t->m_pure,
        t->m_module, t->m_inline, t->m_static, t->m_restrictions,
        t->n_restrictions, t->m_is_restriction, t->m_exec_space));
}

// The type of a variable of a common block, with its constant extents
// written as the constants they are, so that it names nothing of the scope
// it was declared in.
ASR::ttype_t* common_variable_type(Allocator &al, ASR::ttype_t *type) {
    type = ASRUtils::duplicate_type(al, type);
    if (!ASR::is_a<ASR::Array_t>(*type)) return type;
    ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
    for (size_t d = 0; d < arr->n_dims; d++) {
        ASR::dimension_t &dim = arr->m_dims[d];
        if (dim.m_start && ASRUtils::expr_value(dim.m_start)) {
            dim.m_start = ASRUtils::expr_value(dim.m_start);
        }
        if (dim.m_length && ASRUtils::expr_value(dim.m_length)) {
            dim.m_length = ASRUtils::expr_value(dim.m_length);
        }
    }
    return type;
}

// Whether every variable of a common block view can be given to a kernel
// as a variable of its own: one of an intrinsic numeric or logical type,
// placed by the view as one of its members.
bool common_block_view_is_passable(ASR::CommonBlock_t *view) {
    if (view->n_associations > 0) return false;
    for (size_t i = 0; i < view->n_members; i++) {
        ASR::ttype_t *type = ASRUtils::symbol_type(view->m_members[i]);
        if (ASRUtils::is_pointer(type)) return false;
        ASR::ttype_t *element = ASRUtils::type_get_past_array(type);
        if (!ASR::is_a<ASR::Integer_t>(*element)
                && !ASR::is_a<ASR::Real_t>(*element)
                && !ASR::is_a<ASR::Complex_t>(*element)
                && !ASR::is_a<ASR::Logical_t>(*element)) {
            return false;
        }
        if (!ASR::is_a<ASR::Array_t>(*type)) continue;
        ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
        for (size_t d = 0; d < arr->n_dims; d++) {
            const ASR::dimension_t &dim = arr->m_dims[d];
            if ((dim.m_start && !ASRUtils::expr_value(dim.m_start))
                    || (dim.m_length
                        && !ASRUtils::expr_value(dim.m_length))) {
                return false;
            }
        }
    }
    return true;
}

// The scope of the program or procedure that `scope` is part of, which is
// where a view of a common block can be.
SymbolTable* common_block_scope(SymbolTable *scope) {
    for (; scope != nullptr; scope = scope->parent) {
        if (scope->asr_owner == nullptr
                || scope->asr_owner->type != ASR::asrType::symbol) {
            return nullptr;
        }
        ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(
            scope->asr_owner);
        if (ASR::is_a<ASR::Program_t>(*owner)
                || ASR::is_a<ASR::Function_t>(*owner)) {
            return scope;
        }
    }
    return nullptr;
}

} // namespace

void gpu_pass_common_blocks(Allocator &al, const Location &loc,
        const std::vector<ASR::Function_t*> &procedures,
        SymbolTable *kernel_scope, SymbolTable *host_scope,
        Vec<ASR::expr_t*> &kernel_args, Vec<ASR::call_arg_t> &call_args) {
    SymbolTable *host = common_block_scope(host_scope);
    if (host == nullptr) return;
    for (ASR::Function_t *fn : procedures) {
        std::vector<ASR::CommonBlock_t*> views;
        for (auto &item : fn->m_symtab->get_scope()) {
            if (ASR::is_a<ASR::CommonBlock_t>(*item.second)) {
                ASR::CommonBlock_t *view =
                    ASR::down_cast<ASR::CommonBlock_t>(item.second);
                if (common_block_view_is_passable(view)) {
                    views.push_back(view);
                }
            }
        }
        if (views.empty()) continue;
        // Only the variables the procedure names are handed to it.
        std::set<ASR::Function_t*> none;
        KernelScopeReferences refs(fn->m_symtab, none);
        refs.visit_Function(*fn);
        std::map<ASR::symbol_t*, ASR::symbol_t*> own;
        for (ASR::CommonBlock_t *view : views) {
            bool named = false;
            for (size_t i = 0; i < view->n_members && !named; i++) {
                named = refs.variables.count(view->m_members[i]) > 0;
            }
            if (!named) continue;
            std::string prefix = "__common_"
                + std::string(view->m_block_name ? view->m_block_name : "")
                + "_";
            // The host's view of the block, which places the variables
            // where the procedure's view does.
            Vec<ASR::symbol_t*> members;
            members.reserve(al, view->n_members);
            std::vector<std::pair<ASR::symbol_t*, ASR::symbol_t*>> passed;
            for (size_t i = 0; i < view->n_members; i++) {
                ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
                    view->m_members[i]);
                // The kernel's variable has the name of the host's, which
                // nothing of the host, the kernel or the procedure has.
                std::string name = prefix + var->m_name;
                for (int n = 1; host->resolve_symbol(name) != nullptr
                        || kernel_scope->resolve_symbol(name) != nullptr
                        || fn->m_symtab->resolve_symbol(name) != nullptr;
                        n++) {
                    name = prefix + var->m_name + "_" + std::to_string(n);
                }
                ASR::symbol_t *member = gpu_new_variable(al,
                    view->m_members[i]->base.loc, host, name,
                    common_variable_type(al, var->m_type));
                members.push_back(al, member);
                if (refs.variables.count(view->m_members[i]) > 0) {
                    passed.push_back({view->m_members[i], member});
                }
            }
            std::string view_name = host->get_unique_name(
                std::string(view->m_name) + "~gpu", false);
            ASR::symbol_t *host_view = ASR::down_cast<ASR::symbol_t>(
                ASR::make_CommonBlock_t(al, view->base.base.loc, host,
                    s2c(al, view_name), view->m_block_name,
                    view->m_link_name, members.p, members.n, nullptr, 0,
                    view->m_abi, view->m_storage,
                    ASR::common_block_roleType::Tentative));
            host->add_symbol(view_name, host_view);
            for (size_t i = 0; i < members.n; i++) {
                ASR::down_cast<ASR::Variable_t>(members[i])->m_common_block =
                    host_view;
            }
            // Each variable the procedure names becomes a variable of the
            // kernel, which the launch hands the host's.
            for (auto &[variable, member] : passed) {
                ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
                    variable);
                ASR::symbol_t *param = gpu_new_variable(al,
                    variable->base.loc, kernel_scope,
                    ASRUtils::symbol_name(member),
                    common_variable_type(al, var->m_type),
                    ASR::intentType::InOut);
                kernel_args.push_back(al, ASRUtils::EXPR(
                    ASR::make_Var_t(al, loc, param)));
                ASR::call_arg_t arg;
                arg.loc = loc;
                arg.m_value = ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                    member));
                call_args.push_back(al, arg);
                own[variable] = param;
            }
            for (size_t i = 0; i < view->n_members; i++) {
                fn->m_symtab->erase_symbol(
                    ASRUtils::symbol_name(view->m_members[i]));
            }
            fn->m_symtab->erase_symbol(view->m_name);
        }
        HostNameRetargeter retarget(own);
        retarget.visit_Function(*fn);
    }
}

void gpu_lift_host_association(Allocator &al, ASR::Function_t &kernel) {
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
                ASR::symbol_t *own = gpu_new_variable(al, v->base.loc,
                    fn->m_symtab, name, type, ASR::intentType::InOut,
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

} // namespace LCompilers
