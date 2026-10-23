#include <algorithm>
#include <map>
#include <set>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/gpu_kernel_abi.h>
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

    // The designators by which a common block view places a variable
    // EQUIVALENCE associates with it name no variable the code uses.
    void visit_CommonBlock(const ASR::CommonBlock_t &/*x*/) {}

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

// The type of a variable of a common block, with its constant extents and
// length written as the constants they are, so that it names nothing of
// the scope it was declared in.
ASR::ttype_t* common_variable_type(Allocator &al, ASR::ttype_t *type) {
    type = ASRUtils::duplicate_type(al, type);
    ASR::ttype_t *t = ASRUtils::type_get_past_pointer(type);
    if (ASR::is_a<ASR::Array_t>(*t)) {
        ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(t);
        for (size_t d = 0; d < arr->n_dims; d++) {
            ASR::dimension_t &dim = arr->m_dims[d];
            if (dim.m_start && ASRUtils::expr_value(dim.m_start)) {
                dim.m_start = ASRUtils::expr_value(dim.m_start);
            }
            if (dim.m_length && ASRUtils::expr_value(dim.m_length)) {
                dim.m_length = ASRUtils::expr_value(dim.m_length);
            }
        }
        t = arr->m_type;
    }
    if (ASR::is_a<ASR::String_t>(*t)) {
        ASR::String_t *str = ASR::down_cast<ASR::String_t>(t);
        if (str->m_len && ASRUtils::expr_value(str->m_len)) {
            str->m_len = ASRUtils::expr_value(str->m_len);
        }
    }
    return type;
}

// The variables of the procedure's own scope that it names.
std::set<ASR::symbol_t*> named_variables(ASR::Function_t &fn) {
    std::set<ASR::Function_t*> none;
    KernelScopeReferences refs(fn.m_symtab, none);
    refs.visit_Function(fn);
    return refs.variables;
}

// How many members of `view`, from its first, a view has to have to place
// every member in `named` where `view` does; zero when it names none.
size_t placed_members(ASR::CommonBlock_t *view,
        const std::set<ASR::symbol_t*> &named) {
    size_t n = 0;
    for (size_t i = 0; i < view->n_members; i++) {
        if (named.count(view->m_members[i]) > 0) n = i + 1;
    }
    return n;
}

// The common block views of a procedure, in the order of their names.
std::vector<ASR::CommonBlock_t*> common_block_views(ASR::Function_t &fn) {
    std::vector<ASR::CommonBlock_t*> views;
    for (auto &item : fn.m_symtab->get_scope()) {
        if (ASR::is_a<ASR::CommonBlock_t>(*item.second)) {
            views.push_back(ASR::down_cast<ASR::CommonBlock_t>(item.second));
        }
    }
    return views;
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

// The variables of common blocks an ASR subtree names, in the order it
// first names them, the bodies of the BLOCKs it runs included.
class CommonVariableNames : public ASR::BaseWalkVisitor<CommonVariableNames> {
public:
    std::vector<ASR::symbol_t*> variables;

    void visit_Var(const ASR::Var_t &x) {
        ASR::symbol_t *v = ASRUtils::symbol_get_past_external(x.m_v);
        if (ASR::is_a<ASR::Variable_t>(*v)
                && ASR::down_cast<ASR::Variable_t>(v)->m_common_block
                    != nullptr
                && seen.insert(v).second) {
            variables.push_back(v);
        }
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!blocks.insert(b).second) return;
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(b);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }

private:
    std::set<ASR::symbol_t*> seen;
    std::set<ASR::symbol_t*> blocks;
};

// Whether `fn` is an external procedure, or one an external procedure
// contains: the kernel calls such a procedure itself instead of a copy.
bool in_external_procedure(ASR::Function_t &fn) {
    SymbolTable *scope = fn.m_symtab->parent;
    while (scope != nullptr && scope->asr_owner != nullptr
            && scope->asr_owner->type == ASR::asrType::symbol
            && ASR::is_a<ASR::Function_t>(
                *ASR::down_cast<ASR::symbol_t>(scope->asr_owner))) {
        scope = scope->parent;
    }
    return scope != nullptr && (scope->asr_owner == nullptr
        || scope->asr_owner->type != ASR::asrType::symbol);
}

// The view of a common block a variable of it belongs to, and where.
struct CommonPlace {
    ASR::CommonBlock_t *view;
    // The member it is, or -1 for a variable EQUIVALENCE associates with
    // the block.
    int64_t member;
};

CommonPlace common_place(ASR::symbol_t *variable) {
    ASR::CommonBlock_t *view = ASR::down_cast<ASR::CommonBlock_t>(
        ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Variable_t>(variable)->m_common_block));
    for (size_t i = 0; i < view->n_members; i++) {
        if (view->m_members[i] == variable) {
            return {view, static_cast<int64_t>(i)};
        }
    }
    return {view, -1};
}

// Whether two variables of a common block are the same storage or none of
// it: their views place them as members, and agree on the types of the
// members up to the later of the two.
bool same_or_apart(const CommonPlace &a, const CommonPlace &b) {
    if (a.view == b.view) return true;
    if (a.member < 0 || b.member < 0) return false;
    size_t n = static_cast<size_t>(std::max(a.member, b.member)) + 1;
    if (n > a.view->n_members || n > b.view->n_members) return false;
    for (size_t i = 0; i < n; i++) {
        ASR::ttype_t *ta = ASRUtils::symbol_type(a.view->m_members[i]);
        ASR::ttype_t *tb = ASRUtils::symbol_type(b.view->m_members[i]);
        if (!ASRUtils::check_equal_type(ta, tb, nullptr, nullptr, true)) {
            return false;
        }
        std::pair<int64_t, int64_t> sa =
            ASRUtils::compute_struct_member_size_align(ta);
        if (sa.first < 0
                || sa != ASRUtils::compute_struct_member_size_align(tb)) {
            return false;
        }
    }
    return true;
}

std::string block_name(ASR::CommonBlock_t *view) {
    return "/" + std::string(view->m_block_name ? view->m_block_name : "")
        + "/";
}

// The name of the program or procedure a view of a common block is in.
std::string routine_name(ASR::CommonBlock_t *view) {
    SymbolTable *scope = common_block_scope(view->m_parent_symtab);
    if (scope == nullptr) return "";
    return ASRUtils::symbol_name(
        ASR::down_cast<ASR::symbol_t>(scope->asr_owner));
}

} // namespace

GpuDecline gpu_common_block_access_decline(ASR::stmt_t **body, size_t n_body,
        const std::vector<ASR::Function_t*> &routines) {
    struct Access {
        ASR::symbol_t *variable;
        CommonPlace place;
        bool defined;
    };
    std::vector<Access> accesses;
    auto add = [&](const CommonVariableNames &names,
            const std::set<ASR::symbol_t*> &defined) {
        for (ASR::symbol_t *v : names.variables) {
            accesses.push_back({v, common_place(v), defined.count(v) > 0});
        }
    };
    {
        CommonVariableNames names;
        for (size_t i = 0; i < n_body; i++) {
            names.visit_stmt(*body[i]);
        }
        add(names, gpu_written_variables(body, n_body));
    }
    for (ASR::Function_t *fn : routines) {
        CommonVariableNames names;
        names.visit_Function(*fn);
        if (names.variables.empty()) continue;
        if (in_external_procedure(*fn)) {
            ASR::symbol_t *v = names.variables[0];
            GpuDecline d(GpuDeclineReason::CommonVariableExternalProcedure,
                ASRUtils::symbol_name(v));
            d.common_block = block_name(common_place(v).view);
            d.routine = fn->m_name;
            return d;
        }
        std::set<ASR::symbol_t*> defined = gpu_written_variables(
            fn->m_body, fn->n_body);
        // The kernel is handed the value of a scalar, which a definition
        // on the device does not reach the host with; the loop's own
        // definitions of one do.
        for (ASR::symbol_t *v : names.variables) {
            if (defined.count(v) > 0
                    && !ASRUtils::is_array(ASRUtils::symbol_type(v))) {
                GpuDecline d(GpuDeclineReason::CommonVariableDefined,
                    ASRUtils::symbol_name(v));
                d.common_block = block_name(common_place(v).view);
                d.routine = fn->m_name;
                return d;
            }
        }
        add(names, defined);
    }
    // Each view of a block reaches the kernel as memory of its own, so a
    // definition through one view is seen through another only where the
    // two place the variable alike.
    for (const Access &d : accesses) {
        if (!d.defined) continue;
        for (const Access &o : accesses) {
            if (block_name(o.place.view) != block_name(d.place.view)
                    || same_or_apart(d.place, o.place)) {
                continue;
            }
            GpuDecline decline(GpuDeclineReason::CommonBlockLayouts,
                ASRUtils::symbol_name(d.variable));
            decline.common_block = block_name(d.place.view);
            decline.routine = routine_name(d.place.view);
            decline.other_variable = ASRUtils::symbol_name(o.variable);
            decline.other_routine = routine_name(o.place.view);
            return decline;
        }
    }
    return GpuDecline();
}

GpuDecline gpu_common_block_decline(ASR::Function_t &fn) {
    std::set<ASR::symbol_t*> named;
    bool looked = false;
    for (ASR::CommonBlock_t *view : common_block_views(fn)) {
        if (!looked) {
            named = named_variables(fn);
            looked = true;
        }
        auto decline = [&](GpuDeclineReason reason, ASR::symbol_t *v) {
            GpuDecline d(reason, ASRUtils::symbol_name(v));
            d.common_block = "/" + std::string(view->m_block_name
                ? view->m_block_name : "") + "/";
            d.routine = fn.m_name;
            return d;
        };
        for (size_t i = 0; i < view->n_associations; i++) {
            ASR::symbol_t *v = view->m_associations[i].m_variable;
            if (named.count(v) > 0) {
                return decline(GpuDeclineReason::CommonVariableEquivalence,
                    v);
            }
        }
        // The members up to the last one named place it, and so have to
        // be declared on the host; only the ones named reach the device.
        size_t n = placed_members(view, named);
        for (size_t i = 0; i < n; i++) {
            ASR::symbol_t *v = view->m_members[i];
            ASR::ttype_t *type = ASRUtils::symbol_type(v);
            if (ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(type))) {
                if (named.count(v) > 0) {
                    return decline(
                        GpuDeclineReason::CommonVariableDerivedType, v);
                }
                // The first variable named that it places.
                size_t j = i + 1;
                while (named.count(view->m_members[j]) == 0) j++;
                GpuDecline d = decline(
                    GpuDeclineReason::CommonVariableDerivedType,
                    view->m_members[j]);
                d.other_variable = ASRUtils::symbol_name(v);
                return d;
            }
            if (named.count(v) == 0) continue;
            if (ASRUtils::is_pointer(type)) {
                return decline(GpuDeclineReason::CommonVariablePointer, v);
            }
            if (ASRUtils::is_character(*type)) {
                return decline(GpuDeclineReason::CommonVariableCharacter, v);
            }
        }
    }
    return GpuDecline();
}

void gpu_pass_common_blocks(Allocator &al, const Location &loc,
        const std::vector<ASR::Function_t*> &procedures,
        SymbolTable *kernel_scope, SymbolTable *host_scope,
        Vec<ASR::expr_t*> &kernel_args, Vec<ASR::call_arg_t> &call_args) {
    SymbolTable *host = common_block_scope(host_scope);
    for (ASR::Function_t *fn : procedures) {
        std::vector<ASR::CommonBlock_t*> views = common_block_views(*fn);
        // The loop has been declined if a procedure it calls names a
        // variable of a common block that cannot be handed to the kernel
        // (see offloadable_before_rewrites). A copy of a procedure that
        // it does not call, but the kernel has, keeps its views.
        if (views.empty() || gpu_common_block_decline(*fn).declined()) {
            continue;
        }
        LCOMPILERS_ASSERT(host != nullptr);
        std::set<ASR::symbol_t*> named = named_variables(*fn);
        std::map<ASR::symbol_t*, ASR::symbol_t*> own;
        for (ASR::CommonBlock_t *view : views) {
            size_t n = placed_members(view, named);
            if (n == 0) continue;
            std::string prefix = "__common_"
                + std::string(view->m_block_name ? view->m_block_name : "")
                + "_";
            // The host's view of the block, which places the variables
            // the procedure names where the procedure's view does.
            Vec<ASR::symbol_t*> members;
            members.reserve(al, n);
            std::vector<std::pair<ASR::symbol_t*, ASR::symbol_t*>> passed;
            for (size_t i = 0; i < n; i++) {
                ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
                    view->m_members[i]);
                // The kernel's variable has the name of the host's, which
                // nothing of the host, the kernel or the procedure has.
                std::string name = prefix + var->m_name;
                for (int k = 1; host->resolve_symbol(name) != nullptr
                        || kernel_scope->resolve_symbol(name) != nullptr
                        || fn->m_symtab->resolve_symbol(name) != nullptr;
                        k++) {
                    name = prefix + var->m_name + "_" + std::to_string(k);
                }
                ASR::symbol_t *member = gpu_new_variable(al,
                    view->m_members[i]->base.loc, host, name,
                    common_variable_type(al, var->m_type));
                members.push_back(al, member);
                if (named.count(view->m_members[i]) > 0) {
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
            // Nothing names the other variables of the view, which goes
            // with them.
            for (size_t i = 0; i < view->n_members; i++) {
                fn->m_symtab->erase_symbol(
                    ASRUtils::symbol_name(view->m_members[i]));
            }
            for (size_t i = 0; i < view->n_associations; i++) {
                fn->m_symtab->erase_symbol(ASRUtils::symbol_name(
                    view->m_associations[i].m_variable));
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
