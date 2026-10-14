#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/pass/gpu_device_struct_storage.h>
#include <libasr/pass/gpu_device_workspace.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/symbol_expr_substitution.h>

#include <deque>
#include <map>
#include <set>
#include <string>
#include <vector>

namespace LCompilers {

/*
Give the run-time sized locals of a device routine to its callers.

A device that has no variable-length arrays and no heap (see
`device_function_runtime_sized_locals`) cannot declare a local array whose
extent is only known once the routine runs, such as an allocatable local the
`gpu_device_allocatable` pass gave the shape of its `allocate`, or a
temporary a shared pass introduced for the result of an expression. A kernel
can have one: the host allocates a buffer for it before the launch, sized by
the largest extent over the iterations, and every thread works in a slice of
it (see `collect_gpu_vla_workspaces`).

So every such local of a device routine becomes an explicit-shape dummy
argument of the routine, appended after its other arguments, and every call
passes a local of its own for it, declared in the scope of the call with the
extents of the dummy written over the actual arguments of that call. When the
caller is a kernel that local is one of the kernel's workspaces; when it is
another device routine it is a run-time sized local of that routine in turn,
and is given to its callers the same way. Every array therefore ends up in
the kernel that reaches it, however deep the calls that use it are, and a
routine stays one routine whatever position it is called from: an
expression, an `associate` selector, a condition, an elemental call over an
array.

This runs once shared lowering has created every temporary of device code,
and before the address spaces of device arrays are settled, so a local the
call site gives the routine is placed in the space it lives in like any
other.

A routine that reaches itself would need one local per level of the
recursion, which no buffer sized before the launch provides, and an extent
that reads anything other than the routine's dummy arguments and named
constants cannot be written at the call site. Both are reported as a loop
the pipeline cannot offload yet.
*/

namespace {

// Whether `var` is a local array the device cannot declare: an array of an
// explicit shape that is not a compile-time constant.
bool runtime_sized_local(const ASR::Variable_t &var) {
    if (var.m_intent != ASR::intentType::Local) return false;
    if (var.m_storage == ASR::storage_typeType::Parameter) return false;
    if (!ASR::is_a<ASR::Array_t>(*var.m_type)) return false;
    ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(var.m_type);
    for (size_t d = 0; d < arr->n_dims; d++) {
        if (arr->m_dims[d].m_length == nullptr) return false;
    }
    return !ASRUtils::is_fixed_size_array(arr->m_dims, arr->n_dims);
}

class VarReader : public ASR::BaseWalkVisitor<VarReader> {
public:
    std::set<ASR::symbol_t*> symbols;

    void visit_Var(const ASR::Var_t &x) { symbols.insert(x.m_v); }
};

std::set<ASR::symbol_t*> extent_reads(const ASR::Array_t &arr) {
    VarReader reader;
    for (size_t d = 0; d < arr.n_dims; d++) {
        if (arr.m_dims[d].m_start) reader.visit_expr(*arr.m_dims[d].m_start);
        if (arr.m_dims[d].m_length) {
            reader.visit_expr(*arr.m_dims[d].m_length);
        }
    }
    return reader.symbols;
}

ASR::symbol_t* call_target(ASR::symbol_t *name) {
    ASR::symbol_t *target = ASRUtils::symbol_get_past_external(name);
    if (target == nullptr) return nullptr;
    return ASRUtils::symbol_get_past_StructMethodDeclaration(target);
}

// Gives the calls of one routine the locals that routine no longer
// declares: each call passes a new local of the scope the call is made in,
// shaped by the extents of the new dummy over the call's actual arguments.
class CallSiteWorkspaces : public ASR::BaseWalkVisitor<CallSiteWorkspaces> {
public:
    CallSiteWorkspaces(Allocator &al_, ASR::Function_t &callee_,
            size_t n_source_args_, std::vector<ASR::Variable_t*> &dummies_,
            std::map<ASR::symbol_t*, ASR::expr_t*> &constants_,
            std::set<ASR::symbol_t*> &created_)
        : al(al_), callee(callee_), n_source_args(n_source_args_),
          dummies(dummies_), constants(constants_), created(created_) {}

    void visit_Program(const ASR::Program_t &x) {
        for (auto &a : x.m_symtab->get_scope()) visit_symbol(*a.second);
        visit_body(x.m_symtab, x.m_body, x.n_body);
    }

    void visit_Function(const ASR::Function_t &x) {
        for (auto &a : x.m_symtab->get_scope()) visit_symbol(*a.second);
        visit_body(x.m_symtab, x.m_body, x.n_body);
    }

    void visit_Block(const ASR::Block_t &x) {
        for (auto &a : x.m_symtab->get_scope()) visit_symbol(*a.second);
        visit_body(x.m_symtab, x.m_body, x.n_body);
    }

    void visit_AssociateBlock(const ASR::AssociateBlock_t &x) {
        for (auto &a : x.m_symtab->get_scope()) visit_symbol(*a.second);
        visit_body(x.m_symtab, x.m_body, x.n_body);
    }

    // Only a call in a statement is given its arrays: one in a declaration
    // is evaluated when the scope is entered, before any local exists.
    void visit_Variable(const ASR::Variable_t &) {}
    void visit_Struct(const ASR::Struct_t &) {}

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        ASR::BaseWalkVisitor<CallSiteWorkspaces>::visit_FunctionCall(x);
        if (call_target(x.m_name) != &callee.base) return;
        auto &call = const_cast<ASR::FunctionCall_t&>(x);
        give_workspaces(x.base.base.loc, call.m_args, call.n_args);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        ASR::BaseWalkVisitor<CallSiteWorkspaces>::visit_SubroutineCall(x);
        if (call_target(x.m_name) != &callee.base) return;
        auto &call = const_cast<ASR::SubroutineCall_t&>(x);
        give_workspaces(x.base.base.loc, call.m_args, call.n_args);
    }

private:
    Allocator &al;
    ASR::Function_t &callee;
    size_t n_source_args;
    std::vector<ASR::Variable_t*> &dummies;
    std::map<ASR::symbol_t*, ASR::expr_t*> &constants;
    std::set<ASR::symbol_t*> &created;
    SymbolTable *current_scope = nullptr;

    void visit_body(SymbolTable *scope, ASR::stmt_t **body, size_t n_body) {
        SymbolTable *outer = current_scope;
        current_scope = scope;
        for (size_t i = 0; i < n_body; i++) visit_stmt(*body[i]);
        current_scope = outer;
    }

    ASR::expr_t* substitute(ASR::expr_t *e,
            std::map<ASR::symbol_t*, ASR::expr_t*> &values) {
        if (e == nullptr) return nullptr;
        ASRUtils::ExprStmtDuplicator duplicator(al);
        duplicator.allow_procedure_calls = true;
        duplicator.allow_reshape = true;
        duplicator.success = true;
        ASR::expr_t *copy = duplicator.duplicate_expr(e);
        LCOMPILERS_ASSERT(duplicator.success && copy);
        AssociateVarResolver resolver(al, values);
        resolver.current_expr = &copy;
        resolver.replace_expr(copy);
        return copy;
    }

    void give_workspaces(const Location &loc, ASR::call_arg_t *&args,
            size_t &n_args) {
        LCOMPILERS_ASSERT(current_scope != nullptr);
        LCOMPILERS_ASSERT(n_args == n_source_args);
        std::map<ASR::symbol_t*, ASR::expr_t*> values = constants;
        for (size_t i = 0; i < n_source_args; i++) {
            if (!ASR::is_a<ASR::Var_t>(*callee.m_args[i])) continue;
            ASR::symbol_t *dummy =
                ASR::down_cast<ASR::Var_t>(callee.m_args[i])->m_v;
            if (args[i].m_value) values[dummy] = args[i].m_value;
        }
        Vec<ASR::call_arg_t> new_args;
        new_args.reserve(al, n_args + dummies.size());
        for (size_t i = 0; i < n_args; i++) new_args.push_back(al, args[i]);
        ASRUtils::ASRBuilder b(al, loc);
        for (ASR::Variable_t *dummy : dummies) {
            ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(dummy->m_type);
            Vec<ASR::dimension_t> dims;
            dims.reserve(al, arr->n_dims);
            for (size_t d = 0; d < arr->n_dims; d++) {
                ASR::dimension_t dim;
                dim.loc = arr->m_dims[d].loc;
                dim.m_start = substitute(arr->m_dims[d].m_start, values);
                dim.m_length = substitute(arr->m_dims[d].m_length, values);
                dims.push_back(al, dim);
            }
            ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Array_t(al, loc,
                ASRUtils::duplicate_type(al, arr->m_type), dims.p, dims.n,
                arr->m_physical_type, ASR::memory_spaceType::Global));
            ASR::symbol_t *type_declaration = nullptr;
            if (dummy->m_type_declaration) {
                type_declaration = current_scope->resolve_symbol(
                    ASRUtils::symbol_name(dummy->m_type_declaration));
            }
            // The local stands for the callee's, and has its name, which a
            // diagnostic about the local then shows, unless the name is
            // one the scope of the call can already see.
            std::string name = dummy->m_name;
            if (current_scope->resolve_symbol(name) != nullptr) {
                name = current_scope->get_unique_name("__gpu_ws_" + name,
                    false);
            }
            ASR::expr_t *local = b.Variable(current_scope, name, type,
                ASR::intentType::Local, type_declaration);
            created.insert(ASR::down_cast<ASR::Var_t>(local)->m_v);
            ASR::call_arg_t arg;
            arg.loc = loc;
            arg.m_value = local;
            new_args.push_back(al, arg);
        }
        args = new_args.p;
        n_args = new_args.n;
    }
};

// Where each kernel is launched, so that a loop the pipeline cannot
// offload is reported at the loop rather than inside a routine it calls.
class GpuLaunchLocations : public ASR::BaseWalkVisitor<GpuLaunchLocations> {
public:
    std::map<ASR::symbol_t*, Location> launches;

    void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &x) {
        launches.emplace(x.m_kernel, x.base.base.loc);
    }
};

class DeviceWorkspaceLifter {
public:
    DeviceWorkspaceLifter(Allocator &al_, ASR::TranslationUnit_t &unit_,
            const PassOptions &options_)
        : al(al_), unit(unit_), options(options_) {}

    void run() {
        GpuLaunchLocations locations;
        locations.visit_TranslationUnit(unit);
        launches = locations.launches;
        device_routines();
        std::vector<ASR::Function_t*> reached;
        for (auto &item : kernel_of) reached.push_back(item.first);
        for (ASR::Function_t *fn : reached) {
            std::string component;
            ASR::Variable_t *var = gpu_struct_local_without_storage(*fn,
                component);
            if (var == nullptr) continue;
            decline(*fn, var->base.base.loc, GpuDecline(
                GpuDeclineReason::StructComponentNotSizeable,
                std::string(var->m_name) + "%" + component));
        }
        // Every round moves the locals of the routines it lifts one level
        // up the call graph, which has no cycle through a routine that is
        // lifted, so this ends.
        while (true) {
            bool changed = false;
            for (ASR::Function_t *fn : device_routines()) {
                if (declined.count(fn)) continue;
                std::vector<ASR::Variable_t*> locals =
                    runtime_sized_locals(*fn);
                if (locals.empty()) continue;
                if (lift(*fn, locals)) changed = true;
            }
            if (!changed) break;
        }
    }

private:
    Allocator &al;
    ASR::TranslationUnit_t &unit;
    const PassOptions &options;
    std::set<ASR::Function_t*> declined;
    std::map<ASR::symbol_t*, Location> launches;
    // A kernel that reaches each routine.
    std::map<ASR::Function_t*, ASR::Function_t*> kernel_of;
    // The locals this pass declares for the calls of a routine: each has
    // the extents its callee's local has on entry to the callee, the
    // values they have at the call, not where the scope of the local
    // starts.
    std::set<ASR::symbol_t*> call_site_locals;

    // Whether the extents of `local` are evaluated on entry to `fn`, as
    // those of an array the routine declares itself are, and not where a
    // BLOCK starts or a call is made.
    bool sized_on_entry(ASR::Function_t &fn, ASR::Variable_t &local) {
        return local.m_parent_symtab == fn.m_symtab &&
            call_site_locals.count(&local.base) == 0;
    }

    // Every routine a kernel reaches, kernels excluded.
    std::vector<ASR::Function_t*> device_routines() {
        std::vector<ASR::Function_t*> order;
        std::deque<ASR::Function_t*> work;
        kernel_of.clear();
        for (auto &item : unit.m_symtab->get_scope()) {
            if (!ASRUtils::is_device_kernel(item.second)) continue;
            ASR::Function_t *kernel =
                ASR::down_cast<ASR::Function_t>(item.second);
            kernel_of[kernel] = kernel;
            work.push_back(kernel);
        }
        while (!work.empty()) {
            ASR::Function_t *fn = work.front();
            work.pop_front();
            for (ASR::Function_t *callee : ASRUtils::get_called_functions(
                    fn->m_body, fn->n_body)) {
                if (ASRUtils::get_FunctionType(callee)->m_exec_space
                        == ASR::exec_spaceType::Kernel) {
                    continue;
                }
                if (kernel_of.emplace(callee, kernel_of[fn]).second) {
                    order.push_back(callee);
                    work.push_back(callee);
                }
            }
        }
        return order;
    }

    static std::vector<ASR::Variable_t*> runtime_sized_locals(
            ASR::Function_t &fn) {
        std::vector<SymbolTable*> scopes;
        gpu_routine_scopes(fn.m_symtab, scopes);
        std::vector<ASR::Variable_t*> locals;
        for (SymbolTable *scope : scopes) {
            for (auto &item : scope->get_scope()) {
                if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
                ASR::Variable_t *var =
                    ASR::down_cast<ASR::Variable_t>(item.second);
                if (runtime_sized_local(*var)) locals.push_back(var);
            }
        }
        return locals;
    }

    bool reaches_itself(ASR::Function_t &fn) {
        std::set<ASR::Function_t*> seen;
        std::deque<ASR::Function_t*> work = {&fn};
        while (!work.empty()) {
            ASR::Function_t *f = work.front();
            work.pop_front();
            for (ASR::Function_t *callee : ASRUtils::get_called_functions(
                    f->m_body, f->n_body)) {
                if (callee == &fn) return true;
                if (seen.insert(callee).second) work.push_back(callee);
            }
        }
        return false;
    }

    void decline(ASR::Function_t &fn, const Location &loc,
            const GpuDecline &why) {
        declined.insert(&fn);
        auto kernel = kernel_of.find(&fn);
        if (kernel != kernel_of.end()) {
            auto launch = launches.find(&kernel->second->base);
            if (launch != launches.end()) {
                report_gpu_decline(options, launch->second, why);
                return;
            }
        }
        report_gpu_decline(options, loc, why);
    }

    // Whether the routine's signature belongs to this translation unit
    // alone, so that every call to it can be given the new arguments.
    static bool owns_signature(ASR::Function_t &fn) {
        ASR::FunctionType_t *type = ASRUtils::get_FunctionType(fn);
        if (type->m_deftype != ASR::deftypeType::Implementation) return false;
        if (type->m_abi != ASR::abiType::Source) return false;
        SymbolTable *parent = fn.m_symtab->parent;
        if (parent && parent->asr_owner &&
                parent->asr_owner->type == ASR::asrType::symbol &&
                ASR::is_a<ASR::Module_t>(*ASR::down_cast<ASR::symbol_t>(
                    parent->asr_owner))) {
            return false;
        }
        return true;
    }

    bool lift(ASR::Function_t &fn, std::vector<ASR::Variable_t*> &locals) {
        ASR::Variable_t *first = locals[0];
        if (!owns_signature(fn)) {
            decline(fn, first->base.base.loc, GpuDecline(
                GpuDeclineReason::WorkspaceNotSizeableOnHost, first->m_name));
            return false;
        }
        if (reaches_itself(fn)) {
            decline(fn, first->base.base.loc, GpuDecline(
                GpuDeclineReason::RecursiveDeviceFunction, fn.m_name));
            return false;
        }
        std::set<ASR::symbol_t*> dummies;
        for (size_t i = 0; i < fn.n_args; i++) {
            if (ASR::is_a<ASR::Var_t>(*fn.m_args[i])) {
                dummies.insert(ASR::down_cast<ASR::Var_t>(fn.m_args[i])->m_v);
            }
        }
        // A named constant is written out as its value at the call site,
        // which cannot name the routine's own symbols.
        std::map<ASR::symbol_t*, ASR::expr_t*> constants;
        for (ASR::Variable_t *local : locals) {
            // An extent that reads what the routine binds once, or the
            // shape of another of its arrays, has the value it has on
            // entry to the routine. The callers write the extents over
            // the actual arguments of the call, which are the values the
            // dummy arguments have on entry: an extent evaluated later,
            // where a BLOCK starts or a call is made, may not read a dummy
            // argument the routine can change before (`n = n + 4` and
            // then `real :: w(n)` in a BLOCK, or `call inner(n, ...)`).
            ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(local->m_type);
            if (!gpu_write_extents_at_entry(al, fn, arr->m_dims, arr->n_dims)
                    && !sized_on_entry(fn, *local)) {
                decline(fn, local->base.base.loc, GpuDecline(
                    GpuDeclineReason::WorkspaceNotSizeableOnHost,
                    local->m_name));
                return false;
            }
            for (ASR::symbol_t *read : extent_reads(
                    *ASR::down_cast<ASR::Array_t>(local->m_type))) {
                if (dummies.count(read)) continue;
                ASR::symbol_t *s = ASRUtils::symbol_get_past_external(read);
                if (ASR::is_a<ASR::Variable_t>(*s)) {
                    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
                    if (v->m_storage == ASR::storage_typeType::Parameter
                            && v->m_value) {
                        constants[read] = v->m_value;
                        continue;
                    }
                }
                decline(fn, local->base.base.loc, GpuDecline(
                    GpuDeclineReason::WorkspaceNotSizeableOnHost,
                    local->m_name));
                return false;
            }
        }

        size_t n_source_args = fn.n_args;
        Vec<ASR::expr_t*> args;
        args.reserve(al, fn.n_args + locals.size());
        for (size_t i = 0; i < fn.n_args; i++) args.push_back(al, fn.m_args[i]);
        for (ASR::Variable_t *local : locals) {
            if (local->m_parent_symtab != fn.m_symtab) {
                std::string name = fn.m_symtab->get_unique_name(
                    std::string(local->m_name), false);
                local->m_parent_symtab->erase_symbol(local->m_name);
                local->m_name = s2c(al, name);
                local->m_parent_symtab = fn.m_symtab;
                fn.m_symtab->add_symbol(name, &local->base);
            }
            local->m_intent = ASR::intentType::InOut;
            args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al,
                local->base.base.loc, &local->base)));
        }
        fn.m_args = args.p;
        fn.n_args = args.n;
        fn.m_function_signature = ASRUtils::TYPE(
            ASRUtils::make_FunctionType_t_util(al, fn.base.base.loc,
                fn.m_args, fn.n_args, fn.m_return_var,
                ASRUtils::get_FunctionType(fn), fn.m_symtab));
        // Shared lowering has already turned every elemental reference
        // over an array into a loop of scalar calls, and an elemental
        // routine cannot take the array argument it now has.
        ASRUtils::get_FunctionType(fn)->m_elemental = false;

        CallSiteWorkspaces call_sites(al, fn, n_source_args, locals,
            constants, call_site_locals);
        call_sites.visit_TranslationUnit(unit);
        return true;
    }
};

} // namespace

void pass_gpu_device_workspace(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options) {
    GpuDeviceCapabilities caps = gpu_device_capabilities(pass_options);
    if (!caps.device_selected() || caps.device_function_runtime_sized_locals) {
        return;
    }
    DeviceWorkspaceLifter lifter(al, unit, pass_options);
    lifter.run();

    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

} // namespace LCompilers
