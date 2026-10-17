#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/pass/gpu_data_layout.h>
#include <libasr/pass/gpu_device_struct_storage.h>
#include <libasr/pass/gpu_offload_rewrite.h>
#include <libasr/pass/gpu_struct_component_args.h>
#include <libasr/pass/pass_utils.h>

#include <map>
#include <set>
#include <string>
#include <vector>

namespace LCompilers {

/*
Hand device code a derived-type component that holds device buffers through
a dummy argument of its own.

The device layout of a derived-type argument hands each allocatable array
component over as a buffer of its own (see `gpu_decomposed_components`), and
so does every device routine with a dummy of that type: the routine gets the
buffers beside the dummy. A component that is itself a derived-type value,
`self%input_map_` of a type with allocatable array components, is stored
inline in the argument, where its own allocatable components are addresses
of host memory, and nothing hands their buffers over. A routine the
component is passed to (`self%input_map_%map_to_training_range(x)`)
expects them.

This pass makes such a component an argument of its own wherever device
code uses it. A kernel argument or a dummy argument `d` of a device routine
whose component `d%m` the routine reads, or passes to a routine that needs
it, gets a companion dummy argument `d_m` of the type of `m`, which stands
for `d%m` throughout the routine. Every call of the routine passes the
companion of its actual argument, or `actual%m` when the actual has none,
and every launch of a kernel `actual%m` of the host's argument. The
companion is then an ordinary derived-type argument, whose components the
layout, the launch and the code generators hand over as for any other. A
component of a companion that holds buffers itself is split off the same
way, one level at a time, until none is left.

The pass runs once the device copies of the routines exist
(`device_partition`), so it never changes a routine the host calls.
*/

namespace {

// The derived type of `v` when it is a scalar value of one that the
// routine holds itself: neither allocatable, a pointer, nor an array.
ASR::Struct_t* scalar_struct(ASR::Variable_t *v) {
    if (ASRUtils::is_allocatable_or_pointer(v->m_type) ||
            ASRUtils::is_array(v->m_type) ||
            !ASR::is_a<ASR::StructType_t>(*v->m_type)) {
        return nullptr;
    }
    return gpu_struct_definition(v->m_type_declaration);
}

bool holds_device_buffers(ASR::Struct_t *st, std::set<ASR::Struct_t*> &seen);

// Whether `member` is a component that has to be split off: a derived-type
// value stored inline that holds device buffers.
bool split_member(ASR::symbol_t *member, std::set<ASR::Struct_t*> &seen) {
    member = ASRUtils::symbol_get_past_external(member);
    if (member == nullptr || !ASR::is_a<ASR::Variable_t>(*member)) {
        return false;
    }
    ASR::Struct_t *st = scalar_struct(ASR::down_cast<ASR::Variable_t>(member));
    return st != nullptr && holds_device_buffers(st, seen);
}

bool holds_device_buffers(ASR::Struct_t *st, std::set<ASR::Struct_t*> &seen) {
    if (st == nullptr || !seen.insert(st).second) return false;
    if (!gpu_decomposed_components(st).empty()) return true;
    std::vector<ASR::symbol_t*> members;
    gpu_collect_data_members(st, members);
    for (ASR::symbol_t *m : members) {
        if (split_member(m, seen)) return true;
    }
    return false;
}

ASR::symbol_t* var_symbol(ASR::expr_t *e) {
    if (e == nullptr) return nullptr;
    e = ASRUtils::get_past_array_physical_cast(e);
    if (e == nullptr || !ASR::is_a<ASR::Var_t>(*e)) return nullptr;
    return ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(e)->m_v);
}

ASR::Function_t* call_target(ASR::symbol_t *name) {
    ASR::symbol_t *s = ASRUtils::symbol_get_past_external(name);
    if (s == nullptr) return nullptr;
    s = ASRUtils::symbol_get_past_external(
        ASRUtils::symbol_get_past_StructMethodDeclaration(s));
    return s && ASR::is_a<ASR::Function_t>(*s)
        ? ASR::down_cast<ASR::Function_t>(s) : nullptr;
}

// A component of a dummy argument `d%m`, by the dummy's position and the
// component's name.
typedef std::pair<size_t, std::string> Use;

// One call made by device code: its target, its passed object and its
// actual arguments, which the pass appends to.
struct Call {
    ASR::Function_t *callee;
    ASR::expr_t *dt;
    ASR::call_arg_t **args;
    size_t *n_args;
};

class CallCollector : public ASRUtils::BlockBodyWalkVisitor<CallCollector> {
public:
    std::vector<Call> calls;
    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        ASRUtils::BlockBodyWalkVisitor<CallCollector>::visit_FunctionCall(x);
        ASR::FunctionCall_t &c = const_cast<ASR::FunctionCall_t&>(x);
        calls.push_back({call_target(c.m_name), c.m_dt, &c.m_args,
            &c.n_args});
    }
    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        ASRUtils::BlockBodyWalkVisitor<CallCollector>::visit_SubroutineCall(x);
        ASR::SubroutineCall_t &c = const_cast<ASR::SubroutineCall_t&>(x);
        calls.push_back({call_target(c.m_name), c.m_dt, &c.m_args,
            &c.n_args});
    }
};

// The components `d%m` of the dummy arguments of a routine that its body
// and the extents of its arrays name.
class ComponentUses : public ASRUtils::BlockBodyWalkVisitor<ComponentUses> {
public:
    std::map<ASR::symbol_t*, size_t> dummies;
    std::set<Use> uses;
    void visit_StructInstanceMember(const ASR::StructInstanceMember_t &x) {
        ASRUtils::BlockBodyWalkVisitor<ComponentUses>
            ::visit_StructInstanceMember(x);
        auto d = dummies.find(var_symbol(x.m_v));
        if (d == dummies.end()) return;
        std::set<ASR::Struct_t*> seen;
        if (split_member(x.m_m, seen)) {
            uses.insert({d->second, ASRUtils::symbol_name(
                ASRUtils::symbol_get_past_external(x.m_m))});
        }
    }
};

// Replaces `d%m` by the companion of `d` for `m`.
class ComponentReplacer : public ASR::BaseExprReplacer<ComponentReplacer> {
public:
    Allocator &al;
    std::map<std::pair<ASR::symbol_t*, std::string>, ASR::symbol_t*>
        &companions;
    ComponentReplacer(Allocator &al_, std::map<std::pair<ASR::symbol_t*,
            std::string>, ASR::symbol_t*> &companions_)
        : al(al_), companions(companions_) {}
    void replace_StructInstanceMember(ASR::StructInstanceMember_t *x) {
        ASR::BaseExprReplacer<ComponentReplacer>
            ::replace_StructInstanceMember(x);
        ASR::symbol_t *d = var_symbol(x->m_v);
        if (d == nullptr) return;
        auto c = companions.find({d, ASRUtils::symbol_name(
            ASRUtils::symbol_get_past_external(x->m_m))});
        if (c == companions.end()) return;
        *current_expr = ASRUtils::EXPR(ASR::make_Var_t(al, x->base.base.loc,
            c->second));
    }
};

class ComponentReplacerVisitor :
        public ASR::CallReplacerOnExpressionsVisitor<ComponentReplacerVisitor> {
public:
    ComponentReplacer replacer;
    ComponentReplacerVisitor(Allocator &al, std::map<std::pair<ASR::symbol_t*,
            std::string>, ASR::symbol_t*> &companions)
        : replacer(al, companions) {}
    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }
};

class Launches : public ASR::BaseWalkVisitor<Launches> {
public:
    std::vector<std::pair<ASR::GpuKernelLaunch_t*, SymbolTable*>> found;
    SymbolTable *scope = nullptr;
    void visit_Program(const ASR::Program_t &x) {
        SymbolTable *outer = scope;
        scope = x.m_symtab;
        ASR::BaseWalkVisitor<Launches>::visit_Program(x);
        scope = outer;
    }
    void visit_Function(const ASR::Function_t &x) {
        SymbolTable *outer = scope;
        scope = x.m_symtab;
        ASR::BaseWalkVisitor<Launches>::visit_Function(x);
        scope = outer;
    }
    void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &x) {
        found.push_back({const_cast<ASR::GpuKernelLaunch_t*>(&x), scope});
    }
};

class StructComponentArgs {
public:
    StructComponentArgs(Allocator &al_, ASR::TranslationUnit_t &unit_)
        : al(al_), unit(unit_) {}

    // Splits off one level of components. Returns whether it split any.
    bool run() {
        device.clear();
        collect(unit.m_symtab);
        std::map<ASR::Function_t*, std::set<Use>> uses = find_uses();
        companions.clear();
        handed_over.clear();
        std::map<ASR::Function_t*, std::vector<std::pair<Use,
            ASR::symbol_t*>>> added;
        std::map<ASR::Function_t*, size_t> original;
        for (ASR::Function_t *fn : device) {
            original[fn] = fn->n_args;
            for (const Use &u : uses[fn]) {
                ASR::symbol_t *c = companion(*fn, u);
                if (c == nullptr) continue;
                added[fn].push_back({u, c});
                companions[{var_symbol(fn->m_args[u.first]), u.second}] = c;
            }
        }
        if (added.empty()) return false;
        for (auto &a : added) {
            std::vector<ASR::symbol_t*> dummies;
            for (auto &u : a.second) dummies.push_back(u.second);
            append_dummies(*a.first, dummies);
        }
        for (ASR::Function_t *fn : device) {
            CallCollector calls;
            for (size_t i = 0; i < fn->n_body; i++) {
                calls.visit_stmt(*fn->m_body[i]);
            }
            for (Call &call : calls.calls) {
                auto a = added.find(call.callee);
                if (a == added.end()) continue;
                pass_companions(call, original[call.callee], a->second,
                    fn->m_symtab);
            }
        }
        Launches launches;
        launches.visit_TranslationUnit(unit);
        for (auto &l : launches.found) {
            ASR::GpuKernelLaunch_t *launch = l.first;
            ASR::Function_t *kernel = call_target(launch->m_kernel);
            auto a = added.find(kernel);
            if (a == added.end()) continue;
            Call call{kernel, nullptr, &launch->m_args, &launch->n_args};
            size_t before = launch->n_args;
            pass_companions(call, original[kernel], a->second, l.second);
            hand_over_components(*kernel, *launch, before, a->second,
                l.second);
        }
        ComponentReplacerVisitor replace(al, companions);
        for (ASR::Function_t *fn : device) replace.visit_Function(*fn);
        return true;
    }

private:
    Allocator &al;
    ASR::TranslationUnit_t &unit;
    std::vector<ASR::Function_t*> device;
    std::map<std::pair<ASR::symbol_t*, std::string>, ASR::symbol_t*>
        companions;

    // Every kernel and device routine, whose signature belongs to device
    // code alone.
    void collect(SymbolTable *scope) {
        for (auto &item : scope->get_scope()) {
            ASR::symbol_t *s = item.second;
            if (ASR::is_a<ASR::Function_t>(*s)) {
                ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(s);
                ASR::exec_spaceType space = ASRUtils::get_exec_space(*fn);
                if (space == ASR::exec_spaceType::Kernel ||
                        space == ASR::exec_spaceType::Device) {
                    device.push_back(fn);
                }
                collect(fn->m_symtab);
            } else if (ASR::is_a<ASR::Module_t>(*s)) {
                collect(ASR::down_cast<ASR::Module_t>(s)->m_symtab);
            } else if (ASR::is_a<ASR::Program_t>(*s)) {
                collect(ASR::down_cast<ASR::Program_t>(s)->m_symtab);
            }
        }
    }

    // The components each routine needs a companion for: those it names,
    // and those a routine it passes the dummy to needs.
    std::map<ASR::Function_t*, std::set<Use>> find_uses() {
        std::map<ASR::Function_t*, std::set<Use>> uses;
        std::map<ASR::Function_t*, std::vector<Call>> calls;
        std::map<ASR::Function_t*, std::map<ASR::symbol_t*, size_t>> dummies;
        std::set<ASR::Function_t*> is_device(device.begin(), device.end());
        for (ASR::Function_t *fn : device) {
            ComponentUses named;
            for (size_t i = 0; i < fn->n_args; i++) {
                ASR::symbol_t *d = var_symbol(fn->m_args[i]);
                if (d && ASR::is_a<ASR::Variable_t>(*d) && scalar_struct(
                        ASR::down_cast<ASR::Variable_t>(d))) {
                    named.dummies[d] = i;
                }
            }
            dummies[fn] = named.dummies;
            if (named.dummies.empty()) continue;
            for (size_t i = 0; i < fn->n_body; i++) {
                named.visit_stmt(*fn->m_body[i]);
            }
            std::vector<SymbolTable*> scopes;
            gpu_routine_scopes(fn->m_symtab, scopes);
            for (SymbolTable *scope : scopes) {
                for (auto &item : scope->get_scope()) {
                    if (ASR::is_a<ASR::Variable_t>(*item.second)) {
                        named.visit_ttype(*ASR::down_cast<ASR::Variable_t>(
                            item.second)->m_type);
                    }
                }
            }
            uses[fn] = named.uses;
            CallCollector collector;
            for (size_t i = 0; i < fn->n_body; i++) {
                collector.visit_stmt(*fn->m_body[i]);
            }
            calls[fn] = collector.calls;
        }
        for (bool changed = true; changed;) {
            changed = false;
            for (ASR::Function_t *fn : device) {
                if (dummies[fn].empty()) continue;
                for (Call &call : calls[fn]) {
                    if (!is_device.count(call.callee)) continue;
                    for (const Use &u : uses[call.callee]) {
                        ASR::expr_t *actual = actual_of(call,
                            call.callee->n_args, u.first);
                        auto d = dummies[fn].find(var_symbol(actual));
                        if (d == dummies[fn].end()) continue;
                        changed = uses[fn].insert({d->second, u.second})
                            .second || changed;
                    }
                }
            }
        }
        return uses;
    }

    // The actual argument a call passes for the dummy at `index` of a
    // routine of `n_dummies` dummies, the passed object included.
    static ASR::expr_t* actual_of(const Call &call, size_t n_dummies,
            size_t index) {
        size_t shift = call.dt != nullptr && *call.n_args + 1 == n_dummies
            ? 1 : 0;
        if (shift == 1 && index == 0) return call.dt;
        if (index - shift >= *call.n_args) return nullptr;
        return (*call.args)[index - shift].m_value;
    }

    // A dummy argument of `fn` that stands for the component `u`.
    ASR::symbol_t* companion(ASR::Function_t &fn, const Use &u) {
        ASR::Variable_t *d = ASR::down_cast<ASR::Variable_t>(
            var_symbol(fn.m_args[u.first]));
        ASR::Struct_t *st = scalar_struct(d);
        ASR::symbol_t *member = ASRUtils::symbol_get_past_external(
            gpu_struct_lookup_member(&st->base, u.second));
        if (member == nullptr || !ASR::is_a<ASR::Variable_t>(*member)) {
            return nullptr;
        }
        ASR::Variable_t *m = ASR::down_cast<ASR::Variable_t>(member);
        ASR::Struct_t *type = gpu_struct_definition(m->m_type_declaration);
        // The routine declares the companion with the type the component
        // has, which it has to see.
        ASR::symbol_t *declaration = fn.m_symtab->resolve_symbol(
            type->m_name);
        if (declaration == nullptr ||
                ASRUtils::symbol_get_past_external(declaration) !=
                    &type->base) {
            return nullptr;
        }
        const Location &loc = d->base.base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        ASR::expr_t *v = b.Variable(fn.m_symtab, fn.m_symtab->get_unique_name(
                std::string(d->m_name) + "_" + u.second, false),
            ASRUtils::duplicate_type(al, m->m_type), d->m_intent,
            declaration);
        // Present exactly when the argument it is a component of is.
        ASR::down_cast<ASR::Variable_t>(ASR::down_cast<ASR::Var_t>(v)->m_v)
            ->m_presence = d->m_presence;
        return ASR::down_cast<ASR::Var_t>(v)->m_v;
    }

    void append_dummies(ASR::Function_t &fn,
            const std::vector<ASR::symbol_t*> &args) {
        const Location &loc = fn.base.base.loc;
        Vec<ASR::expr_t*> all;
        all.reserve(al, fn.n_args + args.size());
        for (size_t i = 0; i < fn.n_args; i++) all.push_back(al, fn.m_args[i]);
        for (ASR::symbol_t *a : args) {
            all.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc, a)));
        }
        fn.m_args = all.p;
        fn.n_args = all.n;
        fn.m_function_signature = ASRUtils::TYPE(
            ASRUtils::make_FunctionType_t_util(al, loc, fn.m_args, fn.n_args,
                fn.m_return_var, ASRUtils::get_FunctionType(fn),
                fn.m_symtab));
    }

    // Passes `call` the companions its target was given: the companion of
    // the actual argument when it has one, else the component of the
    // actual argument, made in `scope`.
    void pass_companions(Call &call, size_t original_dummies,
            const std::vector<std::pair<Use, ASR::symbol_t*>> &added,
            SymbolTable *scope) {
        std::vector<ASR::expr_t*> actuals;
        for (size_t i = 0; i < original_dummies; i++) {
            actuals.push_back(actual_of(call, original_dummies, i));
        }
        Vec<ASR::call_arg_t> args;
        args.reserve(al, *call.n_args + added.size());
        for (size_t i = 0; i < *call.n_args; i++) {
            args.push_back(al, (*call.args)[i]);
        }
        for (auto &a : added) {
            ASR::expr_t *of = a.first.first < actuals.size()
                ? actuals[a.first.first] : nullptr;
            // The companion of an optional argument the call omits is
            // omitted too.
            ASR::expr_t *value = of ? component_of(of, a.first.second,
                scope) : nullptr;
            LCOMPILERS_ASSERT(of == nullptr || value != nullptr);
            actuals.push_back(value);
            ASR::call_arg_t arg;
            arg.loc = value ? value->base.loc : a.second->base.loc;
            arg.m_value = value;
            args.push_back(al, arg);
        }
        *call.args = args.p;
        *call.n_args = args.n;
    }

    // Kernels whose companions of this round already have the parameters
    // that hand over their components.
    std::set<ASR::Function_t*> handed_over;

    // A companion of a kernel is a derived-type kernel argument like any
    // other, whose allocatable array components reach the device as kernel
    // parameters of their own (see gpu_struct_member_arguments), read from
    // the companion's actual argument, which `launch` passes from `before`
    // on.
    void hand_over_components(ASR::Function_t &kernel,
            ASR::GpuKernelLaunch_t &launch, size_t before,
            const std::vector<std::pair<Use, ASR::symbol_t*>> &added,
            SymbolTable *host_scope) {
        const Location &loc = launch.base.base.loc;
        // A second launch of the kernel passes the same arguments, to the
        // parameters the first one made.
        bool made = !handed_over.insert(&kernel).second;
        SymbolTable *kernel_scope = made
            ? al.make_new<SymbolTable>(kernel.m_symtab) : kernel.m_symtab;
        Vec<ASR::expr_t*> parameters;
        parameters.reserve(al, 1);
        Vec<ASR::call_arg_t> actuals;
        actuals.reserve(al, 1);
        auto element_declaration = [&](ASR::Struct_t *element) {
            ASR::symbol_t *s = kernel.m_symtab->resolve_symbol(
                element->m_name);
            return s && ASRUtils::symbol_get_past_external(s) ==
                &element->base ? s : nullptr;
        };
        for (bool data : {false, true}) {
            for (size_t i = 0; i < added.size(); i++) {
                ASR::expr_t *host = launch.m_args[before + i].m_value;
                ASR::Struct_t *st = gpu_struct_definition(
                    ASRUtils::get_struct_sym_from_struct_expr(host));
                std::string name = ASRUtils::symbol_name(added[i].second);
                auto host_member = [&](ASR::symbol_t *m) {
                    return ASRUtils::EXPR(ASRUtils::getStructInstanceMember_t(
                        al, loc, (ASR::asr_t*)duplicate_expr(host), nullptr,
                        m, host_scope));
                };
                if (data) {
                    gpu_struct_member_data_arguments(al, loc, kernel_scope,
                        name, st, host_member, element_declaration,
                        parameters, actuals);
                } else {
                    gpu_struct_member_extent_arguments(al, loc, kernel_scope,
                        name, st, host_member, parameters, actuals);
                }
            }
        }
        if (!made) {
            std::vector<ASR::symbol_t*> dummies;
            for (size_t i = 0; i < parameters.n; i++) {
                dummies.push_back(var_symbol(parameters[i]));
            }
            append_dummies(kernel, dummies);
        }
        Vec<ASR::call_arg_t> args;
        args.reserve(al, launch.n_args + actuals.n);
        for (size_t i = 0; i < launch.n_args; i++) {
            args.push_back(al, launch.m_args[i]);
        }
        for (size_t i = 0; i < actuals.n; i++) args.push_back(al, actuals[i]);
        launch.m_args = args.p;
        launch.n_args = args.n;
    }

    ASR::expr_t* component_of(ASR::expr_t *actual, const std::string &member,
            SymbolTable *scope) {
        ASR::expr_t *base = ASRUtils::get_past_array_physical_cast(actual);
        auto c = companions.find({var_symbol(base), member});
        const Location &loc = actual->base.loc;
        if (c != companions.end()) {
            return ASRUtils::EXPR(ASR::make_Var_t(al, loc, c->second));
        }
        ASR::symbol_t *m = gpu_struct_lookup_member(
            ASRUtils::get_struct_sym_from_struct_expr(base), member);
        if (m == nullptr) return nullptr;
        return ASRUtils::EXPR(ASRUtils::getStructInstanceMember_t(al, loc,
            (ASR::asr_t*)duplicate_expr(base), nullptr, m, scope));
    }

    ASR::expr_t* duplicate_expr(ASR::expr_t *e) {
        ASRUtils::ExprStmtDuplicator duplicator(al);
        return duplicator.duplicate_expr(e);
    }
};

} // namespace

void pass_gpu_struct_component_args(Allocator &al,
        ASR::TranslationUnit_t &unit, const PassOptions &/*pass_options*/) {
    StructComponentArgs split(al, unit);
    bool changed = false;
    while (split.run()) changed = true;
    if (!changed) return;
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

} // namespace LCompilers
