#ifndef LIBASR_ASR_SIDE_EFFECT_H
#define LIBASR_ASR_SIDE_EFFECT_H

#include <libasr/asr_utils.h>
#include <libasr/pass/intrinsic_subroutines.h>

#include <map>

namespace LCompilers {

namespace ASR {

inline bool is_side_effect_free_intrinsic_impure_subroutine(int64_t id) {
    return id == static_cast<int64_t>(ASRUtils::IntrinsicImpureSubroutines::MoveAlloc)
        || id == static_cast<int64_t>(ASRUtils::IntrinsicImpureSubroutines::Mvbits);
}

class SideEffectFinder : public BaseWalkVisitor<SideEffectFinder> {
public:
    bool found = false;
    Location loc;
    std::string description;
    // Report only unchecked dynamic trait lifecycle effects. Every procedure's
    // effect metadata retains them, not only the bodies of PURE procedures.
    bool lifecycle_only = false;

    void mark_found(const Location &l, const std::string &desc) {
        found = true;
        loc = l;
        description = desc;
    }

    // A value of a type owning runtime trait objects, other than a designator
    // or a function result, is a temporary whose destruction runs dynamic
    // lifecycle code, e.g. the result of PACK applied to such an array.
    void visit_expr(const expr_t &x) {
        if (found) return;
        switch (x.type) {
            case exprType::Var:
            case exprType::StructInstanceMember:
            case exprType::ArrayItem:
            case exprType::ArraySection:
            case exprType::ArrayPhysicalCast:
            case exprType::Cast:
            case exprType::FunctionCall:
                break;
            default:
                if (ASRUtils::contains_trait_owner(ASRUtils::expr_type(
                        const_cast<expr_t*>(&x)))) {
                    mark_found(x.base.loc,
                        "runtime trait temporary with unchecked dynamic lifecycle effects");
                    return;
                }
        }
        BaseWalkVisitor::visit_expr(x);
    }

    template <typename Scope>
    void visit_executable_body(const Scope &scope) {
        if (ASRUtils::has_trait_component_cleanup(scope.m_symtab)) {
            mark_found(scope.base.base.loc,
                "runtime trait component cleanup with unchecked dynamic lifecycle effects");
            return;
        }
        for (size_t i = 0; i < scope.n_body && !found; i++) {
            visit_stmt(*scope.m_body[i]);
        }
    }

    void visit_BlockCall(const BlockCall_t &x) {
        if (found) return;
        visit_executable_body(*down_cast<Block_t>(
            ASRUtils::symbol_get_past_external(x.m_m)));
    }

    void visit_AssociateBlockCall(const AssociateBlockCall_t &x) {
        if (found) return;
        visit_executable_body(*down_cast<AssociateBlock_t>(
            ASRUtils::symbol_get_past_external(x.m_m)));
    }

    void visit_Print(const Print_t &x) {
        if (found || lifecycle_only) return;
        mark_found(x.base.base.loc, "PRINT statement");
    }

    void visit_FileOpen(const FileOpen_t &x) {
        if (found || lifecycle_only) return;
        mark_found(x.base.base.loc, "OPEN statement");
    }

    void visit_FileClose(const FileClose_t &x) {
        if (found || lifecycle_only) return;
        mark_found(x.base.base.loc, "CLOSE statement");
    }

    void visit_FileBackspace(const FileBackspace_t &x) {
        if (found || lifecycle_only) return;
        mark_found(x.base.base.loc, "BACKSPACE statement");
    }

    void visit_FileRewind(const FileRewind_t &x) {
        if (found || lifecycle_only) return;
        mark_found(x.base.base.loc, "REWIND statement");
    }

    void visit_FileEndfile(const FileEndfile_t &x) {
        if (found || lifecycle_only) return;
        mark_found(x.base.base.loc, "ENDFILE statement");
    }

    void visit_FileInquire(const FileInquire_t &x) {
        if (found || lifecycle_only) return;
        mark_found(x.base.base.loc, "INQUIRE statement");
    }

    void visit_Flush(const Flush_t &x) {
        if (found || lifecycle_only) return;
        mark_found(x.base.base.loc, "FLUSH statement");
    }

    // Procedure variables that hold a procedure with an implicit interface,
    // mapped to that procedure, e.g. the temporary a call through an
    // implicit interface is made through. Such a procedure is not known to
    // be pure (F2018 15.4.2.2), so a call through the variable is a call to
    // an impure procedure. Not owned.
    const std::map<const symbol_t*, symbol_t*>* implicit_interface_procedures = nullptr;

    // Marks the call to `name` at `l` if the called procedure is not known to
    // be free of side effects.
    bool check_call(const Location &l, symbol_t* name) {
        symbol_t* sym = ASRUtils::symbol_get_past_external(name);
        auto *declared = ASRUtils::get_function(sym);
        if (declared && ASRUtils::has_trait_out_cleanup(*declared)) {
            mark_found(l,
                "runtime trait intent(out) cleanup with unchecked dynamic lifecycle effects");
            return true;
        }
        if (lifecycle_only) return false;
        std::string proc_name;
        if (is_a<Function_t>(*sym)) {
            if (down_cast<Function_t>(sym)->m_side_effect_free) {
                return false;
            }
            proc_name = down_cast<Function_t>(sym)->m_name;
        } else {
            if (implicit_interface_procedures == nullptr) {
                return false;
            }
            auto proc = implicit_interface_procedures->find(sym);
            if (proc == implicit_interface_procedures->end()) {
                return false;
            }
            proc_name = ASRUtils::symbol_name(proc->second);
        }
        mark_found(l, "Call to impure procedure '" + proc_name + "'");
        return true;
    }

    void visit_SubroutineCall(const SubroutineCall_t &x) {
        if (found) return;
        if (x.m_name && check_call(x.base.base.loc, x.m_name)) {
            return;
        }
        BaseWalkVisitor::visit_SubroutineCall(x);
    }

    void visit_IntrinsicImpureSubroutine(const IntrinsicImpureSubroutine_t &x) {
        if (found) return;
        // MOVE_ALLOC deallocates an allocated TO (F2018 16.9.137).
        if (x.m_sub_intrinsic_id == static_cast<int64_t>(
                ASRUtils::IntrinsicImpureSubroutines::MoveAlloc)) {
            for (size_t i = 0; i < x.n_args; i++) {
                if (ASRUtils::has_trait_lifecycle_target(ASRUtils::expr_type(x.m_args[i]))) {
                    mark_found(x.base.base.loc,
                        "runtime trait move_alloc with unchecked dynamic lifecycle effects");
                    return;
                }
            }
        }
        if (lifecycle_only) return;
        if (is_side_effect_free_intrinsic_impure_subroutine(x.m_sub_intrinsic_id)) {
            return;
        }
        mark_found(x.base.base.loc, "Call to impure intrinsic subroutine");
    }

    void visit_FunctionCall(const FunctionCall_t &x) {
        if (found) return;
        if (ASRUtils::contains_trait_owner(x.m_type)) {
            mark_found(x.base.base.loc,
                "runtime trait result cleanup with unchecked dynamic lifecycle effects");
            return;
        }
        if (x.m_name && check_call(x.base.base.loc, x.m_name)) {
            return;
        }
        BaseWalkVisitor::visit_FunctionCall(x);
    }

    bool check_trait_call(const Location &l, symbol_t *name, int64_t slot) {
        if (!check_call(l, name)) return false;
        auto *contract = down_cast<TraitRuntimeContract_t>(ASRUtils::get_asr_owner(
            ASRUtils::symbol_get_past_external(name)));
        auto *origin = ASRUtils::symbol_get_past_external(
            contract->m_slots[slot].m_origins[0]);
        description = "call to impure trait procedure '" +
            std::string(ASRUtils::symbol_name(origin)) + "'";
        return true;
    }

    void visit_TraitFunctionCall(const TraitFunctionCall_t &x) {
        if (found) return;
        if (check_trait_call(x.base.base.loc, x.m_name, x.m_slot)) return;
        BaseWalkVisitor::visit_TraitFunctionCall(x);
    }

    void visit_TraitSubroutineCall(const TraitSubroutineCall_t &x) {
        if (found) return;
        if (check_trait_call(x.base.base.loc, x.m_name, x.m_slot)) return;
        BaseWalkVisitor::visit_TraitSubroutineCall(x);
    }

    void visit_TraitAllocate(const TraitAllocate_t &x) {
        if (!found) mark_found(x.base.base.loc,
            "runtime trait allocation with unchecked dynamic lifecycle effects");
    }

    void visit_TraitAssignment(const TraitAssignment_t &x) {
        if (!found) mark_found(x.base.base.loc,
            "runtime trait assignment with unchecked dynamic lifecycle effects");
    }
    void visit_TraitRetain(const TraitRetain_t &x) {
        if (!found) mark_found(x.base.base.loc,
            "retained runtime trait results with unchecked dynamic lifecycle effects");
    }

    void visit_Assignment(const Assignment_t &x) {
        if (found) return;
        if (!x.m_overloaded &&
                ASRUtils::has_trait_lifecycle_target(ASRUtils::expr_type(x.m_target))) {
            mark_found(x.base.base.loc,
                "runtime trait component assignment with unchecked dynamic lifecycle effects");
            return;
        }
        BaseWalkVisitor::visit_Assignment(x);
    }

    void check_trait_deallocation(const Location &location,
            expr_t **vars, size_t n_vars) {
        for (size_t i = 0; i < n_vars && !found; i++) {
            if (ASRUtils::has_trait_lifecycle_target(ASRUtils::expr_type(vars[i]))) {
                mark_found(location,
                    "runtime trait deallocation with unchecked dynamic lifecycle effects");
            }
        }
    }

    void visit_ExplicitDeallocate(const ExplicitDeallocate_t &x) {
        check_trait_deallocation(x.base.base.loc, x.m_vars, x.n_vars);
        if (!found) BaseWalkVisitor::visit_ExplicitDeallocate(x);
    }

    void visit_ImplicitDeallocate(const ImplicitDeallocate_t &x) {
        check_trait_deallocation(x.base.base.loc, x.m_vars, x.n_vars);
        if (!found) BaseWalkVisitor::visit_ImplicitDeallocate(x);
    }
};

// Whether `body` performs an operation with unchecked dynamic trait lifecycle
// effects, so that its procedure is neither side-effect free nor deterministic.
inline bool has_trait_lifecycle_effects(stmt_t **body, size_t n_body) {
    SideEffectFinder finder;
    finder.lifecycle_only = true;
    for (size_t i = 0; i < n_body && !finder.found; i++) {
        finder.visit_stmt(*body[i]);
    }
    return finder.found;
}

// Whether `function` has such effects, including intent(out) and local cleanup.
inline bool has_trait_lifecycle_effects(const Function_t &function) {
    return ASRUtils::has_trait_out_cleanup(function) ||
        ASRUtils::has_trait_component_cleanup(function.m_symtab) ||
        has_trait_lifecycle_effects(function.m_body, function.n_body);
}

// Whether an interface declares its procedure pure (F2018 15.7). LFortran
// does not record IMPURE, so an elemental interface counts as pure.
inline bool interface_is_pure(const Function_t &function) {
    auto *type = ASRUtils::get_FunctionType(function);
    return type->m_pure || type->m_elemental;
}

// Calls `visit` for each procedure and derived type in `scope` and the scopes
// it contains. A module loaded from a module file is skipped unless `loaded`
// is set.
template <typename Visit>
void visit_trait_lifecycle_scopes(SymbolTable *scope, bool loaded, Visit &&visit) {
    for (auto &item : scope->get_scope()) {
        symbol_t *symbol = item.second;
        SymbolTable *inner = nullptr;
        switch (symbol->type) {
            case symbolType::Module:
                if (!loaded && down_cast<Module_t>(symbol)->m_loaded_from_mod) continue;
                inner = down_cast<Module_t>(symbol)->m_symtab;
                break;
            case symbolType::Program:
                inner = down_cast<Program_t>(symbol)->m_symtab;
                break;
            case symbolType::Function:
                visit(symbol);
                inner = down_cast<Function_t>(symbol)->m_symtab;
                break;
            case symbolType::Struct:
                visit(symbol);
                inner = down_cast<Struct_t>(symbol)->m_symtab;
                break;
            case symbolType::Block:
                inner = down_cast<Block_t>(symbol)->m_symtab;
                break;
            case symbolType::AssociateBlock:
                inner = down_cast<AssociateBlock_t>(symbol)->m_symtab;
                break;
            case symbolType::TraitRuntimeContract:
                inner = down_cast<TraitRuntimeContract_t>(symbol)->m_symtab;
                break;
            case symbolType::TraitWitness:
                inner = down_cast<TraitWitness_t>(symbol)->m_symtab;
                break;
            case symbolType::TraitErasure:
                inner = down_cast<TraitErasure_t>(symbol)->m_symtab;
                break;
            case symbolType::Template:
                inner = down_cast<Template_t>(symbol)->m_symtab;
                break;
            case symbolType::Requirement:
                inner = down_cast<Requirement_t>(symbol)->m_symtab;
                break;
            default:
                continue;
        }
        visit_trait_lifecycle_scopes(inner, loaded, visit);
    }
}

// The unchecked dynamic trait lifecycle effects that procedures reach through
// the procedures they call. It is built once every body is complete, so the
// order in which the bodies were analyzed does not matter, and solved as a
// fixpoint, so neither does recursion. A procedure with a body is analyzed,
// whether it was compiled now or loaded from a module file. A procedure known
// only through an interface (an external procedure, a dummy or pointer
// procedure, a deferred binding, an override this compilation cannot see, a
// trait slot) has unknown effects unless the interface declares it pure.
class TraitLifecycleSummary {
public:
    enum class Effect { None, Unknown, Lifecycle };

    struct Call {
        Location loc;
        // The procedure called, or nullptr for a trait slot.
        symbol_t *procedure = nullptr;
        std::string description;
        std::vector<Function_t*> callees;
        bool unknown = false;
    };

    // `unit_scope` is the scope of the translation unit being compiled; the
    // scopes of the units it extends, as interactive cells do, are its parents.
    explicit TraitLifecycleSummary(SymbolTable *unit_scope) : unit_scope{unit_scope} {}

    Effect effect(Function_t &function) {
        solve(function);
        return nodes.at(&function).effect;
    }

    // The first call in `function` through which it reaches an effect.
    const Call *first_effect_call(Function_t &function) {
        solve(function);
        for (const Call &call : nodes.at(&function).calls) {
            if (call.unknown) return &call;
            for (Function_t *callee : call.callees) {
                if (nodes.at(callee).effect != Effect::None) return &call;
            }
        }
        return nullptr;
    }

    static bool analyzable(const Function_t &function) {
        return ASRUtils::get_FunctionType(function)->m_deftype ==
            deftypeType::Implementation;
    }

private:
    struct Node {
        Effect own = Effect::None;
        Effect effect = Effect::None;
        std::vector<Call> calls;
    };

    SymbolTable *unit_scope;
    std::map<Function_t*, Node> nodes;
    std::map<const Struct_t*, std::vector<Struct_t*>> extensions;
    bool extensions_built = false;

    class CallCollector : public BaseWalkVisitor<CallCollector> {
    public:
        TraitLifecycleSummary &summary;
        std::vector<Call> calls;

        explicit CallCollector(TraitLifecycleSummary &summary) : summary{summary} {}

        // Specification expressions can call procedures too.
        void collect_declarations(SymbolTable *scope) {
            for (auto &item : scope->get_scope()) {
                if (!is_a<Variable_t>(*item.second)) continue;
                auto *variable = down_cast<Variable_t>(item.second);
                visit_ttype(*variable->m_type);
                if (variable->m_symbolic_value) visit_expr(*variable->m_symbolic_value);
                if (variable->m_value) visit_expr(*variable->m_value);
            }
        }

        template <typename Scope>
        void collect_scope(const Scope &scope) {
            collect_declarations(scope.m_symtab);
            for (size_t i = 0; i < scope.n_body; i++) visit_stmt(*scope.m_body[i]);
        }

        void visit_BlockCall(const BlockCall_t &x) {
            collect_scope(*down_cast<Block_t>(ASRUtils::symbol_get_past_external(x.m_m)));
        }

        void visit_AssociateBlockCall(const AssociateBlockCall_t &x) {
            collect_scope(*down_cast<AssociateBlock_t>(
                ASRUtils::symbol_get_past_external(x.m_m)));
        }

        void add(const Location &loc, symbol_t *name, expr_t *dt) {
            Call call;
            call.loc = loc;
            symbol_t *symbol = ASRUtils::symbol_get_past_external(name);
            call.procedure = symbol;
            call.description = "Call to impure procedure '" +
                std::string(ASRUtils::symbol_name(symbol)) + "'";
            summary.add_procedure(symbol, dt, call);
            if (call.unknown || !call.callees.empty()) calls.push_back(call);
        }

        void visit_SubroutineCall(const SubroutineCall_t &x) {
            if (x.m_name) add(x.base.base.loc, x.m_name, x.m_dt);
            BaseWalkVisitor::visit_SubroutineCall(x);
        }

        void visit_FunctionCall(const FunctionCall_t &x) {
            if (x.m_name) add(x.base.base.loc, x.m_name, x.m_dt);
            BaseWalkVisitor::visit_FunctionCall(x);
        }

        // A slot is implemented by whichever procedures implement the trait.
        void add_trait_call(const Location &loc, symbol_t *name, int64_t slot) {
            symbol_t *symbol = ASRUtils::symbol_get_past_external(name);
            if (is_a<Function_t>(*symbol) &&
                    interface_is_pure(*down_cast<Function_t>(symbol))) return;
            auto *contract = down_cast<TraitRuntimeContract_t>(ASRUtils::get_asr_owner(symbol));
            auto *origin = ASRUtils::symbol_get_past_external(
                contract->m_slots[slot].m_origins[0]);
            Call call;
            call.loc = loc;
            call.description = "call to impure trait procedure '" +
                std::string(ASRUtils::symbol_name(origin)) + "'";
            call.unknown = true;
            calls.push_back(call);
        }

        void visit_TraitFunctionCall(const TraitFunctionCall_t &x) {
            add_trait_call(x.base.base.loc, x.m_name, x.m_slot);
            BaseWalkVisitor::visit_TraitFunctionCall(x);
        }

        void visit_TraitSubroutineCall(const TraitSubroutineCall_t &x) {
            add_trait_call(x.base.base.loc, x.m_name, x.m_slot);
            BaseWalkVisitor::visit_TraitSubroutineCall(x);
        }
    };

    void add_procedure(symbol_t *symbol, expr_t *dt, Call &call) {
        symbol = ASRUtils::symbol_get_past_external(symbol);
        switch (symbol->type) {
            case symbolType::Function: {
                auto *function = down_cast<Function_t>(symbol);
                if (analyzable(*function)) {
                    call.callees.push_back(function);
                } else if (!interface_is_pure(*function)) {
                    call.unknown = true;
                }
                break;
            }
            case symbolType::StructMethodDeclaration: {
                auto *binding = down_cast<StructMethodDeclaration_t>(symbol);
                add_procedure(binding->m_proc, nullptr, call);
                if (binding->m_dispatch_proc) {
                    add_procedure(binding->m_dispatch_proc, nullptr, call);
                }
                if (dt && ASRUtils::is_class_type(ASRUtils::extract_type(
                        ASRUtils::expr_type(dt)))) {
                    add_overrides(*binding, call);
                }
                break;
            }
            case symbolType::Variable: {
                symbol_t *declaration = down_cast<Variable_t>(symbol)->m_type_declaration;
                declaration = declaration ? ASRUtils::symbol_get_past_external(declaration)
                    : nullptr;
                if (!declaration || !is_a<Function_t>(*declaration) ||
                        !interface_is_pure(*down_cast<Function_t>(declaration))) {
                    call.unknown = true;
                }
                break;
            }
            case symbolType::GenericProcedure: {
                auto *generic = down_cast<GenericProcedure_t>(symbol);
                for (size_t i = 0; i < generic->n_procs; i++) {
                    add_procedure(generic->m_procs[i], dt, call);
                }
                break;
            }
            default:
                call.unknown = true;
        }
    }

    // A call through a polymorphic object may reach any override of the
    // binding. Extensions of a type declared in a module may be compiled
    // separately, but an override of a pure binding is pure (F2018 7.5.7.3).
    void add_overrides(StructMethodDeclaration_t &binding, Call &call) {
        if (!extensions_built) {
            for (SymbolTable *scope = unit_scope; scope; scope = scope->parent) {
                visit_trait_lifecycle_scopes(scope, true, [&](symbol_t *symbol) {
                    if (!is_a<Struct_t>(*symbol)) return;
                    auto *type = down_cast<Struct_t>(symbol);
                    if (!type->m_parent) return;
                    symbol_t *parent = ASRUtils::symbol_get_past_external(type->m_parent);
                    if (is_a<Struct_t>(*parent)) {
                        extensions[down_cast<Struct_t>(parent)].push_back(type);
                    }
                });
            }
            extensions_built = true;
        }
        symbol_t *owner = ASRUtils::get_asr_owner(&binding.base);
        if (!owner || !is_a<Struct_t>(*owner)) {
            call.unknown = true;
            return;
        }
        std::vector<const Struct_t*> pending {down_cast<Struct_t>(owner)};
        while (!pending.empty()) {
            const Struct_t *type = pending.back();
            pending.pop_back();
            auto found = extensions.find(type);
            if (found == extensions.end()) continue;
            for (Struct_t *extension : found->second) {
                symbol_t *member = extension->m_symtab->get_symbol(binding.m_name);
                member = member ? ASRUtils::symbol_get_past_external(member) : nullptr;
                if (member && is_a<StructMethodDeclaration_t>(*member)) {
                    auto *override_binding = down_cast<StructMethodDeclaration_t>(member);
                    add_procedure(override_binding->m_proc, nullptr, call);
                    if (override_binding->m_dispatch_proc) {
                        add_procedure(override_binding->m_dispatch_proc, nullptr, call);
                    }
                }
                pending.push_back(extension);
            }
        }
        symbol_t *scope_owner = ASRUtils::get_asr_owner(owner);
        symbol_t *procedure = ASRUtils::symbol_get_past_external(binding.m_proc);
        if (scope_owner && is_a<Module_t>(*scope_owner) &&
                !(is_a<Function_t>(*procedure) &&
                    interface_is_pure(*down_cast<Function_t>(procedure)))) {
            call.unknown = true;
        }
    }

    void discover(Function_t &root, std::vector<Function_t*> &discovered) {
        std::vector<Function_t*> pending {&root};
        while (!pending.empty()) {
            Function_t *function = pending.back();
            pending.pop_back();
            if (nodes.count(function)) continue;
            Node &node = nodes[function];
            discovered.push_back(function);
            if (has_trait_lifecycle_effects(*function)) node.own = Effect::Lifecycle;
            CallCollector collector(*this);
            collector.collect_scope(*function);
            node.calls = std::move(collector.calls);
            for (const Call &call : node.calls) {
                for (Function_t *callee : call.callees) pending.push_back(callee);
            }
        }
    }

    // Procedures discovered earlier are solved already; new ones can call
    // them, but not the other way around.
    void solve(Function_t &root) {
        if (nodes.count(&root)) return;
        std::vector<Function_t*> discovered;
        discover(root, discovered);
        bool changed = true;
        while (changed) {
            changed = false;
            for (Function_t *function : discovered) {
                Node &node = nodes.at(function);
                Effect effect = node.own;
                for (const Call &call : node.calls) {
                    if (call.unknown && effect == Effect::None) effect = Effect::Unknown;
                    for (Function_t *callee : call.callees) {
                        Effect reached = nodes.at(callee).effect;
                        if (reached > effect) effect = reached;
                    }
                }
                if (effect != node.effect) {
                    node.effect = effect;
                    changed = true;
                }
            }
        }
    }
};

} // namespace ASR

} // namespace LCompilers

#endif // LIBASR_ASR_SIDE_EFFECT_H
