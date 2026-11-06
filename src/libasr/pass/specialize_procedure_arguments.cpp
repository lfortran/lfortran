#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/specialize_procedure_arguments.h>
#include <libasr/pass/procedure_argument_utils.h>
#include <libasr/pass/pass_utils.h>
#include <map>
#include <set>
#include <tuple>
#include <vector>

namespace LCompilers {

/*
Procedure arguments specialized in the host
===========================================

With --fast, a call that passes an internal procedure P of a host H to a
procedure G of this translation unit becomes a call of a copy of G, which is
an internal procedure of H that calls P directly instead of its dummy
procedure:

    subroutine h(c)                             ! after the pass
        real, intent(in) :: c
        real :: r
        r = integrate__inner(0.0, 1.0)          ! was integrate(inner, 0.0, 1.0)
    contains
        real function inner(x)
            real, intent(in) :: x
            inner = c*x
        end function
        real function integrate__inner(a, b) result(s)
            real, intent(in) :: a, b
            ...
            s = s + inner(x)                    ! was f(x), f the dummy
            ...
        end function
    end subroutine

nested_vars, which runs next, then sees an ordinary call of an internal
procedure, which passes the frame, instead of a procedure value of P, which
needs a thunk and a frame slot (see nested_vars.cpp): a recursive host is no
longer limited by --internal-procedure-thunks, and the call of P is direct, so
LLVM can inline it. G itself is not changed: its other callers and its
interface are as before.

A dummy procedure of G is removed only if all of these hold:

* G has a body in this translation unit (a procedure of a module that is not
  loaded from a module file, of a program, or an internal procedure of H or
  of a host of H), is called by its name (not through a binding of a
  derived type), and has the default ABI. A procedure whose body is
  elsewhere, called through an interface, is left alone.
* The dummy is a non-optional, non-pointer dummy procedure with an explicit
  interface, and P has the same characteristics: the same number of
  arguments, each with the same type, rank, array representation, intent and
  attributes, and the same result.
* In G, the dummy is only called, or passed on as an actual argument to a
  dummy of a procedure that satisfies these conditions for that dummy
  (itself, for a recursive G such as adaptive quadrature, or another one):
  not assigned to a pointer, stored, given to `c_funloc`, `associated` or
  `present`, or passed to a procedure without a body or with an implicit
  interface. This is decided for all dummies at once, as the largest set
  that is closed under passing on.
* G has no saved variables (SAVE, or initialized in its declaration, which
  implies SAVE; a copy would have its own), no common blocks, no internal
  procedures, no derived types, generic interfaces or namelists of its own,
  and is not elemental. The frontend lowers ENTRY statements to ordinary
  procedures (a procedure with the body, called by one per entry point),
  which are treated like any other.
* G is not too large, and H has not reached its number of copies. There is
  one copy per G, H and actual procedures, shared by every such call in H
  and in the procedures nested in it.

Otherwise the call is left as it is, and nested_vars makes a procedure value
as before. Several dummies of one call are removed together when their
actuals are internal procedures of hosts that are all visible from the
innermost one, which is then H.

The copy is processed like the code that was already in H: a call in it that
passes P (where G passed its dummy) is specialized in turn, so a recursive G
calls its own copy, and a G that passes the dummy to a procedure G2 calls a
copy of G2.

The copy refers to the same entities as G. Those of G's own scope are copies;
the others are reached through the scopes of H when G is a procedure of H or
of a host of H, a program or the global scope. When G is a procedure of a
module, the copy refers to the entities of that module, private ones
included, and to those of the modules it uses through ExternalSymbols of its
own scope, as if it used them. If anything it refers to is not visible from
H after that, there is no copy.
*/

namespace {

using namespace ProcedureArguments;

// The largest number of copies in one host.
constexpr size_t max_copies_per_host = 16;

// The removable dummy procedures of the translation unit: the largest set of
// dummies of copyable procedures that are only called or passed on to
// dummies of the set.
std::set<Dummy> removable_dummies(SymbolTable *global_scope) {
    std::vector<ASR::Function_t*> procedures;
    collect_procedures(global_scope, procedures);
    std::set<Dummy> removable;
    std::map<Dummy, std::vector<Dummy>> passed_to;
    for (ASR::Function_t *f: procedures) {
        DummyUseCollector collector;
        for (size_t i = 0; i < f->n_args; i++) {
            if (removable_dummy_interface(f, i) != nullptr) {
                collector.dummies[dummy_symbol(f, i)] = i;
            }
        }
        if (collector.dummies.empty() || !copyable_procedure(f)) {
            continue;
        }
        for (auto &item: f->m_symtab->get_scope()) {
            collector.visit_symbol(*item.second);
        }
        for (size_t i = 0; i < f->n_body; i++) {
            collector.visit_stmt(*f->m_body[i]);
        }
        if (f->m_return_var != nullptr) {
            collector.visit_expr(*f->m_return_var);
        }
        for (auto &d: collector.dummies) {
            if (collector.unremovable.count(d.first) > 0 ||
                    collector.references[d.first] !=
                        collector.allowed[d.first]) {
                continue;
            }
            Dummy dummy = {f, d.second};
            removable.insert(dummy);
            passed_to[dummy] = collector.passed_to[d.first];
        }
    }
    bool changed = true;
    while (changed) {
        changed = false;
        for (auto it = removable.begin(); it != removable.end(); ) {
            bool keep = true;
            for (const Dummy &next: passed_to[*it]) {
                if (removable.count(next) == 0) {
                    keep = false;
                    break;
                }
            }
            if (keep) {
                ++it;
            } else {
                it = removable.erase(it);
                changed = true;
            }
        }
    }
    return removable;
}

// Replaces the removed dummies of a copy by the actual procedures.
class DummyReplacer: public ASR::BaseWalkVisitor<DummyReplacer> {
public:
    const std::map<ASR::symbol_t*, ASR::symbol_t*> &replacements;

    DummyReplacer(const std::map<ASR::symbol_t*, ASR::symbol_t*> &replacements_):
        replacements(replacements_) {}

    ASR::symbol_t *replace(ASR::symbol_t *sym) {
        auto it = replacements.find(sym);
        return it == replacements.end() ? sym : it->second;
    }

    void visit_Var(const ASR::Var_t &x) {
        const_cast<ASR::Var_t&>(x).m_v = replace(x.m_v);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        ASR::SubroutineCall_t &xx = const_cast<ASR::SubroutineCall_t&>(x);
        if (replacements.count(xx.m_name) > 0) {
            xx.m_name = replace(xx.m_name);
            xx.m_original_name = nullptr;
        }
        ASR::BaseWalkVisitor<DummyReplacer>::visit_SubroutineCall(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        ASR::FunctionCall_t &xx = const_cast<ASR::FunctionCall_t&>(x);
        if (replacements.count(xx.m_name) > 0) {
            xx.m_name = replace(xx.m_name);
            xx.m_original_name = nullptr;
        }
        ASR::BaseWalkVisitor<DummyReplacer>::visit_FunctionCall(x);
    }
};

// A call of a procedure with a body, and the scope it is in.
struct CallSite {
    ASR::SubroutineCall_t *subroutine_call;
    ASR::FunctionCall_t *function_call;
    SymbolTable *scope;
};

class CallSiteCollector: public ASR::BaseWalkVisitor<CallSiteCollector> {
public:
    SymbolTable *current_scope = nullptr;
    std::vector<CallSite> sites;

    bool passes_internal_procedure(ASR::call_arg_t *args, size_t n_args) {
        for (size_t i = 0; i < n_args; i++) {
            ASR::expr_t *arg = args[i].m_value;
            if (arg != nullptr && ASR::is_a<ASR::Var_t>(*arg) &&
                    is_internal_procedure(ASR::down_cast<ASR::Var_t>(arg)->m_v)) {
                return true;
            }
        }
        return false;
    }

    void visit_Program(const ASR::Program_t &x) {
        SymbolTable *parent = current_scope;
        current_scope = x.m_symtab;
        ASR::BaseWalkVisitor<CallSiteCollector>::visit_Program(x);
        current_scope = parent;
    }

    void visit_Function(const ASR::Function_t &x) {
        SymbolTable *parent = current_scope;
        current_scope = x.m_symtab;
        ASR::BaseWalkVisitor<CallSiteCollector>::visit_Function(x);
        current_scope = parent;
    }

    void visit_Block(const ASR::Block_t &x) {
        SymbolTable *parent = current_scope;
        current_scope = x.m_symtab;
        ASR::BaseWalkVisitor<CallSiteCollector>::visit_Block(x);
        current_scope = parent;
    }

    void visit_AssociateBlock(const ASR::AssociateBlock_t &x) {
        SymbolTable *parent = current_scope;
        current_scope = x.m_symtab;
        ASR::BaseWalkVisitor<CallSiteCollector>::visit_AssociateBlock(x);
        current_scope = parent;
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        if (current_scope != nullptr && x.m_dt == nullptr &&
                passes_internal_procedure(x.m_args, x.n_args)) {
            sites.push_back({const_cast<ASR::SubroutineCall_t*>(&x), nullptr,
                current_scope});
        }
        ASR::BaseWalkVisitor<CallSiteCollector>::visit_SubroutineCall(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        if (current_scope != nullptr && x.m_dt == nullptr &&
                passes_internal_procedure(x.m_args, x.n_args)) {
            sites.push_back({nullptr, const_cast<ASR::FunctionCall_t*>(&x),
                current_scope});
        }
        ASR::BaseWalkVisitor<CallSiteCollector>::visit_FunctionCall(x);
    }
};

class ProcedureArgumentSpecializer {
public:
    Allocator &al;
    std::set<Dummy> removable;
    // (procedure, host, removed dummies and their actuals) -> copy
    typedef std::tuple<ASR::Function_t*, SymbolTable*,
        std::vector<std::pair<size_t, ASR::symbol_t*>>> CopyKey;
    std::map<CopyKey, ASR::symbol_t*> copies;
    std::map<SymbolTable*, size_t> copies_per_host;
    std::vector<ASR::Function_t*> new_copies;

    ProcedureArgumentSpecializer(Allocator &al_, SymbolTable *global_scope):
        al(al_), removable(removable_dummies(global_scope)) {}

    // A copy of `g` in `host` without the dummies of `actuals`, which it
    // calls instead, or nullptr.
    ASR::symbol_t *make_copy(ASR::Function_t *g, SymbolTable *host,
            const std::vector<std::pair<size_t, ASR::symbol_t*>> &actuals) {
        SymbolTable *g_scope = g->m_symtab->parent;
        ASR::symbol_t *g_unit = scope_owner(g_scope);
        if (!in_scope_chain(host, g_scope) && (g_unit == nullptr ||
                !ASR::is_a<ASR::Module_t>(*g_unit))) {
            return nullptr;
        }
        // The copy is made in the scope of `g` and then moved into `host`:
        // the duplicator resolves the names of BLOCK and ASSOCIATE bodies
        // from the scope of the copy, and in `host` they could resolve to
        // different entities of the same names.
        ASRUtils::SymbolDuplicator duplicator(al);
        ASR::symbol_t *copy_sym = duplicator.duplicate_Function(g, g_scope);
        if (copy_sym == nullptr) {
            return nullptr;
        }
        ASR::Function_t *copy = ASR::down_cast<ASR::Function_t>(copy_sym);
        copy->m_symtab->parent = host;
        std::string name = g->m_name;
        std::map<ASR::symbol_t*, ASR::symbol_t*> replacements;
        std::set<size_t> removed;
        for (auto &a: actuals) {
            name += std::string("__") + ASRUtils::symbol_name(a.second);
            ASR::symbol_t *dummy = dummy_symbol(copy, a.first);
            if (dummy == nullptr ||
                    ASRUtils::symbol_parent_symtab(dummy) != copy->m_symtab) {
                return nullptr;
            }
            replacements[dummy] = a.second;
            removed.insert(a.first);
        }
        DummyReplacer replacer(replacements);
        replacer.visit_Function(*copy);
        Vec<ASR::expr_t*> args;
        args.reserve(al, copy->n_args);
        for (size_t i = 0; i < copy->n_args; i++) {
            if (removed.count(i) == 0) {
                args.push_back(al, copy->m_args[i]);
            }
        }
        for (auto &r: replacements) {
            copy->m_symtab->erase_symbol(ASRUtils::symbol_name(r.first));
        }
        copy->m_args = args.p;
        copy->n_args = args.size();
        copy->m_function_signature = ASRUtils::TYPE(
            ASRUtils::make_FunctionType_t_util(al, copy->base.base.loc,
                copy->m_args, copy->n_args, copy->m_return_var,
                ASRUtils::get_FunctionType(g), copy->m_symtab));
        name = host->get_unique_name(name, false);
        copy->m_name = s2c(al, name);
        ScopeImporter importer(al, copy->m_symtab, top_level_unit(host));
        importer.visit_Function(*copy);
        if (!importer.ok) {
            return nullptr;
        }
        for (const std::string &module_name: importer.modules) {
            add_module_dependency(al, host, module_name);
        }
        host->add_symbol(name, copy_sym);
        return copy_sym;
    }

    // The copy that a call of `name` with `args` in `scope` can call
    // instead, without the arguments `removed`, or nullptr.
    ASR::symbol_t *specialize(ASR::symbol_t *name, ASR::call_arg_t *args,
            size_t n_args, SymbolTable *scope, std::set<size_t> &removed) {
        ASR::Function_t *g = called_procedure(name, nullptr);
        if (g == nullptr || n_args != g->n_args) {
            return nullptr;
        }
        std::vector<std::pair<size_t, ASR::symbol_t*>> actuals;
        SymbolTable *host = nullptr;
        for (size_t i = 0; i < n_args; i++) {
            ASR::expr_t *arg = args[i].m_value;
            if (arg == nullptr || !ASR::is_a<ASR::Var_t>(*arg) ||
                    removable.count({g, i}) == 0) {
                continue;
            }
            ASR::symbol_t *p = ASR::down_cast<ASR::Var_t>(arg)->m_v;
            if (!is_internal_procedure(p) || !same_interface(
                    removable_dummy_interface(g, i),
                    ASR::down_cast<ASR::Function_t>(p))) {
                continue;
            }
            SymbolTable *p_host = ASRUtils::symbol_parent_symtab(p);
            if (host == nullptr || in_scope_chain(p_host, host)) {
                host = p_host;
            } else if (!in_scope_chain(host, p_host)) {
                return nullptr;
            }
            actuals.push_back({i, p});
        }
        if (actuals.empty() || !in_scope_chain(scope, host)) {
            return nullptr;
        }
        for (auto &a: actuals) {
            removed.insert(a.first);
        }
        CopyKey key = {g, host, actuals};
        auto it = copies.find(key);
        if (it != copies.end()) {
            return it->second;
        }
        if (copies_per_host[host] >= max_copies_per_host) {
            return nullptr;
        }
        ASR::symbol_t *copy = make_copy(g, host, actuals);
        copies[key] = copy;
        if (copy != nullptr) {
            copies_per_host[host]++;
            new_copies.push_back(ASR::down_cast<ASR::Function_t>(copy));
        }
        return copy;
    }

    void process(const CallSite &site) {
        ASR::symbol_t *name = site.subroutine_call ?
            site.subroutine_call->m_name : site.function_call->m_name;
        ASR::call_arg_t *&args = site.subroutine_call ?
            site.subroutine_call->m_args : site.function_call->m_args;
        size_t &n_args = site.subroutine_call ?
            site.subroutine_call->n_args : site.function_call->n_args;
        std::set<size_t> removed;
        ASR::symbol_t *copy = specialize(name, args, n_args, site.scope,
            removed);
        if (copy == nullptr) {
            return;
        }
        if (site.subroutine_call) {
            site.subroutine_call->m_name = copy;
            site.subroutine_call->m_original_name = nullptr;
        } else {
            site.function_call->m_name = copy;
            site.function_call->m_original_name = nullptr;
        }
        Vec<ASR::call_arg_t> new_args;
        new_args.reserve(al, n_args);
        for (size_t i = 0; i < n_args; i++) {
            if (removed.count(i) == 0) {
                new_args.push_back(al, args[i]);
            }
        }
        args = new_args.p;
        n_args = new_args.size();
    }
};

} // namespace

void pass_specialize_procedure_arguments(Allocator &al,
        ASR::TranslationUnit_t &unit, const PassOptions &pass_options) {
    if (!pass_options.fast) {
        return;
    }
    ProcedureArgumentSpecializer specializer(al, unit.m_symtab);
    if (specializer.removable.empty()) {
        return;
    }
    CallSiteCollector collector;
    collector.visit_TranslationUnit(unit);
    std::vector<CallSite> sites = collector.sites;
    bool changed = false;
    while (!sites.empty()) {
        for (const CallSite &site: sites) {
            specializer.process(site);
        }
        sites.clear();
        for (ASR::Function_t *copy: specializer.new_copies) {
            changed = true;
            CallSiteCollector copy_collector;
            copy_collector.visit_Function(*copy);
            sites.insert(sites.end(), copy_collector.sites.begin(),
                copy_collector.sites.end());
        }
        specializer.new_copies.clear();
    }
    if (changed) {
        PassUtils::UpdateDependenciesVisitor dependencies(al);
        dependencies.visit_TranslationUnit(unit);
    }
}

} // namespace LCompilers