#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/specialize_procedure_arguments.h>
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

// The largest number of ASR statements and expressions of a procedure that
// is copied, and of copies in one host.
constexpr size_t max_copied_nodes = 4000;
constexpr size_t max_copies_per_host = 16;

ASR::symbol_t *scope_owner(SymbolTable *scope) {
    if (scope != nullptr && scope->asr_owner != nullptr &&
            ASR::is_a<ASR::symbol_t>(*scope->asr_owner)) {
        return ASR::down_cast<ASR::symbol_t>(scope->asr_owner);
    }
    return nullptr;
}

bool in_scope_chain(SymbolTable *scope, const SymbolTable *s) {
    for (; scope != nullptr; scope = scope->parent) {
        if (scope == s) {
            return true;
        }
    }
    return false;
}

// `sym` is visible from `scope` the way asr_verify requires it: it is the
// symbol of its name in a scope that contains `scope`.
bool visible_from(SymbolTable *scope, ASR::symbol_t *sym) {
    SymbolTable *parent = ASRUtils::symbol_parent_symtab(sym);
    return in_scope_chain(scope, parent) &&
        parent->get_symbol(ASRUtils::symbol_name(sym)) == sym;
}

// An internal procedure: a procedure with a body, declared in a procedure or
// main program.
bool is_internal_procedure(ASR::symbol_t *sym) {
    if (!ASR::is_a<ASR::Function_t>(*sym)) {
        return false;
    }
    ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(sym);
    ASR::symbol_t *owner = scope_owner(f->m_symtab->parent);
    return ASRUtils::get_FunctionType(f)->m_deftype ==
            ASR::deftypeType::Implementation && owner != nullptr &&
        (ASR::is_a<ASR::Function_t>(*owner) ||
         ASR::is_a<ASR::Program_t>(*owner));
}

// The module or program at the top of the scopes that contain `scope`, or
// nullptr in the global scope.
ASR::symbol_t *top_level_unit(SymbolTable *scope) {
    ASR::symbol_t *unit = nullptr;
    for (; scope != nullptr; scope = scope->parent) {
        ASR::symbol_t *owner = scope_owner(scope);
        if (owner != nullptr) {
            unit = owner;
        }
    }
    return unit;
}

// The symbol of a dummy argument, or nullptr.
ASR::symbol_t *dummy_symbol(ASR::Function_t *f, size_t i) {
    if (f->m_args[i] == nullptr || !ASR::is_a<ASR::Var_t>(*f->m_args[i])) {
        return nullptr;
    }
    return ASR::down_cast<ASR::Var_t>(f->m_args[i])->m_v;
}

// The explicit interface of dummy argument `i` of `f` if it is a dummy
// procedure that can be removed: neither optional nor a pointer. It is either
// declared by an interface body (a Function symbol) or with a procedure
// statement (a Variable of procedure type declared by its interface).
ASR::Function_t *removable_dummy_interface(ASR::Function_t *f, size_t i) {
    ASR::symbol_t *sym = dummy_symbol(f, i);
    if (sym == nullptr) {
        return nullptr;
    }
    ASR::Function_t *iface = nullptr;
    if (ASR::is_a<ASR::Function_t>(*sym)) {
        iface = ASR::down_cast<ASR::Function_t>(sym);
    } else if (ASR::is_a<ASR::Variable_t>(*sym)) {
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
        if (!ASR::is_a<ASR::FunctionType_t>(*v->m_type) ||
                v->m_presence != ASR::presenceType::Required ||
                v->m_type_declaration == nullptr) {
            return nullptr;
        }
        ASR::symbol_t *decl = ASRUtils::symbol_get_past_external(
            v->m_type_declaration);
        if (!ASR::is_a<ASR::Function_t>(*decl)) {
            return nullptr;
        }
        iface = ASR::down_cast<ASR::Function_t>(decl);
    } else {
        return nullptr;
    }
    ASR::FunctionType_t *ft = ASRUtils::get_FunctionType(iface);
    if (ft->m_deftype == ASR::deftypeType::ImplicitInterface ||
            ft->m_abi != ASR::abiType::Source || ft->m_elemental) {
        return nullptr;
    }
    return iface;
}

// A dummy argument `a` of the interface of a dummy procedure and the
// corresponding dummy argument `b` of an actual procedure are passed the
// same way.
bool same_dummy(ASR::expr_t *a, ASR::expr_t *b) {
    if (a == nullptr || b == nullptr || !ASR::is_a<ASR::Var_t>(*a) ||
            !ASR::is_a<ASR::Var_t>(*b)) {
        return false;
    }
    ASR::symbol_t *sa = ASR::down_cast<ASR::Var_t>(a)->m_v;
    ASR::symbol_t *sb = ASR::down_cast<ASR::Var_t>(b)->m_v;
    if (!ASR::is_a<ASR::Variable_t>(*sa) || !ASR::is_a<ASR::Variable_t>(*sb)) {
        return false;
    }
    ASR::Variable_t *va = ASR::down_cast<ASR::Variable_t>(sa);
    ASR::Variable_t *vb = ASR::down_cast<ASR::Variable_t>(sb);
    if (va->m_intent != vb->m_intent || va->m_presence != vb->m_presence ||
            va->m_value_attr != vb->m_value_attr ||
            va->m_target_attr != vb->m_target_attr ||
            va->m_contiguous_attr != vb->m_contiguous_attr) {
        return false;
    }
    ASR::ttype_t *ta = va->m_type, *tb = vb->m_type;
    if (ta->type != tb->type || ASRUtils::is_allocatable(ta) !=
                ASRUtils::is_allocatable(tb) ||
            ASRUtils::is_pointer(ta) != ASRUtils::is_pointer(tb)) {
        return false;
    }
    ASR::ttype_t *ea = ASRUtils::type_get_past_allocatable_pointer(ta);
    ASR::ttype_t *eb = ASRUtils::type_get_past_allocatable_pointer(tb);
    if (ea->type != eb->type || ASRUtils::is_array(ea) != ASRUtils::is_array(eb)) {
        return false;
    }
    if (ASRUtils::is_array(ea)) {
        if (ASRUtils::extract_n_dims_from_ttype(ea) !=
                    ASRUtils::extract_n_dims_from_ttype(eb) ||
                ASRUtils::extract_physical_type(ea) !=
                    ASRUtils::extract_physical_type(eb)) {
            return false;
        }
    }
    ASR::ttype_t *xa = ASRUtils::type_get_past_array(ea);
    ASR::ttype_t *xb = ASRUtils::type_get_past_array(eb);
    if (xa->type != xb->type) {
        return false;
    }
    switch (xa->type) {
        case ASR::ttypeType::Integer:
        case ASR::ttypeType::UnsignedInteger:
        case ASR::ttypeType::Real:
        case ASR::ttypeType::Complex:
        case ASR::ttypeType::Logical:
            break;
        case ASR::ttypeType::String: {
            ASR::String_t *a_str = ASR::down_cast<ASR::String_t>(xa);
            ASR::String_t *b_str = ASR::down_cast<ASR::String_t>(xb);
            if (a_str->m_len_kind != b_str->m_len_kind ||
                    a_str->m_physical_type != b_str->m_physical_type) {
                return false;
            }
            if (a_str->m_len_kind == ASR::string_length_kindType::ExpressionLength) {
                int64_t la = -1, lb = -1;
                if (a_str->m_len == nullptr || b_str->m_len == nullptr ||
                        !ASRUtils::extract_value(ASRUtils::expr_value(a_str->m_len), la) ||
                        !ASRUtils::extract_value(ASRUtils::expr_value(b_str->m_len), lb) ||
                        la != lb) {
                    return false;
                }
            }
            break;
        }
        case ASR::ttypeType::StructType: {
            if (ASRUtils::is_class_type(xa) || ASRUtils::is_class_type(xb)) {
                return false;
            }
            break;
        }
        default:
            return false;
    }
    return ASRUtils::types_equal(ta, tb, a, b, true) &&
        ASRUtils::types_equal(tb, ta, b, a, true) &&
        ASRUtils::extract_kind_from_ttype_t(xa) ==
            ASRUtils::extract_kind_from_ttype_t(xb);
}

// The actual procedure `actual` can be called wherever a call of the dummy
// procedure with the interface `iface` was.
bool same_interface(ASR::Function_t *iface, ASR::Function_t *actual) {
    ASR::FunctionType_t *at = ASRUtils::get_FunctionType(actual);
    if (at->m_abi != ASR::abiType::Source || at->m_elemental ||
            iface->n_args != actual->n_args ||
            (iface->m_return_var == nullptr) != (actual->m_return_var == nullptr)) {
        return false;
    }
    for (size_t i = 0; i < iface->n_args; i++) {
        if (!same_dummy(iface->m_args[i], actual->m_args[i])) {
            return false;
        }
    }
    if (iface->m_return_var != nullptr) {
        ASR::Variable_t *ra = ASRUtils::EXPR2VAR(iface->m_return_var);
        ASR::Variable_t *rb = ASRUtils::EXPR2VAR(actual->m_return_var);
        if (ASRUtils::is_array(ra->m_type) || ASRUtils::is_array(rb->m_type) ||
                ASRUtils::is_allocatable_or_pointer(ra->m_type) ||
                ASRUtils::is_allocatable_or_pointer(rb->m_type)) {
            return false;
        }
        switch (ra->m_type->type) {
            case ASR::ttypeType::Integer:
            case ASR::ttypeType::UnsignedInteger:
            case ASR::ttypeType::Real:
            case ASR::ttypeType::Complex:
            case ASR::ttypeType::Logical:
                break;
            default:
                return false;
        }
        if (ra->m_type->type != rb->m_type->type ||
                ASRUtils::extract_kind_from_ttype_t(ra->m_type) !=
                    ASRUtils::extract_kind_from_ttype_t(rb->m_type)) {
            return false;
        }
    }
    return true;
}

// Counts the statements and expressions of a procedure.
class NodeCounter: public ASR::BaseWalkVisitor<NodeCounter> {
public:
    size_t count = 0;

    void visit_stmt(const ASR::stmt_t &x) {
        count++;
        ASR::BaseWalkVisitor<NodeCounter>::visit_stmt(x);
    }

    void visit_expr(const ASR::expr_t &x) {
        count++;
        ASR::BaseWalkVisitor<NodeCounter>::visit_expr(x);
    }
};

// Only variables without saved storage, interfaces, entities of modules and
// BLOCK/ASSOCIATE constructs made of those.
bool copyable_scope(SymbolTable *scope) {
    for (auto &item: scope->get_scope()) {
        ASR::symbol_t *sym = item.second;
        switch (sym->type) {
            case ASR::symbolType::Variable: {
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
                if (v->m_storage == ASR::storage_typeType::Save ||
                        v->m_storage == ASR::storage_typeType::Threadprivate ||
                        v->m_common_block != nullptr) {
                    return false;
                }
                break;
            }
            case ASR::symbolType::Function: {
                if (ASRUtils::get_FunctionType(sym)->m_deftype ==
                        ASR::deftypeType::Implementation) {
                    return false;
                }
                break;
            }
            case ASR::symbolType::ExternalSymbol:
                break;
            case ASR::symbolType::Block:
                if (!copyable_scope(ASR::down_cast<ASR::Block_t>(sym)->m_symtab)) {
                    return false;
                }
                break;
            case ASR::symbolType::AssociateBlock:
                if (!copyable_scope(
                        ASR::down_cast<ASR::AssociateBlock_t>(sym)->m_symtab)) {
                    return false;
                }
                break;
            default:
                return false;
        }
    }
    return true;
}

// A procedure that can be copied into a host, apart from its dummies.
bool copyable_procedure(ASR::Function_t *f) {
    ASR::FunctionType_t *ft = ASRUtils::get_FunctionType(f);
    if (ft->m_deftype != ASR::deftypeType::Implementation ||
            ft->m_abi != ASR::abiType::Source || ft->m_elemental ||
            ft->m_static || ft->n_restrictions > 0 || ft->m_is_restriction ||
            ft->m_exec_space != ASR::exec_spaceType::Host ||
            f->m_gpu != nullptr || f->n_body == 0) {
        return false;
    }
    ASR::symbol_t *unit = top_level_unit(f->m_symtab->parent);
    if (unit != nullptr && ASR::is_a<ASR::Module_t>(*unit)) {
        ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(unit);
        if (m->m_loaded_from_mod || m->m_intrinsic) {
            return false;
        }
    }
    if (!copyable_scope(f->m_symtab)) {
        return false;
    }
    NodeCounter counter;
    counter.visit_Function(*f);
    return counter.count <= max_copied_nodes;
}

// The procedure with a body that a call by its name runs, or nullptr.
ASR::Function_t *called_procedure(ASR::symbol_t *name, ASR::expr_t *dt) {
    if (dt != nullptr) {
        return nullptr;
    }
    ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(name);
    if (sym == nullptr || !ASR::is_a<ASR::Function_t>(*sym) ||
            ASRUtils::get_FunctionType(sym)->m_deftype !=
                ASR::deftypeType::Implementation) {
        return nullptr;
    }
    return ASR::down_cast<ASR::Function_t>(sym);
}

// A dummy procedure: argument `index` of `procedure`.
typedef std::pair<ASR::Function_t*, size_t> Dummy;

// The uses of the removable dummy procedures of one procedure: every
// reference that is not a call or an actual argument of a call of a
// procedure with a body makes a dummy unremovable.
class DummyUseCollector: public ASR::BaseWalkVisitor<DummyUseCollector> {
public:
    // dummy symbol -> its index
    std::map<ASR::symbol_t*, size_t> dummies;
    // references (Var) and the ones that are allowed
    std::map<ASR::symbol_t*, size_t> references, allowed;
    std::set<ASR::symbol_t*> unremovable;
    std::map<ASR::symbol_t*, std::vector<Dummy>> passed_to;

    void visit_Variable(const ASR::Variable_t &x) {
        if (x.m_type_declaration != nullptr &&
                dummies.count(x.m_type_declaration) > 0) {
            unremovable.insert(x.m_type_declaration);
        }
        ASR::BaseWalkVisitor<DummyUseCollector>::visit_Variable(x);
    }

    void visit_Var(const ASR::Var_t &x) {
        if (dummies.count(x.m_v) > 0) {
            references[x.m_v]++;
        }
    }

    void visit_call(ASR::symbol_t *name, ASR::symbol_t *original_name,
            ASR::call_arg_t *args, size_t n_args, ASR::expr_t *dt) {
        if (dummies.count(name) > 0 && dt != nullptr) {
            unremovable.insert(name);
        }
        if (original_name != nullptr && original_name != name &&
                dummies.count(original_name) > 0) {
            unremovable.insert(original_name);
        }
        ASR::Function_t *callee = called_procedure(name, dt);
        for (size_t i = 0; i < n_args; i++) {
            ASR::expr_t *arg = args[i].m_value;
            if (arg == nullptr || !ASR::is_a<ASR::Var_t>(*arg)) {
                continue;
            }
            ASR::symbol_t *sym = ASR::down_cast<ASR::Var_t>(arg)->m_v;
            if (dummies.count(sym) > 0 && callee != nullptr &&
                    i < callee->n_args) {
                allowed[sym]++;
                passed_to[sym].push_back({callee, i});
            }
        }
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        visit_call(x.m_name, x.m_original_name, x.m_args, x.n_args, x.m_dt);
        ASR::BaseWalkVisitor<DummyUseCollector>::visit_SubroutineCall(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        visit_call(x.m_name, x.m_original_name, x.m_args, x.n_args, x.m_dt);
        ASR::BaseWalkVisitor<DummyUseCollector>::visit_FunctionCall(x);
    }
};

// Every procedure with a body, in the order of the scopes.
void collect_procedures(SymbolTable *scope,
        std::vector<ASR::Function_t*> &procedures) {
    for (auto &item: scope->get_scope()) {
        ASR::symbol_t *sym = item.second;
        if (ASR::is_a<ASR::Function_t>(*sym)) {
            ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(sym);
            if (ASRUtils::get_FunctionType(f)->m_deftype ==
                    ASR::deftypeType::Implementation) {
                procedures.push_back(f);
            }
        }
        SymbolTable *nested = ASRUtils::symbol_symtab(sym);
        if (nested != nullptr && (ASR::is_a<ASR::Function_t>(*sym) ||
                ASR::is_a<ASR::Program_t>(*sym) ||
                ASR::is_a<ASR::Module_t>(*sym) ||
                ASR::is_a<ASR::Block_t>(*sym) ||
                ASR::is_a<ASR::AssociateBlock_t>(*sym))) {
            collect_procedures(nested, procedures);
        }
    }
}

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

// Makes every symbol that a copy in a host refers to visible from it, the
// way asr_verify requires: an entity of a module that is not visible is
// referred to through an ExternalSymbol of the copy's scope. `ok` is false if
// some entity cannot be made visible.
class ScopeImporter: public ASR::BaseWalkVisitor<ScopeImporter> {
public:
    Allocator &al;
    SymbolTable *copy_scope;
    SymbolTable *current_scope;
    bool ok = true;
    // entity -> its ExternalSymbol in copy_scope
    std::map<ASR::symbol_t*, ASR::symbol_t*> imports;
    // the modules of the imported entities
    std::set<std::string> modules;

    ScopeImporter(Allocator &al_, SymbolTable *copy_scope_):
        al(al_), copy_scope(copy_scope_), current_scope(copy_scope_) {}

    ASR::symbol_t *import(ASR::symbol_t *sym) {
        if (sym == nullptr || visible_from(current_scope, sym)) {
            return sym;
        }
        ASR::symbol_t *target = ASRUtils::symbol_get_past_external(sym);
        if (target == nullptr) {
            ok = false;
            return sym;
        }
        auto it = imports.find(target);
        if (it != imports.end()) {
            return it->second;
        }
        ASR::symbol_t *owner = scope_owner(ASRUtils::symbol_parent_symtab(target));
        if (owner == nullptr || !ASR::is_a<ASR::Module_t>(*owner)) {
            ok = false;
            return sym;
        }
        ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(owner);
        std::string name = copy_scope->get_unique_name(
            ASRUtils::symbol_name(target), false);
        ASR::symbol_t *ext = ASR::down_cast<ASR::symbol_t>(
            ASR::make_ExternalSymbol_t(al, target->base.loc, copy_scope,
                s2c(al, name), target, m->m_name, nullptr, 0,
                ASRUtils::symbol_name(target), ASR::accessType::Private));
        copy_scope->add_symbol(name, ext);
        imports[target] = ext;
        modules.insert(m->m_name);
        return ext;
    }

    void require_visible(ASR::symbol_t *sym) {
        if (sym != nullptr && !visible_from(current_scope, sym)) {
            ok = false;
        }
    }

    void visit_Function(const ASR::Function_t &x) {
        SymbolTable *parent = current_scope;
        current_scope = x.m_symtab;
        ASR::BaseWalkVisitor<ScopeImporter>::visit_Function(x);
        current_scope = parent;
    }

    void visit_Block(const ASR::Block_t &x) {
        SymbolTable *parent = current_scope;
        current_scope = x.m_symtab;
        ASR::BaseWalkVisitor<ScopeImporter>::visit_Block(x);
        current_scope = parent;
    }

    void visit_AssociateBlock(const ASR::AssociateBlock_t &x) {
        SymbolTable *parent = current_scope;
        current_scope = x.m_symtab;
        ASR::BaseWalkVisitor<ScopeImporter>::visit_AssociateBlock(x);
        current_scope = parent;
    }

    void visit_Variable(const ASR::Variable_t &x) {
        ASR::Variable_t &xx = const_cast<ASR::Variable_t&>(x);
        xx.m_type_declaration = import(xx.m_type_declaration);
        ASR::BaseWalkVisitor<ScopeImporter>::visit_Variable(x);
    }

    void visit_Var(const ASR::Var_t &x) {
        const_cast<ASR::Var_t&>(x).m_v = import(x.m_v);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        ASR::SubroutineCall_t &xx = const_cast<ASR::SubroutineCall_t&>(x);
        xx.m_name = import(xx.m_name);
        xx.m_original_name = import(xx.m_original_name);
        ASR::BaseWalkVisitor<ScopeImporter>::visit_SubroutineCall(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        ASR::FunctionCall_t &xx = const_cast<ASR::FunctionCall_t&>(x);
        xx.m_name = import(xx.m_name);
        xx.m_original_name = import(xx.m_original_name);
        ASR::BaseWalkVisitor<ScopeImporter>::visit_FunctionCall(x);
    }

    void visit_FunctionPointerCast(const ASR::FunctionPointerCast_t &x) {
        ASR::FunctionPointerCast_t &xx =
            const_cast<ASR::FunctionPointerCast_t&>(x);
        xx.m_to = import(xx.m_to);
        ASR::BaseWalkVisitor<ScopeImporter>::visit_FunctionPointerCast(x);
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        require_visible(x.m_m);
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        require_visible(x.m_m);
    }

    void visit_StringFormat(const ASR::StringFormat_t &x) {
        for (size_t i = 0; i < x.n_dtio_bindings; i++) {
            require_visible(x.m_dtio_bindings[i].m_procedure);
            require_visible(x.m_dtio_bindings[i].m_callback);
        }
        ASR::BaseWalkVisitor<ScopeImporter>::visit_StringFormat(x);
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
        ASRUtils::SymbolDuplicator duplicator(al);
        ASR::symbol_t *copy_sym = duplicator.duplicate_Function(g, host);
        if (copy_sym == nullptr) {
            return nullptr;
        }
        ASR::Function_t *copy = ASR::down_cast<ASR::Function_t>(copy_sym);
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
        ScopeImporter importer(al, copy->m_symtab);
        importer.visit_Function(*copy);
        if (!importer.ok) {
            return nullptr;
        }
        for (const std::string &module_name: importer.modules) {
            add_module_dependency(host, module_name);
        }
        host->add_symbol(name, copy_sym);
        return copy_sym;
    }

    // The module or program that contains `scope` depends on the module
    // `module_name`.
    void add_module_dependency(SymbolTable *scope,
            const std::string &module_name) {
        ASR::symbol_t *unit = top_level_unit(scope);
        if (unit == nullptr) {
            return;
        }
        char **deps;
        size_t n_deps;
        if (ASR::is_a<ASR::Module_t>(*unit)) {
            ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(unit);
            if (module_name == m->m_name) {
                return;
            }
            deps = m->m_dependencies;
            n_deps = m->n_dependencies;
        } else if (ASR::is_a<ASR::Program_t>(*unit)) {
            ASR::Program_t *p = ASR::down_cast<ASR::Program_t>(unit);
            deps = p->m_dependencies;
            n_deps = p->n_dependencies;
        } else {
            return;
        }
        for (size_t i = 0; i < n_deps; i++) {
            if (module_name == deps[i]) {
                return;
            }
        }
        Vec<char*> new_deps;
        new_deps.reserve(al, n_deps + 1);
        for (size_t i = 0; i < n_deps; i++) {
            new_deps.push_back(al, deps[i]);
        }
        new_deps.push_back(al, s2c(al, module_name));
        if (ASR::is_a<ASR::Module_t>(*unit)) {
            ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(unit);
            m->m_dependencies = new_deps.p;
            m->n_dependencies = new_deps.size();
        } else {
            ASR::Program_t *p = ASR::down_cast<ASR::Program_t>(unit);
            p->m_dependencies = new_deps.p;
            p->n_dependencies = new_deps.size();
        }
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
