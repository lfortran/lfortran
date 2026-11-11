#ifndef LIBASR_PASS_ACCESS_PATHS_H
#define LIBASR_PASS_ACCESS_PATHS_H

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/intrinsic_array_function_registry.h>

#include <map>
#include <set>
#include <string>
#include <tuple>
#include <vector>

namespace LCompilers {

/*
What a statement of a procedure may access, worked out over *access paths*: a
variable and a chain of components (`workspace%dcdw`), whatever the
subscripts. An associate name stands for its selector, and a local pointer
associated with one thing only in the procedure for that thing. Two paths
overlap when one is a prefix of the other, except that an access of only the
bytes of a derived-type value itself (a polymorphic value the host copies for
a launch) does not reach the data of its allocatable components. Asking about
the shape of an array (`size`, `lbound`, `allocated`) does not access it. A
call accesses its actual arguments -- of a derived-type value, only the
components the callee's body names, when it is known -- and may access every
variable that is not a local or a dummy argument of the procedure without the
`target` attribute (a module variable, a variable of a procedure this one is
contained in, a target, what a pointer may point to); a call to a procedure
contained in this one may access anything.

A collector given a `CallSummaries` (an opt-in of the passes that use it)
counts a call of a procedure whose body it knows, and which launches nothing,
more precisely: such a call accesses what that body accesses, with what the
body names through a dummy argument replaced by the same part of the actual
argument, and the variables of the procedures it is contained in that it
names; it may access what another procedure can reach only when the body
names such a variable or calls a procedure, and it acts outside the program
when the body may, `stop` included.
*/

namespace AccessPaths {

// A variable and a chain of components. A path with no root is whatever a
// pointer may point to: it overlaps every root a pointer may point into
// (`same_storage_root`), and another procedure may reach it
// (`reachable_elsewhere`). A path rooted at a pointer variable, whose
// target the procedure does not resolve (a module, host-associated or dummy
// pointer with no `=>` here), means the same; `may_point_anywhere` tests
// for both. A default-constructed `Path` is a pointee as well, so callers
// use a path only when `path_of` filled it.
struct Path {
    ASR::symbol_t *root = nullptr;
    std::vector<ASR::symbol_t*> members;
    // Only the bytes of the value itself, not the data of its allocatable
    // or pointer components.
    bool shallow = false;

    // Whatever a pointer may point to.
    static Path pointee() {
        return Path();
    }

    bool operator<(const Path &o) const {
        return std::tie(root, members, shallow)
            < std::tie(o.root, o.members, o.shallow);
    }
    bool operator==(const Path &o) const {
        return root == o.root && members == o.members
            && shallow == o.shallow;
    }
};

struct Accesses {
    std::vector<Path> paths;
    // May act outside the program (input/output).
    bool observable = false;
    // May access what a pointer or another procedure can reach.
    bool globals = false;
    // `globals` is from a call of a procedure contained in the one being
    // analysed, which names the variables of its host by host association.
    bool contained_call = false;
    // May access anything.
    bool everything = false;
};

inline bool is_pointer_variable(ASR::symbol_t *s) {
    return s != nullptr && ASR::is_a<ASR::Variable_t>(*s)
        && ASRUtils::is_pointer(ASR::down_cast<ASR::Variable_t>(s)->m_type);
}

// Whether a path with root `root` is whatever a pointer may point to: no
// root (`Path::pointee()`), or a pointer variable.
inline bool may_point_anywhere(ASR::symbol_t *root) {
    return root == nullptr || is_pointer_variable(root);
}

// A root a pointer may point into, or that another procedure can reach.
inline bool aliasable(ASR::symbol_t *s) {
    if (may_point_anywhere(s) || !ASR::is_a<ASR::Variable_t>(*s)) {
        return true;
    }
    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
    if (v->m_target_attr) return true;
    SymbolTable *scope = v->m_parent_symtab;
    if (scope == nullptr || scope->asr_owner == nullptr) return true;
    if (ASR::is_a<ASR::unit_t>(*scope->asr_owner)) return true;
    ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(scope->asr_owner);
    return ASR::is_a<ASR::Module_t>(*owner);
}

// The scope of the procedure (or program) that `scope` is part of, past the
// `block` and `associate` constructs it may be the scope of.
inline SymbolTable *enclosing_routine(SymbolTable *scope) {
    while (scope != nullptr && scope->asr_owner != nullptr
            && ASR::is_a<ASR::symbol_t>(*scope->asr_owner)
            && (ASR::is_a<ASR::Block_t>(*ASR::down_cast<ASR::symbol_t>(
                    scope->asr_owner))
                || ASR::is_a<ASR::AssociateBlock_t>(
                    *ASR::down_cast<ASR::symbol_t>(scope->asr_owner)))) {
        scope = scope->parent;
    }
    return scope;
}

// A root another procedure may access: one a pointer may point into, or a
// variable that is not a local or dummy argument of `routine` -- a module
// variable, or a variable of the procedure `routine` is contained in, which
// the procedures contained there access by host association.
inline bool reachable_elsewhere(ASR::symbol_t *s, SymbolTable *routine) {
    if (aliasable(s)) return true;
    return enclosing_routine(
        ASR::down_cast<ASR::Variable_t>(s)->m_parent_symtab) != routine;
}

inline bool same_storage_root(ASR::symbol_t *a, ASR::symbol_t *b) {
    if (a == b) return true;
    return (may_point_anywhere(a) && aliasable(b))
        || (may_point_anywhere(b) && aliasable(a));
}

// Whether components `from` on of `members` stay within the bytes of the
// value they are components of.
inline bool within_value(const std::vector<ASR::symbol_t*> &members,
        size_t from) {
    for (size_t i = from; i < members.size(); i++) {
        if (ASRUtils::is_allocatable_or_pointer(
                ASRUtils::symbol_type(members[i]))) {
            return false;
        }
    }
    return true;
}

inline bool overlap(const Path &a, const Path &b) {
    if (!same_storage_root(a.root, b.root)) return false;
    if (a.root != b.root) return true;
    size_t n = std::min(a.members.size(), b.members.size());
    for (size_t i = 0; i < n; i++) {
        if (a.members[i] != b.members[i]) return false;
    }
    const Path &shorter = a.members.size() <= b.members.size() ? a : b;
    const Path &longer = &shorter == &a ? b : a;
    if (!shorter.shallow) return true;
    return within_value(longer.members, shorter.members.size());
}

// What each pointer of a procedure is associated with in it: every
// `p => x`, an associate name binding included. A local pointer associated
// with one thing only is that thing wherever it is used. One associated
// with several, or with what c_f_pointer gives it, or a pointer that is not
// a local associated here, may point to anything.
struct PointerTargets {
    std::map<ASR::symbol_t*, std::vector<ASR::expr_t*>> targets;
    std::set<ASR::symbol_t*> unknown;
};

class PointerCollector : public ASR::BaseWalkVisitor<PointerCollector> {
public:
    PointerTargets &map;
    std::set<ASR::symbol_t*> blocks_seen;
    explicit PointerCollector(PointerTargets &map_) : map(map_) {}

    void visit_Associate(const ASR::Associate_t &x) {
        if (!ASR::is_a<ASR::Var_t>(*x.m_target)) return;
        ASR::symbol_t *p = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(x.m_target)->m_v);
        map.targets[p].push_back(x.m_value);
    }
    void visit_CPtrToPointer(const ASR::CPtrToPointer_t &x) {
        if (ASR::is_a<ASR::Var_t>(*x.m_ptr)) {
            map.unknown.insert(ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(x.m_ptr)->m_v));
        }
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!blocks_seen.insert(b).second) return;
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(b);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!blocks_seen.insert(b).second) return;
        ASR::AssociateBlock_t *block = ASR::down_cast<ASR::AssociateBlock_t>(
            b);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
};

// The one thing `p` is associated with, nullptr when there is none, and
// `unknown` set when it may point to anything.
inline ASR::expr_t* pointer_target(const PointerTargets &pointers,
        ASR::symbol_t *p, bool &unknown) {
    unknown = false;
    if (pointers.unknown.count(p) > 0) {
        unknown = true;
        return nullptr;
    }
    auto it = pointers.targets.find(p);
    bool local = ASR::is_a<ASR::Variable_t>(*p)
        && ASR::down_cast<ASR::Variable_t>(p)->m_intent
            == ASR::intentType::Local
        && ASR::down_cast<ASR::Variable_t>(p)->m_storage
            != ASR::storage_typeType::Save;
    if (it == pointers.targets.end()) {
        // Not associated here: only what the caller or another procedure
        // associated it with, which is a target.
        return nullptr;
    }
    if (!local || it->second.size() != 1) {
        unknown = true;
        return nullptr;
    }
    return it->second[0];
}

// The path of a designator, after pointers associated in the procedure,
// and the subscripts it evaluates on the way. False for anything else, and
// for a pointer that may point to anything.
inline bool path_of(const PointerTargets &pointers, ASR::expr_t *e, Path &path,
        std::vector<ASR::expr_t*> *subscripts, int depth = 0) {
    if (depth > 64) return false;
    switch (e->type) {
        case ASR::exprType::Var: {
            ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(e)->m_v);
            if (is_pointer_variable(sym)) {
                bool unknown = false;
                ASR::expr_t *target = pointer_target(pointers, sym, unknown);
                if (unknown) return false;
                if (target != nullptr) {
                    // The subscripts of the target were evaluated where it
                    // was associated, not here.
                    return path_of(pointers, target, path, nullptr,
                        depth + 1);
                }
            }
            path.root = sym;
            path.members.clear();
            return true;
        }
        case ASR::exprType::StructInstanceMember: {
            ASR::StructInstanceMember_t *m =
                ASR::down_cast<ASR::StructInstanceMember_t>(e);
            if (!path_of(pointers, m->m_v, path, subscripts, depth + 1)) {
                return false;
            }
            path.members.push_back(ASRUtils::symbol_get_past_external(
                m->m_m));
            return true;
        }
        case ASR::exprType::ArrayItem: {
            ASR::ArrayItem_t *a = ASR::down_cast<ASR::ArrayItem_t>(e);
            if (subscripts) {
                for (size_t i = 0; i < a->n_args; i++) {
                    if (a->m_args[i].m_left) {
                        subscripts->push_back(a->m_args[i].m_left);
                    }
                    if (a->m_args[i].m_right) {
                        subscripts->push_back(a->m_args[i].m_right);
                    }
                    if (a->m_args[i].m_step) {
                        subscripts->push_back(a->m_args[i].m_step);
                    }
                }
            }
            return path_of(pointers, a->m_v, path, subscripts, depth + 1);
        }
        case ASR::exprType::ArraySection: {
            ASR::ArraySection_t *a = ASR::down_cast<ASR::ArraySection_t>(e);
            if (subscripts) {
                for (size_t i = 0; i < a->n_args; i++) {
                    if (a->m_args[i].m_left) {
                        subscripts->push_back(a->m_args[i].m_left);
                    }
                    if (a->m_args[i].m_right) {
                        subscripts->push_back(a->m_args[i].m_right);
                    }
                    if (a->m_args[i].m_step) {
                        subscripts->push_back(a->m_args[i].m_step);
                    }
                }
            }
            return path_of(pointers, a->m_v, path, subscripts, depth + 1);
        }
        case ASR::exprType::ArrayPhysicalCast:
            return path_of(pointers,
                ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg, path,
                subscripts, depth + 1);
        default:
            return false;
    }
}

// The variable a designator names part of, past its subscripts and
// components; nullptr for anything else.
inline ASR::symbol_t *designator_base(ASR::expr_t *e) {
    while (true) {
        switch (e->type) {
            case ASR::exprType::Var:
                return ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(e)->m_v);
            case ASR::exprType::StructInstanceMember:
                e = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v;
                break;
            case ASR::exprType::ArrayItem:
                e = ASR::down_cast<ASR::ArrayItem_t>(e)->m_v;
                break;
            case ASR::exprType::ArraySection:
                e = ASR::down_cast<ASR::ArraySection_t>(e)->m_v;
                break;
            case ASR::exprType::ArrayPhysicalCast:
                e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
                break;
            default:
                return nullptr;
        }
    }
}

// Whether a value of the type of `e` may hold a pointer, through which a
// procedure handed it as an intent(in) argument could still change storage.
inline bool may_hold_pointer(ASR::expr_t *e) {
    ASR::ttype_t *type = ASRUtils::expr_type(e);
    if (ASRUtils::is_pointer(type)) return true;
    ASR::ttype_t *t = ASRUtils::extract_type(type);
    if (ASR::is_a<ASR::Integer_t>(*t) || ASR::is_a<ASR::UnsignedInteger_t>(*t)
            || ASR::is_a<ASR::Real_t>(*t) || ASR::is_a<ASR::Complex_t>(*t)
            || ASR::is_a<ASR::Logical_t>(*t) || ASR::is_a<ASR::String_t>(*t)) {
        return false;
    }
    if (!ASR::is_a<ASR::StructType_t>(*t)
            || ASR::down_cast<ASR::StructType_t>(t)
                ->m_is_unlimited_polymorphic) {
        return true;
    }
    ASR::symbol_t *declaration = ASRUtils::get_struct_sym_from_struct_expr(e);
    if (declaration == nullptr) return true;
    // The components, and those of the derived-type components, to a depth.
    std::vector<std::pair<ASR::symbol_t*, int>> todo{{declaration, 0}};
    while (!todo.empty()) {
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
            todo.back().first);
        int depth = todo.back().second;
        todo.pop_back();
        if (depth > 8 || !ASR::is_a<ASR::Struct_t>(*sym)) return true;
        ASR::Struct_t *st = ASR::down_cast<ASR::Struct_t>(sym);
        if (st->m_parent) todo.push_back({st->m_parent, depth + 1});
        for (auto &item : st->m_symtab->get_scope()) {
            if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(item.second);
            if (ASRUtils::is_pointer(v->m_type)) return true;
            ASR::ttype_t *m = ASRUtils::extract_type(v->m_type);
            if (ASR::is_a<ASR::StructType_t>(*m)) {
                if (v->m_type_declaration == nullptr) return true;
                todo.push_back({v->m_type_declaration, depth + 1});
            } else if (!ASR::is_a<ASR::Integer_t>(*m)
                    && !ASR::is_a<ASR::UnsignedInteger_t>(*m)
                    && !ASR::is_a<ASR::Real_t>(*m)
                    && !ASR::is_a<ASR::Complex_t>(*m)
                    && !ASR::is_a<ASR::Logical_t>(*m)
                    && !ASR::is_a<ASR::String_t>(*m)) {
                return true;
            }
        }
    }
    return false;
}

// The paths an array designator may name through a local pointer that the
// procedure associates with several things, each of which `path_of`
// follows (`p => x` in one branch, `p => y` in the other, as the lowering of
// an optional argument does): one path for each, and the subscripts it
// evaluates. False for any other designator, and for a pointer that may
// point to anything else.
inline bool alternative_paths(const PointerTargets &pointers, ASR::expr_t *e,
        std::vector<Path> &paths, std::vector<ASR::expr_t*> *subscripts) {
    while (true) {
        if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
            e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
            continue;
        }
        ASR::array_index_t *args = nullptr;
        size_t n_args = 0;
        if (ASR::is_a<ASR::ArraySection_t>(*e)) {
            ASR::ArraySection_t *a = ASR::down_cast<ASR::ArraySection_t>(e);
            args = a->m_args;
            n_args = a->n_args;
            e = a->m_v;
        } else if (ASR::is_a<ASR::ArrayItem_t>(*e)) {
            ASR::ArrayItem_t *a = ASR::down_cast<ASR::ArrayItem_t>(e);
            args = a->m_args;
            n_args = a->n_args;
            e = a->m_v;
        } else {
            break;
        }
        for (size_t i = 0; subscripts && i < n_args; i++) {
            if (args[i].m_left) subscripts->push_back(args[i].m_left);
            if (args[i].m_right) subscripts->push_back(args[i].m_right);
            if (args[i].m_step) subscripts->push_back(args[i].m_step);
        }
    }
    if (!ASR::is_a<ASR::Var_t>(*e)) return false;
    ASR::symbol_t *p = ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(e)->m_v);
    if (!is_pointer_variable(p) || pointers.unknown.count(p) > 0) {
        return false;
    }
    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(p);
    auto it = pointers.targets.find(p);
    if (v->m_intent != ASR::intentType::Local
            || v->m_storage == ASR::storage_typeType::Save
            || it == pointers.targets.end() || it->second.size() < 2) {
        return false;
    }
    std::vector<Path> found;
    for (ASR::expr_t *target : it->second) {
        Path path;
        // The subscripts of a target were evaluated where it was
        // associated, not here.
        if (!path_of(pointers, target, path, nullptr)) return false;
        found.push_back(path);
    }
    paths.insert(paths.end(), found.begin(), found.end());
    return true;
}

// Whether `fn` is contained in the procedure whose scope is `routine`, and
// so may access its variables by host association.
inline bool contained_in(ASR::Function_t *fn, SymbolTable *routine) {
    for (SymbolTable *s = fn->m_symtab->parent; s != nullptr;
            s = s->parent) {
        if (s == routine) return true;
    }
    return false;
}

inline bool acts_outside(ASR::Function_t *fn,
    std::map<ASR::Function_t*, bool> &memo);

// Whether a statement of a procedure body may act outside the program: a
// statement other than those below (input/output, `stop`), or a call that
// may.
class OutsideActions : public ASR::BaseWalkVisitor<OutsideActions> {
public:
    std::map<ASR::Function_t*, bool> &memo;
    bool found = false;
    std::set<ASR::symbol_t*> blocks;
    explicit OutsideActions(std::map<ASR::Function_t*, bool> &memo_)
        : memo(memo_) {}

    void visit_stmt(const ASR::stmt_t &x) {
        if (found) return;
        switch (x.type) {
            case ASR::stmtType::Allocate:
            case ASR::stmtType::ReAlloc:
            case ASR::stmtType::Assignment:
            case ASR::stmtType::DebugCheckArrayBounds:
            case ASR::stmtType::Associate:
            case ASR::stmtType::Cycle:
            case ASR::stmtType::Exit:
            case ASR::stmtType::ExplicitDeallocate:
            case ASR::stmtType::ImplicitDeallocate:
            case ASR::stmtType::DoLoop:
            case ASR::stmtType::DoConcurrentLoop:
            case ASR::stmtType::WhileLoop:
            case ASR::stmtType::If:
            case ASR::stmtType::Select:
            case ASR::stmtType::Return:
            case ASR::stmtType::Nullify:
            case ASR::stmtType::SubroutineCall:
            case ASR::stmtType::Expr:
            case ASR::stmtType::BlockCall:
            case ASR::stmtType::AssociateBlockCall:
                BaseWalkVisitor::visit_stmt(x);
                return;
            default:
                found = true;
                return;
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
    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!blocks.insert(b).second) return;
        ASR::AssociateBlock_t *block = ASR::down_cast<ASR::AssociateBlock_t>(
            b);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
    void callee(ASR::symbol_t *name) {
        ASR::symbol_t *c = ASRUtils::symbol_get_past_external(name);
        if (ASR::is_a<ASR::StructMethodDeclaration_t>(*c)) {
            c = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::StructMethodDeclaration_t>(c)->m_proc);
        }
        if (!ASR::is_a<ASR::Function_t>(*c)
                || acts_outside(ASR::down_cast<ASR::Function_t>(c), memo)) {
            found = true;
        }
    }
    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        callee(x.m_name);
        BaseWalkVisitor::visit_SubroutineCall(x);
    }
    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        callee(x.m_name);
        BaseWalkVisitor::visit_FunctionCall(x);
    }
};

// Whether a call to `fn` may act outside the program. A pure procedure
// cannot do input/output, and neither can one whose body has none, nor a
// `stop`, nor a call that may. A procedure without a body may.
inline bool acts_outside(ASR::Function_t *fn,
        std::map<ASR::Function_t*, bool> &memo) {
    ASR::FunctionType_t *type = ASRUtils::get_FunctionType(fn);
    if (type->m_pure || type->m_elemental) return false;
    auto it = memo.find(fn);
    if (it != memo.end()) return it->second;
    if (type->m_deftype != ASR::deftypeType::Implementation) {
        memo[fn] = true;
        return true;
    }
    // A procedure that calls itself does not act outside by doing so.
    memo[fn] = false;
    OutsideActions v(memo);
    for (size_t i = 0; i < fn->n_body && !v.found; i++) {
        v.visit_stmt(*fn->m_body[i]);
    }
    memo[fn] = v.found;
    return v.found;
}

// The parts of a derived-type dummy argument a procedure's body accesses:
// the chains of components it names the dummy through, or the whole of it
// when it uses the dummy itself (passes it on, assigns it).
struct DummyUse {
    bool whole = false;
    std::vector<std::vector<ASR::symbol_t*>> chains;
};

class DummyUses : public ASR::BaseWalkVisitor<DummyUses> {
public:
    ASR::symbol_t *dummy;
    DummyUse &use;
    std::set<ASR::symbol_t*> blocks;
    DummyUses(ASR::symbol_t *dummy_, DummyUse &use_)
        : dummy(dummy_), use(use_) {}

    void visit_Var(const ASR::Var_t &x) {
        if (ASRUtils::symbol_get_past_external(x.m_v) == dummy) {
            use.whole = true;
        }
    }
    void visit_StructInstanceMember(const ASR::StructInstanceMember_t &x) {
        std::vector<ASR::symbol_t*> chain;
        const ASR::expr_t *e = &x.base;
        while (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
            const ASR::StructInstanceMember_t *m =
                ASR::down_cast<ASR::StructInstanceMember_t>(e);
            chain.insert(chain.begin(),
                ASRUtils::symbol_get_past_external(m->m_m));
            e = m->m_v;
        }
        if (ASR::is_a<ASR::Var_t>(*e) && ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(e)->m_v) == dummy) {
            use.chains.push_back(chain);
            return;
        }
        BaseWalkVisitor::visit_StructInstanceMember(x);
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!blocks.insert(b).second) return;
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(b);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!blocks.insert(b).second) return;
        ASR::AssociateBlock_t *block = ASR::down_cast<ASR::AssociateBlock_t>(
            b);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
};

// What the body of `fn` accesses through its dummy argument `i`, a scalar of
// a derived type: the whole of it unless the body is known and names it
// only through components.
inline DummyUse dummy_use(ASR::Function_t *fn, size_t i) {
    DummyUse use;
    use.whole = true;
    if (ASRUtils::get_FunctionType(fn)->m_deftype
            != ASR::deftypeType::Implementation
            || i >= fn->n_args || !ASR::is_a<ASR::Var_t>(*fn->m_args[i])) {
        return use;
    }
    ASR::symbol_t *dummy = ASR::down_cast<ASR::Var_t>(fn->m_args[i])->m_v;
    if (!ASR::is_a<ASR::Variable_t>(*dummy)) return use;
    ASR::ttype_t *type = ASRUtils::symbol_type(dummy);
    if (ASRUtils::is_array(type) || ASRUtils::is_pointer(type)
            || !ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(type))) {
        return use;
    }
    use.whole = false;
    DummyUses v(dummy, use);
    for (size_t k = 0; k < fn->n_body && !use.whole; k++) {
        v.visit_stmt(*fn->m_body[k]);
    }
    return use;
}

class CallSummaries;

class AccessCollector : public ASR::BaseWalkVisitor<AccessCollector> {
public:
    Accesses &acc;
    SymbolTable *routine;
    const PointerTargets &pointers;
    std::map<ASR::Function_t*, bool> &outside;
    // When set, a call to a procedure contained in `routine` goes here and
    // counts as accessing what another procedure can reach, instead of
    // anything: the caller checks which variables of `routine` it names.
    std::vector<ASR::Function_t*> *contained = nullptr;
    // What the calls of procedures with a known body access, or nullptr to
    // count every call as accessing all its actual arguments.
    CallSummaries *summaries;

    AccessCollector(Accesses &acc_, SymbolTable *routine_,
            const PointerTargets &pointers_,
            std::map<ASR::Function_t*, bool> &outside_,
            CallSummaries *summaries_ = nullptr)
        : acc(acc_), routine(routine_), pointers(pointers_),
          outside(outside_), summaries(summaries_) {}

    void designator(ASR::expr_t *e) {
        Path p;
        std::vector<ASR::expr_t*> subscripts;
        if (path_of(pointers, e, p, &subscripts)) {
            acc.paths.push_back(p);
            for (ASR::expr_t *s : subscripts) visit_expr(*s);
            if (is_pointer_variable(p.root)) acc.globals = true;
        } else {
            acc.everything = true;
        }
    }

    void visit_Var(const ASR::Var_t &x) {
        designator(const_cast<ASR::expr_t*>(&x.base));
    }
    void visit_StructInstanceMember(const ASR::StructInstanceMember_t &x) {
        designator(const_cast<ASR::expr_t*>(&x.base));
    }
    void visit_ArrayItem(const ASR::ArrayItem_t &x) {
        designator(const_cast<ASR::expr_t*>(&x.base));
    }
    void visit_ArraySection(const ASR::ArraySection_t &x) {
        designator(const_cast<ASR::expr_t*>(&x.base));
    }

    // The shape of an array, and whether it is allocated or associated, are
    // not its values. Asking about the shape of a designator evaluates only
    // its subscripts: `shape(t(:))` does not access `t`, but
    // `size(t(t(1):))` reads `t(1)`.
    void inquiry(ASR::expr_t *e) {
        switch (e->type) {
            case ASR::exprType::Var:
                return;
            case ASR::exprType::StructInstanceMember:
                inquiry(ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v);
                return;
            case ASR::exprType::ArrayItem: {
                ASR::ArrayItem_t *a = ASR::down_cast<ASR::ArrayItem_t>(e);
                subscripts(a->m_args, a->n_args);
                inquiry(a->m_v);
                return;
            }
            case ASR::exprType::ArraySection: {
                ASR::ArraySection_t *a =
                    ASR::down_cast<ASR::ArraySection_t>(e);
                subscripts(a->m_args, a->n_args);
                inquiry(a->m_v);
                return;
            }
            case ASR::exprType::ArrayPhysicalCast:
                inquiry(ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg);
                return;
            default:
                visit_expr(*e);
        }
    }
    void subscripts(ASR::array_index_t *args, size_t n_args) {
        for (size_t i = 0; i < n_args; i++) {
            if (args[i].m_left) visit_expr(*args[i].m_left);
            if (args[i].m_right) visit_expr(*args[i].m_right);
            if (args[i].m_step) visit_expr(*args[i].m_step);
        }
    }
    void visit_ArraySize(const ASR::ArraySize_t &x) {
        inquiry(x.m_v);
        if (x.m_dim) visit_expr(*x.m_dim);
    }
    void visit_ArrayBound(const ASR::ArrayBound_t &x) {
        inquiry(x.m_v);
        if (x.m_dim) visit_expr(*x.m_dim);
    }
    void visit_ArrayRank(const ASR::ArrayRank_t &/*x*/) {}
    void visit_ArrayIsContiguous(const ASR::ArrayIsContiguous_t &x) {
        inquiry(x.m_array);
    }
    void visit_IntrinsicArrayFunction(const ASR::IntrinsicArrayFunction_t &x) {
        if (x.m_arr_intrinsic_id == static_cast<int64_t>(
                ASRUtils::IntrinsicArrayFunctions::Shape) &&
                x.n_args == 1) {
            inquiry(x.m_args[0]);
            return;
        }
        BaseWalkVisitor::visit_IntrinsicArrayFunction(x);
    }
    void visit_PointerAssociated(const ASR::PointerAssociated_t &/*x*/) {}
    void visit_IntrinsicImpureFunction(
            const ASR::IntrinsicImpureFunction_t &x) {
        if (x.m_impure_intrinsic_id == static_cast<int64_t>(
                ASRUtils::IntrinsicImpureFunctions::Allocated)) {
            return;
        }
        BaseWalkVisitor::visit_IntrinsicImpureFunction(x);
    }

    void call(ASR::symbol_t *name, ASR::call_arg_t *args, size_t n_args,
            ASR::expr_t *dt) {
        ASR::symbol_t *callee = ASRUtils::symbol_get_past_external(name);
        ASR::Function_t *plain = dt == nullptr
            && ASR::is_a<ASR::Function_t>(*callee)
            ? ASR::down_cast<ASR::Function_t>(callee) : nullptr;
        if (plain != nullptr && n_args == plain->n_args
                && summarized_call(plain, args, n_args)) {
            return;
        }
        for (size_t i = 0; i < n_args; i++) {
            if (!args[i].m_value) continue;
            // A derived-type value handed to a procedure that names only
            // some of its components accesses only those.
            Path p;
            std::vector<ASR::expr_t*> subscripts;
            if (plain != nullptr && n_args == plain->n_args
                    && path_of(pointers, args[i].m_value, p, &subscripts)
                    && !is_pointer_variable(p.root)) {
                DummyUse use = dummy_use(plain, i);
                if (!use.whole) {
                    for (const auto &chain : use.chains) {
                        Path q = p;
                        q.members.insert(q.members.end(), chain.begin(),
                            chain.end());
                        acc.paths.push_back(q);
                    }
                    for (ASR::expr_t *sub : subscripts) visit_expr(*sub);
                    continue;
                }
            }
            visit_expr(*args[i].m_value);
        }
        if (dt) visit_expr(*dt);
        // A type-bound procedure is the procedure its binding names: one
        // that overrides it is pure when it is, and may access what it may.
        if (ASR::is_a<ASR::StructMethodDeclaration_t>(*callee)) {
            callee = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::StructMethodDeclaration_t>(callee)
                    ->m_proc);
        }
        if (!ASR::is_a<ASR::Function_t>(*callee)) {
            acc.observable = true;
            acc.everything = true;
            return;
        }
        ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(callee);
        if (acts_outside(fn, outside)) acc.observable = true;
        if (contained_in(fn, routine)) {
            if (contained != nullptr) {
                contained->push_back(fn);
                acc.globals = true;
                acc.contained_call = true;
            } else {
                acc.everything = true;
            }
        } else {
            acc.globals = true;
        }
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        call(x.m_name, x.m_args, x.n_args, x.m_dt);
    }
    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        call(x.m_name, x.m_args, x.n_args, x.m_dt);
    }

    // Counts a call of `fn` from what its body accesses (CallSummaries);
    // false when that is not known.
    inline bool summarized_call(ASR::Function_t *fn, ASR::call_arg_t *args,
        size_t n_args);

    // A namelist input/output statement accesses every variable of the
    // group, which it does not name.
    void namelist(ASR::symbol_t *nml) {
        if (nml == nullptr) return;
        nml = ASRUtils::symbol_get_past_external(nml);
        if (!ASR::is_a<ASR::Namelist_t>(*nml)) {
            acc.everything = true;
            return;
        }
        ASR::Namelist_t *group = ASR::down_cast<ASR::Namelist_t>(nml);
        for (size_t i = 0; i < group->n_var_list; i++) {
            ASR::symbol_t *v = ASRUtils::symbol_get_past_external(
                group->m_var_list[i]);
            Path p;
            p.root = v;
            acc.paths.push_back(p);
            if (is_pointer_variable(v)) acc.globals = true;
        }
    }
    void visit_FileRead(const ASR::FileRead_t &x) {
        namelist(x.m_nml);
        BaseWalkVisitor::visit_FileRead(x);
    }
    void visit_FileWrite(const ASR::FileWrite_t &x) {
        namelist(x.m_nml);
        BaseWalkVisitor::visit_FileWrite(x);
    }

    // A `block` or `associate` construct accesses what the declarations of
    // its locals evaluate on entry, and what its body does.
    void declarations(SymbolTable *scope) {
        for (auto &item : scope->get_scope()) {
            if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(item.second);
            visit_ttype(*v->m_type);
            if (v->m_symbolic_value) visit_expr(*v->m_symbolic_value);
        }
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::Block_t *b = ASR::down_cast<ASR::Block_t>(
            ASRUtils::symbol_get_past_external(x.m_m));
        declarations(b->m_symtab);
        for (size_t i = 0; i < b->n_body; i++) visit_stmt(*b->m_body[i]);
    }
    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::AssociateBlock_t *b = ASR::down_cast<ASR::AssociateBlock_t>(
            ASRUtils::symbol_get_past_external(x.m_m));
        declarations(b->m_symtab);
        for (size_t i = 0; i < b->n_body; i++) visit_stmt(*b->m_body[i]);
    }
};

// What the body of a procedure accesses, as a call of it counts it.
struct BodyAccesses {
    // What it names through dummy argument `first`: paths rooted at that
    // dummy.
    std::vector<std::pair<size_t, Path>> dummies;
    // What it names of the procedures it is contained in.
    std::vector<Path> host;
    bool globals = false;
    bool everything = false;
    bool observable = false;
};

// Whether a body may run device code or wait for it, which a call of it then
// does too: it launches, waits, or has a loop another compilation of it may
// launch.
class RunsDeviceCode : public ASR::BaseWalkVisitor<RunsDeviceCode> {
public:
    bool found = false;
    bool stops = false;
    std::set<ASR::symbol_t*> blocks;
    void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &) { found = true; }
    void visit_GpuSync(const ASR::GpuSync_t &) { found = true; }
    void visit_GpuWait(const ASR::GpuWait_t &) { found = true; }
    void visit_GpuDiscard(const ASR::GpuDiscard_t &) { found = true; }
    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &) {
        found = true;
    }
    void visit_OMPRegion(const ASR::OMPRegion_t &) { found = true; }
    void visit_Stop(const ASR::Stop_t &x) {
        stops = true;
        BaseWalkVisitor::visit_Stop(x);
    }
    void visit_ErrorStop(const ASR::ErrorStop_t &x) {
        stops = true;
        BaseWalkVisitor::visit_ErrorStop(x);
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!blocks.insert(b).second) return;
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(b);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!blocks.insert(b).second) return;
        ASR::AssociateBlock_t *block = ASR::down_cast<ASR::AssociateBlock_t>(
            b);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
};

// What a call of a procedure accesses, from its body, for the procedures
// whose body is known, that run no device code and whose dummy arguments are
// neither pointers, targets nor procedures. Each body is looked at once; a
// procedure that calls itself, directly or not, is not summarized while its
// summary is being made.
class CallSummaries {
public:
    std::map<ASR::Function_t*, bool> &outside;
    explicit CallSummaries(std::map<ASR::Function_t*, bool> &outside_)
        : outside(outside_) {}

    // nullptr when a call of `fn` is not summarized.
    const BodyAccesses *get(ASR::Function_t *fn) {
        auto it = memo.find(fn);
        if (it != memo.end()) return it->second.known ? &it->second : nullptr;
        if (active.count(fn) > 0) return nullptr;
        Summary s;
        active.insert(fn);
        s.known = summarize(fn, s);
        active.erase(fn);
        Summary &kept = memo[fn] = s;
        return kept.known ? &kept : nullptr;
    }

private:
    struct Summary : BodyAccesses {
        bool known = false;
    };
    std::map<ASR::Function_t*, Summary> memo;
    std::set<ASR::Function_t*> active;

    // Whether a path goes through a Reference, which refers to a variable
    // the path does not name.
    static bool through_reference(const Path &p) {
        if (ASR::is_a<ASR::Reference_t>(*ASRUtils::symbol_type(p.root))) {
            return true;
        }
        for (ASR::symbol_t *m : p.members) {
            if (ASR::is_a<ASR::Reference_t>(*ASRUtils::symbol_type(m))) {
                return true;
            }
        }
        return false;
    }

    bool summarize(ASR::Function_t *fn, Summary &s) {
        ASR::FunctionType_t *type = ASRUtils::get_FunctionType(fn);
        if (type->m_deftype != ASR::deftypeType::Implementation
                || fn->m_gpu != nullptr) {
            return false;
        }
        std::map<ASR::symbol_t*, size_t> dummies;
        for (size_t i = 0; i < fn->n_args; i++) {
            if (fn->m_args[i] == nullptr
                    || !ASR::is_a<ASR::Var_t>(*fn->m_args[i])) {
                return false;
            }
            ASR::symbol_t *d = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(fn->m_args[i])->m_v);
            if (!ASR::is_a<ASR::Variable_t>(*d)) return false;
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(d);
            if (v->m_target_attr || ASRUtils::is_pointer(v->m_type)) {
                return false;
            }
            dummies[d] = i;
        }
        RunsDeviceCode device;
        for (size_t i = 0; i < fn->n_body && !device.found; i++) {
            device.visit_stmt(*fn->m_body[i]);
        }
        if (device.found) return false;
        PointerTargets targets;
        PointerCollector collector(targets);
        for (size_t i = 0; i < fn->n_body; i++) {
            collector.visit_stmt(*fn->m_body[i]);
        }
        Accesses acc;
        AccessCollector c(acc, fn->m_symtab, targets, outside, this);
        c.declarations(fn->m_symtab);
        for (size_t i = 0; i < fn->n_body; i++) c.visit_stmt(*fn->m_body[i]);
        s.everything = acc.everything;
        s.globals = acc.globals;
        s.observable = acc.observable || device.stops
            || acts_outside(fn, outside);
        for (const Path &p : acc.paths) {
            if (p.root == nullptr || !ASR::is_a<ASR::Variable_t>(*p.root)) {
                s.globals = true;
                continue;
            }
            if (through_reference(p)) {
                // A frame of a host procedure (nested_vars): the variable
                // it refers to is not on the path.
                s.everything = true;
                continue;
            }
            auto d = dummies.find(p.root);
            if (d != dummies.end()) {
                s.dummies.push_back({d->second, p});
                continue;
            }
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(p.root);
            SymbolTable *owner = enclosing_routine(v->m_parent_symtab);
            if (owner == fn->m_symtab) {
                // Its own storage, which no launch of the caller can be
                // writing unless a pointer can reach it.
                if (v->m_target_attr || is_pointer_variable(p.root)) {
                    s.globals = true;
                }
            } else if (owner != nullptr && contained_in(fn, owner)
                    && !is_pointer_variable(p.root)
                    && !aliasable(p.root)) {
                // A variable of a procedure `fn` is contained in.
                s.host.push_back(p);
            } else {
                s.globals = true;
            }
        }
        return true;
    }
};

inline bool AccessCollector::summarized_call(ASR::Function_t *fn,
        ASR::call_arg_t *args, size_t n_args) {
    if (summaries == nullptr) return false;
    const BodyAccesses *body = summaries->get(fn);
    if (body == nullptr) return false;
    if (contained != nullptr && contained_in(fn, routine)) {
        contained->push_back(fn);
    }
    for (size_t i = 0; i < n_args; i++) {
        if (!args[i].m_value) continue;
        Path p;
        std::vector<ASR::expr_t*> subscripts;
        if (!path_of(pointers, args[i].m_value, p, &subscripts)) {
            // An expression: the body sees its value, not the storage it
            // reads, which evaluating it accesses.
            visit_expr(*args[i].m_value);
            continue;
        }
        for (ASR::expr_t *sub : subscripts) visit_expr(*sub);
        if (is_pointer_variable(p.root)) acc.globals = true;
        for (const auto &d : body->dummies) {
            if (d.first != i) continue;
            Path q = p;
            q.members.insert(q.members.end(), d.second.members.begin(),
                d.second.members.end());
            q.shallow = d.second.shallow;
            acc.paths.push_back(q);
        }
    }
    for (const Path &p : body->host) {
        acc.paths.push_back(p);
        // Unless this procedure is the one that has them, another
        // procedure's variables.
        if (!contained_in(fn, routine)
                || enclosing_routine(ASR::down_cast<ASR::Variable_t>(p.root)
                    ->m_parent_symtab) != routine) {
            acc.globals = true;
        }
    }
    acc.globals = acc.globals || body->globals;
    acc.everything = acc.everything || body->everything;
    acc.observable = acc.observable || body->observable;
    return true;
}

// Whether `acc`, the accesses of something in the procedure whose scope is
// `routine`, may access what `x` names.
inline bool conflicts(const Accesses &acc, const Path &x,
        SymbolTable *routine) {
    if (acc.everything) return true;
    if (acc.globals && reachable_elsewhere(x.root, routine)) return true;
    for (const Path &a : acc.paths) {
        if (overlap(a, x)) return true;
    }
    return false;
}

// Whether a statement may transfer control out of itself: `return`,
// `goto`, or an `exit` or `cycle` of a loop or construct it is not in.
class Leaves : public ASR::BaseWalkVisitor<Leaves> {
public:
    bool found = false;
    std::vector<std::string> names;
    int loops = 0;
    std::set<ASR::symbol_t*> blocks;

    template <typename Loop>
    void loop(const Loop &x, const char *name) {
        names.push_back(name ? name : "");
        loops++;
        for (size_t i = 0; i < x.n_body; i++) visit_stmt(*x.m_body[i]);
        loops--;
        names.pop_back();
    }
    void visit_DoLoop(const ASR::DoLoop_t &x) {
        loop(x, x.m_name);
        for (size_t i = 0; i < x.n_orelse; i++) visit_stmt(*x.m_orelse[i]);
    }
    void visit_WhileLoop(const ASR::WhileLoop_t &x) {
        loop(x, x.m_name);
        for (size_t i = 0; i < x.n_orelse; i++) visit_stmt(*x.m_orelse[i]);
    }
    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        loop(x, nullptr);
    }
    void transfer(const char *name) {
        if (name == nullptr) {
            if (loops == 0) found = true;
            return;
        }
        for (const std::string &n : names) {
            if (n == name) return;
        }
        found = true;
    }
    void visit_Exit(const ASR::Exit_t &x) { transfer(x.m_stmt_name); }
    void visit_Cycle(const ASR::Cycle_t &x) { transfer(x.m_stmt_name); }
    void visit_Return(const ASR::Return_t &/*x*/) { found = true; }
    void visit_GoTo(const ASR::GoTo_t &/*x*/) { found = true; }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_m);
        if (!blocks.insert(s).second) return;
        ASR::Block_t *b = ASR::down_cast<ASR::Block_t>(s);
        // `exit` names the block it leaves.
        names.push_back(b->m_name);
        for (size_t i = 0; i < b->n_body; i++) visit_stmt(*b->m_body[i]);
        names.pop_back();
    }
    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_m);
        if (!blocks.insert(s).second) return;
        ASR::AssociateBlock_t *b = ASR::down_cast<ASR::AssociateBlock_t>(s);
        names.push_back(b->m_name);
        for (size_t i = 0; i < b->n_body; i++) visit_stmt(*b->m_body[i]);
        names.pop_back();
    }
};

inline Accesses expression_accesses(ASR::expr_t *e, SymbolTable *routine,
        const PointerTargets &pointers,
        std::map<ASR::Function_t*, bool> &outside,
        CallSummaries *summaries = nullptr) {
    Accesses acc;
    if (e != nullptr) {
        AccessCollector c(acc, routine, pointers, outside, summaries);
        c.visit_expr(*e);
    }
    return acc;
}


} // namespace AccessPaths

} // namespace LCompilers

#endif // LIBASR_PASS_ACCESS_PATHS_H
