#ifndef LIBASR_PASS_ACCESS_PATHS_H
#define LIBASR_PASS_ACCESS_PATHS_H

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/intrinsic_function_registry.h>

#include <map>
#include <set>
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
*/

namespace AccessPaths {

// A variable and a chain of components.
struct Path {
    ASR::symbol_t *root = nullptr;
    std::vector<ASR::symbol_t*> members;
    // Only the bytes of the value itself, not the data of its allocatable
    // or pointer components.
    bool shallow = false;

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
    // May access anything.
    bool everything = false;
};

inline bool is_pointer_variable(ASR::symbol_t *s) {
    return s != nullptr && ASR::is_a<ASR::Variable_t>(*s)
        && ASRUtils::is_pointer(ASR::down_cast<ASR::Variable_t>(s)->m_type);
}

// A root a pointer may point into, or that another procedure can reach.
inline bool aliasable(ASR::symbol_t *s) {
    if (s == nullptr || !ASR::is_a<ASR::Variable_t>(*s)) return true;
    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
    if (v->m_target_attr || is_pointer_variable(s)) return true;
    SymbolTable *scope = v->m_parent_symtab;
    if (scope == nullptr || scope->asr_owner == nullptr) return true;
    if (ASR::is_a<ASR::unit_t>(*scope->asr_owner)) return true;
    ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(scope->asr_owner);
    return ASR::is_a<ASR::Module_t>(*owner);
}

// A root another procedure may access: one a pointer may point into, or a
// variable that is not a local or dummy argument of `routine` -- a module
// variable, or a variable of the procedure `routine` is contained in, which
// the procedures contained there access by host association.
inline bool reachable_elsewhere(ASR::symbol_t *s, SymbolTable *routine) {
    if (aliasable(s)) return true;
    SymbolTable *scope = ASR::down_cast<ASR::Variable_t>(s)->m_parent_symtab;
    while (scope != nullptr && scope->asr_owner != nullptr
            && ASR::is_a<ASR::symbol_t>(*scope->asr_owner)
            && (ASR::is_a<ASR::Block_t>(*ASR::down_cast<ASR::symbol_t>(
                    scope->asr_owner))
                || ASR::is_a<ASR::AssociateBlock_t>(
                    *ASR::down_cast<ASR::symbol_t>(scope->asr_owner)))) {
        scope = scope->parent;
    }
    return scope != routine;
}

inline bool same_storage_root(ASR::symbol_t *a, ASR::symbol_t *b) {
    if (a == b) return true;
    bool pa = a == nullptr || is_pointer_variable(a);
    bool pb = b == nullptr || is_pointer_variable(b);
    return (pa && aliasable(b)) || (pb && aliasable(a));
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

class AccessCollector : public ASR::BaseWalkVisitor<AccessCollector> {
public:
    Accesses &acc;
    SymbolTable *routine;
    const PointerTargets &pointers;
    std::map<ASR::Function_t*, bool> &outside;

    AccessCollector(Accesses &acc_, SymbolTable *routine_,
            const PointerTargets &pointers_,
            std::map<ASR::Function_t*, bool> &outside_)
        : acc(acc_), routine(routine_), pointers(pointers_),
          outside(outside_) {}

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
    // not its values.
    void visit_ArraySize(const ASR::ArraySize_t &x) {
        if (x.m_dim) visit_expr(*x.m_dim);
    }
    void visit_ArrayBound(const ASR::ArrayBound_t &x) {
        if (x.m_dim) visit_expr(*x.m_dim);
    }
    void visit_ArrayRank(const ASR::ArrayRank_t &/*x*/) {}
    void visit_ArrayIsContiguous(const ASR::ArrayIsContiguous_t &/*x*/) {}
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
            acc.everything = true;
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
};

inline Accesses expression_accesses(ASR::expr_t *e, SymbolTable *routine,
        const PointerTargets &pointers,
        std::map<ASR::Function_t*, bool> &outside) {
    Accesses acc;
    if (e != nullptr) {
        AccessCollector c(acc, routine, pointers, outside);
        c.visit_expr(*e);
    }
    return acc;
}


} // namespace AccessPaths

} // namespace LCompilers

#endif // LIBASR_PASS_ACCESS_PATHS_H
