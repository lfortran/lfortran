#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/access_paths.h>
#include <libasr/pass/dead_array_store.h>

#include <map>
#include <set>
#include <string>
#include <vector>

namespace LCompilers {

/*
Dead whole-array assignments.

A whole-array assignment whose every element the program assigns again
before anything can read it computes nothing anyone sees:

    dcdw = 0.              ! dead
    block
        real :: r(size(dcdw, 1), size(dcdw, 2), n)
        ...                ! nothing here reads dcdw
        dcdw = sum(r, dim=3)
    end block

On the host it costs a pass over the array; when the array lives on the
device, it costs a kernel launch (gpu_array_statements.cpp). This pass
removes such an assignment, `x = v`, when all of this holds:

* `x` is a whole array of an intrinsic type, named without subscripts: a
  variable, a chain of components, or an associate name, but not another
  pointer, nor a component through a pointer, nor a member of a common
  block, nor volatile or asynchronous (another agent or a pending
  asynchronous operation may read it at any call). Its shape stays: if it
  is allocatable, `v` is a scalar. `v` calls no procedure and no impure
  intrinsic, and the assignment is not a defined assignment.
* A later assignment `x = w` of the same list of statements, or of a
  `block` or `associate` construct that list runs unconditionally, assigns
  every element again: it names `x` in the same way and is not a defined
  assignment.
* Nothing in between -- `w` included, and the declarations of the
  constructs entered -- may access the values of `x` (access_paths.h):
  asking about its shape does not. Nothing in between may leave the
  statements in between (`return`, `goto`, an `exit` or `cycle` of an
  enclosing loop or construct), which could skip the second assignment.
  `stop` may: nothing can see `x` after it.

The pass runs before the passes that lower whole-array assignments, which
only see the assignments that are left.
*/

namespace {

using namespace AccessPaths;

// [variable, components...] of a designator without subscripts, or empty.
std::vector<ASR::symbol_t*> whole_designator(ASR::expr_t *e) {
    std::vector<ASR::symbol_t*> chain;
    while (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
        ASR::StructInstanceMember_t *m =
            ASR::down_cast<ASR::StructInstanceMember_t>(e);
        chain.insert(chain.begin(),
            ASRUtils::symbol_get_past_external(m->m_m));
        e = m->m_v;
    }
    if (!ASR::is_a<ASR::Var_t>(*e)) return {};
    chain.insert(chain.begin(), ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(e)->m_v));
    return chain;
}

bool intrinsic_element_type(ASR::ttype_t *t) {
    t = ASRUtils::type_get_past_array(
        ASRUtils::type_get_past_allocatable_pointer(t));
    return ASR::is_a<ASR::Real_t>(*t) || ASR::is_a<ASR::Integer_t>(*t)
        || ASR::is_a<ASR::Complex_t>(*t) || ASR::is_a<ASR::Logical_t>(*t)
        || ASR::is_a<ASR::UnsignedInteger_t>(*t);
}

// Whether evaluating an expression may do anything but compute a value.
class SideEffects : public ASR::BaseWalkVisitor<SideEffects> {
public:
    bool found = false;
    void visit_FunctionCall(const ASR::FunctionCall_t &/*x*/) {
        found = true;
    }
    void visit_IntrinsicImpureFunction(
            const ASR::IntrinsicImpureFunction_t &/*x*/) {
        found = true;
    }
};

class DeadArrayStores {
public:
    DeadArrayStores(SymbolTable *routine_, const PointerTargets &pointers_)
        : routine(routine_), pointers(pointers_) {}

    bool removed_any = false;

    // Removes the dead assignments of a list of statements and of every
    // list in it.
    void list(ASR::stmt_t **body, size_t &n_body) {
        for (size_t i = 0; i < n_body; i++) nested(body[i]);
        size_t kept = 0;
        for (size_t i = 0; i < n_body; i++) {
            if (dead(body, n_body, i)) {
                removed_any = true;
                continue;
            }
            body[kept++] = body[i];
        }
        n_body = kept;
    }

private:
    SymbolTable *routine;
    const PointerTargets &pointers;
    std::map<ASR::Function_t*, bool> outside;
    std::set<ASR::symbol_t*> blocks;

    enum class Scan { Overwritten, Blocked, Continue };

    void nested(ASR::stmt_t *s) {
        switch (s->type) {
            case ASR::stmtType::If: {
                ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                list(x->m_body, x->n_body);
                list(x->m_orelse, x->n_orelse);
                break;
            }
            case ASR::stmtType::DoLoop: {
                ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(s);
                list(x->m_body, x->n_body);
                list(x->m_orelse, x->n_orelse);
                break;
            }
            case ASR::stmtType::WhileLoop: {
                ASR::WhileLoop_t *x = ASR::down_cast<ASR::WhileLoop_t>(s);
                list(x->m_body, x->n_body);
                list(x->m_orelse, x->n_orelse);
                break;
            }
            case ASR::stmtType::DoConcurrentLoop: {
                ASR::DoConcurrentLoop_t *x =
                    ASR::down_cast<ASR::DoConcurrentLoop_t>(s);
                list(x->m_body, x->n_body);
                break;
            }
            case ASR::stmtType::OMPRegion: {
                ASR::OMPRegion_t *x = ASR::down_cast<ASR::OMPRegion_t>(s);
                list(x->m_body, x->n_body);
                break;
            }
            case ASR::stmtType::Select: {
                ASR::Select_t *x = ASR::down_cast<ASR::Select_t>(s);
                for (size_t i = 0; i < x->n_body; i++) {
                    ASR::case_stmt_t *c = x->m_body[i];
                    if (ASR::is_a<ASR::CaseStmt_t>(*c)) {
                        ASR::CaseStmt_t *cs = ASR::down_cast<ASR::CaseStmt_t>(c);
                        list(cs->m_body, cs->n_body);
                    } else if (ASR::is_a<ASR::CaseStmt_Range_t>(*c)) {
                        ASR::CaseStmt_Range_t *cs =
                            ASR::down_cast<ASR::CaseStmt_Range_t>(c);
                        list(cs->m_body, cs->n_body);
                    }
                }
                list(x->m_default, x->n_default);
                break;
            }
            case ASR::stmtType::BlockCall: {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                if (!blocks.insert(b).second) break;
                ASR::Block_t *x = ASR::down_cast<ASR::Block_t>(b);
                list(x->m_body, x->n_body);
                break;
            }
            case ASR::stmtType::AssociateBlockCall: {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::AssociateBlockCall_t>(s)->m_m);
                if (!blocks.insert(b).second) break;
                ASR::AssociateBlock_t *x =
                    ASR::down_cast<ASR::AssociateBlock_t>(b);
                list(x->m_body, x->n_body);
                break;
            }
            default:
                break;
        }
    }

    bool conflicts(const Accesses &acc, const Path &x) {
        return AccessPaths::conflicts(acc, x, routine);
    }

    // The assignment `s`, when it is one this pass may remove.
    ASR::Assignment_t* candidate(ASR::stmt_t *s, Path &path) {
        if (!ASR::is_a<ASR::Assignment_t>(*s)) return nullptr;
        ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(s);
        if (a->m_overloaded || a->m_move_allocation) return nullptr;
        ASR::ttype_t *type = ASRUtils::expr_type(a->m_target);
        if (!ASRUtils::is_array(type) || !intrinsic_element_type(type)) {
            return nullptr;
        }
        std::vector<ASR::symbol_t*> chain = whole_designator(a->m_target);
        if (chain.empty() || !ASR::is_a<ASR::Variable_t>(*chain[0])) {
            return nullptr;
        }
        ASR::Variable_t *root = ASR::down_cast<ASR::Variable_t>(chain[0]);
        if (root->m_common_block || root->m_is_volatile
                || root->m_is_asynchronous
                || ASRUtils::has_scoped_variable_attribute(*root, routine)) {
            return nullptr;
        }
        // An associate name is bound once; another pointer may be
        // associated with something else in between.
        if (is_pointer_variable(chain[0])) {
            bool unknown = false;
            if (pointer_target(pointers, chain[0], unknown) == nullptr) {
                return nullptr;
            }
        }
        for (size_t i = 1; i < chain.size(); i++) {
            if (ASRUtils::is_pointer(ASRUtils::symbol_type(chain[i]))) {
                return nullptr;
            }
        }
        if (!path_of(pointers, a->m_target, path, nullptr)
                || is_pointer_variable(path.root)) {
            return nullptr;
        }
        if (ASRUtils::is_allocatable(type)
                && ASRUtils::is_array(ASRUtils::expr_type(a->m_value))
                && !ASR::is_a<ASR::ArrayBroadcast_t>(*a->m_value)) {
            return nullptr;
        }
        SideEffects effects;
        effects.visit_expr(*a->m_value);
        if (effects.found) return nullptr;
        return a;
    }

    // Whether `s` is an assignment of every element of what `target`
    // names, in the same way.
    bool overwrites(ASR::stmt_t *s, ASR::expr_t *target) {
        if (!ASR::is_a<ASR::Assignment_t>(*s)) return false;
        ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(s);
        if (a->m_overloaded) return false;
        std::vector<ASR::symbol_t*> chain = whole_designator(a->m_target);
        return !chain.empty() && chain == whole_designator(target);
    }

    Scan scan(ASR::stmt_t **body, size_t n_body, size_t from,
            ASR::expr_t *target, const Path &x) {
        for (size_t i = from; i < n_body; i++) {
            ASR::stmt_t *s = body[i];
            if (overwrites(s, target)) {
                ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(s);
                Accesses acc;
                AccessCollector c(acc, routine, pointers, outside);
                c.visit_expr(*a->m_value);
                return conflicts(acc, x) ? Scan::Blocked : Scan::Overwritten;
            }
            if (ASR::is_a<ASR::BlockCall_t>(*s)
                    || ASR::is_a<ASR::AssociateBlockCall_t>(*s)) {
                SymbolTable *scope;
                ASR::stmt_t **inner;
                size_t n_inner;
                if (ASR::is_a<ASR::BlockCall_t>(*s)) {
                    ASR::Block_t *b = ASR::down_cast<ASR::Block_t>(
                        ASRUtils::symbol_get_past_external(
                            ASR::down_cast<ASR::BlockCall_t>(s)->m_m));
                    scope = b->m_symtab;
                    inner = b->m_body;
                    n_inner = b->n_body;
                } else {
                    ASR::AssociateBlock_t *b =
                        ASR::down_cast<ASR::AssociateBlock_t>(
                            ASRUtils::symbol_get_past_external(
                                ASR::down_cast<ASR::AssociateBlockCall_t>(
                                    s)->m_m));
                    scope = b->m_symtab;
                    inner = b->m_body;
                    n_inner = b->n_body;
                }
                Accesses acc;
                AccessCollector c(acc, routine, pointers, outside);
                c.declarations(scope);
                if (conflicts(acc, x)) return Scan::Blocked;
                Scan r = scan(inner, n_inner, 0, target, x);
                if (r != Scan::Continue) return r;
                // A construct whose body may be left early (`exit` of the
                // construct itself) has been scanned to its end only when
                // nothing left it, which `scan` checked.
                continue;
            }
            Leaves leaves;
            leaves.visit_stmt(*s);
            if (leaves.found) return Scan::Blocked;
            Accesses acc;
            AccessCollector c(acc, routine, pointers, outside);
            c.visit_stmt(*s);
            if (conflicts(acc, x)) return Scan::Blocked;
        }
        return Scan::Continue;
    }

    bool dead(ASR::stmt_t **body, size_t n_body, size_t i) {
        Path x;
        ASR::Assignment_t *a = candidate(body[i], x);
        if (a == nullptr) return false;
        return scan(body, n_body, i + 1, a->m_target, x) == Scan::Overwritten;
    }
};

void remove_dead_stores(SymbolTable *scope, ASR::stmt_t **body,
        size_t &n_body) {
    PointerTargets targets;
    PointerCollector collector(targets);
    for (size_t i = 0; i < n_body; i++) collector.visit_stmt(*body[i]);
    DeadArrayStores stores(scope, targets);
    stores.list(body, n_body);
}

void visit_scope(SymbolTable *scope) {
    for (auto &item : scope->get_scope()) {
        ASR::symbol_t *sym = item.second;
        if (ASR::is_a<ASR::Function_t>(*sym)) {
            ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(sym);
            remove_dead_stores(fn->m_symtab, fn->m_body, fn->n_body);
            visit_scope(fn->m_symtab);
        } else if (ASR::is_a<ASR::Program_t>(*sym)) {
            ASR::Program_t *p = ASR::down_cast<ASR::Program_t>(sym);
            remove_dead_stores(p->m_symtab, p->m_body, p->n_body);
            visit_scope(p->m_symtab);
        } else if (ASR::is_a<ASR::Module_t>(*sym)) {
            visit_scope(ASR::down_cast<ASR::Module_t>(sym)->m_symtab);
        }
    }
}

} // namespace

void pass_dead_array_store(Allocator &/*al*/, ASR::TranslationUnit_t &unit,
        const PassOptions &/*pass_options*/) {
    visit_scope(unit.m_symtab);
}

} // namespace LCompilers
