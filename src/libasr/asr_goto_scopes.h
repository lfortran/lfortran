#ifndef LIBASR_ASR_GOTO_SCOPES_H
#define LIBASR_ASR_GOTO_SCOPES_H

#include <cstdint>
#include <map>
#include <vector>

#include <libasr/asr.h>

namespace LCompilers::ASRUtils {

// Walks the statements of a procedure body keeping `enclosing`, the
// constructs that contain the statement being visited, outermost first: the
// BLOCK and ASSOCIATE constructs (BlockCall, AssociateBlockCall) and the
// loops (DoLoop, DoConcurrentLoop, WhileLoop), each identified by its
// statement. A branch may leave these constructs, but not enter them.
template <class Derived>
class ConstructNestingVisitor : public ASR::BaseWalkVisitor<Derived> {
private:
    Derived& self() { return static_cast<Derived&>(*this); }

public:
    std::vector<const ASR::stmt_t*> enclosing;

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::Block_t* block = ASR::down_cast<ASR::Block_t>(x.m_m);
        visit_construct(&x.base, block->m_body, block->n_body);
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::AssociateBlock_t* block = ASR::down_cast<ASR::AssociateBlock_t>(
            x.m_m);
        visit_construct(&x.base, block->m_body, block->n_body);
    }

    void visit_DoLoop(const ASR::DoLoop_t &x) {
        visit_construct(&x.base, x.m_body, x.n_body);
        // The loop-else runs after the loop, outside it.
        for (size_t i = 0; i < x.n_orelse; i++) {
            self().visit_stmt(*x.m_orelse[i]);
        }
    }

    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        visit_construct(&x.base, x.m_body, x.n_body);
    }

    void visit_WhileLoop(const ASR::WhileLoop_t &x) {
        visit_construct(&x.base, x.m_body, x.n_body);
        for (size_t i = 0; i < x.n_orelse; i++) {
            self().visit_stmt(*x.m_orelse[i]);
        }
    }

    void visit_construct(const ASR::stmt_t *construct, ASR::stmt_t **body,
            size_t n_body) {
        enclosing.push_back(construct);
        for (size_t i = 0; i < n_body; i++) {
            self().visit_stmt(*body[i]);
        }
        enclosing.pop_back();
    }

    void visit_expr(const ASR::expr_t & /*x*/) {}
    void visit_ttype(const ASR::ttype_t & /*x*/) {}
};

// For each statement label of a procedure body, the constructs (in the sense
// of ConstructNestingVisitor) that contain the labeled statement.
class GoToTargetScopes : public ConstructNestingVisitor<GoToTargetScopes> {
public:
    std::map<uint64_t, std::vector<const ASR::stmt_t*>> &scopes;

    GoToTargetScopes(
        std::map<uint64_t, std::vector<const ASR::stmt_t*>> &scopes_):
        scopes(scopes_) {}

    void visit_GoToTarget(const ASR::GoToTarget_t &x) {
        scopes[x.m_id] = enclosing;
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        // The label of the BLOCK statement is outside the construct.
        if (x.m_label != -1) {
            scopes[x.m_label] = enclosing;
        }
        ConstructNestingVisitor<GoToTargetScopes>::visit_BlockCall(x);
    }
};

// The number of constructs that a branch from a statement inside the
// constructs `from` to a label inside the constructs `to` stays in: their
// common prefix. The branch leaves the rest of `from`, and enters the rest
// of `to`, which a valid branch never does.
inline size_t common_construct_depth(
        const std::vector<const ASR::stmt_t*> &from,
        const std::vector<const ASR::stmt_t*> &to) {
    size_t kept = 0;
    while (kept < from.size() && kept < to.size() && from[kept] == to[kept]) {
        kept++;
    }
    return kept;
}

} // namespace LCompilers::ASRUtils

#endif // LIBASR_ASR_GOTO_SCOPES_H
