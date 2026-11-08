#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/pass/gpu_inline_loop_calls.h>
#include <libasr/pass/gpu_loop_utils.h>
#include <libasr/pass/pass_utils.h>

#include <functional>

namespace LCompilers {

/*
Inlining of the calls of a kernel that run a loop.

A pure function that reduces its array argument,

    pure function sum_squares(x) result(total)
        real, intent(in) :: x(:)
        real :: total
        total = sum(x**2)
    end function

called in a parallel loop as `r = 1/sqrt(sum_squares(x(:, m)))`, is a call
of the kernel's copy of the function (gpu_offload), whose body is the loop
that computes the sum. gpu_vector_lanes shares the loops of a kernel's body
between the lanes of an iteration, and a reduction loop among them is
combined across the lanes (GpuLaneReduce), but it does not look into the
procedures the kernel calls: every lane runs the whole loop of the call by
itself. So a call of a function like this one is inlined in the kernel
(GpuInliner): the statement becomes a BLOCK whose variables are the
function's locals and result, and whose body is the function's body, with
the elements of the actual argument in place of those of the dummy,
followed by the statement, which reads the result:

    block
        real :: res, total
        integer :: k
        res = 0
        do k = 1, size(x_m)
            res = res + x_m(k)**2
        end do
        total = res
        r = 1/sqrt(total)
    end block

A call is inlined when it is in an assignment of the kernel (or of the
BLOCKs, loops and branches it runs), in an operand that is always
evaluated, and its function
* is one GpuInliner can inline (gpu_function_inlinable): pure, with a body
  and a scalar result, scalar and assumed-shape array dummies, scalar
  locals, and no RETURN, STOP, input/output or subroutine call;
* runs a loop, which is what the kernel gains by it; a function without
  one costs nothing as a call of its own (the device compiler inlines it);
* is small: its statements, those of its loops included, cost at most
  inline_max_cost (gpu_stmts_cost).
Calls the inlined body makes are inlined in turn when they are calls such
as these. A statement whose picked calls cannot all be inlined, say because
an actual argument is an expression rather than an array, stays as it was.

The function's statements run where the call did, in the same order, so the
transformation is exact.

The pass runs once the array operations of device code are loops
(array_op), while the intrinsics of the inlined bodies are still
intrinsics.
*/

namespace {

// The most a function's statements may cost, its loops included, for its
// calls to be inlined.
constexpr int64_t inline_max_cost = 64;

// What the statements cost, the bodies of their loops included.
int64_t cost_with_loops(ASR::stmt_t **body, size_t n) {
    int64_t total = gpu_stmts_cost(body, n);
    for (size_t i = 0; i < n; i++) {
        ASR::stmt_t *s = body[i];
        if (ASR::is_a<ASR::DoLoop_t>(*s)) {
            ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(s);
            total += cost_with_loops(x->m_body, x->n_body);
        } else if (ASR::is_a<ASR::DoConcurrentLoop_t>(*s)) {
            ASR::DoConcurrentLoop_t *x =
                ASR::down_cast<ASR::DoConcurrentLoop_t>(s);
            total += cost_with_loops(x->m_body, x->n_body);
        } else if (ASR::is_a<ASR::If_t>(*s)) {
            ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
            total += cost_with_loops(x->m_body, x->n_body)
                + cost_with_loops(x->m_orelse, x->n_orelse);
        }
    }
    return total;
}

bool has_loop(ASR::stmt_t **body, size_t n) {
    for (size_t i = 0; i < n; i++) {
        ASR::stmt_t *s = body[i];
        if (ASR::is_a<ASR::DoLoop_t>(*s)
                || ASR::is_a<ASR::DoConcurrentLoop_t>(*s)) {
            return true;
        }
        if (ASR::is_a<ASR::If_t>(*s)) {
            ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
            if (has_loop(x->m_body, x->n_body)
                    || has_loop(x->m_orelse, x->n_orelse)) {
                return true;
            }
        }
    }
    return false;
}

// Whether the calls of `fn` are inlined.
bool picked(ASR::FunctionCall_t *call) {
    ASR::symbol_t *f = ASRUtils::symbol_get_past_external(call->m_name);
    if (!ASR::is_a<ASR::Function_t>(*f)) return false;
    ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(f);
    return gpu_function_inlinable(fn)
        && has_loop(fn->m_body, fn->n_body)
        && cost_with_loops(fn->m_body, fn->n_body) <= inline_max_cost;
}

bool has_picked_call(ASR::expr_t *e) {
    struct Find : public ASR::BaseWalkVisitor<Find> {
        bool found = false;
        void visit_FunctionCall(const ASR::FunctionCall_t &x) {
            if (picked(const_cast<ASR::FunctionCall_t*>(&x))) found = true;
            ASR::BaseWalkVisitor<Find>::visit_FunctionCall(x);
        }
    } f;
    f.visit_expr(*e);
    return f.found;
}

class LoopCallInliner {
public:
    LoopCallInliner(Allocator &al_, int offset_kind_)
        : al(al_), offset_kind(offset_kind_) {}

    void body(SymbolTable *scope, ASR::stmt_t **stmts, size_t n) {
        for (size_t i = 0; i < n; i++) stmt(scope, stmts[i]);
    }

private:
    Allocator &al;
    int offset_kind;

    void stmt(SymbolTable *scope, ASR::stmt_t *&s) {
        switch (s->type) {
            case ASR::stmtType::Assignment: {
                ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(s);
                if (a->m_overloaded || !has_picked_call(a->m_value)) return;
                assignment(scope, s);
                return;
            }
            case ASR::stmtType::BlockCall: {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                if (!ASR::is_a<ASR::Block_t>(*b)) return;
                ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                body(blk->m_symtab, blk->m_body, blk->n_body);
                return;
            }
            case ASR::stmtType::If: {
                ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                body(scope, x->m_body, x->n_body);
                body(scope, x->m_orelse, x->n_orelse);
                return;
            }
            case ASR::stmtType::DoLoop: {
                ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(s);
                body(scope, x->m_body, x->n_body);
                return;
            }
            case ASR::stmtType::DoConcurrentLoop: {
                ASR::DoConcurrentLoop_t *x =
                    ASR::down_cast<ASR::DoConcurrentLoop_t>(s);
                body(scope, x->m_body, x->n_body);
                return;
            }
            case ASR::stmtType::WhileLoop: {
                ASR::WhileLoop_t *x = ASR::down_cast<ASR::WhileLoop_t>(s);
                body(scope, x->m_body, x->n_body);
                return;
            }
            case ASR::stmtType::Select: {
                ASR::Select_t *x = ASR::down_cast<ASR::Select_t>(s);
                for (size_t i = 0; i < x->n_body; i++) {
                    ASR::case_stmt_t *c = x->m_body[i];
                    if (ASR::is_a<ASR::CaseStmt_t>(*c)) {
                        ASR::CaseStmt_t *cs =
                            ASR::down_cast<ASR::CaseStmt_t>(c);
                        body(scope, cs->m_body, cs->n_body);
                    } else {
                        ASR::CaseStmt_Range_t *cs =
                            ASR::down_cast<ASR::CaseStmt_Range_t>(c);
                        body(scope, cs->m_body, cs->n_body);
                    }
                }
                body(scope, x->m_default, x->n_default);
                return;
            }
            default:
                return;
        }
    }

    // Inlines the picked calls of an assignment in a copy of it, which
    // replaces it when every one of them could be.
    void assignment(SymbolTable *scope, ASR::stmt_t *&s) {
        GpuDuplicator dup(al, scope);
        std::vector<std::pair<SymbolTable*, std::string>> added;
        dup.added = &added;
        Vec<ASR::stmt_t*> copy;
        dup.duplicate_body(&s, 1, copy);
        GpuInliner inl(al, picked, offset_kind);
        ASR::stmt_t **cp = copy.p;
        size_t cn = copy.n;
        if (!dup.ok || !inl.run(scope, cp, cn) || cn != 1) {
            inl.rollback();
            for (auto it = added.rbegin(); it != added.rend(); ++it) {
                if (it->first->get_symbol(it->second)) {
                    it->first->erase_symbol(it->second);
                }
            }
            return;
        }
        s = cp[0];
    }
};

} // namespace

void pass_gpu_inline_loop_calls(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options) {
    if (gpu_device_selected(pass_options) == GpuDevice::None) return;
    LoopCallInliner inliner(al,
        gpu_device_capabilities(pass_options).offset_kind);
    for (auto &item : unit.m_symtab->get_scope()) {
        if (!ASRUtils::is_device_kernel(item.second)) continue;
        ASR::Function_t *k = ASR::down_cast<ASR::Function_t>(item.second);
        inliner.body(k->m_symtab, k->m_body, k->n_body);
    }
    PassUtils::UpdateDependenciesVisitor update(al);
    update.visit_TranslationUnit(unit);
}

} // namespace LCompilers
