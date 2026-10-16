#include <algorithm>
#include <functional>
#include <map>
#include <set>
#include <string>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/gpu_offload_collect.h>
#include <libasr/pass/gpu_offload_rewrite.h>
#include <libasr/pass/gpu_offload_visitor.h>
#include <libasr/pass/symbol_expr_substitution.h>

namespace LCompilers {

namespace {

// Whether an ASR subtree names `sym`, counting the declarations of the
// BLOCK and ASSOCIATE constructs it holds as well as their statements.
class GpuSymbolUseFinder :
        public ASRUtils::BlockBodyWalkVisitor<GpuSymbolUseFinder> {
public:
    ASR::symbol_t *sym;
    bool found = false;

    GpuSymbolUseFinder(ASR::symbol_t *sym_) : sym(sym_) {}

    void visit_Var(const ASR::Var_t &x) {
        if (x.m_v == sym) found = true;
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        visit_symbol(*ASRUtils::symbol_get_past_external(x.m_m));
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        visit_symbol(*ASRUtils::symbol_get_past_external(x.m_m));
    }
};

bool stmt_uses(ASR::stmt_t *s, ASR::symbol_t *sym) {
    GpuSymbolUseFinder f(sym);
    f.visit_stmt(*s);
    return f.found;
}

bool expr_uses(ASR::expr_t *e, ASR::symbol_t *sym) {
    if (e == nullptr) return false;
    GpuSymbolUseFinder f(sym);
    f.visit_expr(*e);
    return f.found;
}

bool head_uses(const ASR::do_loop_head_t &h, ASR::symbol_t *sym) {
    return expr_uses(h.m_v, sym) || expr_uses(h.m_start, sym)
        || expr_uses(h.m_end, sym) || expr_uses(h.m_increment, sym);
}

// Whether a procedure has a jump this file's straight-line reading of
// its statements would not follow.
class GpuJumpFinder : public ASRUtils::BlockBodyWalkVisitor<GpuJumpFinder> {
public:
    bool found = false;
    void visit_GoTo(const ASR::GoTo_t &) { found = true; }
    void visit_GoToTarget(const ASR::GoToTarget_t &) { found = true; }
    void visit_IfArithmetic(const ASR::IfArithmetic_t &) { found = true; }
};

// What a run of statements does with the value an array held when the
// run was entered.
//   Untouched -- never reads it: the array is not named, or it is first
//                named by a definition of all of it on every path that
//                names it.
//   Defined   -- defines all of it, on every path, before anything reads
//                it.
//   Read      -- may read it, or this reading cannot tell.
enum class GpuArrayFlow { Untouched, Defined, Read };

// Reads statements in the order they run, to find whether they may read
// the value `sym` held when they were entered. Every shape it does not
// know is taken to read it.
class GpuArrayFlowReader {
public:
    ASR::symbol_t *sym;
    ASR::Array_t *arr;

    GpuArrayFlowReader(ASR::symbol_t *sym_)
        : sym(sym_), arr(ASR::down_cast<ASR::Array_t>(
            ASRUtils::symbol_type(sym_))) {}

    // `escaped` is set when an unnamed EXIT or CYCLE may leave the
    // enclosing loop of the run before its end.
    GpuArrayFlow run(ASR::stmt_t **body, size_t n_body, bool &escaped) {
        for (size_t i = 0; i < n_body; i++) {
            bool leaves = false;
            GpuArrayFlow f = stmt(body[i], escaped, leaves);
            if (f != GpuArrayFlow::Untouched) return f;
            if (leaves) break;
        }
        return GpuArrayFlow::Untouched;
    }

private:
    // The value of an extent the declaration and the loop both name:
    // a compile-time constant, or a variable whose value cannot change
    // while the array exists -- a named constant or an intent(in)
    // dummy argument.
    static bool same_bound(ASR::expr_t *a, ASR::expr_t *b) {
        if (a == nullptr || b == nullptr) return false;
        int64_t va = 0, vb = 0;
        bool ca = ASRUtils::extract_value(ASRUtils::expr_value(a), va);
        bool cb = ASRUtils::extract_value(ASRUtils::expr_value(b), vb);
        if (ca || cb) return ca && cb && va == vb;
        if (!ASR::is_a<ASR::Var_t>(*a) || !ASR::is_a<ASR::Var_t>(*b)) {
            return false;
        }
        ASR::symbol_t *s = ASR::down_cast<ASR::Var_t>(a)->m_v;
        if (s != ASR::down_cast<ASR::Var_t>(b)->m_v) return false;
        s = ASRUtils::symbol_get_past_external(s);
        if (!ASR::is_a<ASR::Variable_t>(*s)) return false;
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
        return v->m_storage == ASR::storage_typeType::Parameter
            || v->m_intent == ASR::intentType::In;
    }

    bool is_lower_bound(size_t d, ASR::expr_t *e) {
        return same_bound(e, arr->m_dims[d].m_start);
    }

    bool is_upper_bound(size_t d, ASR::expr_t *e) {
        ASR::expr_t *start = arr->m_dims[d].m_start;
        ASR::expr_t *length = arr->m_dims[d].m_length;
        if (start == nullptr || length == nullptr || e == nullptr) {
            return false;
        }
        int64_t s = 0, l = 0, v = 0;
        if (!ASRUtils::extract_value(ASRUtils::expr_value(start), s)) {
            return false;
        }
        if (ASRUtils::extract_value(ASRUtils::expr_value(length), l)) {
            return ASRUtils::extract_value(ASRUtils::expr_value(e), v)
                && v == s + l - 1;
        }
        return s == 1 && same_bound(e, length);
    }

    static bool unit_step(ASR::expr_t *step) {
        int64_t v = 0;
        return step == nullptr || (ASRUtils::extract_value(
            ASRUtils::expr_value(step), v) && v == 1);
    }

    bool is_whole(ASR::expr_t *e) {
        if (ASR::is_a<ASR::Var_t>(*e)) {
            return ASR::down_cast<ASR::Var_t>(e)->m_v == sym;
        }
        if (!ASR::is_a<ASR::ArraySection_t>(*e)) return false;
        ASR::ArraySection_t *s = ASR::down_cast<ASR::ArraySection_t>(e);
        if (!ASR::is_a<ASR::Var_t>(*s->m_v) ||
                ASR::down_cast<ASR::Var_t>(s->m_v)->m_v != sym ||
                s->n_args != arr->n_dims) {
            return false;
        }
        for (size_t d = 0; d < s->n_args; d++) {
            const ASR::array_index_t &a = s->m_args[d];
            if (a.m_left == nullptr && a.m_step == nullptr &&
                    a.m_right != nullptr) {
                return false;
            }
            if ((a.m_left && !is_lower_bound(d, a.m_left)) ||
                    (a.m_right && !is_upper_bound(d, a.m_right)) ||
                    !unit_step(a.m_step)) {
                return false;
            }
        }
        return true;
    }

    // `t = v`, or `t(:, ...) = v` over all of `t`, where `v` does not
    // name `t`.
    bool defines_whole(ASR::stmt_t *s) {
        if (!ASR::is_a<ASR::Assignment_t>(*s)) return false;
        ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(s);
        return a->m_overloaded == nullptr && is_whole(a->m_target)
            && !expr_uses(a->m_value, sym);
    }

    // A perfect nest of DO loops, one per dimension, each running with
    // step 1 over the whole extent of its dimension, around one
    // assignment `t(i1, ..., ir) = v` that names each loop's variable as
    // the subscript of its dimension and whose `v` does not name `t`:
    //   do k = 1, m
    //     t(k) = ...
    //   end do
    bool defines_by_loops(ASR::DoLoop_t *outer) {
        std::vector<ASR::DoLoop_t*> loops;
        ASR::stmt_t *s = &outer->base;
        while (ASR::is_a<ASR::DoLoop_t>(*s)) {
            ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(s);
            if (dl->n_body != 1 || dl->n_orelse != 0 ||
                    dl->m_head.m_v == nullptr ||
                    !ASR::is_a<ASR::Var_t>(*dl->m_head.m_v) ||
                    head_uses(dl->m_head, sym) ||
                    !unit_step(dl->m_head.m_increment)) {
                return false;
            }
            loops.push_back(dl);
            s = dl->m_body[0];
        }
        if (!ASR::is_a<ASR::Assignment_t>(*s)) return false;
        ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(s);
        if (a->m_overloaded != nullptr || expr_uses(a->m_value, sym) ||
                !ASR::is_a<ASR::ArrayItem_t>(*a->m_target)) {
            return false;
        }
        ASR::ArrayItem_t *item = ASR::down_cast<ASR::ArrayItem_t>(
            a->m_target);
        if (!ASR::is_a<ASR::Var_t>(*item->m_v) ||
                ASR::down_cast<ASR::Var_t>(item->m_v)->m_v != sym ||
                item->n_args != arr->n_dims ||
                loops.size() != arr->n_dims) {
            return false;
        }
        std::vector<bool> covered(loops.size(), false);
        for (size_t d = 0; d < item->n_args; d++) {
            const ASR::array_index_t &idx = item->m_args[d];
            if (idx.m_left || idx.m_step || idx.m_right == nullptr ||
                    !ASR::is_a<ASR::Var_t>(*idx.m_right)) {
                return false;
            }
            ASR::symbol_t *iv = ASR::down_cast<ASR::Var_t>(
                idx.m_right)->m_v;
            bool matched = false;
            for (size_t l = 0; l < loops.size(); l++) {
                ASR::do_loop_head_t &h = loops[l]->m_head;
                if (ASR::down_cast<ASR::Var_t>(h.m_v)->m_v != iv) continue;
                if (covered[l] || !is_lower_bound(d, h.m_start) ||
                        !is_upper_bound(d, h.m_end)) {
                    return false;
                }
                covered[l] = true;
                matched = true;
                break;
            }
            if (!matched) return false;
        }
        return true;
    }

    // A loop may run no iteration, so what its body defines is not
    // defined after it; what matters is only whether the body reads.
    GpuArrayFlow loop(bool head_names, ASR::stmt_t **body, size_t n_body,
            ASR::stmt_t **orelse, size_t n_orelse) {
        if (head_names) return GpuArrayFlow::Read;
        bool escaped = false;
        if (run(body, n_body, escaped) == GpuArrayFlow::Read) {
            return GpuArrayFlow::Read;
        }
        if (run(orelse, n_orelse, escaped) == GpuArrayFlow::Read) {
            return GpuArrayFlow::Read;
        }
        return GpuArrayFlow::Untouched;
    }

    GpuArrayFlow stmt(ASR::stmt_t *s, bool &escaped, bool &leaves) {
        switch (s->type) {
            case ASR::stmtType::Exit:
            case ASR::stmtType::Cycle: {
                bool named = ASR::is_a<ASR::Exit_t>(*s)
                    ? ASR::down_cast<ASR::Exit_t>(s)->m_stmt_name != nullptr
                    : ASR::down_cast<ASR::Cycle_t>(s)->m_stmt_name != nullptr;
                if (named) return GpuArrayFlow::Read;
                escaped = true;
                leaves = true;
                return GpuArrayFlow::Untouched;
            }
            case ASR::stmtType::Return:
            case ASR::stmtType::Stop:
            case ASR::stmtType::ErrorStop: {
                // Nothing runs after it that could read the array.
                return stmt_uses(s, sym) ? GpuArrayFlow::Read
                    : GpuArrayFlow::Defined;
            }
            default: break;
        }
        if (!stmt_uses(s, sym)) return GpuArrayFlow::Untouched;
        switch (s->type) {
            case ASR::stmtType::Assignment: {
                return defines_whole(s) ? GpuArrayFlow::Defined
                    : GpuArrayFlow::Read;
            }
            case ASR::stmtType::If: {
                ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                if (expr_uses(x->m_test, sym)) return GpuArrayFlow::Read;
                GpuArrayFlow a = run(x->m_body, x->n_body, escaped);
                GpuArrayFlow b = run(x->m_orelse, x->n_orelse, escaped);
                if (a == GpuArrayFlow::Read || b == GpuArrayFlow::Read) {
                    return GpuArrayFlow::Read;
                }
                if (a == GpuArrayFlow::Defined && b == GpuArrayFlow::Defined) {
                    return GpuArrayFlow::Defined;
                }
                return GpuArrayFlow::Untouched;
            }
            case ASR::stmtType::BlockCall: {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                if (!ASR::is_a<ASR::Block_t>(*b)) return GpuArrayFlow::Read;
                ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                for (auto &item : blk->m_symtab->get_scope()) {
                    GpuSymbolUseFinder f(sym);
                    f.visit_symbol(*item.second);
                    if (f.found) return GpuArrayFlow::Read;
                }
                return run(blk->m_body, blk->n_body, escaped);
            }
            case ASR::stmtType::DoLoop: {
                ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(s);
                if (defines_by_loops(x)) return GpuArrayFlow::Defined;
                return loop(head_uses(x->m_head, sym), x->m_body, x->n_body,
                    x->m_orelse, x->n_orelse);
            }
            case ASR::stmtType::WhileLoop: {
                ASR::WhileLoop_t *x = ASR::down_cast<ASR::WhileLoop_t>(s);
                return loop(expr_uses(x->m_test, sym), x->m_body, x->n_body,
                    x->m_orelse, x->n_orelse);
            }
            case ASR::stmtType::DoConcurrentLoop: {
                ASR::DoConcurrentLoop_t *x =
                    ASR::down_cast<ASR::DoConcurrentLoop_t>(s);
                bool head_names = false;
                for (size_t i = 0; i < x->n_head; i++) {
                    head_names = head_names || head_uses(x->m_head[i], sym);
                }
                for (size_t i = 0; i < x->n_shared; i++) {
                    head_names = head_names || expr_uses(x->m_shared[i], sym);
                }
                for (size_t i = 0; i < x->n_local; i++) {
                    head_names = head_names || expr_uses(x->m_local[i], sym);
                }
                for (size_t i = 0; i < x->n_reduction; i++) {
                    head_names = head_names
                        || expr_uses(x->m_reduction[i].m_arg, sym);
                }
                return loop(head_names, x->m_body, x->n_body, nullptr, 0);
            }
            case ASR::stmtType::OMPRegion: {
                ASR::OMPRegion_t *x = ASR::down_cast<ASR::OMPRegion_t>(s);
                bool head_names = false;
                for (size_t i = 0; i < x->n_clauses; i++) {
                    GpuSymbolUseFinder f(sym);
                    f.visit_omp_clause(*x->m_clauses[i]);
                    head_names = head_names || f.found;
                }
                return loop(head_names, x->m_body, x->n_body, nullptr, 0);
            }
            default: return GpuArrayFlow::Read;
        }
    }
};

// One step of the way from a procedure's statements down to a statement
// nested in them: the statement list that holds the next step, the
// position in it, and whether running that list again is possible
// (it is a loop body).
struct GpuStmtPathStep {
    ASR::stmt_t **body;
    size_t n_body;
    size_t index;
    bool repeats;
};

bool find_stmt_path(ASR::stmt_t **body, size_t n_body, bool repeats,
        const ASR::stmt_t *target, std::vector<GpuStmtPathStep> &path) {
    for (size_t i = 0; i < n_body; i++) {
        ASR::stmt_t *s = body[i];
        path.push_back({body, n_body, i, repeats});
        if (s == target) return true;
        bool found = false;
        switch (s->type) {
            case ASR::stmtType::If: {
                ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                found = find_stmt_path(x->m_body, x->n_body, false, target,
                        path)
                    || find_stmt_path(x->m_orelse, x->n_orelse, false,
                        target, path);
                break;
            }
            case ASR::stmtType::DoLoop: {
                ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(s);
                found = find_stmt_path(x->m_body, x->n_body, true, target,
                    path);
                break;
            }
            case ASR::stmtType::WhileLoop: {
                ASR::WhileLoop_t *x = ASR::down_cast<ASR::WhileLoop_t>(s);
                found = find_stmt_path(x->m_body, x->n_body, true, target,
                    path);
                break;
            }
            case ASR::stmtType::DoConcurrentLoop: {
                ASR::DoConcurrentLoop_t *x =
                    ASR::down_cast<ASR::DoConcurrentLoop_t>(s);
                found = find_stmt_path(x->m_body, x->n_body, true, target,
                    path);
                break;
            }
            case ASR::stmtType::OMPRegion: {
                ASR::OMPRegion_t *x = ASR::down_cast<ASR::OMPRegion_t>(s);
                found = find_stmt_path(x->m_body, x->n_body, true, target,
                    path);
                break;
            }
            case ASR::stmtType::BlockCall: {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                if (ASR::is_a<ASR::Block_t>(*b)) {
                    ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                    found = find_stmt_path(blk->m_body, blk->n_body, false,
                        target, path);
                }
                break;
            }
            case ASR::stmtType::AssociateBlockCall: {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::AssociateBlockCall_t>(s)->m_m);
                if (ASR::is_a<ASR::AssociateBlock_t>(*b)) {
                    ASR::AssociateBlock_t *ab =
                        ASR::down_cast<ASR::AssociateBlock_t>(b);
                    found = find_stmt_path(ab->m_body, ab->n_body, false,
                        target, path);
                }
                break;
            }
            default: break;
        }
        if (found) return true;
        path.pop_back();
    }
    return false;
}

// Whether a statement run after `region` may read the value `sym` held
// when the region ended. `path` leads from the procedure's statements to
// the region. The rest of each enclosing list runs after the region, and
// all of an enclosing loop body may run again; past the outermost list the
// procedure returns, and a local the procedure does not save is gone.
bool read_after_region(ASR::symbol_t *sym,
        const std::vector<GpuStmtPathStep> &path) {
    GpuArrayFlowReader reader(sym);
    for (size_t p = path.size(); p-- > 0;) {
        const GpuStmtPathStep &step = path[p];
        bool escaped = false;
        GpuArrayFlow after = reader.run(step.body + step.index + 1,
            step.n_body - step.index - 1, escaped);
        if (after == GpuArrayFlow::Read) return true;
        if (step.repeats) {
            bool again_escaped = false;
            if (reader.run(step.body, step.n_body, again_escaped)
                    == GpuArrayFlow::Read) {
                return true;
            }
        }
        if (after == GpuArrayFlow::Defined && !escaped) return false;
    }
    return false;
}

} // namespace

// An array of unspecified locality that every iteration of a DO
// CONCURRENT loop defines whole before it references it is a temporary of
// the iteration:
//
//   do concurrent (j = 1:n)
//     do k = 1, m
//       t(k) = x(j) + k
//     end do
//     g(:, j) = t(m:1:-1)
//   end do
//
// Fortran 2018, 11.1.7.5 lets such an iteration reference only the values
// it defined itself, and an array several iterations define is undefined
// once the loop ends. One buffer every thread writes would be a race, so
// the array becomes a local of a BLOCK around the body, just as if the
// source had declared it there; the kernel then gives each thread its own
// copy -- a local array, or a slice of a workspace for a run-time extent.
// The host's array keeps the value it had before the loop.
//
// An array is only made local where that cannot be told apart: every
// path through the body that names it first defines all of it -- by
// an assignment to the whole array, or by a nest of DO loops over its full
// declared extent -- and no statement that can run after the loop may read
// it before it is defined again. It is a local, non-saved variable of
// fixed or explicit shape that no pointer can reach, that no clause of the
// loop and no ASSOCIATE around it names, and that no contained procedure
// reaches by host association. Anything else stays one array, shared by
// all threads, as the source wrote it: distinct elements per iteration
// (`y(i) = ...`) are its common case.
void GpuOffloadVisitor::privatize_iteration_arrays(
        const ASR::OMPRegion_t &region, ParallelLoopNest &work,
        const std::map<ASR::symbol_t*, ASR::expr_t*> &enclosing_assoc_map) {
    // The procedure, or main program, the loop is in.
    SymbolTable *proc_scope = current_scope;
    while (proc_scope && proc_scope->asr_owner &&
            ASR::is_a<ASR::symbol_t>(*proc_scope->asr_owner)) {
        ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(
            proc_scope->asr_owner);
        if (!ASR::is_a<ASR::Block_t>(*owner) &&
                !ASR::is_a<ASR::AssociateBlock_t>(*owner)) {
            break;
        }
        proc_scope = proc_scope->parent;
    }
    if (proc_scope == nullptr || proc_scope->asr_owner == nullptr ||
            !ASR::is_a<ASR::symbol_t>(*proc_scope->asr_owner)) {
        return;
    }
    ASR::symbol_t *proc = ASR::down_cast<ASR::symbol_t>(
        proc_scope->asr_owner);
    ASR::stmt_t **proc_body = nullptr;
    size_t proc_n_body = 0;
    if (ASR::is_a<ASR::Program_t>(*proc)) {
        proc_body = ASR::down_cast<ASR::Program_t>(proc)->m_body;
        proc_n_body = ASR::down_cast<ASR::Program_t>(proc)->n_body;
    } else if (ASR::is_a<ASR::Function_t>(*proc)) {
        proc_body = ASR::down_cast<ASR::Function_t>(proc)->m_body;
        proc_n_body = ASR::down_cast<ASR::Function_t>(proc)->n_body;
    } else {
        return;
    }
    {
        GpuJumpFinder jumps;
        for (size_t i = 0; i < proc_n_body; i++) {
            jumps.visit_stmt(*proc_body[i]);
        }
        if (jumps.found) return;
    }
    std::vector<GpuStmtPathStep> path;
    if (!find_stmt_path(proc_body, proc_n_body, false, &region.base,
            path)) {
        return;
    }

    // The scopes from the loop's own out to the procedure's: a variable
    // declared in one of them is declared outside the loop.
    std::set<SymbolTable*> outer_scopes;
    for (SymbolTable *s = current_scope; s; s = s->parent) {
        outer_scopes.insert(s);
        if (s == proc_scope) break;
    }

    std::set<ASR::symbol_t*> named;
    {
        VarSymbolCollector c(named);
        for (size_t i = 0; i < work.n_body; i++) {
            c.visit_stmt(*work.body[i]);
        }
    }

    std::vector<ASR::symbol_t*> privatized;
    for (ASR::symbol_t *sym : named) {
        if (!ASR::is_a<ASR::Variable_t>(*sym)) continue;
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
        if (!outer_scopes.count(v->m_parent_symtab) ||
                v->m_parent_symtab->asr_owner == nullptr ||
                !ASR::is_a<ASR::symbol_t>(*v->m_parent_symtab->asr_owner)) {
            continue;
        }
        ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(
            v->m_parent_symtab->asr_owner);
        if (ASR::is_a<ASR::AssociateBlock_t>(*owner)) continue;
        if (!ASR::is_a<ASR::Array_t>(*v->m_type) ||
                v->m_intent != ASR::intentType::Local ||
                v->m_storage != ASR::storage_typeType::Default ||
                v->m_symbolic_value || v->m_value || v->m_target_attr ||
                v->m_is_volatile || v->m_common_block || v->n_codims > 0) {
            continue;
        }
        ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(v->m_type);
        ASR::ttype_t *elem = arr->m_type;
        if (!ASR::is_a<ASR::Integer_t>(*elem) &&
                !ASR::is_a<ASR::Real_t>(*elem) &&
                !ASR::is_a<ASR::Complex_t>(*elem) &&
                !ASR::is_a<ASR::Logical_t>(*elem)) {
            continue;
        }
        // The local is declared with the extents the array was: they
        // must still have the values they had when it was created.
        bool fixed_extents = true;
        for (size_t d = 0; d < arr->n_dims; d++) {
            ASR::dimension_t &dim = arr->m_dims[d];
            if (!dim.m_start || !dim.m_length) {
                fixed_extents = false;
                break;
            }
            std::set<ASR::symbol_t*> extent_names;
            VarSymbolCollector c(extent_names);
            c.visit_expr(*dim.m_start);
            c.visit_expr(*dim.m_length);
            for (ASR::symbol_t *e : extent_names) {
                ASR::symbol_t *es = ASRUtils::symbol_get_past_external(e);
                if (!ASR::is_a<ASR::Variable_t>(*es)) {
                    fixed_extents = false;
                    break;
                }
                ASR::Variable_t *ev = ASR::down_cast<ASR::Variable_t>(es);
                if (ev->m_storage != ASR::storage_typeType::Parameter &&
                        ev->m_intent != ASR::intentType::In) {
                    fixed_extents = false;
                    break;
                }
            }
        }
        if (!fixed_extents) continue;

        bool named_outside_body = false;
        for (size_t d = 0; d < work.n_heads(); d++) {
            named_outside_body = named_outside_body
                || head_uses(work.head(d), sym);
        }
        for (size_t i = 0; i < region.n_clauses; i++) {
            GpuSymbolUseFinder f(sym);
            f.visit_omp_clause(*region.m_clauses[i]);
            named_outside_body = named_outside_body || f.found;
        }
        for (auto &assoc : enclosing_assoc_map) {
            named_outside_body = named_outside_body
                || assoc.first == sym || expr_uses(assoc.second, sym);
        }
        if (named_outside_body) continue;

        bool host_associated = false;
        std::function<void(SymbolTable*)> scan_contained =
            [&](SymbolTable *st) {
            for (auto &item : st->get_scope()) {
                if (ASR::is_a<ASR::Function_t>(*item.second)) {
                    GpuSymbolUseFinder f(sym);
                    f.visit_symbol(*item.second);
                    host_associated = host_associated || f.found;
                } else if (ASR::is_a<ASR::Block_t>(*item.second)) {
                    scan_contained(ASR::down_cast<ASR::Block_t>(
                        item.second)->m_symtab);
                }
            }
        };
        scan_contained(proc_scope);
        if (host_associated) continue;

        GpuArrayFlowReader reader(sym);
        bool escaped = false;
        if (reader.run(work.body, work.n_body, escaped)
                == GpuArrayFlow::Read) {
            continue;
        }
        if (read_after_region(sym, path)) continue;
        privatized.push_back(sym);
    }
    if (privatized.empty()) return;
    std::sort(privatized.begin(), privatized.end(),
        [](ASR::symbol_t *a, ASR::symbol_t *b) {
            return std::string(ASRUtils::symbol_name(a))
                < std::string(ASRUtils::symbol_name(b));
        });

    Location loc = region.base.base.loc;
    SymbolTable *block_scope = al.make_new<SymbolTable>(current_scope);
    std::map<ASR::symbol_t*, ASR::expr_t*> local_of;
    for (ASR::symbol_t *sym : privatized) {
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
        ASR::symbol_t *local = gpu_new_variable(al, v->base.base.loc,
            block_scope, block_scope->get_unique_name(v->m_name),
            ASRUtils::duplicate_type(al, v->m_type));
        local_of[sym] = ASRUtils::EXPR(ASR::make_Var_t(al,
            v->base.base.loc, local));
    }
    AssociateVarResolverVisitor redirect(al, local_of);
    for (size_t i = 0; i < work.n_body; i++) {
        redirect.visit_stmt(*work.body[i]);
    }
    std::string block_name = current_scope->get_unique_name(
        "__gpu_iteration_scope");
    ASR::asr_t *block = ASR::make_Block_t(al, loc, block_scope,
        s2c(al, block_name), work.body, work.n_body);
    block_scope->asr_owner = block;
    ASR::symbol_t *block_sym = ASR::down_cast<ASR::symbol_t>(block);
    current_scope->add_symbol(block_name, block_sym);
    kernel_blocks.push_back(block_sym);
    Vec<ASR::stmt_t*> body;
    body.reserve(al, 1);
    body.push_back(al, ASRUtils::STMT(ASR::make_BlockCall_t(al, loc, -1,
        block_sym)));
    work.set_body(body.p, body.n);
}

} // namespace LCompilers
