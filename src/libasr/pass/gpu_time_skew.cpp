#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/gpu_affine.h>
#include <libasr/pass/gpu_loop_utils.h>
#include <libasr/pass/gpu_time_skew.h>
#include <libasr/pass/pass_utils.h>

#include <map>
#include <set>

namespace LCompilers {

/*
 * Time skewing of the parallel loops a sequential loop runs on the device.
 *
 * An explicit time-stepping method runs, in every step, a few parallel
 * loops over the whole domain (the stages of a Runge-Kutta method, a
 * stencil and an update). When the arrays they touch are larger than the
 * device's last-level cache, every loop streams all of them from memory,
 * although each point of a loop only reads the points of the loops before
 * it within a few rows of its own. Run instead on a band of rows that fits
 * in the cache, several iterations of the time loop in a row, the loops
 * find what they read in the cache:
 *
 *     do t0 = 0, n - 1, s                  ! s iterations at a time
 *       do b = 0, nb                       ! band by band, in order
 *         do t = t0, t0 + s - 1            ! the iterations, replayed
 *           ...                            ! the k-th loop launched in
 *                                          ! the replay runs rows
 *                                          ! b R - k sigma .. (b + 1) R
 *                                          ! - 1 - k sigma of its domain
 *
 * The windows of a launch partition the rows (the first band starts at
 * the loop's first row and the last ends at its last row; the windows are
 * computed in integer(8)), so every iteration of every loop runs once, and
 * the order of the iterations of one row is kept. The launch k runs its point x, which accesses the row x + o of an
 * array (o = 0 for a write), in band floor((x + k sigma) / R) (measured
 * from the first row, and clamped to the first and the last band). A point
 * x of the launch k that depends on a point x' of an earlier launch k' < k
 * (the two access the same row r, and one of them writes it) runs in a band
 * not before that of x' when x' + k' sigma <= x + k sigma, that is when
 * o - o' <= (k - k') sigma, which holds when o - o' <= sigma; within a
 * band the launches run in their order. A write after a read (o = 0, o'
 * the read's offset) needs -o' <= sigma, a read after a write (o' = 0)
 * needs o <= sigma: sigma bounds the distance of every read of an array
 * the loops write from the row of the loop, behind as well as ahead. So the
 * pass:
 *
 * * takes a DO loop (or a perfect nest of DO loops of rectangular bounds)
 *   whose body, run on the host, only assigns scalars, tests them (IF,
 *   SELECT CASE) and launches parallel loops on the device: the host
 *   statements can be replayed for each band from saved copies of the
 *   scalars they assign, and read no array the device writes;
 * * requires every array a parallel loop writes to be written at a row
 *   (one subscript) that is the outermost index of the loop, and every
 *   read of such an array to be at most a constant number of rows ahead
 *   of that index and at most a constant number of rows behind it, proved
 *   with the affine analysis (gpu_affine.h) on the body with the pure
 *   functions it calls inlined (a body that calls a function that cannot
 *   be inlined is not skewed, since the function may read arrays the
 *   analysis does not see), under the conditions of the branches around
 *   the read; sigma is the largest such distance (a
 *   lower bound of an array that a subscript is shifted by is assumed to
 *   be 1, which the host checks before skewing);
 * * computes, before the loop, the bytes of the arrays the parallel loops
 *   touch (but those only touched by loops that rarely run) and the bytes
 *   of one of their rows, and skews when they exceed the cache, with bands
 *   of 3/4 of the cache, and keeps the original loop otherwise.
 *
 * Index-set splitting (gpu_index_set_split.cpp) then splits the windowed
 * loops by the bounds of their domain (GpuWindows); within a band that
 * fits in the cache the unroll-and-jam of the split pays.
 */

namespace {

const int64_t max_skew = 16;
const int64_t max_stages = 32;
const int64_t min_band_rows = 8;
const size_t max_dnf = 32;

ASR::symbol_t *root_symbol(ASR::expr_t *e) {
    while (true) {
        if (ASR::is_a<ASR::Var_t>(*e)) {
            return ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(e)->m_v);
        }
        if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
            e = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v;
            continue;
        }
        return nullptr;
    }
}

// Whether `s` is a variable of one of the BLOCKs `blocks`.
bool block_local(ASR::symbol_t *s, const std::set<ASR::symbol_t*> &blocks) {
    SymbolTable *t = ASRUtils::symbol_parent_symtab(s);
    return t && t->asr_owner && ASR::is_a<ASR::symbol_t>(*t->asr_owner)
        && blocks.count(ASR::down_cast<ASR::symbol_t>(t->asr_owner)) > 0;
}

// Whether the host can evaluate an expression in every band of the replay
// to the same value: no array element, no whole array (but its bounds and
// size), no call.
class HostExpr : public ASR::BaseWalkVisitor<HostExpr> {
public:
    bool ok = true;
    void visit_FunctionCall(const ASR::FunctionCall_t &) { ok = false; }
    void visit_ArrayItem(const ASR::ArrayItem_t &) { ok = false; }
    void visit_ArraySection(const ASR::ArraySection_t &) { ok = false; }
    void visit_IntrinsicArrayFunction(const ASR::IntrinsicArrayFunction_t &) {
        ok = false;
    }
    void visit_IntrinsicImpureFunction(
            const ASR::IntrinsicImpureFunction_t &) {
        ok = false;
    }
    void visit_ArraySize(const ASR::ArraySize_t &x) {
        if (x.m_dim) visit_expr(*x.m_dim);
    }
    void visit_ArrayBound(const ASR::ArrayBound_t &x) {
        if (x.m_dim) visit_expr(*x.m_dim);
    }
    void visit_Var(const ASR::Var_t &x) {
        if (ASRUtils::is_array(ASRUtils::expr_type(
                const_cast<ASR::expr_t*>(&x.base)))) {
            ok = false;
        }
    }
    void visit_StructInstanceMember(const ASR::StructInstanceMember_t &x) {
        if (ASRUtils::is_array(x.m_type)) {
            ok = false;
            return;
        }
        visit_expr(*x.m_v);
    }
};

bool host_expr(ASR::expr_t *e) {
    if (e == nullptr) return true;
    HostExpr h;
    h.visit_expr(*e);
    return h.ok;
}

// An element of an array a parallel loop reads or writes, with the affine
// forms of its subscripts and the conditions that hold where it is
// accessed.
struct Access {
    ASR::ArrayItem_t *item;
    bool write;
    std::vector<bool> affine;
    std::vector<GpuAffine> subs;
    GpuDnf facts;
};

// Walks the body of a parallel loop (with the functions it calls inlined)
// in order, collecting its array accesses; a call that was not inlined
// clears `ok`.
class AccessWalk {
public:
    GpuAffineBuilder &b;
    const std::set<ASR::symbol_t*> &single;
    std::vector<Access> accesses;
    // Whole arrays it names outside of an element, a size or a bound.
    std::vector<ASR::expr_t*> wholes;
    bool ok = true;

    AccessWalk(GpuAffineBuilder &b_, const std::set<ASR::symbol_t*> &s)
        : b(b_), single(s) {}

    void access(ASR::ArrayItem_t *x, bool write, const GpuDnf &facts) {
        Access a;
        a.item = x;
        a.write = write;
        a.facts = facts;
        for (size_t d = 0; d < x->n_args; d++) {
            GpuAffine e;
            bool ok_d = x->m_args[d].m_left == nullptr
                && x->m_args[d].m_step == nullptr
                && x->m_args[d].m_right != nullptr
                && b.affine(x->m_args[d].m_right, e);
            a.affine.push_back(ok_d);
            a.subs.push_back(e);
        }
        accesses.push_back(a);
    }

    void expr(ASR::expr_t *e, const GpuDnf &facts) {
        if (e == nullptr) return;
        struct V : public ASR::BaseWalkVisitor<V> {
            AccessWalk *w;
            const GpuDnf *facts;
            void visit_ArrayItem(const ASR::ArrayItem_t &x) {
                w->access(const_cast<ASR::ArrayItem_t*>(&x), false, *facts);
                for (size_t d = 0; d < x.n_args; d++) {
                    if (x.m_args[d].m_left) visit_expr(*x.m_args[d].m_left);
                    if (x.m_args[d].m_right) visit_expr(*x.m_args[d].m_right);
                    if (x.m_args[d].m_step) visit_expr(*x.m_args[d].m_step);
                }
            }
            void visit_ArraySize(const ASR::ArraySize_t &x) {
                if (x.m_dim) visit_expr(*x.m_dim);
            }
            void visit_ArrayBound(const ASR::ArrayBound_t &x) {
                if (x.m_dim) visit_expr(*x.m_dim);
            }
            // A call left after inlining may read arrays by host or use
            // association, which the walk does not see.
            void visit_FunctionCall(const ASR::FunctionCall_t &) {
                w->ok = false;
            }
            void visit_Var(const ASR::Var_t &x) {
                ASR::expr_t *e = const_cast<ASR::expr_t*>(&x.base);
                if (ASRUtils::is_array(ASRUtils::expr_type(e))) {
                    w->wholes.push_back(e);
                }
            }
            void visit_StructInstanceMember(
                    const ASR::StructInstanceMember_t &x) {
                ASR::expr_t *e = const_cast<ASR::expr_t*>(&x.base);
                if (ASRUtils::is_array(x.m_type)) {
                    w->wholes.push_back(e);
                    return;
                }
                visit_expr(*x.m_v);
            }
        } v;
        v.w = this;
        v.facts = &facts;
        v.visit_expr(*e);
    }

    GpuDnf both(const GpuDnf &a, const GpuDnf &c) {
        GpuDnf r = gpu_dnf_and(a, c);
        // Fewer facts only make less provable.
        if (r.size() > max_dnf) return gpu_dnf_true();
        return r;
    }

    void stmts(ASR::stmt_t **body, size_t n, const GpuDnf &facts) {
        for (size_t i = 0; i < n && ok; i++) {
            ASR::stmt_t *s = body[i];
            switch (s->type) {
                case ASR::stmtType::Assignment: {
                    ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(
                        s);
                    expr(a->m_value, facts);
                    if (ASR::is_a<ASR::ArrayItem_t>(*a->m_target)) {
                        ASR::ArrayItem_t *x = ASR::down_cast<ASR::ArrayItem_t>(
                            a->m_target);
                        access(x, true, facts);
                        for (size_t d = 0; d < x->n_args; d++) {
                            expr(x->m_args[d].m_left, facts);
                            expr(x->m_args[d].m_right, facts);
                            expr(x->m_args[d].m_step, facts);
                        }
                    } else if (ASR::is_a<ASR::Var_t>(*a->m_target)
                            && !ASRUtils::is_array(ASRUtils::expr_type(
                                a->m_target))) {
                        ASR::symbol_t *v = ASRUtils::symbol_get_past_external(
                            ASR::down_cast<ASR::Var_t>(a->m_target)->m_v);
                        GpuAffine val;
                        if (single.count(v) && b.affine(a->m_value, val)) {
                            b.bindings[v] = val;
                        }
                    } else {
                        ok = false;
                    }
                    break;
                }
                case ASR::stmtType::If: {
                    ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                    expr(x->m_test, facts);
                    stmts(x->m_body, x->n_body,
                        both(facts, b.condition(x->m_test, true)));
                    stmts(x->m_orelse, x->n_orelse,
                        both(facts, b.condition(x->m_test, false)));
                    break;
                }
                case ASR::stmtType::Select: {
                    ASR::Select_t *x = ASR::down_cast<ASR::Select_t>(s);
                    expr(x->m_test, facts);
                    for (size_t k = 0; k < x->n_body; k++) {
                        if (ASR::is_a<ASR::CaseStmt_t>(*x->m_body[k])) {
                            ASR::CaseStmt_t *c = ASR::down_cast<ASR::CaseStmt_t>(
                                x->m_body[k]);
                            for (size_t q = 0; q < c->n_test; q++) {
                                expr(c->m_test[q], facts);
                            }
                            stmts(c->m_body, c->n_body, facts);
                        } else {
                            ASR::CaseStmt_Range_t *c =
                                ASR::down_cast<ASR::CaseStmt_Range_t>(
                                    x->m_body[k]);
                            expr(c->m_start, facts);
                            expr(c->m_end, facts);
                            stmts(c->m_body, c->n_body, facts);
                        }
                    }
                    stmts(x->m_default, x->n_default, facts);
                    break;
                }
                case ASR::stmtType::BlockCall: {
                    ASR::symbol_t *bs = ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                    if (!ASR::is_a<ASR::Block_t>(*bs)) {
                        ok = false;
                        break;
                    }
                    ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(bs);
                    stmts(blk->m_body, blk->n_body, facts);
                    break;
                }
                default:
                    // A loop inside the body would need its own analysis
                    // of what a variable assigned in it holds.
                    ok = false;
                    break;
            }
        }
    }
};

// What the parallel loops of the time loop do with an array.
struct ArrayInfo {
    ASR::expr_t *designator = nullptr;
    int64_t kind = 0;
    // The dimension of the rows (the outermost loop index), or -1.
    int row = -1;
    bool written = false;
    // Whether a loop that runs in most iterations touches it (not only
    // one of the rarely run loops): the cost model counts these.
    bool hot = false;
};

class Skew {
public:
    Allocator &al;
    SymbolTable *unit_scope;
    const PassOptions &pass_options;
    GpuRarelyRun &rarely_run;
    GpuWindows &windows;
    int skews = 0;

    Skew(Allocator &al_, SymbolTable *unit_scope_,
            const PassOptions &pass_options_, GpuRarelyRun &rarely_run_,
            GpuWindows &windows_)
        : al(al_), unit_scope(unit_scope_), pass_options(pass_options_),
          rarely_run(rarely_run_), windows(windows_) {}

    std::string var_key(ASR::symbol_t *v) {
        return gpu_affine_key(ASRUtils::EXPR(ASR::make_Var_t(al, v->base.loc,
            v)));
    }

    // How each variable in `assigned` is named in the time loop: a
    // variable of a module through its external symbol in this scope.
    std::map<ASR::symbol_t*, ASR::symbol_t*> assigned_names;

    // Whether the statements of the time loop's body can be replayed:
    // scalar assignments, IF and SELECT CASE on host values, and parallel
    // loops assigned to the device, collected in `loops`.
    bool host_body(ASR::stmt_t **body, size_t n,
            std::vector<ASR::stmt_t*> &loops,
            std::set<ASR::symbol_t*> &assigned) {
        for (size_t i = 0; i < n; i++) {
            ASR::stmt_t *s = body[i];
            switch (s->type) {
                case ASR::stmtType::OMPRegion: {
                    GpuLoopNest nest;
                    if (!gpu_loop_nest(s, nest)) return false;
                    loops.push_back(s);
                    break;
                }
                case ASR::stmtType::If: {
                    ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                    if (!host_expr(x->m_test)
                            || !host_body(x->m_body, x->n_body, loops,
                                assigned)
                            || !host_body(x->m_orelse, x->n_orelse, loops,
                                assigned)) {
                        return false;
                    }
                    break;
                }
                case ASR::stmtType::Select: {
                    ASR::Select_t *x = ASR::down_cast<ASR::Select_t>(s);
                    if (!host_expr(x->m_test)) return false;
                    for (size_t k = 0; k < x->n_body; k++) {
                        if (ASR::is_a<ASR::CaseStmt_t>(*x->m_body[k])) {
                            ASR::CaseStmt_t *c = ASR::down_cast<ASR::CaseStmt_t>(
                                x->m_body[k]);
                            for (size_t q = 0; q < c->n_test; q++) {
                                if (!host_expr(c->m_test[q])) return false;
                            }
                            if (!host_body(c->m_body, c->n_body, loops,
                                    assigned)) {
                                return false;
                            }
                        } else {
                            ASR::CaseStmt_Range_t *c =
                                ASR::down_cast<ASR::CaseStmt_Range_t>(
                                    x->m_body[k]);
                            if (!host_expr(c->m_start) || !host_expr(c->m_end)
                                    || !host_body(c->m_body, c->n_body, loops,
                                        assigned)) {
                                return false;
                            }
                        }
                    }
                    if (!host_body(x->m_default, x->n_default, loops,
                            assigned)) {
                        return false;
                    }
                    break;
                }
                case ASR::stmtType::Assignment: {
                    ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(
                        s);
                    if (a->m_overloaded || !ASR::is_a<ASR::Var_t>(
                            *a->m_target)) {
                        return false;
                    }
                    ASR::symbol_t *v = ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::Var_t>(a->m_target)->m_v);
                    if (!ASR::is_a<ASR::Variable_t>(*v)) return false;
                    ASR::ttype_t *t = ASR::down_cast<ASR::Variable_t>(
                        v)->m_type;
                    if (!(ASR::is_a<ASR::Integer_t>(*t)
                            || ASR::is_a<ASR::Real_t>(*t)
                            || ASR::is_a<ASR::Logical_t>(*t))
                            || !host_expr(a->m_value)) {
                        return false;
                    }
                    assigned.insert(v);
                    assigned_names[v] = ASR::down_cast<ASR::Var_t>(
                        a->m_target)->m_v;
                    break;
                }
                default:
                    return false;
            }
        }
        return true;
    }

    // The accesses of one parallel loop's body, with the functions it
    // calls inlined into a copy that `added`/`inl` remove afterwards.
    struct LoopAccesses {
        GpuLoopNest nest;
        bool hot = true;
        std::vector<Access> accesses;
        std::vector<ASR::expr_t*> wholes;
        GpuConstraints bounds;
        std::map<std::string, ASR::expr_t*> atoms;
    };

    bool loop_accesses(SymbolTable *scope, ASR::stmt_t *stmt,
            LoopAccesses &out,
            std::vector<std::pair<SymbolTable*, std::string>> &added,
            std::vector<GpuInliner> &inliners) {
        if (!gpu_loop_nest(stmt, out.nest)) return false;
        out.hot = !rarely_run.count(stmt);
        GpuLoopNest &nest = out.nest;
        ASR::DoLoop_t *inner = nest.innermost();
        if (!gpu_simple_body(inner->m_body, inner->n_body)) return false;
        GpuDuplicator dup(al, scope);
        dup.added = &added;
        Vec<ASR::stmt_t*> body;
        dup.duplicate_body(inner->m_body, inner->n_body, body);
        inliners.emplace_back(al, [&](ASR::FunctionCall_t *call) {
            ASR::symbol_t *f = ASRUtils::symbol_get_past_external(call->m_name);
            return ASR::is_a<ASR::Function_t>(*f) && gpu_function_inlinable(
                ASR::down_cast<ASR::Function_t>(f));
        });
        ASR::stmt_t **bp = body.p;
        size_t bn = body.n;
        if (!dup.ok || !inliners.back().run(scope, bp, bn)) return false;

        // What the body may define; a body that may define something the
        // effects do not name is not skewed.
        GpuEffects effects;
        gpu_collect_effects(bp, bn, effects);
        if (effects.unknown) return false;
        const std::set<ASR::symbol_t*> &assigned = effects.scalars;
        // The scalars the body assigns: private to an iteration (of a
        // BLOCK the body runs, or listed as private), so that replaying a
        // launch changes no host value.
        std::set<ASR::symbol_t*> blocks;
        gpu_collect_blocks(bp, bn, blocks);
        for (ASR::symbol_t *s : assigned) {
            if (nest.position(s) >= 0 || block_local(s, blocks)) continue;
            bool priv = false;
            for (ASR::symbol_t *p : nest.privates) if (p == s) priv = true;
            if (!priv) return false;
        }
        // The variables the body defines exactly once.
        std::set<ASR::symbol_t*> single;
        for (auto &kv : effects.definitions) {
            if (kv.second == 1) single.insert(kv.first);
        }
        GpuAffineBuilder b;
        b.invariant_variable = [&](ASR::symbol_t *s) {
            if (nest.position(s) >= 0) return true;
            return assigned.count(s) == 0 && ASR::is_a<ASR::Variable_t>(*s);
        };
        for (size_t d = 0; d < nest.rank(); d++) {
            GpuAffine lo, hi;
            GpuAffine v = GpuAffine::atom(var_key(nest.vars[d]));
            if (b.affine(nest.starts[d], lo)) {
                out.bounds.push_back(v.plus(lo, -1));
            }
            if (b.affine(nest.ends[d], hi)) {
                out.bounds.push_back(hi.plus(v, -1));
            }
        }
        AccessWalk w(b, single);
        w.stmts(bp, bn, gpu_dnf_true());
        if (!w.ok) return false;
        out.accesses = w.accesses;
        out.wholes = w.wholes;
        out.atoms = b.atoms;
        return true;
    }

    // Whether `key` is the key of an atom `lbound(a, d)`.
    static ASR::expr_t *lower_bound_atom(
            const std::map<std::string, ASR::expr_t*> &atoms,
            const std::string &key) {
        auto it = atoms.find(key);
        if (it == atoms.end()) return nullptr;
        ASR::expr_t *e = it->second;
        if (ASR::is_a<ASR::ArrayBound_t>(*e) && ASR::down_cast<
                ASR::ArrayBound_t>(e)->m_bound
                    == ASR::arrayboundType::LBound) {
            return e;
        }
        return nullptr;
    }

    // The analysis: the arrays the loops touch and sigma, the most rows a
    // read of an array the loops write is ahead of, or behind, the row of
    // its loop; false when the dependences are not of that form.
    bool analyse(SymbolTable *scope, const std::vector<ASR::stmt_t*> &loops,
            std::map<std::string, ArrayInfo> &arrays, int64_t &sigma,
            std::map<std::string, ASR::expr_t*> &pins) {
        std::vector<std::pair<SymbolTable*, std::string>> added;
        std::vector<GpuInliner> inliners;
        inliners.reserve(loops.size());
        std::vector<LoopAccesses> all(loops.size());
        bool ok = true;
        for (size_t i = 0; i < loops.size() && ok; i++) {
            ok = loop_accesses(scope, loops[i], all[i], added, inliners);
        }
        if (ok) ok = dependences(all, arrays, sigma, pins);
        for (auto &inl : inliners) inl.rollback();
        for (auto it = added.rbegin(); it != added.rend(); ++it) {
            if (it->first->get_symbol(it->second)) {
                it->first->erase_symbol(it->second);
            }
        }
        return ok;
    }

    bool note_array(std::map<std::string, ArrayInfo> &arrays,
            ASR::expr_t *designator, bool hot, std::string &key) {
        key = gpu_array_key(designator);
        ASR::symbol_t *root = root_symbol(designator);
        if (key.empty() || root == nullptr) return false;
        ASR::ttype_t *t = ASRUtils::expr_type(designator);
        if (ASRUtils::is_pointer(t) || ASRUtils::is_pointer(
                ASRUtils::symbol_type(root))) {
            return false;
        }
        ArrayInfo &info = arrays[key];
        if (hot) info.hot = true;
        if (info.designator == nullptr) {
            info.designator = designator;
            info.kind = ASRUtils::extract_kind_from_ttype_t(
                ASRUtils::extract_type(t));
        }
        return true;
    }

    bool dependences(std::vector<LoopAccesses> &all,
            std::map<std::string, ArrayInfo> &arrays, int64_t &sigma,
            std::map<std::string, ASR::expr_t*> &pins) {
        // The writes: each at the row of the loop's outermost index.
        for (auto &la : all) {
            GpuAffine v0 = GpuAffine::atom(var_key(la.nest.vars[0]));
            for (auto &a : la.accesses) {
                if (!a.write) continue;
                std::string key;
                if (!note_array(arrays, a.item->m_v, la.hot, key)) {
                    return false;
                }
                ArrayInfo &info = arrays[key];
                if (!info.written) {
                    info.written = true;
                    info.row = -1;
                    for (size_t d = 0; d < a.subs.size(); d++) {
                        if (a.affine[d] && a.subs[d] == v0) {
                            info.row = (int)d;
                            break;
                        }
                    }
                    if (info.row < 0) return false;
                } else if (!a.affine[info.row] || !(a.subs[info.row] == v0)) {
                    return false;
                }
            }
        }
        sigma = 0;
        for (auto &la : all) {
            std::string v0k = var_key(la.nest.vars[0]);
            GpuAffine v0 = GpuAffine::atom(v0k);
            for (ASR::expr_t *e : la.wholes) {
                std::string key;
                if (!note_array(arrays, e, la.hot, key)) return false;
                if (arrays[key].written) return false;
            }
            for (auto &a : la.accesses) {
                std::string key;
                if (!note_array(arrays, a.item->m_v, la.hot, key)) {
                    return false;
                }
                ArrayInfo &info = arrays[key];
                if (!info.written) {
                    // Rows of an array only read: for the cost model.
                    if (info.row < 0) {
                        for (size_t d = 0; d < a.subs.size(); d++) {
                            if (a.affine[d] && a.subs[d].coefficient(v0k)
                                    == 1) {
                                info.row = (int)d;
                                break;
                            }
                        }
                    }
                    continue;
                }
                if (a.write) continue;
                if (!a.affine[info.row]) return false;
                const GpuAffine &e = a.subs[info.row];
                GpuAffine off = e.plus(v0, -1);
                if (off.is_constant()) {
                    sigma = std::max(sigma, std::max(off.constant,
                        -off.constant));
                    continue;
                }
                // Within the facts of the read: v0 + s - e >= 0 (a read
                // at most s rows ahead) and e - v0 + s >= 0 (at most s
                // rows behind).
                GpuConstraints base = la.bounds;
                for (auto &t : e.terms) {
                    ASR::expr_t *lb = lower_bound_atom(la.atoms, t.first);
                    if (lb == nullptr) continue;
                    GpuAffine p = GpuAffine::atom(t.first);
                    base.push_back(p.plus(GpuAffine(-1)));
                    base.push_back(GpuAffine(1).plus(p, -1));
                    pins[t.first] = lb;
                }
                GpuDnf facts;
                for (auto &c : a.facts) {
                    GpuConstraints f = base;
                    f.insert(f.end(), c.begin(), c.end());
                    facts.push_back(f);
                }
                for (int dir = 1; dir >= -1; dir -= 2) {
                    bool found = false;
                    for (int64_t s = -max_skew; s <= max_skew; s++) {
                        GpuAffine goal = v0.plus(e, -1).scaled(dir).plus(
                            GpuAffine(s));
                        if (gpu_affine_proves(facts, goal)) {
                            sigma = std::max(sigma, s);
                            found = true;
                            break;
                        }
                    }
                    if (!found) return false;
                }
            }
        }
        bool any = false;
        for (auto &kv : arrays) if (kv.second.written) any = true;
        return any;
    }

    // Replaces each parallel loop of `copy` (a copy of `orig`) by its run
    // on the window of the band and launch counter, and counts it.
    struct Names {
        ASR::symbol_t *band, *bands, *base, *rows, *launch, *wlo, *whi;
        int64_t sigma;
    };

    void window_loops(ASR::stmt_t **orig, size_t n_orig, ASR::stmt_t **&copy,
            size_t &n_copy, const Names &nm, SymbolTable *fn_scope) {
        LCOMPILERS_ASSERT(n_orig == n_copy);
        Vec<ASR::stmt_t*> out;
        out.reserve(al, n_copy);
        for (size_t i = 0; i < n_copy; i++) {
            ASR::stmt_t *o = orig[i], *s = copy[i];
            switch (s->type) {
                case ASR::stmtType::OMPRegion: {
                    std::vector<ASR::stmt_t*> w = window_loop(o, s, nm,
                        fn_scope);
                    for (auto x : w) out.push_back(al, x);
                    continue;
                }
                case ASR::stmtType::If: {
                    ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                    ASR::If_t *y = ASR::down_cast<ASR::If_t>(o);
                    window_loops(y->m_body, y->n_body, x->m_body, x->n_body,
                        nm, fn_scope);
                    window_loops(y->m_orelse, y->n_orelse, x->m_orelse,
                        x->n_orelse, nm, fn_scope);
                    break;
                }
                case ASR::stmtType::Select: {
                    ASR::Select_t *x = ASR::down_cast<ASR::Select_t>(s);
                    ASR::Select_t *y = ASR::down_cast<ASR::Select_t>(o);
                    for (size_t k = 0; k < x->n_body; k++) {
                        if (ASR::is_a<ASR::CaseStmt_t>(*x->m_body[k])) {
                            ASR::CaseStmt_t *c = ASR::down_cast<ASR::CaseStmt_t>(
                                x->m_body[k]);
                            ASR::CaseStmt_t *d = ASR::down_cast<ASR::CaseStmt_t>(
                                y->m_body[k]);
                            window_loops(d->m_body, d->n_body, c->m_body,
                                c->n_body, nm, fn_scope);
                        } else {
                            ASR::CaseStmt_Range_t *c =
                                ASR::down_cast<ASR::CaseStmt_Range_t>(
                                    x->m_body[k]);
                            ASR::CaseStmt_Range_t *d =
                                ASR::down_cast<ASR::CaseStmt_Range_t>(
                                    y->m_body[k]);
                            window_loops(d->m_body, d->n_body, c->m_body,
                                c->n_body, nm, fn_scope);
                        }
                    }
                    window_loops(y->m_default, y->n_default, x->m_default,
                        x->n_default, nm, fn_scope);
                    break;
                }
                default:
                    break;
            }
            out.push_back(al, s);
        }
        copy = out.p;
        n_copy = out.n;
    }

    std::vector<ASR::stmt_t*> window_loop(ASR::stmt_t *orig, ASR::stmt_t *s,
            const Names &nm, SymbolTable *fn_scope) {
        GpuLoopNest nest;
        bool is_nest = gpu_loop_nest(s, nest);
        LCOMPILERS_ASSERT(is_nest);
        (void)is_nest;
        Location loc = s->base.loc;
        ASRUtils::ASRBuilder bld(al, loc);
        ASRUtils::ExprStmtDuplicator ed(al);
        // The window variables are integer(8), whatever the kinds of the
        // time loop's and of this loop's indices.
        ASR::ttype_t *wt = ASRUtils::symbol_type(nm.band);
        ASR::ttype_t *vt = ASRUtils::symbol_type(nest.vars[0]);
        auto v = [&](ASR::symbol_t *x) { return bld.Var(x); };
        auto c = [&](int64_t x) {
            return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, x,
                ASRUtils::duplicate_type(al, wt),
                ASR::integerbozType::Decimal));
        };
        auto convert = [&](ASR::expr_t *x, ASR::ttype_t *t) {
            return ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(x))
                == ASRUtils::extract_kind_from_ttype_t(t) ? x
                : bld.i2i_t(x, ASRUtils::duplicate_type(al, t));
        };
        auto new_var = [&](const std::string &name) {
            return ASR::down_cast<ASR::Var_t>(bld.Variable(fn_scope,
                fn_scope->get_unique_name(name, false),
                ASRUtils::duplicate_type(al, vt),
                ASR::intentType::Local))->m_v;
        };
        ASR::symbol_t *lo = new_var("__gpu_skew_lo");
        ASR::symbol_t *hi = new_var("__gpu_skew_hi");
        ASR::expr_t *start = nest.starts[0], *end = nest.ends[0];
        // The rows of the window: band * rows - launch * sigma + base ..
        // (band + 1) * rows - 1 - launch * sigma + base, but the first band
        // starts at the domain's first row and the last band ends at its
        // last row, in integer(8), so that they keep every row of the
        // domain; the window lies in the domain, and its ends convert to
        // the loop index's kind exactly.
        auto shift = [&]() { return bld.Mul(v(nm.launch), c(nm.sigma)); };
        auto band_lo = [&]() {
            return bld.Sub(bld.Add(v(nm.base), bld.Mul(v(nm.band),
                v(nm.rows))), shift());
        };
        auto band_hi = [&]() {
            return bld.Sub(bld.Sub(bld.Add(v(nm.base), bld.Mul(bld.Add(
                v(nm.band), c(1)), v(nm.rows))), c(1)), shift());
        };
        std::vector<ASR::stmt_t*> out = {
            bld.Assignment(v(nm.wlo), convert(ed.duplicate_expr(start), wt)),
            bld.If(bld.Gt(v(nm.band), c(0)), {
                bld.If(bld.Gt(band_lo(), v(nm.wlo)),
                    {bld.Assignment(v(nm.wlo), band_lo())}, {})}, {}),
            bld.Assignment(v(nm.whi), convert(ed.duplicate_expr(end), wt)),
            bld.If(bld.Lt(v(nm.band), v(nm.bands)), {
                bld.If(bld.Lt(band_hi(), v(nm.whi)),
                    {bld.Assignment(v(nm.whi), band_hi())}, {})}, {}),
        };
        nest.loops[0]->m_head.m_start = v(lo);
        nest.loops[0]->m_head.m_end = v(hi);
        GpuWindow w;
        w.start = start;
        w.end = end;
        windows[s] = w;
        if (rarely_run.count(orig)) rarely_run.insert(s);
        out.push_back(bld.If(bld.LtE(v(nm.wlo), v(nm.whi)), {
            bld.Assignment(v(lo), convert(v(nm.wlo), vt)),
            bld.Assignment(v(hi), convert(v(nm.whi), vt)), s}, {}));
        out.push_back(bld.Assignment(v(nm.launch), bld.Add(v(nm.launch),
            c(1))));
        return out;
    }

    // The statements that replace the DO loop `stmt`, in `pre`, or false.
    bool try_skew(SymbolTable *scope, ASR::stmt_t *stmt,
            std::vector<ASR::stmt_t*> &pre) {
        if (!ASR::is_a<ASR::DoLoop_t>(*stmt)) return false;
        // The perfect nest of DO loops.
        std::vector<ASR::DoLoop_t*> loops;
        ASR::stmt_t *cur = stmt;
        while (true) {
            ASR::DoLoop_t *l = ASR::down_cast<ASR::DoLoop_t>(cur);
            if (l->n_orelse != 0 || l->m_head.m_v == nullptr
                    || !ASR::is_a<ASR::Var_t>(*l->m_head.m_v)
                    || l->m_head.m_start == nullptr
                    || l->m_head.m_end == nullptr) {
                return false;
            }
            if (l->m_head.m_increment) {
                int64_t inc = 0;
                if (!ASRUtils::extract_value(ASRUtils::expr_value(
                        l->m_head.m_increment), inc) || inc != 1) {
                    return false;
                }
            }
            loops.push_back(l);
            if (l->n_body == 1 && ASR::is_a<ASR::DoLoop_t>(*l->m_body[0])) {
                cur = l->m_body[0];
            } else {
                break;
            }
        }
        ASR::DoLoop_t *inner = loops.back();
        std::vector<ASR::stmt_t*> dev;
        std::set<ASR::symbol_t*> assigned;
        assigned_names.clear();
        if (!host_body(inner->m_body, inner->n_body, dev, assigned)
                || dev.empty()) {
            return false;
        }
        std::vector<ASR::symbol_t*> vars;
        ASR::ttype_t *it = ASRUtils::expr_type(loops[0]->m_head.m_v);
        for (auto l : loops) {
            ASR::symbol_t *v = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(l->m_head.m_v)->m_v);
            ASR::ttype_t *t = ASRUtils::expr_type(l->m_head.m_v);
            if (!ASR::is_a<ASR::Integer_t>(*t)
                    || ASRUtils::extract_kind_from_ttype_t(t)
                        != ASRUtils::extract_kind_from_ttype_t(it)
                    || assigned.count(v)) {
                return false;
            }
            // As the loop names it: a variable of a module through its
            // external symbol.
            vars.push_back(ASR::down_cast<ASR::Var_t>(l->m_head.m_v)->m_v);
        }
        // The bounds: the inner loops' and the parallel loops' rows do not
        // change while the time loop runs.
        std::set<ASR::symbol_t*> changing = assigned;
        for (ASR::symbol_t *x : vars) {
            changing.insert(ASRUtils::symbol_get_past_external(x));
        }
        auto invariant = [&](ASR::expr_t *e) {
            if (!host_expr(e) || !gpu_expr_duplicable(e)) return false;
            std::set<ASR::symbol_t*> syms;
            gpu_collect_symbols(e, syms);
            for (ASR::symbol_t *s : syms) {
                if (changing.count(ASRUtils::symbol_get_past_external(s))) {
                    return false;
                }
            }
            return true;
        };
        for (size_t d = 0; d < loops.size(); d++) {
            if (!host_expr(loops[d]->m_head.m_start)
                    || !host_expr(loops[d]->m_head.m_end)
                    || !gpu_expr_duplicable(loops[d]->m_head.m_start)
                    || !gpu_expr_duplicable(loops[d]->m_head.m_end)) {
                return false;
            }
            if (d > 0 && (!invariant(loops[d]->m_head.m_start)
                    || !invariant(loops[d]->m_head.m_end))) {
                return false;
            }
        }
        for (ASR::stmt_t *s : dev) {
            GpuLoopNest nest;
            gpu_loop_nest(s, nest);
            if (!invariant(nest.starts[0]) || !invariant(nest.ends[0])) {
                return false;
            }
        }
        std::map<std::string, ArrayInfo> arrays;
        std::map<std::string, ASR::expr_t*> pins;
        int64_t sigma = 0;
        if (!analyse(scope, dev, arrays, sigma, pins)) return false;

        SymbolTable *fn_scope = scope;
        while (fn_scope->asr_owner && ASR::is_a<ASR::symbol_t>(
                *fn_scope->asr_owner) && ASR::is_a<ASR::Block_t>(
                    *ASR::down_cast<ASR::symbol_t>(fn_scope->asr_owner))) {
            fn_scope = fn_scope->parent;
        }
        Location loc = stmt->base.loc;
        ASRUtils::ASRBuilder bld(al, loc);
        ASRUtils::ExprStmtDuplicator ed(al);
        ASR::ttype_t *i64 = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 8));
        ASR::ttype_t *lt = ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4));
        auto new_var = [&](const std::string &name, ASR::ttype_t *t) {
            return ASR::down_cast<ASR::Var_t>(bld.Variable(fn_scope,
                fn_scope->get_unique_name(name, false),
                ASRUtils::duplicate_type(al, t),
                ASR::intentType::Local))->m_v;
        };
        auto v = [&](ASR::symbol_t *x) { return bld.Var(x); };
        auto c = [&](int64_t x) {
            return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, x,
                ASRUtils::duplicate_type(al, it),
                ASR::integerbozType::Decimal));
        };
        auto set = [&](ASR::symbol_t *x, ASR::expr_t *e) {
            return bld.Assignment(v(x), e);
        };
        auto to_it = [&](ASR::expr_t *e) {
            return ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(e))
                == ASRUtils::extract_kind_from_ttype_t(it) ? e
                : bld.i2i_t(e, ASRUtils::duplicate_type(al, it));
        };
        auto to_i64 = [&](ASR::expr_t *e) {
            return bld.i2i_t(e, ASRUtils::duplicate_type(al, i64));
        };
        size_t m = loops.size();
        std::vector<ASR::symbol_t*> first(m), trips(m);
        ASR::symbol_t *n = new_var("__gpu_skew_n", it);
        for (size_t d = 0; d < m; d++) {
            first[d] = new_var("__gpu_skew_first", it);
            trips[d] = new_var("__gpu_skew_trips", it);
            pre.push_back(set(first[d],
                ed.duplicate_expr(loops[d]->m_head.m_start)));
            pre.push_back(set(trips[d], bld.Add(bld.Sub(
                ed.duplicate_expr(loops[d]->m_head.m_end),
                ed.duplicate_expr(loops[d]->m_head.m_start)), c(1))));
            pre.push_back(bld.If(bld.Lt(v(trips[d]), c(0)),
                {set(trips[d], c(0))}, {}));
            pre.push_back(set(n, d == 0 ? v(trips[d])
                : bld.Mul(v(n), v(trips[d]))));
        }

        // The cost model, on the host before the loop.
        ASR::symbol_t *on = new_var("__gpu_skew_on", lt);
        ASR::symbol_t *cache = new_var("__gpu_skew_cache", i64);
        ASR::symbol_t *total = new_var("__gpu_skew_bytes", i64);
        ASR::symbol_t *per_row = new_var("__gpu_skew_row_bytes", i64);
        ASR::symbol_t *fixed = new_var("__gpu_skew_fixed_bytes", i64);
        ASR::symbol_t *rows64 = new_var("__gpu_skew_rows64", i64);
        ASR::symbol_t *ext = new_var("__gpu_skew_extent", i64);
        // The bytes of an array, and of one of its rows (all of it for
        // an array without rows).
        auto bytes = [&](const ArrayInfo &a) {
            return bld.Mul(to_i64(bld.ArraySize(ed.duplicate_expr(
                a.designator), nullptr, ASRUtils::duplicate_type(al, i64))),
                bld.i64(a.kind));
        };
        std::vector<ASR::stmt_t*> sizes = {set(total, bld.i64(0)),
            set(per_row, bld.i64(0)), set(fixed, bld.i64(0))};
        for (auto &kv : arrays) {
            const ArrayInfo &a = kv.second;
            if (!a.hot) continue;
            sizes.push_back(set(total, bld.Add(v(total), bytes(a))));
            if (a.row >= 0) {
                sizes.push_back(set(ext, to_i64(bld.ArraySize(
                    ed.duplicate_expr(a.designator), bld.i32(a.row + 1),
                    ASRUtils::duplicate_type(al, i64)))));
                sizes.push_back(bld.If(bld.Lt(v(ext), bld.i64(1)),
                    {set(ext, bld.i64(1))}, {}));
                sizes.push_back(set(per_row, bld.Add(v(per_row),
                    bld.Div(bytes(a), v(ext)))));
            } else {
                sizes.push_back(set(fixed, bld.Add(v(fixed), bytes(a))));
            }
        }
        ASR::expr_t *pinned = nullptr;
        for (auto &p : pins) {
            ASR::expr_t *t = bld.Eq(ed.duplicate_expr(p.second),
                ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 1,
                    ASRUtils::duplicate_type(al, ASRUtils::expr_type(
                        p.second)), ASR::integerbozType::Decimal)));
            pinned = pinned ? bld.And(pinned, t) : t;
        }
        std::vector<ASR::stmt_t*> decide = sizes;
        decide.push_back(set(cache, gpu_cache_bytes(al, unit_scope,
            pass_options, loc)));
        decide.push_back(bld.If(bld.And(bld.Gt(v(cache), bld.i64(0)), bld.And(
                    bld.Gt(v(total), v(cache)), bld.Gt(v(per_row),
                        bld.i64(0)))), {
                set(rows64, bld.Div(bld.Sub(bld.Div(bld.Mul(v(cache),
                    bld.i64(3)), bld.i64(4)), v(fixed)), v(per_row))),
                bld.If(bld.GtE(v(rows64), bld.i64(min_band_rows)), {
                    pinned ? bld.If(pinned, {set(on, bld.bool_t(true, lt))},
                        {}) : set(on, bld.bool_t(true, lt))}, {})}, {}));
        pre.push_back(set(on, bld.bool_t(false, lt)));
        pre.push_back(bld.If(bld.Gt(v(n), c(1)), decide, {}));

        // The skewed loop.
        Names nm;
        // The bands and windows in integer(8): the rows of a parallel loop
        // may not fit in the kind of the time loop's index.
        nm.band = new_var("__gpu_skew_band", i64);
        nm.bands = new_var("__gpu_skew_bands", i64);
        nm.base = new_var("__gpu_skew_base", i64);
        nm.rows = rows64;
        nm.launch = new_var("__gpu_skew_launch", i64);
        nm.wlo = new_var("__gpu_skew_window_lo", i64);
        nm.whi = new_var("__gpu_skew_window_hi", i64);
        nm.sigma = sigma;
        ASR::symbol_t *per = new_var("__gpu_skew_steps", it);
        ASR::symbol_t *t0 = new_var("__gpu_skew_t0", it);
        ASR::symbol_t *kn = new_var("__gpu_skew_count", it);
        ASR::symbol_t *t = new_var("__gpu_skew_t", it);
        ASR::symbol_t *q = new_var("__gpu_skew_q", it);
        std::vector<ASR::symbol_t*> saved_vars;
        for (ASR::symbol_t *s : assigned) {
            saved_vars.push_back(assigned_names.at(s));
        }
        std::vector<ASR::symbol_t*> saves;
        for (ASR::symbol_t *s : saved_vars) {
            s = ASRUtils::symbol_get_past_external(s);
            saves.push_back(new_var(std::string("__gpu_skew_saved_")
                + ASRUtils::symbol_name(s), ASRUtils::symbol_type(s)));
        }
        GpuLoopNest first_nest;
        gpu_loop_nest(dev[0], first_nest);
        auto to_i64_expr = [&](ASR::expr_t *e) {
            ASR::expr_t *x = ed.duplicate_expr(e);
            return ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(x))
                == 8 ? x : to_i64(x);
        };
        std::vector<ASR::stmt_t*> skewed;
        // Launches per band: enough that a band's rows stay ahead of the
        // skew (2 sigma per launch), at most max_stages.
        int64_t s1 = std::max<int64_t>(sigma, 1);
        skewed.push_back(set(ext, bld.Div(v(rows64), bld.i64(2 * s1))));
        skewed.push_back(bld.If(bld.Gt(v(ext), bld.i64(max_stages)),
            {set(ext, bld.i64(max_stages))}, {}));
        skewed.push_back(bld.If(bld.Lt(v(ext), bld.i64(2)),
            {set(ext, bld.i64(2))}, {}));
        skewed.push_back(set(per, to_it(v(ext))));
        skewed.push_back(set(nm.base, to_i64_expr(first_nest.starts[0])));
        skewed.push_back(set(nm.bands, bld.Div(bld.Add(bld.Sub(
            to_i64_expr(first_nest.ends[0]), v(nm.base)), v(rows64)),
            v(rows64))));
        skewed.push_back(bld.If(bld.Lt(v(nm.bands), bld.i64(0)),
            {set(nm.bands, bld.i64(0))}, {}));
        skewed.push_back(set(t0, c(0)));
        // The body, copied, with its parallel loops on windows.
        GpuDuplicator dup(al, scope);
        dup.rarely_run = &rarely_run;
        Vec<ASR::stmt_t*> body;
        dup.duplicate_body(inner->m_body, inner->n_body, body);
        if (!dup.ok) return false;
        ASR::stmt_t **bp = body.p;
        size_t bn = body.n;
        window_loops(inner->m_body, inner->n_body, bp, bn, nm, fn_scope);
        // The indices of the DO loops for the replayed iteration t.
        std::vector<ASR::stmt_t*> iteration = {set(q, v(t))};
        for (size_t d = m; d-- > 0;) {
            if (d == 0) {
                iteration.push_back(set(vars[0], bld.Add(v(first[0]),
                    v(q))));
            } else {
                iteration.push_back(set(vars[d], bld.Add(v(first[d]),
                    bld.Sub(v(q), bld.Mul(bld.Div(v(q), v(trips[d])),
                        v(trips[d]))))));
                iteration.push_back(set(q, bld.Div(v(q), v(trips[d]))));
            }
        }
        for (size_t k = 0; k < bn; k++) iteration.push_back(bp[k]);
        std::vector<ASR::stmt_t*> band_body;
        for (size_t k = 0; k < saved_vars.size(); k++) {
            band_body.push_back(set(saved_vars[k], v(saves[k])));
        }
        band_body.push_back(set(nm.launch, bld.i64(0)));
        band_body.push_back(bld.DoLoop(v(t), v(t0), bld.Sub(bld.Add(v(t0),
            v(kn)), c(1)), iteration));
        std::vector<ASR::stmt_t*> block_body = {
            set(kn, bld.Sub(v(n), v(t0))),
            bld.If(bld.Gt(v(kn), v(per)), {set(kn, v(per))}, {})};
        for (size_t k = 0; k < saved_vars.size(); k++) {
            block_body.push_back(set(saves[k], v(saved_vars[k])));
        }
        block_body.push_back(bld.DoLoop(v(nm.band), bld.i64(0), v(nm.bands),
            band_body));
        block_body.push_back(set(t0, bld.Add(v(t0), v(kn))));
        skewed.push_back(bld.While(bld.Lt(v(t0), v(n)), block_body));
        // The indices after the loops, as the DO loops leave them (every
        // loop ran, since n > 1): one past the last iteration's value with
        // use_loop_variable_after_loop, the last iteration's value
        // otherwise (see PassUtils::replace_doloop).
        for (size_t d = 0; d < m; d++) {
            ASR::expr_t *after = bld.Add(v(first[d]), v(trips[d]));
            if (!pass_options.use_loop_variable_after_loop) {
                after = bld.Sub(after, c(1));
            }
            skewed.push_back(set(vars[d], after));
        }
        pre.push_back(bld.If(v(on), skewed, {stmt}));
        skews++;
        return true;
    }

    void visit_body(SymbolTable *scope, ASR::stmt_t **&body, size_t &n) {
        Vec<ASR::stmt_t*> out;
        out.reserve(al, n);
        for (size_t i = 0; i < n; i++) {
            ASR::stmt_t *s = body[i];
            std::vector<ASR::stmt_t*> r;
            if (try_skew(scope, s, r)) {
                for (ASR::stmt_t *x : r) out.push_back(al, x);
                continue;
            }
            switch (s->type) {
                case ASR::stmtType::DoLoop: {
                    ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(s);
                    visit_body(scope, x->m_body, x->n_body);
                    break;
                }
                case ASR::stmtType::If: {
                    ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                    visit_body(scope, x->m_body, x->n_body);
                    visit_body(scope, x->m_orelse, x->n_orelse);
                    break;
                }
                case ASR::stmtType::WhileLoop: {
                    ASR::WhileLoop_t *x = ASR::down_cast<ASR::WhileLoop_t>(s);
                    visit_body(scope, x->m_body, x->n_body);
                    break;
                }
                case ASR::stmtType::Select: {
                    ASR::Select_t *x = ASR::down_cast<ASR::Select_t>(s);
                    for (size_t k = 0; k < x->n_body; k++) {
                        if (ASR::is_a<ASR::CaseStmt_t>(*x->m_body[k])) {
                            ASR::CaseStmt_t *c = ASR::down_cast<ASR::CaseStmt_t>(
                                x->m_body[k]);
                            visit_body(scope, c->m_body, c->n_body);
                        } else {
                            ASR::CaseStmt_Range_t *c =
                                ASR::down_cast<ASR::CaseStmt_Range_t>(
                                    x->m_body[k]);
                            visit_body(scope, c->m_body, c->n_body);
                        }
                    }
                    visit_body(scope, x->m_default, x->n_default);
                    break;
                }
                case ASR::stmtType::BlockCall: {
                    ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                    if (ASR::is_a<ASR::Block_t>(*b)) {
                        ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                        visit_body(blk->m_symtab, blk->m_body, blk->n_body);
                    }
                    break;
                }
                default:
                    break;
            }
            out.push_back(al, s);
        }
        body = out.p;
        n = out.n;
    }
};

} // namespace

void gpu_time_skew(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options, GpuRarelyRun &rarely_run,
        GpuWindows &windows) {
    if (!gpu_loop_transforms_apply(pass_options)
            || !gpu_loop_transform_enabled(pass_options, GpuLoopSkew)) {
        return;
    }
    Skew s(al, unit.m_symtab, pass_options, rarely_run, windows);
    std::function<void(SymbolTable*)> visit_scope = [&](SymbolTable *st) {
        for (auto &item : st->get_scope()) {
            ASR::symbol_t *sym = item.second;
            if (ASR::is_a<ASR::Function_t>(*sym)) {
                ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(sym);
                s.visit_body(fn->m_symtab, fn->m_body, fn->n_body);
                visit_scope(fn->m_symtab);
            } else if (ASR::is_a<ASR::Program_t>(*sym)) {
                ASR::Program_t *pr = ASR::down_cast<ASR::Program_t>(sym);
                s.visit_body(pr->m_symtab, pr->m_body, pr->n_body);
                visit_scope(pr->m_symtab);
            } else if (ASR::is_a<ASR::Module_t>(*sym)) {
                visit_scope(ASR::down_cast<ASR::Module_t>(sym)->m_symtab);
            }
        }
    };
    visit_scope(unit.m_symtab);
    if (s.skews > 0) {
        // The skewed loop may call the runtime (lfortran_gpu_cache_bytes).
        PassUtils::UpdateDependenciesVisitor u(al);
        u.visit_TranslationUnit(unit);
    }
}

void pass_gpu_time_skew(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options) {
    GpuRarelyRun rarely_run;
    GpuWindows windows;
    gpu_time_skew(al, unit, pass_options, rarely_run, windows);
}

} // namespace LCompilers
