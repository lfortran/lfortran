#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/gpu_affine.h>
#include <libasr/pass/gpu_index_set_split.h>
#include <libasr/pass/gpu_loop_utils.h>

#include <map>
#include <set>

namespace LCompilers {

/*
 * Index-set splitting of parallel loop nests assigned to the device.
 *
 * A stencil evaluates different formulas near the boundaries of its domain:
 * its body (often the bodies of the pure functions it calls) tests the loop
 * indices against the bounds, `if (i == 1) ... else if (i + 2 < n) ...`.
 * Every iteration then runs the tests, and the compiler cannot share loads
 * between the formulas of neighbouring faces or of different quantities,
 * which sit in different branches. Most iterations are far from the
 * boundaries, where every such test has the same value.
 *
 * The pass inlines the pure functions the body calls (gpu_loop_utils.h),
 * finds the tests that compare one loop index with an expression that
 * differs from the loop's lower or upper bound by a small constant, and
 * picks, for each dimension, the smallest number of iterations at each end
 * outside of which all of them have one value. The iterations become two
 * sets, run by one nest:
 *
 * * the interior, over the iterations inside, where every test that the
 *   affine analysis (gpu_affine.h) proves to have one value there is
 *   replaced by the branch it takes: straight-line code;
 * * the frame, the iterations outside the interior, numbered by one index
 *   and mapped back to indices of their own (bands along the first
 *   dimension, then strips along the second), which runs the original body.
 *
 * The frame's iterations are packed as extra rows in front of the
 * interior's outer dimension. They are few, and each runs the long general
 * body; in a launch of their own they would leave the device mostly idle
 * for as long as one of them takes, while in front of the interior they
 * start first and run alongside it. The nest keeps the original loop
 * indices for the interior, so they still decide which index is
 * contiguous in the threads of a launch.
 *
 * When the domain is too small to have an interior the original nest runs
 * instead. Nests of one and two dimensions are split; the tests that remain
 * in the interior (ones the analysis cannot decide there) stay as they are.
 */

namespace {

const int64_t max_offset = 16;
// A body larger than this after inlining is not split.
const int64_t max_body_cost = 200000;

struct Leaf {
    GpuAffine d;  // the compare is about the sign of d
};

class Split {
public:
    Allocator &al;
    int splits = 0;
    Split(Allocator &al_) : al(al_) {}

    // Walks a statement list in order, keeping the affine values of the
    // integer scalars assigned once, and calls `on_if` for every IF.
    void walk(ASR::stmt_t **body, size_t n, GpuAffineBuilder &b,
            const std::set<ASR::symbol_t*> &single,
            const std::function<void(ASR::If_t*, GpuAffineBuilder&)> &on_if) {
        for (size_t i = 0; i < n; i++) {
            ASR::stmt_t *s = body[i];
            switch (s->type) {
                case ASR::stmtType::Assignment: {
                    ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(s);
                    if (ASR::is_a<ASR::Var_t>(*a->m_target)) {
                        ASR::symbol_t *v = ASRUtils::symbol_get_past_external(
                            ASR::down_cast<ASR::Var_t>(a->m_target)->m_v);
                        GpuAffine val;
                        if (single.count(v) && b.affine(a->m_value, val)) {
                            b.bindings[v] = val;
                        }
                    }
                    break;
                }
                case ASR::stmtType::If: {
                    ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                    on_if(x, b);
                    walk(x->m_body, x->n_body, b, single, on_if);
                    walk(x->m_orelse, x->n_orelse, b, single, on_if);
                    break;
                }
                case ASR::stmtType::Select: {
                    ASR::Select_t *x = ASR::down_cast<ASR::Select_t>(s);
                    for (size_t k = 0; k < x->n_body; k++) {
                        if (ASR::is_a<ASR::CaseStmt_t>(*x->m_body[k])) {
                            ASR::CaseStmt_t *c = ASR::down_cast<ASR::CaseStmt_t>(
                                x->m_body[k]);
                            walk(c->m_body, c->n_body, b, single, on_if);
                        } else {
                            ASR::CaseStmt_Range_t *c =
                                ASR::down_cast<ASR::CaseStmt_Range_t>(
                                    x->m_body[k]);
                            walk(c->m_body, c->n_body, b, single, on_if);
                        }
                    }
                    walk(x->m_default, x->n_default, b, single, on_if);
                    break;
                }
                case ASR::stmtType::BlockCall: {
                    ASR::symbol_t *bs = ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                    if (ASR::is_a<ASR::Block_t>(*bs)) {
                        ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(bs);
                        walk(blk->m_body, blk->n_body, b, single, on_if);
                    }
                    break;
                }
                default:
                    break;
            }
        }
    }

    static void leaves(ASR::expr_t *cond, GpuAffineBuilder &b,
            std::vector<Leaf> &out) {
        if (ASR::is_a<ASR::LogicalNot_t>(*cond)) {
            leaves(ASR::down_cast<ASR::LogicalNot_t>(cond)->m_arg, b, out);
        } else if (ASR::is_a<ASR::LogicalBinOp_t>(*cond)) {
            ASR::LogicalBinOp_t *x = ASR::down_cast<ASR::LogicalBinOp_t>(cond);
            leaves(x->m_left, b, out);
            leaves(x->m_right, b, out);
        } else if (ASR::is_a<ASR::IntegerCompare_t>(*cond)) {
            ASR::IntegerCompare_t *c = ASR::down_cast<ASR::IntegerCompare_t>(
                cond);
            GpuAffine l, r;
            if (b.affine(c->m_left, l) && b.affine(c->m_right, r)) {
                out.push_back({l.plus(r, -1)});
            }
        }
    }

    // The value a condition has everywhere on the facts: 1 true, 0 false,
    // -1 not known.
    static int constant_value(ASR::expr_t *cond, GpuAffineBuilder &b,
            const GpuConstraints &facts) {
        GpuDnf when_false = b.condition(cond, false);
        bool always_true = true;
        for (auto &c : when_false) {
            GpuConstraints cs = facts;
            cs.insert(cs.end(), c.begin(), c.end());
            if (!gpu_affine_infeasible(cs)) {
                always_true = false;
                break;
            }
        }
        if (always_true) return 1;
        GpuDnf when_true = b.condition(cond, true);
        for (auto &c : when_true) {
            GpuConstraints cs = facts;
            cs.insert(cs.end(), c.begin(), c.end());
            if (!gpu_affine_infeasible(cs)) return -1;
        }
        return 0;
    }

    // Replaces each IF whose test has one value on the facts by the
    // statements of the branch it takes.
    int fold(ASR::stmt_t **&body, size_t &n, GpuAffineBuilder &b,
            const std::set<ASR::symbol_t*> &single,
            const GpuConstraints &facts, bool apply) {
        int folded = 0;
        Vec<ASR::stmt_t*> out;
        out.reserve(al, n);
        for (size_t i = 0; i < n; i++) {
            ASR::stmt_t *s = body[i];
            switch (s->type) {
                case ASR::stmtType::Assignment: {
                    ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(s);
                    if (ASR::is_a<ASR::Var_t>(*a->m_target)) {
                        ASR::symbol_t *v = ASRUtils::symbol_get_past_external(
                            ASR::down_cast<ASR::Var_t>(a->m_target)->m_v);
                        GpuAffine val;
                        if (single.count(v) && b.affine(a->m_value, val)) {
                            b.bindings[v] = val;
                        }
                    }
                    out.push_back(al, s);
                    break;
                }
                case ASR::stmtType::If: {
                    ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                    int v = constant_value(x->m_test, b, facts);
                    if (v == -1) {
                        folded += fold(x->m_body, x->n_body, b, single, facts,
                            apply);
                        folded += fold(x->m_orelse, x->n_orelse, b, single,
                            facts, apply);
                        out.push_back(al, s);
                        break;
                    }
                    folded++;
                    ASR::stmt_t **bb = v ? x->m_body : x->m_orelse;
                    size_t bn = v ? x->n_body : x->n_orelse;
                    folded += fold(bb, bn, b, single, facts, apply);
                    if (apply) {
                        for (size_t k = 0; k < bn; k++) out.push_back(al, bb[k]);
                    } else {
                        out.push_back(al, s);
                    }
                    break;
                }
                case ASR::stmtType::Select: {
                    ASR::Select_t *x = ASR::down_cast<ASR::Select_t>(s);
                    for (size_t k = 0; k < x->n_body; k++) {
                        if (ASR::is_a<ASR::CaseStmt_t>(*x->m_body[k])) {
                            ASR::CaseStmt_t *c = ASR::down_cast<ASR::CaseStmt_t>(
                                x->m_body[k]);
                            folded += fold(c->m_body, c->n_body, b, single,
                                facts, apply);
                        } else {
                            ASR::CaseStmt_Range_t *c =
                                ASR::down_cast<ASR::CaseStmt_Range_t>(
                                    x->m_body[k]);
                            folded += fold(c->m_body, c->n_body, b, single,
                                facts, apply);
                        }
                    }
                    folded += fold(x->m_default, x->n_default, b, single, facts,
                        apply);
                    out.push_back(al, s);
                    break;
                }
                case ASR::stmtType::BlockCall: {
                    ASR::symbol_t *bs = ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                    if (ASR::is_a<ASR::Block_t>(*bs)) {
                        ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(bs);
                        folded += fold(blk->m_body, blk->n_body, b, single,
                            facts, apply);
                    }
                    out.push_back(al, s);
                    break;
                }
                default:
                    out.push_back(al, s);
                    break;
            }
        }
        if (apply) {
            body = out.p;
            n = out.n;
        }
        return folded;
    }

    // Whether the body, or a function it calls, has an IF.
    static bool has_test(ASR::stmt_t **body, size_t n, int depth) {
        struct Find : public ASRUtils::BlockBodyWalkVisitor<Find> {
            int depth;
            bool found = false;
            void visit_If(const ASR::If_t &) { found = true; }
            void visit_FunctionCall(const ASR::FunctionCall_t &x) {
                ASR::symbol_t *f = ASRUtils::symbol_get_past_external(
                    x.m_name);
                if (depth < 8 && ASR::is_a<ASR::Function_t>(*f)) {
                    ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(f);
                    if (has_test(fn->m_body, fn->n_body, depth + 1)) {
                        found = true;
                    }
                }
                ASRUtils::BlockBodyWalkVisitor<Find>::visit_FunctionCall(x);
            }
        } f;
        f.depth = depth;
        for (size_t i = 0; i < n && !f.found; i++) f.visit_stmt(*body[i]);
        return f.found;
    }

    std::string var_key(ASR::symbol_t *v) {
        return gpu_affine_key(ASRUtils::EXPR(ASR::make_Var_t(al, v->base.loc,
            v)));
    }

    ASR::stmt_t *try_split(SymbolTable *scope, ASR::stmt_t *stmt) {
        GpuLoopNest nest;
        if (!gpu_loop_nest(stmt, nest)) return nullptr;
        size_t rank = nest.rank();
        if (rank < 1 || rank > 2) return nullptr;
        ASR::DoLoop_t *inner = nest.innermost();
        if (!gpu_simple_body(inner->m_body, inner->n_body)
                || !has_test(inner->m_body, inner->n_body, 0)) {
            return nullptr;
        }
        for (size_t d = 0; d < rank; d++) {
            if (!gpu_expr_duplicable(nest.starts[d])
                    || !gpu_expr_duplicable(nest.ends[d])) {
                return nullptr;
            }
        }

        // The body, copied, with the pure functions it calls inlined.
        GpuDuplicator dup(al, scope);
        std::vector<std::pair<SymbolTable*, std::string>> added;
        dup.added = &added;
        Vec<ASR::stmt_t*> body;
        dup.duplicate_body(inner->m_body, inner->n_body, body);
        GpuInliner inl(al, [&](ASR::FunctionCall_t *call) {
            ASR::symbol_t *f = ASRUtils::symbol_get_past_external(call->m_name);
            return ASR::is_a<ASR::Function_t>(*f) && gpu_function_inlinable(
                ASR::down_cast<ASR::Function_t>(f));
        });
        auto rollback = [&]() -> ASR::stmt_t* {
            inl.rollback();
            for (auto it = added.rbegin(); it != added.rend(); ++it) {
                if (it->first->get_symbol(it->second)) {
                    it->first->erase_symbol(it->second);
                }
            }
            return nullptr;
        };
        ASR::stmt_t **bp = body.p;
        size_t bn = body.n;
        if (!dup.ok || !inl.run(scope, bp, bn)) return rollback();
        if (gpu_stmts_cost(bp, bn) > max_body_cost) return rollback();

        // What the body assigns, and the scalars it assigns once.
        GpuEffects effects;
        gpu_collect_effects(bp, bn, effects);
        // A statement whose effects are not modelled may define any scalar.
        if (effects.unknown) return rollback();
        std::set<ASR::symbol_t*> &assigned = effects.scalars;
        std::map<ASR::symbol_t*, int> count;
        struct Count : public ASRUtils::BlockBodyWalkVisitor<Count> {
            std::map<ASR::symbol_t*, int> *count;
            void visit_Assignment(const ASR::Assignment_t &x) {
                if (ASR::is_a<ASR::Var_t>(*x.m_target)) {
                    (*count)[ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::Var_t>(x.m_target)->m_v)]++;
                }
            }
            void visit_DoLoop(const ASR::DoLoop_t &x) {
                if (x.m_head.m_v && ASR::is_a<ASR::Var_t>(*x.m_head.m_v)) {
                    (*count)[ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::Var_t>(x.m_head.m_v)->m_v)] += 2;
                }
                ASRUtils::BlockBodyWalkVisitor<Count>::visit_DoLoop(x);
            }
        } cnt;
        cnt.count = &count;
        for (size_t k = 0; k < bn; k++) cnt.visit_stmt(*bp[k]);
        std::set<ASR::symbol_t*> single;
        for (auto &kv : count) if (kv.second == 1) single.insert(kv.first);
        auto invariant = [&](ASR::symbol_t *s) {
            if (nest.position(s) >= 0) return true;
            return assigned.count(s) == 0 && ASR::is_a<ASR::Variable_t>(*s);
        };
        GpuAffineBuilder base;
        base.invariant_variable = invariant;
        std::vector<GpuAffine> lo(rank), hi(rank), v(rank);
        for (size_t d = 0; d < rank; d++) {
            if (!base.affine(nest.starts[d], lo[d])
                    || !base.affine(nest.ends[d], hi[d])) {
                return rollback();
            }
            v[d] = GpuAffine::atom(var_key(nest.vars[d]));
        }

        // The iterations at each end of each dimension where a test
        // compares the index with a bound.
        std::vector<int64_t> a(rank, 0), b(rank, 0);
        {
            GpuAffineBuilder wb = base;
            walk(bp, bn, wb, single, [&](ASR::If_t *x, GpuAffineBuilder &bb) {
                std::vector<Leaf> ls;
                leaves(x->m_test, bb, ls);
                for (auto &l : ls) {
                    int dim = -1;
                    int64_t coef = 0;
                    bool other = false;
                    for (size_t d = 0; d < rank; d++) {
                        int64_t c = l.d.coefficient(var_key(nest.vars[d]));
                        if (c == 0) continue;
                        if (dim >= 0) other = true;
                        dim = (int)d;
                        coef = c;
                    }
                    if (dim < 0 || other || (coef != 1 && coef != -1)) {
                        continue;
                    }
                    // c v + rest switches at v = -rest / c.
                    GpuAffine rest = l.d;
                    rest.terms.erase(var_key(nest.vars[dim]));
                    GpuAffine sw = rest.scaled(-coef);
                    GpuAffine from_lo = sw.plus(lo[dim], -1);
                    GpuAffine from_hi = hi[dim].plus(sw, -1);
                    // The nearer end, when both are a constant away.
                    bool near_lo = from_lo.is_constant()
                        && from_lo.constant >= 0
                        && from_lo.constant <= max_offset;
                    bool near_hi = from_hi.is_constant()
                        && from_hi.constant >= 0
                        && from_hi.constant <= max_offset;
                    if (near_lo && near_hi) {
                        if (from_lo.constant <= from_hi.constant) {
                            near_hi = false;
                        } else {
                            near_lo = false;
                        }
                    }
                    if (near_lo) {
                        a[dim] = std::max(a[dim], from_lo.constant + 1);
                    } else if (near_hi) {
                        b[dim] = std::max(b[dim], from_hi.constant + 1);
                    }
                }
            });
        }
        bool any = false;
        for (size_t d = 0; d < rank; d++) if (a[d] || b[d]) any = true;
        if (!any) return rollback();

        auto interior_facts = [&](const std::vector<int64_t> &aa,
                const std::vector<int64_t> &bb) {
            GpuConstraints f;
            for (size_t d = 0; d < rank; d++) {
                f.push_back(v[d].plus(lo[d], -1).plus(GpuAffine(-aa[d])));
                f.push_back(hi[d].plus(v[d], -1).plus(GpuAffine(-bb[d])));
            }
            return f;
        };
        auto count_folds = [&](const std::vector<int64_t> &aa,
                const std::vector<int64_t> &bb) {
            // An interior that is empty for every value of the bounds folds
            // everything and is of no use.
            if (gpu_affine_infeasible(interior_facts(aa, bb))) return 0;
            GpuAffineBuilder fb = base;
            ASR::stmt_t **p = bp;
            size_t pn = bn;
            return fold(p, pn, fb, single, interior_facts(aa, bb), false);
        };
        int best = count_folds(a, b);
        if (best == 0) return rollback();
        // The thinnest frame that folds as much.
        for (size_t d = 0; d < rank; d++) {
            while (a[d] > 0) {
                std::vector<int64_t> t = a;
                t[d]--;
                if (count_folds(t, b) < best) break;
                a = t;
            }
            while (b[d] > 0) {
                std::vector<int64_t> t = b;
                t[d]--;
                if (count_folds(a, t) < best) break;
                b = t;
            }
        }
        {
            GpuAffineBuilder fb = base;
            fold(bp, bn, fb, single, interior_facts(a, b), true);
        }
        // The BLOCKs of the branches that went are not run anymore.
        {
            std::set<ASR::symbol_t*> used;
            gpu_collect_blocks(bp, bn, used);
            std::vector<std::pair<SymbolTable*, std::string>> all = added;
            all.insert(all.end(), inl.added.begin(), inl.added.end());
            for (auto &x : all) {
                ASR::symbol_t *sym = x.first->get_symbol(x.second);
                if (sym && ASR::is_a<ASR::Block_t>(*sym) && !used.count(sym)) {
                    x.first->erase_symbol(x.second);
                }
            }
        }

        // The statements.
        Location loc = stmt->base.loc;
        ASRUtils::ASRBuilder bld(al, loc);
        ASRUtils::ExprStmtDuplicator ed(al);
        auto c = [&](int64_t x) { return gpu_i32(al, loc, x); };
        auto add = [&](ASR::expr_t *x, ASR::expr_t *y) {
            return gpu_int_add(al, x, y);
        };
        auto sub = [&](ASR::expr_t *x, ASR::expr_t *y) {
            return gpu_int_sub(al, x, y);
        };
        auto mul = [&](ASR::expr_t *x, ASR::expr_t *y) {
            return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc, x,
                ASR::binopType::Mul, y, ASRUtils::expr_type(x), nullptr));
        };
        auto div = [&](ASR::expr_t *x, ASR::expr_t *y) {
            return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc, x,
                ASR::binopType::Div, y, ASRUtils::expr_type(x), nullptr));
        };
        auto lo_e = [&](size_t d) { return ed.duplicate_expr(nest.starts[d]); };
        auto hi_e = [&](size_t d) { return ed.duplicate_expr(nest.ends[d]); };
        // extent(d) = hi - lo + 1
        auto extent = [&](size_t d) { return add(sub(hi_e(d), lo_e(d)), c(1)); };
        // The interior: is(d) .. ie(d).
        auto is = [&](size_t d) { return add(lo_e(d), c(a[d])); };
        auto ie = [&](size_t d) { return sub(hi_e(d), c(b[d])); };

        // The frame: one index t over the iterations outside the interior,
        // mapped to indices of their own.
        SymbolTable *fn_scope = scope;
        while (fn_scope->asr_owner && ASR::is_a<ASR::symbol_t>(
                *fn_scope->asr_owner) && ASR::is_a<ASR::Block_t>(
                    *ASR::down_cast<ASR::symbol_t>(fn_scope->asr_owner))) {
            fn_scope = fn_scope->parent;
        }
        auto new_index = [&](const std::string &name, ASR::ttype_t *type) {
            return ASR::down_cast<ASR::Var_t>(bld.Variable(fn_scope,
                fn_scope->get_unique_name(name, false),
                ASRUtils::duplicate_type(al, type),
                ASR::intentType::Local))->m_v;
        };
        ASR::symbol_t *tsym = new_index("__gpu_frame",
            ASRUtils::expr_type(nest.starts[0]));
        auto t = [&]() { return bld.Var(tsym); };
        std::vector<ASR::symbol_t*> fvars;
        for (size_t d = 0; d < rank; d++) {
            fvars.push_back(new_index(std::string("__gpu_frame_")
                + ASRUtils::symbol_name(nest.vars[d]),
                ASRUtils::symbol_type(nest.vars[d])));
        }
        auto assign = [&](size_t d, ASR::expr_t *val) {
            return bld.Assignment(bld.Var(fvars[d]), val);
        };
        std::vector<ASR::stmt_t*> decode;
        ASR::expr_t *count_e;
        if (rank == 1) {
            // t < a: lo + t, else hi - b + 1 + (t - a)
            count_e = c(a[0] + b[0]);
            decode.push_back(bld.If(bld.Lt(t(), c(a[0])),
                {assign(0, add(lo_e(0), t()))},
                {assign(0, add(sub(add(hi_e(0), c(1)), c(b[0])),
                    sub(t(), c(a[0]))))}));
        } else {
            // Bands of whole rows of the inner dimension at both ends of
            // the outer one, then strips of the inner dimension.
            ASR::expr_t *ni0 = sub(extent(0), c(a[0] + b[0]));
            count_e = add(mul(c(a[0] + b[0]), extent(1)),
                mul(sub(extent(0), c(a[0] + b[0])), c(a[1] + b[1])));
            auto row_col = [&](ASR::expr_t *tt, ASR::expr_t *row0) {
                // v0 = row0 + tt / w, v1 = lo1 + tt - (tt / w) * w
                return std::vector<ASR::stmt_t*>{
                    assign(0, add(row0, div(tt, extent(1)))),
                    assign(1, add(lo_e(1), sub(ed.duplicate_expr(tt),
                        mul(div(ed.duplicate_expr(tt), extent(1)),
                            extent(1)))))};
            };
            ASR::expr_t *band0 = mul(c(a[0]), extent(1));
            ASR::expr_t *bands = mul(c(a[0] + b[0]), extent(1));
            std::vector<ASR::stmt_t*> top = row_col(t(), lo_e(0));
            ASR::expr_t *t2 = sub(t(), mul(c(a[0]), extent(1)));
            std::vector<ASR::stmt_t*> bottom = row_col(t2,
                add(sub(hi_e(0), c(b[0])), c(1)));
            // Strips: t3 = t - bands; v0 = lo0 + a0 + t3 mod ni0; the
            // column c = t3 / ni0 is one of the a1 + b1 strips.
            ASR::expr_t *t3 = sub(t(), ed.duplicate_expr(bands));
            ASR::expr_t *col = div(ed.duplicate_expr(t3),
                ed.duplicate_expr(ni0));
            std::vector<ASR::stmt_t*> strips;
            strips.push_back(assign(0, add(add(lo_e(0), c(a[0])),
                sub(ed.duplicate_expr(t3), mul(ed.duplicate_expr(col),
                    ed.duplicate_expr(ni0))))));
            strips.push_back(bld.If(bld.Lt(ed.duplicate_expr(col), c(a[1])),
                {assign(1, add(lo_e(1), ed.duplicate_expr(col)))},
                {assign(1, add(add(sub(hi_e(1), c(b[1])), c(1)),
                    sub(ed.duplicate_expr(col), c(a[1]))))}));
            decode.push_back(bld.If(bld.Lt(t(), band0), top,
                {bld.If(bld.Lt(t(), ed.duplicate_expr(bands)),
                    bottom, strips)}));
        }
        // The frame runs the original body on its own indices.
        std::vector<ASR::stmt_t*> run = decode;
        {
            GpuDuplicator d2(al, scope);
            d2.added = &added;
            for (size_t d = 0; d < rank; d++) {
                d2.symbol_map[nest.vars[d]] = fvars[d];
            }
            Vec<ASR::stmt_t*> orig;
            d2.duplicate_body(inner->m_body, inner->n_body, orig);
            if (!d2.ok) return rollback();
            for (size_t k = 0; k < orig.n; k++) run.push_back(orig[k]);
        }

        // One launch: the frame's iterations come first, as extra rows of
        // the interior's outer dimension (whole rows of its inner one), so
        // that they start first and run alongside the interior instead of
        // after it. The nest keeps the original indices, which are those of
        // the interior: they decide how the iterations map to threads.
        auto width = [&]() {
            return rank == 1 ? c(1) : add(sub(ie(1), is(1)), c(1));
        };
        // The rows of frame iterations: ceil(count / width).
        auto first_row = [&]() {
            return sub(is(0), div(add(ed.duplicate_expr(count_e),
                sub(width(), c(1))), width()));
        };
        std::vector<ASR::expr_t*> ms, me;
        for (size_t d = 0; d < rank; d++) {
            ms.push_back(d == 0 ? first_row() : is(d));
            me.push_back(ie(d));
        }
        // t = (v0 - first row) * width + (v1 - interior start)
        ASR::expr_t *tv = mul(sub(bld.Var(nest.vars[0]), first_row()),
            width());
        if (rank == 2) tv = add(tv, sub(bld.Var(nest.vars[1]), is(1)));
        std::vector<ASR::stmt_t*> frame_part = {bld.Assignment(t(), tv),
            bld.If(bld.Lt(t(), ed.duplicate_expr(count_e)), run, {})};
        std::vector<ASR::stmt_t*> interior_part(bp, bp + bn);
        Vec<ASR::stmt_t*> mbody;
        mbody.reserve(al, 1);
        mbody.push_back(al, bld.If(bld.Lt(bld.Var(nest.vars[0]), is(0)),
            frame_part, interior_part));
        std::vector<ASR::symbol_t*> privates(fvars.begin(), fvars.end());
        privates.push_back(tsym);
        ASR::stmt_t *merged = gpu_loop_nest_with_body(al, nest, ms, me,
            mbody, privates);

        // Split only when the domain has an interior.
        ASR::expr_t *has_interior = nullptr;
        for (size_t d = 0; d < rank; d++) {
            ASR::expr_t *t1 = bld.Gt(extent(d), c(a[d] + b[d]));
            has_interior = has_interior ? bld.And(has_interior, t1) : t1;
        }
        splits++;
        return bld.If(has_interior, {merged}, {stmt});
    }

    void visit_body(SymbolTable *scope, ASR::stmt_t **&body, size_t &n) {
        for (size_t i = 0; i < n; i++) {
            ASR::stmt_t *s = body[i];
            ASR::stmt_t *r = try_split(scope, s);
            if (r) {
                body[i] = r;
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
        }
    }
};

} // namespace

void pass_gpu_index_set_split(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options) {
    if (!gpu_loop_transforms_apply(pass_options)
            || !gpu_loop_transform_enabled(pass_options, GpuLoopSplit)) {
        return;
    }
    Split s(al);
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
}

} // namespace LCompilers
