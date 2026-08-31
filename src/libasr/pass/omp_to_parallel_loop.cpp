#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/diagnostics.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/replace_omp_to_parallel_loop.h>
#include <libasr/pass/stmt_walk_visitor.h>

#include <set>

namespace LCompilers {

/*
 * Normalizes an OpenMP parallel loop into a `DoConcurrentLoop`.
 *
 * An `!$omp target teams distribute parallel do` region, an `!$omp parallel
 * do` region and a `do concurrent` loop describe the same thing: an iteration
 * space, the data it reads and writes, and the reductions it performs.
 * `DoConcurrentLoop` already carries all three, so a region is rewritten into
 * one and every lowering below -- the serial loop, the host threads, and the
 * whole GPU offload pipeline -- serves all three constructs without knowing
 * which one the loop came from.
 *
 * Converts:
 *      !$omp target map(tofrom: a, b)
 *      !$omp teams
 *      !$omp distribute parallel do
 *      do i = 1, n
 *          a(i) = b(i)
 *      end do
 *
 * to:
 *      do concurrent (i = 1:n) shared(a, b)
 *          a(i) = b(i)
 *      end do
 *
 * The loop is left `ExecAuto`; the dispatch pass that runs next decides who
 * runs its iterations.
 *
 * A `target` region is always normalized, because no lowering below this pass
 * knows what a target region is. A loop construct that does not mention a
 * device is only normalized when the compiler was asked to offload those too,
 * since `!$omp parallel do` asks for host threads and the OpenMP pass already
 * lowers the full clause set onto them.
 *
 * A region this pass declines is left exactly as it was, so the OpenMP pass
 * still gets its chance at it. `pass_flatten_omp_regions` below runs after
 * that pass and unwraps whatever is left, so an unsupported construct runs
 * serially rather than reaching a code generator that cannot lower it.
 */

// The constructs that may sit between the outermost region and the loop it
// runs. `Teams` and `Parallel` only open a data environment, so they are
// peeled but do not on their own make the loop below them a parallel one;
// the rest distribute the iterations and do.
static bool is_parallel_loop_region(ASR::omp_region_typeType region) {
    switch (region) {
        case ASR::omp_region_typeType::Teams:
        case ASR::omp_region_typeType::Parallel:
        case ASR::omp_region_typeType::Do:
        case ASR::omp_region_typeType::Simd:
        case ASR::omp_region_typeType::ParallelDo:
        case ASR::omp_region_typeType::Distribute:
        case ASR::omp_region_typeType::TeamsDistribute:
        case ASR::omp_region_typeType::DistributeParallelDo:
            return true;
        default:
            return false;
    }
}

static bool distributes_iterations(ASR::omp_region_typeType region) {
    switch (region) {
        case ASR::omp_region_typeType::Do:
        case ASR::omp_region_typeType::Simd:
        case ASR::omp_region_typeType::ParallelDo:
        case ASR::omp_region_typeType::Distribute:
        case ASR::omp_region_typeType::TeamsDistribute:
        case ASR::omp_region_typeType::DistributeParallelDo:
            return true;
        default:
            return false;
    }
}

// Whether an OpenMP region is left anywhere under a statement. A region
// inside the loop body -- a `critical` section, a `barrier` -- says the
// iterations synchronize with each other, which is the opposite of what a
// concurrent loop asserts, so such a loop is not normalized.
class NestedRegionFinder : public ASR::BaseWalkVisitor<NestedRegionFinder>
{
public:
    bool found = false;

    void visit_OMPRegion(const ASR::OMPRegion_t &x) {
        found = true;
        ASR::BaseWalkVisitor<NestedRegionFinder>::visit_OMPRegion(x);
    }
};

// Unwraps every OpenMP region of a subtree, leaving the statements they
// contained. This runs after the OpenMP pass, so a region reaching it is one
// no lowering claimed; without this it would reach a code generator that has
// no lowering for it either.
class OMPRegionFlattener : public ASR::StatementWalkVisitor<OMPRegionFlattener>
{
public:
    const PassOptions &pass_options;

    OMPRegionFlattener(Allocator &al, const PassOptions &pass_options_) :
        StatementWalkVisitor(al), pass_options(pass_options_) {
    }

    // A region that asserts its iterations are independent of one another
    // computes the same result whatever runs them, one thread included, so
    // unwrapping it is a lowering of the construct rather than a failure to
    // lower it, and there is nothing to warn about.
    static bool asserts_independence(const ASR::OMPRegion_t &x) {
        for (size_t i = 0; i < x.n_clauses; i++) {
            if (x.m_clauses[i]->type == ASR::omp_clauseType::OMPIndependent) {
                return true;
            }
        }
        return false;
    }

    void visit_OMPRegion(const ASR::OMPRegion_t &x) {
        if (pass_options.diagnostics != nullptr && !asserts_independence(x)) {
            pass_options.diagnostics->message_label(
                "openmp construct not supported, its statements run serially",
                {x.base.base.loc}, "no lowering claimed this region",
                diag::Level::Warning, diag::Stage::ASRPass);
        }
        ASR::OMPRegion_t &xx = const_cast<ASR::OMPRegion_t&>(x);
        transform_stmts(xx.m_body, xx.n_body);
        if (xx.n_body == 0) {
            remove_original_stmt = true;
            return;
        }
        pass_result.reserve(al, xx.n_body);
        for (size_t i = 0; i < xx.n_body; i++) {
            pass_result.push_back(al, xx.m_body[i]);
        }
    }
};

class OMPParallelLoopVisitor :
    public ASR::StatementWalkVisitor<OMPParallelLoopVisitor>
{
public:
    const PassOptions &pass_options;
    // Whether a loop construct that does not name a device may be turned
    // into a concurrent loop as well.
    bool offload_omp_loops;

    OMPParallelLoopVisitor(Allocator &al, const PassOptions &pass_options_) :
        StatementWalkVisitor(al), pass_options(pass_options_) {
        bool gpu = pass_options.gpu_offload_metal || pass_options.gpu_offload_cuda;
        offload_omp_loops = gpu && pass_options.gpu_offload_omp_loops;
    }

    // A region that cannot be turned into a loop still has to run, so it is
    // left as it was and the reason it missed the device is reported. Only a
    // compilation that asked for a device wanted to hear this.
    void report_not_offloaded(const Location &loc, const std::string &why) {
        if (pass_options.diagnostics == nullptr) return;
        if (!pass_options.gpu_offload_metal && !pass_options.gpu_offload_cuda) return;
        pass_options.diagnostics->message_label(
            "omp parallel loop not offloaded to the GPU, "
            "it runs on the CPU instead",
            {loc}, why,
            diag::Level::Warning, diag::Stage::ASRPass);
    }

    void collect_clauses(ASR::omp_clause_t **clauses, size_t n_clauses,
            Vec<ASR::expr_t*> &shared, Vec<ASR::expr_t*> &local,
            Vec<ASR::reduction_expr_t> &reduction,
            std::set<ASR::symbol_t*> &seen_shared, int64_t &collapse,
            bool &has_private_copy) {
        for (size_t i = 0; i < n_clauses; i++) {
            switch (clauses[i]->type) {
                // The direction of a `map` clause is not used yet: every
                // buffer of a launch is copied both ways, which is a
                // superset of `tofrom`. Only the variables matter here.
                case ASR::omp_clauseType::OMPMap: {
                    ASR::OMPMap_t *m = ASR::down_cast<ASR::OMPMap_t>(clauses[i]);
                    add_shared(m->m_vars, m->n_vars, shared, seen_shared);
                    break;
                }
                case ASR::omp_clauseType::OMPShared: {
                    ASR::OMPShared_t *s = ASR::down_cast<ASR::OMPShared_t>(clauses[i]);
                    add_shared(s->m_vars, s->n_vars, shared, seen_shared);
                    break;
                }
                case ASR::omp_clauseType::OMPPrivate: {
                    ASR::OMPPrivate_t *p = ASR::down_cast<ASR::OMPPrivate_t>(clauses[i]);
                    for (size_t j = 0; j < p->n_vars; j++) {
                        local.push_back(al, p->m_vars[j]);
                    }
                    break;
                }
                case ASR::omp_clauseType::OMPReduction: {
                    ASR::OMPReduction_t *r = ASR::down_cast<ASR::OMPReduction_t>(clauses[i]);
                    for (size_t j = 0; j < r->n_vars; j++) {
                        ASR::reduction_expr_t red;
                        red.loc = clauses[i]->base.loc;
                        red.m_op = r->m_operator;
                        red.m_arg = r->m_vars[j];
                        reduction.push_back(al, red);
                    }
                    break;
                }
                // A `do concurrent` loop has no way to say that a variable
                // starts each iteration at the value it had on the host, or
                // that the last iteration writes its value back.
                case ASR::omp_clauseType::OMPFirstPrivate:
                case ASR::omp_clauseType::OMPLastPrivate: {
                    has_private_copy = true;
                    break;
                }
                case ASR::omp_clauseType::OMPCollapse: {
                    ASR::OMPCollapse_t *c = ASR::down_cast<ASR::OMPCollapse_t>(clauses[i]);
                    int64_t n = 1;
                    if (ASRUtils::extract_value(
                            ASRUtils::expr_value(c->m_count), n)) {
                        collapse = n;
                    }
                    break;
                }
                default:
                    break;
            }
        }
    }

    void add_shared(ASR::expr_t **vars, size_t n_vars,
            Vec<ASR::expr_t*> &shared, std::set<ASR::symbol_t*> &seen) {
        for (size_t i = 0; i < n_vars; i++) {
            if (ASR::is_a<ASR::Var_t>(*vars[i])) {
                ASR::symbol_t *sym = ASR::down_cast<ASR::Var_t>(vars[i])->m_v;
                if (!seen.insert(sym).second) continue;
            }
            shared.push_back(al, vars[i]);
        }
    }

    // The index of a concurrent loop is named by its head, and a
    // `do concurrent` loop may not repeat it in its locality list, so the
    // `private(i)` an OpenMP loop writes for the same variable is dropped.
    void drop_loop_indices(Vec<ASR::expr_t*> &local,
            Vec<ASR::do_loop_head_t> &heads) {
        std::set<ASR::symbol_t*> indices;
        for (size_t i = 0; i < heads.n; i++) {
            if (heads[i].m_v && ASR::is_a<ASR::Var_t>(*heads[i].m_v)) {
                indices.insert(ASR::down_cast<ASR::Var_t>(heads[i].m_v)->m_v);
            }
        }
        Vec<ASR::expr_t*> kept; kept.reserve(al, local.n);
        for (size_t i = 0; i < local.n; i++) {
            if (ASR::is_a<ASR::Var_t>(*local[i]) &&
                    indices.count(ASR::down_cast<ASR::Var_t>(local[i])->m_v)) {
                continue;
            }
            kept.push_back(al, local[i]);
        }
        local = kept;
    }

    // Whether this pass looks at a region at all. Everything below it knows
    // what a `parallel do` is; nothing below it knows what a `target` is.
    bool is_normalization_root(ASR::omp_region_typeType region) {
        if (region == ASR::omp_region_typeType::Target) return true;
        return offload_omp_loops && is_parallel_loop_region(region);
    }

    void visit_OMPRegion(const ASR::OMPRegion_t &x) {
        if (!is_normalization_root(x.m_region)) {
            ASR::ASRPassBaseWalkVisitor<OMPParallelLoopVisitor>::visit_OMPRegion(x);
            return;
        }

        Vec<ASR::expr_t*> shared; shared.reserve(al, 4);
        Vec<ASR::expr_t*> local; local.reserve(al, 4);
        Vec<ASR::reduction_expr_t> reduction; reduction.reserve(al, 1);
        std::set<ASR::symbol_t*> seen_shared;
        int64_t collapse = 1;
        bool has_private_copy = false;

        collect_clauses(x.m_clauses, x.n_clauses, shared, local, reduction,
            seen_shared, collapse, has_private_copy);

        // Peel the `teams` / `distribute` / `parallel do` nest, gathering the
        // data environment of every level, until the loop itself is reached.
        ASR::stmt_t **body = x.m_body;
        size_t n_body = x.n_body;
        bool has_loop_construct = distributes_iterations(x.m_region);
        while (n_body == 1 && ASR::is_a<ASR::OMPRegion_t>(*body[0])) {
            ASR::OMPRegion_t *inner = ASR::down_cast<ASR::OMPRegion_t>(body[0]);
            if (!is_parallel_loop_region(inner->m_region)) break;
            has_loop_construct |= distributes_iterations(inner->m_region);
            collect_clauses(inner->m_clauses, inner->n_clauses, shared, local,
                reduction, seen_shared, collapse, has_private_copy);
            body = inner->m_body;
            n_body = inner->n_body;
        }

        if (!has_loop_construct) {
            report_not_offloaded(x.base.base.loc,
                "the region does not distribute a loop over its threads");
            decline(x);
            return;
        }
        if (has_private_copy) {
            report_not_offloaded(x.base.base.loc,
                "a firstprivate or lastprivate variable has no equivalent "
                "in a do concurrent loop");
            decline(x);
            return;
        }
        if (n_body != 1 || !ASR::is_a<ASR::DoLoop_t>(*body[0])) {
            report_not_offloaded(x.base.base.loc,
                "the region does not contain a single loop nest");
            decline(x);
            return;
        }

        // A collapsed nest becomes one multi-dimensional iteration space.
        Vec<ASR::do_loop_head_t> heads;
        heads.reserve(al, (size_t)collapse);
        ASR::stmt_t *loop = body[0];
        for (int64_t level = 0; level < collapse; level++) {
            if (!ASR::is_a<ASR::DoLoop_t>(*loop)) {
                report_not_offloaded(x.base.base.loc,
                    "the loop nest is shallower than the collapse count");
                decline(x);
                return;
            }
            ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(loop);
            heads.push_back(al, dl->m_head);
            body = dl->m_body;
            n_body = dl->n_body;
            if (level + 1 < collapse) {
                if (n_body != 1) {
                    report_not_offloaded(x.base.base.loc,
                        "the collapsed loops are not perfectly nested");
                    decline(x);
                    return;
                }
                loop = dl->m_body[0];
            }
        }

        NestedRegionFinder finder;
        for (size_t i = 0; i < n_body; i++) {
            finder.visit_stmt(*body[i]);
        }
        if (finder.found) {
            report_not_offloaded(x.base.base.loc,
                "the iterations synchronize with each other through an "
                "openmp construct in the loop body");
            decline(x);
            return;
        }

        drop_loop_indices(local, heads);

        ASR::stmt_t *dcl = ASRUtils::STMT(ASR::make_DoConcurrentLoop_t(al,
            x.base.base.loc, heads.p, heads.n, shared.p, shared.n,
            local.p, local.n, reduction.p, reduction.n, body, n_body,
            ASR::exec_targetType::ExecAuto));
        pass_result.reserve(al, 1);
        pass_result.push_back(al, dcl);
    }

    // Leaves a region the pass could not normalize exactly as it was, so the
    // OpenMP pass still sees the construct the user wrote.
    void decline(const ASR::OMPRegion_t &x) {
        ASR::ASRPassBaseWalkVisitor<OMPParallelLoopVisitor>::visit_OMPRegion(x);
    }
};

/*
 * Rewrites a `DoConcurrentLoop` the host runs into an `OMPRegion`.
 *
 * This is the reverse of the normalization above, and it runs where the two
 * meet: the device pipeline has taken the loops it offloads and handed back
 * the ones it declined, so every concurrent loop still standing is one the
 * host runs. The host lowering of an `OMPRegion` -- outlining the body,
 * partitioning the iteration space over the threads, and the reduction
 * epilogue -- is the mature one, and this rewrite is what lets a
 * `do concurrent` loop reach it.
 *
 * Converts:
 *      do concurrent (i = 1:n, j = 1:m) shared(a) reduce(+: s)
 *          s = s + a(i, j)
 *      end do
 *
 * to:
 *      !$omp parallel do collapse(2) shared(a) reduction(+: s)
 *      do i = 1, n
 *          do j = 1, m
 *              s = s + a(i, j)
 *          end do
 *      end do
 *
 * The region says its iterations are independent, which is what the loop
 * asserted and what an `!$omp parallel do` on its own does not. It keeps the
 * execution target the dispatch pass wrote into the loop; nothing is decided
 * again here. The indices stay where the lowering reads them -- in the heads
 * of the loop nest -- and are not repeated in `private`.
 */
class ParallelLoopToOMPVisitor :
    public ASR::StatementWalkVisitor<ParallelLoopToOMPVisitor>
{
public:
    ParallelLoopToOMPVisitor(Allocator &al) : StatementWalkVisitor(al) {
    }

    // A concurrent loop names all of its indices at once. A region describes
    // the same iteration space as the loop nest the source wrote, one loop
    // per index, and says with `collapse` how many of them are partitioned.
    ASR::stmt_t *build_loop_nest(const ASR::DoConcurrentLoop_t &x) {
        Vec<ASR::stmt_t*> body;
        body.reserve(al, x.n_body);
        for (size_t i = 0; i < x.n_body; i++) {
            body.push_back(al, x.m_body[i]);
        }
        for (size_t i = x.n_head; i > 1; i--) {
            ASR::stmt_t *inner = ASRUtils::STMT(ASR::make_DoLoop_t(al,
                x.base.base.loc, s2c(al, ""), x.m_head[i - 1],
                body.p, body.n, nullptr, 0));
            Vec<ASR::stmt_t*> outer;
            outer.reserve(al, 1);
            outer.push_back(al, inner);
            body = outer;
        }
        return ASRUtils::STMT(ASR::make_DoLoop_t(al, x.base.base.loc,
            s2c(al, ""), x.m_head[0], body.p, body.n, nullptr, 0));
    }

    // Only the outermost loop of a nest of concurrent loops becomes a
    // region. The host lowering partitions one iteration space over the
    // threads, and a thread that opened a second partition inside the first
    // would be dividing what it was already given; a concurrent loop left in
    // the body simply runs its iterations in order inside that thread.
    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        const Location &loc = x.base.base.loc;
        Vec<ASR::omp_clause_t*> clauses;
        clauses.reserve(al, 4 + x.n_reduction);

        // What a `do concurrent` loop asserts and an `!$omp parallel do` does
        // not: no iteration depends on another.
        clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(
            ASR::make_OMPIndependent_t(al, loc)));
        if (x.n_shared > 0) {
            clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(
                ASR::make_OMPShared_t(al, loc, x.m_shared, x.n_shared)));
        }
        if (x.n_local > 0) {
            clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(
                ASR::make_OMPPrivate_t(al, loc, x.m_local, x.n_local)));
        }
        // One clause per reduction, since each names its own operator.
        for (size_t i = 0; i < x.n_reduction; i++) {
            Vec<ASR::expr_t*> vars;
            vars.reserve(al, 1);
            vars.push_back(al, x.m_reduction[i].m_arg);
            clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(
                ASR::make_OMPReduction_t(al, loc, x.m_reduction[i].m_op,
                    vars.p, vars.n)));
        }
        // Every index of the loop belongs to the one iteration space that is
        // partitioned; without this only the outermost one would be.
        if (x.n_head > 1) {
            ASR::expr_t *count = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                al, loc, (int64_t)x.n_head,
                ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4))));
            clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(
                ASR::make_OMPCollapse_t(al, loc, count)));
        }

        Vec<ASR::stmt_t*> body;
        body.reserve(al, 1);
        body.push_back(al, build_loop_nest(x));

        pass_result.reserve(al, 1);
        pass_result.push_back(al, ASRUtils::STMT(ASR::make_OMPRegion_t(al, loc,
            ASR::omp_region_typeType::ParallelDo, clauses.p, clauses.n,
            body.p, body.n, x.m_exec_target)));
    }
};

void pass_replace_omp_to_parallel_loop(Allocator &al,
        ASR::TranslationUnit_t &unit, const PassOptions &pass_options) {
    OMPParallelLoopVisitor v(al, pass_options);
    v.visit_TranslationUnit(unit);
}

void pass_replace_parallel_loop_to_omp(Allocator &al,
        ASR::TranslationUnit_t &unit, const PassOptions &/*pass_options*/) {
    ParallelLoopToOMPVisitor v(al);
    v.visit_TranslationUnit(unit);
}

void pass_flatten_omp_regions(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options) {
    OMPRegionFlattener v(al, pass_options);
    v.visit_TranslationUnit(unit);
}

} // namespace LCompilers
