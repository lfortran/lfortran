#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/diagnostics.h>
#include <libasr/pass/parallel_canonicalize.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/stmt_walk_visitor.h>

#include <set>

namespace LCompilers {

/*
 * Canonicalizes every parallel loop, however it was written, into one
 * `OMPRegion`.
 *
 * An `!$omp target teams distribute parallel do` region, an `!$omp parallel
 * do` region and a `do concurrent` loop describe the same thing: an iteration
 * space, the data it reads and writes, and the reductions it performs. This
 * pass says all three the same way, so every lowering below it -- the device
 * offload, the host threads, the single thread -- reads one construct and
 * never asks which one the loop came from.
 *
 * The canonical form is a contract. Exactly one `OMPRegion`, of kind
 * `ParallelDo`, whose clauses are the data environment of the whole
 * construct, and whose body is one perfectly nested loop nest, as deep as
 * `collapse` says. No `target`, `teams` or `distribute` wrapper survives, and
 * a region that was one carries `OMPTargetRequested` instead, so a loop that
 * misses the device can still say the source had asked for one. Every
 * canonical region asserts `OMPIndependent`: that is what each of the three
 * constructs says about its iterations, and it is what tells the passes below
 * that this region, and no other, is a parallel loop they may choose a
 * lowering for.
 *
 * Converts:
 *      !$omp target map(tofrom: a, b)
 *      !$omp teams
 *      !$omp distribute parallel do
 *      do i = 1, n
 *          a(i) = b(i)
 *      end do
 * and:
 *      do concurrent (i = 1:n) shared(a, b)
 *          a(i) = b(i)
 *      end do
 *
 * both to:
 *      !$omp parallel do shared(a, b) independent
 *      do i = 1, n
 *          a(i) = b(i)
 *      end do
 *
 * The region is left `ExecAuto`; the dispatch pass that runs next decides who
 * runs its iterations.
 *
 * A `target` region is always canonicalized, because no lowering below this
 * pass knows what a target region is. A loop construct that does not mention
 * a device is only canonicalized when the compiler was asked to offload those
 * too, since `!$omp parallel do` asks for host threads and the OpenMP pass
 * already lowers the full clause set onto them.
 *
 * A region this pass declines is left exactly as it was, so the OpenMP pass
 * still gets its chance at it, and no pass below mistakes it for a loop it
 * may run somewhere else. `pass_flatten_omp_regions` below runs after that
 * pass and unwraps whatever is left, so an unsupported construct runs
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

bool omp_region_has_clause(const ASR::OMPRegion_t &x,
        ASR::omp_clauseType clause) {
    for (size_t i = 0; i < x.n_clauses; i++) {
        if (x.m_clauses[i]->type == clause) return true;
    }
    return false;
}

// How many of the loops below the region make up the one iteration space it
// partitions. A region that says nothing partitions the outermost loop.
static int64_t collapse_count(ASR::omp_clause_t **clauses, size_t n_clauses) {
    int64_t collapse = 1;
    for (size_t i = 0; i < n_clauses; i++) {
        if (clauses[i]->type != ASR::omp_clauseType::OMPCollapse) continue;
        ASR::OMPCollapse_t *c = ASR::down_cast<ASR::OMPCollapse_t>(clauses[i]);
        int64_t n = 1;
        if (ASRUtils::extract_value(ASRUtils::expr_value(c->m_count), n)) {
            collapse = n;
        }
    }
    return collapse;
}

// Walks `collapse` levels down a loop nest, collecting the loop of each
// level and the statements the innermost one runs. The levels have to be
// perfectly nested, since together they are one iteration space.
static bool descend_loop_nest(ASR::stmt_t *loop, int64_t collapse,
        ParallelLoopNest &nest, const char *&why) {
    nest.loops.clear();
    for (int64_t level = 0; level < collapse; level++) {
        if (!ASR::is_a<ASR::DoLoop_t>(*loop)) {
            why = "the loop nest is shallower than the collapse count";
            return false;
        }
        ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(loop);
        nest.loops.push_back(dl);
        nest.body = dl->m_body;
        nest.n_body = dl->n_body;
        if (level + 1 < collapse) {
            if (dl->n_body != 1) {
                why = "the collapsed loops are not perfectly nested";
                return false;
            }
            loop = dl->m_body[0];
        }
    }
    return true;
}

bool parallel_loop_nest(const ASR::OMPRegion_t &x, ParallelLoopNest &nest) {
    if (x.m_region != ASR::omp_region_typeType::ParallelDo) return false;
    if (x.n_body != 1) return false;
    const char *why = nullptr;
    return descend_loop_nest(x.m_body[0],
        collapse_count(x.m_clauses, x.n_clauses), nest, why);
}

// Builds the one region every lowering below reads. The data environment of
// the whole construct arrives here already merged; what is left is to say it
// in the clauses the canonical form is written in.
static ASR::stmt_t *make_canonical_region(Allocator &al, const Location &loc,
        ASR::expr_t **shared, size_t n_shared,
        ASR::expr_t **local, size_t n_local,
        ASR::reduction_expr_t *reduction, size_t n_reduction,
        int64_t collapse, ASR::omp_clause_t **kept, size_t n_kept,
        bool target_requested, ASR::stmt_t *loop,
        ASR::exec_targetType exec_target) {
    Vec<ASR::omp_clause_t*> clauses;
    clauses.reserve(al, 5 + n_reduction + n_kept);

    // What each of the three constructs asserts about its iterations, and
    // what an `!$omp parallel do` on its own does not say: no iteration
    // depends on another.
    clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(
        ASR::make_OMPIndependent_t(al, loc)));
    if (target_requested) {
        clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(
            ASR::make_OMPTargetRequested_t(al, loc)));
    }
    if (n_shared > 0) {
        clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(
            ASR::make_OMPShared_t(al, loc, shared, n_shared)));
    }
    if (n_local > 0) {
        clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(
            ASR::make_OMPPrivate_t(al, loc, local, n_local)));
    }
    // One clause per reduction, since each names its own operator.
    for (size_t i = 0; i < n_reduction; i++) {
        Vec<ASR::expr_t*> vars;
        vars.reserve(al, 1);
        vars.push_back(al, reduction[i].m_arg);
        clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(
            ASR::make_OMPReduction_t(al, loc, reduction[i].m_op,
                vars.p, vars.n)));
    }
    // Every index of the iteration space that is partitioned is named here,
    // or only the outermost loop would be.
    if (collapse > 1) {
        ASR::expr_t *count = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
            al, loc, collapse,
            ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4))));
        clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(
            ASR::make_OMPCollapse_t(al, loc, count)));
    }
    // Everything the construct said that the canonical form does not
    // rewrite -- how many threads to run it on, how to schedule it -- is
    // carried over untouched, for whichever lowering can honour it.
    for (size_t i = 0; i < n_kept; i++) {
        clauses.push_back(al, kept[i]);
    }

    Vec<ASR::stmt_t*> body;
    body.reserve(al, 1);
    body.push_back(al, loop);
    return ASRUtils::STMT(ASR::make_OMPRegion_t(al, loc,
        ASR::omp_region_typeType::ParallelDo, clauses.p, clauses.n,
        body.p, body.n, exec_target));
}

// Whether an OpenMP region is left anywhere under a statement. A region
// inside the loop body -- a `critical` section, a `barrier` -- says the
// iterations synchronize with each other, which is the opposite of what a
// parallel loop asserts, so such a loop is not canonicalized.
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

    void visit_OMPRegion(const ASR::OMPRegion_t &x) {
        // A region that asserts its iterations are independent of one
        // another computes the same result whatever runs them, one thread
        // included, so unwrapping it is a lowering of the construct rather
        // than a failure to lower it, and there is nothing to warn about.
        if (pass_options.diagnostics != nullptr &&
                !omp_region_has_clause(x,
                    ASR::omp_clauseType::OMPIndependent)) {
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

class ParallelCanonicalizeVisitor :
    public ASR::StatementWalkVisitor<ParallelCanonicalizeVisitor>
{
public:
    const PassOptions &pass_options;
    // Whether a loop construct that does not name a device may be
    // canonicalized as well.
    bool offload_omp_loops;

    ParallelCanonicalizeVisitor(Allocator &al, const PassOptions &pass_options_) :
        StatementWalkVisitor(al), pass_options(pass_options_) {
        bool gpu = pass_options.gpu_offload_metal || pass_options.gpu_offload_cuda;
        offload_omp_loops = gpu && pass_options.gpu_offload_omp_loops;
    }

    // A region that cannot be canonicalized still has to run, so it is left
    // as it was and the reason it missed the device is reported. Only a
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
            Vec<ASR::omp_clause_t*> &kept,
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
                // A parallel loop has no way to say that a variable starts
                // each iteration at the value it had outside the loop, or
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
                // A clause the canonical form does not rewrite says
                // something about the construct that a lowering may still
                // honour, so it is carried over as it stands.
                default:
                    kept.push_back(al, clauses[i]);
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

    // The index of each partitioned loop is named by the head of that loop,
    // so the `private(i)` an OpenMP loop writes for the same variable says
    // nothing the canonical form does not already say.
    void drop_loop_indices(Vec<ASR::expr_t*> &local,
            const ParallelLoopNest &nest) {
        std::set<ASR::symbol_t*> indices;
        for (size_t i = 0; i < nest.n_heads(); i++) {
            ASR::expr_t *v = nest.head(i).m_v;
            if (v && ASR::is_a<ASR::Var_t>(*v)) {
                indices.insert(ASR::down_cast<ASR::Var_t>(v)->m_v);
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
    bool is_canonicalization_root(ASR::omp_region_typeType region) {
        if (region == ASR::omp_region_typeType::Target) return true;
        return offload_omp_loops && is_parallel_loop_region(region);
    }

    void visit_OMPRegion(const ASR::OMPRegion_t &x) {
        // A region that already asserts the independence of its iterations
        // is one this pass produced, and canonicalizing a canonical region
        // says nothing new about it.
        if (!is_canonicalization_root(x.m_region) ||
                omp_region_has_clause(x,
                    ASR::omp_clauseType::OMPIndependent)) {
            decline(x);
            return;
        }

        Vec<ASR::expr_t*> shared; shared.reserve(al, 4);
        Vec<ASR::expr_t*> local; local.reserve(al, 4);
        Vec<ASR::reduction_expr_t> reduction; reduction.reserve(al, 1);
        Vec<ASR::omp_clause_t*> kept; kept.reserve(al, 4);
        std::set<ASR::symbol_t*> seen_shared;
        int64_t collapse = 1;
        bool has_private_copy = false;

        collect_clauses(x.m_clauses, x.n_clauses, shared, local, reduction,
            kept, seen_shared, collapse, has_private_copy);

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
                reduction, kept, seen_shared, collapse, has_private_copy);
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
                "in a canonical parallel loop");
            decline(x);
            return;
        }
        if (n_body != 1 || !ASR::is_a<ASR::DoLoop_t>(*body[0])) {
            report_not_offloaded(x.base.base.loc,
                "the region does not contain a single loop nest");
            decline(x);
            return;
        }

        // A collapsed nest is one multi-dimensional iteration space.
        ParallelLoopNest nest;
        const char *why = nullptr;
        if (!descend_loop_nest(body[0], collapse, nest, why)) {
            report_not_offloaded(x.base.base.loc, why);
            decline(x);
            return;
        }

        NestedRegionFinder finder;
        for (size_t i = 0; i < nest.n_body; i++) {
            finder.visit_stmt(*nest.body[i]);
        }
        if (finder.found) {
            report_not_offloaded(x.base.base.loc,
                "the iterations synchronize with each other through an "
                "openmp construct in the loop body");
            decline(x);
            return;
        }

        drop_loop_indices(local, nest);

        pass_result.reserve(al, 1);
        pass_result.push_back(al, make_canonical_region(al, x.base.base.loc,
            shared.p, shared.n, local.p, local.n, reduction.p, reduction.n,
            collapse, kept.p, kept.n,
            x.m_region == ASR::omp_region_typeType::Target, body[0],
            ASR::exec_targetType::ExecAuto));
    }

    // A concurrent loop names all of its indices at once. The canonical
    // region describes the same iteration space as one loop per index, and
    // says with `collapse` how many of them are partitioned.
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
    // region. A lowering partitions one iteration space over the threads,
    // and a thread that opened a second partition inside the first would be
    // dividing what it was already given; a concurrent loop left in the body
    // simply runs its iterations in order inside that thread.
    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        if (x.n_head == 0) return;
        pass_result.reserve(al, 1);
        pass_result.push_back(al, make_canonical_region(al, x.base.base.loc,
            x.m_shared, x.n_shared, x.m_local, x.n_local,
            x.m_reduction, x.n_reduction, (int64_t)x.n_head, nullptr, 0,
            false, build_loop_nest(x), ASR::exec_targetType::ExecAuto));
    }

    // Leaves a region the pass could not canonicalize exactly as it was, so
    // the OpenMP pass still sees the construct the user wrote, and looks
    // inside it for the loops that can be.
    void decline(const ASR::OMPRegion_t &x) {
        ASR::ASRPassBaseWalkVisitor<ParallelCanonicalizeVisitor>::visit_OMPRegion(x);
    }
};

void pass_parallel_canonicalize(Allocator &al,
        ASR::TranslationUnit_t &unit, const PassOptions &pass_options) {
    ParallelCanonicalizeVisitor v(al, pass_options);
    v.visit_TranslationUnit(unit);
}

void pass_flatten_omp_regions(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options) {
    OMPRegionFlattener v(al, pass_options);
    v.visit_TranslationUnit(unit);
}

} // namespace LCompilers
