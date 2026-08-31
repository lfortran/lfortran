#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/diagnostics.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/replace_openmp_target.h>
#include <libasr/pass/stmt_walk_visitor.h>

#include <set>

namespace LCompilers {

/*
 * Normalizes an `!$omp target` region into a `DoConcurrentLoop`.
 *
 * A target region and a `do concurrent` loop describe the same thing: an
 * iteration space, the data it reads and writes, and the reductions it
 * performs. `DoConcurrentLoop` already carries all three, so a target region
 * is rewritten into one and the whole GPU offload pipeline below -- kernel
 * extraction, the device call graph, array extents, address spaces and the
 * host launch -- serves both constructs without knowing which one it came
 * from.
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
 * The pass only runs when a GPU backend is selected, so plain `--openmp`
 * keeps the behaviour it has always had.
 */

// The constructs that may sit between `target` and the loop it runs. `Teams`
// and `Parallel` only open a data environment, so they are peeled but do not
// on their own make the loop below them a parallel one; the rest distribute
// the iterations and do.
static bool is_target_nest_region(ASR::omp_region_typeType region) {
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

// Unwraps every OpenMP region of a subtree, leaving the statements they
// contained. The OpenMP pass has already run by the time a target region is
// taken apart, so a region this pass declines to offload still holds the
// constructs the OpenMP pass never descended into; without this they would
// reach a code generator that has no lowering for them.
class OMPRegionFlattener : public ASR::StatementWalkVisitor<OMPRegionFlattener>
{
public:
    OMPRegionFlattener(Allocator &al) : StatementWalkVisitor(al) {
    }

    void visit_OMPRegion(const ASR::OMPRegion_t &x) {
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

class OMPTargetVisitor : public ASR::StatementWalkVisitor<OMPTargetVisitor>
{
public:
    const PassOptions &pass_options;

    OMPTargetVisitor(Allocator &al, const PassOptions &pass_options_) :
        StatementWalkVisitor(al), pass_options(pass_options_) {
    }

    // An `!$omp target` that cannot be turned into a loop still has to run,
    // so it is left on the host and the reason is reported.
    void report_not_offloaded(const Location &loc, const std::string &why) {
        if (pass_options.diagnostics == nullptr) return;
        pass_options.diagnostics->message_label(
            "omp target region not offloaded to the GPU, "
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

    void visit_OMPRegion(const ASR::OMPRegion_t &x) {
        if (x.m_region != ASR::omp_region_typeType::Target) {
            ASR::ASRPassBaseWalkVisitor<OMPTargetVisitor>::visit_OMPRegion(x);
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
        bool has_loop_construct = false;
        while (n_body == 1 && ASR::is_a<ASR::OMPRegion_t>(*body[0])) {
            ASR::OMPRegion_t *inner = ASR::down_cast<ASR::OMPRegion_t>(body[0]);
            if (!is_target_nest_region(inner->m_region)) break;
            has_loop_construct |= distributes_iterations(inner->m_region);
            collect_clauses(inner->m_clauses, inner->n_clauses, shared, local,
                reduction, seen_shared, collapse, has_private_copy);
            body = inner->m_body;
            n_body = inner->n_body;
        }

        if (!has_loop_construct) {
            report_not_offloaded(x.base.base.loc,
                "the region does not distribute a loop over the device");
            splice(x);
            return;
        }
        if (has_private_copy) {
            report_not_offloaded(x.base.base.loc,
                "a firstprivate or lastprivate variable has no equivalent "
                "in a do concurrent loop");
            splice(x);
            return;
        }
        if (n_body != 1 || !ASR::is_a<ASR::DoLoop_t>(*body[0])) {
            report_not_offloaded(x.base.base.loc,
                "the region does not contain a single loop nest");
            splice(x);
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
                splice(x);
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
                    splice(x);
                    return;
                }
                loop = dl->m_body[0];
            }
        }

        ASR::stmt_t *dcl = ASRUtils::STMT(ASR::make_DoConcurrentLoop_t(al,
            x.base.base.loc, heads.p, heads.n, shared.p, shared.n,
            local.p, local.n, reduction.p, reduction.n, body, n_body,
            ASR::exec_targetType::ExecAuto));
        pass_result.reserve(al, 1);
        pass_result.push_back(al, dcl);
    }

    // Leaves the statements of a region behind in its place, running them on
    // the host. Any OpenMP region still nested inside goes with it, since
    // nothing below this pass knows how to lower one.
    void splice(const ASR::OMPRegion_t &x) {
        ASR::OMPRegion_t &xx = const_cast<ASR::OMPRegion_t&>(x);
        OMPRegionFlattener flattener(al);
        flattener.current_scope = current_scope;
        flattener.transform_stmts(xx.m_body, xx.n_body);
        // An empty region has to leave something behind, otherwise the
        // original statement is kept and the region survives the pass.
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

void pass_replace_openmp_target(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &pass_options) {
    if (!pass_options.gpu_offload_metal && !pass_options.gpu_offload_cuda) return;
    OMPTargetVisitor v(al, pass_options);
    v.visit_TranslationUnit(unit);
}

} // namespace LCompilers
