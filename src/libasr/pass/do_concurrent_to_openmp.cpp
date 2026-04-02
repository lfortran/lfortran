#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/do_concurrent_to_openmp.h>
#include <libasr/pass/stmt_walk_visitor.h>

namespace LCompilers {

/*
 * This ASR pass rewrites DoConcurrentLoop into an OpenMP Target region
 * for GPU offloading via the C/CUDA backend.
 *
 * Converts:
 *      do concurrent (i = start:end) shared(a, b)
 *          a(i) = i + b(i)*340
 *      end do
 *
 * to:
 *      !$omp target map(tofrom: a, b)
 *          !$omp teams
 *              !$omp distribute parallel do
 *                  do i = start, end
 *                      a(i) = i + b(i)*340
 *                  end do
 *              !$omp end distribute parallel do
 *          !$omp end teams
 *      !$omp end target
 *
 * This runs before the openmp pass so that the generated OpenMP ASR
 * nodes flow through the normal OpenMP/CUDA pipeline.
 */

class DoConcurrentArrayCollector :
    public ASR::BaseWalkVisitor<DoConcurrentArrayCollector>
{
public:
    Allocator &al;
    // symbol name -> Var expr (deduplicated)
    std::map<std::string, ASR::expr_t*> array_vars;
    // loop index variable name, to exclude from map clauses
    std::string loop_var_name;

    DoConcurrentArrayCollector(Allocator &al_, const std::string &loop_var)
        : al(al_), loop_var_name(loop_var) {}

    void visit_Var(const ASR::Var_t &x) {
        std::string name = ASRUtils::symbol_name(x.m_v);
        if (name == loop_var_name) return;
        if (array_vars.find(name) != array_vars.end()) return;
        ASR::ttype_t *type = ASRUtils::symbol_type(x.m_v);
        if (ASR::is_a<ASR::Array_t>(
                *ASRUtils::type_get_past_allocatable(
                    ASRUtils::type_get_past_pointer(type)))) {
            array_vars[name] = ASRUtils::EXPR(
                ASR::make_Var_t(al, x.base.base.loc, x.m_v));
        }
    }
};

class DoConcurrentToOpenMPVisitor :
    public ASR::StatementWalkVisitor<DoConcurrentToOpenMPVisitor>
{
    const PassOptions &pass_options;
public:
    DoConcurrentToOpenMPVisitor(Allocator &al, const PassOptions &opts)
        : StatementWalkVisitor(al), pass_options(opts) {}

    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        if (!pass_options.target_offload_enabled) return;
        // Only handle single-head do concurrent for now
        if (x.n_head != 1) return;

        Location loc = x.base.base.loc;
        const ASR::do_loop_head_t &head = x.m_head[0];

        // Collect array variables from the loop body for map clauses
        std::string loop_var_name;
        if (ASR::is_a<ASR::Var_t>(*head.m_v)) {
            loop_var_name = ASRUtils::symbol_name(
                ASR::down_cast<ASR::Var_t>(head.m_v)->m_v);
        }
        DoConcurrentArrayCollector collector(al, loop_var_name);
        for (size_t i = 0; i < x.n_body; i++) {
            collector.visit_stmt(*x.m_body[i]);
        }

        // Also include explicitly shared variables that are arrays
        for (size_t i = 0; i < x.n_shared; i++) {
            collector.visit_expr(*x.m_shared[i]);
        }

        // Build OMPMap(ToFrom, array_vars) clause
        Vec<ASR::expr_t*> map_exprs;
        map_exprs.reserve(al, collector.array_vars.size());
        for (auto &kv : collector.array_vars) {
            map_exprs.push_back(al, kv.second);
        }

        Vec<ASR::omp_clause_t*> target_clauses;
        target_clauses.reserve(al, 1);
        if (map_exprs.size() > 0) {
            target_clauses.push_back(al, ASR::down_cast<ASR::omp_clause_t>(
                ASR::make_OMPMap_t(al, loc,
                    ASR::map_typeType::ToFrom,
                    map_exprs.p, map_exprs.n)));
        }

        // Build DoLoop from the DoConcurrentLoop head+body
        ASR::stmt_t *do_loop = ASRUtils::STMT(
            ASR::make_DoLoop_t(al, loc,
                nullptr,        // name
                head,           // do_loop_head
                x.m_body, x.n_body,
                nullptr, 0));   // no orelse

        // Wrap in DistributeParallelDo
        Vec<ASR::stmt_t*> dpd_body;
        dpd_body.reserve(al, 1);
        dpd_body.push_back(al, do_loop);

        Vec<ASR::omp_clause_t*> empty_clauses;
        empty_clauses.reserve(al, 0);

        ASR::stmt_t *dpd_region = ASRUtils::STMT(
            ASR::make_OMPRegion_t(al, loc,
                ASR::omp_region_typeType::DistributeParallelDo,
                empty_clauses.p, empty_clauses.n,
                dpd_body.p, dpd_body.n));

        // Wrap in Teams
        Vec<ASR::stmt_t*> teams_body;
        teams_body.reserve(al, 1);
        teams_body.push_back(al, dpd_region);

        ASR::stmt_t *teams_region = ASRUtils::STMT(
            ASR::make_OMPRegion_t(al, loc,
                ASR::omp_region_typeType::Teams,
                empty_clauses.p, empty_clauses.n,
                teams_body.p, teams_body.n));

        // Wrap in Target with map clauses
        Vec<ASR::stmt_t*> target_body;
        target_body.reserve(al, 1);
        target_body.push_back(al, teams_region);

        ASR::stmt_t *target_region = ASRUtils::STMT(
            ASR::make_OMPRegion_t(al, loc,
                ASR::omp_region_typeType::Target,
                target_clauses.p, target_clauses.n,
                target_body.p, target_body.n));

        // Replace the original DoConcurrentLoop
        Vec<ASR::stmt_t*> result;
        result.reserve(al, 1);
        result.push_back(al, target_region);
        pass_result = result;
    }
};

void pass_do_concurrent_to_openmp(Allocator &al, ASR::TranslationUnit_t &unit,
                            const LCompilers::PassOptions &pass_options) {
    if (!pass_options.target_offload_enabled) return;
    DoConcurrentToOpenMPVisitor v(al, pass_options);
    v.visit_TranslationUnit(unit);
}

} // namespace LCompilers
