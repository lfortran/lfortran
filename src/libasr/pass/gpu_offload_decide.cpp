#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/pass/gpu_offload_visitor.h>
#include <libasr/pass/parallel_canonicalize.h>

namespace LCompilers {

// The shapes a kernel can be built out of. Everything this asks is about
// the region as the pass found it: nothing here rewrites anything, so a
// region turned down leaves no trace. False means the region has already
// been left on the host -- either walked into so the regions inside it
// still get their chance, or reported as a loop the launch cannot run --
// and the caller stops.
bool GpuOffloadVisitor::offloadable_loop_nest(const ASR::OMPRegion_t &region,
        ParallelLoopNest &nest) {
    if (!device_caps.device_selected()) {
        decline(region);
        return false;
    }

    // Only the regions the dispatch pass gave to the device. Every other
    // exit of this function leaves the region alone, and the regions
    // still marked for the device once the pass is done are the ones it
    // declined; they are handed back to the host below.
    if (region.m_exec_target != ASR::exec_targetType::ExecDevice) {
        decline(region);
        return false;
    }

    // Only a canonical parallel loop is offloaded: one region, one
    // perfectly nested loop nest, and the whole data environment in one
    // clause list. The kernel is built out of the nest.
    if (!parallel_loop_nest(region, nest)) {
        decline(region);
        return false;
    }

    Location loc = region.base.base.loc;
    size_t n_dims = nest.n_heads();

    // A reduction combines what the threads computed, which the launch
    // does not do yet, so the loop stays where that already works.
    for (size_t i = 0; i < region.n_clauses; i++) {
        if (region.m_clauses[i]->type ==
                ASR::omp_clauseType::OMPReduction) {
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::ReductionClause));
            return false;
        }
    }

    // Any rank is offloaded: the nest is dispatched as a flat 1-D grid of
    // product(extents) threads and every index is recovered from the flat
    // thread id by successive divmod over the per-dimension extents. The
    // 3-D shape of the underlying dispatch grid therefore does not limit
    // the number of loop indices.
    if (n_dims == 0) {
        report_not_offloaded(loc,
            GpuDecline(GpuDeclineReason::LoopWithoutIndex));
        return false;
    }

    for (size_t d = 0; d < n_dims; d++) {
        if (!nest.head(d).m_v || !nest.head(d).m_start ||
                !nest.head(d).m_end) {
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::IncompleteLoopHead));
            return false;
        }
    }

    // The kernel maps a flat thread id onto `start + (flat % extent)`, which
    // is only the loop's iteration set when the stride is one. A strided
    // head would silently address the wrong elements, so it stays on the
    // host until the index arithmetic carries the stride.
    for (size_t d = 0; d < n_dims; d++) {
        ASR::expr_t *step = nest.head(d).m_increment;
        if (!step) continue;
        ASR::expr_t *step_value = ASRUtils::expr_value(step);
        int64_t step_constant = 0;
        if (!step_value ||
                !ASRUtils::extract_value(step_value, step_constant) ||
                step_constant != 1) {
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::StridedLoop));
            return false;
        }
    }
    return true;
}

} // namespace LCompilers
