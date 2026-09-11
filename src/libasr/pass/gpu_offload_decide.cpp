#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/pass/gpu_offload_collect.h>
#include <libasr/pass/gpu_offload_preflight.h>
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

// Whether the loop is one the launch can run, asked of the loop as it was
// found. Every check here is analysis only, so a loop turned down is left
// exactly as it was; false means the decline has already been reported and
// the caller stops. The one thing it leaves behind is the splice plan in
// functions_to_inline, which the rewrites below the call consume.
bool GpuOffloadVisitor::offloadable_before_rewrites(
        const ParallelLoopNest &work,
        const std::set<SymbolTable*> &enclosing_block_scopes,
        const Location &loc) {
    // What the pass's own lowering can and cannot do, which is the same
    // whichever device the launch targets: a local with no extent, an
    // aliased assignment that would need a run-time sized temporary, and
    // a strided actual that has to be gathered into a contiguous one.
    // These used to run for Metal only, so the CUDA path went on to build
    // a kernel that read the wrong elements and said nothing.
    {
        GpuLocalArrayChecker local_array_checker;
        for (size_t i = 0; i < work.n_body; i++) {
            local_array_checker.visit_stmt(*work.body[i]);
        }
        if (local_array_checker.has_unsized_local_array) {
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::UnsizedLocalArray,
                    local_array_checker.unsized_name));
            return false;
        }
        // An array assignment whose two sides overlap the same array
        // needs a temporary (see materialize_aliased_assignments).
        // If that temporary cannot be fixed-size, decline here,
        // while the body is still untouched.
        std::vector<std::string> alias_arg_names;
        collect_kernel_arg_names(work, enclosing_block_scopes,
            alias_arg_names);
        if (body_needs_unsupported_alias_temp(work.body, work.n_body,
                true, alias_arg_names)) {
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::AliasTemporaryRuntimeSized));
            return false;
        }
        // A strided section actual argument is gathered into a
        // contiguous kernel-local temporary below. When that temporary
        // cannot be sized at compile time the gather is impossible,
        // and passing the section on would silently drop its stride.
        if (body_has_ungatherable_strided_section(work.body, work.n_body)) {
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::UngatherableStridedSection));
            return false;
        }
        GpuLocalWidthChecker width_checker;
        width_checker.caps = device_caps;
        for (size_t i = 0; i < work.n_body; i++) {
            width_checker.visit_stmt(*work.body[i]);
        }
        if (width_checker.unsupported) {
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::LocalTypeWidth,
                    width_checker.bad_name, width_checker.bad_type));
            return false;
        }
    }

    // A statement no device can run keeps the loop on the CPU whichever
    // backend is selected. This is asked before the width sweep below
    // because that sweep answers for every symbol reaching the kernel,
    // including the ones a lowering introduced: a `write` brings in an
    // `iomsg` buffer of a type no device has, and reporting that buffer
    // names something the user never wrote instead of the statement they
    // did. Which of the two is the reason does not depend on the device,
    // so it is settled first.
    {
        GpuUnsupportedStatementFinder finder(device_caps);
        for (size_t i = 0; i < work.n_body; i++) {
            finder.visit_stmt(*work.body[i]);
        }
        std::string in_routine;
        if (finder.reason == GpuDeclineReason::None) {
            for (ASR::Function_t *fn : reachable_routines(work.body,
                    work.n_body)) {
                GpuUnsupportedStatementFinder callee_finder(device_caps);
                for (size_t i = 0; i < fn->n_body; i++) {
                    callee_finder.visit_stmt(*fn->m_body[i]);
                }
                if (callee_finder.reason != GpuDeclineReason::None) {
                    finder = callee_finder;
                    in_routine = fn->m_name;
                    break;
                }
            }
        }
        if (finder.reason != GpuDeclineReason::None) {
            report_not_offloaded(finder.loc,
                GpuDecline(finder.reason, in_routine));
            return false;
        }
    }

    // A device whose scalar type set is narrower than the shared width
    // table has to be asked about every symbol that reaches the kernel:
    // where the two sets are the same, the kernel-argument and
    // kernel-local checks that run on every device already ask it.
    if (device_caps.narrows_scalar_types()) {
        std::map<std::string, std::pair<ASR::ttype_t*, ASR::expr_t*>>
            candidate_syms;
        collect_involved_syms(work, enclosing_block_scopes, candidate_syms);
        // Every symbol reaching the kernel — buffer parameters,
        // by-value members of the __ScalarArgs struct and kernel-local
        // temporaries alike — is collected here, so a single sweep
        // covers all of them.
        for (auto &sym : candidate_syms) {
            ASR::ttype_t *unsupported_type = nullptr;
            if (!gpu_device_can_represent_type(device_caps,
                    sym.second.first, sym.second.second, &unsupported_type)) {
                report_not_offloaded(loc, GpuDecline(
                    GpuDeclineReason::SymbolTypeNotRepresentable,
                    sym.first, unsupported_type));
                return false;
            }
        }
    }

    // A device function may need a run-time sized local -- an
    // array-constructor temporary sized from an assumed-shape dummy,
    // say -- which a device that has no variable-length arrays cannot
    // declare. Work out here which callees have to be spliced into the
    // kernel body to move those locals to kernel scope, where the VLA
    // workspace machinery applies. This is analysis only; the splice
    // itself happens below, after the offload decision.
    functions_to_inline.clear();
    if (device_caps.splices_device_functions()) {
        std::map<ASR::Function_t*, bool> needs_inline_memo;
        std::set<ASR::Function_t*> on_stack;
        GpuDecline decline;
        if (!plan_device_function_inlining(work.body, work.n_body,
                needs_inline_memo, on_stack, decline)) {
            // Decline before destructive rewrites if a callee cannot be
            // spliced or its result allocation cannot reach the device.
            functions_to_inline.clear();
            if (!decline.declined()) {
                decline = GpuDecline(GpuDeclineReason::DeviceFunctionInlining);
            }
            report_not_offloaded(loc, decline);
            return false;
        }
    }

    return true;
}

// The last questions a decline can be based on: the ones the rewrites above
// the call made answerable, on symbols and statements that only exist once
// the body has been lowered. False means the decline has been reported and
// the caller stops, which leaves the loop on the host -- the guards it holds
// put back everything the rewrites did to the pass's copy of the nest.
bool GpuOffloadVisitor::offloadable_after_rewrites(
        const ParallelLoopNest &work,
        const std::map<std::string,
            std::pair<ASR::ttype_t*, ASR::expr_t*>> &involved_syms,
        const Location &loc) {
    // The rewrites above can bring in symbols the sweep before them
    // never saw, so ask again of the widths this device narrows. A
    // device that narrows none of them answers no to every symbol.
    for (auto &sym : involved_syms) {
        ASR::ttype_t *base_t =
            ASRUtils::type_get_past_array(sym.second.first);
        if (device_caps.narrows_scalar_type(base_t)) {
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::WideTypeNotOnDevice,
                    sym.first, base_t));
            return false;
        }
    }

    return true;
}

} // namespace LCompilers
