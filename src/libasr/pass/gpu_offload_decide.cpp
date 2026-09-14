#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/pass/gpu_offload_collect.h>
#include <libasr/pass/gpu_offload_preflight.h>
#include <libasr/pass/gpu_offload_visitor.h>
#include <libasr/pass/parallel_canonicalize.h>

namespace LCompilers {

// The shapes a kernel can be built out of. Everything this asks is about
// the region as the pass found it, and nothing here rewrites anything.
// False means either that the region is not one this pass offloads, and it
// has been walked into so the regions inside it still get their chance, or
// that it has been reported as a loop the launch cannot run; the caller
// stops.
bool GpuOffloadVisitor::offloadable_loop_nest(const ASR::OMPRegion_t &region,
        ParallelLoopNest &nest) {
    if (!device_caps.device_selected()) {
        decline(region);
        return false;
    }

    // Only the regions the dispatch pass gave to the device, which are
    // committed to it: every other exit of this function is an error.
    if (region.m_exec_target != ASR::exec_targetType::ExecDevice) {
        decline(region);
        return false;
    }

    // Only a canonical parallel loop is offloaded: one region, one
    // perfectly nested loop nest, and the whole data environment in one
    // clause list. The kernel is built out of the nest.
    if (!parallel_loop_nest(region, nest)) {
        report_not_offloaded(region.base.base.loc,
            GpuDecline(GpuDeclineReason::LoopNestShape));
        return false;
    }

    Location loc = region.base.base.loc;
    size_t n_dims = nest.n_heads();

    // A reduction is given one accumulator per thread and folded on the
    // host afterwards, so what has to be true here is only that the
    // accumulator is something a thread can hold and the operator is one
    // the fold can spell. Anything else is an error.
    pending_reductions.clear();
    for (size_t i = 0; i < region.n_clauses; i++) {
        if (region.m_clauses[i]->type !=
                ASR::omp_clauseType::OMPReduction) {
            continue;
        }
        ASR::OMPReduction_t *clause = ASR::down_cast<ASR::OMPReduction_t>(
            region.m_clauses[i]);
        if (!gpu_reduction_op_supported(clause->m_operator)) {
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::ReductionClause));
            return false;
        }
        for (size_t v = 0; v < clause->n_vars; v++) {
            ASR::expr_t *var = clause->m_vars[v];
            if (!ASR::is_a<ASR::Var_t>(*var)) {
                report_not_offloaded(loc,
                    GpuDecline(GpuDeclineReason::ReductionClause));
                return false;
            }
            ASR::symbol_t *sym = ASR::down_cast<ASR::Var_t>(var)->m_v;
            ASR::ttype_t *type = ASRUtils::expr_type(var);
            // An array accumulator would need one array per thread, which
            // is a different shape of buffer than this builds.
            if (ASRUtils::is_array(type) ||
                    !device_caps.has_scalar_type(
                        ASRUtils::type_get_past_array(type)) ||
                    !gpu_reduction_identity_exists(clause->m_operator,
                        type)) {
                report_not_offloaded(loc,
                    GpuDecline(GpuDeclineReason::ReductionClause));
                return false;
            }
            GpuReductionInfo info;
            info.orig_name = ASRUtils::symbol_name(sym);
            info.op = clause->m_operator;
            info.orig_scalar_sym = sym;
            info.scalar_type = type;
            info.host_buf_sym = nullptr;
            pending_reductions.push_back(info);
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
    // head would silently address the wrong elements, so it is an error
    // until the index arithmetic carries the stride.
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
// found. Every check here is analysis only; false means the error has
// already been reported and the caller stops. The one thing it leaves
// behind is the splice plan in functions_to_inline, which the rewrites below
// the call consume.
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
        // If that temporary cannot be fixed-size, it is an error.
        std::vector<std::string> alias_arg_names;
        collect_kernel_arg_names(work, enclosing_block_scopes,
            alias_arg_names);
        if (body_needs_unsupported_alias_temp(work.body, work.n_body,
                true, alias_arg_names)) {
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::AliasTemporaryRuntimeSized));
            return false;
        }
        // A non-contiguous section actual argument is gathered into a
        // contiguous per-thread temporary below, right before the
        // statement that makes the call. Where the call is evaluated
        // somewhere no such gather can serve -- a do while condition, a
        // FORALL -- the gather is impossible, and so it is when its base
        // is not a designator the copy loops can index. Passing the
        // section on would silently drop its stride.
        {
            Location where = loc;
            std::string name;
            GpuSectionSite site = GpuSectionSite::Statement;
            if (body_has_unplaceable_section(work.body, work.n_body, where,
                    name, site)) {
                report_not_offloaded(where, GpuDecline(
                    GpuDeclineReason::SectionCopyNotPlaceable, name, site));
                return false;
            }
        }
        if (body_has_ungatherable_strided_section(work.body, work.n_body)) {
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::UngatherableStridedSection));
            return false;
        }
        // The gathered buffer is sized on the host. Only its last
        // dimension can be sized from the base array and read through a
        // shorter slice; an earlier one has to have the section's exact
        // extent, which the host cannot know when it changes with the
        // iteration.
        {
            Location where = loc;
            std::string name;
            if (body_has_varying_leading_section_extent(work, where,
                    name)) {
                report_not_offloaded(where, GpuDecline(
                    GpuDeclineReason::SectionLeadingExtentVaries, name));
                return false;
            }
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

    // A statement the device cannot run. The unsupported-construct check
    // has already kept such loops off the device; this is asked before the
    // width sweep below
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
    // kernel-local checks that run on every device already ask it. Only
    // the width is asked here; everything else about a type is left to
    // those checks, exactly as on a device that narrows nothing.
    if (device_caps.narrows_scalar_types()) {
        std::map<std::string, std::pair<ASR::ttype_t*, ASR::expr_t*>>
            candidate_syms;
        collect_involved_syms(work, enclosing_block_scopes, candidate_syms);
        // Every symbol reaching the kernel — buffer parameters,
        // by-value members of the __ScalarArgs struct and kernel-local
        // temporaries alike — is collected here, so a single sweep
        // covers all of them.
        for (auto &sym : candidate_syms) {
            // A derived type is asked about once the body has been
            // rewritten, where it is known whether its layout reaches the
            // kernel at all (see offloadable_after_rewrites).
            if (ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::extract_type(sym.second.first))) {
                continue;
            }
            ASR::ttype_t *narrowed = gpu_device_narrowed_type(device_caps,
                sym.second.first, sym.second.second);
            if (narrowed != nullptr) {
                report_not_offloaded(loc, GpuDecline(
                    GpuDeclineReason::SymbolTypeNotRepresentable,
                    sym.first, narrowed));
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
            // A callee that cannot be spliced, or whose result allocation
            // cannot reach the device, is an error.
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

// The last question a decline can be based on: the one the rewrites above
// the call made answerable, on symbols that only exist once the body has
// been lowered. False means the error has been reported and the caller
// stops.
//
// The statements of the nest are not asked about again: what a statement
// needs of the device does not depend on the rewrites, so that question is
// settled before them, where it can name the statement the source wrote.
// The nest is read only for how it reaches derived types, which decides
// whether a type's whole layout reaches the kernel.
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

    // A derived type reaches the kernel in one of two ways. Read only
    // through allocatable array components, it is split into one buffer per
    // component, and only the element types of those components reach the
    // device. Otherwise its whole layout does, member by member, and a member
    // of a width this device narrows -- a real(8) that is never read
    // included -- would put every member after it at the wrong offset. The
    // split is decided from the same accesses when the kernel is built.
    if (!device_caps.narrows_scalar_types()) return true;
    GpuAllocStructMemberCollector accesses;
    for (size_t i = 0; i < work.n_body; i++) {
        accesses.visit_stmt(*work.body[i]);
    }
    for (auto &sym : involved_syms) {
        ASR::symbol_t *s = current_scope->resolve_symbol(sym.first);
        if (!s || !ASR::is_a<ASR::Variable_t>(*s)) continue;
        ASR::ttype_t *type = ASR::down_cast<ASR::Variable_t>(s)->m_type;
        if (!ASR::is_a<ASR::Array_t>(*type)) continue;
        ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
        for (size_t d = 0; d < arr->n_dims; d++) {
            if (arr->m_dims[d].m_start) {
                accesses.visit_expr(*arr->m_dims[d].m_start);
            }
            if (arr->m_dims[d].m_length) {
                accesses.visit_expr(*arr->m_dims[d].m_length);
            }
        }
    }
    for (auto &sym : involved_syms) {
        if (!ASR::is_a<ASR::StructType_t>(
                *ASRUtils::extract_type(sym.second.first))) {
            continue;
        }
        auto split = accesses.alloc_members.find(sym.first);
        bool whole = split == accesses.alloc_members.end()
            || accesses.has_non_alloc_access.count(sym.first) > 0;
        ASR::ttype_t *narrowed = nullptr;
        if (whole) {
            narrowed = gpu_device_narrowed_type(device_caps,
                sym.second.first, sym.second.second);
        } else {
            for (auto &member : split->second) {
                ASR::ttype_t *element = ASRUtils::type_get_past_array(
                    ASRUtils::type_get_past_allocatable(
                        member.second.second));
                if (device_caps.narrows_scalar_type(element)) {
                    narrowed = element;
                    break;
                }
            }
        }
        if (narrowed != nullptr) {
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::WideTypeNotOnDevice,
                    sym.first, narrowed));
            return false;
        }
    }

    return true;
}

} // namespace LCompilers
