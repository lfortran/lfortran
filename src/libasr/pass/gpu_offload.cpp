#include <filesystem>
#include <iostream>
#include <set>
#include <string>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/diagnostics.h>
#include <libasr/exception.h>
#include <libasr/modfile.h>
#include <libasr/serialization.h>
#include <libasr/string_utils.h>
#include <libasr/pass/device_launch_expand.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/pass/gpu_offload_collect.h>
#include <libasr/pass/gpu_offload_preflight.h>
#include <libasr/pass/gpu_offload_rewrite.h>
#include <libasr/pass/gpu_offload_undo.h>
#include <libasr/pass/gpu_offload_visitor.h>
#include <libasr/pass/parallel_dispatch.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/replace_gpu_offload.h>

namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

static int gpu_kernel_counter = 0;

// A GPU backend was asked for, so a loop left on the host is a failure to
// deliver what was asked for: report it as an error and let the user opt
// into host execution with --gpu-allow-cpu-fallback. Every declining loop
// in the unit is reported before the compilation is stopped, so one run
// lists all of the gaps rather than only the first.
void GpuOffloadVisitor::report_not_offloaded(const Location &where,
        const GpuDecline &decline) {
    if (pass_options.diagnostics == nullptr) return;
    if (region_being_decided != nullptr &&
            !reported_regions.insert(region_being_decided).second) {
        return;
    }
    std::string why = gpu_decline_message(decline);
    // The class is what tells a gap in this compiler apart from a limit
    // of the device. The policy below does not act on it yet -- today a
    // decline of either class is an error unless the fallback is asked
    // for -- but `--gpu-decline-stats` makes the two countable, so that
    // the gaps can be worked through and the waiver list stays honest.
    if (pass_options.gpu_decline_stats) {
        std::cerr << "gpu-decline: " << gpu_decline_class_name(
            gpu_decline_class(decline, device_caps))
            << ": " << why << std::endl;
    }
    if (pass_options.gpu_allow_cpu_fallback) {
        pass_options.diagnostics->message_label(
            "parallel loop not offloaded to the GPU, "
            "it runs on the CPU instead",
            {where}, why,
            diag::Level::Warning, diag::Stage::ASRPass);
    } else {
        pass_options.diagnostics->message_label(
            "parallel loop cannot be offloaded to the GPU: " + why
                + "; pass `--gpu-allow-cpu-fallback` to run it on the "
                  "CPU instead",
            {where}, why,
            diag::Level::Error, diag::Stage::ASRPass);
    }
}

// A clause a kernel launch has no way to honour. The loop still runs on
// the device, which chooses its own layout, so say what was dropped
// rather than drop it in silence.
void GpuOffloadVisitor::report_clause_ignored(
        const Location &where, const std::string &name) {
    if (pass_options.diagnostics == nullptr) return;
    pass_options.diagnostics->message_label(
        "the '" + name + "' clause is ignored on the gpu",
        {where}, "the device decides how to run the iterations",
        diag::Level::Warning, diag::Stage::ASRPass);
}

// What a clause a launch cannot honour is called, or an empty name for
// one it can.
std::string GpuOffloadVisitor::unhonoured_clause(
        const ASR::omp_clause_t *clause) {
    switch (clause->type) {
        case ASR::omp_clauseType::OMPNumTeams: return "num_teams";
        case ASR::omp_clauseType::OMPThreadLimit: return "thread_limit";
        case ASR::omp_clauseType::OMPIf: return "if";
        case ASR::omp_clauseType::OMPSchedule: return "schedule";
        case ASR::omp_clauseType::OMPNumThreads: return "num_threads";
        default: return "";
    }
}

// A region this pass does not take is left exactly as it was, and is
// looked inside for the regions it can take.
void GpuOffloadVisitor::decline(const ASR::OMPRegion_t &x) {
    ASR::ASRPassBaseWalkVisitor<GpuOffloadVisitor>::visit_OMPRegion(x);
}

void GpuOffloadVisitor::visit_OMPRegion(const ASR::OMPRegion_t &region) {
    DecisionScope decision(*this, &region);
    if (!device_caps.device_selected()) {
        decline(region);
        return;
    }

    // Only the regions the dispatch pass gave to the device. Every other
    // exit of this function leaves the region alone, and the regions
    // still marked for the device once the pass is done are the ones it
    // declined; they are handed back to the host below.
    if (region.m_exec_target != ASR::exec_targetType::ExecDevice) {
        decline(region);
        return;
    }

    // Only a canonical parallel loop is offloaded: one region, one
    // perfectly nested loop nest, and the whole data environment in one
    // clause list. The kernel is built out of the nest.
    ParallelLoopNest nest;
    if (!parallel_loop_nest(region, nest)) {
        decline(region);
        return;
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
            return;
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
        return;
    }

    for (size_t d = 0; d < n_dims; d++) {
        if (!nest.head(d).m_v || !nest.head(d).m_start ||
                !nest.head(d).m_end) {
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::IncompleteLoopHead));
            return;
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
            return;
        }
    }

    // Everything below rewrites the loop as it goes -- inlining an
    // intrinsic, splicing a callee, gathering an argument -- and
    // several of those rewrites change a statement in place. A rewrite
    // must not reach the loop the host would run, because the offload
    // can still be declined further down. So the region's loop nest is
    // copied and it is the copy that is rewritten, the original
    // standing until the launch replaces it.
    //
    // A BLOCK or ASSOCIATE in the nest is copied with it. The kernel
    // takes that copy and the host keeps its own, so a rewrite on the
    // way to a kernel cannot reach the host, and a decline has
    // nothing to put back.
    ParallelLoopNest work;
    kernel_blocks.clear();
    // From here on the pass is drafting a kernel: it copies the blocks of
    // the nest into this scope and takes a kernel number. Every exit
    // below that leaves the loop on the host drops both, whichever exit
    // it is; the draft is handed to the kernel by committing the guard
    // once the launch is known to be supported.
    GpuKernelDraftGuard draft_guard(current_scope, kernel_blocks,
        gpu_kernel_counter);
    {
        ASRUtils::ExprStmtDuplicator dup(al);
        dup.allow_procedure_calls = true;
        dup.allow_reshape = true;
        ASR::stmt_t *loop_copy = copy_loop_stmt(region.m_body[0], dup);
        if (loop_copy == nullptr ||
                !parallel_loop_nest_of(loop_copy,
                    parallel_collapse_count(region), work)) {
            // Nothing here can be rewritten safely.
            decline(region);
            return;
        }
    }

    // Resolve associate variables to their original targets if this
    // loop is inside one or more nested AssociateBlocks.
    // The kernel function lives at the translation-unit level and
    // cannot reference symbols from any AssociateBlock's scope, so
    // we walk up through all enclosing AssociateBlock ancestors and
    // collect all their associate mappings.
    // The map is declared outside the block so it is available later
    // when resolving inner AssociateBlockCalls in the loop body.
    std::map<ASR::symbol_t*, ASR::expr_t*> enclosing_assoc_map;
    {
        std::map<ASR::symbol_t*, ASR::expr_t*> &assoc_map = enclosing_assoc_map;
        SymbolTable *scope = current_scope;
        while (scope && scope->asr_owner &&
               scope->asr_owner->type == ASR::asrType::symbol) {
            ASR::symbol_t *owner_sym = down_cast<ASR::symbol_t>(
                scope->asr_owner);
            if (is_a<ASR::Block_t>(*owner_sym)) {
                scope = scope->parent;
                continue;
            }
            if (!is_a<ASR::AssociateBlock_t>(*owner_sym)) break;
            ASR::AssociateBlock_t *ab =
                ASR::down_cast2<ASR::AssociateBlock_t>(scope->asr_owner);
            for (size_t i = 0; i < ab->n_body; i++) {
                if (is_a<ASR::Associate_t>(*ab->m_body[i])) {
                    ASR::Associate_t *assoc = down_cast<ASR::Associate_t>(
                        ab->m_body[i]);
                    if (is_a<ASR::Var_t>(*assoc->m_target)) {
                        ASR::symbol_t *assoc_sym =
                            down_cast<ASR::Var_t>(assoc->m_target)->m_v;
                        assoc_map[assoc_sym] = assoc->m_value;
                    }
                } else if (is_a<ASR::Assignment_t>(*ab->m_body[i])) {
                    // associate(n => constant_expr) generates an
                    // Assignment instead of Associate. Capture the
                    // initial value for variables owned by this
                    // AssociateBlock so they can be resolved.
                    // Only add if the symbol isn't already mapped
                    // (e.g., from a prior Associate node); otherwise
                    // we would overwrite the real alias with a
                    // regular assignment like `v = 0.`, whose RHS
                    // may reference `v` itself and cause infinite
                    // recursion during resolution.
                    // Only scalar selectors are inlined. An
                    // array-valued selector is materialized by the
                    // compiler into a temporary that lives in this
                    // AssociateBlock's symtab, and its defining
                    // expression may itself reference further
                    // AssociateBlock-local symbols (the array
                    // constant buffer, the resolved specific
                    // procedure of a generic constructor, ...).
                    // Inlining it would drag those unreachable
                    // symbols into the kernel. The associate name is
                    // backed by real storage here, so it is passed
                    // into the kernel as an ordinary buffer instead.
                    ASR::Assignment_t *asgn = down_cast<ASR::Assignment_t>(
                        ab->m_body[i]);
                    if (is_a<ASR::Var_t>(*asgn->m_target)) {
                        ASR::symbol_t *sym =
                            down_cast<ASR::Var_t>(asgn->m_target)->m_v;
                        if (is_a<ASR::Variable_t>(*sym) &&
                            down_cast<ASR::Variable_t>(sym)->m_parent_symtab
                                == ab->m_symtab &&
                            !ASRUtils::is_array(
                                down_cast<ASR::Variable_t>(sym)->m_type) &&
                            assoc_map.find(sym) == assoc_map.end()) {
                            assoc_map[sym] = asgn->m_value;
                        }
                    }
                }
            }
            scope = scope->parent;
        }
        if (!assoc_map.empty()) {
            AssociateVarResolver resolver(al, assoc_map);
            for (size_t d = 0; d < n_dims; d++) {
                ASR::do_loop_head_t &head = work.head(d);
                if (head.m_start) {
                    resolver.current_expr = &(head.m_start);
                    resolver.replace_expr(head.m_start);
                }
                if (head.m_end) {
                    resolver.current_expr = &(head.m_end);
                    resolver.replace_expr(head.m_end);
                }
                if (head.m_increment) {
                    resolver.current_expr = &(head.m_increment);
                    resolver.replace_expr(head.m_increment);
                }
            }
            AssociateVarResolverVisitor resolver_visitor(al, assoc_map);
            for (size_t i = 0; i < work.n_body; i++) {
                resolver_visitor.visit_stmt(*work.body[i]);
            }
            // The statement visitor above does not descend into
            // BlockCall targets (Blocks have their own scope), so
            // resolve associate aliases in both block body statements
            // and block-local type expressions (e.g., `real a(n)` where
            // `n` is an associate alias from an enclosing associate).
            // This must be recursive to handle nested blocks.
            std::function<void(ASR::stmt_t**, size_t)>
                resolve_assoc_in_blocks = [&](ASR::stmt_t **stmts,
                                              size_t n_stmts) {
                for (size_t i = 0; i < n_stmts; i++) {
                    if (!ASR::is_a<ASR::BlockCall_t>(*stmts[i])) continue;
                    ASR::BlockCall_t *bc =
                        ASR::down_cast<ASR::BlockCall_t>(stmts[i]);
                    if (!ASR::is_a<ASR::Block_t>(*bc->m_m)) continue;
                    ASR::Block_t *block =
                        ASR::down_cast<ASR::Block_t>(bc->m_m);
                    // Resolve in block body statements
                    for (size_t j = 0; j < block->n_body; j++) {
                        resolver_visitor.visit_stmt(*block->m_body[j]);
                    }
                    // Recurse into nested blocks
                    resolve_assoc_in_blocks(block->m_body,
                                            block->n_body);
                    // Resolve in block-local array dimension expressions
                    AssociateVarResolver type_resolver(al, assoc_map);
                    for (auto &item : block->m_symtab->get_scope()) {
                        if (!ASR::is_a<ASR::Variable_t>(*item.second))
                            continue;
                        ASR::Variable_t *var =
                            ASR::down_cast<ASR::Variable_t>(item.second);
                        if (!ASR::is_a<ASR::Array_t>(*var->m_type))
                            continue;
                        ASR::Array_t *arr =
                            ASR::down_cast<ASR::Array_t>(var->m_type);
                        for (size_t d = 0; d < arr->n_dims; d++) {
                            if (arr->m_dims[d].m_start) {
                                type_resolver.current_expr =
                                    &(arr->m_dims[d].m_start);
                                type_resolver.replace_expr(
                                    arr->m_dims[d].m_start);
                            }
                            if (arr->m_dims[d].m_length) {
                                type_resolver.current_expr =
                                    &(arr->m_dims[d].m_length);
                                type_resolver.replace_expr(
                                    arr->m_dims[d].m_length);
                            }
                        }
                    }
                }
            };
            resolve_assoc_in_blocks(work.body, work.n_body);
            // Resolve associate aliases in enclosing Block scopes'
            // variable type expressions. When a parallel loop is
            // inside a Block that is inside an AssociateBlock, the
            // block-local arrays may use associate variables in
            // their dimension expressions (e.g., `real r(size(n))`
            // where `n` is an associate alias). These must be
            // resolved before kernel extraction moves the block
            // into the kernel scope where the AssociateBlock's
            // symtab is no longer reachable.
            {
                SymbolTable *bs = current_scope;
                while (bs && bs->asr_owner &&
                       bs->asr_owner->type == ASR::asrType::symbol) {
                    ASR::symbol_t *owner = down_cast<ASR::symbol_t>(
                        bs->asr_owner);
                    if (is_a<ASR::Block_t>(*owner)) {
                        AssociateVarResolver type_resolver(al,
                            assoc_map);
                        for (auto &item : bs->get_scope()) {
                            if (!ASR::is_a<ASR::Variable_t>(
                                    *item.second))
                                continue;
                            ASR::Variable_t *var =
                                ASR::down_cast<ASR::Variable_t>(
                                    item.second);
                            if (!ASR::is_a<ASR::Array_t>(
                                    *var->m_type))
                                continue;
                            ASR::Array_t *arr =
                                ASR::down_cast<ASR::Array_t>(
                                    var->m_type);
                            for (size_t d = 0; d < arr->n_dims;
                                 d++) {
                                if (arr->m_dims[d].m_start) {
                                    type_resolver.current_expr =
                                        &(arr->m_dims[d].m_start);
                                    type_resolver.replace_expr(
                                        arr->m_dims[d].m_start);
                                }
                                if (arr->m_dims[d].m_length) {
                                    type_resolver.current_expr =
                                        &(arr->m_dims[d].m_length);
                                    type_resolver.replace_expr(
                                        arr->m_dims[d].m_length);
                                }
                            }
                        }
                        bs = bs->parent;
                    } else if (is_a<ASR::AssociateBlock_t>(*owner)) {
                        bs = bs->parent;
                    } else {
                        break;
                    }
                }
            }
        }
    }

    // Detect if the do concurrent is inside a Block scope. If so,
    // block-local variables need to be collected as kernel parameters
    // rather than skipped. Walk up through AssociateBlock and Block
    // parents to find ALL enclosing Block scopes (e.g., do concurrent
    // inside a nested Block that accesses variables from outer Blocks).
    std::set<SymbolTable*> enclosing_block_scopes;
    {
        SymbolTable *scope = current_scope;
        while (scope && scope->asr_owner &&
               scope->asr_owner->type == ASR::asrType::symbol) {
            ASR::symbol_t *owner_sym = down_cast<ASR::symbol_t>(
                scope->asr_owner);
            if (is_a<ASR::Block_t>(*owner_sym) ||
                is_a<ASR::AssociateBlock_t>(*owner_sym)) {
                enclosing_block_scopes.insert(scope);
                scope = scope->parent;
            } else {
                break;
            }
        }
    }

    // An element of an array of derived type reached through a
    // component -- `x%c_(k)` -- is copied to a temporary of this
    // scope before the launch, and the loop body reads the temporary.
    // This runs ahead of the checks below on purpose: they must judge
    // the shape the kernel would really be built from. The guard puts
    // the loop back untouched if any of them declines.
    Vec<ASR::stmt_t*> gather_stmts;
    gather_stmts.reserve(al, 1);
    Vec<ASR::stmt_t*> scatter_stmts;
    scatter_stmts.reserve(al, 1);
    std::vector<std::pair<ASR::expr_t**, ASR::expr_t*>> gather_undo;
    std::vector<std::string> gather_temp_names;
    GpuGatherGuard gather_guard(current_scope, gather_undo,
        gather_temp_names);
    // The gather is a copy the host makes before it launches, so this
    // holds for every dialect.
    if (!hoist_struct_element_gathers(work, gather_stmts,
            scatter_stmts, gather_undo, gather_temp_names)) {
        // The element could not be hoisted -- a subscript that moves
        // with the loop, or a write to the object that the copy back
        // after the launch could not reproduce exactly. Passing
        // the chain on unchanged reaches the device as a component of
        // the wrong element, which is a wrong number and no
        // diagnostic, so decline the loop instead.
        report_not_offloaded(loc,
            GpuDecline(GpuDeclineReason::StructElementGather));
        return;
    }

    // Decide whether this loop can be offloaded at all *before* any of
    // the inline_* helpers below rewrite the loop body. Those helpers
    // are destructive: they lower array-section and intrinsic-array
    // assignments into explicit element loops, a half-lowered shape
    // that only the kernel extractor understands. If we declined the
    // offload after rewriting, the loop would stay on the host in a
    // form the later array_op pass no longer normalizes, and codegen
    // would fail. So: no mutation until the decision is made.
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
            return;
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
            return;
        }
        // A strided section actual argument is gathered into a
        // contiguous kernel-local temporary below. When that temporary
        // cannot be sized at compile time the gather is impossible,
        // and passing the section on would silently drop its stride.
        if (body_has_ungatherable_strided_section(work.body, work.n_body)) {
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::UngatherableStridedSection));
            return;
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
            return;
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
            if (!gpu_device_can_represent_type(device_caps,
                    sym.second.first, sym.second.second)) {
                report_not_offloaded(loc, GpuDecline(
                    GpuDeclineReason::SymbolTypeNotRepresentable,
                    sym.first, scalar_type_of(sym.second.first)));
                return;
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
            return;
        }
    }

    // Splice the planned device functions into the loop body. This
    // must come first among the rewrites below: the intrinsic and
    // array-section inliners then see the spliced-in statements too.
    // The splice is recorded so that it can be undone: the workspace
    // pre-flight right below needs the spliced shape, but must still
    // be able to leave the loop untouched when it declines.
    GpuLoopBodySnapshot splice_snapshot;
    std::vector<ScopeArrayDims> scope_dims_undo;
    std::vector<std::pair<ASR::expr_t**, ASR::expr_t*>>
        member_extent_undo;
    GpuSpliceRestoreGuard splice_guard(splice_snapshot, scope_dims_undo);
    {
        splice_snapshot.record(work, current_scope);
        if (!inline_device_function_calls(work.body, work.n_body)) {
            functions_to_inline.clear();
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::DeviceFunctionInlining));
            return;
        }
        functions_to_inline.clear();
        // Run-time sized alias temporaries become BLOCK locals here,
        // ahead of the workspace pre-flight below, so that the
        // pre-flight sizes them too and can still decline the loop.
        materialize_runtime_alias_blocks(work);
        size_scope_array_temporaries(work.body, work.n_body,
            scope_dims_undo);
    }

    // Each run-time sized local of a kernel BLOCK becomes a per-thread
    // workspace buffer, which the host has to size before it launches
    // the kernel. An extent the host cannot work out from the kernel
    // arguments is a code generation error -- raised long after the
    // pass has committed to offloading, and so a hard build failure.
    // Run the backend's own resolution here instead, while the loop
    // can still be left on the host. This is the last point at which
    // it can be: the workspaces only exist once the callees are
    // spliced in, and the rewrites below are not reversible.
    //
    // The host sizes the workspace the same way whichever device it
    // launches on, so this holds for every dialect: an extent written
    // in terms of a spliced callee's own dummy names a symbol that no
    // longer exists once the callee is gone.
    //
    // What this cannot see, and what nothing here can: the workspaces
    // the passes after this one create. `subroutine_from_function`
    // turns a call whose result is an array into a temporary at the
    // call site, `array_struct_temporary` and `array_op` lower array
    // expressions into temporaries of their own, and every one of
    // those is a local of the kernel that does not exist yet. Over the
    // GPU corpus a third of the launches that carry a workspace at all
    // reach `device_launch_expand` with more of them than were counted
    // here. So this pre-flight is a filter, not a verdict: it keeps on
    // the host the loops it can already tell apart, and the launch
    // layout asks the same question again of the kernel that exists.
    {
        std::vector<std::string> kernel_arg_names;
        collect_kernel_arg_names(work, enclosing_block_scopes,
            kernel_arg_names);
        std::string unresolved_name;
        if (!gpu_block_workspace_extents_resolvable(work.body,
                work.n_body, kernel_arg_names, unresolved_name)) {
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::WorkspaceNotSizeableOnHost,
                    unresolved_name));
            return;
        }
    }
    // Splicing a callee is what can leave a section of a section in the
    // body, so the shape is only possible where the pass splices.
    if (device_caps.splices_device_functions()) {
        GpuNestedSectionFinder nested_section;
        for (size_t i = 0; i < work.n_body; i++) {
            nested_section.visit_stmt(*work.body[i]);
        }
        if (nested_section.found) {
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::NestedArraySection));
            return;
        }
    }

    // Inline IntrinsicArrayFunction All before kernel extraction
    all_reduction_targets.clear();
    inline_intrinsic_all(work);

    // Hoist matmuls out of the expression positions the matmul
    // lowering below cannot see them in.
    hoist_nested_matmuls(work);

    // Inline IntrinsicArrayFunction MatMul before kernel extraction
    inline_intrinsic_matmul(work);

    // Inline IntrinsicArrayFunction DotProduct before kernel
    // extraction
    inline_intrinsic_dot_product(work);

    // Inline IntrinsicArrayFunction Sum before kernel extraction
    inline_intrinsic_sum(work);

    // Also inline Sum in helper functions called from the
    // loop body. This ensures that sum(f(x)) patterns
    // inside helper functions are expanded into loops before
    // kernel extraction, avoiding allocatable temporaries that
    // cannot be represented as VLAs in Metal shaders.
    {
        GpuFunctionCollector sum_fc;
        for (size_t i = 0; i < work.n_body; i++) {
            sum_fc.visit_stmt(*work.body[i]);
        }
        bool sum_added = true;
        while (sum_added) {
            sum_added = false;
            GpuFunctionCollector sum_tc;
            for (auto &[fn_name, fn_sym] : sum_fc.functions) {
                ASR::symbol_t *resolved =
                    ASRUtils::symbol_get_past_external(fn_sym);
                if (ASR::is_a<ASR::Function_t>(*resolved)) {
                    ASR::Function_t *fn =
                        ASR::down_cast<ASR::Function_t>(resolved);
                    for (size_t i = 0; i < fn->n_body; i++) {
                        sum_tc.visit_stmt(*fn->m_body[i]);
                    }
                }
            }
            for (auto &[name, sym] : sum_tc.functions) {
                if (sum_fc.functions.find(name) ==
                        sum_fc.functions.end()) {
                    sum_fc.functions[name] = sym;
                    sum_added = true;
                }
            }
        }
        for (auto &[fn_name, fn_sym] : sum_fc.functions) {
            ASR::symbol_t *resolved =
                ASRUtils::symbol_get_past_external(fn_sym);
            if (ASR::is_a<ASR::Function_t>(*resolved)) {
                ASR::Function_t *fn =
                    ASR::down_cast<ASR::Function_t>(resolved);
                inline_dot_product_in_stmts(fn->m_body, fn->n_body,
                    fn->m_symtab);
                inline_sum_in_stmts(fn->m_body, fn->n_body,
                    fn->m_symtab);
            }
        }
    }

    // Inline IntrinsicArrayFunction Transpose before kernel extraction
    inline_intrinsic_transpose(work);

    // Materialise temporaries for assignments whose target and value
    // overlap the same array, before the element loops below are
    // built from them.
    materialize_aliased_assignments(
        work);

    // Inline ArraySection assignments before kernel extraction
    inline_array_section_assignment(
        work);

    // Inline whole-array elemental assignments (e.g., b = abs(a(:,l)))
    inline_elemental_array_var_assignment(
        work);

    // Recursive helper to inline an AssociateBlock's body.
    // Collects Associate mappings into assoc_map and non-Associate
    // statements into resolved_stmts. Handles nested
    // AssociateBlockCalls by recursing into inner blocks.
    std::function<void(ASR::AssociateBlock_t*,
                       std::map<ASR::symbol_t*, ASR::expr_t*>&,
                       Vec<ASR::stmt_t*>&)>
        inline_assoc_body = [&](ASR::AssociateBlock_t *ab,
                                std::map<ASR::symbol_t*, ASR::expr_t*> &assoc_map,
                                Vec<ASR::stmt_t*> &resolved_stmts) {
        for (size_t ai = 0; ai < ab->n_body; ai++) {
            if (ASR::is_a<ASR::Associate_t>(*ab->m_body[ai])) {
                ASR::Associate_t *assoc =
                    ASR::down_cast<ASR::Associate_t>(
                        ab->m_body[ai]);
                if (ASR::is_a<ASR::Var_t>(*assoc->m_target)) {
                    ASR::symbol_t *sym =
                        ASR::down_cast<ASR::Var_t>(
                            assoc->m_target)->m_v;
                    ASRUtils::ExprStmtDuplicator dup(al);
                    dup.success = true;
                    ASR::expr_t *value =
                        dup.duplicate_expr(assoc->m_value);
                    if (!assoc_map.empty()) {
                        AssociateVarResolver resolver(al, assoc_map);
                        resolver.current_expr = &value;
                        resolver.replace_expr(value);
                    }
                    assoc_map[sym] = value;
                }
            } else if (ASR::is_a<ASR::AssociateBlockCall_t>(
                           *ab->m_body[ai])) {
                ASR::AssociateBlockCall_t *inner_abc =
                    ASR::down_cast<ASR::AssociateBlockCall_t>(
                        ab->m_body[ai]);
                if (ASR::is_a<ASR::AssociateBlock_t>(
                        *inner_abc->m_m)) {
                    ASR::AssociateBlock_t *inner_ab =
                        ASR::down_cast<ASR::AssociateBlock_t>(
                            inner_abc->m_m);
                    Vec<ASR::stmt_t*> inner_stmts;
                    inner_stmts.reserve(al, inner_ab->n_body);
                    inline_assoc_body(inner_ab, assoc_map,
                        inner_stmts);
                    // Resolve the inner statements now: once they are
                    // wrapped in a Block below the caller's resolver
                    // no longer reaches them.
                    if (!assoc_map.empty()) {
                        AssociateVarResolverVisitor inner_resolver(
                            al, assoc_map);
                        for (size_t ii = 0; ii < inner_stmts.n; ii++) {
                            inner_resolver.visit_stmt(
                                *inner_stmts.p[ii]);
                        }
                    }
                    ASR::stmt_t *inner_call =
                        wrap_assoc_scope_in_block(inner_ab,
                            inner_stmts, ab->m_symtab);
                    if (inner_call) {
                        resolved_stmts.push_back(al, inner_call);
                    } else {
                        for (size_t ii = 0; ii < inner_stmts.n; ii++) {
                            resolved_stmts.push_back(al,
                                inner_stmts.p[ii]);
                        }
                        migrate_inlined_assoc_symbols(inner_ab,
                            ab->m_symtab);
                    }
                    std::string inner_name = inner_ab->m_name;
                    ab->m_symtab->erase_symbol(inner_name);
                } else {
                    resolved_stmts.push_back(al,
                        ab->m_body[ai]);
                }
            } else if (ASR::is_a<ASR::Assignment_t>(
                           *ab->m_body[ai])) {
                ASR::Assignment_t *asgn =
                    ASR::down_cast<ASR::Assignment_t>(
                        ab->m_body[ai]);
                if (ASR::is_a<ASR::Var_t>(*asgn->m_target)) {
                    ASR::symbol_t *sym =
                        ASR::down_cast<ASR::Var_t>(
                            asgn->m_target)->m_v;
                    if (ASR::is_a<ASR::Variable_t>(*sym) &&
                        ASR::down_cast<ASR::Variable_t>(sym)
                            ->m_parent_symtab == ab->m_symtab &&
                        assoc_map.find(sym) == assoc_map.end() &&
                        is_single_assignment_binding(sym,
                            ab->m_body, ab->n_body)) {
                        assoc_map[sym] = asgn->m_value;
                    } else {
                        resolved_stmts.push_back(al,
                            ab->m_body[ai]);
                    }
                } else {
                    resolved_stmts.push_back(al, ab->m_body[ai]);
                }
            } else {
                resolved_stmts.push_back(al, ab->m_body[ai]);
            }
        }
    };

    // Resolve AssociateBlocks inside the loop body (e.g.,
    // block { associate(nh => n) ... } within the loop). GPU kernels
    // cannot use Pointer-based associate aliases, so we inline the
    // associate targets and replace the AssociateBlockCall with the
    // resolved statements.
    for (size_t bi = 0; bi < work.n_body; bi++) {
        if (!ASR::is_a<ASR::BlockCall_t>(*work.body[bi])) continue;
        ASR::BlockCall_t *bc = ASR::down_cast<ASR::BlockCall_t>(
            work.body[bi]);
        if (!ASR::is_a<ASR::Block_t>(*bc->m_m)) continue;
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(bc->m_m);
        Vec<ASR::stmt_t*> new_block_body;
        new_block_body.reserve(al, block->n_body);
        bool changed = false;
        for (size_t si = 0; si < block->n_body; si++) {
            if (!ASR::is_a<ASR::AssociateBlockCall_t>(
                    *block->m_body[si])) {
                new_block_body.push_back(al, block->m_body[si]);
                continue;
            }
            ASR::AssociateBlockCall_t *abc =
                ASR::down_cast<ASR::AssociateBlockCall_t>(
                    block->m_body[si]);
            if (!ASR::is_a<ASR::AssociateBlock_t>(*abc->m_m)) {
                new_block_body.push_back(al, block->m_body[si]);
                continue;
            }
            ASR::AssociateBlock_t *ab =
                ASR::down_cast<ASR::AssociateBlock_t>(abc->m_m);
            std::map<ASR::symbol_t*, ASR::expr_t*> assoc_map(
                enclosing_assoc_map);
            Vec<ASR::stmt_t*> resolved_stmts;
            resolved_stmts.reserve(al, ab->n_body);
            inline_assoc_body(ab, assoc_map, resolved_stmts);
            if (!assoc_map.empty()) {
                AssociateVarResolverVisitor resolver(al, assoc_map);
                for (size_t ri = 0; ri < resolved_stmts.n; ri++) {
                    resolver.visit_stmt(*resolved_stmts.p[ri]);
                }
            }
            // If the AssociateBlock still owns variables referenced by
            // the resolved statements (an array-valued selector
            // temporary), keep the scope alive as a Block instead of
            // dropping it.
            ASR::stmt_t *block_call = wrap_assoc_scope_in_block(
                ab, resolved_stmts, block->m_symtab);
            if (block_call) {
                new_block_body.push_back(al, block_call);
            } else {
                for (size_t ri = 0; ri < resolved_stmts.n; ri++) {
                    new_block_body.push_back(al, resolved_stmts.p[ri]);
                }
                // Migrate ExternalSymbol entries (e.g., type-bound
                // procedure references like `1_t_f`) from the
                // AssociateBlock's symtab to the enclosing scope
                // before erasing it. These symbols are still
                // referenced by FunctionCall/SubroutineCall nodes
                // in the resolved statements and must remain
                // reachable for import_struct_def.
                migrate_inlined_assoc_symbols(ab, block->m_symtab);
            }
            std::string ab_name = ab->m_name;
            block->m_symtab->erase_symbol(ab_name);
            changed = true;
        }
        if (changed) {
            block->m_body = new_block_body.p;
            block->n_body = new_block_body.n;
        }
    }

    // Resolve bare AssociateBlockCall statements directly in the
    // loop body (not wrapped in a BlockCall). GPU kernels
    // cannot use Pointer-based associate aliases, so we inline the
    // associate targets and replace each AssociateBlockCall with
    // the resolved statements.
    {
        Vec<ASR::stmt_t*> new_dc_body;
        new_dc_body.reserve(al, work.n_body);
        bool dc_changed = false;
        for (size_t bi = 0; bi < work.n_body; bi++) {
            if (!ASR::is_a<ASR::AssociateBlockCall_t>(*work.body[bi])) {
                new_dc_body.push_back(al, work.body[bi]);
                continue;
            }
            ASR::AssociateBlockCall_t *abc =
                ASR::down_cast<ASR::AssociateBlockCall_t>(
                    work.body[bi]);
            if (!ASR::is_a<ASR::AssociateBlock_t>(*abc->m_m)) {
                new_dc_body.push_back(al, work.body[bi]);
                continue;
            }
            ASR::AssociateBlock_t *ab =
                ASR::down_cast<ASR::AssociateBlock_t>(abc->m_m);
            // Start with mappings from enclosing AssociateBlocks so
            // that references to outer associate variables (e.g., `m`
            // from an outer `associate(m => n)`) are resolved even
            // when they appear inside an inner associate block.
            std::map<ASR::symbol_t*, ASR::expr_t*> assoc_map(
                enclosing_assoc_map);
            Vec<ASR::stmt_t*> resolved_stmts;
            resolved_stmts.reserve(al, ab->n_body);
            inline_assoc_body(ab, assoc_map, resolved_stmts);
            if (!assoc_map.empty()) {
                AssociateVarResolverVisitor resolver(al, assoc_map);
                for (size_t ri = 0; ri < resolved_stmts.n; ri++) {
                    resolver.visit_stmt(*resolved_stmts.p[ri]);
                }
            }
            // Keep the scope alive as a Block when it still owns
            // variables referenced by the resolved statements.
            ASR::stmt_t *block_call = wrap_assoc_scope_in_block(
                ab, resolved_stmts, current_scope);
            if (block_call) {
                new_dc_body.push_back(al, block_call);
            } else {
                for (size_t ri = 0; ri < resolved_stmts.n; ri++) {
                    new_dc_body.push_back(al, resolved_stmts.p[ri]);
                }
                // Migrate ExternalSymbol entries from the
                // AssociateBlock's symtab to the enclosing scope
                // before erasing it (same as above for BlockCall).
                migrate_inlined_assoc_symbols(ab, current_scope);
            }
            std::string ab_name = ab->m_name;
            current_scope->erase_symbol(ab_name);
            dc_changed = true;
        }
        if (dc_changed) {
            work.body = new_dc_body.p;
            work.n_body = new_dc_body.n;
        }
    }

    // A strided section actual argument cannot be handed to a device
    // function as a base pointer; gather it into a contiguous
    // temporary first. This runs after the ASSOCIATE scopes above have
    // been inlined, so a section whose bounds come from an associate
    // name is gathered with the selector substituted in.
    gather_strided_section_arguments(
        work);

    // 1. Collect all symbols from body AND head expressions
    std::map<std::string, std::pair<ASR::ttype_t*, ASR::expr_t*>> involved_syms;
    collect_involved_syms(work, enclosing_block_scopes, involved_syms);

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
            return;
        }
    }

    // A statement no device can run keeps the loop on the CPU whichever
    // backend is selected.
    {
        GpuUnsupportedStatementFinder finder;
        for (size_t i = 0; i < work.n_body; i++) {
            finder.visit_stmt(*work.body[i]);
        }
        std::string in_routine;
        if (finder.reason == GpuDeclineReason::None) {
            for (ASR::Function_t *fn : reachable_routines(work.body,
                    work.n_body)) {
                GpuUnsupportedStatementFinder callee_finder;
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
            return;
        }
    }

    // Collect loop variable names
    std::vector<std::string> loop_var_names;
    for (size_t d = 0; d < n_dims; d++) {
        ASR::Var_t *lv = down_cast<ASR::Var_t>(work.head(d).m_v);
        loop_var_names.push_back(ASRUtils::symbol_name(lv->m_v));
    }

    // Find local scalar temporaries (assigned but not arrays, not loop vars)
    std::set<std::string> local_vars, assigned_vars;
    GpuLocalVarCollector lv_collector(local_vars, assigned_vars, enclosing_block_scopes);
    for (size_t i = 0; i < work.n_body; i++) {
        lv_collector.visit_stmt(*work.body[i]);
    }

    // Separate into kernel params vs local vars
    // Params: arrays + scalars that are read but NOT assigned in loop body
    // (unless they're also read from arrays, in which case they're params)
    // Local: scalars that are assigned in the loop body and not arrays
    std::set<std::string> loop_var_set(loop_var_names.begin(), loop_var_names.end());

    // Remove loop variables from involved_syms (kernel computes them)
    for (auto &lvn : loop_var_names) {
        involved_syms.erase(lvn);
    }

    // Identify which symbols are local temporaries (assigned scalar, non-array)
    // vs kernel parameters (arrays or read-only scalars).
    // Assigned scalars are kernel-local unless they are reduction
    // targets from inlined all(), or are referenced after the do
    // concurrent loop (liveout) — those need to be communicated back
    // to the host via 1-element array device buffers.

    // Collect variables referenced in statements after this parallel loop
    // in the parent body, to identify liveout scalars.
    std::set<std::string> post_loop_vars;
    {
        ASR::stmt_t **parent_body = nullptr;
        size_t parent_n_body = 0;
        SymbolTable *scope = current_scope;
        while (scope && scope->asr_owner) {
            if (scope->asr_owner->type == ASR::asrType::symbol) {
                ASR::symbol_t *owner_sym = ASR::down_cast<ASR::symbol_t>(
                    scope->asr_owner);
                if (ASR::is_a<ASR::Program_t>(*owner_sym)) {
                    ASR::Program_t *prog = ASR::down_cast<ASR::Program_t>(
                        owner_sym);
                    parent_body = prog->m_body;
                    parent_n_body = prog->n_body;
                    break;
                } else if (ASR::is_a<ASR::Function_t>(*owner_sym)) {
                    ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(
                        owner_sym);
                    parent_body = fn->m_body;
                    parent_n_body = fn->n_body;
                    break;
                } else if (ASR::is_a<ASR::Block_t>(*owner_sym)) {
                    ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(
                        owner_sym);
                    parent_body = blk->m_body;
                    parent_n_body = blk->n_body;
                    break;
                }
            }
            scope = scope->parent;
        }
        if (parent_body) {
            bool found_dc = false;
            for (size_t si = 0; si < parent_n_body; si++) {
                if (!found_dc) {
                    if (parent_body[si]->base.loc.first == loc.first &&
                            parent_body[si]->base.loc.last == loc.last) {
                        found_dc = true;
                    }
                    continue;
                }
                PostLoopVarCollector plvc(post_loop_vars);
                plvc.visit_stmt(*parent_body[si]);
            }
        }
    }

    std::set<std::string> local_scalar_names;
    for (auto &name : assigned_vars) {
        if (loop_var_set.count(name)) continue;
        if (all_reduction_targets.count(name)) continue;
        if (post_loop_vars.count(name)) continue;
        auto it = involved_syms.find(name);
        if (it != involved_syms.end()) {
            ASR::ttype_t *type = it->second.first;
            if (!ASRUtils::is_array(type)) {
                local_scalar_names.insert(name);
            }
        }
    }

    // Remove local scalars from involved_syms (they become kernel locals)
    for (auto &name : local_scalar_names) {
        involved_syms.erase(name);
    }

    // Collect optional variables from involved_syms. When an optional
    // argument is used inside a loop body guarded by present(),
    // the kernel launch and all buffer setup must be skipped when the
    // argument is not present, otherwise the host will segfault trying
    // to read a null descriptor.
    std::vector<ASR::symbol_t*> optional_syms;
    for (auto &[sym_name, sym_info] : involved_syms) {
        ASR::symbol_t *sym = this->current_scope->resolve_symbol(sym_name);
        if (!sym) continue;
        ASR::symbol_t *resolved = ASRUtils::symbol_get_past_external(sym);
        if (!ASR::is_a<ASR::Variable_t>(*resolved)) continue;
        ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(resolved);
        if (var->m_presence == ASR::presenceType::Optional) {
            optional_syms.push_back(sym);
        }
    }

    // Wrap liveout scalars (assigned user variables still in
    // involved_syms) in 1-element FixedSizeArrays so they can be
    // passed as writable device buffers and read back after the kernel.
    struct LiveoutScalarInfo {
        std::string orig_name;
        std::string buf_name;
        ASR::symbol_t *host_buf_sym;
        ASR::symbol_t *orig_scalar_sym;
        ASR::ttype_t *scalar_type;
    };
    std::vector<LiveoutScalarInfo> liveout_scalars;
    {
        ASR::ttype_t *int4_type = ASRUtils::TYPE(
            ASR::make_Integer_t(al, loc, 4));
        std::vector<std::string> liveout_names;
        for (auto &name : assigned_vars) {
            if (loop_var_set.count(name)) continue;
            if (local_scalar_names.count(name)) continue;
            auto it = involved_syms.find(name);
            if (it != involved_syms.end()) {
                ASR::ttype_t *type = it->second.first;
                if (!ASRUtils::is_array(type)) {
                    liveout_names.push_back(name);
                }
            }
        }
        for (auto &name : liveout_names) {
            auto it = involved_syms.find(name);
            ASR::ttype_t *scalar_type = it->second.first;
            ASR::symbol_t *orig_sym = current_scope->resolve_symbol(name);

            ASR::dimension_t dim;
            dim.loc = loc;
            dim.m_start = ASRUtils::EXPR(
                ASR::make_IntegerConstant_t(al, loc, 1, int4_type,
                    ASR::integerbozType::Decimal));
            dim.m_length = ASRUtils::EXPR(
                ASR::make_IntegerConstant_t(al, loc, 1, int4_type,
                    ASR::integerbozType::Decimal));
            Vec<ASR::dimension_t> dims_vec;
            dims_vec.reserve(al, 1);
            dims_vec.push_back(al, dim);
            ASR::ttype_t *arr_type = ASRUtils::TYPE(
                ASR::make_Array_t(al, loc,
                    ASRUtils::duplicate_type(al, scalar_type),
                    dims_vec.p, 1,
                    ASR::array_physical_typeType::FixedSizeArray, ASR::memory_spaceType::Global));

            std::string buf_name = current_scope->get_unique_name(
                "__gpu_buf_" + name);
            ASR::symbol_t *buf_sym = gpu_new_variable(al, loc,
                current_scope, buf_name, ASRUtils::duplicate_type(al,
                    arr_type));
            it->second.first = arr_type;

            liveout_scalars.push_back(
                {name, buf_name, buf_sym, orig_sym, scalar_type});
        }
    }

    // Decompose struct variables with allocatable array members.
    // Metal cannot represent allocatable descriptors inside structs,
    // so we extract each allocatable array member into a separate
    // kernel buffer parameter and replace StructInstanceMember
    // references in the body with the new flat-array Var.
    GpuAllocStructMemberCollector alloc_collector;
    for (size_t i = 0; i < work.n_body; i++) {
        alloc_collector.visit_stmt(*work.body[i]);
    }
    // Also scan array dimension expressions of involved symbols for
    // StructInstanceMember accesses. VLA arrays sized by struct
    // members (e.g., `integer :: n(x%m)`) constitute a non-allocatable
    // access that must prevent struct removal from involved_syms.
    for (auto &[sym_name, sym_info] : involved_syms) {
        ASR::symbol_t *sym = current_scope->resolve_symbol(sym_name);
        if (!sym || !ASR::is_a<ASR::Variable_t>(*sym)) continue;
        ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
        if (!ASR::is_a<ASR::Array_t>(*var->m_type)) continue;
        ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(var->m_type);
        for (size_t d = 0; d < arr->n_dims; d++) {
            if (arr->m_dims[d].m_start)
                alloc_collector.visit_expr(*arr->m_dims[d].m_start);
            if (arr->m_dims[d].m_length)
                alloc_collector.visit_expr(*arr->m_dims[d].m_length);
        }
    }
    // Maps (struct_name, member_name) -> decomposed parameter name
    std::map<std::pair<std::string, std::string>, std::string>
        decomp_map;
    // Info for creating host-side call arguments later
    struct DecompInfo {
        std::string struct_name;
        std::string member_name;
        std::string param_name;
        ASR::symbol_t *orig_mem_sym;
        ASR::ttype_t *alloc_type;
    };
    std::vector<DecompInfo> decomp_infos;
    for (auto &[struct_name, members] :
            alloc_collector.alloc_members) {
        if (involved_syms.find(struct_name) == involved_syms.end())
            continue;
        for (auto &[mem_name, mem_info] : members) {
            std::string param_name = struct_name + "__" + mem_name;
            decomp_map[{struct_name, mem_name}] = param_name;
            decomp_infos.push_back({struct_name, mem_name,
                param_name, mem_info.first, mem_info.second});
        }
        // If struct only accessed through allocatable members,
        // remove from involved_syms (it won't be passed as a
        // kernel parameter)
        if (alloc_collector.has_non_alloc_access.find(struct_name)
                == alloc_collector.has_non_alloc_access.end()) {
            involved_syms.erase(struct_name);
        }
    }

    // 2. Create kernel scope and parameters
    SymbolTable *tu_symtab = tu.m_symtab;
    std::string kernel_name = tu_symtab->get_unique_name(
        "__lfortran_gpu_kernel_" + std::to_string(gpu_kernel_counter++));
    SymbolTable *kernel_scope = al.make_new<SymbolTable>(tu_symtab);

    Vec<ASR::expr_t*> kernel_args;
    kernel_args.reserve(al, involved_syms.size());
    Vec<ASR::call_arg_t> call_args;
    call_args.reserve(al, involved_syms.size());

    SymbolTable *orig_scope = this->current_scope;

    for (auto &[sym_name, sym_info] : involved_syms) {
        ASR::ttype_t *type = sym_info.first;

        // For struct-typed variables, import the Struct into kernel scope
        ASR::symbol_t *type_decl = nullptr;
        ASR::symbol_t *orig_sym = orig_scope->resolve_symbol(sym_name);
        if (orig_sym == nullptr && sym_info.second != nullptr
                && ASR::is_a<ASR::Var_t>(*sym_info.second)) {
            // A name the loop body reads that the enclosing scope
            // cannot look up: a module `parameter` that a spliced-in
            // callee uses and the caller's `use ... , only:` list
            // leaves out. The reference the body carries still names
            // the symbol, so take it from there -- without it the
            // launch is handed an argument built over a null symbol.
            orig_sym = ASR::down_cast<ASR::Var_t>(sym_info.second)->m_v;
        }
        if (orig_sym) {
            type_decl = import_struct_type(orig_sym,
                orig_scope, kernel_scope, loc);
        }

        // Parameter variables are compile-time constants without
        // runtime storage. Clone them into the kernel scope with
        // their values preserved instead of passing as GPU buffers.
        if (orig_sym) {
            ASR::symbol_t *resolved =
                ASRUtils::symbol_get_past_external(orig_sym);
            if (ASR::is_a<ASR::Variable_t>(*resolved)) {
                ASR::Variable_t *orig_var =
                    ASR::down_cast<ASR::Variable_t>(resolved);
                if (orig_var->m_storage ==
                        ASR::storage_typeType::Parameter) {
                    ASR::symbol_t *cloned =
                        ASR::down_cast<ASR::symbol_t>(
                            ASRUtils::make_Variable_t_util(al, loc,
                                kernel_scope, s2c(al, sym_name),
                                nullptr, 0,
                                ASR::intentType::Local,
                                orig_var->m_value,
                                orig_var->m_value,
                                ASR::storage_typeType::Parameter,
                                ASRUtils::duplicate_type(al,
                                    orig_var->m_type),
                                nullptr, orig_var->m_abi,
                                orig_var->m_access,
                                ASR::presenceType::Required, false));
                    kernel_scope->add_symbol(sym_name, cloned);
                    continue;
                }
            }
        }

        // Strip Allocatable/Pointer wrapper: GPU kernel parameters
        // receive raw array data, not array descriptors
        ASR::ttype_t *dup_type = ASRUtils::duplicate_type(al,
            ASRUtils::type_get_past_allocatable_pointer(type));

        // Recompute dependencies from the type alone (symbolic_value
        // and value are nullptr for kernel parameters)
        SetChar deps_vec;
        deps_vec.reserve(al, 1);
        ASRUtils::collect_variable_dependencies(
            al, deps_vec, dup_type, nullptr, nullptr, sym_name);

        ASR::symbol_t *param = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(al, loc, kernel_scope,
                s2c(al, sym_name), deps_vec.p, deps_vec.size(),
                ASR::intentType::InOut, nullptr, nullptr,
                ASR::storage_typeType::Default, dup_type,
                type_decl, ASR::abiType::Source,
                ASR::accessType::Public, ASR::presenceType::Required, false));
        kernel_scope->add_symbol(sym_name, param);
        kernel_args.push_back(al,
            ASRUtils::EXPR(ASR::make_Var_t(al, loc, param)));

        ASR::call_arg_t carg;
        carg.loc = loc;
        // For liveout scalars, use the host-side 1-element array
        // buffer as the call arg so it's passed as a device buffer
        bool is_liveout = false;
        for (auto &ls : liveout_scalars) {
            if (ls.orig_name == sym_name) {
                carg.m_value = ASRUtils::EXPR(
                    ASR::make_Var_t(al, loc, ls.host_buf_sym));
                is_liveout = true;
                break;
            }
        }
        if (!is_liveout) {
            carg.m_value = ASRUtils::EXPR(
                ASR::make_Var_t(al, loc, orig_sym));
        }
        call_args.push_back(al, carg);
    }

    // Create kernel parameters for decomposed allocatable struct
    // members. Each allocatable array member becomes a separate
    // flat-array buffer parameter.
    for (auto &di : decomp_infos) {
        ASR::ttype_t *flat_type = ASRUtils::duplicate_type(al,
            ASRUtils::type_get_past_allocatable(di.alloc_type));

        ASR::symbol_t *flat_type_decl = nullptr;
        if (ASR::is_a<ASR::StructType_t>(
                *ASRUtils::extract_type(flat_type))) {
            ASR::symbol_t *mem_resolved =
                ASRUtils::symbol_get_past_external(di.orig_mem_sym);
            if (is_a<ASR::Variable_t>(*mem_resolved)) {
                ASR::Variable_t *mv =
                    down_cast<ASR::Variable_t>(mem_resolved);
                if (mv->m_type_declaration) {
                    ASR::symbol_t *inner_struct_sym =
                        ASRUtils::symbol_get_past_external(
                            mv->m_type_declaration);
                    if (is_a<ASR::Struct_t>(*inner_struct_sym)) {
                        flat_type_decl = import_struct_def(
                            down_cast<ASR::Struct_t>(inner_struct_sym),
                            orig_scope, kernel_scope, loc);
                    }
                }
            }
        }

        SetChar deps_vec;
        deps_vec.reserve(al, 1);
        ASRUtils::collect_variable_dependencies(
            al, deps_vec, flat_type, nullptr, nullptr, di.param_name);

        ASR::symbol_t *param = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(al, loc, kernel_scope,
                s2c(al, di.param_name), deps_vec.p, deps_vec.size(),
                ASR::intentType::InOut, nullptr, nullptr,
                ASR::storage_typeType::Default, flat_type,
                flat_type_decl, ASR::abiType::Source,
                ASR::accessType::Public,
                ASR::presenceType::Required, false));
        kernel_scope->add_symbol(di.param_name, param);
        kernel_args.push_back(al,
            ASRUtils::EXPR(ASR::make_Var_t(al, loc, param)));

        // Host-side: pass StructInstanceMember(Var(x), member)
        ASR::symbol_t *orig_struct_sym =
            orig_scope->resolve_symbol(di.struct_name);
        ASR::call_arg_t carg;
        carg.loc = loc;
        carg.m_value = ASRUtils::EXPR(
            ASR::make_StructInstanceMember_t(al, loc,
                ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                    orig_struct_sym)),
                di.orig_mem_sym, di.alloc_type, nullptr));
        call_args.push_back(al, carg);
    }

    // Pass dimension sizes for decomposed allocatable struct
    // members so the kernel can compute ArraySize and strides.
    {
        ASR::ttype_t *int_type_dim = ASRUtils::TYPE(
            ASR::make_Integer_t(al, loc, 4));
        for (auto &di : decomp_infos) {
            ASR::ttype_t *inner =
                ASRUtils::type_get_past_allocatable(di.alloc_type);
            if (!ASR::is_a<ASR::Array_t>(*inner)) continue;
            ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(inner);

            ASR::symbol_t *k_sym =
                kernel_scope->get_symbol(di.param_name);
            LCOMPILERS_ASSERT(k_sym);
            ASR::Variable_t *k_var =
                ASR::down_cast<ASR::Variable_t>(k_sym);
            ASR::Array_t *k_arr = ASR::down_cast<ASR::Array_t>(
                ASRUtils::type_get_past_allocatable(k_var->m_type));

            ASR::symbol_t *orig_struct_sym =
                orig_scope->resolve_symbol(di.struct_name);
            ASR::expr_t *host_member_expr = ASRUtils::EXPR(
                ASR::make_StructInstanceMember_t(al, loc,
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                        orig_struct_sym)),
                    di.orig_mem_sym, di.alloc_type, nullptr));

            for (size_t d = 0; d < arr->n_dims; d++) {
                std::string dim_name =
                    GpuNames::dim_arg(di.param_name, d);
                ASR::symbol_t *dim_sym = gpu_new_variable(al, loc,
                    kernel_scope, dim_name, ASRUtils::duplicate_type(al,
                        int_type_dim), ASR::intentType::InOut);
                kernel_args.push_back(al,
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                        dim_sym)));

                ASR::expr_t *dim_expr = ASRUtils::EXPR(
                    ASR::make_IntegerConstant_t(al, loc,
                        (int64_t)(d + 1), int_type_dim,
                        ASR::integerbozType::Decimal));
                ASR::expr_t *host_size = ASRUtils::EXPR(
                    ASR::make_ArraySize_t(al, loc,
                        host_member_expr, dim_expr,
                        int_type_dim, nullptr));
                ASR::call_arg_t carg;
                carg.loc = loc;
                carg.m_value = host_size;
                call_args.push_back(al, carg);

                k_arr->m_dims[d].m_length = ASRUtils::EXPR(
                    ASR::make_Var_t(al, loc, dim_sym));
                if (!k_arr->m_dims[d].m_start) {
                    // Pass lower bound as kernel parameter
                    std::string lb_name = GpuNames::lower_bound(
                        di.param_name, d);
                    ASR::symbol_t *lb_sym = gpu_new_variable(al, loc,
                        kernel_scope, lb_name, ASRUtils::duplicate_type(al,
                            int_type_dim), ASR::intentType::InOut);
                    kernel_args.push_back(al,
                        ASRUtils::EXPR(ASR::make_Var_t(al, loc, lb_sym)));
                    // Host-side value: lbound(struct%member, dim=d+1)
                    ASR::expr_t *lb_dim_expr = ASRUtils::EXPR(
                        ASR::make_IntegerConstant_t(al, loc, (int64_t)(d + 1),
                            int_type_dim, ASR::integerbozType::Decimal));
                    ASR::expr_t *host_lb = ASRUtils::EXPR(
                        ASR::make_ArrayBound_t(al, loc,
                            host_member_expr, lb_dim_expr,
                            int_type_dim,
                            ASR::arrayboundType::LBound, nullptr));
                    ASR::call_arg_t lb_carg;
                    lb_carg.loc = loc;
                    lb_carg.m_value = host_lb;
                    call_args.push_back(al, lb_carg);
                    k_arr->m_dims[d].m_start = ASRUtils::EXPR(
                        ASR::make_Var_t(al, loc, lb_sym));
                }
            }
        }
    }
    for (auto &[sym_name, sym_info] : involved_syms) {
        ASR::ttype_t *orig_type = sym_info.first;
        // Any array whose shape is not known until run time: a
        // deferred-shape allocatable or pointer, and an automatic
        // array like `z(size(x), 3)` just the same. The kernel
        // computes strides from those extents, so it has to be given
        // them; without them it linearizes with a zero stride and
        // writes every column over the first.
        ASR::ttype_t *inner =
            ASRUtils::type_get_past_allocatable_pointer(orig_type);
        if (!ASR::is_a<ASR::Array_t>(*inner)) continue;
        ASR::Array_t *orig_arr = ASR::down_cast<ASR::Array_t>(inner);
        // An explicit-shape array says what it can at compile time, so
        // only the dimensions it cannot are passed; a deferred-shape
        // one says nothing, and passes them all.
        bool shape_from_type =
            !ASRUtils::is_allocatable_or_pointer(orig_type);
        if (shape_from_type) {
            bool runtime_shape = false;
            for (size_t d = 0; d < orig_arr->n_dims; d++) {
                ASR::expr_t *len = orig_arr->m_dims[d].m_length;
                if (len == nullptr ||
                        ASRUtils::expr_value(len) == nullptr) {
                    runtime_shape = true;
                    break;
                }
            }
            if (!runtime_shape) continue;
        }

        // Locate the kernel-scope Variable whose type we must update
        ASR::symbol_t *k_sym = kernel_scope->get_symbol(sym_name);
        LCOMPILERS_ASSERT(k_sym);
        ASR::Variable_t *k_var = ASR::down_cast<ASR::Variable_t>(k_sym);
        ASR::ttype_t *k_type = k_var->m_type;
        if (!ASR::is_a<ASR::Array_t>(
                *ASRUtils::type_get_past_allocatable_pointer(k_type))) {
            continue;
        }
        ASR::Array_t *k_arr = ASR::down_cast<ASR::Array_t>(
            ASRUtils::type_get_past_allocatable_pointer(k_type));

        ASR::symbol_t *orig_sym = orig_scope->resolve_symbol(sym_name);
        ASR::ttype_t *int_type_dim = ASRUtils::TYPE(
            ASR::make_Integer_t(al, loc, 4));

        for (size_t d = 0; d < k_arr->n_dims; d++) {
            if (shape_from_type && d < orig_arr->n_dims
                    && orig_arr->m_dims[d].m_length != nullptr
                    && ASRUtils::expr_value(
                        orig_arr->m_dims[d].m_length) != nullptr) {
                continue;
            }
            std::string dim_name = GpuNames::dim_arg(sym_name, d);
            ASR::symbol_t *dim_sym = gpu_new_variable(al, loc,
                kernel_scope, dim_name, ASRUtils::duplicate_type(al,
                    int_type_dim), ASR::intentType::InOut);
            kernel_args.push_back(al,
                ASRUtils::EXPR(ASR::make_Var_t(al, loc, dim_sym)));

            // Host-side value: size(arr, dim=d+1)
            ASR::expr_t *dim_expr = ASRUtils::EXPR(
                ASR::make_IntegerConstant_t(al, loc, (int64_t)(d + 1),
                    int_type_dim, ASR::integerbozType::Decimal));
            ASR::expr_t *host_size = ASRUtils::EXPR(
                ASR::make_ArraySize_t(al, loc,
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc, orig_sym)),
                    dim_expr, int_type_dim, nullptr));
            ASR::call_arg_t carg;
            carg.loc = loc;
            carg.m_value = host_size;
            call_args.push_back(al, carg);

            // Set dimension length in kernel-scope array type
            k_arr->m_dims[d].m_length = ASRUtils::EXPR(
                ASR::make_Var_t(al, loc, dim_sym));
            if (!k_arr->m_dims[d].m_start) {
                // Pass lower bound as kernel parameter
                std::string lb_name = GpuNames::lower_bound(
                    sym_name, d);
                ASR::symbol_t *lb_sym = gpu_new_variable(al, loc,
                    kernel_scope, lb_name, ASRUtils::duplicate_type(al,
                        int_type_dim), ASR::intentType::InOut);
                kernel_args.push_back(al,
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc, lb_sym)));
                // Host-side value: lbound(arr, dim=d+1)
                ASR::expr_t *lb_dim_expr = ASRUtils::EXPR(
                    ASR::make_IntegerConstant_t(al, loc, (int64_t)(d + 1),
                        int_type_dim, ASR::integerbozType::Decimal));
                ASR::expr_t *host_lb = ASRUtils::EXPR(
                    ASR::make_ArrayBound_t(al, loc,
                        ASRUtils::EXPR(ASR::make_Var_t(al, loc, orig_sym)),
                        lb_dim_expr, int_type_dim,
                        ASR::arrayboundType::LBound, nullptr));
                ASR::call_arg_t lb_carg;
                lb_carg.loc = loc;
                lb_carg.m_value = host_lb;
                call_args.push_back(al, lb_carg);
                k_arr->m_dims[d].m_start = ASRUtils::EXPR(
                    ASR::make_Var_t(al, loc, lb_sym));
            }
        }
    }

    // Add total-size kernel parameters for allocatable array members
    // of struct-typed kernel parameters. These sizes are needed by
    // Metal inline functions that call size() on struct members.
    // Skip array-of-structs variables — StructInstanceMember requires
    // a scalar struct base, not an array of structs.
    for (auto &[sym_name, sym_info] : involved_syms) {
        ASR::ttype_t *type = sym_info.first;
        ASR::ttype_t *inner_t = ASRUtils::type_get_past_allocatable(type);
        if (ASRUtils::is_array(inner_t)) continue;
        if (!ASR::is_a<ASR::StructType_t>(
                *ASRUtils::extract_type(inner_t)))
            continue;
        ASR::symbol_t *orig_sym = orig_scope->resolve_symbol(sym_name);
        if (!orig_sym || !is_a<ASR::Variable_t>(*orig_sym)) continue;
        ASR::Variable_t *orig_var =
            down_cast<ASR::Variable_t>(orig_sym);
        if (!orig_var->m_type_declaration) continue;
        ASR::symbol_t *struct_sym =
            ASRUtils::symbol_get_past_external(
                orig_var->m_type_declaration);
        if (!is_a<ASR::Struct_t>(*struct_sym)) continue;
        ASR::Struct_t *st = down_cast<ASR::Struct_t>(struct_sym);
        ASR::ttype_t *int_type_sz = ASRUtils::TYPE(
            ASR::make_Integer_t(al, loc, 4));
        for (auto &mem_entry :
                ASRUtils::collect_allocatable_array_members(st)) {
            const std::string &mem_name = mem_entry.first;
            ASR::Variable_t *mv = mem_entry.second;
            ASR::symbol_t *mem_sym = (ASR::symbol_t*)mv;
            std::string size_name = GpuNames::member_size(
                sym_name, mem_name);
            ASR::symbol_t *size_sym = gpu_new_variable(al, loc,
                kernel_scope, size_name, ASRUtils::duplicate_type(al,
                    int_type_sz), ASR::intentType::InOut);
            kernel_args.push_back(al,
                ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                    size_sym)));
            // Host-side: size(struct%member) (total size)
            // Look up the member symbol in the original struct's
            // scope for the ExternalSymbol reference used in the
            // program scope (needed for StructInstanceMember).
            ASR::symbol_t *orig_mem_ref = nullptr;
            for (auto &scope_item :
                    orig_scope->get_scope()) {
                if (!is_a<ASR::ExternalSymbol_t>(
                        *scope_item.second)) continue;
                ASR::ExternalSymbol_t *es =
                    down_cast<ASR::ExternalSymbol_t>(
                        scope_item.second);
                ASR::symbol_t *resolved =
                    ASRUtils::symbol_get_past_external(
                        es->m_external);
                if (resolved == mem_sym) {
                    orig_mem_ref = scope_item.second;
                    break;
                }
            }
            if (!orig_mem_ref) orig_mem_ref = mem_sym;
            ASR::expr_t *host_member = ASRUtils::EXPR(
                ASR::make_StructInstanceMember_t(al, loc,
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                        orig_sym)),
                    orig_mem_ref, mv->m_type, nullptr));
            ASR::expr_t *host_size = ASRUtils::EXPR(
                ASR::make_ArraySize_t(al, loc,
                    host_member, nullptr, int_type_sz,
                    nullptr));
            ASR::call_arg_t carg;
            carg.loc = loc;
            carg.m_value = host_size;
            call_args.push_back(al, carg);

            // For a component of rank > 1 the total size is not the
            // extent of any single dimension, so also pass each
            // per-dimension extent; size(struct%member, dim) in the
            // kernel reads these.
            ASR::ttype_t *mem_inner =
                ASRUtils::type_get_past_allocatable(mv->m_type);
            size_t rank = ASR::down_cast<ASR::Array_t>(
                mem_inner)->n_dims;
            if (rank <= 1) continue;
            for (size_t d = 0; d < rank; d++) {
                std::string dim_size_name = size_name + "_dim"
                    + std::to_string(d + 1);
                ASR::symbol_t *dim_size_sym = gpu_new_variable(al, loc,
                    kernel_scope, dim_size_name,
                    ASRUtils::duplicate_type(al, int_type_sz),
                    ASR::intentType::InOut);
                kernel_args.push_back(al,
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                        dim_size_sym)));
                ASR::expr_t *dim_expr = ASRUtils::EXPR(
                    ASR::make_IntegerConstant_t(al, loc,
                        (int64_t)(d + 1), int_type_sz,
                        ASR::integerbozType::Decimal));
                ASR::expr_t *dim_member = ASRUtils::EXPR(
                    ASR::make_StructInstanceMember_t(al, loc,
                        ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                            orig_sym)),
                        orig_mem_ref, mv->m_type, nullptr));
                ASR::call_arg_t dim_carg;
                dim_carg.loc = loc;
                dim_carg.m_value = ASRUtils::EXPR(
                    ASR::make_ArraySize_t(al, loc, dim_member,
                        dim_expr, int_type_sz, nullptr));
                call_args.push_back(al, dim_carg);
            }
        }
    }

    // Add allocatable-member data kernel parameters for struct-typed
    // kernel parameters that were NOT fully decomposed. These provide
    // the actual array data as separate device buffers so that Metal
    // inline functions can index into allocatable members.
    // Skip array-of-structs variables — StructInstanceMember requires
    // a scalar struct base, not an array of structs.
    for (auto &[sym_name, sym_info] : involved_syms) {
        ASR::ttype_t *type = sym_info.first;
        ASR::ttype_t *inner_t = ASRUtils::type_get_past_allocatable(type);
        if (ASRUtils::is_array(inner_t)) continue;
        if (!ASR::is_a<ASR::StructType_t>(
                *ASRUtils::extract_type(inner_t)))
            continue;
        ASR::symbol_t *orig_sym = orig_scope->resolve_symbol(sym_name);
        if (!orig_sym || !is_a<ASR::Variable_t>(*orig_sym)) continue;
        ASR::Variable_t *orig_var =
            down_cast<ASR::Variable_t>(orig_sym);
        if (!orig_var->m_type_declaration) continue;
        ASR::symbol_t *struct_sym =
            ASRUtils::symbol_get_past_external(
                orig_var->m_type_declaration);
        if (!is_a<ASR::Struct_t>(*struct_sym)) continue;
        ASR::Struct_t *st = down_cast<ASR::Struct_t>(struct_sym);
        for (auto &mem_entry :
                ASRUtils::collect_allocatable_array_members(st)) {
            const std::string &mem_name = mem_entry.first;
            ASR::Variable_t *mv = mem_entry.second;
            ASR::symbol_t *mem_sym = (ASR::symbol_t*)mv;
            ASR::ttype_t *mem_inner =
                ASRUtils::type_get_past_allocatable(mv->m_type);
            std::string data_name = GpuNames::member_data(
                sym_name, mem_name);
            ASR::ttype_t *data_type =
                ASRUtils::duplicate_type(al, mem_inner);
            ASR::symbol_t *data_type_decl = nullptr;
            if (ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::extract_type(data_type)) &&
                    mv->m_type_declaration) {
                ASR::symbol_t *inner_struct_sym =
                    ASRUtils::symbol_get_past_external(
                        mv->m_type_declaration);
                if (is_a<ASR::Struct_t>(*inner_struct_sym)) {
                    data_type_decl = import_struct_def(
                        down_cast<ASR::Struct_t>(inner_struct_sym),
                        orig_scope, kernel_scope, loc);
                }
            }
            SetChar deps_vec;
            deps_vec.reserve(al, 1);
            ASRUtils::collect_variable_dependencies(
                al, deps_vec, data_type, nullptr, nullptr,
                data_name);
            ASR::symbol_t *data_sym =
                ASR::down_cast<ASR::symbol_t>(
                    ASRUtils::make_Variable_t_util(al, loc,
                        kernel_scope, s2c(al, data_name),
                        deps_vec.p, deps_vec.size(),
                        ASR::intentType::InOut, nullptr,
                        nullptr,
                        ASR::storage_typeType::Default,
                        data_type,
                        data_type_decl, ASR::abiType::Source,
                        ASR::accessType::Public,
                        ASR::presenceType::Required, false));
            kernel_scope->add_symbol(data_name, data_sym);
            kernel_args.push_back(al,
                ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                    data_sym)));
            ASR::symbol_t *orig_mem_ref = nullptr;
            for (auto &scope_item :
                    orig_scope->get_scope()) {
                if (!is_a<ASR::ExternalSymbol_t>(
                        *scope_item.second)) continue;
                ASR::ExternalSymbol_t *es =
                    down_cast<ASR::ExternalSymbol_t>(
                        scope_item.second);
                ASR::symbol_t *resolved =
                    ASRUtils::symbol_get_past_external(
                        es->m_external);
                if (resolved == mem_sym) {
                    orig_mem_ref = scope_item.second;
                    break;
                }
            }
            if (!orig_mem_ref) orig_mem_ref = mem_sym;
            ASR::call_arg_t carg;
            carg.loc = loc;
            carg.m_value = ASRUtils::EXPR(
                ASR::make_StructInstanceMember_t(al, loc,
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                        orig_sym)),
                    orig_mem_ref, mv->m_type, nullptr));
            call_args.push_back(al, carg);
        }
    }

    // Create loop variables in kernel scope (local, not parameters)
    for (size_t d = 0; d < n_dims; d++) {
        ASR::Var_t *lv = down_cast<ASR::Var_t>(work.head(d).m_v);
        ASR::ttype_t *loop_var_type = ASRUtils::symbol_type(lv->m_v);
        std::string lvn = loop_var_names[d];
        gpu_new_variable(al, loc, kernel_scope, lvn,
            ASRUtils::duplicate_type(al, loop_var_type));
    }

    // Create local scalar temporaries in kernel scope
    for (auto &name : local_scalar_names) {
        auto it_orig = orig_scope->resolve_symbol(name);
        if (!it_orig) continue;
        ASR::ttype_t *type = ASRUtils::symbol_type(it_orig);
        ASR::symbol_t *type_decl = import_struct_type(it_orig,
            orig_scope, kernel_scope, loc);
        gpu_new_variable(al, loc, kernel_scope, name,
            ASRUtils::duplicate_type(al, type),
            ASR::intentType::Local, type_decl);
    }

    // Import functions/subroutines called in the loop body
    // into the kernel scope so FunctionCall/SubroutineCall nodes
    // can reference them after symbol remapping.
    // Collect transitively: if f() calls g(), both must be imported.
    {
        GpuFunctionCollector func_collector;
        for (size_t i = 0; i < work.n_body; i++) {
            func_collector.visit_stmt(*work.body[i]);
        }
        {
            bool added = true;
            while (added) {
                added = false;
                GpuFunctionCollector transitive_collector;
                for (auto &[fn_name, fn_sym] : func_collector.functions) {
                    ASR::symbol_t *fn_resolved =
                        ASRUtils::symbol_get_past_external(fn_sym);
                    ASR::Function_t *fn = nullptr;
                    if (ASR::is_a<ASR::Function_t>(*fn_resolved)) {
                        fn = ASR::down_cast<ASR::Function_t>(fn_resolved);
                    } else if (ASR::is_a<ASR::StructMethodDeclaration_t>(
                            *fn_resolved)) {
                        ASR::StructMethodDeclaration_t *smd =
                            ASR::down_cast<ASR::StructMethodDeclaration_t>(
                                fn_resolved);
                        ASR::symbol_t *proc =
                            ASRUtils::symbol_get_past_external(smd->m_proc);
                        if (ASR::is_a<ASR::Function_t>(*proc)) {
                            fn = ASR::down_cast<ASR::Function_t>(proc);
                        }
                    }
                    if (fn) {
                        ASR::Function_t *fn_impl =
                            resolve_function_implementation(fn);
                        for (size_t i = 0; i < fn_impl->n_body; i++) {
                            transitive_collector.visit_stmt(
                                *fn_impl->m_body[i]);
                        }
                    }
                }
                for (auto &[name, sym] : transitive_collector.functions) {
                    if (func_collector.functions.find(name) ==
                            func_collector.functions.end()) {
                        func_collector.functions[name] = sym;
                        added = true;
                    }
                }
            }
        }
        ASRUtils::SymbolDuplicator sym_dup(al);
        for (auto &[func_name, func_sym] : func_collector.functions) {
            ASR::symbol_t *resolved =
                ASRUtils::symbol_get_past_external(func_sym);
            if (kernel_scope->get_symbol(func_name)) {
                // ExternalSymbol already created (e.g., by
                // import_struct_def). Still need to import the
                // function body for StructMethodDeclaration calls
                // so the Metal backend can generate shader code.
            } else if (ASR::is_a<ASR::ExternalSymbol_t>(*func_sym) &&
                       ASR::is_a<ASR::Function_t>(*resolved)) {
                // The function is accessed via use-association
                // (ExternalSymbol). Duplicate the underlying function
                // body into the kernel scope so its types reference
                // the kernel's struct copies (not the module's).
                ASR::Function_t *resolved_func =
                    ASR::down_cast<ASR::Function_t>(resolved);
                ASR::FunctionType_t *resolved_ftype =
                    ASR::down_cast<ASR::FunctionType_t>(
                        resolved_func->m_function_signature);
                if (resolved_ftype->m_deftype ==
                        ASR::deftypeType::Interface) {
                    // Submodule interface: find the Implementation
                    // in a submodule already in the TU, or load it
                    // from disk (needed for --separate-compilation).
                    std::string pname =
                        ASRUtils::symbol_name(resolved);
                    bool found = false;
                    for (auto &tu_item :
                            tu.m_symtab->get_scope()) {
                        if (!ASR::is_a<ASR::Module_t>(
                                *tu_item.second)) continue;
                        ASR::Module_t *mod =
                            ASR::down_cast<ASR::Module_t>(
                                tu_item.second);
                        ASR::symbol_t *impl_sym =
                            mod->m_symtab->get_symbol(pname);
                        if (!impl_sym ||
                            !ASR::is_a<ASR::Function_t>(
                                *impl_sym)) continue;
                        ASR::Function_t *impl_func =
                            ASR::down_cast<ASR::Function_t>(
                                impl_sym);
                        ASR::FunctionType_t *impl_ft =
                            ASR::down_cast<ASR::FunctionType_t>(
                                impl_func
                                    ->m_function_signature);
                        if (impl_ft->m_deftype !=
                                ASR::deftypeType::Implementation)
                            continue;
                        resolved = impl_sym;
                        found = true;
                        break;
                    }
                    if (!found) {
                        // Load submodule from smod file on disk.
                        SymbolTable *parent_st =
                            ASRUtils::symbol_parent_symtab(
                                resolved);
                        if (parent_st->asr_owner &&
                                parent_st->asr_owner->type ==
                                    ASR::asrType::symbol &&
                                ASR::is_a<ASR::Module_t>(
                                    *ASR::down_cast<ASR::symbol_t>(
                                        parent_st->asr_owner))) {
                            std::string parent_mod =
                                ASR::down_cast<ASR::Module_t>(
                                    ASR::down_cast<ASR::symbol_t>(
                                        parent_st->asr_owner))
                                    ->m_name;
                            std::string smod_prefix =
                                parent_mod + "@";
                            std::vector<std::filesystem::path>
                                mod_dirs;
                            mod_dirs.push_back(
                                pass_options.runtime_library_dir);
                            mod_dirs.push_back(
                                pass_options.mod_files_dir);
                            mod_dirs.insert(mod_dirs.end(),
                                pass_options.include_dirs.begin(),
                                pass_options.include_dirs.end());
                            for (auto &dir : mod_dirs) {
                                if (dir.empty())
                                    dir = ".";
                                if (!std::filesystem::is_directory(
                                        dir)) continue;
                                for (auto &file :
                                        std::filesystem::
                                            directory_iterator(
                                                dir)) {
                                    std::string fname =
                                        file.path().filename()
                                            .string();
                                    if (!startswith(fname,
                                            smod_prefix) ||
                                        !endswith(fname, ".smod"))
                                        continue;
                                    std::string content;
                                    if (!read_file(
                                            file.path().string(),
                                            content) ||
                                        content.empty())
                                        continue;
                                    LocationManager lm_tmp;
                                    auto res = load_modfile(
                                        al, content, false,
                                        *tu.m_symtab, lm_tmp);
                                    if (!res.ok) continue;
                                    load_submodule_deps(
                                        *res.result);
                                    fix_external_symbols(
                                        *res.result,
                                        *tu.m_symtab);
                                    lower_loaded_implied_do_loops(
                                        *res.result);
                                    ASR::Module_t *submod =
                                        ASRUtils::extract_module(
                                            *res.result);
                                    ASR::symbol_t *impl_sym =
                                        submod->m_symtab
                                            ->get_symbol(pname);
                                    if (!impl_sym ||
                                        !ASR::is_a<ASR::Function_t>(
                                            *impl_sym)) continue;
                                    ASR::Function_t *impl_func =
                                        ASR::down_cast<
                                            ASR::Function_t>(
                                                impl_sym);
                                    ASR::FunctionType_t *impl_ft =
                                        ASR::down_cast<
                                            ASR::FunctionType_t>(
                                                impl_func
                                                ->m_function_signature);
                                    if (impl_ft->m_deftype !=
                                            ASR::deftypeType::
                                                Implementation)
                                        continue;
                                    resolved = impl_sym;
                                    found = true;
                                    break;
                                }
                                if (found) break;
                            }
                        }
                    }
                }
                if (ASR::is_a<ASR::Function_t>(*resolved)) {
                    ASR::Function_t *rf =
                        ASR::down_cast<ASR::Function_t>(resolved);
                    ASR::FunctionType_t *rft =
                        ASR::down_cast<ASR::FunctionType_t>(
                            rf->m_function_signature);
                    if (rft->m_deftype ==
                            ASR::deftypeType::Interface) {
                        // Still an interface after searching TU and
                        // .smod files — the submodule body is
                        // unavailable (e.g., parallel build race).
                        // Error out instead of generating an empty
                        // function in the Metal shader.
                        throw LCompilersException(
                            "GPU Metal offload: cannot find "
                            "submodule implementation for '" +
                            std::string(rf->m_name) + "'; "
                            "ensure the submodule is compiled "
                            "before the file that uses it");
                    }
                }
                std::string real_name =
                    ASRUtils::symbol_name(resolved);
                // When two modules define functions with the same
                // name (e.g., both have "my_construct"), the first
                // gets added under real_name. For subsequent
                // collisions, sanitize the ExternalSymbol name to
                // a valid C identifier to disambiguate.
                std::string dup_name = real_name;
                if (kernel_scope->get_symbol(real_name)) {
                    dup_name = func_name;
                    for (char &c : dup_name) {
                        if (c == '~' || c == '@') c = '_';
                    }
                }
                if (!kernel_scope->get_symbol(dup_name)) {
                    ASR::symbol_t *dup =
                        sym_dup.duplicate_Function(
                            ASR::down_cast<ASR::Function_t>(
                                resolved),
                            kernel_scope);
                    if (dup) {
                        ASR::down_cast<ASR::Function_t>(dup)
                            ->m_name = s2c(al, dup_name);
                        kernel_scope->add_symbol(dup_name, dup);
                        // The duplicated function still references
                        // the module's struct definitions. Remap
                        // ExternalSymbol targets and Variable
                        // m_type_declarations to point to the
                        // kernel's struct copies instead.
                        fixup_struct_refs_in_scope(
                            ASR::down_cast<ASR::Function_t>(dup)
                                ->m_symtab,
                            kernel_scope,
                            s2c(al, kernel_name));
                    }
                }
            } else if (ASR::is_a<ASR::ExternalSymbol_t>(*func_sym) &&
                       !ASR::is_a<ASR::StructMethodDeclaration_t>(
                           *resolved)) {
                // Non-function, non-method ExternalSymbol (e.g.,
                // GenericProcedure from m_original_name). Create a
                // matching ExternalSymbol in the kernel scope.
                ASR::ExternalSymbol_t *es =
                    ASR::down_cast<ASR::ExternalSymbol_t>(func_sym);
                ASR::asr_t *new_es = ASR::make_ExternalSymbol_t(
                    al, loc, kernel_scope, s2c(al, func_name),
                    es->m_external, es->m_module_name,
                    nullptr, 0, es->m_original_name,
                    es->m_access);
                kernel_scope->add_symbol(func_name,
                    ASR::down_cast<ASR::symbol_t>(new_es));
            } else if (ASR::is_a<ASR::Function_t>(*resolved)) {
                // Skip functions that are already accessible through
                // the kernel scope's parent chain (e.g., TU-scope
                // generated helpers from the
                // function_call_in_declaration pass).
                if (kernel_scope->parent &&
                        kernel_scope->parent->resolve_symbol(
                            ASRUtils::symbol_name(resolved))) {
                    if (!ASR::is_a<ASR::StructMethodDeclaration_t>(
                            *resolved)) {
                        continue;
                    }
                }
                ASR::symbol_t *dup = sym_dup.duplicate_Function(
                    ASR::down_cast<ASR::Function_t>(resolved),
                    kernel_scope);
                if (dup) {
                    kernel_scope->add_symbol(func_name, dup);
                }
            } else if (ASR::is_a<ASR::StructMethodDeclaration_t>(
                           *resolved)) {
                // Type-bound procedure call: the resolved symbol is
                // a StructMethodDeclaration inside a Struct's symtab.
                // Create an ExternalSymbol in the kernel scope that
                // points to the corresponding method declaration in
                // the kernel's copy of the struct (imported earlier
                // by import_struct_def for the struct-typed variable).
                SymbolTable *method_st =
                    ASRUtils::symbol_parent_symtab(resolved);
                if (method_st->asr_owner &&
                        method_st->asr_owner->type ==
                            ASR::asrType::symbol) {
                    ASR::symbol_t *struct_owner =
                        down_cast<ASR::symbol_t>(method_st->asr_owner);
                    if (is_a<ASR::Struct_t>(*struct_owner)) {
                        std::string struct_name =
                            down_cast<ASR::Struct_t>(struct_owner)
                                ->m_name;
                        std::string orig_name =
                            ASRUtils::symbol_name(resolved);
                        ASR::symbol_t *kernel_struct =
                            find_kernel_struct(kernel_scope,
                                struct_name, orig_name);
                        if (kernel_struct &&
                                is_a<ASR::Struct_t>(*kernel_struct)) {
                            struct_name = down_cast<ASR::Struct_t>(
                                kernel_struct)->m_name;
                            ASR::symbol_t *kernel_method =
                                gpu_struct_lookup_member(kernel_struct,
                                    orig_name);
                            if (kernel_method) {
                                struct_name = struct_member_owner_name(
                                    kernel_method, struct_name);
                                ASR::asr_t *new_es =
                                    ASR::make_ExternalSymbol_t(al, loc,
                                        kernel_scope,
                                        s2c(al, func_name),
                                        kernel_method,
                                        s2c(al, struct_name),
                                        nullptr, 0,
                                        s2c(al, orig_name),
                                        ASR::accessType::Public);
                                kernel_scope->add_symbol(func_name,
                                    down_cast<ASR::symbol_t>(new_es));
                            }
                        }
                    }
                }
            }
            // For type-bound procedure calls, also import the
            // underlying Function body into the kernel scope so
            // the Metal backend can generate shader code.
            // For submodule procedures, the module-scope Function
            // is just an interface (no body); find and import the
            // submodule implementation instead.
            if (ASR::is_a<ASR::StructMethodDeclaration_t>(
                    *resolved)) {
                ASR::StructMethodDeclaration_t *smd =
                    ASR::down_cast<ASR::StructMethodDeclaration_t>(
                        resolved);
                ASR::symbol_t *proc_sym =
                    ASRUtils::symbol_get_past_external(smd->m_proc);
                if (ASR::is_a<ASR::Function_t>(*proc_sym)) {
                    ASR::Function_t *proc_func =
                        ASR::down_cast<ASR::Function_t>(proc_sym);
                    std::string pname =
                        ASRUtils::symbol_name(proc_sym);
                    ASR::symbol_t *existing =
                        kernel_scope->get_symbol(pname);
                    bool already_has_body = false;
                    if (existing &&
                            ASR::is_a<ASR::Function_t>(*existing)) {
                        ASR::FunctionType_t *eft =
                            ASR::down_cast<ASR::FunctionType_t>(
                                ASR::down_cast<ASR::Function_t>(
                                    existing)
                                    ->m_function_signature);
                        if (eft->m_deftype ==
                                ASR::deftypeType::Implementation) {
                            already_has_body = true;
                        }
                    }
                    if (!already_has_body) {
                        if (existing) {
                            kernel_scope->erase_symbol(pname);
                        }
                        ASR::FunctionType_t *ftype =
                            ASR::down_cast<ASR::FunctionType_t>(
                                proc_func->m_function_signature);
                        if (ftype->m_deftype ==
                                ASR::deftypeType::Interface) {
                            // Submodule interface: find the
                            // Implementation in a submodule
                            // already in the TU, or load it from
                            // disk (--separate-compilation).
                            bool found = false;
                            for (auto &tu_item :
                                    tu.m_symtab->get_scope()) {
                                if (!ASR::is_a<ASR::Module_t>(
                                        *tu_item.second)) continue;
                                ASR::Module_t *mod =
                                    ASR::down_cast<ASR::Module_t>(
                                        tu_item.second);
                                ASR::symbol_t *impl_sym =
                                    mod->m_symtab->get_symbol(pname);
                                if (!impl_sym ||
                                    !ASR::is_a<ASR::Function_t>(
                                        *impl_sym)) continue;
                                ASR::Function_t *impl_func =
                                    ASR::down_cast<ASR::Function_t>(
                                        impl_sym);
                                ASR::FunctionType_t *impl_ft =
                                    ASR::down_cast<ASR::FunctionType_t>(
                                        impl_func
                                            ->m_function_signature);
                                if (impl_ft->m_deftype !=
                                        ASR::deftypeType::Implementation)
                                    continue;
                                ASR::symbol_t *dup =
                                    sym_dup.duplicate_Function(
                                        impl_func, kernel_scope);
                                if (dup) {
                                    kernel_scope->add_symbol(
                                        pname, dup);
                                }
                                found = true;
                                break;
                            }
                            if (!found) {
                                // Load submodule from smod file.
                                SymbolTable *parent_st =
                                    ASRUtils::symbol_parent_symtab(
                                        proc_sym);
                                if (parent_st->asr_owner &&
                                        parent_st->asr_owner->type ==
                                            ASR::asrType::symbol &&
                                        ASR::is_a<ASR::Module_t>(
                                            *ASR::down_cast<
                                                ASR::symbol_t>(
                                                parent_st
                                                    ->asr_owner))) {
                                    std::string parent_mod =
                                        ASR::down_cast<ASR::Module_t>(
                                            ASR::down_cast<
                                                ASR::symbol_t>(
                                                parent_st
                                                    ->asr_owner))
                                            ->m_name;
                                    std::string smod_prefix =
                                        parent_mod + "@";
                                    std::vector<
                                        std::filesystem::path>
                                            mod_dirs;
                                    mod_dirs.push_back(
                                        pass_options
                                            .runtime_library_dir);
                                    mod_dirs.push_back(
                                        pass_options.mod_files_dir);
                                    mod_dirs.insert(mod_dirs.end(),
                                        pass_options.include_dirs
                                            .begin(),
                                        pass_options.include_dirs
                                            .end());
                                    for (auto &dir : mod_dirs) {
                                        if (dir.empty())
                                            dir = ".";
                                        if (!std::filesystem::
                                                is_directory(dir))
                                            continue;
                                        for (auto &file :
                                                std::filesystem::
                                                    directory_iterator(
                                                        dir)) {
                                            std::string fname =
                                                file.path()
                                                    .filename()
                                                    .string();
                                            if (!startswith(fname,
                                                    smod_prefix) ||
                                                !endswith(fname,
                                                    ".smod"))
                                                continue;
                                            std::string content;
                                            if (!read_file(
                                                    file.path()
                                                        .string(),
                                                    content) ||
                                                content.empty())
                                                continue;
                                            LocationManager
                                                lm_tmp;
                                            auto res =
                                                load_modfile(
                                                    al, content,
                                                    false,
                                                    *tu.m_symtab,
                                                    lm_tmp);
                                            if (!res.ok) continue;
                                            load_submodule_deps(
                                                *res.result);
                                            fix_external_symbols(
                                                *res.result,
                                                *tu.m_symtab);
                                            lower_loaded_implied_do_loops(
                                                *res.result);
                                            ASR::Module_t *submod =
                                                ASRUtils::
                                                    extract_module(
                                                        *res.result);
                                            ASR::symbol_t
                                                *impl_sym =
                                                submod->m_symtab
                                                    ->get_symbol(
                                                        pname);
                                            if (!impl_sym ||
                                                !ASR::is_a<
                                                    ASR::Function_t
                                                        >(*impl_sym))
                                                continue;
                                            ASR::Function_t
                                                *impl_func =
                                                ASR::down_cast<
                                                    ASR::Function_t>(
                                                        impl_sym);
                                            ASR::FunctionType_t
                                                *impl_ft =
                                                ASR::down_cast<
                                                    ASR::FunctionType_t>(
                                                    impl_func
                                                    ->m_function_signature);
                                            if (impl_ft->m_deftype
                                                    != ASR::
                                                    deftypeType::
                                                    Implementation)
                                                continue;
                                            ASR::symbol_t *dup =
                                                sym_dup
                                                    .duplicate_Function(
                                                    impl_func,
                                                    kernel_scope);
                                            if (dup) {
                                                kernel_scope
                                                    ->add_symbol(
                                                        pname, dup);
                                            }
                                            found = true;
                                            break;
                                        }
                                        if (found) break;
                                    }
                                }
                            }
                            if (!found) {
                                throw LCompilersException(
                                    "GPU Metal offload: cannot find "
                                    "submodule implementation for '"
                                    + pname + "'; ensure the "
                                    "submodule is compiled before "
                                    "the file that uses it");
                            }
                        } else {
                            // Non-submodule: function has a body.
                            ASR::symbol_t *dup =
                                sym_dup.duplicate_Function(
                                    proc_func, kernel_scope);
                            if (dup) {
                                kernel_scope->add_symbol(pname, dup);
                            }
                        }
                    }
                    // Update the StructMethodDeclaration in the
                    // kernel's struct to point to the kernel-scope
                    // function copy instead of the original module
                    // interface (which may have no body).
                    ASR::symbol_t *kernel_func =
                        kernel_scope->get_symbol(pname);
                    if (kernel_func) {
                        SymbolTable *method_st =
                            ASRUtils::symbol_parent_symtab(resolved);
                        if (method_st->asr_owner &&
                                method_st->asr_owner->type ==
                                    ASR::asrType::symbol) {
                            ASR::symbol_t *struct_owner =
                                down_cast<ASR::symbol_t>(
                                    method_st->asr_owner);
                            if (is_a<ASR::Struct_t>(*struct_owner)) {
                                std::string sname =
                                    down_cast<ASR::Struct_t>(
                                        struct_owner)->m_name;
                                std::string mname =
                                    ASRUtils::symbol_name(
                                        resolved);
                                ASR::symbol_t *ks =
                                    find_kernel_struct(kernel_scope,
                                        sname, mname);
                                if (ks &&
                                        is_a<ASR::Struct_t>(*ks)) {
                                    ASR::symbol_t *km =
                                        down_cast<ASR::Struct_t>(ks)
                                            ->m_symtab
                                            ->get_symbol(mname);
                                    if (km && is_a<
                                        ASR::StructMethodDeclaration_t
                                            >(*km)) {
                                        down_cast<ASR::
                                            StructMethodDeclaration_t
                                                >(km)->m_proc =
                                                    kernel_func;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Fix up struct references in ALL duplicated kernel functions.
    // After duplication, ExternalSymbol targets and Variable
    // m_type_declarations may still reference the original module's
    // struct definitions. Remap them to the kernel's copies.
    // This recurses into nested scopes (Block, AssociateBlock, etc.).
    {
        for (auto &item : kernel_scope->get_scope()) {
            if (!ASR::is_a<ASR::Function_t>(*item.second)) continue;
            ASR::Function_t *dfunc = ASR::down_cast<ASR::Function_t>(
                item.second);
            fixup_struct_refs_in_scope(dfunc->m_symtab,
                kernel_scope, s2c(al, kernel_name));
        }
    }

    // Fix dangling variable references in duplicated kernel functions.
    // When a contained function references variables from the original
    // enclosing scope (e.g., a program-scope Parameter used by a
    // contained function), the duplicated function body retains the
    // original Var references which are unreachable from the kernel.
    // For Parameter variables, clone them into the function's scope.
    // For other variables, add them as extra kernel parameters.
    {
        for (auto &item : kernel_scope->get_scope()) {
            if (!ASR::is_a<ASR::Function_t>(*item.second)) continue;
            ASR::Function_t *func = ASR::down_cast<ASR::Function_t>(
                item.second);
            DanglingVarCollector dvc(func->m_symtab);
            for (size_t bi = 0; bi < func->n_body; bi++) {
                dvc.visit_stmt(*func->m_body[bi]);
            }
            if (dvc.dangling.empty()) continue;

            std::set<std::string> fixed_names;
            for (auto &[name, orig_sym] : dvc.dangling) {
                ASR::symbol_t *resolved_sym =
                    ASRUtils::symbol_get_past_external(orig_sym);
                if (!ASR::is_a<ASR::Variable_t>(*resolved_sym)) continue;
                ASR::Variable_t *orig_var =
                    ASR::down_cast<ASR::Variable_t>(resolved_sym);
                if (orig_var->m_storage ==
                        ASR::storage_typeType::Parameter) {
                    ASR::symbol_t *new_var =
                        ASR::down_cast<ASR::symbol_t>(
                            ASRUtils::make_Variable_t_util(al, loc,
                                func->m_symtab, s2c(al, name),
                                nullptr, 0,
                                ASR::intentType::Local,
                                orig_var->m_symbolic_value,
                                orig_var->m_value,
                                ASR::storage_typeType::Parameter,
                                ASRUtils::duplicate_type(al,
                                    orig_var->m_type),
                                nullptr, orig_var->m_abi,
                                orig_var->m_access,
                                ASR::presenceType::Required, false));
                    func->m_symtab->add_symbol(name, new_var);
                    fixed_names.insert(name);
                } else {
                    if (!kernel_scope->get_symbol(name)) {
                        ASR::ttype_t *dup_type =
                            ASRUtils::duplicate_type(al,
                                ASRUtils::type_get_past_allocatable(
                                    orig_var->m_type));
                        SetChar deps_vec;
                        deps_vec.reserve(al, 1);
                        ASRUtils::collect_variable_dependencies(
                            al, deps_vec, dup_type, nullptr,
                            nullptr, name);
                        ASR::symbol_t *param =
                            ASR::down_cast<ASR::symbol_t>(
                                ASRUtils::make_Variable_t_util(al,
                                    loc, kernel_scope,
                                    s2c(al, name),
                                    deps_vec.p, deps_vec.size(),
                                    ASR::intentType::InOut,
                                    nullptr, nullptr,
                                    ASR::storage_typeType::Default,
                                    dup_type, nullptr,
                                    ASR::abiType::Source,
                                    ASR::accessType::Public,
                                    ASR::presenceType::Required,
                                    false));
                        kernel_scope->add_symbol(name, param);
                        kernel_args.push_back(al,
                            ASRUtils::EXPR(ASR::make_Var_t(
                                al, loc, param)));
                        ASR::symbol_t *host_sym =
                            orig_scope->resolve_symbol(name);
                        ASR::call_arg_t carg;
                        carg.loc = loc;
                        carg.m_value = ASRUtils::EXPR(
                            ASR::make_Var_t(al, loc,
                                host_sym ? host_sym : orig_sym));
                        call_args.push_back(al, carg);
                    }
                    fixed_names.insert(name);
                }
            }
            if (!fixed_names.empty()) {
                DanglingVarFixer fixer(func->m_symtab, fixed_names);
                for (size_t bi = 0; bi < func->n_body; bi++) {
                    fixer.visit_stmt(*func->m_body[bi]);
                }
            }
        }
    }

    // Remap FunctionCall/SubroutineCall references inside duplicated
    // kernel functions. When function f() calls g() and both are
    // duplicated into the kernel scope, f's body still references the
    // original g from the program scope. Fix those up.
    // Also descend into AssociateBlock and Block bodies within
    // duplicated functions — the statement visitor does not enter
    // these sub-scopes, so FunctionCall m_name references inside
    // them (e.g., type-bound procedure calls in associate blocks)
    // still point to the original scope after duplication.
    {
        for (auto &item : kernel_scope->get_scope()) {
            if (!ASR::is_a<ASR::Function_t>(*item.second)) continue;
            ASR::Function_t *func = ASR::down_cast<ASR::Function_t>(
                item.second);
            // A local whose extent is written in terms of another
            // symbol of the same function -- `real :: r(self%m_ + 1)`
            // -- is copied before that symbol is, and the copy is then
            // left naming the original. Point every such extent at the
            // copy's own symbols.
            retarget_local_extents(func->m_symtab);
            GpuReplaceSymbolsVisitor fn_replacer(*kernel_scope);
            fn_replacer.replacer.skip_scopes.insert(func->m_symtab);
            for (size_t bi = 0; bi < func->n_body; bi++) {
                fn_replacer.visit_stmt(*func->m_body[bi]);
            }
            for (auto &fn_item : func->m_symtab->get_scope()) {
                if (ASR::is_a<ASR::AssociateBlock_t>(
                        *fn_item.second)) {
                    ASR::AssociateBlock_t *ab =
                        ASR::down_cast<ASR::AssociateBlock_t>(
                            fn_item.second);
                    for (size_t bi = 0; bi < ab->n_body; bi++) {
                        fn_replacer.visit_stmt(*ab->m_body[bi]);
                    }
                } else if (ASR::is_a<ASR::Block_t>(
                               *fn_item.second)) {
                    ASR::Block_t *block =
                        ASR::down_cast<ASR::Block_t>(fn_item.second);
                    for (size_t bi = 0; bi < block->n_body; bi++) {
                        fn_replacer.visit_stmt(*block->m_body[bi]);
                    }
                }
            }
        }
    }

    // Decompose StructInstanceMember references in kernel variable
    // type expressions (e.g., ArraySize(StructInstanceMember(Var(x),
    // nodes)) in VLA dimensions). When a struct variable is fully
    // decomposed into flat-array parameters, it is removed from the
    // kernel scope, but other variables' VLA dimensions may still
    // reference it through StructInstanceMember. Replace those with
    // the decomposed flat-array parameter Var before general symbol
    // remapping.
    if (!decomp_map.empty()) {
        GpuDecomposeStructReplacer type_decomp(al, kernel_scope,
            decomp_map);
        for (auto &item : kernel_scope->get_scope()) {
            if (!is_a<ASR::Variable_t>(*item.second)) continue;
            ASR::Variable_t *var = down_cast<ASR::Variable_t>(
                item.second);
            if (!ASR::is_a<ASR::Array_t>(*var->m_type)) continue;
            ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(
                var->m_type);
            for (size_t d = 0; d < arr->n_dims; d++) {
                if (arr->m_dims[d].m_start) {
                    type_decomp.current_expr =
                        &(arr->m_dims[d].m_start);
                    type_decomp.replace_expr(
                        arr->m_dims[d].m_start);
                }
                if (arr->m_dims[d].m_length) {
                    type_decomp.current_expr =
                        &(arr->m_dims[d].m_length);
                    type_decomp.replace_expr(
                        arr->m_dims[d].m_length);
                }
            }
        }
    }

    // Remap symbol references in kernel parameter types (e.g., array
    // dimension expressions like s(x%n) that still point to the
    // original scope after duplicate_type).
    {
        GpuReplaceSymbols type_replacer(*kernel_scope);
        for (auto &item : kernel_scope->get_scope()) {
            if (!is_a<ASR::Variable_t>(*item.second)) continue;
            ASR::Variable_t *var = down_cast<ASR::Variable_t>(item.second);
            ASR::ttype_t *type = var->m_type;
            if (ASR::is_a<ASR::Array_t>(*type)) {
                ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
                for (size_t i = 0; i < arr->n_dims; i++) {
                    if (arr->m_dims[i].m_start) {
                        type_replacer.current_expr = &(arr->m_dims[i].m_start);
                        type_replacer.replace_expr(arr->m_dims[i].m_start);
                    }
                    if (arr->m_dims[i].m_length) {
                        type_replacer.current_expr = &(arr->m_dims[i].m_length);
                        type_replacer.replace_expr(arr->m_dims[i].m_length);
                    }
                }
            }
        }
    }

    // Save host-side head expressions BEFORE in-place replacement
    struct DimInfo {
        ASR::expr_t *host_start;
        ASR::expr_t *host_end;
    };
    std::vector<DimInfo> dim_info;
    for (size_t d = 0; d < n_dims; d++) {
        dim_info.push_back({work.head(d).m_start, work.head(d).m_end});
    }

    // Deep-copy the body statements so that in-place symbol remapping
    // does not corrupt types shared with the original function scope
    // (e.g., ArrayBroadcast type sharing the same Array dimension Var
    // nodes as the original variable's type).
    ASRUtils::ExprStmtDuplicator body_dup(al);
    body_dup.success = true;
    Vec<ASR::stmt_t*> body_copy;
    body_copy.reserve(al, work.n_body);
    for (size_t i = 0; i < work.n_body; i++) {
        ASR::stmt_t *copy = body_dup.duplicate_stmt(work.body[i]);
        LCOMPILERS_ASSERT(copy);
        body_copy.push_back(al, copy);
    }

    // Turn every `size(a(i)%m, d)` in the copied body into a scalar
    // kernel argument the host computes at launch time.  The extent
    // of an allocatable component reached through a subscript into an
    // array of derived types is otherwise available to neither side:
    // the kernel is handed only that component's flattened data and
    // its per-element total size.  A workspace sized by such an
    // extent would be declined -- or, worse, sized by a guess -- so
    // it is resolved here into a plain integer parameter.  This has
    // to happen before the decomposition below rewrites the component
    // access into a flat-array Var, and while the body still names
    // the host symbols the launch site passes as actuals.

    // Replace StructInstanceMember references to decomposed
    // allocatable members with Var references to the new
    // flat-array kernel parameters, before general symbol remapping.
    if (!decomp_map.empty()) {
        GpuDecomposeStructVisitor decomp_visitor(al, kernel_scope,
            decomp_map);
        for (size_t i = 0; i < body_copy.n; i++) {
            decomp_visitor.visit_stmt(*body_copy.p[i]);
        }
    }

    // 3. Replace Var references in copied body to point to kernel scope
    GpuReplaceSymbolsVisitor sym_replacer(*kernel_scope);
    for (size_t i = 0; i < body_copy.n; i++) {
        sym_replacer.visit_stmt(*body_copy.p[i]);
    }

    // 4. Build kernel body
    Vec<ASR::stmt_t*> kernel_body;
    kernel_body.reserve(al, work.n_body + 2 * n_dims + 1);

    ASR::ttype_t *int_type = ASRUtils::TYPE(
        ASR::make_Integer_t(al, loc, 4));

    ASR::expr_t *thread_idx = ASRUtils::EXPR(
        ASR::make_GpuThreadIndex_t(al, loc, 0, int_type, nullptr));
    ASR::expr_t *block_idx = ASRUtils::EXPR(
        ASR::make_GpuBlockIndex_t(al, loc, 0, int_type, nullptr));
    ASR::expr_t *block_sz = ASRUtils::EXPR(
        ASR::make_GpuBlockSize_t(al, loc, 0, int_type, nullptr));

    // flat_idx = block_idx * block_size + thread_idx
    ASR::expr_t *flat_idx = ASRUtils::EXPR(
        ASR::make_IntegerBinOp_t(al, loc,
            ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                block_idx, ASR::binopType::Mul, block_sz, int_type, nullptr)),
            ASR::binopType::Add, thread_idx, int_type, nullptr));

    // For multi-dimensional: linearize index
    // For an iteration space (i=1:m, j=1:n, k=1:p):
    //   flat = flat_idx
    //   i = flat % m + 1;  flat = flat / m
    //   j = flat % n + 1;  flat = flat / n
    //   k = flat + 1  (last dim)
    //   guard: flat_idx >= m*n*k → return

    // Create kernel-scope versions of start/end for each dimension.
    // Instead of duplicating host expressions (which may contain
    // ArrayBound/ArraySize on allocatable arrays that cannot be
    // correctly evaluated in the kernel scope), pass the loop
    // bounds as pre-computed scalar parameters from the host.
    std::vector<ASR::expr_t*> kernel_starts, kernel_ends;
    for (size_t d = 0; d < n_dims; d++) {
        ASR::expr_t *host_start = dim_info[d].host_start;
        ASR::expr_t *host_end = dim_info[d].host_end;
        bool start_is_const = ASR::is_a<ASR::IntegerConstant_t>(*host_start);
        bool end_is_const = ASR::is_a<ASR::IntegerConstant_t>(*host_end);

        if (start_is_const) {
            kernel_starts.push_back(dup_expr_to_scope(host_start, kernel_scope));
        } else {
            std::string name = "__loop_start_" + std::to_string(d);
            ASR::symbol_t *param = gpu_new_variable(al, loc, kernel_scope,
                name, ASRUtils::duplicate_type(al, int_type),
                ASR::intentType::InOut);
            kernel_args.push_back(al,
                ASRUtils::EXPR(ASR::make_Var_t(al, loc, param)));
            ASR::call_arg_t carg;
            carg.loc = loc;
            carg.m_value = host_start;
            call_args.push_back(al, carg);
            kernel_starts.push_back(
                ASRUtils::EXPR(ASR::make_Var_t(al, loc, param)));
        }

        if (end_is_const) {
            kernel_ends.push_back(dup_expr_to_scope(host_end, kernel_scope));
        } else {
            std::string name = "__loop_end_" + std::to_string(d);
            ASR::symbol_t *param = gpu_new_variable(al, loc, kernel_scope,
                name, ASRUtils::duplicate_type(al, int_type),
                ASR::intentType::InOut);
            kernel_args.push_back(al,
                ASRUtils::EXPR(ASR::make_Var_t(al, loc, param)));
            ASR::call_arg_t carg;
            carg.loc = loc;
            carg.m_value = host_end;
            call_args.push_back(al, carg);
            kernel_ends.push_back(
                ASRUtils::EXPR(ASR::make_Var_t(al, loc, param)));
        }
    }

    // Decompose StructInstanceMember references in kernel head
    // expressions. After associate resolution, head bounds may
    // contain e.g. ArraySize(StructInstanceMember(Var(arg), nodes))
    // where arg was decomposed and removed from involved_syms.
    // Replace these with Var(arg__nodes) to match the kernel params.
    if (!decomp_map.empty()) {
        GpuDecomposeStructReplacer head_decomp(al, kernel_scope,
            decomp_map);
        for (size_t d = 0; d < n_dims; d++) {
            if (kernel_starts[d]) {
                head_decomp.current_expr = &kernel_starts[d];
                head_decomp.replace_expr(kernel_starts[d]);
            }
            if (kernel_ends[d]) {
                head_decomp.current_expr = &kernel_ends[d];
                head_decomp.replace_expr(kernel_ends[d]);
            }
        }
    }

    // Compute total_elements for host-side grid size
    // Also compute per-dim range: range_d = end_d - start_d + 1
    // For kernel: dim_size_d = end_d - start_d + 1
    ASR::expr_t *one_const = ASRUtils::EXPR(
        ASR::make_IntegerConstant_t(al, loc, 1, int_type,
            ASR::integerbozType::Decimal));

    // Compute total flat size for guard
    ASR::expr_t *total_size_kernel = nullptr;
    for (size_t d = 0; d < n_dims; d++) {
        // dim_range = kernel_end - kernel_start + 1
        ASR::expr_t *dim_range = ASRUtils::EXPR(
            ASR::make_IntegerBinOp_t(al, loc,
                ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                    kernel_ends[d], ASR::binopType::Sub,
                    kernel_starts[d], int_type, nullptr)),
                ASR::binopType::Add, one_const, int_type, nullptr));
        if (total_size_kernel == nullptr) {
            total_size_kernel = dim_range;
        } else {
            total_size_kernel = ASRUtils::EXPR(
                ASR::make_IntegerBinOp_t(al, loc,
                    total_size_kernel, ASR::binopType::Mul,
                    dim_range, int_type, nullptr));
        }
    }

    // Guard: if (flat_idx >= total_size) return
    ASR::expr_t *guard = ASRUtils::EXPR(
        ASR::make_IntegerCompare_t(al, loc, flat_idx,
            ASR::cmpopType::GtE, total_size_kernel,
            ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4)), nullptr));
    Vec<ASR::stmt_t*> guard_body;
    guard_body.reserve(al, 1);
    guard_body.push_back(al, ASRUtils::STMT(ASR::make_Return_t(al, loc)));
    Vec<ASR::stmt_t*> guard_else;
    guard_else.reserve(al, 0);
    kernel_body.push_back(al, ASRUtils::STMT(
        ASR::make_If_t(al, loc, nullptr, guard,
            guard_body.p, guard_body.n,
            guard_else.p, guard_else.n)));

    // Compute per-dim loop variable from flat_idx
    // We need a "remaining" variable in kernel scope
    std::string remain_name = "__flat_idx";
    {
        gpu_new_variable(al, loc, kernel_scope, remain_name,
            ASRUtils::duplicate_type(al, int_type));
    }
    ASR::expr_t *remain_var = ASRUtils::EXPR(
        ASR::make_Var_t(al, loc, kernel_scope->get_symbol(remain_name)));

    // __flat_idx = flat_idx (the raw thread index)
    kernel_body.push_back(al, ASRUtils::STMT(
        ASR::make_Assignment_t(al, loc, remain_var, flat_idx, nullptr, false, false)));

    for (size_t d = 0; d < n_dims; d++) {
        ASR::expr_t *dim_range = ASRUtils::EXPR(
            ASR::make_IntegerBinOp_t(al, loc,
                ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                    kernel_ends[d], ASR::binopType::Sub,
                    kernel_starts[d], int_type, nullptr)),
                ASR::binopType::Add, one_const, int_type, nullptr));

        ASR::symbol_t *kvar = kernel_scope->get_symbol(loop_var_names[d]);
        ASR::expr_t *kvar_expr = ASRUtils::EXPR(
            ASR::make_Var_t(al, loc, kvar));

        if (d < n_dims - 1) {
            // loop_var = __flat_idx % dim_range + start
            // Since ASR has no Mod binop, compute as: a - (a/b)*b
            ASR::expr_t *div_part = ASRUtils::EXPR(
                ASR::make_IntegerBinOp_t(al, loc,
                    remain_var, ASR::binopType::Div,
                    dim_range, int_type, nullptr));
            ASR::expr_t *mul_part = ASRUtils::EXPR(
                ASR::make_IntegerBinOp_t(al, loc,
                    div_part, ASR::binopType::Mul,
                    dim_range, int_type, nullptr));
            ASR::expr_t *mod_val = ASRUtils::EXPR(
                ASR::make_IntegerBinOp_t(al, loc,
                    remain_var, ASR::binopType::Sub,
                    mul_part, int_type, nullptr));
            ASR::expr_t *val = ASRUtils::EXPR(
                ASR::make_IntegerBinOp_t(al, loc,
                    mod_val, ASR::binopType::Add,
                    kernel_starts[d], int_type, nullptr));
            kernel_body.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, kvar_expr, val, nullptr, false, false)));

            // __flat_idx = __flat_idx / dim_range
            ASR::expr_t *div_val = ASRUtils::EXPR(
                ASR::make_IntegerBinOp_t(al, loc,
                    remain_var, ASR::binopType::Div,
                    dim_range, int_type, nullptr));
            kernel_body.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, remain_var, div_val, nullptr, false, false)));
        } else {
            // Last dim: loop_var = __flat_idx + start
            ASR::expr_t *val = ASRUtils::EXPR(
                ASR::make_IntegerBinOp_t(al, loc,
                    remain_var, ASR::binopType::Add,
                    kernel_starts[d], int_type, nullptr));
            kernel_body.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, kvar_expr, val, nullptr, false, false)));
        }
    }

    // This helper processes a block and recursively handles any nested
    // BlockCall statements, since GpuReplaceSymbolsVisitor does not
    // descend into BlockCall/AssociateBlockCall automatically.
    // `reparent` is true only for the top-level block; nested blocks
    // keep their existing parent (the enclosing block's symtab).
    std::function<void(ASR::Block_t*, bool)> process_block_for_kernel =
        [&](ASR::Block_t *block, bool reparent) {
        if (reparent) {
            block->m_symtab->parent = kernel_scope;
        }
        // A BLOCK-local derived type is not a kernel argument, so
        // involved_syms never imported it. Bring the Struct into the
        // kernel and retarget the local's type_declaration: the
        // kernel's parent is the translation unit, which cannot see
        // the host procedure's types.
        {
            std::function<void(SymbolTable*)> import_scope_structs =
                [&](SymbolTable *st) {
                for (auto &item : st->get_scope()) {
                    if (ASR::is_a<ASR::Variable_t>(*item.second)) {
                        import_struct_type(item.second, orig_scope,
                            kernel_scope, loc);
                    } else if (ASR::is_a<ASR::Block_t>(*item.second)) {
                        import_scope_structs(
                            ASR::down_cast<ASR::Block_t>(
                                item.second)->m_symtab);
                    } else if (ASR::is_a<ASR::AssociateBlock_t>(
                            *item.second)) {
                        import_scope_structs(
                            ASR::down_cast<ASR::AssociateBlock_t>(
                                item.second)->m_symtab);
                    }
                }
            };
            import_scope_structs(block->m_symtab);
            fixup_struct_refs_in_scope(block->m_symtab, kernel_scope,
                s2c(al, kernel_name));
        }
        // Pre-compute VLA dimension expressions that contain
        // FunctionCall nodes on the host side and pass the
        // results as scalar kernel parameters, because GPU
        // kernels cannot call arbitrary host-side functions.
        // This must happen BEFORE body remapping: the variable
        // type and body expression types (e.g. ArrayBroadcast
        // m_type) may share the same Array_t pointer, so body
        // remapping would change Var references in the shared
        // dimension to point to kernel-scope symbols. The
        // host_expr duplicate must capture the original
        // (caller-scope) references for the host-side call args.
        // Track old→new expression replacements so that DoLoop
        // bounds created by the ArrayBroadcast lowering (which
        // copied the old dimension pointers) can be updated too.
        std::vector<std::pair<ASR::expr_t*, ASR::expr_t*>>
            dim_replacements;
        {
            ASRUtils::ExprStmtDuplicator dim_dup(al);
            dim_dup.success = true;
            std::set<std::string> loop_index_names;
            for (size_t d = 0; d < work.n_heads(); d++) {
                if (!work.head(d).m_v) continue;
                if (!ASR::is_a<ASR::Var_t>(*work.head(d).m_v)) continue;
                loop_index_names.insert(ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(work.head(d).m_v)->m_v));
            }
            for (auto &item : block->m_symtab->get_scope()) {
                if (!ASR::is_a<ASR::Variable_t>(*item.second))
                    continue;
                ASR::Variable_t *bvar =
                    ASR::down_cast<ASR::Variable_t>(item.second);
                if (!ASR::is_a<ASR::Array_t>(*bvar->m_type))
                    continue;
                ASR::Array_t *arr =
                    ASR::down_cast<ASR::Array_t>(bvar->m_type);
                for (size_t d = 0; d < arr->n_dims; d++) {
                    ASR::expr_t **dim_ptrs[2] = {
                        &arr->m_dims[d].m_start,
                        &arr->m_dims[d].m_length};
                    for (int e = 0; e < 2; e++) {
                        if (!*dim_ptrs[e]) continue;
                        if (!expr_has_function_call(
                                *dim_ptrs[e]))
                            continue;
                        ASR::expr_t *old_dim_expr = *dim_ptrs[e];
                        // The host evaluates this expression before
                        // it launches, where the loop index has no
                        // value -- so a `size(f(i, ...))` must be
                        // taken from the shape in its own type
                        // rather than by calling `f` on the host.
                        ASR::expr_t *host_expr =
                            gpu_simplify_array_sizes(al,
                                dim_dup.duplicate_expr(
                                    *dim_ptrs[e]));
                        gpu_check_host_expr_index_free(host_expr,
                            loop_index_names,
                            "the extent of '" +
                                std::string(bvar->m_name) + "'");
                        std::string pname =
                            kernel_scope->get_unique_name(
                                "__lfortran_gpu_dim_", false);
                        ASR::ttype_t *ptype =
                            ASRUtils::duplicate_type(al,
                                ASRUtils::expr_type(
                                    *dim_ptrs[e]));
                        ASR::symbol_t *psym = gpu_new_variable(al, loc,
                            kernel_scope, pname, ptype,
                            ASR::intentType::InOut);
                        kernel_args.push_back(al,
                            ASRUtils::EXPR(ASR::make_Var_t(
                                al, loc, psym)));
                        ASR::call_arg_t carg;
                        carg.loc = loc;
                        carg.m_value = host_expr;
                        call_args.push_back(al, carg);
                        ASR::expr_t *new_dim_expr = ASRUtils::EXPR(
                            ASR::make_Var_t(al, loc, psym));
                        *dim_ptrs[e] = new_dim_expr;
                        dim_replacements.push_back(
                            {old_dim_expr, new_dim_expr});
                    }
                }
            }
        }
        // The ArrayBroadcast lowering (inline_elemental_array_var_
        // in_body) may have created DoLoop statements whose bounds
        // copied the old VLA dimension expression pointers before
        // the pre-computation above replaced them. Walk the block
        // body and patch any DoLoop bounds that still reference the
        // old expressions.
        if (!dim_replacements.empty()) {
            std::function<void(ASR::stmt_t**, size_t)>
                patch_do_loop_bounds = [&](ASR::stmt_t **stmts,
                                           size_t n_stmts) {
                for (size_t si = 0; si < n_stmts; si++) {
                    if (ASR::is_a<ASR::DoLoop_t>(*stmts[si])) {
                        ASR::DoLoop_t *dl =
                            ASR::down_cast<ASR::DoLoop_t>(
                                stmts[si]);
                        for (auto &[old_e, new_e] :
                                dim_replacements) {
                            if (dl->m_head.m_start == old_e)
                                dl->m_head.m_start = new_e;
                            if (dl->m_head.m_end == old_e)
                                dl->m_head.m_end = new_e;
                        }
                        patch_do_loop_bounds(dl->m_body,
                            dl->n_body);
                    }
                }
            };
            patch_do_loop_bounds(block->m_body, block->n_body);
        }
        // Remap Var references inside the block body
        GpuReplaceSymbolsVisitor block_replacer(*kernel_scope);
        for (size_t j = 0; j < block->n_body; j++) {
            block_replacer.visit_stmt(*block->m_body[j]);
        }
        // Also remap Var references inside AssociateBlock bodies
        // within this Block, since the visitor does not descend
        // into AssociateBlockCall targets automatically.
        for (auto &item : block->m_symtab->get_scope()) {
            if (!ASR::is_a<ASR::AssociateBlock_t>(*item.second))
                continue;
            ASR::AssociateBlock_t *ab =
                ASR::down_cast<ASR::AssociateBlock_t>(
                    item.second);
            for (size_t j = 0; j < ab->n_body; j++) {
                block_replacer.visit_stmt(*ab->m_body[j]);
            }
        }
        // Replace StructInstanceMember references to decomposed
        // allocatable members inside the block body.
        if (!decomp_map.empty()) {
            GpuDecomposeStructVisitor block_decomp(al, kernel_scope,
                decomp_map);
            for (size_t j = 0; j < block->n_body; j++) {
                block_decomp.visit_stmt(*block->m_body[j]);
            }
            // Also decompose StructInstanceMember references in
            // block-local variable type expressions (e.g., VLA
            // dimensions like size(self%x) after associate
            // resolution). Without this, a fully-decomposed struct
            // removed from involved_syms leaves dangling Var refs.
            GpuDecomposeStructReplacer block_type_decomp(al,
                kernel_scope, decomp_map);
            for (auto &item : block->m_symtab->get_scope()) {
                if (!ASR::is_a<ASR::Variable_t>(*item.second))
                    continue;
                ASR::Variable_t *bvar =
                    ASR::down_cast<ASR::Variable_t>(item.second);
                if (!ASR::is_a<ASR::Array_t>(*bvar->m_type))
                    continue;
                ASR::Array_t *arr =
                    ASR::down_cast<ASR::Array_t>(bvar->m_type);
                for (size_t d = 0; d < arr->n_dims; d++) {
                    if (arr->m_dims[d].m_start) {
                        block_type_decomp.current_expr =
                            &(arr->m_dims[d].m_start);
                        block_type_decomp.replace_expr(
                            arr->m_dims[d].m_start);
                    }
                    if (arr->m_dims[d].m_length) {
                        block_type_decomp.current_expr =
                            &(arr->m_dims[d].m_length);
                        block_type_decomp.replace_expr(
                            arr->m_dims[d].m_length);
                    }
                }
            }
        }
        // Recursively process nested BlockCall statements
        for (size_t j = 0; j < block->n_body; j++) {
            if (ASR::is_a<ASR::BlockCall_t>(*block->m_body[j])) {
                ASR::BlockCall_t *inner_bc =
                    ASR::down_cast<ASR::BlockCall_t>(block->m_body[j]);
                if (ASR::is_a<ASR::Block_t>(*inner_bc->m_m)) {
                    ASR::Block_t *inner =
                        ASR::down_cast<ASR::Block_t>(inner_bc->m_m);
                    std::string inner_name = inner->m_name;
                    bool host_owned = orig_scope->get_symbol(inner_name)
                        == inner_bc->m_m;
                    process_block_for_kernel(inner, host_owned);
                    if (host_owned) {
                        orig_scope->erase_symbol(inner_name);
                        if (!kernel_scope->get_symbol(inner_name)) {
                            kernel_scope->add_symbol(inner_name,
                                inner_bc->m_m);
                        }
                    }
                }
            }
        }
        // Remap type expressions of block-local variables
        // (e.g., VLA dimensions like n(i) in real :: a(n(i)))
        GpuReplaceSymbols block_type_replacer(*kernel_scope);
        for (auto &item : block->m_symtab->get_scope()) {
            if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
                item.second);
            ASR::ttype_t *type = var->m_type;
            if (ASR::is_a<ASR::Array_t>(*type)) {
                ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
                for (size_t d = 0; d < arr->n_dims; d++) {
                    if (arr->m_dims[d].m_start) {
                        block_type_replacer.current_expr =
                            &(arr->m_dims[d].m_start);
                        block_type_replacer.replace_expr(
                            arr->m_dims[d].m_start);
                    }
                    if (arr->m_dims[d].m_length) {
                        block_type_replacer.current_expr =
                            &(arr->m_dims[d].m_length);
                        block_type_replacer.replace_expr(
                            arr->m_dims[d].m_length);
                    }
                }
            }
        }
    };
    // Recursively find and move all BlockCall targets from any
    // nesting depth (e.g., BlockCall inside a DoLoop inside the
    // loop body) into the kernel scope.
    std::function<void(ASR::stmt_t**, size_t)>
        move_blocks_to_kernel = [&](ASR::stmt_t **stmts,
                                    size_t n_stmts) {
        for (size_t i = 0; i < n_stmts; i++) {
            if (ASR::is_a<ASR::BlockCall_t>(*stmts[i])) {
                ASR::BlockCall_t *bc =
                    ASR::down_cast<ASR::BlockCall_t>(stmts[i]);
                if (ASR::is_a<ASR::Block_t>(*bc->m_m)) {
                    ASR::Block_t *block =
                        ASR::down_cast<ASR::Block_t>(bc->m_m);
                    std::string block_name = block->m_name;
                    // Only a copy this pass made is safe to take. A
                    // BlockCall that still names the host's own block
                    // is a copy_loop_stmt miss; mutating it here
                    // would steal the host nest if the launch is then
                    // declined.
                    bool is_copy = false;
                    for (ASR::symbol_t *b : kernel_blocks) {
                        if (b == bc->m_m) {
                            is_copy = true;
                            break;
                        }
                    }
                    if (!is_copy) continue;
                    process_block_for_kernel(block, true);
                    if (orig_scope->get_symbol(block_name) == bc->m_m) {
                        orig_scope->erase_symbol(block_name);
                    }
                    if (!kernel_scope->get_symbol(block_name)) {
                        kernel_scope->add_symbol(block_name, bc->m_m);
                    }
                }
            } else if (ASR::is_a<ASR::DoLoop_t>(*stmts[i])) {
                ASR::DoLoop_t *dl =
                    ASR::down_cast<ASR::DoLoop_t>(stmts[i]);
                move_blocks_to_kernel(dl->m_body, dl->n_body);
            } else if (ASR::is_a<ASR::If_t>(*stmts[i])) {
                ASR::If_t *ifs =
                    ASR::down_cast<ASR::If_t>(stmts[i]);
                move_blocks_to_kernel(ifs->m_body, ifs->n_body);
                move_blocks_to_kernel(ifs->m_orelse, ifs->n_orelse);
            } else if (ASR::is_a<ASR::WhileLoop_t>(*stmts[i])) {
                ASR::WhileLoop_t *wl =
                    ASR::down_cast<ASR::WhileLoop_t>(stmts[i]);
                move_blocks_to_kernel(wl->m_body, wl->n_body);
            }
        }
    };
    move_blocks_to_kernel(body_copy.p, body_copy.n);

    {
        std::vector<std::string> kernel_arg_names;
        for (size_t i = 0; i < kernel_args.n; i++) {
            kernel_arg_names.push_back(ASRUtils::symbol_name(
                ASR::down_cast<ASR::Var_t>(kernel_args.p[i])->m_v));
        }
        std::vector<std::pair<ASR::symbol_t*, ASR::expr_t*>>
            member_extent_args;
        GpuStructArrayMemberExtentVisitor mev(al, orig_scope,
            kernel_scope, kernel_arg_names, member_extent_args);
        for (size_t i = 0; i < body_copy.n; i++) {
            mev.visit_stmt(*body_copy.p[i]);
        }
        member_extent_undo = mev.replacer.undo;
        for (auto &pair : member_extent_args) {
            kernel_args.push_back(al,
                ASRUtils::EXPR(ASR::make_Var_t(al, loc, pair.first)));
            ASR::call_arg_t carg;
            carg.loc = loc;
            carg.m_value = pair.second;
            call_args.push_back(al, carg);
        }
    }

    // Add copied loop body (already remapped)
    for (size_t i = 0; i < body_copy.n; i++) {
        kernel_body.push_back(al, body_copy.p[i]);
    }

    // 5. Build function signature
    // FunctionType arg_types must not contain scope-bound expressions,
    // so strip dimension expressions that reference variables.
    Vec<ASR::ttype_t*> arg_types;
    arg_types.reserve(al, kernel_args.n);
    for (size_t i = 0; i < kernel_args.n; i++) {
        ASR::Var_t *v = down_cast<ASR::Var_t>(kernel_args.p[i]);
        ASR::ttype_t *t = ASRUtils::symbol_type(v->m_v);
        if (ASR::is_a<ASR::Array_t>(*t)) {
            ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(t);
            ASR::dimension_t *new_dims = al.allocate<ASR::dimension_t>(arr->n_dims);
            for (size_t d = 0; d < arr->n_dims; d++) {
                new_dims[d].loc = arr->m_dims[d].loc;
                new_dims[d].m_start = nullptr;
                new_dims[d].m_length = nullptr;
            }
            t = ASRUtils::TYPE(ASR::make_Array_t(al, arr->base.base.loc,
                arr->m_type, new_dims, arr->n_dims,
                arr->m_physical_type, arr->m_memory_space));
        }
        arg_types.push_back(al, t);
    }
    ASR::ttype_t *fn_sig = ASRUtils::TYPE(
        ASR::make_FunctionType_t(al, loc,
            arg_types.p, arg_types.n, nullptr,
            ASR::abiType::Source, ASR::deftypeType::Implementation,
            nullptr, false, false, false, false, false, nullptr, 0, false,
            ASR::exec_spaceType::Kernel));

    // 6. Create the kernel as a Kernel function. A kernel is
    // subroutine-shaped, so it has no return variable, and `fn_sig` is
    // built above rather than by `make_Function_t_util` because the
    // argument types must not carry scope-bound dimension expressions.
    ASR::asr_t *kernel_func = ASR::make_Function_t(al, loc,
        kernel_scope, s2c(al, kernel_name), fn_sig,
        nullptr, 0,
        kernel_args.p, kernel_args.n,
        kernel_body.p, kernel_body.n,
        nullptr, ASR::accessType::Public, false, false,
        nullptr, nullptr, nullptr);

    // `device_launch_expand` builds the host side of the launch, laying
    // every argument out exactly as the device code generator does. An
    // argument shape it cannot lay out keeps the loop on the host, where
    // ordinary Fortran semantics always apply. The kernel is checked
    // before it enters the symbol table, so nothing is left behind.
    {
        GpuDecline decline;
        if (!gpu_launch_is_supported(al,
                ASR::down_cast<ASR::symbol_t>(kernel_func),
                call_args.p, call_args.n, decline)) {
            report_not_offloaded(loc, decline);
            for (auto it = member_extent_undo.rbegin();
                    it != member_extent_undo.rend(); ++it) {
                *it->first = it->second;
            }
            member_extent_undo.clear();
            // The host's own blocks were never touched: the kernel was
            // given copies, which the draft guard drops.
            return;
        }
    }
    // The launch stands, so the blocks, the spliced shape, and the
    // kernel number are the kernel's from here on.
    draft_guard.commit();
    splice_guard.commit();

    // The loop is offloaded from here on, so this is where a clause the
    // launch cannot honour is reported: before this every exit still
    // leaves the loop on the host, where the clause is honoured.
    for (size_t i = 0; i < region.n_clauses; i++) {
        std::string clause_name = unhonoured_clause(region.m_clauses[i]);
        if (!clause_name.empty()) {
            report_clause_ignored(region.m_clauses[i]->base.loc,
                clause_name);
        }
    }

    tu_symtab->add_symbol(kernel_name,
        ASR::down_cast<ASR::symbol_t>(kernel_func));

    // Pre-allocate host-side allocatable arrays that are assigned
    // from a FunctionCall inside the loop body. The GPU
    // kernel receives the buffer pointer at launch time, so the
    // array must already be allocated on the host before dispatch.
    Vec<ASR::stmt_t*> pre_launch_stmts;
    pre_launch_stmts.reserve(al, 4);
    for (size_t si = 0; si < work.n_body; si++) {
        ASR::stmt_t *stmt = work.body[si];
        // Unwrap BlockCall to inspect block body statements
        ASR::stmt_t **stmts_to_scan = &stmt;
        size_t n_stmts_to_scan = 1;
        if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
            ASR::BlockCall_t *bc =
                ASR::down_cast<ASR::BlockCall_t>(stmt);
            if (ASR::is_a<ASR::Block_t>(*bc->m_m)) {
                ASR::Block_t *blk =
                    ASR::down_cast<ASR::Block_t>(bc->m_m);
                stmts_to_scan = blk->m_body;
                n_stmts_to_scan = blk->n_body;
            }
        }
        for (size_t sj = 0; sj < n_stmts_to_scan; sj++) {
            if (!ASR::is_a<ASR::Assignment_t>(*stmts_to_scan[sj]))
                continue;
            ASR::Assignment_t *asgn =
                ASR::down_cast<ASR::Assignment_t>(stmts_to_scan[sj]);
            if (!ASR::is_a<ASR::Var_t>(*asgn->m_target)) continue;
            if (!ASR::is_a<ASR::FunctionCall_t>(*asgn->m_value))
                continue;

            ASR::Var_t *target_var =
                ASR::down_cast<ASR::Var_t>(asgn->m_target);
            ASR::symbol_t *orig_sym =
                ASRUtils::symbol_get_past_external(target_var->m_v);
            if (!ASR::is_a<ASR::Variable_t>(*orig_sym)) continue;
            ASR::Variable_t *var =
                ASR::down_cast<ASR::Variable_t>(orig_sym);
            if (!ASRUtils::is_allocatable(var->m_type)) continue;

            ASR::FunctionCall_t *fc =
                ASR::down_cast<ASR::FunctionCall_t>(asgn->m_value);
            ASR::symbol_t *fn_sym =
                ASRUtils::symbol_get_past_external(fc->m_name);
            if (!ASR::is_a<ASR::Function_t>(*fn_sym)) continue;

            ASR::Function_t *fn =
                ASR::down_cast<ASR::Function_t>(fn_sym);
            std::string ret_name;
            if (fn->m_return_var &&
                    ASR::is_a<ASR::Var_t>(*fn->m_return_var)) {
                ret_name = ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(
                        fn->m_return_var)->m_v);
            }
            if (ret_name.empty()) continue;

            // Find the Allocate statement for the return variable
            // in the function body and use its dimensions.
            bool alloc_found = false;
            for (size_t bi = 0;
                    bi < fn->n_body && !alloc_found; bi++) {
                if (!ASR::is_a<ASR::Allocate_t>(*fn->m_body[bi]))
                    continue;
                ASR::Allocate_t *fn_alloc =
                    ASR::down_cast<ASR::Allocate_t>(fn->m_body[bi]);
                for (size_t ai = 0; ai < fn_alloc->n_args; ai++) {
                    if (!fn_alloc->m_args[ai].m_a ||
                            !ASR::is_a<ASR::Var_t>(
                                *fn_alloc->m_args[ai].m_a))
                        continue;
                    std::string aname = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(
                            fn_alloc->m_args[ai].m_a)->m_v);
                    if (aname != ret_name) continue;

                    ASRUtils::ExprStmtDuplicator dup(al);
                    dup.success = true;
                    ASR::alloc_arg_t host_arg;
                    host_arg.loc = loc;
                    host_arg.m_a = asgn->m_target;
                    host_arg.n_dims =
                        fn_alloc->m_args[ai].n_dims;
                    host_arg.m_dims =
                        al.allocate<ASR::dimension_t>(
                            host_arg.n_dims);
                    for (size_t d = 0; d < host_arg.n_dims; d++) {
                        host_arg.m_dims[d].loc = loc;
                        host_arg.m_dims[d].m_start =
                            fn_alloc->m_args[ai].m_dims[d].m_start
                            ? dup.duplicate_expr(
                                fn_alloc->m_args[ai]
                                    .m_dims[d].m_start)
                            : nullptr;
                        host_arg.m_dims[d].m_length =
                            fn_alloc->m_args[ai].m_dims[d].m_length
                            ? dup.duplicate_expr(
                                fn_alloc->m_args[ai]
                                    .m_dims[d].m_length)
                            : nullptr;
                    }
                    host_arg.m_len_expr = nullptr;
                    host_arg.m_sym_subclass = nullptr;
                    host_arg.m_type = nullptr;
                    host_arg.m_codims = nullptr;
                    host_arg.n_codims = 0;

                    Vec<ASR::alloc_arg_t> alloc_vec;
                    alloc_vec.reserve(al, 1);
                    alloc_vec.push_back(al, host_arg);
                    pre_launch_stmts.push_back(al,
                        ASRUtils::STMT(ASR::make_Allocate_t(
                            al, loc, alloc_vec.p, alloc_vec.n,
                            nullptr, nullptr, nullptr)));
                    alloc_found = true;
                    break;
                }
            }
        }
    }

    // 7. Replace the region with GpuKernelLaunch + GpuSync
    // Collect all launch-related statements into a temporary Vec.
    // If any involved variable is optional, wrap them in a
    // present() guard so the host never reads a null descriptor.
    gather_guard.commit();
    Vec<ASR::stmt_t*> launch_stmts;
    launch_stmts.reserve(al, gather_stmts.n + pre_launch_stmts.n
        + scatter_stmts.n + liveout_scalars.size() + 2
        + liveout_scalars.size());
    for (size_t gi = 0; gi < gather_stmts.n; gi++) {
        launch_stmts.push_back(al, gather_stmts.p[gi]);
    }
    for (size_t pi = 0; pi < pre_launch_stmts.n; pi++) {
        launch_stmts.push_back(al, pre_launch_stmts.p[pi]);
    }

    // Copy liveout scalars into their 1-element array buffers
    // before the kernel launch so the buffer has the initial value
    for (auto &ls : liveout_scalars) {
        ASR::expr_t *buf_var = ASRUtils::EXPR(
            ASR::make_Var_t(al, loc, ls.host_buf_sym));
        ASR::expr_t *scalar_var = ASRUtils::EXPR(
            ASR::make_Var_t(al, loc, ls.orig_scalar_sym));
        ASR::expr_t *idx_one = ASRUtils::EXPR(
            ASR::make_IntegerConstant_t(al, loc, 1, int_type,
                ASR::integerbozType::Decimal));
        Vec<ASR::array_index_t> ai_args;
        ai_args.reserve(al, 1);
        ASR::array_index_t ai;
        ai.loc = loc;
        ai.m_left = nullptr;
        ai.m_right = idx_one;
        ai.m_step = nullptr;
        ai_args.push_back(al, ai);
        ASR::expr_t *buf_item = ASRUtils::EXPR(
            ASR::make_ArrayItem_t(al, loc, buf_var,
                ai_args.p, 1, ls.scalar_type,
                ASR::arraystorageType::ColMajor, nullptr));
        launch_stmts.push_back(al, ASRUtils::STMT(
            ASR::make_Assignment_t(al, loc, buf_item, scalar_var,
                nullptr, false, false)));
    }

    ASR::expr_t *block_size_const = ASRUtils::EXPR(
        ASR::make_IntegerConstant_t(al, loc, 256, int_type,
            ASR::integerbozType::Decimal));

    // Compute host-side total_elements = product of (end_d - start_d + 1)
    ASR::expr_t *host_one = ASRUtils::EXPR(
        ASR::make_IntegerConstant_t(al, loc, 1, int_type,
            ASR::integerbozType::Decimal));
    ASR::expr_t *host_total = nullptr;
    for (size_t d = 0; d < n_dims; d++) {
        ASR::expr_t *dim_range = ASRUtils::EXPR(
            ASR::make_IntegerBinOp_t(al, loc,
                ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                    dim_info[d].host_end, ASR::binopType::Sub,
                    dim_info[d].host_start, int_type, nullptr)),
                ASR::binopType::Add, host_one, int_type, nullptr));
        if (host_total == nullptr) {
            host_total = dim_range;
        } else {
            host_total = ASRUtils::EXPR(
                ASR::make_IntegerBinOp_t(al, loc,
                    host_total, ASR::binopType::Mul,
                    dim_range, int_type, nullptr));
        }
    }

    // grid_size = (total + 255) / 256
    ASR::expr_t *grid_padded = ASRUtils::EXPR(
        ASR::make_IntegerBinOp_t(al, loc, host_total, ASR::binopType::Add,
            ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 255, int_type,
                ASR::integerbozType::Decimal)),
            int_type, nullptr));
    ASR::expr_t *grid_size = ASRUtils::EXPR(
        ASR::make_IntegerBinOp_t(al, loc, grid_padded, ASR::binopType::Div,
            block_size_const, int_type, nullptr));

    launch_stmts.push_back(al, ASRUtils::STMT(
        ASR::make_GpuKernelLaunch_t(al, loc,
            ASR::down_cast<ASR::symbol_t>(kernel_func),
            grid_size, block_size_const,
            call_args.p, call_args.n)));

    launch_stmts.push_back(al, ASRUtils::STMT(
        ASR::make_GpuSync_t(al, loc)));

    // Put every gathered element the kernel wrote into back over the
    // original, before anything on the host can read it again.
    for (size_t si = 0; si < scatter_stmts.n; si++) {
        launch_stmts.push_back(al, scatter_stmts.p[si]);
    }

    // Copy liveout scalar results back from the 1-element array
    // buffers after the kernel has completed
    for (auto &ls : liveout_scalars) {
        ASR::expr_t *buf_var = ASRUtils::EXPR(
            ASR::make_Var_t(al, loc, ls.host_buf_sym));
        ASR::expr_t *scalar_var = ASRUtils::EXPR(
            ASR::make_Var_t(al, loc, ls.orig_scalar_sym));
        ASR::expr_t *idx_one = ASRUtils::EXPR(
            ASR::make_IntegerConstant_t(al, loc, 1, int_type,
                ASR::integerbozType::Decimal));
        Vec<ASR::array_index_t> ai_args;
        ai_args.reserve(al, 1);
        ASR::array_index_t ai;
        ai.loc = loc;
        ai.m_left = nullptr;
        ai.m_right = idx_one;
        ai.m_step = nullptr;
        ai_args.push_back(al, ai);
        ASR::expr_t *buf_item = ASRUtils::EXPR(
            ASR::make_ArrayItem_t(al, loc, buf_var,
                ai_args.p, 1, ls.scalar_type,
                ASR::arraystorageType::ColMajor, nullptr));
        launch_stmts.push_back(al, ASRUtils::STMT(
            ASR::make_Assignment_t(al, loc, scalar_var, buf_item,
                nullptr, false, false)));
    }

    // If any involved variable is optional, wrap the whole kernel
    // launch block in if(present(v1) .and. present(v2) ...) so
    // the host never tries to read a null descriptor or compute
    // ArraySize on an absent argument.
    if (!optional_syms.empty()) {
        ASR::ttype_t *log_type = ASRUtils::TYPE(
            ASR::make_Logical_t(al, loc, 4));
        ASR::expr_t *guard = nullptr;
        for (ASR::symbol_t *opt_sym : optional_syms) {
            Vec<ASR::expr_t*> present_args;
            present_args.reserve(al, 1);
            present_args.push_back(al, ASRUtils::EXPR(
                ASR::make_Var_t(al, loc, opt_sym)));
            ASR::expr_t *present_call = ASRUtils::EXPR(
                ASR::make_IntrinsicElementalFunction_t(al, loc,
                    static_cast<int64_t>(
                        ASRUtils::IntrinsicElementalFunctions::Present),
                    present_args.p, present_args.n, 0,
                    log_type, nullptr));
            if (guard == nullptr) {
                guard = present_call;
            } else {
                guard = ASRUtils::EXPR(
                    ASR::make_LogicalBinOp_t(al, loc, guard,
                        ASR::logicalbinopType::And, present_call,
                        log_type, nullptr));
            }
        }
        Vec<ASR::stmt_t*> empty_else;
        empty_else.reserve(al, 0);
        pass_result.reserve(al, 1);
        pass_result.push_back(al, ASRUtils::STMT(
            ASR::make_If_t(al, loc, nullptr, guard,
                launch_stmts.p, launch_stmts.n,
                empty_else.p, empty_else.n)));
    } else {
        pass_result.reserve(al, launch_stmts.n);
        for (size_t i = 0; i < launch_stmts.n; i++) {
            pass_result.push_back(al, launch_stmts.p[i]);
        }
    }
}

// A loop the offload pass turned down is still a parallel loop, so it goes
// back to whoever else can run it rather than to a single thread by default.
class DeclinedLoopVisitor : public ASR::BaseWalkVisitor<DeclinedLoopVisitor>
{
public:
    const PassOptions &pass_options;

    DeclinedLoopVisitor(const PassOptions &pass_options_) :
        pass_options(pass_options_) {
    }

    void visit_OMPRegion(const ASR::OMPRegion_t &x) {
        ASR::OMPRegion_t &xx = const_cast<ASR::OMPRegion_t&>(x);
        if (xx.m_exec_target == ASR::exec_targetType::ExecDevice) {
            xx.m_exec_target = host_exec_target(pass_options);
        }
        ASR::BaseWalkVisitor<DeclinedLoopVisitor>::visit_OMPRegion(x);
    }
};

void pass_replace_gpu_offload(Allocator &al, ASR::TranslationUnit_t &unit,
                              const LCompilers::PassOptions& pass_options) {
    if (!gpu_device_capabilities(pass_options).device_selected()) return;
    GpuOffloadVisitor v(al, pass_options, unit);
    v.asr_changed = true;
    while (v.asr_changed) {
        v.asr_changed = false;
        v.mark_regions_device_code_runs();
        v.visit_TranslationUnit(unit);
    }
    DeclinedLoopVisitor d(pass_options);
    d.visit_TranslationUnit(unit);
    // Kernel extraction moves Block symbols out of their enclosing
    // function, which can leave stale entries in that function's
    // dependency list. Recompute all dependencies to fix this.
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

} // namespace LCompilers
