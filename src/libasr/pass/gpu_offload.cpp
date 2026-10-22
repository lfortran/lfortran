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
#include <libasr/pass/gpu_data_layout.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/pass/gpu_device_struct_storage.h>
#include <libasr/pass/gpu_offload_collect.h>
#include <libasr/pass/gpu_offload_designator.h>
#include <libasr/pass/gpu_offload_host_association.h>
#include <libasr/pass/gpu_offload_preflight.h>
#include <libasr/pass/symbol_expr_substitution.h>
#include <libasr/pass/gpu_offload_rewrite.h>
#include <libasr/pass/gpu_offload_visitor.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/replace_gpu_offload.h>

namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

static int gpu_kernel_counter = 0;

class GpuContiguousStoreFinder : public ASR::BaseWalkVisitor<GpuContiguousStoreFinder> {
    const std::set<ASR::symbol_t*> &loop_variables;
    // Which subscript of a store, counted from the contiguous one.
    size_t subscript;
    std::set<ASR::symbol_t*> visited_blocks;

public:
    std::set<ASR::symbol_t*> candidates;
    bool has_unrecognized_store = false;

    GpuContiguousStoreFinder(const std::set<ASR::symbol_t*> &variables,
            size_t subscript) : loop_variables(variables),
            subscript(subscript) {}

    void note_store(ASR::expr_t *target) {
        ASR::ttype_t *type = ASRUtils::expr_type(target);
        if (ASR::is_a<ASR::Var_t>(*target) && !ASRUtils::is_array(type)
                && !ASRUtils::is_pointer(type)
                && !ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(type))) {
            return;
        }
        if (!ASR::is_a<ASR::ArrayItem_t>(*target)) {
            has_unrecognized_store = true;
            return;
        }
        const auto &item = *ASR::down_cast<ASR::ArrayItem_t>(target);
        if (item.n_args <= subscript) {
            has_unrecognized_store = true;
            return;
        }
        size_t dimension = item.m_storage_format == ASR::arraystorageType::ColMajor
            ? subscript : item.n_args - 1 - subscript;
        ASR::expr_t *index = item.m_args[dimension].m_right;
        if (index && ASR::is_a<ASR::Var_t>(*index)) {
            ASR::symbol_t *variable = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(index)->m_v);
            if (loop_variables.count(variable)) {
                candidates.insert(variable);
                return;
            }
        }
        // A subscript that names no loop index (one the body computes from
        // them, such as the indices of the frame index-set splitting packs
        // into the nest, gpu_index_set_split.cpp) says nothing about which
        // index is contiguous. The same holds for a scalar the source
        // computes, as in `k = i + 1; b(k) = ...`. This only picks the
        // layout of the threads, which every choice runs correctly, so
        // such a store is passed over instead of disabling the choice.
        if (index && !names_loop_variable(index)) return;
        has_unrecognized_store = true;
    }

    bool names_loop_variable(ASR::expr_t *e) {
        struct Find : public ASR::BaseWalkVisitor<Find> {
            const std::set<ASR::symbol_t*> *vars;
            bool found = false;
            void visit_Var(const ASR::Var_t &x) {
                if (vars->count(ASRUtils::symbol_get_past_external(x.m_v))) {
                    found = true;
                }
            }
        } f;
        f.vars = &loop_variables;
        f.visit_expr(*e);
        return f.found;
    }

    void note_call_args(ASR::symbol_t *name, ASR::call_arg_t *args,
            size_t n_args) {
        for (size_t i = 0; i < n_args; i++) {
            if (!args[i].m_value) continue;
            ASR::Variable_t *dummy = GpuWrittenRootCollector::dummy_of(name, i);
            if (dummy && dummy->m_intent == ASR::intentType::In) continue;
            note_store(args[i].m_value);
        }
    }

    void visit_Assignment(const ASR::Assignment_t &x) {
        note_store(x.m_target);
        ASR::BaseWalkVisitor<GpuContiguousStoreFinder>::visit_Assignment(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        note_call_args(x.m_name, x.m_args, x.n_args);
        ASR::BaseWalkVisitor<GpuContiguousStoreFinder>::visit_SubroutineCall(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        note_call_args(x.m_name, x.m_args, x.n_args);
        ASR::BaseWalkVisitor<GpuContiguousStoreFinder>::visit_FunctionCall(x);
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *symbol = ASRUtils::symbol_get_past_external(x.m_m);
        if (!visited_blocks.insert(symbol).second) return;
        const auto &block = *ASR::down_cast<ASR::Block_t>(symbol);
        for (size_t i = 0; i < block.n_body; i++) visit_stmt(*block.m_body[i]);
    }
};

// The dimension of the nest that every store indexes with its subscript
// at `subscript` (0 is the contiguous one), or `otherwise`.
static size_t store_dimension(const ParallelLoopNest &nest, size_t subscript,
        size_t otherwise) {
    if (nest.n_heads() < 2) return otherwise;
    std::vector<ASR::symbol_t*> variables;
    std::set<ASR::symbol_t*> variable_set;
    for (size_t d = 0; d < nest.n_heads(); d++) {
        ASR::symbol_t *variable = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(nest.head(d).m_v)->m_v);
        variables.push_back(variable);
        variable_set.insert(variable);
    }
    GpuContiguousStoreFinder finder(variable_set, subscript);
    for (size_t i = 0; i < nest.n_body; i++) finder.visit_stmt(*nest.body[i]);
    // Conflicting or unrecognized store layouts retain the original mapping.
    if (!finder.has_unrecognized_store && finder.candidates.size() == 1) {
        for (size_t d = 0; d < variables.size(); d++) {
            if (finder.candidates.count(variables[d])) return d;
        }
    }
    return otherwise;
}

// The loop was assigned to the device by the unsupported-construct check,
// so a decline here is a lowering this pass is missing and is reported as
// an error (see report_gpu_decline). Every loop of the round that finds
// one is reported before the compilation is stopped, so one run lists the
// gaps rather than only the first.
void GpuOffloadVisitor::report_not_offloaded(const Location &where,
        const GpuDecline &decline) {
    declined = true;
    report_gpu_decline(pass_options, where, decline);
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
    ParallelLoopNest nest;
    if (!offloadable_loop_nest(region, nest)) return;

    Location loc = region.base.base.loc;
    size_t n_dims = nest.n_heads();

    // Everything below rewrites the loop as it goes -- inlining an
    // intrinsic, splicing a callee, gathering an argument -- and several
    // of those rewrites change a statement, an expression or a BLOCK local
    // in place. The kernel is built from its own copy of the nest, BLOCKs
    // and ASSOCIATEs included, so that no rewrite reaches a node the loop
    // shares with the rest of the program.
    ParallelLoopNest work;
    kernel_blocks.clear();
    {
        ASRUtils::ExprStmtDuplicator dup(al);
        dup.allow_procedure_calls = true;
        dup.allow_reshape = true;
        ASR::stmt_t *loop_copy = copy_loop_stmt(region.m_body[0], dup);
        if (loop_copy == nullptr ||
                !parallel_loop_nest_of(loop_copy,
                    parallel_collapse_count(region), work)) {
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::LoopNestNotCopyable));
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
    resolve_enclosing_associates(work, n_dims, enclosing_assoc_map);
    Vec<ASR::stmt_t*> private_shapes;
    private_shapes.reserve(al, 4);
    if (!privatize_iteration_arrays(region, work,
            enclosing_assoc_map, private_shapes)) {
        return;
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
    // the shape the kernel would really be built from.
    Vec<ASR::stmt_t*> gather_stmts;
    gather_stmts.reserve(al, 1);
    Vec<ASR::stmt_t*> scatter_stmts;
    scatter_stmts.reserve(al, 1);
    // The gather is a copy the host makes before it launches, so this
    // holds for every dialect.
    if (!hoist_struct_element_gathers(work, gather_stmts,
            scatter_stmts)) {
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
    gpu_hoist_present_tests(al, current_scope, work.body, work.n_body,
        gather_stmts);

    // What can be asked of the loop as it was written is asked before the
    // inline_* helpers below rewrite the body, so that an error names what
    // the source wrote rather than the shape a rewrite left.
    if (!offloadable_before_rewrites(work, enclosing_block_scopes, loc)) {
        return;
    }

    // Splice the planned device functions into the loop body. This
    // must come first among the rewrites below: the intrinsic and
    // array-section inliners then see the spliced-in statements too.
    {
        if (!inline_device_function_calls(work.body, work.n_body)) {
            functions_to_inline.clear();
            report_not_offloaded(loc,
                GpuDecline(GpuDeclineReason::DeviceFunctionInlining));
            return;
        }
        functions_to_inline.clear();
        // Run-time sized alias temporaries become BLOCK locals here,
        // ahead of the workspace pre-flight below, so that the
        // pre-flight sizes them too.
        {
            std::vector<std::string> alias_arg_names;
            collect_kernel_arg_names(work, enclosing_block_scopes,
                alias_arg_names);
            materialize_runtime_alias_blocks(work, alias_arg_names);
        }
        size_scope_array_temporaries(work.body, work.n_body);
    }

    // A spliced callee can bring a section into a place no gather can
    // serve, such as its own do while condition, which the check before
    // the rewrites did not see.
    {
        Location where = loc;
        std::string name;
        GpuSectionPlace place;
        if (body_has_unplaceable_section(work.body, work.n_body, where,
                name, place)) {
            report_not_offloaded(where, GpuDecline(
                GpuDeclineReason::SectionCopyNotPlaceable, name, place));
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

    rewrite_loop_body_for_kernel(work, enclosing_assoc_map);
    size_t fastest_dimension = store_dimension(work, 0, 0);
    // The dimension the stores step through next in memory, if they agree
    // on one: a tiled launch (below) runs the height of its tiles along it.
    size_t next_store_dimension = store_dimension(work, 1,
        fastest_dimension);

    // 1. Collect all symbols from body AND head expressions
    std::map<std::string, std::pair<ASR::ttype_t*, ASR::expr_t*>> involved_syms;
    collect_involved_syms(work, enclosing_block_scopes, involved_syms);

    if (!offloadable_after_rewrites(work, involved_syms, loc)) {
        return;
    }

    // Collect loop variable names
    std::vector<std::string> loop_var_names;
    for (size_t d = 0; d < n_dims; d++) {
        ASR::Var_t *lv = down_cast<ASR::Var_t>(work.head(d).m_v);
        loop_var_names.push_back(ASRUtils::symbol_name(lv->m_v));
    }

    // Find local scalar temporaries (assigned but not arrays, not loop vars)
    std::set<std::string> assigned_vars, inner_loop_vars;
    GpuLocalVarCollector lv_collector(assigned_vars, inner_loop_vars, enclosing_block_scopes);
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

    // A `reduce` accumulator is live after the loop by definition, whether
    // or not anything the scan above found reads it: an intent(out) dummy
    // at the end of a procedure, or a loop nested in another construct,
    // leaves no later statement in the body it scans. It is never a
    // kernel local.
    std::set<std::string> reduction_names;
    for (auto &r : pending_reductions) reduction_names.insert(r.orig_name);

    std::set<std::string> local_scalar_names;
    for (auto &name : assigned_vars) {
        if (loop_var_set.count(name)) continue;
        if (all_reduction_targets.count(name)) continue;
        if (reduction_names.count(name)) continue;
        if (post_loop_vars.count(name) && !inner_loop_vars.count(name)) continue;
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
    std::vector<LiveoutScalarInfo> liveout_scalars;
    {
        ASR::ttype_t *int4_type = ASRUtils::TYPE(
            ASR::make_Integer_t(al, loc, 4));
        std::vector<std::string> liveout_names;
        for (auto &name : assigned_vars) {
            if (loop_var_set.count(name)) continue;
            if (local_scalar_names.count(name)) continue;
            // A reduction accumulator is written by every thread. One
            // shared slot would be a race, so it gets a slot per thread
            // below instead of the single liveout buffer.
            if (reduction_names.count(name)) continue;
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

        // One accumulator per iteration. The extent is only known when the
        // launch has worked out how many threads it is dispatching, so the
        // host array is deferred-shape and the launch allocates it.
        for (auto &r : pending_reductions) {
            auto it = involved_syms.find(r.orig_name);
            if (it == involved_syms.end()) continue;
            ASR::dimension_t dim;
            dim.loc = loc;
            dim.m_start = nullptr;
            dim.m_length = nullptr;
            Vec<ASR::dimension_t> dims_vec;
            dims_vec.reserve(al, 1);
            dims_vec.push_back(al, dim);
            ASR::ttype_t *arr_type = ASRUtils::TYPE(
                ASR::make_Array_t(al, loc,
                    ASRUtils::duplicate_type(al, r.scalar_type),
                    dims_vec.p, 1,
                    ASR::array_physical_typeType::DescriptorArray,
                    ASR::memory_spaceType::Global));
            ASR::ttype_t *alloc_type = ASRUtils::TYPE(
                ASR::make_Allocatable_t(al, loc, arr_type));
            r.buf_name = current_scope->get_unique_name(
                "__gpu_red_" + r.orig_name);
            r.host_buf_sym = gpu_new_variable(al, loc, current_scope,
                r.buf_name, ASRUtils::duplicate_type(al, alloc_type));
            // The kernel takes the accumulators, not the scalar. The host
            // side is allocatable because the launch sizes it; the kernel
            // side is a plain array dummy, which is what an allocatable
            // actual is passed to anywhere else.
            it->second.first = arr_type;
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
        if (alloc_collector.has_non_alloc_access.count(struct_name))
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
        // A reduction accumulator reaches the kernel as its per-thread
        // buffer, so that is the actual whose shape is measured and
        // passed; the scalar the source named has none.
        for (auto &r : pending_reductions) {
            if (r.host_buf_sym && r.orig_name == sym_name) {
                orig_sym = r.host_buf_sym;
                break;
            }
        }
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
        for (auto &r : pending_reductions) {
            if (r.host_buf_sym && r.orig_name == sym_name) {
                carg.m_value = ASRUtils::EXPR(
                    ASR::make_Var_t(al, loc, r.host_buf_sym));
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
        // A reduction accumulator reaches the kernel as its per-thread
        // buffer, so that is the actual whose shape is measured and
        // passed; the scalar the source named has none.
        for (auto &r : pending_reductions) {
            if (r.host_buf_sym && r.orig_name == sym_name) {
                orig_sym = r.host_buf_sym;
                break;
            }
        }
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

    // The allocatable array components of a struct-typed kernel parameter
    // reach the device as kernel parameters of their own (see
    // gpu_struct_member_arguments): first the extents and lower bounds of
    // every parameter's components, then their data. Skip array-of-structs
    // variables -- StructInstanceMember requires a scalar struct base, not
    // an array of structs.
    std::vector<std::pair<std::string, ASR::Variable_t*>> struct_params;
    for (auto &[sym_name, sym_info] : involved_syms) {
        ASR::ttype_t *type = sym_info.first;
        ASR::ttype_t *inner_t = ASRUtils::type_get_past_allocatable(type);
        if (ASRUtils::is_array(inner_t)) continue;
        if (!ASR::is_a<ASR::StructType_t>(
                *ASRUtils::extract_type(inner_t)))
            continue;
        ASR::symbol_t *orig_sym = orig_scope->resolve_symbol(sym_name);
        // A reduction accumulator reaches the kernel as its per-thread
        // buffer, so that is the actual whose shape is measured and
        // passed; the scalar the source named has none.
        for (auto &r : pending_reductions) {
            if (r.host_buf_sym && r.orig_name == sym_name) {
                orig_sym = r.host_buf_sym;
                break;
            }
        }
        if (!orig_sym || !is_a<ASR::Variable_t>(*orig_sym)) continue;
        ASR::Variable_t *orig_var =
            down_cast<ASR::Variable_t>(orig_sym);
        if (!orig_var->m_type_declaration) continue;
        ASR::symbol_t *struct_sym =
            ASRUtils::symbol_get_past_external(
                orig_var->m_type_declaration);
        if (!is_a<ASR::Struct_t>(*struct_sym)) continue;
        struct_params.push_back({sym_name, orig_var});
    }
    // The host value of a component of `orig_var`, through the external
    // symbol the scope of the loop has for the component when it has one.
    auto host_member = [&](ASR::Variable_t *orig_var, ASR::symbol_t *mem_sym) {
        ASR::symbol_t *orig_mem_ref = nullptr;
        for (auto &scope_item : orig_scope->get_scope()) {
            if (!is_a<ASR::ExternalSymbol_t>(*scope_item.second)) continue;
            ASR::ExternalSymbol_t *es =
                down_cast<ASR::ExternalSymbol_t>(scope_item.second);
            if (ASRUtils::symbol_get_past_external(es->m_external)
                    == mem_sym) {
                orig_mem_ref = scope_item.second;
                break;
            }
        }
        if (!orig_mem_ref) orig_mem_ref = mem_sym;
        return ASRUtils::EXPR(ASR::make_StructInstanceMember_t(al, loc,
            ASRUtils::EXPR(ASR::make_Var_t(al, loc, &orig_var->base)),
            orig_mem_ref, ASRUtils::symbol_type(mem_sym), nullptr));
    };
    for (auto &[sym_name, orig_var] : struct_params) {
        ASR::Variable_t *v = orig_var;
        gpu_struct_member_extent_arguments(al, loc, kernel_scope, sym_name,
            gpu_struct_definition(orig_var->m_type_declaration),
            [&](ASR::symbol_t *m) { return host_member(v, m); },
            kernel_args, call_args);
    }
    for (auto &[sym_name, orig_var] : struct_params) {
        ASR::Variable_t *v = orig_var;
        gpu_struct_member_data_arguments(al, loc, kernel_scope, sym_name,
            gpu_struct_definition(orig_var->m_type_declaration),
            [&](ASR::symbol_t *m) { return host_member(v, m); },
            [&](ASR::Struct_t *element) {
                return import_struct_def(element, orig_scope, kernel_scope,
                    loc);
            },
            kernel_args, call_args);
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
                        // A function the declaration of a local calls,
                        // `real :: w(f(n))`, is called on entry.
                        std::vector<SymbolTable*> scopes;
                        gpu_routine_scopes(fn_impl->m_symtab, scopes);
                        for (SymbolTable *scope : scopes) {
                            for (auto &local : scope->get_scope()) {
                                if (ASR::is_a<ASR::Variable_t>(
                                        *local.second)) {
                                    transitive_collector.visit_symbol(
                                        *local.second);
                                }
                            }
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
                // A separate module procedure a routine of its module
                // calls is named by its interface, whose body is in a
                // submodule.
                ASR::symbol_t *dup = sym_dup.duplicate_Function(
                    resolve_function_implementation(
                        ASR::down_cast<ASR::Function_t>(resolved)),
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

    // The kernel's copies of procedures and every procedure they contain,
    // contained ones first.
    std::vector<ASR::Function_t*> kernel_funcs;
    std::function<void(SymbolTable*)> collect_funcs =
            [&](SymbolTable *scope) {
        for (auto &item : scope->get_scope()) {
            if (!ASR::is_a<ASR::Function_t>(*item.second)) continue;
            ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(
                item.second);
            collect_funcs(f->m_symtab);
            kernel_funcs.push_back(f);
        }
    };
    collect_funcs(kernel_scope);

    // A procedure of a module read from its module file is an external
    // declaration on the host (ExternalUndefined), which the passes need
    // not prepare for code generation: they drop the values of its
    // variables, the named constants included. Its copy in the kernel,
    // and every procedure the copy contains, is the definition the
    // device code is generated from.
    for (ASR::Function_t *f : kernel_funcs) {
        ASR::FunctionType_t *ftype = ASRUtils::get_FunctionType(f);
        if (ftype->m_abi == ASR::abiType::ExternalUndefined
                && ftype->m_deftype == ASR::deftypeType::Implementation) {
            ftype->m_abi = ASR::abiType::Source;
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
        for (ASR::Function_t *func : kernel_funcs) {
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
                    // Its value, rather than the expression it was
                    // declared with, which names other constants of the
                    // original scope.
                    ASR::symbol_t *new_var =
                        ASR::down_cast<ASR::symbol_t>(
                            ASRUtils::make_Variable_t_util(al, loc,
                                func->m_symtab, s2c(al, name),
                                nullptr, 0,
                                ASR::intentType::Local,
                                orig_var->m_value ? orig_var->m_value
                                    : orig_var->m_symbolic_value,
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
            retarget_declaration_calls(func->m_symtab, *kernel_scope);
            // `sum(f(x))` in a routine becomes a loop, as in the loop body,
            // so that it needs no temporary for the array `f` returns. The
            // routine is the kernel's own copy: the host's is left as it is.
            inline_sum_in_stmts(func->m_body, func->n_body, func->m_symtab);
            gather_strided_sections_in_routine(*func);
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

    for (auto &item : kernel_scope->get_scope()) {
        if (ASR::is_a<ASR::Function_t>(*item.second)) {
            gpu_alias_scalar_associate_names(al,
                *ASR::down_cast<ASR::Function_t>(item.second));
        }
    }

    // Which allocatable dummies a routine hands on to another routine is
    // read from the calls, so they have to name the kernel's copies first.
    for (auto &item : kernel_scope->get_scope()) {
        if (ASR::is_a<ASR::Function_t>(*item.second)) {
            plain_allocatable_dummies(
                ASR::down_cast<ASR::Function_t>(item.second));
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

    ASR::ttype_t *loop_int_type = ASRUtils::expr_type(work.head(0).m_v);

    // Save host-side head expressions BEFORE in-place replacement
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

    // The accumulator slot this thread owns. Created here because the
    // substitution below has to run before the body is remapped into
    // kernel scope: the name being replaced is still the host's scalar,
    // and what replaces it already names the kernel's buffer, so the
    // replacement contains nothing that would be replaced again.
    ASR::expr_t *slot_index = nullptr;
    if (!pending_reductions.empty()) {
        ASR::ttype_t *slot_type = ASRUtils::duplicate_type(al, loop_int_type);
        gpu_new_variable(al, loc, kernel_scope, "__gpu_slot",
            ASRUtils::duplicate_type(al, slot_type));
        slot_index = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
            ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                kernel_scope->get_symbol("__gpu_slot"))),
            ASR::binopType::Add,
            ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 1,
                ASRUtils::duplicate_type(al, slot_type),
                ASR::integerbozType::Decimal)),
            ASRUtils::duplicate_type(al, slot_type), nullptr));
        std::map<ASR::symbol_t*, ASR::expr_t*> slot_map;
        for (auto &r : pending_reductions) {
            if (!r.host_buf_sym) continue;
            ASR::symbol_t *param = kernel_scope->get_symbol(r.orig_name);
            if (!param) continue;
            slot_map[r.orig_scalar_sym] = gpu_reduction_slot(al, loc,
                param, slot_index, r.scalar_type);
        }
        if (!slot_map.empty()) {
            AssociateVarResolverVisitor slot_replacer(al, slot_map);
            for (size_t i = 0; i < body_copy.n; i++) {
                slot_replacer.visit_stmt(*body_copy.p[i]);
            }
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

    ASR::ttype_t *int_type = loop_int_type;

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

    auto int_binop = [&](ASR::expr_t *left, ASR::binopType op,
            ASR::expr_t *right) {
        return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
            left, op, right, int_type, nullptr));
    };
    // ASR has no integer remainder operator: a - (a/b)*b.
    auto int_mod = [&](ASR::expr_t *left, ASR::expr_t *right) {
        return int_binop(left, ASR::binopType::Sub, int_binop(
            int_binop(left, ASR::binopType::Div, right),
            ASR::binopType::Mul, right));
    };
    auto assign = [&](ASR::expr_t *target, ASR::expr_t *value) {
        kernel_body.push_back(al, ASRUtils::STMT(ASR::make_Assignment_t(al,
            loc, target, value, nullptr, false, false, false)));
    };
    auto return_if = [&](ASR::expr_t *test) {
        Vec<ASR::stmt_t*> exit_body;
        exit_body.reserve(al, 1);
        exit_body.push_back(al, ASRUtils::STMT(ASR::make_Return_t(al, loc)));
        Vec<ASR::stmt_t*> no_else;
        no_else.reserve(al, 0);
        kernel_body.push_back(al, ASRUtils::STMT(ASR::make_If_t(al, loc,
            nullptr, test, exit_body.p, exit_body.n, no_else.p, no_else.n)));
    };
    auto kernel_local = [&](const std::string &name) {
        return ASRUtils::EXPR(ASR::make_Var_t(al, loc, gpu_new_variable(al,
            loc, kernel_scope, name, ASRUtils::duplicate_type(al,
                int_type))));
    };
    // The number of iterations of dimension d, in a launch that has some.
    auto kernel_range = [&](size_t d) {
        return int_binop(int_binop(kernel_ends[d], ASR::binopType::Sub,
            kernel_starts[d]), ASR::binopType::Add, one_const);
    };
    auto loop_var = [&](size_t d) {
        return ASRUtils::EXPR(ASR::make_Var_t(al, loc,
            kernel_scope->get_symbol(loop_var_names[d])));
    };

    // A nest of two or more dimensions can be launched in tiles where the
    // device asks for them (GpuDeviceCapabilities::tile_width): a block
    // covers tile_width iterations of the fastest dimension by the rest of
    // its threads along a second one, the dimension the stores step through
    // next in memory (or else the next one in loop order), and the block
    // index numbers the tiles. The launch runs whole tiles, and a thread of
    // a partial tile that falls outside the nest returns. Where whole tiles
    // would pad the nest by more than a quarter, as a short dimension
    // does, the launch passes __tiled = 0 instead and the kernel numbers
    // the iterations flat, one thread each (gpu_offload_launch.cpp). Tile
    // widths and block sizes are powers of two, so a thread finds its place
    // in a tile with constant shifts and masks, which a device compiler
    // folds, and one division of the block index finds the tile. A
    // reduction keeps the flat numbering, which gives every thread an
    // accumulator slot.
    auto is_power_of_two = [](int64_t value) {
        return value > 0 && (value & (value - 1)) == 0;
    };
    bool tiled = device_caps.tile_width > 0 && n_dims >= 2
        && slot_index == nullptr
        && is_power_of_two(device_caps.tile_width)
        && is_power_of_two(device_caps.threads_per_block)
        && device_caps.threads_per_block % device_caps.tile_width == 0;
    ASR::expr_t *tiled_flag = nullptr;
    int tiled_arg = -1;
    if (tiled) {
        ASR::symbol_t *param = gpu_new_variable(al, loc, kernel_scope,
            "__tiled", ASRUtils::duplicate_type(al, int_type),
            ASR::intentType::InOut);
        tiled_flag = ASRUtils::EXPR(ASR::make_Var_t(al, loc, param));
        kernel_args.push_back(al, tiled_flag);
        // The launch replaces this with the numbering it picks.
        ASR::call_arg_t carg;
        carg.loc = loc;
        carg.m_value = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
            0, int_type, ASR::integerbozType::Decimal));
        call_args.push_back(al, carg);
        tiled_arg = static_cast<int>(call_args.n) - 1;
    }
    // The dimensions in the order the iterations are numbered along: the
    // fastest first, then the one a tile's height runs along, then the
    // others in the order the loop names them.
    std::vector<size_t> order;
    order.push_back(fastest_dimension);
    if (n_dims >= 2) {
        order.push_back(tiled && next_store_dimension != fastest_dimension
            ? next_store_dimension : (fastest_dimension == 0 ? 1 : 0));
    }
    for (size_t d = 0; d < n_dims; d++) {
        if (std::find(order.begin(), order.end(), d) == order.end()) {
            order.push_back(d);
        }
    }

    // Every thread past the number of iterations returns, so the per-dim
    // ranges below are only computed when none is empty.
    auto return_past_iterations = [&]() {
        ASR::expr_t *total_size_kernel = nullptr;
        for (size_t d = 0; d < n_dims; d++) {
            // dim_range = max(0, kernel_end - kernel_start + 1)
            ASR::expr_t *dim_range = gpu_loop_extent(al, loc,
                kernel_starts[d], kernel_ends[d], int_type);
            total_size_kernel = total_size_kernel == nullptr ? dim_range
                : int_binop(total_size_kernel, ASR::binopType::Mul,
                    dim_range);
        }

        // Guard: if (flat_idx >= total_size) return
        return_if(ASRUtils::EXPR(ASR::make_IntegerCompare_t(al, loc,
            flat_idx, ASR::cmpopType::GtE, total_size_kernel,
            ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4)), nullptr)));
    };

    // Compute per-dim loop variable from flat_idx
    // We need a "remaining" variable in kernel scope
    ASR::expr_t *remain_var = kernel_local("__flat_idx");
    // The loop indices at positions [first, last) of the order, recovered
    // by dividing __flat_idx down.
    auto number_flat = [&](size_t first, size_t last) {
        for (size_t position = first; position < last; position++) {
            size_t d = order[position];
            if (position < n_dims - 1) {
                // loop_var = __flat_idx % dim_range + start
                assign(loop_var(d), int_binop(int_mod(remain_var,
                    kernel_range(d)), ASR::binopType::Add,
                    kernel_starts[d]));
                // __flat_idx = __flat_idx / dim_range
                assign(remain_var, int_binop(remain_var,
                    ASR::binopType::Div, kernel_range(d)));
            } else {
                // Last dim: loop_var = __flat_idx + start
                assign(loop_var(d), int_binop(remain_var,
                    ASR::binopType::Add, kernel_starts[d]));
            }
        }
    };

    size_t first_flat_position = 0;
    if (tiled) {
        // __tiled = 0: one thread per iteration, numbered flat.
        Vec<ASR::stmt_t*> enclosing_body = kernel_body;
        kernel_body.reserve(al, 8);
        return_past_iterations();
        assign(remain_var, flat_idx);
        number_flat(0, 2);
        Vec<ASR::stmt_t*> flat_body = kernel_body;
        kernel_body.reserve(al, 16);
        // The tile, then the thread's place in it: its width runs along the
        // fastest dimension, its height along the second one.
        size_t across = order[0], down = order[1];
        int64_t width_shift = 0, block_shift = 0;
        while ((int64_t(1) << width_shift) < device_caps.tile_width) {
            width_shift++;
        }
        while ((int64_t(1) << block_shift) < device_caps.threads_per_block) {
            block_shift++;
        }
        int64_t height_shift = block_shift - width_shift;
        auto constant = [&](int64_t value) {
            return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, value,
                int_type, ASR::integerbozType::Decimal));
        };
        // ceil(range / 2**shift)
        auto tiles_of = [&](ASR::expr_t *range, int64_t shift) {
            return int_binop(int_binop(range, ASR::binopType::Add,
                constant((int64_t(1) << shift) - 1)), ASR::binopType::BitRShift,
                constant(shift));
        };
        ASR::expr_t *tiles = kernel_local("__tiles");
        assign(remain_var, block_idx);
        assign(tiles, tiles_of(kernel_range(across), width_shift));
        assign(loop_var(across), int_binop(
            int_binop(int_mod(remain_var, tiles), ASR::binopType::BitLShift,
                constant(width_shift)),
            ASR::binopType::Add, int_binop(thread_idx, ASR::binopType::BitAnd,
                constant(device_caps.tile_width - 1))));
        assign(remain_var, int_binop(remain_var, ASR::binopType::Div, tiles));
        ASR::expr_t *row_tile = remain_var;
        if (n_dims > 2) {
            // The tiles of the first two dimensions repeat over the others.
            assign(tiles, tiles_of(kernel_range(down), height_shift));
            row_tile = int_mod(remain_var, tiles);
        }
        assign(loop_var(down), int_binop(
            int_binop(row_tile, ASR::binopType::BitLShift,
                constant(height_shift)),
            ASR::binopType::Add, int_binop(thread_idx,
                ASR::binopType::BitRShift, constant(width_shift))));
        if (n_dims > 2) {
            assign(remain_var, int_binop(remain_var, ASR::binopType::Div,
                tiles));
        }
        // Reject padded ordinals before adding the lower bounds: a padded
        // source index could overflow even when every valid index fits.
        for (size_t d : {across, down}) {
            return_if(ASRUtils::EXPR(ASR::make_IntegerCompare_t(al, loc,
                loop_var(d), ASR::cmpopType::GtE, kernel_range(d),
                ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4)), nullptr)));
        }
        for (size_t d : {across, down}) {
            assign(loop_var(d), int_binop(loop_var(d), ASR::binopType::Add,
                kernel_starts[d]));
        }
        Vec<ASR::stmt_t*> tiled_body = kernel_body;
        kernel_body = enclosing_body;
        kernel_body.push_back(al, ASRUtils::STMT(ASR::make_If_t(al, loc,
            nullptr, ASRUtils::EXPR(ASR::make_IntegerCompare_t(al, loc,
                tiled_flag, ASR::cmpopType::Eq, constant(0),
                ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4)), nullptr)),
            flat_body.p, flat_body.n, tiled_body.p, tiled_body.n)));
        first_flat_position = 2;
    } else {
        return_past_iterations();
        // __flat_idx = flat_idx (the raw thread index)
        assign(remain_var, flat_idx);
    }

    // The loop indices below are recovered by dividing __flat_idx down,
    // which destroys it, so the thread keeps its own number separately.
    // Every slot starts at the identity, so a thread whose body never
    // reaches the accumulator still leaves something the fold can use.
    if (slot_index != nullptr) {
        assign(ASRUtils::EXPR(ASR::make_Var_t(al, loc,
            kernel_scope->get_symbol("__gpu_slot"))), remain_var);
        for (auto &r : pending_reductions) {
            if (!r.host_buf_sym) continue;
            ASR::symbol_t *param = kernel_scope->get_symbol(r.orig_name);
            if (!param) continue;
            assign(gpu_reduction_slot(al, loc, param, slot_index,
                    r.scalar_type),
                gpu_reduction_identity(al, loc, r.op, r.scalar_type));
        }
    }

    number_flat(first_flat_position, n_dims);

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
        // Recursively process nested BlockCall statements, including
        // those inside a loop or an IF of the block.
        std::function<void(ASR::stmt_t**, size_t)> process_nested =
            [&](ASR::stmt_t **stmts, size_t n_stmts) {
            for (size_t j = 0; j < n_stmts; j++) {
                ASR::stmt_t *s = stmts[j];
                if (ASR::is_a<ASR::DoLoop_t>(*s)) {
                    ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(s);
                    process_nested(dl->m_body, dl->n_body);
                } else if (ASR::is_a<ASR::DoConcurrentLoop_t>(*s)) {
                    ASR::DoConcurrentLoop_t *dc =
                        ASR::down_cast<ASR::DoConcurrentLoop_t>(s);
                    process_nested(dc->m_body, dc->n_body);
                } else if (ASR::is_a<ASR::If_t>(*s)) {
                    ASR::If_t *ifs = ASR::down_cast<ASR::If_t>(s);
                    process_nested(ifs->m_body, ifs->n_body);
                    process_nested(ifs->m_orelse, ifs->n_orelse);
                } else if (ASR::is_a<ASR::WhileLoop_t>(*s)) {
                    ASR::WhileLoop_t *wl = ASR::down_cast<ASR::WhileLoop_t>(s);
                    process_nested(wl->m_body, wl->n_body);
                } else if (ASR::is_a<ASR::Select_t>(*s)) {
                    gpu_for_each_case_body(ASR::down_cast<ASR::Select_t>(s),
                        process_nested);
                }
                if (!ASR::is_a<ASR::BlockCall_t>(*s)) continue;
                ASR::BlockCall_t *inner_bc =
                    ASR::down_cast<ASR::BlockCall_t>(s);
                if (!ASR::is_a<ASR::Block_t>(*inner_bc->m_m)) continue;
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
        };
        process_nested(block->m_body, block->n_body);
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
            } else if (ASR::is_a<ASR::DoConcurrentLoop_t>(*stmts[i])) {
                ASR::DoConcurrentLoop_t *dc =
                    ASR::down_cast<ASR::DoConcurrentLoop_t>(stmts[i]);
                move_blocks_to_kernel(dc->m_body, dc->n_body);
            } else if (ASR::is_a<ASR::If_t>(*stmts[i])) {
                ASR::If_t *ifs =
                    ASR::down_cast<ASR::If_t>(stmts[i]);
                move_blocks_to_kernel(ifs->m_body, ifs->n_body);
                move_blocks_to_kernel(ifs->m_orelse, ifs->n_orelse);
            } else if (ASR::is_a<ASR::WhileLoop_t>(*stmts[i])) {
                ASR::WhileLoop_t *wl =
                    ASR::down_cast<ASR::WhileLoop_t>(stmts[i]);
                move_blocks_to_kernel(wl->m_body, wl->n_body);
            } else if (ASR::is_a<ASR::Select_t>(*stmts[i])) {
                gpu_for_each_case_body(
                    ASR::down_cast<ASR::Select_t>(stmts[i]),
                    move_blocks_to_kernel);
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
        for (auto &pair : member_extent_args) {
            kernel_args.push_back(al,
                ASRUtils::EXPR(ASR::make_Var_t(al, loc, pair.first)));
            ASR::call_arg_t carg;
            carg.loc = loc;
            carg.m_value = pair.second;
            call_args.push_back(al, carg);
        }
    }

    // A thread runs one iteration of the loop, so a cycle of the loop ends
    // the thread: it returns from the kernel, whose body comes last in it.
    {
        GpuOffloadedLoopLeaves leaves(&al);
        leaves.transform_stmts(body_copy.p, body_copy.n);
        LCOMPILERS_ASSERT(leaves.reason == GpuDeclineReason::None);
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
        nullptr, nullptr, nullptr, nullptr);
    gpu_lift_host_association(al,
        *ASR::down_cast2<ASR::Function_t>(kernel_func));

    GpuLaunchPlan plan;
    plan.kernel_func = kernel_func;
    plan.kernel_name = kernel_name;
    plan.tu_symtab = tu_symtab;
    plan.int_type = int_type;
    plan.n_dims = n_dims;
    plan.call_args = call_args;
    plan.private_shapes = private_shapes;
    plan.gather_stmts = gather_stmts;
    plan.scatter_stmts = scatter_stmts;
    plan.liveout_scalars = liveout_scalars;
    plan.reductions = pending_reductions;
    plan.dim_info = dim_info;
    plan.optional_syms = optional_syms;
    plan.tiled_arg = tiled_arg;
    plan.tile_dims[0] = tiled ? order[0] : 0;
    plan.tile_dims[1] = tiled ? order[1] : 0;
    build_kernel_launch(region, work, loc, plan);
}

// Every loop assigned to the device is either offloaded or an error, so a
// loop still assigned to it once the pass is done is one the pass missed. It
// is reported rather than left to the passes below, which would run it on
// the CPU.
class UnloweredDeviceLoopReporter :
        public ASR::BaseWalkVisitor<UnloweredDeviceLoopReporter> {
public:
    const PassOptions &pass_options;

    explicit UnloweredDeviceLoopReporter(const PassOptions &pass_options_) :
        pass_options(pass_options_) {}

    void visit_OMPRegion(const ASR::OMPRegion_t &x) {
        if (x.m_exec_target == ASR::exec_targetType::ExecDevice) {
            report_gpu_decline(pass_options, x.base.base.loc,
                GpuDecline(GpuDeclineReason::LoopNotLowered));
            return;
        }
        ASR::BaseWalkVisitor<UnloweredDeviceLoopReporter>::visit_OMPRegion(x);
    }
};

// A parallel loop inside a kernel is run serially by the thread that runs
// the kernel, so once no more kernels are being built it is written as the
// loop it is. Every analysis of device code below -- the memory space of a
// local array, the workspaces the host sizes, the kernel source itself --
// walks loops, and a loop still wrapped in a region would hide what it
// allocates from all of them.
class KernelSerialRegionFlattener :
        public ASR::StatementWalkVisitor<KernelSerialRegionFlattener> {
public:
    explicit KernelSerialRegionFlattener(Allocator &al) :
        StatementWalkVisitor(al) {}

    void visit_OMPRegion(const ASR::OMPRegion_t &x) {
        ASR::OMPRegion_t &xx = const_cast<ASR::OMPRegion_t&>(x);
        transform_stmts(xx.m_body, xx.n_body);
        if (xx.m_exec_target != ASR::exec_targetType::ExecSerial) return;
        remove_original_stmt = true;
        pass_result.reserve(al, xx.n_body);
        for (size_t i = 0; i < xx.n_body; i++) {
            pass_result.push_back(al, xx.m_body[i]);
        }
    }

    void flatten(ASR::TranslationUnit_t &unit) {
        for (auto &item : unit.m_symtab->get_scope()) {
            if (!ASRUtils::is_device_kernel(item.second)) continue;
            visit_Function(*ASR::down_cast<ASR::Function_t>(item.second));
        }
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
        // A loop this pass cannot lower is an error, and the compilation
        // stops after the pass. What was drafted for that loop is not
        // taken back, so another round could walk into it.
        if (v.declined) return;
    }
    // The last round built no kernel, so every region a kernel holds was
    // marked serial at the start of it.
    KernelSerialRegionFlattener kernels(al);
    kernels.flatten(unit);
    UnloweredDeviceLoopReporter unlowered(pass_options);
    unlowered.visit_TranslationUnit(unit);
    // Kernel extraction moves Block symbols out of their enclosing
    // function, which can leave stale entries in that function's
    // dependency list. Recompute all dependencies to fix this.
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

} // namespace LCompilers
