#include <functional>
#include <map>
#include <string>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/gpu_offload_collect.h>
#include <libasr/pass/gpu_offload_rewrite.h>
#include <libasr/pass/gpu_offload_visitor.h>
#include <libasr/pass/parallel_canonicalize.h>
#include <libasr/pass/symbol_expr_substitution.h>

namespace LCompilers {

// Lowers the copied loop body into the shape the kernel extraction below
// understands: array intrinsics and array-section assignments become
// explicit element loops, ASSOCIATE scopes are inlined away, and strided
// actual arguments are gathered into contiguous temporaries.
//
// Every one of these rewrites is destructive, which is why they all run
// here, after the offload decision and on the pass's own copy of the nest:
// the loop the host would run must never be left half-lowered. Nothing
// below can decline any more.
void GpuOffloadVisitor::rewrite_loop_body_for_kernel(ParallelLoopNest &work,
        const std::map<ASR::symbol_t*, ASR::expr_t*> &enclosing_assoc_map) {
    // Inline IntrinsicArrayFunction All before kernel extraction
    all_reduction_targets.clear();
    inline_intrinsic_all(work);

    // Hoist matmuls out of the expression positions the matmul
    // lowering below cannot see them in.
    hoist_nested_matmuls(work);

    // Inline IntrinsicArrayFunction MatMul before kernel extraction
    inline_intrinsic_matmul(work);

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
}

} // namespace LCompilers
