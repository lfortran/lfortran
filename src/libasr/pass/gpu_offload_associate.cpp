#include <functional>
#include <map>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/gpu_offload_visitor.h>
#include <libasr/pass/parallel_canonicalize.h>
#include <libasr/pass/symbol_expr_substitution.h>

namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

// Substitutes the selector for every associate name the loop inherits from
// the ASSOCIATE scopes it sits in -- in the loop heads, in the body, and in
// the array-dimension expressions of the BLOCK locals around it -- and
// hands the caller the mapping it used, which the body rewrites below need
// again for the ASSOCIATE scopes inside the loop.
void GpuOffloadVisitor::resolve_enclosing_associates(ParallelLoopNest &work,
        size_t n_dims,
        std::map<ASR::symbol_t*, ASR::expr_t*> &enclosing_assoc_map) {
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

} // namespace LCompilers
