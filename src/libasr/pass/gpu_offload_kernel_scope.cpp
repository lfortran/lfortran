#include <map>
#include <set>
#include <string>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/gpu_offload_collect.h>
#include <libasr/pass/gpu_offload_designator.h>
#include <libasr/pass/gpu_offload_rewrite.h>
#include <libasr/pass/gpu_offload_undo.h>
#include <libasr/pass/gpu_offload_visitor.h>
#include <libasr/pass/parallel_canonicalize.h>

namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

// Collects every symbol the kernel would have to reference for `x`:
// the symbols appearing in the loop head and body, plus the symbols
// that only appear inside the array-dimension expressions of those
// symbols' types (e.g. `tmp(size(b))` pulls in `b`).
// The names the kernel arguments will carry, in the spelling the
// workspace extent resolver expects: every symbol the loop involves,
// plus the synthetic per-dimension extent scalar the kernel
// extraction adds for each dimension of an array argument.
void GpuOffloadVisitor::collect_kernel_arg_names(const ParallelLoopNest &nest,
        const std::set<SymbolTable*> &enclosing_block_scopes,
        std::vector<std::string> &arg_names) {
    std::map<std::string,
        std::pair<ASR::ttype_t*, ASR::expr_t*>> syms;
    collect_involved_syms(nest, enclosing_block_scopes, syms);
    // A loop index is not one of them: the kernel works it out from the
    // thread id. Counting it would tell the workspace pre-flight that
    // the host can read `n(l)`, which it cannot -- `l` only exists once
    // the kernel is running.
    std::set<std::string> indices;
    for (size_t d = 0; d < nest.n_heads(); d++) {
        if (!nest.head(d).m_v) continue;
        if (!ASR::is_a<ASR::Var_t>(*nest.head(d).m_v)) continue;
        indices.insert(ASRUtils::symbol_name(
            ASR::down_cast<ASR::Var_t>(nest.head(d).m_v)->m_v));
    }
    for (auto &sym : syms) {
        if (indices.count(sym.first)) continue;
        arg_names.push_back(sym.first);
        if (sym.second.first == nullptr) continue;
        ASR::ttype_t *type = ASRUtils::type_get_past_allocatable_pointer(
            sym.second.first);
        if (!ASR::is_a<ASR::Array_t>(*type)) continue;
        ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
        for (size_t d = 0; d < arr->n_dims; d++) {
            arg_names.push_back(GpuNames::dim_arg(sym.first, d));
        }
    }
}

void GpuOffloadVisitor::collect_involved_syms(const ParallelLoopNest &nest,
        const std::set<SymbolTable*> &enclosing_block_scopes,
        std::map<std::string,
            std::pair<ASR::ttype_t*, ASR::expr_t*>> &involved_syms) {
    GpuSymbolCollector collector(al, involved_syms,
        enclosing_block_scopes);
    // The whole nest: the loop heads name symbols too, and an index of
    // an outer level is read by the levels inside it.
    for (ASR::DoLoop_t *level : nest.loops) {
        collector.visit_stmt(*ASRUtils::STMT((ASR::asr_t*)level));
    }
    bool added = true;
    while (added) {
        added = false;
        std::map<std::string,
            std::pair<ASR::ttype_t*, ASR::expr_t*>> extra_syms;
        GpuSymbolCollector type_collector(al, extra_syms,
            enclosing_block_scopes);
        for (auto &[sym_name, sym_info] : involved_syms) {
            ASR::symbol_t *sym = current_scope->resolve_symbol(sym_name);
            if (!sym || !ASR::is_a<ASR::Variable_t>(*sym)) continue;
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
            if (!ASR::is_a<ASR::Array_t>(*var->m_type)) continue;
            ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(var->m_type);
            for (size_t d = 0; d < arr->n_dims; d++) {
                if (arr->m_dims[d].m_start)
                    type_collector.visit_expr(*arr->m_dims[d].m_start);
                if (arr->m_dims[d].m_length)
                    type_collector.visit_expr(*arr->m_dims[d].m_length);
            }
        }
        for (auto &[name, info] : extra_syms) {
            if (involved_syms.find(name) == involved_syms.end()) {
                involved_syms[name] = info;
                added = true;
            }
        }
    }
}

// An AssociateBlock nested inside a do concurrent body is normally
// inlined away by substituting each associate name with its selector
// expression. An array-valued selector, however, is materialised into
// a temporary variable owned by the AssociateBlock's own symbol table,
// and that temporary is still referenced by the inlined statements.
// It has to stay private to each loop iteration, so the scope cannot
// simply be dropped. Rebuild it as an equivalent Block over the same
// symbol table, which the kernel extraction already knows how to carry
// into the generated kernel, and return the BlockCall replacing the
// AssociateBlockCall. Returns nullptr when no symbol of the
// AssociateBlock survives, in which case the caller inlines the
// statements directly as before.
ASR::stmt_t* GpuOffloadVisitor::wrap_assoc_scope_in_block(
        ASR::AssociateBlock_t *ab,
        Vec<ASR::stmt_t*> &resolved_stmts, SymbolTable *parent_scope) {
    std::set<ASR::symbol_t*> referenced_syms;
    VarSymbolCollector collector(referenced_syms);
    for (size_t i = 0; i < resolved_stmts.n; i++) {
        collector.visit_stmt(*resolved_stmts.p[i]);
    }
    bool scope_needed = false;
    for (auto &item : ab->m_symtab->get_scope()) {
        if (ASR::is_a<ASR::Variable_t>(*item.second) &&
                referenced_syms.find(item.second)
                    != referenced_syms.end()) {
            scope_needed = true;
            break;
        }
    }
    if (!scope_needed) return nullptr;
    std::string block_name = parent_scope->get_unique_name(
        std::string(ab->m_name) + "_scope");
    ab->m_symtab->parent = parent_scope;
    ASR::asr_t *block = ASR::make_Block_t(al, ab->base.base.loc,
        ab->m_symtab, s2c(al, block_name), resolved_stmts.p,
        resolved_stmts.n);
    ab->m_symtab->asr_owner = block;
    ASR::symbol_t *block_sym = ASR::down_cast<ASR::symbol_t>(block);
    parent_scope->add_symbol(block_name, block_sym);
    if (parent_scope == current_scope) {
        kernel_blocks.push_back(block_sym);
    }
    return ASRUtils::STMT(ASR::make_BlockCall_t(al, ab->base.base.loc,
        -1, block_sym));
}

// Move the symbols of an inlined AssociateBlock that are still
// reachable from the resolved statements into the scope that now owns
// those statements. ExternalSymbol entries (e.g. type-bound procedure
// references) remain referenced by call nodes, and a Block scope may
// have been created for a nested AssociateBlock.
void GpuOffloadVisitor::migrate_inlined_assoc_symbols(
        ASR::AssociateBlock_t *ab,
        SymbolTable *parent_scope) {
    std::vector<std::pair<std::string, ASR::symbol_t*>> to_move;
    for (auto &item : ab->m_symtab->get_scope()) {
        if (ASR::is_a<ASR::ExternalSymbol_t>(*item.second) ||
                ASR::is_a<ASR::Block_t>(*item.second)) {
            to_move.push_back({item.first, item.second});
        }
    }
    for (auto &item : to_move) {
        std::string name = item.first;
        if (ASR::is_a<ASR::ExternalSymbol_t>(*item.second)) {
            if (parent_scope->get_symbol(name)) continue;
            ASR::down_cast<ASR::ExternalSymbol_t>(item.second)
                ->m_parent_symtab = parent_scope;
        } else {
            ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(
                item.second);
            if (parent_scope->get_symbol(name)) {
                name = parent_scope->get_unique_name(name);
                block->m_name = s2c(al, name);
            }
            block->m_symtab->parent = parent_scope;
        }
        parent_scope->add_symbol(name, item.second);
        if (ASR::is_a<ASR::Block_t>(*item.second)
                && parent_scope == current_scope) {
            kernel_blocks.push_back(item.second);
        }
    }
}

// Copy every loop-invariant, read-only element of an array of
// derived type that the loop reaches through a component --
// `x%c_(k)` -- into a temporary of the enclosing scope, and let the
// loop body name the temporary instead.  The copy runs once, on the
// host, before the launch; the temporary is then an ordinary
// derived-type kernel argument whose own components have extents
// the device can be told about, which is what the chain itself does
// not.
//
// An element the loop writes into is handled the same way, with a
// copy back over the original after the launch: because the gather
// brought the whole element in first, the bytes the kernel left
// alone still hold what was read, so the copy back is exact even
// when only part of the element was written.  It is only allowed
// when every write to the object lands inside that one element --
// a write anywhere else, `allocate` of the array the element comes
// from included, would be undone by the copy back.
//
// `undo` records every slot that was overwritten and `temp_names`
// every symbol that was added, so `GpuGatherGuard` can put the loop
// back exactly as it was if the offload is declined further down.
bool GpuOffloadVisitor::hoist_struct_element_gathers(
        const ParallelLoopNest &nest,
        Vec<ASR::stmt_t*> &gather_stmts,
        Vec<ASR::stmt_t*> &scatter_stmts,
        std::vector<std::pair<ASR::expr_t**, ASR::expr_t*>> &undo,
        std::vector<std::string> &temp_names) {
    GpuStructElementGatherCollector collector;
    for (size_t i = 0; i < nest.n_body; i++) {
        collector.visit_stmt(*nest.body[i]);
    }
    if (collector.found.empty()) return true;

    GpuWrittenRootCollector written;
    for (size_t i = 0; i < nest.n_body; i++) {
        written.visit_stmt(*nest.body[i]);
    }
    std::set<ASR::symbol_t*> loop_indices;
    for (size_t d = 0; d < nest.n_heads(); d++) {
        if (!nest.head(d).m_v) continue;
        if (!ASR::is_a<ASR::Var_t>(*nest.head(d).m_v)) continue;
        loop_indices.insert(ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(nest.head(d).m_v)->m_v));
    }

    std::vector<GpuStructElementGather> gathers;
    for (ASR::ArrayItem_t *item : collector.found) {
        ASR::expr_t *chain = ASRUtils::EXPR((ASR::asr_t*)item);
        GpuDesignatorBase base = gpu_designator_base(chain);
        if (!base.is_known()) return false;
        // A write to the object the chain hangs off makes a
        // read-only copy stale. The copy can still stand in for the
        // element when every such write lands inside that element,
        // because it is then put back over the original after the
        // launch. A write anywhere else in the object would be lost
        // or, worse, undone by that copy back.
        bool scatter = false;
        if (written.roots.count(base.root)) {
            // A callee that writes the object through one of its
            // dummies is spliced into the kernel, and the pass does
            // not build a kernel for that shape yet; the copy back
            // would be over storage the launch never reached.
            if (written.call_roots.count(base.root)) return false;
            for (ASR::expr_t *target : written.targets) {
                if (gpu_designator_base(target).root != base.root) {
                    continue;
                }
                if (!gpu_designator_within(target, chain)) return false;
            }
            scatter = true;
        }
        if (!host_nameable(base.root)) return false;
        // The subscripts have to mean the same thing for every
        // iteration, and mean it in the scope the copy is made in.
        bool invariant = true;
        for (size_t k = 0; k < item->n_args && invariant; k++) {
            GpuExprSymbolCollector sc;
            sc.visit_expr(*item->m_args[k].m_right);
            for (ASR::symbol_t *sym : sc.syms) {
                if (loop_indices.count(sym) || written.roots.count(sym)
                        || !host_nameable(sym)) {
                    invariant = false;
                    break;
                }
            }
        }
        if (!invariant) return false;
        bool seen = false;
        for (const GpuStructElementGather &g : gathers) {
            if (gpu_same_designator(g.chain, chain)) {
                seen = true;
                break;
            }
        }
        if (seen) continue;
        ASR::symbol_t *struct_sym =
            ASRUtils::get_struct_sym_from_struct_expr(chain);
        if (struct_sym == nullptr) return false;
        GpuStructElementGather g;
        g.chain = chain;
        g.temp = nullptr;
        g.scatter = scatter;
        gathers.push_back(g);
    }
    if (gathers.empty()) return true;

    // Two elements of the same object can designate the same
    // storage at run time -- `x%c_(i)` and `x%c_(k)` with `i == k`
    // -- and then one copy back would silently undo the other. Take
    // only a single element per written object.
    for (const GpuStructElementGather &g : gathers) {
        if (!g.scatter) continue;
        ASR::symbol_t *root = gpu_designator_base(g.chain).root;
        for (const GpuStructElementGather &other : gathers) {
            if (&other == &g) continue;
            if (gpu_designator_base(other.chain).root == root) {
                return false;
            }
        }
    }

    gather_stmts.reserve(al, gathers.size());
    scatter_stmts.reserve(al, gathers.size());
    for (GpuStructElementGather &g : gathers) {
        const Location &gloc = g.chain->base.loc;
        GpuDesignatorBase base = gpu_designator_base(g.chain);
        std::string stem = base.members.empty()
            ? std::string("elem")
            : std::string(ASRUtils::symbol_name(base.members.front()));
        std::string name = current_scope->get_unique_name(
            "__gpu_gather_" + stem);
        ASR::symbol_t *temp = gpu_new_variable(al, gloc, current_scope,
            name, ASRUtils::duplicate_type(al,
                ASRUtils::expr_type(g.chain)), ASR::intentType::Local,
            ASRUtils::get_struct_sym_from_struct_expr(g.chain));
        temp_names.push_back(name);
        g.temp = temp;
        ASRUtils::ExprStmtDuplicator dup(al);
        dup.success = true;
        ASR::expr_t *rhs = dup.duplicate_expr(g.chain);
        if (!rhs || !dup.success) rhs = g.chain;
        gather_stmts.push_back(al, ASRUtils::STMT(
            ASR::make_Assignment_t(al, gloc,
                ASRUtils::EXPR(ASR::make_Var_t(al, gloc, temp)),
                rhs, nullptr, false, false)));
        if (!g.scatter) continue;
        ASRUtils::ExprStmtDuplicator back(al);
        back.success = true;
        ASR::expr_t *lhs = back.duplicate_expr(g.chain);
        if (!lhs || !back.success) lhs = g.chain;
        scatter_stmts.push_back(al, ASRUtils::STMT(
            ASR::make_Assignment_t(al, gloc, lhs,
                ASRUtils::EXPR(ASR::make_Var_t(al, gloc, temp)),
                nullptr, false, false)));
    }

    GpuStructElementGatherVisitor sub(al, gathers, undo);
    for (size_t i = 0; i < nest.n_body; i++) {
        sub.visit_stmt(*nest.body[i]);
    }
    return true;
}

// True when `sym` is the very symbol the enclosing scope resolves its
// name to, so an expression written in terms of it can be repeated
// outside the loop.
bool GpuOffloadVisitor::host_nameable(ASR::symbol_t *sym) {
    if (sym == nullptr) return false;
    ASR::symbol_t *found = current_scope->resolve_symbol(
        ASRUtils::symbol_name(sym));
    return found != nullptr
        && ASRUtils::symbol_get_past_external(found) == sym;
}

// Copies a loop nest so that the pass can rewrite it without touching
// the loop the host would run if the offload is declined.
//
// A BLOCK or ASSOCIATE is copied along with it, including one nested
// in `if` or `while`. The kernel takes the copy and the host keeps
// its own, so no rewrite on the way to a kernel can reach the host,
// and a decline has nothing to put back.
ASR::stmt_t* GpuOffloadVisitor::copy_loop_stmt(ASR::stmt_t *stmt,
        ASRUtils::ExprStmtDuplicator &dup) {
    if (stmt == nullptr) return nullptr;
    if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
        ASR::BlockCall_t *bc = ASR::down_cast<ASR::BlockCall_t>(stmt);
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(bc->m_m);
        if (sym == nullptr || !ASR::is_a<ASR::Block_t>(*sym)) return stmt;
        ASR::Block_t *orig = ASR::down_cast<ASR::Block_t>(sym);
        ASRUtils::SymbolDuplicator sym_dup(al);
        ASR::symbol_t *copy = sym_dup.duplicate_Block(orig,
            current_scope);
        if (copy == nullptr) return nullptr;
        ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(copy);
        retarget_block_calls_in(blk);
        remap_block_to_own_scope(blk);
        std::string name = current_scope->get_unique_name(
            std::string(orig->m_name) + "_gpu");
        blk->m_name = s2c(al, name);
        current_scope->add_symbol(name, copy);
        kernel_blocks.push_back(copy);
        return ASRUtils::STMT(ASR::make_BlockCall_t(al,
            stmt->base.loc, bc->m_label, copy));
    }
    if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
        ASR::AssociateBlockCall_t *abc =
            ASR::down_cast<ASR::AssociateBlockCall_t>(stmt);
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(abc->m_m);
        if (sym == nullptr || !ASR::is_a<ASR::AssociateBlock_t>(*sym)) {
            return stmt;
        }
        ASR::AssociateBlock_t *orig =
            ASR::down_cast<ASR::AssociateBlock_t>(sym);
        ASRUtils::SymbolDuplicator sym_dup(al);
        ASR::symbol_t *copy = sym_dup.duplicate_AssociateBlock(orig,
            current_scope);
        if (copy == nullptr) return nullptr;
        ASR::AssociateBlock_t *ab =
            ASR::down_cast<ASR::AssociateBlock_t>(copy);
        remap_associate_to_own_scope(ab);
        std::string name = current_scope->get_unique_name(
            std::string(orig->m_name) + "_gpu");
        ab->m_name = s2c(al, name);
        current_scope->add_symbol(name, copy);
        kernel_blocks.push_back(copy);
        return ASRUtils::STMT(ASR::make_AssociateBlockCall_t(al,
            stmt->base.loc, copy));
    }
    if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
        // Only the nest itself is rebuilt, so that each level keeps a
        // body of its own to rewrite; the statements inside are copied
        // by the same rules.
        ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
        Vec<ASR::stmt_t*> body;
        body.reserve(al, dl->n_body);
        for (size_t i = 0; i < dl->n_body; i++) {
            ASR::stmt_t *c = copy_loop_stmt(dl->m_body[i], dup);
            if (c == nullptr) return nullptr;
            body.push_back(al, c);
        }
        return ASRUtils::STMT(ASR::make_DoLoop_t(al, dl->base.base.loc,
            dl->m_name, dl->m_head, body.p, body.n, dl->m_orelse,
            dl->n_orelse));
    }
    if (ASR::is_a<ASR::If_t>(*stmt)) {
        ASR::If_t *ifs = ASR::down_cast<ASR::If_t>(stmt);
        Vec<ASR::stmt_t*> body;
        body.reserve(al, ifs->n_body);
        for (size_t i = 0; i < ifs->n_body; i++) {
            ASR::stmt_t *c = copy_loop_stmt(ifs->m_body[i], dup);
            if (c == nullptr) return nullptr;
            body.push_back(al, c);
        }
        Vec<ASR::stmt_t*> orelse;
        orelse.reserve(al, ifs->n_orelse);
        for (size_t i = 0; i < ifs->n_orelse; i++) {
            ASR::stmt_t *c = copy_loop_stmt(ifs->m_orelse[i], dup);
            if (c == nullptr) return nullptr;
            orelse.push_back(al, c);
        }
        return ASRUtils::STMT(ASR::make_If_t(al, ifs->base.base.loc,
            ifs->m_name, ifs->m_test, body.p, body.n, orelse.p,
            orelse.n));
    }
    if (ASR::is_a<ASR::WhileLoop_t>(*stmt)) {
        ASR::WhileLoop_t *wl = ASR::down_cast<ASR::WhileLoop_t>(stmt);
        Vec<ASR::stmt_t*> body;
        body.reserve(al, wl->n_body);
        for (size_t i = 0; i < wl->n_body; i++) {
            ASR::stmt_t *c = copy_loop_stmt(wl->m_body[i], dup);
            if (c == nullptr) return nullptr;
            body.push_back(al, c);
        }
        Vec<ASR::stmt_t*> orelse;
        orelse.reserve(al, wl->n_orelse);
        for (size_t i = 0; i < wl->n_orelse; i++) {
            ASR::stmt_t *c = copy_loop_stmt(wl->m_orelse[i], dup);
            if (c == nullptr) return nullptr;
            orelse.push_back(al, c);
        }
        return ASRUtils::STMT(ASR::make_WhileLoop_t(al,
            wl->base.base.loc, wl->m_name, wl->m_test, body.p, body.n,
            orelse.p, orelse.n));
    }
    dup.success = true;
    ASR::stmt_t *c = dup.duplicate_stmt(stmt);
    if (c == nullptr || !dup.success) return nullptr;
    return c;
}

// duplicate_Block copies the symbol table but leaves Var nodes in the
// body pointing at the original Variables. Point them at the copies so
// the copied block is self-contained. Nested blocks resolve through the
// copy's parent chain: an inner use of an outer-block local must find
// the outer copy, not the original the host keeps.
// Point the extents of a scope's own array locals at that scope's
// symbols, and do the same for every scope nested in it. Duplicating a
// symbol table copies its symbols one by one, so an extent that names
// a symbol copied later is left pointing at the original.
void GpuOffloadVisitor::retarget_local_extents(SymbolTable *scope) {
    GpuReplaceSymbols type_replacer(*scope);
    for (auto &item : scope->get_scope()) {
        if (ASR::is_a<ASR::Variable_t>(*item.second)) {
            ASR::ttype_t *type = ASRUtils::type_get_past_allocatable_pointer(
                ASR::down_cast<ASR::Variable_t>(item.second)->m_type);
            if (!type || !ASR::is_a<ASR::Array_t>(*type)) continue;
            ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
            for (size_t d = 0; d < arr->n_dims; d++) {
                if (arr->m_dims[d].m_start) {
                    type_replacer.current_expr =
                        &(arr->m_dims[d].m_start);
                    type_replacer.replace_expr(arr->m_dims[d].m_start);
                }
                if (arr->m_dims[d].m_length) {
                    type_replacer.current_expr =
                        &(arr->m_dims[d].m_length);
                    type_replacer.replace_expr(arr->m_dims[d].m_length);
                }
            }
        } else if (ASR::is_a<ASR::Block_t>(*item.second)) {
            retarget_local_extents(
                ASR::down_cast<ASR::Block_t>(item.second)->m_symtab);
        } else if (ASR::is_a<ASR::AssociateBlock_t>(*item.second)) {
            retarget_local_extents(ASR::down_cast<ASR::AssociateBlock_t>(
                item.second)->m_symtab);
        }
    }
}

// duplicate_AssociateBlock copies the symbol table but leaves Var
// nodes in the body pointing at the original Variables. Point them at
// the copies so the inliner rewrites the draft, not the host
// construct the original nest still names.
void GpuOffloadVisitor::remap_associate_to_own_scope(
        ASR::AssociateBlock_t *ab) {
    GpuReplaceSymbolsVisitor body_v(*ab->m_symtab);
    body_v.replacer.resolve_through_parents = true;
    for (size_t j = 0; j < ab->n_body; j++) {
        body_v.visit_stmt(*ab->m_body[j]);
    }
    retarget_local_extents(ab->m_symtab);
    for (auto &item : ab->m_symtab->get_scope()) {
        if (ASR::is_a<ASR::Block_t>(*item.second)) {
            remap_block_to_own_scope(
                ASR::down_cast<ASR::Block_t>(item.second));
        } else if (ASR::is_a<ASR::AssociateBlock_t>(*item.second)
                && item.second != (ASR::symbol_t*)ab) {
            remap_associate_to_own_scope(
                ASR::down_cast<ASR::AssociateBlock_t>(item.second));
        }
    }
    retarget_nested_calls_in((ASR::symbol_t*)ab, ab->m_symtab,
        ab->m_body, ab->n_body);
}

void GpuOffloadVisitor::remap_block_to_own_scope(ASR::Block_t *block) {
    GpuReplaceSymbolsVisitor body_v(*block->m_symtab);
    body_v.replacer.resolve_through_parents = true;
    for (size_t j = 0; j < block->n_body; j++) {
        body_v.visit_stmt(*block->m_body[j]);
    }
    GpuReplaceSymbols type_replacer(*block->m_symtab);
    type_replacer.resolve_through_parents = true;
    for (auto &item : block->m_symtab->get_scope()) {
        if (ASR::is_a<ASR::Block_t>(*item.second)) {
            remap_block_to_own_scope(
                ASR::down_cast<ASR::Block_t>(item.second));
            continue;
        }
        if (ASR::is_a<ASR::AssociateBlock_t>(*item.second)) {
            // Through the ASSOCIATE's own table: nested calls and
            // the names it binds live there, and resolving them
            // against the block's would leave them pointing at the
            // original's.
            remap_associate_to_own_scope(
                ASR::down_cast<ASR::AssociateBlock_t>(item.second));
            continue;
        }
        if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
        ASR::Variable_t *var =
            ASR::down_cast<ASR::Variable_t>(item.second);
        if (!ASR::is_a<ASR::Array_t>(*var->m_type)) continue;
        ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(var->m_type);
        for (size_t d = 0; d < arr->n_dims; d++) {
            if (arr->m_dims[d].m_start) {
                type_replacer.current_expr = &(arr->m_dims[d].m_start);
                type_replacer.replace_expr(arr->m_dims[d].m_start);
            }
            if (arr->m_dims[d].m_length) {
                type_replacer.current_expr = &(arr->m_dims[d].m_length);
                type_replacer.replace_expr(arr->m_dims[d].m_length);
            }
        }
    }
}

// A call to a nested BLOCK or ASSOCIATE names the original; point it
// at the copy that `scope` holds.
void GpuOffloadVisitor::retarget_nested_calls_in(
        ASR::symbol_t *owner, SymbolTable *scope,
        ASR::stmt_t **body, size_t n_body) {
    std::function<void(ASR::stmt_t**, size_t)> walk =
        [&](ASR::stmt_t **stmts, size_t n_stmts) {
        for (size_t i = 0; i < n_stmts; i++) {
            if (ASR::is_a<ASR::BlockCall_t>(*stmts[i])) {
                ASR::BlockCall_t *bc =
                    ASR::down_cast<ASR::BlockCall_t>(stmts[i]);
                if (!ASR::is_a<ASR::Block_t>(*bc->m_m)) continue;
                ASR::Block_t *inner =
                    ASR::down_cast<ASR::Block_t>(bc->m_m);
                ASR::symbol_t *local =
                    scope->get_symbol(inner->m_name);
                if (local && ASR::is_a<ASR::Block_t>(*local)
                        && local != owner) {
                    bc->m_m = local;
                    retarget_block_calls_in(
                        ASR::down_cast<ASR::Block_t>(local));
                }
                continue;
            }
            if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmts[i])) {
                // An ASSOCIATE inside the construct is copied with it,
                // and the call has to name the copy for the same
                // reason a nested block's does.
                ASR::AssociateBlockCall_t *abc =
                    ASR::down_cast<ASR::AssociateBlockCall_t>(stmts[i]);
                ASR::symbol_t *inner = ASRUtils::symbol_get_past_external(
                    abc->m_m);
                if (inner == nullptr
                        || !ASR::is_a<ASR::AssociateBlock_t>(*inner)) {
                    continue;
                }
                ASR::symbol_t *local = scope->get_symbol(
                    ASRUtils::symbol_name(inner));
                if (local && ASR::is_a<ASR::AssociateBlock_t>(*local)) {
                    abc->m_m = local;
                    ASR::AssociateBlock_t *ab =
                        ASR::down_cast<ASR::AssociateBlock_t>(local);
                    retarget_nested_calls_in(local, ab->m_symtab,
                        ab->m_body, ab->n_body);
                }
                continue;
            }
            if (ASR::is_a<ASR::DoLoop_t>(*stmts[i])) {
                ASR::DoLoop_t *dl =
                    ASR::down_cast<ASR::DoLoop_t>(stmts[i]);
                walk(dl->m_body, dl->n_body);
            } else if (ASR::is_a<ASR::If_t>(*stmts[i])) {
                ASR::If_t *ifs = ASR::down_cast<ASR::If_t>(stmts[i]);
                walk(ifs->m_body, ifs->n_body);
                walk(ifs->m_orelse, ifs->n_orelse);
            } else if (ASR::is_a<ASR::WhileLoop_t>(*stmts[i])) {
                ASR::WhileLoop_t *wl =
                    ASR::down_cast<ASR::WhileLoop_t>(stmts[i]);
                walk(wl->m_body, wl->n_body);
            }
        }
    };
    walk(body, n_body);
}

void GpuOffloadVisitor::retarget_block_calls_in(ASR::Block_t *block) {
    retarget_nested_calls_in((ASR::symbol_t*)block, block->m_symtab,
        block->m_body, block->n_body);
}

} // namespace LCompilers
