#include <string>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/pass/gpu_offload_collect.h>
#include <libasr/pass/gpu_offload_designator.h>
#include <libasr/pass/gpu_offload_preflight.h>
#include <libasr/pass/gpu_offload_rewrite.h>
#include <libasr/pass/gpu_offload_undo.h>
#include <libasr/pass/gpu_offload_visitor.h>

namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

// Is `e` the integer literal `value`?
bool GpuOffloadVisitor::is_int_literal(ASR::expr_t *e, int64_t value) {
    if (!e) return false;
    ASR::expr_t *v = ASRUtils::expr_value(e);
    if (!v) v = e;
    if (!ASR::is_a<ASR::IntegerConstant_t>(*v)) return false;
    return ASR::down_cast<ASR::IntegerConstant_t>(v)->m_n == value;
}

ASR::expr_t *GpuOffloadVisitor::int32_const(const Location &loc, int64_t n) {
    ASR::ttype_t *int_type = ASRUtils::TYPE(
        ASR::make_Integer_t(al, loc, 4));
    return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, n,
        int_type, ASR::integerbozType::Decimal));
}

// The loop counters generated below are integer(4); a section bound
// of another integer kind has to be converted before it can be
// combined with them.
ASR::expr_t *GpuOffloadVisitor::to_int32(const Location &loc, ASR::expr_t *e) {
    ASR::ttype_t *t = ASRUtils::extract_type(ASRUtils::expr_type(e));
    if (ASR::is_a<ASR::Integer_t>(*t)
            && ASR::down_cast<ASR::Integer_t>(t)->m_kind == 4) {
        return e;
    }
    ASR::ttype_t *int_type = ASRUtils::TYPE(
        ASR::make_Integer_t(al, loc, 4));
    return ASRUtils::EXPR(ASR::make_Cast_t(al, loc, e,
        ASR::cast_kindType::IntegerToInteger, int_type, nullptr,
        nullptr));
}

// Number of elements of a section dimension `lo:hi:step`, which is
// (hi - lo)/step + 1. Truncating integer division gives the right
// answer for a negative step too, since numerator and denominator
// then have the same sign.
ASR::expr_t *GpuOffloadVisitor::section_extent(const Location &loc,
        const ASR::array_index_t &d) {
    if (is_int_literal(d.m_left, 1) && is_int_literal(d.m_step, 1)) {
        return d.m_right;
    }
    ASR::ttype_t *int_type = ASRUtils::TYPE(
        ASR::make_Integer_t(al, loc, 4));
    ASR::expr_t *span = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al,
        loc, to_int32(loc, d.m_right), ASR::binopType::Sub,
        to_int32(loc, d.m_left), int_type, nullptr));
    if (!is_int_literal(d.m_step, 1)) {
        span = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc, span,
            ASR::binopType::Div, to_int32(loc, d.m_step), int_type,
            nullptr));
    }
    return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc, span,
        ASR::binopType::Add, int32_const(loc, 1), int_type, nullptr));
}

// Array index of the `counter`-th element (counter = 1..extent) of a
// section dimension `lo:hi:step`, which is lo + (counter - 1)*step.
ASR::expr_t *GpuOffloadVisitor::section_index(const Location &loc,
        const ASR::array_index_t &d, ASR::expr_t *counter) {
    if (is_int_literal(d.m_left, 1) && is_int_literal(d.m_step, 1)) {
        return counter;
    }
    ASR::ttype_t *int_type = ASRUtils::TYPE(
        ASR::make_Integer_t(al, loc, 4));
    ASR::expr_t *offset = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al,
        loc, counter, ASR::binopType::Sub, int32_const(loc, 1),
        int_type, nullptr));
    if (!is_int_literal(d.m_step, 1)) {
        offset = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
            offset, ASR::binopType::Mul, to_int32(loc, d.m_step),
            int_type, nullptr));
    }
    return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
        to_int32(loc, d.m_left), ASR::binopType::Add, offset,
        int_type, nullptr));
}

// Inline ArraySection assignments inside a parallel loop body.
// Replaces:
//   b(1:n(l), l) = 1.0   (ArraySection = ArrayBroadcast)
// With:
//   do __gpu_sec_i = 1, n(l)
//     b(__gpu_sec_i, l) = 1.0
//   end do
// This avoids complex lowered code (descriptor temps, ArrayBound)
// that the Metal backend cannot handle inside GPU kernels.
// Evaluate an integer expression made up entirely of literals.
// `section_extent` builds its result unfolded (`(6 - 3) + 1`), and an
// unfolded extent would make the temporary below a descriptor array
// even though its size is known, so fold it here.
bool GpuOffloadVisitor::eval_int_literal(ASR::expr_t *e, int64_t &out) {
    if (!e) return false;
    ASR::expr_t *v = ASRUtils::expr_value(e);
    if (v) e = v;
    if (ASR::is_a<ASR::IntegerConstant_t>(*e)) {
        out = ASR::down_cast<ASR::IntegerConstant_t>(e)->m_n;
        return true;
    }
    if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*e)) {
        int64_t a;
        if (!eval_int_literal(
                ASR::down_cast<ASR::IntegerUnaryMinus_t>(e)->m_arg, a)) {
            return false;
        }
        out = -a;
        return true;
    }
    if (ASR::is_a<ASR::Cast_t>(*e)) {
        return eval_int_literal(
            ASR::down_cast<ASR::Cast_t>(e)->m_arg, out);
    }
    if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
        ASR::IntegerBinOp_t *b = ASR::down_cast<ASR::IntegerBinOp_t>(e);
        int64_t l, r;
        if (!eval_int_literal(b->m_left, l)
                || !eval_int_literal(b->m_right, r)) {
            return false;
        }
        switch (b->m_op) {
            case ASR::binopType::Add: out = l + r; return true;
            case ASR::binopType::Sub: out = l - r; return true;
            case ASR::binopType::Mul: out = l * r; return true;
            case ASR::binopType::Div: {
                if (r == 0) return false;
                out = l / r;
                return true;
            }
            default: return false;
        }
    }
    return false;
}

// Declare a temporary array with `n_extents` dimensions of the given
// extents in `var_scope` and return a reference to it. A dimension
// whose extent is not a compile-time constant makes the temporary a
// descriptor array, exactly as the array-constructor hoisting above
// does, so the same run-time sizing machinery applies.
ASR::expr_t *GpuOffloadVisitor::declare_temp_array(const Location &loc,
        SymbolTable *var_scope, ASR::ttype_t *elem_type,
        ASR::expr_t **extents, size_t n_extents,
        const std::string &prefix) {
    Vec<ASR::dimension_t> dims;
    dims.reserve(al, n_extents);
    bool all_const = true;
    for (size_t i = 0; i < n_extents; i++) {
        ASR::dimension_t d;
        d.loc = loc;
        d.m_start = int32_const(loc, 1);
        d.m_length = extents[i];
        int64_t n;
        if (extents[i] && eval_int_literal(extents[i], n)) {
            d.m_length = int32_const(loc, (int)n);
        } else {
            all_const = false;
        }
        dims.push_back(al, d);
    }
    ASR::ttype_t *tmp_type = ASRUtils::TYPE(
        ASR::make_Array_t(al, loc, elem_type, dims.p, dims.n,
            all_const
                ? ASR::array_physical_typeType::FixedSizeArray
                : ASR::array_physical_typeType::DescriptorArray,
            ASR::memory_spaceType::Global));
    std::string name = var_scope->get_unique_name(prefix);
    ASR::symbol_t *sym = gpu_new_variable(al, loc, var_scope, name,
        tmp_type);
    return ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
}

// The target of `asgn` when its value reads the target's storage
// through a designator that is not element-for-element identical to
// it, and nullptr otherwise. See the comment above
// gpu_designator_base for why such an assignment needs a temporary.
ASR::expr_t *GpuOffloadVisitor::self_aliasing_target(ASR::Assignment_t *asgn) {
    ASR::expr_t *target = asgn->m_target;
    while (ASR::is_a<ASR::ArrayPhysicalCast_t>(*target)) {
        target = ASR::down_cast<ASR::ArrayPhysicalCast_t>(
            target)->m_arg;
    }
    if (!ASRUtils::is_array(ASRUtils::expr_type(target))) {
        return nullptr;
    }
    GpuDesignatorBase base = gpu_designator_base(target);
    if (!base.is_known()) return nullptr;
    GpuSelfAliasChecker checker;
    checker.base = base;
    checker.target = target;
    checker.visit_expr(*asgn->m_value);
    return checker.aliased ? target : nullptr;
}

// Whether the temporary such an assignment needs can be given
// compile-time constant extents. Metal has no variable-length
// arrays, and a run-time sized kernel temporary would have to become
// a device buffer shared by every thread of the kernel, so a loop
// that would need one is not offloaded at all and runs on the host.
bool GpuOffloadVisitor::alias_temp_is_fixed_size(ASR::expr_t *target) {
    const Location &loc = target->base.loc;
    int64_t n;
    if (ASR::is_a<ASR::ArraySection_t>(*target)) {
        ASR::ArraySection_t *as =
            ASR::down_cast<ASR::ArraySection_t>(target);
        size_t n_ranges = 0;
        for (size_t i = 0; i < as->n_args; i++) {
            if (as->m_args[i].m_left && as->m_args[i].m_right
                    && as->m_args[i].m_step) {
                n_ranges++;
                if (!eval_int_literal(
                        section_extent(loc, as->m_args[i]), n)) {
                    return false;
                }
            }
        }
        return n_ranges > 0;
    }
    ASR::ttype_t *tt = ASRUtils::type_get_past_allocatable(
        ASRUtils::type_get_past_pointer(ASRUtils::expr_type(target)));
    if (!ASR::is_a<ASR::Array_t>(*tt)) return false;
    ASR::Array_t *at = ASR::down_cast<ASR::Array_t>(tt);
    for (size_t i = 0; i < at->n_dims; i++) {
        if (!eval_int_literal(at->m_dims[i].m_length, n)) return false;
    }
    return at->n_dims > 0;
}

// The extents of the temporary an aliased assignment to `target`
// needs, one per dimension, in order. False when the shape is not
// written anywhere the temporary could be sized from, so nothing
// could give the temporary the right extent.
bool GpuOffloadVisitor::alias_temp_extents(ASR::expr_t *target,
        Vec<ASR::expr_t*> &extents) {
    const Location &loc = target->base.loc;
    if (ASR::is_a<ASR::ArraySection_t>(*target)) {
        ASR::ArraySection_t *as =
            ASR::down_cast<ASR::ArraySection_t>(target);
        extents.reserve(al, as->n_args);
        for (size_t i = 0; i < as->n_args; i++) {
            if (as->m_args[i].m_left && as->m_args[i].m_right
                    && as->m_args[i].m_step) {
                extents.push_back(al,
                    section_extent(loc, as->m_args[i]));
            }
        }
        return extents.n > 0;
    }
    ASR::ttype_t *tt = ASRUtils::type_get_past_allocatable(
        ASRUtils::type_get_past_pointer(ASRUtils::expr_type(target)));
    if (!ASR::is_a<ASR::Array_t>(*tt)) return false;
    ASR::Array_t *at = ASR::down_cast<ASR::Array_t>(tt);
    if (at->n_dims == 0) return false;
    extents.reserve(al, at->n_dims);
    for (size_t i = 0; i < at->n_dims; i++) {
        if (!at->m_dims[i].m_length) return false;
        extents.push_back(al, at->m_dims[i].m_length);
    }
    return true;
}

// Reports a self-aliasing array assignment whose temporary this pass
// cannot give a per-thread home. Called before any of the destructive
// inline_* helpers, so the loop can still be left on the host.
//
// A fixed-size temporary is a kernel-scope stack array, private to
// the thread by construction. A run-time sized one has to be a
// BLOCK local instead, so that the workspace machinery binds it to a
// per-thread slice of a device buffer -- and only a BLOCK at the top
// level of the loop body is scanned for those, so only a top-level
// assignment can have one. Whether the host can then evaluate the
// extents is settled by the workspace pre-flight further down.
// Whether the backend could size the temporary an aliased assignment
// needs. It is a run-time sized BLOCK local, so it becomes a workspace,
// and the backend describes one with declared_shape_to_vla_workspace.
// An extent that function cannot resolve is a code-generation error
// later, so it is a decline here.
bool GpuOffloadVisitor::alias_temp_extents_resolvable(ASR::expr_t *target,
        ASR::expr_t **extents, size_t n_extents,
        const std::vector<std::string> &arg_names) {
    Vec<ASR::dimension_t> dims;
    dims.reserve(al, n_extents);
    for (size_t i = 0; i < n_extents; i++) {
        ASR::dimension_t d;
        d.loc = extents[i]->base.loc;
        d.m_start = int32_const(extents[i]->base.loc, 1);
        d.m_length = extents[i];
        dims.push_back(al, d);
    }
    ASR::ttype_t *elem_type = ASRUtils::extract_type(
        ASRUtils::expr_type(target));
    ASR::ttype_t *arr_type = ASRUtils::TYPE(ASR::make_Array_t(al,
        target->base.loc, elem_type, dims.p, dims.n,
        ASR::array_physical_typeType::FixedSizeArray,
        ASR::memory_spaceType::Global));
    GpuVlaWorkspace ws;
    return declared_shape_to_vla_workspace(
        ASR::down_cast<ASR::Array_t>(arr_type), "__gpu_alias",
        GpuExtentScope{nullptr, arg_names, nullptr, nullptr, 0}, ws);
}

bool GpuOffloadVisitor::body_needs_unsupported_alias_temp(ASR::stmt_t **body,
        size_t n_body, bool top_level,
        const std::vector<std::string> &arg_names) {
    for (size_t si = 0; si < n_body; si++) {
        ASR::stmt_t *stmt = body[si];
        if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
            ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
            if (body_needs_unsupported_alias_temp(dl->m_body,
                    dl->n_body, false, arg_names)) return true;
            continue;
        }
        if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::BlockCall_t>(stmt)->m_m);
            if (b && ASR::is_a<ASR::Block_t>(*b)) {
                ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                if (body_needs_unsupported_alias_temp(blk->m_body,
                        blk->n_body, false, arg_names)) return true;
            }
            continue;
        }
        if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
            ASR::AssociateBlock_t *ab =
                ASR::down_cast<ASR::AssociateBlock_t>(
                    ASR::down_cast<ASR::AssociateBlockCall_t>(
                        stmt)->m_m);
            if (body_needs_unsupported_alias_temp(ab->m_body,
                    ab->n_body, false, arg_names)) return true;
            continue;
        }
        if (!ASR::is_a<ASR::Assignment_t>(*stmt)) continue;
        ASR::expr_t *target = self_aliasing_target(
            ASR::down_cast<ASR::Assignment_t>(stmt));
        if (!target) continue;
        if (alias_temp_is_fixed_size(target)) continue;
        Vec<ASR::expr_t*> extents;
        if (top_level && alias_temp_extents(target, extents)
                && alias_temp_extents_resolvable(target, extents.p,
                    extents.n, arg_names)) {
            continue;
        }
        return true;
    }
    return false;
}

// Give a materialised ASSOCIATE array temporary the shape of its
// selector, in its own type.
//
// `associate(r => sqrt((x-x0)**2 + (y(j)-y0)**2))` over an
// assumed-shape `x` becomes an allocatable local of the ASSOCIATE's
// symbol table with deferred extents and no ALLOCATE; the shape lives
// only in the expression assigned to it. In a kernel that local
// becomes a per-thread workspace, which has to be sized -- and the
// rewrites further down lower the whole-array assignment into an
// element loop bounded by `ubound(r)`, after which the shape is gone.
// So write it into the type here, while the assignment it can be read
// from is still whole-array. Every replaced dimension list is
// recorded in `undo` so the loop can still be left untouched if a
// later check declines the offload.
void GpuOffloadVisitor::size_scope_array_temporaries(
        ASR::stmt_t **body, size_t n_body,
        std::vector<ScopeArrayDims> &undo) {
    for (size_t si = 0; si < n_body; si++) {
        SymbolTable *symtab = nullptr;
        ASR::stmt_t **inner_body = nullptr;
        size_t inner_n_body = 0;
        if (ASR::is_a<ASR::BlockCall_t>(*body[si])) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::BlockCall_t>(body[si])->m_m);
            if (!b || !ASR::is_a<ASR::Block_t>(*b)) continue;
            ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
            symtab = blk->m_symtab;
            inner_body = blk->m_body;
            inner_n_body = blk->n_body;
        } else if (ASR::is_a<ASR::AssociateBlockCall_t>(*body[si])) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::AssociateBlockCall_t>(
                    body[si])->m_m);
            if (!b || !ASR::is_a<ASR::AssociateBlock_t>(*b)) continue;
            ASR::AssociateBlock_t *ab =
                ASR::down_cast<ASR::AssociateBlock_t>(b);
            symtab = ab->m_symtab;
            inner_body = ab->m_body;
            inner_n_body = ab->n_body;
        } else if (ASR::is_a<ASR::DoLoop_t>(*body[si])) {
            ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(body[si]);
            size_scope_array_temporaries(dl->m_body, dl->n_body, undo);
            continue;
        } else {
            continue;
        }
        for (auto &item : symtab->get_scope()) {
            if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
            ASR::Variable_t *var =
                ASR::down_cast<ASR::Variable_t>(item.second);
            ASR::expr_t *shape_src = gpu_scope_array_shape_source(
                var, inner_body, inner_n_body);
            if (!shape_src) continue;
            ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(
                ASRUtils::type_get_past_allocatable(var->m_type));
            const Location &vloc = var->base.base.loc;
            Vec<ASR::dimension_t> dims;
            dims.reserve(al, arr->n_dims);
            for (size_t d = 0; d < arr->n_dims; d++) {
                ASR::dimension_t dim;
                dim.loc = vloc;
                dim.m_start = int32_const(vloc, 1);
                dim.m_length = ASRUtils::get_size(shape_src,
                    (int)d + 1, al);
                dims.push_back(al, dim);
            }
            // An allocatable must keep deferred extents, so the
            // temporary becomes an automatic array of the same
            // shape -- which is what the workspace machinery binds.
            undo.push_back({var, var->m_type});
            var->m_type = ASRUtils::TYPE(ASR::make_Array_t(al, vloc,
                arr->m_type, dims.p, dims.n,
                ASR::array_physical_typeType::DescriptorArray,
                ASR::memory_spaceType::Global));
        }
        size_scope_array_temporaries(inner_body, inner_n_body, undo);
    }
}

// Give the temporary of a run-time sized aliased assignment a BLOCK
// of its own, at the top level of the loop body.
//
//   a(:,c) = a(n:1:-1,c)
// becomes
//   block
//     real :: __gpu_alias(n)
//     __gpu_alias(1:n) = a(n:1:-1,c)
//     a(:,c)           = __gpu_alias(1:n)
//   end block
//
// The BLOCK is what makes the temporary safe: a run-time sized
// kernel-scope local becomes one device buffer shared by every
// thread, and every thread would write it -- a race. A BLOCK local is
// bound to a per-thread slice of a workspace buffer instead
// (analyze_gpu_vla_workspaces), which is private to the iteration.
// Only a top-level BLOCK of the kernel body is scanned for
// workspaces, which is why only a top-level assignment is rewritten
// here; body_needs_unsupported_alias_temp has already declined the
// rest.
void GpuOffloadVisitor::materialize_runtime_alias_blocks(
        ParallelLoopNest &nest) {
    Vec<ASR::stmt_t*> new_body;
    new_body.reserve(al, nest.n_body);
    bool changed = false;
    for (size_t si = 0; si < nest.n_body; si++) {
        ASR::stmt_t *stmt = nest.body[si];
        if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
            new_body.push_back(al, stmt);
            continue;
        }
        ASR::Assignment_t *asgn =
            ASR::down_cast<ASR::Assignment_t>(stmt);
        ASR::expr_t *target = self_aliasing_target(asgn);
        if (!target || alias_temp_is_fixed_size(target)) {
            new_body.push_back(al, stmt);
            continue;
        }
        Vec<ASR::expr_t*> extents;
        if (!alias_temp_extents(target, extents)) {
            new_body.push_back(al, stmt);
            continue;
        }
        Location loc = stmt->base.loc;
        SymbolTable *block_scope =
            al.make_new<SymbolTable>(current_scope);
        ASR::ttype_t *elem_type = ASRUtils::extract_type(
            ASRUtils::expr_type(target));
        ASR::expr_t *tmp = declare_temp_array(loc, block_scope,
            elem_type, extents.p, extents.n, "__gpu_alias");
        bool sectioned = ASR::is_a<ASR::ArraySection_t>(*target);
        // A full 1:extent:1 section over the temporary, built once
        // per use so the two statements do not share nodes.
        auto tmp_ref = [&]() -> ASR::expr_t* {
            if (!sectioned) return tmp;
            Vec<ASR::array_index_t> args;
            args.reserve(al, extents.n);
            for (size_t i = 0; i < extents.n; i++) {
                ASR::array_index_t idx;
                idx.loc = loc;
                idx.m_left = int32_const(loc, 1);
                idx.m_right = extents[i];
                idx.m_step = int32_const(loc, 1);
                args.push_back(al, idx);
            }
            return ASRUtils::EXPR(ASR::make_ArraySection_t(al, loc,
                tmp, args.p, args.n, ASRUtils::expr_type(tmp),
                nullptr));
        };
        Vec<ASR::stmt_t*> block_body;
        block_body.reserve(al, 2);
        block_body.push_back(al, ASRUtils::STMT(
            ASR::make_Assignment_t(al, loc, tmp_ref(),
                asgn->m_value, nullptr, false, false)));
        asgn->m_value = tmp_ref();
        block_body.push_back(al, stmt);
        std::string block_name = current_scope->get_unique_name(
            "__gpu_alias_scope");
        ASR::asr_t *block = ASR::make_Block_t(al, loc, block_scope,
            s2c(al, block_name), block_body.p, block_body.n);
        block_scope->asr_owner = block;
        ASR::symbol_t *block_sym =
            ASR::down_cast<ASR::symbol_t>(block);
        current_scope->add_symbol(block_name, block_sym);
        kernel_blocks.push_back(block_sym);
        new_body.push_back(al, ASRUtils::STMT(ASR::make_BlockCall_t(
            al, loc, -1, block_sym)));
        changed = true;
    }
    if (changed) {
        // Through set_body, so the loop the nest reads from carries
        // the rewrite too and not just the view of it.
        nest.set_body(new_body.p, new_body.n);
    }
}

// Materialise a temporary for an array assignment whose target and
// value designate overlapping storage of the same array. See the
// comment above gpu_designator_base: without it the element loops
// built below read elements the same statement has already written.
//   a(:) = a(n:1:-1)
// becomes
//   __gpu_alias(1:n) = a(n:1:-1)
//   a(:)            = __gpu_alias(1:n)
// Both halves are alias-free, and the existing ArraySection and
// whole-array lowerings turn each of them into an element loop.
void GpuOffloadVisitor::materialize_aliased_assignments(
        ParallelLoopNest &nest) {
    bool changed = false;
    NestBodyWriteBack back(nest);
    materialize_aliased_in_body(back.body, back.n_body, changed);
}

void GpuOffloadVisitor::materialize_aliased_in_body(
        ASR::stmt_t** &body, size_t &n_body,
        bool &changed) {
    Vec<ASR::stmt_t*> new_body;
    new_body.reserve(al, n_body * 2);
    bool local_changed = false;

    for (size_t si = 0; si < n_body; si++) {
        ASR::stmt_t *stmt = body[si];
        if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
            ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
            materialize_aliased_in_body(dl->m_body, dl->n_body, changed);
            new_body.push_back(al, stmt);
            continue;
        }
        if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::BlockCall_t>(stmt)->m_m);
            if (b && ASR::is_a<ASR::Block_t>(*b)) {
                ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                materialize_aliased_in_body(blk->m_body, blk->n_body,
                    changed);
            }
            new_body.push_back(al, stmt);
            continue;
        }
        if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
            ASR::AssociateBlock_t *ab =
                ASR::down_cast<ASR::AssociateBlock_t>(
                    ASR::down_cast<ASR::AssociateBlockCall_t>(
                        stmt)->m_m);
            materialize_aliased_in_body(ab->m_body, ab->n_body,
                changed);
            new_body.push_back(al, stmt);
            continue;
        }
        if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
            new_body.push_back(al, stmt);
            continue;
        }
        ASR::Assignment_t *asgn = ASR::down_cast<ASR::Assignment_t>(stmt);
        ASR::expr_t *target = self_aliasing_target(asgn);
        // A loop holding an assignment that needs a temporary this
        // pass cannot size was already declined for offload, so the
        // second test only guards the statements spliced in since.
        if (!target || !alias_temp_is_fixed_size(target)) {
            new_body.push_back(al, stmt);
            continue;
        }

        Location loc = stmt->base.loc;
        SymbolTable *var_scope = current_scope;
        while (var_scope && var_scope->asr_owner &&
               var_scope->asr_owner->type == ASR::asrType::symbol &&
               ASR::is_a<ASR::AssociateBlock_t>(
                   *ASR::down_cast<ASR::symbol_t>(
                       var_scope->asr_owner))) {
            var_scope = var_scope->parent;
        }
        ASR::ttype_t *elem_type = ASRUtils::extract_type(
            ASRUtils::expr_type(target));

        if (ASR::is_a<ASR::ArraySection_t>(*target)) {
            ASR::ArraySection_t *as =
                ASR::down_cast<ASR::ArraySection_t>(target);
            Vec<ASR::expr_t*> extents;
            extents.reserve(al, as->n_args);
            for (size_t i = 0; i < as->n_args; i++) {
                if (as->m_args[i].m_left && as->m_args[i].m_right
                        && as->m_args[i].m_step) {
                    extents.push_back(al,
                        section_extent(loc, as->m_args[i]));
                }
            }
            if (extents.n == 0) {
                new_body.push_back(al, stmt);
                continue;
            }
            ASR::expr_t *tmp = declare_temp_array(loc, var_scope,
                elem_type, extents.p, extents.n, "__gpu_alias");
            // A full 1:extent:1 section over the temporary, built
            // twice so the two statements do not share nodes.
            auto tmp_section = [&]() -> ASR::expr_t* {
                Vec<ASR::array_index_t> args;
                args.reserve(al, extents.n);
                for (size_t i = 0; i < extents.n; i++) {
                    ASR::array_index_t idx;
                    idx.loc = loc;
                    idx.m_left = int32_const(loc, 1);
                    idx.m_right = extents[i];
                    idx.m_step = int32_const(loc, 1);
                    args.push_back(al, idx);
                }
                return ASRUtils::EXPR(ASR::make_ArraySection_t(al, loc,
                    tmp, args.p, args.n,
                    ASRUtils::expr_type(tmp), nullptr));
            };
            new_body.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, tmp_section(),
                    asgn->m_value, nullptr, false, false)));
            asgn->m_value = tmp_section();
        } else {
            Vec<ASR::expr_t*> extents;
            ASR::ttype_t *tt = ASRUtils::type_get_past_allocatable(
                ASRUtils::type_get_past_pointer(
                    ASRUtils::expr_type(target)));
            ASR::Array_t *at = ASR::down_cast<ASR::Array_t>(tt);
            extents.reserve(al, at->n_dims);
            for (size_t i = 0; i < at->n_dims; i++) {
                extents.push_back(al, at->m_dims[i].m_length);
            }
            ASR::expr_t *tmp = declare_temp_array(loc, var_scope,
                elem_type, extents.p, extents.n, "__gpu_alias");
            new_body.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, tmp, asgn->m_value,
                    nullptr, false, false)));
            asgn->m_value = tmp;
        }
        new_body.push_back(al, stmt);
        local_changed = true;
    }

    if (local_changed) {
        body = new_body.p;
        n_body = new_body.n;
        changed = true;
    }
}

void GpuOffloadVisitor::inline_array_section_assignment(
        ParallelLoopNest &nest) {
    bool changed = false;
    NestBodyWriteBack back(nest);
    inline_array_section_in_body(back.body, back.n_body, changed);
}

void GpuOffloadVisitor::inline_array_section_in_body(
        ASR::stmt_t** &body, size_t &n_body,
        bool &changed) {
    Vec<ASR::stmt_t*> new_body;
    new_body.reserve(al, n_body * 2);

    for (size_t si = 0; si < n_body; si++) {
        ASR::stmt_t *stmt = body[si];
        // Recurse into DoLoop bodies
        if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
            ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
            inline_array_section_in_body(dl->m_body, dl->n_body,
                changed);
            new_body.push_back(al, stmt);
            continue;
        }
        // Recurse into BlockCall bodies
        if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
            ASR::BlockCall_t *bc =
                ASR::down_cast<ASR::BlockCall_t>(stmt);
            if (ASR::is_a<ASR::Block_t>(*bc->m_m)) {
                ASR::Block_t *block =
                    ASR::down_cast<ASR::Block_t>(bc->m_m);
                inline_array_section_in_body(block->m_body,
                    block->n_body, changed);
            }
            new_body.push_back(al, stmt);
            continue;
        }
        // Recurse into AssociateBlockCall bodies
        if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
            ASR::AssociateBlockCall_t *abc =
                ASR::down_cast<ASR::AssociateBlockCall_t>(stmt);
            ASR::AssociateBlock_t *ab =
                ASR::down_cast<ASR::AssociateBlock_t>(abc->m_m);
            inline_array_section_in_body(ab->m_body,
                ab->n_body, changed);
            new_body.push_back(al, stmt);
            continue;
        }
        if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
            new_body.push_back(al, stmt);
            continue;
        }
        ASR::Assignment_t *asgn = ASR::down_cast<ASR::Assignment_t>(stmt);
        if (!ASR::is_a<ASR::ArraySection_t>(*asgn->m_target)) {
            new_body.push_back(al, stmt);
            continue;
        }
        ASR::ArraySection_t *as = ASR::down_cast<ASR::ArraySection_t>(
            asgn->m_target);

        // Collect all range dimensions (have m_left, m_right, m_step
        // set, meaning it's a slice like 1:n, not a scalar index)
        std::vector<int> range_dims;
        for (size_t i = 0; i < as->n_args; i++) {
            if (as->m_args[i].m_left && as->m_args[i].m_right
                    && as->m_args[i].m_step) {
                range_dims.push_back((int)i);
            }
        }
        if (range_dims.empty()) {
            new_body.push_back(al, stmt);
            continue;
        }

        Location loc = stmt->base.loc;
        ASR::ttype_t *int_type = ASRUtils::TYPE(
            ASR::make_Integer_t(al, loc, 4));

        // Create loop variable(s) in the containing function/program
        // scope, not in any enclosing AssociateBlock scope.
        SymbolTable *var_scope = current_scope;
        while (var_scope && var_scope->asr_owner &&
               var_scope->asr_owner->type == ASR::asrType::symbol &&
               ASR::is_a<ASR::AssociateBlock_t>(
                   *ASR::down_cast<ASR::symbol_t>(
                       var_scope->asr_owner))) {
            var_scope = var_scope->parent;
        }

        // Create a loop variable for each range dimension
        std::vector<ASR::expr_t*> loop_vars(range_dims.size());
        for (size_t ri = 0; ri < range_dims.size(); ri++) {
            std::string loop_var_name = var_scope->get_unique_name(
                "__gpu_sec_i");
            ASR::symbol_t *loop_var_sym = gpu_new_variable(al, loc,
                var_scope, loop_var_name, ASRUtils::duplicate_type(al,
                    int_type));
            loop_vars[ri] = ASRUtils::EXPR(
                ASR::make_Var_t(al, loc, loop_var_sym));
        }

        // Build ArrayItem: replace each range dim with its loop var,
        // keep scalar-index dims as-is
        Vec<ASR::array_index_t> new_args;
        new_args.reserve(al, as->n_args);
        for (size_t i = 0; i < as->n_args; i++) {
            ASR::array_index_t idx;
            idx.loc = as->m_args[i].loc;
            // Check if this dimension is a range dimension
            bool is_range = false;
            for (size_t ri = 0; ri < range_dims.size(); ri++) {
                if ((int)i == range_dims[ri]) {
                    idx.m_left = nullptr;
                    idx.m_right = section_index(loc, as->m_args[i],
                        loop_vars[ri]);
                    idx.m_step = nullptr;
                    is_range = true;
                    break;
                }
            }
            if (!is_range) {
                idx.m_left = as->m_args[i].m_left;
                idx.m_right = as->m_args[i].m_right;
                idx.m_step = as->m_args[i].m_step;
            }
            new_args.push_back(al, idx);
        }
        ASR::ttype_t *elem_type = ASRUtils::extract_type(
            ASRUtils::expr_type(as->m_v));
        ASR::expr_t *array_item = ASRUtils::EXPR(
            ASR::make_ArrayItem_t(al, loc, as->m_v,
                new_args.p, new_args.n, elem_type,
                ASR::arraystorageType::ColMajor, nullptr));

        // Elementize: recursively replace ArraySection with
        // ArrayItem and unwrap ArrayBroadcast in the RHS
        std::function<ASR::expr_t*(ASR::expr_t*)> elementize_rhs =
            [&](ASR::expr_t *e) -> ASR::expr_t* {
            if (ASR::is_a<ASR::ArraySection_t>(*e)) {
                ASR::ArraySection_t *rhs_as =
                    ASR::down_cast<ASR::ArraySection_t>(e);
                Vec<ASR::array_index_t> rhs_new_args;
                rhs_new_args.reserve(al, rhs_as->n_args);
                size_t rv_idx = 0;
                for (size_t i = 0; i < rhs_as->n_args; i++) {
                    ASR::array_index_t idx;
                    idx.loc = rhs_as->m_args[i].loc;
                    if (rhs_as->m_args[i].m_left &&
                            rhs_as->m_args[i].m_right &&
                            rhs_as->m_args[i].m_step) {
                        if (rv_idx < loop_vars.size()) {
                            idx.m_left = nullptr;
                            idx.m_right = section_index(loc,
                                rhs_as->m_args[i], loop_vars[rv_idx]);
                            idx.m_step = nullptr;
                            rv_idx++;
                        } else {
                            idx = rhs_as->m_args[i];
                        }
                    } else {
                        idx.m_left = rhs_as->m_args[i].m_left;
                        idx.m_right = rhs_as->m_args[i].m_right;
                        idx.m_step = rhs_as->m_args[i].m_step;
                    }
                    rhs_new_args.push_back(al, idx);
                }
                ASR::ttype_t *rhs_elem = ASRUtils::extract_type(
                    ASRUtils::expr_type(rhs_as->m_v));
                return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc,
                    rhs_as->m_v, rhs_new_args.p, rhs_new_args.n,
                    rhs_elem, ASR::arraystorageType::ColMajor,
                    nullptr));
            } else if (ASR::is_a<ASR::ArrayBroadcast_t>(*e)) {
                return ASR::down_cast<ASR::ArrayBroadcast_t>(
                    e)->m_array;
            } else if (ASR::is_a<ASR::RealBinOp_t>(*e)) {
                ASR::RealBinOp_t *rb =
                    ASR::down_cast<ASR::RealBinOp_t>(e);
                ASR::ttype_t *et = ASRUtils::extract_type(
                    ASRUtils::expr_type(e));
                return ASRUtils::EXPR(ASR::make_RealBinOp_t(al,
                    loc, elementize_rhs(rb->m_left), rb->m_op,
                    elementize_rhs(rb->m_right), et, nullptr));
            } else if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
                ASR::IntegerBinOp_t *ib =
                    ASR::down_cast<ASR::IntegerBinOp_t>(e);
                ASR::ttype_t *et = ASRUtils::extract_type(
                    ASRUtils::expr_type(e));
                return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al,
                    loc, elementize_rhs(ib->m_left), ib->m_op,
                    elementize_rhs(ib->m_right), et, nullptr));
            } else if (ASR::is_a<ASR::RealUnaryMinus_t>(*e)) {
                ASR::RealUnaryMinus_t *ru =
                    ASR::down_cast<ASR::RealUnaryMinus_t>(e);
                ASR::ttype_t *et = ASRUtils::extract_type(
                    ASRUtils::expr_type(e));
                return ASRUtils::EXPR(ASR::make_RealUnaryMinus_t(al,
                    loc, elementize_rhs(ru->m_arg), et, nullptr));
            } else if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*e)) {
                ASR::IntegerUnaryMinus_t *iu =
                    ASR::down_cast<ASR::IntegerUnaryMinus_t>(e);
                ASR::ttype_t *et = ASRUtils::extract_type(
                    ASRUtils::expr_type(e));
                return ASRUtils::EXPR(ASR::make_IntegerUnaryMinus_t(al,
                    loc, elementize_rhs(iu->m_arg), et, nullptr));
            } else if (ASR::is_a<ASR::LogicalNot_t>(*e)) {
                ASR::LogicalNot_t *ln =
                    ASR::down_cast<ASR::LogicalNot_t>(e);
                ASR::ttype_t *et = ASRUtils::extract_type(
                    ASRUtils::expr_type(e));
                return ASRUtils::EXPR(ASR::make_LogicalNot_t(al,
                    loc, elementize_rhs(ln->m_arg), et, nullptr));
            } else if (ASR::is_a<ASR::RealCompare_t>(*e)) {
                ASR::RealCompare_t *rc =
                    ASR::down_cast<ASR::RealCompare_t>(e);
                ASR::ttype_t *et = ASRUtils::extract_type(
                    ASRUtils::expr_type(e));
                return ASRUtils::EXPR(ASR::make_RealCompare_t(al,
                    loc, elementize_rhs(rc->m_left), rc->m_op,
                    elementize_rhs(rc->m_right), et, nullptr));
            } else if (ASR::is_a<ASR::IntegerCompare_t>(*e)) {
                ASR::IntegerCompare_t *ic =
                    ASR::down_cast<ASR::IntegerCompare_t>(e);
                ASR::ttype_t *et = ASRUtils::extract_type(
                    ASRUtils::expr_type(e));
                return ASRUtils::EXPR(ASR::make_IntegerCompare_t(al,
                    loc, elementize_rhs(ic->m_left), ic->m_op,
                    elementize_rhs(ic->m_right), et, nullptr));
            } else if (ASR::is_a<ASR::LogicalCompare_t>(*e)) {
                ASR::LogicalCompare_t *lc =
                    ASR::down_cast<ASR::LogicalCompare_t>(e);
                ASR::ttype_t *et = ASRUtils::extract_type(
                    ASRUtils::expr_type(e));
                return ASRUtils::EXPR(ASR::make_LogicalCompare_t(al,
                    loc, elementize_rhs(lc->m_left), lc->m_op,
                    elementize_rhs(lc->m_right), et, nullptr));
            } else if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(
                    *e)) {
                ASR::IntrinsicElementalFunction_t *f =
                    ASR::down_cast<
                        ASR::IntrinsicElementalFunction_t>(e);
                Vec<ASR::expr_t*> new_fargs;
                new_fargs.reserve(al, f->n_args);
                for (size_t i = 0; i < f->n_args; i++) {
                    new_fargs.push_back(al,
                        f->m_args[i]
                            ? elementize_rhs(f->m_args[i])
                            : nullptr);
                }
                ASR::ttype_t *et = ASRUtils::extract_type(
                    ASRUtils::expr_type(e));
                return ASRUtils::EXPR(
                    ASR::make_IntrinsicElementalFunction_t(al,
                        loc, f->m_intrinsic_id, new_fargs.p,
                        new_fargs.n, f->m_overload_id, et,
                        f->m_value));
            } else if (ASR::is_a<ASR::FunctionCall_t>(*e)) {
                ASR::FunctionCall_t *fc =
                    ASR::down_cast<ASR::FunctionCall_t>(e);
                // Check if the function natively returns an array
                // (not an elemental function with array args).
                // In that case, keep the original return type and
                // wrap with ArrayItem below.
                ASR::ttype_t *fc_type = ASRUtils::type_get_past_allocatable(
                    fc->m_type);
                ASR::Function_t *fn = ASRUtils::get_function(fc->m_name);
                ASR::ttype_t *fn_ret = fn
                    ? ASRUtils::get_FunctionType(fn)->m_return_var_type
                    : nullptr;
                bool fn_returns_array = fn_ret &&
                    ASR::is_a<ASR::Array_t>(
                        *ASRUtils::type_get_past_allocatable(fn_ret));
                ASR::FunctionType_t *fn_type = fn
                    ? ASRUtils::get_FunctionType(fn) : nullptr;
                Vec<ASR::call_arg_t> new_fargs;
                new_fargs.reserve(al, fc->n_args);
                for (size_t i = 0; i < fc->n_args; i++) {
                    ASR::call_arg_t arg;
                    arg.loc = fc->m_args[i].loc;
                    if (!fc->m_args[i].m_value) {
                        arg.m_value = nullptr;
                    } else if (fn_returns_array && fn_type
                            && i < fn_type->n_arg_types
                            && ASR::is_a<ASR::Array_t>(
                                *ASRUtils::type_get_past_allocatable(
                                    fn_type->m_arg_types[i]))) {
                        // Keep array arguments as-is for functions
                        // that return arrays; elementizing would
                        // turn ArraySection into scalar ArrayItem,
                        // breaking the function's array contract.
                        arg.m_value = fc->m_args[i].m_value;
                    } else {
                        arg.m_value = elementize_rhs(
                            fc->m_args[i].m_value);
                    }
                    new_fargs.push_back(al, arg);
                }
                if (fn_returns_array && ASR::is_a<ASR::Array_t>(*fc_type)) {
                    ASR::expr_t *new_fc = ASRUtils::EXPR(
                        ASR::make_FunctionCall_t(al, loc,
                            fc->m_name, fc->m_original_name,
                            new_fargs.p, new_fargs.n, fc->m_type,
                            fc->m_value, fc->m_dt));
                    Vec<ASR::array_index_t> rhs_args;
                    rhs_args.reserve(al, range_dims.size());
                    for (size_t ri = 0; ri < range_dims.size(); ri++) {
                        ASR::array_index_t idx;
                        idx.loc = loc;
                        idx.m_left = nullptr;
                        idx.m_right = loop_vars[ri];
                        idx.m_step = nullptr;
                        rhs_args.push_back(al, idx);
                    }
                    ASR::ttype_t *rhs_elem = ASRUtils::extract_type(
                        fc->m_type);
                    return ASRUtils::EXPR(
                        ASR::make_ArrayItem_t(al, loc, new_fc,
                            rhs_args.p, rhs_args.n, rhs_elem,
                            ASR::arraystorageType::ColMajor, nullptr));
                }
                ASR::ttype_t *et = ASRUtils::extract_type(
                    ASRUtils::expr_type(e));
                return ASRUtils::EXPR(
                    ASR::make_FunctionCall_t(al, loc,
                        fc->m_name, fc->m_original_name,
                        new_fargs.p, new_fargs.n, et,
                        fc->m_value, fc->m_dt));
            } else if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
                return elementize_rhs(
                    ASR::down_cast<ASR::ArrayPhysicalCast_t>(
                        e)->m_arg);
            } else if (ASR::is_a<ASR::Cast_t>(*e)) {
                ASR::Cast_t *c = ASR::down_cast<ASR::Cast_t>(e);
                ASR::ttype_t *ct = c->m_type;
                if (ASR::is_a<ASR::Array_t>(*ct)) {
                    ct = ASRUtils::extract_type(ct);
                }
                return ASRUtils::EXPR(ASR::make_Cast_t(al, loc,
                    elementize_rhs(c->m_arg), c->m_kind, ct,
                    c->m_value, nullptr));
            }
            // Fallback: if still array-typed, wrap with ArrayItem
            ASR::ttype_t *e_type = ASRUtils::expr_type(e);
            ASR::ttype_t *e_type_inner =
                ASRUtils::type_get_past_allocatable(e_type);
            if (ASR::is_a<ASR::Array_t>(*e_type_inner)) {
                Vec<ASR::array_index_t> rhs_args;
                rhs_args.reserve(al, range_dims.size());
                for (size_t ri = 0; ri < range_dims.size(); ri++) {
                    ASR::array_index_t idx;
                    idx.loc = loc;
                    idx.m_left = nullptr;
                    idx.m_right = loop_vars[ri];
                    idx.m_step = nullptr;
                    rhs_args.push_back(al, idx);
                }
                ASR::ttype_t *rhs_elem = ASRUtils::extract_type(
                    e_type);
                return ASRUtils::EXPR(
                    ASR::make_ArrayItem_t(al, loc, e,
                        rhs_args.p, rhs_args.n, rhs_elem,
                        ASR::arraystorageType::ColMajor, nullptr));
            }
            return e;
        };
        ASR::expr_t *scalar_value = elementize_rhs(asgn->m_value);

        // Build innermost loop body: array_item = scalar_value
        Vec<ASR::stmt_t*> inner_body;
        inner_body.reserve(al, 1);
        inner_body.push_back(al, ASRUtils::STMT(
            ASR::make_Assignment_t(al, loc, array_item, scalar_value,
                nullptr, false, false)));

        // Build nested DoLoops from innermost to outermost
        ASR::stmt_t *loop_stmt = nullptr;
        for (int ri = (int)range_dims.size() - 1; ri >= 0; ri--) {
            int dim = range_dims[ri];
            ASR::do_loop_head_t head;
            head.loc = loc;
            head.m_v = loop_vars[ri];
            head.m_start = int32_const(loc, 1);
            head.m_end = section_extent(loc, as->m_args[dim]);
            head.m_increment = nullptr;

            Vec<ASR::stmt_t*> body;
            body.reserve(al, 1);
            if (loop_stmt) {
                body.push_back(al, loop_stmt);
            } else {
                body.push_back(al, inner_body[0]);
            }
            loop_stmt = ASRUtils::STMT(
                ASR::make_DoLoop_t(al, loc, nullptr,
                    head, body.p, body.n, nullptr, 0));
        }
        new_body.push_back(al, loop_stmt);

        changed = true;
    }

    if (changed) {
        body = new_body.p;
        n_body = new_body.n;
    }
}

bool GpuOffloadVisitor::linear_form(
        ASR::expr_t *e, LinearForm &f, int64_t scale) {
    if (!e) return false;
    ASR::expr_t *v = ASRUtils::expr_value(e);
    if (v) e = v;
    if (ASR::is_a<ASR::IntegerConstant_t>(*e)) {
        f.constant += scale
            * ASR::down_cast<ASR::IntegerConstant_t>(e)->m_n;
        return true;
    }
    if (ASR::is_a<ASR::Cast_t>(*e)) {
        return linear_form(ASR::down_cast<ASR::Cast_t>(e)->m_arg, f,
            scale);
    }
    if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*e)) {
        return linear_form(
            ASR::down_cast<ASR::IntegerUnaryMinus_t>(e)->m_arg, f,
            -scale);
    }
    if (ASR::is_a<ASR::Var_t>(*e)) {
        f.terms[ASR::down_cast<ASR::Var_t>(e)->m_v] += scale;
        return true;
    }
    if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
        ASR::IntegerBinOp_t *b = ASR::down_cast<ASR::IntegerBinOp_t>(e);
        int64_t c;
        switch (b->m_op) {
            case ASR::binopType::Add:
                return linear_form(b->m_left, f, scale)
                    && linear_form(b->m_right, f, scale);
            case ASR::binopType::Sub:
                return linear_form(b->m_left, f, scale)
                    && linear_form(b->m_right, f, -scale);
            case ASR::binopType::Mul:
                if (eval_int_literal(b->m_left, c)) {
                    return linear_form(b->m_right, f, scale * c);
                }
                if (eval_int_literal(b->m_right, c)) {
                    return linear_form(b->m_left, f, scale * c);
                }
                return false;
            default: return false;
        }
    }
    return false;
}

// Number of elements of the section dimension `lo:hi:step` when it is
// a compile-time constant, which it is whenever `hi - lo` and `step`
// are -- the bounds themselves need not be.
bool GpuOffloadVisitor::const_section_extent(
        const ASR::array_index_t &d, int64_t &n) {
    int64_t step;
    if (!d.m_left || !d.m_right || !d.m_step) return false;
    if (!eval_int_literal(d.m_step, step) || step == 0) return false;
    LinearForm f;
    if (!linear_form(d.m_right, f, 1)) return false;
    if (!linear_form(d.m_left, f, -1)) return false;
    for (auto &t : f.terms) {
        if (t.second != 0) return false;
    }
    n = f.constant / step + 1;
    if (n < 0) n = 0;
    return true;
}

bool GpuOffloadVisitor::section_is_strided(const ASR::ArraySection_t *as) {
    for (size_t i = 0; i < as->n_args; i++) {
        if (!as->m_args[i].m_left || !as->m_args[i].m_right
                || !as->m_args[i].m_step) {
            continue;
        }
        if (!is_int_literal(as->m_args[i].m_step, 1)) return true;
    }
    return false;
}

// A dummy the callee may write has to be copied back. An unknown
// intent is treated as writable: a wrong answer is worse than a copy.
bool GpuOffloadVisitor::dummy_is_written(
        ASR::Function_t *fn, size_t arg_index) {
    if (!fn || arg_index >= fn->n_args) return true;
    if (!ASR::is_a<ASR::Var_t>(*fn->m_args[arg_index])) return true;
    ASR::symbol_t *s = ASR::down_cast<ASR::Var_t>(
        fn->m_args[arg_index])->m_v;
    if (!ASR::is_a<ASR::Variable_t>(*s)) return true;
    return ASR::down_cast<ASR::Variable_t>(s)->m_intent
        != ASR::intentType::In;
}

// Build `do c = 1, extent ... end do` nests copying between the
// section `as` of its base array and the contiguous temporary `tmp`.
// With `to_temp` the section is read into the temporary (gather);
// otherwise the temporary is written back into the section (scatter).
ASR::stmt_t* GpuOffloadVisitor::build_section_copy_loops(const Location &loc,
        SymbolTable *var_scope, ASR::ArraySection_t *as,
        const std::vector<int> &range_dims, ASR::expr_t *tmp,
        bool to_temp) {
    ASR::ttype_t *int_type = ASRUtils::TYPE(
        ASR::make_Integer_t(al, loc, 4));
    std::vector<ASR::expr_t*> counters(range_dims.size());
    for (size_t ri = 0; ri < range_dims.size(); ri++) {
        std::string name = var_scope->get_unique_name("__gpu_gather_i");
        ASR::symbol_t *sym = gpu_new_variable(al, loc, var_scope, name,
            ASRUtils::duplicate_type(al, int_type));
        counters[ri] = ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
    }
    ASR::ttype_t *elem_type = ASRUtils::extract_type(
        ASRUtils::expr_type(as->m_v));

    // The section element for this iteration.
    Vec<ASR::array_index_t> src_args;
    src_args.reserve(al, as->n_args);
    size_t ri = 0;
    for (size_t d = 0; d < as->n_args; d++) {
        ASR::array_index_t idx;
        idx.loc = as->m_args[d].loc;
        bool is_range = ri < range_dims.size()
            && (int)d == range_dims[ri];
        if (is_range) {
            idx.m_left = nullptr;
            idx.m_right = section_index(loc, as->m_args[d],
                counters[ri]);
            idx.m_step = nullptr;
            ri++;
        } else {
            idx.m_left = as->m_args[d].m_left;
            idx.m_right = as->m_args[d].m_right;
            idx.m_step = as->m_args[d].m_step;
        }
        src_args.push_back(al, idx);
    }
    ASR::expr_t *src_elem = ASRUtils::EXPR(ASR::make_ArrayItem_t(al,
        loc, as->m_v, src_args.p, src_args.n, elem_type,
        ASR::arraystorageType::ColMajor, nullptr));

    // The temporary's element for the same iteration: the temporary is
    // 1-based and contiguous, so the counters index it directly.
    Vec<ASR::array_index_t> tmp_args;
    tmp_args.reserve(al, range_dims.size());
    for (size_t k = 0; k < range_dims.size(); k++) {
        ASR::array_index_t idx;
        idx.loc = loc;
        idx.m_left = nullptr;
        idx.m_right = counters[k];
        idx.m_step = nullptr;
        tmp_args.push_back(al, idx);
    }
    ASR::expr_t *tmp_elem = ASRUtils::EXPR(ASR::make_ArrayItem_t(al,
        loc, tmp, tmp_args.p, tmp_args.n, elem_type,
        ASR::arraystorageType::ColMajor, nullptr));

    ASR::stmt_t *inner = ASRUtils::STMT(ASR::make_Assignment_t(al, loc,
        to_temp ? tmp_elem : src_elem,
        to_temp ? src_elem : tmp_elem, nullptr, false, false));
    for (int k = (int)range_dims.size() - 1; k >= 0; k--) {
        ASR::do_loop_head_t head;
        head.loc = loc;
        head.m_v = counters[k];
        head.m_start = int32_const(loc, 1);
        head.m_end = section_extent(loc, as->m_args[range_dims[k]]);
        head.m_increment = nullptr;
        Vec<ASR::stmt_t*> body;
        body.reserve(al, 1);
        body.push_back(al, inner);
        inner = ASRUtils::STMT(ASR::make_DoLoop_t(al, loc, nullptr,
            head, body.p, body.n, nullptr, 0));
    }
    return inner;
}

// The strided section under any physical casts of an actual argument,
// or nullptr when the argument is not one.
ASR::ArraySection_t* GpuOffloadVisitor::strided_section_actual(
        ASR::expr_t *e) {
    while (e && ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
        e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
    }
    if (!e || !ASR::is_a<ASR::ArraySection_t>(*e)) return nullptr;
    ASR::ArraySection_t *as = ASR::down_cast<ASR::ArraySection_t>(e);
    return section_is_strided(as) ? as : nullptr;
}

// Can this strided section be gathered into a contiguous temporary?
// The base has to be a designator the copy loops can index, and every
// extent has to fold to a constant, because the temporary is a
// kernel-local array.
bool GpuOffloadVisitor::strided_section_is_gatherable(
        ASR::ArraySection_t *as) {
    if (!ASR::is_a<ASR::Var_t>(*as->m_v)
            && !ASR::is_a<ASR::StructInstanceMember_t>(*as->m_v)) {
        return false;
    }
    bool any_range = false;
    for (size_t d = 0; d < as->n_args; d++) {
        if (!as->m_args[d].m_left || !as->m_args[d].m_right
                || !as->m_args[d].m_step) {
            continue;
        }
        any_range = true;
        int64_t n;
        if (!const_section_extent(as->m_args[d], n)) return false;
    }
    return any_range;
}

// Replace a strided section actual argument in `slot` with a gathered
// temporary, appending the gather to `before` and, when the dummy may
// be written, the scatter to `after`. Returns true when it did.
bool GpuOffloadVisitor::gather_strided_section_arg(const Location &loc,
        SymbolTable *block_scope, ASR::expr_t **slot, bool writable,
        std::vector<ASR::stmt_t*> &before,
        std::vector<ASR::stmt_t*> &after) {
    ASR::ArrayPhysicalCast_t *cast = nullptr;
    ASR::expr_t *inner = *slot;
    while (inner && ASR::is_a<ASR::ArrayPhysicalCast_t>(*inner)) {
        cast = ASR::down_cast<ASR::ArrayPhysicalCast_t>(inner);
        inner = cast->m_arg;
    }
    if (!inner || !ASR::is_a<ASR::ArraySection_t>(*inner)) return false;
    ASR::ArraySection_t *as = ASR::down_cast<ASR::ArraySection_t>(inner);
    if (!section_is_strided(as)) return false;
    if (!strided_section_is_gatherable(as)) return false;
    std::vector<int> range_dims;
    for (size_t d = 0; d < as->n_args; d++) {
        if (as->m_args[d].m_left && as->m_args[d].m_right
                && as->m_args[d].m_step) {
            range_dims.push_back((int)d);
        }
    }
    if (range_dims.empty()) return false;

    // Every extent has to fold to a constant: the gathered buffer is a
    // kernel-local array, and a device function cannot declare one
    // whose size is only known per thread.
    Vec<ASR::expr_t*> extents;
    extents.reserve(al, range_dims.size());
    for (int d : range_dims) {
        int64_t n = 0;
        const_section_extent(as->m_args[d], n);
        extents.push_back(al, int32_const(loc, (int)n));
    }
    ASR::ttype_t *elem_type = ASRUtils::extract_type(
        ASRUtils::expr_type(as->m_v));
    ASR::expr_t *tmp = declare_temp_array(loc, block_scope, elem_type,
        extents.p, extents.n, "__gpu_gather");

    before.push_back(build_section_copy_loops(loc, block_scope, as,
        range_dims, tmp, true));
    if (writable) {
        after.push_back(build_section_copy_loops(loc, block_scope, as,
            range_dims, tmp, false));
    }
    // The temporary is contiguous, so no physical-type cast is left to
    // make: the dummy takes the array as it stands.
    *slot = tmp;
    (void)cast;
    return true;
}

// Rewrite every strided section actual argument of every call in
// `stmt`, collecting the gather and scatter statements that have to
// bracket it.
bool GpuOffloadVisitor::gather_strided_sections_in_stmt(ASR::stmt_t *stmt,
        SymbolTable *block_scope, std::vector<ASR::stmt_t*> &before,
        std::vector<ASR::stmt_t*> &after) {
    bool changed = false;
    const Location &loc = stmt->base.loc;
    auto do_call = [&](ASR::symbol_t *name, ASR::call_arg_t *args,
            size_t n_args) {
        ASR::symbol_t *resolved =
            ASRUtils::symbol_get_past_external(name);
        ASR::Function_t *fn =
            (resolved && ASR::is_a<ASR::Function_t>(*resolved))
                ? ASR::down_cast<ASR::Function_t>(resolved) : nullptr;
        for (size_t i = 0; i < n_args; i++) {
            if (!args[i].m_value) continue;
            if (gather_strided_section_arg(loc, block_scope,
                    &args[i].m_value, dummy_is_written(fn, i),
                    before, after)) {
                changed = true;
            }
        }
    };
    if (ASR::is_a<ASR::SubroutineCall_t>(*stmt)) {
        ASR::SubroutineCall_t *sc =
            ASR::down_cast<ASR::SubroutineCall_t>(stmt);
        do_call(sc->m_name, sc->m_args, sc->n_args);
    }
    GpuCallSiteCollector csc;
    csc.visit_stmt(*stmt);
    for (const ASR::FunctionCall_t *c : csc.calls) {
        ASR::FunctionCall_t *fc = const_cast<ASR::FunctionCall_t*>(c);
        do_call(fc->m_name, fc->m_args, fc->n_args);
    }
    return changed;
}

// True when some call in `body` takes a strided section this pass
// cannot gather. Passing it on would drop the stride silently, so the
// loop is declined for offload instead, while the body is untouched.
bool GpuOffloadVisitor::body_has_ungatherable_strided_section(
        ASR::stmt_t **body,
        size_t n_body) {
    for (size_t si = 0; si < n_body; si++) {
        ASR::stmt_t *stmt = body[si];
        if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
            ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
            if (body_has_ungatherable_strided_section(dl->m_body,
                    dl->n_body)) return true;
            continue;
        }
        if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::BlockCall_t>(stmt)->m_m);
            if (b && ASR::is_a<ASR::Block_t>(*b)) {
                ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                if (body_has_ungatherable_strided_section(blk->m_body,
                        blk->n_body)) return true;
            }
            continue;
        }
        if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::AssociateBlockCall_t>(stmt)->m_m);
            if (b && ASR::is_a<ASR::AssociateBlock_t>(*b)) {
                ASR::AssociateBlock_t *ab =
                    ASR::down_cast<ASR::AssociateBlock_t>(b);
                if (body_has_ungatherable_strided_section(ab->m_body,
                        ab->n_body)) return true;
            }
            continue;
        }
        std::vector<ASR::call_arg_t*> arg_lists;
        std::vector<size_t> arg_counts;
        if (ASR::is_a<ASR::SubroutineCall_t>(*stmt)) {
            ASR::SubroutineCall_t *sc =
                ASR::down_cast<ASR::SubroutineCall_t>(stmt);
            arg_lists.push_back(sc->m_args);
            arg_counts.push_back(sc->n_args);
        }
        GpuCallSiteCollector csc;
        csc.visit_stmt(*stmt);
        for (const ASR::FunctionCall_t *c : csc.calls) {
            arg_lists.push_back(c->m_args);
            arg_counts.push_back(c->n_args);
        }
        for (size_t li = 0; li < arg_lists.size(); li++) {
            for (size_t i = 0; i < arg_counts[li]; i++) {
                if (!arg_lists[li][i].m_value) continue;
                ASR::ArraySection_t *as = strided_section_actual(
                    arg_lists[li][i].m_value);
                if (as && !strided_section_is_gatherable(as)) {
                    return true;
                }
            }
        }
    }
    return false;
}

void GpuOffloadVisitor::gather_strided_section_arguments(
        ParallelLoopNest &nest) {
    NestBodyWriteBack back(nest);
    gather_strided_sections_in_body(back.body, back.n_body,
        current_scope);
}

// `scope` owns the statements in `body`: the new BLOCK has to be
// registered there, not in the procedure, or it will not resolve from
// the BlockCall that replaces the statement.
void GpuOffloadVisitor::gather_strided_sections_in_body(ASR::stmt_t** &body,
        size_t &n_body, SymbolTable *scope) {
    Vec<ASR::stmt_t*> new_body;
    new_body.reserve(al, n_body);
    bool changed = false;
    for (size_t si = 0; si < n_body; si++) {
        ASR::stmt_t *stmt = body[si];
        if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
            ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
            gather_strided_sections_in_body(dl->m_body, dl->n_body,
                scope);
            new_body.push_back(al, stmt);
            continue;
        }
        if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::BlockCall_t>(stmt)->m_m);
            if (b && ASR::is_a<ASR::Block_t>(*b)) {
                ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                gather_strided_sections_in_body(blk->m_body,
                    blk->n_body, blk->m_symtab);
            }
            new_body.push_back(al, stmt);
            continue;
        }
        if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::AssociateBlockCall_t>(stmt)->m_m);
            if (b && ASR::is_a<ASR::AssociateBlock_t>(*b)) {
                ASR::AssociateBlock_t *ab =
                    ASR::down_cast<ASR::AssociateBlock_t>(b);
                gather_strided_sections_in_body(ab->m_body,
                    ab->n_body, ab->m_symtab);
            }
            new_body.push_back(al, stmt);
            continue;
        }
        // The gathered buffers and their loop counters live in a
        // BLOCK scope nested inside the loop. A variable owned by such
        // a scope travels into the kernel with the block instead of
        // becoming a kernel parameter, which is what makes the buffer
        // per-thread: one shared buffer written by every thread would
        // be a race.
        SymbolTable *block_scope = al.make_new<SymbolTable>(scope);
        std::vector<ASR::stmt_t*> before, after;
        if (!gather_strided_sections_in_stmt(stmt, block_scope, before,
                after)) {
            new_body.push_back(al, stmt);
            continue;
        }
        Vec<ASR::stmt_t*> block_body;
        block_body.reserve(al, before.size() + after.size() + 1);
        for (ASR::stmt_t *s : before) block_body.push_back(al, s);
        block_body.push_back(al, stmt);
        for (ASR::stmt_t *s : after) block_body.push_back(al, s);
        std::string block_name = scope->get_unique_name(
            "__gpu_gather_scope");
        ASR::asr_t *block = ASR::make_Block_t(al, stmt->base.loc,
            block_scope, s2c(al, block_name), block_body.p,
            block_body.n);
        block_scope->asr_owner = block;
        ASR::symbol_t *block_sym =
            ASR::down_cast<ASR::symbol_t>(block);
        scope->add_symbol(block_name, block_sym);
        if (scope == current_scope) {
            kernel_blocks.push_back(block_sym);
        }
        new_body.push_back(al, ASRUtils::STMT(ASR::make_BlockCall_t(
            al, stmt->base.loc, -1, block_sym)));
        changed = true;
    }
    if (changed) {
        body = new_body.p;
        n_body = new_body.n;
    }
}

// Inline whole-array assignments whose RHS contains ArraySection
// wrapped in elemental operations (e.g., b = abs(a(:,l))).
// Replaces:
//   b = abs(a(:,l))
// With:
//   do __gpu_elem_i = lbound(a,1), ubound(a,1)
//     b(__gpu_elem_i) = abs(a(__gpu_elem_i, l))
//   end do
void GpuOffloadVisitor::inline_elemental_array_var_assignment(
        ParallelLoopNest &nest) {
    bool changed = false;
    NestBodyWriteBack back(nest);
    inline_elemental_array_var_in_body(back.body, back.n_body, changed);
}

void GpuOffloadVisitor::inline_elemental_array_var_in_body(ASR::stmt_t** &body,
        size_t &n_body, bool &changed) {
    Vec<ASR::stmt_t*> new_body;
    new_body.reserve(al, n_body * 2);

    for (size_t si = 0; si < n_body; si++) {
        ASR::stmt_t *stmt = body[si];
        // Recurse into DoLoop bodies
        if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
            ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
            inline_elemental_array_var_in_body(dl->m_body, dl->n_body,
                changed);
            new_body.push_back(al, stmt);
            continue;
        }
        // Recurse into BlockCall bodies
        if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
            ASR::BlockCall_t *bc =
                ASR::down_cast<ASR::BlockCall_t>(stmt);
            if (ASR::is_a<ASR::Block_t>(*bc->m_m)) {
                ASR::Block_t *block =
                    ASR::down_cast<ASR::Block_t>(bc->m_m);
                inline_elemental_array_var_in_body(block->m_body,
                    block->n_body, changed);
            }
            new_body.push_back(al, stmt);
            continue;
        }
        // Recurse into AssociateBlockCall bodies
        if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
            ASR::AssociateBlockCall_t *abc =
                ASR::down_cast<ASR::AssociateBlockCall_t>(stmt);
            ASR::AssociateBlock_t *ab =
                ASR::down_cast<ASR::AssociateBlock_t>(abc->m_m);
            inline_elemental_array_var_in_body(ab->m_body,
                ab->n_body, changed);
            new_body.push_back(al, stmt);
            continue;
        }
        if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
            new_body.push_back(al, stmt);
            continue;
        }
        ASR::Assignment_t *asgn = ASR::down_cast<ASR::Assignment_t>(stmt);

        // An array constructor is not an elementwise expression: its
        // i-th element does not come from the i-th element of each
        // operand. Rewriting `r = [a, b]` as `r(i) = [a, b]` would
        // make the backend emit a whole constructor per element. The
        // Metal backend already expands a whole-array constructor
        // assignment into element writes, so leave it alone.
        {
            ASR::expr_t *rhs = asgn->m_value;
            while (rhs && ASR::is_a<ASR::ArrayPhysicalCast_t>(*rhs)) {
                rhs = ASR::down_cast<ASR::ArrayPhysicalCast_t>(
                    rhs)->m_arg;
            }
            if (rhs && ASR::is_a<ASR::ArrayConstructor_t>(*rhs)) {
                new_body.push_back(al, stmt);
                continue;
            }
        }

        // Only handle Var targets with array type
        if (!ASR::is_a<ASR::Var_t>(*asgn->m_target)) {
            new_body.push_back(al, stmt);
            continue;
        }
        ASR::ttype_t *target_type = ASRUtils::type_get_past_allocatable(
            ASRUtils::expr_type(asgn->m_target));
        if (!ASR::is_a<ASR::Array_t>(*target_type)) {
            new_body.push_back(al, stmt);
            continue;
        }

        // Walk the RHS to find the first ArraySection
        ASR::ArraySection_t *first_as = nullptr;
        std::function<void(ASR::expr_t*)> find_array_section =
            [&](ASR::expr_t *e) {
            if (first_as) return;
            if (ASR::is_a<ASR::ArraySection_t>(*e)) {
                first_as = ASR::down_cast<ASR::ArraySection_t>(e);
            } else if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*e)) {
                ASR::IntrinsicElementalFunction_t *f =
                    ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e);
                for (size_t i = 0; i < f->n_args; i++) {
                    if (f->m_args[i]) find_array_section(f->m_args[i]);
                }
            } else if (ASR::is_a<ASR::FunctionCall_t>(*e)) {
                ASR::FunctionCall_t *fc =
                    ASR::down_cast<ASR::FunctionCall_t>(e);
                for (size_t i = 0; i < fc->n_args; i++) {
                    if (fc->m_args[i].m_value)
                        find_array_section(fc->m_args[i].m_value);
                }
            } else if (ASR::is_a<ASR::RealBinOp_t>(*e)) {
                ASR::RealBinOp_t *rb = ASR::down_cast<ASR::RealBinOp_t>(e);
                find_array_section(rb->m_left);
                find_array_section(rb->m_right);
            } else if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
                ASR::IntegerBinOp_t *ib = ASR::down_cast<ASR::IntegerBinOp_t>(e);
                find_array_section(ib->m_left);
                find_array_section(ib->m_right);
            } else if (ASR::is_a<ASR::RealUnaryMinus_t>(*e)) {
                find_array_section(
                    ASR::down_cast<ASR::RealUnaryMinus_t>(e)->m_arg);
            } else if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*e)) {
                find_array_section(
                    ASR::down_cast<ASR::IntegerUnaryMinus_t>(e)->m_arg);
            } else if (ASR::is_a<ASR::RealCompare_t>(*e)) {
                ASR::RealCompare_t *rc = ASR::down_cast<ASR::RealCompare_t>(e);
                find_array_section(rc->m_left);
                find_array_section(rc->m_right);
            } else if (ASR::is_a<ASR::IntegerCompare_t>(*e)) {
                ASR::IntegerCompare_t *ic = ASR::down_cast<ASR::IntegerCompare_t>(e);
                find_array_section(ic->m_left);
                find_array_section(ic->m_right);
            } else if (ASR::is_a<ASR::LogicalCompare_t>(*e)) {
                ASR::LogicalCompare_t *lc = ASR::down_cast<ASR::LogicalCompare_t>(e);
                find_array_section(lc->m_left);
                find_array_section(lc->m_right);
            } else if (ASR::is_a<ASR::LogicalBinOp_t>(*e)) {
                ASR::LogicalBinOp_t *lb = ASR::down_cast<ASR::LogicalBinOp_t>(e);
                find_array_section(lb->m_left);
                find_array_section(lb->m_right);
            } else if (ASR::is_a<ASR::ArrayBroadcast_t>(*e)) {
                ASR::ArrayBroadcast_t *ab = ASR::down_cast<ASR::ArrayBroadcast_t>(e);
                find_array_section(ab->m_array);
            } else if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
                ASR::ArrayPhysicalCast_t *apc =
                    ASR::down_cast<ASR::ArrayPhysicalCast_t>(e);
                find_array_section(apc->m_arg);
            }
        };
        find_array_section(asgn->m_value);

        if (!first_as) {
            if (ASR::is_a<ASR::ArrayBroadcast_t>(*asgn->m_value)) {
                // Handle whole-array broadcast assignment:
                //   x = 1.0  (Var_array = ArrayBroadcast(scalar))
                // Convert to: do i = 1, size(x); x(i) = 1.0; end do
                ASR::ArrayBroadcast_t *ab =
                    ASR::down_cast<ASR::ArrayBroadcast_t>(asgn->m_value);
                ASR::expr_t *scalar_value = ab->m_array;

                Location loc = stmt->base.loc;
                ASR::ttype_t *int_type = ASRUtils::TYPE(
                    ASR::make_Integer_t(al, loc, 4));
                ASR::ttype_t *elem_type =
                    ASRUtils::extract_type(target_type);

                ASR::Array_t *arr =
                    ASR::down_cast<ASR::Array_t>(target_type);
                ASR::dimension_t *dims = arr->m_dims;
                size_t n_dims = arr->n_dims;

                SymbolTable *var_scope = current_scope;
                while (var_scope && var_scope->asr_owner &&
                       var_scope->asr_owner->type ==
                           ASR::asrType::symbol &&
                       ASR::is_a<ASR::AssociateBlock_t>(
                           *ASR::down_cast<ASR::symbol_t>(
                               var_scope->asr_owner))) {
                    var_scope = var_scope->parent;
                }

                auto make_bc_loop_var =
                    [&](const std::string &prefix) -> ASR::expr_t* {
                    std::string name =
                        var_scope->get_unique_name(prefix);
                    ASR::symbol_t *sym = gpu_new_variable(al, loc,
                        var_scope, name, ASRUtils::duplicate_type(al,
                            int_type));
                    return ASRUtils::EXPR(
                        ASR::make_Var_t(al, loc, sym));
                };

                std::vector<ASR::expr_t*> loop_vars;
                for (size_t d = 0; d < n_dims; d++) {
                    loop_vars.push_back(
                        make_bc_loop_var("__gpu_bc_i"));
                }

                Vec<ASR::array_index_t> lhs_args;
                lhs_args.reserve(al, n_dims);
                for (size_t d = 0; d < n_dims; d++) {
                    ASR::array_index_t idx;
                    idx.loc = loc;
                    idx.m_left = nullptr;
                    idx.m_right = loop_vars[d];
                    idx.m_step = nullptr;
                    lhs_args.push_back(al, idx);
                }
                ASR::expr_t *lhs_item = ASRUtils::EXPR(
                    ASR::make_ArrayItem_t(al, loc, asgn->m_target,
                        lhs_args.p, lhs_args.n, elem_type,
                        ASR::arraystorageType::ColMajor, nullptr));

                Vec<ASR::stmt_t*> innermost_body;
                innermost_body.reserve(al, 1);
                innermost_body.push_back(al, ASRUtils::STMT(
                    ASR::make_Assignment_t(al, loc, lhs_item,
                        scalar_value, nullptr, false, false)));

                ASR::stmt_t *loop_nest = nullptr;
                for (int d = (int)n_dims - 1; d >= 0; d--) {
                    ASR::do_loop_head_t head;
                    head.loc = loc;
                    head.m_v = loop_vars[d];
                    set_loop_head_bounds(al, loc, head, dims, (size_t)d,
                        asgn->m_target);
                    head.m_increment = nullptr;
                    if (loop_nest == nullptr) {
                        loop_nest = ASRUtils::STMT(
                            ASR::make_DoLoop_t(al, loc, nullptr,
                                head, innermost_body.p,
                                innermost_body.n, nullptr, 0));
                    } else {
                        Vec<ASR::stmt_t*> outer_body;
                        outer_body.reserve(al, 1);
                        outer_body.push_back(al, loop_nest);
                        loop_nest = ASRUtils::STMT(
                            ASR::make_DoLoop_t(al, loc, nullptr,
                                head, outer_body.p, outer_body.n,
                                nullptr, 0));
                    }
                }
                new_body.push_back(al, loop_nest);
                changed = true;
                continue;
            }

            // Handle whole-array elemental assignment without
            // ArraySection (e.g., a = obj%eval(z) where eval is
            // elemental and z is a whole-array Var).
            // Convert to:
            //   do i = 1, size(a); a(i) = obj%eval(z(i)); end do
            ASR::ttype_t *rhs_type =
                ASRUtils::type_get_past_allocatable(
                    ASRUtils::expr_type(asgn->m_value));
            if (!ASR::is_a<ASR::Array_t>(*rhs_type)) {
                new_body.push_back(al, stmt);
                continue;
            }

            // Skip decomposition for non-elemental FunctionCalls
            // that return arrays (e.g., a = f() where f returns
            // a whole array). Only elemental operations can be
            // safely decomposed into element-wise loops.
            if (ASR::is_a<ASR::FunctionCall_t>(*asgn->m_value)) {
                ASR::FunctionCall_t *fc =
                    ASR::down_cast<ASR::FunctionCall_t>(
                        asgn->m_value);
                if (!ASRUtils::is_elemental(fc->m_name)) {
                    new_body.push_back(al, stmt);
                    continue;
                }
            }

            ASR::Array_t *target_arr =
                ASR::down_cast<ASR::Array_t>(target_type);

            Location loc = stmt->base.loc;
            ASR::ttype_t *int_type = ASRUtils::TYPE(
                ASR::make_Integer_t(al, loc, 4));
            ASR::ttype_t *elem_type =
                ASRUtils::extract_type(target_type);
            ASR::dimension_t *dims = target_arr->m_dims;
            size_t n_dims = target_arr->n_dims;

            SymbolTable *var_scope = current_scope;
            while (var_scope && var_scope->asr_owner &&
                   var_scope->asr_owner->type ==
                       ASR::asrType::symbol &&
                   ASR::is_a<ASR::AssociateBlock_t>(
                       *ASR::down_cast<ASR::symbol_t>(
                           var_scope->asr_owner))) {
                var_scope = var_scope->parent;
            }

            auto make_elem_loop_var =
                [&](const std::string &prefix) -> ASR::expr_t* {
                std::string name =
                    var_scope->get_unique_name(prefix);
                ASR::symbol_t *sym = gpu_new_variable(al, loc, var_scope,
                    name, ASRUtils::duplicate_type(al, int_type));
                return ASRUtils::EXPR(
                    ASR::make_Var_t(al, loc, sym));
            };

            std::vector<ASR::expr_t*> loop_vars;
            for (size_t d = 0; d < n_dims; d++) {
                loop_vars.push_back(
                    make_elem_loop_var("__gpu_elem_i"));
            }

            // Elementize: replace array-typed Vars with ArrayItem
            std::function<ASR::expr_t*(ASR::expr_t*)> elementize =
                [&](ASR::expr_t *e) -> ASR::expr_t* {
                if (ASR::is_a<ASR::Var_t>(*e)) {
                    ASR::ttype_t *vtype =
                        ASRUtils::type_get_past_allocatable(
                            ASRUtils::expr_type(e));
                    if (ASR::is_a<ASR::Array_t>(*vtype)) {
                        ASR::ttype_t *velem =
                            ASRUtils::extract_type(vtype);
                        ASR::Array_t *va =
                            ASR::down_cast<ASR::Array_t>(vtype);
                        Vec<ASR::array_index_t> idx_args;
                        idx_args.reserve(al, va->n_dims);
                        for (size_t d = 0; d < va->n_dims; d++) {
                            ASR::array_index_t idx;
                            idx.loc = loc;
                            idx.m_left = nullptr;
                            idx.m_right = loop_vars[
                                d < loop_vars.size() ? d : 0];
                            idx.m_step = nullptr;
                            idx_args.push_back(al, idx);
                        }
                        return ASRUtils::EXPR(
                            ASR::make_ArrayItem_t(al, loc, e,
                                idx_args.p, idx_args.n, velem,
                                ASR::arraystorageType::ColMajor,
                                nullptr));
                    }
                    return e;
                } else if (ASR::is_a<ASR::FunctionCall_t>(*e)) {
                    ASR::FunctionCall_t *fc =
                        ASR::down_cast<ASR::FunctionCall_t>(e);
                    Vec<ASR::call_arg_t> new_args;
                    new_args.reserve(al, fc->n_args);
                    for (size_t i = 0; i < fc->n_args; i++) {
                        ASR::call_arg_t arg;
                        arg.loc = fc->m_args[i].loc;
                        arg.m_value = fc->m_args[i].m_value
                            ? elementize(fc->m_args[i].m_value)
                            : nullptr;
                        new_args.push_back(al, arg);
                    }
                    ASR::ttype_t *ret_type =
                        ASRUtils::extract_type(
                            ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(
                        ASR::make_FunctionCall_t(al, loc,
                            fc->m_name, fc->m_original_name,
                            new_args.p, new_args.n, ret_type,
                            fc->m_value, fc->m_dt));
                } else if (ASR::is_a<
                        ASR::IntrinsicElementalFunction_t>(*e)) {
                    ASR::IntrinsicElementalFunction_t *f =
                        ASR::down_cast<
                            ASR::IntrinsicElementalFunction_t>(e);
                    Vec<ASR::expr_t*> new_args;
                    new_args.reserve(al, f->n_args);
                    for (size_t i = 0; i < f->n_args; i++) {
                        new_args.push_back(al,
                            f->m_args[i]
                                ? elementize(f->m_args[i])
                                : nullptr);
                    }
                    ASR::ttype_t *ret_type =
                        ASRUtils::extract_type(
                            ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(
                        ASR::make_IntrinsicElementalFunction_t(
                            al, loc, f->m_intrinsic_id,
                            new_args.p, new_args.n,
                            f->m_overload_id, ret_type,
                            f->m_value));
                } else if (ASR::is_a<ASR::RealBinOp_t>(*e)) {
                    ASR::RealBinOp_t *rb =
                        ASR::down_cast<ASR::RealBinOp_t>(e);
                    ASR::ttype_t *ret_type =
                        ASRUtils::extract_type(
                            ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(
                        ASR::make_RealBinOp_t(al, loc,
                            elementize(rb->m_left), rb->m_op,
                            elementize(rb->m_right), ret_type,
                            nullptr));
                } else if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
                    ASR::IntegerBinOp_t *ib =
                        ASR::down_cast<ASR::IntegerBinOp_t>(e);
                    ASR::ttype_t *ret_type =
                        ASRUtils::extract_type(
                            ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(
                        ASR::make_IntegerBinOp_t(al, loc,
                            elementize(ib->m_left), ib->m_op,
                            elementize(ib->m_right), ret_type,
                            nullptr));
                } else if (ASR::is_a<ASR::RealUnaryMinus_t>(*e)) {
                    ASR::RealUnaryMinus_t *u =
                        ASR::down_cast<ASR::RealUnaryMinus_t>(e);
                    ASR::ttype_t *ret_type =
                        ASRUtils::extract_type(
                            ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(
                        ASR::make_RealUnaryMinus_t(al, loc,
                            elementize(u->m_arg), ret_type,
                            nullptr));
                } else if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*e)) {
                    ASR::IntegerUnaryMinus_t *u =
                        ASR::down_cast<ASR::IntegerUnaryMinus_t>(e);
                    ASR::ttype_t *ret_type =
                        ASRUtils::extract_type(
                            ASRUtils::expr_type(e));
                    return ASRUtils::EXPR(
                        ASR::make_IntegerUnaryMinus_t(al, loc,
                            elementize(u->m_arg), ret_type,
                            nullptr));
                } else if (ASR::is_a<ASR::ArrayBroadcast_t>(*e)) {
                    return ASR::down_cast<ASR::ArrayBroadcast_t>(
                        e)->m_array;
                } else if (ASR::is_a<
                        ASR::ArrayPhysicalCast_t>(*e)) {
                    return elementize(
                        ASR::down_cast<ASR::ArrayPhysicalCast_t>(
                            e)->m_arg);
                } else if (ASR::is_a<
                        ASR::StructInstanceMember_t>(*e)) {
                    ASR::ttype_t *mtype =
                        ASRUtils::type_get_past_allocatable(
                            ASRUtils::expr_type(e));
                    if (ASR::is_a<ASR::Array_t>(*mtype)) {
                        ASR::ttype_t *melem =
                            ASRUtils::extract_type(mtype);
                        ASR::Array_t *ma =
                            ASR::down_cast<ASR::Array_t>(mtype);
                        Vec<ASR::array_index_t> idx_args;
                        idx_args.reserve(al, ma->n_dims);
                        for (size_t d = 0; d < ma->n_dims; d++) {
                            ASR::array_index_t idx;
                            idx.loc = loc;
                            idx.m_left = nullptr;
                            idx.m_right = loop_vars[
                                d < loop_vars.size() ? d : 0];
                            idx.m_step = nullptr;
                            idx_args.push_back(al, idx);
                        }
                        return ASRUtils::EXPR(
                            ASR::make_ArrayItem_t(al, loc, e,
                                idx_args.p, idx_args.n, melem,
                                ASR::arraystorageType::ColMajor,
                                nullptr));
                    }
                    return e;
                }
                return e;
            };

            Vec<ASR::array_index_t> lhs_args;
            lhs_args.reserve(al, n_dims);
            for (size_t d = 0; d < n_dims; d++) {
                ASR::array_index_t idx;
                idx.loc = loc;
                idx.m_left = nullptr;
                idx.m_right = loop_vars[d];
                idx.m_step = nullptr;
                lhs_args.push_back(al, idx);
            }
            ASR::expr_t *lhs_item = ASRUtils::EXPR(
                ASR::make_ArrayItem_t(al, loc, asgn->m_target,
                    lhs_args.p, lhs_args.n, elem_type,
                    ASR::arraystorageType::ColMajor, nullptr));

            ASR::expr_t *rhs_item = elementize(asgn->m_value);

            Vec<ASR::stmt_t*> innermost_body;
            innermost_body.reserve(al, 1);
            innermost_body.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, lhs_item,
                    rhs_item, nullptr, false, false)));

            ASR::stmt_t *loop_nest = nullptr;
            for (int d = (int)n_dims - 1; d >= 0; d--) {
                ASR::do_loop_head_t head;
                head.loc = loc;
                head.m_v = loop_vars[d];
                set_loop_head_bounds(al, loc, head, dims, (size_t)d,
                    asgn->m_target);
                head.m_increment = nullptr;
                if (loop_nest == nullptr) {
                    loop_nest = ASRUtils::STMT(
                        ASR::make_DoLoop_t(al, loc, nullptr,
                            head, innermost_body.p,
                            innermost_body.n, nullptr, 0));
                } else {
                    Vec<ASR::stmt_t*> outer_body;
                    outer_body.reserve(al, 1);
                    outer_body.push_back(al, loop_nest);
                    loop_nest = ASRUtils::STMT(
                        ASR::make_DoLoop_t(al, loc, nullptr,
                            head, outer_body.p, outer_body.n,
                            nullptr, 0));
                }
            }
            new_body.push_back(al, loop_nest);
            changed = true;
            continue;
        }

        // Find the range dimension
        int range_dim = -1;
        for (size_t i = 0; i < first_as->n_args; i++) {
            if (first_as->m_args[i].m_left && first_as->m_args[i].m_right
                    && first_as->m_args[i].m_step) {
                if (range_dim != -1) {
                    range_dim = -1;
                    break;
                }
                range_dim = (int)i;
            }
        }
        if (range_dim == -1) {
            new_body.push_back(al, stmt);
            continue;
        }

        Location loc = stmt->base.loc;
        ASR::ttype_t *int_type = ASRUtils::TYPE(
            ASR::make_Integer_t(al, loc, 4));

        ASR::expr_t *loop_start = int32_const(loc, 1);
        ASR::expr_t *loop_end = section_extent(loc,
            first_as->m_args[range_dim]);

        // Create loop variable in the containing function/program scope
        SymbolTable *var_scope = current_scope;
        while (var_scope && var_scope->asr_owner &&
               var_scope->asr_owner->type == ASR::asrType::symbol &&
               ASR::is_a<ASR::AssociateBlock_t>(
                   *ASR::down_cast<ASR::symbol_t>(
                       var_scope->asr_owner))) {
            var_scope = var_scope->parent;
        }
        std::string loop_var_name = var_scope->get_unique_name(
            "__gpu_elem_i");
        ASR::symbol_t *loop_var_sym = gpu_new_variable(al, loc, var_scope,
            loop_var_name, ASRUtils::duplicate_type(al, int_type));
        ASR::expr_t *loop_var = ASRUtils::EXPR(
            ASR::make_Var_t(al, loc, loop_var_sym));

        // Elementize: replace ArraySection with ArrayItem, recurse
        // into elemental wrappers
        std::function<ASR::expr_t*(ASR::expr_t*)> elementize =
            [&](ASR::expr_t *e) -> ASR::expr_t* {
            if (ASR::is_a<ASR::ArraySection_t>(*e)) {
                ASR::ArraySection_t *as =
                    ASR::down_cast<ASR::ArraySection_t>(e);
                Vec<ASR::array_index_t> new_args;
                new_args.reserve(al, as->n_args);
                for (size_t i = 0; i < as->n_args; i++) {
                    ASR::array_index_t idx;
                    idx.loc = as->m_args[i].loc;
                    if (as->m_args[i].m_left && as->m_args[i].m_right
                            && as->m_args[i].m_step) {
                        idx.m_left = nullptr;
                        idx.m_right = section_index(loc,
                            as->m_args[i], loop_var);
                        idx.m_step = nullptr;
                    } else {
                        idx.m_left = as->m_args[i].m_left;
                        idx.m_right = as->m_args[i].m_right;
                        idx.m_step = as->m_args[i].m_step;
                    }
                    new_args.push_back(al, idx);
                }
                ASR::ttype_t *elem_type = ASRUtils::extract_type(
                    ASRUtils::expr_type(as->m_v));
                return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc,
                    as->m_v, new_args.p, new_args.n,
                    elem_type, ASR::arraystorageType::ColMajor, nullptr));
            } else if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*e)) {
                ASR::IntrinsicElementalFunction_t *f =
                    ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e);
                Vec<ASR::expr_t*> new_args;
                new_args.reserve(al, f->n_args);
                for (size_t i = 0; i < f->n_args; i++) {
                    new_args.push_back(al,
                        f->m_args[i] ? elementize(f->m_args[i])
                                     : nullptr);
                }
                ASR::ttype_t *elem_type = ASRUtils::extract_type(
                    ASRUtils::expr_type(e));
                return ASRUtils::EXPR(
                    ASR::make_IntrinsicElementalFunction_t(al, loc,
                        f->m_intrinsic_id, new_args.p, new_args.n,
                        f->m_overload_id, elem_type, f->m_value));
            } else if (ASR::is_a<ASR::RealBinOp_t>(*e)) {
                ASR::RealBinOp_t *rb =
                    ASR::down_cast<ASR::RealBinOp_t>(e);
                ASR::ttype_t *elem_type = ASRUtils::extract_type(
                    ASRUtils::expr_type(e));
                return ASRUtils::EXPR(ASR::make_RealBinOp_t(al, loc,
                    elementize(rb->m_left), rb->m_op,
                    elementize(rb->m_right), elem_type, nullptr));
            } else if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
                ASR::IntegerBinOp_t *ib =
                    ASR::down_cast<ASR::IntegerBinOp_t>(e);
                ASR::ttype_t *elem_type = ASRUtils::extract_type(
                    ASRUtils::expr_type(e));
                return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                    elementize(ib->m_left), ib->m_op,
                    elementize(ib->m_right), elem_type, nullptr));
            } else if (ASR::is_a<ASR::RealUnaryMinus_t>(*e)) {
                ASR::RealUnaryMinus_t *u =
                    ASR::down_cast<ASR::RealUnaryMinus_t>(e);
                ASR::ttype_t *elem_type = ASRUtils::extract_type(
                    ASRUtils::expr_type(e));
                return ASRUtils::EXPR(ASR::make_RealUnaryMinus_t(al, loc,
                    elementize(u->m_arg), elem_type, nullptr));
            } else if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*e)) {
                ASR::IntegerUnaryMinus_t *u =
                    ASR::down_cast<ASR::IntegerUnaryMinus_t>(e);
                ASR::ttype_t *elem_type = ASRUtils::extract_type(
                    ASRUtils::expr_type(e));
                return ASRUtils::EXPR(ASR::make_IntegerUnaryMinus_t(al, loc,
                    elementize(u->m_arg), elem_type, nullptr));
            } else if (ASR::is_a<ASR::RealCompare_t>(*e)) {
                ASR::RealCompare_t *rc =
                    ASR::down_cast<ASR::RealCompare_t>(e);
                ASR::ttype_t *elem_type = ASRUtils::extract_type(
                    ASRUtils::expr_type(e));
                return ASRUtils::EXPR(ASR::make_RealCompare_t(al, loc,
                    elementize(rc->m_left), rc->m_op,
                    elementize(rc->m_right), elem_type, nullptr));
            } else if (ASR::is_a<ASR::IntegerCompare_t>(*e)) {
                ASR::IntegerCompare_t *ic =
                    ASR::down_cast<ASR::IntegerCompare_t>(e);
                ASR::ttype_t *elem_type = ASRUtils::extract_type(
                    ASRUtils::expr_type(e));
                return ASRUtils::EXPR(ASR::make_IntegerCompare_t(al, loc,
                    elementize(ic->m_left), ic->m_op,
                    elementize(ic->m_right), elem_type, nullptr));
            } else if (ASR::is_a<ASR::LogicalCompare_t>(*e)) {
                ASR::LogicalCompare_t *lc =
                    ASR::down_cast<ASR::LogicalCompare_t>(e);
                ASR::ttype_t *elem_type = ASRUtils::extract_type(
                    ASRUtils::expr_type(e));
                return ASRUtils::EXPR(ASR::make_LogicalCompare_t(al, loc,
                    elementize(lc->m_left), lc->m_op,
                    elementize(lc->m_right), elem_type, nullptr));
            } else if (ASR::is_a<ASR::FunctionCall_t>(*e)) {
            } else if (ASR::is_a<ASR::ArrayBroadcast_t>(*e)) {
                return ASR::down_cast<ASR::ArrayBroadcast_t>(e)->m_array;
            } else if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
                return elementize(
                    ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg);
            }
            return e;
        };

        // Build LHS ArrayItem: b(loop_var)
        ASR::ttype_t *elem_type = ASRUtils::extract_type(target_type);
        Vec<ASR::array_index_t> lhs_args;
        lhs_args.reserve(al, 1);
        ASR::array_index_t lhs_idx;
        lhs_idx.loc = loc;
        lhs_idx.m_left = nullptr;
        lhs_idx.m_right = loop_var;
        lhs_idx.m_step = nullptr;
        lhs_args.push_back(al, lhs_idx);
        ASR::expr_t *lhs_item = ASRUtils::EXPR(
            ASR::make_ArrayItem_t(al, loc, asgn->m_target,
                lhs_args.p, lhs_args.n, elem_type,
                ASR::arraystorageType::ColMajor, nullptr));

        // Build RHS: elementize the value expression
        ASR::expr_t *rhs_item = elementize(asgn->m_value);

        // Build loop body: lhs_item = rhs_item
        Vec<ASR::stmt_t*> loop_body;
        loop_body.reserve(al, 1);
        loop_body.push_back(al, ASRUtils::STMT(
            ASR::make_Assignment_t(al, loc, lhs_item, rhs_item,
                nullptr, false, false)));

        // Build DoLoop
        ASR::do_loop_head_t head;
        head.loc = loc;
        head.m_v = loop_var;
        head.m_start = loop_start;
        head.m_end = loop_end;
        head.m_increment = nullptr;
        new_body.push_back(al, ASRUtils::STMT(
            ASR::make_DoLoop_t(al, loc, nullptr,
                head, loop_body.p, loop_body.n, nullptr, 0)));

        changed = true;
    }

    if (changed) {
        body = new_body.p;
        n_body = new_body.n;
    }
}

} // namespace LCompilers
