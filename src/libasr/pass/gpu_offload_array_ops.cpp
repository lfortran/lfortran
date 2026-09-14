#include <functional>
#include <map>
#include <set>
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
// that would need one is an error.
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
// cannot give a per-thread home. Called before any of the inline_*
// helpers rewrite the body.
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
// from is still whole-array.
void GpuOffloadVisitor::size_scope_array_temporaries(
        ASR::stmt_t **body, size_t n_body) {
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
            size_scope_array_temporaries(dl->m_body, dl->n_body);
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
            var->m_type = ASRUtils::TYPE(ASR::make_Array_t(al, vloc,
                arr->m_type, dims.p, dims.n,
                ASR::array_physical_typeType::DescriptorArray,
                ASR::memory_spaceType::Global));
        }
        size_scope_array_temporaries(inner_body, inner_n_body);
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

// The integer an extent expression folds to, if it is a constant.
static bool section_int_constant(ASR::expr_t *e, int64_t &out) {
    while (e && ASR::is_a<ASR::Cast_t>(*e)) {
        e = ASR::down_cast<ASR::Cast_t>(e)->m_arg;
    }
    if (!e) return false;
    ASR::expr_t *value = ASRUtils::expr_value(e);
    if (value) e = value;
    if (!ASR::is_a<ASR::IntegerConstant_t>(*e)) return false;
    out = ASR::down_cast<ASR::IntegerConstant_t>(e)->m_n;
    return true;
}

// Whether two designators name the same array: the same variable, or the
// same component of the same designator.
static bool same_designator(ASR::expr_t *x, ASR::expr_t *y) {
    if (!x || !y || x->type != y->type) return false;
    if (ASR::is_a<ASR::Var_t>(*x)) {
        return ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(x)->m_v)
            == ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(y)->m_v);
    }
    if (ASR::is_a<ASR::StructInstanceMember_t>(*x)) {
        ASR::StructInstanceMember_t *mx =
            ASR::down_cast<ASR::StructInstanceMember_t>(x);
        ASR::StructInstanceMember_t *my =
            ASR::down_cast<ASR::StructInstanceMember_t>(y);
        return ASRUtils::symbol_get_past_external(mx->m_m)
                == ASRUtils::symbol_get_past_external(my->m_m)
            && same_designator(mx->m_v, my->m_v);
    }
    return false;
}

// Whether `e` is the lower or upper bound of dimension `d` (0-based) of the
// array `base` itself -- how `:` is spelled.
static bool is_bound_of(ASR::expr_t *e, ASR::expr_t *base, size_t d,
        ASR::arrayboundType kind) {
    while (e && ASR::is_a<ASR::Cast_t>(*e)) {
        e = ASR::down_cast<ASR::Cast_t>(e)->m_arg;
    }
    if (!e || !ASR::is_a<ASR::ArrayBound_t>(*e)) return false;
    ASR::ArrayBound_t *bound = ASR::down_cast<ASR::ArrayBound_t>(e);
    int64_t dim;
    if (bound->m_bound != kind || !bound->m_dim
            || !section_int_constant(bound->m_dim, dim)
            || dim != (int64_t) d + 1) {
        return false;
    }
    return same_designator(bound->m_v, base);
}

// Whether dimension `d` of the section runs over the whole of that dimension
// of its base, one element at a time, as far as can be told before run time.
static bool section_dim_is_whole(const ASR::ArraySection_t *as, size_t d) {
    const ASR::array_index_t &index = as->m_args[d];
    if (!index.m_left || !index.m_right || !index.m_step) return false;
    int64_t step;
    if (!section_int_constant(index.m_step, step) || step != 1) return false;
    if (is_bound_of(index.m_left, as->m_v, d, ASR::arrayboundType::LBound)
            && is_bound_of(index.m_right, as->m_v, d,
                ASR::arrayboundType::UBound)) {
        return true;
    }
    ASR::dimension_t *dims = nullptr;
    size_t n_dims = ASRUtils::extract_dimensions_from_ttype(
        ASRUtils::expr_type(as->m_v), dims);
    int64_t start, length, left, right;
    return d < n_dims && dims[d].m_start && dims[d].m_length
        && section_int_constant(dims[d].m_start, start)
        && section_int_constant(dims[d].m_length, length)
        && section_int_constant(index.m_left, left)
        && section_int_constant(index.m_right, right)
        && left == start && right == start + length - 1;
}

// Whether the section's elements are not adjacent in its base: a dimension
// stepped by other than one, or a range behind a dimension that does not run
// over the whole base -- the row `a(i,:)` of a column-major matrix advances
// by a column per element. Handed to a device function as a base pointer,
// such a section would be read as if it were contiguous. A dimension with a
// single element does not advance, so `a(1:i,1:1)` is contiguous.
bool GpuOffloadVisitor::section_is_noncontiguous(
        const ASR::ArraySection_t *as) {
    for (size_t i = 0; i < as->n_args; i++) {
        if (!as->m_args[i].m_left || !as->m_args[i].m_right
                || !as->m_args[i].m_step) {
            continue;
        }
        int64_t n;
        if (const_section_extent(as->m_args[i], n) && n == 1) continue;
        int64_t step;
        if (!section_int_constant(as->m_args[i].m_step, step) || step != 1) {
            return true;
        }
        for (size_t e = 0; e < i; e++) {
            if (!section_dim_is_whole(as, e)) return true;
        }
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

// The non-contiguous section under any physical casts of an actual
// argument, or nullptr when the argument is not one. A section stepped
// by one can still skip elements of its base -- the row `a(i,:)` --
// and a base pointer would drop that stride just the same.
ASR::ArraySection_t* GpuOffloadVisitor::strided_section_actual(
        ASR::expr_t *e) {
    while (e && ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
        e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
    }
    if (!e || !ASR::is_a<ASR::ArraySection_t>(*e)) return nullptr;
    ASR::ArraySection_t *as = ASR::down_cast<ASR::ArraySection_t>(e);
    return section_is_noncontiguous(as) ? as : nullptr;
}

// Can this section be gathered into a contiguous temporary? The base
// has to be a designator the copy loops can index. An extent only known
// at run time is fine: the temporary is then sized per thread by the
// workspace machinery (see gather_strided_section_arg for how its extents
// are chosen, and body_has_varying_leading_section_extent for the one
// shape that cannot be sized on the host).
bool GpuOffloadVisitor::strided_section_is_gatherable(
        ASR::ArraySection_t *as) {
    if (!ASR::is_a<ASR::Var_t>(*as->m_v)
            && !ASR::is_a<ASR::StructInstanceMember_t>(*as->m_v)) {
        return false;
    }
    for (size_t d = 0; d < as->n_args; d++) {
        if (as->m_args[d].m_left && as->m_args[d].m_right
                && as->m_args[d].m_step) {
            return true;
        }
    }
    return false;
}

// Replace a strided section actual argument in `slot` with a gathered
// temporary, appending the gather to `before` and, when the dummy may
// be written, the scatter to `after`. Returns true when it did.
bool GpuOffloadVisitor::gather_strided_section_arg(const Location &loc,
        SymbolTable *block_scope, ASR::expr_t **slot, bool writable,
        const GpuVaries &varies, std::vector<ASR::stmt_t*> &before,
        std::vector<ASR::stmt_t*> &after) {
    ASR::ArrayPhysicalCast_t *cast = nullptr;
    ASR::expr_t *inner = *slot;
    while (inner && ASR::is_a<ASR::ArrayPhysicalCast_t>(*inner)) {
        cast = ASR::down_cast<ASR::ArrayPhysicalCast_t>(inner);
        inner = cast->m_arg;
    }
    if (!inner || !ASR::is_a<ASR::ArraySection_t>(*inner)) return false;
    ASR::ArraySection_t *as = ASR::down_cast<ASR::ArraySection_t>(inner);
    // A section stepped by one whose elements are still not adjacent -- a
    // row of a matrix -- is gathered too.
    if (!section_is_noncontiguous(as)) return false;
    if (!strided_section_is_gatherable(as)) return false;
    std::vector<int> range_dims;
    for (size_t d = 0; d < as->n_args; d++) {
        if (as->m_args[d].m_left && as->m_args[d].m_right
                && as->m_args[d].m_step) {
            range_dims.push_back((int)d);
        }
    }
    if (range_dims.empty()) return false;

    // An extent that folds to a constant makes the buffer an ordinary
    // kernel-local array. One only known at run time makes it a
    // run-time sized local of the BLOCK below, which the workspace
    // machinery binds to a per-thread slice of a buffer the host sizes
    // before the launch, where the loop index has no value. So a last
    // dimension whose extent changes with the iteration, `a(i,1:i)`, is
    // sized as the whole of that base dimension, and the callee is
    // handed the leading part of the buffer. An extent that does not
    // change is used as it is. An earlier dimension has to keep its exact
    // extent, or that part would not be contiguous;
    // offloadable_before_rewrites declines a loop in which such an
    // extent changes with the iteration.
    ASR::ttype_t *int_type = ASRUtils::TYPE(
        ASR::make_Integer_t(al, loc, 4));
    ASRUtils::ExprStmtDuplicator dup(al);
    Vec<ASR::expr_t*> extents;
    extents.reserve(al, range_dims.size());
    Vec<ASR::array_index_t> part;
    part.reserve(al, range_dims.size());
    bool partial = false;
    for (size_t k = 0; k < range_dims.size(); k++) {
        int d = range_dims[k];
        int64_t n = 0;
        ASR::expr_t *extent;
        if (const_section_extent(as->m_args[d], n)) {
            extent = int32_const(loc, (int)n);
            extents.push_back(al, extent);
        } else {
            extent = section_extent(loc, as->m_args[d]);
            const ASR::array_index_t &index = as->m_args[d];
            if (k + 1 == range_dims.size()
                    && !section_dim_is_whole(as, d)
                    && (varies(index.m_left) || varies(index.m_right)
                        || varies(index.m_step))) {
                ASR::array_index_t whole;
                whole.loc = loc;
                whole.m_left = ASRUtils::EXPR(ASR::make_ArrayBound_t(al,
                    loc, dup.duplicate_expr(as->m_v),
                    int32_const(loc, d + 1), int_type,
                    ASR::arrayboundType::LBound, nullptr));
                whole.m_right = ASRUtils::EXPR(ASR::make_ArrayBound_t(al,
                    loc, dup.duplicate_expr(as->m_v),
                    int32_const(loc, d + 1), int_type,
                    ASR::arrayboundType::UBound, nullptr));
                whole.m_step = int32_const(loc, 1);
                extents.push_back(al, section_extent(loc, whole));
                partial = true;
            } else {
                extents.push_back(al, extent);
            }
        }
        ASR::array_index_t idx;
        idx.loc = loc;
        idx.m_left = int32_const(loc, 1);
        idx.m_right = dup.duplicate_expr(extent);
        idx.m_step = int32_const(loc, 1);
        part.push_back(al, idx);
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
    if (!partial) {
        // The temporary is contiguous, so no physical-type cast is left
        // to make: the dummy takes the array as it stands.
        *slot = tmp;
        return true;
    }
    // The leading part of the temporary has the shape the section had,
    // and is handed over the way that section was.
    ASR::expr_t *leading = ASRUtils::EXPR(ASR::make_ArraySection_t(al, loc,
        tmp, part.p, part.n, ASRUtils::duplicate_type(al, as->m_type),
        nullptr));
    if (cast) {
        cast->m_arg = leading;
    } else {
        *slot = leading;
    }
    return true;
}

namespace {

class GpuIterationVaryingSymbols;

// What `body` changes (see GpuIterationVaryingSymbols), and whether an
// expression reads any of it.
std::shared_ptr<GpuIterationVaryingSymbols> gpu_symbols_changed_in(
    ASR::stmt_t **body, size_t n_body);
// A symbol in `ignored` does not count as changed.
bool gpu_reads_changed(const GpuIterationVaryingSymbols &changed,
    ASR::expr_t *e, const std::set<ASR::symbol_t*> *ignored = nullptr);
// Whether a procedure `e` calls may read a changed symbol without being
// handed it: one of a module, or of a scope the procedure is nested in.
bool gpu_calls_may_read_changed(const GpuIterationVaryingSymbols &changed,
    ASR::expr_t *e);

// A condition under which a call is evaluated: an arm of a conditional
// expression (the test in `test`, taken when it is `holds`), or a FORALL
// that runs at least once (`head`, when `test` is nullptr). The slots are
// the construct's own, so that a value can be computed once before it.
struct GpuSectionGuard {
    ASR::expr_t **test;
    bool holds;
    ASR::do_loop_head_t *head;
};

// The slots of the expressions a guard evaluates.
std::vector<ASR::expr_t**> gpu_guard_slots(const GpuSectionGuard &guard) {
    if (guard.test) return {guard.test};
    return {&guard.head->m_start, &guard.head->m_end,
        &guard.head->m_increment};
}

// The index of a FORALL head, or nullptr.
ASR::symbol_t* gpu_head_index(const ASR::do_loop_head_t &head) {
    if (!head.m_v || !ASR::is_a<ASR::Var_t>(*head.m_v)) return nullptr;
    return ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(head.m_v)->m_v);
}

// Whether an expression calls a procedure that is not known to be pure.
class GpuImpureCallFinder :
        public ASR::BaseWalkVisitor<GpuImpureCallFinder> {
public:
    bool found = false;

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_name);
        if (!s || !ASR::is_a<ASR::Function_t>(*s)) {
            found = true;
        } else {
            ASR::FunctionType_t *ft = ASRUtils::get_FunctionType(
                ASR::down_cast<ASR::Function_t>(s));
            if (!ft->m_pure && !ft->m_elemental) found = true;
        }
        ASR::BaseWalkVisitor<GpuImpureCallFinder>::visit_FunctionCall(x);
    }
};

bool gpu_calls_impure(ASR::expr_t *e) {
    GpuImpureCallFinder finder;
    finder.visit_expr(*e);
    return finder.found;
}

// Whether an expression reads the variable `sym`.
class GpuSymbolReadFinder :
        public ASR::BaseWalkVisitor<GpuSymbolReadFinder> {
public:
    ASR::symbol_t *sym;
    bool found = false;

    explicit GpuSymbolReadFinder(ASR::symbol_t *s) : sym(s) {}

    void visit_Var(const ASR::Var_t &x) {
        if (ASRUtils::symbol_get_past_external(x.m_v) == sym) found = true;
    }
};

bool gpu_expr_reads_symbol(ASR::expr_t *e, ASR::symbol_t *sym) {
    if (!e || !sym) return false;
    GpuSymbolReadFinder finder(sym);
    finder.visit_expr(*e);
    return finder.found;
}

// Makes every variable of `subs` read its replacement instead.
class GpuSubstituteSymbols :
        public ASR::BaseExprReplacer<GpuSubstituteSymbols> {
public:
    const std::map<ASR::symbol_t*, ASR::symbol_t*> &subs;

    explicit GpuSubstituteSymbols(
            const std::map<ASR::symbol_t*, ASR::symbol_t*> &s) : subs(s) {}

    void replace_Var(ASR::Var_t *x) {
        auto it = subs.find(ASRUtils::symbol_get_past_external(x->m_v));
        if (it != subs.end()) x->m_v = it->second;
    }
};

// A call among whose actual arguments a section may have to be gathered,
// and where it is evaluated. `site` is `Statement` when the call is
// evaluated once, right at the statement; otherwise it names the
// construct that evaluates it (for the message when no gather fits).
// `guards` are the conditions under which it is evaluated, outermost
// first. `changed`, when set, is what the construct changes between two
// evaluations of the call. `opaque` marks a call no gather before the
// statement can serve.
struct GpuSectionCall {
    ASR::symbol_t *name;
    ASR::call_arg_t *args;
    size_t n_args;
    GpuSectionSite site;
    std::vector<GpuSectionGuard> guards;
    std::shared_ptr<GpuIterationVaryingSymbols> changed;
    bool opaque;
};

// A statement list nested in a construct, and the scope that owns it:
// nullptr when that is the scope that owns the construct itself.
struct GpuNestedStmts {
    ASR::stmt_t ***body;
    size_t *n_body;
    SymbolTable *scope;
};

// Every function and subroutine call in an expression or a statement,
// with the site each one is evaluated at: `Statement` unless it sits in
// an arm of a conditional expression or in the values of an implied DO,
// and the guards it is evaluated under. `nested` tells whether the walk
// went into a statement other than `root`, the one it started from. Of
// the statements nested in `root`, only a FORALL and the assignment it
// makes are followed; a call in any other one is opaque.
class GpuSectionCallCollector :
        public ASRUtils::BlockBodyWalkVisitor<GpuSectionCallCollector> {
    using Base = ASRUtils::BlockBodyWalkVisitor<GpuSectionCallCollector>;
    GpuSectionSite site = GpuSectionSite::Statement;
    std::vector<GpuSectionGuard> guards;
    int opaque = 0;

    template <typename F>
    void at(GpuSectionSite inner, F &&walk) {
        GpuSectionSite outer = site;
        if (site == GpuSectionSite::Statement) site = inner;
        walk();
        site = outer;
    }

    template <typename F>
    void under(GpuSectionGuard guard, F &&walk) {
        guards.push_back(guard);
        walk();
        guards.pop_back();
    }

    void add(ASR::symbol_t *name, ASR::call_arg_t *args, size_t n_args) {
        calls.push_back({name, args, n_args, site, guards, nullptr,
            opaque > 0});
    }

public:
    const ASR::stmt_t *root = nullptr;
    bool nested = false;
    std::vector<GpuSectionCall> calls;

    void visit_stmt(const ASR::stmt_t &x) {
        if (&x == root) {
            Base::visit_stmt(x);
            return;
        }
        nested = true;
        bool followed = ASR::is_a<ASR::Assignment_t>(x)
            || ASR::is_a<ASR::ForAllSingle_t>(x);
        if (!followed) opaque++;
        Base::visit_stmt(x);
        if (!followed) opaque--;
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        ASR::FunctionCall_t &fc = const_cast<ASR::FunctionCall_t&>(x);
        add(fc.m_name, fc.m_args, fc.n_args);
        Base::visit_FunctionCall(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        ASR::SubroutineCall_t &sc = const_cast<ASR::SubroutineCall_t&>(x);
        add(sc.m_name, sc.m_args, sc.n_args);
        Base::visit_SubroutineCall(x);
    }

    void visit_IfExp(const ASR::IfExp_t &x) {
        visit_expr(*x.m_test);
        ASR::IfExp_t &ie = const_cast<ASR::IfExp_t&>(x);
        at(GpuSectionSite::ConditionalExpression, [&]() {
            under({&ie.m_test, true, nullptr}, [&]() {
                visit_expr(*x.m_body);
            });
            under({&ie.m_test, false, nullptr}, [&]() {
                visit_expr(*x.m_orelse);
            });
        });
    }

    void visit_ImpliedDoLoop(const ASR::ImpliedDoLoop_t &x) {
        visit_expr(*x.m_start);
        visit_expr(*x.m_end);
        if (x.m_increment) visit_expr(*x.m_increment);
        opaque++;
        at(GpuSectionSite::ImpliedDo, [&]() {
            for (size_t i = 0; i < x.n_values; i++) {
                visit_expr(*x.m_values[i]);
            }
        });
        opaque--;
    }

    // The head is evaluated once; the assignment on every index, and not
    // at all when the FORALL runs no times.
    void visit_ForAllSingle(const ASR::ForAllSingle_t &x) {
        visit_expr(*x.m_head.m_start);
        visit_expr(*x.m_head.m_end);
        if (x.m_head.m_increment) visit_expr(*x.m_head.m_increment);
        ASR::ForAllSingle_t &fa = const_cast<ASR::ForAllSingle_t&>(x);
        under({nullptr, true, &fa.m_head}, [&]() {
            visit_stmt(*x.m_assign_stmt);
        });
    }
};

// Add the calls, evaluated at `site` unless the collector found a more
// specific one, with what the construct changes between evaluations. An
// arm of a conditional expression in a construct that evaluates it again
// is taken to be at that construct, which is what can keep a copy before
// it from serving the call.
void gpu_file_section_calls(std::vector<GpuSectionCall> &found,
        GpuSectionSite site,
        const std::shared_ptr<GpuIterationVaryingSymbols> &changed,
        std::vector<GpuSectionCall> &calls) {
    for (GpuSectionCall &call : found) {
        if (call.site == GpuSectionSite::Statement
                || (changed
                    && call.site == GpuSectionSite::ConditionalExpression)) {
            call.site = site;
        }
        call.changed = changed;
        calls.push_back(call);
    }
}

void gpu_add_section_calls(ASR::expr_t *e, GpuSectionSite site,
        const std::shared_ptr<GpuIterationVaryingSymbols> &changed,
        std::vector<GpuSectionCall> &calls) {
    if (!e) return;
    GpuSectionCallCollector csc;
    csc.visit_expr(*e);
    gpu_file_section_calls(csc.calls, site, changed, calls);
}

// The calls `stmt` makes itself, and the statement lists nested in it.
// The gather for a section actual argument reads the section's bounds
// when it runs, and they may read the index of an inner loop or a value
// tested by an IF or a SELECT CASE, so it has to run next to the
// innermost statement that makes the call. The statement lists of a loop,
// an IF, a SELECT CASE, a BLOCK or an ASSOCIATE therefore go to `nested`,
// to be taken one statement at a time. A statement with no nested
// statements makes all the calls in it. So does a construct that has no
// statement a gather could be put in front of: a DO WHILE for its
// condition, which is evaluated again on every pass, and a FORALL, whose
// assignment is evaluated on every index. Such calls carry what the
// construct changes in between, and a gather before the construct serves
// them only when the section reads none of it (see
// gpu_section_gather_placeable). Any other construct -- WHERE, SELECT
// TYPE, SELECT RANK, or one added later -- makes every call it contains
// opaque, so that a section in it is refused rather than gathered outside
// the branches its bounds may read.
void gpu_split_section_calls(ASR::stmt_t *stmt,
        std::vector<GpuNestedStmts> &nested,
        std::vector<GpuSectionCall> &calls) {
    const GpuSectionSite here = GpuSectionSite::Statement;
    const std::shared_ptr<GpuIterationVaryingSymbols> once;
    if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
        ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(stmt);
        gpu_add_section_calls(x->m_head.m_start, here, once, calls);
        gpu_add_section_calls(x->m_head.m_end, here, once, calls);
        gpu_add_section_calls(x->m_head.m_increment, here, once, calls);
        nested.push_back({&x->m_body, &x->n_body, nullptr});
        nested.push_back({&x->m_orelse, &x->n_orelse, nullptr});
    } else if (ASR::is_a<ASR::DoConcurrentLoop_t>(*stmt)) {
        ASR::DoConcurrentLoop_t *x =
            ASR::down_cast<ASR::DoConcurrentLoop_t>(stmt);
        for (size_t i = 0; i < x->n_head; i++) {
            gpu_add_section_calls(x->m_head[i].m_start, here, once, calls);
            gpu_add_section_calls(x->m_head[i].m_end, here, once, calls);
            gpu_add_section_calls(x->m_head[i].m_increment, here, once,
                calls);
        }
        nested.push_back({&x->m_body, &x->n_body, nullptr});
    } else if (ASR::is_a<ASR::If_t>(*stmt)) {
        ASR::If_t *x = ASR::down_cast<ASR::If_t>(stmt);
        gpu_add_section_calls(x->m_test, here, once, calls);
        nested.push_back({&x->m_body, &x->n_body, nullptr});
        nested.push_back({&x->m_orelse, &x->n_orelse, nullptr});
    } else if (ASR::is_a<ASR::Select_t>(*stmt)) {
        ASR::Select_t *x = ASR::down_cast<ASR::Select_t>(stmt);
        gpu_add_section_calls(x->m_test, here, once, calls);
        for (size_t i = 0; i < x->n_body; i++) {
            ASR::case_stmt_t *c = x->m_body[i];
            if (ASR::is_a<ASR::CaseStmt_t>(*c)) {
                ASR::CaseStmt_t *cs = ASR::down_cast<ASR::CaseStmt_t>(c);
                for (size_t t = 0; t < cs->n_test; t++) {
                    gpu_add_section_calls(cs->m_test[t], here, once, calls);
                }
                nested.push_back({&cs->m_body, &cs->n_body, nullptr});
            } else {
                ASR::CaseStmt_Range_t *cr =
                    ASR::down_cast<ASR::CaseStmt_Range_t>(c);
                gpu_add_section_calls(cr->m_start, here, once, calls);
                gpu_add_section_calls(cr->m_end, here, once, calls);
                nested.push_back({&cr->m_body, &cr->n_body, nullptr});
            }
        }
        nested.push_back({&x->m_default, &x->n_default, nullptr});
    } else if (ASR::is_a<ASR::WhileLoop_t>(*stmt)) {
        ASR::WhileLoop_t *x = ASR::down_cast<ASR::WhileLoop_t>(stmt);
        gpu_add_section_calls(x->m_test, GpuSectionSite::WhileCondition,
            gpu_symbols_changed_in(x->m_body, x->n_body), calls);
        nested.push_back({&x->m_body, &x->n_body, nullptr});
        nested.push_back({&x->m_orelse, &x->n_orelse, nullptr});
    } else if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::BlockCall_t>(stmt)->m_m);
        if (b && ASR::is_a<ASR::Block_t>(*b)) {
            ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
            nested.push_back({&blk->m_body, &blk->n_body, blk->m_symtab});
        }
    } else if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::AssociateBlockCall_t>(stmt)->m_m);
        if (b && ASR::is_a<ASR::AssociateBlock_t>(*b)) {
            ASR::AssociateBlock_t *ab =
                ASR::down_cast<ASR::AssociateBlock_t>(b);
            nested.push_back({&ab->m_body, &ab->n_body, ab->m_symtab});
        }
    } else {
        GpuSectionCallCollector csc;
        csc.root = stmt;
        csc.visit_stmt(*stmt);
        if (!csc.nested) {
            gpu_file_section_calls(csc.calls, here, once, calls);
        } else if (ASR::is_a<ASR::ForAllSingle_t>(*stmt)) {
            ASR::stmt_t *stmts[1] = {stmt};
            gpu_file_section_calls(csc.calls, GpuSectionSite::Forall,
                gpu_symbols_changed_in(stmts, 1), calls);
        } else {
            GpuSectionSite site = GpuSectionSite::Construct;
            if (ASR::is_a<ASR::Where_t>(*stmt)) {
                site = GpuSectionSite::Where;
            } else if (ASR::is_a<ASR::SelectType_t>(*stmt)) {
                site = GpuSectionSite::SelectType;
            } else if (ASR::is_a<ASR::SelectRank_t>(*stmt)) {
                site = GpuSectionSite::SelectRank;
            }
            for (GpuSectionCall &call : csc.calls) {
                call.site = site;
                call.opaque = true;
            }
            gpu_file_section_calls(csc.calls, site, once, calls);
        }
    }
}

// Whether the gather of `as`, the section passed as argument `arg` of
// `call`, can be placed before the statement that makes the call: where
// the call is evaluated, with `Statement` as its site, when it can, and
// why not otherwise. Once guarded by the conditions the call is evaluated
// under, the gather copies the section the call would see, provided that
// neither the section nor those conditions read anything the construct
// changes between two evaluations of the call. A callee that may write
// the section it is handed would change it too, and so would a procedure
// a condition calls that can read a changed variable it is not handed.
// The one change a condition may read is the index of a FORALL it is
// nested in: the gather is then guarded by whether some index gives the
// condition a value that evaluates the call (see
// gather_strided_sections_in_stmt). Fortran evaluates only the arm of a
// conditional expression that is selected, and a FORALL assignment only
// for the indices it has, so the gather is not left unguarded: the
// section may not even exist where the call is not evaluated. A
// condition may be evaluated again for the gather, so a procedure it
// calls has to be pure.
//
// A write through a pointer or an associate name is not seen as a change
// to its target. Neither can reach a gather yet: such a write is lost in
// an offloaded loop (#12860), and an associate in a do while body fails
// before this pass (#12861). Fixing either has to count it here.
GpuSectionPlace gpu_section_gather_place(const GpuSectionCall &call,
        size_t arg, ASR::ArraySection_t *as) {
    GpuSectionPlace place;
    auto refuse = [&](GpuSectionConflict conflict) {
        place.site = call.site == GpuSectionSite::Statement
            ? GpuSectionSite::Construct : call.site;
        place.conflict = conflict;
        return place;
    };
    if (call.opaque) return refuse(GpuSectionConflict::NoPlace);
    const GpuIterationVaryingSymbols *changed = call.changed.get();
    auto reads_changed = [&](ASR::expr_t *e) {
        return e && changed && gpu_reads_changed(*changed, e);
    };
    if (changed) {
        ASR::symbol_t *resolved =
            ASRUtils::symbol_get_past_external(call.name);
        ASR::Function_t *fn =
            (resolved && ASR::is_a<ASR::Function_t>(*resolved))
                ? ASR::down_cast<ASR::Function_t>(resolved) : nullptr;
        bool changes = GpuOffloadVisitor::dummy_is_written(fn, arg)
            || reads_changed(as->m_v);
        for (size_t d = 0; d < as->n_args; d++) {
            changes = changes || reads_changed(as->m_args[d].m_left)
                || reads_changed(as->m_args[d].m_right)
                || reads_changed(as->m_args[d].m_step);
        }
        if (changes) return refuse(GpuSectionConflict::ValueChanges);
    }
    std::set<ASR::symbol_t*> indices;
    for (const GpuSectionGuard &guard : call.guards) {
        for (ASR::expr_t **slot : gpu_guard_slots(guard)) {
            ASR::expr_t *e = *slot;
            if (!e) continue;
            if (gpu_calls_impure(e)) {
                return refuse(GpuSectionConflict::ImpureCondition);
            }
            if (changed && (gpu_reads_changed(*changed, e, &indices)
                    || gpu_calls_may_read_changed(*changed, e))) {
                return refuse(GpuSectionConflict::ValueChanges);
            }
        }
        if (guard.head) {
            if (ASR::symbol_t *index = gpu_head_index(*guard.head)) {
                indices.insert(index);
            }
        }
    }
    return place;
}

bool gpu_section_gather_placeable(const GpuSectionCall &call, size_t arg,
        ASR::ArraySection_t *as) {
    return gpu_section_gather_place(call, arg, as).site
        == GpuSectionSite::Statement;
}

} // namespace

// Rewrite every strided section actual argument of the calls `stmt`
// makes itself (see gpu_split_section_calls), collecting the gather and
// scatter statements that have to bracket it.
bool GpuOffloadVisitor::gather_strided_sections_in_stmt(ASR::stmt_t *stmt,
        SymbolTable *block_scope, const GpuVaries &varies,
        std::vector<ASR::stmt_t*> &before,
        std::vector<ASR::stmt_t*> &after) {
    std::vector<GpuNestedStmts> nested;
    std::vector<GpuSectionCall> calls;
    gpu_split_section_calls(stmt, nested, calls);
    bool changed = false;
    const Location &loc = stmt->base.loc;
    ASRUtils::ExprStmtDuplicator dup(al);
    ASRUtils::ASRBuilder b(al, loc);
    // A copy of `e` in which the variables of `subs` read their
    // replacements.
    auto copy = [&](ASR::expr_t *e,
            const std::map<ASR::symbol_t*, ASR::symbol_t*> &subs) {
        if (!e) return e;
        ASR::expr_t *c = dup.duplicate_expr(e);
        if (!subs.empty()) {
            GpuSubstituteSymbols r(subs);
            r.current_expr = &c;
            r.replace_expr(c);
        }
        return c;
    };
    // Whether a FORALL head gives its index a value at all. A zero step
    // is not Fortran, and gives none.
    auto runs = [&](const ASR::do_loop_head_t &head,
            const std::map<ASR::symbol_t*, ASR::symbol_t*> &subs) {
        int64_t step = 1;
        if (!head.m_increment
                || (section_int_constant(head.m_increment, step)
                    && step != 0)) {
            ASR::expr_t *start = copy(head.m_start, subs);
            ASR::expr_t *end = copy(head.m_end, subs);
            return step > 0 ? b.LtE(start, end) : b.GtE(start, end);
        }
        return b.Or(
            b.And(b.Gt(copy(head.m_increment, subs), int32_const(loc, 0)),
                b.LtE(copy(head.m_start, subs), copy(head.m_end, subs))),
            b.And(b.Lt(copy(head.m_increment, subs), int32_const(loc, 0)),
                b.GtE(copy(head.m_start, subs), copy(head.m_end, subs))));
    };
    auto new_var = [&](const std::string &prefix, ASR::ttype_t *type) {
        ASR::symbol_t *sym = gpu_new_variable(al, loc, block_scope,
            block_scope->get_unique_name(prefix), type);
        return ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
    };
    auto make_if = [&](ASR::expr_t *test, bool holds,
            const std::vector<ASR::stmt_t*> &stmts) {
        Vec<ASR::stmt_t*> body;
        body.reserve(al, stmts.size());
        for (ASR::stmt_t *s : stmts) body.push_back(al, s);
        Vec<ASR::stmt_t*> none;
        none.reserve(al, 0);
        return ASRUtils::STMT(ASR::make_If_t(al, loc, nullptr, test,
            holds ? body.p : none.p, holds ? body.n : none.n,
            holds ? none.p : body.p, holds ? none.n : body.n));
    };
    const std::map<ASR::symbol_t*, ASR::symbol_t*> no_subs;
    for (GpuSectionCall &call : calls) {
        ASR::symbol_t *resolved =
            ASRUtils::symbol_get_past_external(call.name);
        ASR::Function_t *fn =
            (resolved && ASR::is_a<ASR::Function_t>(*resolved))
                ? ASR::down_cast<ASR::Function_t>(resolved) : nullptr;
        const std::vector<GpuSectionGuard> &guards = call.guards;
        const size_t n_guards = guards.size();
        // Whether a guard after the k-th reads its index.
        auto index_read_later = [&](size_t k) {
            ASR::symbol_t *index = guards[k].head
                ? gpu_head_index(*guards[k].head) : nullptr;
            for (size_t l = k + 1; index && l < n_guards; l++) {
                for (ASR::expr_t **slot : gpu_guard_slots(guards[l])) {
                    if (gpu_expr_reads_symbol(*slot, index)) return true;
                }
            }
            return false;
        };
        // The guards before `outer` read no FORALL index; from `outer`
        // on, a guard may read the index of a FORALL around it.
        size_t outer = 0;
        while (outer < n_guards && !index_read_later(outer)) outer++;
        // `stmts` under the guards before `upto`, outermost first.
        auto guarded = [&](size_t upto, std::vector<ASR::stmt_t*> stmts) {
            for (size_t k = upto; k-- > 0 && !stmts.empty();) {
                const GpuSectionGuard &guard = guards[k];
                ASR::expr_t *test = guard.test
                    ? copy(*guard.test, no_subs) : runs(*guard.head, no_subs);
                stmts = {make_if(test, !guard.test || guard.holds, stmts)};
            }
            return stmts;
        };
        // Sets `flag` when some value of the indices of the FORALLs from
        // guard `k` on gives every guard a value that evaluates the call.
        std::function<std::vector<ASR::stmt_t*>(size_t, ASR::expr_t*,
            std::map<ASR::symbol_t*, ASR::symbol_t*>&)> search;
        search = [&](size_t k, ASR::expr_t *flag,
                std::map<ASR::symbol_t*, ASR::symbol_t*> &subs)
                -> std::vector<ASR::stmt_t*> {
            if (k == n_guards) {
                return {ASRUtils::STMT(ASR::make_Assignment_t(al, loc,
                    flag, b.bool_t(true, ASRUtils::expr_type(flag)),
                    nullptr, false, false))};
            }
            const GpuSectionGuard &guard = guards[k];
            if (guard.test) {
                return {make_if(copy(*guard.test, subs), guard.holds,
                    search(k + 1, flag, subs))};
            }
            if (!index_read_later(k)) {
                return {make_if(runs(*guard.head, subs), true,
                    search(k + 1, flag, subs))};
            }
            ASR::symbol_t *index = gpu_head_index(*guard.head);
            ASR::expr_t *i = new_var("__gpu_guard_i",
                ASRUtils::duplicate_type(al,
                    ASRUtils::expr_type(guard.head->m_v)));
            ASR::do_loop_head_t head;
            head.loc = loc;
            head.m_v = i;
            head.m_start = copy(guard.head->m_start, subs);
            head.m_end = copy(guard.head->m_end, subs);
            head.m_increment = copy(guard.head->m_increment, subs);
            subs[index] = ASR::down_cast<ASR::Var_t>(i)->m_v;
            std::vector<ASR::stmt_t*> inner = search(k + 1, flag, subs);
            subs.erase(index);
            Vec<ASR::stmt_t*> body;
            body.reserve(al, inner.size());
            for (ASR::stmt_t *s : inner) body.push_back(al, s);
            return {ASRUtils::STMT(ASR::make_DoLoop_t(al, loc, nullptr,
                head, body.p, body.n, nullptr, 0))};
        };
        for (size_t i = 0; i < call.n_args; i++) {
            if (!call.args[i].m_value) continue;
            ASR::ArraySection_t *as =
                strided_section_actual(call.args[i].m_value);
            if (!as || !strided_section_is_gatherable(as)) continue;
            // offloadable_before_rewrites refused every such section of
            // the body as the user wrote it. One that a rewrite since
            // then put where no gather can serve it would lose its
            // stride.
            if (!gpu_section_gather_placeable(call, i, as)) {
                throw LCompilersException("gpu offload: a section passed "
                    "to a procedure was placed where it cannot be "
                    "gathered");
            }
            std::vector<ASR::stmt_t*> copy_in, copy_out;
            if (!gather_strided_section_arg(loc, block_scope,
                    &call.args[i].m_value, dummy_is_written(fn, i),
                    varies, copy_in, copy_out)) {
                continue;
            }
            changed = true;
            // A value an outer guard calls a procedure for is computed
            // once, where the construct would compute it, and read by
            // both the construct and the gather.
            for (size_t k = 0; k < outer; k++) {
                for (ASR::expr_t **slot : gpu_guard_slots(guards[k])) {
                    if (!*slot || !expr_has_function_call(*slot)) continue;
                    ASR::expr_t *value = new_var("__gpu_guard_value",
                        ASRUtils::duplicate_type(al,
                            ASRUtils::expr_type(*slot)));
                    std::vector<ASR::stmt_t*> set = {ASRUtils::STMT(
                        ASR::make_Assignment_t(al, loc, value, *slot,
                            nullptr, false, false))};
                    for (ASR::stmt_t *s : guarded(k, set)) {
                        before.push_back(s);
                    }
                    *slot = dup.duplicate_expr(value);
                }
            }
            if (outer < n_guards) {
                // Pure calls in these guards are evaluated again here.
                ASR::expr_t *flag = new_var("__gpu_guard",
                    ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4)));
                std::map<ASR::symbol_t*, ASR::symbol_t*> subs;
                std::vector<ASR::stmt_t*> in = {ASRUtils::STMT(
                    ASR::make_Assignment_t(al, loc, flag,
                        b.bool_t(false, ASRUtils::expr_type(flag)),
                        nullptr, false, false))};
                for (ASR::stmt_t *s : search(outer, flag, subs)) {
                    in.push_back(s);
                }
                in.push_back(make_if(dup.duplicate_expr(flag), true,
                    copy_in));
                copy_in = in;
                if (!copy_out.empty()) {
                    copy_out = {make_if(dup.duplicate_expr(flag), true,
                        copy_out)};
                }
            }
            for (ASR::stmt_t *s : guarded(outer, copy_in)) {
                before.push_back(s);
            }
            for (ASR::stmt_t *s : guarded(outer, copy_out)) {
                after.push_back(s);
            }
        }
    }
    return changed;
}

// The first strided section actual argument of a call in `body` for
// which `pred` holds, or nullptr. `pred` is also told where the call is
// evaluated: GpuSectionSite::Statement when a gather for the section can
// be placed before the statement (see gpu_section_gather_place), the
// construct that prevents it, and why, otherwise.
ASR::ArraySection_t* GpuOffloadVisitor::find_strided_section_actual(
        ASR::stmt_t **body, size_t n_body,
        const std::function<bool(ASR::ArraySection_t*,
            const GpuSectionPlace&)> &pred) {
    for (size_t si = 0; si < n_body; si++) {
        std::vector<GpuNestedStmts> nested;
        std::vector<GpuSectionCall> calls;
        gpu_split_section_calls(body[si], nested, calls);
        for (const GpuNestedStmts &n : nested) {
            if (ASR::ArraySection_t *as = find_strided_section_actual(
                    *n.body, *n.n_body, pred)) return as;
        }
        for (const GpuSectionCall &call : calls) {
            for (size_t i = 0; i < call.n_args; i++) {
                if (!call.args[i].m_value) continue;
                ASR::ArraySection_t *as = strided_section_actual(
                    call.args[i].m_value);
                if (!as) continue;
                GpuSectionPlace place;
                if (strided_section_is_gatherable(as)) {
                    place = gpu_section_gather_place(call, i, as);
                }
                if (pred(as, place)) return as;
            }
        }
    }
    return nullptr;
}

// True when some call in `body` takes a strided section this pass
// cannot gather. Passing it on would drop the stride silently, so the
// loop is declined for offload instead, while the body is untouched.
bool GpuOffloadVisitor::body_has_ungatherable_strided_section(
        ASR::stmt_t **body,
        size_t n_body) {
    return find_strided_section_actual(body, n_body,
        [&](ASR::ArraySection_t *as, const GpuSectionPlace&) {
            return !strided_section_is_gatherable(as);
        }) != nullptr;
}

namespace {

// The symbols whose value can change from one iteration of a loop to the
// next. `whole` holds those that can change as a whole -- the ones the
// body assigns, allocates, associates, counts with or passes to a dummy
// that may be written, and the index of every loop -- and `parts` those only an
// element or a component of which is written, which leaves the bounds of
// an array as they are. A symbol declared in a scope the body opens is
// new in every iteration.
class GpuIterationVaryingSymbols :
        public ASRUtils::BlockBodyWalkVisitor<GpuIterationVaryingSymbols> {
    using Base = ASRUtils::BlockBodyWalkVisitor<GpuIterationVaryingSymbols>;
public:
    std::set<ASR::symbol_t*> whole, parts;
    std::set<SymbolTable*> scopes;

    void add(ASR::expr_t *e) {
        bool is_whole = true;
        while (e) {
            if (ASR::is_a<ASR::Var_t>(*e)) {
                ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(e)->m_v);
                (is_whole ? whole : parts).insert(s);
                return;
            } else if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
                e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
            } else if (ASR::is_a<ASR::ArrayItem_t>(*e)) {
                e = ASR::down_cast<ASR::ArrayItem_t>(e)->m_v;
                is_whole = false;
            } else if (ASR::is_a<ASR::ArraySection_t>(*e)) {
                e = ASR::down_cast<ASR::ArraySection_t>(e)->m_v;
                is_whole = false;
            } else if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
                e = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v;
                is_whole = false;
            } else {
                return;
            }
        }
    }

    void visit_Assignment(const ASR::Assignment_t &x) {
        add(x.m_target);
        Base::visit_Assignment(x);
    }

    void visit_Associate(const ASR::Associate_t &x) {
        add(x.m_target);
        Base::visit_Associate(x);
    }

    void visit_DoLoop(const ASR::DoLoop_t &x) {
        add(x.m_head.m_v);
        Base::visit_DoLoop(x);
    }

    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        for (size_t i = 0; i < x.n_head; i++) add(x.m_head[i].m_v);
        Base::visit_DoConcurrentLoop(x);
    }

    void visit_ForAllSingle(const ASR::ForAllSingle_t &x) {
        add(x.m_head.m_v);
        Base::visit_ForAllSingle(x);
    }

    void visit_ImpliedDoLoop(const ASR::ImpliedDoLoop_t &x) {
        add(x.m_var);
        Base::visit_ImpliedDoLoop(x);
    }

    void visit_Allocate(const ASR::Allocate_t &x) {
        for (size_t i = 0; i < x.n_args; i++) {
            if (x.m_args[i].m_a) whole.insert(root(x.m_args[i].m_a));
        }
        Base::visit_Allocate(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        ASR::symbol_t *resolved = ASRUtils::symbol_get_past_external(
            x.m_name);
        ASR::Function_t *fn =
            (resolved && ASR::is_a<ASR::Function_t>(*resolved))
                ? ASR::down_cast<ASR::Function_t>(resolved) : nullptr;
        for (size_t i = 0; i < x.n_args; i++) {
            if (x.m_args[i].m_value
                    && GpuOffloadVisitor::dummy_is_written(fn, i)) {
                whole.insert(root(x.m_args[i].m_value));
            }
        }
        Base::visit_SubroutineCall(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        ASR::symbol_t *resolved = ASRUtils::symbol_get_past_external(
            x.m_name);
        ASR::Function_t *fn =
            (resolved && ASR::is_a<ASR::Function_t>(*resolved))
                ? ASR::down_cast<ASR::Function_t>(resolved) : nullptr;
        for (size_t i = 0; i < x.n_args; i++) {
            if (x.m_args[i].m_value
                    && GpuOffloadVisitor::dummy_is_written(fn, i)) {
                whole.insert(root(x.m_args[i].m_value));
            }
        }
        Base::visit_FunctionCall(x);
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_m);
        if (s && ASR::is_a<ASR::Block_t>(*s)) {
            scopes.insert(ASR::down_cast<ASR::Block_t>(s)->m_symtab);
        }
        Base::visit_BlockCall(x);
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_m);
        if (s && ASR::is_a<ASR::AssociateBlock_t>(*s)) {
            scopes.insert(
                ASR::down_cast<ASR::AssociateBlock_t>(s)->m_symtab);
        }
        Base::visit_AssociateBlockCall(x);
    }

    // The variable a designator is rooted at.
    static ASR::symbol_t* root(ASR::expr_t *e) {
        while (e) {
            if (ASR::is_a<ASR::Var_t>(*e)) {
                return ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(e)->m_v);
            } else if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
                e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
            } else if (ASR::is_a<ASR::ArrayItem_t>(*e)) {
                e = ASR::down_cast<ASR::ArrayItem_t>(e)->m_v;
            } else if (ASR::is_a<ASR::ArraySection_t>(*e)) {
                e = ASR::down_cast<ASR::ArraySection_t>(e)->m_v;
            } else if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
                e = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v;
            } else {
                return nullptr;
            }
        }
        return nullptr;
    }
};

// Whether an expression reads a value that changes with the iteration.
// A bound or the size of an array variable, or of a component of one,
// only changes when the array as a whole does.
class GpuIterationVaryingUse :
        public ASR::BaseWalkVisitor<GpuIterationVaryingUse> {
    const GpuIterationVaryingSymbols &varying;

    bool changes(ASR::symbol_t *s, bool whole_only) const {
        if (ignored && ignored->count(s)) return false;
        return varying.whole.count(s)
            || (!whole_only && varying.parts.count(s))
            || varying.scopes.count(ASRUtils::symbol_parent_symtab(s));
    }

    // The variable a chain of components ends at, or nullptr when the
    // chain passes through anything else.
    static ASR::symbol_t* member_chain_root(ASR::expr_t *e) {
        while (e && ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
            e = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v;
        }
        if (!e || !ASR::is_a<ASR::Var_t>(*e)) return nullptr;
        return ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(e)->m_v);
    }

public:
    bool found = false;
    const std::set<ASR::symbol_t*> *ignored = nullptr;

    explicit GpuIterationVaryingUse(const GpuIterationVaryingSymbols &v)
        : varying(v) {}

    void visit_Var(const ASR::Var_t &x) {
        if (changes(ASRUtils::symbol_get_past_external(x.m_v), false)) {
            found = true;
        }
    }

    void visit_ArrayBound(const ASR::ArrayBound_t &x) {
        if (ASR::symbol_t *s = member_chain_root(x.m_v)) {
            if (changes(s, true)) found = true;
            if (x.m_dim) visit_expr(*x.m_dim);
            return;
        }
        ASR::BaseWalkVisitor<GpuIterationVaryingUse>::visit_ArrayBound(x);
    }

    void visit_ArraySize(const ASR::ArraySize_t &x) {
        if (ASR::symbol_t *s = member_chain_root(x.m_v)) {
            if (changes(s, true)) found = true;
            if (x.m_dim) visit_expr(*x.m_dim);
            return;
        }
        ASR::BaseWalkVisitor<GpuIterationVaryingUse>::visit_ArraySize(x);
    }
};

std::shared_ptr<GpuIterationVaryingSymbols> gpu_symbols_changed_in(
        ASR::stmt_t **body, size_t n_body) {
    auto changed = std::make_shared<GpuIterationVaryingSymbols>();
    for (size_t i = 0; i < n_body; i++) {
        changed->visit_stmt(*body[i]);
    }
    return changed;
}

bool gpu_reads_changed(const GpuIterationVaryingSymbols &changed,
        ASR::expr_t *e, const std::set<ASR::symbol_t*> *ignored) {
    GpuIterationVaryingUse use(changed);
    use.ignored = ignored;
    use.visit_expr(*e);
    return use.found;
}

// The procedures an expression calls.
class GpuCalledFunctions : public ASR::BaseWalkVisitor<GpuCalledFunctions> {
public:
    std::vector<ASR::Function_t*> functions;

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_name);
        if (s && ASR::is_a<ASR::Function_t>(*s)) {
            functions.push_back(ASR::down_cast<ASR::Function_t>(s));
        }
        ASR::BaseWalkVisitor<GpuCalledFunctions>::visit_FunctionCall(x);
    }
};

bool gpu_calls_may_read_changed(const GpuIterationVaryingSymbols &changed,
        ASR::expr_t *e) {
    GpuCalledFunctions called;
    called.visit_expr(*e);
    if (called.functions.empty()) return false;
    // A procedure reaches a variable it is not handed only through a
    // module or a scope it is nested in, and so does every procedure it
    // can call in turn.
    auto reachable = [&](ASR::symbol_t *s) {
        SymbolTable *owner = ASRUtils::symbol_parent_symtab(s);
        if (!owner) return true;
        if (owner->asr_owner && ASR::is_a<ASR::symbol_t>(*owner->asr_owner)
                && ASR::is_a<ASR::Module_t>(
                    *ASR::down_cast<ASR::symbol_t>(owner->asr_owner))) {
            return true;
        }
        for (ASR::Function_t *fn : called.functions) {
            for (SymbolTable *t = fn->m_symtab; t; t = t->parent) {
                if (t == owner) return true;
            }
        }
        return false;
    };
    for (ASR::symbol_t *s : changed.whole) if (reachable(s)) return true;
    for (ASR::symbol_t *s : changed.parts) if (reachable(s)) return true;
    return false;
}

std::string section_base_name(ASR::expr_t *e) {
    if (ASR::is_a<ASR::Var_t>(*e)) {
        return ASRUtils::symbol_name(ASR::down_cast<ASR::Var_t>(e)->m_v);
    }
    if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
        ASR::StructInstanceMember_t *m =
            ASR::down_cast<ASR::StructInstanceMember_t>(e);
        return section_base_name(m->m_v) + "%"
            + ASRUtils::symbol_name(m->m_m);
    }
    if (ASR::is_a<ASR::ArrayItem_t>(*e)) {
        return section_base_name(ASR::down_cast<ASR::ArrayItem_t>(e)->m_v)
            + "(...)";
    }
    return "array";
}

} // namespace

namespace {

// Whether an expression reads a value that changes from one iteration of
// `nest` to the next: one of its indices, the index of any loop in
// `body`, or anything `body` changes.
GpuOffloadVisitor::GpuVaries gpu_iteration_varies(
        const ParallelLoopNest &nest, ASR::stmt_t **body, size_t n_body) {
    auto varying = std::make_shared<GpuIterationVaryingSymbols>();
    for (size_t d = 0; d < nest.n_heads(); d++) {
        varying->add(nest.head(d).m_v);
    }
    for (size_t i = 0; i < n_body; i++) {
        varying->visit_stmt(*body[i]);
    }
    return [varying](ASR::expr_t *e) {
        if (!e) return false;
        GpuIterationVaryingUse use(*varying);
        use.visit_expr(*e);
        return use.found;
    };
}

} // namespace

// True when a section that will be gathered has a dimension before its
// last whose extent changes with the iteration. The gathered buffer is
// sized on the host, and only its last dimension can be sized from the
// base array instead (see gather_strided_section_arg).
bool GpuOffloadVisitor::body_has_varying_leading_section_extent(
        const ParallelLoopNest &work, Location &where, std::string &name) {
    GpuVaries varies = gpu_iteration_varies(work, work.body, work.n_body);
    ASR::ArraySection_t *found = find_strided_section_actual(work.body,
        work.n_body, [&](ASR::ArraySection_t *as, const GpuSectionPlace&) {
            if (!strided_section_is_gatherable(as)) return false;
            std::vector<int> range_dims;
            for (size_t d = 0; d < as->n_args; d++) {
                if (as->m_args[d].m_left && as->m_args[d].m_right
                        && as->m_args[d].m_step) {
                    range_dims.push_back((int)d);
                }
            }
            for (size_t k = 0; k + 1 < range_dims.size(); k++) {
                const ASR::array_index_t &index = as->m_args[range_dims[k]];
                int64_t n = 0;
                if (const_section_extent(index, n)) continue;
                if (varies(index.m_left) || varies(index.m_right)
                        || varies(index.m_step)) {
                    return true;
                }
            }
            return false;
        });
    if (!found) return false;
    where = found->base.base.loc;
    name = section_base_name(found->m_v);
    return true;
}

// True when a section that would be gathered is passed to a procedure
// where no gather can be placed next to the call (see
// gpu_split_section_calls). `where`, `name` and `place` then say which
// section, where it is and why.
bool GpuOffloadVisitor::body_has_unplaceable_section(ASR::stmt_t **body,
        size_t n_body, Location &where, std::string &name,
        GpuSectionPlace &place) {
    ASR::ArraySection_t *found = find_strided_section_actual(body, n_body,
        [&](ASR::ArraySection_t *as, const GpuSectionPlace &p) {
            if (p.site == GpuSectionSite::Statement
                    || !strided_section_is_gatherable(as)) {
                return false;
            }
            place = p;
            return true;
        });
    if (!found) return false;
    where = found->base.base.loc;
    name = section_base_name(found->m_v);
    return true;
}

void GpuOffloadVisitor::gather_strided_section_arguments(
        ParallelLoopNest &nest) {
    NestBodyWriteBack back(nest);
    // Asked of the body as the rewrites before this one left it, so that
    // the loops they introduced count too.
    GpuVaries varies = gpu_iteration_varies(nest, back.body, back.n_body);
    gather_strided_sections_in_body(back.body, back.n_body,
        current_scope, varies);
}

// `scope` owns the statements in `body`: the new BLOCK has to be
// registered there, not in the procedure, or it will not resolve from
// the BlockCall that replaces the statement. Each gather brackets the
// innermost statement that makes its call (see gpu_split_section_calls).
void GpuOffloadVisitor::gather_strided_sections_in_body(ASR::stmt_t** &body,
        size_t &n_body, SymbolTable *scope, const GpuVaries &varies) {
    Vec<ASR::stmt_t*> new_body;
    new_body.reserve(al, n_body);
    bool changed = false;
    for (size_t si = 0; si < n_body; si++) {
        ASR::stmt_t *stmt = body[si];
        // The gathered buffers and their loop counters live in a
        // BLOCK scope nested inside the loop. A variable owned by such
        // a scope travels into the kernel with the block instead of
        // becoming a kernel parameter, which is what makes the buffer
        // per-thread: one shared buffer written by every thread would
        // be a race.
        SymbolTable *block_scope = al.make_new<SymbolTable>(scope);
        std::vector<ASR::stmt_t*> before, after;
        bool gathered = gather_strided_sections_in_stmt(stmt, block_scope,
            varies, before, after);
        // A construct the new BLOCK brackets moves into its scope, and so
        // do the BLOCKs made for the statements nested in it.
        std::vector<GpuNestedStmts> nested;
        std::vector<GpuSectionCall> calls;
        gpu_split_section_calls(stmt, nested, calls);
        for (const GpuNestedStmts &n : nested) {
            gather_strided_sections_in_body(*n.body, *n.n_body,
                n.scope ? n.scope : (gathered ? block_scope : scope),
                varies);
        }
        if (!gathered) {
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
