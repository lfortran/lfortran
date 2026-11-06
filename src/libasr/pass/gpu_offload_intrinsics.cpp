#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/gpu_offload_designator.h>
#include <libasr/pass/gpu_offload_rewrite.h>
#include <libasr/pass/gpu_offload_visitor.h>
#include <libasr/pass/intrinsic_array_function_registry.h>

namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

namespace {

// Whether `e` designates a whole array: a variable or a component, whose
// elements an elemental expression reads one by one.
bool gpu_is_whole_array_designator(ASR::expr_t *e) {
    if (!ASR::is_a<ASR::Var_t>(*e)
            && !ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
        return false;
    }
    return ASR::is_a<ASR::Array_t>(*ASRUtils::type_get_past_allocatable_pointer(
        ASRUtils::expr_type(e)));
}

} // namespace

// The extent of each dimension of the array an elemental expression
// evaluates to, read off its first array operand: the ranged dimensions
// of a section, or every dimension of a whole array. The loops that
// evaluate the expression element by element count each dimension from
// one to its extent, in index_type, so operands with different bounds --
// `a(2:4) + b`, with `b` counted from zero -- each pick their own element
// of the same position (see elementize_array_expr).
void GpuOffloadVisitor::find_array_section_all_bounds(ASR::expr_t *e,
        std::vector<std::pair<ASR::expr_t*, ASR::expr_t*>> &dim_bounds) {
    if (!dim_bounds.empty()) return;
    const Location &loc = e->base.loc;
    if (ASR::is_a<ASR::ArraySection_t>(*e)) {
        ASR::ArraySection_t *as = ASR::down_cast<ASR::ArraySection_t>(e);
        for (size_t i = 0; i < as->n_args; i++) {
            if (as->m_args[i].m_left && as->m_args[i].m_right) {
                dim_bounds.push_back({index_const(loc, 1),
                    section_extent(loc, as->m_args[i])});
            }
        }
    } else if (gpu_is_whole_array_designator(e)) {
        ASR::dimension_t *dims = nullptr;
        size_t rank = ASRUtils::extract_dimensions_from_ttype(
            ASRUtils::expr_type(e), dims);
        for (size_t d = 0; d < rank; d++) {
            ASR::dimension_t dim = dim_or_runtime_extent(e, dims, d,
                device_caps.offset_kind);
            dim_bounds.push_back({index_const(loc, 1),
                to_index_kind(loc, dim.m_length)});
        }
    } else if (ASR::is_a<ASR::RealCompare_t>(*e)) {
        ASR::RealCompare_t *rc = ASR::down_cast<ASR::RealCompare_t>(e);
        find_array_section_all_bounds(rc->m_left, dim_bounds);
        find_array_section_all_bounds(rc->m_right, dim_bounds);
    } else if (ASR::is_a<ASR::IntegerCompare_t>(*e)) {
        ASR::IntegerCompare_t *ic = ASR::down_cast<ASR::IntegerCompare_t>(e);
        find_array_section_all_bounds(ic->m_left, dim_bounds);
        find_array_section_all_bounds(ic->m_right, dim_bounds);
    } else if (ASR::is_a<ASR::LogicalBinOp_t>(*e)) {
        ASR::LogicalBinOp_t *lb = ASR::down_cast<ASR::LogicalBinOp_t>(e);
        find_array_section_all_bounds(lb->m_left, dim_bounds);
        find_array_section_all_bounds(lb->m_right, dim_bounds);
    } else if (ASR::is_a<ASR::RealBinOp_t>(*e)) {
        ASR::RealBinOp_t *rb = ASR::down_cast<ASR::RealBinOp_t>(e);
        find_array_section_all_bounds(rb->m_left, dim_bounds);
        find_array_section_all_bounds(rb->m_right, dim_bounds);
    } else if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
        ASR::IntegerBinOp_t *ib = ASR::down_cast<ASR::IntegerBinOp_t>(e);
        find_array_section_all_bounds(ib->m_left, dim_bounds);
        find_array_section_all_bounds(ib->m_right, dim_bounds);
    } else if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*e)) {
        ASR::IntrinsicElementalFunction_t *ief =
            ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e);
        for (size_t i = 0; i < ief->n_args; i++) {
            if (ief->m_args[i])
                find_array_section_all_bounds(ief->m_args[i], dim_bounds);
        }
    } else if (ASR::is_a<ASR::FunctionCall_t>(*e)) {
        // An elemental call evaluates to an array of the shape of its
        // array arguments (see elementize_array_expr).
        ASR::FunctionCall_t *call = ASR::down_cast<ASR::FunctionCall_t>(e);
        if (!ASRUtils::is_elemental(call->m_name)) return;
        for (size_t i = 0; i < call->n_args; i++) {
            if (call->m_args[i].m_value)
                find_array_section_all_bounds(call->m_args[i].m_value,
                    dim_bounds);
        }
    } else if (ASR::is_a<ASR::Cast_t>(*e)) {
        find_array_section_all_bounds(
            ASR::down_cast<ASR::Cast_t>(e)->m_arg, dim_bounds);
    } else if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
        find_array_section_all_bounds(
            ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg, dim_bounds);
    } else if (ASR::is_a<ASR::RealUnaryMinus_t>(*e)) {
        find_array_section_all_bounds(
            ASR::down_cast<ASR::RealUnaryMinus_t>(e)->m_arg, dim_bounds);
    } else if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*e)) {
        find_array_section_all_bounds(
            ASR::down_cast<ASR::IntegerUnaryMinus_t>(e)->m_arg, dim_bounds);
    }
}

// Build an element-wise expression by replacing every ArraySection and
// whole array with the element at the position the loop counters give,
// one counter of index_type per dimension counted from one (see
// find_array_section_all_bounds): the counter-th element of a section
// dimension, or the counter-th element of an array dimension from its
// lower bound.
ASR::expr_t* GpuOffloadVisitor::elementize_array_expr(ASR::expr_t *e,
        std::vector<ASR::expr_t*> &loop_vars, const Location &loc) {
    if (!ASRUtils::is_array(ASRUtils::expr_type(e))) return e;
    ASR::ttype_t *element_type = ASRUtils::extract_type(
        ASRUtils::expr_type(e));
    if (ASR::is_a<ASR::ArraySection_t>(*e)) {
        ASR::ArraySection_t *as = ASR::down_cast<ASR::ArraySection_t>(e);
        Vec<ASR::array_index_t> new_args;
        new_args.reserve(al, as->n_args);
        size_t lv_idx = 0;
        for (size_t i = 0; i < as->n_args; i++) {
            ASR::array_index_t idx;
            idx.loc = as->m_args[i].loc;
            if (as->m_args[i].m_left && as->m_args[i].m_right) {
                LCOMPILERS_ASSERT(lv_idx < loop_vars.size());
                idx.m_left = nullptr;
                idx.m_right = section_index(loc, as->m_args[i],
                    loop_vars[lv_idx++]);
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
            elem_type, ASR::arraystorageType::ColMajor, nullptr, false));
    } else if (gpu_is_whole_array_designator(e)) {
        ASR::ttype_t *int_type = index_type(loc);
        size_t rank = ASRUtils::extract_n_dims_from_ttype(
            ASRUtils::expr_type(e));
        LCOMPILERS_ASSERT(rank <= loop_vars.size());
        ASR::dimension_t *dims = nullptr;
        ASRUtils::extract_dimensions_from_ttype(
            ASRUtils::type_get_past_allocatable_pointer(
                ASRUtils::expr_type(e)), dims);
        Vec<ASR::array_index_t> new_args;
        new_args.reserve(al, rank);
        for (size_t d = 0; d < rank; d++) {
            ASR::array_index_t idx;
            idx.loc = loc;
            idx.m_left = nullptr;
            idx.m_step = nullptr;
            if (dims && is_int_literal(dims[d].m_start, 1)) {
                idx.m_right = loop_vars[d];
                new_args.push_back(al, idx);
                continue;
            }
            ASR::expr_t *lb = ASRUtils::EXPR(ASR::make_ArrayBound_t(al,
                loc, e, int32_const(loc, d + 1), int_type,
                ASR::arrayboundType::LBound, nullptr));
            ASR::expr_t *offset = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(
                al, loc, loop_vars[d], ASR::binopType::Sub,
                index_const(loc, 1), int_type, nullptr));
            idx.m_right = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                lb, ASR::binopType::Add, offset, int_type, nullptr));
            new_args.push_back(al, idx);
        }
        ASR::ttype_t *elem_type = ASRUtils::extract_type(
            ASRUtils::expr_type(e));
        return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc,
            e, new_args.p, new_args.n,
            elem_type, ASR::arraystorageType::ColMajor, nullptr, false));
    } else if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
        return elementize_array_expr(
            ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg,
            loop_vars, loc);
    } else if (ASR::is_a<ASR::Cast_t>(*e)) {
        ASR::Cast_t *cast = ASR::down_cast<ASR::Cast_t>(e);
        return ASRUtils::EXPR(ASR::make_Cast_t(al, loc,
            elementize_array_expr(cast->m_arg, loop_vars, loc),
            cast->m_kind, element_type, nullptr, nullptr));
    } else if (ASR::is_a<ASR::ArrayBroadcast_t>(*e)) {
        return ASR::down_cast<ASR::ArrayBroadcast_t>(e)->m_array;
    } else if (ASR::is_a<ASR::FunctionCall_t>(*e)) {
        ASR::FunctionCall_t *call = ASR::down_cast<ASR::FunctionCall_t>(e);
        if (!ASRUtils::is_elemental(call->m_name)) return e;
        Vec<ASR::call_arg_t> args;
        args.reserve(al, call->n_args);
        for (size_t i = 0; i < call->n_args; i++) {
            ASR::call_arg_t arg = call->m_args[i];
            if (arg.m_value) {
                arg.m_value = elementize_array_expr(arg.m_value,
                    loop_vars, loc);
            }
            args.push_back(al, arg);
        }
        return ASRUtils::EXPR(ASR::make_FunctionCall_t(al, loc,
            call->m_name, call->m_original_name, args.p, args.n,
            element_type, nullptr, call->m_dt));
    } else if (ASR::is_a<ASR::RealUnaryMinus_t>(*e)) {
        return ASRUtils::EXPR(ASR::make_RealUnaryMinus_t(al, loc,
            elementize_array_expr(
                ASR::down_cast<ASR::RealUnaryMinus_t>(e)->m_arg,
                loop_vars, loc), element_type, nullptr));
    } else if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*e)) {
        return ASRUtils::EXPR(ASR::make_IntegerUnaryMinus_t(al, loc,
            elementize_array_expr(
                ASR::down_cast<ASR::IntegerUnaryMinus_t>(e)->m_arg,
                loop_vars, loc), element_type, nullptr));
    } else if (ASR::is_a<ASR::LogicalNot_t>(*e)) {
        return ASRUtils::EXPR(ASR::make_LogicalNot_t(al, loc,
            elementize_array_expr(
                ASR::down_cast<ASR::LogicalNot_t>(e)->m_arg,
                loop_vars, loc), element_type, nullptr));
    } else if (ASR::is_a<ASR::LogicalCompare_t>(*e)) {
        ASR::LogicalCompare_t *cmp = ASR::down_cast<ASR::LogicalCompare_t>(e);
        return ASRUtils::EXPR(ASR::make_LogicalCompare_t(al, loc,
            elementize_array_expr(cmp->m_left, loop_vars, loc),
            cmp->m_op,
            elementize_array_expr(cmp->m_right, loop_vars, loc),
            element_type, nullptr));
    } else if (ASR::is_a<ASR::RealCompare_t>(*e)) {
        ASR::RealCompare_t *rc = ASR::down_cast<ASR::RealCompare_t>(e);
        return ASRUtils::EXPR(ASR::make_RealCompare_t(al, loc,
            elementize_array_expr(rc->m_left, loop_vars, loc),
            rc->m_op,
            elementize_array_expr(rc->m_right, loop_vars, loc),
            element_type, nullptr));
    } else if (ASR::is_a<ASR::IntegerCompare_t>(*e)) {
        ASR::IntegerCompare_t *ic = ASR::down_cast<ASR::IntegerCompare_t>(e);
        return ASRUtils::EXPR(ASR::make_IntegerCompare_t(al, loc,
            elementize_array_expr(ic->m_left, loop_vars, loc),
            ic->m_op,
            elementize_array_expr(ic->m_right, loop_vars, loc),
            element_type, nullptr));
    } else if (ASR::is_a<ASR::LogicalBinOp_t>(*e)) {
        ASR::LogicalBinOp_t *lb = ASR::down_cast<ASR::LogicalBinOp_t>(e);
        return ASRUtils::EXPR(ASR::make_LogicalBinOp_t(al, loc,
            elementize_array_expr(lb->m_left, loop_vars, loc),
            lb->m_op,
            elementize_array_expr(lb->m_right, loop_vars, loc),
            element_type, nullptr));
    } else if (ASR::is_a<ASR::RealBinOp_t>(*e)) {
        ASR::RealBinOp_t *rb = ASR::down_cast<ASR::RealBinOp_t>(e);
        ASR::ttype_t *real_type = ASRUtils::extract_type(
            ASRUtils::expr_type(e));
        return ASRUtils::EXPR(ASR::make_RealBinOp_t(al, loc,
            elementize_array_expr(rb->m_left, loop_vars, loc),
            rb->m_op,
            elementize_array_expr(rb->m_right, loop_vars, loc),
            real_type, nullptr));
    } else if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
        ASR::IntegerBinOp_t *ib = ASR::down_cast<ASR::IntegerBinOp_t>(e);
        ASR::ttype_t *int_elem_type = ASRUtils::extract_type(
            ASRUtils::expr_type(e));
        return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
            elementize_array_expr(ib->m_left, loop_vars, loc),
            ib->m_op,
            elementize_array_expr(ib->m_right, loop_vars, loc),
            int_elem_type, nullptr));
    } else if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*e)) {
        ASR::IntrinsicElementalFunction_t *ief =
            ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e);
        Vec<ASR::expr_t*> new_args;
        new_args.reserve(al, ief->n_args);
        for (size_t i = 0; i < ief->n_args; i++) {
            new_args.push_back(al, ief->m_args[i]
                ? elementize_array_expr(ief->m_args[i], loop_vars, loc)
                : nullptr);
        }
        ASR::ttype_t *elem_type = ASRUtils::extract_type(
            ASRUtils::expr_type(e));
        return ASRUtils::EXPR(
            ASR::make_IntrinsicElementalFunction_t(al, loc,
                ief->m_intrinsic_id, new_args.p, new_args.n,
                ief->m_overload_id, elem_type, nullptr));
    }
    Vec<ASR::array_index_t> indices;
    indices.reserve(al, loop_vars.size());
    for (ASR::expr_t *counter : loop_vars) {
        ASR::array_index_t index;
        index.loc = loc;
        index.m_left = nullptr;
        index.m_right = counter;
        index.m_step = nullptr;
        indices.push_back(al, index);
    }
    return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc, e,
        indices.p, indices.n, element_type,
        ASR::arraystorageType::ColMajor, nullptr, false));
}

// Inline a single IntrinsicArrayFunction All into preamble statements
// and return a Var expression referencing the result. Returns nullptr
// if the All cannot be inlined.
ASR::expr_t* GpuOffloadVisitor::inline_single_all(
        ASR::IntrinsicArrayFunction_t *iaf,
        const Location &loc, Vec<ASR::stmt_t*> &preamble) {
    if (iaf->n_args < 1 || !iaf->m_args[0]) return nullptr;
    ASR::expr_t *mask = iaf->m_args[0];

    std::vector<std::pair<ASR::expr_t*, ASR::expr_t*>> dim_bounds;
    find_array_section_all_bounds(mask, dim_bounds);
    if (dim_bounds.empty()) return nullptr;

    ASR::ttype_t *logical_type = ASRUtils::TYPE(
        ASR::make_Logical_t(al, loc, 4));

    SymbolTable *var_scope = current_scope;
    while (var_scope && var_scope->asr_owner &&
           var_scope->asr_owner->type == ASR::asrType::symbol &&
           ASR::is_a<ASR::AssociateBlock_t>(
               *ASR::down_cast<ASR::symbol_t>(
                   var_scope->asr_owner))) {
        var_scope = var_scope->parent;
    }

    // Create loop variables for each dimension
    std::vector<ASR::expr_t*> loop_vars;
    for (size_t d = 0; d < dim_bounds.size(); d++) {
        std::string loop_var_name = var_scope->get_unique_name(
            "__gpu_all_i" + std::to_string(d));
        ASR::symbol_t *loop_var_sym = gpu_new_variable(al, loc, var_scope,
            loop_var_name, index_type(loc));
        loop_vars.push_back(ASRUtils::EXPR(
            ASR::make_Var_t(al, loc, loop_var_sym)));
    }

    // Create result variable
    std::string res_var_name = var_scope->get_unique_name("__gpu_all_res");
    ASR::symbol_t *res_var_sym = gpu_new_variable(al, loc, var_scope,
        res_var_name, ASRUtils::duplicate_type(al, logical_type));
    ASR::expr_t *res_var = ASRUtils::EXPR(
        ASR::make_Var_t(al, loc, res_var_sym));

    // __gpu_all_res = .true.
    preamble.push_back(al, ASRUtils::STMT(
        ASR::make_Assignment_t(al, loc, res_var,
            ASRUtils::EXPR(ASR::make_LogicalConstant_t(al, loc,
                true, logical_type)),
            nullptr, false, false, false, false)));

    ASR::expr_t *elem_mask = elementize_array_expr(mask, loop_vars, loc);

    // Build innermost body: if (.not. elem_mask) __gpu_all_res = .false.
    Vec<ASR::stmt_t*> if_body;
    if_body.reserve(al, 1);
    if_body.push_back(al, ASRUtils::STMT(
        ASR::make_Assignment_t(al, loc, res_var,
            ASRUtils::EXPR(ASR::make_LogicalConstant_t(al, loc,
                false, logical_type)),
            nullptr, false, false, false, false)));
    Vec<ASR::stmt_t*> if_else;
    if_else.reserve(al, 0);
    ASR::expr_t *not_mask = ASRUtils::EXPR(
        ASR::make_LogicalNot_t(al, loc, elem_mask, logical_type, nullptr));
    ASR::stmt_t *inner_stmt = ASRUtils::STMT(
        ASR::make_If_t(al, loc, nullptr, not_mask,
            if_body.p, if_body.n, if_else.p, if_else.n));

    // Build nested loops from innermost dimension outward
    ASR::stmt_t *loop_nest = inner_stmt;
    for (int d = (int)dim_bounds.size() - 1; d >= 0; d--) {
        ASR::do_loop_head_t head;
        head.loc = loc;
        head.m_v = loop_vars[d];
        head.m_start = dim_bounds[d].first;
        head.m_end = dim_bounds[d].second;
        head.m_increment = nullptr;
        Vec<ASR::stmt_t*> loop_body;
        loop_body.reserve(al, 1);
        loop_body.push_back(al, loop_nest);
        loop_nest = ASRUtils::STMT(ASRUtils::make_DoLoop_t_util(al, loc, nullptr,
            head, loop_body.p, loop_body.n, nullptr, 0));
    }
    preamble.push_back(al, loop_nest);

    return res_var;
}

// Check if an expression tree contains any IntrinsicArrayFunction All.
bool GpuOffloadVisitor::contains_intrinsic_all(ASR::expr_t *e) {
    if (!e) return false;
    if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*e)) {
        ASR::IntrinsicArrayFunction_t *iaf =
            ASR::down_cast<ASR::IntrinsicArrayFunction_t>(e);
        if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
                iaf->m_arr_intrinsic_id)
                    == ASRUtils::IntrinsicArrayFunctions::All) {
            return true;
        }
    }
    if (ASR::is_a<ASR::LogicalBinOp_t>(*e)) {
        ASR::LogicalBinOp_t *lb = ASR::down_cast<ASR::LogicalBinOp_t>(e);
        return contains_intrinsic_all(lb->m_left) ||
               contains_intrinsic_all(lb->m_right);
    }
    if (ASR::is_a<ASR::LogicalNot_t>(*e)) {
        return contains_intrinsic_all(
            ASR::down_cast<ASR::LogicalNot_t>(e)->m_arg);
    }
    return false;
}

// Recursively replace IntrinsicArrayFunction All nodes in an expression
// with temporary variables, emitting inline loops into preamble.
ASR::expr_t* GpuOffloadVisitor::replace_all_in_expr(
        ASR::expr_t *e, const Location &loc,
        Vec<ASR::stmt_t*> &preamble) {
    if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*e)) {
        ASR::IntrinsicArrayFunction_t *iaf =
            ASR::down_cast<ASR::IntrinsicArrayFunction_t>(e);
        if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
                iaf->m_arr_intrinsic_id)
                    == ASRUtils::IntrinsicArrayFunctions::All) {
            ASR::expr_t *res = inline_single_all(iaf, loc, preamble);
            if (res) return res;
        }
        return e;
    }
    if (ASR::is_a<ASR::LogicalBinOp_t>(*e)) {
        ASR::LogicalBinOp_t *lb = ASR::down_cast<ASR::LogicalBinOp_t>(e);
        ASR::expr_t *new_left = replace_all_in_expr(lb->m_left, loc,
            preamble);
        ASR::expr_t *new_right = replace_all_in_expr(lb->m_right, loc,
            preamble);
        if (new_left != lb->m_left || new_right != lb->m_right) {
            return ASRUtils::EXPR(ASR::make_LogicalBinOp_t(al, loc,
                new_left, lb->m_op, new_right, lb->m_type, nullptr));
        }
        return e;
    }
    if (ASR::is_a<ASR::LogicalNot_t>(*e)) {
        ASR::LogicalNot_t *ln = ASR::down_cast<ASR::LogicalNot_t>(e);
        ASR::expr_t *new_arg = replace_all_in_expr(ln->m_arg, loc,
            preamble);
        if (new_arg != ln->m_arg) {
            return ASRUtils::EXPR(ASR::make_LogicalNot_t(al, loc,
                new_arg, ln->m_type, nullptr));
        }
        return e;
    }
    return e;
}

// Inline IntrinsicArrayFunction All inside a parallel loop body.
// Replaces:
//   eq(l) = all(a(:,l) == b(:,l))
// or:
//   eq(l) = all(a(1:l) > 0) .and. all(b(1:l) > 0)
// With inlined loops that compute the All result into temporaries.
// This avoids complex lowered code (Associate, Allocate, FunctionCall)
// that the Metal backend cannot handle inside GPU kernels.
void GpuOffloadVisitor::inline_intrinsic_all(ParallelLoopNest &nest) {
    Vec<ASR::stmt_t*> new_body;
    new_body.reserve(al, nest.n_body * 3);
    bool changed = false;

    for (size_t si = 0; si < nest.n_body; si++) {
        ASR::stmt_t *stmt = nest.body[si];
        if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
            new_body.push_back(al, stmt);
            continue;
        }
        ASR::Assignment_t *asgn = ASR::down_cast<ASR::Assignment_t>(stmt);

        if (!contains_intrinsic_all(asgn->m_value)) {
            new_body.push_back(al, stmt);
            continue;
        }

        Location loc = stmt->base.loc;
        Vec<ASR::stmt_t*> preamble;
        preamble.reserve(al, 8);

        ASR::expr_t *new_value = replace_all_in_expr(asgn->m_value,
            loc, preamble);

        if (preamble.n > 0) {
            changed = true;
            for (size_t pi = 0; pi < preamble.n; pi++) {
                new_body.push_back(al, preamble[pi]);
            }
            new_body.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc, asgn->m_target,
                    new_value, nullptr, false, false, false, false)));
            // Track non-array scalar targets as reduction liveouts
            if (ASR::is_a<ASR::Var_t>(*asgn->m_target)) {
                ASR::ttype_t *tgt_type =
                    ASRUtils::expr_type(asgn->m_target);
                if (!ASRUtils::is_array(tgt_type)) {
                    all_reduction_targets.insert(
                        ASRUtils::symbol_name(
                            ASR::down_cast<ASR::Var_t>(
                                asgn->m_target)->m_v));
                }
            }
        } else {
            new_body.push_back(al, stmt);
        }
    }

    if (changed) {
        // Through set_body, so the loop the nest reads from carries
        // the rewrite too and not just the view of it.
        nest.set_body(new_body.p, new_body.n);
    }
}

// Inline IntrinsicArrayFunction MatMul inside a parallel loop body.
// Replaces:
//   c = matmul(a, b)
// With nested DoLoops that compute the matrix multiplication directly.
// The ordinary lowering would leave a call to the `_lcompilers_matmul`
// helper the intrinsic pass instantiates. That helper does reach the
// device -- `device_partition` marks whatever a kernel calls as device
// code, and the shader carries a definition of it -- but the call it
// leaves behind does not always carry an extent for every argument,
// and the loops below avoid the question.
// The MatMul shapes `inline_matmul_stmts` lowers on an Assignment:
// the matmul is either the whole right-hand side, or a direct operand
// of a RealBinOp on the right-hand side (`z = matmul(w, a) + b`). On a
// match of the second shape the other operand, the operator and the
// side of the matmul are reported back to the caller.
ASR::IntrinsicArrayFunction_t* GpuOffloadVisitor::match_statement_matmul(
        ASR::expr_t *value, ASR::expr_t *&binop_other,
        ASR::binopType &binop_op, bool &matmul_is_left) {
    auto is_matmul = [](ASR::expr_t *e) -> ASR::IntrinsicArrayFunction_t* {
        if (!ASR::is_a<ASR::IntrinsicArrayFunction_t>(*e)) return nullptr;
        auto *f = ASR::down_cast<ASR::IntrinsicArrayFunction_t>(e);
        if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
                f->m_arr_intrinsic_id)
                    != ASRUtils::IntrinsicArrayFunctions::MatMul) {
            return nullptr;
        }
        return f;
    };
    if (!value) return nullptr;
    if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*value)) {
        return is_matmul(value);
    }
    if (!ASR::is_a<ASR::RealBinOp_t>(*value)) return nullptr;
    ASR::RealBinOp_t *rbop = ASR::down_cast<ASR::RealBinOp_t>(value);
    ASR::expr_t *left = rbop->m_left;
    ASR::expr_t *right = rbop->m_right;
    if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*left))
        left = ASR::down_cast<ASR::ArrayPhysicalCast_t>(left)->m_arg;
    if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*right))
        right = ASR::down_cast<ASR::ArrayPhysicalCast_t>(right)->m_arg;
    if (auto *f = is_matmul(left)) {
        binop_other = rbop->m_right;
        binop_op = rbop->m_op;
        matmul_is_left = true;
        return f;
    }
    if (auto *f = is_matmul(right)) {
        binop_other = rbop->m_left;
        binop_op = rbop->m_op;
        matmul_is_left = false;
        return f;
    }
    return nullptr;
}

void GpuOffloadVisitor::inline_matmul_stmts(
        ASR::stmt_t** &body, size_t &n_body) {
    Vec<ASR::stmt_t*> new_body;
    new_body.reserve(al, n_body * 4);
    bool changed = false;

    for (size_t si = 0; si < n_body; si++) {
        ASR::stmt_t *stmt = body[si];
        if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
            ASR::DoLoop_t &dl = *ASR::down_cast<ASR::DoLoop_t>(stmt);
            inline_matmul_stmts(dl.m_body, dl.n_body);
            new_body.push_back(al, stmt);
            continue;
        }
        if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
            ASR::BlockCall_t &bc = *ASR::down_cast<ASR::BlockCall_t>(stmt);
            ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(bc.m_m);
            inline_matmul_stmts(block->m_body, block->n_body);
            new_body.push_back(al, stmt);
            continue;
        }
        if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
            ASR::AssociateBlockCall_t *abc =
                ASR::down_cast<ASR::AssociateBlockCall_t>(stmt);
            ASR::AssociateBlock_t *ab =
                ASR::down_cast<ASR::AssociateBlock_t>(abc->m_m);
            inline_matmul_stmts(ab->m_body, ab->n_body);
            new_body.push_back(al, stmt);
            continue;
        }
        if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
            new_body.push_back(al, stmt);
            continue;
        }
        ASR::Assignment_t *asgn = ASR::down_cast<ASR::Assignment_t>(stmt);

        ASR::expr_t *binop_other = nullptr;
        ASR::binopType binop_op = ASR::binopType::Add;
        bool matmul_is_left = true;
        ASR::IntrinsicArrayFunction_t *iaf = match_statement_matmul(
            asgn->m_value, binop_other, binop_op, matmul_is_left);
        if (!iaf) {
            new_body.push_back(al, stmt);
            continue;
        }

        // The loops count, and the indices of the operands are computed,
        // in the dialect's kind for element offsets (index_type): with
        // 64-bit offsets an operand's bounds need not fit integer(4).
        Location loc = stmt->base.loc;
        ASR::ttype_t *int_type = index_type(loc);
        auto in_index_kind = [&](ASR::expr_t *e) -> ASR::expr_t* {
            return device_caps.offset_kind == 4 ? to_int32(loc, e)
                : bound_to_index_kind(loc, e);
        };

        // Strip ArrayPhysicalCast from arguments
        ASR::expr_t *arg_a = iaf->m_args[0];
        ASR::expr_t *arg_b = iaf->m_args[1];
        if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arg_a)) {
            arg_a = ASR::down_cast<ASR::ArrayPhysicalCast_t>(arg_a)->m_arg;
        }
        if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arg_b)) {
            arg_b = ASR::down_cast<ASR::ArrayPhysicalCast_t>(arg_b)->m_arg;
        }

        // Detect and unwrap Transpose on matmul arguments so the
        // inlined loops index into the original array with swapped
        // indices instead of calling _lcompilers_transpose (which
        // is unavailable inside Metal GPU kernels).
        bool transpose_a = false, transpose_b = false;
        if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*arg_a)) {
            auto *iaf_a = ASR::down_cast<ASR::IntrinsicArrayFunction_t>(arg_a);
            if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
                    iaf_a->m_arr_intrinsic_id)
                        == ASRUtils::IntrinsicArrayFunctions::Transpose) {
                arg_a = iaf_a->m_args[0];
                if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arg_a)) {
                    arg_a = ASR::down_cast<ASR::ArrayPhysicalCast_t>(arg_a)->m_arg;
                }
                transpose_a = true;
            }
        }
        if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*arg_b)) {
            auto *iaf_b = ASR::down_cast<ASR::IntrinsicArrayFunction_t>(arg_b);
            if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
                    iaf_b->m_arr_intrinsic_id)
                        == ASRUtils::IntrinsicArrayFunctions::Transpose) {
                arg_b = iaf_b->m_args[0];
                if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arg_b)) {
                    arg_b = ASR::down_cast<ASR::ArrayPhysicalCast_t>(arg_b)->m_arg;
                }
                transpose_b = true;
            }
        }

        ASR::ttype_t *type_a = ASRUtils::expr_type(arg_a);
        ASR::ttype_t *type_b = ASRUtils::expr_type(arg_b);
        ASR::dimension_t *dims_a = nullptr, *dims_b = nullptr;
        int rank_a = checked_integer_cast<int>(ASRUtils::extract_dimensions_from_ttype(type_a, dims_a));
        int rank_b = checked_integer_cast<int>(ASRUtils::extract_dimensions_from_ttype(type_b, dims_b));

        ASR::ttype_t *elem_type = ASRUtils::extract_type(
            ASRUtils::expr_type(asgn->m_target));
        ASR::dimension_t *dims_c = nullptr;
        ASRUtils::extract_dimensions_from_ttype(
            ASRUtils::expr_type(asgn->m_target), dims_c);

        SymbolTable *var_scope = current_scope;
        while (var_scope && var_scope->asr_owner &&
               var_scope->asr_owner->type == ASR::asrType::symbol &&
               ASR::is_a<ASR::AssociateBlock_t>(
                   *ASR::down_cast<ASR::symbol_t>(
                       var_scope->asr_owner))) {
            var_scope = var_scope->parent;
        }

        auto make_loop_var = [&](const std::string &prefix) -> ASR::expr_t* {
            std::string name = var_scope->get_unique_name(prefix);
            ASR::symbol_t *sym = gpu_new_variable(al, loc, var_scope, name,
                ASRUtils::duplicate_type(al, int_type));
            return ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
        };

        auto make_array_item_1d = [&](ASR::expr_t *arr,
                ASR::expr_t *idx) -> ASR::expr_t* {
            Vec<ASR::array_index_t> args;
            args.reserve(al, 1);
            ASR::array_index_t ai;
            ai.loc = loc;
            ai.m_left = nullptr;
            ai.m_right = idx;
            ai.m_step = nullptr;
            args.push_back(al, ai);
            return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc, arr,
                args.p, args.n, elem_type,
                ASR::arraystorageType::ColMajor, nullptr, false));
        };

        auto make_array_item_2d = [&](ASR::expr_t *arr,
                ASR::expr_t *idx1, ASR::expr_t *idx2) -> ASR::expr_t* {
            Vec<ASR::array_index_t> args;
            args.reserve(al, 2);
            ASR::array_index_t ai1;
            ai1.loc = loc;
            ai1.m_left = nullptr;
            ai1.m_right = idx1;
            ai1.m_step = nullptr;
            args.push_back(al, ai1);
            ASR::array_index_t ai2;
            ai2.loc = loc;
            ai2.m_left = nullptr;
            ai2.m_right = idx2;
            ai2.m_step = nullptr;
            args.push_back(al, ai2);
            return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc, arr,
                args.p, args.n, elem_type,
                ASR::arraystorageType::ColMajor, nullptr, false));
        };

        // The bounds and starts below may be the very expressions an
        // operand's declaration holds; what is built from them gets its
        // own copies (own_bound).
        auto make_do_loop = [&](ASR::expr_t *var, ASR::expr_t *start,
                ASR::expr_t *end, ASR::expr_t *step,
                Vec<ASR::stmt_t*> &body) -> ASR::stmt_t* {
            ASR::do_loop_head_t head;
            head.loc = loc;
            head.m_v = var;
            head.m_start = own_bound(start);
            head.m_end = own_bound(end);
            head.m_increment = step == nullptr || is_int_literal(step, 1)
                ? nullptr : own_bound(step);
            if (device_caps.offset_kind != 4) {
                head.m_start = bound_to_index_kind(loc, head.m_start);
                head.m_end = bound_to_index_kind(loc, head.m_end);
                head.m_increment = bound_to_index_kind(loc,
                    head.m_increment);
            }
            return ASRUtils::STMT(ASRUtils::make_DoLoop_t_util(al, loc, nullptr,
                head, body.p, body.n, nullptr, 0));
        };

        // When an argument is an ArraySection, extract loop bounds
        // from the section's range specs rather than from the type
        // dimensions (which may be null for section result types).
        auto get_loop_bounds = [&](ASR::expr_t *arg,
                ASR::dimension_t *dims,
                int dim_idx) -> std::pair<ASR::expr_t*, ASR::expr_t*> {
            if (ASR::is_a<ASR::ArraySection_t>(*arg)) {
                ASR::ArraySection_t *sec =
                    ASR::down_cast<ASR::ArraySection_t>(arg);
                int range_idx = 0;
                for (size_t d = 0; d < sec->n_args; d++) {
                    if (sec->m_args[d].m_left != nullptr) {
                        if (range_idx == dim_idx) {
                            return {sec->m_args[d].m_left,
                                    sec->m_args[d].m_right};
                        }
                        range_idx++;
                    }
                }
            }
            return get_dim_bounds(al, arg->base.loc, dims,
                (size_t)dim_idx, arg);
        };

        // The step of the `dim_idx`-th range of a section, or nullptr
        // when the operand is not a section.
        auto section_step = [&](ASR::expr_t *arg,
                int dim_idx) -> ASR::expr_t* {
            if (!ASR::is_a<ASR::ArraySection_t>(*arg)) return nullptr;
            ASR::ArraySection_t *sec =
                ASR::down_cast<ASR::ArraySection_t>(arg);
            int range_idx = 0;
            for (size_t d = 0; d < sec->n_args; d++) {
                if (sec->m_args[d].m_left != nullptr) {
                    if (range_idx == dim_idx) return sec->m_args[d].m_step;
                    range_idx++;
                }
            }
            return nullptr;
        };

        // matmul pairs its operands by position: the k-th column of
        // `a` multiplies the k-th element of `b` whatever lower bound
        // either operand declares and wherever an operand that is an
        // array section starts inside its parent array, and whatever
        // step a section has.  The loop variables run over the index
        // space of one chosen operand, with its start and step
        // (`ref_start`, `ref_step`), so a variable used to index a
        // different operand is first turned into the position of the
        // element, (var - ref_start)/ref_step, and then into that
        // operand's own index: its first index plus the position times
        // its step.  The n-th element of `v(3:1:-1)` is `v(3 - (n-1))`.
        // When both spaces are known to start at the same index and
        // neither has a step the variable is used as it is, which leaves
        // the usual lower-bound-of-one case exactly as it was.
        auto rebase_index = [&](ASR::expr_t *operand,
                ASR::dimension_t *operand_dims, int dim_idx,
                ASR::expr_t *var, ASR::expr_t *ref_start,
                ASR::expr_t *ref_step) -> ASR::expr_t* {
            ASR::expr_t *start = get_loop_bounds(operand, operand_dims,
                dim_idx).first;
            if (start == nullptr || ref_start == nullptr) return var;
            ASR::expr_t *step = section_step(operand, dim_idx);
            bool unit_step = step == nullptr || is_int_literal(step, 1);
            bool unit_ref_step = ref_step == nullptr
                || is_int_literal(ref_step, 1);
            if (unit_step && unit_ref_step) {
                if (start == ref_start) return var;
                if (is_int_literal(start, 1)
                        && is_int_literal(ref_start, 1))
                    return var;
            }
            ASR::expr_t *offset = ASRUtils::EXPR(
                ASR::make_IntegerBinOp_t(al, loc, var,
                    ASR::binopType::Sub,
                    in_index_kind(own_bound(ref_start)),
                    int_type, nullptr));
            if (!unit_ref_step) {
                offset = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                    offset, ASR::binopType::Div,
                    in_index_kind(own_bound(ref_step)), int_type,
                    nullptr));
            }
            if (!unit_step) {
                offset = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                    offset, ASR::binopType::Mul,
                    in_index_kind(own_bound(step)), int_type, nullptr));
            }
            return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                in_index_kind(own_bound(start)),
                ASR::binopType::Add, offset, int_type, nullptr));
        };

        // When an argument or target is an ArraySection (e.g. v(:,i)),
        // expand it into an ArrayItem on the base array by replacing
        // each range dimension with the corresponding loop variable
        // and keeping fixed dimensions as-is.
        // When the expression is an elemental FunctionCall with array
        // arguments (e.g. f(z(1:n))), elementize by converting each
        // array argument to a scalar indexed by the loop variable,
        // producing f(z(i)) instead of f(z(1:n))[i].
        std::function<ASR::expr_t*(ASR::expr_t*,
            std::vector<ASR::expr_t*>)> make_section_item;
        make_section_item = [&](ASR::expr_t *arr_expr,
                std::vector<ASR::expr_t*> loop_vars) -> ASR::expr_t* {
            if (ASR::is_a<ASR::ArraySection_t>(*arr_expr)) {
                ASR::ArraySection_t *sec =
                    ASR::down_cast<ASR::ArraySection_t>(arr_expr);
                Vec<ASR::array_index_t> args;
                args.reserve(al, sec->n_args);
                size_t lv_idx = 0;
                for (size_t d = 0; d < sec->n_args; d++) {
                    ASR::array_index_t ai;
                    ai.loc = loc;
                    if (sec->m_args[d].m_left != nullptr) {
                        ai.m_left = nullptr;
                        ai.m_right = loop_vars[lv_idx++];
                        ai.m_step = nullptr;
                    } else {
                        ai.m_left = nullptr;
                        ai.m_right = sec->m_args[d].m_right;
                        ai.m_step = nullptr;
                    }
                    args.push_back(al, ai);
                }
                return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc,
                    sec->m_v, args.p, args.n, elem_type,
                    ASR::arraystorageType::ColMajor, nullptr, false));
            }
            if (ASR::is_a<ASR::FunctionCall_t>(*arr_expr)) {
                ASR::FunctionCall_t *fc =
                    ASR::down_cast<ASR::FunctionCall_t>(arr_expr);
                if (ASRUtils::is_elemental(fc->m_name)) {
                    Vec<ASR::call_arg_t> new_args;
                    new_args.reserve(al, fc->n_args);
                    for (size_t i = 0; i < fc->n_args; i++) {
                        ASR::call_arg_t arg;
                        arg.loc = fc->m_args[i].loc;
                        if (fc->m_args[i].m_value &&
                                ASRUtils::is_array(
                                    ASRUtils::expr_type(
                                        fc->m_args[i].m_value))) {
                            arg.m_value = make_section_item(
                                fc->m_args[i].m_value, loop_vars);
                        } else {
                            arg.m_value = fc->m_args[i].m_value;
                        }
                        new_args.push_back(al, arg);
                    }
                    ASR::ttype_t *ret_type = elem_type;
                    return ASRUtils::EXPR(
                        ASR::make_FunctionCall_t(al, fc->base.base.loc,
                            fc->m_name, fc->m_original_name,
                            new_args.p, new_args.n, ret_type,
                            nullptr, fc->m_dt));
                }
            }
            if (loop_vars.size() == 1)
                return make_array_item_1d(arr_expr, loop_vars[0]);
            return make_array_item_2d(arr_expr, loop_vars[0],
                loop_vars[1]);
        };

        // The other operand of `z = matmul(w, a) <op> b` is combined
        // with the matmul result element by element. A scalar operand
        // is the same for every element, so it is used as it is; only
        // an array operand is indexed by the loop variables. A scalar
        // reaches here wrapped in an ArrayBroadcast, whose type is an
        // array, so the wrapper is stripped before the rank is
        // checked.
        auto make_binop_other_item = [&](ASR::expr_t *other,
                std::vector<ASR::expr_t*> loop_vars,
                std::vector<ASR::expr_t*> ref_starts,
                std::vector<ASR::expr_t*> ref_steps) -> ASR::expr_t* {
            while (true) {
                if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*other)) {
                    other = ASR::down_cast<ASR::ArrayPhysicalCast_t>(
                        other)->m_arg;
                } else if (ASR::is_a<ASR::ArrayBroadcast_t>(*other)) {
                    other = ASR::down_cast<ASR::ArrayBroadcast_t>(
                        other)->m_array;
                } else {
                    break;
                }
            }
            if (!ASRUtils::is_array(ASRUtils::expr_type(other)))
                return other;
            ASR::dimension_t *other_dims = nullptr;
            ASRUtils::extract_dimensions_from_ttype(
                ASRUtils::expr_type(other), other_dims);
            std::vector<ASR::expr_t*> idx;
            for (size_t d = 0; d < loop_vars.size(); d++) {
                idx.push_back(rebase_index(other, other_dims, (int)d,
                    loop_vars[d], ref_starts[d], ref_steps[d]));
            }
            return make_section_item(other, idx);
        };

        ASR::expr_t *zero;
        if (ASR::is_a<ASR::Real_t>(*elem_type)) {
            zero = ASRUtils::EXPR(ASR::make_RealConstant_t(al, loc,
                0.0, elem_type));
        } else {
            zero = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
                0, elem_type, ASR::integerbozType::Decimal));
        }

        // Each element of the product is accumulated in a scalar local,
        // which the device keeps in a register, and stored into the
        // target once: accumulating into the target itself would load
        // and store device memory for every term of the sum.
        auto make_accumulator = [&]() -> ASR::expr_t* {
            std::string name = var_scope->get_unique_name("__gpu_mm_acc");
            ASR::symbol_t *sym = gpu_new_variable(al, loc, var_scope, name,
                ASRUtils::duplicate_type(al, elem_type));
            return ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
        };
        auto assign = [&](ASR::expr_t *target,
                ASR::expr_t *value) -> ASR::stmt_t* {
            return ASRUtils::STMT(ASR::make_Assignment_t(al, loc, target,
                value, nullptr, false, false, false, false));
        };
        // acc = 0; do k ...: acc = acc + a*b; c = acc [OP other]
        auto accumulate_element = [&](Vec<ASR::stmt_t*> &out,
                ASR::expr_t *c_elem, ASR::expr_t *var_k,
                ASR::expr_t *k_start, ASR::expr_t *k_end,
                ASR::expr_t *k_step,
                ASR::expr_t *a_elem, ASR::expr_t *b_elem,
                ASR::expr_t *other_elem) {
            ASR::expr_t *acc = make_accumulator();
            Vec<ASR::stmt_t*> k_body;
            k_body.reserve(al, 1);
            ASR::expr_t *prod = ASRUtils::EXPR(
                ASR::make_RealBinOp_t(al, loc, a_elem,
                    ASR::binopType::Mul, b_elem, elem_type, nullptr));
            k_body.push_back(al, assign(acc, ASRUtils::EXPR(
                ASR::make_RealBinOp_t(al, loc, acc,
                    ASR::binopType::Add, prod, elem_type, nullptr))));
            out.push_back(al, assign(acc, zero));
            out.push_back(al, make_do_loop(var_k, k_start, k_end, k_step,
                k_body));
            ASR::expr_t *value = acc;
            if (other_elem) {
                ASR::expr_t *lhs = matmul_is_left ? acc : other_elem;
                ASR::expr_t *rhs = matmul_is_left ? other_elem : acc;
                value = ASRUtils::EXPR(ASR::make_RealBinOp_t(al, loc, lhs,
                    binop_op, rhs, elem_type, nullptr));
            }
            out.push_back(al, assign(c_elem, value));
        };

        int64_t overload_id = iaf->m_overload_id;

        if (overload_id == 2 && rank_a == 2 && rank_b == 1) {
            // c(i) = sum_k a(i,k) * b(k)
            // With transpose_a: c(i) = sum_k a(k,i) * b(k)
            ASR::expr_t *var_i = make_loop_var("__gpu_mm_i");
            ASR::expr_t *var_k = make_loop_var("__gpu_mm_k");

            int i_dim = transpose_a ? 1 : 0;
            int k_dim = transpose_a ? 0 : 1;
            auto [k_start, k_end] = get_loop_bounds(arg_a, dims_a, k_dim);
            auto [i_start, i_end] = get_loop_bounds(arg_a, dims_a, i_dim);
            ASR::expr_t *k_step = section_step(arg_a, k_dim);
            ASR::expr_t *i_step = section_step(arg_a, i_dim);

            ASR::expr_t *c_i = make_section_item(asgn->m_target,
                {rebase_index(asgn->m_target, dims_c, 0, var_i,
                    i_start, i_step)});
            ASR::expr_t *a_ik = transpose_a
                ? make_section_item(arg_a, {var_k, var_i})
                : make_section_item(arg_a, {var_i, var_k});
            ASR::expr_t *b_k = make_section_item(arg_b,
                {rebase_index(arg_b, dims_b, 0, var_k, k_start, k_step)});

            Vec<ASR::stmt_t*> i_body;
            i_body.reserve(al, 3);
            accumulate_element(i_body, c_i, var_k, k_start, k_end, k_step,
                a_ik, b_k, binop_other ? make_binop_other_item(binop_other,
                    {var_i}, {i_start}, {i_step}) : nullptr);

            new_body.push_back(al,
                make_do_loop(var_i, i_start, i_end, i_step, i_body));
        } else if (overload_id == 1 && rank_a == 1 && rank_b == 2) {
            // c(j) = sum_k a(k) * b(k, j)
            // With transpose_b: c(j) = sum_k a(k) * b(j, k)
            ASR::expr_t *var_j = make_loop_var("__gpu_mm_j");
            ASR::expr_t *var_k = make_loop_var("__gpu_mm_k");

            int k_dim = transpose_b ? 1 : 0;
            int j_dim = transpose_b ? 0 : 1;
            auto [k_start, k_end] = get_loop_bounds(arg_b, dims_b, k_dim);
            auto [j_start, j_end] = get_loop_bounds(arg_b, dims_b, j_dim);
            ASR::expr_t *k_step = section_step(arg_b, k_dim);
            ASR::expr_t *j_step = section_step(arg_b, j_dim);

            ASR::expr_t *c_j = make_section_item(asgn->m_target,
                {rebase_index(asgn->m_target, dims_c, 0, var_j,
                    j_start, j_step)});
            ASR::expr_t *a_k = make_section_item(arg_a,
                {rebase_index(arg_a, dims_a, 0, var_k, k_start, k_step)});
            ASR::expr_t *b_kj = transpose_b
                ? make_section_item(arg_b, {var_j, var_k})
                : make_section_item(arg_b, {var_k, var_j});

            Vec<ASR::stmt_t*> j_body;
            j_body.reserve(al, 3);
            accumulate_element(j_body, c_j, var_k, k_start, k_end, k_step,
                a_k, b_kj, binop_other ? make_binop_other_item(binop_other,
                    {var_j}, {j_start}, {j_step}) : nullptr);

            new_body.push_back(al,
                make_do_loop(var_j, j_start, j_end, j_step, j_body));
        } else if (overload_id == 3 && rank_a == 2 && rank_b == 2) {
            // c(i,j) = sum_k a(i,k) * b(k,j)
            // With transpose_a: a(i,k) becomes a(k,i)
            // With transpose_b: b(k,j) becomes b(j,k)
            ASR::expr_t *var_i = make_loop_var("__gpu_mm_i");
            ASR::expr_t *var_j = make_loop_var("__gpu_mm_j");
            ASR::expr_t *var_k = make_loop_var("__gpu_mm_k");

            int a_k_dim = transpose_a ? 0 : 1;
            int a_i_dim = transpose_a ? 1 : 0;
            int b_j_dim = transpose_b ? 0 : 1;
            int b_k_dim = transpose_b ? 1 : 0;
            auto [k_start, k_end] = get_loop_bounds(arg_a, dims_a, a_k_dim);
            auto [j_start, j_end] = get_loop_bounds(arg_b, dims_b, b_j_dim);
            auto [i_start, i_end] = get_loop_bounds(arg_a, dims_a, a_i_dim);
            ASR::expr_t *k_step = section_step(arg_a, a_k_dim);
            ASR::expr_t *j_step = section_step(arg_b, b_j_dim);
            ASR::expr_t *i_step = section_step(arg_a, a_i_dim);

            // `k` runs over `a`'s contraction dimension, so only
            // `b`'s copy of it is rebased; `j` already runs over
            // `b`'s own dimension.
            ASR::expr_t *var_k_b = rebase_index(arg_b, dims_b, b_k_dim,
                var_k, k_start, k_step);
            ASR::expr_t *c_ij = make_section_item(asgn->m_target,
                {rebase_index(asgn->m_target, dims_c, 0, var_i, i_start,
                    i_step),
                 rebase_index(asgn->m_target, dims_c, 1, var_j, j_start,
                    j_step)});
            ASR::expr_t *a_ik = transpose_a
                ? make_section_item(arg_a, {var_k, var_i})
                : make_section_item(arg_a, {var_i, var_k});
            ASR::expr_t *b_kj = transpose_b
                ? make_section_item(arg_b, {var_j, var_k_b})
                : make_section_item(arg_b, {var_k_b, var_j});

            Vec<ASR::stmt_t*> j_body;
            j_body.reserve(al, 3);
            accumulate_element(j_body, c_ij, var_k, k_start, k_end, k_step,
                a_ik, b_kj, binop_other ? make_binop_other_item(binop_other,
                    {var_i, var_j}, {i_start, j_start}, {i_step, j_step})
                    : nullptr);

            Vec<ASR::stmt_t*> i_body;
            i_body.reserve(al, 1);
            i_body.push_back(al,
                make_do_loop(var_j, j_start, j_end, j_step, j_body));

            new_body.push_back(al,
                make_do_loop(var_i, i_start, i_end, i_step, i_body));
        } else {
            new_body.push_back(al, stmt);
            continue;
        }
        changed = true;
    }

    if (changed) {
        body = new_body.p;
        n_body = new_body.n;
    }
}

// A matmul that `inline_matmul_stmts` does not match -- one nested
// inside a unary minus, inside another intrinsic, inside a call
// argument, inside an array constructor (`r = [0.0, matmul(a, b)]`)
// or as an argument of another matmul -- is left to the ordinary
// lowering, which turns it into a call to the `_lcompilers_matmul`
// helper. The helper itself is emitted into the shader, but the call
// is not always given the extent of every actual argument, and the
// answer is wrong where it is emitted at all. Hoist every such matmul
// into its own temporary first, so the existing whole-right-hand-side
// lowering applies to it and the enclosing expression is left with a
// plain array variable (a shape the Metal backend already handles).
void GpuOffloadVisitor::hoist_nested_matmuls(ParallelLoopNest &nest) {
    SymbolTable *var_scope = current_scope;
    while (var_scope && var_scope->asr_owner &&
           var_scope->asr_owner->type == ASR::asrType::symbol &&
           ASR::is_a<ASR::AssociateBlock_t>(
               *ASR::down_cast<ASR::symbol_t>(var_scope->asr_owner))) {
        var_scope = var_scope->parent;
    }
    NestBodyWriteBack back(nest);
    hoist_nested_matmuls_in_body(back.body, back.n_body, var_scope,
        false, true);
}

// `scope_has_workspaces` says whether a run-time sized temporary put
// into `var_scope` will be given a per-thread VLA workspace buffer.
// Only a BLOCK that is a direct statement of the loop body is scanned
// for those; a temporary at kernel scope would be a single buffer
// shared by every thread.  `at_loop_top` tracks whether this
// statement list is that loop body itself.
void GpuOffloadVisitor::hoist_nested_matmuls_in_body(
        ASR::stmt_t** &body, size_t &n_body,
        SymbolTable *var_scope, bool scope_has_workspaces,
        bool at_loop_top) {
    Vec<ASR::stmt_t*> new_body;
    new_body.reserve(al, n_body * 2);
    bool changed = false;

    for (size_t si = 0; si < n_body; si++) {
        ASR::stmt_t *stmt = body[si];
        if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
            ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
            hoist_nested_matmuls_in_body(dl->m_body, dl->n_body,
                var_scope, scope_has_workspaces, false);
            new_body.push_back(al, stmt);
            continue;
        }
        // A spliced-in device function body lives in its own BLOCK;
        // hoist inside it too, into that block's scope.
        if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::BlockCall_t>(stmt)->m_m);
            if (b && ASR::is_a<ASR::Block_t>(*b)) {
                ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                hoist_nested_matmuls_in_body(blk->m_body, blk->n_body,
                    blk->m_symtab, at_loop_top, false);
            }
            new_body.push_back(al, stmt);
            continue;
        }
        if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
            ASR::AssociateBlock_t *ab =
                ASR::down_cast<ASR::AssociateBlock_t>(
                    ASR::down_cast<ASR::AssociateBlockCall_t>(stmt)->m_m);
            hoist_nested_matmuls_in_body(ab->m_body, ab->n_body,
                var_scope, scope_has_workspaces, false);
            new_body.push_back(al, stmt);
            continue;
        }
        if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
            new_body.push_back(al, stmt);
            continue;
        }
        size_t before = new_body.size();
        hoist_matmuls_from_assignment(
            ASR::down_cast<ASR::Assignment_t>(stmt), new_body, var_scope,
            scope_has_workspaces);
        new_body.push_back(al, stmt);
        if (new_body.size() != before + 1) changed = true;
    }

    if (changed) {
        body = new_body.p;
        n_body = new_body.n;
    }
}

// Hoist every matmul in the value of `asgn` that the statement-level
// lowering cannot see into a temporary, appending the temporaries'
// assignments to `out`. The matmul the lowering does match is left in
// place; its arguments are still searched, so a nested matmul such as
// `matmul(a, matmul(a, b))` has its inner operand hoisted.
void GpuOffloadVisitor::hoist_matmuls_from_assignment(ASR::Assignment_t *asgn,
        Vec<ASR::stmt_t*> &out, SymbolTable *var_scope,
        bool scope_has_workspaces) {
    ASR::expr_t *binop_other = nullptr;
    ASR::binopType binop_op = ASR::binopType::Add;
    bool matmul_is_left = true;
    ASR::IntrinsicArrayFunction_t *handled = match_statement_matmul(
        asgn->m_value, binop_other, binop_op, matmul_is_left);
    Location loc = asgn->base.base.loc;
    while (true) {
        ASR::IntrinsicArrayFunction_t *mm = find_array_intrinsic_in_expr(
            asgn->m_value, ASRUtils::IntrinsicArrayFunctions::MatMul,
            handled);
        if (!mm) break;
        ASR::expr_t *tmp_var = make_matmul_result_temp(mm, loc,
            var_scope, scope_has_workspaces);
        if (!tmp_var) break;
        ASR::stmt_t *tmp_asgn = ASRUtils::STMT(ASR::make_Assignment_t(
            al, loc, tmp_var, (ASR::expr_t*)mm, nullptr, false, false, false, false));
        if (!replace_array_intrinsic_in_expr(asgn->m_value, mm,
                tmp_var)) {
            break;
        }
        hoist_matmuls_from_assignment(
            ASR::down_cast<ASR::Assignment_t>(tmp_asgn), out, var_scope,
            scope_has_workspaces);
        out.push_back(al, tmp_asgn);
    }
}

// A local temporary holding the result of `mm`, or nullptr if the
// result shape cannot be determined from the operands.
ASR::expr_t* GpuOffloadVisitor::make_matmul_result_temp(
        ASR::IntrinsicArrayFunction_t *mm,
        const Location &loc, SymbolTable *var_scope,
        bool scope_has_workspaces) {
    ASR::expr_t *e = (ASR::expr_t*)mm;
    if (!ASRUtils::is_array(ASRUtils::expr_type(e))) return nullptr;
    Vec<ASR::dimension_t> dims;
    if (!intrinsic_array_result_dims(e, dims)) return nullptr;
    ASR::ttype_t *elem_type = ASRUtils::extract_type(
        ASRUtils::expr_type(e));
    bool all_const = true;
    for (size_t d = 0; d < dims.n; d++) {
        if (!dims[d].m_length ||
                !ASRUtils::expr_value(dims[d].m_length)) {
            all_const = false;
        }
    }
    // A run-time sized temporary is only correct where each thread
    // gets its own workspace slice; anywhere else it would be one
    // buffer written by every thread at once.
    if (!all_const && !scope_has_workspaces) return nullptr;
    ASR::ttype_t *tmp_type = ASRUtils::TYPE(
        ASR::make_Array_t(al, loc, elem_type, dims.p, dims.n,
            all_const
                ? ASR::array_physical_typeType::FixedSizeArray
                : ASR::array_physical_typeType::DescriptorArray,
            ASR::memory_spaceType::Global));
    std::string name = var_scope->get_unique_name("__gpu_matmul_tmp");
    ASR::symbol_t *sym = gpu_new_variable(al, loc, var_scope, name,
        tmp_type);
    return ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
}

// Shape of an array-valued intrinsic's result, taken from its
// operands' declared dimensions.
bool GpuOffloadVisitor::intrinsic_array_result_dims(ASR::expr_t *e,
        Vec<ASR::dimension_t> &dims) {
    ASR::IntrinsicArrayFunction_t *iaf =
        ASR::down_cast<ASR::IntrinsicArrayFunction_t>(e);
    if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
            iaf->m_arr_intrinsic_id)
                != ASRUtils::IntrinsicArrayFunctions::MatMul) {
        return false;
    }
    if (iaf->n_args < 2) return false;
    ASR::expr_t *a = iaf->m_args[0];
    ASR::expr_t *b = iaf->m_args[1];
    while (a && ASR::is_a<ASR::ArrayPhysicalCast_t>(*a))
        a = ASR::down_cast<ASR::ArrayPhysicalCast_t>(a)->m_arg;
    while (b && ASR::is_a<ASR::ArrayPhysicalCast_t>(*b))
        b = ASR::down_cast<ASR::ArrayPhysicalCast_t>(b)->m_arg;
    if (!a || !b) return false;
    ASR::dimension_t *da = nullptr, *db = nullptr;
    int ra = checked_integer_cast<int>(ASRUtils::extract_dimensions_from_ttype(
        ASRUtils::type_get_past_allocatable_pointer(
            ASRUtils::expr_type(a)), da));
    int rb = checked_integer_cast<int>(ASRUtils::extract_dimensions_from_ttype(
        ASRUtils::type_get_past_allocatable_pointer(
            ASRUtils::expr_type(b)), db));
    dims.reserve(al, 2);
    if (ra == 2 && rb == 1) {
        dims.push_back(al, dim_or_runtime_extent(a, da, 0));
    } else if (ra == 1 && rb == 2) {
        dims.push_back(al, dim_or_runtime_extent(b, db, 1));
    } else if (ra == 2 && rb == 2) {
        dims.push_back(al, dim_or_runtime_extent(a, da, 0));
        dims.push_back(al, dim_or_runtime_extent(b, db, 1));
    } else {
        return false;
    }
    return true;
}

// A copy of `bound`, an expression the lowering of the kernel's loop
// nest may have taken from an array's declaration, for a loop or a
// shape that the kernel will own.  The statements of a BLOCK in the
// nest are remapped into the kernel in place, so a node embedded there
// that a host declaration also holds would be rewritten to name a
// kernel symbol.  Each copy records the node it was made from:
// process_block_for_kernel replaces a function-call extent of a BLOCK
// local by the value the host computes, and finds what was built from
// that extent through its origin (bound_origin).  A device routine's
// statements are already the kernel's own copy, so outside the nest
// the bound is returned as it is.
ASR::expr_t* GpuOffloadVisitor::own_bound(ASR::expr_t *bound) {
    if (!lowering_kernel_nest) return bound;
    return copy_bound(bound);
}

// Duplicates an expression and records, for each node of the copy, the
// node it stands for: the origin of the node it was copied from.  A bound
// can hold an extent below its root -- converted to another kind, say --
// and the copy of that extent must still be found by its origin.
namespace {
class GpuBoundDuplicator :
        public ASR::BaseExprStmtDuplicator<GpuBoundDuplicator> {
    std::map<ASR::expr_t*, ASR::expr_t*> &copies;

public:
    GpuBoundDuplicator(Allocator &al,
            std::map<ASR::expr_t*, ASR::expr_t*> &copies)
        : BaseExprStmtDuplicator(al), copies(copies) {}

    ASR::expr_t* duplicate_expr(ASR::expr_t *x) {
        ASR::expr_t *copy = ASR::BaseExprStmtDuplicator<GpuBoundDuplicator>
            ::duplicate_expr(x);
        if (copy != nullptr && copy != x) {
            auto it = copies.find(x);
            copies[copy] = it == copies.end() ? x : it->second;
        }
        return copy;
    }
};
} // namespace

// A copy of `bound` that records its origin, whatever is being lowered.
ASR::expr_t* GpuOffloadVisitor::copy_bound(ASR::expr_t *bound) {
    if (bound == nullptr) return bound;
    GpuBoundDuplicator dup(al, bound_copies);
    return dup.duplicate_expr(bound);
}

// The node `e` was copied from by copy_bound, through any number of
// copies, or `e` itself.
ASR::expr_t* GpuOffloadVisitor::bound_origin(ASR::expr_t *e) {
    auto it = bound_copies.find(e);
    return it == bound_copies.end() ? e : it->second;
}

// Dimension `d` of `operand`, described so that the extent is
// available wherever the shape is needed.  A deferred-shape operand
// -- an allocatable, a pointer or an assumed-shape dummy -- carries no
// declared length, so its extent is the operand's own run-time
// `size(operand, d + 1)` instead, of kind `size_kind`.  That expression
// is what the VLA workspace machinery resolves back to the extents the
// host already passes to the kernel.  A declared dimension is copied
// (own_bound): its expressions belong to the operand's declaration.
ASR::dimension_t GpuOffloadVisitor::dim_or_runtime_extent(ASR::expr_t *operand,
        ASR::dimension_t *dims, size_t d, int size_kind) {
    if (dims && dims[d].m_length) {
        ASR::dimension_t dim = dims[d];
        dim.m_start = own_bound(dim.m_start);
        dim.m_length = own_bound(dim.m_length);
        return dim;
    }
    const Location &loc = operand->base.loc;
    ASR::ttype_t *int_type = ASRUtils::TYPE(
        ASR::make_Integer_t(al, loc, 4));
    ASR::dimension_t res;
    res.loc = loc;
    res.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
        1, int_type, ASR::integerbozType::Decimal));
    ASR::expr_t *dim_expr = ASRUtils::EXPR(
        ASR::make_IntegerConstant_t(al, loc, (int64_t)d + 1, int_type,
            ASR::integerbozType::Decimal));
    res.m_length = ASRUtils::EXPR(ASR::make_ArraySize_t(al, loc,
        operand, dim_expr, ASRUtils::TYPE(ASR::make_Integer_t(al, loc,
            size_kind)), nullptr));
    return res;
}

void GpuOffloadVisitor::inline_intrinsic_matmul(ParallelLoopNest &nest) {
    NestBodyWriteBack back(nest);
    inline_matmul_stmts(back.body, back.n_body);
}

// Distribute ArrayItem indexing through an array expression tree
// to produce a scalar expression. For example:
//   sum(a + b) with index k  -->  a(k) + b(k)
// instead of the incorrect (a + b)[k] which would be pointer arithmetic.
// A broadcast scalar stays the scalar, and every node takes the element
// type of its own array type: in `a**2` the exponent stays integer.
ASR::expr_t* GpuOffloadVisitor::index_array_expr(ASR::expr_t *expr,
        ASR::array_index_t *idx_p, size_t idx_n, const Location &loc) {
    if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*expr)) {
        expr = ASR::down_cast<ASR::ArrayPhysicalCast_t>(expr)->m_arg;
    }
    if (!ASRUtils::is_array(ASRUtils::expr_type(expr))) {
        return expr;
    }
    if (ASR::is_a<ASR::ArrayBroadcast_t>(*expr)) {
        return ASR::down_cast<ASR::ArrayBroadcast_t>(expr)->m_array;
    }
    ASR::ttype_t *elem_type = ASRUtils::extract_type(
        ASRUtils::expr_type(expr));
    if (ASR::is_a<ASR::Var_t>(*expr)) {
        return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc, expr,
            idx_p, idx_n, elem_type,
            ASR::arraystorageType::ColMajor, nullptr, false));
    }
    if (ASR::is_a<ASR::RealBinOp_t>(*expr)) {
        ASR::RealBinOp_t *op = ASR::down_cast<ASR::RealBinOp_t>(expr);
        ASR::expr_t *left = index_array_expr(op->m_left,
            idx_p, idx_n, loc);
        ASR::expr_t *right = index_array_expr(op->m_right,
            idx_p, idx_n, loc);
        return ASRUtils::EXPR(ASR::make_RealBinOp_t(al, loc,
            left, op->m_op, right, elem_type, nullptr));
    }
    if (ASR::is_a<ASR::IntegerBinOp_t>(*expr)) {
        ASR::IntegerBinOp_t *op =
            ASR::down_cast<ASR::IntegerBinOp_t>(expr);
        ASR::expr_t *left = index_array_expr(op->m_left,
            idx_p, idx_n, loc);
        ASR::expr_t *right = index_array_expr(op->m_right,
            idx_p, idx_n, loc);
        return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
            left, op->m_op, right, elem_type, nullptr));
    }
    if (ASR::is_a<ASR::RealUnaryMinus_t>(*expr)) {
        ASR::RealUnaryMinus_t *u =
            ASR::down_cast<ASR::RealUnaryMinus_t>(expr);
        ASR::expr_t *arg = index_array_expr(u->m_arg,
            idx_p, idx_n, loc);
        return ASRUtils::EXPR(ASR::make_RealUnaryMinus_t(al, loc,
            arg, elem_type, nullptr));
    }
    if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*expr)) {
        ASR::IntegerUnaryMinus_t *u =
            ASR::down_cast<ASR::IntegerUnaryMinus_t>(expr);
        ASR::expr_t *arg = index_array_expr(u->m_arg,
            idx_p, idx_n, loc);
        return ASRUtils::EXPR(ASR::make_IntegerUnaryMinus_t(al, loc,
            arg, elem_type, nullptr));
    }
    return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc, expr,
        idx_p, idx_n, elem_type,
        ASR::arraystorageType::ColMajor, nullptr, false));
}

// Inline IntrinsicArrayFunction Sum inside a parallel loop body.
// Replaces:
//   results(i) = sum(a)
// With:
//   __gpu_sum_res = 0.0
//   do __gpu_sum_k = 1, n
//     __gpu_sum_res = __gpu_sum_res + a(__gpu_sum_k)
//   end do
//   results(i) = __gpu_sum_res
// This avoids generating a call to _lcompilers_Sum which is not
// available inside Metal GPU kernels.
// Search an expression tree for an IntrinsicArrayFunction node of the
// given kind. `skip` names a node the caller already handles: it is
// not reported, but its arguments are still searched.
ASR::IntrinsicArrayFunction_t* GpuOffloadVisitor::find_array_intrinsic_in_expr(
        ASR::expr_t *expr, ASRUtils::IntrinsicArrayFunctions which,
        ASR::IntrinsicArrayFunction_t *skip) {
    if (!expr) return nullptr;
    if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*expr)) {
        auto *iaf = ASR::down_cast<ASR::IntrinsicArrayFunction_t>(expr);
        if (iaf != skip &&
                static_cast<ASRUtils::IntrinsicArrayFunctions>(
                    iaf->m_arr_intrinsic_id) == which) {
            return iaf;
        }
        for (size_t i = 0; i < iaf->n_args; i++) {
            auto *found = find_array_intrinsic_in_expr(iaf->m_args[i],
                which, skip);
            if (found) return found;
        }
        return nullptr;
    }
    if (ASR::is_a<ASR::RealBinOp_t>(*expr)) {
        auto *op = ASR::down_cast<ASR::RealBinOp_t>(expr);
        auto *found = find_array_intrinsic_in_expr(op->m_left, which,
            skip);
        if (found) return found;
        return find_array_intrinsic_in_expr(op->m_right, which, skip);
    }
    if (ASR::is_a<ASR::IntegerBinOp_t>(*expr)) {
        auto *op = ASR::down_cast<ASR::IntegerBinOp_t>(expr);
        auto *found = find_array_intrinsic_in_expr(op->m_left, which,
            skip);
        if (found) return found;
        return find_array_intrinsic_in_expr(op->m_right, which, skip);
    }
    if (ASR::is_a<ASR::RealUnaryMinus_t>(*expr)) {
        return find_array_intrinsic_in_expr(
            ASR::down_cast<ASR::RealUnaryMinus_t>(expr)->m_arg, which,
            skip);
    }
    if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*expr)) {
        return find_array_intrinsic_in_expr(
            ASR::down_cast<ASR::IntegerUnaryMinus_t>(expr)->m_arg, which,
            skip);
    }
    if (ASR::is_a<ASR::Cast_t>(*expr)) {
        return find_array_intrinsic_in_expr(
            ASR::down_cast<ASR::Cast_t>(expr)->m_arg, which, skip);
    }
    if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*expr)) {
        return find_array_intrinsic_in_expr(
            ASR::down_cast<ASR::ArrayPhysicalCast_t>(expr)->m_arg, which,
            skip);
    }
    if (ASR::is_a<ASR::RealCompare_t>(*expr)) {
        auto *cmp = ASR::down_cast<ASR::RealCompare_t>(expr);
        auto *found = find_array_intrinsic_in_expr(cmp->m_left, which,
            skip);
        if (found) return found;
        return find_array_intrinsic_in_expr(cmp->m_right, which, skip);
    }
    if (ASR::is_a<ASR::IntegerCompare_t>(*expr)) {
        auto *cmp = ASR::down_cast<ASR::IntegerCompare_t>(expr);
        auto *found = find_array_intrinsic_in_expr(cmp->m_left, which,
            skip);
        if (found) return found;
        return find_array_intrinsic_in_expr(cmp->m_right, which, skip);
    }
    if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*expr)) {
        auto *ief = ASR::down_cast<ASR::IntrinsicElementalFunction_t>(expr);
        for (size_t i = 0; i < ief->n_args; i++) {
            auto *found = find_array_intrinsic_in_expr(ief->m_args[i],
                which, skip);
            if (found) return found;
        }
    }
    if (ASR::is_a<ASR::FunctionCall_t>(*expr)) {
        auto *fc = ASR::down_cast<ASR::FunctionCall_t>(expr);
        for (size_t i = 0; i < fc->n_args; i++) {
            auto *found = find_array_intrinsic_in_expr(
                fc->m_args[i].m_value, which, skip);
            if (found) return found;
        }
    }
    if (ASR::is_a<ASR::ArrayConstructor_t>(*expr)) {
        auto *ac = ASR::down_cast<ASR::ArrayConstructor_t>(expr);
        for (size_t i = 0; i < ac->n_args; i++) {
            auto *found = find_array_intrinsic_in_expr(ac->m_args[i],
                which, skip);
            if (found) return found;
        }
    }
    return nullptr;
}

// Replace a specific IntrinsicArrayFunction node in an expression tree
// with a replacement expression.
bool GpuOffloadVisitor::replace_array_intrinsic_in_expr(ASR::expr_t* &expr,
        ASR::IntrinsicArrayFunction_t *target,
        ASR::expr_t *replacement) {
    if (!expr) return false;
    if (expr == (ASR::expr_t*)target) {
        expr = replacement;
        return true;
    }
    if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*expr)) {
        auto *c = ASR::down_cast<ASR::ArrayPhysicalCast_t>(expr);
        if (c->m_arg == (ASR::expr_t*)target) {
            c->m_arg = replacement;
            c->m_old = ASRUtils::extract_physical_type(
                ASRUtils::expr_type(replacement));
            return true;
        }
        return replace_array_intrinsic_in_expr(c->m_arg, target,
            replacement);
    }
    if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*expr)) {
        auto *iaf = ASR::down_cast<ASR::IntrinsicArrayFunction_t>(expr);
        for (size_t i = 0; i < iaf->n_args; i++) {
            if (replace_array_intrinsic_in_expr(iaf->m_args[i], target,
                    replacement))
                return true;
        }
        return false;
    }
    if (ASR::is_a<ASR::RealBinOp_t>(*expr)) {
        auto *op = ASR::down_cast<ASR::RealBinOp_t>(expr);
        if (replace_array_intrinsic_in_expr(op->m_left, target,
                replacement))
            return true;
        return replace_array_intrinsic_in_expr(op->m_right, target,
            replacement);
    }
    if (ASR::is_a<ASR::IntegerBinOp_t>(*expr)) {
        auto *op = ASR::down_cast<ASR::IntegerBinOp_t>(expr);
        if (replace_array_intrinsic_in_expr(op->m_left, target,
                replacement))
            return true;
        return replace_array_intrinsic_in_expr(op->m_right, target,
            replacement);
    }
    if (ASR::is_a<ASR::RealUnaryMinus_t>(*expr)) {
        return replace_array_intrinsic_in_expr(
            ASR::down_cast<ASR::RealUnaryMinus_t>(expr)->m_arg, target,
            replacement);
    }
    if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*expr)) {
        return replace_array_intrinsic_in_expr(
            ASR::down_cast<ASR::IntegerUnaryMinus_t>(expr)->m_arg, target,
            replacement);
    }
    if (ASR::is_a<ASR::Cast_t>(*expr)) {
        return replace_array_intrinsic_in_expr(
            ASR::down_cast<ASR::Cast_t>(expr)->m_arg, target, replacement);
    }
    if (ASR::is_a<ASR::RealCompare_t>(*expr)) {
        auto *cmp = ASR::down_cast<ASR::RealCompare_t>(expr);
        if (replace_array_intrinsic_in_expr(cmp->m_left, target,
                replacement))
            return true;
        return replace_array_intrinsic_in_expr(cmp->m_right, target,
            replacement);
    }
    if (ASR::is_a<ASR::IntegerCompare_t>(*expr)) {
        auto *cmp = ASR::down_cast<ASR::IntegerCompare_t>(expr);
        if (replace_array_intrinsic_in_expr(cmp->m_left, target,
                replacement))
            return true;
        return replace_array_intrinsic_in_expr(cmp->m_right, target,
            replacement);
    }
    if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*expr)) {
        auto *ief = ASR::down_cast<ASR::IntrinsicElementalFunction_t>(expr);
        for (size_t i = 0; i < ief->n_args; i++) {
            if (replace_array_intrinsic_in_expr(ief->m_args[i], target,
                    replacement))
                return true;
        }
    }
    if (ASR::is_a<ASR::FunctionCall_t>(*expr)) {
        auto *fc = ASR::down_cast<ASR::FunctionCall_t>(expr);
        for (size_t i = 0; i < fc->n_args; i++) {
            if (replace_array_intrinsic_in_expr(fc->m_args[i].m_value,
                    target, replacement))
                return true;
        }
    }
    if (ASR::is_a<ASR::ArrayConstructor_t>(*expr)) {
        auto *ac = ASR::down_cast<ASR::ArrayConstructor_t>(expr);
        for (size_t i = 0; i < ac->n_args; i++) {
            if (replace_array_intrinsic_in_expr(ac->m_args[i], target,
                    replacement))
                return true;
        }
    }
    return false;
}

// Extract nested Sum calls from assignment values into separate
// temporary assignments so the main Sum inlining logic can handle them.
// E.g., "cost = cost + sum(a)" becomes:
//   "__gpu_sum_tmp = sum(a)"
//   "cost = cost + __gpu_sum_tmp"
void GpuOffloadVisitor::extract_nested_sums(
        ASR::stmt_t** &stmts, size_t &n_stmts,
                         SymbolTable *scope) {
    Vec<ASR::stmt_t*> expanded;
    expanded.reserve(al, n_stmts * 2);
    bool changed = false;

    for (size_t i = 0; i < n_stmts; i++) {
        ASR::stmt_t *stmt = stmts[i];
        if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
            expanded.push_back(al, stmt);
            continue;
        }
        ASR::Assignment_t *asgn =
            ASR::down_cast<ASR::Assignment_t>(stmt);

        // Skip if value is already a direct Sum
        if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*asgn->m_value)) {
            expanded.push_back(al, stmt);
            continue;
        }

        ASR::IntrinsicArrayFunction_t *sum_node =
            find_array_intrinsic_in_expr(asgn->m_value,
                ASRUtils::IntrinsicArrayFunctions::Sum);
        if (!sum_node) {
            expanded.push_back(al, stmt);
            continue;
        }

        Location loc = stmt->base.loc;
        ASR::ttype_t *sum_type = sum_node->m_type;

        SymbolTable *var_scope = scope;
        while (var_scope && var_scope->asr_owner &&
               var_scope->asr_owner->type == ASR::asrType::symbol &&
               ASR::is_a<ASR::AssociateBlock_t>(
                   *ASR::down_cast<ASR::symbol_t>(
                       var_scope->asr_owner))) {
            var_scope = var_scope->parent;
        }

        std::string tmp_name =
            var_scope->get_unique_name("__gpu_sum_tmp");
        ASR::symbol_t *tmp_sym = gpu_new_variable(al, loc, var_scope,
            tmp_name, ASRUtils::duplicate_type(al, sum_type));
        ASR::expr_t *tmp_var = ASRUtils::EXPR(
            ASR::make_Var_t(al, loc, tmp_sym));

        // Create: __gpu_sum_tmp = sum(a)
        ASR::expr_t *sum_expr = (ASR::expr_t*)sum_node;
        expanded.push_back(al, ASRUtils::STMT(
            ASR::make_Assignment_t(al, loc, tmp_var, sum_expr,
                nullptr, false, false, false, false)));

        // Replace sum node in original expression with tmp_var
        replace_array_intrinsic_in_expr(asgn->m_value, sum_node,
            tmp_var);

        // Add modified original assignment
        expanded.push_back(al, stmt);
        changed = true;
    }

    if (changed) {
        stmts = expanded.p;
        n_stmts = expanded.n;
    }
}

void GpuOffloadVisitor::inline_sum_in_stmts(
        ASR::stmt_t** &stmts, size_t &n_stmts,
                         SymbolTable *scope) {
    // Pre-pass: extract nested Sum calls into separate assignments
    extract_nested_sums(stmts, n_stmts, scope);

    Vec<ASR::stmt_t*> new_body;
    new_body.reserve(al, n_stmts * 4);
    bool changed = false;

    for (size_t si = 0; si < n_stmts; si++) {
        ASR::stmt_t *stmt = stmts[si];

        // Recurse into DoLoop bodies
        if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
            ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
            inline_sum_in_stmts(dl->m_body, dl->n_body, scope);
            new_body.push_back(al, stmt);
            continue;
        }

        // Recurse into Block bodies
        if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
            ASR::BlockCall_t *bc =
                ASR::down_cast<ASR::BlockCall_t>(stmt);
            if (ASR::is_a<ASR::Block_t>(*bc->m_m)) {
                ASR::Block_t *block =
                    ASR::down_cast<ASR::Block_t>(bc->m_m);
                inline_sum_in_stmts(block->m_body, block->n_body,
                    block->m_symtab);
            }
            new_body.push_back(al, stmt);
            continue;
        }

        // Recurse into AssociateBlock bodies
        if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
            ASR::AssociateBlockCall_t *abc =
                ASR::down_cast<ASR::AssociateBlockCall_t>(stmt);
            ASR::AssociateBlock_t *ab =
                ASR::down_cast<ASR::AssociateBlock_t>(abc->m_m);
            inline_sum_in_stmts(ab->m_body, ab->n_body,
                ab->m_symtab);
            new_body.push_back(al, stmt);
            continue;
        }

        // Recurse into If bodies
        if (ASR::is_a<ASR::If_t>(*stmt)) {
            ASR::If_t *if_stmt = ASR::down_cast<ASR::If_t>(stmt);
            inline_sum_in_stmts(if_stmt->m_body, if_stmt->n_body,
                scope);
            inline_sum_in_stmts(if_stmt->m_orelse, if_stmt->n_orelse,
                scope);
            new_body.push_back(al, stmt);
            continue;
        }

        if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
            new_body.push_back(al, stmt);
            continue;
        }
        ASR::Assignment_t *asgn = ASR::down_cast<ASR::Assignment_t>(stmt);
        if (!ASR::is_a<ASR::IntrinsicArrayFunction_t>(*asgn->m_value)) {
            new_body.push_back(al, stmt);
            continue;
        }
        ASR::IntrinsicArrayFunction_t *iaf =
            ASR::down_cast<ASR::IntrinsicArrayFunction_t>(asgn->m_value);
        // Only `sum(array)` is lowered here. A `dim` or a `mask` is left
        // to the shared lowering, which the device runs as it is.
        if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
                iaf->m_arr_intrinsic_id)
                    != ASRUtils::IntrinsicArrayFunctions::Sum ||
                iaf->n_args != 1) {
            new_body.push_back(al, stmt);
            continue;
        }

        Location loc = stmt->base.loc;
        ASR::ttype_t *int_type = ASRUtils::TYPE(
            ASR::make_Integer_t(al, loc, 4));

        ASR::expr_t *arr_arg = iaf->m_args[0];
        if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arr_arg)) {
            arr_arg = ASR::down_cast<ASR::ArrayPhysicalCast_t>(
                arr_arg)->m_arg;
        }

        ASR::ttype_t *elem_type = iaf->m_type;

        SymbolTable *var_scope = scope;
        while (var_scope && var_scope->asr_owner &&
               var_scope->asr_owner->type == ASR::asrType::symbol &&
               ASR::is_a<ASR::AssociateBlock_t>(
                   *ASR::down_cast<ASR::symbol_t>(
                       var_scope->asr_owner))) {
            var_scope = var_scope->parent;
        }

        auto make_var = [&](const std::string &prefix,
                ASR::ttype_t *type) -> ASR::expr_t* {
            std::string name = var_scope->get_unique_name(prefix);
            ASR::symbol_t *sym = gpu_new_variable(al, loc, var_scope, name,
                ASRUtils::duplicate_type(al, type));
            return ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
        };

        // A loop that runs over the subscripts of the summed array itself,
        // or over the elements of an elemental expression from one to its
        // extent, counts in the dialect's kind for element offsets
        // (index_type). With 64-bit offsets the bounds of such a loop need
        // not fit integer(4): a lower bound beyond huge(0), or one near it
        // that the last index carries past it.
        ASR::ttype_t *counter_type = index_type(loc);

        // The last index of a dimension that starts at `start` and has
        // `length` elements.
        auto last_index = [&](ASR::expr_t *start,
                ASR::expr_t *length) -> ASR::expr_t* {
            if (is_int_literal(start, 1)) return length;
            if (device_caps.offset_kind != 4) {
                return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                    ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                        bound_to_index_kind(loc, start),
                        ASR::binopType::Add,
                        bound_to_index_kind(loc, length), counter_type,
                        nullptr)),
                    ASR::binopType::Sub,
                    ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 1,
                        counter_type, ASR::integerbozType::Decimal)),
                    counter_type, nullptr));
            }
            return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                    to_int32(loc, start), ASR::binopType::Add,
                    to_int32(loc, length), int_type, nullptr)),
                ASR::binopType::Sub, int32_const(loc, 1), int_type,
                nullptr));
        };

        ASR::expr_t *res_var = make_var("__gpu_sum_res", elem_type);
        ASR::expr_t *zero;
        if (ASR::is_a<ASR::Real_t>(*elem_type)) {
            zero = ASRUtils::EXPR(ASR::make_RealConstant_t(al, loc,
                0.0, elem_type));
        } else {
            zero = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
                0, elem_type, ASR::integerbozType::Decimal));
        }

        // __gpu_sum_res = 0
        new_body.push_back(al, ASRUtils::STMT(
            ASR::make_Assignment_t(al, loc, res_var, zero,
                nullptr, false, false, false, false)));

        std::vector<ASR::expr_t*> loop_vars;
        std::vector<ASR::expr_t*> loop_starts;
        std::vector<ASR::expr_t*> loop_ends;
        std::vector<ASR::expr_t*> loop_steps;
        Vec<ASR::array_index_t> idx_args = {};
        ASR::expr_t *base_arr = nullptr;
        ASR::expr_t *arr_elem = nullptr;

        if (ASR::is_a<ASR::ArraySection_t>(*arr_arg)) {
            // ArraySection (e.g., x(:,i)): loop over range dimensions,
            // use scalar indices directly
            ASR::ArraySection_t *section =
                ASR::down_cast<ASR::ArraySection_t>(arr_arg);
            base_arr = section->m_v;
            std::vector<size_t> range_dims;
            for (size_t d = 0; d < section->n_args; d++) {
                if (section->m_args[d].m_left != nullptr) {
                    range_dims.push_back(d);
                }
            }
            if (range_dims.empty()) {
                new_body.push_back(al, stmt);
                continue;
            }
            for (size_t ri = 0; ri < range_dims.size(); ri++) {
                size_t d = range_dims[ri];
                loop_vars.push_back(make_var("__gpu_sum_k", counter_type));
                loop_starts.push_back(section->m_args[d].m_left);
                loop_ends.push_back(section->m_args[d].m_right);
                loop_steps.push_back(section->m_args[d].m_step);
            }
            idx_args.reserve(al, section->n_args);
            size_t lv_idx = 0;
            for (size_t d = 0; d < section->n_args; d++) {
                ASR::array_index_t ai;
                ai.loc = loc;
                ai.m_left = nullptr;
                ai.m_step = nullptr;
                if (section->m_args[d].m_left != nullptr) {
                    ai.m_right = loop_vars[lv_idx++];
                } else {
                    ai.m_right = section->m_args[d].m_right;
                }
                idx_args.push_back(al, ai);
            }
        } else {
            // A whole array, or an elemental expression of arrays and
            // sections (a(1:n) + b(1:n)), is summed over every
            // dimension of the array it evaluates to.
            std::vector<std::pair<ASR::expr_t*, ASR::expr_t*>> dim_bounds;
            find_array_section_all_bounds(arr_arg, dim_bounds);
            if (!dim_bounds.empty()) {
                for (auto &bounds : dim_bounds) {
                    loop_vars.push_back(
                        make_var("__gpu_sum_k", counter_type));
                    loop_starts.push_back(bounds.first);
                    loop_ends.push_back(bounds.second);
                    loop_steps.push_back(nullptr);
                }
                arr_elem = elementize_array_expr(arr_arg, loop_vars, loc);
            } else {
                // Whole array: loop over all dimensions
                ASR::ttype_t *arr_type =
                    ASRUtils::expr_type(arr_arg);
                ASR::dimension_t *dims = nullptr;
                int rank =
                    checked_integer_cast<int>(ASRUtils::extract_dimensions_from_ttype(
                        arr_type, dims));
                if (rank < 1) {
                    new_body.push_back(al, stmt);
                    continue;
                }

                // If arr_arg is a FunctionCall returning an
                // allocatable copy of a struct member (e.g.,
                // sum(vals(x)) where vals returns x%v), resolve
                // to the actual struct member access (x%v) so the
                // sum loop iterates directly over the member data
                // without allocating a temporary array.
                if (ASR::is_a<ASR::FunctionCall_t>(*arr_arg)) {
                    ASR::FunctionCall_t *fc2 =
                        ASR::down_cast<ASR::FunctionCall_t>(
                            arr_arg);
                    ASR::symbol_t *fn_sym2 =
                        ASRUtils::symbol_get_past_external(
                            fc2->m_name);
                    if (ASR::is_a<ASR::Function_t>(*fn_sym2)) {
                        ASR::Function_t *fn2 =
                            ASR::down_cast<ASR::Function_t>(
                                fn_sym2);
                        if (fn2->m_return_var &&
                                ASR::is_a<ASR::Var_t>(
                                    *fn2->m_return_var)) {
                            std::string ret_name2 =
                                ASRUtils::symbol_name(
                                    ASR::down_cast<ASR::Var_t>(
                                        fn2->m_return_var)->m_v);
                            for (size_t bi = 0;
                                    bi < fn2->n_body; bi++) {
                                if (!ASR::is_a<ASR::Assignment_t>(
                                        *fn2->m_body[bi]))
                                    continue;
                                ASR::Assignment_t *ba =
                                    ASR::down_cast<
                                        ASR::Assignment_t>(
                                            fn2->m_body[bi]);
                                if (!ASR::is_a<ASR::Var_t>(
                                        *ba->m_target))
                                    continue;
                                std::string tname =
                                    ASRUtils::symbol_name(
                                        ASR::down_cast<
                                            ASR::Var_t>(
                                                ba->m_target)
                                            ->m_v);
                                if (tname != ret_name2) continue;
                                if (!ASR::is_a<
                                        ASR::StructInstanceMember_t>(
                                            *ba->m_value))
                                    continue;
                                ASR::StructInstanceMember_t *sim =
                                    ASR::down_cast<
                                        ASR::StructInstanceMember_t>(
                                            ba->m_value);
                                if (!ASR::is_a<ASR::Var_t>(
                                        *sim->m_v))
                                    continue;
                                ASR::symbol_t *param_sym2 =
                                    ASR::down_cast<ASR::Var_t>(
                                        sim->m_v)->m_v;
                                int pidx = -1;
                                for (size_t pi = 0;
                                        pi < fn2->n_args; pi++) {
                                    if (ASR::is_a<ASR::Var_t>(
                                            *fn2->m_args[pi]) &&
                                        ASR::down_cast<ASR::Var_t>(
                                            fn2->m_args[pi])
                                            ->m_v == param_sym2) {
                                        pidx = (int)pi;
                                        break;
                                    }
                                }
                                if (pidx < 0 ||
                                    (size_t)pidx >= fc2->n_args ||
                                    !fc2->m_args[pidx].m_value)
                                    break;
                                ASR::expr_t *actual =
                                    fc2->m_args[pidx].m_value;
                                // Create ExternalSymbol for the
                                // struct member in the caller scope
                                ASR::symbol_t *orig_mem =
                                    ASRUtils::
                                        symbol_get_past_external(
                                            sim->m_m);
                                std::string mem_name =
                                    ASRUtils::symbol_name(orig_mem);
                                SymbolTable *mem_st =
                                    ASRUtils::
                                        symbol_parent_symtab(
                                            orig_mem);
                                ASR::symbol_t *struct_sym2 =
                                    ASR::down_cast<ASR::symbol_t>(
                                        mem_st->asr_owner);
                                std::string sname =
                                    ASRUtils::symbol_name(
                                        struct_sym2);
                                std::string ext_name =
                                    var_scope->get_unique_name(
                                        "1_" + sname + "_"
                                        + mem_name);
                                ASR::symbol_t *ext_sym =
                                    ASR::down_cast<ASR::symbol_t>(
                                        ASR::make_ExternalSymbol_t(
                                            al, loc, var_scope,
                                            s2c(al, ext_name),
                                            orig_mem,
                                            s2c(al, sname),
                                            nullptr, 0,
                                            s2c(al, mem_name),
                                            ASR::accessType::
                                                Public));
                                var_scope->add_symbol(
                                    ext_name, ext_sym);
                                arr_arg = ASRUtils::EXPR(
                                    ASR::make_StructInstanceMember_t(
                                        al, loc, actual, ext_sym,
                                        sim->m_type, nullptr));
                                arr_type =
                                    ASRUtils::
                                        type_get_past_allocatable_pointer(
                                            ASRUtils::expr_type(
                                                arr_arg));
                                dims = nullptr;
                                rank =
                                    checked_integer_cast<int>(ASRUtils::
                                        extract_dimensions_from_ttype(
                                            arr_type, dims));
                                break;
                            }
                        }
                    }
                }

                base_arr = arr_arg;
                for (int d = 0; d < rank; d++) {
                    loop_vars.push_back(
                        make_var("__gpu_sum_k", counter_type));
                    if (dims[d].m_start && dims[d].m_length) {
                        loop_starts.push_back(dims[d].m_start);
                        loop_ends.push_back(last_index(dims[d].m_start,
                            dims[d].m_length));
                    } else if (ASR::is_a<ASR::FunctionCall_t>(
                            *arr_arg)) {
                        // FunctionCall returns allocatable with
                        // unknown dims. Extract allocation bounds
                        // from the function body to avoid emitting
                        // ArrayBound on a FunctionCall (unsupported
                        // by Metal codegen).
                        ASR::FunctionCall_t *fc =
                            ASR::down_cast<ASR::FunctionCall_t>(
                                arr_arg);
                        ASR::symbol_t *fn_sym =
                            ASRUtils::symbol_get_past_external(
                                fc->m_name);
                        bool found = false;
                        if (ASR::is_a<ASR::Function_t>(*fn_sym)) {
                            ASR::Function_t *fn =
                                ASR::down_cast<ASR::Function_t>(
                                    fn_sym);
                            std::string ret_name;
                            if (fn->m_return_var &&
                                    ASR::is_a<ASR::Var_t>(
                                        *fn->m_return_var)) {
                                ret_name =
                                    ASRUtils::symbol_name(
                                        ASR::down_cast<
                                            ASR::Var_t>(
                                            fn->m_return_var)
                                            ->m_v);
                            }
                            for (size_t bi = 0;
                                    bi < fn->n_body &&
                                    !ret_name.empty() && !found;
                                    bi++) {
                                if (!ASR::is_a<ASR::Allocate_t>(
                                        *fn->m_body[bi]))
                                    continue;
                                ASR::Allocate_t *al_stmt =
                                    ASR::down_cast<
                                        ASR::Allocate_t>(
                                            fn->m_body[bi]);
                                for (size_t ai2 = 0;
                                        ai2 < al_stmt->n_args;
                                        ai2++) {
                                    if (!al_stmt->m_args[ai2].m_a
                                        || !ASR::is_a<ASR::Var_t>(
                                            *al_stmt->m_args[ai2]
                                                .m_a))
                                        continue;
                                    std::string aname =
                                        ASRUtils::symbol_name(
                                            ASR::down_cast<
                                                ASR::Var_t>(
                                                al_stmt->m_args
                                                    [ai2].m_a)
                                                ->m_v);
                                    if (aname != ret_name)
                                        continue;
                                    if ((size_t)d <
                                            al_stmt->m_args[ai2]
                                                .n_dims) {
                                        ASR::dimension_t &adim =
                                            al_stmt->m_args[ai2]
                                                .m_dims[d];
                                        if (adim.m_start) {
                                            loop_starts.push_back(
                                                adim.m_start);
                                        } else {
                                            loop_starts.push_back(
                                                ASRUtils::EXPR(
                                                    ASR::make_IntegerConstant_t(
                                                        al, loc,
                                                        1,
                                                        int_type,
                                                        ASR::integerbozType::Decimal)));
                                        }
                                        if (adim.m_length) {
                                            loop_ends.push_back(
                                                last_index(
                                                    loop_starts.back(),
                                                    adim.m_length));
                                        }
                                        found = true;
                                    }
                                    break;
                                }
                            }
                        }
                        if (!found) {
                            // No Allocate found in the function
                            // body.  Fall back to the actual call
                            // arguments: use the first array
                            // actual argument's bounds (the
                            // return shape typically matches the
                            // input shape for element-wise
                            // functions like r = a).
                            for (size_t ai3 = 0;
                                    ai3 < fc->n_args && !found;
                                    ai3++) {
                                if (!fc->m_args[ai3].m_value)
                                    continue;
                                ASR::expr_t *actual =
                                    fc->m_args[ai3].m_value;
                                if (ASR::is_a<
                                        ASR::ArrayPhysicalCast_t>(
                                            *actual)) {
                                    actual = ASR::down_cast<
                                        ASR::ArrayPhysicalCast_t>(
                                            actual)->m_arg;
                                }
                                ASR::ttype_t *atype =
                                    ASRUtils::type_get_past_allocatable_pointer(
                                        ASRUtils::expr_type(
                                            actual));
                                ASR::dimension_t *adims = nullptr;
                                int arank =
                                    checked_integer_cast<int>(ASRUtils::extract_dimensions_from_ttype(
                                        atype, adims));
                                if (arank < 1 ||
                                        (size_t)d >= (size_t)arank)
                                    continue;
                                if (adims[d].m_start &&
                                        adims[d].m_length) {
                                    loop_starts.push_back(
                                        adims[d].m_start);
                                    loop_ends.push_back(last_index(
                                        adims[d].m_start,
                                        adims[d].m_length));
                                    found = true;
                                }
                            }
                        }
                        if (!found) {
                            ASR::expr_t *dim_expr =
                                ASRUtils::EXPR(
                                    ASR::make_IntegerConstant_t(
                                        al, loc, d + 1,
                                        int_type,
                                        ASR::integerbozType::Decimal));
                            loop_starts.push_back(ASRUtils::EXPR(
                                ASR::make_ArrayBound_t(al, loc,
                                    arr_arg, dim_expr, int_type,
                                    ASR::arrayboundType::LBound,
                                    nullptr)));
                            loop_ends.push_back(ASRUtils::EXPR(
                                ASR::make_ArrayBound_t(al, loc,
                                    arr_arg, dim_expr, int_type,
                                    ASR::arrayboundType::UBound,
                                    nullptr)));
                        }
                    } else {
                        ASR::expr_t *dim_expr = ASRUtils::EXPR(
                            ASR::make_IntegerConstant_t(al, loc,
                                d + 1, int_type,
                                ASR::integerbozType::Decimal));
                        loop_starts.push_back(ASRUtils::EXPR(
                            ASR::make_ArrayBound_t(al, loc,
                                arr_arg, dim_expr, int_type,
                                ASR::arrayboundType::LBound,
                                nullptr)));
                        loop_ends.push_back(ASRUtils::EXPR(
                            ASR::make_ArrayBound_t(al, loc,
                                arr_arg, dim_expr, int_type,
                                ASR::arrayboundType::UBound,
                                nullptr)));
                    }
                    loop_steps.push_back(nullptr);
                }
                idx_args.reserve(al, rank);
                for (int d = 0; d < rank; d++) {
                    ASR::array_index_t ai;
                    ai.loc = loc;
                    ai.m_left = nullptr;
                    ai.m_right = loop_vars[d];
                    ai.m_step = nullptr;
                    idx_args.push_back(al, ai);
                }
            }
        }

        if (!arr_elem) {
            arr_elem = index_array_expr(base_arr,
                    idx_args.p, idx_args.n, loc);
        }

        // res = res + a(k1, k2, ...) or a(k1, i, ...)
        ASR::expr_t *add_expr;
        if (ASR::is_a<ASR::Real_t>(*elem_type)) {
            add_expr = ASRUtils::EXPR(ASR::make_RealBinOp_t(al, loc,
                res_var, ASR::binopType::Add, arr_elem,
                elem_type, nullptr));
        } else {
            add_expr = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                res_var, ASR::binopType::Add, arr_elem,
                elem_type, nullptr));
        }
        ASR::stmt_t *accum_stmt = ASRUtils::STMT(
            ASR::make_Assignment_t(al, loc, res_var, add_expr,
                nullptr, false, false, false, false));

        // Build nested loops from innermost to outermost
        int n_loops = (int)loop_vars.size();
        Vec<ASR::stmt_t*> innermost_body;
        innermost_body.reserve(al, 1);
        innermost_body.push_back(al, accum_stmt);

        // The bounds may be the very expressions an array's declaration
        // or an ALLOCATE of it holds; the loops get their own copies
        // (own_bound).
        ASR::stmt_t *loop_nest = nullptr;
        for (int d = n_loops - 1; d >= 0; d--) {
            ASR::do_loop_head_t head;
            head.loc = loc;
            head.m_v = loop_vars[d];
            head.m_start = own_bound(loop_starts[d]);
            head.m_end = own_bound(loop_ends[d]);
            head.m_increment = loop_steps[d];
            if (device_caps.offset_kind != 4
                    && ASRUtils::extract_kind_from_ttype_t(
                        ASRUtils::expr_type(loop_vars[d]))
                            == device_caps.offset_kind) {
                head.m_start = bound_to_index_kind(loc, head.m_start);
                head.m_end = bound_to_index_kind(loc, head.m_end);
                head.m_increment = bound_to_index_kind(loc,
                    head.m_increment);
            }
            if (d == n_loops - 1) {
                loop_nest = ASRUtils::STMT(ASRUtils::make_DoLoop_t_util(al, loc,
                    nullptr, head, innermost_body.p, innermost_body.n,
                    nullptr, 0));
            } else {
                Vec<ASR::stmt_t*> outer_body;
                outer_body.reserve(al, 1);
                outer_body.push_back(al, loop_nest);
                loop_nest = ASRUtils::STMT(ASRUtils::make_DoLoop_t_util(al, loc,
                    nullptr, head, outer_body.p, outer_body.n,
                    nullptr, 0));
            }
        }
        new_body.push_back(al, loop_nest);

        // target = __gpu_sum_res
        new_body.push_back(al, ASRUtils::STMT(
            ASR::make_Assignment_t(al, loc, asgn->m_target, res_var,
                nullptr, false, false, false, false)));

        changed = true;
    }

    if (changed) {
        stmts = new_body.p;
        n_stmts = new_body.n;
    }
}

void GpuOffloadVisitor::inline_intrinsic_sum(ParallelLoopNest &nest) {
    NestBodyWriteBack back(nest);
    inline_sum_in_stmts(back.body, back.n_body, current_scope);
}

// Inline IntrinsicArrayFunction Transpose inside a parallel loop body.
// Replaces:
//   b = transpose(a)
// With:
//   do __gpu_tr_j = 1, n
//     do __gpu_tr_i = 1, m
//       b(__gpu_tr_i, __gpu_tr_j) = a(__gpu_tr_j, __gpu_tr_i)
//     end do
//   end do
// This avoids generating a call to _lcompilers_transpose which is not
// available inside Metal GPU kernels.
void GpuOffloadVisitor::inline_intrinsic_transpose(ParallelLoopNest &nest) {
    ASR::stmt_t **body = nest.body;
    size_t n_body = nest.n_body;
    if (inline_transpose_stmts(body, n_body)) {
        // Through set_body, so the loop the nest reads from carries
        // the rewrite too and not just the view of it.
        nest.set_body(body, n_body);
    }
}

// Lowers each `b = transpose(a)` of `body`, and of the BLOCKs made for
// an assignment whose target a TRANSPOSE operand reads (whose own
// statements are alias-free), into element loops. True when `body`
// itself changed.
bool GpuOffloadVisitor::inline_transpose_stmts(ASR::stmt_t **&body,
        size_t &n_body) {
    Vec<ASR::stmt_t*> new_body;
    new_body.reserve(al, n_body * 4);
    bool changed = false;

    for (size_t si = 0; si < n_body; si++) {
        ASR::stmt_t *stmt = body[si];
        if (ASR::is_a<ASR::BlockCall_t>(*stmt)
                && non_elemental_alias_blocks.count(
                    ASR::down_cast<ASR::BlockCall_t>(stmt)->m_m)) {
            ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(
                ASR::down_cast<ASR::BlockCall_t>(stmt)->m_m);
            inline_transpose_stmts(block->m_body, block->n_body);
            new_body.push_back(al, stmt);
            continue;
        }
        if (!ASR::is_a<ASR::Assignment_t>(*stmt)) {
            new_body.push_back(al, stmt);
            continue;
        }
        ASR::Assignment_t *asgn = ASR::down_cast<ASR::Assignment_t>(stmt);
        ASR::expr_t *value = asgn->m_value;
        if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*value)) {
            value = ASR::down_cast<ASR::ArrayPhysicalCast_t>(value)->m_arg;
        }
        if (!ASR::is_a<ASR::IntrinsicArrayFunction_t>(*value)) {
            new_body.push_back(al, stmt);
            continue;
        }
        ASR::IntrinsicArrayFunction_t *iaf =
            ASR::down_cast<ASR::IntrinsicArrayFunction_t>(value);
        if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
                iaf->m_arr_intrinsic_id)
                    != ASRUtils::IntrinsicArrayFunctions::Transpose) {
            new_body.push_back(al, stmt);
            continue;
        }

        Location loc = stmt->base.loc;
        ASR::ttype_t *int_type = ASRUtils::TYPE(
            ASR::make_Integer_t(al, loc, 4));

        ASR::expr_t *arr_arg = iaf->m_args[0];
        if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arr_arg)) {
            arr_arg = ASR::down_cast<ASR::ArrayPhysicalCast_t>(
                arr_arg)->m_arg;
        }

        ASR::ttype_t *arr_type = ASRUtils::expr_type(arr_arg);
        ASR::dimension_t *dims = nullptr;
        int rank = checked_integer_cast<int>(ASRUtils::extract_dimensions_from_ttype(arr_type, dims));
        if (rank != 2) {
            new_body.push_back(al, stmt);
            continue;
        }

        ASR::ttype_t *elem_type = ASRUtils::extract_type(
            ASRUtils::expr_type(asgn->m_target));

        SymbolTable *var_scope = current_scope;
        while (var_scope && var_scope->asr_owner &&
               var_scope->asr_owner->type == ASR::asrType::symbol &&
               ASR::is_a<ASR::AssociateBlock_t>(
                   *ASR::down_cast<ASR::symbol_t>(
                       var_scope->asr_owner))) {
            var_scope = var_scope->parent;
        }

        auto make_var = [&](const std::string &prefix) -> ASR::expr_t* {
            std::string name = var_scope->get_unique_name(prefix);
            ASR::symbol_t *sym = gpu_new_variable(al, loc, var_scope, name,
                ASRUtils::duplicate_type(al, int_type));
            return ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
        };

        auto make_array_item_2d = [&](ASR::expr_t *arr,
                ASR::expr_t *idx1, ASR::expr_t *idx2) -> ASR::expr_t* {
            Vec<ASR::array_index_t> args;
            args.reserve(al, 2);
            ASR::array_index_t ai1;
            ai1.loc = loc;
            ai1.m_left = nullptr;
            ai1.m_right = idx1;
            ai1.m_step = nullptr;
            args.push_back(al, ai1);
            ASR::array_index_t ai2;
            ai2.loc = loc;
            ai2.m_left = nullptr;
            ai2.m_right = idx2;
            ai2.m_step = nullptr;
            args.push_back(al, ai2);
            return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc, arr,
                args.p, args.n, elem_type,
                ASR::arraystorageType::ColMajor, nullptr, false));
        };

        // a is (m, n) => b = transpose(a) is (n, m)
        // b(i, j) = a(j, i) for i=1..n, j=1..m
        // The bounds come from the declaration of `a`, so the loops get
        // their own copies of them (own_bound).
        ASR::expr_t *var_i = make_var("__gpu_tr_i");
        ASR::expr_t *var_j = make_var("__gpu_tr_j");

        ASR::expr_t *b_ij = make_array_item_2d(asgn->m_target,
            var_i, var_j);
        ASR::expr_t *a_ji = make_array_item_2d(arr_arg,
            var_j, var_i);

        // Inner loop body: b(i, j) = a(j, i)
        Vec<ASR::stmt_t*> inner_body;
        inner_body.reserve(al, 1);
        inner_body.push_back(al, ASRUtils::STMT(
            ASR::make_Assignment_t(al, loc, b_ij, a_ji,
                nullptr, false, false, false, false)));

        // i loops over rows of b = columns of a (dim 1 of result)
        // j loops over columns of b = rows of a (dim 0 of result)
        // a(m, n): dims[0] = m, dims[1] = n
        // b(n, m): i = 1..n, j = 1..m
        ASR::do_loop_head_t inner_head;
        inner_head.loc = loc;
        inner_head.m_v = var_i;
        set_loop_head_bounds(al, loc, inner_head, dims, 1, arr_arg);
        inner_head.m_start = own_bound(inner_head.m_start);
        inner_head.m_end = own_bound(inner_head.m_end);
        inner_head.m_increment = nullptr;
        ASR::stmt_t *inner_loop = ASRUtils::STMT(
            ASRUtils::make_DoLoop_t_util(al, loc, nullptr, inner_head,
                inner_body.p, inner_body.n, nullptr, 0));

        Vec<ASR::stmt_t*> outer_body;
        outer_body.reserve(al, 1);
        outer_body.push_back(al, inner_loop);

        ASR::do_loop_head_t outer_head;
        outer_head.loc = loc;
        outer_head.m_v = var_j;
        set_loop_head_bounds(al, loc, outer_head, dims, 0, arr_arg);
        outer_head.m_start = own_bound(outer_head.m_start);
        outer_head.m_end = own_bound(outer_head.m_end);
        outer_head.m_increment = nullptr;
        ASR::stmt_t *outer_loop = ASRUtils::STMT(
            ASRUtils::make_DoLoop_t_util(al, loc, nullptr, outer_head,
                outer_body.p, outer_body.n, nullptr, 0));

        new_body.push_back(al, outer_loop);
        changed = true;
    }

    if (changed) {
        body = new_body.p;
        n_body = new_body.n;
    }
    return changed;
}

} // namespace LCompilers
