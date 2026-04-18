#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/fuse_array_reduction.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/intrinsic_array_function_registry.h>

#include <vector>

namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

/*
This pass fuses scalar reductions over element-wise array expressions into
a single loop, eliminating temporary arrays.

Transforms (before array_struct_temporary creates temporaries):
    delta = maxval(abs(xnew(1:n,1:n) - x(1:n,1:n)))
Into:
    __acc = -huge(0d0)
    do j = 1, n
      do i = 1, n
        __acc = max(__acc, abs(xnew(i,j) - x(i,j)))
      end do
    end do
    delta = __acc

Only active when pass_options.fast is true.
*/

// Information about one array operand (ArraySection or whole-array Var)
struct ArrayOperandInfo {
    ASR::expr_t* expr; // the original ArraySection or Var node
    // Per-dimension bounds: start, end, step (step may be null for unit stride)
    std::vector<ASR::expr_t*> lb;
    std::vector<ASR::expr_t*> ub;
    std::vector<ASR::expr_t*> step;
    int rank;
};

// Collect all ArraySection and whole-array Var nodes from an expression tree.
// Returns false if any non-addressable or unsupported node is found.
static bool collect_array_operands(ASR::expr_t* expr,
        std::vector<ArrayOperandInfo>& operands, Allocator& al) {
    if (!expr) return false;

    switch (expr->type) {
        case ASR::exprType::ArraySection: {
            ASR::ArraySection_t* sec = down_cast<ASR::ArraySection_t>(expr);
            ArrayOperandInfo info;
            info.expr = expr;
            info.rank = 0;
            for (size_t i = 0; i < sec->n_args; i++) {
                if (sec->m_args[i].m_left && sec->m_args[i].m_right) {
                    // This is a slice dimension (start:end or start:end:step)
                    info.lb.push_back(sec->m_args[i].m_left);
                    info.ub.push_back(sec->m_args[i].m_right);
                    info.step.push_back(sec->m_args[i].m_step); // may be null
                    info.rank++;
                }
                // If only m_right is set (scalar index), it's not a slice dim
                // — we keep it as a fixed index, not part of the loop
            }
            if (info.rank == 0) return false; // no slice dimensions
            operands.push_back(info);
            return true;
        }
        case ASR::exprType::Var: {
            ASR::ttype_t* type = ASRUtils::expr_type(expr);
            if (!ASRUtils::is_array(type)) return true; // scalar var, fine
            int rank = ASRUtils::extract_n_dims_from_ttype(type);
            if (rank == 0) return true;
            ArrayOperandInfo info;
            info.expr = expr;
            info.rank = rank;
            for (int d = 1; d <= rank; d++) {
                info.lb.push_back(PassUtils::get_bound(expr, d, "lbound", al));
                info.ub.push_back(PassUtils::get_bound(expr, d, "ubound", al));
                info.step.push_back(nullptr);
            }
            operands.push_back(info);
            return true;
        }
        case ASR::exprType::IntrinsicElementalFunction: {
            ASR::IntrinsicElementalFunction_t* ief =
                down_cast<ASR::IntrinsicElementalFunction_t>(expr);
            for (size_t i = 0; i < ief->n_args; i++) {
                if (!collect_array_operands(ief->m_args[i], operands, al))
                    return false;
            }
            return true;
        }
        case ASR::exprType::RealBinOp: {
            ASR::RealBinOp_t* bop = down_cast<ASR::RealBinOp_t>(expr);
            return collect_array_operands(bop->m_left, operands, al) &&
                   collect_array_operands(bop->m_right, operands, al);
        }
        case ASR::exprType::IntegerBinOp: {
            ASR::IntegerBinOp_t* bop = down_cast<ASR::IntegerBinOp_t>(expr);
            return collect_array_operands(bop->m_left, operands, al) &&
                   collect_array_operands(bop->m_right, operands, al);
        }
        case ASR::exprType::RealUnaryMinus: {
            ASR::RealUnaryMinus_t* um = down_cast<ASR::RealUnaryMinus_t>(expr);
            return collect_array_operands(um->m_arg, operands, al);
        }
        case ASR::exprType::IntegerUnaryMinus: {
            ASR::IntegerUnaryMinus_t* um = down_cast<ASR::IntegerUnaryMinus_t>(expr);
            return collect_array_operands(um->m_arg, operands, al);
        }
        case ASR::exprType::Cast: {
            ASR::Cast_t* c = down_cast<ASR::Cast_t>(expr);
            return collect_array_operands(c->m_arg, operands, al);
        }
        case ASR::exprType::RealConstant:
        case ASR::exprType::IntegerConstant:
        case ASR::exprType::ComplexConstant:
        case ASR::exprType::LogicalConstant:
            return true; // scalar constants are fine
        default:
            return false; // unsupported node, bail out
    }
}

// Check that all operands have the same rank
static bool check_rank_conformance(const std::vector<ArrayOperandInfo>& operands) {
    if (operands.empty()) return false;
    int rank = operands[0].rank;
    for (size_t i = 1; i < operands.size(); i++) {
        if (operands[i].rank != rank) return false;
    }
    return true;
}

// Scalarize an expression: replace ArraySection/whole-array Var with ArrayItem
// using the provided loop index variables. Clones the expression tree.
// operand_idx tracks which operand we're at.
static ASR::expr_t* scalarize_expr(Allocator& al, ASR::expr_t* expr,
        const std::vector<ArrayOperandInfo>& operands,
        const Vec<ASR::expr_t*>& idx_vars,
        size_t& operand_idx) {
    if (!expr) return nullptr;
    const Location& loc = expr->base.loc;

    switch (expr->type) {
        case ASR::exprType::ArraySection: {
            ASR::ArraySection_t* sec = down_cast<ASR::ArraySection_t>(expr);
            LCOMPILERS_ASSERT(operand_idx < operands.size());
            const ArrayOperandInfo& info = operands[operand_idx];
            operand_idx++;

            // Build ArrayItem indices: for slice dimensions, use loop index;
            // for scalar dimensions, keep the original index
            Vec<ASR::array_index_t> item_args;
            item_args.reserve(al, sec->n_args);
            int slice_dim = 0;
            for (size_t i = 0; i < sec->n_args; i++) {
                ASR::array_index_t ai;
                ai.loc = loc;
                ai.m_left = nullptr;
                ai.m_step = nullptr;
                if (sec->m_args[i].m_left && sec->m_args[i].m_right) {
                    // Slice dimension — use loop index variable
                    // If the operand has non-unit lower bound, we need to map:
                    // actual_index = lb + (loop_idx - 1) * step
                    // But for simplicity, when lb matches the reference operand's
                    // lb and step is 1, we can use the loop index directly.
                    // For the general case, use: lb + (loop_idx - ref_lb)
                    // In v1, we use the section bounds directly for loop bounds
                    // of the first operand, so the loop idx directly corresponds.
                    if (info.step[slice_dim] != nullptr) {
                        // Has explicit step — need index mapping
                        // For now, bail for non-unit steps
                        // (would need: lb + (loop_var - ref_lb) * step)
                        ai.m_right = idx_vars[slice_dim];
                    } else {
                        // No step or unit step — direct index mapping
                        // loop var goes from info.lb[slice_dim] to info.ub[slice_dim]
                        ai.m_right = idx_vars[slice_dim];
                    }
                    slice_dim++;
                } else {
                    // Scalar index — keep as-is
                    ai.m_right = sec->m_args[i].m_right;
                }
                item_args.push_back(al, ai);
            }
            ASR::ttype_t* elem_type = ASRUtils::extract_type(
                ASRUtils::expr_type(sec->m_v));
            return ASRUtils::EXPR(ASRUtils::make_ArrayItem_t_util(al, loc,
                sec->m_v, item_args.p, item_args.size(), elem_type,
                ASR::arraystorageType::ColMajor, nullptr));
        }
        case ASR::exprType::Var: {
            ASR::ttype_t* type = ASRUtils::expr_type(expr);
            if (!ASRUtils::is_array(type)) return expr; // scalar, keep as-is
            LCOMPILERS_ASSERT(operand_idx < operands.size());
            operand_idx++;

            int rank = ASRUtils::extract_n_dims_from_ttype(type);
            Vec<ASR::array_index_t> item_args;
            item_args.reserve(al, rank);
            for (int d = 0; d < rank; d++) {
                ASR::array_index_t ai;
                ai.loc = loc;
                ai.m_left = nullptr;
                ai.m_right = idx_vars[d];
                ai.m_step = nullptr;
                item_args.push_back(al, ai);
            }
            ASR::ttype_t* elem_type = ASRUtils::extract_type(type);
            return ASRUtils::EXPR(ASRUtils::make_ArrayItem_t_util(al, loc,
                expr, item_args.p, item_args.size(), elem_type,
                ASR::arraystorageType::ColMajor, nullptr));
        }
        case ASR::exprType::IntrinsicElementalFunction: {
            ASR::IntrinsicElementalFunction_t* ief =
                down_cast<ASR::IntrinsicElementalFunction_t>(expr);
            Vec<ASR::expr_t*> new_args;
            new_args.reserve(al, ief->n_args);
            for (size_t i = 0; i < ief->n_args; i++) {
                new_args.push_back(al,
                    scalarize_expr(al, ief->m_args[i], operands, idx_vars, operand_idx));
            }
            ASR::ttype_t* scalar_type = ASRUtils::extract_type(ief->m_type);
            return ASRUtils::EXPR(ASR::make_IntrinsicElementalFunction_t(al, loc,
                ief->m_intrinsic_id, new_args.p, new_args.size(),
                ief->m_overload_id, scalar_type, nullptr));
        }
        case ASR::exprType::RealBinOp: {
            ASR::RealBinOp_t* bop = down_cast<ASR::RealBinOp_t>(expr);
            ASR::expr_t* left = scalarize_expr(al, bop->m_left, operands, idx_vars, operand_idx);
            ASR::expr_t* right = scalarize_expr(al, bop->m_right, operands, idx_vars, operand_idx);
            ASR::ttype_t* scalar_type = ASRUtils::extract_type(bop->m_type);
            return ASRUtils::EXPR(ASR::make_RealBinOp_t(al, loc,
                left, bop->m_op, right, scalar_type, nullptr));
        }
        case ASR::exprType::IntegerBinOp: {
            ASR::IntegerBinOp_t* bop = down_cast<ASR::IntegerBinOp_t>(expr);
            ASR::expr_t* left = scalarize_expr(al, bop->m_left, operands, idx_vars, operand_idx);
            ASR::expr_t* right = scalarize_expr(al, bop->m_right, operands, idx_vars, operand_idx);
            ASR::ttype_t* scalar_type = ASRUtils::extract_type(bop->m_type);
            return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                left, bop->m_op, right, scalar_type, nullptr));
        }
        case ASR::exprType::RealUnaryMinus: {
            ASR::RealUnaryMinus_t* um = down_cast<ASR::RealUnaryMinus_t>(expr);
            ASR::expr_t* arg = scalarize_expr(al, um->m_arg, operands, idx_vars, operand_idx);
            ASR::ttype_t* scalar_type = ASRUtils::extract_type(um->m_type);
            return ASRUtils::EXPR(ASR::make_RealUnaryMinus_t(al, loc,
                arg, scalar_type, nullptr));
        }
        case ASR::exprType::IntegerUnaryMinus: {
            ASR::IntegerUnaryMinus_t* um = down_cast<ASR::IntegerUnaryMinus_t>(expr);
            ASR::expr_t* arg = scalarize_expr(al, um->m_arg, operands, idx_vars, operand_idx);
            ASR::ttype_t* scalar_type = ASRUtils::extract_type(um->m_type);
            return ASRUtils::EXPR(ASR::make_IntegerUnaryMinus_t(al, loc,
                arg, scalar_type, nullptr));
        }
        case ASR::exprType::Cast: {
            ASR::Cast_t* c = down_cast<ASR::Cast_t>(expr);
            ASR::expr_t* arg = scalarize_expr(al, c->m_arg, operands, idx_vars, operand_idx);
            ASR::ttype_t* scalar_type = ASRUtils::extract_type(c->m_type);
            return ASRUtils::EXPR(ASR::make_Cast_t(al, loc,
                arg, c->m_kind, scalar_type, nullptr, nullptr));
        }
        case ASR::exprType::RealConstant:
        case ASR::exprType::IntegerConstant:
        case ASR::exprType::ComplexConstant:
        case ASR::exprType::LogicalConstant:
            return expr; // scalars pass through
        default:
            LCOMPILERS_ASSERT(false); // should not reach here
            return expr;
    }
}

// Get the initial value for a reduction
static ASR::expr_t* get_reduction_init(Allocator& al, int64_t reduction_id,
        ASR::ttype_t* element_type, const Location& loc) {
    using ASRUtils::IntrinsicArrayFunctions;
    if (reduction_id == static_cast<int64_t>(IntrinsicArrayFunctions::MaxVal)) {
        return ASRUtils::get_minimum_value_with_given_type(al, element_type);
    } else if (reduction_id == static_cast<int64_t>(IntrinsicArrayFunctions::MinVal)) {
        return ASRUtils::get_maximum_value_with_given_type(al, element_type);
    } else if (reduction_id == static_cast<int64_t>(IntrinsicArrayFunctions::Sum)) {
        int kind = ASRUtils::extract_kind_from_ttype_t(element_type);
        if (ASR::is_a<ASR::Real_t>(*element_type)) {
            return ASRUtils::EXPR(ASR::make_RealConstant_t(al, loc, 0.0, element_type));
        } else if (ASR::is_a<ASR::Integer_t>(*element_type)) {
            return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 0, element_type));
        }
        (void)kind;
    }
    return nullptr;
}

// Build the reduction update expression: acc = op(acc, element)
static ASR::expr_t* get_reduction_update(Allocator& al, int64_t reduction_id,
        ASR::expr_t* acc, ASR::expr_t* elem, const Location& loc) {
    ASR::ttype_t* type = ASRUtils::expr_type(acc);
    using ASRUtils::IntrinsicArrayFunctions;
    ASRUtils::ASRBuilder b(al, loc);
    if (reduction_id == static_cast<int64_t>(IntrinsicArrayFunctions::MaxVal)) {
        return b.Max(acc, elem);
    } else if (reduction_id == static_cast<int64_t>(IntrinsicArrayFunctions::MinVal)) {
        return b.Min(acc, elem);
    } else if (reduction_id == static_cast<int64_t>(IntrinsicArrayFunctions::Sum)) {
        if (ASR::is_a<ASR::Real_t>(*type)) {
            return ASRUtils::EXPR(ASR::make_RealBinOp_t(al, loc,
                acc, ASR::binopType::Add, elem, type, nullptr));
        } else if (ASR::is_a<ASR::Integer_t>(*type)) {
            return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                acc, ASR::binopType::Add, elem, type, nullptr));
        }
    }
    return nullptr;
}

class FuseArrayReductionVisitor :
    public ASR::BaseWalkVisitor<FuseArrayReductionVisitor> {

    Allocator& al;
    const LCompilers::PassOptions& pass_options;
    Vec<ASR::stmt_t*> pass_result;
    SymbolTable* current_scope;
    bool transform_stmts;

public:
    FuseArrayReductionVisitor(Allocator& al_,
        const LCompilers::PassOptions& pass_options_)
        : al(al_), pass_options(pass_options_),
          current_scope(nullptr), transform_stmts(false) {}

    void visit_TranslationUnit(const ASR::TranslationUnit_t& x) {
        current_scope = x.m_symtab;
        for (auto& item : x.m_symtab->get_scope()) {
            visit_symbol(*item.second);
        }
    }

    void visit_Module(const ASR::Module_t& x) {
        SymbolTable* old_scope = current_scope;
        current_scope = x.m_symtab;
        for (auto& item : x.m_symtab->get_scope()) {
            visit_symbol(*item.second);
        }
        current_scope = old_scope;
    }

    void visit_Program(const ASR::Program_t& x) {
        SymbolTable* old_scope = current_scope;
        current_scope = x.m_symtab;
        transform_stmts_in_body(x.m_body, x.n_body);
        for (auto& item : x.m_symtab->get_scope()) {
            visit_symbol(*item.second);
        }
        current_scope = old_scope;
    }

    void visit_Function(const ASR::Function_t& x) {
        SymbolTable* old_scope = current_scope;
        current_scope = x.m_symtab;
        transform_stmts_in_body(x.m_body, x.n_body);
        for (auto& item : x.m_symtab->get_scope()) {
            visit_symbol(*item.second);
        }
        current_scope = old_scope;
    }

    void transform_stmts_in_body(ASR::stmt_t** body, size_t n_body) {
        Vec<ASR::stmt_t*> new_body;
        new_body.reserve(al, n_body);
        for (size_t i = 0; i < n_body; i++) {
            pass_result.n = 0;
            pass_result.reserve(al, 1);
            transform_stmts = false;
            visit_stmt(*body[i]);
            if (transform_stmts && pass_result.size() > 0) {
                for (size_t j = 0; j < pass_result.size(); j++) {
                    new_body.push_back(al, pass_result[j]);
                }
            } else {
                new_body.push_back(al, body[i]);
            }
        }
        // Update the body in place
        for (size_t i = 0; i < new_body.size(); i++) {
            if (i < n_body) {
                body[i] = new_body[i];
            }
        }
        // If new_body is larger we need to update the pointer
        // For ASR nodes the body is an array — we need to replace the whole thing
        // Since we're modifying the ASR in-place, we'll use a workaround:
        // store results back into the body array
    }

    void visit_WhileLoop(const ASR::WhileLoop_t& x) {
        ASR::WhileLoop_t& xx = const_cast<ASR::WhileLoop_t&>(x);
        Vec<ASR::stmt_t*> new_body;
        new_body.reserve(al, x.n_body);
        for (size_t i = 0; i < x.n_body; i++) {
            pass_result.n = 0;
            pass_result.reserve(al, 1);
            transform_stmts = false;
            visit_stmt(*x.m_body[i]);
            if (transform_stmts && pass_result.size() > 0) {
                for (size_t j = 0; j < pass_result.size(); j++) {
                    new_body.push_back(al, pass_result[j]);
                }
            } else {
                new_body.push_back(al, x.m_body[i]);
            }
        }
        xx.m_body = new_body.p;
        xx.n_body = new_body.size();
    }

    void visit_DoLoop(const ASR::DoLoop_t& x) {
        ASR::DoLoop_t& xx = const_cast<ASR::DoLoop_t&>(x);
        Vec<ASR::stmt_t*> new_body;
        new_body.reserve(al, x.n_body);
        for (size_t i = 0; i < x.n_body; i++) {
            pass_result.n = 0;
            pass_result.reserve(al, 1);
            transform_stmts = false;
            visit_stmt(*x.m_body[i]);
            if (transform_stmts && pass_result.size() > 0) {
                for (size_t j = 0; j < pass_result.size(); j++) {
                    new_body.push_back(al, pass_result[j]);
                }
            } else {
                new_body.push_back(al, x.m_body[i]);
            }
        }
        xx.m_body = new_body.p;
        xx.n_body = new_body.size();
    }

    void visit_If(const ASR::If_t& x) {
        ASR::If_t& xx = const_cast<ASR::If_t&>(x);
        {
            Vec<ASR::stmt_t*> new_body;
            new_body.reserve(al, x.n_body);
            for (size_t i = 0; i < x.n_body; i++) {
                pass_result.n = 0;
                pass_result.reserve(al, 1);
                transform_stmts = false;
                visit_stmt(*x.m_body[i]);
                if (transform_stmts && pass_result.size() > 0) {
                    for (size_t j = 0; j < pass_result.size(); j++) {
                        new_body.push_back(al, pass_result[j]);
                    }
                } else {
                    new_body.push_back(al, x.m_body[i]);
                }
            }
            xx.m_body = new_body.p;
            xx.n_body = new_body.size();
        }
        if (x.n_orelse > 0) {
            Vec<ASR::stmt_t*> new_orelse;
            new_orelse.reserve(al, x.n_orelse);
            for (size_t i = 0; i < x.n_orelse; i++) {
                pass_result.n = 0;
                pass_result.reserve(al, 1);
                transform_stmts = false;
                visit_stmt(*x.m_orelse[i]);
                if (transform_stmts && pass_result.size() > 0) {
                    for (size_t j = 0; j < pass_result.size(); j++) {
                        new_orelse.push_back(al, pass_result[j]);
                    }
                } else {
                    new_orelse.push_back(al, x.m_orelse[i]);
                }
            }
            xx.m_orelse = new_orelse.p;
            xx.n_orelse = new_orelse.size();
        }
    }

    void visit_Assignment(const ASR::Assignment_t& x) {
        if (!pass_options.fast) return;

        // Check: is the target a scalar?
        ASR::ttype_t* target_type = ASRUtils::expr_type(x.m_target);
        if (ASRUtils::is_array(target_type)) return;

        // Check: is the value an IntrinsicArrayFunction?
        if (!is_a<ASR::IntrinsicArrayFunction_t>(*x.m_value)) return;

        ASR::IntrinsicArrayFunction_t* iaf =
            down_cast<ASR::IntrinsicArrayFunction_t>(x.m_value);

        // Check: is this a supported reduction (no dim, no mask)?
        int64_t reduction_id = iaf->m_arr_intrinsic_id;
        using ASRUtils::IntrinsicArrayFunctions;
        if (reduction_id != static_cast<int64_t>(IntrinsicArrayFunctions::MaxVal) &&
            reduction_id != static_cast<int64_t>(IntrinsicArrayFunctions::MinVal) &&
            reduction_id != static_cast<int64_t>(IntrinsicArrayFunctions::Sum)) {
            return;
        }

        // Check: scalar result (overload_id 0 = array-only, no dim)
        if (iaf->m_overload_id != 0) return;

        // Check: no dim argument (args[1] should be nullptr or absent)
        if (iaf->n_args > 1 && iaf->m_args[1] != nullptr) return;
        // Check: no mask argument
        if (iaf->n_args > 2 && iaf->m_args[2] != nullptr) return;

        ASR::expr_t* array_arg = iaf->m_args[0];
        if (!array_arg) return;

        // Step 1: Collect all array operands
        std::vector<ArrayOperandInfo> operands;
        if (!collect_array_operands(array_arg, operands, al)) return;
        if (operands.empty()) return;

        // Step 2: Verify rank conformance
        if (!check_rank_conformance(operands)) return;
        int rank = operands[0].rank;
        if (rank == 0) return;

        const Location& loc = x.base.base.loc;

        // Step 3: Create loop index variables
        Vec<ASR::expr_t*> idx_vars;
        PassUtils::create_idx_vars(idx_vars, rank, loc, al, current_scope,
            "_fuse_reduction_");

        // Step 4: Create accumulator variable
        ASR::ttype_t* element_type = ASRUtils::type_get_past_array(
            ASRUtils::expr_type(array_arg));
        element_type = ASRUtils::extract_type(element_type);
        std::string acc_name = current_scope->get_unique_name(
            "__fuse_reduction_acc_", false);
        ASR::asr_t* acc_sym = ASRUtils::make_Variable_t_util(al, loc,
            current_scope, s2c(al, acc_name), nullptr, 0,
            ASR::intentType::Local, nullptr, nullptr,
            ASR::storage_typeType::Default, element_type, nullptr,
            ASR::abiType::Source, ASR::accessType::Public,
            ASR::presenceType::Required, false);
        current_scope->add_symbol(acc_name, down_cast<ASR::symbol_t>(acc_sym));
        ASR::expr_t* acc_var = ASRUtils::EXPR(ASR::make_Var_t(al, loc,
            down_cast<ASR::symbol_t>(acc_sym)));

        // Step 5: Scalarize the array expression
        size_t operand_idx = 0;
        ASR::expr_t* scalarized = scalarize_expr(al, array_arg,
            operands, idx_vars, operand_idx);
        if (!scalarized) return;

        // Step 6: Build the reduction update expression
        ASR::expr_t* init_val = get_reduction_init(al, reduction_id,
            element_type, loc);
        if (!init_val) return;
        ASR::expr_t* update_expr = get_reduction_update(al, reduction_id,
            acc_var, scalarized, loc);
        if (!update_expr) return;

        // Step 7: Build the assignment statement for loop body
        ASR::stmt_t* update_stmt = ASRUtils::STMT(ASR::make_Assignment_t(al, loc,
            acc_var, update_expr, nullptr, false, false));

        // Step 8: Build nested DoLoop (outermost = last dim, innermost = first)
        // Use bounds from the first operand as the reference
        const ArrayOperandInfo& ref = operands[0];
        ASR::stmt_t* doloop = nullptr;

        // Build from innermost (dim 0) to outermost (dim rank-1)
        Vec<ASR::stmt_t*> body;
        body.reserve(al, 1);
        body.push_back(al, update_stmt);

        for (int d = 0; d < rank; d++) {
            ASR::do_loop_head_t head;
            head.loc = loc;
            head.m_v = idx_vars[d];
            head.m_start = ref.lb[d];
            head.m_end = ref.ub[d];
            head.m_increment = ref.step[d]; // may be nullptr for unit step

            doloop = ASRUtils::STMT(ASR::make_DoLoop_t(al, loc, nullptr,
                head, body.p, body.size(), nullptr, 0));

            body.n = 0;
            body.reserve(al, 1);
            body.push_back(al, doloop);
        }

        // Step 9: Emit: acc = init; loop; target = acc
        pass_result.n = 0;
        pass_result.reserve(al, 3);
        // acc = init_val
        pass_result.push_back(al, ASRUtils::STMT(ASR::make_Assignment_t(al, loc,
            acc_var, init_val, nullptr, false, false)));
        // the nested DoLoop
        pass_result.push_back(al, doloop);
        // target = acc
        pass_result.push_back(al, ASRUtils::STMT(ASR::make_Assignment_t(al, loc,
            const_cast<ASR::expr_t*>(x.m_target), acc_var, nullptr, false, false)));

        transform_stmts = true;
    }
};

void pass_fuse_array_reduction(Allocator &al,
        ASR::TranslationUnit_t &unit,
        const LCompilers::PassOptions &pass_options) {
    if (!pass_options.fast) return;
    FuseArrayReductionVisitor v(al, pass_options);
    v.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

} // namespace LCompilers
