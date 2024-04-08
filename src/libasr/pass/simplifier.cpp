#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/simplifier.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/intrinsic_array_function_registry.h>

#include <vector>
#include <utility>
#include <set>

namespace LCompilers {

/*
This pass collector that the BinOp only Var nodes and nothing else.
*/
class ArrayVarCollector: public ASR::BaseWalkVisitor<ArrayVarCollector> {
    private:

    Allocator& al;
    Vec<ASR::expr_t*>& vars;

    public:

    ArrayVarCollector(Allocator& al_, Vec<ASR::expr_t*>& vars_): al(al_), vars(vars_) {}

    void visit_Var(const ASR::Var_t& x) {
        if( ASRUtils::is_array(ASRUtils::symbol_type(x.m_v)) ) {
            vars.push_back(al, const_cast<ASR::expr_t*>(&(x.base)));
        }
    }

};

ASR::expr_t* create_temporary_variable_for_array(Allocator& al,
    ASR::expr_t* value, SymbolTable* scope, std::string name_hint) {
    ASR::ttype_t* value_type = ASRUtils::expr_type(value);
    LCOMPILERS_ASSERT(ASRUtils::is_array(value_type));

    /* Figure out the type of the temporary array variable */
    ASR::dimension_t* value_m_dims = nullptr;
    size_t value_n_dims = ASRUtils::extract_dimensions_from_ttype(value_type, value_m_dims);
    bool is_fixed_sized_array = ASRUtils::is_fixed_size_array(value_type);
    bool is_size_only_dependent_on_arguments = ASRUtils::is_dimension_dependent_only_on_arguments(
        value_m_dims, value_n_dims);
    bool is_allocatable = ASRUtils::is_allocatable(value_type);
    ASR::ttype_t* var_type = nullptr;
    if( is_fixed_sized_array || is_size_only_dependent_on_arguments || is_allocatable ) {
        var_type = value_type;
    } else {
        Vec<ASR::dimension_t> empty_dims; empty_dims.reserve(al, value_n_dims);
        for( size_t i = 0; i < value_n_dims; i++ ) {
            ASR::dimension_t empty_dim;
            Location loc; loc.first = 1, loc.last = 1;
            empty_dim.loc = loc;
            empty_dim.m_length = nullptr;
            empty_dim.m_start = nullptr;
            empty_dims.push_back(al, empty_dim);
        }
        var_type = ASRUtils::make_Array_t_util(al, value_type->base.loc,
            ASRUtils::extract_type(value_type), empty_dims.p, empty_dims.size());
        var_type = ASRUtils::TYPE(ASR::make_Allocatable_t(al, var_type->base.loc, var_type));
    }

    std::string var_name = scope->get_unique_name("__libasr_created_" + name_hint);
    ASR::symbol_t* temporary_variable = ASR::down_cast<ASR::symbol_t>(ASR::make_Variable_t(
        al, value->base.loc, scope, s2c(al, var_name), nullptr, 0, ASR::intentType::Local,
        nullptr, nullptr, ASR::storage_typeType::Default, var_type, nullptr, ASR::abiType::Source,
        ASR::accessType::Public, ASR::presenceType::Required, false));
    scope->add_symbol(var_name, temporary_variable);

    return ASRUtils::EXPR(ASR::make_Var_t(al, temporary_variable->base.loc, temporary_variable));
}

bool set_allocation_size(Allocator& al, ASR::expr_t* value, Vec<ASR::dimension_t>& allocate_dims) {
    LCOMPILERS_ASSERT(ASRUtils::is_array(ASRUtils::expr_type(value)));
    switch( value->type ) {
        case ASR::exprType::FunctionCall: {
            ASR::FunctionCall_t* function_call = ASR::down_cast<ASR::FunctionCall_t>(value);
            ASR::ttype_t* type = function_call->m_type;
            if( ASRUtils::is_allocatable(type) ) {
                return false;
            }
            ASRUtils::ExprStmtDuplicator duplicator(al);
            ASR::dimension_t* dims = nullptr;
            size_t n_dims = ASRUtils::extract_dimensions_from_ttype(type, dims);
            allocate_dims.reserve(al, n_dims);
            for( size_t i = 0; i < n_dims; i++ ) {
                ASR::dimension_t dim = dims[i];
                ASR::dimension_t dim_copy;
                dim_copy.loc = dim.loc;
                dim_copy.m_start = !dim.m_start ? nullptr : duplicator.duplicate_expr(dim.m_start);
                dim_copy.m_length = !dim.m_length ? nullptr : duplicator.duplicate_expr(dim.m_length);
                allocate_dims.push_back(al, dim_copy);
            }
            break ;
        }
        case ASR::exprType::IntegerBinOp:
        case ASR::exprType::RealBinOp:
        case ASR::exprType::ComplexBinOp:
        case ASR::exprType::LogicalBinOp:
        case ASR::exprType::UnsignedIntegerBinOp:
        case ASR::exprType::IntegerCompare:
        case ASR::exprType::RealCompare:
        case ASR::exprType::ComplexCompare:
        case ASR::exprType::LogicalCompare:
        case ASR::exprType::UnsignedIntegerCompare:
        case ASR::exprType::IntegerUnaryMinus:
        case ASR::exprType::RealUnaryMinus:
        case ASR::exprType::ComplexUnaryMinus: {
            /*
                Collect all the variables from these expressions,
                then take the size of one of the arrays having
                maximum dimensions for now. For now LFortran will
                assume that broadcasting is doable for arrays with lesser
                dimensions and the array having maximum dimensions
                has compatible size of each dimension with other arrays.
            */

            Vec<ASR::expr_t*> array_vars; array_vars.reserve(al, 1);
            ArrayVarCollector array_var_collector(al, array_vars);
            array_var_collector.visit_expr(*value);
            Vec<ASR::expr_t*> arrays_with_maximum_rank;
            arrays_with_maximum_rank.reserve(al, 1);
            size_t max_rank = 0;
            for( size_t i = 0; i < array_vars.size(); i++ ) {
                size_t rank = ASRUtils::extract_n_dims_from_ttype(
                    ASRUtils::expr_type(array_vars[i]));
                if( rank > max_rank ) {
                    max_rank = rank;
                }
            }
            LCOMPILERS_ASSERT(max_rank > 0);
            for( size_t i = 0; i < array_vars.size(); i++ ) {
                if( (size_t) ASRUtils::extract_n_dims_from_ttype(
                        ASRUtils::expr_type(array_vars[i])) == max_rank ) {
                    arrays_with_maximum_rank.push_back(al, array_vars[i]);
                }
            }

            LCOMPILERS_ASSERT(arrays_with_maximum_rank.size() > 0);
            ASR::expr_t* selected_array = arrays_with_maximum_rank[0];
            allocate_dims.reserve(al, max_rank);
            for( size_t i = 0; i < max_rank; i++ ) {
                ASR::dimension_t allocate_dim;
                Location loc; loc.first = 1, loc.last = 1;
                allocate_dim.loc = loc;
                // Assume 1 for Fortran.
                allocate_dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                    al, loc, 1, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4))));
                ASR::expr_t* dim = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                    al, loc, i + 1, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4))));
                allocate_dim.m_length = ASRUtils::EXPR(ASR::make_ArraySize_t(
                    al, loc, selected_array, dim, ASRUtils::TYPE(
                        ASR::make_Integer_t(al, loc, 4)), nullptr));
                allocate_dims.push_back(al, allocate_dim);
            }
            break;
        }
        default: {
            LCOMPILERS_ASSERT_MSG(false, "ASR::exprType::" + std::to_string(value->type)
                + " not handled yet in set_allocation_size");
        }
    }
    return true;
}

void insert_allocate_stmt(Allocator& al, ASR::expr_t* temporary_var,
    ASR::expr_t* value, Vec<ASR::stmt_t*>* current_body) {
    if( !ASRUtils::is_allocatable(temporary_var) ) {
        return ;
    }
    Vec<ASR::dimension_t> allocate_dims;
    if( !set_allocation_size(al, value, allocate_dims) ) {
        return ;
    }
    Vec<ASR::alloc_arg_t> alloc_args; alloc_args.reserve(al, 1);
    ASR::alloc_arg_t alloc_arg;
    alloc_arg.loc = value->base.loc;
    alloc_arg.m_a = temporary_var;
    alloc_arg.m_dims = allocate_dims.p;
    alloc_arg.n_dims = allocate_dims.size();
    alloc_arg.m_len_expr = nullptr;
    alloc_arg.m_type = nullptr;
    alloc_args.push_back(al, alloc_arg);

    Vec<ASR::expr_t*> dealloc_args; dealloc_args.reserve(al, 1);
    dealloc_args.push_back(al, temporary_var);
    current_body->push_back(al, ASRUtils::STMT(ASR::make_ExplicitDeallocate_t(al,
        temporary_var->base.loc, dealloc_args.p, dealloc_args.size())));
    current_body->push_back(al, ASRUtils::STMT(ASR::make_Allocate_t(al,
        temporary_var->base.loc, alloc_args.p, alloc_args.size(),
        nullptr, nullptr, nullptr)));
}

#define transform_stmts_impl Vec<ASR::stmt_t*>* current_body_copy = current_body; \
    Vec<ASR::stmt_t*> current_body_vec; current_body_vec.reserve(al, 1); \
    current_body_vec.reserve(al, n_body); \
    current_body = &current_body_vec; \
    for (size_t i = 0; i < n_body; i++) { \
        visit_stmt(*m_body[i]); \
        current_body->push_back(al, m_body[i]); \
    } \
    m_body = current_body_vec.p; n_body = current_body_vec.size(); \
    current_body = current_body_copy;

ASR::expr_t* create_and_allocate_temporary_variable_for_array(
    ASR::expr_t* array_expr, const std::string& name_hint, Allocator& al,
    Vec<ASR::stmt_t*>*& current_body, SymbolTable* current_scope,
    std::set<ASR::expr_t*>& exprs_with_target) {
    const Location& loc = array_expr->base.loc;
    ASR::expr_t* array_var_temporary = create_temporary_variable_for_array(
        al, array_expr, current_scope, name_hint);
    insert_allocate_stmt(al, array_var_temporary, array_expr, current_body);
    array_expr = ASRUtils::get_past_array_physical_cast(array_expr);
    exprs_with_target.insert(array_expr);
    current_body->push_back(al, ASRUtils::STMT(ASR::make_Assignment_t(
        al, loc, array_var_temporary, array_expr, nullptr)));
    return array_var_temporary;
}

class ArgSimplifier: public ASR::CallReplacerOnExpressionsVisitor<ArgSimplifier>
{

    private:

    Allocator& al;
    Vec<ASR::stmt_t*>* current_body;
    std::set<ASR::expr_t*>& exprs_with_target;

    public:

    ArgSimplifier(Allocator& al_, std::set<ASR::expr_t*>& exprs_with_target_) :
        al(al_), current_body(nullptr), exprs_with_target(exprs_with_target_) {}

    void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
        transform_stmts_impl
    }

    #define BEGIN_VAR_CHECK(expr) if( !ASR::is_a<ASR::Var_t>(*expr) ) {
    #define END_VAR_CHECK }

    template <typename T>
    void visit_IO(const T& x, const std::string& name_hint) {
        Vec<ASR::expr_t*> x_m_values; x_m_values.reserve(al, x.n_values);
        /* For frontends like LC, we will need to traverse the print statement arguments
           in reverse order. */
        for( size_t i = 0; i < x.n_values; i++ ) {
            if( ASRUtils::is_array(ASRUtils::expr_type(x.m_values[i])) &&
                !ASR::is_a<ASR::Var_t>(*x.m_values[i]) ) {
                visit_expr(*x.m_values[i]);
                ASR::expr_t* array_var_temporary = create_and_allocate_temporary_variable_for_array(
                    x.m_values[i], name_hint, al, current_body, current_scope, exprs_with_target);
                x_m_values.push_back(al, array_var_temporary);
            } else {
                x_m_values.push_back(al, x.m_values[i]);
            }
        }

        T& xx = const_cast<T&>(x);
        xx.m_values = x_m_values.p;
        xx.n_values = x_m_values.size();
    }

    void visit_Print(const ASR::Print_t& x) {
        visit_IO(x, "print");
    }

    void visit_FileWrite(const ASR::FileWrite_t& x) {
        visit_IO(x, "file_write");
    }

    void traverse_args(Vec<ASR::expr_t*>& x_m_args_vec, ASR::expr_t** x_m_args,
        size_t x_n_args, const std::string& name_hint) {
        /* For other frontends, we might need to traverse the arguments
           in reverse order. */
        for( size_t i = 0; i < x_n_args; i++ ) {
            if( ASRUtils::is_array(ASRUtils::expr_type(x_m_args[i])) &&
                !ASR::is_a<ASR::Var_t>(*x_m_args[i]) ) {
                visit_expr(*x_m_args[i]);
                ASR::expr_t* array_var_temporary = create_and_allocate_temporary_variable_for_array(
                    x_m_args[i], name_hint, al, current_body, current_scope, exprs_with_target);
                if( ASR::is_a<ASR::ArrayPhysicalCast_t>(*x_m_args[i]) ) {
                    ASR::ArrayPhysicalCast_t* x_m_args_i = ASR::down_cast<ASR::ArrayPhysicalCast_t>(x_m_args[i]);
                    array_var_temporary = ASRUtils::EXPR(ASRUtils::make_ArrayPhysicalCast_t_util(
                        al, array_var_temporary->base.loc, array_var_temporary,
                        ASRUtils::extract_physical_type(ASRUtils::expr_type(array_var_temporary)),
                        x_m_args_i->m_new, x_m_args_i->m_type, nullptr));
                }
                x_m_args_vec.push_back(al, array_var_temporary);
            } else {
                x_m_args_vec.push_back(al, x_m_args[i]);
            }
        }
    }

    template <typename T>
    void visit_TypeConstructor(const T& x, const std::string& name_hint) {
        Vec<ASR::expr_t*> x_m_args; x_m_args.reserve(al, x.n_args);
        traverse_args(x_m_args, x.m_args, x.n_args, name_hint);
        T& xx = const_cast<T&>(x);
        xx.m_args = x_m_args.p;
        xx.n_args = x_m_args.size();
    }

    void visit_EnumTypeConstructor(const ASR::EnumTypeConstructor_t& x) {
        visit_TypeConstructor(x, std::string("_enum_type_constructor_") +
            ASRUtils::symbol_name(x.m_dt_sym));
    }

    void visit_UnionTypeConstructor(const ASR::UnionTypeConstructor_t& x) {
        visit_TypeConstructor(x, std::string("_union_type_constructor_") +
            ASRUtils::symbol_name(x.m_dt_sym));
    }

    void visit_ArrayConstructor(const ASR::ArrayConstructor_t& x) {
        Vec<ASR::expr_t*> x_m_args; x_m_args.reserve(al, x.n_args);
        traverse_args(x_m_args, x.m_args, x.n_args, std::string("_array_constructor_"));
    }

    template <typename T>
    void visit_IntrinsicCall(const T& x, const std::string& name_hint) {
        Vec<ASR::expr_t*> x_m_args; x_m_args.reserve(al, x.n_args);
        traverse_args(x_m_args, x.m_args, x.n_args, name_hint);
        T& xx = const_cast<T&>(x);
        xx.m_args = x_m_args.p;
        xx.n_args = x_m_args.size();
    }

    void visit_IntrinsicImpureSubroutine(const ASR::IntrinsicImpureSubroutine_t& x) {
        visit_IntrinsicCall(x, "_intrinsic_impure_subroutine_" +
            ASRUtils::get_impure_intrinsic_name(x.m_intrinsic_id));
    }

    void visit_IntrinsicElementalFunction(const ASR::IntrinsicElementalFunction_t& x) {
        visit_IntrinsicCall(x, "_intrinsic_elemental_function_" +
            ASRUtils::get_intrinsic_name(x.m_intrinsic_id));
    }

    void visit_IntrinsicArrayFunction(const ASR::IntrinsicArrayFunction_t& x) {
        visit_IntrinsicCall(x, "_intrinsic_array_function_" +
            ASRUtils::get_array_intrinsic_name(x.m_arr_intrinsic_id));
    }

    void visit_IntrinsicImpureFunction(const ASR::IntrinsicImpureFunction_t& x) {
        visit_IntrinsicCall(x, "_intrinsic_array_function_" +
            ASRUtils::get_impure_intrinsic_name(x.m_impure_intrinsic_id));
    }

    void traverse_call_args(Vec<ASR::call_arg_t>& x_m_args_vec, ASR::call_arg_t* x_m_args,
        size_t x_n_args, const std::string& name_hint) {
        /* For other frontends, we might need to traverse the arguments
           in reverse order. */
        for( size_t i = 0; i < x_n_args; i++ ) {
            if( x_m_args[i].m_value &&
                ASRUtils::is_array(ASRUtils::expr_type(x_m_args[i].m_value)) &&
                !ASR::is_a<ASR::Var_t>(*x_m_args[i].m_value) ) {
                visit_call_arg(x_m_args[i]);
                ASR::expr_t* array_var_temporary = create_and_allocate_temporary_variable_for_array(
                    x_m_args[i].m_value, name_hint, al, current_body, current_scope, exprs_with_target);
                if( ASR::is_a<ASR::ArrayPhysicalCast_t>(*x_m_args[i].m_value) ) {
                    ASR::ArrayPhysicalCast_t* x_m_args_i = ASR::down_cast<ASR::ArrayPhysicalCast_t>(x_m_args[i].m_value);
                    array_var_temporary = ASRUtils::EXPR(ASRUtils::make_ArrayPhysicalCast_t_util(
                        al, array_var_temporary->base.loc, array_var_temporary,
                        ASRUtils::extract_physical_type(ASRUtils::expr_type(array_var_temporary)),
                        x_m_args_i->m_new, x_m_args_i->m_type, nullptr));
                }
                ASR::call_arg_t call_arg;
                call_arg.loc = array_var_temporary->base.loc;
                call_arg.m_value = array_var_temporary;
                x_m_args_vec.push_back(al, call_arg);
            } else {
                x_m_args_vec.push_back(al, x_m_args[i]);
            }
        }
    }

    template <typename T>
    void visit_Call(const T& x, const std::string& name_hint) {
        LCOMPILERS_ASSERT(!x.m_dt || !ASRUtils::is_array(ASRUtils::expr_type(x.m_dt)));
        Vec<ASR::call_arg_t> x_m_args; x_m_args.reserve(al, x.n_args);
        traverse_call_args(x_m_args, x.m_args, x.n_args,
            name_hint + ASRUtils::symbol_name(x.m_name));
        T& xx = const_cast<T&>(x);
        xx.m_args = x_m_args.p;
        xx.n_args = x_m_args.size();
    }

    void visit_StructTypeConstructor(const ASR::StructTypeConstructor_t& x) {
        Vec<ASR::call_arg_t> x_m_args; x_m_args.reserve(al, x.n_args);
        traverse_call_args(x_m_args, x.m_args, x.n_args,
            std::string("_struct_type_constructor_") + ASRUtils::symbol_name(x.m_dt_sym));
        ASR::StructTypeConstructor_t& xx = const_cast<ASR::StructTypeConstructor_t&>(x);
        xx.m_args = x_m_args.p;
        xx.n_args = x_m_args.size();
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t& x) {
        visit_Call(x, "_subroutine_call_");
    }

    void visit_FunctionCall(const ASR::FunctionCall_t& x) {
        visit_Call(x, "_function_call_");
    }

    #define replace_expr_with_temporary_variable(member, name_hint) BEGIN_VAR_CHECK(x.m_##member) \
        visit_expr(*x.m_##member); \
        xx.m_##member = create_and_allocate_temporary_variable_for_array( \
            x.m_##member, name_hint, al, current_body, current_scope, exprs_with_target); \
        END_VAR_CHECK

    void visit_ComplexConstructor(const ASR::ComplexConstructor_t& x) {
        ASR::ComplexConstructor_t& xx = const_cast<ASR::ComplexConstructor_t&>(x);

        replace_expr_with_temporary_variable(re, "_complex_constructor_re")

        replace_expr_with_temporary_variable(im, "_complex_constructor_im")
    }

    void visit_ArrayTranspose(const ASR::ArrayTranspose_t& x) {
        ASR::ArrayTranspose_t& xx = const_cast<ASR::ArrayTranspose_t&>(x);

        replace_expr_with_temporary_variable(matrix, "_array_transpose_matrix_")
    }

    void visit_ArrayPack(const ASR::ArrayPack_t& x) {
        ASR::ArrayPack_t& xx = const_cast<ASR::ArrayPack_t&>(x);

        replace_expr_with_temporary_variable(array, "_array_pack_array_")

        replace_expr_with_temporary_variable(mask, "_array_pack_mask_")

        if( x.m_vector ) {
            replace_expr_with_temporary_variable(vector, "_array_pack_vector_")
        }
    }

    void visit_ArrayAll(const ASR::ArrayAll_t& x) {
        ASR::ArrayAll_t& xx = const_cast<ASR::ArrayAll_t&>(x);

        replace_expr_with_temporary_variable(mask, "_array_all_mask_")
    }

    void visit_Cast(const ASR::Cast_t& x) {
        ASR::Cast_t& xx = const_cast<ASR::Cast_t&>(x);

        replace_expr_with_temporary_variable(arg, "_cast_")
    }

    void visit_ComplexRe(const ASR::ComplexRe_t& x) {
        ASR::ComplexRe_t& xx = const_cast<ASR::ComplexRe_t&>(x);

        replace_expr_with_temporary_variable(arg, "_complex_re_")
    }

    void visit_ComplexIm(const ASR::ComplexIm_t& x) {
        ASR::ComplexIm_t& xx = const_cast<ASR::ComplexIm_t&>(x);

        replace_expr_with_temporary_variable(arg, "_complex_im_")
    }

    void visit_RealSqrt(const ASR::RealSqrt_t& x) {
        ASR::RealSqrt_t& xx = const_cast<ASR::RealSqrt_t&>(x);

        replace_expr_with_temporary_variable(arg, "_real_sqrt_")
    }
};

class ReplaceExprWithTemporary: public ASR::BaseExprReplacer<ReplaceExprWithTemporary> {

    private:

    Allocator& al;
    std::set<ASR::expr_t*>& exprs_with_target;

    public:

    Vec<ASR::stmt_t*>* current_body;
    SymbolTable* current_scope;

    ReplaceExprWithTemporary(Allocator& al_, std::set<ASR::expr_t*>& exprs_with_target_) :
        al(al_), exprs_with_target(exprs_with_target_), current_scope(nullptr) {}

    #define is_current_expr_linked_to_target_then_return if( exprs_with_target.find(*current_expr) != exprs_with_target.end() ) { \
            return ; \
        }

    #define replace_current_expr(name_hint) is_current_expr_linked_to_target_then_return \
        if( ASRUtils::is_array(x->m_type) ) { \
            *current_expr = create_and_allocate_temporary_variable_for_array( \
                *current_expr, name_hint, al, current_body, \
                current_scope, exprs_with_target); \
        }

    void replace_ComplexConstructor(ASR::ComplexConstructor_t* x) {
        replace_current_expr("_complex_constructor_")
    }

    void replace_FunctionCall(ASR::FunctionCall_t* x) {
        if( PassUtils::is_elemental(x->m_name) ) {
            return ;
        }

        replace_current_expr(std::string("_function_call_") +
            ASRUtils::symbol_name(x->m_name))
    }

    void replace_IntrinsicArrayFunction(ASR::IntrinsicArrayFunction_t* x) {
        replace_current_expr(std::string("_intrinsic_array_function_") +
            ASRUtils::get_array_intrinsic_name(x->m_arr_intrinsic_id))
    }

    void replace_IntrinsicImpureFunction(ASR::IntrinsicImpureFunction_t* x) {
        replace_current_expr(std::string("_intrinsic_impure_function_") +
            ASRUtils::get_impure_intrinsic_name(x->m_impure_intrinsic_id))
    }

    void replace_StructTypeConstructor(ASR::StructTypeConstructor_t* x) {
        replace_current_expr("_struct_type_constructor_")
    }

    void replace_EnumTypeConstructor(ASR::EnumTypeConstructor_t* x) {
        replace_current_expr("_enum_type_constructor_")
    }

    void replace_UnionTypeConstructor(ASR::UnionTypeConstructor_t* x) {
        replace_current_expr("_union_type_constructor_")
    }

    void replace_ImpliedDoLoop(ASR::ImpliedDoLoop_t* x) {
        replace_current_expr("_implied_do_loop_")
    }

    void replace_ListConstant(ASR::ListConstant_t* x) {
        replace_current_expr("_list_constant_")
    }

    void replace_SetConstant(ASR::SetConstant_t* x) {
        replace_current_expr("_set_constant_")
    }

    void replace_TupleConstant(ASR::TupleConstant_t* x) {
        replace_current_expr("_tuple_constant_")
    }

    void replace_StringSection(ASR::StringSection_t* x) {
        replace_current_expr("_string_section_")
    }

    void replace_DictConstant(ASR::DictConstant_t* x) {
        replace_current_expr("_dict_constant_")
    }

};

class ReplaceExprWithTemporaryVisitor:
    public ASR::CallReplacerOnExpressionsVisitor<ReplaceExprWithTemporaryVisitor> {

    private:

    Allocator& al;
    std::set<ASR::expr_t*>& exprs_with_target;
    Vec<ASR::stmt_t*>* current_body;
    ReplaceExprWithTemporary replacer;

    public:

    ReplaceExprWithTemporaryVisitor(Allocator& al_, std::set<ASR::expr_t*>& exprs_with_target_):
        al(al_), exprs_with_target(exprs_with_target_), replacer(al, exprs_with_target) {}

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.current_body = current_body;
        replacer.current_scope = current_scope;
        replacer.replace_expr(*current_expr);
    }

    void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
        transform_stmts_impl
    }

};

void pass_simplifier(Allocator &al, ASR::TranslationUnit_t &unit,
                     const PassOptions &/*pass_options*/) {
    std::set<ASR::expr_t*> exprs_with_target;
    ArgSimplifier v(al, exprs_with_target);
    v.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}


} // namespace LCompilers
