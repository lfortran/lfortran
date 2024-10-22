#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/simplifier.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/intrinsic_subroutine_registry.h>
#include <libasr/pass/intrinsic_array_function_registry.h>
#include <libasr/pickle.h>

#include <vector>
#include <utility>
#include <set>

namespace LCompilers {

enum targetType {
    GeneratedTarget,
    OriginalTarget
};

typedef std::map<ASR::expr_t*, std::pair<ASR::expr_t*, targetType>> ExprsWithTargetType;

const std::vector<ASR::exprType>& exprs_with_no_type = {
};

static inline ASR::asr_t* make_Assignment_t_util(
    Allocator &al, const Location &a_loc, ASR::expr_t* a_target,
    ASR::expr_t* a_value, ASR::stmt_t* a_overloaded,
    ExprsWithTargetType& exprs_with_target) {
    ASRUtils::ExprStmtDuplicator expr_duplicator(al);
    a_target = expr_duplicator.duplicate_expr(a_target);
    a_value = expr_duplicator.duplicate_expr(a_value);

    exprs_with_target[a_value] = std::make_pair(a_target, targetType::GeneratedTarget);
    return ASR::make_Assignment_t(al, a_loc, a_target, a_value, a_overloaded);
}


ASR::expr_t* get_ImpliedDoLoop_size(Allocator& al, ASR::ImpliedDoLoop_t* implied_doloop) {
    const Location& loc = implied_doloop->base.base.loc;
    ASRUtils::ASRBuilder builder(al, loc);
    ASR::expr_t* start = implied_doloop->m_start;
    ASR::expr_t* end = implied_doloop->m_end;
    ASR::expr_t* d = implied_doloop->m_increment;
    ASR::expr_t* implied_doloop_size = nullptr;
    int kind = ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(end));
    start = builder.i2i_t(start, ASRUtils::expr_type(end));
    if( d == nullptr ) {
        implied_doloop_size = builder.Add(
            builder.Sub(end, start),
            make_ConstantWithKind(make_IntegerConstant_t, make_Integer_t, 1, kind, loc));
    } else {
        implied_doloop_size = builder.Add(builder.Div(
            builder.Sub(end, start), d),
            make_ConstantWithKind(make_IntegerConstant_t, make_Integer_t, 1, kind, loc));
    }
    int const_elements = 0;
    ASR::expr_t* implied_doloop_size_ = nullptr;
    for( size_t i = 0; i < implied_doloop->n_values; i++ ) {
        if( ASR::is_a<ASR::ImpliedDoLoop_t>(*implied_doloop->m_values[i]) ) {
            if( implied_doloop_size_ == nullptr ) {
                implied_doloop_size_ = get_ImpliedDoLoop_size(al,
                    ASR::down_cast<ASR::ImpliedDoLoop_t>(implied_doloop->m_values[i]));
            } else {
                implied_doloop_size_ = builder.Add(get_ImpliedDoLoop_size(al,
                    ASR::down_cast<ASR::ImpliedDoLoop_t>(implied_doloop->m_values[i])),
                    implied_doloop_size_);
            }
        } else {
            const_elements += 1;
        }
    }
    if( const_elements > 1 ) {
        if( implied_doloop_size_ == nullptr ) {
            implied_doloop_size_ = make_ConstantWithKind(make_IntegerConstant_t,
                make_Integer_t, const_elements, kind, loc);
        } else {
            implied_doloop_size_ = builder.Add(
                make_ConstantWithKind(make_IntegerConstant_t,
                    make_Integer_t, const_elements, kind, loc),
                implied_doloop_size_);
        }
    }
    if( implied_doloop_size_ ) {
        implied_doloop_size = builder.Mul(implied_doloop_size_, implied_doloop_size);
    }
    return implied_doloop_size;
}

size_t get_constant_ArrayConstant_size(ASR::ArrayConstant_t* x) {
    return ASRUtils::get_fixed_size_of_array(x->m_type);
}

ASR::expr_t* get_ArrayConstant_size(Allocator& al, ASR::ArrayConstant_t* x) {
    ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x->base.base.loc, 4));
    return make_ConstantWithType(make_IntegerConstant_t,
            ASRUtils::get_fixed_size_of_array(x->m_type), int_type, x->base.base.loc);
}

ASR::expr_t* get_ArrayConstructor_size(Allocator& al, ASR::ArrayConstructor_t* x) {
    ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x->base.base.loc, 4));
    ASR::expr_t* array_size = nullptr;
    int64_t constant_size = 0;
    const Location& loc = x->base.base.loc;
    ASRUtils::ASRBuilder builder(al, loc);
    for( size_t i = 0; i < x->n_args; i++ ) {
        ASR::expr_t* element = x->m_args[i];
        if( ASR::is_a<ASR::ArrayConstant_t>(*element) ) {
            if( ASRUtils::is_value_constant(element) ) {
                constant_size += get_constant_ArrayConstant_size(
                    ASR::down_cast<ASR::ArrayConstant_t>(element));
            } else {
                ASR::expr_t* element_array_size = get_ArrayConstant_size(al,
                    ASR::down_cast<ASR::ArrayConstant_t>(element));
                if( array_size == nullptr ) {
                    array_size = element_array_size;
                } else {
                    array_size = builder.Add(array_size,
                                    element_array_size);
                }
            }
        } else if( ASR::is_a<ASR::ArrayConstructor_t>(*element) ) {
            ASR::expr_t* element_array_size = get_ArrayConstructor_size(al,
                ASR::down_cast<ASR::ArrayConstructor_t>(element));
            if( array_size == nullptr ) {
                array_size = element_array_size;
            } else {
                array_size = builder.Add(array_size,
                                element_array_size);
            }
        } else if( ASR::is_a<ASR::Var_t>(*element) ) {
            ASR::ttype_t* element_type = ASRUtils::type_get_past_allocatable(
                ASRUtils::expr_type(element));
            if( ASRUtils::is_array(element_type) ) {
                if( ASRUtils::is_fixed_size_array(element_type) ) {
                    ASR::dimension_t* m_dims = nullptr;
                    size_t n_dims = ASRUtils::extract_dimensions_from_ttype(element_type, m_dims);
                    constant_size += ASRUtils::get_fixed_size_of_array(m_dims, n_dims);
                } else {
                    ASR::expr_t* element_array_size = ASRUtils::get_size(element, al);
                    if( array_size == nullptr ) {
                        array_size = element_array_size;
                    } else {
                        array_size = builder.Add(array_size,
                                        element_array_size);
                    }
                }
            } else {
                constant_size += 1;
            }
        } else if( ASR::is_a<ASR::ImpliedDoLoop_t>(*element) ) {
            ASR::expr_t* implied_doloop_size = get_ImpliedDoLoop_size(al,
                ASR::down_cast<ASR::ImpliedDoLoop_t>(element));
            if( array_size ) {
                array_size = builder.Add(implied_doloop_size, array_size);
            } else {
                array_size = implied_doloop_size;
            }
        } else if( ASR::is_a<ASR::ArraySection_t>(*element) ) {
            ASR::ArraySection_t* array_section_t = ASR::down_cast<ASR::ArraySection_t>(element);
            ASR::expr_t* array_section_size = nullptr;
            for( size_t j = 0; j < array_section_t->n_args; j++ ) {
                ASR::expr_t* start = array_section_t->m_args[j].m_left;
                ASR::expr_t* end = array_section_t->m_args[j].m_right;
                ASR::expr_t* d = array_section_t->m_args[j].m_step;
                if( d == nullptr ) {
                    continue;
                }
                ASR::expr_t* dim_size = builder.Add(builder.Div(
                    builder.Sub(end, start), d),
                    make_ConstantWithKind(make_IntegerConstant_t, make_Integer_t, 1, 4, loc));
                if( array_section_size == nullptr ) {
                    array_section_size = dim_size;
                } else {
                    array_section_size = builder.Mul(array_section_size, dim_size);
                }
            }
            if( array_size == nullptr ) {
                array_size = array_section_size;
            } else {
                builder.Add(array_section_size, array_size);
            }
        } else {
            constant_size += 1;
        }
    }
    ASR::expr_t* constant_size_asr = nullptr;
    if (constant_size == 0 && array_size == nullptr) {
        constant_size = ASRUtils::get_fixed_size_of_array(x->m_type);
    }
    if( constant_size > 0 ) {
        constant_size_asr = make_ConstantWithType(make_IntegerConstant_t,
                                constant_size, int_type, x->base.base.loc);
        if( array_size == nullptr ) {
            return constant_size_asr;
        }
    }
    if( constant_size_asr ) {
        array_size = builder.Add(array_size, constant_size_asr);
    }

    if( array_size == nullptr ) {
        array_size = make_ConstantWithKind(make_IntegerConstant_t,
            make_Integer_t, 0, 4, x->base.base.loc);
    }
    return array_size;
}

ASR::ttype_t* create_array_type_with_empty_dims(Allocator& al,
    size_t value_n_dims, ASR::ttype_t* value_type) {
    Vec<ASR::dimension_t> empty_dims; empty_dims.reserve(al, value_n_dims);
    for( size_t i = 0; i < value_n_dims; i++ ) {
        ASR::dimension_t empty_dim;
        Location loc; loc.first = 1, loc.last = 1;
        empty_dim.loc = loc;
        empty_dim.m_length = nullptr;
        empty_dim.m_start = nullptr;
        empty_dims.push_back(al, empty_dim);
    }
    return ASRUtils::make_Array_t_util(al, value_type->base.loc,
        ASRUtils::extract_type(value_type), empty_dims.p, empty_dims.size());
}

ASR::expr_t* create_temporary_variable_for_scalar(Allocator& al,
    ASR::expr_t* value, SymbolTable* scope, std::string name_hint) {
    ASR::ttype_t* value_type = ASRUtils::expr_type(value);
    LCOMPILERS_ASSERT(!ASRUtils::is_array(value_type));

    ASR::ttype_t* var_type = ASRUtils::duplicate_type(al, ASRUtils::extract_type(value_type));
    std::string var_name = scope->get_unique_name("__libasr_created_" + name_hint);
    ASR::symbol_t* temporary_variable = ASR::down_cast<ASR::symbol_t>(ASR::make_Variable_t(
        al, value->base.loc, scope, s2c(al, var_name), nullptr, 0, ASR::intentType::Local,
        nullptr, nullptr, ASR::storage_typeType::Default, var_type, nullptr, ASR::abiType::Source,
        ASR::accessType::Public, ASR::presenceType::Required, false));
    scope->add_symbol(var_name, temporary_variable);

    return ASRUtils::EXPR(ASR::make_Var_t(al, temporary_variable->base.loc, temporary_variable));
}

ASR::expr_t* create_temporary_variable_for_array(Allocator& al,
    ASR::expr_t* value, SymbolTable* scope, std::string name_hint,
    bool is_pointer_required=false) {
    ASR::ttype_t* value_type = ASRUtils::expr_type(value);
    LCOMPILERS_ASSERT(ASRUtils::is_array(value_type));

    /* Figure out the type of the temporary array variable */
    ASR::dimension_t* value_m_dims = nullptr;
    size_t value_n_dims = ASRUtils::extract_dimensions_from_ttype(value_type, value_m_dims);
    // dimensions can be different for an ArrayConstructor e.g. [1, a], where `a` is an
    // ArrayConstructor like [5, 2, 1]
    if (ASR::is_a<ASR::ArrayConstructor_t>(*value)) {
        ASR::ArrayConstructor_t* arr_constructor = ASR::down_cast<ASR::ArrayConstructor_t>(value);
        value_m_dims->m_length = get_ArrayConstructor_size(al, arr_constructor);
    }
    bool is_fixed_sized_array = ASRUtils::is_fixed_size_array(value_type);
    bool is_size_only_dependent_on_arguments = ASRUtils::is_dimension_dependent_only_on_arguments(
        value_m_dims, value_n_dims);
    bool is_allocatable = ASRUtils::is_allocatable(value_type);
    ASR::ttype_t* var_type = nullptr;
    if( (is_fixed_sized_array || is_size_only_dependent_on_arguments || is_allocatable) &&
        !is_pointer_required ) {
        var_type = value_type;
    } else {
        var_type = create_array_type_with_empty_dims(al, value_n_dims, value_type);
        if( ASR::is_a<ASR::ArraySection_t>(*value) && is_pointer_required ) {
            if( ASRUtils::is_simd_array(value) ) {
                var_type = ASRUtils::expr_type(value);
            } else {
                var_type = ASRUtils::TYPE(ASR::make_Pointer_t(al, var_type->base.loc, var_type));
            }
        } else {
            var_type = ASRUtils::TYPE(ASRUtils::make_Allocatable_t_util(al, var_type->base.loc, var_type));
        }
    }

    std::string var_name = scope->get_unique_name("__libasr_created_" + name_hint);
    ASR::symbol_t* temporary_variable = ASR::down_cast<ASR::symbol_t>(ASR::make_Variable_t(
        al, value->base.loc, scope, s2c(al, var_name), nullptr, 0, ASR::intentType::Local,
        nullptr, nullptr, ASR::storage_typeType::Default, var_type, nullptr, ASR::abiType::Source,
        ASR::accessType::Public, ASR::presenceType::Required, false));
    scope->add_symbol(var_name, temporary_variable);

    return ASRUtils::EXPR(ASR::make_Var_t(al, temporary_variable->base.loc, temporary_variable));
}

ASR::expr_t* create_temporary_variable_for_array(Allocator& al, const Location& loc,
    SymbolTable* scope, std::string name_hint, ASR::ttype_t* value_type) {

    std::string var_name = scope->get_unique_name("__libasr_created_" + name_hint);
    ASR::symbol_t* temporary_variable = ASR::down_cast<ASR::symbol_t>(ASR::make_Variable_t(
        al, loc, scope, s2c(al, var_name), nullptr, 0, ASR::intentType::Local,
        nullptr, nullptr, ASR::storage_typeType::Default, value_type, nullptr, ASR::abiType::Source,
        ASR::accessType::Public, ASR::presenceType::Required, false));
    scope->add_symbol(var_name, temporary_variable);

    return ASRUtils::EXPR(ASR::make_Var_t(al, temporary_variable->base.loc, temporary_variable));
}

ASR::expr_t* create_temporary_variable_for_struct(Allocator& al,
    ASR::expr_t* value, SymbolTable* scope, std::string name_hint) {
    ASR::ttype_t* value_type = ASRUtils::expr_type(value);
    LCOMPILERS_ASSERT(ASRUtils::is_struct(*value_type));

    std::string var_name = scope->get_unique_name("__libasr_created_" + name_hint);
    ASR::symbol_t* temporary_variable = ASR::down_cast<ASR::symbol_t>(ASR::make_Variable_t(
        al, value->base.loc, scope, s2c(al, var_name), nullptr, 0, ASR::intentType::Local,
        nullptr, nullptr, ASR::storage_typeType::Default, value_type, nullptr, ASR::abiType::Source,
        ASR::accessType::Public, ASR::presenceType::Required, false));
    scope->add_symbol(var_name, temporary_variable);

    return ASRUtils::EXPR(ASR::make_Var_t(al, temporary_variable->base.loc, temporary_variable));
}

template <typename T>
ASR::expr_t* get_first_array_function_args(T* func) {
    int64_t first_array_arg_idx = -1;
    ASR::expr_t* first_array_arg = nullptr;
    for (int64_t i = 0; i < (int64_t)func->n_args; i++) {
        ASR::ttype_t* func_arg_type;
        if constexpr (std::is_same_v<T, ASR::FunctionCall_t>) {
            func_arg_type = ASRUtils::expr_type(func->m_args[i].m_value);
        } else {
            func_arg_type = ASRUtils::expr_type(func->m_args[i]);
        }
        if (ASRUtils::is_array(func_arg_type)) {
            first_array_arg_idx = i;
            break;
        }
    }
    LCOMPILERS_ASSERT(first_array_arg_idx != -1)
    if constexpr (std::is_same_v<T, ASR::FunctionCall_t>) {
        first_array_arg = func->m_args[first_array_arg_idx].m_value;
    } else {
        first_array_arg = func->m_args[first_array_arg_idx];
    }
    return first_array_arg;
}

/*
    sets allocation size of an elemental function, which can be
    either an intrinsic elemental function or a user-defined
*/
template <typename T>
void set_allocation_size_elemental_function(
    Allocator& al, const Location& loc,
    T* elemental_function,
    Vec<ASR::dimension_t>& allocate_dims
) {
    ASR::expr_t* int32_one = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                al, loc, 1, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4))));
    size_t n_dims = ASRUtils::extract_n_dims_from_ttype(elemental_function->m_type);
    allocate_dims.reserve(al, n_dims);
    ASR::expr_t* first_array_arg = get_first_array_function_args(elemental_function);
    for( size_t i = 0; i < n_dims; i++ ) {
        ASR::dimension_t allocate_dim;
        allocate_dim.loc = loc;
        allocate_dim.m_start = int32_one;
        ASR::expr_t* size_i_1 = ASRUtils::EXPR(ASR::make_ArraySize_t(
            al, loc, first_array_arg,
            ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                al, loc, i + 1, ASRUtils::expr_type(int32_one))),
            ASRUtils::expr_type(int32_one), nullptr));
        allocate_dim.m_length = size_i_1;
        allocate_dims.push_back(al, allocate_dim);
    }
}

bool set_allocation_size(
    Allocator& al, ASR::expr_t* value,
    Vec<ASR::dimension_t>& allocate_dims
) {
    if ( !ASRUtils::is_array(ASRUtils::expr_type(value)) ) {
        return false;
    }
    const Location& loc = value->base.loc;
    ASR::expr_t* int32_one = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                al, loc, 1, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4))));
    if( ASRUtils::is_fixed_size_array(ASRUtils::expr_type(value)) ) {
        ASR::dimension_t* m_dims = nullptr;
        size_t n_dims = ASRUtils::extract_dimensions_from_ttype(
            ASRUtils::expr_type(value), m_dims);
        allocate_dims.reserve(al, n_dims);
        for( size_t i = 0; i < n_dims; i++ ) {
            ASR::dimension_t allocate_dim;
            allocate_dim.loc = value->base.loc;
            allocate_dim.m_start = int32_one;
            allocate_dim.m_length = m_dims[i].m_length;
            allocate_dims.push_back(al, allocate_dim);
        }
        return true;
    }
    switch( value->type ) {
        case ASR::exprType::FunctionCall: {
            ASR::FunctionCall_t* function_call = ASR::down_cast<ASR::FunctionCall_t>(value);
            ASR::ttype_t* type = function_call->m_type;
            if( ASRUtils::is_allocatable(type) ) {
                return false;
            }
            if (PassUtils::is_elemental(function_call->m_name)) {
                set_allocation_size_elemental_function(al, loc, function_call, allocate_dims);
                break;
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
                LCOMPILERS_ASSERT(dim_copy.m_start);
                LCOMPILERS_ASSERT(dim_copy.m_length);
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
        case ASR::exprType::StringCompare:
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
            // ArrayVarCollector array_var_collector(al, array_vars);
            // array_var_collector.visit_expr(*value);
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
        case ASR::exprType::LogicalNot: {
            ASR::LogicalNot_t* logical_not = ASR::down_cast<ASR::LogicalNot_t>(value);
            if ( ASRUtils::is_array(ASRUtils::expr_type(logical_not->m_arg)) ) {
                size_t rank = ASRUtils::extract_n_dims_from_ttype(
                    ASRUtils::expr_type(logical_not->m_arg));
                ASR::expr_t* selected_array = logical_not->m_arg;
                allocate_dims.reserve(al, rank);
                for( size_t i = 0; i < rank; i++ ) {
                    ASR::dimension_t allocate_dim;
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
            }
            break;
        }
        case ASR::exprType::Cast: {
            ASR::Cast_t* cast = ASR::down_cast<ASR::Cast_t>(value);
            if ( ASRUtils::is_array(ASRUtils::expr_type(cast->m_arg)) ) {
                size_t rank = ASRUtils::extract_n_dims_from_ttype(
                    ASRUtils::expr_type(cast->m_arg));
                ASR::expr_t* selected_array = cast->m_arg;
                allocate_dims.reserve(al, rank);
                for( size_t i = 0; i < rank; i++ ) {
                    ASR::dimension_t allocate_dim;
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
            }
            break;
        }
        case ASR::exprType::ArraySection: {
            ASR::ArraySection_t* array_section_t = ASR::down_cast<ASR::ArraySection_t>(value);
            allocate_dims.reserve(al, array_section_t->n_args);
            for( size_t i = 0; i < array_section_t->n_args; i++ ) {
                ASR::expr_t* start = array_section_t->m_args[i].m_left;
                ASR::expr_t* end = array_section_t->m_args[i].m_right;
                ASR::expr_t* step = array_section_t->m_args[i].m_step;
                ASR::expr_t* end_minus_start = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                    end, ASR::binopType::Sub, start, ASRUtils::expr_type(end), nullptr));
                ASR::expr_t* by_step = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                    end_minus_start, ASR::binopType::Div, step, ASRUtils::expr_type(end_minus_start),
                    nullptr));
                ASR::expr_t* length = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc,
                    by_step, ASR::binopType::Add, int32_one, ASRUtils::expr_type(by_step), nullptr));
                ASR::dimension_t allocate_dim;
                allocate_dim.loc = loc;
                allocate_dim.m_start = int32_one;
                allocate_dim.m_length = length;
                allocate_dims.push_back(al, allocate_dim);
            }
            break;
        }
        case ASR::exprType::IntrinsicElementalFunction: {
            ASR::IntrinsicElementalFunction_t* intrinsic_elemental_function =
                ASR::down_cast<ASR::IntrinsicElementalFunction_t>(value);
            switch (intrinsic_elemental_function->m_intrinsic_id) {
                case static_cast<int64_t>(ASRUtils::IntrinsicElementalFunctions::BesselJ0):
                case static_cast<int64_t>(ASRUtils::IntrinsicElementalFunctions::BesselJ1):
                case static_cast<int64_t>(ASRUtils::IntrinsicElementalFunctions::BesselJN):
                case static_cast<int64_t>(ASRUtils::IntrinsicElementalFunctions::BesselY0):
                case static_cast<int64_t>(ASRUtils::IntrinsicElementalFunctions::BesselY1):
                case static_cast<int64_t>(ASRUtils::IntrinsicElementalFunctions::BesselYN):
                case static_cast<int64_t>(ASRUtils::IntrinsicElementalFunctions::Real):
                case static_cast<int64_t>(ASRUtils::IntrinsicElementalFunctions::Sin):
                case static_cast<int64_t>(ASRUtils::IntrinsicElementalFunctions::Exp):
                case static_cast<int64_t>(ASRUtils::IntrinsicElementalFunctions::Abs):
                case static_cast<int64_t>(ASRUtils::IntrinsicElementalFunctions::Aimag):
                case static_cast<int64_t>(ASRUtils::IntrinsicElementalFunctions::Int):
                case static_cast<int64_t>(ASRUtils::IntrinsicElementalFunctions::Merge): {
                    set_allocation_size_elemental_function(al, loc, intrinsic_elemental_function,
                        allocate_dims);
                    break;
                }
                default: {
                    LCOMPILERS_ASSERT_MSG(false, "ASR::IntrinsicElementalFunctions::" +
                        ASRUtils::get_intrinsic_name(intrinsic_elemental_function->m_intrinsic_id)
                        + " not handled yet in set_allocation_size");
                }
            }
            break;
        }
        case ASR::exprType::IntrinsicArrayFunction: {
            ASR::IntrinsicArrayFunction_t* intrinsic_array_function =
                ASR::down_cast<ASR::IntrinsicArrayFunction_t>(value);
            switch (intrinsic_array_function->m_arr_intrinsic_id) {
                case static_cast<int64_t>(ASRUtils::IntrinsicArrayFunctions::All):
                case static_cast<int64_t>(ASRUtils::IntrinsicArrayFunctions::Any):
                case static_cast<int64_t>(ASRUtils::IntrinsicArrayFunctions::Count):
                case static_cast<int64_t>(ASRUtils::IntrinsicArrayFunctions::Parity):
                case static_cast<int64_t>(ASRUtils::IntrinsicArrayFunctions::Sum): {
                    size_t n_dims = ASRUtils::extract_n_dims_from_ttype(
                        intrinsic_array_function->m_type);
                    allocate_dims.reserve(al, n_dims);
                    for( size_t i = 0; i < n_dims; i++ ) {
                        ASR::dimension_t allocate_dim;
                        allocate_dim.loc = loc;
                        allocate_dim.m_start = int32_one;
                        ASR::expr_t* size_i_1 = ASRUtils::EXPR(ASR::make_ArraySize_t(
                            al, loc, intrinsic_array_function->m_args[0],
                            ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                                al, loc, i + 1, ASRUtils::expr_type(int32_one))),
                            ASRUtils::expr_type(int32_one), nullptr));
                        ASR::expr_t* size_i_2 = ASRUtils::EXPR(ASR::make_ArraySize_t(
                            al, loc, intrinsic_array_function->m_args[0],
                            ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                                al, loc, i + 2, ASRUtils::expr_type(int32_one))),
                            ASRUtils::expr_type(int32_one), nullptr));
                        Vec<ASR::expr_t*> merge_i_args; merge_i_args.reserve(al, 3);
                        merge_i_args.push_back(al, size_i_1); merge_i_args.push_back(al, size_i_2);
                        merge_i_args.push_back(al, ASRUtils::EXPR(ASR::make_IntegerCompare_t(al, loc,
                            ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                                al, loc, i + 1, ASRUtils::expr_type(int32_one))), ASR::cmpopType::Lt,
                                intrinsic_array_function->m_args[1],
                                ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4)), nullptr)));
                        ASR::expr_t* merge_i = ASRUtils::EXPR(ASRUtils::make_IntrinsicElementalFunction_t_util(
                            al, loc, static_cast<int64_t>(ASRUtils::IntrinsicElementalFunctions::Merge),
                            merge_i_args.p, merge_i_args.size(), 0, ASRUtils::expr_type(int32_one), nullptr));
                        allocate_dim.m_length = merge_i;
                        allocate_dims.push_back(al, allocate_dim);
                    }
                    break;
                }
                case static_cast<int64_t>(ASRUtils::IntrinsicArrayFunctions::Pack): {
                    size_t n_dims = ASRUtils::extract_n_dims_from_ttype(
                        intrinsic_array_function->m_type);
                    allocate_dims.reserve(al, n_dims);
                    for ( size_t i = 0; i < n_dims; i++ ) {
                        ASR::dimension_t allocate_dim;
                        allocate_dim.loc = loc;
                        allocate_dim.m_start = int32_one;
                        ASR::expr_t* size_i_1 = nullptr;
                        if (intrinsic_array_function->n_args == 3) {
                            size_i_1 = ASRUtils::EXPR(ASR::make_ArraySize_t(
                                al, loc, intrinsic_array_function->m_args[2],
                                ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                                    al, loc, i + 1, ASRUtils::expr_type(int32_one))),
                                ASRUtils::expr_type(int32_one), nullptr));
                        } else {
                            Vec<ASR::expr_t*> count_i_args; count_i_args.reserve(al, 1);
                            count_i_args.push_back(al, intrinsic_array_function->m_args[1]);
                            size_i_1 = ASRUtils::EXPR(ASRUtils::make_IntrinsicArrayFunction_t_util(
                                al, loc, static_cast<int64_t>(ASRUtils::IntrinsicArrayFunctions::Count),
                                count_i_args.p, count_i_args.size(), 0, ASRUtils::expr_type(int32_one), nullptr));
                        }
                        allocate_dim.m_length = size_i_1;
                        allocate_dims.push_back(al, allocate_dim);
                    }
                    break;
                }
                case static_cast<int64_t>(ASRUtils::IntrinsicArrayFunctions::Shape): {
                    size_t n_dims = ASRUtils::extract_n_dims_from_ttype(
                        intrinsic_array_function->m_type);
                    allocate_dims.reserve(al, n_dims);
                    for( size_t i = 0; i < n_dims; i++ ) {
                        ASR::dimension_t allocate_dim;
                        allocate_dim.loc = loc;
                        allocate_dim.m_start = int32_one;
                        allocate_dim.m_length = int32_one;
                        allocate_dims.push_back(al, allocate_dim);
                    }
                    break;
                }
                default: {
                    LCOMPILERS_ASSERT_MSG(false, "ASR::IntrinsicArrayFunctions::" +
                        ASRUtils::get_array_intrinsic_name(intrinsic_array_function->m_arr_intrinsic_id)
                        + " not handled yet in set_allocation_size");
                }
            }
            break;
        }
        case ASR::exprType::StructInstanceMember: {
            ASR::StructInstanceMember_t* struct_instance_member_t =
                ASR::down_cast<ASR::StructInstanceMember_t>(value);
            size_t n_dims = ASRUtils::extract_n_dims_from_ttype(struct_instance_member_t->m_type);
            allocate_dims.reserve(al, n_dims);
            if( ASRUtils::is_array(ASRUtils::expr_type(struct_instance_member_t->m_v)) ) {
                value = struct_instance_member_t->m_v;
            }
            ASRUtils::ExprStmtDuplicator expr_duplicator(al);
            for( size_t i = 0; i < n_dims; i++ ) {
                ASR::dimension_t allocate_dim;
                allocate_dim.loc = loc;
                allocate_dim.m_start = int32_one;
                allocate_dim.m_length = ASRUtils::EXPR(ASR::make_ArraySize_t(
                    al, loc, expr_duplicator.duplicate_expr(value),
                        ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                        al, loc, i + 1, ASRUtils::expr_type(int32_one))),
                    ASRUtils::expr_type(int32_one), nullptr));
                allocate_dims.push_back(al, allocate_dim);
            }
            break;
        }
        case ASR::exprType::ArrayReshape: {
            ASR::ArrayReshape_t* array_reshape_t = ASR::down_cast<ASR::ArrayReshape_t>(value);
            size_t n_dims = ASRUtils::extract_n_dims_from_ttype(
                ASRUtils::expr_type(array_reshape_t->m_shape));
            allocate_dims.reserve(al, n_dims);
            for( size_t i = 0; i < n_dims; i++ ) {
                ASR::dimension_t allocate_dim;
                allocate_dim.loc = loc;
                allocate_dim.m_start = int32_one;
                allocate_dim.m_length = int32_one;
                allocate_dims.push_back(al, allocate_dim);
            }
            break;
        }
        case ASR::exprType::ArrayConstructor: {
            allocate_dims.reserve(al, 1);
            ASR::dimension_t allocate_dim;
            allocate_dim.loc = loc;
            allocate_dim.m_start = int32_one;
            allocate_dim.m_length = get_ArrayConstructor_size(al,
                ASR::down_cast<ASR::ArrayConstructor_t>(value));
            allocate_dims.push_back(al, allocate_dim);
            break;
        }
        case ASR::exprType::ArrayConstant: {
            allocate_dims.reserve(al, 1);
            ASR::dimension_t allocate_dim;
            allocate_dim.loc = loc;
            allocate_dim.m_start = int32_one;
            allocate_dim.m_length = get_ArrayConstant_size(al,
                ASR::down_cast<ASR::ArrayConstant_t>(value));
            allocate_dims.push_back(al, allocate_dim);
            break;
        }
        default: {
            LCOMPILERS_ASSERT_MSG(false, "ASR::exprType::" + std::to_string(value->type)
                + " not handled yet in set_allocation_size");
        }
    }
    return true;
}

void insert_allocate_stmt_for_array(Allocator& al, ASR::expr_t* temporary_var,
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

void insert_allocate_stmt_for_struct(Allocator& al, ASR::expr_t* temporary_var,
    ASR::expr_t* value, Vec<ASR::stmt_t*>* current_body) {
    if( !ASRUtils::is_allocatable(temporary_var) ) {
        return ;
    }

    Vec<ASR::alloc_arg_t> alloc_args; alloc_args.reserve(al, 1);
    ASR::alloc_arg_t alloc_arg;
    alloc_arg.loc = value->base.loc;
    alloc_arg.m_a = temporary_var;
    alloc_arg.m_dims = nullptr;
    alloc_arg.n_dims = 0;
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

ASR::expr_t* create_and_declare_temporary_variable_for_scalar(
    ASR::expr_t* scalar_expr, const std::string& name_hint, Allocator& al,
    Vec<ASR::stmt_t*>*& current_body, SymbolTable* current_scope,
    ExprsWithTargetType& exprs_with_target) {
    const Location& loc = scalar_expr->base.loc;
    ASR::expr_t* scalar_var_temporary = create_temporary_variable_for_scalar(
        al, scalar_expr, current_scope, name_hint);
    current_body->push_back(al, ASRUtils::STMT(make_Assignment_t_util(
        al, loc, scalar_var_temporary, scalar_expr, nullptr, exprs_with_target)));
    return scalar_var_temporary;
}

ASR::expr_t* create_and_allocate_temporary_variable_for_array(
    ASR::expr_t* array_expr, const std::string& name_hint, Allocator& al,
    Vec<ASR::stmt_t*>*& current_body, SymbolTable* current_scope,
    ExprsWithTargetType& exprs_with_target, bool is_pointer_required=false) {
    const Location& loc = array_expr->base.loc;
    ASR::expr_t* array_var_temporary = create_temporary_variable_for_array(
        al, array_expr, current_scope, name_hint, is_pointer_required);
    if( ASRUtils::is_pointer(ASRUtils::expr_type(array_var_temporary)) ) {
        exprs_with_target[array_expr] = std::make_pair(array_var_temporary, targetType::GeneratedTarget);
        current_body->push_back(al, ASRUtils::STMT(ASR::make_Associate_t(
            al, loc, array_var_temporary, array_expr)));
    } else {
        insert_allocate_stmt_for_array(al, array_var_temporary, array_expr, current_body);
        array_expr = ASRUtils::get_past_array_physical_cast(array_expr);
        if( ASR::is_a<ASR::ArraySection_t>(*array_expr) && !is_pointer_required &&
            !ASRUtils::is_simd_array(array_expr) ) {
            size_t value_n_dims = ASRUtils::extract_n_dims_from_ttype(
                ASRUtils::expr_type(array_expr));
            ASR::ttype_t* tmp_type = create_array_type_with_empty_dims(
                al, value_n_dims, ASRUtils::expr_type(array_expr));
            tmp_type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc, tmp_type));
            ASR::expr_t* array_expr_ptr = create_temporary_variable_for_array(
                al, array_expr->base.loc, current_scope,
                "_array_section_pointer_", tmp_type);
            current_body->push_back(al, ASRUtils::STMT(ASR::make_Associate_t(
                al, loc, array_expr_ptr, array_expr)));
            exprs_with_target[array_expr] = std::make_pair(array_expr_ptr, targetType::GeneratedTarget);
            array_expr = array_expr_ptr;
        }
        current_body->push_back(al, ASRUtils::STMT(make_Assignment_t_util(
            al, loc, array_var_temporary, array_expr, nullptr, exprs_with_target)));
    }
    return array_var_temporary;
}

ASR::expr_t* create_and_allocate_temporary_variable_for_struct(
    ASR::expr_t* struct_expr, const std::string& name_hint, Allocator& al,
    Vec<ASR::stmt_t*>*& current_body, SymbolTable* current_scope,
    ExprsWithTargetType& exprs_with_target) {
    const Location& loc = struct_expr->base.loc;
    ASR::expr_t* struct_var_temporary = create_temporary_variable_for_struct(
        al, struct_expr, current_scope, name_hint);
    if( ASRUtils::is_pointer(ASRUtils::expr_type(struct_var_temporary)) ) {
        exprs_with_target[struct_expr] = std::make_pair(struct_var_temporary, targetType::GeneratedTarget);
        current_body->push_back(al, ASRUtils::STMT(ASR::make_Associate_t(
            al, loc, struct_var_temporary, struct_expr)));
    } else {
        insert_allocate_stmt_for_struct(al, struct_var_temporary, struct_expr, current_body);
        struct_expr = ASRUtils::get_past_array_physical_cast(struct_expr);
        current_body->push_back(al, ASRUtils::STMT(make_Assignment_t_util(
            al, loc, struct_var_temporary, struct_expr, nullptr, exprs_with_target)));
    }
    return struct_var_temporary;
}

bool is_elemental_expr(ASR::expr_t* value) {
    value = ASRUtils::get_past_array_physical_cast(value);
    switch( value->type ) {
        case ASR::exprType::Var:
        case ASR::exprType::StructInstanceMember: {
            return true;
        }
        default: {
            return false;
        }
    }
}

bool is_temporary_needed(ASR::expr_t* value) {
    bool is_expr_with_no_type = value && (std::find(exprs_with_no_type.begin(), exprs_with_no_type.end(),
        value->type) == exprs_with_no_type.end()) && ASRUtils::is_array(ASRUtils::expr_type(value));
    bool is_non_empty_fixed_size_array = value && (!ASRUtils::is_fixed_size_array(ASRUtils::expr_type(value)) ||
        (ASRUtils::is_fixed_size_array(ASRUtils::expr_type(value)) &&
        ASRUtils::get_fixed_size_of_array(ASRUtils::expr_type(value)) > 0));
    return value && is_expr_with_no_type &&
            !is_elemental_expr(value) && is_non_empty_fixed_size_array;
}


class ReplaceExprWithTemporary: public ASR::BaseExprReplacer<ReplaceExprWithTemporary> {

    private:

    // Allocator& al;
    // ExprsWithTargetType& exprs_with_target;
    // bool realloc_lhs;

    public:

    Vec<ASR::stmt_t*>* current_body;
    SymbolTable* current_scope;
    bool is_assignment_target_array_section;
    bool is_simd_expression;
    ASR::ttype_t* simd_type;

    // ReplaceExprWithTemporary(Allocator& al_, ExprsWithTargetType& exprs_with_target_, bool realloc_lhs_) :
    //     al(al_), exprs_with_target(exprs_with_target_), realloc_lhs(realloc_lhs_), current_scope(nullptr),
    //     is_assignment_target_array_section(false), is_simd_expression(false), simd_type(nullptr) {}

    #define is_current_expr_linked_to_target exprs_with_target.find(*current_expr) != exprs_with_target.end()

    #define is_current_expr_linked_to_target_then_return if( is_current_expr_linked_to_target ) { \
            std::pair<ASR::expr_t*, targetType>& target_info = exprs_with_target[*current_expr]; \
            ASR::expr_t* target = target_info.first; targetType target_Type = target_info.second; \
            if( ASRUtils::is_allocatable(ASRUtils::expr_type(target)) && \
                target_Type == targetType::OriginalTarget && \
                realloc_lhs ) { \
                insert_allocate_stmt_for_array(al, target, *current_expr, current_body); \
            } \
            return ; \
        }

    #define force_replace_current_expr_for_array(name_hint) *current_expr = create_and_allocate_temporary_variable_for_array( \
                *current_expr, name_hint, al, current_body, \
                current_scope, exprs_with_target, is_assignment_target_array_section); \

    #define force_replace_current_expr_for_struct(name_hint) *current_expr = create_and_allocate_temporary_variable_for_struct( \
            *current_expr, name_hint, al, current_body, \
            current_scope, exprs_with_target); \

    #define force_replace_current_expr_for_scalar(name_hint) *current_expr = create_and_declare_temporary_variable_for_scalar( \
                *current_expr, name_hint, al, current_body, \
                current_scope, exprs_with_target); \

    #define replace_current_expr(name_hint) is_current_expr_linked_to_target_then_return \
        if( ASRUtils::is_array(x->m_type) ) { \
            force_replace_current_expr_for_array(name_hint) \
        } else if( ASRUtils::is_struct(*x->m_type) ) { \
            force_replace_current_expr_for_struct(name_hint) \
        }
};

#define check_if_ASR_owner_is_module(asr_owner) asr_owner && \
    ASR::is_a<ASR::symbol_t>(*asr_owner) && \
    ASR::is_a<ASR::Module_t>(*ASR::down_cast<ASR::symbol_t>(asr_owner))

#define check_if_ASR_owner_is_enum(asr_owner) asr_owner && \
    ASR::is_a<ASR::symbol_t>(*asr_owner) && \
    ASR::is_a<ASR::Enum_t>(*ASR::down_cast<ASR::symbol_t>(asr_owner))

#define check_if_ASR_owner_is_struct(asr_owner) asr_owner && \
    ASR::is_a<ASR::symbol_t>(*asr_owner) && \
    ASR::is_a<ASR::Struct_t>(*ASR::down_cast<ASR::symbol_t>(asr_owner))




void pass_simplifier(Allocator &al, ASR::TranslationUnit_t &unit,
                     const PassOptions &pass_options) {
    // TODO: Add a visitor in asdl_cpp.py which will replace
    // current_expr with its own `m_value` (if `m_value` is not nullptr)
    // Call the visitor here.

    (void)al; // remove once we start porting code from simplifier branch (warning silence)
    (void)unit; // remove once we start porting code from simplifier branch (warning silence)
    (void)pass_options; // remove once we start porting code from simplifier branch (warning silence)
}


} // namespace LCompilers
