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
    ASR::symbol_t* temporary_variable = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Variable_t_util(
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
    ASR::symbol_t* temporary_variable = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Variable_t_util(
        al, value->base.loc, scope, s2c(al, var_name), nullptr, 0, ASR::intentType::Local,
        nullptr, nullptr, ASR::storage_typeType::Default, var_type, nullptr, ASR::abiType::Source,
        ASR::accessType::Public, ASR::presenceType::Required, false));
    scope->add_symbol(var_name, temporary_variable);

    return ASRUtils::EXPR(ASR::make_Var_t(al, temporary_variable->base.loc, temporary_variable));
}

ASR::expr_t* create_temporary_variable_for_array(Allocator& al, const Location& loc,
    SymbolTable* scope, std::string name_hint, ASR::ttype_t* value_type) {

    std::string var_name = scope->get_unique_name("__libasr_created_" + name_hint);
    ASR::symbol_t* temporary_variable = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Variable_t_util(
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
    ASR::symbol_t* temporary_variable = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Variable_t_util(
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
    LCOMPILERS_ASSERT(ASRUtils::is_array(ASRUtils::expr_type(value)));
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
        case ASR::exprType::Var: {
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

class ArgSimplifier: public ASR::CallReplacerOnExpressionsVisitor<ArgSimplifier>
{

    private:

    Allocator& al;
    Vec<ASR::stmt_t*>* current_body;
    ExprsWithTargetType& exprs_with_target;
    bool realloc_lhs;

    public:

    ArgSimplifier(Allocator& al_, ExprsWithTargetType& exprs_with_target_, bool realloc_lhs_) :
        al(al_), current_body(nullptr), exprs_with_target(exprs_with_target_),
        realloc_lhs(realloc_lhs_) {}

    void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
        transform_stmts_impl
    }

    #define BEGIN_VAR_CHECK(expr) if( !ASR::is_a<ASR::Var_t>(*expr) && \
        ASRUtils::is_array(ASRUtils::expr_type(expr)) ) {
    #define END_VAR_CHECK }

    #define call_create_and_allocate_temporary_variable(expr) ASR::expr_t* x_m_args_i = ASRUtils::get_past_array_physical_cast(expr); \
        ASR::expr_t* array_var_temporary = create_and_allocate_temporary_variable_for_array( \
            x_m_args_i, name_hint, al, current_body, current_scope, exprs_with_target, \
            ASR::is_a<ASR::ArraySection_t>(*x_m_args_i));

    void visit_IO(ASR::expr_t**& x_values, size_t& n_values, const std::string& name_hint) {
        Vec<ASR::expr_t*> x_m_values; x_m_values.reserve(al, n_values);
        /* For frontends like LC, we will need to traverse the print statement arguments
           in reverse order. */
        for( size_t i = 0; i < n_values; i++ ) {
            if( is_temporary_needed(x_values[i]) ) {
                visit_expr(*x_values[i]);
                call_create_and_allocate_temporary_variable(x_values[i])
                x_m_values.push_back(al, array_var_temporary);
            } else if( ASRUtils::is_struct(*ASRUtils::expr_type(x_values[i])) &&
                       !ASR::is_a<ASR::Var_t>(
                            *ASRUtils::get_past_array_physical_cast(x_values[i])) ) {
                visit_expr(*x_values[i]);
                ASR::expr_t* struct_var_temporary = create_and_allocate_temporary_variable_for_struct(
                    ASRUtils::get_past_array_physical_cast(x_values[i]), name_hint, al, current_body,
                    current_scope, exprs_with_target);
                if( ASR::is_a<ASR::ArrayPhysicalCast_t>(*x_values[i]) ) {
                    ASR::ArrayPhysicalCast_t* x_m_values_i = ASR::down_cast<ASR::ArrayPhysicalCast_t>(x_values[i]);
                    struct_var_temporary = ASRUtils::EXPR(ASRUtils::make_ArrayPhysicalCast_t_util(
                        al, struct_var_temporary->base.loc, struct_var_temporary,
                        ASRUtils::extract_physical_type(ASRUtils::expr_type(struct_var_temporary)),
                        x_m_values_i->m_new, x_m_values_i->m_type, nullptr));
                }
                x_m_values.push_back(al, struct_var_temporary);
            } else if( ASR::is_a<ASR::ImpliedDoLoop_t>(*x_values[i]) ) {
                ASR::ImpliedDoLoop_t* implied_do_loop = ASR::down_cast<ASR::ImpliedDoLoop_t>(x_values[i]);
                const Location& loc = x_values[i]->base.loc;
                Vec<ASR::expr_t*> array_con_args; array_con_args.reserve(al, 1);
                array_con_args.push_back(al, x_values[i]);
                Vec<ASR::dimension_t> m_dims; m_dims.reserve(al, 1);
                ASRUtils::ASRBuilder builder(al, loc);
                ASR::dimension_t m_dim; m_dim.loc = loc;
                m_dim.m_start = builder.i32(1);
                m_dim.m_length = get_ImpliedDoLoop_size(al, implied_do_loop);
                m_dims.push_back(al, m_dim);
                ASR::ttype_t* type = ASRUtils::make_Array_t_util(al, loc,
                    implied_do_loop->m_type, m_dims.p, m_dims.size());
                x_m_values.push_back(al, ASRUtils::EXPR(ASRUtils::make_ArrayConstructor_t_util(al, loc,
                    array_con_args.p, array_con_args.size(), type, ASR::arraystorageType::ColMajor)));
            } else {
                visit_expr(*x_values[i]);
                x_m_values.push_back(al, x_values[i]);
            }
        }

        x_values = x_m_values.p;
        n_values = x_m_values.size();
    }

    void traverse_args(Vec<ASR::expr_t*>& x_m_args_vec, ASR::expr_t** x_m_args,
        size_t x_n_args, const std::string& name_hint) {
        /* For other frontends, we might need to traverse the arguments
           in reverse order. */
        for( size_t i = 0; i < x_n_args; i++ ) {
            visit_expr(*x_m_args[i]);
            if( is_temporary_needed(x_m_args[i]) ) {
                call_create_and_allocate_temporary_variable(x_m_args[i])
                if( ASR::is_a<ASR::ArrayPhysicalCast_t>(*x_m_args[i]) ) {
                    ASR::ArrayPhysicalCast_t* x_m_args_i = ASR::down_cast<ASR::ArrayPhysicalCast_t>(x_m_args[i]);
                    array_var_temporary = ASRUtils::EXPR(ASRUtils::make_ArrayPhysicalCast_t_util(
                        al, array_var_temporary->base.loc, array_var_temporary,
                        ASRUtils::extract_physical_type(ASRUtils::expr_type(array_var_temporary)),
                        x_m_args_i->m_new, x_m_args_i->m_type, nullptr));
                }
                x_m_args_vec.push_back(al, array_var_temporary);
            } else if( ASRUtils::is_struct(*ASRUtils::expr_type(x_m_args[i])) &&
                       !ASR::is_a<ASR::Var_t>(
                            *ASRUtils::get_past_array_physical_cast(x_m_args[i])) ) {
                ASR::expr_t* struct_var_temporary = create_and_allocate_temporary_variable_for_struct(
                    ASRUtils::get_past_array_physical_cast(x_m_args[i]), name_hint, al, current_body,
                    current_scope, exprs_with_target);
                if( ASR::is_a<ASR::ArrayPhysicalCast_t>(*x_m_args[i]) ) {
                    ASR::ArrayPhysicalCast_t* x_m_args_i = ASR::down_cast<ASR::ArrayPhysicalCast_t>(x_m_args[i]);
                    struct_var_temporary = ASRUtils::EXPR(ASRUtils::make_ArrayPhysicalCast_t_util(
                        al, struct_var_temporary->base.loc, struct_var_temporary,
                        ASRUtils::extract_physical_type(ASRUtils::expr_type(struct_var_temporary)),
                        x_m_args_i->m_new, x_m_args_i->m_type, nullptr));
                }
                x_m_args_vec.push_back(al, struct_var_temporary);
            } else {
                x_m_args_vec.push_back(al, x_m_args[i]);
            }
        }
    }

    void traverse_call_args(Vec<ASR::call_arg_t>& x_m_args_vec, ASR::call_arg_t* x_m_args,
        size_t x_n_args, const std::string& name_hint) {
        /* For other frontends, we might need to traverse the arguments
           in reverse order. */
        for( size_t i = 0; i < x_n_args; i++ ) {
            if( is_temporary_needed(x_m_args[i].m_value) ) {
                visit_call_arg(x_m_args[i]);
                call_create_and_allocate_temporary_variable(x_m_args[i].m_value)
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

    void visit_Variable(const ASR::Variable_t& /*x*/) {
        // Do nothing
    }

    void visit_FunctionType(const ASR::FunctionType_t& /*x*/) {
        // Do nothing
    }

    // void visit_ArrayBroadcast(const ASR::ArrayBroadcast_t& /*x*/) {
    //     // Do nothing
    // }

    void visit_Assignment(const ASR::Assignment_t& x) {
        ASR::Assignment_t& xx = const_cast<ASR::Assignment_t&>(x);
        // e.g.; a = [b, a], where 'a' is an allocatable
        if (realloc_lhs && ASR::is_a<ASR::ArrayConstructor_t>(*xx.m_value) &&
            ASRUtils::is_allocatable(xx.m_target)
        ) {
            // TODO: dealing with StructType would need thinking similar to the
            // way `traverse_args` handles it, the only reason to not
            // add it is because there is currently no integration test
            // for it
            if (!ASRUtils::is_struct(*ASRUtils::expr_type(xx.m_value))) {
                ASR::Var_t* v1 = ASR::down_cast<ASR::Var_t>(xx.m_target);
                bool create_temp_var_for_rhs = false;
                Vec<ASR::expr_t*> array_vars; array_vars.reserve(al, 1);
                ArrayVarCollector array_var_collector(al, array_vars);
                array_var_collector.visit_expr(*xx.m_value);
                // after collecting variables from RHS, we check whether
                // there is any common variable
                for (size_t i=0; i < array_vars.size(); i++) {
                    ASR::Var_t* v = ASR::down_cast<ASR::Var_t>(array_vars[i]);
                    if (v->m_v == v1->m_v) {
                        create_temp_var_for_rhs = true;
                    }
                }

                if (create_temp_var_for_rhs) {
                    std::string name_hint = "_assignment_";
                    call_create_and_allocate_temporary_variable(xx.m_value)
                    xx.m_value = array_var_temporary;
                }
            }
        }
        CallReplacerOnExpressionsVisitor::visit_Assignment(x);
    }

    void visit_Print(const ASR::Print_t& x) {
        ASR::Print_t& xx = const_cast<ASR::Print_t&>(x);
        std::string name_hint = "print";
        if( is_temporary_needed(xx.m_text) ) {
            visit_expr(*xx.m_text);
            call_create_and_allocate_temporary_variable(xx.m_text);
            xx.m_text = array_var_temporary;
        } else if( ASRUtils::is_struct(*ASRUtils::expr_type(xx.m_text)) &&
                    !ASR::is_a<ASR::Var_t>(
                        *ASRUtils::get_past_array_physical_cast(xx.m_text)) ) {
            visit_expr(*xx.m_text);
            ASR::expr_t* struct_var_temporary = create_and_allocate_temporary_variable_for_struct(
                ASRUtils::get_past_array_physical_cast(xx.m_text), name_hint, al, current_body,
                current_scope, exprs_with_target);
            if( ASR::is_a<ASR::ArrayPhysicalCast_t>(*xx.m_text) ) {
                ASR::ArrayPhysicalCast_t* x_m_values_i = ASR::down_cast<ASR::ArrayPhysicalCast_t>(xx.m_text);
                struct_var_temporary = ASRUtils::EXPR(ASRUtils::make_ArrayPhysicalCast_t_util(
                    al, struct_var_temporary->base.loc, struct_var_temporary,
                    ASRUtils::extract_physical_type(ASRUtils::expr_type(struct_var_temporary)),
                    x_m_values_i->m_new, x_m_values_i->m_type, nullptr));
            }
            xx.m_text = struct_var_temporary;
        } else if( ASR::is_a<ASR::ImpliedDoLoop_t>(*xx.m_text) ) {
            ASR::ImpliedDoLoop_t* implied_do_loop = ASR::down_cast<ASR::ImpliedDoLoop_t>(xx.m_text);
            const Location& loc = xx.m_text->base.loc;
            Vec<ASR::expr_t*> array_con_args; array_con_args.reserve(al, 1);
            array_con_args.push_back(al, xx.m_text);
            Vec<ASR::dimension_t> m_dims; m_dims.reserve(al, 1);
            ASRUtils::ASRBuilder builder(al, loc);
            ASR::dimension_t m_dim; m_dim.loc = loc;
            m_dim.m_start = builder.i32(1);
            m_dim.m_length = get_ImpliedDoLoop_size(al, implied_do_loop);
            m_dims.push_back(al, m_dim);
            ASR::ttype_t* type = ASRUtils::make_Array_t_util(al, loc,
                implied_do_loop->m_type, m_dims.p, m_dims.size());
            xx.m_text = ASRUtils::EXPR(ASRUtils::make_ArrayConstructor_t_util(al, loc,
                array_con_args.p, array_con_args.size(), type, ASR::arraystorageType::ColMajor));
        } else {
            visit_expr(*xx.m_text);
        }
        CallReplacerOnExpressionsVisitor::visit_Print(x);
    }

    void visit_FileWrite(const ASR::FileWrite_t& x) {
        ASR::FileWrite_t& xx = const_cast<ASR::FileWrite_t&>(x);
        visit_IO(xx.m_values, xx.n_values, "file_write");
        CallReplacerOnExpressionsVisitor::visit_FileWrite(x);
    }

    void visit_StringFormat(const ASR::StringFormat_t& x) {
        ASR::StringFormat_t& xx = const_cast<ASR::StringFormat_t&>(x);
        visit_IO(xx.m_args, xx.n_args, "string_format");
        CallReplacerOnExpressionsVisitor::visit_StringFormat(x);
    }

    ASR::expr_t* visit_BinOp_expr(ASR::expr_t* expr, const std::string& name_hint) {
        if (ASRUtils::is_array(ASRUtils::expr_type(expr)) &&
            !ASR::is_a<ASR::ArrayBroadcast_t>(*expr) &&
            !ASR::is_a<ASR::Var_t>(*ASRUtils::get_past_array_physical_cast(expr))
        ) {
            visit_expr(*expr);
            call_create_and_allocate_temporary_variable(expr)
            return array_var_temporary;
        } else if( ASRUtils::is_struct(*ASRUtils::expr_type(expr)) &&
                    !ASR::is_a<ASR::Var_t>(
                        *ASRUtils::get_past_array_physical_cast(expr)) ) {
            visit_expr(*expr);
            ASR::expr_t* struct_var_temporary = create_and_allocate_temporary_variable_for_struct(
                ASRUtils::get_past_array_physical_cast(expr), name_hint, al, current_body,
                current_scope, exprs_with_target);
            if( ASR::is_a<ASR::ArrayPhysicalCast_t>(*expr) ) {
                ASR::ArrayPhysicalCast_t* x_m_values_i = ASR::down_cast<ASR::ArrayPhysicalCast_t>(expr);
                struct_var_temporary = ASRUtils::EXPR(ASRUtils::make_ArrayPhysicalCast_t_util(
                    al, struct_var_temporary->base.loc, struct_var_temporary,
                    ASRUtils::extract_physical_type(ASRUtils::expr_type(struct_var_temporary)),
                    x_m_values_i->m_new, x_m_values_i->m_type, nullptr));
            }
            return struct_var_temporary;
        } else {
            return expr;
        }
    }

    template <typename T>
    bool visit_BinOpUtil(T* binop, const std::string& name_hint,
        std::pair<ASR::expr_t*, ASR::expr_t*>& left_right) {
        if( ASRUtils::is_simd_array(binop->m_type) ) {
            return false;
        }
        ASR::expr_t* left = visit_BinOp_expr(binop->m_left, name_hint + "_left_");
        ASR::expr_t* right = visit_BinOp_expr(binop->m_right, name_hint + "_right_");
        left_right = std::make_pair(left, right);
        return true;
    }

    void visit_IntegerBinOp(const ASR::IntegerBinOp_t& x) {
        ASR::IntegerBinOp_t& xx = const_cast<ASR::IntegerBinOp_t&>(x);
        std::pair<ASR::expr_t*, ASR::expr_t*> binop;
        if( !visit_BinOpUtil(&xx, "integer_binop", binop) ) {
            return ;
        }
        xx.m_left = binop.first;
        xx.m_right = binop.second;
        CallReplacerOnExpressionsVisitor::visit_IntegerBinOp(x);
    }

    void visit_RealBinOp(const ASR::RealBinOp_t& x) {
        ASR::RealBinOp_t& xx = const_cast<ASR::RealBinOp_t&>(x);
        std::pair<ASR::expr_t*, ASR::expr_t*> binop;
        if( !visit_BinOpUtil(&xx, "real_binop", binop) ) {
            return ;
        }
        xx.m_left = binop.first;
        xx.m_right = binop.second;
        CallReplacerOnExpressionsVisitor::visit_RealBinOp(x);
    }

    void visit_ComplexBinOp(const ASR::ComplexBinOp_t& x) {
        ASR::ComplexBinOp_t& xx = const_cast<ASR::ComplexBinOp_t&>(x);
        std::pair<ASR::expr_t*, ASR::expr_t*> binop;
        if( !visit_BinOpUtil(&xx, "complex_binop", binop) ) {
            return ;
        }
        xx.m_left = binop.first;
        xx.m_right = binop.second;
    }

    void visit_LogicalBinOp(const ASR::LogicalBinOp_t& x) {
        ASR::LogicalBinOp_t& xx = const_cast<ASR::LogicalBinOp_t&>(x);
        std::pair<ASR::expr_t*, ASR::expr_t*> binop;
        if( !visit_BinOpUtil(&xx, "logical_binop", binop) ) {
            return ;
        }
        xx.m_left = binop.first;
        xx.m_right = binop.second;
        CallReplacerOnExpressionsVisitor::visit_LogicalBinOp(x);
    }

    void visit_RealCompare(const ASR::RealCompare_t& x) {
        ASR::RealCompare_t& xx = const_cast<ASR::RealCompare_t&>(x);
        std::pair<ASR::expr_t*, ASR::expr_t*> binop;
        if( !visit_BinOpUtil(&xx, "real_compare", binop) ) {
            return ;
        }
        xx.m_left = binop.first;
        xx.m_right = binop.second;
        CallReplacerOnExpressionsVisitor::visit_RealCompare(x);
    }

    void visit_IntegerCompare(const ASR::IntegerCompare_t& x) {
        ASR::IntegerCompare_t& xx = const_cast<ASR::IntegerCompare_t&>(x);
        std::pair<ASR::expr_t*, ASR::expr_t*> binop;
        if( !visit_BinOpUtil(&xx, "integer_compare", binop) ) {
            return ;
        }
        xx.m_left = binop.first;
        xx.m_right = binop.second;
        CallReplacerOnExpressionsVisitor::visit_IntegerCompare(x);
    }

    template <typename T>
    void visit_TypeConstructor(const T& x, const std::string& name_hint) {
        Vec<ASR::expr_t*> x_m_args; x_m_args.reserve(al, x.n_args);
        traverse_args(x_m_args, x.m_args, x.n_args, name_hint);
        T& xx = const_cast<T&>(x);
        xx.m_args = x_m_args.p;
        xx.n_args = x_m_args.size();
    }

    void visit_EnumConstructor(const ASR::EnumConstructor_t& x) {
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
        ASR::ArrayConstructor_t& xx = const_cast<ASR::ArrayConstructor_t&>(x);
        xx.m_args = x_m_args.p;
        xx.n_args = x_m_args.size();
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
            ASRUtils::get_intrinsic_subroutine_name(x.m_sub_intrinsic_id));
    }

    void visit_IntrinsicElementalFunction(const ASR::IntrinsicElementalFunction_t& x) {
        visit_IntrinsicCall(x, "_intrinsic_elemental_function_" +
            ASRUtils::get_intrinsic_name(x.m_intrinsic_id));
        ASR::CallReplacerOnExpressionsVisitor<ArgSimplifier>::visit_IntrinsicElementalFunction(x);
    }

    void visit_IntrinsicArrayFunction(const ASR::IntrinsicArrayFunction_t& x) {
        visit_IntrinsicCall(x, "_intrinsic_array_function_" +
            ASRUtils::get_array_intrinsic_name(x.m_arr_intrinsic_id));
        ASR::IntrinsicArrayFunction_t& xx = const_cast<ASR::IntrinsicArrayFunction_t&>(x);
        if( ASRUtils::IntrinsicArrayFunctionRegistry::get_dim_index(
                static_cast<ASRUtils::IntrinsicArrayFunctions>(x.m_arr_intrinsic_id)) == 1 &&
            x.n_args > 1 && ASRUtils::is_array(x.m_type) ) {
            Vec<ASR::dimension_t> dims;
            diag::Diagnostics diags;
            ASRUtils::ArrIntrinsic::fill_dimensions_for_ArrIntrinsic(
                al, ASRUtils::extract_n_dims_from_ttype(x.m_type), x.m_args[0],
                x.m_args[1], diags, !ASRUtils::is_value_constant(x.m_args[1]), dims);
            xx.m_type = ASRUtils::duplicate_type(al, x.m_type, &dims,
                ASR::array_physical_typeType::DescriptorArray, true);
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

    void visit_StructConstructor(const ASR::StructConstructor_t& x) {
        Vec<ASR::call_arg_t> x_m_args; x_m_args.reserve(al, x.n_args);
        traverse_call_args(x_m_args, x.m_args, x.n_args,
            std::string("_struct_type_constructor_") + ASRUtils::symbol_name(x.m_dt_sym));
        ASR::StructConstructor_t& xx = const_cast<ASR::StructConstructor_t&>(x);
        xx.m_args = x_m_args.p;
        xx.n_args = x_m_args.size();
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t& x) {
        visit_Call(x, "_subroutine_call_");
    }

    void visit_FunctionCall(const ASR::FunctionCall_t& x) {
        visit_Call(x, "_function_call_");
        ASR::CallReplacerOnExpressionsVisitor<ArgSimplifier>::visit_FunctionCall(x);
    }

    #define replace_expr_with_temporary_variable(member, name_hint) BEGIN_VAR_CHECK(x.m_##member) \
        visit_expr(*x.m_##member); \
        xx.m_##member = create_and_allocate_temporary_variable_for_array( \
            x.m_##member, name_hint, al, current_body, current_scope, exprs_with_target); \
        END_VAR_CHECK

    void visit_ArrayReshape(const ASR::ArrayReshape_t& x) {
        ASR::ArrayReshape_t& xx = const_cast<ASR::ArrayReshape_t&>(x);
        replace_expr_with_temporary_variable(array, "_array_reshape_array")
    }

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

    void visit_Cast(const ASR::Cast_t& x) {
        ASR::Cast_t& xx = const_cast<ASR::Cast_t&>(x);

        replace_expr_with_temporary_variable(arg, "_cast_")
        CallReplacerOnExpressionsVisitor::visit_Cast(x);
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

    void visit_ArrayBound(const ASR::ArrayBound_t& x) {
        ASR::ArrayBound_t& xx = const_cast<ASR::ArrayBound_t&>(x);

        replace_expr_with_temporary_variable(v, "_array_bound_")
        if (x.m_dim) {
            replace_expr_with_temporary_variable(dim, "_array_bound_dim_")
        }
    }
};

class ReplaceExprWithTemporary: public ASR::BaseExprReplacer<ReplaceExprWithTemporary> {

    private:

    Allocator& al;
    ExprsWithTargetType& exprs_with_target;
    bool realloc_lhs;

    public:

    Vec<ASR::stmt_t*>* current_body;
    SymbolTable* current_scope;
    bool is_assignment_target_array_section;
    bool is_simd_expression;
    ASR::ttype_t* simd_type;

    ReplaceExprWithTemporary(Allocator& al_, ExprsWithTargetType& exprs_with_target_, bool realloc_lhs_) :
        al(al_), exprs_with_target(exprs_with_target_), realloc_lhs(realloc_lhs_), current_scope(nullptr),
        is_assignment_target_array_section(false), is_simd_expression(false), simd_type(nullptr) {}

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

    void replace_ComplexConstructor(ASR::ComplexConstructor_t* x) {
        replace_current_expr("_complex_constructor_")
    }

    void replace_FunctionCall(ASR::FunctionCall_t* x) {
        if( PassUtils::is_elemental(x->m_name) ) {
            return ;
        }

        if( is_current_expr_linked_to_target && ASRUtils::is_array(x->m_type) ) {
            targetType target_Type = exprs_with_target[*current_expr].second;
            if( target_Type == targetType::OriginalTarget ) {
                force_replace_current_expr_for_array(std::string("_function_call_") +
                                           ASRUtils::symbol_name(x->m_name))
                return ;
            }
        }

        replace_current_expr(std::string("_function_call_") +
            ASRUtils::symbol_name(x->m_name))
    }

    void replace_IntrinsicArrayFunction(ASR::IntrinsicArrayFunction_t* x) {
        std::string name_hint = std::string("_intrinsic_array_function_") + ASRUtils::get_array_intrinsic_name(x->m_arr_intrinsic_id);
        if (!(is_current_expr_linked_to_target || ASRUtils::is_array(x->m_type))) {
            force_replace_current_expr_for_scalar(name_hint)
        } else {
            replace_current_expr(name_hint)
        }
    }

    void replace_IntrinsicImpureFunction(ASR::IntrinsicImpureFunction_t* x) {
        replace_current_expr(std::string("_intrinsic_impure_function_") +
            ASRUtils::get_impure_intrinsic_name(x->m_impure_intrinsic_id))
    }

    void replace_IntrinsicElementalFunction(ASR::IntrinsicElementalFunction_t* x) {
        replace_current_expr(std::string("_intrinsic_elemental_function_") +
            ASRUtils::get_intrinsic_name(x->m_intrinsic_id))
    }

    void replace_StructConstructor(ASR::StructConstructor_t* x) {
        replace_current_expr("_struct_constructor_")
    }

    void replace_EnumConstructor(ASR::EnumConstructor_t* x) {
        replace_current_expr("_enum_constructor_")
    }

    void replace_UnionConstructor(ASR::UnionTypeConstructor_t* x) {
        replace_current_expr("_union_constructor_")
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

    void replace_IntegerBinOp(ASR::IntegerBinOp_t* x) {
        replace_current_expr("_integer_binop_")
    }

    void replace_RealBinOp(ASR::RealBinOp_t* x) {
        if( ASRUtils::is_simd_array(x->m_type) ) {
            ASR::BaseExprReplacer<ReplaceExprWithTemporary>::replace_RealBinOp(x);
            return ;
        }
        replace_current_expr("_real_binop_")
    }

    void replace_IntegerCompare(ASR::IntegerCompare_t* x) {
        replace_current_expr("_integer_compare_")
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

    void replace_ArrayConstructor(ASR::ArrayConstructor_t* x) {
        replace_current_expr("_array_constructor_")
    }

    void replace_ArrayConstant(ASR::ArrayConstant_t* /*x*/) {
        // assign a temporary variable only when either
        // (a). there is no target, e.g. size([1, 2, 3])
        // (b). there is an OriginalTarget and realloc_lhs is true e.g. `x = [1, 2, 3, 4]`
        if (exprs_with_target.find(*current_expr) == exprs_with_target.end() ||
            (exprs_with_target[*current_expr].second == targetType::OriginalTarget && realloc_lhs)) {
            force_replace_current_expr_for_array("_array_constant_")
        }
    }

    void replace_ArraySection(ASR::ArraySection_t* x) {
        #define generate_associate_for_array_section size_t value_n_dims = \
            ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(*current_expr)); \
            ASR::ttype_t* tmp_type = create_array_type_with_empty_dims( \
                al, value_n_dims, ASRUtils::expr_type(*current_expr)); \
            tmp_type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc, tmp_type)); \
            ASR::expr_t* array_expr_ptr = create_temporary_variable_for_array( \
                al, loc, current_scope, "_array_section_pointer_", tmp_type); \
            current_body->push_back(al, ASRUtils::STMT(ASR::make_Associate_t( \
                al, loc, array_expr_ptr, *current_expr))); \
            *current_expr = array_expr_ptr;

        const Location& loc = x->base.base.loc;
        if( is_simd_expression ) {
            if( is_current_expr_linked_to_target ) {
                return ;
            }

            generate_associate_for_array_section

            array_expr_ptr = create_temporary_variable_for_array(
                al, loc, current_scope, "_array_section_copy_", simd_type);
            current_body->push_back(al, ASRUtils::STMT(ASR::make_Assignment_t(
                    al, loc, array_expr_ptr, *current_expr, nullptr)));
            *current_expr = array_expr_ptr;
            return ;
        }

        if( exprs_with_target.find(*current_expr) != exprs_with_target.end() ) {
            generate_associate_for_array_section
            return ;
        }

        replace_current_expr("_array_section_")
    }

    void replace_ArrayTranspose(ASR::ArrayTranspose_t* x) {
        replace_current_expr("_array_transpose_")
    }

    void replace_ArrayPack(ASR::ArrayPack_t* x) {
        replace_current_expr("_array_pack_")
    }

    void replace_ArrayReshape(ASR::ArrayReshape_t* x) {
        replace_current_expr("_array_reshape_")
    }

    void replace_ArrayBroadcast(ASR::ArrayBroadcast_t* x) {
        ASR::expr_t** current_expr_copy_161 = current_expr;
        current_expr = &(x->m_array);
        replace_expr(x->m_array);
        current_expr = current_expr_copy_161;
        replace_ttype(x->m_type);
        if (call_replacer_on_value) {
            ASR::expr_t** current_expr_copy_163 = current_expr;
            current_expr = &(x->m_value);
            replace_expr(x->m_value);
            current_expr = current_expr_copy_163;
        }
    }

    void replace_ArrayItem(ASR::ArrayItem_t* x) {
        if( ASR::is_a<ASR::StructInstanceMember_t>(*x->m_v) ) {
            return ;
        }
        ASR::BaseExprReplacer<ReplaceExprWithTemporary>::replace_ArrayItem(x);
    }

    void replace_StructStaticMember(ASR::StructStaticMember_t* x) {
        replace_current_expr("_struct_static_member_")
    }

    void replace_EnumStaticMember(ASR::EnumStaticMember_t* x) {
        replace_current_expr("_enum_static_member_")
    }

    void replace_UnionInstanceMember(ASR::UnionInstanceMember_t* x) {
        replace_current_expr("_union_instance_member_")
    }

    void replace_OverloadedCompare(ASR::OverloadedCompare_t* x) {
        replace_current_expr("_overloaded_compare_")
    }

    template <typename T>
    void replace_OverloadedOperator(T* x) {
        LCOMPILERS_ASSERT(x->m_overloaded);
        std::pair<ASR::expr_t*, targetType> target_Info =
            std::make_pair(nullptr, targetType::GeneratedTarget);
        if( exprs_with_target.find(*current_expr) != exprs_with_target.end() ) {
            target_Info = exprs_with_target[*current_expr];
        }
        *current_expr = x->m_overloaded;
        if( target_Info.first != nullptr ) {
            exprs_with_target[*current_expr] = target_Info;
        }
        ASR::BaseExprReplacer<ReplaceExprWithTemporary>::replace_expr(*current_expr);
    }

    void replace_OverloadedBinOp(ASR::OverloadedBinOp_t* x) {
        replace_OverloadedOperator(x);
    }

    void replace_OverloadedUnaryMinus(ASR::OverloadedUnaryMinus_t* x) {
        replace_OverloadedOperator(x);
    }

    void replace_OverloadedStringConcat(ASR::OverloadedStringConcat_t* x) {
        replace_OverloadedOperator(x);
    }

    void replace_ComplexRe(ASR::ComplexRe_t* x) {
        replace_current_expr("_complex_re_")
    }

    void replace_ComplexIm(ASR::ComplexIm_t* x) {
        replace_current_expr("_complex_im_")
    }

    void replace_ListSection(ASR::ListSection_t* x) {
        replace_current_expr("_list_section_")
    }

    void replace_ListRepeat(ASR::ListRepeat_t* x) {
        replace_current_expr("_list_repeat_")
    }

    void replace_DictPop(ASR::DictPop_t* x) {
        replace_current_expr("_dict_pop_")
    }

    void replace_SetPop(ASR::SetPop_t* x) {
        replace_current_expr("_set_pop_")
    }

    void replace_RealSqrt(ASR::RealSqrt_t* x) {
        replace_current_expr("_real_sqrt_")
    }

    void replace_ArrayBound(ASR::ArrayBound_t* x) {
        replace_current_expr("_array_bound_")
    }
};

class ReplaceExprWithTemporaryVisitor:
    public ASR::CallReplacerOnExpressionsVisitor<ReplaceExprWithTemporaryVisitor> {

    private:

    Allocator& al;
    ExprsWithTargetType& exprs_with_target;
    Vec<ASR::stmt_t*>* current_body;
    ReplaceExprWithTemporary replacer;

    public:

    ReplaceExprWithTemporaryVisitor(Allocator& al_, ExprsWithTargetType& exprs_with_target_, bool realloc_lhs_):
        al(al_), exprs_with_target(exprs_with_target_), replacer(al, exprs_with_target, realloc_lhs_) {
        replacer.call_replacer_on_value = false;
        call_replacer_on_value = false;
    }

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.current_body = current_body;
        replacer.current_scope = current_scope;
        replacer.replace_expr(*current_expr);
    }

    void visit_Variable(const ASR::Variable_t& /*x*/) {
        // Do nothing
    }

    void visit_FunctionType(const ASR::FunctionType_t& /*x*/) {
        // Do nothing
    }

    void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
        transform_stmts_impl
    }

    void visit_ArrayBroadcast(const ASR::ArrayBroadcast_t &x) {
        ASR::expr_t** current_expr_copy_273 = current_expr;
        current_expr = const_cast<ASR::expr_t**>(&(x.m_array));
        call_replacer();
        current_expr = current_expr_copy_273;
        if( x.m_array )
        visit_expr(*x.m_array);
        visit_ttype(*x.m_type);
        if (x.m_value) {
            if (call_replacer_on_value) {
                ASR::expr_t** current_expr_copy_275 = current_expr;
                current_expr = const_cast<ASR::expr_t**>(&(x.m_value));
                call_replacer();
                current_expr = current_expr_copy_275;
            }
            if( x.m_value ) {
                visit_expr(*x.m_value);
            }
        }
    }

    void visit_ArrayItem(const ASR::ArrayItem_t& x) {
        if( ASR::is_a<ASR::StructInstanceMember_t>(*x.m_v) ) {
            return ;
        }
        ASR::CallReplacerOnExpressionsVisitor<ReplaceExprWithTemporaryVisitor>::visit_ArrayItem(x);
    }

    void visit_Assignment(const ASR::Assignment_t &x) {
        if( ASR::is_a<ASR::ArraySection_t>(*x.m_target) ) {
            bool is_assignment_target_array_section = replacer.is_assignment_target_array_section;
            replacer.is_assignment_target_array_section = true;
            ASR::expr_t** current_expr_copy_8 = current_expr;
            current_expr = const_cast<ASR::expr_t**>(&(x.m_target));
            call_replacer();
            current_expr = current_expr_copy_8;
            replacer.is_assignment_target_array_section = is_assignment_target_array_section;
        }
        ASR::expr_t** current_expr_copy_9 = current_expr;
        bool is_simd_expr_copy = replacer.is_simd_expression;
        ASR::ttype_t* simd_type_copy = replacer.simd_type;
        replacer.is_simd_expression = ASRUtils::is_simd_array(x.m_value);
        replacer.simd_type = ASRUtils::expr_type(x.m_value);
        current_expr = const_cast<ASR::expr_t**>(&(x.m_value));
        call_replacer();
        current_expr = current_expr_copy_9;
        replacer.is_simd_expression = is_simd_expr_copy;
        replacer.simd_type = simd_type_copy;
        if( !ASRUtils::is_simd_array(x.m_value) ) {
            visit_expr(*x.m_value);
        }
        if (x.m_overloaded) {
            visit_stmt(*x.m_overloaded);
        }
    }

    void visit_Associate(const ASR::Associate_t& /*x*/) {
        return ;
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

class ReplaceModuleVarWithValue:
    public ASR::BaseExprReplacer<ReplaceModuleVarWithValue> {

    private:

    Allocator& al;

    public:

    ReplaceModuleVarWithValue(Allocator& al_): al(al_) {}

    void replace_Var(ASR::Var_t* x) {
        if( !ASR::is_a<ASR::Variable_t>(
                *ASRUtils::symbol_get_past_external(x->m_v)) ) {
            return ;
        }

        ASR::Variable_t* y = ASR::down_cast<ASR::Variable_t>(
            ASRUtils::symbol_get_past_external(x->m_v));
        if( !((check_if_ASR_owner_is_module(y->m_parent_symtab->asr_owner)) &&
              y->m_storage == ASR::storage_typeType::Parameter) ||
            y->m_symbolic_value == nullptr ) {
            return ;
        }

        ASRUtils::ExprStmtDuplicator expr_duplicator(al);
        ASR::expr_t* value = nullptr;
        if (y->m_value) {
            value = y->m_value;
        } else {
            value = y->m_symbolic_value;
        }
        *current_expr = expr_duplicator.duplicate_expr(value);
        replace_expr(*current_expr);
    }

};

class TransformVariableInitialiser:
    public ASR::CallReplacerOnExpressionsVisitor<TransformVariableInitialiser> {

    private:

    Allocator& al;
    ExprsWithTargetType& exprs_with_target;
    std::map<SymbolTable*, Vec<ASR::stmt_t*>> symtab2decls;
    ReplaceModuleVarWithValue replacer;

    public:

    TransformVariableInitialiser(Allocator& al_, ExprsWithTargetType& exprs_with_target_): al(al_),
        exprs_with_target(exprs_with_target_), replacer(al_) {}

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }

    void visit_Variable(const ASR::Variable_t &x) {
        ASR::expr_t* value = x.m_value ? x.m_value : x.m_symbolic_value;
        // TODO: StructType expressions aren't evaluated at compile time
        // currently, see: https://github.com/lfortran/lfortran/issues/4909
        if ((check_if_ASR_owner_is_module(x.m_parent_symtab->asr_owner)) ||
            (check_if_ASR_owner_is_enum(x.m_parent_symtab->asr_owner)) ||
            (check_if_ASR_owner_is_struct(x.m_parent_symtab->asr_owner)) ||
            ( x.m_storage == ASR::storage_typeType::Parameter &&
                // this condition ensures that currently constants
                // not evaluated at compile time like
                // real(4), parameter :: z(1) = [x % y]
                // are converted to an assignment for now
                ASRUtils::is_value_constant(value) &&
                !ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::type_get_past_array_pointer_allocatable(ASRUtils::expr_type(value))
                )
            )
        ) {
            return;
        }

        const Location& loc = x.base.base.loc;
        for( size_t i = 0; i < x.n_dependencies; i++ ) {
            std::string dep_name = x.m_dependencies[i];
            visit_symbol(*(current_scope->resolve_symbol(dep_name)));
        }

        ASR::Variable_t& xx = const_cast<ASR::Variable_t&>(x);
        if (value) {
            if( symtab2decls.find(x.m_parent_symtab) == symtab2decls.end() ) {
                Vec<ASR::stmt_t*> result_vec; result_vec.reserve(al, 1);
                symtab2decls[x.m_parent_symtab] = result_vec;
            }
            Vec<ASR::stmt_t*>& result_vec = symtab2decls[x.m_parent_symtab];
            ASR::expr_t* target = ASRUtils::EXPR(ASR::make_Var_t(al, loc, &(xx.base)));

            // if `m_value` is present, then use that for converting it into
            // assignment/association below, otherwise use `m_symbolic_value`
            // for the same. As `m_value` is usually more "simplified" than
            // `m_symbolic_value`
            ASR::expr_t* value = nullptr;
            if (xx.m_value) {
                value = xx.m_value;
            } else {
                value = xx.m_symbolic_value;
            }

            exprs_with_target[value] = std::make_pair(target, targetType::OriginalTarget);
            if (ASRUtils::is_pointer(x.m_type)) {
                result_vec.push_back(al, ASRUtils::STMT(ASR::make_Associate_t(
                    al, loc, target, value)));
            } else {
                result_vec.push_back(al, ASRUtils::STMT(make_Assignment_t_util(
                    al, loc, target, value, nullptr, exprs_with_target)));
            }
            xx.m_symbolic_value = nullptr;
            xx.m_value = nullptr;
        }
    }

    void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
        Vec<ASR::stmt_t*> body;
        body.reserve(al, n_body);

        if( symtab2decls.find(current_scope) != symtab2decls.end() ) {
            Vec<ASR::stmt_t*>& decls = symtab2decls[current_scope];
            for (size_t j = 0; j < decls.size(); j++) {
                body.push_back(al, decls[j]);
            }
            symtab2decls.erase(current_scope);
        }

        for (size_t i = 0; i < n_body; i++) {
            visit_stmt(*m_body[i]);
            body.push_back(al, m_body[i]);
        }
        m_body = body.p;
        n_body = body.size();
    }

    void visit_StructType(const ASR::StructType_t& x) {
        std::string derived_type_name = ASRUtils::symbol_name(x.m_derived_type);
        if( x.m_derived_type == current_scope->resolve_symbol(derived_type_name) ) {
            return ;
        }

        ASR::StructType_t& xx = const_cast<ASR::StructType_t&>(x);
        xx.m_derived_type = current_scope->resolve_symbol(derived_type_name);
    }

};

class CheckNodeTypesInExpr: public ASR::BaseWalkVisitor<CheckNodeTypesInExpr> {
    private:

    Vec<ASR::exprType>& nodes;

    public:

    bool is_node_incorrect;
    CheckNodeTypesInExpr(Vec<ASR::exprType>& nodes_):
        nodes(nodes_), is_node_incorrect(false) {}

    void visit_expr(const ASR::expr_t& e) {
        if( is_node_incorrect ) {
            return;
        }
        bool is_node_correct = false;
        for( size_t i = 0; i < nodes.size(); i++ ) {
            if( e.type == nodes[i] ) {
                if( e.type == ASR::exprType::FunctionCall ) {
                    ASR::FunctionCall_t* func_call = ASR::down_cast<ASR::FunctionCall_t>(&(e));
                    if( !ASRUtils::is_array(func_call->m_type) ) {
                        is_node_correct = true;
                    }
                } else if( e.type == ASR::exprType::IntrinsicElementalFunction ) {
                    ASR::IntrinsicElementalFunction_t* elem_func = ASR::down_cast<ASR::IntrinsicElementalFunction_t>(&(e));
                    if( !ASRUtils::is_array(elem_func->m_type) ) {
                        is_node_correct = true;
                    }
                } else {
                    is_node_correct = true;
                }
                break;
            }
        }
        is_node_incorrect = is_node_incorrect || !is_node_correct;
        ASR::BaseWalkVisitor<CheckNodeTypesInExpr>::visit_expr(e);
    }

};

class VerifySimplifierASROutput:
    public ASR::BaseWalkVisitor<VerifySimplifierASROutput> {

    private:

    Allocator& al;
    ExprsWithTargetType& exprs_with_target;

    public:

    VerifySimplifierASROutput(Allocator& al_, ExprsWithTargetType& exprs_with_target_) :
        al(al_), exprs_with_target(exprs_with_target_) {
        visit_compile_time_value = false;
        (void)exprs_with_target; // explicitly reference to avoid unused warning
    }

    void visit_ArrayBroadcast(const ASR::ArrayBroadcast_t &x) {
        visit_expr(*x.m_array);
        visit_ttype(*x.m_type);
        if (x.m_value && visit_compile_time_value) {
            visit_expr(*x.m_value);
        }
    }

    void visit_Assignment(const ASR::Assignment_t& x) {
        if( !ASRUtils::is_simd_array(x.m_value) ) {
            LCOMPILERS_ASSERT(!ASR::is_a<ASR::ArrayPhysicalCast_t>(*x.m_value));
        }
        if( ASR::is_a<ASR::ArraySection_t>(*x.m_target) ) {
            visit_expr(*x.m_target);
        }
        visit_expr(*x.m_value);
        if (x.m_overloaded) {
            visit_stmt(*x.m_overloaded);
        }
    }

    void visit_FunctionType(const ASR::FunctionType_t& /*x*/) {
        // Do nothing
    }

    void visit_Associate(const ASR::Associate_t& /*x*/) {
        return ;
    }

    #define check_for_var_if_array(expr) if( is_temporary_needed(expr) ) { \
            LCOMPILERS_ASSERT(ASR::is_a<ASR::Var_t>(*ASRUtils::get_past_array_physical_cast(expr))); \
        } \

    #define check_if_linked_to_target(expr, type) if( ASRUtils::is_aggregate_type(type) \
        && ASRUtils::is_simd_array(type) ) { \
         LCOMPILERS_ASSERT( exprs_with_target.find(&(const_cast<ASR::expr_t&>(expr))) != \
                            exprs_with_target.end()); \
    }

    template <typename T>
    void visit_IO(const T& x) {
        for( size_t i = 0; i < x.n_values; i++ ) {
            check_for_var_if_array(x.m_values[i]);
        }
    }

    void visit_Print(const ASR::Print_t& x) {
        check_for_var_if_array(x.m_text)
    }

    void visit_FileWrite(const ASR::FileWrite_t& x) {
        visit_IO(x);
    }

    void traverse_call_args(ASR::call_arg_t* m_args, size_t n_args) {
        for( size_t i = 0; i < n_args; i++ ) {
            check_for_var_if_array(m_args[i].m_value);
        }
    }

    void traverse_args(ASR::expr_t** m_args, size_t n_args) {
        for( size_t i = 0; i < n_args; i++ ) {
            check_for_var_if_array(m_args[i]);
        }
    }

    template <typename T>
    void visit_Call(const T& x) {
        traverse_call_args(x.m_args, x.n_args);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t& x) {
        visit_Call(x);
    }

    template <typename T>
    void visit_IntrinsicCall(const T& x) {
        traverse_args(x.m_args, x.n_args);
    }

    void visit_IntrinsicImpureSubroutine(const ASR::IntrinsicImpureSubroutine_t& x) {
        visit_IntrinsicCall(x);
    }

    void visit_ComplexConstructor(const ASR::ComplexConstructor_t& x) {
        check_for_var_if_array(x.m_re);
        check_for_var_if_array(x.m_im);
        check_if_linked_to_target(x.base, x.m_type);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t& x) {
        visit_Call(x);
        if( !PassUtils::is_elemental(x.m_name) ) {
            check_if_linked_to_target(x.base, x.m_type);
        }
    }

    void visit_IntrinsicElementalFunction(const ASR::IntrinsicElementalFunction_t& x) {
        visit_IntrinsicCall(x);
    }

    void visit_IntrinsicArrayFunction(const ASR::IntrinsicArrayFunction_t& x) {
        visit_IntrinsicCall(x);
        check_if_linked_to_target(x.base, x.m_type);
    }

    void visit_StructConstructor(const ASR::StructConstructor_t& x) {
        traverse_call_args(x.m_args, x.n_args);
        check_if_linked_to_target(x.base, x.m_type);
    }

    void visit_EnumTypeConstructor(const ASR::EnumConstructor_t& x) {
        traverse_args(x.m_args, x.n_args);
        check_if_linked_to_target(x.base, x.m_type);
    }

    void visit_UnionTypeConstructor(const ASR::UnionTypeConstructor_t& x) {
        traverse_args(x.m_args, x.n_args);
        check_if_linked_to_target(x.base, x.m_type);
    }

    void visit_ImpliedDoLoop(const ASR::ImpliedDoLoop_t& x) {
        check_if_linked_to_target(x.base, x.m_type);
    }

    void visit_ListConstant(const ASR::ListConstant_t& x) {
        check_if_linked_to_target(x.base, x.m_type);
    }

    void visit_SetConstant(const ASR::SetConstant_t& x) {
        check_if_linked_to_target(x.base, x.m_type);
    }

    void visit_TupleConstant(const ASR::TupleConstant_t& x) {
        check_if_linked_to_target(x.base, x.m_type);
    }

    void visit_StringSection(const ASR::StringSection_t& x) {
        check_if_linked_to_target(x.base, x.m_type);
    }

    void visit_DictConstant(const ASR::DictConstant_t& x) {
        check_if_linked_to_target(x.base, x.m_type);
    }

    void visit_ArrayConstant(const ASR::ArrayConstant_t& x) {
        check_if_linked_to_target(x.base, x.m_type);
    }

    void visit_ArraySection(const ASR::ArraySection_t& x) {
        check_if_linked_to_target(x.base, x.m_type);
    }

    void visit_ArrayReshape(const ASR::ArrayReshape_t& x) {
        check_if_linked_to_target(x.base, x.m_type);
    }

    void visit_ArrayConstructor(const ASR::ArrayConstructor_t& x) {
        traverse_args(x.m_args, x.n_args);
    }

    void visit_ArrayTranspose(const ASR::ArrayTranspose_t& x) {
        check_for_var_if_array(x.m_matrix);
    }

    void visit_ArrayPack(const ASR::ArrayPack_t& x) {
        check_for_var_if_array(x.m_array);
        check_for_var_if_array(x.m_mask);
        check_for_var_if_array(x.m_vector);
    }

    void visit_ArrayItem(const ASR::ArrayItem_t& x) {
        if( ASR::is_a<ASR::StructInstanceMember_t>(*x.m_v) ) {
            return ;
        }
        ASR::BaseWalkVisitor<VerifySimplifierASROutput>::visit_ArrayItem(x);
    }

    void visit_StructStaticMember(const ASR::StructStaticMember_t& x) {
        check_if_linked_to_target(x.base, x.m_type);
    }

    void visit_EnumStaticMember(const ASR::EnumStaticMember_t& x) {
        check_if_linked_to_target(x.base, x.m_type);
    }

    void visit_UnionInstanceMember(const ASR::UnionInstanceMember_t& x) {
        check_if_linked_to_target(x.base, x.m_type);
    }

    void visit_Cast(const ASR::Cast_t& x) {
        check_for_var_if_array(x.m_arg);
    }

    void visit_OverloadedCompare(const ASR::OverloadedCompare_t& x) {
        check_if_linked_to_target(x.base, x.m_type);
    }

    void visit_OverloadedBinOp(const ASR::OverloadedBinOp_t& x) {
        check_if_linked_to_target(x.base, x.m_type);
    }

    void visit_OverloadedUnaryMinus(const ASR::OverloadedUnaryMinus_t& x) {
        check_if_linked_to_target(x.base, x.m_type);
    }

    void visit_OverloadedStringConcat(const ASR::OverloadedStringConcat_t& x) {
        check_if_linked_to_target(x.base, x.m_type);
    }

    void visit_ComplexRe(const ASR::ComplexRe_t& x) {
        check_for_var_if_array(x.m_arg);
        check_if_linked_to_target(x.base, x.m_type);
    }

    void visit_ComplexIm(const ASR::ComplexIm_t& x) {
        check_for_var_if_array(x.m_arg);
        check_if_linked_to_target(x.base, x.m_type);
    }

    void visit_ListSection(const ASR::ListSection_t& x) {
        check_if_linked_to_target(x.base, x.m_type);
    }

    void visit_ListRepeat(const ASR::ListRepeat_t& x) {
        check_if_linked_to_target(x.base, x.m_type);
    }

    void visit_DictPop(const ASR::DictPop_t& x) {
        check_if_linked_to_target(x.base, x.m_type);
    }

    void visit_SetPop(const ASR::SetPop_t& x) {
        check_if_linked_to_target(x.base, x.m_type);
    }

    void visit_RealSqrt(const ASR::RealSqrt_t& x) {
        check_for_var_if_array(x.m_arg);
        check_if_linked_to_target(x.base, x.m_type);
    }

    void visit_ArrayBound(const ASR::ArrayBound_t& x) {
        check_for_var_if_array(x.m_v);
        check_for_var_if_array(x.m_dim);

    }

    void visit_Variable(const ASR::Variable_t& x) {
        if( (ASRUtils::is_array(x.m_type) ||
            ASRUtils::is_aggregate_type(x.m_type)) &&
            !(check_if_ASR_owner_is_module(x.m_parent_symtab->asr_owner)) &&
            !(check_if_ASR_owner_is_enum(x.m_parent_symtab->asr_owner)) &&
            !(check_if_ASR_owner_is_struct(x.m_parent_symtab->asr_owner)) &&
            x.m_storage != ASR::storage_typeType::Parameter ) {
            LCOMPILERS_ASSERT(x.m_symbolic_value == nullptr);
            LCOMPILERS_ASSERT(x.m_value == nullptr);
        }
    }

    void visit_Allocate(const ASR::Allocate_t& x) {
        for( size_t i = 0; i < x.n_args; i++ ) {
            for( size_t j = 0; j < x.m_args[i].n_dims; j++ ) {
                ASR::dimension_t& alloc_dim = x.m_args[i].m_dims[j];
                LCOMPILERS_ASSERT(alloc_dim.m_length);
                Vec<ASR::exprType> vec;
                vec.reserve(al, 2);
                vec.push_back(al, ASR::exprType::Var);
                vec.push_back(al, ASR::exprType::FunctionCall);
                vec.push_back(al, ASR::exprType::IntrinsicElementalFunction);
                CheckNodeTypesInExpr check(vec);
                check.visit_expr(*alloc_dim.m_length);
                if( alloc_dim.m_start != nullptr ) {
                    check.visit_expr(*alloc_dim.m_start);
                }
            }
        }
    }

};

class InitialiseExprWithTarget: public ASR::BaseWalkVisitor<InitialiseExprWithTarget> {
    private:

    ExprsWithTargetType& exprs_with_target;

    public:

    InitialiseExprWithTarget(ExprsWithTargetType& exprs_with_target_):
        exprs_with_target(exprs_with_target_) {}

    void visit_Assignment(const ASR::Assignment_t& x) {
        exprs_with_target[x.m_value] = std::make_pair(const_cast<ASR::expr_t*>(x.m_target), targetType::OriginalTarget);
    }

};

void pass_simplifier(Allocator &al, ASR::TranslationUnit_t &unit,
                     const PassOptions &pass_options) {
    // TODO: Add a visitor in asdl_cpp.py which will replace
    // current_expr with its own `m_value` (if `m_value` is not nullptr)
    // Call the visitor here.
    ExprsWithTargetType exprs_with_target;
    InitialiseExprWithTarget init_expr_with_target(exprs_with_target);
    init_expr_with_target.visit_TranslationUnit(unit);
    TransformVariableInitialiser a(al, exprs_with_target);
    a.visit_TranslationUnit(unit);
    ArgSimplifier b(al, exprs_with_target, pass_options.realloc_lhs);
    b.visit_TranslationUnit(unit);
    ReplaceExprWithTemporaryVisitor c(al, exprs_with_target, pass_options.realloc_lhs);
    c.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor d(al);
    d.visit_TranslationUnit(unit);
    #if defined(WITH_LFORTRAN_ASSERT)
    VerifySimplifierASROutput e(al, exprs_with_target);
    e.visit_TranslationUnit(unit);
    #endif
}


} // namespace LCompilers
