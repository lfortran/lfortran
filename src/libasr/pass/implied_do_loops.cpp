#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/replace_implied_do_loops.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/intrinsic_function_registry.h>


namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

class ReplaceArrayConstant: public ASR::BaseExprReplacer<ReplaceArrayConstant> {

    public:

    Allocator& al;
    Vec<ASR::stmt_t*>& pass_result;
    bool& remove_original_statement;

    SymbolTable* current_scope;
    ASR::expr_t* result_var;
    int result_counter;
    std::map<ASR::expr_t*, ASR::expr_t*>& resultvar2value;
    bool realloc_lhs, allocate_target;
    const LCompilers::PassOptions& pass_options;

    int get_index_kind() const {
        return pass_options.descriptor_index_64 ? 8 : 4;
    }

    ASR::expr_t* get_first_scalar_expr(ASR::expr_t* expr) {
        // Visitor to replace loop variable with its starting value in an expression
        class ReplaceLoopVarWithStart : public ASR::BaseExprReplacer<ReplaceLoopVarWithStart> {
        public:
            Allocator& al;
            ASR::symbol_t* loop_var_sym;
            ASR::expr_t* start_value;

            ReplaceLoopVarWithStart(Allocator& al_, ASR::symbol_t* loop_var_sym_, ASR::expr_t* start_value_)
                : al(al_), loop_var_sym(loop_var_sym_), start_value(start_value_) {}

            void replace_Var(ASR::Var_t* x) {
                if (x->m_v == loop_var_sym) {
                    *current_expr = start_value;
                }
            }
        };
        if( expr == nullptr ) {
            return nullptr;
        }
        if( ASR::is_a<ASR::ImpliedDoLoop_t>(*expr) ) {
            ASR::ImpliedDoLoop_t* idl = ASR::down_cast<ASR::ImpliedDoLoop_t>(expr);
            for( size_t i = 0; i < idl->n_values; i++ ) {
                // Replace loop variable with its starting value in the expression
                ASR::expr_t* value_expr = idl->m_values[i];
                
                // Get the loop variable symbol
                if (ASR::is_a<ASR::Var_t>(*idl->m_var)) {
                    ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(idl->m_var);
                    ASR::symbol_t* loop_var_sym = var->m_v;
                    
                    // Create a copy of the expression and replace the loop var with start value
                    value_expr = ASRUtils::expr_value(value_expr);
                    if (value_expr == nullptr) {
                        value_expr = idl->m_values[i];
                    }

                    ASRUtils::ExprStmtDuplicator duplicator(al);
                    value_expr = duplicator.duplicate_expr(value_expr);
                    
                    ReplaceLoopVarWithStart replacer(al, loop_var_sym, idl->m_start);
                    replacer.current_expr = &value_expr;
                    replacer.replace_expr(value_expr);
                }
                
                ASR::expr_t* candidate = get_first_scalar_expr(value_expr);
                if( candidate != nullptr ) {
                    return candidate;
                }
            }
            return nullptr;
        } else if( ASR::is_a<ASR::ArrayConstructor_t>(*expr) ) {
            ASR::ArrayConstructor_t* arr_cons = ASR::down_cast<ASR::ArrayConstructor_t>(expr);
            for( size_t i = 0; i < arr_cons->n_args; i++ ) {
                ASR::expr_t* candidate = get_first_scalar_expr(arr_cons->m_args[i]);
                if( candidate != nullptr ) {
                    return candidate;
                }
            }
            return nullptr;
        } else if( ASR::is_a<ASR::ArrayConstant_t>(*expr) ) {
            ASR::ArrayConstant_t* arr_const = ASR::down_cast<ASR::ArrayConstant_t>(expr);
            int64_t array_size = ASRUtils::get_fixed_size_of_array(arr_const->m_type);
            if( array_size <= 0 ) {
                return nullptr;
            }
            return ASRUtils::fetch_ArrayConstant_value(al, arr_const, 0);
        }
        return expr;
    }

    ASR::expr_t* get_string_source_len(ASR::expr_t* expr) {
        // Derive a conservative element length without evaluating indices (e.g., trim(lines(iii)) uses declared length).
        if (!expr) return nullptr;
        if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*expr)) {
            ASR::IntrinsicElementalFunction_t* f = ASR::down_cast<ASR::IntrinsicElementalFunction_t>(expr);
            if (f->m_intrinsic_id == static_cast<int64_t>(ASRUtils::IntrinsicElementalFunctions::StringTrim) && f->n_args >= 1) {
                return get_string_source_len(f->m_args[0]);
            } else if (f->m_intrinsic_id == static_cast<int64_t>(ASRUtils::IntrinsicElementalFunctions::StringConcat) && f->n_args > 1) {
                ASR::expr_t* total_len = nullptr;
                for (size_t i = 0; i < f->n_args; i++) {
                    ASR::expr_t* arg_len = get_string_source_len(f->m_args[i]);
                    if (arg_len == nullptr) {
                        return nullptr;
                    }
                    if (total_len == nullptr) {
                        total_len = arg_len;
                    } else {
                        ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, expr->base.loc, 4));
                        total_len = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, expr->base.loc,
                            total_len, ASR::binopType::Add, arg_len,
                            int_type, nullptr));
                    }
                }
                return total_len;
            }
        }

        ASR::ttype_t* t = ASRUtils::expr_type(expr);
        t = ASRUtils::extract_type(t);
        if (ASR::is_a<ASR::String_t>(*t)) {
            ASR::String_t* st = ASR::down_cast<ASR::String_t>(t);
            if (st->m_len && ASRUtils::is_value_constant(st->m_len)) {
                return st->m_len;
            } else {
                return ASRUtils::EXPR(ASR::make_StringLen_t(al, expr->base.loc,
                    expr, ASRUtils::TYPE(ASR::make_Integer_t(al, expr->base.loc, 4)), nullptr));
            }
        }
        return nullptr;
    }

    ReplaceArrayConstant(Allocator& al_, Vec<ASR::stmt_t*>& pass_result_,
        bool& remove_original_statement_,
        std::map<ASR::expr_t*, ASR::expr_t*>& resultvar2value_,
        bool realloc_lhs_, bool allocate_target_,
        const LCompilers::PassOptions& pass_options_) :
    al(al_), pass_result(pass_result_),
    remove_original_statement(remove_original_statement_),
    current_scope(nullptr),
    result_var(nullptr), result_counter(0),
    resultvar2value(resultvar2value_),
    realloc_lhs(realloc_lhs_), allocate_target(allocate_target_),
    pass_options(pass_options_) {}

    ASR::expr_t* get_ImpliedDoLoop_size(ASR::ImpliedDoLoop_t* implied_doloop) {
        const Location& loc = implied_doloop->base.base.loc;
        ASRUtils::ASRBuilder builder(al, loc);
        ASR::expr_t* start = implied_doloop->m_start;
        ASR::expr_t* end = implied_doloop->m_end;
        ASR::expr_t* d = implied_doloop->m_increment;
        ASR::expr_t* implied_doloop_size = nullptr;
        int kind = ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(end));
        start = builder.i2i_t(start, ASRUtils::expr_type(end));
        if( d == nullptr ) {
            implied_doloop_size = ASRUtils::compute_length_from_start_end(al, start, end);
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
                    implied_doloop_size_ = get_ImpliedDoLoop_size(
                        ASR::down_cast<ASR::ImpliedDoLoop_t>(implied_doloop->m_values[i]));
                } else {
                    implied_doloop_size_ = builder.Add(get_ImpliedDoLoop_size(
                        ASR::down_cast<ASR::ImpliedDoLoop_t>(implied_doloop->m_values[i])),
                        implied_doloop_size_);
                }
            } else {
                const_elements += 1;
            }
        }

        ASR::symbol_t* loop_var_sym = nullptr;
        if (ASR::is_a<ASR::Var_t>(*implied_doloop->m_var)) {
            loop_var_sym = ASR::down_cast<ASR::Var_t>(implied_doloop->m_var)->m_v;
        }
        bool inner_depends_on_var = (loop_var_sym != nullptr &&
            implied_doloop_size_ != nullptr && d == nullptr &&
            ASRUtils::expr_references_symbol(implied_doloop_size_, loop_var_sym));

        if (inner_depends_on_var) {
            ASR::expr_t* two = make_ConstantWithKind(
                make_IntegerConstant_t, make_Integer_t, 2, kind, loc);
            ASR::expr_t* arith_sum = builder.Div(
                builder.Mul(implied_doloop_size, builder.Add(start, end)), two);
            if (const_elements > 0) {
                ASR::expr_t* const_total = builder.Mul(
                    make_ConstantWithKind(make_IntegerConstant_t,
                        make_Integer_t, const_elements, kind, loc),
                    implied_doloop_size);
                implied_doloop_size = builder.Add(arith_sum, const_total);
            } else {
                implied_doloop_size = arith_sum;
            }
        } else {
            if( const_elements > 0 ) {
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
        }
        return implied_doloop_size;
    }

    size_t get_constant_ArrayConstant_size(ASR::ArrayConstant_t* x) {
        return ASRUtils::get_fixed_size_of_array(x->m_type);
    }

    ASR::expr_t* get_ArrayConstructor_size(ASR::ArrayConstructor_t* x, bool& is_allocatable) {
        ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x->base.base.loc, get_index_kind()));
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
                    ASR::expr_t* element_array_size = get_ArrayConstant_size(
                        ASR::down_cast<ASR::ArrayConstant_t>(element));
                    if( array_size == nullptr ) {
                        array_size = element_array_size;
                    } else {
                        array_size = builder.Add(array_size,
                                        element_array_size);
                    }
                }
            } else if( ASR::is_a<ASR::ArrayConstructor_t>(*element) ) {
                ASR::expr_t* element_array_size = get_ArrayConstructor_size(
                    ASR::down_cast<ASR::ArrayConstructor_t>(element), is_allocatable);
                if( array_size == nullptr ) {
                    array_size = element_array_size;
                } else {
                    array_size = builder.Add(array_size,
                                    element_array_size);
                }
            } else if ( ASR::is_a<ASR::ArrayItem_t>(*element) ) {
                ASR::ArrayItem_t* array_item = ASR::down_cast<ASR::ArrayItem_t>(element);
                if ( ASR::is_a<ASR::Array_t>(*array_item->m_type) ) {
                    if ( ASRUtils::is_fixed_size_array(array_item->m_type) ) {
                        ASR::Array_t* array_type = ASR::down_cast<ASR::Array_t>(array_item->m_type);
                        constant_size += ASRUtils::get_fixed_size_of_array(array_type->m_dims, array_type->n_dims);
                    } else {
                        ASR::expr_t* element_array_size = ASRUtils::get_size(element, al, false);
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
                ASR::expr_t* implied_doloop_size = get_ImpliedDoLoop_size(
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
                        make_ConstantWithKind(make_IntegerConstant_t, make_Integer_t, 1, get_index_kind(), loc));
                    if( array_section_size == nullptr ) {
                        array_section_size = dim_size;
                    } else {
                        array_section_size = builder.Mul(array_section_size, dim_size);
                    }
                }
                if( array_size == nullptr ) {
                    array_size = array_section_size;
                } else {
                    array_size = builder.Add(array_section_size, array_size);
                }
            } else if( ASR::is_a<ASR::IntrinsicElementalFunction_t>(*element) &&
                       ASRUtils::is_array(ASRUtils::expr_type(element)) ) {
                ASR::IntrinsicElementalFunction_t* intrinsic_element_t = ASR::down_cast<ASR::IntrinsicElementalFunction_t>(element);
                if( ASRUtils::is_fixed_size_array(intrinsic_element_t->m_type) ) {
                    constant_size += ASRUtils::get_fixed_size_of_array(intrinsic_element_t->m_type);
                } else {
                    ASR::expr_t* element_array_size = ASRUtils::get_size(element, al, false);
                    if( array_size == nullptr ) {
                        array_size = element_array_size;
                    } else {
                        array_size = builder.Add(array_size,
                                        element_array_size);
                    }
                }
            } else if( ASR::is_a<ASR::FunctionCall_t>(*element)  &&
                       ASRUtils::is_array(ASRUtils::expr_type(element)) ) {
                ASR::FunctionCall_t* fc_element_t = ASR::down_cast<ASR::FunctionCall_t>(element);
                if( ASRUtils::is_fixed_size_array(fc_element_t->m_type) ) {
                    constant_size += ASRUtils::get_fixed_size_of_array(fc_element_t->m_type);
                } else {
                    ASR::expr_t* element_array_size = ASRUtils::get_size(element, al, false);
                    if( array_size == nullptr ) {
                        array_size = element_array_size;
                    } else {
                        array_size = builder.Add(array_size,
                                        element_array_size);
                    }
                }
            } else if( ASR::is_a<ASR::IntrinsicArrayFunction_t>(*element)  &&
                       ASRUtils::is_array(ASRUtils::expr_type(element)) ) {
                ASR::IntrinsicArrayFunction_t* intrinsic_element_t = ASR::down_cast<ASR::IntrinsicArrayFunction_t>(element);
                if( ASRUtils::is_fixed_size_array(intrinsic_element_t->m_type) ) {
                    constant_size += ASRUtils::get_fixed_size_of_array(intrinsic_element_t->m_type);
                } else {
                    ASR::expr_t* element_array_size = ASRUtils::get_size(element, al, false);
                    if( array_size == nullptr ) {
                        array_size = element_array_size;
                    } else {
                        array_size = builder.Add(array_size,
                                        element_array_size);
                    }
                }
            } else if( (ASR::is_a<ASR::RealBinOp_t>(*element) || ASR::is_a<ASR::IntegerBinOp_t>(*element) ||
                       ASR::is_a<ASR::RealUnaryMinus_t>(*element) || ASR::is_a<ASR::IntegerUnaryMinus_t>(*element)) &&
                       ASRUtils::is_array(ASRUtils::expr_type(element)) ) {
                ASR::ttype_t* arr_type = ASRUtils::expr_type(element);
                if( ASRUtils::is_fixed_size_array(arr_type) ) {
                    constant_size += ASRUtils::get_fixed_size_of_array(arr_type);
                } else {
                    ASR::expr_t* element_array_size = ASRUtils::get_size(element, al, false);
                    if( array_size == nullptr ) {
                        array_size = element_array_size;
                    } else {
                        array_size = builder.Add(array_size,
                                        element_array_size);
                    }
                }
            } else {
                ASR::ttype_t* element_type = ASRUtils::type_get_past_allocatable(
                    ASRUtils::type_get_past_pointer(ASRUtils::expr_type(element)));
                if( ASRUtils::is_array(element_type) ) {
                    if( ASRUtils::is_fixed_size_array(element_type) ) {
                        constant_size += ASRUtils::get_fixed_size_of_array(element_type);
                    } else {
                        ASR::expr_t* element_array_size = ASRUtils::get_size(element, al, false);
                        if( array_size == nullptr ) {
                            array_size = element_array_size;
                        } else {
                            array_size = builder.Add(array_size, element_array_size);
                        }
                    }
                } else {
                    constant_size += 1;
                }
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
        is_allocatable = true;
        if( array_size == nullptr ) {
            array_size = make_ConstantWithKind(make_IntegerConstant_t,
                make_Integer_t, 0, get_index_kind(), x->base.base.loc);
        }
        return array_size;
    }

    ASR::expr_t* get_ArrayConstant_size(ASR::ArrayConstant_t* x) {
        ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x->base.base.loc, get_index_kind()));
        return make_ConstantWithType(make_IntegerConstant_t,
                ASRUtils::get_fixed_size_of_array(x->m_type), int_type, x->base.base.loc);
    }

   void replace_ArrayConstructor(ASR::ArrayConstructor_t* x) {
        const Location& loc = x->base.base.loc;
        ASR::expr_t* result_var_copy = result_var;
        bool is_result_var_fixed_size = false;
        if (result_var != nullptr &&
            resultvar2value.find(result_var) != resultvar2value.end() &&
            resultvar2value[result_var] == &(x->base)) {
            is_result_var_fixed_size = ASRUtils::is_fixed_size_array(ASRUtils::expr_type(result_var));
        }
        ASR::ttype_t* result_type_ = nullptr;
        bool is_allocatable = false;
        ASR::expr_t* array_constructor = get_ArrayConstructor_size(x, is_allocatable);

        // Case: `keywords = [character(len=ii) :: value]`
        // For runtime-dependent string lengths, defer evaluation and use allocatable results with per-element runtime allocation.
        ASR::ttype_t* element_type = ASRUtils::extract_type(x->m_type);
        ASR::expr_t* declared_len_expr = nullptr;
        if (element_type && ASRUtils::is_character(*element_type)) {
            ASR::String_t* string_type = ASR::down_cast<ASR::String_t>(element_type);
            declared_len_expr = string_type->m_len;
            bool len_is_constant = ASRUtils::is_value_constant(declared_len_expr);
            if( !len_is_constant ) {
                is_allocatable = true;
            }
        }
        Vec<ASR::dimension_t> dims;
        dims.reserve(al, 1);
        ASR::dimension_t dim;
        dim.loc = loc;
        dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 1,
                        ASRUtils::type_get_past_pointer(
                            ASRUtils::type_get_past_allocatable(
                                ASRUtils::expr_type(array_constructor)))));
        dim.m_length = array_constructor;
        dims.push_back(al, dim);
        remove_original_statement = false;
        if( is_result_var_fixed_size ) {
            result_type_ = ASRUtils::expr_type(result_var);
            is_allocatable = false;
        } else {
            if( is_allocatable ) {
                result_type_ = ASRUtils::TYPE(ASR::make_Allocatable_t(al, x->m_type->base.loc,
                    ASRUtils::type_get_past_allocatable_pointer(
                        ASRUtils::duplicate_type_with_empty_dims(al, x->m_type))));
                // Always defer string length for runtime-sized strings
                if (element_type && ASRUtils::is_character(*element_type)) {
                    ASR::down_cast<ASR::String_t>(ASRUtils::extract_type(result_type_))->m_len = nullptr;
                    ASR::down_cast<ASR::String_t>(ASRUtils::extract_type(result_type_))->m_len_kind = ASR::string_length_kindType::DeferredLength;
                }
            } else {
                result_type_ = ASRUtils::duplicate_type(al,
                    ASRUtils::type_get_past_allocatable_pointer(x->m_type), &dims);
                // Keep fixed-size type as is; allocation will handle string length.
            }
        }
        ASR::expr_t* struct_var = nullptr;
        if (x->m_struct_var) {
            struct_var = x->m_struct_var;
        } else if (x->m_args[0]) {
            struct_var = x->m_args[0];
        }
        result_var = PassUtils::create_var(result_counter, "_array_constructor_",
                        loc, result_type_, al, current_scope, struct_var);
        result_counter += 1;
        *current_expr = result_var;

        // Try to derive a safe element string length for allocation
        ASR::expr_t* elem_len_expr_for_alloc = nullptr;
        if (element_type && ASRUtils::is_character(*element_type)) {
            // Default to declared length if available; override with a derived source length when safe.
            elem_len_expr_for_alloc = declared_len_expr;
            if (!declared_len_expr && x->n_args > 0) {
                ASR::expr_t* sample_expr_any = get_first_scalar_expr(x->m_args[0]);
                ASR::expr_t* derived_len = get_string_source_len(sample_expr_any);
                if (derived_len) elem_len_expr_for_alloc = derived_len;
            }
        }

        Vec<ASR::alloc_arg_t> alloc_args;
        alloc_args.reserve(al, 1);
        ASR::alloc_arg_t arg;
        // Prefer declared source length if available; else fall back to previously detected expr
        arg.m_len_expr = elem_len_expr_for_alloc;
        
        // If we're allocating a deferred-length string but have no length expression,
        // we cannot proceed. In this case, revert to non-deferred type.
        if (is_allocatable && element_type && ASRUtils::is_character(*element_type) && 
            !arg.m_len_expr) {
            is_allocatable = false;
            result_type_ = ASRUtils::duplicate_type(al,
                ASRUtils::type_get_past_allocatable(x->m_type), &dims);
        }
        arg.m_type = nullptr;
        arg.m_sym_subclass = nullptr;
        arg.m_dims = dims.p;
        arg.n_dims = dims.size();
        if( is_allocatable ) {
            arg.loc = result_var->base.loc;
            arg.m_a = result_var;
            alloc_args.push_back(al, arg);
            Vec<ASR::expr_t*> to_be_deallocated;
            to_be_deallocated.reserve(al, alloc_args.size());
            for( size_t i = 0; i < alloc_args.size(); i++ ) {
                to_be_deallocated.push_back(al, alloc_args.p[i].m_a);
            }
            pass_result.push_back(al, ASRUtils::STMT(ASR::make_ExplicitDeallocate_t(
                al, loc, to_be_deallocated.p, to_be_deallocated.size())));
            ASR::stmt_t* allocate_stmt = ASRUtils::STMT(ASR::make_Allocate_t(
                al, loc, alloc_args.p, alloc_args.size(), nullptr, nullptr, nullptr));
            pass_result.push_back(al, allocate_stmt);
        }
        if ( allocate_target && realloc_lhs ) {
            allocate_target = false;
            arg.loc = result_var_copy->base.loc;
            arg.m_a = result_var_copy;
            alloc_args.push_back(al, arg);
            Vec<ASR::expr_t*> to_be_deallocated;
            to_be_deallocated.reserve(al, alloc_args.size());
            for( size_t i = 0; i < alloc_args.size(); i++ ) {
                to_be_deallocated.push_back(al, alloc_args.p[i].m_a);
            }
            pass_result.push_back(al, ASRUtils::STMT(ASR::make_ExplicitDeallocate_t(
                al, loc, to_be_deallocated.p, to_be_deallocated.size())));
            ASR::stmt_t* allocate_stmt = ASRUtils::STMT(ASR::make_Allocate_t(
                al, loc, alloc_args.p, alloc_args.size(), nullptr, nullptr, nullptr));
            pass_result.push_back(al, allocate_stmt);
        }
        for (size_t i = 0; i < x->n_args; i++) {
            if(ASR::is_a<ASR::ArrayItem_t>(*x->m_args[i]) || ASR::is_a<ASR::ArraySection_t>(*x->m_args[i])){
                ASR::expr_t** temp = current_expr;
                current_expr = &(x->m_args[i]);
                self().replace_expr(x->m_args[i]);
                current_expr = temp;
            }
        }
        LCOMPILERS_ASSERT(result_var != nullptr);
        Vec<ASR::stmt_t*>* result_vec = &pass_result;
        PassUtils::ReplacerUtils::replace_ArrayConstructor_(al, x, result_var,
            result_vec, current_scope);
        result_var = result_var_copy;
    }

    void replace_ArrayConstant(ASR::ArrayConstant_t* x) {
        const Location& loc = x->base.base.loc;
        ASR::expr_t* result_var_copy = result_var;
        bool is_result_var_fixed_size = false;
        if (result_var != nullptr &&
            resultvar2value.find(result_var) != resultvar2value.end() &&
            resultvar2value[result_var] == &(x->base)) {
            is_result_var_fixed_size = ASRUtils::is_fixed_size_array(ASRUtils::expr_type(result_var));
        }
        ASR::ttype_t* result_type_ = nullptr;
        bool is_allocatable = false;
        ASR::expr_t* array_constant_size = get_ArrayConstant_size(x);
        Vec<ASR::dimension_t> dims;
        dims.reserve(al, 1);
        ASR::dimension_t dim;
        dim.loc = loc;
        dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 1,
                        ASRUtils::type_get_past_pointer(
                            ASRUtils::type_get_past_allocatable(
                                ASRUtils::expr_type(array_constant_size)))));
        dim.m_length = array_constant_size;
        dims.push_back(al, dim);
        remove_original_statement = false;
        if( is_result_var_fixed_size ) {
            result_type_ = ASRUtils::expr_type(result_var);
            is_allocatable = false;
        } else {
            if( is_allocatable ) {
                result_type_ = ASRUtils::TYPE(ASRUtils::make_Allocatable_t_util(al, x->m_type->base.loc,
                    ASRUtils::type_get_past_allocatable(
                        ASRUtils::duplicate_type_with_empty_dims(al, x->m_type))));
            } else {
                result_type_ = ASRUtils::duplicate_type(al,
                    ASRUtils::type_get_past_allocatable(x->m_type), &dims);
            }
        }
        result_var = PassUtils::create_var(result_counter, "_array_constant_",
                        loc, result_type_, al, current_scope);
        result_counter += 1;
        *current_expr = result_var;

        Vec<ASR::alloc_arg_t> alloc_args;
        alloc_args.reserve(al, 1);
        ASR::alloc_arg_t arg;
        arg.m_len_expr = nullptr;
        arg.m_type = nullptr;
        arg.m_sym_subclass = nullptr;
        arg.m_dims = dims.p;
        arg.n_dims = dims.size();
        if( is_allocatable ) {
            arg.loc = result_var->base.loc;
            arg.m_a = result_var;
            alloc_args.push_back(al, arg);
            Vec<ASR::expr_t*> to_be_deallocated;
            to_be_deallocated.reserve(al, alloc_args.size());
            for( size_t i = 0; i < alloc_args.size(); i++ ) {
                to_be_deallocated.push_back(al, alloc_args.p[i].m_a);
            }
            pass_result.push_back(al, ASRUtils::STMT(ASR::make_ExplicitDeallocate_t(
                al, loc, to_be_deallocated.p, to_be_deallocated.size())));
            ASR::stmt_t* allocate_stmt = ASRUtils::STMT(ASR::make_Allocate_t(
                al, loc, alloc_args.p, alloc_args.size(), nullptr, nullptr, nullptr));
            pass_result.push_back(al, allocate_stmt);
        }
        if ( allocate_target && realloc_lhs ) {
            allocate_target = false;
            arg.loc = result_var_copy->base.loc;
            arg.m_a = result_var_copy;
            alloc_args.push_back(al, arg);
            Vec<ASR::expr_t*> to_be_deallocated;
            to_be_deallocated.reserve(al, alloc_args.size());
            for( size_t i = 0; i < alloc_args.size(); i++ ) {
                to_be_deallocated.push_back(al, alloc_args.p[i].m_a);
            }
            pass_result.push_back(al, ASRUtils::STMT(ASR::make_ExplicitDeallocate_t(
                al, loc, to_be_deallocated.p, to_be_deallocated.size())));
            ASR::stmt_t* allocate_stmt = ASRUtils::STMT(ASR::make_Allocate_t(
                al, loc, alloc_args.p, alloc_args.size(), nullptr, nullptr, nullptr));
            pass_result.push_back(al, allocate_stmt);
        }
        LCOMPILERS_ASSERT(result_var != nullptr);
        Vec<ASR::stmt_t*>* result_vec = &pass_result;
        PassUtils::ReplacerUtils::replace_ArrayConstant(x, this,
            remove_original_statement, result_vec);
        result_var = result_var_copy;
    }

    void replace_ArrayPhysicalCast(ASR::ArrayPhysicalCast_t* x) {
        [[maybe_unused]] bool is_arr_construct_arg = ASR::is_a<ASR::ArrayConstructor_t>(*x->m_arg);
        ASR::BaseExprReplacer<ReplaceArrayConstant>::replace_ArrayPhysicalCast(x);
        if( x->m_old != ASRUtils::extract_physical_type(ASRUtils::expr_type(x->m_arg)) ) {
            x->m_old = ASRUtils::extract_physical_type(ASRUtils::expr_type(x->m_arg));
        }
        if( (is_arr_construct_arg && ASRUtils::is_fixed_size_array(ASRUtils::expr_type(x->m_arg))) ){
            *current_expr = x->m_arg;
            return;
        }

        if( (x->m_old == x->m_new &&
             x->m_old != ASR::array_physical_typeType::DescriptorArray) ||
            (x->m_old == x->m_new && x->m_old == ASR::array_physical_typeType::DescriptorArray &&
            (ASR::is_a<ASR::Allocatable_t>(*ASRUtils::expr_type(x->m_arg)) ||
            ASR::is_a<ASR::Pointer_t>(*ASRUtils::expr_type(x->m_arg)))) ||
            x->m_old != ASRUtils::extract_physical_type(ASRUtils::expr_type(x->m_arg)) ) {
            *current_expr = x->m_arg;
        } else {
            x->m_old = ASRUtils::extract_physical_type(ASRUtils::expr_type(x->m_arg));
        }
    }

    void replace_ArrayBroadcast(ASR::ArrayBroadcast_t* x) {
        ASR::expr_t** current_expr_copy_161 = current_expr;
        current_expr = &(x->m_array);
        replace_expr(x->m_array);
        current_expr = current_expr_copy_161;
    }

};

class ArrayConstantVisitor : public ASR::CallReplacerOnExpressionsVisitor<ArrayConstantVisitor>
{
    private:

        Allocator& al;
        bool remove_original_statement, allocate_target = false;
        bool print = false, file_write = false;
        ASR::expr_t* m_unit = nullptr;
        ReplaceArrayConstant replacer;
        Vec<ASR::stmt_t*> pass_result;
        Vec<ASR::stmt_t*>* parent_body;
        std::map<ASR::expr_t*, ASR::expr_t*> resultvar2value;
        const LCompilers::PassOptions& pass_options;

    public:

        int get_index_kind() const {
            return pass_options.descriptor_index_64 ? 8 : 4;
        }

        ArrayConstantVisitor(Allocator& al_, const LCompilers::PassOptions& pass_options_) :
        al(al_), remove_original_statement(false),
        replacer(al_, pass_result, remove_original_statement,
            resultvar2value, pass_options_.realloc_lhs_arrays, allocate_target, pass_options_),
        parent_body(nullptr), pass_options(pass_options_) {
            call_replacer_on_value = false;
            replacer.call_replacer_on_value = false;
            pass_result.n = 0;
            pass_result.reserve(al, 0);
            print = false, file_write = false;
            m_unit = nullptr;
        }

        void visit_Variable(const ASR::Variable_t& /*x*/) {
            // Do nothing, already handled in init_expr pass
        }

        void call_replacer() {
            replacer.current_expr = current_expr;
            replacer.current_scope = current_scope;
            replacer.replace_expr(*current_expr);
        }

        // Helper to check if an expression contains StringTrim
        bool contains_string_trim(ASR::expr_t* expr) {
            if (!expr) return false;
            if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*expr)) {
                ASR::IntrinsicElementalFunction_t* f = ASR::down_cast<ASR::IntrinsicElementalFunction_t>(expr);
                if (f->m_intrinsic_id == static_cast<int64_t>(ASRUtils::IntrinsicElementalFunctions::StringTrim)) {
                    return true;
                }
                // Check arguments recursively
                for (size_t i = 0; i < f->n_args; i++) {
                    if (contains_string_trim(f->m_args[i])) return true;
                }
            } else if (ASR::is_a<ASR::ImpliedDoLoop_t>(*expr)) {
                ASR::ImpliedDoLoop_t* idl = ASR::down_cast<ASR::ImpliedDoLoop_t>(expr);
                for (size_t i = 0; i < idl->n_values; i++) {
                    if (contains_string_trim(idl->m_values[i])) return true;
                }
            } else if (ASR::is_a<ASR::ArrayConstructor_t>(*expr)) {
                ASR::ArrayConstructor_t* arr = ASR::down_cast<ASR::ArrayConstructor_t>(expr);
                for (size_t i = 0; i < arr->n_args; i++) {
                    if (contains_string_trim(arr->m_args[i])) return true;
                }
            }
            return false;
        }

        // Check if an implied do loop contains StringTrim in any of its values
        bool implied_do_loop_has_string_trim(ASR::ImpliedDoLoop_t* idl) {
            for (size_t i = 0; i < idl->n_values; i++) {
                if (contains_string_trim(idl->m_values[i])) return true;
            }
            return false;
        }

        void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
            Vec<ASR::stmt_t*> body;
            body.reserve(al, n_body);
            if( parent_body ) {
                for (size_t j=0; j < pass_result.size(); j++) {
                    parent_body->push_back(al, pass_result[j]);
                }
            }

            for (size_t i = 0; i < n_body; i++) {
                pass_result.n = 0;
                pass_result.reserve(al, 1);
                remove_original_statement = false;
                replacer.result_var = nullptr;
                Vec<ASR::stmt_t*>* parent_body_copy = parent_body;
                parent_body = &body;
                visit_stmt(*m_body[i]);
                parent_body = parent_body_copy;
                for (size_t j = 0; j < pass_result.size(); j++) {
                    body.push_back(al, pass_result[j]);
                }
                if( !remove_original_statement ) {
                    body.push_back(al, m_body[i]);
                }
                remove_original_statement = false;
            }
            m_body = body.p;
            n_body = body.size();
            replacer.result_var = nullptr;
            pass_result.n = 0;
            pass_result.reserve(al, 0);
        }

        void visit_Assignment(const ASR::Assignment_t &x) {
            if( (ASR::is_a<ASR::Pointer_t>(*ASRUtils::expr_type(x.m_target)) &&
                ASR::is_a<ASR::GetPointer_t>(*x.m_value)) ||
                ASR::is_a<ASR::ArrayReshape_t>(*x.m_value) ) {
                return ;
            }

            if (x.m_overloaded) {
                this->visit_stmt(*x.m_overloaded);
                remove_original_statement = false;
                return ;
            }

            if (ASRUtils::is_allocatable(x.m_target) &&
                    ASR::is_a<ASR::ArrayConstant_t>(*x.m_value)) {
                allocate_target = true;
            }
            replacer.result_var = x.m_target;
            resultvar2value[replacer.result_var] = x.m_value;
            ASR::expr_t** current_expr_copy_9 = current_expr;
            current_expr = const_cast<ASR::expr_t**>(&(x.m_value));
            this->call_replacer();
            current_expr = const_cast<ASR::expr_t**>(&(x.m_target));
            this->call_replacer();
            current_expr = current_expr_copy_9;
            if( !remove_original_statement ) {
                this->visit_expr(*x.m_value);
            }
        }

        template <typename T>
        ASR::asr_t* create_array_constant(const T& x, ASR::expr_t* value) {
            // wrap the implied do loop in an array constant
            Vec<ASR::expr_t*> args;
            args.reserve(al, 1);
            args.push_back(al, value);

            Vec<ASR::dimension_t> dim;
            dim.reserve(al, 1);

            ASR::dimension_t d;
            d.loc = value->base.loc;

            ASR::ttype_t *index_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, get_index_kind()));
            ASR::expr_t* one = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, 1, index_type));

            d.m_start = one;
            d.m_length = one;

            dim.push_back(al, d);

            ASR::ttype_t* array_type = ASRUtils::TYPE(ASR::make_Array_t(al, value->base.loc, ASRUtils::expr_type(value), dim.p, dim.size(), ASR::array_physical_typeType::FixedSizeArray));
            ASR::asr_t* array_constant = ASRUtils::make_ArrayConstructor_t_util(al, value->base.loc,
                                        args.p, args.n, array_type, ASR::arraystorageType::ColMajor);
            return array_constant;
        }

        class VarSubstituter : public ASR::BaseExprReplacer<VarSubstituter> {
        public:
            Allocator& al;
            ASR::symbol_t* loop_var;
            ASR::expr_t* replacement;
            
            VarSubstituter(Allocator& al_, ASR::symbol_t* var, ASR::expr_t* repl) 
                : BaseExprReplacer(), al(al_), loop_var(var), replacement(repl) {}
            
            void replace_Var(ASR::Var_t* x) {
                if (x->m_v == loop_var) {
                    *current_expr = replacement;
                }
            }
        };
        
        bool expand_implied_do_loop_flat(ASR::ImpliedDoLoop_t* idl, Vec<ASR::expr_t*>& result) {
            ASR::expr_t* start = idl->m_start;
            ASR::expr_t* end = idl->m_end;
            ASR::expr_t* step = idl->m_increment;
            
            if (!ASR::is_a<ASR::IntegerConstant_t>(*start) ||
                !ASR::is_a<ASR::IntegerConstant_t>(*end)) {
                return false;
            }
            
            int64_t start_val = ASR::down_cast<ASR::IntegerConstant_t>(start)->m_n;
            int64_t end_val = ASR::down_cast<ASR::IntegerConstant_t>(end)->m_n;
            int64_t step_val = 1;
            if (step && ASR::is_a<ASR::IntegerConstant_t>(*step)) {
                step_val = ASR::down_cast<ASR::IntegerConstant_t>(step)->m_n;
            }
            
            if (step_val == 0 || (step_val > 0 && start_val > end_val) || 
                (step_val < 0 && start_val < end_val)) {
                return true;
            }
            
            ASR::symbol_t* loop_var_symbol = ASR::down_cast<ASR::Var_t>(idl->m_var)->m_v;
            
            for (int64_t i = start_val; 
                 (step_val > 0) ? (i <= end_val) : (i >= end_val); 
                 i += step_val) {
                ASR::expr_t* loop_var_val = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                    al, idl->base.base.loc, i, 
                    ASRUtils::expr_type(idl->m_var)));
                
                for (size_t j = 0; j < idl->n_values; j++) {
                    ASR::expr_t* val = idl->m_values[j];
                    
                    if (ASR::is_a<ASR::ImpliedDoLoop_t>(*val)) {
                        ASR::ImpliedDoLoop_t* nested_idl = ASR::down_cast<ASR::ImpliedDoLoop_t>(val);
                        if (!expand_implied_do_loop_flat(nested_idl, result)) {
                            return false;
                        }
                    } else {
                        ASRUtils::ExprStmtDuplicator duplicator(al);
                        ASR::expr_t* substituted = duplicator.duplicate_expr(val);
                        
                        VarSubstituter substituter(al, loop_var_symbol, loop_var_val);
                        substituter.current_expr = &substituted;
                        substituter.replace_expr(substituted);
                        result.push_back(al, substituted);
                    }
                }
            }
            
            return true;
        }

        ASR::stmt_t* create_do_loop_form_idl(ASR::ImpliedDoLoop_t* x, ASR::expr_t* format_string) {
            ASR::stmt_t* do_loop = nullptr;

            ASR::do_loop_head_t do_loop_head;
            do_loop_head.loc = x->base.base.loc; do_loop_head.m_v = x->m_var;
            do_loop_head.m_start = x->m_start; do_loop_head.m_end = x->m_end;
            do_loop_head.m_increment = x->m_increment;

            Vec<ASR::stmt_t*> do_loop_body; do_loop_body.reserve(al, 1);
            
            Vec<ASR::expr_t*> args;
            args.reserve(al, x->n_values);
            for (size_t i = 0; i < x->n_values; i++ ) {
                if ( ASR::is_a<ASR::ImpliedDoLoop_t>(*x->m_values[i]) ) {
                    do_loop_body.push_back(al, create_do_loop_form_idl(
                        ASR::down_cast<ASR::ImpliedDoLoop_t>(x->m_values[i]), format_string));
                } else {
                    args.push_back(al, x->m_values[i]);
                }
            }
            
            if (args.size() > 0) {
                ASR::expr_t* fmt_val = ASRUtils::EXPR(ASR::make_StringFormat_t(al,x->base.base.loc, format_string,
                    args.p, args.size(), ASR::string_format_kindType::FormatFortran,
                    ASRUtils::TYPE(ASR::make_Allocatable_t(al, x->base.base.loc, 
                        ASRUtils::TYPE(ASR::make_String_t(al,x->base.base.loc, 1,
                            nullptr,
                            ASR::string_length_kindType::DeferredLength,
                            ASR::string_physical_typeType::DescriptorString)))),
                    nullptr));
                Vec<ASR::expr_t*> print_values;
                print_values.reserve(al, 1);
                print_values.push_back(al, fmt_val);
                ASR::stmt_t* stmt = nullptr;
                if ( print ) {
                    stmt = ASRUtils::STMT(ASRUtils::make_print_t_util(al, x->base.base.loc, print_values.p, print_values.size()));
                } else {
                    // this will be file_write
                    LCOMPILERS_ASSERT(file_write);
                    stmt = ASRUtils::STMT(ASR::make_FileWrite_t(al, x->base.base.loc, 0, m_unit, nullptr, nullptr, nullptr, print_values.p, print_values.size(), nullptr, nullptr, nullptr, true, nullptr, nullptr));
                }
                do_loop_body.push_back(al, stmt);
            }
            
            do_loop = ASRUtils::STMT(ASR::make_DoLoop_t(al, x->base.base.loc, nullptr, do_loop_head, do_loop_body.p, do_loop_body.size(), nullptr, 0));
            return do_loop;
        }

        void visit_Print(const ASR::Print_t &x) {
            print = true;
            /*
                integer :: i
                print *, (i, i=1, 10)

                TO

                integer :: i
                print *, [(i, i=1, 10)]
            */
            if(ASR::is_a<ASR::String_t>(*ASRUtils::extract_type(ASRUtils::expr_type(x.m_text)))){
                ASR::Print_t* print_stmt = const_cast<ASR::Print_t*>(&x);
                ASR::expr_t** current_expr_copy_9 = current_expr;
                current_expr = const_cast<ASR::expr_t**>(&(print_stmt->m_text));
                this->call_replacer();
                current_expr = current_expr_copy_9;
                if( !remove_original_statement ) {
                    this->visit_expr(*print_stmt->m_text);
                }
                print = false;
            } else {
                LCOMPILERS_ASSERT_MSG(false, "print should support stringFormat or single string");
            }
        }

        void visit_StringFormat(const ASR::StringFormat_t &x) {
            /*
                integer :: i
                write(*, '(i)') (i, i=1, 10)

                TO

                integer :: i
                write(*, '(i)') [(i, i=1, 10)]
            */
            ASR::StringFormat_t* string_format_stmt = const_cast<ASR::StringFormat_t*>(&x);
            for(size_t i = 0; i < x.n_args; i++) {
                ASR::expr_t* value = x.m_args[i];
                if (ASR::is_a<ASR::ImpliedDoLoop_t>(*value)) {
                    ASR::ImpliedDoLoop_t* implied_do_loop = ASR::down_cast<ASR::ImpliedDoLoop_t>(value);
                    // Use do-loop approach ONLY for formatted I/O (not list-directed)
                    // with Tuple types or when values contain StringTrim
                    // (to preserve variable-length trimmed strings instead of storing in fixed-length array)
                    if ( (x.m_fmt != nullptr) &&  // Only for formatted I/O, not list-directed (print *)
                         (ASR::is_a<ASR::Tuple_t>(*implied_do_loop->m_type) ||
                          implied_do_loop_has_string_trim(implied_do_loop)) ) {
                        remove_original_statement = true;
                        pass_result.push_back(al, create_do_loop_form_idl(implied_do_loop, x.m_fmt));
                        continue;
                    }
                    if ( (x.m_fmt == nullptr) &&
                         (ASR::is_a<ASR::Tuple_t>(*implied_do_loop->m_type) ||
                          implied_do_loop_has_string_trim(implied_do_loop)) ) {
                        Vec<ASR::expr_t*> expanded_values;
                        expanded_values.reserve(al, 16);
                        if (expand_implied_do_loop_flat(implied_do_loop, expanded_values)) {
                            Vec<ASR::expr_t*> new_args;
                            new_args.reserve(al, x.n_args + expanded_values.size());
                            for (size_t j = 0; j < i; j++) {
                                new_args.push_back(al, x.m_args[j]);
                            }
                            for (size_t j = 0; j < expanded_values.size(); j++) {
                                new_args.push_back(al, expanded_values.p[j]);
                            }
                            for (size_t j = i + 1; j < x.n_args; j++) {
                                new_args.push_back(al, x.m_args[j]);
                            }
                            string_format_stmt->m_args = new_args.p;
                            string_format_stmt->n_args = new_args.size();
                            break;
                        }
                    }
                    ASR::asr_t* array_constant = create_array_constant(x, value);
                    string_format_stmt->m_args[i] = ASRUtils::EXPR(array_constant);

                    replacer.result_var = value;
                    resultvar2value[replacer.result_var] = ASRUtils::EXPR(array_constant);
                    ASR::expr_t** current_expr_copy_9 = current_expr;
                    current_expr = const_cast<ASR::expr_t**>(&(string_format_stmt->m_args[i]));
                    this->call_replacer();
                    current_expr = current_expr_copy_9;
                    if( !remove_original_statement ) {
                        this->visit_expr(*string_format_stmt->m_args[i]);
                    }
                } else {
                    ASR::expr_t** current_expr_copy_9 = current_expr;
                    current_expr = const_cast<ASR::expr_t**>(&(string_format_stmt->m_args[i]));
                    this->call_replacer();
                    current_expr = current_expr_copy_9;
                    if( !remove_original_statement ) {
                        this->visit_expr(*string_format_stmt->m_args[i]);
                    }
                }
            }
        }

        void visit_FileRead(const ASR::FileRead_t &x) {
            if (x.m_overloaded) {
                this->visit_stmt(*x.m_overloaded);
                remove_original_statement = false;
                return;
            }
        }

        void visit_FileWrite(const ASR::FileWrite_t &x) {
            file_write = true;
            m_unit = x.m_unit;
            if (x.m_overloaded) {
                this->visit_stmt(*x.m_overloaded);
                remove_original_statement = false;
                file_write = false;
                return;
            }

            /*
                integer :: i
                write(*,*) (i, i=1, 10)

                TO

                integer :: i
                write(*,*) [(i, i=1, 10)]
            */
            ASR::FileWrite_t* write_stmt = const_cast<ASR::FileWrite_t*>(&x);
            for(size_t i = 0; i < x.n_values; i++) {
                ASR::expr_t* value = x.m_values[i];
                if (ASR::is_a<ASR::ImpliedDoLoop_t>(*value)) {
                    ASR::asr_t* array_constant = create_array_constant(x, value);

                    write_stmt->m_values[i] = ASRUtils::EXPR(array_constant);

                    replacer.result_var = value;
                    resultvar2value[replacer.result_var] = ASRUtils::EXPR(array_constant);
                    ASR::expr_t** current_expr_copy_9 = current_expr;
                    current_expr = const_cast<ASR::expr_t**>(&(write_stmt->m_values[i]));
                    this->call_replacer();
                    current_expr = current_expr_copy_9;
                    if( !remove_original_statement ) {
                        this->visit_expr(*write_stmt->m_values[i]);
                    }
                } else {
                    ASR::expr_t** current_expr_copy_9 = current_expr;
                    current_expr = const_cast<ASR::expr_t**>(&(write_stmt->m_values[i]));
                    this->call_replacer();
                    current_expr = current_expr_copy_9;
                    if( !remove_original_statement ) {
                        this->visit_expr(*write_stmt->m_values[i]);
                    }
                }
            }
            file_write = false;
        }

        void visit_CPtrToPointer(const ASR::CPtrToPointer_t& x) {
            if (x.m_shape) {
                ASR::expr_t** current_expr_copy = current_expr;
                current_expr = const_cast<ASR::expr_t**>(&(x.m_shape));
                this->call_replacer();
                current_expr = current_expr_copy;
                if( x.m_shape )
                this->visit_expr(*x.m_shape);
            }
        }

        void visit_ArrayBroadcast(const ASR::ArrayBroadcast_t& x) {
            ASR::expr_t** current_expr_copy_269 = current_expr;
            current_expr = const_cast<ASR::expr_t**>(&(x.m_array));
            call_replacer();
            current_expr = current_expr_copy_269;
            if( x.m_array ) {
                visit_expr(*x.m_array);
            }
        }

};

void pass_replace_implied_do_loops(Allocator &al,
    ASR::TranslationUnit_t &unit,
    const LCompilers::PassOptions& pass_options) {
    ArrayConstantVisitor v(al, pass_options);
    v.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}


} // namespace LCompilers
