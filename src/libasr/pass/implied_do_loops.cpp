#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/asr_builder.h>
#include <libasr/pass/replace_implied_do_loops.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/symbol_expr_substitution.h>
#include <libasr/diagnostics.h>

#include <algorithm>
#include <limits>
#include <map>
#include <set>
#include <string>
#include <vector>


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
    bool skip_idl_save_restore;
    bool materialize_constants = false;
    SymbolTable* constructor_scope = nullptr;
    std::vector<ASR::expr_t*> constructor_results;
    // Index variables of the enclosing FORALL statements
    std::vector<ASR::symbol_t*> forall_indices;

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
    pass_options(pass_options_), skip_idl_save_restore(false) {}

    // Given an array expression (e.g. elemental function call, binop on
    // arrays), return the size of the array without referencing the
    // implied-do loop variable.  We first check the expression's type for
    // explicit dimension lengths; if they are absent (DescriptorArray with
    // deferred dims) we drill into operands/arguments to find an
    // ArraySection or Var whose size can be computed from bounds alone.
    ASR::expr_t* get_array_expr_size(ASR::expr_t* expr) {
        ASR::ttype_t* type = ASRUtils::type_get_past_allocatable(
            ASRUtils::type_get_past_pointer(ASRUtils::expr_type(expr)));

        // Fixed-size arrays: known at compile time
        if( ASRUtils::is_fixed_size_array(type) ) {
            return nullptr; // handled by caller via const_elements
        }

        // If the type carries explicit dimension lengths, compute the
        // product directly — these typically reference ArrayBound on the
        // original arrays and do NOT reference the loop variable.
        ASR::dimension_t* m_dims = nullptr;
        size_t n_dims = ASRUtils::extract_dimensions_from_ttype(type, m_dims);
        if( n_dims > 0 && m_dims[0].m_length != nullptr ) {
            ASR::expr_t* size = m_dims[0].m_length;
            for( size_t d = 1; d < n_dims; d++ ) {
                if( m_dims[d].m_length == nullptr ) break;
                ASRUtils::ASRBuilder b(al, expr->base.loc);
                size = b.Mul(size, m_dims[d].m_length);
            }
            return size;
        }

        // Otherwise, find the first array operand whose size is
        // representable without the loop variable.
        auto first_array_arg = [&](ASR::expr_t** args, size_t n) -> ASR::expr_t* {
            for( size_t j = 0; j < n; j++ ) {
                if( args[j] && ASRUtils::is_array(ASRUtils::expr_type(args[j])) )
                    return args[j];
            }
            return nullptr;
        };

        if( ASR::is_a<ASR::FunctionCall_t>(*expr) ) {
            ASR::FunctionCall_t* fc = ASR::down_cast<ASR::FunctionCall_t>(expr);
            for( size_t j = 0; j < fc->n_args; j++ ) {
                if( fc->m_args[j].m_value &&
                    ASRUtils::is_array(ASRUtils::expr_type(fc->m_args[j].m_value)) ) {
                    ASR::expr_t* r = get_array_expr_size(fc->m_args[j].m_value);
                    if( r ) return r;
                }
            }
        } else if( ASR::is_a<ASR::IntrinsicElementalFunction_t>(*expr) ) {
            auto* ef = ASR::down_cast<ASR::IntrinsicElementalFunction_t>(expr);
            ASR::expr_t* a = first_array_arg(ef->m_args, ef->n_args);
            if( a ) { ASR::expr_t* r = get_array_expr_size(a); if( r ) return r; }
        } else if( ASR::is_a<ASR::RealBinOp_t>(*expr) ) {
            auto* bo = ASR::down_cast<ASR::RealBinOp_t>(expr);
            ASR::expr_t* ops[2] = {bo->m_left, bo->m_right};
            ASR::expr_t* a = first_array_arg(ops, 2);
            if( a ) { ASR::expr_t* r = get_array_expr_size(a); if( r ) return r; }
        } else if( ASR::is_a<ASR::IntegerBinOp_t>(*expr) ) {
            auto* bo = ASR::down_cast<ASR::IntegerBinOp_t>(expr);
            ASR::expr_t* ops[2] = {bo->m_left, bo->m_right};
            ASR::expr_t* a = first_array_arg(ops, 2);
            if( a ) { ASR::expr_t* r = get_array_expr_size(a); if( r ) return r; }
        } else if( ASR::is_a<ASR::RealUnaryMinus_t>(*expr) ) {
            auto* uo = ASR::down_cast<ASR::RealUnaryMinus_t>(expr);
            ASR::expr_t* r = get_array_expr_size(uo->m_arg);
            if( r ) return r;
        } else if( ASR::is_a<ASR::IntegerUnaryMinus_t>(*expr) ) {
            auto* uo = ASR::down_cast<ASR::IntegerUnaryMinus_t>(expr);
            ASR::expr_t* r = get_array_expr_size(uo->m_arg);
            if( r ) return r;
        }

        // Leaf array expressions (ArraySection, Var, etc.)
        return ASRUtils::get_size(expr, al, false);
    }

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
            d = builder.i2i_t(d, ASRUtils::expr_type(end));
            implied_doloop_size = builder.Add(builder.Div(
                builder.Sub(end, start), d),
                make_ConstantWithKind(make_IntegerConstant_t, make_Integer_t, 1, kind, loc));
        }
        int64_t const_elements = 0;
        ASR::expr_t* implied_doloop_size_ = nullptr;
        // Track whether size came from a runtime expression (e.g. FunctionCall
        // returning allocatable array) vs a nested ImpliedDoLoop/ArrayConstructor.
        // The arithmetic sum formula only applies to the latter.
        bool has_runtime_array_size = false;
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
            } else if( ASR::is_a<ASR::ArrayConstructor_t>(*implied_doloop->m_values[i]) ) {
                bool is_alloc_inner = false;
                ASR::expr_t* ac_size = get_ArrayConstructor_size(
                    ASR::down_cast<ASR::ArrayConstructor_t>(implied_doloop->m_values[i]),
                    is_alloc_inner);
                if( implied_doloop_size_ == nullptr ) {
                    implied_doloop_size_ = ac_size;
                } else {
                    implied_doloop_size_ = builder.Add(ac_size, implied_doloop_size_);
                }
            } else {
                ASR::ttype_t* element_type = ASRUtils::type_get_past_allocatable(
                    ASRUtils::type_get_past_pointer(
                        ASRUtils::expr_type(implied_doloop->m_values[i])));
                if( ASRUtils::is_array(element_type) ) {
                    if( ASRUtils::is_fixed_size_array(element_type) ) {
                        const_elements += ASRUtils::get_fixed_size_of_array(element_type);
                    } else {
                        has_runtime_array_size = true;
                        ASR::expr_t* element_array_size = get_array_expr_size(
                            implied_doloop->m_values[i]);
                        // The size expression may reference the loop variable
                        // (e.g. size(a(i)%values())). Since this expression is
                        // evaluated before the loop starts, replace the loop
                        // variable with its start value so the index is valid.
                        if (ASR::is_a<ASR::Var_t>(*implied_doloop->m_var)) {
                            ASR::symbol_t* lv_sym = ASR::down_cast<ASR::Var_t>(
                                implied_doloop->m_var)->m_v;
                            ASRUtils::ExprStmtDuplicator dup(al);
                            element_array_size = dup.duplicate_expr(element_array_size);
                            class ReplaceLoopVar : public ASR::BaseExprReplacer<ReplaceLoopVar> {
                            public:
                                Allocator& al;
                                ASR::symbol_t* sym;
                                ASR::expr_t* replacement;
                                ReplaceLoopVar(Allocator& a, ASR::symbol_t* s, ASR::expr_t* r)
                                    : al(a), sym(s), replacement(r) {}
                                void replace_Var(ASR::Var_t* x) {
                                    if (x->m_v == sym) *current_expr = replacement;
                                }
                            };
                            ReplaceLoopVar replacer(al, lv_sym, implied_doloop->m_start);
                            replacer.current_expr = &element_array_size;
                            replacer.replace_expr(element_array_size);
                        }
                        if( implied_doloop_size_ == nullptr ) {
                            implied_doloop_size_ = element_array_size;
                        } else {
                            implied_doloop_size_ = builder.Add(element_array_size,
                                implied_doloop_size_);
                        }
                    }
                } else {
                    const_elements += 1;
                }
            }
        }

        ASR::symbol_t* loop_var_sym = nullptr;
        if (ASR::is_a<ASR::Var_t>(*implied_doloop->m_var)) {
            loop_var_sym = ASR::down_cast<ASR::Var_t>(implied_doloop->m_var)->m_v;
        }
        bool inner_depends_on_var = (loop_var_sym != nullptr &&
            implied_doloop_size_ != nullptr && d == nullptr &&
            !has_runtime_array_size &&
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
            } else if (ASR::is_a<ASR::ImpliedDoLoop_t>(*element)) {
                ASR::expr_t* implied_doloop_size = ASRUtils::get_ImpliedDoLoop_size(al,
                    ASR::down_cast<ASR::ImpliedDoLoop_t>(element));
                if (array_size) {
                    array_size = builder.Add(implied_doloop_size, array_size);
                } else {
                    array_size = implied_doloop_size;
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

    void replace_ImpliedDoLoop(ASR::ImpliedDoLoop_t* /*x*/) {
        // Do not descend into ImpliedDoLoop, as it has its own scope for loop variables.
        // Any ArrayConstructor inside will be handled by create_do_loop during ImpliedDoLoop expansion.
    }

    bool depends_on_forall_index(ASR::expr_t* x) {
        for (ASR::symbol_t* index: forall_indices) {
            if (ASRUtils::expr_references_symbol(x, index)) {
                return true;
            }
        }
        return false;
    }

    void prepare_constructor_leaf(ASR::expr_t*& value, Vec<ASR::stmt_t*>& body) {
        // Nested constructors must be evaluated in this expression's parent
        // activation, including when the outer cardinality is already known.
        bool redirect = &body != &pass_result;
        if (redirect) std::swap(pass_result, body);
        ASR::expr_t** saved_expr = current_expr;
        ASR::expr_t* saved_result = result_var;
        bool saved_remove = remove_original_statement;
        current_expr = &value;
        result_var = nullptr;
        replace_expr(value);
        current_expr = saved_expr;
        result_var = saved_result;
        remove_original_statement = saved_remove;
        if (redirect) std::swap(pass_result, body);
    }

    static bool is_runtime_constructor(ASR::ArrayConstructor_t* x) {
        ASR::ttype_t* element = ASRUtils::extract_type(x->m_type);
        ASR::Array_t* array = ASR::down_cast<ASR::Array_t>(
            ASRUtils::type_get_past_allocatable_pointer(x->m_type));
        return (ASRUtils::is_integer(*element) || ASRUtils::is_real(*element) ||
                ASRUtils::is_complex(*element) || ASRUtils::is_logical(*element)) &&
            array->m_memory_space == ASR::memory_spaceType::Global &&
            ASRUtils::constructor_has_runtime_extent(&x->base);
    }

    // Whether a nested implied-DO control has the same value in every
    // iteration of the enclosing implied-DOs, so that saving it once is
    // equivalent to evaluating it at each nested loop entry.
    static bool is_invariant_control(ASR::expr_t* control,
            const std::vector<ASR::symbol_t*>& indices) {
        class InvariantControl: public ASR::BaseWalkVisitor<InvariantControl> {
        public:
            const std::vector<ASR::symbol_t*>& indices;
            bool invariant = true;
            InvariantControl(const std::vector<ASR::symbol_t*>& indices):
                indices(indices) {}
            void visit_ttype(const ASR::ttype_t&) {}
            void visit_Var(const ASR::Var_t& x) {
                if (std::find(indices.begin(), indices.end(), x.m_v) !=
                        indices.end()) {
                    invariant = false;
                }
            }
            void visit_FunctionCall(const ASR::FunctionCall_t&) {
                invariant = false;
            }
            void visit_ArrayConstructor(const ASR::ArrayConstructor_t&) {
                invariant = false;
            }
            void visit_ImpliedDoLoop(const ASR::ImpliedDoLoop_t&) {
                invariant = false;
            }
        } v(indices);
        v.visit_compile_time_value = false;
        v.visit_expr(*control);
        return v.invariant;
    }

    // Whether the element count of a constructor item is known before its
    // traversal without evaluating anything twice. It is not when a nested
    // implied-DO control depends on an enclosing index or calls a procedure,
    // or when an array leaf inside an implied-DO has a runtime extent that
    // is only known once the leaf is evaluated.
    static bool has_closed_form_cardinality(ASR::expr_t* item,
            std::vector<ASR::symbol_t*>& indices) {
        if (ASR::is_a<ASR::Cast_t>(*item)) {
            return has_closed_form_cardinality(
                ASR::down_cast<ASR::Cast_t>(item)->m_arg, indices);
        }
        if (ASR::is_a<ASR::ArrayConstructor_t>(*item)) {
            ASR::ArrayConstructor_t* c = ASR::down_cast<ASR::ArrayConstructor_t>(item);
            for (size_t i = 0; i < c->n_args; i++) {
                if (!has_closed_form_cardinality(c->m_args[i], indices)) {
                    return false;
                }
            }
            return true;
        }
        if (ASR::is_a<ASR::ImpliedDoLoop_t>(*item)) {
            ASR::ImpliedDoLoop_t* loop = ASR::down_cast<ASR::ImpliedDoLoop_t>(item);
            if (!ASR::is_a<ASR::Var_t>(*loop->m_var)) return false;
            for (ASR::expr_t* control: {loop->m_start, loop->m_end,
                    loop->m_increment}) {
                if (control && !indices.empty() &&
                        !is_invariant_control(control, indices)) {
                    return false;
                }
            }
            indices.push_back(ASR::down_cast<ASR::Var_t>(loop->m_var)->m_v);
            bool known = true;
            for (size_t i = 0; i < loop->n_values && known; i++) {
                known = has_closed_form_cardinality(loop->m_values[i], indices);
            }
            indices.pop_back();
            return known;
        }
        ASR::ttype_t* type = ASRUtils::expr_type(item);
        if (!ASRUtils::is_array(type) || ASRUtils::is_fixed_size_array(type)) {
            return true;
        }
        return indices.empty() || ASR::is_a<ASR::Var_t>(*item);
    }

    // Rebuilds a constructor item whose cardinality has a closed form with
    // each implied-DO control saved once in the parent activation and each
    // runtime-extent array leaf outside implied-DOs snapshotted, in element
    // order, and sets `count` to its element count.
    ASR::expr_t* capture_cardinality(ASR::expr_t* item, ASR::expr_t*& count,
            std::vector<ASR::expr_t*>& snapshots, ASR::ttype_t* index_type,
            bool nested) {
        const Location& loc = item->base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        auto integer = [&](int64_t n) {
            return ASRUtils::EXPR(
                ASR::make_IntegerConstant_t(al, loc, n, index_type));
        };
        auto add = [&](ASR::expr_t* sum, ASR::expr_t* term) {
            return sum ? b.Add(sum, term) : term;
        };
        if (ASR::is_a<ASR::Cast_t>(*item)) {
            ASR::Cast_t* cast = ASR::down_cast<ASR::Cast_t>(item);
            ASR::expr_t* arg = capture_cardinality(cast->m_arg, count,
                snapshots, index_type, nested);
            return ASRUtils::EXPR(ASR::make_Cast_t(al, loc, arg, cast->m_kind,
                cast->m_type, nullptr, cast->m_dest));
        }
        if (ASR::is_a<ASR::ArrayConstructor_t>(*item)) {
            ASR::ArrayConstructor_t* c = ASR::down_cast<ASR::ArrayConstructor_t>(item);
            Vec<ASR::expr_t*> args;
            args.reserve(al, c->n_args);
            count = nullptr;
            for (size_t i = 0; i < c->n_args; i++) {
                ASR::expr_t* arg_count = nullptr;
                args.push_back(al, capture_cardinality(c->m_args[i], arg_count,
                    snapshots, index_type, nested));
                count = add(count, arg_count);
            }
            if (!count) count = integer(0);
            return ASRUtils::EXPR(ASR::make_ArrayConstructor_t(al, loc,
                args.p, args.n, c->m_type, nullptr, c->m_storage_format,
                c->m_struct_var));
        }
        if (ASR::is_a<ASR::ImpliedDoLoop_t>(*item)) {
            ASR::ImpliedDoLoop_t* loop = ASR::down_cast<ASR::ImpliedDoLoop_t>(item);
            ASR::expr_t* controls[] = {
                loop->m_start, loop->m_end, loop->m_increment};
            for (ASR::expr_t*& control: controls) {
                if (!control) continue;
                prepare_constructor_leaf(control, pass_result);
                LCOMPILERS_ASSERT(ASRUtils::types_equal(
                    ASRUtils::expr_type(control),
                    ASRUtils::expr_type(loop->m_var), nullptr, nullptr));
                std::string name = current_scope->get_unique_name("_idl_control");
                ASR::stmt_t* assignment;
                control = PassUtils::create_auxiliary_variable_for_expr(
                    control, name, al, current_scope, assignment);
                pass_result.push_back(al, assignment);
            }
            // The trip count is computed in the kind of the controls, so
            // wide controls are not truncated before the division.
            ASR::ttype_t* control_type = ASRUtils::expr_type(loop->m_var);
            ASR::expr_t* step = controls[2] ? controls[2]
                : b.i_t(1, control_type);
            ASR::expr_t* control_trip = PassUtils::create_var(result_counter++,
                "_constructor_trip", loc, control_type, al, current_scope);
            pass_result.push_back(al, b.Assignment(control_trip, b.Div(b.Add(
                b.Sub(controls[1], controls[0]), step), step)));
            pass_result.push_back(al, b.If(b.Lt(control_trip,
                b.i_t(0, control_type)),
                {b.Assignment(control_trip, b.i_t(0, control_type))}, {}));
            ASR::expr_t* trip = CastingUtil::perform_casting(control_trip,
                index_type, al, loc);
            Vec<ASR::expr_t*> values;
            values.reserve(al, loop->n_values);
            ASR::expr_t* per_iteration = nullptr;
            for (size_t i = 0; i < loop->n_values; i++) {
                ASR::expr_t* value_count = nullptr;
                values.push_back(al, capture_cardinality(loop->m_values[i],
                    value_count, snapshots, index_type, true));
                per_iteration = add(per_iteration, value_count);
            }
            count = per_iteration ? b.Mul(trip, per_iteration) : integer(0);
            return ASRUtils::EXPR(ASR::make_ImpliedDoLoop_t(al, loc,
                values.p, values.n, loop->m_var, controls[0], controls[1],
                controls[2], loop->m_type, nullptr));
        }
        ASR::ttype_t* type = ASRUtils::expr_type(item);
        if (!ASRUtils::is_array(type)) {
            count = integer(1);
            return item;
        }
        if (ASRUtils::is_fixed_size_array(type)) {
            count = integer(ASRUtils::get_fixed_size_of_array(type));
            return item;
        }
        if (!ASR::is_a<ASR::Var_t>(*item)) {
            LCOMPILERS_ASSERT(!nested);
            prepare_constructor_leaf(item, pass_result);
            ASR::ttype_t* snapshot_type = ASRUtils::duplicate_type_with_empty_dims(
                al, ASRUtils::type_get_past_allocatable_pointer(
                    ASRUtils::expr_type(item)),
                ASR::array_physical_typeType::DescriptorArray, true);
            ASR::expr_t* snapshot = PassUtils::create_var(result_counter++,
                "_constructor_leaf", loc, ASRUtils::TYPE(
                    ASR::make_Allocatable_t(al, loc, snapshot_type)),
                al, current_scope);
            // An internal owning snapshot, independent of the user's
            // allocation-on-assignment option.
            pass_result.push_back(al, ASRUtils::STMT(
                ASRUtils::make_Assignment_t_util(al, loc, snapshot, item,
                    nullptr, true, false)));
            snapshots.push_back(snapshot);
            item = snapshot;
        }
        count = b.ArraySize(item, nullptr, index_type);
        return item;
    }

    // Fills a constructor whose element count is known before its traversal
    // directly into an exact-size result.
    void materialize_counted_constructor(ASR::ArrayConstructor_t* x,
            ASR::ttype_t* result_type, ASR::ttype_t* index_type) {
        const Location& loc = x->base.base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        ASR::expr_t* count = nullptr;
        std::vector<ASR::expr_t*> snapshots;
        ASR::expr_t* items = capture_cardinality(&x->base, count, snapshots,
            index_type, false);
        ASR::expr_t* result = PassUtils::create_var(result_counter++,
            "_constructor_result", loc, result_type, al, current_scope);
        ASR::expr_t* index = PassUtils::create_var(result_counter++,
            "_constructor_index", loc, index_type, al, current_scope);
        Vec<ASR::dimension_t> dims;
        dims.reserve(al, 1);
        ASR::dimension_t dim;
        dim.loc = loc;
        dim.m_start = ASRUtils::EXPR(
            ASR::make_IntegerConstant_t(al, loc, 1, index_type));
        dim.m_length = count;
        dims.push_back(al, dim);
        pass_result.push_back(al, b.Deallocate(result));
        pass_result.push_back(al, b.Allocate(result, dims));
        pass_result.push_back(al, b.Assignment(index, dim.m_start));
        auto store = [&](ASR::expr_t* value, Vec<ASR::stmt_t*>& body) {
            body.push_back(al, b.Assignment(PassUtils::create_array_ref(
                result, index, al, current_scope), value));
            body.push_back(al, b.Assignment(index, b.Add(index, dim.m_start)));
        };
        PassUtils::ReplacerUtils::ConstructorLeaf fill =
            [&](ASR::expr_t* value, Vec<ASR::stmt_t*>& body,
                    bool perform_cast, ASR::cast_kindType cast_kind,
                    ASR::ttype_t* casted_type) {
                prepare_constructor_leaf(value, body);
                ASR::ttype_t* type = ASRUtils::expr_type(value);
                if (!ASRUtils::is_array(type)) {
                    if (perform_cast && !ASRUtils::types_equal(
                            type, casted_type, nullptr, nullptr)) {
                        value = ASRUtils::EXPR(ASR::make_Cast_t(al, loc, value,
                            cast_kind, casted_type, nullptr, nullptr));
                    }
                    store(value, body);
                    return;
                }
                ASR::expr_t* leaf = value;
                if (!ASR::is_a<ASR::Var_t>(*value)) {
                    ASR::ttype_t* leaf_type = ASRUtils::duplicate_type_with_empty_dims(
                        al, ASRUtils::type_get_past_allocatable_pointer(type),
                        ASR::array_physical_typeType::DescriptorArray, true);
                    leaf = PassUtils::create_var(result_counter++,
                        "_constructor_leaf", loc, ASRUtils::TYPE(
                            ASR::make_Allocatable_t(al, loc, leaf_type)),
                        al, current_scope);
                    body.push_back(al, ASRUtils::STMT(
                        ASRUtils::make_Assignment_t_util(al, loc, leaf, value,
                            nullptr, true, false)));
                }
                Vec<ASR::expr_t*> indices;
                Vec<ASR::stmt_t*> inner;
                PassUtils::ReplacerUtils::create_do_loop(al, loc,
                    ASRUtils::extract_n_dims_from_ttype(type), leaf,
                    indices, inner, [&]() {
                        store(PassUtils::create_array_ref(leaf, indices, al,
                            current_scope, perform_cast, cast_kind,
                            casted_type), inner);
                    }, current_scope, &body);
                if (leaf != value) body.push_back(al, b.Deallocate(leaf));
            };
        PassUtils::ReplacerUtils::ConstructorPrepare prepare_control =
            [&](ASR::expr_t*& value, Vec<ASR::stmt_t*>& body) {
                prepare_constructor_leaf(value, body);
            };
        PassUtils::ReplacerUtils::visit_ArrayConstructor(
            ASR::down_cast<ASR::ArrayConstructor_t>(items), al, result,
            &pass_result, index, current_scope, false,
            ASR::cast_kindType::IntegerToInteger, nullptr,
            skip_idl_save_restore, &fill, &prepare_control);
        for (ASR::expr_t* snapshot: snapshots) {
            pass_result.push_back(al, b.Deallocate(snapshot));
        }
        constructor_results.push_back(result);
        *current_expr = result;
        remove_original_statement = false;
    }

    void materialize_dynamic_constructor(ASR::ArrayConstructor_t* x) {
        const Location& loc = x->base.base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        LCOMPILERS_ASSERT(is_runtime_constructor(x));
        if (!constructor_scope) {
            constructor_scope = al.make_new<SymbolTable>(current_scope);
        }
        current_scope = constructor_scope;
        ASR::ttype_t* element = ASRUtils::duplicate_type(al,
            ASRUtils::extract_type(x->m_type));
        ASR::ttype_t* index_type = ASRUtils::TYPE(
            ASR::make_Integer_t(al, loc, get_index_kind()));
        auto integer = [&](int64_t n) {
            return ASRUtils::EXPR(
                ASR::make_IntegerConstant_t(al, loc, n, index_type));
        };
        auto variable = [&](const std::string& name, ASR::ttype_t* type) {
            return PassUtils::create_var(result_counter++, name,
                loc, type, al, current_scope);
        };
        auto owner_type = [&]() {
            Vec<ASR::dimension_t> dims;
            dims.reserve(al, 1);
            ASR::dimension_t dim;
            dim.loc = loc;
            dim.m_start = nullptr;
            dim.m_length = nullptr;
            dims.push_back(al, dim);
            return ASRUtils::TYPE(ASR::make_Allocatable_t(al, loc,
                ASRUtils::TYPE(ASR::make_Array_t(al, loc,
                    ASRUtils::duplicate_type(al, element), dims.p, dims.n,
                    ASR::array_physical_typeType::DescriptorArray,
                    ASR::memory_spaceType::Global))));
        };
        std::vector<ASR::symbol_t*> enclosing_indices;
        if (has_closed_form_cardinality(&x->base, enclosing_indices)) {
            materialize_counted_constructor(x, owner_type(), index_type);
            return;
        }
        // The cardinality is only known once the constructor has been
        // traversed, so its elements are appended to a growing buffer.
        struct AppendContext {
            ASR::expr_t *buffer, *scratch, *used, *capacity;
        } context {
            variable("_constructor_buffer", owner_type()),
            variable("_constructor_scratch", owner_type()),
            variable("_constructor_used", index_type),
            variable("_constructor_capacity", index_type)
        };
        ASR::expr_t* result = variable("_constructor_result", owner_type());
        ASR::expr_t* copy_index = variable("_constructor_copy", index_type);
        auto allocate = [&](ASR::expr_t* array, ASR::expr_t* size) {
            Vec<ASR::dimension_t> dims;
            dims.reserve(al, 1);
            ASR::dimension_t dim;
            dim.loc = loc;
            dim.m_start = integer(1);
            dim.m_length = size;
            dims.push_back(al, dim);
            return b.Allocate(array, dims);
        };
        auto copy_prefix = [&](ASR::expr_t* target, ASR::expr_t* source) {
            return b.DoLoop(copy_index, integer(1), context.used, {b.Assignment(
                PassUtils::create_array_ref(target, copy_index, al, current_scope),
                PassUtils::create_array_ref(source, copy_index, al, current_scope))});
        };
        ASR::stmt_t* overflow = ASRUtils::STMT(
            ASR::make_ErrorStop_t(al, loc, integer(1)));
        int64_t max_count = get_index_kind() == 8 ?
            std::numeric_limits<int64_t>::max() :
            std::numeric_limits<int32_t>::max();
        pass_result.push_back(al, b.Assignment(context.used, integer(0)));
        pass_result.push_back(al, b.Assignment(context.capacity, integer(0)));
        pass_result.push_back(al, b.Deallocate(context.buffer));
        pass_result.push_back(al, allocate(context.buffer, integer(0)));

        auto append_scalar = [&](ASR::expr_t* value, Vec<ASR::stmt_t*>& body) {
            body.push_back(al, b.If(b.Eq(context.used, integer(max_count)),
                {overflow}, {}));
            std::vector<ASR::stmt_t*> growth;
            growth.push_back(b.If(b.Gt(context.capacity, integer(max_count / 2)),
                {overflow}, {}));
            growth.push_back(b.If(b.Gt(context.used, integer(0)),
                {allocate(context.scratch, context.used),
                 copy_prefix(context.scratch, context.buffer)}, {}));
            growth.push_back(b.Deallocate(context.buffer));
            growth.push_back(b.If(b.Eq(context.capacity, integer(0)),
                {b.Assignment(context.capacity, integer(1))},
                {b.Assignment(context.capacity,
                    b.Mul(context.capacity, integer(2)))}));
            growth.push_back(allocate(context.buffer, context.capacity));
            growth.push_back(b.If(b.Gt(context.used, integer(0)),
                {copy_prefix(context.buffer, context.scratch),
                 b.Deallocate(context.scratch)}, {}));
            body.push_back(al, b.If(b.Eq(context.used, context.capacity), growth, {}));
            body.push_back(al, b.Assignment(context.used,
                b.Add(context.used, integer(1))));
            body.push_back(al, b.Assignment(
                PassUtils::create_array_ref(context.buffer, context.used,
                    al, current_scope), value));
        };
        PassUtils::ReplacerUtils::ConstructorLeaf append =
            [&](ASR::expr_t* value, Vec<ASR::stmt_t*>& body,
                    bool perform_cast, ASR::cast_kindType cast_kind,
                    ASR::ttype_t* casted_type) {
                prepare_constructor_leaf(value, body);
                if (ASRUtils::is_array(ASRUtils::expr_type(value))) {
                    ASR::ttype_t* type = ASRUtils::duplicate_type_with_empty_dims(
                        al, ASRUtils::type_get_past_allocatable_pointer(
                            ASRUtils::expr_type(value)),
                        ASR::array_physical_typeType::DescriptorArray, true);
                    ASR::expr_t* leaf = variable("_constructor_leaf",
                        ASRUtils::TYPE(ASR::make_Allocatable_t(al, loc, type)));
                    // This is an internal owning snapshot, independent of the
                    // user's allocation-on-assignment option.
                    body.push_back(al, ASRUtils::STMT(
                        ASRUtils::make_Assignment_t_util(al, loc, leaf, value,
                            nullptr, true, false)));
                    Vec<ASR::expr_t*> indices;
                    Vec<ASR::stmt_t*> inner;
                    PassUtils::ReplacerUtils::create_do_loop(al, loc,
                        ASRUtils::extract_n_dims_from_ttype(type), leaf,
                        indices, inner, [&]() {
                            append_scalar(PassUtils::create_array_ref(
                                leaf, indices, al, current_scope, perform_cast,
                                cast_kind, casted_type), inner);
                        }, current_scope, &body);
                    body.push_back(al, b.Deallocate(leaf));
                } else {
                    if (perform_cast && !ASRUtils::types_equal(
                            ASRUtils::expr_type(value), casted_type, nullptr, nullptr)) {
                        value = ASRUtils::EXPR(ASR::make_Cast_t(al, loc, value,
                            cast_kind, casted_type, nullptr, nullptr));
                    }
                    std::string name = current_scope->get_unique_name("_constructor_leaf");
                    ASR::stmt_t* assignment;
                    ASR::expr_t* leaf = PassUtils::create_auxiliary_variable_for_expr(
                        value, name, al, current_scope, assignment);
                    body.push_back(al, assignment);
                    append_scalar(leaf, body);
                }
            };
        PassUtils::ReplacerUtils::ConstructorPrepare prepare_control =
            [&](ASR::expr_t*& value, Vec<ASR::stmt_t*>& body) {
                prepare_constructor_leaf(value, body);
            };
        PassUtils::ReplacerUtils::visit_ArrayConstructor(x, al, context.buffer,
            &pass_result, context.used, current_scope, false,
            ASR::cast_kindType::IntegerToInteger, nullptr,
            skip_idl_save_restore, &append, &prepare_control);
        pass_result.push_back(al, b.Deallocate(result));
        pass_result.push_back(al, allocate(result, context.used));
        pass_result.push_back(al, copy_prefix(result, context.buffer));
        pass_result.push_back(al, b.Deallocate(context.buffer));
        constructor_results.push_back(result);
        *current_expr = result;
        remove_original_statement = false;
    }

    void replace_ArrayConstructor(ASR::ArrayConstructor_t* x) {
        // The statements that evaluate a constructor are put in front of
        // the FORALL, where its indices are not defined. A constructor that
        // depends on them is materialized when FORALL has lowered its loops.
        if (depends_on_forall_index(&x->base)) {
            return;
        }
        ASR::ttype_t* scalar_type = ASRUtils::extract_type(x->m_type);
        bool numeric = ASR::is_a<ASR::Integer_t>(*scalar_type) ||
            ASR::is_a<ASR::Real_t>(*scalar_type) ||
            ASR::is_a<ASR::Complex_t>(*scalar_type) ||
            ASR::is_a<ASR::Logical_t>(*scalar_type);
        ASR::Array_t* array_type = ASR::down_cast<ASR::Array_t>(
            ASRUtils::type_get_past_allocatable_pointer(x->m_type));
        if (is_runtime_constructor(x)) {
            materialize_dynamic_constructor(x);
            return;
        }
        // Skip ArrayConstructor replacement when any argument is a function
        // call (FunctionCall or IntrinsicElementalFunction) returning an array.
        // The implied_do_loop pass incorrectly expands these into do-loops,
        // generating invalid code (e.g. lbound(Nint(data), ...) or calling
        // functions multiple times). The downstream passes
        // (array_struct_temporary + array_op) handle these cases properly.
        for( size_t i = 0; i < x->n_args; i++ ) {
            ASR::expr_t* arg = x->m_args[i];
            if( arg == nullptr ) continue;
            bool is_array_func_call = false;
            if( ASR::is_a<ASR::FunctionCall_t>(*arg) &&
                ASRUtils::is_array(ASRUtils::expr_type(arg)) ) {
                is_array_func_call = true;
            } else if( ASR::is_a<ASR::IntrinsicElementalFunction_t>(*arg) &&
                       ASRUtils::is_array(ASRUtils::expr_type(arg)) ) {
                is_array_func_call = true;
            } else if( ASR::is_a<ASR::IntrinsicArrayFunction_t>(*arg) &&
                       ASRUtils::is_array(ASRUtils::expr_type(arg)) ) {
                is_array_func_call = true;
            }
            if( is_array_func_call ) {
                return;
            }
        }
        const Location& loc = x->base.base.loc;
        ASR::expr_t* result_var_copy = result_var;
        bool is_result_var_fixed_size = false;
        if (result_var != nullptr && !ASR::is_a<ASR::ImpliedDoLoop_t>(*result_var) &&
            resultvar2value.find(result_var) != resultvar2value.end() &&
            resultvar2value[result_var] == &(x->base)) {
            is_result_var_fixed_size = ASRUtils::is_fixed_size_array(ASRUtils::expr_type(result_var));
        }
        ASR::ttype_t* result_type_ = nullptr;
        bool is_allocatable = false;
        ASR::expr_t* array_constructor = nullptr;
        if (materialize_constants && ASRUtils::is_fixed_size_array(x->m_type)) {
            // Use the constructor's extent, independently of its values or destination.
            array_constructor = make_ConstantWithKind(make_IntegerConstant_t,
                make_Integer_t, ASRUtils::get_fixed_size_of_array(x->m_type),
                get_index_kind(), loc);
        } else {
            array_constructor = get_ArrayConstructor_size(x, is_allocatable);
        }

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
        if( is_result_var_fixed_size && !materialize_constants ) {
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
        } else if (x->n_args > 0 && x->m_args[0]) {
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
        arg.m_codims = nullptr;
        arg.n_codims = 0;
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
                al, loc, to_be_deallocated.p, to_be_deallocated.size(), nullptr, nullptr)));
            ASR::stmt_t* allocate_stmt = ASRUtils::STMT(ASR::make_Allocate_t(
                al, loc, alloc_args.p, alloc_args.size(), nullptr, nullptr, nullptr));
            pass_result.push_back(al, allocate_stmt);
        }
        if ( allocate_target && realloc_lhs && !materialize_constants ) {
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
                al, loc, to_be_deallocated.p, to_be_deallocated.size(), nullptr, nullptr)));
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
        ASR::expr_t* constructor_index = nullptr;
        PassUtils::ReplacerUtils::ConstructorPrepare prepare_control =
            [&](ASR::expr_t*& value, Vec<ASR::stmt_t*>& body) {
                prepare_constructor_leaf(value, body);
            };
        PassUtils::ReplacerUtils::ConstructorLeaf prepare =
            [&](ASR::expr_t* value, Vec<ASR::stmt_t*>& body,
                    bool perform_cast, ASR::cast_kindType cast_kind,
                    ASR::ttype_t* casted_type) {
                prepare_constructor_leaf(value, body);
                ASR::ArrayConstructor_t leaf = *x;
                leaf.m_args = &value;
                leaf.n_args = 1;
                PassUtils::ReplacerUtils::visit_ArrayConstructor(&leaf, al,
                    result_var, &body, constructor_index, current_scope,
                    perform_cast, cast_kind, casted_type,
                    skip_idl_save_restore, nullptr, nullptr);
            };
        class RuntimeConstructorVisitor:
            public ASR::BaseWalkVisitor<RuntimeConstructorVisitor> {
        public:
            bool found = false;
            void visit_ttype(const ASR::ttype_t&) {}
            void visit_ArrayConstructor(const ASR::ArrayConstructor_t& x) {
                if (is_runtime_constructor(const_cast<ASR::ArrayConstructor_t*>(&x))) {
                    found = true;
                } else {
                    for (size_t i = 0; i < x.n_args; i++) visit_expr(*x.m_args[i]);
                }
            }
        } nested;
        nested.visit_compile_time_value = false;
        nested.visit_ArrayConstructor(*x);
        if (numeric && array_type->m_memory_space == ASR::memory_spaceType::Global &&
                nested.found) {
            Vec<ASR::expr_t*> indices;
            PassUtils::create_idx_vars(indices, 1, loc, al, current_scope,
                "__libasr_index_");
            constructor_index = indices[0];
            ASRUtils::ASRBuilder b(al, loc);
            pass_result.push_back(al, b.Assignment(constructor_index,
                PassUtils::get_bound(result_var, 1, "lbound", al)));
            PassUtils::ReplacerUtils::visit_ArrayConstructor(x, al, result_var,
                result_vec, constructor_index, current_scope, false,
                ASR::cast_kindType::IntegerToInteger, nullptr,
                skip_idl_save_restore, &prepare, &prepare_control);
        } else {
            PassUtils::ReplacerUtils::replace_ArrayConstructor_(al, x, result_var,
                result_vec, current_scope, false, ASR::cast_kindType::IntegerToInteger,
                nullptr, skip_idl_save_restore);
        }
        result_var = result_var_copy;
    }

    void replace_StructConstant(ASR::StructConstant_t* /*x*/) {
        // A StructConstant is emitted as static data, so its array
        // arguments stay constants and are never expanded into temporaries
    }

    // A rank one constant of intrinsic numeric or logical type that is the
    // whole value of an assignment is named as a named constant (parameter),
    // which backends keep as static data. Filling a temporary with it
    // instead takes code for each element, for every such assignment.
    bool is_named_array_constant(ASR::ArrayConstant_t* x, bool is_assignment_value) {
        if (materialize_constants || !is_assignment_value || !PassUtils::is_nameable_array_constant(x)) {
            return false;
        }
        ASR::ttype_t* element_type = ASRUtils::extract_type(x->m_type);
        return ASR::is_a<ASR::Integer_t>(*element_type) ||
            ASR::is_a<ASR::Real_t>(*element_type) ||
            ASR::is_a<ASR::Complex_t>(*element_type) ||
            ASR::is_a<ASR::Logical_t>(*element_type);
    }

    void replace_ArrayConstant(ASR::ArrayConstant_t* x) {
        const Location& loc = x->base.base.loc;
        ASR::expr_t* result_var_copy = result_var;
        bool is_result_var_fixed_size = false;
        bool is_assignment_value = false;
        if (result_var != nullptr &&
            resultvar2value.find(result_var) != resultvar2value.end() &&
            resultvar2value[result_var] == &(x->base)) {
            is_assignment_value = true;
            is_result_var_fixed_size = ASRUtils::is_fixed_size_array(ASRUtils::expr_type(result_var));
        }
        bool is_named_constant = is_named_array_constant(x, is_assignment_value);
        ASR::ttype_t* result_type_ = nullptr;
        bool is_allocatable = false;
        ASR::expr_t* array_constant_size = get_ArrayConstant_size(x);
        ASR::dimension_t* orig_m_dims = nullptr;
        size_t orig_n_dims = ASRUtils::extract_dimensions_from_ttype(x->m_type, orig_m_dims);
        Vec<ASR::dimension_t> dims;
        if (orig_n_dims > 1 && ASRUtils::is_fixed_size_array(x->m_type)) {
            dims.reserve(al, orig_n_dims);
            for (size_t i = 0; i < orig_n_dims; i++) {
                dims.push_back(al, orig_m_dims[i]);
            }
        } else {
            dims.reserve(al, 1);
            ASR::dimension_t dim;
            dim.loc = loc;
            dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 1,
                            ASRUtils::type_get_past_pointer(
                                ASRUtils::type_get_past_allocatable(
                                    ASRUtils::expr_type(array_constant_size)))));
            dim.m_length = array_constant_size;
            dims.push_back(al, dim);
        }
        remove_original_statement = false;
        if( is_result_var_fixed_size && !materialize_constants ) {
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
        // A temporary of a derived type is declared by the type's symbol, so
        // an expression naming that type is passed along.
        ASR::expr_t* struct_expr = nullptr;
        if (ASRUtils::is_struct(*result_type_)) {
            struct_expr = ASRUtils::EXPR((ASR::asr_t*) x);
        }
        if (is_named_constant) {
            result_var = PassUtils::create_named_array_constant(al, x, current_scope);
        } else {
            result_var = PassUtils::create_var(result_counter, "_array_constant_",
                            loc, result_type_, al, current_scope, struct_expr);
        }
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
        arg.m_codims = nullptr;
        arg.n_codims = 0;
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
                al, loc, to_be_deallocated.p, to_be_deallocated.size(), nullptr, nullptr)));
            ASR::stmt_t* allocate_stmt = ASRUtils::STMT(ASR::make_Allocate_t(
                al, loc, alloc_args.p, alloc_args.size(), nullptr, nullptr, nullptr));
            pass_result.push_back(al, allocate_stmt);
        }
        if ( allocate_target && realloc_lhs && !materialize_constants ) {
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
                al, loc, to_be_deallocated.p, to_be_deallocated.size(), nullptr, nullptr)));
            ASR::stmt_t* allocate_stmt = ASRUtils::STMT(ASR::make_Allocate_t(
                al, loc, alloc_args.p, alloc_args.size(), nullptr, nullptr, nullptr));
            pass_result.push_back(al, allocate_stmt);
        }
        LCOMPILERS_ASSERT(result_var != nullptr);
        if (!is_named_constant) {
            Vec<ASR::stmt_t*>* result_vec = &pass_result;
            PassUtils::ReplacerUtils::replace_ArrayConstant(x, this,
                remove_original_statement, result_vec);
        }
        result_var = result_var_copy;
    }

    void replace_IntrinsicArrayFunction(ASR::IntrinsicArrayFunction_t* x) {
        if (materialize_constants && x->m_value &&
                ASR::is_a<ASR::ArrayConstant_t>(*x->m_value)) {
            // Materialize the whole folded result before IO preparation can
            // capture it as a parameter initializer, not its cached metadata.
            replace_ArrayConstant(ASR::down_cast<ASR::ArrayConstant_t>(x->m_value));
            return;
        }
        ASR::BaseExprReplacer<ReplaceArrayConstant>::replace_IntrinsicArrayFunction(x);
    }

    void replace_ArrayPhysicalCast(ASR::ArrayPhysicalCast_t* x) {
        [[maybe_unused]] bool is_arr_construct_arg = ASR::is_a<ASR::ArrayConstructor_t>(*x->m_arg);
        ASR::BaseExprReplacer<ReplaceArrayConstant>::replace_ArrayPhysicalCast(x);
        if (!ASRUtils::is_array(ASRUtils::expr_type(x->m_arg))) {
            return;
        }
        if( x->m_old != ASRUtils::extract_physical_type(ASRUtils::expr_type(x->m_arg)) ) {
            x->m_old = ASRUtils::extract_physical_type(ASRUtils::expr_type(x->m_arg));
        }
        if( (is_arr_construct_arg && ASRUtils::is_fixed_size_array(ASRUtils::expr_type(x->m_arg))
                && x->m_old == x->m_new) ){
            *current_expr = x->m_arg;
            return;
        }

        bool will_collapse = (x->m_old == x->m_new &&
             x->m_old != ASR::array_physical_typeType::DescriptorArray) ||
            (x->m_old == x->m_new && x->m_old == ASR::array_physical_typeType::DescriptorArray &&
            (ASR::is_a<ASR::Allocatable_t>(*ASRUtils::expr_type(x->m_arg)) ||
            ASR::is_a<ASR::Pointer_t>(*ASRUtils::expr_type(x->m_arg)))) ||
            x->m_old != ASRUtils::extract_physical_type(ASRUtils::expr_type(x->m_arg));
        if (will_collapse) {
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
        ASR::expr_t* m_rec = nullptr;
        ASR::expr_t* m_iomsg = nullptr;
        ASR::expr_t* m_iostat = nullptr;
        ASR::expr_t* m_pos = nullptr;
        ASR::expr_t* m_decimal = nullptr;
        bool m_is_formatted = true;
        ReplaceArrayConstant replacer;
        Vec<ASR::stmt_t*> pass_result;
        Vec<ASR::stmt_t*>* parent_body;
        std::map<ASR::expr_t*, ASR::expr_t*> resultvar2value;
        const LCompilers::PassOptions& pass_options;

    public:

        int get_index_kind() const {
            return pass_options.descriptor_index_64 ? 8 : 4;
        }

        ArrayConstantVisitor(Allocator& al_, const LCompilers::PassOptions& pass_options_,
                bool materialize_constants) :
        al(al_), remove_original_statement(false),
        replacer(al_, pass_result, remove_original_statement,
            resultvar2value, pass_options_.realloc_lhs_arrays, allocate_target, pass_options_),
        parent_body(nullptr), pass_options(pass_options_) {
            call_replacer_on_value = false;
            replacer.call_replacer_on_value = false;
            replacer.materialize_constants = materialize_constants;
            pass_result.n = 0;
            pass_result.reserve(al, 0);
            print = false, file_write = false;
            m_unit = nullptr;
            m_rec = nullptr;
            m_iomsg = nullptr;
            m_iostat = nullptr;
            m_pos = nullptr;
            m_decimal = nullptr;
            m_is_formatted = true;
        }

        void visit_Variable(const ASR::Variable_t& /*x*/) {
            // Do nothing, already handled in init_expr pass
        }

        void visit_StructConstant(const ASR::StructConstant_t& /*x*/) {
            // Its arguments are constants emitted as static data
        }

        void visit_ttype(const ASR::ttype_t& /*x*/) {
            // Do nothing. Array constants that appear inside type metadata
            // (e.g. dynamic-length strings) are not executable statements.
            // Replacing them here creates dangling local variables.
        }

        void call_replacer() {
            replacer.current_expr = current_expr;
            replacer.current_scope = current_scope;
            replacer.replace_expr(*current_expr);
            if (replacer.constructor_scope) {
                current_scope = replacer.constructor_scope;
            }
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

        // Check if the values of an implied do loop hold character values that
        // do not all share one and the same compile time length. Such a loop
        // must not be gathered into a temporary array: an array has a single
        // element length, so every character value would be padded (or
        // truncated) to it. Each value has to be emitted at its own length
        // instead.
        bool implied_do_loop_has_varying_string_length(ASR::ImpliedDoLoop_t* idl) {
            size_t string_values = 0;
            int64_t common_len = -1;
            bool has_runtime_len = false;
            for (size_t i = 0; i < idl->n_values; i++) {
                ASR::expr_t* value = idl->m_values[i];
                if (ASR::is_a<ASR::ImpliedDoLoop_t>(*value)) {
                    // A nested loop carries its own values, leave it alone.
                    return false;
                }
                ASR::ttype_t* value_type = ASRUtils::extract_type(
                    ASRUtils::expr_type(value));
                if (!ASR::is_a<ASR::String_t>(*value_type)) {
                    continue;
                }
                string_values += 1;
                ASR::expr_t* len = ASR::down_cast<ASR::String_t>(value_type)->m_len;
                int64_t len_value = -1;
                if (len == nullptr ||
                    !ASRUtils::extract_value(ASRUtils::expr_value(len), len_value)) {
                    has_runtime_len = true;
                    continue;
                }
                if (common_len == -1) {
                    common_len = len_value;
                } else if (common_len != len_value) {
                    return true;
                }
            }
            if (string_values < 2) {
                return false;
            }
            // A length known only at runtime cannot be shown to agree with the
            // lengths of the other values.
            return has_runtime_len;
        }

        void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
            Vec<ASR::stmt_t*> body;
            body.reserve(al, n_body);
            Vec<ASR::stmt_t*> enclosing_preparation;
            enclosing_preparation.reserve(al, pass_result.size());
            if (replacer.constructor_scope) {
                for (ASR::stmt_t* stmt: pass_result) {
                    enclosing_preparation.push_back(al, stmt);
                }
            } else if( parent_body ) {
                for (size_t j=0; j < pass_result.size(); j++) {
                    parent_body->push_back(al, pass_result[j]);
                }
            }

            for (size_t i = 0; i < n_body; i++) {
                SymbolTable* scope_before = current_scope;
                SymbolTable* outer_constructor_scope = replacer.constructor_scope;
                auto outer_results = std::move(replacer.constructor_results);
                replacer.constructor_scope = nullptr;
                replacer.constructor_results.clear();
                pass_result.n = 0;
                pass_result.reserve(al, 1);
                remove_original_statement = false;
                replacer.result_var = nullptr;
                Vec<ASR::stmt_t*>* parent_body_copy = parent_body;
                parent_body = &body;
                visit_stmt(*m_body[i]);
                parent_body = parent_body_copy;
                if (replacer.constructor_scope) {
                    Vec<ASR::stmt_t*> prepared;
                    prepared.reserve(al, pass_result.size() + 1);
                    for (size_t j = 0; j < pass_result.size(); j++) {
                        prepared.push_back(al, pass_result[j]);
                    }
                    if (!remove_original_statement) {
                        prepared.push_back(al, m_body[i]);
                    }
                    ASRUtils::ASRBuilder b(al, m_body[i]->base.loc);
                    for (ASR::expr_t* result: replacer.constructor_results) {
                        prepared.push_back(al, b.Deallocate(result));
                    }
                    std::string name = scope_before->get_unique_name("_constructor_activation");
                    ASR::symbol_t* block = ASR::down_cast<ASR::symbol_t>(
                        ASR::make_Block_t(al, m_body[i]->base.loc,
                            replacer.constructor_scope, s2c(al, name),
                            prepared.p, prepared.n));
                    scope_before->add_symbol(name, block);
                    body.push_back(al, ASRUtils::STMT(ASR::make_BlockCall_t(
                        al, m_body[i]->base.loc, -1, block)));
                } else {
                    for (size_t j = 0; j < pass_result.size(); j++) {
                        body.push_back(al, pass_result[j]);
                    }
                    if (!remove_original_statement) {
                        body.push_back(al, m_body[i]);
                    }
                }
                current_scope = scope_before;
                replacer.constructor_scope = outer_constructor_scope;
                replacer.constructor_results = std::move(outer_results);
                remove_original_statement = false;
            }
            m_body = body.p;
            n_body = body.size();
            replacer.result_var = nullptr;
            pass_result = enclosing_preparation;
        }

        void visit_WhileLoop(const ASR::WhileLoop_t& x) {
            ASR::WhileLoop_t& loop = const_cast<ASR::WhileLoop_t&>(x);
            ASR::expr_t** saved_expr = current_expr;
            current_expr = &loop.m_test;
            call_replacer();
            current_expr = saved_expr;
            bool constructor_in_test = replacer.constructor_scope != nullptr;
            transform_stmts(loop.m_body, loop.n_body);
            transform_stmts(loop.m_orelse, loop.n_orelse);
            if (!constructor_in_test) return;

            const Location& loc = x.base.base.loc;
            ASRUtils::ASRBuilder b(al, loc);
            ASR::expr_t* test = PassUtils::create_var(replacer.result_counter++,
                "_constructor_test", loc, ASRUtils::expr_type(loop.m_test),
                al, current_scope);
            Vec<ASR::stmt_t*> iteration;
            iteration.reserve(al, pass_result.size() + loop.n_body + 2);
            for (ASR::stmt_t* stmt: pass_result) {
                iteration.push_back(al, stmt);
            }
            iteration.push_back(al, b.Assignment(test, loop.m_test));
            for (ASR::expr_t* result: replacer.constructor_results) {
                iteration.push_back(al, b.Deallocate(result));
            }
            replacer.constructor_results.clear();
            iteration.push_back(al, b.If(b.Not(test),
                {ASRUtils::STMT(ASR::make_Exit_t(al, loc, loop.m_name))}, {}));
            for (size_t i = 0; i < loop.n_body; i++) {
                iteration.push_back(al, loop.m_body[i]);
            }
            loop.m_test = b.logical_true();
            loop.m_body = iteration.p;
            loop.n_body = iteration.n;
            pass_result.n = 0;
        }

        void visit_ForAllSingle(const ASR::ForAllSingle_t &x) {
            visit_do_loop_head(x.m_head);
            replacer.forall_indices.push_back(
                ASR::down_cast<ASR::Var_t>(x.m_head.m_v)->m_v);
            visit_stmt(*x.m_assign_stmt);
            replacer.forall_indices.pop_back();
        }

        // A shape check only reads the shapes of its operands; a constructor
        // there carries its size in its type and is never evaluated
        void visit_DebugCheckArrayBounds(const ASR::DebugCheckArrayBounds_t& /*x*/) {
        }

        void visit_Assignment(const ASR::Assignment_t &x) {
            if( ASR::is_a<ASR::Pointer_t>(*ASRUtils::expr_type(x.m_target)) &&
                ASR::is_a<ASR::GetPointer_t>(*x.m_value) ) {
                return ;
            }

            if (replacer.materialize_constants && !x.m_overloaded &&
                    ASR::is_a<ASR::ArrayReshape_t>(*x.m_value)) {
                ASR::expr_t* value =
                    ASR::down_cast<ASR::ArrayReshape_t>(x.m_value)->m_value;
                if (value && ASR::is_a<ASR::ArrayConstant_t>(*value) &&
                        ASRUtils::is_fixed_size_array(ASRUtils::expr_type(value))) {
                    // Materialize the folded result with its result rank and
                    // element order, not the SOURCE and SHAPE operands.
                    const_cast<ASR::Assignment_t&>(x).m_value = value;
                }
            }

            if( ASR::is_a<ASR::ArrayReshape_t>(*x.m_value) ) {
                ASR::ArrayReshape_t* reshape = ASR::down_cast<ASR::ArrayReshape_t>(x.m_value);
                if( ASR::is_a<ASR::ArrayConstructor_t>(*reshape->m_array) ) {
                    ASR::ArrayConstructor_t* arr_con =
                        ASR::down_cast<ASR::ArrayConstructor_t>(reshape->m_array);
                    bool has_implied_do = false;
                    for( size_t i = 0; i < arr_con->n_args; i++ ) {
                        if( ASR::is_a<ASR::ImpliedDoLoop_t>(*arr_con->m_args[i]) ) {
                            has_implied_do = true;
                            break;
                        }
                    }
                    if( has_implied_do || ReplaceArrayConstant::is_runtime_constructor(arr_con) ) {
                        ASR::expr_t* result_var_copy = replacer.result_var;
                        replacer.result_var = nullptr;
                        ASR::expr_t** current_expr_copy = current_expr;
                        current_expr = &(const_cast<ASR::ArrayReshape_t*>(reshape)->m_array);
                        this->call_replacer();
                        current_expr = current_expr_copy;
                        replacer.result_var = result_var_copy;
                    }
                }
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

            ASR::ttype_t* array_type;
            if (ASRUtils::is_array(ASRUtils::expr_type(value))) {
                array_type = ASRUtils::expr_type(value);
            } else {
                array_type = ASRUtils::TYPE(ASR::make_Array_t(al, value->base.loc, ASRUtils::expr_type(value), dim.p, dim.size(), ASR::array_physical_typeType::FixedSizeArray, ASR::memory_spaceType::Global));
            }
            ASR::asr_t* array_constant = ASRUtils::make_ArrayConstructor_t_util(al, value->base.loc,
                                        args.p, args.n, array_type, ASR::arraystorageType::ColMajor);
            return array_constant;
        }

        // The values of `idl` with the loop variables replaced by their
        // values (see `ASRUtils::expand_implied_do_loop`), if it can be
        // expanded at compile time.
        bool expand_implied_do_loop_flat(ASR::ImpliedDoLoop_t* idl, Vec<ASR::expr_t*>& result) {
            return ASRUtils::expand_implied_do_loop(al, idl, result).blocker ==
                ASRUtils::ImpliedDoExpansionBlocker::None;
        }

        // I/O items may have different character lengths. Store each in a
        // descriptor member, rather than padding them into a character array,
        // so one StringFormat consumes the entire implied-do and its siblings.
        ASR::expr_t* wrap_character_io_items(ASR::expr_t* value) {
            if (ASR::is_a<ASR::ImpliedDoLoop_t>(*value)) {
                ASR::ImpliedDoLoop_t* idl = ASR::down_cast<ASR::ImpliedDoLoop_t>(value);
                Vec<ASR::expr_t*> values;
                values.reserve(al, idl->n_values);
                for (size_t i = 0; i < idl->n_values; i++) {
                    values.push_back(al, wrap_character_io_items(idl->m_values[i]));
                }
                return ASRUtils::EXPR(ASR::make_ImpliedDoLoop_t(al,
                    value->base.loc, values.p, values.size(), idl->m_var,
                    idl->m_start, idl->m_end, idl->m_increment,
                    ASRUtils::expr_type(values[0]), nullptr));
            }
            const Location& loc = value->base.loc;
            int kind = ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(value));
            std::string name = "~io_character_item_" + std::to_string(kind);
            SymbolTable* global_scope = current_scope;
            while (global_scope->parent) global_scope = global_scope->parent;
            ASR::symbol_t* sym = global_scope->get_symbol(name);
            if (!sym) {
                SymbolTable* members_scope = al.make_new<SymbolTable>(global_scope);
                ASRUtils::ASRBuilder b(al, loc);
                b.Variable(members_scope, "text", ASRUtils::TYPE(
                    ASR::make_Allocatable_t(al, loc, ASRUtils::TYPE(
                        ASR::make_String_t(al, loc, kind, nullptr,
                            ASR::DeferredLength, ASR::DescriptorString)))), ASR::Local);
                Vec<char*> members;
                members.reserve(al, 1);
                members.push_back(al, s2c(al, "text"));
                sym = ASR::down_cast<ASR::symbol_t>(ASR::make_Struct_t(al,
                    loc, members_scope, s2c(al, name), nullptr, nullptr, 0,
                    members.p, members.size(), nullptr, 0, ASR::Source,
                    ASR::Public, false, false, false, nullptr, 0, nullptr,
                    nullptr, nullptr, 0));
                ASR::down_cast<ASR::Struct_t>(sym)->m_struct_signature =
                    ASRUtils::make_StructType_t_util(al, loc, sym, true);
                global_scope->add_symbol(name, sym);
            }
            Vec<ASR::call_arg_t> args;
            args.reserve(al, 1);
            ASR::call_arg_t arg;
            arg.loc = loc;
            arg.m_value = value;
            args.push_back(al, arg);
            return ASRUtils::EXPR(ASR::make_StructConstructor_t(al, loc,
                sym, args.p, args.size(), ASRUtils::make_StructType_t_util(
                    al, loc, sym, true), nullptr));
        }

        // Items of different types: one struct per iteration, so one array holds them all.
        ASR::expr_t* wrap_tuple_io_items(ASR::ImpliedDoLoop_t* idl) {
            ASR::expr_t* item = nullptr;
            if (idl->n_values == 1 && ASR::is_a<ASR::ImpliedDoLoop_t>(*idl->m_values[0])) {
                item = wrap_tuple_io_items(ASR::down_cast<ASR::ImpliedDoLoop_t>(idl->m_values[0]));
            } else {
                item = make_io_tuple_item(idl);
            }
            if (!item) {
                return nullptr;
            }
            Vec<ASR::expr_t*> values;
            values.reserve(al, 1);
            values.push_back(al, item);
            return ASRUtils::EXPR(ASR::make_ImpliedDoLoop_t(al, idl->base.base.loc,
                values.p, values.size(), idl->m_var, idl->m_start, idl->m_end,
                idl->m_increment, ASRUtils::expr_type(item), nullptr));
        }

        ASR::expr_t* make_io_tuple_item(ASR::ImpliedDoLoop_t* idl) {
            const Location& loc = idl->base.base.loc;
            Vec<ASR::ttype_t*> member_types;
            member_types.reserve(al, idl->n_values);
            for (size_t i = 0; i < idl->n_values; i++) {
                ASR::expr_t* value = idl->m_values[i];
                if (ASR::is_a<ASR::ImpliedDoLoop_t>(*value) ||
                        ASRUtils::is_array(ASRUtils::expr_type(value))) {
                    return nullptr;
                }
                ASR::ttype_t* type = ASRUtils::type_get_past_allocatable_pointer(
                    ASRUtils::expr_type(value));
                if (ASR::is_a<ASR::String_t>(*type)) {
                    ASR::String_t* str = ASR::down_cast<ASR::String_t>(type);
                    int64_t len = 0;
                    if (str->m_len == nullptr ||
                            !ASRUtils::extract_value(ASRUtils::expr_value(str->m_len), len)) {
                        return nullptr;
                    }
                    type = ASRUtils::TYPE(ASR::make_String_t(al, loc, str->m_kind,
                        ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, len,
                            ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)))),
                        ASR::ExpressionLength, ASR::DescriptorString));
                } else if (ASR::is_a<ASR::Integer_t>(*type) ||
                        ASR::is_a<ASR::Real_t>(*type) ||
                        ASR::is_a<ASR::Complex_t>(*type) ||
                        ASR::is_a<ASR::Logical_t>(*type)) {
                    type = ASRUtils::duplicate_type(al, type);
                } else {
                    return nullptr;
                }
                member_types.push_back(al, type);
            }
            SymbolTable* global_scope = current_scope;
            while (global_scope->parent) global_scope = global_scope->parent;
            std::string name = global_scope->get_unique_name("~io_tuple_item");
            SymbolTable* members_scope = al.make_new<SymbolTable>(global_scope);
            ASRUtils::ASRBuilder b(al, loc);
            Vec<char*> members;
            members.reserve(al, member_types.size());
            for (size_t i = 0; i < member_types.size(); i++) {
                std::string member = "item_" + std::to_string(i + 1);
                b.Variable(members_scope, member, member_types[i], ASR::Local);
                members.push_back(al, s2c(al, member));
            }
            ASR::symbol_t* sym = ASR::down_cast<ASR::symbol_t>(ASR::make_Struct_t(al,
                loc, members_scope, s2c(al, name), nullptr, nullptr, 0,
                members.p, members.size(), nullptr, 0, ASR::Source,
                ASR::Public, false, false, false, nullptr, 0, nullptr,
                nullptr, nullptr, 0));
            ASR::down_cast<ASR::Struct_t>(sym)->m_struct_signature =
                ASRUtils::make_StructType_t_util(al, loc, sym, true);
            global_scope->add_symbol(name, sym);
            Vec<ASR::call_arg_t> args;
            args.reserve(al, idl->n_values);
            for (size_t i = 0; i < idl->n_values; i++) {
                ASR::call_arg_t arg;
                arg.loc = idl->m_values[i]->base.loc;
                arg.m_value = idl->m_values[i];
                args.push_back(al, arg);
            }
            return ASRUtils::EXPR(ASR::make_StructConstructor_t(al, loc, sym,
                args.p, args.size(), ASRUtils::make_StructType_t_util(al, loc, sym, true),
                nullptr));
        }

        bool character_io_items(ASR::ImpliedDoLoop_t* idl) {
            for (size_t i = 0; i < idl->n_values; i++) {
                ASR::expr_t* value = idl->m_values[i];
                if (ASR::is_a<ASR::ImpliedDoLoop_t>(*value)) {
                    if (!character_io_items(ASR::down_cast<ASR::ImpliedDoLoop_t>(value))) return false;
                } else if (ASRUtils::is_array(ASRUtils::expr_type(value)) ||
                    !ASRUtils::is_character(*ASRUtils::expr_type(value))) {
                    return false;
                }
            }
            return true;
        }

        // Replace the implied-do argument `i` of a StringFormat whose values
        // are all character (see character_io_items) by an array of
        // descriptor items, so every value is written at its own length.
        void replace_with_character_io_items(ASR::StringFormat_t* string_format,
                size_t i, ASR::expr_t* value) {
            value = wrap_character_io_items(value);
            ASR::asr_t* items = create_array_constant(*string_format, value);
            string_format->m_args[i] = ASRUtils::EXPR(items);
            replacer.result_var = value;
            resultvar2value[value] = ASRUtils::EXPR(items);
            ASR::expr_t** saved_expr = current_expr;
            current_expr = &string_format->m_args[i];
            this->call_replacer();
            current_expr = saved_expr;
            this->visit_expr(*string_format->m_args[i]);
        }

        ASR::stmt_t* create_do_loop_form_idl(ASR::ImpliedDoLoop_t* x, const ASR::StringFormat_t &source) {
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
                        ASR::down_cast<ASR::ImpliedDoLoop_t>(x->m_values[i]), source));
                } else {
                    args.push_back(al, x->m_values[i]);
                }
            }
            
            if (args.size() > 0) {
                ASR::expr_t* fmt_val = ASRUtils::EXPR(ASR::make_StringFormat_t(al,x->base.base.loc, source.m_fmt,
                    args.p, args.size(), ASR::string_format_kindType::FormatFortran,
                    source.m_dtio_bindings, source.n_dtio_bindings, source.m_dtio_unit,
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
                    stmt = ASRUtils::STMT(ASR::make_FileWrite_t(al, x->base.base.loc, 0, m_unit, nullptr, nullptr, nullptr, print_values.p, print_values.size(), nullptr, nullptr, nullptr, true, nullptr, nullptr, nullptr, nullptr, m_decimal));
                }
                do_loop_body.push_back(al, stmt);
            }
            
            do_loop = ASRUtils::STMT(ASR::make_DoLoop_t(al, x->base.base.loc, nullptr, do_loop_head, do_loop_body.p, do_loop_body.size(), nullptr, 0));
            return do_loop;
        }

        ASR::stmt_t* create_do_loop_unformatted_idl(ASR::ImpliedDoLoop_t* x) {
            ASR::do_loop_head_t do_loop_head;
            do_loop_head.loc = x->base.base.loc; do_loop_head.m_v = x->m_var;
            do_loop_head.m_start = x->m_start; do_loop_head.m_end = x->m_end;
            do_loop_head.m_increment = x->m_increment;

            Vec<ASR::stmt_t*> do_loop_body; do_loop_body.reserve(al, 1);

            Vec<ASR::expr_t*> args;
            args.reserve(al, x->n_values);
            for (size_t i = 0; i < x->n_values; i++ ) {
                if ( ASR::is_a<ASR::ImpliedDoLoop_t>(*x->m_values[i]) ) {
                    do_loop_body.push_back(al, create_do_loop_unformatted_idl(
                        ASR::down_cast<ASR::ImpliedDoLoop_t>(x->m_values[i])));
                } else {
                    args.push_back(al, x->m_values[i]);
                }
            }

            if (args.size() > 0) {
                ASR::stmt_t* stmt = ASRUtils::STMT(ASR::make_FileWrite_t(al, x->base.base.loc,
                    0, m_unit, m_iomsg, m_iostat, nullptr,
                    args.p, args.size(), nullptr, nullptr, nullptr,
                    m_is_formatted, nullptr, m_rec, m_pos, nullptr, m_decimal));
                do_loop_body.push_back(al, stmt);
            }

            return ASRUtils::STMT(ASR::make_DoLoop_t(al, x->base.base.loc, nullptr,
                do_loop_head, do_loop_body.p, do_loop_body.size(), nullptr, 0));
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
                bool prev_skip = replacer.skip_idl_save_restore;
                if (pass_options.use_loop_variable_after_loop) {
                    replacer.skip_idl_save_restore = true;
                }
                this->call_replacer();
                current_expr = current_expr_copy_9;
                if( !remove_original_statement ) {
                    this->visit_expr(*print_stmt->m_text);
                }
                replacer.skip_idl_save_restore = prev_skip;
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
                    // Character values of differing lengths cannot share one
                    // temporary array, so expand the loop and keep every value
                    // at its own length.
                    if (implied_do_loop_has_varying_string_length(implied_do_loop)) {
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
                            i = i + expanded_values.size() - 1;
                            continue;
                        }
                    }
                    if (ASR::is_a<ASR::Tuple_t>(*implied_do_loop->m_type)) {
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
                            i = i + expanded_values.size() - 1;
                            continue;
                        }
                        ASR::expr_t* items = wrap_tuple_io_items(implied_do_loop);
                        if (items) {
                            value = items;
                            implied_do_loop = ASR::down_cast<ASR::ImpliedDoLoop_t>(items);
                        }
                    }
                    // Use do-loop approach ONLY for formatted I/O (not list-directed)
                    // with Tuple types or when values contain StringTrim
                    // (to preserve variable-length trimmed strings instead of storing in fixed-length array)
                    bool is_internal_file = false;
                    if (file_write && m_unit != nullptr) {
                        ASR::ttype_t* unit_type = ASRUtils::extract_type(ASRUtils::expr_type(m_unit));
                        is_internal_file = ASR::is_a<ASR::String_t>(*unit_type);
                    }
                    if ( (x.m_fmt != nullptr) && is_internal_file &&
                         implied_do_loop_has_string_trim(implied_do_loop) ) {
                        // Try flat expansion first to preserve sibling args
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
                            i = i + expanded_values.size() - 1;
                            continue;
                        }
                        // Each trimmed value is a separate output item with
                        // its own length, so the whole loop and its siblings
                        // are written by this one StringFormat. Writing the
                        // loop one iteration at a time would start a new
                        // record of the internal file at every iteration.
                        if (character_io_items(implied_do_loop)) {
                            replace_with_character_io_items(string_format_stmt, i, value);
                            continue;
                        }
                    }
                    if ( (x.m_fmt != nullptr) &&
                         (ASR::is_a<ASR::Tuple_t>(*implied_do_loop->m_type) ||
                          (!is_internal_file &&
                           implied_do_loop_has_string_trim(implied_do_loop))) ) {
                        // Try flat expansion first to preserve sibling args.
                        // Values of different types cannot share a temporary
                        // array, so an internal file is expanded too; only a
                        // write per iteration would overwrite it.
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
                            i = i + expanded_values.size() - 1;
                            continue;
                        }
                        if (character_io_items(implied_do_loop)) {
                            replace_with_character_io_items(string_format_stmt, i, value);
                            continue;
                        }
                        if (!is_internal_file) {
                            // Use DO loop to preserve trim() semantics.
                            // When there are sibling args (n_args > 1), merge
                            // non-IDL siblings into the IDL values so each
                            // iteration writes all values together. Skip sibling
                            // ImpliedDoLoops — they have their own bounds and
                            // must produce independent DO loops (otherwise the
                            // nested inner loop overwrites the shared loop
                            // variable).
                            ASR::ImpliedDoLoop_t* idl_for_loop = implied_do_loop;
                            if (x.n_args > 1) {
                                Vec<ASR::expr_t*> merged_values;
                                merged_values.reserve(al, x.n_args + implied_do_loop->n_values - 1);
                                for (size_t j = 0; j < i; j++) {
                                    if (!ASR::is_a<ASR::ImpliedDoLoop_t>(*x.m_args[j])) {
                                        merged_values.push_back(al, x.m_args[j]);
                                    }
                                }
                                for (size_t j = 0; j < implied_do_loop->n_values; j++) {
                                    merged_values.push_back(al, implied_do_loop->m_values[j]);
                                }
                                for (size_t j = i + 1; j < x.n_args; j++) {
                                    if (!ASR::is_a<ASR::ImpliedDoLoop_t>(*x.m_args[j])) {
                                        merged_values.push_back(al, x.m_args[j]);
                                    }
                                }
                                if (merged_values.size() != implied_do_loop->n_values) {
                                    idl_for_loop = ASR::down_cast2<ASR::ImpliedDoLoop_t>(
                                        ASR::make_ImpliedDoLoop_t(al, implied_do_loop->base.base.loc,
                                            merged_values.p, merged_values.size(),
                                            implied_do_loop->m_var,
                                            implied_do_loop->m_start, implied_do_loop->m_end,
                                            implied_do_loop->m_increment,
                                            implied_do_loop->m_type, implied_do_loop->m_value));
                                }
                            }
                            remove_original_statement = true;
                            pass_result.push_back(al, create_do_loop_form_idl(idl_for_loop, x));
                            continue;
                        }
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
                            i = i + expanded_values.size() - 1;
                            continue;
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

        // Collects the variables an expression refers to.
        class VariablesOf : public ASR::BaseWalkVisitor<VariablesOf> {
        public:
            std::set<ASR::symbol_t*> &syms;
            VariablesOf(std::set<ASR::symbol_t*> &syms_) : syms(syms_) {}
            void visit_Var(const ASR::Var_t &x) {
                syms.insert(x.m_v);
            }
        };

        // Whether `e` refers to a variable of `syms`.
        static bool references_any(ASR::expr_t *e,
                const std::set<ASR::symbol_t*> &syms) {
            if (e == nullptr) return false;
            for (ASR::symbol_t *sym : syms) {
                if (ASRUtils::expr_references_symbol(e, sym)) return true;
            }
            return false;
        }

        // A copy of `e` in which the variable of an earlier implied DO loop
        // is replaced by the value it has after its loop.
        ASR::expr_t *with_values_after_loops(ASR::expr_t *e,
                std::map<ASR::symbol_t*, ASR::expr_t*> &after_loop) {
            if (after_loop.empty()) return e;
            ASRUtils::ExprStmtDuplicator dup(al);
            ASR::expr_t *copy = dup.duplicate_expr(e);
            AssociateVarResolver resolver(al, after_loop);
            resolver.current_expr = &copy;
            resolver.replace_expr(copy);
            return copy;
        }

        // The array `a` of `(a(i), i = m, n, s)`, a rank 1 array indexed by
        // the loop variable, or nullptr.
        static ASR::expr_t *implied_do_array(ASR::ImpliedDoLoop_t *idl) {
            if (idl->n_values != 1 || !ASR::is_a<ASR::ArrayItem_t>(*idl->m_values[0])) {
                return nullptr;
            }
            ASR::ArrayItem_t *item = ASR::down_cast<ASR::ArrayItem_t>(idl->m_values[0]);
            if (item->n_args != 1 || item->m_args[0].m_left || item->m_args[0].m_step ||
                    item->m_args[0].m_right == nullptr ||
                    !ASR::is_a<ASR::Var_t>(*item->m_args[0].m_right) ||
                    !ASR::is_a<ASR::Var_t>(*idl->m_var) ||
                    ASR::down_cast<ASR::Var_t>(item->m_args[0].m_right)->m_v !=
                        ASR::down_cast<ASR::Var_t>(idl->m_var)->m_v ||
                    ASRUtils::extract_n_dims_from_ttype(
                        ASRUtils::expr_type(item->m_v)) != 1) {
                return nullptr;
            }
            return item->m_v;
        }

        // A formatted READ from an internal file transfers its items in one
        // call, which reads the record from its start, so its implied DO
        // loops cannot be read one iteration at a time. When every one is
        // `(a(i), i = m, n, s)`, it reads the section `a(m:n:s)` instead,
        // and afterwards `i` has the value it has after the loop,
        // `m + max((n - m + s) / s, 0) * s`. The section is taken before the
        // statement, so a variable of an earlier implied DO loop in its
        // bounds (or in a subscript of a later item) stands for the value it
        // has after that loop, and when its bounds use a variable that an
        // earlier item of the statement reads, the whole array `a` is read
        // instead.
        void read_internal_implied_do_loops(const ASR::FileRead_t &x) {
            ASR::expr_t **values = x.m_values;
            size_t n_values = x.n_values;
            if (ASR::is_a<ASR::IntegerConstant_t>(*x.m_fmt) && n_values == 1 &&
                    ASR::is_a<ASR::StringFormat_t>(*values[0])) {
                ASR::StringFormat_t *sf = ASR::down_cast<ASR::StringFormat_t>(values[0]);
                values = sf->m_args;
                n_values = sf->n_args;
            }
            bool has_implied_do = false;
            for (size_t i = 0; i < n_values; i++) {
                if (ASR::is_a<ASR::ImpliedDoLoop_t>(*values[i])) {
                    if (implied_do_array(ASR::down_cast<ASR::ImpliedDoLoop_t>(
                            values[i])) == nullptr) {
                        return;
                    }
                    has_implied_do = true;
                }
            }
            if (!has_implied_do) return;
            std::set<ASR::symbol_t*> items_read;
            // The value that the variable of an earlier implied DO loop
            // has after its loop, in terms of values before the statement.
            std::map<ASR::symbol_t*, ASR::expr_t*> after_loop;
            // The variables of earlier implied DO loops.
            std::set<ASR::symbol_t*> loop_vars;
            std::vector<ASR::expr_t*> new_values(values, values + n_values);
            std::vector<ASR::stmt_t*> loop_variables;
            for (size_t i = 0; i < n_values; i++) {
                if (!ASR::is_a<ASR::ImpliedDoLoop_t>(*values[i])) {
                    // A loop variable is set after the statement, so a
                    // later item can only use one in a subscript, as the
                    // value it has after its loop.
                    if (ASR::is_a<ASR::Var_t>(*values[i]) && loop_vars.count(
                            ASR::down_cast<ASR::Var_t>(values[i])->m_v)) {
                        return;
                    }
                    for (ASR::symbol_t *var : loop_vars) {
                        if (ASRUtils::expr_references_symbol(values[i], var) &&
                                (after_loop.count(var) == 0 ||
                                references_any(after_loop[var], items_read))) {
                            return;
                        }
                    }
                    new_values[i] = with_values_after_loops(values[i], after_loop);
                    VariablesOf(items_read).visit_expr(*values[i]);
                    continue;
                }
                ASR::ImpliedDoLoop_t *idl = ASR::down_cast<ASR::ImpliedDoLoop_t>(values[i]);
                const Location &loc = idl->base.base.loc;
                ASRUtils::ASRBuilder b(al, loc);
                ASR::expr_t *array = with_values_after_loops(implied_do_array(idl),
                    after_loop);
                ASR::ttype_t *int_type = ASRUtils::expr_type(idl->m_var);
                ASR::symbol_t *var = ASR::down_cast<ASR::Var_t>(idl->m_var)->m_v;
                ASR::expr_t *step = idl->m_increment ? idl->m_increment
                    : b.i_t(1, int_type);
                ASR::expr_t *start = with_values_after_loops(idl->m_start, after_loop);
                ASR::expr_t *end = with_values_after_loops(idl->m_end, after_loop);
                step = with_values_after_loops(step, after_loop);
                bool bounds_read = references_any(start, items_read) ||
                    references_any(end, items_read) ||
                    references_any(step, items_read);
                VariablesOf(items_read).visit_expr(*array);
                loop_vars.insert(var);
                if (bounds_read) {
                    new_values[i] = array;
                    after_loop.erase(var);
                    items_read.insert(var);
                } else {
                    ASRUtils::ExprStmtDuplicator dup(al);
                    after_loop[var] = b.Add(dup.duplicate_expr(start),
                        b.Mul(b.Max(b.Div(b.Add(b.Sub(dup.duplicate_expr(end),
                            dup.duplicate_expr(start)), dup.duplicate_expr(step)),
                            dup.duplicate_expr(step)), b.i_t(0, int_type)),
                            dup.duplicate_expr(step)));
                    items_read.erase(var);
                    Vec<ASR::array_index_t> args; args.reserve(al, 1);
                    ASR::array_index_t ai;
                    ai.loc = loc;
                    ai.m_left = start;
                    ai.m_right = end;
                    ai.m_step = step;
                    args.push_back(al, ai);
                    Vec<ASR::dimension_t> dims; dims.reserve(al, 1);
                    dims.push_back(al, b.set_dim(nullptr, nullptr));
                    ASR::ttype_t *section_type = ASRUtils::make_Array_t_util(al, loc,
                        ASRUtils::duplicate_type(al, ASRUtils::extract_type(
                            ASRUtils::expr_type(array))), dims.p, dims.size());
                    new_values[i] = ASRUtils::EXPR(ASR::make_ArraySection_t(al, loc,
                        array, args.p, args.size(), section_type, nullptr));
                }
                ASRUtils::ExprStmtDuplicator dup(al);
                ASR::expr_t *loop_step = idl->m_increment ? idl->m_increment
                    : b.i_t(1, int_type);
                ASR::expr_t *trip_count = b.Max(b.Div(b.Add(b.Sub(
                    dup.duplicate_expr(idl->m_end), dup.duplicate_expr(idl->m_start)),
                    dup.duplicate_expr(loop_step)), dup.duplicate_expr(loop_step)),
                    b.i_t(0, int_type));
                loop_variables.push_back(b.Assignment(dup.duplicate_expr(idl->m_var),
                    b.Add(dup.duplicate_expr(idl->m_start),
                        b.Mul(trip_count, dup.duplicate_expr(loop_step)))));
            }
            for (size_t i = 0; i < n_values; i++) {
                values[i] = new_values[i];
            }
            remove_original_statement = true;
            pass_result.push_back(al, const_cast<ASR::stmt_t*>(&x.base));
            for (ASR::stmt_t *s : loop_variables) {
                pass_result.push_back(al, s);
            }
        }

        // The variables that expressions use, and whether they call a
        // procedure that is not intrinsic, which may use any variable.
        class UsedVariables : public ASR::BaseWalkVisitor<UsedVariables> {
        public:
            std::set<ASR::symbol_t*> vars;
            bool calls_procedure = false;
            void visit_Var(const ASR::Var_t &x) {
                vars.insert(ASRUtils::symbol_get_past_external(x.m_v));
            }
            void visit_FunctionCall(const ASR::FunctionCall_t &x) {
                ASR::symbol_t *fn = ASRUtils::symbol_get_past_external(x.m_name);
                calls_procedure = calls_procedure || !is_a<ASR::Function_t>(*fn) ||
                    !ASRUtils::is_intrinsic_function2(down_cast<ASR::Function_t>(fn));
                ASR::BaseWalkVisitor<UsedVariables>::visit_FunctionCall(x);
            }
        };

        template <typename T>
        static ASR::expr_t *subscripted_designator(T *x, std::vector<ASR::expr_t*> &uses) {
            if (ASRUtils::is_array_indexed_with_array_indices(x)) return nullptr;
            for (size_t i = 0; i < x->n_args; i++) {
                uses.insert(uses.end(), {x->m_args[i].m_left, x->m_args[i].m_right,
                    x->m_args[i].m_step});
            }
            return x->m_v;
        }

        // Collects the variable that the input item `e` (a variable, or an
        // element, section, component or substring of one) defines, and its
        // components there, into `defined`, and the expressions its address
        // uses into `uses`. Returns false if `e` is not such a designator or
        // has a vector subscript.
        static bool collect_read_item(ASR::expr_t *e, std::vector<ASR::symbol_t*> &defined,
                std::vector<ASR::expr_t*> &uses) {
            while (e) {
                if (is_a<ASR::Var_t>(*e)) {
                    defined.push_back(ASRUtils::symbol_get_past_external(
                        down_cast<ASR::Var_t>(e)->m_v));
                    return true;
                } else if (is_a<ASR::ArrayItem_t>(*e)) {
                    e = subscripted_designator(down_cast<ASR::ArrayItem_t>(e), uses);
                } else if (is_a<ASR::ArraySection_t>(*e)) {
                    e = subscripted_designator(down_cast<ASR::ArraySection_t>(e), uses);
                } else if (is_a<ASR::StructInstanceMember_t>(*e)) {
                    ASR::StructInstanceMember_t *m = down_cast<ASR::StructInstanceMember_t>(e);
                    defined.push_back(ASRUtils::symbol_get_past_external(m->m_m));
                    e = m->m_v;
                } else if (is_a<ASR::StringItem_t>(*e)) {
                    ASR::StringItem_t *si = down_cast<ASR::StringItem_t>(e);
                    uses.push_back(si->m_idx);
                    e = si->m_arg;
                } else if (is_a<ASR::StringSection_t>(*e)) {
                    ASR::StringSection_t *ss = down_cast<ASR::StringSection_t>(e);
                    uses.insert(uses.end(), {ss->m_start, ss->m_end, ss->m_step});
                    e = ss->m_arg;
                } else {
                    return false;
                }
            }
            return false;
        }

        // Whether the variables `a` and `b` may share storage: they are the
        // same, either is a pointer (which includes an ASSOCIATE name and
        // EQUIVALENCE outside common blocks), both have TARGET, or they are
        // in one common block, but not both members of the same view of it,
        // whose members are laid out one after another.
        static bool may_share_storage(ASR::symbol_t *a, ASR::symbol_t *b) {
            if (a == b) return true;
            if (!is_a<ASR::Variable_t>(*a) || !is_a<ASR::Variable_t>(*b)) return false;
            ASR::Variable_t *u = down_cast<ASR::Variable_t>(a);
            ASR::Variable_t *v = down_cast<ASR::Variable_t>(b);
            if (is_a<ASR::Pointer_t>(*u->m_type) || is_a<ASR::Pointer_t>(*v->m_type) ||
                    (u->m_target_attr && v->m_target_attr)) {
                return true;
            }
            ASR::CommonBlock_t *uc = ASRUtils::get_common_block(u);
            ASR::CommonBlock_t *vc = ASRUtils::get_common_block(v);
            auto is_member = [&](ASR::CommonBlock_t *c, ASR::symbol_t *s) {
                return std::find(c->m_members, c->m_members + c->n_members, s) !=
                    c->m_members + c->n_members;
            };
            return uc && vc && std::string(uc->m_link_name) == vc->m_link_name &&
                (uc != vc || !is_member(uc, a) || !is_member(vc, b));
        }

        // Collects the variables that the input items `values` define, and
        // what is evaluated before any value is read: the bounds of their
        // implied DO loops and the subscripts of the items. Returns false
        // for an item that the backend cannot read in a READ with nested
        // implied DO loops: in a loop, anything but a scalar of intrinsic
        // type; anywhere, a real or complex item of a kind other than 4 and
        // 8, which the runtime stores as one of those, a vector-subscripted
        // item, whose copy would be read instead (#15410), or an
        // allocatable scalar that is not a character, which a formatted
        // READ does not store (#15698).
        static bool collect_read_items(ASR::expr_t **values, size_t n_values,
                bool in_loop, std::vector<ASR::symbol_t*> &items,
                std::vector<ASR::expr_t*> &uses) {
            for (size_t i = 0; i < n_values; i++) {
                if (is_a<ASR::ImpliedDoLoop_t>(*values[i])) {
                    ASR::ImpliedDoLoop_t *idl = down_cast<ASR::ImpliedDoLoop_t>(values[i]);
                    uses.insert(uses.end(), {idl->m_var, idl->m_start, idl->m_end,
                        idl->m_increment});
                    if (!collect_read_items(idl->m_values, idl->n_values, true,
                            items, uses)) {
                        return false;
                    }
                    continue;
                }
                ASR::ttype_t *type = ASRUtils::expr_type(values[i]);
                ASR::ttype_t *t = ASRUtils::extract_type(type);
                bool is_real = is_a<ASR::Real_t>(*t) || is_a<ASR::Complex_t>(*t);
                int kind = is_real ? ASRUtils::extract_kind_from_ttype_t(t) : 4;
                bool is_scalar = !ASRUtils::is_array(type) &&
                    !is_a<ASR::Allocatable_t>(*type) && !is_a<ASR::Pointer_t>(*type);
                bool is_intrinsic = is_real || is_a<ASR::Integer_t>(*t) ||
                    is_a<ASR::Logical_t>(*t) || is_a<ASR::String_t>(*t);
                if ((kind != 4 && kind != 8) ||
                        (in_loop && !(is_scalar && is_intrinsic)) ||
                        (is_a<ASR::Allocatable_t>(*type) && !ASRUtils::is_array(type) &&
                            !is_a<ASR::String_t>(*t))) {
                    return false;
                }
                if (!collect_read_item(values[i], items, uses)) return false;
            }
            return true;
        }

        // The backend does a formatted READ with a nested implied DO loop
        // as one data transfer: it runs each implied DO loop of the input
        // list first, collecting the addresses of its items, and then reads
        // the values. So the READ must not define anything those loops use,
        // and the items have to be ones that it can read. Report any other
        // such READ as not yet supported.
        void check_nested_implied_do_read(const ASR::FileRead_t &x) {
            ASR::expr_t **values = x.m_values;
            size_t n_values = x.n_values;
            if (ASR::is_a<ASR::IntegerConstant_t>(*x.m_fmt) && n_values == 1 &&
                    ASR::is_a<ASR::StringFormat_t>(*values[0])) {
                ASR::StringFormat_t *sf = ASR::down_cast<ASR::StringFormat_t>(values[0]);
                values = sf->m_args;
                n_values = sf->n_args;
            }
            if (!ASRUtils::has_nested_implied_do_loop(values, n_values)) return;
            std::vector<ASR::symbol_t*> items;
            std::vector<ASR::expr_t*> uses;
            bool supported = collect_read_items(values, n_values, false, items, uses);
            UsedVariables used;
            for (ASR::expr_t *e : uses) {
                if (e) used.visit_expr(*e);
            }
            supported = supported && !used.calls_procedure;
            for (ASR::symbol_t *item : items) {
                for (ASR::symbol_t *var : used.vars) {
                    supported = supported && !may_share_storage(item, var);
                }
            }
            if (supported) return;
            std::string msg = "this formatted READ with nested implied-do loops "
                "is not yet supported";
            if (pass_options.diagnostics == nullptr) {
                throw LCompilersException(msg);
            }
            pass_options.diagnostics->message_label(msg, {x.base.base.loc},
                "an item may share storage with a loop bound or subscript, "
                "or has a type or kind it cannot read yet",
                diag::Level::Error, diag::Stage::ASRPass);
        }

        void visit_FileRead(const ASR::FileRead_t &x) {
            if (x.m_overloaded) {
                this->visit_stmt(*x.m_overloaded);
                remove_original_statement = false;
                return;
            }
            if (x.m_fmt) {
                check_nested_implied_do_read(x);
            }
            if (x.m_fmt && x.m_unit &&
                    ASRUtils::is_character(*ASRUtils::expr_type(x.m_unit))) {
                read_internal_implied_do_loops(x);
            }
        }

        void visit_FileWrite(const ASR::FileWrite_t &x) {
            file_write = true;
            m_unit = x.m_unit;
            m_rec = x.m_rec;
            m_iomsg = x.m_iomsg;
            m_iostat = x.m_iostat;
            m_pos = x.m_pos;
            m_decimal = x.m_decimal;
            m_is_formatted = x.m_is_formatted;
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
            bool prev_skip = replacer.skip_idl_save_restore;
            if (pass_options.use_loop_variable_after_loop) {
                replacer.skip_idl_save_restore = true;
            }
            ASR::FileWrite_t* write_stmt = const_cast<ASR::FileWrite_t*>(&x);
            for(size_t i = 0; i < x.n_values; i++) {
                ASR::expr_t* value = x.m_values[i];
                if (ASR::is_a<ASR::ImpliedDoLoop_t>(*value)) {
                    ASR::ImpliedDoLoop_t* idl = ASR::down_cast<ASR::ImpliedDoLoop_t>(value);
                    if (ASR::is_a<ASR::Tuple_t>(*idl->m_type)) {
                        Vec<ASR::expr_t*> expanded_values;
                        expanded_values.reserve(al, 16);
                        if (expand_implied_do_loop_flat(idl, expanded_values)) {
                            Vec<ASR::expr_t*> new_values;
                            new_values.reserve(al, x.n_values + expanded_values.size());
                            for (size_t j = 0; j < i; j++) {
                                new_values.push_back(al, x.m_values[j]);
                            }
                            for (size_t j = 0; j < expanded_values.size(); j++) {
                                new_values.push_back(al, expanded_values.p[j]);
                            }
                            for (size_t j = i + 1; j < x.n_values; j++) {
                                new_values.push_back(al, x.m_values[j]);
                            }
                            write_stmt->m_values = new_values.p;
                            write_stmt->n_values = new_values.size();
                            i = i + expanded_values.size() - 1;
                            continue;
                        }
                        remove_original_statement = true;
                        pass_result.push_back(al, create_do_loop_unformatted_idl(idl));
                        continue;
                    }
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
            replacer.skip_idl_save_restore = prev_skip;
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
    ArrayConstantVisitor v(al, pass_options, pass_options.materialize_array_constants);
    v.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

void replace_implied_do_loops_in_body(Allocator &al,
    ASR::stmt_t **&body, size_t &n_body, SymbolTable *scope,
    const LCompilers::PassOptions& pass_options) {
    ArrayConstantVisitor v(al, pass_options, pass_options.materialize_array_constants);
    v.current_scope = scope;
    v.transform_stmts(body, n_body);
}


} // namespace LCompilers
