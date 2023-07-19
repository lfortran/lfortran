#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/replace_implied_do_loops.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/intrinsic_function_registry.h>

#include <vector>
#include <utility>

namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

uint64_t static inline get_hash(ASR::asr_t *node)
{
    return (uint64_t)node;
}

class ReplaceImpliedDoLoops: public ASR::BaseExprReplacer<ReplaceImpliedDoLoops> {

    public:

    Allocator& al;
    Vec<ASR::stmt_t*>& pass_result;
    bool& remove_original_statement;

    SymbolTable* current_scope;
    ASR::stmt_t* parent_stmt;
    ASR::expr_t* result_var;
    ASR::expr_t* idx_var_ = nullptr;
    ASR::expr_t* array_constant_size;
    ASR::expr_t* global_idx_var;
    std::unordered_map<uint64_t, ASR::expr_t*> array_map;
    std::unordered_map<uint64_t, ASR::expr_t*> idx_var_map;
    bool is_array_item = false;
    int result_counter;
    int num_implied_do_loops = 0;
    ASR::ttype_t* target_type = nullptr;
    std::map<ASR::expr_t*, ASR::expr_t*>& resultvar2value;

    ReplaceImpliedDoLoops(Allocator& al_, Vec<ASR::stmt_t*>& pass_result_,
        bool& remove_original_statement_,
        std::map<ASR::expr_t*, ASR::expr_t*>& resultvar2value_) :
    al(al_), pass_result(pass_result_),
    remove_original_statement(remove_original_statement_),
    current_scope(nullptr), result_var(nullptr), idx_var_(nullptr), result_counter(0),
    resultvar2value(resultvar2value_) {}

    void replace_Var(ASR::Var_t* x) {
        uint64_t hash = get_hash((ASR::asr_t*) x->m_v);
        if( idx_var_ && !is_array_item && idx_var_map.find(hash) != idx_var_map.end() ) {
            *current_expr = idx_var_map[hash];
        }
        if (idx_var_ && is_array_item && array_map.find(hash) != array_map.end()) {
            *current_expr = array_map[hash];
        }
    }

    #define BinOpReplacement(Constructor) ASR::expr_t** current_expr_copy = current_expr; \
        current_expr = const_cast<ASR::expr_t**>(&(x->m_left)); \
        this->replace_expr(x->m_left); \
        ASR::expr_t* left = *current_expr; \
        current_expr = current_expr_copy; \
        current_expr = const_cast<ASR::expr_t**>(&(x->m_right)); \
        this->replace_expr(x->m_right); \
        ASR::expr_t* right = *current_expr; \
        current_expr = current_expr_copy; \
        *current_expr = ASRUtils::EXPR(ASR::Constructor(al, x->base.base.loc, \
            left, x->m_op, right, x->m_type, nullptr)); \

    void replace_IntegerBinOp(ASR::IntegerBinOp_t* x) {
        BinOpReplacement(make_IntegerBinOp_t)
    }

    void replace_RealBinOp(ASR::RealBinOp_t* x) {
        BinOpReplacement(make_RealBinOp_t)
    }

    void replace_ArrayItem(ASR::ArrayItem_t* x) {
        is_array_item = true;
        ASR::expr_t** current_expr_copy = current_expr;
        current_expr = const_cast<ASR::expr_t**>(&(x->m_v));
        this->replace_expr(x->m_v);
        current_expr = current_expr_copy;
        is_array_item = false;
        Vec<ASR::array_index_t> args;
        args.reserve(al, x->n_args);
        for (size_t i = 0; i < x->n_args; i++) {
            ASR::array_index_t arg = x->m_args[i];
            ASR::expr_t** current_expr_copy = current_expr;
            current_expr = const_cast<ASR::expr_t**>(&(arg.m_right));
            this->replace_expr(arg.m_right);
            arg.m_right = *current_expr;
            current_expr = current_expr_copy;
            current_expr = const_cast<ASR::expr_t**>(&(arg.m_left));
            this->replace_expr(arg.m_left);
            arg.m_left = *current_expr;
            current_expr = current_expr_copy;
            current_expr = const_cast<ASR::expr_t**>(&(arg.m_step));
            this->replace_expr(arg.m_step);
            arg.m_step = *current_expr;
            current_expr = current_expr_copy;

            args.push_back(al, arg);
        }
        *current_expr = ASRUtils::EXPR(ASR::make_ArrayItem_t(al, x->base.base.loc, x->m_v, args.p, args.n,
                        x->m_type, x->m_storage_format, x->m_value));
    }

    ASR::expr_t* get_ImpliedDoLoop_size(ASR::ImpliedDoLoop_t* implied_doloop) {
        const Location& loc = implied_doloop->base.base.loc;
        ASRUtils::ASRBuilder builder(al, loc);
        ASR::expr_t* start = implied_doloop->m_start;
        ASR::expr_t* end = implied_doloop->m_end;
        ASR::expr_t* d = implied_doloop->m_increment;
        ASR::expr_t* implied_doloop_size = nullptr;
        if( d == nullptr ) {
            implied_doloop_size = builder.ElementalAdd(
                builder.ElementalSub(end, start, loc),
                make_ConstantWithKind(make_IntegerConstant_t, make_Integer_t, 1, 4, loc), loc);
        } else {
            implied_doloop_size = builder.ElementalAdd(builder.ElementalDiv(
                builder.ElementalSub(end, start, loc), d, loc),
                make_ConstantWithKind(make_IntegerConstant_t, make_Integer_t, 1, 4, loc), loc);
        }
        int const_elements = 0;
        for (size_t i = 0; i < implied_doloop->n_values; i++) {
            ASR::expr_t* value = implied_doloop->m_values[i];
            if( !ASR::is_a<ASR::ImpliedDoLoop_t>(*value) ) {
                const_elements += 1;
            }
        }
        ASR::expr_t* implied_doloop_size_constant = nullptr;
        if(  const_elements > 0 ) {
            implied_doloop_size_constant = builder.ElementalMul(
                make_ConstantWithKind(make_IntegerConstant_t, make_Integer_t, const_elements, 4, loc),
                implied_doloop_size, loc);
        }
        ASR::expr_t* implied_doloop_size_nested = nullptr;
        for( size_t i = 0; i < implied_doloop->n_values; i++ ) {
            if( ASR::is_a<ASR::ImpliedDoLoop_t>(*implied_doloop->m_values[i]) ) {
                ASR::expr_t* implied_doloop_size_ = get_ImpliedDoLoop_size(
                    ASR::down_cast<ASR::ImpliedDoLoop_t>(implied_doloop->m_values[i]));
                implied_doloop_size_nested = builder.ElementalMul(implied_doloop_size_, implied_doloop_size, loc);
            }
        }

        if( implied_doloop_size_constant ) {
            if( implied_doloop_size_nested ) {
                implied_doloop_size = builder.ElementalAdd(implied_doloop_size_constant,
                    implied_doloop_size_nested, loc);
            } else {
                implied_doloop_size = implied_doloop_size_constant;
            }
        } else if( implied_doloop_size_nested ) {
            implied_doloop_size = implied_doloop_size_nested;
        }

        return implied_doloop_size;
    }

    size_t get_constant_ArrayConstant_size(ASR::ArrayConstant_t* x) {
        size_t size = 0;
        for( size_t i = 0; i < x->n_args; i++ ) {
            if( ASR::is_a<ASR::ArrayConstant_t>(*x->m_args[i]) ) {
                size += get_constant_ArrayConstant_size(
                    ASR::down_cast<ASR::ArrayConstant_t>(x->m_args[i]));
            } else {
                size += 1;
            }
        }
        return size;
    }

    ASR::expr_t* get_ArrayConstant_size(ASR::ArrayConstant_t* x, bool& is_allocatable) {
        ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x->base.base.loc, 4));
        ASR::expr_t* array_size = nullptr;
        size_t constant_size = 0;
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
                                    ASR::down_cast<ASR::ArrayConstant_t>(element), is_allocatable);
                    if( array_size == nullptr ) {
                        array_size = element_array_size;
                    } else {
                        array_size = builder.ElementalAdd(array_size,
                                        element_array_size, x->base.base.loc);
                    }
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
                            array_size = builder.ElementalAdd(array_size,
                                            element_array_size, x->base.base.loc);
                        }
                    }
                } else {
                    constant_size += 1;
                }
            } else if( ASR::is_a<ASR::ImpliedDoLoop_t>(*element) ) {
                ASR::expr_t* implied_doloop_size = get_ImpliedDoLoop_size(
                    ASR::down_cast<ASR::ImpliedDoLoop_t>(element));
                if( array_size ) {
                    array_size = builder.ElementalAdd(implied_doloop_size, array_size, loc);
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
                    ASR::expr_t* dim_size = builder.ElementalAdd(builder.ElementalDiv(
                        builder.ElementalSub(end, start, loc), d, loc),
                        make_ConstantWithKind(make_IntegerConstant_t, make_Integer_t, 1, 4, loc), loc);
                    if( array_section_size == nullptr ) {
                        array_section_size = dim_size;
                    } else {
                        array_section_size = builder.ElementalMul(array_section_size, dim_size, loc);
                    }
                }
                if( array_size == nullptr ) {
                    array_size = array_section_size;
                } else {
                    builder.ElementalAdd(array_section_size, array_size, loc);
                }
            } else {
                constant_size += 1;
            }
        }
        ASR::expr_t* constant_size_asr = nullptr;
        if( constant_size != 0 ) {
            constant_size_asr = make_ConstantWithType(make_IntegerConstant_t,
                                    constant_size, int_type, x->base.base.loc);
            if( array_size == nullptr ) {
                return constant_size_asr;
            }
        }
        if( constant_size_asr ) {
            array_size = builder.ElementalAdd(array_size, constant_size_asr, x->base.base.loc);
        }
        is_allocatable = true;
        return array_size;
    }

    void replace_ArrayReshape(ASR::ArrayReshape_t* x) {
        if (ASR::is_a<ASR::ImpliedDoLoop_t>(*x->m_array) || ASR::is_a<ASR::ArrayConstant_t>(*x->m_array)) {
            ASR::expr_t* curr_expr_copy = *current_expr;
            replace_expr(x->m_array);
            ASR::expr_t* array = *current_expr;
            *current_expr = curr_expr_copy;

            if (array) {
                *current_expr = ASRUtils::EXPR(ASR::make_ArrayReshape_t(al, x->base.base.loc, array,
                                x->m_shape, x->m_type, x->m_value));
            }
        }
    }


    void replace_ArrayConstant(ASR::ArrayConstant_t* x) {
        if (x->n_args == 1) {
            // As of now only supporting cases like [(i, i=1, 10)] due to #1992
            for(size_t i = 0; i < x->n_args; i++) {
                ASR::expr_t* arg = x->m_args[i];
                if (ASR::is_a<ASR::ImpliedDoLoop_t>(*arg)) {
                    replace_ImpliedDoLoop(ASR::down_cast<ASR::ImpliedDoLoop_t>(arg));
                } else if (ASR::is_a<ASR::Cast_t>(*arg)) {
                    ASR::Cast_t* cast = ASR::down_cast<ASR::Cast_t>(arg);
                    if (ASR::is_a<ASR::ImpliedDoLoop_t>(*cast->m_arg)) {
                        replace_ImpliedDoLoop(ASR::down_cast<ASR::ImpliedDoLoop_t>(cast->m_arg));
                    }
                }
            }
        }
    }

    int get_implied_do_loop_nesting(ASR::ImpliedDoLoop_t* x, int nest) {
        for (size_t i = 0; i < x->n_values; i++) {
            ASR::expr_t* value = x->m_values[i];
            if( ASR::is_a<ASR::ImpliedDoLoop_t>(*value) ) {
                nest = get_implied_do_loop_nesting(
                    ASR::down_cast<ASR::ImpliedDoLoop_t>(value), nest+1);
            }
        }
        return nest;
    }

    ASR::stmt_t* handle_DoLoopCreation(ASR::ImpliedDoLoop_t* x, Vec<ASR::expr_t*> idx_vars, int current_index, ASR::ttype_t* return_type, ASR::expr_t* result_var) {
        ASR::stmt_t* do_loop = nullptr;

        ASR::expr_t* idx_var = idx_vars[current_index];
        ASRUtils::ASRBuilder builder(al, x->base.base.loc);

        // create do loop head
        ASR::do_loop_head_t head;
        head.loc = x->base.base.loc;
        head.m_start = x->m_start;
        head.m_end = x->m_end;
        head.m_increment = x->m_increment;
        head.m_v = idx_var;

        // create do loop body
        Vec<ASR::stmt_t*> do_loop_body;

        // find number of constant values
        int const_elements = 0;
        for (size_t i = 0; i < x->n_values; i++) {
            ASR::expr_t* value = x->m_values[i];
            if( !ASR::is_a<ASR::ImpliedDoLoop_t>(*value) ) {
                const_elements += 1;
            }
        }

        int implied_do_loops = x->n_values - const_elements;
        do_loop_body.reserve(al, const_elements*2 + implied_do_loops);

        for (size_t i = 0; i < x->n_values; i++) {
            Vec<ASR::array_index_t> indices;
            indices.reserve(al, 1);

            ASR::array_index_t index;
            index.loc = x->base.base.loc;
            index.m_left = nullptr;
            index.m_right = global_idx_var;
            index.m_step = nullptr;

            indices.push_back(al, index);
            ASR::asr_t* array_item = ASR::make_ArrayItem_t(al, x->base.base.loc, result_var, indices.p, indices.n, 
                                    return_type, ASR::arraystorageType::ColMajor, nullptr);
            ASR::ttype_t* cast_type = nullptr;
            if (return_type->type == ASR::ttypeType::Array) {
                ASR::Array_t* return_array_type = ASR::down_cast<ASR::Array_t>(return_type);
                cast_type = return_array_type->m_type;
            }
            ASR::expr_t* value = x->m_values[i];
            if (!ASR::is_a<ASR::ImpliedDoLoop_t>(*value)) {
                idx_var_ = idx_vars[current_index];
                ASR::expr_t** current_expr_copy = current_expr;
                current_expr = const_cast<ASR::expr_t**>(&(value));
                replace_expr(*current_expr);
                current_expr = current_expr_copy;
                idx_var_ = nullptr;

                ASR::ttype_t* value_type = ASRUtils::expr_type(value);
                if (ASR::is_a<ASR::Real_t>(*cast_type)) {
                    // Cast to Real
                    if (ASR::is_a<ASR::Integer_t>(*value_type)) {
                        // Cast: IntegerToReal
                        value = ASRUtils::EXPR(ASR::make_Cast_t(al, x->base.base.loc, value, ASR::cast_kindType::IntegerToReal, cast_type, nullptr));
                    } else if (ASR::is_a<ASR::Real_t>(*value_type)) {
                        // Cast: RealToReal
                        value = ASRUtils::EXPR(ASR::make_Cast_t(al, x->base.base.loc, value, ASR::cast_kindType::RealToReal, cast_type, nullptr));
                    }
                } else if (ASR::is_a<ASR::Integer_t>(*cast_type)) {
                    // Cast to Integer
                    if (ASR::is_a<ASR::Integer_t>(*value_type)) {
                        // Cast: IntegerToInteger
                        value = ASRUtils::EXPR(ASR::make_Cast_t(al, x->base.base.loc, value, ASR::cast_kindType::IntegerToInteger, cast_type, nullptr));
                    } else if (ASR::is_a<ASR::Real_t>(*value_type)) {
                        // Cast: RealToInteger
                        value = ASRUtils::EXPR(ASR::make_Cast_t(al, x->base.base.loc, value, ASR::cast_kindType::RealToInteger, cast_type, nullptr));
                    }
                }
                ASR::asr_t* assign = ASR::make_Assignment_t(al, x->base.base.loc, ASRUtils::EXPR(array_item), value, nullptr);
                do_loop_body.push_back(al, ASRUtils::STMT(assign));
                do_loop_body.push_back(al, ASRUtils::STMT(ASR::make_Assignment_t(al, x->base.base.loc, global_idx_var, builder.ElementalAdd(
                    make_ConstantWithKind(make_IntegerConstant_t, make_Integer_t, 1, 4, x->base.base.loc),
                    global_idx_var, x->base.base.loc), nullptr)));
            } else {
                do_loop_body.push_back(al, handle_DoLoopCreation(
                    ASR::down_cast<ASR::ImpliedDoLoop_t>(value), idx_vars, current_index+1, return_type, result_var));
            }
        }

        // create do loop
        do_loop = ASRUtils::STMT(ASR::make_DoLoop_t(al, x->base.base.loc, s2c(al, "do_loop_" + std::to_string(current_index)), head, do_loop_body.p, do_loop_body.size()));

        return do_loop;
    }

    void populate_idx_var_map(ASR::ImpliedDoLoop_t* x, Vec<ASR::expr_t*> idx_vars, int current_index) {
        ASR::expr_t* var_expr = x->m_var;
        ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(var_expr);
        uint64_t hash = get_hash((ASR::asr_t*) var->m_v);
        idx_var_map[hash] = idx_vars[current_index];
        for (size_t i = 0; i < x->n_values; i++) {
            ASR::expr_t* value = x->m_values[i];
            if( ASR::is_a<ASR::ImpliedDoLoop_t>(*value) ) {
                populate_idx_var_map(ASR::down_cast<ASR::ImpliedDoLoop_t>(value), idx_vars, current_index+1);
            }
        }
    }

    int get_num_array_items(ASR::ImpliedDoLoop_t* x, std::vector<ASR::expr_t*> &args) {
        int num_array_items = 0;
        for (size_t i = 0; i < x->n_values; i++) {
            ASR::expr_t* value = x->m_values[i];
            if (ASR::is_a<ASR::ArrayItem_t>(*value)) {
                num_array_items += 1;
                ASR::ArrayItem_t *array_item = ASR::down_cast<ASR::ArrayItem_t>(value);
                args.push_back(array_item->m_v);
            } else if (ASR::is_a<ASR::ImpliedDoLoop_t>(*value)) {
                num_array_items += get_num_array_items(ASR::down_cast<ASR::ImpliedDoLoop_t>(value), args);
            } else if (ASR::is_a<ASR::RealBinOp_t>(*value)) {
                ASR::RealBinOp_t *real_bin_op = ASR::down_cast<ASR::RealBinOp_t>(value);
                if (ASR::is_a<ASR::ArrayItem_t>(*real_bin_op->m_left)) {
                    num_array_items += 1;
                    ASR::ArrayItem_t *array_item = ASR::down_cast<ASR::ArrayItem_t>(real_bin_op->m_left);
                    args.push_back(array_item->m_v);
                }
                if (ASR::is_a<ASR::ArrayItem_t>(*real_bin_op->m_right)) {
                    num_array_items += 1;
                    ASR::ArrayItem_t *array_item = ASR::down_cast<ASR::ArrayItem_t>(real_bin_op->m_right);
                    args.push_back(array_item->m_v);
                }
            }
        }
        return num_array_items;
    }

    ASR::expr_t* resolve_bin_op(ASR::IntegerBinOp_t* x) {
        ASR::expr_t* left = x->m_left;
        ASR::expr_t* right = x->m_right;
        ASR::binopType op = x->m_op;

        if (ASR::is_a<ASR::IntegerBinOp_t>(*left)) {
            left = resolve_bin_op(ASR::down_cast<ASR::IntegerBinOp_t>(left));
        }

        if (ASR::is_a<ASR::IntegerBinOp_t>(*right)) {
            right = resolve_bin_op(ASR::down_cast<ASR::IntegerBinOp_t>(right));
        }

        ASR::expr_t* result = nullptr;

        LCOMPILERS_ASSERT(left);
        LCOMPILERS_ASSERT(right);
        if (!ASR::is_a<ASR::IntegerConstant_t>(*left)) {

        }
        int left_value = ASR::down_cast<ASR::IntegerConstant_t>(left)->m_n;
        int right_value = ASR::down_cast<ASR::IntegerConstant_t>(right)->m_n;

        int result_value = 0;

        switch (op) {
            case ASR::binopType::Add:
                result_value = left_value + right_value;
                break;
            case ASR::binopType::Sub:
                result_value = left_value - right_value;
                break;
            case ASR::binopType::Mul:
                result_value = left_value * right_value;
                break;
            case ASR::binopType::Div:
                result_value = left_value / right_value;
                break;
            case ASR::binopType::Pow:
                result_value = pow(left_value, right_value);
                break;
            default:
                throw LCompilersException("Unsupported binary operation");
        }

        result = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x->base.base.loc, result_value, ASRUtils::TYPE(ASR::make_Integer_t(al, x->base.base.loc, 4))));

        return result;
    }

    void replace_ImpliedDoLoop(ASR::ImpliedDoLoop_t* x) {

        Location loc = x->base.base.loc;

        // get implied do loop nesting
        int nest = get_implied_do_loop_nesting(x, 1);

        ASR::expr_t* implied_do_loop_size = get_ImpliedDoLoop_size(x);

        // create a new function
        SetChar current_function_dependencies;

        ASR::accessType s_access = ASR::Public;;
        ASR::deftypeType deftype = ASR::deftypeType::Implementation;

        // SymbolTable *old_scope = current_scope;
        SymbolTable *parent_scope = current_scope;
        current_scope = al.make_new<SymbolTable>(parent_scope);

        std::string func_name = parent_scope->get_unique_name("implied_do_loop_" + std::to_string(num_implied_do_loops));
        num_implied_do_loops++;

        // create index variables
        Vec<ASR::expr_t*> idx_vars;
        PassUtils::create_idx_vars(idx_vars, nest + 1, loc, al, current_scope);

        // clear idx_var_map
        idx_var_map.clear();
        populate_idx_var_map(x, idx_vars, 0);

        global_idx_var = idx_vars[nest];


        // create return var
        Vec<ASR::dimension_t> dims;
        dims.reserve(al, 1);
        ASR::dimension_t dim;
        dim.loc = loc;

        ASR::ttype_t* int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));

        dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
                        1, int32_type));

        dim.m_length = implied_do_loop_size;
        dims.push_back(al, dim);

        ASR::ttype_t* return_type = ASRUtils::duplicate_type(al, x->m_type, &dims);
        if (return_type->type == ASR::ttypeType::Array) {
            ASR::Array_t* return_array_type = ASR::down_cast<ASR::Array_t>(return_type);
            for (size_t i = 0; i < return_array_type->n_dims; i++) {
                ASR::dimension_t dim = return_array_type->m_dims[i];
                if (dim.m_length) {

                    if (ASR::is_a<ASR::IntegerBinOp_t>(*(dim.m_length))) {
                        ASR::IntegerBinOp_t* bin_op = ASR::down_cast<ASR::IntegerBinOp_t>(dim.m_length);
                        ASR::expr_t* new_length = resolve_bin_op(bin_op);
                        return_array_type->m_dims[i].m_length = new_length;
                    }

                }
            }
            return_array_type->m_physical_type = ASR::array_physical_typeType::FixedSizeArray;

            // currently hardcoding to real*4
            if (target_type) {
                return_array_type->m_type = target_type;
            }
        }

        ASR::expr_t* return_var_expr = PassUtils::create_var(0, "array_constant",
                        loc, return_type, al, current_scope);

        ASR::Variable_t* return_var = ASRUtils::EXPR2VAR(return_var_expr);

        return_var->m_intent = ASR::intentType::Out;

        // create function arguments
        std::vector<ASR::expr_t*> vec_args;
        int num_array_items = get_num_array_items(x, vec_args);

        Vec<ASR::expr_t*> args;
        args.reserve(al, num_array_items);

        int counter = 0;

        for( auto& itr: vec_args ) {
            ASR::expr_t* arg = itr;

            ASR::expr_t* arg_m_value = nullptr;

            ASR::ttype_t* arg_type = ASRUtils::expr_type(arg);

            ASR::expr_t* new_expr = PassUtils::create_var(counter, "array_arg",
                        loc, arg_type, al, current_scope);

            if (ASR::is_a<ASR::Var_t>(*arg)) {
                ASR::Var_t* arg_var = ASR::down_cast<ASR::Var_t>(arg);
                uint64_t hash = get_hash((ASR::asr_t*) arg_var->m_v);
                array_map[hash] = new_expr;
                if (ASR::is_a<ASR::Variable_t>(*arg_var->m_v)) {
                    ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(arg_var->m_v);
                    arg_m_value = var->m_value;
                }
            }
            
            ASR::Variable_t* new_var = ASRUtils::EXPR2VAR(new_expr);
            new_var->m_intent = ASR::intentType::In;
            new_var->m_value = arg_m_value;

            args.push_back(al, new_expr);
            
            counter++;
        }

        // create function body
        Vec<ASR::stmt_t*> body;
        body.reserve(al, 2);

        // initialize global_idx_var to one
        ASR::expr_t* one = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
                        1, int32_type));
        body.push_back(al, ASRUtils::STMT(ASR::make_Assignment_t(al, loc, global_idx_var, one, nullptr)));

        // create a do loop
        ASR::stmt_t* do_loop = handle_DoLoopCreation(x, idx_vars, 0, return_type, return_var_expr);

        // add do loop to function body
        body.push_back(al, do_loop);

        SetChar func_deps;
        func_deps.reserve(al, current_function_dependencies.size());
        for( auto& itr: current_function_dependencies ) {
            func_deps.push_back(al, s2c(al, itr));
        }

        ASR::asr_t* tmp_ = ASRUtils::make_Function_t_util(
            al, loc,
            /* a_symtab */ current_scope,
            /* a_name */ s2c(al, to_lower(func_name)),
            func_deps.p, func_deps.n,
            /* a_args */ args.p,
            /* n_args */ args.size(),
            /* a_body */ body.p,
            /* n_body */ body.n,
            return_var_expr,
            ASR::abiType::Source,
            s_access, deftype, nullptr,
            false, false, false, false, false,
            nullptr, 0, nullptr, 0,
            false, false, false);
        ASR::symbol_t* func_sym = ASR::down_cast<ASR::symbol_t>(tmp_);
        parent_scope->add_symbol(func_name, func_sym);

        Vec<ASR::call_arg_t> call_args;
        call_args.reserve(al, args.size());

        for( auto& itr: vec_args ) {
            ASR::call_arg_t call_arg;
            call_arg.m_value = itr;
            call_args.push_back(al, call_arg);
        }

        ASR::asr_t* func_call_asr = ASR::make_FunctionCall_t(al, loc, func_sym, nullptr, 
                                call_args.p, call_args.n, return_type, nullptr, nullptr);
        
        *current_expr = ASRUtils::EXPR(func_call_asr);

        parent_stmt = nullptr;
    }

    void replace_ArrayPhysicalCast(ASR::ArrayPhysicalCast_t* x) {
        ASR::BaseExprReplacer<ReplaceImpliedDoLoops>::replace_ArrayPhysicalCast(x);
        x->m_old = ASRUtils::extract_physical_type(ASRUtils::expr_type(x->m_arg));
        if( x->m_old == x->m_new ) {
            *current_expr = x->m_arg;
        }
    }

};

class ImpliedDoLoopsVisitor : public ASR::CallReplacerOnExpressionsVisitor<ImpliedDoLoopsVisitor>
{
    private:

        Allocator& al;
        bool remove_original_statement;
        ReplaceImpliedDoLoops replacer;
        Vec<ASR::stmt_t*> pass_result;
        std::map<ASR::expr_t*, ASR::expr_t*> resultvar2value;

    public:

        ImpliedDoLoopsVisitor(Allocator& al_) :
        al(al_), remove_original_statement(false),
        replacer(al_, pass_result,
            remove_original_statement, resultvar2value) {
            pass_result.n = 0;
            pass_result.reserve(al, 0);
        }

        void visit_Variable(const ASR::Variable_t& /*x*/) {
            // Do nothing, already handled in init_expr pass
        }

        void call_replacer() {
            replacer.current_expr = current_expr;
            replacer.current_scope = current_scope;
            replacer.replace_expr(*current_expr);
        }

        void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
            Vec<ASR::stmt_t*> body;
            body.reserve(al, n_body);

            for (size_t i = 0; i < n_body; i++) {
                pass_result.n = 0;
                pass_result.reserve(al, 1);
                remove_original_statement = false;
                replacer.result_var = nullptr;
                visit_stmt(*m_body[i]);
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
                ASR::is_a<ASR::GetPointer_t>(*x.m_value)) ) {
                return ;
            }

            if (x.m_overloaded) {
                this->visit_stmt(*x.m_overloaded);
                remove_original_statement = false;
                return ;
            }

            ASR::expr_t* target = x.m_target;
            ASR::ttype_t* target_type = ASRUtils::expr_type(target);

            if (target_type->type == ASR::ttypeType::Array) {
                ASR::Array_t* array_type = ASR::down_cast<ASR::Array_t>(target_type);
                replacer.target_type = array_type->m_type;
            }

            replacer.result_var = x.m_target;
            resultvar2value[replacer.result_var] = x.m_value;
            ASR::expr_t** current_expr_copy_9 = current_expr;
            current_expr = const_cast<ASR::expr_t**>(&(x.m_value));
            this->call_replacer();
            current_expr = current_expr_copy_9;
            if( !remove_original_statement ) {
                this->visit_expr(*x.m_value);
            }
            replacer.target_type = nullptr;
        }

        void visit_CPtrToPointer(const ASR::CPtrToPointer_t& /*x*/) {
            // Do nothing.
        }

        void visit_Print(const ASR::Print_t& x) {
            ASR::Print_t* print = (ASR::Print_t*)(&x);
            for (size_t i = 0; i < print->n_values; i++) {
                ASR::expr_t** current_expr_copy_10 = current_expr;
                current_expr = const_cast<ASR::expr_t**>(&(print->m_values[i]));
                replacer.parent_stmt = ASRUtils::STMT((ASR::asr_t*) print);
                this->call_replacer();
                current_expr = current_expr_copy_10;
                if( !remove_original_statement ) {
                    this->visit_expr(*print->m_values[i]);
                }
            }
        }

};

void pass_replace_implied_do_loops(Allocator &al,
    ASR::TranslationUnit_t &unit,
    const LCompilers::PassOptions& /*pass_options*/) {
    ImpliedDoLoopsVisitor v(al);
    v.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}


} // namespace LCompilers
