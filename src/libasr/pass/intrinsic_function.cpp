#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/replace_intrinsic_function.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/intrinsic_array_function_registry.h>
#include <libasr/pass/pass_utils.h>

#include <vector>


namespace LCompilers {

/*

This ASR pass replaces the IntrinsicFunction node with a call to an
implementation in ASR that we construct (and cache) on the fly for the actual
arguments.

Call this pass if you do not want to implement intrinsic functions directly
in the backend.

*/

class ReplaceIntrinsicFunctions: public ASR::BaseExprReplacer<ReplaceIntrinsicFunctions> {

    private:

    Allocator& al;
    SymbolTable* global_scope;
    std::map<ASR::symbol_t*, ASRUtils::IntrinsicArrayFunctions>& func2intrinsicid;

    public:

    ReplaceIntrinsicFunctions(Allocator& al_, SymbolTable* global_scope_,
    std::map<ASR::symbol_t*, ASRUtils::IntrinsicArrayFunctions>& func2intrinsicid_) :
        al(al_), global_scope(global_scope_), func2intrinsicid(func2intrinsicid_) {}


    void replace_IntrinsicElementalFunction(ASR::IntrinsicElementalFunction_t* x) {
        if (x->m_value) {
            *current_expr = x->m_value;
            return;
        }

        Vec<ASR::call_arg_t> new_args; new_args.reserve(al, x->n_args);
        // Replace any IntrinsicElementalFunctions in the argument first:
        for( size_t i = 0; i < x->n_args; i++ ) {
            if (ASRUtils::use_experimental_simplifier) {
                ASR::call_arg_t arg0;
                arg0.loc = (*current_expr)->base.loc;
                arg0.m_value = x->m_args[i];
                new_args.push_back(al, arg0);
            } else {
                ASR::expr_t** current_expr_copy_ = current_expr;
                current_expr = &(x->m_args[i]);
                replace_expr(x->m_args[i]);
                ASR::call_arg_t arg0;
                arg0.loc = (*current_expr)->base.loc;
                arg0.m_value = *current_expr; // Use the converted arg
                new_args.push_back(al, arg0);
                current_expr = current_expr_copy_;
            }
        }
        // TODO: currently we always instantiate a new function.
        // Rather we should reuse the old instantiation if it has
        // exactly the same arguments. For that we could use the
        // overload_id, and uniquely encode the argument types.
        // We could maintain a mapping of type -> id and look it up.

        ASRUtils::impl_function instantiate_function =
            ASRUtils::IntrinsicElementalFunctionRegistry::get_instantiate_function(x->m_intrinsic_id);
        if( instantiate_function == nullptr ) {
            return ;
        }
        Vec<ASR::ttype_t*> arg_types;
        arg_types.reserve(al, x->n_args);
        for( size_t i = 0; i < x->n_args; i++ ) {
            arg_types.push_back(al, ASRUtils::expr_type(x->m_args[i]));
        }
        ASR::ttype_t* type = nullptr;
        if (ASRUtils::use_experimental_simplifier) {
            type = ASRUtils::extract_type(x->m_type);
        } else {
            type = x->m_type;
        }
        ASR::expr_t* current_expr_ = instantiate_function(al, x->base.base.loc,
            global_scope, arg_types, type, new_args, x->m_overload_id);
        *current_expr = current_expr_;
    }

    void replace_IntrinsicArrayFunction(ASR::IntrinsicArrayFunction_t* x) {
        std::string intrinsic_name_ = std::string(ASRUtils::get_array_intrinsic_name(x->m_arr_intrinsic_id));
        if (x->m_value) {
            *current_expr = x->m_value;
            return;
        }
        replace_ttype(x->m_type);
        Vec<ASR::call_arg_t> new_args; new_args.reserve(al, x->n_args);
        // Replace any IntrinsicArrayFunctions in the argument first:
        for( size_t i = 0; i < x->n_args; i++ ) {
            if (ASRUtils::use_experimental_simplifier) {
                ASR::call_arg_t arg0;
                arg0.loc = (*current_expr)->base.loc;
                arg0.m_value = x->m_args[i];
                new_args.push_back(al, arg0);
            } else {
                ASR::expr_t** current_expr_copy_ = current_expr;
                current_expr = &(x->m_args[i]);
                replace_expr(x->m_args[i]);
                ASR::call_arg_t arg0;
                arg0.loc = (*current_expr)->base.loc;
                arg0.m_value = *current_expr; // Use the converted arg
                new_args.push_back(al, arg0);
                current_expr = current_expr_copy_;
            }
        }

        // TODO: currently we always instantiate a new function.
        // Rather we should reuse the old instantiation if it has
        // exactly the same arguments. For that we could use the
        // overload_id, and uniquely encode the argument types.
        // We could maintain a mapping of type -> id and look it up.

        ASRUtils::impl_function instantiate_function =
            ASRUtils::IntrinsicArrayFunctionRegistry::get_instantiate_function(x->m_arr_intrinsic_id);
        if( instantiate_function == nullptr ) {
            return ;
        }
        Vec<ASR::ttype_t*> arg_types;
        arg_types.reserve(al, x->n_args);
        for( size_t i = 0; i < x->n_args; i++ ) {
            arg_types.push_back(al, ASRUtils::expr_type(x->m_args[i]));
        }
        ASR::expr_t* current_expr_ = instantiate_function(al, x->base.base.loc,
            global_scope, arg_types, x->m_type, new_args, x->m_overload_id);
        ASR::expr_t* func_call = current_expr_;
        *current_expr = current_expr_;
        bool condition = ASR::is_a<ASR::FunctionCall_t>(*func_call);
        if (ASRUtils::use_experimental_simplifier) {
            condition = condition && ASRUtils::is_array(x->m_type);
        }
        if (condition) {
            ASR::symbol_t *call_sym = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::FunctionCall_t>(func_call)->m_name);
            func2intrinsicid[call_sym] = (ASRUtils::IntrinsicArrayFunctions) x->m_arr_intrinsic_id;
        }
    }
};

/*
The following visitor calls the above replacer i.e., ReplaceFunctionCalls
on expressions present in ASR so that FunctionCall get replaced everywhere
and we don't end up with false positives.
*/
class ReplaceIntrinsicFunctionsVisitor : public ASR::CallReplacerOnExpressionsVisitor<ReplaceIntrinsicFunctionsVisitor>
{
    private:

        ReplaceIntrinsicFunctions replacer;

    public:

        ReplaceIntrinsicFunctionsVisitor(Allocator& al_, SymbolTable* global_scope_,
            std::map<ASR::symbol_t*, ASRUtils::IntrinsicArrayFunctions>& func2intrinsicid_) :
            replacer(al_, global_scope_, func2intrinsicid_) {}

        void call_replacer() {
            replacer.current_expr = current_expr;
            replacer.replace_expr(*current_expr);
        }

};

class ReplaceFunctionCallReturningArray: public ASR::BaseExprReplacer<ReplaceFunctionCallReturningArray> {

    private:

    Allocator& al;
    Vec<ASR::stmt_t*>& pass_result;
    size_t result_counter;
    std::map<ASR::symbol_t*, ASRUtils::IntrinsicArrayFunctions>& func2intrinsicid;

    public:
    ASR::expr_t* result_var_; // Declared in simplifier
    SymbolTable* current_scope;

    ReplaceFunctionCallReturningArray(Allocator& al_, Vec<ASR::stmt_t*>& pass_result_,
    std::map<ASR::symbol_t*, ASRUtils::IntrinsicArrayFunctions>& func2intrinsicid_) :
    al(al_), pass_result(pass_result_), result_counter(0),
    func2intrinsicid(func2intrinsicid_),
    result_var_(nullptr),
    current_scope(nullptr) {}

    // Not called from anywhere but kept for future use.
    // Especially if we don't find alternatives to allocatables
    ASR::expr_t* get_result_var_for_runtime_dim(ASR::expr_t* dim, int n_dims,
        std::string m_name, const Location& loc, ASR::ttype_t* m_type,
        ASR::expr_t* input_array) {
        m_type = ASRUtils::TYPE(ASR::make_Allocatable_t(al, loc,
            ASRUtils::type_get_past_allocatable(m_type)));
        ASR::expr_t* result_var_ = PassUtils::create_var(result_counter,
                                    m_name + "_res",
                                    loc, m_type, al, current_scope);
        ASR::stmt_t** else_ = nullptr;
        size_t else_n = 0;
        const Location& loc_ = dim->base.loc;
        for( int i = 1; i <= n_dims + 1; i++ ) {
            ASR::expr_t* current_dim = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                                        al, loc_, i, ASRUtils::expr_type(dim)));
            ASR::expr_t* test_expr = ASRUtils::EXPR(ASR::make_IntegerCompare_t(
                                        al, loc_, dim, ASR::cmpopType::Eq,
                                        current_dim, ASRUtils::TYPE(ASR::make_Logical_t(
                                            al, loc_, 4)), nullptr));

            ASR::alloc_arg_t alloc_arg;
            alloc_arg.m_len_expr = nullptr;
            alloc_arg.m_type = nullptr;
            alloc_arg.loc = loc_;
            alloc_arg.m_a = result_var_;
            Vec<ASR::dimension_t> alloc_dims;
            alloc_dims.reserve(al, n_dims);
            for( int j = 1; j <= n_dims + 1; j++ ) {
                if( j == i ) {
                    continue ;
                }
                ASR::dimension_t m_dim;
                m_dim.loc = loc_;
                m_dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                                al, loc_, 1, ASRUtils::expr_type(dim)));
                // Assuming that first argument is the array
                m_dim.m_length = ASRUtils::get_size(input_array, j, al);
                alloc_dims.push_back(al, m_dim);
            }
            alloc_arg.m_dims = alloc_dims.p;
            alloc_arg.n_dims = alloc_dims.size();
            Vec<ASR::alloc_arg_t> alloc_args;
            alloc_args.reserve(al, 1);
            alloc_args.push_back(al, alloc_arg);
            Vec<ASR::expr_t*> to_be_deallocated;
            to_be_deallocated.reserve(al, alloc_args.size());
            for( size_t i = 0; i < alloc_args.size(); i++ ) {
                to_be_deallocated.push_back(al, alloc_args.p[i].m_a);
            }
            ASR::stmt_t* allocate_stmt = ASRUtils::STMT(ASR::make_Allocate_t(
                al, loc_, alloc_args.p, alloc_args.size(), nullptr, nullptr, nullptr));
            Vec<ASR::stmt_t*> if_body;
            if_body.reserve(al, 2);
            if_body.push_back(al, ASRUtils::STMT(ASR::make_ExplicitDeallocate_t(
                al, loc, to_be_deallocated.p, to_be_deallocated.size())));
            if_body.push_back(al, allocate_stmt);
            ASR::stmt_t* if_ = ASRUtils::STMT(ASR::make_If_t(al, loc_, test_expr,
                                if_body.p, if_body.size(), else_, else_n));
            Vec<ASR::stmt_t*> if_else_if;
            if_else_if.reserve(al, 1);
            if_else_if.push_back(al, if_);
            else_ = if_else_if.p;
            else_n = if_else_if.size();
        }
        pass_result.push_back(al, else_[0]);
        return result_var_;
    }

    ASR::expr_t* get_result_var_for_constant_dim(int dim, int n_dims,
        std::string m_name, const Location& loc, ASR::ttype_t* m_type,
        ASR::expr_t* input_array) {
        Vec<ASR::dimension_t> result_dims;
        result_dims.reserve(al, n_dims);
        for( int j = 1; j <= n_dims + 1; j++ ) {
            if( j == dim ) {
                continue ;
            }
            ASR::dimension_t m_dim;
            m_dim.loc = loc;
            m_dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                            al, loc, 1, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)) ));
            // Assuming that first argument is the array
            m_dim.m_length = ASRUtils::get_size(input_array, j, al);
            result_dims.push_back(al, m_dim);
        }
        ASR::ttype_t* result_type = ASRUtils::duplicate_type(al, m_type, &result_dims);
        return PassUtils::create_var(result_counter,
                                    m_name + "_res",
                                    loc, result_type, al, current_scope);
    }

    void replace_FunctionCall(ASR::FunctionCall_t* x) {
        ASR::symbol_t* x_m_name = ASRUtils::symbol_get_past_external(x->m_name);
        int n_dims = ASRUtils::extract_n_dims_from_ttype(x->m_type);
        if ( ASRUtils::use_experimental_simplifier ) {
            if( func2intrinsicid.find(x_m_name) == func2intrinsicid.end() ) {
                return;
            }
        } else {
            if( func2intrinsicid.find(x_m_name) == func2intrinsicid.end() || n_dims == 0 ||
                    !ASRUtils::IntrinsicArrayFunctionRegistry::handle_dim(func2intrinsicid[x_m_name])) {
                ASR::BaseExprReplacer<ReplaceFunctionCallReturningArray>::replace_FunctionCall(x);
                return;
            }
        }

        Vec<ASR::call_arg_t> new_args;
        new_args.reserve(al, x->n_args + 1);
        for( size_t i = 0; i < x->n_args; i++ ) {
            if ( ASRUtils::use_experimental_simplifier ) {
                ASR::call_arg_t new_arg;
                new_arg.loc = x->m_args[i].loc;
                new_arg.m_value = x->m_args[i].m_value;
                new_args.push_back(al, new_arg);
            } else {
                if (x->m_args[i].m_value != nullptr) {
                    ASR::expr_t** current_expr_copy_9 = current_expr;
                    current_expr = &(x->m_args[i].m_value);
                    this->replace_expr(x->m_args[i].m_value);
                    ASR::call_arg_t new_arg;
                    new_arg.loc = x->m_args[i].loc;
                    new_arg.m_value = *current_expr;
                    new_args.push_back(al, new_arg);
                    current_expr = current_expr_copy_9;
                }
            }
        }
        if (!ASRUtils::use_experimental_simplifier) {
            ASR::expr_t* result_var_ = nullptr;
            int dim_index = ASRUtils::IntrinsicArrayFunctionRegistry::
                get_dim_index(func2intrinsicid[x_m_name]);
            if( dim_index == 1 ) {
                ASR::expr_t* dim = x->m_args[dim_index].m_value;
                if( !ASRUtils::is_value_constant(ASRUtils::expr_value(dim)) ) {
                    result_var_ = PassUtils::create_var(result_counter,
                        std::string(ASRUtils::symbol_name(x->m_name)) + "_res",
                        x->base.base.loc, x->m_type, al, current_scope);
                    if (ASRUtils::is_allocatable(ASRUtils::expr_type(result_var_)) &&
                        func2intrinsicid[x_m_name] == ASRUtils::IntrinsicArrayFunctions::Sum) {
                        PassUtils::allocate_res_var(al, x, new_args, result_var_, pass_result, {0, 0, 1});
                    }
                } else {
                    int constant_dim;
                    if (ASRUtils::extract_value(ASRUtils::expr_value(dim), constant_dim)) {
                        result_var_ = get_result_var_for_constant_dim(constant_dim, n_dims,
                                        ASRUtils::symbol_name(x->m_name), x->base.base.loc,
                                        x->m_type, x->m_args[0].m_value);
                    } else {
                        throw LCompilersException("Constant dimension cannot be extracted.");
                    }
                }
            } else if ( dim_index == 2 ) {
                ASR::expr_t* func_call_count = nullptr;
                if (func2intrinsicid[x_m_name] == ASRUtils::IntrinsicArrayFunctions::Pack) {
                    ASR::Function_t* pack = ASR::down_cast<ASR::Function_t>(ASRUtils::symbol_get_past_external(x->m_name));
                    ASR::symbol_t* res = pack->m_symtab->resolve_symbol("result");
                    if (res) {
                        ASR::Variable_t* res_var = ASR::down_cast<ASR::Variable_t>(res);
                        ASR::Array_t* res_arr = ASR::down_cast<ASR::Array_t>(res_var->m_type);
                        if (ASR::is_a<ASR::FunctionCall_t>(*res_arr->m_dims[0].m_length)) {
                            ASRUtils::ExprStmtDuplicator expr_stmt_duplicator(al);
                            func_call_count = res_arr->m_dims[0].m_length;
                            func_call_count = expr_stmt_duplicator.duplicate_expr(func_call_count);

                            ASR::FunctionCall_t* func_call = ASR::down_cast<ASR::FunctionCall_t>(func_call_count);
                            if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*func_call->m_args[0].m_value)) {
                                ASR::ArrayPhysicalCast_t *array_cast = ASR::down_cast<ASR::ArrayPhysicalCast_t>(func_call->m_args[0].m_value);
                                if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*new_args[1].m_value)) {
                                array_cast->m_arg = ASR::down_cast<ASR::ArrayPhysicalCast_t>(new_args[1].m_value)->m_arg;
                                } else {
                                    array_cast->m_arg = new_args[1].m_value;
                                }
                                array_cast->m_old = ASRUtils::extract_physical_type(ASRUtils::expr_type(array_cast->m_arg));
                                array_cast->m_type = ASRUtils::duplicate_type(al, ASRUtils::expr_type(array_cast->m_arg), nullptr,
                                                    ASR::array_physical_typeType::DescriptorArray, true);

                                func_call->m_args[0].m_value = ASRUtils::EXPR((ASR::asr_t*) array_cast);
                            }
                        }
                    }
                }
                result_var_ = PassUtils::create_var(result_counter,
                    std::string(ASRUtils::symbol_name(x->m_name)) + "_res",
                    x->base.base.loc, x->m_type, al, current_scope);
                if ( ASRUtils::is_allocatable(ASRUtils::expr_type(result_var_)) &&
                    func2intrinsicid[x_m_name] == ASRUtils::IntrinsicArrayFunctions::Transpose) {
                    // allocate result array
                    int n_dims = ASRUtils::extract_n_dims_from_ttype(x->m_type);
                    Vec<ASR::alloc_arg_t> alloc_args; alloc_args.reserve(al, 1);
                    Vec<ASR::dimension_t> alloc_dims; alloc_dims.reserve(al, n_dims);
                    ASR::alloc_arg_t alloc_arg; alloc_arg.loc = x->base.base.loc;
                    for (int i = 2; i > 0; i--) {
                        ASR::dimension_t dim; dim.loc = x->base.base.loc;
                        dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x->base.base.loc, 1,
                            ASRUtils::TYPE(ASR::make_Integer_t(al, x->base.base.loc, 4))));
                        ASRUtils::ASRBuilder b(al, x->base.base.loc);
                        dim.m_length = b.ArraySize(x->m_args[0].m_value, b.i32(i), ASRUtils::TYPE(ASR::make_Integer_t(al, x->base.base.loc, 4)));
                        alloc_dims.push_back(al, dim);
                    }
                    alloc_arg.m_a = result_var_; alloc_arg.m_len_expr = nullptr;
                    alloc_arg.m_type = nullptr; alloc_arg.m_dims = alloc_dims.p;
                    alloc_arg.n_dims = alloc_dims.size();
                    alloc_args.push_back(al, alloc_arg);
                    ASR::stmt_t* allocate_stmt = ASRUtils::STMT(ASR::make_Allocate_t(al,
                                            x->base.base.loc, alloc_args.p, alloc_args.n, nullptr, nullptr, nullptr));
                    pass_result.push_back(al, allocate_stmt);
                }
                if ( ASRUtils::is_allocatable(ASRUtils::expr_type(result_var_)) &&
                    func2intrinsicid[x_m_name] == ASRUtils::IntrinsicArrayFunctions::Spread) {
                    // TODO: simplify this
                    Vec<ASR::alloc_arg_t> alloc_args; alloc_args.reserve(al, 1);
                    Vec<ASR::dimension_t> result_dims;
                    ASR::alloc_arg_t alloc_arg; alloc_arg.loc = x->base.base.loc;
                    size_t n_dims = ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(x->m_args[0].m_value));
                    result_dims.reserve(al, (int) n_dims + 1);
                    // result(merge(ncopies, size(source, 1), dim == 1), merge(ncopies, size(source, 2-1), dim == 2), merge(ncopies, size(source, 3-1), dim == 3))
                    ASRUtils::ASRBuilder b(al, x->base.base.loc);
                    const Location& loc = x->base.base.loc;
                    ASR::symbol_t* merge_function = ASRUtils::symbol_parent_symtab(x_m_name)->resolve_symbol("_lcompilers_merge_i32");
                    LCOMPILERS_ASSERT(merge_function != nullptr); // can be assured by implementation of `create_Spread`
                    Vec<ASR::call_arg_t> merge_function_call_args; merge_function_call_args.reserve(al, 3);
                    ASR::call_arg_t call_arg; call_arg.loc = x->m_args[0].m_value->base.loc; call_arg.m_value = x->m_args[2].m_value; merge_function_call_args.push_back(al, call_arg);
                    ASR::call_arg_t call_arg2; call_arg2.loc = x->m_args[0].m_value->base.loc; call_arg2.m_value = b.ArraySize(x->m_args[0].m_value, b.i32(1), int32); merge_function_call_args.push_back(al, call_arg2);
                    ASR::call_arg_t call_arg3; call_arg3.loc = x->m_args[0].m_value->base.loc; call_arg3.m_value = b.Eq(x->m_args[1].m_value, b.i32(1)); merge_function_call_args.push_back(al, call_arg3);
                    ASR::expr_t* merge = ASRUtils::EXPR(ASR::make_FunctionCall_t(al, x->base.base.loc, merge_function, merge_function, merge_function_call_args.p, merge_function_call_args.n, int32, nullptr, nullptr));
                    ASR::dimension_t dim_;
                    dim_.loc = x->m_args[0].m_value->base.loc;
                    dim_.m_start = b.i32(1);
                    dim_.m_length = merge;
                    result_dims.push_back(al, dim_);
                    for( int it = 0; it < (int) n_dims; it++ ) {
                        Vec<ASR::call_arg_t> merge_function_call_args; merge_function_call_args.reserve(al, 3);
                        ASR::call_arg_t call_arg; call_arg.loc = x->m_args[0].m_value->base.loc; call_arg.m_value = x->m_args[2].m_value; merge_function_call_args.push_back(al, call_arg);
                        ASR::call_arg_t call_arg2; call_arg2.loc = x->m_args[0].m_value->base.loc; call_arg2.m_value = b.ArraySize(x->m_args[0].m_value, b.i32(it+1), int32); merge_function_call_args.push_back(al, call_arg2);
                        ASR::call_arg_t call_arg3; call_arg3.loc = x->m_args[0].m_value->base.loc; call_arg3.m_value = b.Eq(x->m_args[1].m_value, b.i32(it+2)); merge_function_call_args.push_back(al, call_arg3);
                        ASR::expr_t* merge = ASRUtils::EXPR(ASR::make_FunctionCall_t(al, x->base.base.loc, merge_function, merge_function, merge_function_call_args.p, merge_function_call_args.n, int32, nullptr, nullptr));
                        ASR::dimension_t dim;
                        dim.loc = x->m_args[0].m_value->base.loc;
                        dim.m_start = b.i32(1);
                        dim.m_length = merge;
                        result_dims.push_back(al, dim);
                    }
                    alloc_arg.m_a = result_var_; alloc_arg.m_len_expr = nullptr;
                    alloc_arg.m_type = nullptr; alloc_arg.m_dims = result_dims.p;
                    alloc_arg.n_dims = result_dims.size();
                    alloc_args.push_back(al, alloc_arg);
                    ASR::stmt_t* allocate_stmt = ASRUtils::STMT(ASR::make_Allocate_t(al,
                                            x->base.base.loc, alloc_args.p, alloc_args.n, nullptr, nullptr, nullptr));
                    pass_result.push_back(al, allocate_stmt);
                }
                if (ASRUtils::is_allocatable(ASRUtils::expr_type(result_var_)) &&
                    func_call_count) {
                    // allocate result array
                    Vec<ASR::alloc_arg_t> alloc_args; alloc_args.reserve(al, 1);
                    Vec<ASR::dimension_t> alloc_dims; alloc_dims.reserve(al, 2);
                    ASR::alloc_arg_t alloc_arg; alloc_arg.loc = x->base.base.loc;
                    ASR::dimension_t dim; dim.loc = x->base.base.loc;
                    dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x->base.base.loc, 1,
                        ASRUtils::TYPE(ASR::make_Integer_t(al, x->base.base.loc, 4))));
                    dim.m_length = func_call_count;
                    alloc_dims.push_back(al, dim);
                    alloc_arg.m_a = result_var_; alloc_arg.m_len_expr = nullptr;
                    alloc_arg.m_type = nullptr; alloc_arg.m_dims = alloc_dims.p;
                    alloc_arg.n_dims = alloc_dims.size();
                    alloc_args.push_back(al, alloc_arg);

                    ASR::stmt_t* allocate_stmt = ASRUtils::STMT(ASR::make_Allocate_t(al,
                                            x->base.base.loc, alloc_args.p, alloc_args.n, nullptr, nullptr, nullptr));
                    pass_result.push_back(al, allocate_stmt);
                }
            } else {
                LCOMPILERS_ASSERT(false);
            }
            result_counter += 1;
            this->result_var_ = result_var_; // Have to do this because of simplifier
        }
        ASR::call_arg_t new_arg;
        LCOMPILERS_ASSERT(this->result_var_)
        new_arg.loc = this->result_var_->base.loc;
        new_arg.m_value = this->result_var_;
        new_args.push_back(al, new_arg);
        if ( ASRUtils::use_experimental_simplifier ) {
            ASR::stmt_t* subrout_call = ASRUtils::STMT(ASRUtils::make_SubroutineCall_t_util(
                al, x->base.base.loc, x->m_name, x->m_original_name, new_args.p,
                new_args.size(), x->m_dt, nullptr, false, false));
            pass_result.push_back(al, subrout_call);
        } else {
            pass_result.push_back(al, ASRUtils::STMT(ASRUtils::make_SubroutineCall_t_util(
                al, x->base.base.loc, x->m_name, x->m_original_name, new_args.p,
                new_args.size(), x->m_dt, nullptr, false, false)));
            ASR::expr_t* subroutineCall_res = new_args.p[new_args.size() - 1].m_value;
            // Avoid ArrayPhysicalCasting. Use actual argument.
            // intrinsicArrayFunction has a return that's handled be the super expression. Don't use casted expr created by `make_SubroutineCall_t_uti()`.
            while(ASR::is_a<ASR::ArrayPhysicalCast_t>(*subroutineCall_res)){
                subroutineCall_res = ASRUtils::get_past_array_physical_cast(subroutineCall_res);
            }
            // Superexpression acts upon the replaced functionCall(Which we remove in this replace function).
            // we have to make the subroutineCall_res match the phytical type of the original-removed-functionCall expression.
            ASR::array_physical_typeType original_functionCall_array_ret_phsyical_type = ASRUtils::extract_physical_type(x->m_type);
            ASR::array_physical_typeType subroutineCall_res_array__physical_type = ASRUtils::extract_physical_type(ASRUtils::expr_type(subroutineCall_res));
            if(original_functionCall_array_ret_phsyical_type == subroutineCall_res_array__physical_type){
                // Do Nothing
            } else {
                ASR::ttype_t* arrayPhysicalCast_type = ASRUtils::duplicate_type(al, ASRUtils::expr_type(subroutineCall_res));
                ASR::down_cast<ASR::Array_t>(arrayPhysicalCast_type)->m_physical_type = original_functionCall_array_ret_phsyical_type;
                subroutineCall_res = ASRUtils::EXPR(ASR::make_ArrayPhysicalCast_t(al, x->base.base.loc, subroutineCall_res,
                    subroutineCall_res_array__physical_type, original_functionCall_array_ret_phsyical_type,
                    arrayPhysicalCast_type, nullptr));
            }
            *current_expr = subroutineCall_res;
        }
    }
};

class ReplaceFunctionCallReturningArrayVisitor : public ASR::CallReplacerOnExpressionsVisitor<ReplaceFunctionCallReturningArrayVisitor>
{
    private:

        Allocator& al;
        ReplaceFunctionCallReturningArray replacer;
        Vec<ASR::stmt_t*> pass_result;
        Vec<ASR::stmt_t*>* parent_body;

    public:

        ReplaceFunctionCallReturningArrayVisitor(Allocator& al_,
            std::map<ASR::symbol_t*, ASRUtils::IntrinsicArrayFunctions>& func2intrinsicid_) :
        al(al_),
        replacer(al_, pass_result, func2intrinsicid_),
        parent_body(nullptr) {
            pass_result.n = 0;
        }


        void call_replacer() {
            replacer.current_expr = current_expr;
            replacer.current_scope = current_scope;
            replacer.replace_expr(*current_expr);
        }

        void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
            Vec<ASR::stmt_t*> body;
            body.reserve(al, n_body);
            if( parent_body ) {
                for (size_t j=0; j < pass_result.size(); j++) {
                    parent_body->push_back(al, pass_result[j]);
                }
            }

            for (size_t i=0; i<n_body; i++) {
                pass_result.n = 0;
                pass_result.reserve(al, 1);
                Vec<ASR::stmt_t*>* parent_body_copy = parent_body;
                parent_body = &body;
                visit_stmt(*m_body[i]);
                parent_body = parent_body_copy;
                if ( !ASRUtils::use_experimental_simplifier ) {
                    for (size_t j=0; j < pass_result.size(); j++) {
                        body.push_back(al, pass_result[j]);
                    }
                    body.push_back(al, m_body[i]);
                } else {
                    if( pass_result.size() > 0 ) {
                        for (size_t j=0; j < pass_result.size(); j++) {
                            body.push_back(al, pass_result[j]);
                        }
                    } else {
                        body.push_back(al, m_body[i]);
                    }
                }
            }
            m_body = body.p;
            n_body = body.size();
            pass_result.n = 0;
        }

        void visit_Assignment(const ASR::Assignment_t& x) {
            if( ASRUtils::use_experimental_simplifier ) {
                replacer.result_var_ = x.m_target;
                ASR::CallReplacerOnExpressionsVisitor<ReplaceFunctionCallReturningArrayVisitor>::visit_Assignment(x);
                replacer.result_var_ = nullptr;
            } else {
                ASR::expr_t** current_expr_copy_8 = current_expr;
                current_expr = const_cast<ASR::expr_t**>(&(x.m_target));
                call_replacer();
                current_expr = current_expr_copy_8;
                if( x.m_target )
                visit_expr(*x.m_target);
                ASR::expr_t** current_expr_copy_9 = current_expr;
                current_expr = const_cast<ASR::expr_t**>(&(x.m_value));
                call_replacer();
                current_expr = current_expr_copy_9;
                if( x.m_value )
                visit_expr(*x.m_value);
                if (x.m_overloaded) {
                    visit_stmt(*x.m_overloaded);
                }

                /*
                Uncomment in case we encounter a case where target and value ranks are different.
                Can be used in a pass where expressions are replaced with their compile time values.
                if( x.m_value && x.m_target ) {
                    size_t target_rank = ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(x.m_target));
                    size_t value_rank = ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(x.m_value));
                    if( target_rank != value_rank && target_rank > 0 && value_rank > 0 ) {
                        ASR::Assignment_t& xx = const_cast<ASR::Assignment_t&>(x);
                        Vec<ASR::expr_t*> shape_args;
                        shape_args.reserve(al, 2); shape_args.push_back(al, x.m_target);
                        shape_args.push_back(al, nullptr);
                        diag::Diagnostics diags;
                        ASR::expr_t* target_shape = ASRUtils::EXPR(
                            ASRUtils::Shape::create_Shape(al, x.m_value->base.loc, shape_args, diags));
                        xx.m_value = ASRUtils::EXPR(ASR::make_ArrayReshape_t(al, x.m_value->base.loc,
                            xx.m_value, target_shape, ASRUtils::expr_type(x.m_target), nullptr));
                    }
                }
                */
            }
        }

};

void pass_replace_intrinsic_function(Allocator &al, ASR::TranslationUnit_t &unit,
                            const LCompilers::PassOptions& /*pass_options*/) {
    std::map<ASR::symbol_t*, ASRUtils::IntrinsicArrayFunctions> func2intrinsicid;
    ReplaceIntrinsicFunctionsVisitor v(al, unit.m_symtab, func2intrinsicid);
    v.visit_TranslationUnit(unit);
    ReplaceFunctionCallReturningArrayVisitor u(al, func2intrinsicid);
    u.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor w(al);
    w.visit_TranslationUnit(unit);
}


} // namespace LCompilers
