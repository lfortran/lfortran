#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/create_subroutine_from_function.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/intrinsic_array_function_registry.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/array_struct_temporary.h>

namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

class CreateFunctionFromSubroutine: public ASR::BaseWalkVisitor<CreateFunctionFromSubroutine> {

    public:

        Allocator& al;

        CreateFunctionFromSubroutine(Allocator &al_): al(al_)
        {
        }

        void visit_Function(const ASR::Function_t& x) {
            ASR::Function_t& xx = const_cast<ASR::Function_t&>(x);
            ASR::Function_t* x_ptr = ASR::down_cast<ASR::Function_t>(&(xx.base));
            PassUtils::handle_fn_return_var(al, x_ptr, PassUtils::is_aggregate_or_array_or_nonPrimitive_type);
        }

};

class ReplaceFunctionCallWithSubroutineCall:
    public ASR::BaseExprReplacer<ReplaceFunctionCallWithSubroutineCall> {
private :
    void insert_implicit_deallocate(ASR::expr_t* result_var) {
        Vec<ASR::expr_t*> to_be_deallocated;
        to_be_deallocated.reserve(al, 1);
        to_be_deallocated.push_back(al, result_var);
        pass_result.push_back(al, ASRUtils::STMT(
        ASR::make_ImplicitDeallocate_t(al, result_var->base.loc,
            to_be_deallocated.p, to_be_deallocated.size())));
    }

public :
    Allocator & al;
    int result_counter = 0;
    SymbolTable* current_scope;
    Vec<ASR::stmt_t*> &pass_result;
    ReplaceFunctionCallWithSubroutineCall(Allocator& al_, Vec<ASR::stmt_t*> &pass_result_) :
        al(al_),pass_result(pass_result_) {}

    void traverse_functionCall_args(ASR::call_arg_t* call_args, size_t call_args_n){
        for(size_t i = 0; i < call_args_n; i++){
            ASR::expr_t** current_expr_copy = current_expr;
            current_expr = &call_args[i].m_value;
            replace_expr(call_args[i].m_value);
            current_expr = current_expr_copy;
        }
    }

    void replace_FunctionCall(ASR::FunctionCall_t* x){
        traverse_functionCall_args(x->m_args, x->n_args);
        if(PassUtils::is_non_primitive_return_type(x->m_type)){ // Arrays and structs are handled by the array_struct_temporary. No need to check for them here.
            // Create variable in current_scope to be holding the return + Deallocate.
            ASR::expr_t* result_var = PassUtils::create_var(result_counter++,
                "_func_call_res", x->base.base.loc, ASRUtils::duplicate_type(al, x->m_type), al, current_scope);
            if(ASRUtils::is_allocatable(result_var)){
                insert_implicit_deallocate(result_var);
            }
            // Create allocate statement if needed
            if(ASRUtils::is_string_only(x->m_type)){ 
                ASR::String_t* str = ASRUtils::get_string_type(result_var);
                if( str->m_len &&
                !ASRUtils::is_value_constant(str->m_len) &&
                !ASRUtils::is_allocatable(result_var)){ // Corresponds to -> `character(n) :: str` (Non-allocatable string of non-compile-time length)
                    ASR::expr_t* len_expr_to_allocate_with = str->m_len; // length Expression
                    {
                    /*
                        Replace allocate length (could be a functionCall).
                        TODO :: Do proper replacement if functionCall is dependant on FunctionParam from the current functionCall,
                        as the current visit does redundant functionCall replacement(FunctionCall + variable).
                    */ 
                        ASR::expr_t** current_expr_copy = current_expr;
                        current_expr = &len_expr_to_allocate_with;
                        replace_expr(len_expr_to_allocate_with);
                        current_expr = current_expr_copy;
                    }
                    // Modify String info to be deferred allocatable string
                    str->m_len = nullptr; str->m_len_kind = ASR::DeferredLength;str->m_physical_type = ASR::DescriptorString;
                    ASRUtils::EXPR2VAR(result_var)->m_type =
                        ASRUtils::TYPE(ASR::make_Allocatable_t(al, str->base.base.loc, ASRUtils::EXPR2VAR(result_var)->m_type));

                    // Make an implicit deallocate before allocating the return var (handles when allocate is in a do while loop)
                    insert_implicit_deallocate(result_var);

                    // Create allocate statement
                    Vec<ASR::alloc_arg_t> v;
                    v.reserve(al, 1);
                    ASR::alloc_arg_t alloc_arg{};
                    alloc_arg.m_a = result_var;
                    alloc_arg.m_dims = nullptr;
                    alloc_arg.n_dims = 0;
                    alloc_arg.m_len_expr = len_expr_to_allocate_with;
                    alloc_arg.m_type = nullptr;
                    v.push_back(al, alloc_arg);
                    pass_result.push_back(al,
                        ASRUtils::STMT(ASR::make_Allocate_t(al, str->base.base.loc, v.p, 1, nullptr, nullptr, nullptr)));    
                }
            }
            // Create new call args with `result_var` as last argument capturing return + Create a `subroutineCall`.
            Vec<ASR::call_arg_t> new_call_args;
            new_call_args.reserve(al,1);
            new_call_args.from_pointer_n_copy(al, x->m_args, x->n_args);
            new_call_args.push_back(al, {result_var->base.loc, result_var});
            ASR::stmt_t* subrout_call = ASRUtils::STMT(ASRUtils::make_SubroutineCall_t_util(al, x->base.base.loc,
                                                x->m_name, nullptr, new_call_args.p, new_call_args.size(), x->m_dt,
                                                nullptr, false, current_scope));
            // replace functionCall with `result_var` + push subroutineCall into the body.
            *current_expr = result_var;
            pass_result.push_back(al, subrout_call);
        }
    }
};
class ReplaceFunctionCallWithSubroutineCallVisitor:
    public ASR::CallReplacerOnExpressionsVisitor<ReplaceFunctionCallWithSubroutineCallVisitor> {

    private:

        Allocator& al;
        Vec<ASR::stmt_t*> pass_result;
        ReplaceFunctionCallWithSubroutineCall replacer;
        bool remove_original_statement = false;
        Vec<ASR::stmt_t*>* parent_body = nullptr;
        int result_counter = 0;


    public:

        ReplaceFunctionCallWithSubroutineCallVisitor(Allocator& al_): al(al_), replacer(al, pass_result)
        {
            pass_result.n = 0;
            pass_result.reserve(al, 1);
        }

        void call_replacer(){
            replacer.current_expr = current_expr;
            replacer.current_scope = current_scope;
            replacer.replace_expr(*current_expr);
        }

        void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
            Vec<ASR::stmt_t*> body;
            body.reserve(al, n_body);
            if(!pass_result.empty()){  // Flush `pass_result`.
                LCOMPILERS_ASSERT(parent_body != nullptr);
                for(size_t i = 0; i < pass_result.size(); i++){
                    parent_body->push_back(al, pass_result[i]);
                }
                pass_result.n = 0;
            }
            bool remove_original_statement_copy = remove_original_statement;
            for (size_t i = 0; i < n_body; i++) {
                parent_body = &body;
                remove_original_statement = false;
                visit_stmt(*m_body[i]);
                if( pass_result.size() > 0 ) {
                    for (size_t j=0; j < pass_result.size(); j++) {
                        body.push_back(al, pass_result[j]);
                    }
                    pass_result.n = 0;
                }
                if (!remove_original_statement){
                    body.push_back(al, m_body[i]);
                }
            }
            remove_original_statement = remove_original_statement_copy;
            m_body = body.p;
            n_body = body.size();
        }

        bool is_function_call_returning_aggregate_type(ASR::expr_t* m_value) {
            bool is_function_call = ASR::is_a<ASR::FunctionCall_t>(*m_value);
            if (!is_function_call)
                return false;

            ASR::FunctionCall_t *fc = ASR::down_cast<ASR::FunctionCall_t>(m_value);
            if (ASRUtils::is_elemental(fc->m_name) && ASRUtils::is_array(fc->m_type)) {
                ASR::Array_t* array_t = ASR::down_cast<ASR::Array_t>(fc->m_type);

                // Check if the type of elements in the array is aggregate or not
                return ASRUtils::is_aggregate_type(array_t->m_type) ||
                    PassUtils::is_aggregate_or_array_type(array_t->m_type);
            }

            return (ASRUtils::is_aggregate_type(ASRUtils::expr_type(m_value)) ||
                    PassUtils::is_aggregate_or_array_type(m_value));
        }

        void subroutine_call_from_function(const Location &loc, ASR::stmt_t &xx) {
            ASR::expr_t* value = nullptr;
            ASR::expr_t* target = nullptr;
            if (ASR::is_a<ASR::Assignment_t>(xx)) {
                ASR::Assignment_t* assignment = ASR::down_cast<ASR::Assignment_t>(&xx);
                value = assignment->m_value;
                target = assignment->m_target;
            } else if (ASR::is_a<ASR::Associate_t>(xx)) {
                ASR::Associate_t* associate = ASR::down_cast<ASR::Associate_t>(&xx);
                value = associate->m_value;
                target = associate->m_target;
            } else {
                LCOMPILERS_ASSERT("Only Assignment_t and Associate_t allowed.");
            }
            ASR::FunctionCall_t* fc = ASR::down_cast<ASR::FunctionCall_t>(value);

            ASR::symbol_t* func_sym = ASRUtils::symbol_get_past_external(fc->m_name);
            if(ASR::is_a<ASR::Function_t>(*func_sym)) {
                ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(func_sym);
                ASR::ttype_t* func_type = func->m_function_signature;
                if(ASR::is_a<ASR::FunctionType_t>(*func_type)){
                    ASR::FunctionType_t* func_type_type = ASR::down_cast<ASR::FunctionType_t>(func_type);
                    if (func_type_type->m_abi == ASR::abiType::BindC) {
                        return; // Skip transformation for bind(C) functions
                    }
                }
            }

            bool use_temp_var_for_return = false;
            bool check_use_temp_var = !ASRUtils::is_allocatable_or_pointer(ASRUtils::expr_type(target));          // TODO: Find a way to handle allocatables too
            Vec<ASR::call_arg_t> s_args;
            s_args.reserve(al, fc->n_args + 1);
            for( size_t i = 0; i < fc->n_args; i++ ) {
                s_args.push_back(al, fc->m_args[i]);

                if (check_use_temp_var && ASRUtils::expr_equal(target, fc->m_args[i].m_value)) {      //TODO: Write a utility function that checks if 2 expr are the SAME
                    use_temp_var_for_return = true;
                }
            }

            if (check_use_temp_var && fc->m_dt && ASRUtils::expr_equal(target, fc->m_dt)) {       //TODO: Write a utility function that checks if 2 expr are the SAME
                use_temp_var_for_return = true;
            }

            if (use_temp_var_for_return) {
                ASR::expr_t *result_var = nullptr;

                if (ASRUtils::is_array(ASRUtils::expr_type(target))) {
                    result_var = create_temporary_variable_for_array(al, target, current_scope, "_subroutine_from_function_");      //TODO: move this function impl & definition from array_struct_temporary.cpp file to pass_utils
                } else {
                    result_var = create_temporary_variable_for_scalar(al, target, current_scope, "_subroutine_from_function_");     //TODO: move this function impl & definition from array_struct_temporary.cpp file to pass_utils
                }

                insert_allocate_stmt_for_array(al, result_var, value, &pass_result);
                target = result_var;
            }

            bool value_and_target_allocatable_array = false;
            if (ASRUtils::is_allocatable(value) && ASRUtils::is_allocatable(target)) {
                // Pass in a temporary instead of the target, this is done for bounds checking in assignment to an array from a FunctionCall
                if (ASRUtils::is_array(ASRUtils::expr_type(target)) &&
                    ASRUtils::is_array(ASRUtils::expr_type(value)) &&
                    ASR::is_a<ASR::Assignment_t>(xx)) {
                    target = create_temporary_variable_for_array(al, target, current_scope, "_subroutine_from_function_");
                    value_and_target_allocatable_array = true;
                }
                // Make sure to deallocate the temporary that will hold the return of function.
                Vec<ASR::expr_t*> to_be_deallocated;
                to_be_deallocated.reserve(al, 1);
                to_be_deallocated.push_back(al, target);
                pass_result.push_back(al, ASRUtils::STMT(
                    ASR::make_ImplicitDeallocate_t(al, target->base.loc,
                    to_be_deallocated.p, to_be_deallocated.size())));
            }
            ASR::call_arg_t result_arg;
            result_arg.loc = target->base.loc;
            result_arg.m_value = target;
            s_args.push_back(al, result_arg);
            ASR::stmt_t* subrout_call = ASRUtils::STMT(ASRUtils::make_SubroutineCall_t_util(al, loc,
                fc->m_name, fc->m_original_name, s_args.p, s_args.size(), fc->m_dt, nullptr, false, current_scope));
            pass_result.push_back(al, subrout_call);

            if (value_and_target_allocatable_array || use_temp_var_for_return) {
                if (ASR::is_a<ASR::Assignment_t>(xx)) {
                    ASR::Assignment_t* assignment = ASR::down_cast<ASR::Assignment_t>(&xx);
                    assignment->m_value = target;

                    if (value_and_target_allocatable_array) {
                        // We are using a temporary so make this assignment a move assignment
                        assignment->m_move_allocation = true;
                    }
                } else {
                    ASR::Associate_t* associate = ASR::down_cast<ASR::Associate_t>(&xx);
                    associate->m_value = target;
                }

                remove_original_statement = false;
            } else {
                remove_original_statement = true;
            }
        }

        void visit_Assignment(const ASR::Assignment_t &x) {
            ASR::CallReplacerOnExpressionsVisitor \
            <ReplaceFunctionCallWithSubroutineCallVisitor>::visit_Assignment(x);
            if(is_function_call_returning_aggregate_type(x.m_value)) {
                ASR::Assignment_t& xx = const_cast<ASR::Assignment_t&>(x);
                subroutine_call_from_function(x.base.base.loc, (ASR::stmt_t &)xx);
            }
        }

        void visit_Associate(const ASR::Associate_t &x) {
            ASR::CallReplacerOnExpressionsVisitor \
            <ReplaceFunctionCallWithSubroutineCallVisitor>::visit_Associate(x);
            ASR::ttype_t* t = ASRUtils::extract_type(ASRUtils::expr_type(x.m_target));
            if(is_function_call_returning_aggregate_type(x.m_value) && ASR::is_a<ASR::StructType_t>(*t)) {
                ASR::Associate_t& xx = const_cast<ASR::Associate_t&>(x);
                subroutine_call_from_function(x.base.base.loc, (ASR::stmt_t &)xx);
            }
        }
};

void pass_create_subroutine_from_function(Allocator &al, ASR::TranslationUnit_t &unit,
                                          const LCompilers::PassOptions& /*pass_options*/) {
    CreateFunctionFromSubroutine v(al);
    v.visit_TranslationUnit(unit);
    ReplaceFunctionCallWithSubroutineCallVisitor u(al);
    u.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor w(al);
    w.visit_TranslationUnit(unit);
}


} // namespace LCompilers
