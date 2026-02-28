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


/**
 * @class CreateFunctionFromSubroutine
 * @brief This pass transforms functions that return aggregate types (like arrays, structs, strings, etc.) into subroutines.
 * @see @ref doc/src/passes/subroutine_from_function.md
 */
class CreateFunctionFromSubroutine: public ASR::BaseWalkVisitor<CreateFunctionFromSubroutine> {

private :

        Allocator& al;
        // Mapping Modified Functions With What Used To Be Its Return Type -- It's Useful Later When Allocating a Temporary is Needed For FuncCall Return.
        std::unordered_map<ASR::Function_t*, ASR::ttype_t*> &Function__TO__ReturnType_MAP_;

public:

        /* Constructor */
        CreateFunctionFromSubroutine(
            Allocator &al_,
            std::unordered_map<ASR::Function_t*, ASR::ttype_t*> &Function__ReturnType_MAP)
            : al(al_), Function__TO__ReturnType_MAP_(Function__ReturnType_MAP){}


        void visit_Function(const ASR::Function_t& x) {
            ASR::Function_t* x_ptr = &const_cast<ASR::Function_t&>(x);
            ASR::ttype_t* const return_type = ASRUtils::get_FunctionType(x_ptr)->m_return_var_type;
            
            /* Transform This Function Into Subroutine IF NEEDED */
            bool transform_success = PassUtils::handle_fn_return_var(al, x_ptr, PassUtils::is_aggregate_or_array_or_nonPrimitive_type);
            if(transform_success) {Function__TO__ReturnType_MAP_[x_ptr] = return_type;}

            /* Visit Functions In Current SymTable */
            for (auto &str_sym_pair : x.m_symtab->get_scope()) {
                if (ASR::is_a<ASR::Function_t>(*str_sym_pair.second)) {
                    this->visit_Function(*down_cast<ASR::Function_t>(str_sym_pair.second));
                }
            }

            for (auto &a : x.m_symtab->get_scope()) {
                if (ASR::is_a<ASR::Variable_t>(*a.second) && 
                    ASR::is_a<ASR::FunctionType_t>(*ASRUtils::extract_type(ASRUtils::symbol_type(a.second)))) {
                    this->visit_Variable(*down_cast<ASR::Variable_t>(a.second));
                }
            }
        }

        void visit_Program(const ASR::Program_t &x){ // Avoid visiting Body + Just Visit Functions
            /* Visit Functions In Current SymTable */
            for (auto &str_sym_pair : x.m_symtab->get_scope()) {
                if (ASR::is_a<ASR::Function_t>(*str_sym_pair.second)) {
                    this->visit_Function(*down_cast<ASR::Function_t>(str_sym_pair.second));
                }
            }
            for (auto &str_sym_pair : x.m_symtab->get_scope()) {
                if (ASR::is_a<ASR::Variable_t>(*str_sym_pair.second) && 
                    ASR::is_a<ASR::FunctionType_t>(*ASRUtils::extract_type(ASRUtils::symbol_type(str_sym_pair.second)))) {
                    this->visit_Variable(*down_cast<ASR::Variable_t>(str_sym_pair.second));
                }
            }
        }

        void visit_Module(const ASR::Module_t &x) {
            for (auto &a : x.m_symtab->get_scope()) {
                if (ASR::is_a<ASR::Function_t>(*a.second)) {
                    this->visit_Function(*down_cast<ASR::Function_t>(a.second));
                }
            }
            for (auto &a : x.m_symtab->get_scope()) {
                if (ASR::is_a<ASR::Variable_t>(*a.second) && 
                    ASR::is_a<ASR::FunctionType_t>(*ASRUtils::extract_type(ASRUtils::symbol_type(a.second)))) {
                    this->visit_Variable(*down_cast<ASR::Variable_t>(a.second));
                }
            }
        }

        void visit_Variable(const ASR::Variable_t &x){
            ASR::Variable_t* x_ptr = &const_cast<ASR::Variable_t&>(x);
            if(ASR::is_a<ASR::FunctionType_t>(*ASRUtils::extract_type(x.m_type))){
                ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(ASRUtils::symbol_get_past_external(x_ptr->m_type_declaration));
                ASR::ttype_t* new_type = func->m_function_signature;
                if (ASR::is_a<ASR::Pointer_t>(*x.m_type)) {
                    new_type = ASRUtils::TYPE(ASR::make_Pointer_t(al, x.base.base.loc, new_type));
                }
                x_ptr->m_type = new_type;
            }
        }


};

/**
 * @class AllocateVarBasedOnFuncCall
 * @brief This class is responsible for inserting an ALLOCATE statement for a variable based on the return type of a function call.
 *
 * @details
 * >>> Why This Class :
 *      Avoiding double function calls -- One inside allocation stmt, another in the original functionCall itself
 *
 * @see @ref doc/passes/subroutine_from_function.md
 */
class AllocateVarBasedOnFuncCall : public ASR::BaseExprReplacer<AllocateVarBasedOnFuncCall> {
        
private :

    Allocator            &al_;
    ASR::FunctionCall_t  *f_call_; // FunctionCall that we're allocating a var based on.
    SymbolTable          *current_scope_;
    Vec<ASR::stmt_t*>    &pass_result_;

    AllocateVarBasedOnFuncCall(
        Allocator           &al,
        ASR::FunctionCall_t *f_call,
        SymbolTable         *current_scope,
        Vec<ASR::stmt_t*>   &pass_result)
        :al_(al), f_call_(f_call), current_scope_(current_scope), pass_result_(pass_result) {}


    /// Inserts Allocate Statement for `var_to_allocate` based on the information from `funcCall_ret_type`
    void insert_allocate_stmt(ASR::Var_t* var_to_allocate, ASR::ttype_t* funcCall_ret_type){
        /* Assertions */
        LCOMPILERS_ASSERT_MSG(   ASRUtils::is_array(funcCall_ret_type) 
                                || ASRUtils::is_string_only(funcCall_ret_type),
                                "Cannot allocate type -- Only String and Array types are supported.")

        ASR::dimension_t* array_m_dims { };
        size_t            array_n_dims {0};
        ASR::expr_t* allocate_len_expr { };

        /* Set `m_dims` + `n_dims`` (if found) */
        if(ASRUtils::is_array(funcCall_ret_type)){
            ASR::Array_t* array_type = down_cast<ASR::Array_t>(ASRUtils::extract_type(funcCall_ret_type));
            array_n_dims = array_type->n_dims;
            array_m_dims = array_type->m_dims;
        }

        /* Set `len_expr` (if found)*/
        if(ASRUtils::is_character(*funcCall_ret_type)){
            allocate_len_expr = ASRUtils::get_string_type(funcCall_ret_type)->m_len;
        }

        /* Create Statement + Push it*/
        ASR::stmt_t* allocate_stmt =  ASRUtils::ASRBuilder(al_, f_call_->base.base.loc).
                                        Allocate(&var_to_allocate->base, array_m_dims, array_n_dims, allocate_len_expr);
        pass_result_.push_back(al_, allocate_stmt);
    }

    /**
     * === Look Up Symbol In Current Scope -- If Not Found, Create An External Symbol ===
     *  We're Traversing Function Return Node. Symbols Like (Var, FunctionCall, ..) Might Need ExternalSymbol
     *  
     *  EXAMPLE ::: return_var_type ==> ` (String 1 (foo()) ExpressionLength DescriptorString) `
     *    
     *  An ExternalSymbol should be already in the current scope, but we might not be able 
     *  to get it due to name mangling -- So just create a one.
     */
    ASR::symbol_t* get_resolved_symbol(ASR::symbol_t* sym){
        ASR::Module_t *sym_mod = ASRUtils::get_sym_module(sym);
        if(ASR::symbol_t* func = current_scope_->resolve_symbol(ASRUtils::symbol_name(sym))){
            auto const sym_deep  =  ASRUtils::symbol_get_past_external(sym);
            auto const func_deep =  ASRUtils::symbol_get_past_external(func);
            if(sym_deep == func_deep) return func; // Found In Current Scope -- Do Nothing
            if (!sym_mod) {
                return func;
            }
        }
        if (!sym_mod) {
            return sym;
        }

        /* Not Found In Current Scope -- Create An External Symbol */
        char* const unique_name = s2c(al_, current_scope_->get_unique_name(ASRUtils::symbol_name(sym)));
        auto ext_sym = ASR::down_cast<ASR::symbol_t>(
                ASR::make_ExternalSymbol_t(al_, sym->base.loc, 
                                    current_scope_, unique_name, sym,
                                    sym_mod->m_name, nullptr, 0,
                                    ASRUtils::symbol_name(sym), ASR::Private));
        current_scope_->add_symbol(unique_name, ext_sym);
        return ext_sym;
    }

public : 

    void replace_FunctionParam(ASR::FunctionParam_t* x){

        ASR::expr_t* fnCall_argument {};
        fnCall_argument = ASRUtils::get_past_array_physical_cast(f_call_->m_args[x->m_param_number].m_value); // Cleaned Up

        if(ASR::is_a<ASR::FunctionCall_t>(*fnCall_argument)){ // We have to resolve the issue of double evaluation
            /* Create Temporary Variable To Hold Call Return -- We'll Re-use The Temp Instead of Re-evaluating*/
            ASR::expr_t* temp_var {};
            {
                static int cnt = 0;
                ASR::ttype_t* temp_t  = ASRUtils::expr_type(fnCall_argument);
                // NOTE : We depend on the fact that the FuncCall is simple enough that its return doesn't need any special handling.
                temp_var = PassUtils::create_var(cnt++, "funcCall_temp_var", x->base.base.loc, temp_t, al_, current_scope_);
            }

            /* Create Assignment ---> `funcCall_temp_var = f()` --- Push Assignment Statement */
            ASR::stmt_t* assignment_stmt = ASRUtils::STMT(ASR::make_Assignment_t(al_, f_call_->base.base.loc,temp_var, fnCall_argument, nullptr, false, false));
            pass_result_.push_back(al_, assignment_stmt);                            

            /* Replace Current FuncParam With `temp_var` */
            *current_expr = temp_var;

            /* Replace the argument in the functionCall with `temp_var` */
            f_call_->m_args[x->m_param_number].m_value = temp_var;
        } else {
            /* Do Nothing -- Replace Current FuncParam With Argument */
            *current_expr = fnCall_argument;
        }

    }

    void replace_FunctionCall(ASR::FunctionCall_t *x){
        x->m_name = get_resolved_symbol(x->m_name);
        if(x->m_original_name) x->m_original_name = get_resolved_symbol(x->m_original_name);
        ASR::BaseExprReplacer<AllocateVarBasedOnFuncCall>::replace_FunctionCall(x);
    }
    void replace_Var(ASR::Var_t *x){
        x->m_v = get_resolved_symbol(x->m_v);
    }

    /**
     *
     * =================== Entry Point ===================
     *
     */
    static void 
    Allocate( Allocator& al, ASR::FunctionCall_t* f_call, 
    ASR::Var_t* var_to_allocate, SymbolTable* current_scope, Vec<ASR::stmt_t*> &pass_result,
    ASR::ttype_t* func_ret_type = nullptr /*Pass In case Function was modified to subroutine*/) {

        /* Assertions */
        LCOMPILERS_ASSERT(f_call && var_to_allocate && current_scope)
        LCOMPILERS_ASSERT(ASRUtils::is_allocatable(&var_to_allocate->base))

        /* Get Return Type -- Duplicate It -- Set `return_t` */
        ASR::ttype_t* return_t {};
        {
            ASR::ttype_t* return_t_ = ASRUtils::get_FunctionType(ASRUtils::get_function(f_call->m_name))->m_return_var_type;
            if(!return_t_){
                LCOMPILERS_ASSERT_MSG(func_ret_type,
                                        "Must pass Function Return Type manually -- "
                                        "You're now allocating against a functionCall that its"
                                        "function has no return var"
                                        " -- If it got modified into subroutine,"
                                        " You'll probably find type in the Function_returnType MAP.")
                return_t_ = func_ret_type;
            }
            return_t = ASRUtils::duplicate_type(al, return_t_);
        }

        /* Create Instance */
        auto instance = AllocateVarBasedOnFuncCall(al, f_call, current_scope, pass_result);
          
        // Replacing Type Will Make us End Up Having Appropriate Type (No FunctionParam, No FuncCall Inplace of FunctionParam) 
        instance.replace_ttype(return_t);

        /* Insert ALLOCATE Statment */
        instance.insert_allocate_stmt(var_to_allocate, return_t);
    }
};



class ReplaceFunctionCallWithSubroutineCall : public ASR::BaseExprReplacer<ReplaceFunctionCallWithSubroutineCall> {


private :

    Allocator & al;
    int result_counter = 0;
    SymbolTable* &current_scope;
    Vec<ASR::stmt_t*> &pass_result;
    // Mapping Modified Functions With What Used To Be Its Return Type -- It's Useful Later When Allocate Stmt Needed For FuncCall Return
    std::unordered_map<ASR::Function_t*, ASR::ttype_t*> &Function__TO__ReturnType_MAP_;

public:

    ReplaceFunctionCallWithSubroutineCall(
        Allocator                                           &al_,
        SymbolTable*                                        &current_scope,
        Vec<ASR::stmt_t*>                                   &pass_result_,
        std::unordered_map<ASR::Function_t*, ASR::ttype_t*> &Function__TO__ReturnType_MAP) 
        :al(al_),
         current_scope(current_scope),
         pass_result(pass_result_),
         Function__TO__ReturnType_MAP_(Function__TO__ReturnType_MAP) {}

private :

    void insert_implicit_deallocate(ASR::expr_t* result_var) {
        pass_result.push_back(al, ASRUtils::ASRBuilder(al, result_var->base.loc).Deallocate(result_var));
    }


    
    void traverse_functionCall_args(ASR::call_arg_t* call_args, size_t call_args_n){
        for(size_t i = 0; i < call_args_n; i++){
            ASR::expr_t** current_expr_copy = current_expr;
            current_expr = &call_args[i].m_value;
            replace_expr(call_args[i].m_value);
            current_expr = current_expr_copy;
        }
    }

    /// Check if return slot must be allocated before passed to functionCall.
    bool allocate_stmt_needed_for_return_slot(ASR::ttype_t* function_return_t, ASR::ttype_t* return_slot_t){
        LCOMPILERS_ASSERT(
            ASRUtils::type_get_past_allocatable_pointer(function_return_t)->type ==
            ASRUtils::type_get_past_allocatable_pointer(return_slot_t)->type)
            
        if(ASRUtils::is_string_only(function_return_t)){
            LCOMPILERS_ASSERT(ASRUtils::is_string_only(return_slot_t))
            /*
                TRUE WHEN :
                * return slot is     --> `character(:), allocatable :: str`
                * function return is --> `character(len=expr) :: ret`

            */
            return  ASRUtils::is_allocatable(return_slot_t)
                &&  ASRUtils::get_string_type(return_slot_t)->m_len_kind == ASR::DeferredLength
                && !ASRUtils::is_allocatable(function_return_t) 
                &&  ASRUtils::get_string_type(function_return_t)->m_len_kind == ASR::ExpressionLength;

        } else if (ASRUtils::is_array(function_return_t)) {
            return ASRUtils::is_allocatable(return_slot_t)
                && !ASRUtils::is_pointer(return_slot_t);
        } else if (   ASR::is_a<ASR::List_t> (*function_return_t)
                   || ASR::is_a<ASR::Dict_t> (*function_return_t)
                   || ASR::is_a<ASR::Set_t>  (*function_return_t)
                   || ASR::is_a<ASR::Tuple_t>(*function_return_t)) {
            return false;
        } else {
             throw LCompilersException("Unhandled Type : " + ASRUtils::type_to_str_fortran_expr(function_return_t, nullptr));
        }
    }

    /// This function is creating the proper type for return slot variable
    /// WHY? Some return slot variables can't be of the exact type as their function return type.
    ASR::ttype_t* create_type_for_return_slot_var(ASR::ttype_t* function_return_t){
        /* Initially Create Exactly The Same Type */
        ASR::ttype_t* new_type = ASRUtils::duplicate_type(al, function_return_t);

        /* Now Start To Modify Type If Needed */

        /* ================ STRING TYPE ================ */

        /* Handle The Case Of DeferredLength + Non Allocatable (some functions have that return type) */
        {
            const bool allocatable_needed = ASRUtils::is_deferredLength_string(new_type)
                                            && !ASRUtils::is_allocatable(new_type)
                                            && !ASRUtils::is_pointer(new_type);

            if(allocatable_needed) new_type = ASRUtils::TYPE(ASR::make_Allocatable_t(al, new_type->base.loc, new_type));
        }
        /* Handle Case `character(n) :: str` --  Create `character(:), alloactable :: str` */
        {
            const bool allocatable_needed = ASRUtils::is_string_only(new_type)
                                            && !ASRUtils::is_value_constant(ASRUtils::get_string_type(new_type)->m_len)
                                            && ASRUtils::get_string_type(new_type)->m_len_kind == ASR::ExpressionLength
                                            && !ASRUtils::is_allocatable(new_type)
                                            && !ASRUtils::is_pointer(new_type);
            if(allocatable_needed){ 
                ASR::String_t* const str = ASRUtils::get_string_type(new_type);
                str->m_len = nullptr;
                str->m_len_kind = ASR::DeferredLength;
                str->m_physical_type = ASR::DescriptorString;
                new_type = ASRUtils::TYPE(ASR::make_Allocatable_t(al, new_type->base.loc, new_type));
            }
        }
        return new_type;
    }

public :

    void replace_FunctionCall(ASR::FunctionCall_t* x){
        traverse_functionCall_args(x->m_args, x->n_args);
        if (x->m_dt) {
            ASR::expr_t** current_expr_copy = current_expr;
            current_expr = &(x->m_dt);
            replace_expr(x->m_dt);
            current_expr = current_expr_copy;
        }

        ASR::Function_t* func = ASRUtils::get_function(x->m_name);
        bool was_converted = func && Function__TO__ReturnType_MAP_.count(func) > 0;
        if(PassUtils::is_non_primitive_return_type(x->m_type)
            || PassUtils::is_aggregate_or_array_type(x->m_type)){

            // Create variable in current_scope to be holding the return.
            // For converted functions (structs/arrays), pass the last arg as
            // the sibling so create_var can extract the type_decl symbol.
            ASR::expr_t* sibling_var = (was_converted && func->n_args > 0)
                ? func->m_args[func->n_args - 1] : nullptr;
            ASR::expr_t* result_var = PassUtils::create_var(
                                            result_counter++,
                                            "return_slot", x->base.base.loc,
                                            create_type_for_return_slot_var(x->m_type) , al, current_scope, sibling_var);

            /* Make Sure To Deallocate -- To Avoid Douple Allocation With Loops */
            if(ASRUtils::is_allocatable(ASRUtils::expr_type(result_var))) { insert_implicit_deallocate(result_var); }

            bool alloc_needed = false;
            if (ASRUtils::is_array(x->m_type) || PassUtils::is_non_primitive_return_type(x->m_type)) {
                alloc_needed = allocate_stmt_needed_for_return_slot(
                    x->m_type, ASRUtils::expr_type(result_var));
            }
            if(ASRUtils::is_array(x->m_type) && alloc_needed) {
                insert_allocate_stmt_for_array(al, result_var, ASRUtils::EXPR((ASR::asr_t*)x), &pass_result);
            } else if(PassUtils::is_non_primitive_return_type(x->m_type)
                && alloc_needed){

                // FunctionCall to modified function (currently subroutine)
                ASR::ttype_t* alloc_type = was_converted ? Function__TO__ReturnType_MAP_[func] : nullptr;

                AllocateVarBasedOnFuncCall::Allocate(
                    al, x, ASR::down_cast<ASR::Var_t>(result_var),current_scope, pass_result, alloc_type);
            }
            // Create new call args with `result_var` as last argument capturing return + Create a `subroutineCall`.
            Vec<ASR::call_arg_t> new_call_args;
            new_call_args.reserve(al,1);
            new_call_args.from_pointer_n_copy(al, x->m_args, x->n_args);
            new_call_args.push_back(al, {result_var->base.loc, result_var});
            ASR::stmt_t* subrout_call = ASRUtils::STMT(ASRUtils::make_SubroutineCall_t_util(al, x->base.base.loc,
                                                x->m_name, nullptr, new_call_args.p, new_call_args.size(), x->m_dt,
                                                nullptr, false, current_scope, std::nullopt, true));
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

        bool expr_same(ASR::expr_t *a, ASR::expr_t *b) {
            if (a->type != b->type) {
                return false;
            }

            // Get past any array item or struct member to the actual Var
            while (ASR::is_a<ASR::ArrayItem_t>(*a) || ASR::is_a<ASR::StructInstanceMember_t>(*a)) {
                if (ASR::is_a<ASR::ArrayItem_t>(*a)) {
                    a = ASR::down_cast<ASR::ArrayItem_t>(a)->m_v;
                } else {
                    a = ASR::down_cast<ASR::StructInstanceMember_t>(a)->m_v;
                }
            }

            // Get past any array item or struct member to the actual Var
            while (ASR::is_a<ASR::ArrayItem_t>(*b) || ASR::is_a<ASR::StructInstanceMember_t>(*b)) {
                if (ASR::is_a<ASR::ArrayItem_t>(*b)) {
                    b = ASR::down_cast<ASR::ArrayItem_t>(b)->m_v;
                } else {
                    b = ASR::down_cast<ASR::StructInstanceMember_t>(b)->m_v;
                }
            }

            // In normal cases, both a and b should be a Var_t
            LCOMPILERS_ASSERT(ASR::is_a<ASR::Var_t>(*a));
            LCOMPILERS_ASSERT(ASR::is_a<ASR::Var_t>(*b));

            // Check if the 2 expressions refer to the same symbol
            return ASR::down_cast<ASR::Var_t>(a)->m_v == ASR::down_cast<ASR::Var_t>(b)->m_v;
        }

    public:

        ReplaceFunctionCallWithSubroutineCallVisitor(
            Allocator& al_,
            std::unordered_map<ASR::Function_t*, ASR::ttype_t*> &Function__TO__ReturnType_MAP)
            :al(al_), replacer(al, current_scope, pass_result, Function__TO__ReturnType_MAP)
        {
            pass_result.n = 0;
            pass_result.reserve(al, 1);
            visit_expr_after_replacement = false;
        }

        void call_replacer(){
            replacer.current_expr = current_expr;
            replacer.replace_expr(*current_expr);
        }

        void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
            Vec<ASR::stmt_t*> body;
            body.reserve(al, n_body);
            if(!pass_result.empty()){  // Flush `pass_result`.
                if (parent_body != nullptr) {
                    for(size_t i = 0; i < pass_result.size(); i++){
                        parent_body->push_back(al, pass_result[i]);
                    }
                } else {
                    for(size_t i = 0; i < pass_result.size(); i++){
                        body.push_back(al, pass_result[i]);
                    }
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
            parent_body = nullptr; // Avoid dangling pointer bugs
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

            return PassUtils::is_aggregate_or_array_type(m_value);
        }

        bool subroutine_call_from_function(const Location &loc, ASR::stmt_t &xx) {
            ASR::expr_t* value = nullptr;
            ASR::expr_t* target = nullptr;
            bool is_pointer_return = false;
            bool use_temp_var_for_return = false;
            if (ASR::is_a<ASR::Assignment_t>(xx)) {
                ASR::Assignment_t* assignment = ASR::down_cast<ASR::Assignment_t>(&xx);
                value = assignment->m_value;
                target = assignment->m_target;
                use_temp_var_for_return = (ASRUtils::is_pointer(ASRUtils::expr_type(assignment->m_value)) &&
                                            !ASRUtils::is_pointer(ASRUtils::expr_type(assignment->m_target)));
                is_pointer_return = use_temp_var_for_return;
            } else if (ASR::is_a<ASR::Associate_t>(xx)) {
                ASR::Associate_t* associate = ASR::down_cast<ASR::Associate_t>(&xx);
                value = associate->m_value;
                target = associate->m_target;
            } else {
                LCOMPILERS_ASSERT("Only Assignment_t and Associate_t allowed.");
            }
            ASR::FunctionCall_t* fc = ASR::down_cast<ASR::FunctionCall_t>(value);

            // We skip the generic expression visitor on this statement, so
            // ensure nested calls inside the function call arguments are still replaced.
            for (size_t i = 0; i < fc->n_args; i++) {
                ASR::expr_t** current_expr_copy = current_expr;
                current_expr = &fc->m_args[i].m_value;
                call_replacer();
                current_expr = current_expr_copy;
            }
            if (fc->m_dt) {
                ASR::expr_t** current_expr_copy = current_expr;
                current_expr = &fc->m_dt;
                call_replacer();
                current_expr = current_expr_copy;
            }

            ASR::symbol_t* func_sym = ASRUtils::symbol_get_past_external(fc->m_name);
            if(ASR::is_a<ASR::Function_t>(*func_sym)) {
                ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(func_sym);
                ASR::ttype_t* func_type = func->m_function_signature;
                if(ASR::is_a<ASR::FunctionType_t>(*func_type)){
                    ASR::FunctionType_t* func_type_type = ASR::down_cast<ASR::FunctionType_t>(func_type);
                    if (func_type_type->m_abi == ASR::abiType::BindC) {
                        return false; // Skip transformation for bind(C) functions
                    }
                }
            }

            Vec<ASR::call_arg_t> s_args;
            s_args.reserve(al, fc->n_args + 1);
            for( size_t i = 0; i < fc->n_args; i++ ) {
                s_args.push_back(al, fc->m_args[i]);

                if (this->expr_same(target, fc->m_args[i].m_value)) {
                    use_temp_var_for_return = true;
                }
            }

            if (fc->m_dt && this->expr_same(target, fc->m_dt)) {
                use_temp_var_for_return = true;
            }

            // Use a temp var for storing result if target is passed as input to the function, then assign the temp var to the target.
            if (use_temp_var_for_return) {
                ASR::expr_t *result_var = nullptr;

                if (ASRUtils::is_array(ASRUtils::expr_type(target))) {
                    result_var = create_temporary_variable_for_array(al, target, current_scope, "_subroutine_from_function_", is_pointer_return);      //TODO: move this function impl & definition from array_struct_temporary.cpp file to pass_utils
                } else {
                    result_var = create_temporary_variable_for_scalar(al, target, current_scope, "_subroutine_from_function_", is_pointer_return);     //TODO: move this function impl & definition from array_struct_temporary.cpp file to pass_utils
                }

                // It doesn't (and shouldn't) insert anything if result_var isn't array or allocatable
                insert_allocate_stmt_for_array(al, result_var, value, &pass_result);
                target = result_var;
            }

            bool value_and_target_allocatable_array = false;
            if (ASRUtils::is_allocatable(value) && ASRUtils::is_allocatable(target)) {
                // Pass in a temporary instead of the target, this is done for bounds checking in assignment to an array from a FunctionCall
                if (ASRUtils::is_array(ASRUtils::expr_type(target)) &&
                    ASRUtils::is_array(ASRUtils::expr_type(value)) &&
                    ASR::is_a<ASR::Assignment_t>(xx)) {
                    // If we already used a temp var for return don't use it here
                    if (!use_temp_var_for_return) {
                        target = create_temporary_variable_for_array(al, target, current_scope, "_subroutine_from_function_");
                    }
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
                fc->m_name, fc->m_original_name, s_args.p, s_args.size(), fc->m_dt, nullptr, false, current_scope, std::nullopt, true));
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
            return true;
        }

        void visit_Assignment(const ASR::Assignment_t &x) {
            if(is_function_call_returning_aggregate_type(x.m_value)) {
                if (x.m_overloaded) {
                    // User-defined assignment(=) where the RHS is a function
                    // call returning an aggregate type. The array_struct_temporary
                    // pass has already created a temp for the function result
                    // and wired the m_overloaded SubroutineCall to use it.
                    // Just emit the overloaded call and drop this Assignment.
                    pass_result.push_back(al, x.m_overloaded);
                    remove_original_statement = true;
                    return;
                }
                ASR::Assignment_t& xx = const_cast<ASR::Assignment_t&>(x);
                if (subroutine_call_from_function(x.base.base.loc, (ASR::stmt_t &)xx)) {
                    return;
                }
            }
            ASR::CallReplacerOnExpressionsVisitor \
            <ReplaceFunctionCallWithSubroutineCallVisitor>::visit_Assignment(x);
        }

        void visit_Associate(const ASR::Associate_t &x) {
            ASR::ttype_t* t = ASRUtils::extract_type(ASRUtils::expr_type(x.m_target));
            if(is_function_call_returning_aggregate_type(x.m_value) && ASR::is_a<ASR::StructType_t>(*t)) {
                ASR::Associate_t& xx = const_cast<ASR::Associate_t&>(x);
                if (subroutine_call_from_function(x.base.base.loc, (ASR::stmt_t &)xx)) {
                    return;
                }
            }
            ASR::CallReplacerOnExpressionsVisitor \
            <ReplaceFunctionCallWithSubroutineCallVisitor>::visit_Associate(x);
        }

    /**
     * In case `x.test` expression needs temporaries
     *
     * FROM :
     *      do while(ff(flag) == "Hello")
     *       ...
     *      END DO
     * TO :
     *     DO while (.true.)
     *      temp1 = ff(flag)
     *      if ((temp1 == "Hello") == .false.) exit
     *      ...
     *     END DO
     */
    void visit_WhileLoop(const ASR::WhileLoop_t &x){
        Vec<ASR::stmt_t*> pass_result_TMP; // Move pass_result
        pass_result_TMP.p   = pass_result.p;
        pass_result_TMP.n   = pass_result.n;
        pass_result_TMP.max = pass_result.max;

        pass_result.reserve(al, 0); // Reset
        visit_expr(*x.m_test);
        if (!pass_result.empty()){ // Temps Created!
            ASRUtils::ASRBuilder builder(al, x.base.base.loc);
            pass_result.push_back(al, builder.If(builder.Eq(x.m_test, builder.logical_false()), {builder.Exit()}, {}));
            for(size_t i = 0; i< x.n_body; i++){
                pass_result.push_back(al, x.m_body[i]);
            }
            const_cast<ASR::WhileLoop_t&>(x).m_body = pass_result.p; 
            const_cast<ASR::WhileLoop_t&>(x).n_body = pass_result.n;
            const_cast<ASR::WhileLoop_t&>(x).m_test = builder.logical_true();
        }
        // Move back
        pass_result.p   = pass_result_TMP.p;
        pass_result.n   = pass_result_TMP.n;
        pass_result.max = pass_result_TMP.max;
        CallReplacerOnExpressionsVisitor::visit_WhileLoop(x);      
    }
};

void pass_create_subroutine_from_function(Allocator &al, ASR::TranslationUnit_t &unit,
                                          const LCompilers::PassOptions& /*pass_options*/) {
    std::unordered_map<ASR::Function_t*, ASR::ttype_t*> Function__TO__ReturnType_MAP;
    CreateFunctionFromSubroutine v(al,Function__TO__ReturnType_MAP);
    v.visit_TranslationUnit(unit);
    ReplaceFunctionCallWithSubroutineCallVisitor u(al, Function__TO__ReturnType_MAP);
    u.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor w(al);
    w.visit_TranslationUnit(unit);
}


} // namespace LCompilers
