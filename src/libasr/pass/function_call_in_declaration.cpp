#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/asr_builder.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/replace_function_call_in_declaration.h>
#include <libasr/pass/intrinsic_array_function_registry.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/pickle.h>

#include <set>

namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

/*

This ASR pass replaces function calls in declarations with a new function call.
The function `pass_replace_function_call_in_declaration` transforms the ASR tree inplace.

Converts:

pure function diag_rsp_mat(A) result(res)
real, intent(in) :: A(:,:)
real :: res(minval(shape(A)))

res = 123.71_4
end function diag_rsp_mat

To:

pure integer function __lcompilers_created_helper_function_(A) result(r)
real, intent(in) :: A(:,:)
r = minval(shape(A))
end function __lcompilers_created_helper_function_

pure function diag_rsp_mat(A) result(res)
real, intent(in) :: A(:,:)
real :: res(__lcompilers_created_helper_function_(A))

res = 123.71_4
end function diag_rsp_mat

*/

/*
    The pass is necessary for passes:
        array_struct_temporary + subroutine_from_function
*/

class ReplaceFunctionCall : public ASR::BaseExprReplacer<ReplaceFunctionCall>
{
private :
    /*
        *Used to iterate over the functionCall node (function call in declaration)
        to get the externalSymbols, so we can duplicate them again in the new helper function

        * e.g. : `character(foo_ret_int(foo_ret_char())) :: str`
        assume `foo_ret_char` is an externalSymbol in the current function, moving the call into
        the helper function requires creating an externalSymbol node tailored for the new helper function scope.
    */ 
    class getExternalSymbol : public ASR::BaseWalkVisitor<getExternalSymbol>{
        std::vector<std::pair<ASR::ExternalSymbol_t*,ASR::symbol_t**>> &collected_external_symbols; // Collector
        public :

        getExternalSymbol
        (std::vector<std::pair<ASR::ExternalSymbol_t*,ASR::symbol_t**>> &v):collected_external_symbols(v){
            
        }
        void visit_expr(const ASR::expr_t &x){
            if(x.type == ASR::FunctionCall){
                if(ASR::is_a<ASR::ExternalSymbol_t>(*ASR::down_cast<ASR::FunctionCall_t>(&x)->m_name)){
                    ASR::FunctionCall_t* func_call = ASR::down_cast<ASR::FunctionCall_t>(&x);
                    collected_external_symbols.push_back({
                        ASR::down_cast<ASR::ExternalSymbol_t>(func_call->m_name),
                        &func_call->m_name
                    });
                }
            } 
            ASR::BaseVisitor<getExternalSymbol>::visit_expr(x);
        }
    };

    std::vector<std::pair<ASR::ExternalSymbol_t*,ASR::symbol_t**>> get_externalSymbols(ASR::expr_t* expr){
        std::vector<std::pair<ASR::ExternalSymbol_t*,ASR::symbol_t**>> v;
        getExternalSymbol get_external_symbols(v);
        get_external_symbols.visit_expr(*expr);
        return v;
    }

    void collect_and_create_new_externalSymbols(ASR::expr_t* expr){
        LCOMPILERS_ASSERT(new_function_scope && expr);
        std::vector<std::pair<ASR::ExternalSymbol_t*,ASR::symbol_t**>> 
            externalSymbols_vec = get_externalSymbols(expr);
        for(auto &ext_sym : externalSymbols_vec){
            // The same function may be called more than once in the expression.
            ASR::symbol_t* already_duplicated = new_function_scope->get_symbol(ext_sym.first->m_name);
            if (already_duplicated) {
                *ext_sym.second = already_duplicated;
                continue;
            }
            ASRUtils::SymbolDuplicator sym_duplicator_instance(al);
            ASR::symbol_t* extSym_duplicated =  
                sym_duplicator_instance.duplicate_ExternalSymbol(ext_sym.first, new_function_scope);
            new_function_scope->add_symbol(ext_sym.first->m_name, extSym_duplicated);
            *ext_sym.second = extSym_duplicated;
        }
    }
    
public:
    Allocator& al;
    SymbolTable* new_function_scope = nullptr;
    std::map<ASR::symbol_t*, ASR::symbol_t*> helper_arguments;
    SymbolTable* &current_scope; // Dependency -- Passed by visitor -- Avoids maintaining 2 separate variables
    ASR::expr_t* assignment_value = nullptr;
    ASR::expr_t* call_for_return_var = nullptr;
    Vec<ASR::expr_t*>* newargsp = nullptr;
    ASR::TranslationUnit_t &tt;

    struct ArgInfo {
        int arg_number;
        ASR::ttype_t* arg_type;
        ASR::expr_t* arg_expr;
        ASR::expr_t* arg_param;
    };

    ReplaceFunctionCall(Allocator &al_, ASR::TranslationUnit_t& tt, SymbolTable* &current_scope_visitor_ref) 
    : al(al_),  current_scope(current_scope_visitor_ref), tt(tt) {}

    void replace_Var(ASR::Var_t* x) {
        if ( newargsp == nullptr) {
            return ;
        }
        if ( new_function_scope == nullptr ) {
            return ;
        }
        auto helper_argument = helper_arguments.find(x->m_v);
        ASR::symbol_t* new_sym = helper_argument != helper_arguments.end()
            ? helper_argument->second
            : new_function_scope->get_symbol(ASRUtils::symbol_name(x->m_v));
        *current_expr = ASRUtils::EXPR(ASR::make_Var_t(al, x->base.base.loc, new_sym));
    }

    /*
        Adds the argument of a helper function through which the helper reads
        the variable `sym` of the helped scope. Different variables, e.g. two
        host- or use-associated ones whose names clash in the helper, always
        get different arguments; `helper_arguments` maps each variable to its
        argument for replace_Var.
    */
    ASR::symbol_t* add_helper_argument(ASR::symbol_t* sym, SymbolTable* new_scope,
            ASRUtils::SymbolDuplicator &sd) {
        ASR::symbol_t* original = ASRUtils::symbol_get_past_external(sym);
        ASR::symbol_t* new_sym = nullptr;
        if (ASR::is_a<ASR::Variable_t>(*original)) {
            ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(original);
            new_sym = sd.duplicate_Variable(v, new_scope);
            LCOMPILERS_ASSERT(new_sym)
            ASR::Variable_t* temp_var = ASR::down_cast<ASR::Variable_t>(new_sym);
            std::string name = new_scope->get_symbol(v->m_name)
                ? new_scope->get_unique_name(std::string(v->m_name), false)
                : std::string(v->m_name);
            if (temp_var->m_intent == ASR::intentType::Local) {
                new_sym = ASR::down_cast<ASR::symbol_t>(
                    ASRUtils::make_Variable_t_util(al, new_sym->base.loc, temp_var->m_parent_symtab,
                    s2c(al, name), temp_var->m_dependencies, temp_var->n_dependencies, ASR::intentType::In,
                    nullptr, nullptr, ASR::storage_typeType::Default, temp_var->m_type,
                    temp_var->m_type_declaration, temp_var->m_abi, temp_var->m_access,
                    ASR::presenceType::Required, temp_var->m_value_attr, temp_var->m_target_attr));
            } else {
                temp_var->m_name = s2c(al, name);
            }
            new_scope->add_symbol(name, new_sym);
        } else {
            sd.duplicate_symbol(original, new_scope);
            new_sym = new_scope->get_symbol(ASRUtils::symbol_name(original));
        }
        helper_arguments[sym] = new_sym;
        return new_sym;
    }

    // TODO : This replacer should be in a dedicated replacer class, rather than implementing it in the same current replacer. 
    void replace_FunctionParam(ASR::FunctionParam_t* x) {
        if( newargsp == nullptr ) return ; // If not preparing the helper function -- RETURN.

        // FunctionParam in new helper function could be pointing to the wrong arguments.
        // It'll be pointing to arguments indices in helped-function scope while the helper function has new-different indices arrangement. 
        // We'll depend on the fact that variables' names in both HELPER function and the HELPED function are the exact same
        // So we can pick the correct argument.
        LCOMPILERS_ASSERT(current_scope && current_scope->asr_owner)
        ASR::Function_t* func = ASR::down_cast2<ASR::Function_t>(current_scope->asr_owner);
        ASR::Variable_t* v = ASRUtils::EXPR2VAR(func->m_args[x->m_param_number]);
        char* const name_in_helped_func = v->m_name;

        // Match on Symbol name -- Use argument from `newargsp` -- replace current
        for(size_t i = 0; i < newargsp->n; i++) {
            char* const name_in_helper_func = ASRUtils::symbol_name(down_cast<ASR::Var_t>((*newargsp)[i])->m_v);
            if( std::strcmp(name_in_helper_func, name_in_helped_func) == 0 ){
                *current_expr = newargsp->p[i];
                return;
            }
        }
        // If everthing was fine, Function would've returned earlier -- Now it's not so raise ERROR.
        throw LCompilersException("Argument Not Found -- FuncParam Points to an argument that is likely not in the current scope");
    }

    void replace_FunctionParam_with_FunctionArgs(ASR::expr_t*& value, Vec<ASR::expr_t*>& new_args) {
        if( !value ) {
            return ;
        }
        newargsp = &new_args;
        ASR::expr_t** current_expr_copy = current_expr;
        current_expr = &value;
        replace_expr(value);
        current_expr = current_expr_copy;
        newargsp = nullptr;
    }

    class get_arg_indices_used 
    : public ASR::BaseWalkVisitor<get_arg_indices_used>{
    private:
        get_arg_indices_used() = default;

        bool exists_in_arginfo(int arg_number, std::vector<ArgInfo>& indices) {
            for (auto info: indices) {
                if (ASR::is_a<ASR::FunctionParam_t>(*info.arg_param) &&
                        info.arg_number == arg_number) return true;
            }
            return false;
        }
        std::vector<ArgInfo> indices {};
        SymbolTable *current_scope {nullptr};
    public : 

        void visit_Function(const ASR::Function_t &x){(void)x;throw LCompilersException("Not expected to visit");}
        void visit_Program(const ASR::Program_t &x)  {(void)x;throw LCompilersException("Not expected to visit");}
        void visit_Module(const ASR::Module_t &x)    {(void)x;throw LCompilersException("Not expected to visit");}
        void visit_FunctionParam(const ASR::FunctionParam_t &x){
            LCOMPILERS_ASSERT(current_scope)
            ASR::Function_t* func = ASR::down_cast2<ASR::Function_t>(current_scope->asr_owner);
            ArgInfo info = {static_cast<int>(x.m_param_number), x.m_type, func->m_args[x.m_param_number], &const_cast<ASR::expr_t&>((x.base))};
            if (!exists_in_arginfo(x.m_param_number, indices)) {
                indices.push_back(info);
            }
        }
        // A variable is identified by its symbol, not by a position in the
        // current scope: host- and use-associated variables are not in it.
        void visit_Var(const ASR::Var_t& x) {
            LCOMPILERS_ASSERT(current_scope)
            ASR::Var_t* xx = &const_cast<ASR::Var_t&>(x);
            for (auto &info: indices) {
                if (ASR::is_a<ASR::Var_t>(*info.arg_param) &&
                        ASR::down_cast<ASR::Var_t>(info.arg_param)->m_v == xx->m_v) {
                    return;
                }
            }
            ArgInfo info = {-1, ASRUtils::expr_type(&xx->base), &xx->base , &xx->base};
            indices.push_back(info);
        }
        // 
        static std::vector<ArgInfo> get(const ASR::expr_t* arg, SymbolTable* current_scope){
            get_arg_indices_used instance {};
            instance.current_scope = current_scope;
            instance.visit_expr(*arg);
            return instance.indices;
        }

    };

    void replace_IntrinsicArrayFunction(ASR::IntrinsicArrayFunction_t *x) {
        if( newargsp != nullptr /*Processing FunctionParam*/) {
            return BaseExprReplacer<ReplaceFunctionCall>::replace_IntrinsicArrayFunction(x);
            
        }
        if (!assignment_value) return;

        std::vector<ArgInfo> indices = get_arg_indices_used::get(&x->base, current_scope);

        SymbolTable* global_scope = current_scope->get_tu_scope();
        SetChar current_function_dependencies; current_function_dependencies.clear(al);
        // The hoisted expression may name a type declared inside a program,
        // which nothing in the global scope can reach and no ExternalSymbol
        // can import, so the helper belongs where that expression stood.
        SymbolTable* helper_parent = PassUtils::instantiation_scope_for_expr(
            global_scope, current_scope, ASRUtils::EXPR((ASR::asr_t*)x));
        SymbolTable* new_scope = al.make_new<SymbolTable>(helper_parent);
        SymbolTable* new_function_scope_copy = new_function_scope;
        new_function_scope = new_scope;

        ASRUtils::SymbolDuplicator sd(al);
        ASRUtils::ASRBuilder b(al, x->base.base.loc);
        Vec<ASR::expr_t*> new_args; new_args.reserve(al, indices.size());
        Vec<ASR::call_arg_t> new_call_args; new_call_args.reserve(al, indices.size());
        Vec<ASR::call_arg_t> args_for_return_var; args_for_return_var.reserve(al, indices.size());

        Vec<ASR::stmt_t*> new_body; new_body.reserve(al, 1);
        std::string new_function_name = global_scope->get_unique_name("__lcompilers_created_helper_function_", false);
        ASR::ttype_t* integer_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x->base.base.loc, 4));
        ASR::expr_t* return_var = b.Variable(new_scope, new_scope->get_unique_name("__lcompilers_return_var_", false), integer_type, ASR::intentType::ReturnVar);

        helper_arguments.clear();
        for (auto arg: indices) {
            ASR::expr_t* arg_expr = arg.arg_expr;
            if (is_a<ASR::Var_t>(*arg_expr)) {
                ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(arg_expr);
                ASR::symbol_t* new_sym = add_helper_argument(var->m_v, new_scope, sd);
                ASR::expr_t* new_var_expr = ASRUtils::EXPR(ASR::make_Var_t(al, var->base.base.loc, new_sym));
                new_args.push_back(al, new_var_expr);
            }
            ASR::call_arg_t new_call_arg; new_call_arg.loc = arg_expr->base.loc; new_call_arg.m_value = arg.arg_param;
            new_call_args.push_back(al, new_call_arg);

            ASR::call_arg_t arg_for_return_var; arg_for_return_var.loc = arg_expr->base.loc; arg_for_return_var.m_value = arg.arg_expr;
            args_for_return_var.push_back(al, arg_for_return_var);
        }

        ASRUtils::ExprStmtDuplicator duplicator(al);
        ASR::expr_t* assignment_value_copy = duplicator.duplicate_expr(assignment_value);
        collect_and_create_new_externalSymbols(assignment_value_copy);
        replace_FunctionParam_with_FunctionArgs(assignment_value_copy, new_args);
        new_body.push_back(al, b.Assignment(return_var, assignment_value_copy));
        ASR::asr_t* new_function = ASRUtils::make_Function_t_util(al, x->base.base.loc,
                    new_scope, s2c(al, new_function_name), current_function_dependencies.p, current_function_dependencies.n,
                    new_args.p, new_args.n,
                    new_body.p, new_body.n,
                    return_var,
                    ASR::abiType::Source, ASR::accessType::Public, ASR::deftypeType::Implementation,
                    nullptr, false, false, false, false, false, nullptr, 0,
                    false, false, false);

        ASR::symbol_t* new_function_sym = ASR::down_cast<ASR::symbol_t>(new_function);
        helper_parent->add_or_overwrite_symbol(new_function_name, new_function_sym);

        ASR::expr_t* new_function_call = ASRUtils::EXPR(ASRUtils::make_FunctionCall_t_util(al, x->base.base.loc,
                        new_function_sym,
                        new_function_sym,
                        new_call_args.p, new_call_args.n,
                        integer_type,
                        nullptr,
                        nullptr
                        ));
        *current_expr = new_function_call;

        ASR::expr_t* function_call_for_return_var = ASRUtils::EXPR(ASRUtils::make_FunctionCall_t_util(al, x->base.base.loc,
                        new_function_sym,
                        new_function_sym,
                        args_for_return_var.p, args_for_return_var.n,
                        integer_type,
                        nullptr,
                        nullptr
                        ));
        call_for_return_var = function_call_for_return_var;
        new_function_scope = new_function_scope_copy;
    }

    /*
        *Replaces an expression that calls a function returning a non-scalar
        (array, struct, character) by a call to a new integer helper function
        that evaluates it. Used for the length member of the ASR::String type
        and for the dimensions of a local array.
        * Handles :
        - `ASR::StrLen`
        - `ASR::FunctionCall`
        - Any other integer expression (when not processing FunctionParam)
    */
    void replace_with_helper_function_call(ASR::expr_t *x) {
        if( newargsp != nullptr /*Processing FunctionParam*/) {
            switch(x->type){
                case ASR::StringLen:
                    return BaseExprReplacer<ReplaceFunctionCall>::replace_StringLen(ASR::down_cast<ASR::StringLen_t>(x));
                case ASR::FunctionCall:
                    return BaseExprReplacer<ReplaceFunctionCall>::replace_FunctionCall(ASR::down_cast<ASR::FunctionCall_t>(x));
                default : 
                    throw LCompilersException("Unhandled case");
            }
        }

        if (!assignment_value) return;

        std::vector<ArgInfo> indices = get_arg_indices_used::get(x, current_scope);
        SymbolTable* global_scope = current_scope->parent;
        SetChar current_function_dependencies; current_function_dependencies.clear(al);
        // The hoisted expression may name a type declared inside a program,
        // which nothing in the global scope can reach and no ExternalSymbol
        // can import, so the helper belongs where that expression stood.
        SymbolTable* helper_parent = PassUtils::instantiation_scope_for_expr(
            global_scope, current_scope, ASRUtils::EXPR((ASR::asr_t*)x));
        SymbolTable* new_scope = al.make_new<SymbolTable>(helper_parent);
        SymbolTable* new_function_scope_copy = new_function_scope;
        new_function_scope = new_scope;

        ASRUtils::SymbolDuplicator sd(al);
        ASRUtils::ASRBuilder b(al, x->base.loc);
        Vec<ASR::expr_t*> new_args; new_args.reserve(al, indices.size());
        Vec<ASR::call_arg_t> new_call_args; new_call_args.reserve(al, indices.size());
        Vec<ASR::call_arg_t> args_for_return_var; args_for_return_var.reserve(al, indices.size());

        Vec<ASR::stmt_t*> new_body; new_body.reserve(al, 1);
        std::string new_function_name = helper_parent->get_unique_name("__lcompilers_created_helper_function_", false);
        ASR::ttype_t* integer_type = ASRUtils::duplicate_type(al, ASRUtils::expr_type(assignment_value));
        ASR::expr_t* return_var = b.Variable(new_scope, new_scope->get_unique_name("__lcompilers_return_var_", false), integer_type, ASR::intentType::ReturnVar);

        helper_arguments.clear();
        for (auto arg: indices) {
            ASR::expr_t* arg_expr = arg.arg_expr;
            if (is_a<ASR::Var_t>(*arg_expr)) {
                ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(arg_expr);
                ASR::symbol_t* new_sym = add_helper_argument(var->m_v, new_scope, sd);
                ASR::expr_t* new_var_expr = ASRUtils::EXPR(ASR::make_Var_t(al, var->base.base.loc, new_sym));
                new_args.push_back(al, new_var_expr);
            }
            ASR::call_arg_t new_call_arg; new_call_arg.loc = arg_expr->base.loc; new_call_arg.m_value = arg.arg_param;
            new_call_args.push_back(al, new_call_arg);

            ASR::call_arg_t arg_for_return_var; arg_for_return_var.loc = arg_expr->base.loc; arg_for_return_var.m_value = arg.arg_expr;
            args_for_return_var.push_back(al, arg_for_return_var);
        }

        ASRUtils::ExprStmtDuplicator duplicator(al);
        ASR::expr_t* assignment_value_copy = duplicator.duplicate_expr(assignment_value);
        replace_FunctionParam_with_FunctionArgs(assignment_value_copy, new_args);
        
        collect_and_create_new_externalSymbols(assignment_value_copy);
        new_body.push_back(al, b.Assignment(return_var, assignment_value_copy));
        ASR::asr_t* new_function = ASRUtils::make_Function_t_util(al, x->base.loc,
                    new_scope, s2c(al, new_function_name), current_function_dependencies.p, current_function_dependencies.n,
                    new_args.p, new_args.n,
                    new_body.p, new_body.n,
                    return_var,
                    ASR::abiType::Source, ASR::accessType::Public, ASR::deftypeType::Implementation,
                    nullptr, false, false, false, false, false, nullptr, 0,
                    false, false, false);

        ASR::symbol_t* new_function_sym = ASR::down_cast<ASR::symbol_t>(new_function);
        helper_parent->add_or_overwrite_symbol(new_function_name, new_function_sym);

        ASR::expr_t* new_function_call = ASRUtils::EXPR(ASRUtils::make_FunctionCall_t_util(al, x->base.loc,
                        new_function_sym,
                        new_function_sym,
                        new_call_args.p, new_call_args.n,
                        integer_type,
                        nullptr,
                        nullptr
                        ));
        *current_expr = new_function_call;

        ASR::expr_t* function_call_for_return_var = ASRUtils::EXPR(ASRUtils::make_FunctionCall_t_util(al, x->base.loc,
                        new_function_sym,
                        new_function_sym,
                        args_for_return_var.p, args_for_return_var.n,
                        integer_type,
                        nullptr,
                        nullptr
                        ));
        call_for_return_var = function_call_for_return_var;
        new_function_scope = new_function_scope_copy;
    }

};

/* ========================= AUTOMATIC ARRAY BOUNDS ===========================*/

/*
    The bounds of an explicit-shape local (automatic) array are specification
    expressions. They are evaluated on entry to the procedure or BLOCK, and a
    later redefinition of a variable they reference does not change them
    (F2018 8.5.8.2, 10.1.11). The type of the array, and every array type
    that semantics copied from it into the body, would otherwise evaluate the
    bound expression again at each use.

    Each bound whose value can change while the procedure or BLOCK executes
    (see bound_may_change) is therefore captured in a new integer local that
    is initialized with the bound on entry, and the array type, as well as
    the types in the body that describe the shape of the array, refer to that
    local instead:

        real :: tmp(n)              integer :: __lcompilers_tmp_extent_1 = n
        n = 7                  ->   real :: tmp(__lcompilers_tmp_extent_1)
        print *, size(tmp + 1)      ...
*/

// Whether two expressions are the same tree. Unlike ASRUtils::expr_equal it
// does not treat unknown node kinds as equal.
static bool same_expr(ASR::expr_t* a, ASR::expr_t* b) {
    if (a == b) return true;
    if (a == nullptr || b == nullptr || a->type != b->type) return false;
    return LCompilers::pickle(a->base) == LCompilers::pickle(b->base);
}

// Replaces every occurrence of a target in an expression by its replacement.
// An enclosing occurrence is replaced before the ones it contains.
class ReplaceSameExpr : public ASR::BaseExprReplacer<ReplaceSameExpr> {
public:
    std::vector<std::pair<ASR::expr_t*, ASR::expr_t*>> replacements;

    void replace_expr(ASR::expr_t* x) {
        if (x == nullptr) return;
        for (auto &r : replacements) {
            if (same_expr(x, r.first)) {
                *current_expr = r.second;
                return;
            }
        }
        ASR::BaseExprReplacer<ReplaceSameExpr>::replace_expr(x);
    }

    void replace_in(ASR::expr_t* &expr) {
        current_expr = &expr;
        replace_expr(expr);
    }
};

struct CapturedArrayBounds {
    // Bounds as declared, and as captured (a Var of the new local for each
    // captured bound, nullptr for a bound that is left alone).
    ASR::dimension_t* declared;
    ASR::dimension_t* captured;
    size_t n_dims;
};

static bool is_constant_bound(ASR::expr_t* bound) {
    return bound == nullptr || ASRUtils::is_value_constant(bound) ||
        ASRUtils::is_value_constant(ASRUtils::expr_value(bound));
}

// Whether the value of the variable `sym` stays the same while the procedure
// or BLOCK that declares an automatic array executes: a named constant, a
// nonpointer intent(in) dummy argument (of the procedure or of a host), or a
// local that captures a bound.
static bool is_invariant_variable(ASR::symbol_t* sym) {
    sym = ASRUtils::symbol_get_past_external(sym);
    if (!ASR::is_a<ASR::Variable_t>(*sym)) return false;
    ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(sym);
    if (v->m_storage == ASR::storage_typeType::Parameter) return true;
    if (ASRUtils::is_entry_initialized_local(*v)) return true;
    return v->m_intent == ASR::intentType::In && !ASRUtils::is_pointer(v->m_type);
}

static bool bound_may_change(ASR::expr_t* e);

// Whether the shape and the length type parameters of the variable `sym`
// stay the same: those of a nonallocatable, nonpointer variable, and those
// of an intent(in) dummy argument, cannot change.
static bool has_invariant_shape(ASR::symbol_t* sym) {
    sym = ASRUtils::symbol_get_past_external(sym);
    if (!ASR::is_a<ASR::Variable_t>(*sym)) return false;
    ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(sym);
    if (v->m_intent == ASR::intentType::In) return true;
    if (ASRUtils::is_allocatable(v->m_type) || ASRUtils::is_pointer(v->m_type)) {
        return false;
    }
    if (ASRUtils::is_character(*v->m_type)) {
        // The length of a nonallocatable, nonpointer character variable
        // is fixed when the variable is declared or associated.
        return true;
    }
    return ASRUtils::is_array(v->m_type);
}

// The same for the designator `e`: a variable as above, or a nonpointer
// component or an element of an intent(in) dummy argument (whose
// allocatable components cannot be allocated or deallocated either).
static bool has_invariant_shape(ASR::expr_t* e) {
    e = ASRUtils::get_past_array_physical_cast(e);
    if (ASR::is_a<ASR::Var_t>(*e)) {
        return has_invariant_shape(ASR::down_cast<ASR::Var_t>(e)->m_v);
    }
    if (ASR::is_a<ASR::StructInstanceMember_t>(*e) ||
            ASR::is_a<ASR::ArrayItem_t>(*e)) {
        return !ASRUtils::is_pointer(ASRUtils::expr_type(e)) &&
            !bound_may_change(e);
    }
    return false;
}

/*
    Whether the value of a bound expression can change while the procedure
    or BLOCK that declares the automatic array executes. Only such a bound
    is captured: a bound that references only constants, named constants,
    intent(in) dummy arguments and inquiries about the shape of dummy
    arguments evaluates to the same value at each use, and is left as it is.
    Anything else (a variable of the procedure, of a host or of a module, a
    dummy argument that can be defined, a function reference) is captured.
*/
static bool bound_may_change(ASR::expr_t* e) {
    if (e == nullptr || ASRUtils::is_value_constant(e) ||
            ASRUtils::is_value_constant(ASRUtils::expr_value(e))) {
        return false;
    }
    switch (e->type) {
        case ASR::exprType::Var:
            return !is_invariant_variable(ASR::down_cast<ASR::Var_t>(e)->m_v);
        case ASR::exprType::ArraySize: {
            ASR::ArraySize_t* x = ASR::down_cast<ASR::ArraySize_t>(e);
            return !has_invariant_shape(x->m_v) || bound_may_change(x->m_dim);
        }
        case ASR::exprType::ArrayBound: {
            ASR::ArrayBound_t* x = ASR::down_cast<ASR::ArrayBound_t>(e);
            return !has_invariant_shape(x->m_v) || bound_may_change(x->m_dim);
        }
        case ASR::exprType::StringLen: {
            return !has_invariant_shape(ASR::down_cast<ASR::StringLen_t>(e)->m_arg);
        }
        case ASR::exprType::ArrayItem: {
            ASR::ArrayItem_t* x = ASR::down_cast<ASR::ArrayItem_t>(e);
            if (bound_may_change(x->m_v)) return true;
            for (size_t i = 0; i < x->n_args; i++) {
                if (bound_may_change(x->m_args[i].m_left) ||
                        bound_may_change(x->m_args[i].m_right) ||
                        bound_may_change(x->m_args[i].m_step)) {
                    return true;
                }
            }
            return false;
        }
        case ASR::exprType::StructInstanceMember: {
            ASR::StructInstanceMember_t* x = ASR::down_cast<ASR::StructInstanceMember_t>(e);
            return ASRUtils::is_pointer(x->m_type) || bound_may_change(x->m_v);
        }
        case ASR::exprType::IntegerBinOp: {
            ASR::IntegerBinOp_t* x = ASR::down_cast<ASR::IntegerBinOp_t>(e);
            return bound_may_change(x->m_left) || bound_may_change(x->m_right);
        }
        case ASR::exprType::IntegerUnaryMinus:
            return bound_may_change(ASR::down_cast<ASR::IntegerUnaryMinus_t>(e)->m_arg);
        case ASR::exprType::IntegerCompare: {
            ASR::IntegerCompare_t* x = ASR::down_cast<ASR::IntegerCompare_t>(e);
            return bound_may_change(x->m_left) || bound_may_change(x->m_right);
        }
        case ASR::exprType::RealBinOp: {
            ASR::RealBinOp_t* x = ASR::down_cast<ASR::RealBinOp_t>(e);
            return bound_may_change(x->m_left) || bound_may_change(x->m_right);
        }
        case ASR::exprType::RealUnaryMinus:
            return bound_may_change(ASR::down_cast<ASR::RealUnaryMinus_t>(e)->m_arg);
        case ASR::exprType::RealCompare: {
            ASR::RealCompare_t* x = ASR::down_cast<ASR::RealCompare_t>(e);
            return bound_may_change(x->m_left) || bound_may_change(x->m_right);
        }
        case ASR::exprType::LogicalBinOp: {
            ASR::LogicalBinOp_t* x = ASR::down_cast<ASR::LogicalBinOp_t>(e);
            return bound_may_change(x->m_left) || bound_may_change(x->m_right);
        }
        case ASR::exprType::LogicalCompare: {
            ASR::LogicalCompare_t* x = ASR::down_cast<ASR::LogicalCompare_t>(e);
            return bound_may_change(x->m_left) || bound_may_change(x->m_right);
        }
        case ASR::exprType::LogicalNot:
            return bound_may_change(ASR::down_cast<ASR::LogicalNot_t>(e)->m_arg);
        case ASR::exprType::ArrayPhysicalCast:
            return bound_may_change(ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg);
        case ASR::exprType::ArrayBroadcast: {
            ASR::ArrayBroadcast_t* x = ASR::down_cast<ASR::ArrayBroadcast_t>(e);
            return bound_may_change(x->m_array) || bound_may_change(x->m_shape);
        }
        case ASR::exprType::Cast:
            return bound_may_change(ASR::down_cast<ASR::Cast_t>(e)->m_arg);
        case ASR::exprType::IntrinsicElementalFunction: {
            ASR::IntrinsicElementalFunction_t* x =
                ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e);
            for (size_t i = 0; i < x->n_args; i++) {
                if (bound_may_change(x->m_args[i])) return true;
            }
            return false;
        }
        case ASR::exprType::IntrinsicArrayFunction: {
            ASR::IntrinsicArrayFunction_t* x =
                ASR::down_cast<ASR::IntrinsicArrayFunction_t>(e);
            for (size_t i = 0; i < x->n_args; i++) {
                if (bound_may_change(x->m_args[i])) return true;
            }
            return false;
        }
        default:
            return true;
    }
}

static bool is_automatic_array(const ASR::Variable_t &v) {
    if (v.m_intent != ASR::intentType::Local ||
            v.m_storage != ASR::storage_typeType::Default ||
            !ASR::is_a<ASR::Array_t>(*v.m_type)) {
        return false;
    }
    ASR::Array_t* array_t = ASR::down_cast<ASR::Array_t>(v.m_type);
    if (array_t->m_physical_type == ASR::array_physical_typeType::AssumedRankArray) {
        return false;
    }
    for (size_t i = 0; i < array_t->n_dims; i++) {
        if (!is_constant_bound(array_t->m_dims[i].m_start) ||
                !is_constant_bound(array_t->m_dims[i].m_length)) {
            return true;
        }
    }
    return false;
}

/*
    Rewrites the array types in the body that were copied from the type of a
    captured array. An expression takes the shape of a captured array if it
    is a reference to it, or if it is an elemental or shape preserving
    operation on such an expression and has the same rank. Only the bounds
    that are the same tree as the declared bound are replaced, so a bound
    evaluated at that point of the body (e.g. of a function result) is kept.
*/
class CapturedArrayTypeRewriter : public ASR::BaseWalkVisitor<CapturedArrayTypeRewriter> {
private:
    Allocator &al;
    std::map<ASR::symbol_t*, CapturedArrayBounds> &captured;
    // For each expression being visited, the captured array whose shape one
    // of its operands has (nullptr if none).
    std::vector<ASR::symbol_t*> operand_shape;

    // The type of an expression that has the shape of its array operands.
    static ASR::ttype_t** shape_preserving_type(ASR::expr_t* x) {
        switch (x->type) {
            case ASR::exprType::ArrayPhysicalCast:
                return &ASR::down_cast<ASR::ArrayPhysicalCast_t>(x)->m_type;
            case ASR::exprType::ArrayBroadcast:
                return &ASR::down_cast<ASR::ArrayBroadcast_t>(x)->m_type;
            case ASR::exprType::Cast:
                return &ASR::down_cast<ASR::Cast_t>(x)->m_type;
            case ASR::exprType::IntegerBinOp:
                return &ASR::down_cast<ASR::IntegerBinOp_t>(x)->m_type;
            case ASR::exprType::UnsignedIntegerBinOp:
                return &ASR::down_cast<ASR::UnsignedIntegerBinOp_t>(x)->m_type;
            case ASR::exprType::RealBinOp:
                return &ASR::down_cast<ASR::RealBinOp_t>(x)->m_type;
            case ASR::exprType::ComplexBinOp:
                return &ASR::down_cast<ASR::ComplexBinOp_t>(x)->m_type;
            case ASR::exprType::LogicalBinOp:
                return &ASR::down_cast<ASR::LogicalBinOp_t>(x)->m_type;
            case ASR::exprType::IntegerCompare:
                return &ASR::down_cast<ASR::IntegerCompare_t>(x)->m_type;
            case ASR::exprType::UnsignedIntegerCompare:
                return &ASR::down_cast<ASR::UnsignedIntegerCompare_t>(x)->m_type;
            case ASR::exprType::RealCompare:
                return &ASR::down_cast<ASR::RealCompare_t>(x)->m_type;
            case ASR::exprType::ComplexCompare:
                return &ASR::down_cast<ASR::ComplexCompare_t>(x)->m_type;
            case ASR::exprType::LogicalCompare:
                return &ASR::down_cast<ASR::LogicalCompare_t>(x)->m_type;
            case ASR::exprType::StringCompare:
                return &ASR::down_cast<ASR::StringCompare_t>(x)->m_type;
            case ASR::exprType::StringConcat:
                return &ASR::down_cast<ASR::StringConcat_t>(x)->m_type;
            case ASR::exprType::IntegerUnaryMinus:
                return &ASR::down_cast<ASR::IntegerUnaryMinus_t>(x)->m_type;
            case ASR::exprType::RealUnaryMinus:
                return &ASR::down_cast<ASR::RealUnaryMinus_t>(x)->m_type;
            case ASR::exprType::ComplexUnaryMinus:
                return &ASR::down_cast<ASR::ComplexUnaryMinus_t>(x)->m_type;
            case ASR::exprType::IntegerBitNot:
                return &ASR::down_cast<ASR::IntegerBitNot_t>(x)->m_type;
            case ASR::exprType::LogicalNot:
                return &ASR::down_cast<ASR::LogicalNot_t>(x)->m_type;
            case ASR::exprType::RealCopySign:
                return &ASR::down_cast<ASR::RealCopySign_t>(x)->m_type;
            case ASR::exprType::ComplexRe:
                return &ASR::down_cast<ASR::ComplexRe_t>(x)->m_type;
            case ASR::exprType::ComplexIm:
                return &ASR::down_cast<ASR::ComplexIm_t>(x)->m_type;
            case ASR::exprType::StructInstanceMember:
                return &ASR::down_cast<ASR::StructInstanceMember_t>(x)->m_type;
            case ASR::exprType::IntrinsicElementalFunction:
                return &ASR::down_cast<ASR::IntrinsicElementalFunction_t>(x)->m_type;
            case ASR::exprType::IntrinsicArrayFunction: {
                ASR::IntrinsicArrayFunction_t* f = ASR::down_cast<ASR::IntrinsicArrayFunction_t>(x);
                switch (static_cast<ASRUtils::IntrinsicArrayFunctions>(f->m_arr_intrinsic_id)) {
                    case ASRUtils::IntrinsicArrayFunctions::Cshift:
                    case ASRUtils::IntrinsicArrayFunctions::Eoshift:
                        return &f->m_type;
                    default:
                        return nullptr;
                }
            }
            case ASR::exprType::FunctionCall: {
                ASR::FunctionCall_t* call = ASR::down_cast<ASR::FunctionCall_t>(x);
                ASR::symbol_t* fn = ASRUtils::symbol_get_past_external(call->m_name);
                if (ASR::is_a<ASR::Function_t>(*fn) &&
                        ASRUtils::get_FunctionType(ASR::down_cast<ASR::Function_t>(fn))->m_elemental) {
                    return &call->m_type;
                }
                return nullptr;
            }
            default:
                return nullptr;
        }
    }

    // Replaces the declared bounds of `bounds` in `type` by the captured ones.
    ASR::ttype_t* rewrite_type(ASR::ttype_t* type, const CapturedArrayBounds &bounds) {
        if (!ASR::is_a<ASR::Array_t>(*type)) return nullptr;
        ASR::Array_t* array_t = ASR::down_cast<ASR::Array_t>(type);
        if (array_t->n_dims != bounds.n_dims) return nullptr;
        Vec<ASR::dimension_t> dims;
        dims.reserve(al, array_t->n_dims);
        bool changed = false;
        for (size_t i = 0; i < array_t->n_dims; i++) {
            ASR::dimension_t dim = array_t->m_dims[i];
            if (bounds.captured[i].m_start && dim.m_start &&
                    same_expr(dim.m_start, bounds.declared[i].m_start)) {
                dim.m_start = bounds.captured[i].m_start;
                changed = true;
            }
            if (bounds.captured[i].m_length && dim.m_length &&
                    same_expr(dim.m_length, bounds.declared[i].m_length)) {
                dim.m_length = bounds.captured[i].m_length;
                changed = true;
            }
            dims.push_back(al, dim);
        }
        if (!changed) return type;
        return ASRUtils::duplicate_type(al, type, &dims, array_t->m_physical_type, true);
    }

    void replace_declared_bounds(ASR::expr_t* &value, const CapturedArrayBounds &bounds) {
        if (value == nullptr || ASRUtils::is_value_constant(value)) return;
        // The value may share nodes with the declared type.
        ASRUtils::ExprStmtDuplicator duplicator(al);
        value = duplicator.duplicate_expr(value);
        ReplaceSameExpr replace;
        for (size_t i = 0; i < bounds.n_dims; i++) {
            if (bounds.captured[i].m_length) {
                replace.replacements.push_back({bounds.declared[i].m_length,
                    bounds.captured[i].m_length});
            }
        }
        for (size_t i = 0; i < bounds.n_dims; i++) {
            if (bounds.captured[i].m_start) {
                replace.replacements.push_back({bounds.declared[i].m_start,
                    bounds.captured[i].m_start});
            }
        }
        replace.replace_in(value);
    }

    ASR::symbol_t* shape_of(ASR::expr_t* x, ASR::symbol_t* operand) {
        if (ASR::is_a<ASR::Var_t>(*x)) {
            ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(x)->m_v;
            return captured.find(sym) != captured.end() ? sym : nullptr;
        }
        if (operand == nullptr) return nullptr;
        // The value of an inquiry about the shape of the array may be given
        // in terms of the declared bounds.
        if (ASR::is_a<ASR::ArraySize_t>(*x)) {
            replace_declared_bounds(ASR::down_cast<ASR::ArraySize_t>(x)->m_value, captured[operand]);
            return nullptr;
        }
        if (ASR::is_a<ASR::ArrayBound_t>(*x)) {
            replace_declared_bounds(ASR::down_cast<ASR::ArrayBound_t>(x)->m_value, captured[operand]);
            return nullptr;
        }
        if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*x) &&
                ASR::down_cast<ASR::IntrinsicArrayFunction_t>(x)->m_arr_intrinsic_id ==
                    static_cast<int64_t>(ASRUtils::IntrinsicArrayFunctions::Shape)) {
            replace_declared_bounds(ASR::down_cast<ASR::IntrinsicArrayFunction_t>(x)->m_value,
                captured[operand]);
            // `shape(tmp)`, as used by the broadcast in `tmp = 1`.
            return operand;
        }
        ASR::ttype_t** type = shape_preserving_type(x);
        if (type == nullptr) return nullptr;
        ASR::ttype_t* new_type = rewrite_type(*type, captured[operand]);
        if (new_type == nullptr) return nullptr;
        *type = new_type;
        return operand;
    }

public:
    CapturedArrayTypeRewriter(Allocator &al_,
        std::map<ASR::symbol_t*, CapturedArrayBounds> &captured_)
        : al(al_), captured(captured_) {}

    void visit_expr(const ASR::expr_t &x) {
        operand_shape.push_back(nullptr);
        ASR::BaseWalkVisitor<CapturedArrayTypeRewriter>::visit_expr(x);
        ASR::symbol_t* operand = operand_shape.back();
        operand_shape.pop_back();
        ASR::symbol_t* shape = shape_of(const_cast<ASR::expr_t*>(&x), operand);
        if (shape && !operand_shape.empty() && operand_shape.back() == nullptr) {
            operand_shape.back() = shape;
        }
    }

    void visit_ttype(const ASR::ttype_t &x) {
        // Expressions in a type (e.g. bounds) are not operands of the
        // expression that has the type.
        operand_shape.push_back(nullptr);
        ASR::BaseWalkVisitor<CapturedArrayTypeRewriter>::visit_ttype(x);
        operand_shape.pop_back();
    }

    // The captured array whose shape the expression `x` has (nullptr if none).
    ASR::symbol_t* visit_operand(ASR::expr_t* x) {
        operand_shape.push_back(nullptr);
        visit_expr(*x);
        ASR::symbol_t* shape = operand_shape.back();
        operand_shape.pop_back();
        return shape;
    }

    /*
        An associate name takes the shape of its selector. Its type was
        copied from the type of the selector, so it gets the captured bounds
        as well, and it is itself treated as a captured array from then on:
        a selector of a nested ASSOCIATE, or an expression in the body, may
        have its shape.
    */
    void capture_associate_name(ASR::expr_t* target, ASR::symbol_t* shape) {
        if (shape == nullptr || !ASR::is_a<ASR::Var_t>(*target)) return;
        ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(target)->m_v;
        if (!ASR::is_a<ASR::Variable_t>(*sym)) return;
        ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(sym);
        ASR::ttype_t* new_type = rewrite_type(v->m_type, captured[shape]);
        if (new_type) v->m_type = new_type;
        if (captured.find(sym) == captured.end()) {
            captured[sym] = captured[shape];
            added_associate_names = true;
        }
    }

    // Whether `target` is the associate name of an ASSOCIATE whose selector
    // is an expression: semantics declares a nonpointer variable in the
    // AssociateBlock and assigns the value of the selector to it.
    static bool is_expression_associate_name(ASR::expr_t* target) {
        if (!ASR::is_a<ASR::Var_t>(*target)) return false;
        ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(target)->m_v;
        if (!ASR::is_a<ASR::Variable_t>(*sym)) return false;
        ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(sym);
        return v->m_parent_symtab->asr_owner &&
            ASR::is_a<ASR::symbol_t>(*v->m_parent_symtab->asr_owner) &&
            ASR::is_a<ASR::AssociateBlock_t>(*ASR::down_cast<ASR::symbol_t>(
                v->m_parent_symtab->asr_owner)) &&
            !ASRUtils::is_pointer(v->m_type) &&
            !ASRUtils::is_allocatable(v->m_type);
    }

public:
    // Whether an associate name became a captured array during the last
    // walk (see capture_associate_name); another walk then updates the
    // expressions that have its shape.
    bool added_associate_names = false;

    void visit_Associate(const ASR::Associate_t &x) {
        ASR::symbol_t* shape = visit_operand(x.m_value);
        visit_expr(*x.m_target);
        capture_associate_name(x.m_target, shape);
    }

    void visit_Assignment(const ASR::Assignment_t &x) {
        if (!is_expression_associate_name(x.m_target)) {
            ASR::BaseWalkVisitor<CapturedArrayTypeRewriter>::visit_Assignment(x);
            return;
        }
        ASR::symbol_t* shape = visit_operand(x.m_value);
        visit_expr(*x.m_target);
        capture_associate_name(x.m_target, shape);
        if (x.m_overloaded) visit_stmt(*x.m_overloaded);
    }
};

/*
    Under --gpu=..., the offload passes turn the BLOCKs of an offloaded loop
    (DO CONCURRENT, or an OpenMP region), and the procedures such a loop
    references directly or through other procedures, into kernel code, and
    size the per-thread storage of their automatic arrays from the declared
    bound expressions. They do not handle a bound captured in a local
    initialized on entry, so the scopes they may take are not captured: those
    BLOCKs, the device procedures, the procedures reachable from them or from
    an offloaded loop, and the BLOCKs of those procedures. Every other scope
    (a PURE or ELEMENTAL procedure that only host code references included)
    is captured as without --gpu.
*/
class OffloadableScopeCollector : public ASR::BaseWalkVisitor<OffloadableScopeCollector> {
private:
    std::set<SymbolTable*> &scopes;
    // Whether the statements being visited may be device code.
    bool offloadable = false;
    std::vector<ASR::Function_t*> reached;
    std::set<ASR::Function_t*> reached_set;

    void reach(ASR::symbol_t* sym) {
        if (sym == nullptr) return;
        sym = ASRUtils::symbol_get_past_external(sym);
        if (ASR::is_a<ASR::StructMethodDeclaration_t>(*sym)) {
            sym = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::StructMethodDeclaration_t>(sym)->m_proc);
        }
        if (!ASR::is_a<ASR::Function_t>(*sym)) return;
        ASR::Function_t* f = ASR::down_cast<ASR::Function_t>(sym);
        if (reached_set.insert(f).second) {
            reached.push_back(f);
        }
    }

    template <typename T>
    void visit_offloadable(const T &x) {
        bool offloadable_copy = offloadable;
        offloadable = true;
        for (size_t i = 0; i < x.n_body; i++) {
            visit_stmt(*x.m_body[i]);
        }
        offloadable = offloadable_copy;
    }

public:
    OffloadableScopeCollector(std::set<SymbolTable*> &scopes_) : scopes(scopes_) {}

    void collect(ASR::TranslationUnit_t &unit) {
        visit_TranslationUnit(unit);
        offloadable = true;
        for (size_t i = 0; i < reached.size(); i++) {
            ASR::Function_t* f = reached[i];
            scopes.insert(f->m_symtab);
            for (size_t j = 0; j < f->n_body; j++) {
                visit_stmt(*f->m_body[j]);
            }
        }
    }

    void visit_Function(const ASR::Function_t &x) {
        if (ASRUtils::get_FunctionType(x)->m_exec_space != ASR::exec_spaceType::Host) {
            reach(const_cast<ASR::symbol_t*>(&x.base));
        }
        bool offloadable_copy = offloadable;
        offloadable = false;
        ASR::BaseWalkVisitor<OffloadableScopeCollector>::visit_Function(x);
        offloadable = offloadable_copy;
    }

    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        visit_offloadable(x);
    }

    void visit_OMPRegion(const ASR::OMPRegion_t &x) {
        visit_offloadable(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        if (offloadable) reach(x.m_name);
        ASR::BaseWalkVisitor<OffloadableScopeCollector>::visit_FunctionCall(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        if (offloadable) reach(x.m_name);
        ASR::BaseWalkVisitor<OffloadableScopeCollector>::visit_SubroutineCall(x);
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        if (!offloadable) return;
        ASR::Block_t* block = ASR::down_cast<ASR::Block_t>(x.m_m);
        scopes.insert(block->m_symtab);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        if (!offloadable) return;
        ASR::AssociateBlock_t* block = ASR::down_cast<ASR::AssociateBlock_t>(x.m_m);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
};

/* ================================== VISITOR ==================================*/

class FunctionTypeVisitor : public ASR::CallReplacerOnExpressionsVisitor<FunctionTypeVisitor>
{
private:

    // Check if expression contains any sub-expression that returns a non-scalar (array, struct,character)
    class expr_contains_functionCall_with_Nonscalar_return 
    : public ASR::BaseWalkVisitor<expr_contains_functionCall_with_Nonscalar_return>{
    private :
        expr_contains_functionCall_with_Nonscalar_return() = default;
        bool found = false; // If any sub-expression is of non-scalar return
        bool is_non_scalar(ASR::ttype_t* type){
            type = ASRUtils::type_get_past_allocatable_pointer(type);
            return ASRUtils::is_character(*type) || 
            ASRUtils::is_array(type)      ||
            ASRUtils::is_struct(*type);
        }
        // Whether only references to user procedures count.
        bool function_calls_only = false;
        bool is_call_to_function(const ASR::expr_t* expr){
            if (function_calls_only) {
                return ASR::is_a<ASR::FunctionCall_t>(*expr);
            }
            return ASR::is_a<ASR::FunctionCall_t>(*expr) || 
            ASR::is_a<ASR::IntrinsicArrayFunction_t>(*expr) ||
            ASR::is_a<ASR::IntrinsicElementalFunction_t>(*expr);
        }
    public :
        static bool check(const ASR::expr_t* expr, bool function_calls_only = false){
            LCOMPILERS_ASSERT(expr)
            expr_contains_functionCall_with_Nonscalar_return instance {};
            instance.function_calls_only = function_calls_only;
            instance.visit_expr(*expr);
            return instance.found;
        }

        void visit_expr(const ASR::expr_t &b){
            if(
                is_call_to_function(&b) &&
                is_non_scalar(ASRUtils::expr_type(&b))
            ){
                found = true;
                return;
            }
            ASR::BaseWalkVisitor<expr_contains_functionCall_with_Nonscalar_return>::visit_expr(b);
        }
    };

public:

    Allocator &al;
    ReplaceFunctionCall replacer;
    SymbolTable* current_scope;
    Vec<ASR::stmt_t*> pass_result;
    ASR::TranslationUnit_t &tt;
    std::map<ASR::symbol_t*, CapturedArrayBounds> captured;
    // The scopes whose automatic arrays are not captured (see
    // OffloadableScopeCollector).
    std::set<SymbolTable*> uncaptured_scopes;



    FunctionTypeVisitor(Allocator &al_, ASR::TranslationUnit_t &tt) : al(al_), replacer(al_, tt, current_scope), tt(tt) {
        current_scope = nullptr;
        pass_result.reserve(al, 1);
    }

    void call_replacer_(ASR::expr_t* value) {
        replacer.current_expr = current_expr;
        replacer.assignment_value = value;
        replacer.replace_expr(*current_expr);
        replacer.assignment_value = nullptr;
    }


    /*
        A specification expression of a local array may call a function
        returning a non-scalar, e.g. `real :: tmp(comp(construct(3)))` with
        `construct` returning a derived type. Later passes would evaluate
        such a call as a statement at the start of the body, after the
        array has already been sized. Evaluate the whole bound in a helper
        function instead, so that the bound is self-contained and the
        function result is finalized before the body executes.
    */
    void replace_nonscalar_calls_in_array_bounds(const ASR::Variable_t &x) {
        if (x.m_intent != ASR::intentType::Local) return;
        ASR::ttype_t* type = ASRUtils::type_get_past_allocatable_pointer(x.m_type);
        if (!ASR::is_a<ASR::Array_t>(*type)) return;
        ASR::Array_t* array_t = ASR::down_cast<ASR::Array_t>(type);
        for (size_t i = 0; i < array_t->n_dims; i++) {
            ASR::expr_t** bounds[2] = {&array_t->m_dims[i].m_start, &array_t->m_dims[i].m_length};
            for (ASR::expr_t** bound : bounds) {
                // Intrinsic array functions in a bound are handled by
                // later passes as before.
                if (*bound && expr_contains_functionCall_with_Nonscalar_return::check(*bound, true)) {
                    replacer.current_expr = bound;
                    replacer.assignment_value = *bound;
                    replacer.replace_with_helper_function_call(*bound);
                    replacer.assignment_value = nullptr;
                }
            }
        }
    }

    // Same for the initializer of a local that captures such a bound on entry.
    void replace_nonscalar_calls_in_entry_initializer(const ASR::Variable_t &x) {
        if (!ASRUtils::is_entry_initialized_local(x)) return;
        if (expr_contains_functionCall_with_Nonscalar_return::check(x.m_symbolic_value)) {
            ASR::expr_t** initializer = const_cast<ASR::expr_t**>(&x.m_symbolic_value);
            replacer.current_expr = initializer;
            replacer.assignment_value = *initializer;
            replacer.replace_with_helper_function_call(*initializer);
            replacer.assignment_value = nullptr;
        }
    }

    ASR::expr_t* create_bound_local(SymbolTable* scope, const ASR::Variable_t &array,
            const char* what, size_t dim, ASR::expr_t* bound) {
        std::string name = scope->get_unique_name("__lcompilers_" +
            std::string(array.m_name) + "_" + what + "_" + std::to_string(dim + 1), false);
        ASR::ttype_t* type = ASRUtils::duplicate_type(al,
            ASRUtils::type_get_past_allocatable_pointer(ASRUtils::expr_type(bound)));
        SetChar dependencies; dependencies.reserve(al, 1);
        ASRUtils::collect_variable_dependencies(al, dependencies, type, bound);
        ASR::symbol_t* sym = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Variable_t_util(
            al, bound->base.loc, scope, s2c(al, name), dependencies.p, dependencies.size(),
            ASR::intentType::Local, bound, nullptr, ASR::storage_typeType::Default, type,
            nullptr, array.m_abi, ASR::accessType::Public, ASR::presenceType::Required, false));
        scope->add_symbol(name, sym);
        return ASRUtils::EXPR(ASR::make_Var_t(al, bound->base.loc, sym));
    }

    // Captures the non-constant bounds of the automatic arrays declared in
    // `scope` in locals initialized on entry (see CapturedArrayTypeRewriter).
    bool capture_automatic_array_bounds(SymbolTable* scope) {
        bool any_captured = false;
        if (uncaptured_scopes.count(scope) > 0) return false;
        ASRUtils::ExprStmtDuplicator duplicator(al);
        for (auto &name : ASRUtils::determine_variable_declaration_order(scope)) {
            ASR::symbol_t* sym = scope->get_symbol(name);
            if (!ASR::is_a<ASR::Variable_t>(*sym)) continue;
            ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(sym);
            if (!is_automatic_array(*v)) continue;
            ASR::Array_t* array_t = ASR::down_cast<ASR::Array_t>(v->m_type);
            Vec<ASR::dimension_t> captured_dims; captured_dims.reserve(al, array_t->n_dims);
            Vec<ASR::dimension_t> new_dims; new_dims.reserve(al, array_t->n_dims);
            bool any_dim_captured = false;
            for (size_t i = 0; i < array_t->n_dims; i++) {
                ASR::dimension_t dim = array_t->m_dims[i];
                ASR::dimension_t captured_dim; captured_dim.loc = dim.loc;
                captured_dim.m_start = nullptr; captured_dim.m_length = nullptr;
                ASR::dimension_t new_dim = dim;
                if (bound_may_change(dim.m_start)) {
                    captured_dim.m_start = create_bound_local(scope, *v, "lbound", i,
                        duplicator.duplicate_expr(dim.m_start));
                    new_dim.m_start = captured_dim.m_start;
                }
                if (bound_may_change(dim.m_length)) {
                    ASR::expr_t* length = duplicator.duplicate_expr(dim.m_length);
                    if (captured_dim.m_start) {
                        // The extent is `ubound - lbound + 1`: evaluate the
                        // lower bound only once.
                        ReplaceSameExpr replace_start;
                        replace_start.replacements.push_back({dim.m_start, captured_dim.m_start});
                        replace_start.replace_in(length);
                    }
                    captured_dim.m_length = create_bound_local(scope, *v, "extent", i, length);
                    new_dim.m_length = captured_dim.m_length;
                }
                captured_dims.push_back(al, captured_dim);
                new_dims.push_back(al, new_dim);
                any_dim_captured = any_dim_captured ||
                    captured_dim.m_start || captured_dim.m_length;
            }
            if (!any_dim_captured) continue;
            v->m_type = ASRUtils::duplicate_type(al, v->m_type, &new_dims,
                array_t->m_physical_type, true);
            SetChar dependencies; dependencies.reserve(al, 1);
            ASRUtils::collect_variable_dependencies(al, dependencies, v->m_type,
                v->m_symbolic_value, v->m_value, v->m_name);
            v->m_dependencies = dependencies.p;
            v->n_dependencies = dependencies.size();
            captured[sym] = {ASRUtils::duplicate_dimensions(al, array_t->m_dims, array_t->n_dims),
                captured_dims.p, array_t->n_dims};
            any_captured = true;
        }
        return any_captured;
    }

    void visit_Variable(const ASR::Variable_t &x){
        replace_nonscalar_calls_in_array_bounds(x);
        if (ASRUtils::is_entry_initialized_local(x)) {
            // Treat the initializer like the array bound it was taken from.
            replace_nonscalar_calls_in_entry_initializer(x);
            ASR::expr_t** current_expr_copy = current_expr;
            current_expr = const_cast<ASR::expr_t**>(&x.m_symbolic_value);
            if (is_function_call_or_intrinsic_array_function(x.m_symbolic_value)) {
                this->call_replacer_(x.m_symbolic_value);
            }
            this->visit_expr(*x.m_symbolic_value);
            current_expr = current_expr_copy;
        }
        visit_ttype(*x.m_type);
    }

/* --------------------> CONSTRUCTS VISITORS <--------------------*/

/*  Construct Visitors Mustn't Visit The Body  */

    void visit_Function(const ASR::Function_t &x) { // NO Body Visiting
        SymbolTable* current_scope_copy = current_scope;
        current_scope = x.m_symtab;
        if (ASRUtils::get_FunctionType(x)->m_deftype == ASR::deftypeType::Implementation &&
                capture_automatic_array_bounds(x.m_symtab)) {
            CapturedArrayTypeRewriter rewriter(al, captured);
            do {
                rewriter.added_associate_names = false;
                rewriter.visit_Function(x);
            } while (rewriter.added_associate_names);
        }
        this->visit_ttype(*x.m_function_signature); // Visit signature first to handle returnVar
        for( auto sym: x.m_symtab->get_scope() ) {
            visit_symbol(*sym.second);
        }
        current_scope = current_scope_copy;
    }
    
    template<typename T>
    void visit_construct(const T &x){ // NO Body Visiting
        SymbolTable* current_scope_copy = current_scope;
        current_scope = x.m_symtab;
        for( auto sym: x.m_symtab->get_scope() ) {
            visit_symbol(*sym.second);
        }
        current_scope = current_scope_copy;
    }
    
    void visit_Block(const ASR::Block_t &x) {
        if (capture_automatic_array_bounds(x.m_symtab)) {
            CapturedArrayTypeRewriter rewriter(al, captured);
            do {
                rewriter.added_associate_names = false;
                rewriter.visit_Block(x);
            } while (rewriter.added_associate_names);
        }
        visit_construct(x);
    }
    void visit_Module(const ASR::Module_t &x)                   { visit_construct(x); }
    void visit_Program(const ASR::Program_t &x)                 { visit_construct(x); }
    void visit_AssociateBlock(const ASR::AssociateBlock_t &x)   { visit_construct(x); }

/* <---------------------------------------->*/

    bool is_function_call_or_intrinsic_array_function(ASR::expr_t* expr) {
        if (!expr) return false;
        if (is_a<ASR::FunctionCall_t>(*expr)) {
            return true;
        } else if (is_a<ASR::IntrinsicArrayFunction_t>(*expr)) {
            return true;
        }
        return false;
    }

    void set_type_of_result_var(const ASR::FunctionType_t &x, ASR::Function_t* func) {
        if (func->m_return_var == nullptr) {
            // A function that only declares a procedure type, such as the
            // interface of an opaque procedure, has no result variable.
            return;
        }
        if( ASR::is_a<ASR::Array_t>(*x.m_return_var_type) ) {
            ASR::ttype_t* return_type_copy = ASRUtils::duplicate_type(al, x.m_return_var_type);
            ASR::Array_t* array_t = ASR::down_cast<ASR::Array_t>(return_type_copy);
            Vec<ASR::expr_t*> new_args; new_args.reserve(al, func->n_args);
            for (size_t j = 0; j < func->n_args; j++) {
                new_args.push_back(al, func->m_args[j]);
            }
            for( size_t i = 0; i < array_t->n_dims; i++ ) {
                replacer.replace_FunctionParam_with_FunctionArgs(array_t->m_dims[i].m_start, new_args);
                replacer.replace_FunctionParam_with_FunctionArgs(array_t->m_dims[i].m_length, new_args);
            }
            if (ASR::is_a<ASR::String_t>(*array_t->m_type) &&
                    ASR::down_cast<ASR::String_t>(array_t->m_type)->m_len) {
                replacer.replace_FunctionParam_with_FunctionArgs(
                    ASR::down_cast<ASR::String_t>(array_t->m_type)->m_len, new_args);
            }
            ASRUtils::EXPR2VAR(func->m_return_var)->m_type = return_type_copy;
        } else if (ASR::is_a<ASR::String_t>(*x.m_return_var_type)){
            ASR::ttype_t* return_type_copy = ASRUtils::duplicate_type(al, x.m_return_var_type);
            ASR::String_t* str_type = ASR::down_cast<ASR::String_t>(return_type_copy);
            Vec<ASR::expr_t*> new_args; new_args.reserve(al, func->n_args);
            for (size_t i = 0; i < func->n_args; i++) {new_args.push_back(al, func->m_args[i]);}
            if(str_type->m_len){
                replacer.replace_FunctionParam_with_FunctionArgs(str_type->m_len, new_args);
            }
            ASRUtils::EXPR2VAR(func->m_return_var)->m_type = return_type_copy;
        } else {
            LCompilersException("Type : " +ASRUtils::type_to_str_fortran_expr(x.m_return_var_type, func->m_return_var) + " isn't a supproted case\n");
        }
    }

    void visit_Array(const ASR::Array_t &x) {
        if (x.m_physical_type == ASR::array_physical_typeType::AssumedRankArray) return;
        if (is_function_call_or_intrinsic_array_function(x.m_dims->m_length)) {
            ASR::expr_t** current_expr_copy = current_expr;
            current_expr = const_cast<ASR::expr_t**>(&(x.m_dims->m_length));
            this->call_replacer_(x.m_dims->m_length);
            current_expr = current_expr_copy;
        }

        ASR::CallReplacerOnExpressionsVisitor<FunctionTypeVisitor>::visit_Array(x);
    }

    void visit_String(const ASR::String_t &x){
        if (x.m_len && expr_contains_functionCall_with_Nonscalar_return::check(x.m_len)) {
            ASR::expr_t** current_expr_copy = current_expr;
            current_expr = const_cast<ASR::expr_t**>(&(x.m_len));
            { // Same as `this->call_replacer_()`. We did in here to workaround. In general this needs a refactor.
                replacer.current_expr = current_expr;
                replacer.assignment_value = x.m_len;
                replacer.replace_with_helper_function_call(*current_expr);
            }
            current_expr = current_expr_copy;
        }
    }

    void visit_IntegerBinOp(const ASR::IntegerBinOp_t &x) {
        if (is_function_call_or_intrinsic_array_function(x.m_left)) {
            ASR::expr_t** current_expr_copy = current_expr;
            current_expr = const_cast<ASR::expr_t**>(&(x.m_left));
            this->call_replacer_(x.m_left);
            current_expr = current_expr_copy;
        }

        if (is_function_call_or_intrinsic_array_function(x.m_right)) {
            ASR::expr_t** current_expr_copy = current_expr;
            current_expr = const_cast<ASR::expr_t**>(&(x.m_right));
            this->call_replacer_(x.m_right);
            current_expr = current_expr_copy;
        }

        ASR::CallReplacerOnExpressionsVisitor<FunctionTypeVisitor>::visit_IntegerBinOp(x);
    }

    void visit_IntrinsicElementalFunction(const ASR::IntrinsicElementalFunction_t &x) {
        for (size_t i = 0; i < x.n_args; i++) {
            ASR::expr_t* arg = x.m_args[i];
            if (is_function_call_or_intrinsic_array_function(arg)) {
                ASR::expr_t** current_expr_copy = current_expr;
                current_expr = const_cast<ASR::expr_t**>(&(x.m_args[i]));
                this->call_replacer_(x.m_args[i]);
                current_expr = current_expr_copy;
            }
        }

        ASR::CallReplacerOnExpressionsVisitor<FunctionTypeVisitor>::visit_IntrinsicElementalFunction(x);
    }

    void visit_RealBinOp(const ASR::RealBinOp_t &x) {
        if (is_function_call_or_intrinsic_array_function(x.m_left)) {
            ASR::expr_t** current_expr_copy = current_expr;
            current_expr = const_cast<ASR::expr_t**>(&(x.m_left));
            this->call_replacer_(x.m_left);
            current_expr = current_expr_copy;
        }

        if (is_function_call_or_intrinsic_array_function(x.m_right)) {
            ASR::expr_t** current_expr_copy = current_expr;
            current_expr = const_cast<ASR::expr_t**>(&(x.m_right));
            this->call_replacer_(x.m_right);
            current_expr = current_expr_copy;
        }

        ASR::CallReplacerOnExpressionsVisitor<FunctionTypeVisitor>::visit_RealBinOp(x);
    }

    void visit_Cast(const ASR::Cast_t &x) {

        if (is_function_call_or_intrinsic_array_function(x.m_arg)) {
            ASR::expr_t** current_expr_copy = current_expr;
            current_expr = const_cast<ASR::expr_t**>(&(x.m_arg));
            this->call_replacer_(x.m_arg);
            current_expr = current_expr_copy;
        }

        ASR::CallReplacerOnExpressionsVisitor<FunctionTypeVisitor>::visit_Cast(x);
    }

    void visit_FunctionType(const ASR::FunctionType_t &x) {
        ASR::Function_t* func = nullptr;
        ASR::asr_t* asr_owner = current_scope->asr_owner;
        if (ASR::is_a<ASR::symbol_t>(*asr_owner)) {
            ASR::symbol_t* sym = ASR::down_cast<ASR::symbol_t>(asr_owner);
            if (ASR::is_a<ASR::Function_t>(*sym)) {
                func = ASR::down_cast2<ASR::Function_t>(current_scope->asr_owner);
                const ASR::ttype_t* current_type = (const ASR::ttype_t*)&x;
                if (func->m_function_signature != current_type) {
                    return;
                }
            } else {
                return;
            }
        } else {
            return;
        }

        ASR::ttype_t* return_var_type = x.m_return_var_type;

        if (return_var_type && ASRUtils::is_array(return_var_type)) {
            ASR::Array_t* arr = ASR::down_cast<ASR::Array_t>(ASRUtils::type_get_past_allocatable(ASRUtils::type_get_past_pointer(return_var_type)));
            for (size_t i = 0; i < arr->n_dims; i++) {
                ASR::dimension_t dim = arr->m_dims[i];
                ASR::expr_t* start = dim.m_start;
                ASR::expr_t* end = dim.m_length;
                if (start && is_a<ASR::IntegerBinOp_t>(*start)) {
                    ASR::IntegerBinOp_t* binop = ASR::down_cast<ASR::IntegerBinOp_t>(start);
                    if (is_function_call_or_intrinsic_array_function(binop->m_left)) {
                        ASR::expr_t** current_expr_copy = current_expr;
                        current_expr = const_cast<ASR::expr_t**>(&(binop->m_left));
                        this->call_replacer_(binop->m_left);
                        current_expr = current_expr_copy;
                    }
                    if (is_function_call_or_intrinsic_array_function(binop->m_right)) {
                        ASR::expr_t** current_expr_copy = current_expr;
                        current_expr = const_cast<ASR::expr_t**>(&(binop->m_right));
                        this->call_replacer_(binop->m_right);
                        current_expr = current_expr_copy;
                    }

                }
                if (end && is_a<ASR::IntegerBinOp_t>(*end)) {
                    ASR::IntegerBinOp_t* binop = ASR::down_cast<ASR::IntegerBinOp_t>(end);
                    if (is_function_call_or_intrinsic_array_function(binop->m_left)) {
                        ASR::expr_t** current_expr_copy = current_expr;
                        current_expr = const_cast<ASR::expr_t**>(&(binop->m_left));
                        this->call_replacer_(binop->m_left);
                        current_expr = current_expr_copy;
                    }
                    if (is_function_call_or_intrinsic_array_function(binop->m_right)) {
                        ASR::expr_t** current_expr_copy = current_expr;
                        current_expr = const_cast<ASR::expr_t**>(&(binop->m_right));
                        this->call_replacer_(binop->m_right);
                        current_expr = current_expr_copy;
                    }

                }
                if (is_function_call_or_intrinsic_array_function(start)) {
                    ASR::expr_t** current_expr_copy = current_expr;
                    current_expr = const_cast<ASR::expr_t**>(&(ASR::down_cast<ASR::Array_t>(x.m_return_var_type)->m_dims[i].m_start));
                    this->call_replacer_(start);
                    current_expr = current_expr_copy;
                }
                if (is_function_call_or_intrinsic_array_function(end)) {
                    ASR::expr_t** current_expr_copy = current_expr;
                    current_expr = const_cast<ASR::expr_t**>(&(ASR::down_cast<ASR::Array_t>(x.m_return_var_type)->m_dims[i].m_length));
                    this->call_replacer_(end);
                    current_expr = current_expr_copy;
                }
            } 

            set_type_of_result_var(x, func); // TODO : Make sure to call this only when the returnVar already changed (This does unnecessary replacement for all array return nodes + string)
        } else if (return_var_type && ASRUtils::is_character(*return_var_type)) {
            visit_String(*ASR::down_cast<ASR::String_t>(ASRUtils::extract_type(return_var_type)));
            set_type_of_result_var(x, func);
        }
    }

};

void pass_replace_function_call_in_declaration(Allocator &al, ASR::TranslationUnit_t &unit,
                        const LCompilers::PassOptions& pass_options) {
    FunctionTypeVisitor v(al, unit);
    if (gpu_device_capabilities(pass_options).device_selected()) {
        OffloadableScopeCollector collector(v.uncaptured_scopes);
        collector.collect(unit);
    }
    v.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor x(al);
    x.visit_TranslationUnit(unit);
}


} // namespace LCompilers
