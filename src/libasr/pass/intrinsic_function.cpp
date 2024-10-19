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
#include <utility>


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
            return ;
        }

        Vec<ASR::call_arg_t> new_args; new_args.reserve(al, x->n_args);
        // Replace any IntrinsicElementalFunctions in the argument first:
        for( size_t i = 0; i < x->n_args; i++ ) {
            ASR::call_arg_t arg0;
            arg0.loc = (*current_expr)->base.loc;
            arg0.m_value = x->m_args[i]; // Use the converted arg
            new_args.push_back(al, arg0);
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
        ASR::expr_t* current_expr_ = instantiate_function(al, x->base.base.loc,
            global_scope, arg_types, ASRUtils::extract_type(x->m_type), new_args, x->m_overload_id);
        *current_expr = current_expr_;
    }

    void replace_IntrinsicArrayFunction(ASR::IntrinsicArrayFunction_t* x) {
        if (x->m_value) {
            *current_expr = x->m_value;
            return ;
        }

        replace_ttype(x->m_type);
        Vec<ASR::call_arg_t> new_args; new_args.reserve(al, x->n_args);
        // Replace any IntrinsicArrayFunctions in the argument first:
        for( size_t i = 0; i < x->n_args; i++ ) {
            ASR::call_arg_t arg0;
            arg0.loc = (*current_expr)->base.loc;
            arg0.m_value = x->m_args[i]; // Use the converted arg
            new_args.push_back(al, arg0);
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
        if (ASR::is_a<ASR::FunctionCall_t>(*func_call) && ASRUtils::is_array(x->m_type)
        ) {
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
    std::map<ASR::symbol_t*, ASRUtils::IntrinsicArrayFunctions>& func2intrinsicid;

    public:

    ASR::expr_t* result_var_;
    SymbolTable* current_scope;

    ReplaceFunctionCallReturningArray(Allocator& al_, Vec<ASR::stmt_t*>& pass_result_,
    std::map<ASR::symbol_t*, ASRUtils::IntrinsicArrayFunctions>& func2intrinsicid_) :
    al(al_), pass_result(pass_result_), func2intrinsicid(func2intrinsicid_),
    result_var_(nullptr), current_scope(nullptr) {}

    void replace_FunctionCall(ASR::FunctionCall_t* x) {
        ASR::symbol_t* x_m_name = ASRUtils::symbol_get_past_external(x->m_name);
        if( func2intrinsicid.find(x_m_name) == func2intrinsicid.end() ) {
            return ;
        }

        Vec<ASR::call_arg_t> new_args;
        new_args.reserve(al, x->n_args + 1);
        for( size_t i = 0; i < x->n_args; i++ ) {
            ASR::call_arg_t new_arg;
            new_arg.loc = x->m_args[i].loc;
            new_arg.m_value = x->m_args[i].m_value;
            new_args.push_back(al, new_arg);
        }

        ASR::call_arg_t new_arg;
        LCOMPILERS_ASSERT(result_var_)
        new_arg.loc = result_var_->base.loc;
        new_arg.m_value = result_var_;
        new_args.push_back(al, new_arg);
        ASR::stmt_t* subrout_call = ASRUtils::STMT(ASRUtils::make_SubroutineCall_t_util(
            al, x->base.base.loc, x->m_name, x->m_original_name, new_args.p,
            new_args.size(), x->m_dt, nullptr, false, false));
        pass_result.push_back(al, subrout_call);
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
        al(al_), replacer(al_, pass_result, func2intrinsicid_), parent_body(nullptr) {
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
                if( pass_result.size() > 0 ) {
                    for (size_t j=0; j < pass_result.size(); j++) {
                        body.push_back(al, pass_result[j]);
                    }
                } else {
                    body.push_back(al, m_body[i]);
                }
            }
            m_body = body.p;
            n_body = body.size();
            pass_result.n = 0;
        }

        void visit_Assignment(const ASR::Assignment_t& x) {
            replacer.result_var_ = x.m_target;
            ASR::CallReplacerOnExpressionsVisitor<ReplaceFunctionCallReturningArrayVisitor>::visit_Assignment(x);
            replacer.result_var_ = nullptr;
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
