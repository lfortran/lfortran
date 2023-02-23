#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/intrinsic_function.h>
#include <libasr/pass/pass_utils.h>

#include <vector>
#include <utility>


namespace LCompilers {

/*

This ASR pass replaces the IntrinsicFunction node with a call to an
implementation in the surface language (ASR).

Call this pass if you do not want to implement intrinsic functions directly
in the backend.

*/

class ReplaceIntrinsicFunction: public ASR::BaseExprReplacer<ReplaceIntrinsicFunction> {

    private:

    Allocator& al;

    public:

    ReplaceIntrinsicFunction(Allocator& al_) : al(al_)
    {}

    ASR::symbol_t* instantiate_LogGamma(ASR::ttype_t *arg_type) {
        ASR::asr_t* new_subrout = nullptr; /*ASRUtils::make_Function_t_util(al, x->base.base.loc,
                                new_symtab, s2c(al, new_name), x->m_dependencies, x->n_dependencies,
                                new_args.p, new_args.size(),  new_body.p, new_body.size(),
                                return_var, x_func_type->m_abi, x->m_access, x_func_type->m_deftype,
                                s2c(al, new_bindc_name), x_func_type->m_elemental,
                                x_func_type->m_pure, x_func_type->m_module, x_func_type->m_inline,
                                x_func_type->m_static, nullptr, 0, nullptr, 0, false, false, false); */
        ASR::symbol_t *new_symbol = ASR::down_cast<ASR::symbol_t>(new_subrout);
        //current_scope->add_symbol(new_name, new_symbol);
        return new_symbol;
    };


    void replace_IntrinsicFunction(ASR::IntrinsicFunction_t* x) {
        switch (x->m_intrinsic_id) {
            case (static_cast<int64_t>(ASRUtils::IntrinsicFunctions::LogGamma)) : {
                LCOMPILERS_ASSERT(x->n_args == 1)
                // Replace any IntrinsicFunctions in the argument first:
                ASR::expr_t** current_expr_copy_ = current_expr;
                current_expr = &(x->m_args[0]);
                replace_expr(x->m_args[0]);
                ASR::expr_t *arg = *current_expr; // Use the converted arg
                current_expr = current_expr_copy_;
                ASR::symbol_t* new_func_sym = instantiate_LogGamma(ASRUtils::expr_type(x->m_args[0]));

                Vec<ASR::call_arg_t> new_args;
                new_args.reserve(al, x->n_args);
                ASR::call_arg_t arg0;
                arg0.m_value = arg;
                new_args.push_back(al, arg0);
                ASR::expr_t* new_call = ASRUtils::EXPR(ASR::make_FunctionCall_t(al,
                                            x->base.base.loc, new_func_sym, new_func_sym,
                                            new_args.p, new_args.size(), ASRUtils::symbol_type(new_func_sym), nullptr,
                                            nullptr));

                *current_expr = new_call;
                break;
            }
            default : {
                throw LCompilersException("Intrinsic function not implemented");
            }
        }
    }

};

/*
The following visitor calls the above replacer i.e., ReplaceFunctionCalls
on expressions present in ASR so that FunctionCall get replaced everywhere
and we don't end up with false positives.
*/
class ReplaceIntrinsicFunctionVisitor : public ASR::CallReplacerOnExpressionsVisitor<ReplaceIntrinsicFunctionVisitor>
{
    private:

        ReplaceIntrinsicFunction replacer;

    public:

        ReplaceIntrinsicFunctionVisitor(Allocator& al_) : replacer(al_) {}

        void call_replacer() {
            replacer.current_expr = current_expr;
            replacer.replace_expr(*current_expr);
        }

};

void pass_replace_intrinsic_function(Allocator &al, ASR::TranslationUnit_t &unit,
                             const LCompilers::PassOptions& /*pass_options*/) {
    ReplaceIntrinsicFunctionVisitor v(al);
    v.visit_TranslationUnit(unit);
}


} // namespace LCompilers
