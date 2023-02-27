#include <cmath>

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
implementation in ASR that we construct (and cache) on the fly for the actual
arguments.

Call this pass if you do not want to implement intrinsic functions directly
in the backend.

*/

ASR::expr_t *eval_log_gamma(Allocator &al, const Location &loc, ASR::expr_t* arg) {
    double rv = ASR::down_cast<ASR::RealConstant_t>(arg)->m_r;
    double val = lgamma(rv);
    ASR::ttype_t *t = ASRUtils::expr_type(arg);
    return ASR::down_cast<ASR::expr_t>(ASR::make_RealConstant_t(al, loc, val, t));
}

ASR::symbol_t* instantiate_LogGamma(Allocator &al, Location &loc,
        SymbolTable *global_scope, const std::string &new_name,
        ASR::ttype_t *arg_type) {
    SymbolTable *fn_symtab = al.make_new<SymbolTable>(global_scope);

    Vec<ASR::expr_t*> args;
    {
        args.reserve(al, 1);
        ASR::symbol_t *arg = ASR::down_cast<ASR::symbol_t>(ASR::make_Variable_t(
            al, loc, fn_symtab, s2c(al, "x"), nullptr, 0, ASR::intentType::In,
            nullptr, nullptr, ASR::storage_typeType::Default, arg_type,
            ASR::abiType::Source, ASR::Public, ASR::presenceType::Required, false));
        fn_symtab->add_symbol(s2c(al, "x"), arg);
        args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc, arg)));
    }

    ASR::symbol_t *return_var = ASR::down_cast<ASR::symbol_t>(ASR::make_Variable_t(
        al, loc, fn_symtab, s2c(al, new_name), nullptr, 0, ASRUtils::intent_return_var,
        nullptr, nullptr, ASR::storage_typeType::Default, arg_type,
        ASR::abiType::Source, ASR::Public, ASR::presenceType::Required, false));
    fn_symtab->add_symbol(s2c(al, new_name), return_var);

    Vec<ASR::stmt_t*> body;
    body.reserve(al, 1);

    Vec<char *> dep;
    dep.reserve(al, 1);

    {
        SymbolTable *fn_symtab_1 = al.make_new<SymbolTable>(fn_symtab);
        std::string c_func_name;
        if (ASRUtils::extract_kind_from_ttype_t(arg_type) == 4) {
            c_func_name = "_lfortran_slog_gamma";
        } else {
            c_func_name = "_lfortran_dlog_gamma";
        }
        Vec<ASR::expr_t*> args_1;
        {
            args_1.reserve(al, 1);
            ASR::symbol_t *arg = ASR::down_cast<ASR::symbol_t>(ASR::make_Variable_t(
                al, loc, fn_symtab_1, s2c(al, "x"), nullptr, 0, ASR::intentType::In,
                nullptr, nullptr, ASR::storage_typeType::Default, arg_type,
                ASR::abiType::BindC, ASR::Public, ASR::presenceType::Required, true));
            fn_symtab_1->add_symbol(s2c(al, "x"), arg);
            args_1.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc, arg)));
        }

        ASR::symbol_t *return_var_1 = ASR::down_cast<ASR::symbol_t>(ASR::make_Variable_t(
            al, loc, fn_symtab_1, s2c(al, c_func_name), nullptr, 0, ASRUtils::intent_return_var,
            nullptr, nullptr, ASR::storage_typeType::Default, arg_type,
            ASR::abiType::BindC, ASR::Public, ASR::presenceType::Required, false));
        fn_symtab_1->add_symbol(s2c(al, c_func_name), return_var_1);

        ASR::symbol_t *s =  ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Function_t_util(al, loc, fn_symtab_1,
            s2c(al, c_func_name), nullptr, 0, args_1.p, args_1.n, nullptr, 0,
            ASRUtils::EXPR(ASR::make_Var_t(al, loc, return_var_1)),
            ASR::abiType::BindC, ASR::accessType::Public,
            ASR::deftypeType::Interface, s2c(al, c_func_name), false, false,
            false, false, false, nullptr, 0, nullptr, 0, false, false, false));
        fn_symtab->add_symbol(c_func_name, s);
        dep.push_back(al, s2c(al, c_func_name));
        Vec<ASR::call_arg_t> call_args;
        {
            call_args.reserve(al, 1);
            ASR::call_arg_t arg;
            arg.m_value = args[0];
            call_args.push_back(al, arg);
        }
        body.push_back(al, ASRUtils::STMT(ASR::make_Assignment_t(al, loc,
            ASRUtils::EXPR(ASR::make_Var_t(al, loc, return_var)),
            ASRUtils::EXPR(ASR::make_FunctionCall_t(al, loc, s, s,
            call_args.p, call_args.n, arg_type, nullptr, nullptr)), nullptr)));
    }

    ASR::asr_t* new_subrout = ASRUtils::make_Function_t_util(al, loc,
        fn_symtab, s2c(al, new_name), dep.p, dep.n, args.p, args.n, body.p, body.n,
        ASRUtils::EXPR(ASR::make_Var_t(al, loc, return_var)),
        ASR::abiType::Source, ASR::accessType::Public,
        ASR::deftypeType::Implementation, nullptr, false, false, false,
        false, false, nullptr, 0, nullptr, 0, false, false, false);
    ASR::symbol_t *new_symbol = ASR::down_cast<ASR::symbol_t>(new_subrout);
    global_scope->add_symbol(new_name, new_symbol);
    return new_symbol;
}


class ReplaceIntrinsicFunction: public ASR::BaseExprReplacer<ReplaceIntrinsicFunction> {

    private:

    Allocator& al;
    SymbolTable* global_scope;

    public:

    ReplaceIntrinsicFunction(Allocator& al_, SymbolTable* global_scope_) :
        al(al_), global_scope(global_scope_) {}


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
                // TODO: currently we always instantiate a new function.
                // Rather we should reuse the old instantiation if it has
                // exactly the same arguments. For that we could use the
                // overload_id, and uniquely encode the argument types.
                // We could maintain a mapping of type -> id and look it up.
                std::string new_name = global_scope->get_unique_name("_lcompilers_LogGamma");
                ASR::symbol_t* new_func_sym = instantiate_LogGamma(al, x->base.base.loc,
                    global_scope, new_name, ASRUtils::expr_type(x->m_args[0]));
                Vec<ASR::call_arg_t> new_args;
                new_args.reserve(al, x->n_args);
                ASR::call_arg_t arg0;
                arg0.m_value = arg;
                new_args.push_back(al, arg0);
                ASR::expr_t *value = nullptr;
                ASR::expr_t *arg_value = ASRUtils::expr_value(arg);
                if (arg_value) {
                    value = eval_log_gamma(al, x->base.base.loc, arg_value);
                }

                ASR::expr_t* new_call = ASRUtils::EXPR(ASR::make_FunctionCall_t(al,
                    x->base.base.loc, new_func_sym, new_func_sym,
                    new_args.p, new_args.size(), ASRUtils::expr_type(x->m_args[0]),
                    value, nullptr));

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

        ReplaceIntrinsicFunctionVisitor(Allocator& al_, SymbolTable* global_scope_) :
            replacer(al_, global_scope_) {}

        void call_replacer() {
            replacer.current_expr = current_expr;
            replacer.replace_expr(*current_expr);
        }

};

void pass_replace_intrinsic_function(Allocator &al, ASR::TranslationUnit_t &unit,
                             const LCompilers::PassOptions& /*pass_options*/) {
    ReplaceIntrinsicFunctionVisitor v(al, unit.m_global_scope);
    v.visit_TranslationUnit(unit);
}


} // namespace LCompilers
