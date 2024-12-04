#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/create_subroutine_from_function.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/intrinsic_array_function_registry.h>
#include <libasr/pass/pass_utils.h>

namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

class CreateFunctionFromSubroutineSimplifier: public ASR::BaseWalkVisitor<CreateFunctionFromSubroutineSimplifier> {

    public:

        Allocator& al;

        CreateFunctionFromSubroutineSimplifier(Allocator &al_): al(al_)
        {
        }

        void visit_Function(const ASR::Function_t& x) {
            ASR::Function_t& xx = const_cast<ASR::Function_t&>(x);
            ASR::Function_t* x_ptr = ASR::down_cast<ASR::Function_t>(&(xx.base));
            PassUtils::handle_fn_return_var(al, x_ptr, PassUtils::is_aggregate_or_array_type);
        }

};

class ReplaceFunctionCallWithSubroutineCallSimplifierVisitor:
    public ASR::CallReplacerOnExpressionsVisitor<ReplaceFunctionCallWithSubroutineCallSimplifierVisitor> {

    private:

        Allocator& al;
        Vec<ASR::stmt_t*> pass_result;
        Vec<ASR::stmt_t*>* parent_body = nullptr;


    public:

        ReplaceFunctionCallWithSubroutineCallSimplifierVisitor(Allocator& al_): al(al_)
        {
            pass_result.n = 0;
            pass_result.reserve(al, 1);
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
            for (size_t i = 0; i < n_body; i++) {
                parent_body = &body;
                visit_stmt(*m_body[i]);
                if( pass_result.size() > 0 ) {
                    for (size_t j=0; j < pass_result.size(); j++) {
                        body.push_back(al, pass_result[j]);
                    }
                    pass_result.n = 0;
                } else {
                    body.push_back(al, m_body[i]);
                }
            }
            m_body = body.p;
            n_body = body.size();
        }

        bool is_function_call_returning_aggregate_type(ASR::expr_t* m_value) {
            bool is_function_call = ASR::is_a<ASR::FunctionCall_t>(*m_value);
            bool is_aggregate_type = (ASRUtils::is_aggregate_type(
                ASRUtils::expr_type(m_value)) ||
                PassUtils::is_aggregate_or_array_type(m_value));
            return is_function_call && is_aggregate_type;
        }

        void visit_Assignment(const ASR::Assignment_t &x) {
            if( !is_function_call_returning_aggregate_type(x.m_value) ) {
                return ;
            }

            ASR::FunctionCall_t* fc = ASR::down_cast<ASR::FunctionCall_t>(x.m_value);
            if( PassUtils::is_elemental(fc->m_name) && ASRUtils::is_array(fc->m_type) ) {
                return ;
            }
            const Location& loc = x.base.base.loc;
            Vec<ASR::call_arg_t> s_args;
            s_args.reserve(al, fc->n_args + 1);
            for( size_t i = 0; i < fc->n_args; i++ ) {
                s_args.push_back(al, fc->m_args[i]);
            }
            if(ASRUtils::is_allocatable(x.m_value)){ // Make sure to deallocate the argument that will hold the return of function.
                Vec<ASR::expr_t*> to_be_deallocated;
                to_be_deallocated.reserve(al, 1);
                to_be_deallocated.push_back(al, x.m_target);
                pass_result.push_back(al, ASRUtils::STMT(
                    ASR::make_ImplicitDeallocate_t(al, x.m_target->base.loc,
                    to_be_deallocated.p, to_be_deallocated.size())));
            }
            ASR::call_arg_t result_arg;
            result_arg.loc = x.m_target->base.loc;
            result_arg.m_value = x.m_target;
            s_args.push_back(al, result_arg);
            ASR::stmt_t* subrout_call = ASRUtils::STMT(ASRUtils::make_SubroutineCall_t_util(al, loc,
                fc->m_name, fc->m_original_name, s_args.p, s_args.size(), fc->m_dt, nullptr, false, false));
            pass_result.push_back(al, subrout_call);
        }
};

void pass_create_subroutine_from_function_simplifier(Allocator &al, ASR::TranslationUnit_t &unit,
                                          const LCompilers::PassOptions& /*pass_options*/) {
    CreateFunctionFromSubroutineSimplifier v(al);
    v.visit_TranslationUnit(unit);
    ReplaceFunctionCallWithSubroutineCallSimplifierVisitor u(al);
    u.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor w(al);
    w.visit_TranslationUnit(unit);
}


} // namespace LCompilers