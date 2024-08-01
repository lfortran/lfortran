#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/create_subroutine_from_function.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/intrinsic_array_function_registry.h>
#include <libasr/pass/pass_utils.h>

#include <vector>
#include <string>


namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

class CreateFunctionFromSubroutine: public PassUtils::PassVisitor<CreateFunctionFromSubroutine> {

    public:

        CreateFunctionFromSubroutine(Allocator &al_) :
        PassVisitor(al_, nullptr)
        {
            pass_result.reserve(al, 1);
        }

        void visit_TranslationUnit(const ASR::TranslationUnit_t &x) {
            // Transform functions returning arrays to subroutines
            for (auto &item : x.m_symtab->get_scope()) {
                if (is_a<ASR::Function_t>(*item.second)) {
                    PassUtils::handle_fn_return_var(al,
                        ASR::down_cast<ASR::Function_t>(item.second),
                        PassUtils::is_aggregate_or_array_type);
                }
            }

            std::vector<std::string> build_order
                = ASRUtils::determine_module_dependencies(x);
            for (auto &item : build_order) {
                LCOMPILERS_ASSERT(x.m_symtab->get_symbol(item));
                ASR::symbol_t *mod = x.m_symtab->get_symbol(item);
                visit_symbol(*mod);
            }
            // Now visit everything else
            for (auto &item : x.m_symtab->get_scope()) {
                if (!ASR::is_a<ASR::Module_t>(*item.second)) {
                    this->visit_symbol(*item.second);
                }
            }
        }

        void visit_Module(const ASR::Module_t &x) {
            current_scope = x.m_symtab;
            for (auto &item : x.m_symtab->get_scope()) {
                if (is_a<ASR::Function_t>(*item.second)) {
                    PassUtils::handle_fn_return_var(al,
                        ASR::down_cast<ASR::Function_t>(item.second),
                        PassUtils::is_aggregate_or_array_type);
                }
            }

            // Now visit everything else
            for (auto &item : x.m_symtab->get_scope()) {
                this->visit_symbol(*item.second);
            }
        }

        void visit_Program(const ASR::Program_t &x) {
            std::vector<std::pair<std::string, ASR::symbol_t*> > replace_vec;
            // FIXME: this is a hack, we need to pass in a non-const `x`,
            // which requires to generate a TransformVisitor.
            ASR::Program_t &xx = const_cast<ASR::Program_t&>(x);
            current_scope = xx.m_symtab;

            for (auto &item : x.m_symtab->get_scope()) {
                if (is_a<ASR::Function_t>(*item.second)) {
                    PassUtils::handle_fn_return_var(al,
                        ASR::down_cast<ASR::Function_t>(item.second),
                        PassUtils::is_aggregate_or_array_type);
                }
            }

            for (auto &item : x.m_symtab->get_scope()) {
                if (is_a<ASR::AssociateBlock_t>(*item.second)) {
                    ASR::AssociateBlock_t *s = ASR::down_cast<ASR::AssociateBlock_t>(item.second);
                    visit_AssociateBlock(*s);
                }
                if (is_a<ASR::Function_t>(*item.second)) {
                    visit_Function(*ASR::down_cast<ASR::Function_t>(item.second));
                }
            }

            current_scope = xx.m_symtab;
            transform_stmts(xx.m_body, xx.n_body);

        }

};

class ReplaceFunctionCallWithSubroutineCallVisitor:
    public ASR::CallReplacerOnExpressionsVisitor<ReplaceFunctionCallWithSubroutineCallVisitor> {

    private:

        Allocator& al;
        Vec<ASR::stmt_t*> pass_result;

    public:

        ReplaceFunctionCallWithSubroutineCallVisitor(Allocator& al_): al(al_)
        {
            pass_result.n = 0;
        }

        void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
            Vec<ASR::stmt_t*> body;
            body.reserve(al, n_body);
            for (size_t i = 0; i < n_body; i++) {
                pass_result.n = 0;
                pass_result.reserve(al, 1);
                visit_stmt(*m_body[i]);
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
            if( PassUtils::is_elemental(fc->m_name) ) {
                return ;
            }
            const Location& loc = x.base.base.loc;
            Vec<ASR::call_arg_t> s_args;
            s_args.reserve(al, fc->n_args + 1);
            for( size_t i = 0; i < fc->n_args; i++ ) {
                s_args.push_back(al, fc->m_args[i]);
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
