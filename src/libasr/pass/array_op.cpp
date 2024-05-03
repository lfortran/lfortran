#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/replace_array_op.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/intrinsic_array_function_registry.h>

#include <vector>
#include <utility>

namespace LCompilers {

class ReplaceArrayOp: public ASR::BaseExprReplacer<ReplaceArrayOp> {

    private:

    Allocator& al;
    Vec<ASR::stmt_t*>& pass_result;

    public:

    ASR::expr_t* result_expr;

    ReplaceArrayOp(Allocator& al_, Vec<ASR::stmt_t*>& pass_result_):
        al(al_), pass_result(pass_result_) {}

    void replace_ArrayConstant(ASR::ArrayConstant_t* x) {
        pass_result.reserve(al, x->m_n_data);
        const Location& loc = x->base.base.loc;
        LCOMPILERS_ASSERT(result_expr != nullptr);
        ASR::ttype_t* result_type = ASRUtils::expr_type(result_expr);
        ASR::ttype_t* result_element_type = ASRUtils::type_get_past_array_pointer_allocatable(result_type);
        for( int64_t i = 0; i < ASRUtils::get_fixed_size_of_array(x->m_type); i++ ) {
            ASR::expr_t* x_i = ASRUtils::fetch_ArrayConstant_value(al, x, i);
            Vec<ASR::array_index_t> array_index_args;
            array_index_args.reserve(al, 1);
            ASR::array_index_t array_index_arg;
            array_index_arg.loc = loc;
            array_index_arg.m_left = nullptr;
            array_index_arg.m_right = make_ConstantWithKind(
                make_IntegerConstant_t, make_Integer_t, i + 1, 4, loc);
            array_index_arg.m_step = nullptr;
            array_index_args.push_back(al, array_index_arg);
            ASR::expr_t* y_i = ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc,
                result_expr, array_index_args.p, array_index_args.size(),
                result_element_type, ASR::arraystorageType::ColMajor, nullptr));
            pass_result.push_back(al, ASRUtils::STMT(ASR::make_Assignment_t(al, loc, y_i, x_i, nullptr)));
        }
    }

};

class ArrayOpVisitor: public ASR::CallReplacerOnExpressionsVisitor<ArrayOpVisitor> {
    private:

    Allocator& al;
    ReplaceArrayOp replacer;
    Vec<ASR::stmt_t*> pass_result;

    public:

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }

    ArrayOpVisitor(Allocator& al_): al(al_), replacer(al, pass_result) {
        pass_result.n = 0;
        pass_result.reserve(al, 0);
    }

    void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
        Vec<ASR::stmt_t*> body;
        body.reserve(al, n_body);
        for (size_t i=0; i<n_body; i++) {
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

    void visit_Assignment(const ASR::Assignment_t& x) {
        replacer.result_expr = x.m_target;
        ASR::expr_t** current_expr_copy = current_expr;
        current_expr = const_cast<ASR::expr_t**>(&x.m_value);
        this->call_replacer();
        current_expr = current_expr_copy;
        replacer.result_expr = nullptr;
    }

};

void pass_replace_array_op(Allocator &al, ASR::TranslationUnit_t &unit,
                           const LCompilers::PassOptions& /*pass_options*/) {
    ArrayOpVisitor v(al);
    v.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}


} // namespace LCompilers
