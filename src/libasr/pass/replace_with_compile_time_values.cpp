#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/replace_with_compile_time_values.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/intrinsic_array_function_registry.h>

#include <libasr/asr_builder.h>

#include <vector>

namespace LCompilers {

class CompileTimeValueReplacer: public ASR::BaseExprReplacer<CompileTimeValueReplacer> {

    private:

    Allocator& al;

    public:

    CompileTimeValueReplacer(Allocator& al_): al(al_) {}

    void replace_ArrayReshape(ASR::ArrayReshape_t* x) {
        ASR::BaseExprReplacer<CompileTimeValueReplacer>::replace_ArrayReshape(x);
        if( ASRUtils::is_fixed_size_array(
                ASRUtils::expr_type(x->m_array)) ) {
            x->m_type = ASRUtils::duplicate_type(al, x->m_type, nullptr,
                    ASR::array_physical_typeType::FixedSizeArray, true);
        }
    }

    void replace_expr(ASR::expr_t* x) {
        if( x == nullptr ) {
            return ;
        }

        bool is_array_broadcast = ASR::is_a<ASR::ArrayBroadcast_t>(*x);
        if( is_array_broadcast ) {
            return ;
        }

        ASR::BaseExprReplacer<CompileTimeValueReplacer>::replace_expr(x);

        ASR::expr_t* compile_time_value = ASRUtils::expr_value(x);
        if( compile_time_value == nullptr ) {
            return ;
        }

        size_t value_rank = ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(compile_time_value));
        size_t expr_rank = ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(x));
        // TODO: Handle with reshape later
        if( value_rank != expr_rank ) {
            return ;
        }

        *current_expr = compile_time_value;
    }
};

class ExprVisitor: public ASR::CallReplacerOnExpressionsVisitor<ExprVisitor> {

    private:

    CompileTimeValueReplacer replacer;

    public:

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }

    ExprVisitor(Allocator& al_): replacer(al_)
    {}

};

void pass_replace_with_compile_time_values(
    Allocator &al, ASR::TranslationUnit_t &unit,
    const LCompilers::PassOptions& /*pass_options*/) {
    ExprVisitor v(al);
    v.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}


} // namespace LCompilers
