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

    bool inside_prohibited_expression;


    CompileTimeValueReplacer(Allocator& al_):
        al(al_), inside_prohibited_expression(false) {}

    void replace_ArrayReshape(ASR::ArrayReshape_t* x) {
        ASR::BaseExprReplacer<CompileTimeValueReplacer>::replace_ArrayReshape(x);
        if( ASRUtils::is_fixed_size_array(
                ASRUtils::expr_type(x->m_array)) ) {
            x->m_type = ASRUtils::duplicate_type(al, x->m_type, nullptr,
                    ASR::array_physical_typeType::FixedSizeArray, true);
        }
    }

    template <typename ExprType>
    void replace_ExprMethod(ExprType* x,
            void (ASR::BaseExprReplacer<CompileTimeValueReplacer>::*replacer_function)(ExprType*) ) {
        bool inside_prohibited_expression_copy = inside_prohibited_expression;
        inside_prohibited_expression = true;
        (this->*replacer_function)(x);
        inside_prohibited_expression = inside_prohibited_expression_copy;
    }

    void replace_ArrayItem(ASR::ArrayItem_t* x) {
        replace_ExprMethod(x, &ASR::BaseExprReplacer<CompileTimeValueReplacer>::replace_ArrayItem);
    }

    void replace_StringSection(ASR::StringSection_t* x) {
        replace_ExprMethod(x, &ASR::BaseExprReplacer<CompileTimeValueReplacer>::replace_StringSection);
    }

    void replace_StringItem(ASR::StringItem_t* x) {
        replace_ExprMethod(x, &ASR::BaseExprReplacer<CompileTimeValueReplacer>::replace_StringItem);
    }

    void replace_expr(ASR::expr_t* x) {
        if( x == nullptr || inside_prohibited_expression ) {
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

    template <typename ExprType>
    void visit_ExprMethod(const ExprType& x,
            void (ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::*visitor_function)(const ExprType&) ) {
        bool inside_prohibited_expression_copy = replacer.inside_prohibited_expression;
        replacer.inside_prohibited_expression = true;
        (this->*visitor_function)(x);
        replacer.inside_prohibited_expression = inside_prohibited_expression_copy;
    }

    void visit_ArrayItem(const ASR::ArrayItem_t& x) {
        visit_ExprMethod(x, &ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_ArrayItem);
    }

    void visit_StringSection(const ASR::StringSection_t& x) {
        visit_ExprMethod(x, &ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_StringSection);
    }

    void visit_StringItem(const ASR::StringItem_t& x) {
        visit_ExprMethod(x, &ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_StringItem);
    }

    void visit_Assignment(const ASR::Assignment_t& x) {
        // Don't replace the assignment target with its compile-time value
        // (e.g., ch(5:5) should not become "4" when used as an assignment target)
        ASR::Assignment_t& xx = const_cast<ASR::Assignment_t&>(x);

        // Visit the target without calling the replacer
        bool inside_prohibited_expression_copy = replacer.inside_prohibited_expression;
        replacer.inside_prohibited_expression = true;
        ASR::expr_t** current_expr_copy = current_expr;
        current_expr = &(xx.m_target);
        replacer.current_expr = current_expr;
        ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_expr(**current_expr);
        current_expr = current_expr_copy;
        replacer.inside_prohibited_expression = inside_prohibited_expression_copy;

        // Visit the value normally (can be replaced)
        current_expr = &(xx.m_value);
        replacer.current_expr = current_expr;
        ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_expr(**current_expr);
        current_expr = current_expr_copy;
    }

    void visit_FileWrite(const ASR::FileWrite_t& x) {
        // Don't replace string write targets as compile-time constants
        ASR::FileWrite_t& xx = const_cast<ASR::FileWrite_t&>(x);
        ASR::expr_t** current_expr_copy = current_expr;
        // Check if writing to a string (not a file unit)
        bool is_string_write = xx.m_unit && ASRUtils::is_character(*ASRUtils::expr_type(xx.m_unit));
        if (is_string_write) {
            // Visit the unit (write target) without calling the replacer
            bool inside_prohibited_expression_copy = replacer.inside_prohibited_expression;
            replacer.inside_prohibited_expression = true;
            current_expr = &(xx.m_unit);
            replacer.current_expr = current_expr;
            ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_expr(**current_expr);
            replacer.inside_prohibited_expression = inside_prohibited_expression_copy;
        } else {
            // Unit can be replaced normally
            if (xx.m_unit) {
                current_expr = &(xx.m_unit);
                replacer.current_expr = current_expr;
                ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_expr(**current_expr);
            }
        }
        // Visit other expr_t* fields normally
        if (xx.m_iomsg) {
            current_expr = &(xx.m_iomsg);
            replacer.current_expr = current_expr;
            ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_expr(**current_expr);
        }
        if (xx.m_iostat) {
            current_expr = &(xx.m_iostat);
            replacer.current_expr = current_expr;
            ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_expr(**current_expr);
        }
        if (xx.m_id) {
            current_expr = &(xx.m_id);
            replacer.current_expr = current_expr;
            ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_expr(**current_expr);
        }
        // Visit values (can be replaced)
        for (size_t i = 0; i < xx.n_values; i++) {
            current_expr = &(xx.m_values[i]);
            replacer.current_expr = current_expr;
            this->visit_expr(*xx.m_values[i]);
        }
        if (xx.m_separator) {
            current_expr = &(xx.m_separator);
            replacer.current_expr = current_expr;
            ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_expr(**current_expr);
        }
        if (xx.m_end) {
            current_expr = &(xx.m_end);
            replacer.current_expr = current_expr;
            ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_expr(**current_expr);
        }
        current_expr = current_expr_copy;
    }
    
    void visit_FileRead(const ASR::FileRead_t& x) {
        ASR::FileRead_t& xx = const_cast<ASR::FileRead_t&>(x);
        ASR::expr_t** current_expr_copy = current_expr;

        // Visit unit, iomsg, iostat, id normally
        if (xx.m_unit) {
            current_expr = &(xx.m_unit);
            replacer.current_expr = current_expr;
            ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_expr(**current_expr);
        }
        if (xx.m_iomsg) {
            current_expr = &(xx.m_iomsg);
            replacer.current_expr = current_expr;
            ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_expr(**current_expr);
        }
        if (xx.m_iostat) {
            current_expr = &(xx.m_iostat);
            replacer.current_expr = current_expr;
            ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_expr(**current_expr);
        }
        if (xx.m_id) {
            current_expr = &(xx.m_id);
            replacer.current_expr = current_expr;
            ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_expr(**current_expr);
        }
        for (size_t i = 0; i < xx.n_values; i++) {
            bool inside_copy = replacer.inside_prohibited_expression;
            replacer.inside_prohibited_expression = true;
            current_expr = &(xx.m_values[i]);
            replacer.current_expr = current_expr;
            this->visit_expr(*xx.m_values[i]);
            replacer.inside_prohibited_expression = inside_copy;
        }
        if (xx.m_fmt) {
            current_expr = &(xx.m_fmt);
            replacer.current_expr = current_expr;
            ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_expr(**current_expr);
        }
        current_expr = current_expr_copy;
    }
    
};

void pass_replace_with_compile_time_values(
    Allocator &al, ASR::TranslationUnit_t &unit,
    const LCompilers::PassOptions& /*pass_options*/) {
    ExprVisitor v(al);
    // v.call_replacer_on_value = false;
    v.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}


} // namespace LCompilers
