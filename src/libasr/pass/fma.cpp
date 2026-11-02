#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/replace_fma.h>
#include <libasr/pass/pass_utils.h>


namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

/*

This ASR pass converts fused multiplication and addition (FMA) expressions
with a call to internal optimization routine for FMA. This allows
backend specific code generation for FMA expressions resulting in optimized
code.

Converts:

    real :: a, b, c, d
    d = a + b * c

to:

    d = fma(a, b, c)

A scalar accumulation such as `s = s + a * b` (the assignment target is the
addend) is not converted unless fast-math is disabled: the loop-carried
fma call would hide the reduction from the LLVM loop vectorizer, while with
fast-math LLVM fuses the multiply and the add itself.

*/

class ReplaceRealBinOpFMA : public ASR::BaseExprReplacer<ReplaceRealBinOpFMA>{
    Allocator& al;
    const LCompilers::PassOptions& pass_options;
    const ASR::TranslationUnit_t& unit;

    bool is_BinOpMul(ASR::expr_t* expr) {
        if (ASR::is_a<ASR::RealBinOp_t>(*expr)) {
            ASR::RealBinOp_t* expr_binop = ASR::down_cast<ASR::RealBinOp_t>(expr);
            return expr_binop->m_op == ASR::binopType::Mul;
        }
        return false;
    }

    // An expression that still adds to the accumulator, i.e. the
    // accumulator itself or an unconverted `acc + ...` chain.
    bool is_accumulation(ASR::expr_t* expr) {
        if (ASR::is_a<ASR::Var_t>(*expr)) {
            return ASR::down_cast<ASR::Var_t>(expr)->m_v == accumulator;
        }
        if (ASR::is_a<ASR::RealBinOp_t>(*expr)) {
            ASR::RealBinOp_t* binop = ASR::down_cast<ASR::RealBinOp_t>(expr);
            return (binop->m_op == ASR::binopType::Add ||
                    binop->m_op == ASR::binopType::Sub) &&
                (is_accumulation(binop->m_left) || is_accumulation(binop->m_right));
        }
        return false;
    }

public :
    // Scalar variable assigned by the current statement, if its value is
    // to be kept out of fma calls.
    ASR::symbol_t* accumulator = nullptr;

    ReplaceRealBinOpFMA(Allocator& al, 
        const LCompilers::PassOptions& pass_options, const ASR::TranslationUnit_t& unit)
        :al{al}, pass_options{pass_options}, unit{unit}{}
    
    void replace_RealBinOp(ASR::RealBinOp_t* x) {
        BaseExprReplacer::replace_RealBinOp(x);

        if( x->m_op != ASR::binopType::Add && x->m_op != ASR::binopType::Sub ) {
            return ;
        }
        ASR::expr_t *mul_expr = nullptr, *other_expr = nullptr;
        bool is_mul_expr_negative = false, is_other_expr_negative = false;
        if( is_BinOpMul(x->m_right) ) {
            mul_expr = x->m_right;
            other_expr = x->m_left;
            is_mul_expr_negative = (x->m_op == ASR::binopType::Sub);
        } else if( is_BinOpMul(x->m_left) ) {
            mul_expr = x->m_left;
            other_expr = x->m_right;
            is_other_expr_negative = (x->m_op == ASR::binopType::Sub);
        } else {
            return ;
        }
        if( accumulator && is_accumulation(other_expr) ) {
            return ;
        }

        if( is_other_expr_negative ) {
            other_expr = ASRUtils::EXPR(ASR::make_RealUnaryMinus_t(al, other_expr->base.loc, other_expr,
                ASRUtils::expr_type(other_expr), nullptr));
        }

        ASR::RealBinOp_t* mul_binop = ASR::down_cast<ASR::RealBinOp_t>(mul_expr);
        ASR::expr_t *first_arg = mul_binop->m_left, *second_arg = mul_binop->m_right;
        if( is_mul_expr_negative ) {
            first_arg = ASRUtils::EXPR(ASR::make_RealUnaryMinus_t(al, first_arg->base.loc, first_arg,
                ASRUtils::expr_type(first_arg), nullptr));
        }

        *current_expr = PassUtils::get_fma(other_expr, first_arg, second_arg,
                                     al, const_cast<ASR::TranslationUnit_t&>(unit), x->base.base.loc, 
                                     const_cast<LCompilers::PassOptions&>(pass_options));
    }


};



class CallReplacerFMA : public ASR::CallReplacerOnExpressionsVisitor<CallReplacerFMA>{
    ReplaceRealBinOpFMA replacer;
    const LCompilers::PassOptions& pass_options;
public :
    CallReplacerFMA(Allocator &al, const ASR::TranslationUnit_t &unit,
                      const LCompilers::PassOptions& pass_options) 
                      : replacer{al, pass_options, unit}, pass_options{pass_options}{}
    void visit_Assignment(const ASR::Assignment_t& x){
        ASR::ttype_t* target_type = ASRUtils::expr_type(x.m_target);
        if( !pass_options.no_fast_math && ASR::is_a<ASR::Var_t>(*x.m_target) &&
                ASRUtils::is_real(*target_type) && !ASRUtils::is_array(target_type) ) {
            replacer.accumulator = ASR::down_cast<ASR::Var_t>(x.m_target)->m_v;
        }
        CallReplacerOnExpressionsVisitor::visit_Assignment(x);
        replacer.accumulator = nullptr;
    }
    void call_replacer(){
        if(ASR::is_a<ASR::RealBinOp_t>(**current_expr)){
            replacer.current_expr = current_expr;
            replacer.replace_expr(*current_expr);
        }
    }
    void visit_Function(const ASR::Function_t& x){ // Avoid visiting RealBinOp in the fma` function itself
        // Leave contraction to the device compiler after loop scheduling.
        if (ASRUtils::runs_on_device(x)) return;
        if(std::string(x.m_name) == "_lcompilers_optimization_fma_f32" || 
            std::string(x.m_name) == "_lcompilers_optimization_fma_f64"){
            return;
        }
        CallReplacerOnExpressionsVisitor::visit_Function(x);
    }
    
};


void pass_replace_fma(Allocator &al, ASR::TranslationUnit_t &unit,
                      const LCompilers::PassOptions& pass_options) {
    CallReplacerFMA realBinOpFMA_replacer{al, unit, pass_options};
    realBinOpFMA_replacer.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}


} // namespace LCompilers
