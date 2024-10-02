#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/replace_param_to_const.h>


namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

/*

This ASR pass replaces initializer expressions with evaluated values. The function
`pass_replace_param_to_const` transforms the ASR tree in-place.

Converts:

    integer, parameter :: i = 20
    integer :: a = i**2 + i

to:

    integer, parameter :: i = 20
    integer :: a = 20**2 + 20

*/

class VarVisitor : public ASR::BaseWalkVisitor<VarVisitor>
{
private:
    ASR::expr_t* asr;

public:
    // Currently `al` is not used, but we might need it in the future:
    Allocator &al;

    VarVisitor(Allocator &al) : asr{nullptr}, al{al} {
    }

    void visit_Program( ASR::Program_t &x) {
        for (auto &item : x.m_symtab->get_scope()) {
            if (is_a<ASR::Variable_t>(*item.second)) {
                ASR::Variable_t *v = down_cast<ASR::Variable_t>(item.second);
                visit_Variable(*v);
            }
        }
    }

    void visit_IntegerUnaryMinus( ASR::IntegerUnaryMinus_t &x) {
        visit_UnaryOp(x);
    }
    void visit_RealUnaryMinus( ASR::RealUnaryMinus_t &x) {
        visit_UnaryOp(x);
    }
    void visit_ComplexUnaryMinus( ASR::ComplexUnaryMinus_t &x) {
        visit_UnaryOp(x);
    }
    void visit_IntegerBitNot( ASR::IntegerBitNot_t &x) {
        visit_UnaryOp(x);
    }
    void visit_LogicalNot( ASR::LogicalNot_t &x) {
        visit_UnaryOp(x);
    }

    template<typename T>
    void visit_UnaryOp( T& x) {
        asr = nullptr;
        this->visit_expr(*x.m_arg);
        if( asr != nullptr ) {
            x.m_arg = asr;
        }
        asr = &(x.base);
    }

    void visit_IntegerCompare( ASR::IntegerCompare_t& x) {
        handle_Compare(x);
    }

    void visit_RealCompare( ASR::RealCompare_t &x) {
        handle_Compare(x);
    }

    void visit_ComplexCompare( ASR::ComplexCompare_t &x) {
        handle_Compare(x);
    }

    void visit_LogicalCompare( ASR::LogicalCompare_t &x) {
        handle_Compare(x);
    }

    void visit_StringCompare( ASR::StringCompare_t &x) {
        handle_Compare(x);
    }

    template <typename T>
    void handle_Compare( T& x) {
        asr = nullptr;
        this->visit_expr(*x.m_left);
        if( asr != nullptr ) {
            x.m_left = asr;
        }
        asr = nullptr;
        this->visit_expr(*x.m_right);
        if( asr != nullptr ) {
            x.m_right = asr;
        }
        asr = &(x.base);
    }

    void visit_IntegerBinOp( ASR::IntegerBinOp_t& x) {
        handle_BinOp(x);
    }

    void visit_RealBinOp( ASR::RealBinOp_t& x) {
        handle_BinOp(x);
    }

    void visit_ComplexBinOp( ASR::ComplexBinOp_t& x) {
        handle_BinOp(x);
    }

    void visit_LogicalBinOp( ASR::LogicalBinOp_t& x) {
        handle_BinOp(x);
    }

    template <typename T>
    void handle_BinOp( T& x) {
        asr = nullptr;
        this->visit_expr(*x.m_left);
        if( asr != nullptr ) {
            x.m_left = asr;
        }
        asr = nullptr;
        this->visit_expr(*x.m_right);
        if( asr != nullptr ) {
            x.m_right = asr;
        }
        asr = &(x.base);
    }

    void visit_Cast( ASR::Cast_t& x) {
        /*
        asr = nullptr;
        this->visit_expr(*x.m_arg);
        if( asr != nullptr ) {
            x.m_arg = asr;
        }
        */
        asr = &(x.base);
    }

    void visit_Var( ASR::Var_t& x) {
        if (is_a<ASR::Variable_t>(*ASRUtils::symbol_get_past_external(x.m_v))) {
            ASR::Variable_t *init_var = ASR::down_cast<ASR::Variable_t>(ASRUtils::symbol_get_past_external(x.m_v));
            if( init_var->m_storage == ASR::storage_typeType::Parameter ) {
                if( init_var->m_symbolic_value == nullptr ) {
                    asr = init_var->m_symbolic_value;
                } else {
                    switch( init_var->m_symbolic_value->type ) {
                        case ASR::exprType::IntegerConstant:
                        case ASR::exprType::RealConstant:
                        case ASR::exprType::ComplexConstant:
                        case ASR::exprType::LogicalConstant:
                        case ASR::exprType::StringConstant: {
                            asr = init_var->m_symbolic_value;
                            break;
                        }
                        default: {
                            this->visit_expr(*(init_var->m_symbolic_value));
                        }
                    }
                }
            }
        }
    }

    void visit_Variable( ASR::Variable_t& x) {
        if( x.m_symbolic_value != nullptr ) {
            asr = nullptr;
            visit_expr(*(x.m_symbolic_value));
            if( asr != nullptr ) {
                x.m_symbolic_value = asr;
                asr = nullptr;
            }
        }
    }
};

void pass_replace_param_to_const(Allocator &al, ASR::TranslationUnit_t &unit,
                                 const LCompilers::PassOptions &/*pass_options*/
                                 ) {
    VarVisitor v(al);
    v.visit_TranslationUnit(unit);
}

} // namespace LCompilers
