#ifndef LFORTRAN_SEMANTICS_AST_COMMON_VISITOR_H
#define LFORTRAN_SEMANTICS_AST_COMMON_VISITOR_H

#include <libasr/assert.h>
#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <lfortran/ast.h>
#include <libasr/bigint.h>
#include <libasr/string_utils.h>
#include <lfortran/utils.h>
#include <lfortran/semantics/comptime_eval.h>

#include <string>
#include <unordered_set>
#include <set>
#include <map>

using LCompilers::diag::Level;
using LCompilers::diag::Stage;
using LCompilers::diag::Label;
using LCompilers::diag::Diagnostic;

namespace LCompilers::LFortran {

uint64_t static inline get_hash(ASR::asr_t *node)
{
    return (uint64_t)node;
}

#define LFORTRAN_STMT_LABEL_TYPE(x) \
        case AST::stmtType::x: { return AST::down_cast<AST::x##_t>(f)->m_label; }

static inline int64_t stmt_label(AST::stmt_t *f)
{
    switch (f->type) {
        LFORTRAN_STMT_LABEL_TYPE(Allocate)
        LFORTRAN_STMT_LABEL_TYPE(Assign)
        LFORTRAN_STMT_LABEL_TYPE(Assignment)
        LFORTRAN_STMT_LABEL_TYPE(Associate)
        LFORTRAN_STMT_LABEL_TYPE(Backspace)
        LFORTRAN_STMT_LABEL_TYPE(Close)
        LFORTRAN_STMT_LABEL_TYPE(Continue)
        LFORTRAN_STMT_LABEL_TYPE(Cycle)
        LFORTRAN_STMT_LABEL_TYPE(Deallocate)
        LFORTRAN_STMT_LABEL_TYPE(Endfile)
        LFORTRAN_STMT_LABEL_TYPE(Entry)
        LFORTRAN_STMT_LABEL_TYPE(ErrorStop)
        LFORTRAN_STMT_LABEL_TYPE(EventPost)
        LFORTRAN_STMT_LABEL_TYPE(EventWait)
        LFORTRAN_STMT_LABEL_TYPE(Exit)
        LFORTRAN_STMT_LABEL_TYPE(Flush)
        LFORTRAN_STMT_LABEL_TYPE(ForAllSingle)
        LFORTRAN_STMT_LABEL_TYPE(Format)
        LFORTRAN_STMT_LABEL_TYPE(DataStmt)
        LFORTRAN_STMT_LABEL_TYPE(FormTeam)
        LFORTRAN_STMT_LABEL_TYPE(GoTo)
        LFORTRAN_STMT_LABEL_TYPE(Include)
        LFORTRAN_STMT_LABEL_TYPE(Inquire)
        LFORTRAN_STMT_LABEL_TYPE(Nullify)
        LFORTRAN_STMT_LABEL_TYPE(Open)
        LFORTRAN_STMT_LABEL_TYPE(Return)
        LFORTRAN_STMT_LABEL_TYPE(Print)
        LFORTRAN_STMT_LABEL_TYPE(Read)
        LFORTRAN_STMT_LABEL_TYPE(Rewind)
        LFORTRAN_STMT_LABEL_TYPE(Stop)
        LFORTRAN_STMT_LABEL_TYPE(SubroutineCall)
        LFORTRAN_STMT_LABEL_TYPE(SyncAll)
        LFORTRAN_STMT_LABEL_TYPE(SyncImages)
        LFORTRAN_STMT_LABEL_TYPE(SyncMemory)
        LFORTRAN_STMT_LABEL_TYPE(SyncTeam)
        LFORTRAN_STMT_LABEL_TYPE(Write)
        LFORTRAN_STMT_LABEL_TYPE(AssociateBlock)
        LFORTRAN_STMT_LABEL_TYPE(Block)
        LFORTRAN_STMT_LABEL_TYPE(ChangeTeam)
        LFORTRAN_STMT_LABEL_TYPE(Critical)
        LFORTRAN_STMT_LABEL_TYPE(DoConcurrentLoop)
        LFORTRAN_STMT_LABEL_TYPE(DoLoop)
        LFORTRAN_STMT_LABEL_TYPE(ForAll)
        LFORTRAN_STMT_LABEL_TYPE(If)
        LFORTRAN_STMT_LABEL_TYPE(IfArithmetic)
        LFORTRAN_STMT_LABEL_TYPE(Select)
        LFORTRAN_STMT_LABEL_TYPE(SelectRank)
        LFORTRAN_STMT_LABEL_TYPE(SelectType)
        LFORTRAN_STMT_LABEL_TYPE(Where)
        LFORTRAN_STMT_LABEL_TYPE(WhileLoop)
        default : throw LCompilersException("Not implemented");
    }
}


class CommonVisitorMethods {
public:

  inline static void visit_Compare(Allocator &al, const AST::Compare_t &x,
                                   ASR::expr_t *&left, ASR::expr_t *&right,
                                   ASR::asr_t *&asr, std::string& intrinsic_op_name,
                                   SymbolTable* curr_scope,
                                   std::set<std::string>& current_function_dependencies,
                                   Vec<char*>& current_module_dependencies) {
    ASR::cmpopType asr_op;
    switch (x.m_op) {
        case (AST::cmpopType::Eq): {
        asr_op = ASR::cmpopType::Eq;
        break;
        }
        case (AST::cmpopType::Gt): {
        asr_op = ASR::cmpopType::Gt;
        break;
        }
        case (AST::cmpopType::GtE): {
        asr_op = ASR::cmpopType::GtE;
        break;
        }
        case (AST::cmpopType::Lt): {
        asr_op = ASR::cmpopType::Lt;
        break;
        }
        case (AST::cmpopType::LtE): {
        asr_op = ASR::cmpopType::LtE;
        break;
        }
        case (AST::cmpopType::NotEq): {
        asr_op = ASR::cmpopType::NotEq;
        break;
        }
        default: {
        throw SemanticError("Comparison operator not implemented",
                            x.base.base.loc);
        }
    }
    // Cast LHS or RHS if necessary
    ASR::ttype_t *left_type = ASRUtils::expr_type(left);
    ASR::ttype_t *right_type = ASRUtils::expr_type(right);

    ASR::expr_t *overloaded = nullptr;
    if ( ASRUtils::use_overloaded(left, right, asr_op,
        intrinsic_op_name, curr_scope, asr, al,
        x.base.base.loc, current_function_dependencies,
        current_module_dependencies,
        [&](const std::string &msg, const Location &loc) { throw SemanticError(msg, loc); }) ) {
        overloaded = ASRUtils::EXPR(asr);
    }

    ASR::ttype_t *source_type = nullptr;
    ASR::ttype_t *dest_type = nullptr;

    if (((left_type->type != ASR::ttypeType::Real &&
         left_type->type != ASR::ttypeType::Integer) &&
        (right_type->type != ASR::ttypeType::Real &&
         right_type->type != ASR::ttypeType::Integer) &&
        ((left_type->type != ASR::ttypeType::Complex ||
          right_type->type != ASR::ttypeType::Complex) &&
         x.m_op != AST::cmpopType::Eq && x.m_op != AST::cmpopType::NotEq) &&
         (left_type->type != ASR::ttypeType::Character ||
          right_type->type != ASR::ttypeType::Character))
         && overloaded == nullptr) {
      throw SemanticError(
          "Compare: only Integer or Real can be on the LHS and RHS. "
          "If operator is .eq. or .neq. then Complex type is also acceptable",
          x.base.base.loc);
    } else {
      dest_type = right_type;
      source_type = left_type;
      if( overloaded == nullptr ) {
        ASR::expr_t **conversion_cand = &left;
        ImplicitCastRules::find_conversion_candidate(&left, &right, left_type,
                                                    right_type, conversion_cand,
                                                    &source_type, &dest_type);

        ImplicitCastRules::set_converted_value(
            al, x.base.base.loc, conversion_cand, source_type, dest_type);
      }
    }

    if( overloaded == nullptr ) {
        LCOMPILERS_ASSERT(
            ASRUtils::check_equal_type(ASRUtils::expr_type(left),
                                    ASRUtils::expr_type(right)));
    }
    ASR::ttype_t *type = ASRUtils::TYPE(
        ASR::make_Logical_t(al, x.base.base.loc, 4, nullptr, 0));

    ASR::expr_t *value = nullptr;

    if (ASRUtils::is_integer(*dest_type)) {

        if (ASRUtils::expr_value(left) != nullptr && ASRUtils::expr_value(right) != nullptr) {
            int64_t left_value = ASR::down_cast<ASR::IntegerConstant_t>(
                                    ASRUtils::expr_value(left))->m_n;
            int64_t right_value = ASR::down_cast<ASR::IntegerConstant_t>(
                                    ASRUtils::expr_value(right))->m_n;
            bool result;
            switch (asr_op) {
                case (ASR::cmpopType::Eq):  { result = left_value == right_value; break; }
                case (ASR::cmpopType::Gt): { result = left_value > right_value; break; }
                case (ASR::cmpopType::GtE): { result = left_value >= right_value; break; }
                case (ASR::cmpopType::Lt): { result = left_value < right_value; break; }
                case (ASR::cmpopType::LtE): { result = left_value <= right_value; break; }
                case (ASR::cmpopType::NotEq): { result = left_value != right_value; break; }
                default: {
                    throw SemanticError("Comparison operator not implemented",
                                        x.base.base.loc);
                }
            }
            value = ASR::down_cast<ASR::expr_t>(ASR::make_LogicalConstant_t(
                al, x.base.base.loc, result, type));
        }

        asr = ASR::make_IntegerCompare_t(al, x.base.base.loc, left, asr_op, right, type, value);

    } else if (ASRUtils::is_real(*dest_type)) {

        if (ASRUtils::expr_value(left) != nullptr && ASRUtils::expr_value(right) != nullptr) {
            double left_value = ASR::down_cast<ASR::RealConstant_t>(
                                    ASRUtils::expr_value(left))->m_r;
            double right_value = ASR::down_cast<ASR::RealConstant_t>(
                                    ASRUtils::expr_value(right))->m_r;
            bool result;
            switch (asr_op) {
                case (ASR::cmpopType::Eq):  { result = left_value == right_value; break; }
                case (ASR::cmpopType::Gt): { result = left_value > right_value; break; }
                case (ASR::cmpopType::GtE): { result = left_value >= right_value; break; }
                case (ASR::cmpopType::Lt): { result = left_value < right_value; break; }
                case (ASR::cmpopType::LtE): { result = left_value <= right_value; break; }
                case (ASR::cmpopType::NotEq): { result = left_value != right_value; break; }
                default: {
                    throw SemanticError("Comparison operator not implemented",
                                        x.base.base.loc);
                }
            }
            value = ASR::down_cast<ASR::expr_t>(ASR::make_LogicalConstant_t(
                al, x.base.base.loc, result, type));
        }

        asr = ASR::make_RealCompare_t(al, x.base.base.loc, left, asr_op, right, type, value);

    } else if (ASRUtils::is_complex(*dest_type)) {

        if (ASRUtils::expr_value(left) != nullptr && ASRUtils::expr_value(right) != nullptr) {
            ASR::ComplexConstant_t *left0
                = ASR::down_cast<ASR::ComplexConstant_t>(ASRUtils::expr_value(left));
            ASR::ComplexConstant_t *right0
                = ASR::down_cast<ASR::ComplexConstant_t>(ASRUtils::expr_value(right));
            std::complex<double> left_value(left0->m_re, left0->m_im);
            std::complex<double> right_value(right0->m_re, right0->m_im);
            bool result;
            switch (asr_op) {
                case (ASR::cmpopType::Eq) : {
                    result = left_value.real() == right_value.real() &&
                            left_value.imag() == right_value.imag();
                    break;
                }
                case (ASR::cmpopType::NotEq) : {
                    result = left_value.real() != right_value.real() ||
                            left_value.imag() != right_value.imag();
                    break;
                }
                default: {
                    throw SemanticError("'" + ASRUtils::cmpop_to_str(asr_op) +
                                        "' comparison is not supported between complex numbers",
                                        x.base.base.loc);
                }
            }
            value = ASR::down_cast<ASR::expr_t>(ASR::make_LogicalConstant_t(
                al, x.base.base.loc, result, type));
        }

        asr = ASR::make_ComplexCompare_t(al, x.base.base.loc, left, asr_op, right, type, value);

    } else if (ASRUtils::is_logical(*dest_type)) {

        if (ASRUtils::expr_value(left) != nullptr && ASRUtils::expr_value(right) != nullptr) {
            bool left_value = ASR::down_cast<ASR::LogicalConstant_t>(
                                    ASRUtils::expr_value(left))->m_value;
            bool right_value = ASR::down_cast<ASR::LogicalConstant_t>(
                                    ASRUtils::expr_value(right))->m_value;
            bool result;
            switch (asr_op) {
                case (ASR::cmpopType::Eq):  { result = left_value == right_value; break; }
                case (ASR::cmpopType::Gt): { result = left_value > right_value; break; }
                case (ASR::cmpopType::GtE): { result = left_value >= right_value; break; }
                case (ASR::cmpopType::Lt): { result = left_value < right_value; break; }
                case (ASR::cmpopType::LtE): { result = left_value <= right_value; break; }
                case (ASR::cmpopType::NotEq): { result = left_value != right_value; break; }
                default: {
                    throw SemanticError("Comparison operator not implemented",
                                        x.base.base.loc);
                }
            }
            value = ASR::down_cast<ASR::expr_t>(ASR::make_LogicalConstant_t(
                al, x.base.base.loc, result, type));
        }

        asr = ASR::make_LogicalCompare_t(al, x.base.base.loc, left, asr_op, right, type, value);

    } else if (ASRUtils::is_character(*dest_type)) {

        if (ASRUtils::expr_value(left) != nullptr && ASRUtils::expr_value(right) != nullptr) {
            char* left_value = ASR::down_cast<ASR::StringConstant_t>(
                                    ASRUtils::expr_value(left))->m_s;
            char* right_value = ASR::down_cast<ASR::StringConstant_t>(
                                    ASRUtils::expr_value(right))->m_s;
            std::string left_str = std::string(left_value);
            std::string right_str = std::string(right_value);
            int8_t strcmp = left_str.compare(right_str);
            bool result;
            switch (asr_op) {
                case (ASR::cmpopType::Eq) : {
                    result = (strcmp == 0);
                    break;
                }
                case (ASR::cmpopType::NotEq) : {
                    result = (strcmp != 0);
                    break;
                }
                case (ASR::cmpopType::Gt) : {
                    result = (strcmp > 0);
                    break;
                }
                case (ASR::cmpopType::GtE) : {
                    result = (strcmp > 0 || strcmp == 0);
                    break;
                }
                case (ASR::cmpopType::Lt) : {
                    result = (strcmp < 0);
                    break;
                }
                case (ASR::cmpopType::LtE) : {
                    result = (strcmp < 0 || strcmp == 0);
                    break;
                }
                default: LCOMPILERS_ASSERT(false); // should never happen
            }
            value = ASR::down_cast<ASR::expr_t>(ASR::make_LogicalConstant_t(
                al, x.base.base.loc, result, type));
        }

        asr = ASR::make_StringCompare_t(al, x.base.base.loc, left, asr_op, right, type, value);
    }
    if (overloaded != nullptr) {
        asr = ASR::make_OverloadedCompare_t(al, x.base.base.loc, left, asr_op, right, type,
            value, overloaded);
    }
  }

  inline static void visit_BoolOp(Allocator &al, const AST::BoolOp_t &x,
                                  ASR::expr_t *&left, ASR::expr_t *&right,
                                  ASR::asr_t *&asr, diag::Diagnostics &diag) {
    ASR::logicalbinopType op;
    switch (x.m_op) {
    case (AST::And):
      op = ASR::And;
      break;
    case (AST::Or):
      op = ASR::Or;
      break;
    case (AST::Xor):
      op = ASR::Xor;
        diag.semantic_warning_label(
            ".xor. is an LFortran extension",
            {x.base.base.loc},
            "LFortran extension"
        );
      break;
    case (AST::NEqv):
      op = ASR::NEqv;
      break;
    case (AST::Eqv):
      op = ASR::Eqv;
      break;
    default:
      throw SemanticError(R"""(Only .and., .or., .xor., .neqv., .eqv.
                                    implemented for logical type operands.)""",
                          x.base.base.loc);
    }

    // Cast LHS or RHS if necessary
    ASR::ttype_t *left_type = ASRUtils::expr_type(left);
    ASR::ttype_t *right_type = ASRUtils::expr_type(right);
    ASR::expr_t **conversion_cand = &left;
    ASR::ttype_t *source_type = left_type;
    ASR::ttype_t *dest_type = right_type;

    ImplicitCastRules::find_conversion_candidate(&left, &right, left_type,
                                                 right_type, conversion_cand,
                                                 &source_type, &dest_type);
    ImplicitCastRules::set_converted_value(al, x.base.base.loc, conversion_cand,
                                           source_type, dest_type);

    LCOMPILERS_ASSERT(
        ASRUtils::check_equal_type(ASRUtils::expr_type(left),
                                   ASRUtils::expr_type(right)));

    ASR::expr_t *value = nullptr;
    // Assign evaluation to `value` if possible, otherwise leave nullptr
    if (ASRUtils::expr_value(left) != nullptr &&
        ASRUtils::expr_value(right) != nullptr) {
        LCOMPILERS_ASSERT(ASR::is_a<ASR::Logical_t>(*dest_type))

        bool left_value = ASR::down_cast<ASR::LogicalConstant_t>(
                                 ASRUtils::expr_value(left))
                                 ->m_value;
        bool right_value = ASR::down_cast<ASR::LogicalConstant_t>(
                                  ASRUtils::expr_value(right))
                                  ->m_value;
        bool result;
        switch (op) {
            case (ASR::And):
                result = left_value && right_value;
                break;
            case (ASR::Or):
                result = left_value || right_value;
                break;
            case (ASR::NEqv):
                result = left_value != right_value;
                break;
            case (ASR::Eqv):
                result = left_value == right_value;
                break;
            default:
                throw SemanticError(R"""(Only .and., .or., .neqv., .eqv.
                                                implemented for logical type operands.)""",
                                    x.base.base.loc);
        }
        value = ASR::down_cast<ASR::expr_t>(ASR::make_LogicalConstant_t(
            al, x.base.base.loc, result, dest_type));
    }

    asr = ASR::make_LogicalBinOp_t(al, x.base.base.loc, left, op, right, dest_type, value);

  }

  inline static void visit_UnaryOp(Allocator &al, const AST::UnaryOp_t &x,
                                   ASR::expr_t *&operand, ASR::asr_t *&asr) {

    ASR::ttype_t *operand_type = ASRUtils::expr_type(operand);
    ASR::expr_t *value = nullptr;
    if (x.m_op == AST::unaryopType::UAdd) {

        if (ASRUtils::is_integer(*operand_type)) {
            if (ASRUtils::expr_value(operand) != nullptr) {
                int64_t op_value = ASR::down_cast<ASR::IntegerConstant_t>(
                                        ASRUtils::expr_value(operand))->m_n;
                asr = ASR::make_IntegerConstant_t(al, x.base.base.loc, op_value, operand_type);
            }
        }
        else if (ASRUtils::is_real(*operand_type)) {
            if (ASRUtils::expr_value(operand) != nullptr) {
                double op_value = ASR::down_cast<ASR::RealConstant_t>(
                                ASRUtils::expr_value(operand))->m_r;
                asr = ASR::make_RealConstant_t(al, x.base.base.loc, op_value, operand_type);
            }
        }
        return;

    } else if (x.m_op == AST::unaryopType::USub) {

        if (ASRUtils::is_integer(*operand_type)) {
            if (ASRUtils::expr_value(operand) != nullptr) {
                int64_t op_value = ASR::down_cast<ASR::IntegerConstant_t>(
                                        ASRUtils::expr_value(operand))->m_n;
                value = ASR::down_cast<ASR::expr_t>(
                    ASR::make_IntegerConstant_t(al, x.base.base.loc, -op_value, operand_type));
            }
            asr = ASR::make_IntegerUnaryMinus_t(al, x.base.base.loc, operand,
                                                    operand_type, value);
            return;
        }
        else if (ASRUtils::is_real(*operand_type)) {
            if (ASRUtils::expr_value(operand) != nullptr) {
                double op_value = ASR::down_cast<ASR::RealConstant_t>(
                                        ASRUtils::expr_value(operand))->m_r;
                value = ASR::down_cast<ASR::expr_t>(ASR::make_RealConstant_t(
                    al, x.base.base.loc, -op_value, operand_type));
            }
            asr = ASR::make_RealUnaryMinus_t(al, x.base.base.loc, operand,
                                             operand_type, value);
            return;
        }
        else if (ASRUtils::is_complex(*operand_type)) {
            if (ASRUtils::expr_value(operand) != nullptr) {
                ASR::ComplexConstant_t *c = ASR::down_cast<ASR::ComplexConstant_t>(
                                    ASRUtils::expr_value(operand));
                std::complex<double> op_value(c->m_re, c->m_im);
                std::complex<double> result;
                result = -op_value;
                value = ASR::down_cast<ASR::expr_t>(
                        ASR::make_ComplexConstant_t(al, x.base.base.loc, std::real(result),
                        std::imag(result), operand_type));
            }
            asr = ASR::make_ComplexUnaryMinus_t(al, x.base.base.loc, operand,
                                                    operand_type, value);
            return;
        }

    } else if (x.m_op == AST::unaryopType::Invert) {

        if (ASRUtils::is_integer(*operand_type)) {
            if (ASRUtils::expr_value(operand) != nullptr) {
                int64_t op_value = ASR::down_cast<ASR::IntegerConstant_t>(
                                        ASRUtils::expr_value(operand))->m_n;
                value = ASR::down_cast<ASR::expr_t>(
                    ASR::make_IntegerConstant_t(al, x.base.base.loc, ~op_value, operand_type));
            }
            asr = ASR::make_IntegerBitNot_t(al, x.base.base.loc, operand, operand_type, value);
            return;
        }
        else {
            throw SemanticError("Argument of `not` intrinsic must be INTEGER", x.base.base.loc);
        }

    } else if (x.m_op == AST::unaryopType::Not) {

        if (ASRUtils::is_logical(*operand_type)) {
            if (ASRUtils::expr_value(operand) != nullptr) {
                bool op_value = ASR::down_cast<ASR::LogicalConstant_t>(
                                ASRUtils::expr_value(operand))->m_value;
                value = ASR::down_cast<ASR::expr_t>(ASR::make_LogicalConstant_t(
                    al, x.base.base.loc, !op_value, operand_type));
            }
            asr = ASR::make_LogicalNot_t(al, x.base.base.loc, operand, operand_type, value);
            return;
        }
        else {
            throw SemanticError("Operand of .not. operator is "+
                std::string(ASRUtils::type_to_str(operand_type)), x.base.base.loc);
        }

    }
  }

  static inline void visit_StrOp(Allocator &al, const AST::StrOp_t &x,
                                 ASR::expr_t *&left, ASR::expr_t *&right,
                                 ASR::asr_t *&asr) {
    LCOMPILERS_ASSERT(x.m_op == AST::Concat)
    ASR::ttype_t *left_type = ASRUtils::expr_type(left);
    ASR::ttype_t *right_type = ASRUtils::expr_type(right);
    LCOMPILERS_ASSERT(ASR::is_a<ASR::Character_t>(*left_type))
    LCOMPILERS_ASSERT(ASR::is_a<ASR::Character_t>(*right_type))
    ASR::Character_t *left_type2 = ASR::down_cast<ASR::Character_t>(left_type);
    ASR::Character_t *right_type2 = ASR::down_cast<ASR::Character_t>(right_type);
    LCOMPILERS_ASSERT(left_type2->n_dims == 0);
    LCOMPILERS_ASSERT(right_type2->n_dims == 0);
    ASR::ttype_t *dest_type = ASR::down_cast<ASR::ttype_t>(ASR::make_Character_t(al, x.base.base.loc, left_type2->m_kind,
        left_type2->m_len + right_type2->m_len, nullptr, nullptr, 0));

    ASR::expr_t *value = nullptr;
    // Assign evaluation to `value` if possible, otherwise leave nullptr
    ASR::expr_t* left_value = ASRUtils::expr_value(left);
    ASR::expr_t* right_value = ASRUtils::expr_value(right);
    if (left_value != nullptr && right_value != nullptr) {
        ASR::ttype_t* left_value_type = ASRUtils::expr_type(left_value);
        ASR::Character_t* left_value_type2 = ASR::down_cast<ASR::Character_t>(left_value_type);
        char* left_value_ = ASR::down_cast<ASR::StringConstant_t>(left_value)->m_s;
        char* right_value_ = ASR::down_cast<ASR::StringConstant_t>(right_value)->m_s;
        ASR::ttype_t *dest_value_type = ASR::down_cast<ASR::ttype_t>(ASR::make_Character_t(al, x.base.base.loc,
            left_value_type2->m_kind, strlen(left_value_) + strlen(right_value_), nullptr, nullptr, 0));
        char* result;
        std::string result_s = std::string(left_value_) + std::string(right_value_);
        Str s; s.from_str_view(result_s);
        result = s.c_str(al);
        LCOMPILERS_ASSERT((int64_t)strlen(result) == ASR::down_cast<ASR::Character_t>(dest_value_type)->m_len)
        value = ASR::down_cast<ASR::expr_t>(ASR::make_StringConstant_t(
            al, x.base.base.loc, result, dest_value_type));
      }
    asr = ASR::make_StringConcat_t(al, x.base.base.loc, left, right, dest_type,
                            value);
  }

static ASR::asr_t* comptime_intrinsic_real(ASR::expr_t *A,
        ASR::expr_t * kind,
        Allocator &al, const Location &loc) {
    int kind_int = 4;
    if (kind) {
        ASR::expr_t* kind_value = ASRUtils::expr_value(kind);
        if (kind_value) {
            if (ASR::is_a<ASR::IntegerConstant_t>(*kind_value)) {
                kind_int = ASR::down_cast<ASR::IntegerConstant_t>(kind_value)->m_n;
            } else {
                throw SemanticError("kind argument to real(a, kind) is not a constant integer", loc);
            }
        } else {
            throw SemanticError("kind argument to real(a, kind) is not constant", loc);
        }
    }
    ASR::expr_t *result = A;
    ASR::ttype_t *dest_type = ASRUtils::TYPE(ASR::make_Real_t(al, loc,
                kind_int, nullptr, 0));
    ASR::ttype_t *source_type = ASRUtils::expr_type(A);

    // TODO: this is implicit cast, use ExplicitCast
    ImplicitCastRules::set_converted_value(al, loc, &result,
                                           source_type, dest_type);
    return (ASR::asr_t*)result;
}

static ASR::asr_t* comptime_intrinsic_int(ASR::expr_t *A,
        ASR::expr_t * kind,
        Allocator &al, const Location &loc) {
    int kind_int = 4;
    if (kind) {
        ASR::expr_t* kind_value = ASRUtils::expr_value(kind);
        if (kind_value) {
            if (ASR::is_a<ASR::IntegerConstant_t>(*kind_value)) {
                kind_int = ASR::down_cast<ASR::IntegerConstant_t>(kind_value)->m_n;
            } else {
                throw SemanticError("kind argument to int(a, kind) is not a constant integer", loc);
            }
        } else {
            throw SemanticError("kind argument to int(a, kind) is not constant", loc);
        }
    }
    ASR::expr_t *result = A;
    ASR::ttype_t *dest_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc,
                kind_int, nullptr, 0));
    ASR::ttype_t *source_type = ASRUtils::expr_type(A);

    // TODO: this is implicit cast, use ExplicitCast
    ImplicitCastRules::set_converted_value(al, loc, &result,
                                           source_type, dest_type);
    return (ASR::asr_t*)result;
}

}; // class CommonVisitorMethods


struct TypeMissingData {
    SymbolTable* scope;
    std::string sym_name;
    AST::expr_t* expr;
    int64_t sym_type;
    ASR::ttype_t* type;
};

template <class Derived>
class CommonVisitor : public AST::BaseVisitor<Derived> {
public:
    diag::Diagnostics &diag;
    std::map<AST::operatorType, std::string> binop2str = {
        {AST::operatorType::Mul, "~mul"},
        {AST::operatorType::Add, "~add"},
    };

    std::map<AST::cmpopType, std::string> cmpop2str = {
        {AST::cmpopType::Eq, "~eq"},
        {AST::cmpopType::NotEq, "~noteq"},
        {AST::cmpopType::Lt, "~lt"},
        {AST::cmpopType::LtE, "~lte"},
        {AST::cmpopType::Gt, "~gt"},
        {AST::cmpopType::GtE, "~gte"}
    };


    ASR::asr_t *tmp;
    Allocator &al;
    CompilerOptions &compiler_options;
    SymbolTable *current_scope;
    ASR::Module_t *current_module = nullptr;
    Vec<char *> current_module_dependencies;
    IntrinsicProcedures intrinsic_procedures;
    IntrinsicProceduresAsASRNodes intrinsic_procedures_as_asr_nodes;
    std::set<std::string> intrinsic_module_procedures_as_asr_nodes = {
        "c_loc", "c_f_pointer"
    };

    ASR::accessType dflt_access = ASR::Public;
    bool in_module = false;
    std::map<SymbolTable*, std::map<AST::decl_attribute_t*, AST::simple_attributeType>> overloaded_ops;
    std::map<SymbolTable*, ASR::accessType> assgn;
    std::map<std::string, ASR::accessType> assgnd_access;
    ASR::presenceType dflt_presence = ASR::presenceType::Required;
    std::map<std::string, ASR::presenceType> assgnd_presence;
    // Current procedure arguments. Only non-empty for SymbolTableVisitor,
    // empty for BodyVisitor.
    std::vector<std::string> current_procedure_args;
    std::vector<std::string> excluded_from_symtab;
    int64_t current_symbol;
    std::vector<TypeMissingData*> type_info;
    ASR::abiType current_procedure_abi_type = ASR::abiType::Source;
    bool is_derived_type = false;
    bool is_body_visitor = false;
    bool is_requirement = false;
    bool is_template = false;
    bool is_instantiate = false;
    bool is_current_procedure_templated = false;
    Vec<ASR::stmt_t*> *current_body = nullptr;

    // fields for generics
    std::vector<ASR::asr_t*> current_requirement_type_parameters;
    std::vector<ASR::asr_t*> current_requirement_functions;
    std::map<std::string, std::map<std::string, ASR::asr_t*>> requirement_map;
    std::map<std::string, ASR::asr_t*> called_requirement;
    std::map<std::string, std::map<std::string, ASR::asr_t*>> template_asr_map;
    std::map<std::string, std::map<int, std::string>> template_arg_map;
    std::vector<ASR::symbol_t*> rt_vec;

    std::map<std::string, ASR::ttype_t*> implicit_dictionary;
    std::map<uint64_t, std::map<std::string, ASR::ttype_t*>> &implicit_mapping;

    Vec<char*> data_member_names;
    std::set<std::string> current_function_dependencies;
    ASR::ttype_t* current_variable_type_;

    int32_t enum_init_val; // TODO: rework this to not use global variable

    CommonVisitor(Allocator &al, SymbolTable *symbol_table,
            diag::Diagnostics &diagnostics, CompilerOptions &compiler_options,
            std::map<uint64_t, std::map<std::string, ASR::ttype_t*>> &implicit_mapping)
        : diag{diagnostics}, al{al}, compiler_options{compiler_options},
          current_scope{symbol_table}, implicit_mapping{implicit_mapping},
          current_variable_type_{nullptr} {
        current_module_dependencies.reserve(al, 4);
        enum_init_val = 0;
    }

    ASR::symbol_t* resolve_symbol(const Location &loc, const std::string &sub_name) {
        SymbolTable *scope = current_scope;
        ASR::symbol_t *sub = scope->resolve_symbol(sub_name);
        if (!sub) {
            throw SemanticError("Symbol '" + sub_name + "' not declared", loc);
        }
        return sub;
    }

    ASR::symbol_t* declare_implicit_variable(const Location &loc,
            const std::string &var_name, ASR::intentType intent) {
        ASR::ttype_t *type;
        char first_letter = var_name[0];
        // The default implicit typing is:
        // implicit real (a-h,o-z)
        if (first_letter >= 'i' && first_letter <= 'n') {
            // it is an integer
            type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc,
                4, nullptr, 0));
        } else {
            // it is a real
            type = ASRUtils::TYPE(ASR::make_Real_t(al, loc,
                4, nullptr, 0));
        }
        Vec<char*> variable_dependencies_vec;
        variable_dependencies_vec.reserve(al, 1);
        ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, type);
        ASR::symbol_t *v = ASR::down_cast<ASR::symbol_t>(ASR::make_Variable_t(al, loc,
            current_scope, s2c(al, var_name), variable_dependencies_vec.p,
            variable_dependencies_vec.size(), intent, nullptr, nullptr,
            ASR::storage_typeType::Default, type,
            current_procedure_abi_type, ASR::Public,
            ASR::presenceType::Required, false));
        current_scope->add_symbol(var_name, v);
        return v;
    }

    ASR::symbol_t* declare_implicit_variable2(const Location &loc,
            const std::string &var_name, ASR::intentType intent,
            ASR::ttype_t *type) {
        Vec<char*> variable_dependencies_vec;
        variable_dependencies_vec.reserve(al, 1);
        ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, type);
        ASR::symbol_t *v = ASR::down_cast<ASR::symbol_t>(ASR::make_Variable_t(al, loc,
            current_scope, s2c(al, var_name), variable_dependencies_vec.p,
            variable_dependencies_vec.size(), intent, nullptr, nullptr,
            ASR::storage_typeType::Default, type,
            current_procedure_abi_type, ASR::Public,
            ASR::presenceType::Required, false));
        current_scope->add_symbol(var_name, v);
        return v;
    }


    ASR::asr_t* resolve_variable(const Location &loc, const std::string &var_name) {
        SymbolTable *scope = current_scope;
        ASR::symbol_t *v = scope->resolve_symbol(var_name);
        if (compiler_options.implicit_typing) {
            if (implicit_dictionary.find(std::string(1,var_name[0])) == implicit_dictionary.end()) {
                implicit_dictionary = implicit_mapping[get_hash(current_scope->asr_owner)];
            }
        }

        // Check variable in enum scope, if enum is defined
        if (v == nullptr && scope->resolve_symbol("enum") != nullptr) {
            ASR::symbol_t *enum_s = scope->resolve_symbol("enum");
            ASR::EnumType_t *enum_ = ASR::down_cast<ASR::EnumType_t>(
                ASRUtils::symbol_get_past_external(enum_s));
            v = ASRUtils::import_enum_member(al, enum_->m_symtab->get_symbol(var_name), scope);
        }
        if (!v) {
            if (compiler_options.implicit_typing) {
                std::string first_letter = std::string(1,var_name[0]);
                if (implicit_dictionary.find(first_letter) != implicit_dictionary.end()) {
                    ASR::ttype_t *t = implicit_dictionary[first_letter];
                    if (t == nullptr) {
                        diag.semantic_error_label("Variable '" + var_name
                            + "' is not declared", {loc},
                            "'" + var_name + "' is undeclared");
                        throw SemanticAbort();
                    }
                    ASR::intentType intent;
                    if (std::find(current_procedure_args.begin(),
                            current_procedure_args.end(), var_name) !=
                            current_procedure_args.end()) {
                        intent = ASRUtils::intent_unspecified;
                    } else {
                        intent = ASRUtils::intent_local;
                    }
                    v = declare_implicit_variable2(loc, var_name, intent, t);
                } else {
                    ASR::intentType intent;
                    if (std::find(current_procedure_args.begin(),
                            current_procedure_args.end(), var_name) !=
                            current_procedure_args.end()) {
                        intent = ASRUtils::intent_unspecified;
                    } else {
                        intent = ASRUtils::intent_local;
                    }
                    v = declare_implicit_variable(loc, var_name, intent);
                }
            } else {
                // DONE: fix this ad-hoc solution ==> remove this solution
                /*
                if (is_instantiate) {
                    ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Character_t(al, loc,
                            1, strlen(s2c(al, var_name)), nullptr, nullptr, 0));
                    return ASR::make_StringConstant_t(al, loc, s2c(al, var_name), type);
                }
                */
                diag.semantic_error_label("Variable '" + var_name
                    + "' is not declared", {loc},
                    "'" + var_name + "' is undeclared");
                throw SemanticAbort();
            }
        }
        return ASR::make_Var_t(al, loc, v);
    }

    void process_dims(Allocator &al, Vec<ASR::dimension_t> &dims,
        AST::dimension_t *m_dim, size_t n_dim, bool &is_compile_time) {
        LCOMPILERS_ASSERT(dims.size() == 0);
        is_compile_time = false;
        dims.reserve(al, n_dim);
        for (size_t i=0; i<n_dim; i++) {
            ASR::dimension_t dim;
            dim.loc = m_dim[i].loc;
            if (m_dim[i].m_start) {
                this->visit_expr(*m_dim[i].m_start);
                dim.m_start = ASRUtils::EXPR(tmp);
            } else {
                dim.m_start = nullptr;
            }
            if (m_dim[i].m_end) {
                this->visit_expr(*m_dim[i].m_end);
                dim.m_length = ASRUtils::compute_length_from_start_end(al, dim.m_start,
                                    ASRUtils::EXPR(tmp));
            } else {
                dim.m_length = nullptr;
            }
            if ( !dim.m_start && !dim.m_length ) {
                is_compile_time = true;
            }
            dims.push_back(al, dim);
        }
    }

    ASR::accessType get_asr_simple_attr(AST::simple_attributeType simple_attr) {
        ASR::accessType access_type = ASR::accessType::Public;
        switch( simple_attr ) {
            case AST::simple_attributeType::AttrPublic: {
                access_type = ASR::accessType::Public;
                break;
            }
            case AST::simple_attributeType::AttrPrivate: {
                access_type = ASR::accessType::Private;
                break;
            }
            default:
                LCOMPILERS_ASSERT(false);
        }
        return access_type;
    }

    void visit_Format(const AST::Format_t &x) {
        diag.semantic_warning_label(
            "Format statement is not implemented yet, for now we will ignore it",
            {x.base.base.loc},
            "ignored for now"
        );
        tmp = nullptr;
    }

    void visit_Include(const AST::Include_t &x) {
        diag.semantic_error_label(
            "Include statement is not implemented at the AST level yet. You have to run LFortran with prescanning which can handle include statements.",
            {x.base.base.loc},
            "Enable prescanner to handle this"
        );
        tmp = nullptr;
    }

    void visit_DataStmt(const AST::DataStmt_t &x) {
        // The DataStmt is a statement, so it occurs in the BodyVisitor.
        // We add its contents into the symbol table here. This visitor
        // could probably be in either the CommonVisitor or the BodyVisitor.

        // This outer loop is iterating over sections in the data statement,
        // for example in the following we have three items:
        //   data x / 1.0, 2.0 /, a, b / 1.0, 2.0 /, c / 1.0 /
        for (size_t i=0; i < x.n_items; i++) {
            AST::DataStmtSet_t *a = AST::down_cast<AST::DataStmtSet_t>(x.m_items[i]);
            // Now we are dealing with just one item, there are three cases possible:
            // data x / 1, 2, 3 /       ! x must be an array
            // data x / 1 /             ! x must be a scalar (integer)
            // data x, y, z / 1, 2, 3 / ! x, y, z must be a scalar (integer)
            if (a->n_object != a->n_value) {
                // This is the first case:
                // data x / 1, 2, 3 /       ! x must be an array
                if (a->n_object == 1) {
                    this->visit_expr(*a->m_object[0]);
                    ASR::expr_t* object = ASRUtils::EXPR(tmp);
                    ASR::ttype_t* obj_type = ASRUtils::expr_type(object);
                    if (ASRUtils::is_array(obj_type)) { // it is an array
                        Vec<ASR::expr_t*> body;
                        body.reserve(al, a->n_value);
                        for (size_t j=0; j < a->n_value; j++) {
                            this->visit_expr(*a->m_value[j]);
                            ASR::expr_t* value = ASRUtils::EXPR(tmp);
                            if (ASRUtils::expr_type(value)->type != obj_type->type) {
                                throw SemanticError("Type mismatch during data initialization",
                                    x.base.base.loc);
                            }
                            ASR::expr_t* expression_value = ASRUtils::expr_value(value);
                            if (expression_value) {
                                body.push_back(al, expression_value);
                            } else {
                                throw SemanticError("The value in data must be a constant",
                                    x.base.base.loc);
                            }

                        }
                        Vec<ASR::dimension_t> dims;
                        dims.reserve(al, 1);
                        ASR::dimension_t dim;
                        dim.loc = x.base.base.loc;
                        ASR::ttype_t *int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc,
                                                                                        4, nullptr, 0));
                        ASR::expr_t* one = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, 1, int32_type));
                        ASR::expr_t* x_n_args = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc,
                                            a->n_value, int32_type));
                        dim.m_start = one;
                        dim.m_length = x_n_args;
                        dims.push_back(al, dim);
                        obj_type = ASRUtils::duplicate_type(al, obj_type, &dims);
                        tmp = ASR::make_ArrayConstant_t(al, x.base.base.loc, body.p,
                            body.size(), obj_type, ASR::arraystorageType::ColMajor);
                        ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(object);
                        ASR::Variable_t *v2 = ASR::down_cast<ASR::Variable_t>(v->m_v);
                        v2->m_value = ASRUtils::EXPR(tmp);
                        v2->m_symbolic_value = ASRUtils::EXPR(tmp);
                        Vec<char*> var_deps_vec;
                        var_deps_vec.reserve(al, 1);
                        ASRUtils::collect_variable_dependencies(al, var_deps_vec, v2->m_type,
                            v2->m_symbolic_value, v2->m_value);
                        v2->m_dependencies = var_deps_vec.p;
                        v2->n_dependencies = var_deps_vec.size();

                    } else {
                        throw SemanticError("There is one variable and multiple values, but the variable is not an array",
                            x.base.base.loc);
                    }
                } else {
                    throw SemanticError("The number of values and variables do not match, and there is more than one variable",
                        x.base.base.loc);
                }
            } else {
                // This is the second and third case:
                // data x / 1 /             ! x must be a scalar (integer)
                // data x, y, z / 1, 2, 3 / ! x, y, z must be a scalar (integer)

                // Note: this also happens for a case like:
                // data x(1), x(2), x(3) / 1, 2, 3 /
                for (size_t i=0;i<a->n_object;++i) {
                    // Here we are now dealing with just one variable (object)
                    // and the corresponding value at a time, such as:
                    // y / 2 /
                    // or
                    // x(2) / 2 /
                    //
                    this->visit_expr(*a->m_object[i]);
                    ASR::expr_t* object = ASRUtils::EXPR(tmp);
                    this->visit_expr(*a->m_value[i]);
                    ASR::expr_t* value = ASRUtils::EXPR(tmp);
                    // The parser ensures object is a TK_NAME
                    // The `visit_expr` ensures it resolves as an expression
                    // which must be a `Var_t` pointing to a `Variable_t`,
                    // so no checks are needed:
                    ImplicitCastRules::set_converted_value(al, x.base.base.loc, &value,
                                            ASRUtils::expr_type(value), ASRUtils::expr_type(object));
                    ASR::expr_t* expression_value = ASRUtils::expr_value(value);
                    if (!expression_value) {
                        throw SemanticError("The value in data must be a constant",
                            x.base.base.loc);
                    }
                    if (ASR::is_a<ASR::Var_t>(*object)) {
                        // This is the following case:
                        // y / 2 /
                        ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(object);
                        ASR::Variable_t *v2 = ASR::down_cast<ASR::Variable_t>(v->m_v);
                        v2->m_value = expression_value;
                        Vec<char*> var_deps_vec;
                        var_deps_vec.reserve(al, 1);
                        ASRUtils::collect_variable_dependencies(al, var_deps_vec, v2->m_type,
                            v2->m_symbolic_value, v2->m_value);
                        v2->m_dependencies = var_deps_vec.p;
                        v2->n_dependencies = var_deps_vec.size();
                        ASR::stmt_t* assign_stmt = ASRUtils::STMT(ASR::make_Assignment_t(al,
                                    object->base.loc, object, expression_value, nullptr));
                        LCOMPILERS_ASSERT(current_body != nullptr)
                        current_body->push_back(al, assign_stmt);
                    } else if (ASR::is_a<ASR::ArrayItem_t>(*object)) {
                        // This is the following case:
                        // x(2) / 2 /
                        // We create an assignment node and insert into the current body.
                        // i.e., x(2) = 2.
                        // Note: this will only work if the data statement is
                        // above the place where it is being used, otherwise it
                        // won't work correctly
                        // To fix that, we would have to iterate over data statements first
                        // but we can fix that later.
                        ASR::stmt_t* assign_stmt = ASRUtils::STMT(ASR::make_Assignment_t(al,
                                    object->base.loc, object, expression_value, nullptr));
                        LCOMPILERS_ASSERT(current_body != nullptr)
                        current_body->push_back(al, assign_stmt);
                    } else {
                        throw SemanticError("The variable (object) type is not supported (only variables and array items are supported so far)",
                            x.base.base.loc);
                    }
                }
            }
        }
        tmp = nullptr;
    }

    void visit_DeclarationUtil(const AST::Declaration_t &x) {
        if (x.m_vartype == nullptr &&
                x.n_attributes == 1 &&
                AST::is_a<AST::AttrNamelist_t>(*x.m_attributes[0])) {
            //char *name = down_cast<AttrNamelist_t>(x.m_attributes[0])->m_name;
            throw SemanticError("Namelists not implemented yet", x.base.base.loc);
        }
        for (size_t i=0; i<x.n_attributes; i++) {
            if (AST::is_a<AST::AttrType_t>(*x.m_attributes[i])) {
                throw SemanticError("Type must be declared first",
                    x.base.base.loc);
            };
        }
        if (x.m_vartype == nullptr) {
            // Examples:
            // private
            // public
            // private :: x, y, z
            if (x.n_attributes == 0) {
                throw SemanticError("No attribute specified",
                    x.base.base.loc);
            }
            for (size_t i=0; i<x.n_attributes; i++) {
                if (AST::is_a<AST::SimpleAttribute_t>(*x.m_attributes[i])) {
                    AST::SimpleAttribute_t *sa =
                        AST::down_cast<AST::SimpleAttribute_t>(x.m_attributes[i]);
                    if (x.n_syms == 0) {
                        // Example:
                        // private
                        if (sa->m_attr == AST::simple_attributeType
                                ::AttrPrivate) {
                            dflt_access = ASR::accessType::Private;
                        } else if (sa->m_attr == AST::simple_attributeType
                                ::AttrPublic) {
                            // Do nothing (public access is the default)
                            LCOMPILERS_ASSERT(dflt_access == ASR::accessType::Public);
                        } else if (sa->m_attr == AST::simple_attributeType
                                ::AttrSave) {
                            if (in_module) {
                                // Do nothing (all variables implicitly have the
                                // save attribute in a module/main program)
                            } else {
                                throw SemanticError("Save Attribute not "
                                        "supported yet", x.base.base.loc);
                            }
                        } else if (sa->m_attr == AST::simple_attributeType
                                ::AttrSequence) {
                            // TODO: Implement it for CPP backend
                        } else {
                            throw SemanticError("Attribute declaration not "
                                    "supported yet", x.base.base.loc);
                        }
                    } else {
                        // Example:
                        // private :: x, y, z
                        for (size_t i=0; i<x.n_syms; i++) {
                            AST::var_sym_t &s = x.m_syms[i];
                            if (s.m_name == nullptr) {
                                if (s.m_spec->type == AST::decl_attributeType::AttrIntrinsicOperator) {
                                    // Operator Overloading Encountered
                                    if( sa->m_attr != AST::simple_attributeType::AttrPublic &&
                                        sa->m_attr != AST::simple_attributeType::AttrPrivate ) {
                                        overloaded_ops[current_scope][s.m_spec] = AST::simple_attributeType::AttrPublic;
                                    } else {
                                        overloaded_ops[current_scope][s.m_spec] = sa->m_attr;
                                    }
                                } else if( s.m_spec->type == AST::decl_attributeType::AttrAssignment ) {
                                    // Assignment Overloading Encountered
                                    if( sa->m_attr != AST::simple_attributeType::AttrPublic &&
                                        sa->m_attr != AST::simple_attributeType::AttrPrivate ) {
                                        assgn[current_scope] = ASR::Public;
                                    } else {
                                        assgn[current_scope] = get_asr_simple_attr(sa->m_attr);
                                    }
                                 } else if (s.m_spec->type == AST::decl_attributeType::AttrDefinedOperator) {
                                    //std::string op_name = to_lower(AST::down_cast<AST::AttrDefinedOperator_t>(s.m_spec)->m_op_name);
                                    // Custom Operator Overloading Encountered
                                    if( sa->m_attr != AST::simple_attributeType::AttrPublic &&
                                        sa->m_attr != AST::simple_attributeType::AttrPrivate ) {
                                        overloaded_ops[current_scope][s.m_spec] = AST::simple_attributeType::AttrPublic;
                                    } else {
                                        overloaded_ops[current_scope][s.m_spec] = sa->m_attr;
                                    }
                                } else {
                                    throw SemanticError("Attribute type not implemented yet.", x.base.base.loc);
                                }
                            } else {
                                std::string sym = to_lower(s.m_name);
                                if (sa->m_attr == AST::simple_attributeType
                                        ::AttrPrivate) {
                                    assgnd_access[sym] = ASR::accessType::Private;
                                } else if (sa->m_attr == AST::simple_attributeType
                                        ::AttrPublic || sa->m_attr == AST::simple_attributeType
                                                ::AttrParameter || sa->m_attr == AST::simple_attributeType
                                                        ::AttrExternal) {
                                    assgnd_access[sym] = ASR::accessType::Public;
                                } else if (sa->m_attr == AST::simple_attributeType
                                        ::AttrOptional) {
                                    assgnd_presence[sym] = ASR::presenceType::Optional;
                                } else if(sa->m_attr == AST::simple_attributeType
                                        ::AttrIntrinsic) {
                                    // Ignore Intrinsic attribute
                                } else if (sa->m_attr == AST::simple_attributeType
                                        ::AttrExternal) {
                                    // TODO
                                    throw SemanticError("Attribute declaration not "
                                        "supported yet", x.base.base.loc);
                                } else if (sa->m_attr == AST::simple_attributeType::AttrEnumerator) {
                                    ASR::symbol_t *sym;
                                    ASR::ttype_t *init_type = ASRUtils::TYPE(
                                        ASR::make_Integer_t(al, x.m_syms[i].loc,
                                        4, nullptr, 0));
                                    ASR::expr_t *init_expr = ASRUtils::EXPR(
                                        ASR::make_IntegerConstant_t(al, x.m_syms[i].loc,
                                        enum_init_val, init_type));
                                    if (x.m_syms[i].m_sym == AST::symbolType::None) {
                                        // a_value_attr?
                                        // storage_type?
                                        sym = ASR::down_cast<ASR::symbol_t>(ASR::make_Variable_t(
                                            al, x.m_syms[i].loc, current_scope,
                                            x.m_syms[i].m_name, nullptr, 0, ASR::intentType::Local,
                                            init_expr, init_expr, ASR::storage_typeType::Parameter,
                                            init_type, ASR::abiType::Source, ASR::accessType::Public,
                                            ASR::presenceType::Required, false));
                                        current_scope->add_symbol(x.m_syms[i].m_name, sym);
                                    } else {
                                        enum_init_val = AST::down_cast<AST::Num_t>(
                                            x.m_syms[i].m_initializer)->m_n;
                                        this->visit_expr(*x.m_syms[i].m_initializer);
                                        ASR::expr_t *init_expr = ASRUtils::EXPR(tmp);
                                        init_type = ASRUtils::expr_type(init_expr);
                                        sym = ASR::down_cast<ASR::symbol_t>(ASR::make_Variable_t(
                                            al, x.m_syms[i].loc, current_scope,
                                            x.m_syms[i].m_name, nullptr, 0, ASR::intentType::Local,
                                            init_expr, init_expr, ASR::storage_typeType::Parameter,
                                            init_type, ASR::abiType::Source, ASR::accessType::Public,
                                            ASR::presenceType::Required, false));
                                        current_scope->add_symbol(x.m_syms[i].m_name, sym);
                                    }
                                    enum_init_val++;
                                } else {
                                    throw SemanticError("Attribute declaration not "
                                            "supported", x.base.base.loc);
                                }
                            }
                        }
                    }
                // enable sole `dimension` attribute
                } else if (AST::is_a<AST::AttrDimension_t>(*x.m_attributes[i])) {
                    for (size_t i=0;i<x.n_syms;++i) { // symbols for line only
                        AST::var_sym_t &s = x.m_syms[i];
                        std::string sym = to_lower(s.m_name);
                        ASR::symbol_t *get_sym = current_scope->get_symbol(sym);
                        // get actual variable from SymTab, not the current line
                        if (get_sym == nullptr) {
                            if (compiler_options.implicit_typing) {
                                ASR::intentType intent;
                                if (std::find(current_procedure_args.begin(),
                                        current_procedure_args.end(), sym) !=
                                        current_procedure_args.end()) {
                                    intent = ASRUtils::intent_unspecified;
                                } else {
                                    intent = ASRUtils::intent_local;
                                }
                                get_sym = declare_implicit_variable(s.loc, sym, intent);
                            } else {
                                throw SemanticError("Cannot set dimension for undeclared variable", x.base.base.loc);
                            }
                        }
                        bool is_compile_time = false;
                        if (ASR::is_a<ASR::Variable_t>(*get_sym)) {
                            Vec<ASR::dimension_t> dims;
                            dims.reserve(al, 0);
                            process_dims(al, dims, x.m_syms[i].m_dim, x.m_syms[i].n_dim, is_compile_time);
                            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(get_sym);
                            if (!ASRUtils::ttype_set_dimensions(v->m_type, dims.data(), dims.size())) {
                                throw SemanticError("Cannot set dimension for variable of non-numerical type", x.base.base.loc);
                            }
                            Vec<char*> variable_dependencies_vec;
                            variable_dependencies_vec.reserve(al, 1);
                            ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, v->m_type, v->m_symbolic_value, v->m_value);
                            v->m_dependencies = variable_dependencies_vec.p;
                            v->n_dependencies = variable_dependencies_vec.size();
                        } else {
                            throw SemanticError("Cannot attribute non-variable type with dimension", x.base.base.loc);
                        }
                    }
                } else if (AST::is_a<AST::AttrEquivalence_t>(*x.m_attributes[i])) {
                    diag.semantic_warning_label(
                        "Equivalence statement is not implemented yet, for now we will ignore it",
                        {x.base.base.loc},
                        "ignored for now"
                    );
                } else {
                    throw SemanticError("Attribute declaration not supported",
                        x.base.base.loc);
                }
            }
        } else {
            // Example
            // real(dp), private :: x, y(3), z
            for (size_t i=0; i<x.n_syms; i++) {
                bool is_compile_time = false;
                bool is_implicitly_declared = false;
                bool is_external = false;
                AST::var_sym_t &s = x.m_syms[i];
                std::string sym = to_lower(s.m_name);
                ASR::accessType s_access = dflt_access;
                ASR::presenceType s_presence = dflt_presence;
                bool value_attr = false;
                AST::AttrType_t *sym_type =
                    AST::down_cast<AST::AttrType_t>(x.m_vartype);
                if (assgnd_access.count(sym)) {
                    s_access = assgnd_access[sym];
                }
                if (assgnd_presence.count(sym)) {
                    s_presence = assgnd_presence[sym];
                }
                ASR::storage_typeType storage_type =
                        ASR::storage_typeType::Default;
                bool is_pointer = false;
                if (current_scope->get_symbol(sym) !=
                        nullptr) {
                    if (current_scope->parent != nullptr) {
                        if ( compiler_options.implicit_typing && implicit_dictionary[sym]!=nullptr ) {
                            // sym is implicitly declared
                            is_implicitly_declared = true;
                        } else {
                            // re-declaring a global scope variable is allowed
                            // Otherwise raise an error
                            ASR::symbol_t *orig_decl = current_scope->get_symbol(sym);
                            throw SemanticError(diag::Diagnostic(
                                "Symbol is already declared in the same scope",
                                diag::Level::Error, diag::Stage::Semantic, {
                                    diag::Label("redeclaration", {s.loc}),
                                    diag::Label("original declaration", {orig_decl->base.loc}, false),
                                }));
                        }
                    }
                }
                ASR::intentType s_intent;
                if (std::find(current_procedure_args.begin(),
                        current_procedure_args.end(), to_lower(s.m_name)) !=
                        current_procedure_args.end()) {
                    s_intent = ASRUtils::intent_unspecified;
                } else {
                    s_intent = ASRUtils::intent_local;
                }
                Vec<ASR::dimension_t> dims;
                dims.reserve(al, 0);
                // location for dimension(...) if present
                Location dims_attr_loc;
                if (x.n_attributes > 0) {
                    for (size_t i=0; i < x.n_attributes; i++) {
                        AST::decl_attribute_t *a = x.m_attributes[i];
                        if (AST::is_a<AST::SimpleAttribute_t>(*a)) {
                            AST::SimpleAttribute_t *sa =
                                AST::down_cast<AST::SimpleAttribute_t>(a);
                            if (sa->m_attr == AST::simple_attributeType
                                    ::AttrPrivate) {
                                s_access = ASR::accessType::Private;
                            } else if (sa->m_attr == AST::simple_attributeType
                                    ::AttrPublic) {
                                s_access = ASR::accessType::Public;
                            } else if (sa->m_attr == AST::simple_attributeType
                                    ::AttrSave) {
                                storage_type = ASR::storage_typeType::Save;
                            } else if (sa->m_attr == AST::simple_attributeType
                                    ::AttrParameter) {
                                storage_type = ASR::storage_typeType::Parameter;
                            } else if( sa->m_attr == AST::simple_attributeType
                                    ::AttrAllocatable ) {
                                storage_type = ASR::storage_typeType::Allocatable;
                            } else if (sa->m_attr == AST::simple_attributeType
                                    ::AttrPointer) {
                                is_pointer = true;
                            } else if (sa->m_attr == AST::simple_attributeType
                                    ::AttrOptional) {
                                s_presence = ASR::presenceType::Optional;
                            } else if (sa->m_attr == AST::simple_attributeType
                                    ::AttrTarget) {
                                // Do nothing for now
                            } else if (sa->m_attr == AST::simple_attributeType
                                    ::AttrAllocatable) {
                                // TODO
                            } else if (sa->m_attr == AST::simple_attributeType
                                    ::AttrValue) {
                                value_attr = true;
                            } else if(sa->m_attr == AST::simple_attributeType
                                    ::AttrIntrinsic) {
                                excluded_from_symtab.push_back(sym);
                            } else if(sa->m_attr == AST::simple_attributeType
                                    ::AttrExternal) {
                                assgnd_access[sym] = ASR::accessType::Public;
                                if (assgnd_access.count(sym)) {
                                    s_access = assgnd_access[sym];
                                }
                                is_external = true;
                            } else {
                                throw SemanticError("Attribute type not implemented yet",
                                        x.base.base.loc);
                            }
                        } else if (AST::is_a<AST::AttrIntent_t>(*a)) {
                            AST::AttrIntent_t *ai =
                                AST::down_cast<AST::AttrIntent_t>(a);
                            switch (ai->m_intent) {
                                case (AST::attr_intentType::In) : {
                                    s_intent = ASRUtils::intent_in;
                                    break;
                                }
                                case (AST::attr_intentType::Out) : {
                                    s_intent = ASRUtils::intent_out;
                                    break;
                                }
                                case (AST::attr_intentType::InOut) : {
                                    s_intent = ASRUtils::intent_inout;
                                    break;
                                }
                                default : {
                                    s_intent = ASRUtils::intent_unspecified;
                                    break;
                                }
                            }
                        } else if (AST::is_a<AST::AttrDimension_t>(*a)) {
                            AST::AttrDimension_t *ad =
                                AST::down_cast<AST::AttrDimension_t>(a);
                            if (dims.size() > 0) {
                                throw SemanticError("Dimensions specified twice",
                                        x.base.base.loc);
                            }
                            dims_attr_loc = ad->base.base.loc;
                            process_dims(al, dims, ad->m_dim, ad->n_dim, is_compile_time);
                        } else {
                            throw SemanticError("Attribute type not implemented yet",
                                    x.base.base.loc);
                        }
                    }
                }
                if (s.n_dim > 0) {
                    if (dims.size() > 0) {
                        // This happens for:
                        // integer, private, dimension(2,2) :: a(2,2)
                        diag.semantic_warning_label(
                            "Dimensions are specified twice",
                            {dims_attr_loc, s.loc}, // dimension(2,2), a(2,2)
                            "help: consider specifying it just one way or the other"
                        );
                        dims.n = 0;
                    }
                    process_dims(al, dims, s.m_dim, s.n_dim, is_compile_time);
                }
                ASR::ttype_t *type = determine_type(x.base.base.loc, sym, x.m_vartype, is_pointer, dims);
                current_variable_type_ = type;

                ASR::expr_t* init_expr = nullptr;
                ASR::expr_t* value = nullptr;
                if (s.m_initializer != nullptr
                        && sym_type->m_type == AST::decl_typeType::TypeType) {
                    if (AST::is_a<AST::FuncCallOrArray_t>(*s.m_initializer)) {
                        AST::FuncCallOrArray_t* func_call =
                            AST::down_cast<AST::FuncCallOrArray_t>(s.m_initializer);
                        ASR::symbol_t *sym_found = current_scope->resolve_symbol(
                            func_call->m_func);
                        if (sym_found == nullptr) {
                            visit_FuncCallOrArray(*func_call);
                            init_expr = ASRUtils::EXPR(tmp);
                        }
                    } else {
                        throw SemanticError("Only function call assignment is allowed for now",
                            x.base.base.loc);
                    }
                } else if (s.m_initializer != nullptr) {
                    this->visit_expr(*s.m_initializer);
                    if (is_compile_time && AST::is_a<AST::ArrayInitializer_t>(*s.m_initializer)) {
                        AST::ArrayInitializer_t *temp_array =
                            AST::down_cast<AST::ArrayInitializer_t>(s.m_initializer);
                        // For case  `integer, parameter :: x(*) = [1,2,3], get the compile time length of RHS array.
                        Vec<ASR::dimension_t> temp_dims;
                        temp_dims.reserve(al, 1);
                        ASR::dimension_t temp_dim;
                        temp_dim.loc = (temp_array->base).base.loc;
                        ASR::ttype_t *int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, (temp_array->base).base.loc,
                                                                                    4, nullptr, 0));
                        ASR::expr_t* one = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, (temp_array->base).base.loc, 1, int32_type));
                        ASR::expr_t* x_n_args = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, (temp_array->base).base.loc, temp_array->n_args, int32_type));
                        temp_dim.m_start = one;
                        temp_dim.m_length = x_n_args;
                        temp_dims.push_back(al, temp_dim);
                        type = ASRUtils::duplicate_type(al, type, &temp_dims);
                    }
                    init_expr = ASRUtils::EXPR(tmp);
                    ASR::ttype_t *init_type = ASRUtils::expr_type(init_expr);
                    ImplicitCastRules::set_converted_value(al, x.base.base.loc, &init_expr, init_type, type);
                    LCOMPILERS_ASSERT(init_expr != nullptr);
                    if (storage_type == ASR::storage_typeType::Parameter) {
                        value = ASRUtils::expr_value(init_expr);
                        if (value == nullptr) {
                            throw SemanticError("Value of a parameter variable must evaluate to a compile time constant",
                                x.base.base.loc);
                        }
                        if (sym_type->m_type == AST::decl_typeType::TypeCharacter) {
                            ASR::Character_t *lhs_type = ASR::down_cast<ASR::Character_t>(type);
                            ASR::Character_t *rhs_type = ASR::down_cast<ASR::Character_t>(ASRUtils::expr_type(value));
                            int lhs_len = lhs_type->m_len;
                            int rhs_len = rhs_type->m_len;
                            if (rhs_len >= 0) {
                                if (lhs_len == -1) {
                                    // The RHS len is known at compile time
                                    // and the LHS is inferred length
                                    lhs_len = rhs_len;
                                } else if (lhs_len >= 0) {
                                    if (lhs_len != rhs_len) {
                                        // Note: this might be valid, perhaps
                                        // change this to a warning
                                        throw SemanticError("The LHS character len="
                                            + std::to_string(lhs_len)
                                            + " and the RHS character len="
                                            + std::to_string(rhs_len)
                                            + " are not equal.", x.base.base.loc);
                                    }
                                } else {
                                    LCOMPILERS_ASSERT(lhs_len == -2)
                                    throw SemanticError("The LHS character len must not be allocatable in a parameter declaration",
                                        x.base.base.loc);
                                }
                            } else {
                                throw SemanticError("The RHS character len must be known at compile time",
                                    x.base.base.loc);
                            }
                            LCOMPILERS_ASSERT(lhs_len == rhs_len)
                            LCOMPILERS_ASSERT(lhs_len >= 0)
                            lhs_type->m_len = lhs_len;
                        }
                    } else {
                        storage_type = ASR::storage_typeType::Save; // implicit save
                    }
                }
                if( std::find(excluded_from_symtab.begin(), excluded_from_symtab.end(), sym) == excluded_from_symtab.end() ) {
                    if ( !is_implicitly_declared && !is_external) {
                        Vec<char*> variable_dependencies_vec;
                        variable_dependencies_vec.reserve(al, 1);
                        ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, type, init_expr, value);
                        ASR::asr_t *v = ASR::make_Variable_t(al, s.loc, current_scope,
                                s2c(al, to_lower(s.m_name)), variable_dependencies_vec.p,
                                variable_dependencies_vec.size(), s_intent, init_expr, value,
                                storage_type, type, current_procedure_abi_type, s_access, s_presence,
                                value_attr);
                        current_scope->add_symbol(sym, ASR::down_cast<ASR::symbol_t>(v));
                        if( is_derived_type ) {
                            data_member_names.push_back(al, s2c(al, to_lower(s.m_name)));
                        }
                    }
                }
            } // for m_syms
        }
    }

    ASR::ttype_t* determine_type(const Location &loc, std::string& sym, AST::decl_attribute_t* decl_attribute, bool is_pointer, Vec<ASR::dimension_t>& dims){
        AST::AttrType_t *sym_type = AST::down_cast<AST::AttrType_t>(decl_attribute);
        ASR::ttype_t *type;

        int a_kind = 4;
        if (sym_type->m_type != AST::decl_typeType::TypeCharacter &&
            sym_type->m_kind != nullptr &&
            sym_type->m_kind->m_value != nullptr) {
            this->visit_expr(*sym_type->m_kind->m_value);
            ASR::expr_t* kind_expr = ASRUtils::EXPR(tmp);
            a_kind = ASRUtils::extract_kind<SemanticError>(kind_expr, loc);
        }
        if (sym_type->m_type == AST::decl_typeType::TypeReal) {
            type = ASRUtils::TYPE(ASR::make_Real_t(al, loc,
                a_kind, dims.p, dims.size()));
            if (is_pointer) {
                type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
                    type));
            }
        } else if (sym_type->m_type == AST::decl_typeType::TypeDoublePrecision) {
            a_kind = 8;
            type = ASRUtils::TYPE(ASR::make_Real_t(al, loc,
                a_kind, dims.p, dims.size()));
            if (is_pointer) {
                type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
                    type));
            }
        } else if (sym_type->m_type == AST::decl_typeType::TypeInteger) {
            type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc,
                a_kind, dims.p, dims.size()));
            if (is_pointer) {
                type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
                    type));
            }
        } else if (sym_type->m_type == AST::decl_typeType::TypeLogical) {
            type = ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4,
                dims.p, dims.size()));
            if (is_pointer) {
                type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
                    type));
            }
        } else if (sym_type->m_type == AST::decl_typeType::TypeComplex) {
            type = ASRUtils::TYPE(ASR::make_Complex_t(al, loc,
                a_kind, dims.p, dims.size()));
            if (is_pointer) {
                type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
                    type));
            }
        } else if (sym_type->m_type == AST::decl_typeType::TypeDoubleComplex) {
            a_kind = 8;
            type = ASRUtils::TYPE(ASR::make_Complex_t(al, loc,
                a_kind, dims.p, dims.size()));
            if (is_pointer) {
                type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
                    type));
            }
        } else if (sym_type->m_type == AST::decl_typeType::TypeCharacter) {
            int a_len = -10;
            int a_kind = 1;
            ASR::expr_t *len_expr = nullptr;
            TypeMissingData* char_data = al.make_new<TypeMissingData>();
            char_data->sym_name = sym;
            LCOMPILERS_ASSERT(sym_type->n_kind < 3) // TODO
            for (size_t i = 0; i < sym_type->n_kind; i++) {
                // TODO: Allow len or/and kind only once (else throw SyntaxError)
                if (sym_type->m_kind[i].m_id != nullptr
                        && to_lower(sym_type->m_kind[i].m_id) == "kind") {
                    // TODO: take into account m_kind->m_id and all kind items
                    if (a_len == -10) {
                        a_len = -1; // "character(kind=1) :: x"
                    }
                    continue;
                }
                switch (sym_type->m_kind[i].m_type) {
                    case (AST::kind_item_typeType::Value) : {
                        LCOMPILERS_ASSERT(sym_type->m_kind[i].m_value != nullptr);
                        if( sym_type->m_kind[i].m_value->type == AST::exprType::FuncCallOrArray ) {
                            char_data->expr = sym_type->m_kind[i].m_value;
                            char_data->scope = current_scope;
                            char_data->sym_type = current_symbol;
                            AST::FuncCallOrArray_t* call =
                                AST::down_cast<AST::FuncCallOrArray_t>(
                                sym_type->m_kind[i].m_value);
                            if (AST::is_a<AST::Name_t>(*call->m_args->m_end)) {
                                ASR::symbol_t* sym = current_scope->get_symbol(
                                    AST::down_cast<AST::Name_t>(call->m_args->m_end)->m_id);
                                if (sym != nullptr) {
                                    sym = ASRUtils::symbol_get_past_external(sym);
                                    ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(sym);
                                    if (ASR::is_a<ASR::Character_t>(*v->m_type)) {
                                        a_len = ASR::down_cast<ASR::Character_t>(
                                            v->m_type)->m_len;
                                    }
                                }
                            }
                            if (a_len == -10) {
                                a_len = 1;
                            }
                        } else {
                            this->visit_expr(*sym_type->m_kind[i].m_value);
                            ASR::expr_t* len_expr0 = ASRUtils::EXPR(tmp);
                            a_len = ASRUtils::extract_len<SemanticError>(len_expr0, loc);
                            if (a_len == -3) {
                                len_expr = len_expr0;
                            }
                        }
                        break;
                    }
                    case (AST::kind_item_typeType::Star) : {
                        LCOMPILERS_ASSERT(sym_type->m_kind[i].m_value == nullptr);
                        a_len = -1;
                        break;
                    }
                    case (AST::kind_item_typeType::Colon) : {
                        LCOMPILERS_ASSERT(sym_type->m_kind[i].m_value == nullptr);
                        a_len = -2;
                        break;
                    }
                }
            }
            if (sym_type->m_kind == nullptr) {
                a_len = 1; // The default len of "character :: x" is 1
            }
            LCOMPILERS_ASSERT(a_len != -10)
            type = ASRUtils::TYPE(ASR::make_Character_t(al, loc, a_kind,
                a_len, len_expr, dims.p, dims.size()));
            if( char_data->scope != nullptr ) {
                char_data->type = type;
                type_info.push_back(char_data);
            }
            if (is_pointer) {
                type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
                    type));
            }
        } else if (sym_type->m_type == AST::decl_typeType::TypeType) {
            LCOMPILERS_ASSERT(sym_type->m_name);
            std::string derived_type_name = to_lower(sym_type->m_name);
            bool type_param = false;
            if (is_requirement) {
                for (size_t i = 0; i < current_requirement_type_parameters.size(); i++) {
                    ASR::TypeParameter_t* param = ASR::down_cast2<ASR::TypeParameter_t>(current_requirement_type_parameters[i]);
                    std::string name = std::string(param->m_param);
                    if(name.compare(derived_type_name) == 0){
                        type_param = true;
                        type = ASRUtils::TYPE(ASR::make_TypeParameter_t(al, loc,
                                                        param->m_param, param->m_dims, param->n_dims));
                    }
                }
            } else if (is_template) {
                for (const auto &pair: called_requirement) {
                    if (pair.first.compare(derived_type_name) == 0) {
                        ASR::asr_t *req_asr = pair.second;
                        if (ASR::is_a<ASR::ttype_t>(*req_asr)) {
                            type_param = true;
                            type = ASRUtils::TYPE(ASR::make_TypeParameter_t(al, loc,
                                                            s2c(al, derived_type_name), dims.p, dims.size()));
                        }
                    }
                }
            }

            ASR::symbol_t *v = current_scope->resolve_symbol(derived_type_name);
            if(!type_param) {
                if( v && ASRUtils::is_c_ptr(v, derived_type_name) ) {
                    type = ASRUtils::TYPE(ASR::make_CPtr_t(al, loc));
                } else {
                    if (!v) {
                        // Placeholder symbol for Struct type
                        // Derived type can be used before its actually defined
                        v = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(
                                al, loc, current_scope, s2c(al, derived_type_name),
                                nullptr, nullptr, nullptr, 0, s2c(al, derived_type_name),
                                ASR::accessType::Private));
                    }
                    type = ASRUtils::TYPE(ASR::make_Struct_t(al, loc, v,
                        dims.p, dims.size()));
                    if (is_pointer) {
                        type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
                            type));
                    }
                }
            }
        } else if (sym_type->m_type == AST::decl_typeType::TypeClass) {
            std::string derived_type_name;
            if( !sym_type->m_name ) {
                derived_type_name = "~abstract_type";
            } else {
                derived_type_name = to_lower(sym_type->m_name);
            }
            ASR::symbol_t *v = current_scope->resolve_symbol(derived_type_name);
            if( !v ) {
                if( derived_type_name != "~abstract_type" ) {
                    throw SemanticError("Derived type '" + derived_type_name
                                        + "' not declared", loc);
                }
                SymbolTable *parent_scope = current_scope;
                current_scope = al.make_new<SymbolTable>(parent_scope);
                ASR::asr_t* dtype = ASR::make_StructType_t(al, loc, current_scope,
                                                s2c(al, to_lower(derived_type_name)), nullptr, 0, nullptr, 0,
                                                ASR::abiType::Source, dflt_access, false, nullptr, nullptr);
                v = ASR::down_cast<ASR::symbol_t>(dtype);
                parent_scope->add_symbol(derived_type_name, v);
                current_scope = parent_scope;
            }
            type = ASRUtils::TYPE(ASR::make_Class_t(al,
                loc, v, dims.p, dims.size()));
            if (is_pointer) {
                type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
                    type));
            }
        } else if (sym_type->m_type == AST::decl_typeType::TypeProcedure) {
            std::string func_name = to_lower(sym_type->m_name);
            ASR::symbol_t *v = current_scope->resolve_symbol(func_name);
            if( !v ) {
                throw SemanticError("Procedure type '" + func_name
                                    + "' not declared", loc);
            }
            LCOMPILERS_ASSERT(ASR::is_a<ASR::Function_t>(*v));
            type = ASR::down_cast<ASR::Function_t>(v)->m_function_signature;
        } else {
            throw SemanticError("Type not implemented yet.",
                    loc);
        }

        return type;
    }


    ASR::asr_t* create_DerivedTypeConstructor(const Location &loc,
            AST::fnarg_t* m_args, size_t n_args, ASR::symbol_t *v) {
        Vec<ASR::expr_t*> vals = visit_expr_list(m_args, n_args);
        ASR::ttype_t* der = ASRUtils::TYPE(
                            ASR::make_Struct_t(al, loc, v,
                                                nullptr, 0));
        return ASR::make_StructTypeConstructor_t(al, loc,
                v, vals.p, vals.size(), der, nullptr);
    }

    ASR::asr_t* create_ArrayRef(const Location &loc,
                AST::fnarg_t* m_args, size_t n_args,
                    ASR::expr_t* v_expr,
                    ASR::symbol_t *v,
                    ASR::symbol_t *f2) {
        bool is_item = true;
        Vec<ASR::array_index_t> args;
        args.reserve(al, n_args);
        ASR::expr_t* v_Var = nullptr;
        if( v_expr ) {
            ASR::ttype_t* struct_t_mem_type = ASRUtils::type_get_past_pointer(ASRUtils::symbol_type(v));
            ASR::symbol_t* v_ext = ASRUtils::import_struct_instance_member(al, v, current_scope, struct_t_mem_type);
            v_Var = ASRUtils::EXPR(ASR::make_StructInstanceMember_t(
                        al, v_expr->base.loc, v_expr, v_ext,
                        struct_t_mem_type, nullptr));
        } else {
            v_Var = ASRUtils::EXPR(ASR::make_Var_t(al, loc, v));
        }
        for (size_t i=0; i<n_args; i++) {
            ASR::array_index_t ai;
            ai.loc = loc;
            ASR::expr_t *m_start, *m_end, *m_step;
            m_start = m_end = m_step = nullptr;
            if (m_args[i].m_start != nullptr) {
                this->visit_expr(*(m_args[i].m_start));
                m_start = ASRUtils::EXPR(tmp);
                ai.loc = m_start->base.loc;
            }
            if (m_args[i].m_end != nullptr) {
                this->visit_expr(*(m_args[i].m_end));
                m_end = ASRUtils::EXPR(tmp);
                ai.loc = m_end->base.loc;
            } else {
                if( ASR::is_a<ASR::Character_t>(*ASRUtils::symbol_type(v)) ) {
                    ASR::Character_t* char_type = ASR::down_cast<ASR::Character_t>(
                                                    ASRUtils::symbol_type(v));
                    bool is_comp_time_value = false;
                    if( char_type->m_len_expr &&
                        ASRUtils::expr_value(char_type->m_len_expr) ) {
                        int64_t m_len_expr_value = -1;
                        if( ASRUtils::extract_value(
                                ASRUtils::expr_value(char_type->m_len_expr),
                                m_len_expr_value) ) {
                            is_comp_time_value = true;
                        }
                    } else {
                        if( ASR::is_a<ASR::Variable_t>(*v) ) {
                            ASR::Variable_t* v_variable = ASR::down_cast<ASR::Variable_t>(v);
                            is_comp_time_value = v_variable->m_storage == ASR::storage_typeType::Parameter;
                        }
                    }
                    if( is_comp_time_value ) {
                        if( char_type->m_len_expr ) {
                            m_end = ASRUtils::expr_value(char_type->m_len_expr);
                        } else {
                            m_end = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                                        al, char_type->base.base.loc, char_type->m_len,
                                        ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4, nullptr, 0))));
                        }
                    } else {
                        m_end = ASRUtils::EXPR(ASR::make_StringLen_t(al, loc,
                                    v_Var, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4, nullptr, 0)),
                                    nullptr));
                    }
                } else {
                    m_end = ASRUtils::get_bound(v_Var, i + 1, "ubound", al);
                }
            }
            if (m_args[i].m_step != nullptr) {
                this->visit_expr(*(m_args[i].m_step));
                m_step = ASRUtils::EXPR(tmp);
                ai.loc = m_step->base.loc;
            }
            if( m_start != nullptr &&
                ASR::is_a<ASR::Var_t>(*m_start) &&
                ASR::is_a<ASR::Var_t>(*m_end) ) {
                ASR::Variable_t* startv = ASRUtils::EXPR2VAR(m_start);
                ASR::Variable_t* endv = ASRUtils::EXPR2VAR(m_end);
                is_item = is_item && (startv == endv);
                if( is_item ) {
                    m_start = nullptr;
                    m_step = nullptr;
                }
            } else {
                is_item = is_item && (m_start == nullptr &&
                                      m_step == nullptr &&
                                      m_end != nullptr);
            }
            ai.m_left = m_start;
            ai.m_right = m_end;
            ai.m_step = m_step;
            args.push_back(al, ai);
        }

        ASR::ttype_t *type;
        type = ASRUtils::type_get_past_pointer(ASRUtils::symbol_type(f2));
        ASR::expr_t *arr_ref_val = nullptr;
        bool all_args_eval = ASRUtils::all_args_evaluated(args);
        for( auto& a : args ) {
            // Assume that indices are constant integers
            int64_t start = -1, end = -1, step = -1;
            if( a.m_left ) {
                if( all_args_eval ) {
                    ASR::expr_t* m_left_expr = ASRUtils::expr_value(a.m_left);
                    ASR::IntegerConstant_t *m_left = ASR::down_cast<ASR::IntegerConstant_t>(m_left_expr);
                    start = m_left->m_n;
                }
            } else {
                start = 1;
            }
            if( a.m_right ) {
                if( all_args_eval ) {
                    ASR::expr_t* m_right_expr = ASRUtils::expr_value(a.m_right);
                    ASR::IntegerConstant_t *m_right = ASR::down_cast<ASR::IntegerConstant_t>(m_right_expr);
                    end = m_right->m_n;
                }
            }
            if( a.m_step ) {
                if( all_args_eval ) {
                    ASR::expr_t* m_step_expr = ASRUtils::expr_value(a.m_step);
                    ASR::IntegerConstant_t *m_step = ASR::down_cast<ASR::IntegerConstant_t>(m_step_expr);
                    step = m_step->m_n;
                }
            } else {
                step = 1;
            }
            if( v->type == ASR::symbolType::Variable ) {
                ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(v);
                ASR::expr_t *m_value = var->m_value;
                if( m_value && m_value->type == ASR::exprType::StringConstant ) {
                    ASR::StringConstant_t *m_str = ASR::down_cast<ASR::StringConstant_t>(m_value);
                    std::string sliced_str;
                    if( start != -1 && step != -1 && end == -1 ) {
                        end = 0;
                        while( m_str->m_s[end] != '\0' ) {
                            end += 1;
                        }
                        end -= 1;
                    }
                    if( start != -1 && end != -1 && step != -1 ) {
                        for( int i = start - 1; i <= end - 1; i += step ) {
                            sliced_str.push_back(m_str->m_s[i]);
                        }
                        Str l_str;
                        l_str.from_str(al, sliced_str);
                        arr_ref_val = ASRUtils::EXPR(ASR::make_StringConstant_t(al, loc, l_str.c_str(al), m_str->m_type));
                    }
                }
            }
        }

        if( is_item ) {
            Vec<ASR::dimension_t> empty_dims;
            empty_dims.reserve(al, 1);
            if (ASR::is_a<ASR::StructInstanceMember_t>(*v_Var)) {
                type = ASR::down_cast<ASR::StructInstanceMember_t>(v_Var)->m_type;
            }
            type = ASRUtils::duplicate_type(al, type, &empty_dims);
            return ASR::make_ArrayItem_t(al, loc,
                v_Var, args.p, args.size(), type, ASR::arraystorageType::ColMajor, arr_ref_val);
        } else {
            return ASR::make_ArraySection_t(al, loc,
                v_Var, args.p, args.size(), type, arr_ref_val);
        }
    }

    void visit_ArrayInitializer(const AST::ArrayInitializer_t &x) {
        Vec<ASR::expr_t*> body;
        body.reserve(al, x.n_args);
        ASR::ttype_t *type = nullptr;
        for (size_t i=0; i<x.n_args; i++) {
            this->visit_expr(*x.m_args[i]);
            ASR::expr_t *expr = ASRUtils::EXPR(tmp);
            if (type == nullptr) {
                type = ASRUtils::expr_type(expr);
            } else {
                if (ASRUtils::expr_type(expr)->type != type->type) {
                    throw SemanticError("Type mismatch in array initializer",
                        x.base.base.loc);
                }
            }
            body.push_back(al, expr);
        }
        Vec<ASR::dimension_t> dims;
        dims.reserve(al, 1);
        if (x.m_vartype != nullptr) {
            std::string sym = "";
            type = determine_type(x.base.base.loc, sym, x.m_vartype, false, dims);
        }
        ASR::dimension_t dim;
        dim.loc = x.base.base.loc;
        ASR::ttype_t *int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc,
                                                                      4, nullptr, 0));
        ASR::expr_t* one = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, 1, int32_type));
        ASR::expr_t* x_n_args = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, x.n_args, int32_type));
        dim.m_start = one;
        dim.m_length = x_n_args;
        dims.push_back(al, dim);
        type = ASRUtils::duplicate_type(al, type, &dims);
        tmp = ASR::make_ArrayConstant_t(al, x.base.base.loc, body.p,
            body.size(), type, ASR::arraystorageType::ColMajor);
    }

    void fill_expr_in_ttype_t(std::vector<ASR::expr_t*>& exprs, ASR::dimension_t* dims, size_t n_dims) {
        for( size_t i = 0; i < n_dims; i++ ) {
            exprs.push_back(dims[i].m_start);
            exprs.push_back(dims[i].m_length);
        }
    }

    void fix_exprs_ttype_t(std::vector<ASR::expr_t*>& exprs,
                           Vec<ASR::call_arg_t>& orig_args,
                           ASR::Function_t* orig_func=nullptr) {
        ASRUtils::ExprStmtDuplicator expr_duplicator(al);
        expr_duplicator.allow_procedure_calls = true;
        ASRUtils::ReplaceArgVisitor arg_replacer(al, current_scope, orig_func,
                                                 orig_args, current_function_dependencies);
        for( size_t i = 0; i < exprs.size(); i++ ) {
            ASR::expr_t* expri = exprs[i];
            if (expri) {
                expr_duplicator.success = true;
                ASR::expr_t* expri_copy = expr_duplicator.duplicate_expr(expri);
                LCOMPILERS_ASSERT(expr_duplicator.success);
                arg_replacer.current_expr = &expri_copy;
                arg_replacer.replace_expr(expri_copy);
                exprs[i] = expri_copy;
            }
        }
    }

    ASR::ttype_t* handle_return_type(ASR::ttype_t *return_type, const Location &loc,
                                     Vec<ASR::call_arg_t>& args,
                                     ASR::Function_t* f=nullptr) {
        // Rebuild the return type if needed and make FunctionCalls use ExternalSymbol
        std::vector<ASR::expr_t*> func_calls;
        switch( return_type->type ) {
            case ASR::ttypeType::Character: {
                ASR::Character_t *t = ASR::down_cast<ASR::Character_t>(return_type);
                func_calls.push_back(t->m_len_expr);
                fill_expr_in_ttype_t(func_calls, t->m_dims, t->n_dims);
                fix_exprs_ttype_t(func_calls, args, f);
                Vec<ASR::dimension_t> new_dims;
                new_dims.reserve(al, t->n_dims);
                for( size_t i = 1; i < func_calls.size(); i += 2 ) {
                    ASR::dimension_t new_dim;
                    new_dim.loc = func_calls[i]->base.loc;
                    new_dim.m_start = func_calls[i];
                    new_dim.m_length = func_calls[i + 1];
                    new_dims.push_back(al, new_dim);
                }
                int64_t a_len = t->m_len;
                if( func_calls[0] ) {
                    a_len = ASRUtils::extract_len<SemanticError>(func_calls[0], loc);
                }
                return ASRUtils::TYPE(ASR::make_Character_t(al, loc, t->m_kind, a_len, func_calls[0], new_dims.p, new_dims.size()));
            }
            case ASR::ttypeType::Struct: {
                ASR::Struct_t* struct_t_type = ASR::down_cast<ASR::Struct_t>(return_type);
                ASR::symbol_t *sym = struct_t_type->m_derived_type;
                ASR::symbol_t *es_s = current_scope->resolve_symbol(
                    ASRUtils::symbol_name(sym));
                if (es_s == nullptr) {
                    ASR::StructType_t *st = ASR::down_cast<ASR::StructType_t>(sym);
                    ASR::Module_t* sym_module = ASRUtils::get_sym_module(sym);
                    LCOMPILERS_ASSERT(sym_module != nullptr);
                    std::string st_name = "1_" + std::string(st->m_name);
                    if (current_scope->get_symbol(st_name)) {
                        sym = current_scope->get_symbol(st_name);
                    } else {
                        sym = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(
                            al, st->base.base.loc, current_scope, s2c(al, st_name),
                            sym, sym_module->m_name, nullptr, 0, st->m_name,
                            ASR::accessType::Public));
                        current_scope->add_symbol(st_name, sym);
                    }
                } else {
                    sym = es_s;
                }
                return ASRUtils::TYPE(ASR::make_Struct_t(al, loc, sym, struct_t_type->m_dims,
                        struct_t_type->n_dims));
            }
            case ASR::ttypeType::Integer: {
                ASR::Integer_t *t = ASR::down_cast<ASR::Integer_t>(return_type);
                fill_expr_in_ttype_t(func_calls, t->m_dims, t->n_dims);
                fix_exprs_ttype_t(func_calls, args, f);
                Vec<ASR::dimension_t> new_dims;
                new_dims.reserve(al, t->n_dims);
                for( size_t i = 0; i < func_calls.size(); i += 2 ) {
                    ASR::dimension_t new_dim;
                    if (func_calls[i] != nullptr) {
                        new_dim.loc = func_calls[i]->base.loc;
                        new_dim.m_start = func_calls[i];
                        new_dim.m_length = func_calls[i + 1];
                        new_dims.push_back(al, new_dim);
                    }
                }
                return ASRUtils::TYPE(ASR::make_Integer_t(al, loc, t->m_kind, new_dims.p, new_dims.size()));
            }
            case ASR::ttypeType::Real: {
                ASR::Real_t *t = ASR::down_cast<ASR::Real_t>(return_type);
                fill_expr_in_ttype_t(func_calls, t->m_dims, t->n_dims);
                fix_exprs_ttype_t(func_calls, args, f);
                Vec<ASR::dimension_t> new_dims;
                new_dims.reserve(al, t->n_dims);
                for( size_t i = 0; i < func_calls.size(); i += 2 ) {
                    ASR::dimension_t new_dim;
                    if (func_calls[i] != nullptr) {
                        new_dim.loc = func_calls[i]->base.loc;
                        new_dim.m_start = func_calls[i];
                        new_dim.m_length = func_calls[i + 1];
                        new_dims.push_back(al, new_dim);
                    }
                }
                return ASRUtils::TYPE(ASR::make_Real_t(al, loc, t->m_kind, new_dims.p, new_dims.size()));
                break;
            }
            default: {
                return return_type;
            }
        }
        return nullptr;
    }


    // Transforms intrinsics real(),int() to ImplicitCast. Return true if `f` is
    // real/int (result in `tmp`), false otherwise (`tmp` unchanged)
    ASR::asr_t* intrinsic_function_transformation(Allocator &al, const Location &loc,
            const std::string &fn_name, Vec<ASR::call_arg_t>& args) {
        // real(), int() are represented using ExplicitCast (for now we use
        // ImplicitCast) in ASR, so we save them to tmp and exit:
        if (fn_name == "real") {
            ASR::expr_t *arg1;
            if (args.size() == 1) {
                arg1 = nullptr;
            } else if (args.size() == 2) {
                arg1 = args[1].m_value;
            } else {
                throw SemanticError("real(...) must have 1 or 2 arguments", loc);
            }
            return LFortran::CommonVisitorMethods::comptime_intrinsic_real(args[0].m_value, arg1, al, loc);
        } else if (fn_name == "int") {
            ASR::expr_t *arg1;
            if (args.size() == 1) {
                arg1 = nullptr;
            } else if (args.size() == 2) {
                arg1 = args[1].m_value;
            } else {
                throw SemanticError("int(...) must have 1 or 2 arguments", loc);
            }
            if (ASR::is_a<ASR::IntegerBOZ_t>(*args[0].m_value)) {
                // Things like `int(b'01011101')` are skipped for now
                // They are converted in comptime_eval. We should probably
                // just convert them here instead.
                return nullptr;
            }
            return LFortran::CommonVisitorMethods::comptime_intrinsic_int(args[0].m_value, arg1, al, loc);
        } else {
            return nullptr;
        }
    }

    ASR::asr_t* symbol_resolve_external_generic_procedure_util(const Location &loc,
        int idx, ASR::symbol_t *v, Vec<ASR::call_arg_t>& args,
        ASR::GenericProcedure_t *g, ASR::ExternalSymbol_t *p) {
        ASR::symbol_t *final_sym;
        final_sym = g->m_procs[idx];
        if (!ASR::is_a<ASR::Function_t>(*final_sym)) {
            throw SemanticError("ExternalSymbol must point to a Function", loc);
        }
        ASR::ttype_t *return_type = nullptr;
        ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(final_sym);
        if( func->m_elemental && func->n_args == 1 && ASRUtils::is_array(ASRUtils::expr_type(args[0].m_value)) ) {
            return_type = ASRUtils::duplicate_type(al, ASRUtils::expr_type(args[0].m_value));
        } else {
            return_type = ASRUtils::EXPR2VAR(func->m_return_var)->m_type;
            return_type = handle_return_type(return_type, loc, args, func);
        }
        // Create ExternalSymbol for the final subroutine:
        // We mangle the new ExternalSymbol's local name as:
        //   generic_procedure_local_name @
        //     specific_procedure_remote_name
        std::string local_sym = std::string(p->m_name) + "@"
            + ASRUtils::symbol_name(final_sym);
        if (current_scope->get_symbol(local_sym) == nullptr) {
            Str name;
            name.from_str(al, local_sym);
            char *cname = name.c_str(al);
            ASR::asr_t *sub = ASR::make_ExternalSymbol_t(
                al, g->base.base.loc,
                /* a_symtab */ current_scope,
                /* a_name */ cname,
                final_sym,
                p->m_module_name, nullptr, 0, ASRUtils::symbol_name(final_sym),
                ASR::accessType::Private
                );
            final_sym = ASR::down_cast<ASR::symbol_t>(sub);
            current_scope->add_symbol(local_sym, final_sym);
        } else {
            final_sym = current_scope->get_symbol(local_sym);
        }
        ASR::expr_t *value = nullptr;
        ASR::symbol_t* final_sym2 = ASRUtils::symbol_get_past_external(final_sym);
        if (ASR::is_a<ASR::Function_t>(*final_sym2)) {
            ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(final_sym2);
            if (ASRUtils::is_intrinsic_procedure(f)) {
                ASR::symbol_t* v2 = ASRUtils::symbol_get_past_external(v);
                ASR::GenericProcedure_t *gp = ASR::down_cast<ASR::GenericProcedure_t>(v2);

                ASR::asr_t *result = intrinsic_function_transformation(al, loc, gp->m_name, args);
                if (result) {
                    return result;
                } else if (args.size() <= 2) {
                    value = intrinsic_procedures.comptime_eval(gp->m_name, al, loc, args);
                }
            }
        }
        current_function_dependencies.insert(std::string(ASRUtils::symbol_name(final_sym)));
        return ASR::make_FunctionCall_t(al, loc,
            final_sym, v, args.p, args.size(), return_type,
            value, nullptr);
    }

    ASR::asr_t* symbol_resolve_external_generic_procedure(
                const Location &loc,
                ASR::symbol_t *v, Vec<ASR::call_arg_t>& args) {
        ASR::ExternalSymbol_t *p = ASR::down_cast<ASR::ExternalSymbol_t>(v);
        ASR::symbol_t *f2 = ASR::down_cast<ASR::ExternalSymbol_t>(v)->m_external;
        ASR::GenericProcedure_t *g = ASR::down_cast<ASR::GenericProcedure_t>(f2);
        int idx = ASRUtils::select_generic_procedure(args, *g, loc,
                    [&](const std::string &msg, const Location &loc) { throw SemanticError(msg, loc); });
        return symbol_resolve_external_generic_procedure_util(loc, idx, v, args, g, p);
    }

    ASR::asr_t* symbol_resolve_external_generic_procedure_with_ast_node(
                const AST::FuncCallOrArray_t &x,
                ASR::symbol_t *v, Vec<ASR::call_arg_t>& args) {
        const Location& loc = x.base.base.loc;
        ASR::ExternalSymbol_t *p = ASR::down_cast<ASR::ExternalSymbol_t>(v);
        ASR::symbol_t *f2 = ASR::down_cast<ASR::ExternalSymbol_t>(v)->m_external;
        ASR::GenericProcedure_t *g = ASR::down_cast<ASR::GenericProcedure_t>(f2);
        int idx = ASRUtils::select_generic_procedure(args, *g, loc,
                    [&](const std::string &msg, const Location &loc) { throw SemanticError(msg, loc); },
                    false);
        if( idx == -1 ) {
            bool is_function = true;
            v = intrinsic_as_node(x, is_function);
            if( !is_function ) {
                return tmp;
            }
            return create_FunctionCall(loc, v, args);
        } else {
            return symbol_resolve_external_generic_procedure_util(loc, idx, v, args, g, p);
        }
    }

    ASR::asr_t* create_ClassProcedure(const Location &loc,
                AST::fnarg_t* m_args, size_t n_args,
                    ASR::symbol_t *v,
                    ASR::expr_t *v_expr) {
        Vec<ASR::call_arg_t> args;
        visit_expr_list(m_args, n_args, args);
        ASR::ClassProcedure_t *v_class_proc = ASR::down_cast<ASR::ClassProcedure_t>(ASRUtils::symbol_get_past_external(v));
        ASR::ttype_t *type = nullptr;
        ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(v_class_proc->m_proc);
        if( func->m_elemental && func->n_args == 1 && ASRUtils::is_array(ASRUtils::expr_type(args[0].m_value)) ) {
            type = ASRUtils::duplicate_type(al, ASRUtils::expr_type(args[0].m_value));
        } else {
            type = ASRUtils::EXPR2VAR(func->m_return_var)->m_type;
        }
        current_function_dependencies.insert(std::string(ASRUtils::symbol_name(v)));
        return ASR::make_FunctionCall_t(al, loc,
                v, nullptr, args.p, args.size(), type, nullptr,
                v_expr);
    }

    ASR::asr_t* create_GenericProcedure(const Location &loc,
                Vec<ASR::call_arg_t>& args, ASR::symbol_t *v) {
        if (ASR::is_a<ASR::ExternalSymbol_t>(*v)) {
            return symbol_resolve_external_generic_procedure(loc, v,
                    args);
        } else {
            ASR::GenericProcedure_t *p = ASR::down_cast<ASR::GenericProcedure_t>(v);
            int idx = ASRUtils::select_generic_procedure(args, *p, loc,
                    [&](const std::string &msg, const Location &loc) { throw SemanticError(msg, loc); },
                    false);
            if( idx == -1 ) {
                std::string v_name = ASRUtils::symbol_name(v);
                v = resolve_intrinsic_function(loc, v_name);
                if( !v ) {
                    throw SemanticError("Couldn't find any function " + v_name + ".",
                                        loc);
                }
                return create_FunctionCall(loc, v, args);
            }
            ASR::symbol_t *final_sym = p->m_procs[idx];

            ASR::ttype_t *type = nullptr;
            ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(final_sym);
            if( func->m_elemental && func->n_args == 1 && ASRUtils::is_array(ASRUtils::expr_type(args[0].m_value)) ) {
                type = ASRUtils::duplicate_type(al, ASRUtils::expr_type(args[0].m_value));
            } else {
                type = ASRUtils::EXPR2VAR(func->m_return_var)->m_type;
                type = handle_return_type(type, loc, args, func);
            }
            current_function_dependencies.insert(std::string(ASRUtils::symbol_name(final_sym)));
            return ASR::make_FunctionCall_t(al, loc,
                final_sym, v, args.p, args.size(), type,
                nullptr, nullptr);
        }
    }

    ASR::asr_t* create_GenericProcedureWithASTNode(const AST::FuncCallOrArray_t& x,
                Vec<ASR::call_arg_t>& args, ASR::symbol_t *v) {
        const Location& loc = x.base.base.loc;
        if (ASR::is_a<ASR::ExternalSymbol_t>(*v)) {
            return symbol_resolve_external_generic_procedure_with_ast_node(x, v,
                    args);
        } else {
            ASR::GenericProcedure_t *p = ASR::down_cast<ASR::GenericProcedure_t>(v);
            int idx = ASRUtils::select_generic_procedure(args, *p, loc,
                    [&](const std::string &msg, const Location &loc) { throw SemanticError(msg, loc); },
                    false);
            if( idx == -1 ) {
                bool is_function = true;
                v = intrinsic_as_node(x, is_function);
                if( !is_function ) {
                    return tmp;
                }
                return create_FunctionCall(loc, v, args);
            }
            ASR::symbol_t *final_sym = p->m_procs[idx];

            ASR::ttype_t *type = nullptr;
            ASR::symbol_t *cp_s = nullptr;
            if (ASR::is_a<ASR::ClassProcedure_t>(*final_sym)) {
                cp_s = ASRUtils::import_class_procedure(al, x.base.base.loc,
                    final_sym, current_scope);
                final_sym = ASR::down_cast<ASR::ClassProcedure_t>(final_sym)->m_proc;
            }
            LCOMPILERS_ASSERT(ASR::is_a<ASR::Function_t>(*final_sym))
            ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(final_sym);
            if( func->m_elemental && func->n_args == 1 && ASRUtils::is_array(ASRUtils::expr_type(args[0].m_value)) ) {
                type = ASRUtils::duplicate_type(al, ASRUtils::expr_type(args[0].m_value));
            } else {
                type = ASRUtils::EXPR2VAR(func->m_return_var)->m_type;
                type = handle_return_type(type, loc, args, func);
            }
            current_function_dependencies.insert(std::string(ASRUtils::symbol_name(final_sym)));
            if (cp_s != nullptr) {
                return ASR::make_FunctionCall_t(al, loc,
                    cp_s, v, args.p, args.size(), type,
                    nullptr, nullptr);
            } else {
                return ASR::make_FunctionCall_t(al, loc,
                    final_sym, v, args.p, args.size(), type,
                    nullptr, nullptr);
            }
        }
    }

    ASR::asr_t* create_Function(const Location &loc,
                Vec<ASR::call_arg_t>& args, ASR::symbol_t *v) {
        ASR::symbol_t *f2 = ASRUtils::symbol_get_past_external(v);
        ASR::ttype_t *return_type = nullptr;
        ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(f2);
        if( func->m_elemental && func->n_args == 1 && ASRUtils::is_array(ASRUtils::expr_type(args[0].m_value)) ) {
            return_type = ASRUtils::duplicate_type(al, ASRUtils::expr_type(args[0].m_value));
        } else {
            return_type = ASRUtils::EXPR2VAR(func->m_return_var)->m_type;
            return_type = handle_return_type(return_type, loc, args, func);
        }
        ASR::expr_t* value = nullptr;
        if (ASR::is_a<ASR::ExternalSymbol_t>(*v)) {
            // Populate value
            ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(f2);
            if (ASRUtils::is_intrinsic_procedure(f)) {
                ASR::asr_t* result = intrinsic_function_transformation(al, loc, f->m_name, args);
                if (result) {
                    return result;
                } else {
                    value = intrinsic_procedures.comptime_eval(f->m_name, al, loc, args);
                    char *mod = ASR::down_cast<ASR::ExternalSymbol_t>(
                        current_scope->resolve_symbol(f->m_name))->m_module_name;
                    if (!present(current_module_dependencies, mod)) {
                        current_module_dependencies.push_back(al, mod);
                    }
                }
            }
        }
        current_function_dependencies.insert(std::string(ASRUtils::symbol_name(v)));
        ASR::Module_t* v_module = ASRUtils::get_sym_module0(f2);
        if( v_module ) {
            if( !present(current_module_dependencies, v_module->m_name) ) {
                current_module_dependencies.push_back(al, v_module->m_name);
            }
        }
        return ASR::make_FunctionCall_t(al, loc, v, nullptr,
            args.p, args.size(), return_type, value, nullptr);
    }

    ASR::asr_t* create_FunctionFromFunctionTypeVariable(const Location &loc,
                Vec<ASR::call_arg_t>& args, ASR::symbol_t *v) {
        ASR::FunctionType_t* func = ASR::down_cast<ASR::FunctionType_t>(ASRUtils::symbol_type(v));
        ASR::ttype_t *return_type = func->m_return_var_type;
        current_function_dependencies.insert(std::string(ASRUtils::symbol_name(v)));
        return ASR::make_FunctionCall_t(al, loc, v, nullptr,
            args.p, args.size(), return_type, nullptr, nullptr);
    }

    // `fn` is a local Function or GenericProcedure (that resolves to a
    // Function), or an ExternalSymbol that points to a Function or
    // GenericProcedure (that resolves to a Function). This function resolves
    // the GeneralProcedure (based on `args`) and repacks the final function
    // into an ExternalSymbol if needed. It returns a FunctionCall ASR node.
    //
    // `args` are arguments of the function call as a list of `expr` nodes.
    //
    // If `fn` is intrinsic, it will also try to evaluate it into the `value`
    // member of the returned `FunctionCall`.
    ASR::asr_t* create_FunctionCall(const Location &loc,
                ASR::symbol_t *fn, Vec<ASR::call_arg_t>& args) {
        ASR::symbol_t *f2 = ASRUtils::symbol_get_past_external(fn);
        if (ASR::is_a<ASR::Function_t>(*f2)) {
            return create_Function(loc, args, fn);
        } else {
            LCOMPILERS_ASSERT(ASR::is_a<ASR::GenericProcedure_t>(*f2))
            return create_GenericProcedure(loc, args, fn);
        }
    }

    ASR::asr_t* create_FunctionCallWithASTNode(const AST::FuncCallOrArray_t& x,
                ASR::symbol_t *v, Vec<ASR::call_arg_t>& args) {
        ASR::symbol_t *f2 = ASRUtils::symbol_get_past_external(v);
        if (ASR::is_a<ASR::Function_t>(*f2)) {
            return create_Function(x.base.base.loc, args, v);
        } else if (ASR::is_a<ASR::Variable_t>(*f2)) {
            return create_FunctionFromFunctionTypeVariable(x.base.base.loc, args, v);
        } else {
            LCOMPILERS_ASSERT(ASR::is_a<ASR::GenericProcedure_t>(*f2))
            return create_GenericProcedureWithASTNode(x, args, v);
        }
    }

    ASR::asr_t* resolve_variable2(const Location &loc, const std::string &var_name,
            const std::string &dt_name, SymbolTable*& scope) {
        ASR::symbol_t *v = scope->resolve_symbol(dt_name);
        if (!v) {
            throw SemanticError("Variable '" + dt_name + "' not declared", loc);
        }
        ASR::Variable_t* v_variable = ASR::down_cast<ASR::Variable_t>(ASRUtils::symbol_get_past_external(v));
        if (ASR::is_a<ASR::Struct_t>(*ASRUtils::type_get_past_pointer(v_variable->m_type)) ||
                ASR::is_a<ASR::Class_t>(*ASRUtils::type_get_past_pointer(v_variable->m_type))) {
            ASR::ttype_t* v_type = ASRUtils::type_get_past_pointer(v_variable->m_type);
            ASR::symbol_t *derived_type = nullptr;
            if (ASR::is_a<ASR::Struct_t>(*v_type)) {
                derived_type = ASR::down_cast<ASR::Struct_t>(v_type)->m_derived_type;
            } else if (ASR::is_a<ASR::Class_t>(*v_type)) {
                derived_type = ASR::down_cast<ASR::Class_t>(v_type)->m_class_type;
            }
            ASR::StructType_t *der_type;
            if (ASR::is_a<ASR::ExternalSymbol_t>(*derived_type)) {
                ASR::ExternalSymbol_t* der_ext = ASR::down_cast<ASR::ExternalSymbol_t>(derived_type);
                ASR::symbol_t* der_sym = der_ext->m_external;
                if (der_sym == nullptr) {
                    throw SemanticError("'" + std::string(der_ext->m_name) + "' isn't a Derived type.", loc);
                } else {
                    der_type = ASR::down_cast<ASR::StructType_t>(der_sym);
                }
            } else {
                der_type = ASR::down_cast<ASR::StructType_t>(derived_type);
            }
            ASR::StructType_t *par_der_type = der_type;
            // scope = der_type->m_symtab;
            // ASR::symbol_t* member = der_type->m_symtab->resolve_symbol(var_name);
            ASR::symbol_t* member = nullptr;
            while( par_der_type != nullptr && member == nullptr ) {
                scope = par_der_type->m_symtab;
                member = par_der_type->m_symtab->resolve_symbol(var_name);
                if( par_der_type->m_parent != nullptr ) {
                    par_der_type = ASR::down_cast<ASR::StructType_t>(ASRUtils::symbol_get_past_external(par_der_type->m_parent));
                } else {
                    par_der_type = nullptr;
                }
            }
            if( member != nullptr ) {
                ASR::asr_t* v_var = ASR::make_Var_t(al, loc, v);
                return ASRUtils::getStructInstanceMember_t(al, loc, v_var, v, member, current_scope);
            } else {
                throw SemanticError("Variable '" + dt_name + "' doesn't have any member named, '" + var_name + "'.", loc);
            }
        } else if (ASR::is_a<ASR::Complex_t>(*v_variable->m_type)) {
            if (var_name == "re") {
                ASR::expr_t *val = ASR::down_cast<ASR::expr_t>(ASR::make_Var_t(al, loc, v));
                int kind = ASRUtils::extract_kind_from_ttype_t(v_variable->m_type);
                ASR::ttype_t *dest_type = ASR::down_cast<ASR::ttype_t>(ASR::make_Real_t(al, loc, kind, nullptr, 0));
                ImplicitCastRules::set_converted_value(
                    al, loc, &val, v_variable->m_type, dest_type);
                return (ASR::asr_t*)val;
            } else if (var_name == "im") {
                ASR::expr_t *val = ASR::down_cast<ASR::expr_t>(ASR::make_Var_t(al, loc, v));
                ASR::symbol_t *fn_aimag = resolve_intrinsic_function(loc, "aimag");
                Vec<ASR::call_arg_t> args;
                args.reserve(al, 1);
                ASR::call_arg_t val_arg;
                val_arg.loc = val->base.loc;
                val_arg.m_value = val;
                args.push_back(al, val_arg);
                ASR::asr_t *result = create_FunctionCall(loc, fn_aimag, args);
                return result;
            } else {
                throw SemanticError("Complex variable '" + dt_name + "' only has %re and %im members, not '" + var_name + "'", loc);
            }
        } else {
            throw SemanticError("Variable '" + dt_name + "' is not a derived type", loc);
        }
    }

    ASR::symbol_t* resolve_deriv_type_proc(const Location &loc, const std::string &var_name,
            const std::string dt_name, ASR::ttype_t* dt_type, SymbolTable*& scope,
            ASR::symbol_t* parent=nullptr) {
        ASR::symbol_t* v = nullptr;
        ASR::StructType_t* der_type = nullptr;
        if( parent == nullptr ) {
            if ( ASR::is_a<ASR::Struct_t>(*dt_type) ) {
                ASR::Struct_t* der = ASR::down_cast<ASR::Struct_t>(dt_type);
                der_type = ASR::down_cast<ASR::StructType_t>(ASRUtils::symbol_get_past_external(der->m_derived_type));
            } else if( ASR::is_a<ASR::Class_t>(*dt_type) ) {
                ASR::Class_t* der = ASR::down_cast<ASR::Class_t>(dt_type);
                der_type = ASR::down_cast<ASR::StructType_t>(ASRUtils::symbol_get_past_external(der->m_class_type));
            } else {
                throw SemanticError("Variable '" + dt_name + "' is not a derived type", loc);
            }
        } else {
            v = ASRUtils::symbol_get_past_external(parent);
            der_type = ASR::down_cast<ASR::StructType_t>(v);
        }
        ASR::symbol_t* member = der_type->m_symtab->get_symbol(var_name);
        if( member != nullptr ) {
            scope = der_type->m_symtab;
        } else if( der_type->m_parent != nullptr ) {
            member = resolve_deriv_type_proc(loc, var_name, "", nullptr, scope, der_type->m_parent);
        } else {
            throw SemanticError("Variable '" + dt_name + "' doesn't have any member named, '" + var_name + "'.", loc);
        }
        return member;
    }

    // TODO: Use Vec<expr_t*> instead of std::vector<expr_t*> for performance
    template <typename T>
    bool handle_intrinsic_node_args(const T& x,
        std::vector<ASR::expr_t*>& args, std::vector<std::string>& kwarg_names,
        size_t min_args, size_t max_args, const std::string& intrinsic_name,
        bool raise_error=true) {
        size_t total_args = x.n_args + x.n_keywords;
        if( !(total_args <= max_args && total_args >= min_args) ) {
            if( !raise_error ) {
                return false;
            }
            throw SemanticError("Incorrect number of arguments "
                                "passed to the " + intrinsic_name + " intrinsic."
                                "It accepts at least " + std::to_string(min_args) +
                                " and at most " + std::to_string(max_args) + " arguments.",
                                x.base.base.loc);
        }

        for( size_t i = 0; i < max_args; i++ ) {
            args.push_back(nullptr);
        }

        for( size_t i = 0; i < x.n_args; i++ ) {
            this->visit_expr(*x.m_args[i].m_end);
            args[i] = ASRUtils::EXPR(tmp);
        }

        for( size_t i = 0; i < x.n_keywords; i++ ) {
            std::string curr_kwarg_name = to_lower(x.m_keywords[i].m_arg);
            if( std::find(kwarg_names.begin(), kwarg_names.end(),
                          curr_kwarg_name) == kwarg_names.end() ) {
                if( !raise_error ) {
                    return false;
                }
                throw SemanticError("Unrecognized keyword argument " + curr_kwarg_name +
                                    " passed to " + intrinsic_name + " intrinsic.",
                                    x.base.base.loc);
            }
        }

        size_t offset = min_args;
        for( size_t i = 0; i < x.n_keywords; i++ ) {
            std::string curr_kwarg_name = to_lower(x.m_keywords[i].m_arg);
            auto it = std::find(kwarg_names.begin(), kwarg_names.end(),
                                curr_kwarg_name);
            int64_t kwarg_idx = it - kwarg_names.begin();
            if( args[kwarg_idx + offset] != nullptr ) {
                if( !raise_error ) {
                    return false;
                }
                throw SemanticError(curr_kwarg_name + " has already " +
                                    "been specified as a positional/keyword " +
                                    "argument to " + intrinsic_name + ".",
                                    x.base.base.loc);
            }
            this->visit_expr(*x.m_keywords[i].m_value);
            args[kwarg_idx + offset] = ASRUtils::EXPR(tmp);
        }
        return true;
    }

    int64_t handle_kind(ASR::expr_t* kind) {
        if( kind == nullptr ) {
            return 4;
        }

        ASR::expr_t* kind_value = ASRUtils::expr_value(kind);
        if( kind_value == nullptr ) {
            throw SemanticError(("Only Integer literals or expressions "
                                "which reduce to constant Integer are "
                                "accepted as kind parameters."),
                                kind->base.loc);
        }
        return ASR::down_cast<ASR::IntegerConstant_t>(kind_value)->m_n;
    }

    ASR::asr_t* create_Floor(const AST::FuncCallOrArray_t &x,
            ASR::ExternalSymbol_t* gp_ext, ASR::symbol_t* v) {
        std::vector<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names = {"kind"};
        handle_intrinsic_node_args(x, args, kwarg_names, 1, 2, "floor");
        ASR::expr_t* kind = args[1];
        int64_t kind_value = handle_kind(kind);
        ASR::ttype_t *kind_type = ASRUtils::TYPE(ASR::make_Integer_t(al,
                x.base.base.loc, kind_value, nullptr, 0));
        ASR::expr_t* kind_arg = ASR::down_cast<ASR::expr_t>(
                ASR::make_IntegerConstant_t(al, x.base.base.loc, 0, kind_type));
        Vec<ASR::call_arg_t> args2;
        args2.reserve(al, 2);
        ASR::call_arg_t arg1;
        arg1.loc = args[0]->base.loc;
        arg1.m_value = args[0];
        args2.push_back(al, arg1);
        ASR::call_arg_t kind_arg2;
        kind_arg2.loc = x.base.base.loc;
        kind_arg2.m_value = kind_arg;
        args2.push_back(al, kind_arg2);
        ASR::GenericProcedure_t* gp = ASR::down_cast<ASR::GenericProcedure_t>(
                gp_ext->m_external);
        int idx = ASRUtils::select_generic_procedure(args2, *gp, x.base.base.loc,
                        [&](const std::string &msg, const Location &loc) { throw SemanticError(msg, loc); },
                        true);
        return symbol_resolve_external_generic_procedure_util(gp->base.base.loc, idx, v, args2, gp, gp_ext);
    }

    ASR::asr_t* create_ArrayBound(const AST::FuncCallOrArray_t& x, std::string& bound_name) {
        std::vector<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names = {"dim", "kind"};
        handle_intrinsic_node_args(x, args, kwarg_names, 1, 3, bound_name);
        ASR::expr_t *v_Var = args[0], *dim = args[1], *kind = args[2];
        ASR::arrayboundType bound = ASR::arrayboundType::LBound;
        if( bound_name == "lbound" ) {
            bound = ASR::arrayboundType::LBound;
        } else if( bound_name == "ubound" ) {
            bound = ASR::arrayboundType::UBound;
        }
        int64_t kind_const = handle_kind(kind);
        ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc,
                                            kind_const, nullptr, 0));
        return ASR::make_ArrayBound_t(al, x.base.base.loc, v_Var, dim, type,
                                      bound, nullptr);
    }

    ASR::asr_t* create_ArraySize(const AST::FuncCallOrArray_t& x) {
        std::vector<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names = {"dim", "kind"};
        handle_intrinsic_node_args(x, args, kwarg_names, 1, 3, std::string("size"));
        ASR::expr_t *v_Var = args[0], *dim = args[1], *kind = args[2];
        int64_t kind_const = handle_kind(kind);
        ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc,
                                            kind_const, nullptr, 0));
        ASR::dimension_t* m_dims = nullptr;
        int n_dims = ASRUtils::extract_dimensions_from_ttype(ASRUtils::expr_type(v_Var), m_dims);
        int64_t compile_time_size = 1;
        if (dim != nullptr) {
            int32_t dim_idx = -1;
            ASRUtils::extract_value(dim, dim_idx);
            if (dim_idx == -1) {
                compile_time_size = -1;
            } else {
                ASR::dimension_t m_dim = m_dims[dim_idx - 1];
                ASR::expr_t* length_expr = m_dim.m_length;
                int64_t length = -1;
                ASRUtils::extract_value(length_expr, length);
                if (length == -1) {
                    compile_time_size = -1;
                } else {
                    compile_time_size = length;
                }
            }
        } else {
            for( int i = 0; i < n_dims; i++ ) {
                ASR::dimension_t m_dim = m_dims[i];
                ASR::expr_t* length_expr = m_dim.m_length;
                int64_t length = -1;
                ASRUtils::extract_value(length_expr, length);
                if( length == -1 ) {
                    compile_time_size = -1;
                    break ;
                }
                compile_time_size *= length;
            }
        }
        ASR::expr_t* size_compiletime = nullptr;
        if( compile_time_size != -1 ) {
            size_compiletime = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc,
                                                compile_time_size, type));
        }
        return ASR::make_ArraySize_t(al, x.base.base.loc, v_Var, dim, type, size_compiletime);
    }

    ASR::asr_t* create_ArrayTranspose(const AST::FuncCallOrArray_t& x) {
        std::vector<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names;
        handle_intrinsic_node_args(x, args, kwarg_names, 1, 1, std::string("transpose"));
        ASR::expr_t *matrix = args[0];
        ASR::ttype_t *type = ASRUtils::expr_type(matrix);
        ASR::dimension_t* matrix_dims = nullptr;
        int matrix_rank = ASRUtils::extract_dimensions_from_ttype(type, matrix_dims);
        if( matrix_rank != 2 ) {
            throw SemanticError("transpose accepts arrays "
                                "of rank 2 only, provided an array "
                                "with rank, " + std::to_string(matrix_rank),
                                matrix->base.loc);
        }
        Vec<ASR::dimension_t> reversed_dims;
        reversed_dims.reserve(al, 2);
        reversed_dims.push_back(al, matrix_dims[1]);
        reversed_dims.push_back(al, matrix_dims[0]);
        ASR::ttype_t* ret_type = ASRUtils::duplicate_type(al, type, &reversed_dims);
        return ASR::make_ArrayTranspose_t(al, x.base.base.loc, matrix, ret_type, nullptr);
    }

    ASR::asr_t* create_ArrayMatMul(const AST::FuncCallOrArray_t& x) {
        std::vector<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names;
        handle_intrinsic_node_args(x, args, kwarg_names, 2, 2, std::string("matmul"));
        ASR::expr_t *matrix_a = args[0], *matrix_b = args[1];
        ASR::ttype_t *type_a = ASRUtils::expr_type(matrix_a);
        ASR::ttype_t *type_b = ASRUtils::expr_type(matrix_b);
        bool matrix_a_numeric = ASR::is_a<ASR::Integer_t>(*type_a) ||
                                ASR::is_a<ASR::Real_t>(*type_a) ||
                                ASR::is_a<ASR::Complex_t>(*type_a);
        bool matrix_a_logical = ASR::is_a<ASR::Logical_t>(*type_a);
        bool matrix_b_numeric = ASR::is_a<ASR::Integer_t>(*type_b) ||
                                ASR::is_a<ASR::Real_t>(*type_b) ||
                                ASR::is_a<ASR::Complex_t>(*type_b);
        bool matrix_b_logical = ASR::is_a<ASR::Logical_t>(*type_b);
        if( !matrix_a_numeric && !matrix_a_logical ) {
            throw SemanticError("matmul accepts first matrix of "
                                "type Integer, Real, Complex or Logical.",
                                matrix_a->base.loc);
        }
        if( matrix_a_numeric ) {
            if( !matrix_b_numeric ) {
                throw SemanticError("matmul accepts second matrix of "
                                    "type Integer, Real or Complex if "
                                    "first matrix is of numeric type.",
                                    matrix_b->base.loc);
            }
        } else {
            if( !matrix_b_logical ) {
                throw SemanticError("matmul accepts second matrix of type "
                                    "Logical if first matrix is of Logical type",
                                    matrix_b->base.loc);
            }
        }
        ASR::dimension_t* matrix_a_dims = nullptr;
        ASR::dimension_t* matrix_b_dims = nullptr;
        int matrix_a_rank = ASRUtils::extract_dimensions_from_ttype(type_a, matrix_a_dims);
        int matrix_b_rank = ASRUtils::extract_dimensions_from_ttype(type_b, matrix_b_dims);
        if( matrix_a_rank != 1 && matrix_a_rank != 2 ) {
            throw SemanticError("matmul accepts arrays "
                                "of rank 1 or 2 only, provided an array "
                                "with rank, " + std::to_string(matrix_a_rank),
                                matrix_a->base.loc);
        }
        if( matrix_b_rank != 1 && matrix_b_rank != 2 ) {
            throw SemanticError("matmul accepts arrays "
                                "of rank 1 or 2 only, provided an array "
                                "with rank, " + std::to_string(matrix_b_rank),
                                matrix_b->base.loc);
        }
        if( matrix_a_rank == 1 && matrix_b_rank == 1 ) {
            throw SemanticError("matmul provided with two arrays, each of rank 1.",
                                x.base.base.loc);
        }
        ASR::ttype_t *int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc,
                                                                      4, nullptr, 0));
        ASR::expr_t* one = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, 1, int32_type));
        ASR::dimension_t default_dim;
        default_dim.m_start = one;
        default_dim.m_length = one;
        default_dim.loc = one->base.loc;
        std::vector<ASR::dimension_t> pair_a(2), pair_b(2);
        if( matrix_a_rank == 1 ) {
            pair_a[0] = matrix_a_dims[0];
            pair_a[1] = default_dim;
        } else {
            pair_a[0] = matrix_a_dims[0];
            pair_a[1] = matrix_a_dims[1];
        }
        if( matrix_b_rank == 1 ) {
            pair_b[0] = matrix_b_dims[0];
            pair_b[1] = default_dim;
        } else {
            pair_b[0] = matrix_b_dims[0];
            pair_b[1] = matrix_b_dims[1];
        }
        // TODO: Check if second dimension of matrix is equal
        // TODO: to first dimension of matrix_b
        Vec<ASR::dimension_t> reversed_dims;
        reversed_dims.reserve(al, 2);
        reversed_dims.push_back(al, pair_a[0]);
        reversed_dims.push_back(al, pair_b[1]);
        ASR::ttype_t* selected_type = nullptr;
        if( ImplicitCastRules::get_type_priority(type_a->type) >=
            ImplicitCastRules::get_type_priority(type_b->type) ) {
            selected_type = type_a;
        } else {
            selected_type = type_b;
        }
        ASR::ttype_t* ret_type = ASRUtils::duplicate_type(al, selected_type, &reversed_dims);
        return ASR::make_ArrayMatMul_t(al, x.base.base.loc, matrix_a, matrix_b, ret_type, nullptr);
    }

    ASR::asr_t* create_ArrayPack(const AST::FuncCallOrArray_t& x) {
        std::vector<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names = {"vector"};
        handle_intrinsic_node_args(x, args, kwarg_names, 2, 3, "pack");
        ASR::expr_t *array = args[0], *mask = args[1], *vector = args[2];
        Vec<ASR::dimension_t> new_dims;
        new_dims.reserve(al, 1);
        ASR::dimension_t new_dim;
        ASR::ttype_t* int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, 4, nullptr, 0));
        new_dim.loc = x.base.base.loc;
        new_dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, 1, int32_type));
        new_dim.m_length = ASRUtils::EXPR(ASR::make_ArraySize_t(al, x.base.base.loc,
                            vector ? vector : mask, nullptr,
                            int32_type, nullptr));
        new_dims.push_back(al, new_dim);
        ASR::ttype_t *type = ASRUtils::duplicate_type(al, ASRUtils::expr_type(array), &new_dims);
        return ASR::make_ArrayPack_t(al, x.base.base.loc, array, mask,
                                     vector, type, nullptr);
    }

    ASR::asr_t* create_ArrayMaxloc(const AST::FuncCallOrArray_t& x) {
        ASR::expr_t *array, *dim, *mask, *kind, *back;
        array = dim = mask = kind = back = nullptr;

        std::vector<ASR::expr_t*> args_0, args_1;
        std::vector<std::string> kwarg_names_0 = {"dim", "mask", "kind", "back"};
        std::vector<std::string> kwarg_names_1 = {"mask", "kind", "back"};
        // Try syntax MAXLOC(ARRAY, DIM [, MASK] [,KIND] [,BACK])
        bool syntax_0_matched = handle_intrinsic_node_args(x, args_0, kwarg_names_0, 2, 5, "maxloc", false);
        // Try syntax MAXLOC(ARRAY [, MASK] [,KIND] [,BACK])
        bool syntax_1_matched = handle_intrinsic_node_args(x, args_1, kwarg_names_1, 1, 4, "maxloc", false);

        if( !syntax_0_matched && !syntax_1_matched ) {
            throw SemanticError("maxloc can only be called by either "
                                "MAXLOC(ARRAY, DIM [, MASK] [,KIND] [,BACK])"
                                " or MAXLOC(ARRAY [, MASK] [,KIND] [,BACK]) syntax.",
                                x.base.base.loc);
        }

        if( syntax_0_matched ) {
            array = args_0[0], dim = args_0[1], mask = args_0[2], kind = args_0[3], back = args_0[4];
        } else {
            array = args_1[0], mask = args_1[1], kind = args_1[2], back = args_1[3];
        }

        ASR::ttype_t *type = nullptr;
        Vec<ASR::dimension_t> new_dims;
        ASR::ttype_t* int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, 4, nullptr, 0));
        if( !dim ) {
            new_dims.reserve(al, 1);
            ASR::dimension_t new_dim;
            new_dim.loc = x.base.base.loc;
            new_dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, 1, int32_type));
            new_dim.m_length = ASRUtils::EXPR(ASR::make_ArraySize_t(al, x.base.base.loc,
                                    array, nullptr, int32_type, nullptr));
            new_dims.push_back(al, new_dim);
            type = ASRUtils::duplicate_type(al, ASRUtils::expr_type(array), &new_dims);
        } else {
            ASR::dimension_t* m_dims;
            int n_dims = ASRUtils::extract_dimensions_from_ttype(ASRUtils::expr_type(array), m_dims);
            if( n_dims == 1 ) {
                type = ASRUtils::duplicate_type(al, ASRUtils::expr_type(array));
            } else {
                new_dims.reserve(al, n_dims - 1);
                for( int i = 0; i < n_dims - 1; i++ ) {
                    ASR::dimension_t new_dim;
                    new_dim.loc = x.base.base.loc;
                    new_dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, 1, int32_type));
                    new_dim.m_length = ASRUtils::EXPR(ASR::make_ArraySize_t(al, x.base.base.loc,
                                            array, nullptr, int32_type, nullptr));
                    new_dims.push_back(al, new_dim);
                }
                type = ASRUtils::duplicate_type(al, ASRUtils::expr_type(array), &new_dims);
            }
        }
        return ASR::make_ArrayMaxloc_t(al, x.base.base.loc, array, dim,
                                       mask, kind, back, type, nullptr);
    }

    ASR::asr_t* create_ArrayReshape(const AST::FuncCallOrArray_t& x) {
        if( x.n_args != 2 ) {
             throw SemanticError("reshape accepts only 2 arguments, got " +
                                 std::to_string(x.n_args) + " arguments instead.",
                                 x.base.base.loc);
         }
         this->visit_expr(*x.m_args[0].m_end);
         ASR::expr_t* array = ASRUtils::EXPR(tmp);
         this->visit_expr(*x.m_args[1].m_end);
         ASR::expr_t* newshape = ASRUtils::EXPR(tmp);
         if( !ASRUtils::is_array(ASRUtils::expr_type(newshape)) ) {
             throw SemanticError("reshape only accept arrays for shape "
                                 "arguments, found " +
                                 ASRUtils::type_to_str_python(ASRUtils::expr_type(newshape)) +
                                 " instead.",
                                 x.base.base.loc);
         }
         Vec<ASR::dimension_t> dims;
         dims.reserve(al, 1);
         ASR::dimension_t newdim;
         newdim.loc = x.base.base.loc;
         newdim.m_start = nullptr, newdim.m_length = nullptr;
         dims.push_back(al, newdim);
         ASR::ttype_t* empty_type = ASRUtils::duplicate_type(al, ASRUtils::expr_type(array), &dims);
         return ASR::make_ArrayReshape_t(al, x.base.base.loc, array, newshape, empty_type, nullptr);
    }

    ASR::asr_t* create_BitCast(const AST::FuncCallOrArray_t& x) {
        std::vector<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names = {"size"};
        handle_intrinsic_node_args(x, args, kwarg_names, 2, 3, "transfer");
        ASR::expr_t *source = args[0], *mold = args[1], *size = args[2];
        if( size && !ASR::is_a<ASR::Integer_t>(*ASRUtils::expr_type(size)) ) {
            throw SemanticError("size argument to transfer intrinsic must "
                                "be of Integer type.",
                                size->base.loc);
        }
        Vec<ASR::dimension_t> new_dims;
        new_dims.reserve(al, 1);
        if( size ) {
            ASR::expr_t* one = ASRUtils::EXPR(
                ASR::make_IntegerConstant_t(al, x.base.base.loc, 1,
                    ASRUtils::expr_type(size)));
            ASR::dimension_t size_dim;
            size_dim.loc = size->base.loc;
            size_dim.m_start = one;
            size_dim.m_length = size;
            new_dims.push_back(al, size_dim);
        } else {
            if( ASR::is_a<ASR::ArrayConstant_t>(*mold) ||
                ASRUtils::is_array(ASRUtils::expr_type(mold)) ) {
                // TODO: Make resulting array size more efficient by
                // considering bit length of source.
                ASR::ttype_t *int32_type = ASRUtils::TYPE(
                    ASR::make_Integer_t(al, x.base.base.loc,
                                        4, nullptr, 0));
                ASR::expr_t* one = ASRUtils::EXPR(
                    ASR::make_IntegerConstant_t(al, x.base.base.loc, 1,
                                                int32_type));
                ASR::expr_t* b64 = ASRUtils::EXPR(
                    ASR::make_IntegerConstant_t(al, x.base.base.loc, 64,
                                                int32_type));
                ASR::dimension_t size_dim;
                size_dim.loc = x.base.base.loc;
                size_dim.m_start = one;
                size_dim.m_length = b64;
                new_dims.push_back(al, size_dim);
            }
        }
        ASR::ttype_t* type = ASRUtils::duplicate_type(al, ASRUtils::expr_type(mold), &new_dims);
        return ASR::make_BitCast_t(al, x.base.base.loc, source, mold,
                                     size, type, nullptr);
    }

    ASR::asr_t* create_Cmplx(const AST::FuncCallOrArray_t& x) {
        std::vector<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names = {"y", "kind"};
        handle_intrinsic_node_args(x, args, kwarg_names, 1, 3, "cmplx");
        ASR::expr_t *x_ = args[0], *y_ = args[1], *kind = args[2];
        if( ASR::is_a<ASR::Complex_t>(*ASRUtils::expr_type(x_)) ) {
            if( y_ != nullptr ) {
                throw SemanticError("The first argument of cmplx intrinsic"
                                    " is of complex type, the second argument "
                                    "in this case must be absent",
                                    x.base.base.loc);
            }
            return (ASR::asr_t*) x_;
        }
        int64_t kind_value = handle_kind(kind);
        if( y_ == nullptr ) {
            ASR::ttype_t* real_type = ASRUtils::TYPE(ASR::make_Real_t(al,
                                        x.base.base.loc, kind_value,
                                        nullptr, 0));
            y_ = ASRUtils::EXPR(ASR::make_RealConstant_t(al, x.base.base.loc,
                                                         0.0, real_type));
        }
        ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Complex_t(al, x.base.base.loc,
                                kind_value, nullptr, 0));
        return ASR::make_ComplexConstructor_t(al, x.base.base.loc, x_, y_, type, nullptr);
    }

    ASR::asr_t* create_NullPointerConstant(const AST::FuncCallOrArray_t& x) {
        std::vector<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names = {"mold"};
        handle_intrinsic_node_args(x, args, kwarg_names, 0, 1, "null");
        ASR::expr_t *mold_ = args[0];
        ASR::ttype_t* null_ptr_type_ = nullptr;
        if( mold_ ) {
            null_ptr_type_ = ASRUtils::expr_type(mold_);
        } else {
            LCOMPILERS_ASSERT(current_variable_type_ != nullptr);
            null_ptr_type_ = current_variable_type_;
        }
        return ASR::make_PointerNullConstant_t(al, x.base.base.loc, null_ptr_type_);
    }

    ASR::asr_t* create_Associated(const AST::FuncCallOrArray_t& x) {
        std::vector<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names = {"tgt"};
        handle_intrinsic_node_args(x, args, kwarg_names, 1, 2, "associated");
        ASR::expr_t *ptr_ = args[0], *tgt_ = args[1];
        ASR::ttype_t* associated_type_ = ASRUtils::TYPE(ASR::make_Logical_t(
                                            al, x.base.base.loc, 4, nullptr, 0));
        return ASR::make_PointerAssociated_t(al, x.base.base.loc, ptr_, tgt_, associated_type_, nullptr);
    }

    ASR::asr_t* create_DCmplx(const AST::FuncCallOrArray_t& x) {
        std::vector<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names = {"y"};
        handle_intrinsic_node_args(x, args, kwarg_names, 1, 2, "dcmplx");
        ASR::expr_t *x_ = args[0], *y_ = args[1];
        if( ASR::is_a<ASR::Complex_t>(*ASRUtils::expr_type(x_)) ) {
            if( y_ != nullptr ) {
                throw SemanticError("The first argument of dcmplx intrinsic"
                                    " is of complex type, the second argument "
                                    "in this case must be absent",
                                    x.base.base.loc);
            }
            return (ASR::asr_t*) x_;
        }
        int64_t kind_value = 8;
        if( y_ == nullptr ) {
            ASR::ttype_t* real_type = ASRUtils::TYPE(ASR::make_Real_t(al,
                                        x.base.base.loc, kind_value,
                                        nullptr, 0));
            y_ = ASRUtils::EXPR(ASR::make_RealConstant_t(al, x.base.base.loc,
                                                         0.0, real_type));
        }
        ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Complex_t(al, x.base.base.loc,
                                kind_value, nullptr, 0));
        return ASR::make_ComplexConstructor_t(al, x.base.base.loc, x_, y_, type, nullptr);
    }

    ASR::asr_t* create_Ichar(const AST::FuncCallOrArray_t& x) {
        std::vector<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names = {"kind"};
        handle_intrinsic_node_args(x, args, kwarg_names, 1, 2, "ichar");
        ASR::expr_t *arg = args[0], *kind = args[1];
        int64_t kind_value = handle_kind(kind);
        ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc,
                                kind_value, nullptr, 0));
        ASR::expr_t* ichar_value = nullptr;
        ASR::expr_t* arg_value = ASRUtils::expr_value(arg);
        if( arg_value ) {
            std::string arg_str;
            bool is_const_value = ASRUtils::is_value_constant(arg_value, arg_str);
            if( is_const_value ) {
                int64_t ascii_code = arg_str[0];
                ichar_value = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc,
                                ascii_code, type));
            }
        }
        return ASR::make_Ichar_t(al, x.base.base.loc, arg, type, ichar_value);
    }

    ASR::asr_t* create_Iachar(const AST::FuncCallOrArray_t& x) {
        std::vector<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names = {"kind"};
        handle_intrinsic_node_args(x, args, kwarg_names, 1, 2, "iachar");
        ASR::expr_t *arg = args[0], *kind = args[1];
        int64_t kind_value = handle_kind(kind);
        ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc,
                                kind_value, nullptr, 0));
        ASR::expr_t* iachar_value = nullptr;
        ASR::expr_t* arg_value = ASRUtils::expr_value(arg);
        if( arg_value ) {
            std::string arg_str;
            bool is_const_value = ASRUtils::is_value_constant(arg_value, arg_str);
            if( is_const_value ) {
                int64_t ascii_code = arg_str[0];
                iachar_value = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc,
                                ascii_code, type));
            }
        }
        return ASR::make_Iachar_t(al, x.base.base.loc, arg, type, iachar_value);
    }

    ASR::asr_t* create_ScanVerify_util(const AST::FuncCallOrArray_t& x, std::string func_name) {
        ASR::expr_t *string, *set, *back, *kind;
        ASR::ttype_t *type;
        string = nullptr, set = nullptr, back = nullptr;
        type = nullptr, kind = nullptr;
        std::vector<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names = {"back", "kind"};
        handle_intrinsic_node_args(x, args, kwarg_names, 2, 4, func_name);
        string = args[0], set = args[1], back = args[2], kind = args[3];
        int64_t kind_value = handle_kind(kind);
        type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, kind_value, nullptr, 0));
        std::string function_name = func_name + "_kind" + std::to_string(kind_value);

        ASR::call_arg_t string_arg, set_arg, back_arg;
        string_arg.loc = string->base.loc;
        string_arg.m_value = string;
        if( set ) {
            set_arg.loc = set->base.loc;
        } else {
            set_arg.loc = x.base.base.loc;
        }
        set_arg.m_value = set;
        if( back ) {
            back_arg.loc = back->base.loc;
        } else {
            back_arg.loc = x.base.base.loc;
        }
        back_arg.m_value = back;

        Vec<ASR::call_arg_t> func_args;
        func_args.reserve(al, 3);
        func_args.push_back(al, string_arg);
        func_args.push_back(al, set_arg);
        func_args.push_back(al, back_arg);
        ASR::symbol_t* function = current_scope->resolve_symbol(function_name);
        if( !function ) {
            function = resolve_intrinsic_function(x.base.base.loc, function_name);
            ASR::Module_t* function_module = ASRUtils::get_sym_module(ASRUtils::symbol_get_past_external(function));
            if( function_module ) {
                char* module_name = function_module->m_name;
                if (!present(current_module_dependencies, module_name)) {
                    current_module_dependencies.push_back(al, module_name);
                }
            }
        }

        current_function_dependencies.insert(function_name);
        return ASR::make_FunctionCall_t(al, x.base.base.loc, function, nullptr, func_args.p,
            func_args.size(), type, nullptr, nullptr);
    }

    ASR::asr_t* create_All(const AST::FuncCallOrArray_t& x) {
        std::vector<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names = {"dim"};
        handle_intrinsic_node_args(x, args, kwarg_names, 1, 2, "all");
        ASR::expr_t *mask = args[0], *dim = args[1];
        // TODO: compute the compile_time value
        ASR::expr_t* value = nullptr;
        ASR::ttype_t *type = ASRUtils::TYPE(
            ASR::make_Logical_t(al, x.base.base.loc, 4, nullptr, 0));
        return ASR::make_All_t(al, x.base.base.loc, mask, dim, type, value);
    }

    ASR::symbol_t* intrinsic_as_node(const AST::FuncCallOrArray_t &x,
                                     bool& is_function) {
        std::string var_name = to_lower(x.m_func);
        if( intrinsic_procedures_as_asr_nodes.is_intrinsic_present_in_ASR(var_name) ||
            intrinsic_procedures_as_asr_nodes.is_kind_based_selection_required(var_name) ) {
            is_function = false;
            if( var_name == "size" ) {
                tmp = create_ArraySize(x);
            } else if( var_name == "lbound" || var_name == "ubound" ) {
                tmp = create_ArrayBound(x, var_name);
            } else if( var_name == "transpose" ) {
                tmp = create_ArrayTranspose(x);
            } else if( var_name == "matmul" ) {
                tmp = create_ArrayMatMul(x);
            } else if( var_name == "pack" ) {
                tmp = create_ArrayPack(x);
            } else if( var_name == "transfer" ) {
                tmp = create_BitCast(x);
            } else if( var_name == "cmplx" ) {
                tmp = create_Cmplx(x);
            } else if( var_name == "dcmplx" ) {
                tmp = create_DCmplx(x);
            } else if( var_name == "reshape" ) {
                tmp = create_ArrayReshape(x);
            } else if( var_name == "ichar" ) {
                tmp = create_Ichar(x);
            } else if( var_name == "iachar" ) {
                tmp = create_Iachar(x);
            } else if( var_name == "maxloc" ) {
                tmp = create_ArrayMaxloc(x);
            } else if( var_name == "scan" ) {
                tmp = create_ScanVerify_util(x, var_name);
            } else if( var_name == "verify" ) {
                tmp = create_ScanVerify_util(x, var_name);
            } else if( var_name == "null" ) {
                tmp = create_NullPointerConstant(x);
            } else if( var_name == "associated" ) {
                tmp = create_Associated(x);
            } else if( var_name == "all" ) {
                tmp = create_All(x);
            } else {
                LCompilersException("create_" + var_name + " not implemented yet.");
            }
            return nullptr;
        }
        return resolve_intrinsic_function(x.base.base.loc, var_name);
    }

    ASR::asr_t* create_PointerToCptr(const AST::FuncCallOrArray_t& x) {
        std::vector<ASR::expr_t*> args;
        std::vector<std::string> kwarg_names = {};
        handle_intrinsic_node_args(x, args, kwarg_names, 1, 1, std::string("c_loc"));
        ASR::expr_t *v_Var = args[0];
        if( !ASR::is_a<ASR::GetPointer_t>(*v_Var) &&
            !ASRUtils::is_pointer(ASRUtils::expr_type(v_Var)) ) {
            ASR::ttype_t* ptr_type = ASRUtils::TYPE(ASR::make_Pointer_t(al, x.base.base.loc,
                                        ASRUtils::expr_type(v_Var)));
            v_Var = ASRUtils::EXPR(ASR::make_GetPointer_t(al, x.base.base.loc,
                            v_Var, ptr_type, nullptr));
        }
        ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_CPtr_t(al, x.base.base.loc));
        return ASR::make_PointerToCPtr_t(al, x.base.base.loc, v_Var, type, nullptr);
    }

    ASR::asr_t* handle_intrinsic_float(Allocator &al, Vec<ASR::call_arg_t> args,
                                        const Location &loc) {
        ASR::expr_t *arg = nullptr, *value = nullptr;
        ASR::ttype_t *type = nullptr;
        if (args.size() > 0) {
            arg = args[0].m_value;
            type = ASRUtils::expr_type(arg);
        }
        ASR::ttype_t *to_type = ASRUtils::TYPE(ASR::make_Real_t(al, loc,
                                    8, nullptr, 0));
        if (!arg) {
            return ASR::make_RealConstant_t(al, loc, 0.0, to_type);
        }
        if (ASRUtils::is_integer(*type)) {
            if (ASRUtils::expr_value(arg) != nullptr) {
                double dval = ASR::down_cast<ASR::IntegerConstant_t>(
                                        ASRUtils::expr_value(arg))->m_n;
                value =  ASR::down_cast<ASR::expr_t>(make_RealConstant_t(al,
                                loc, dval, to_type));
            }
            return (ASR::asr_t *)ASR::down_cast<ASR::expr_t>(ASR::make_Cast_t(
                al, loc, arg, ASR::cast_kindType::IntegerToReal,
                to_type, value));
        } else if (ASRUtils::is_logical(*type)) {
            if (ASRUtils::expr_value(arg) != nullptr) {
                double dval = ASR::down_cast<ASR::LogicalConstant_t>(
                                        ASRUtils::expr_value(arg))->m_value;
                value =  ASR::down_cast<ASR::expr_t>(make_RealConstant_t(al,
                                loc, dval, to_type));
            }
            return (ASR::asr_t *)ASR::down_cast<ASR::expr_t>(ASR::make_Cast_t(
                al, loc, arg, ASR::cast_kindType::LogicalToReal,
                to_type, value));
        } else if (ASRUtils::is_real(*type)) {
            // float() always returns 64-bit floating point numbers.
            if (ASRUtils::extract_kind_from_ttype_t(type) != 8) {
                return (ASR::asr_t *)ASR::down_cast<ASR::expr_t>(ASR::make_Cast_t(
                    al, loc, arg, ASR::cast_kindType::RealToReal,
                    to_type, value));
            }
            return (ASR::asr_t *)arg;
        } else if (ASRUtils::is_complex(*type)) {
            return (ASR::asr_t *)ASR::down_cast<ASR::expr_t>(ASR::make_Cast_t(
                    al, loc, arg, ASR::cast_kindType::ComplexToReal,
                    to_type, value));
        } else {
            std::string stype = ASRUtils::type_to_str(type);
            throw SemanticError(
                "Conversion of '" + stype + "' to float is not Implemented",
                loc);
        }
        // TODO: Make this work if the argument is, let's say, a class.
        return nullptr;
    }

    template <class Call>
    void create_implicit_interface_function(const Call &x, std::string func_name, bool add_return) {
        SymbolTable *parent_scope = current_scope;
        current_scope = al.make_new<SymbolTable>(parent_scope);

        Vec<ASR::call_arg_t> c_args;
        visit_expr_list(x.m_args, x.n_args, c_args);

        Vec<ASR::expr_t*> args;
        args.reserve(al, x.n_args);
        std::string sym_name = to_lower(func_name);
        for (size_t i=0; i<x.n_args; i++) {
            std::string arg_name = sym_name + "_arg_" + std::to_string(i);
            arg_name = to_lower(arg_name);
            ASR::expr_t *var_expr = c_args[i].m_value;
            ASR::symbol_t *v;
            if (ASR::is_a<ASR::Var_t>(*var_expr) &&
                    ASR::is_a<ASR::Function_t>(*ASR::down_cast<ASR::Var_t>(var_expr)->m_v)) {
                v = ASR::down_cast<ASR::Var_t>(var_expr)->m_v;
            } else {
                ASR::ttype_t *var_type = ASRUtils::expr_type(var_expr);
                if (ASRUtils::is_array(var_type)) {
                    // For arrays like A(n, m) we use A(*) in BindC, so that
                    // the C ABI is just a pointer
                    var_type = ASRUtils::duplicate_type_without_dims(al, var_type, x.base.base.loc);
                    Vec<ASR::dimension_t> dims;
                    dims.reserve(al, 1);
                    ASR::dimension_t dim;
                    dim.loc = x.base.base.loc;
                    dim.m_start = nullptr;
                    dim.m_length = nullptr;
                    dims.push_back(al, dim);
                    ASRUtils::ttype_set_dimensions(var_type, dims.p, dims.size());
                }
                Vec<char*> variable_dependencies_vec;
                variable_dependencies_vec.reserve(al, 1);
                ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, var_type);
                v = ASR::down_cast<ASR::symbol_t>(
                    ASR::make_Variable_t(al, x.base.base.loc,
                    current_scope, s2c(al, arg_name), variable_dependencies_vec.p,
                    variable_dependencies_vec.size(), ASRUtils::intent_unspecified,
                    nullptr, nullptr, ASR::storage_typeType::Default, var_type,
                    ASR::abiType::BindC, ASR::Public, ASR::presenceType::Required,
                    false));
                current_scope->add_symbol(arg_name, v);
            }
            LCOMPILERS_ASSERT(v != nullptr)
            args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc,
                v)));
        }
        // FIXME: accept this type as an argument
        // currently hardcoding the return type to real-8
        ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Real_t(al, x.base.base.loc,
                                8, nullptr, 0));
        ASR::expr_t *to_return = nullptr;
        if (add_return) {
            std::string return_var_name = sym_name + "_return_var_name";
            Vec<char*> variable_dependencies_vec;
            variable_dependencies_vec.reserve(al, 1);
            ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, type);
            ASR::asr_t *return_var = ASR::make_Variable_t(al, x.base.base.loc,
                current_scope, s2c(al, return_var_name), variable_dependencies_vec.p,
                variable_dependencies_vec.size(), ASRUtils::intent_return_var,
                nullptr, nullptr, ASR::storage_typeType::Default, type,
                ASR::abiType::BindC, ASR::Public, ASR::presenceType::Required,
                false);
            current_scope->add_symbol(return_var_name, ASR::down_cast<ASR::symbol_t>(return_var));
            to_return = ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc,
                ASR::down_cast<ASR::symbol_t>(return_var)));
        }

        tmp = ASRUtils::make_Function_t_util(
            al, x.base.base.loc,
            /* a_symtab */ current_scope,
            /* a_name */ s2c(al, sym_name),
            nullptr, 0,
            /* a_args */ args.p,
            /* n_args */ args.size(),
            /* a_body */ nullptr,
            /* n_body */ 0,
            /* a_return_var */ to_return,
            ASR::abiType::BindC, ASR::accessType::Public, ASR::deftypeType::Interface,
            nullptr, false, false, false, false, false, /* a_type_parameters */ nullptr,
            /* n_type_parameters */ 0, nullptr, 0, false);
        parent_scope->add_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(tmp));
        current_scope = parent_scope;
    }

    void visit_ImpliedDoLoop(const AST::ImpliedDoLoop_t& x) {
        Vec<ASR::expr_t*> a_values_vec;
        ASR::expr_t *a_start, *a_end, *a_increment;
        a_start = a_end = a_increment = nullptr;
        a_values_vec.reserve(al, x.n_values);
        for( size_t i = 0; i < x.n_values; i++ ) {
            this->visit_expr(*(x.m_values[i]));
            a_values_vec.push_back(al, ASRUtils::EXPR(tmp));
        }
        this->visit_expr(*(x.m_start));
        a_start = ASRUtils::EXPR(tmp);
        this->visit_expr(*(x.m_end));
        a_end = ASRUtils::EXPR(tmp);
        if( x.m_increment != nullptr ) {
            this->visit_expr(*(x.m_increment));
            a_increment = ASRUtils::EXPR(tmp);
        }
        ASR::expr_t** a_values = a_values_vec.p;
        size_t n_values = a_values_vec.size();

        ASR::symbol_t* a_sym = current_scope->resolve_symbol(to_lower(x.m_var));
        if (a_sym == nullptr) {
            throw SemanticError("The implied do loop variable '" +
                to_lower(x.m_var) + "' is not declared", x.base.base.loc);
        }
        ASR::expr_t* a_var = ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc, a_sym));
        tmp = ASR::make_ImpliedDoLoop_t(al, x.base.base.loc, a_values, n_values,
                                            a_var, a_start, a_end, a_increment,
                                            ASRUtils::expr_type(a_start), nullptr);
    }

    void visit_FuncCallOrArray(const AST::FuncCallOrArray_t &x) {
        SymbolTable *scope = current_scope;
        std::string var_name = to_lower(x.m_func);
        ASR::symbol_t *v = nullptr;
        ASR::expr_t *v_expr = nullptr;
        // If this is a type bound procedure (in a class) it won't be in the
        // main symbol table. Need to check n_member.
        if (x.n_member >= 1) {
            visit_NameUtil(x.m_member, x.n_member - 1,
                x.m_member[x.n_member - 1].m_name, x.base.base.loc);
            v_expr = ASRUtils::EXPR(tmp);
            v = resolve_deriv_type_proc(x.base.base.loc, var_name,
                    to_lower(x.m_member[x.n_member - 1].m_name),
                    ASRUtils::type_get_past_pointer(ASRUtils::expr_type(v_expr)), scope);
            v = ASRUtils::import_class_procedure(al, x.base.base.loc, v, current_scope);
        } else {
            v = current_scope->resolve_symbol(var_name);
        }
        if (!v) {
            if (var_name == "float" || var_name == "dble") {
                Vec<ASR::call_arg_t> args;
                visit_expr_list(x.m_args, x.n_args, args);
                tmp = handle_intrinsic_float(al, args, x.base.base.loc);
                return;
            }
            bool is_function = true;
            v = intrinsic_as_node(x, is_function);
            if( !is_function ) {
                return;
            }
            if (compiler_options.implicit_interface && is_function && !v) {
                // Function Call is not defined in this case.
                // We need to create an interface and add the Function into
                // the symbol table.
                create_implicit_interface_function(x, var_name, true);
                v = current_scope->resolve_symbol(var_name);
                LCOMPILERS_ASSERT(v!=nullptr);
            }
        }
        if (compiler_options.implicit_interface
                && ASR::is_a<ASR::Variable_t>(*v)
                && (!ASRUtils::is_array(ASRUtils::symbol_type(v)))
                && (!ASRUtils::is_character(*ASR::down_cast<ASR::Variable_t>(v)->m_type))) {
            // If implicit interface is allowed, we have to handle the
            // following case here:
            // real :: x
            // print *, x(5)
            // Which is a function call.
            // We remove "x" from the symbol table and instead recreate it.
            // We use the type of the old "x" as the return value type.
            // FIXME: for now we drop the old type
            current_scope->erase_symbol(var_name);
            create_implicit_interface_function(x, var_name, true);
            v = current_scope->resolve_symbol(var_name);
            LCOMPILERS_ASSERT(v!=nullptr);
        }
        ASR::symbol_t *f2 = ASRUtils::symbol_get_past_external(v);
        if (ASR::is_a<ASR::Function_t>(*f2)) {
            ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(f2);
            if (ASRUtils::is_intrinsic_procedure(f)) {
                if (intrinsic_module_procedures_as_asr_nodes.find(var_name) != intrinsic_module_procedures_as_asr_nodes.end()) {
                    if (var_name == "c_loc") {
                        tmp = create_PointerToCptr(x);
                    } else {
                        LCOMPILERS_ASSERT(false)
                    }
                    return;
                }
            }
        }
        if (ASR::is_a<ASR::Function_t>(*f2) ||
            ASR::is_a<ASR::GenericProcedure_t>(*f2) ||
            (ASR::is_a<ASR::Variable_t>(*f2) &&
            ASR::is_a<ASR::FunctionType_t>(*ASRUtils::symbol_type(f2))) ) {
            if (ASRUtils::is_intrinsic_symbol(f2)) {
                // Here we handle all intrinsic functions that are implemented
                // in Fortran, but have different interface (API), e.g.,
                // the `kind` argument is handled differently, such as `floor`.
                // In these cases we have to handle them here, since we need
                // to process the arguments ourselves, not via comparison
                // with the `floor` implementation.
                if (var_name == "floor") {
                    if (ASR::is_a<ASR::ExternalSymbol_t>(*v)) {
                        // The above check is needed to ensure we are in an
                        // intrinsic module and calling an external, that is, we
                        // skip a locally defined `floor` function in an
                        // intrinsic module
                        ASR::ExternalSymbol_t *p = ASR::down_cast<ASR::ExternalSymbol_t>(v);
                        ASR::symbol_t *f2 = ASR::down_cast<ASR::ExternalSymbol_t>(v)->m_external;
                        if (ASR::is_a<ASR::GenericProcedure_t>(*f2)) {
                            LCOMPILERS_ASSERT(std::string(ASR::down_cast<ASR::GenericProcedure_t>(f2)->m_name) == "floor")
                            tmp = create_Floor(x, p, v);
                            return;
                        }
                    }
                } else if (var_name == "not") {
                    Vec<ASR::call_arg_t> args;
                    visit_expr_list(x.m_args, x.n_args, args);
                    LCOMPILERS_ASSERT(args.size() == 1);
                    ASR::expr_t* operand = args[0].m_value;
                    ASR::expr_t* value = nullptr;
                    ASR::ttype_t* operand_type = ASRUtils::expr_type(operand);
                    if (ASRUtils::is_integer(*operand_type)) {
                        if (ASRUtils::expr_value(operand) != nullptr) {
                            int64_t op_value = ASR::down_cast<ASR::IntegerConstant_t>(
                                                   ASRUtils::expr_value(operand))
                                                   ->m_n;
                            value = ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(
                                al, x.base.base.loc, ~op_value, operand_type));
                        }
                        tmp = ASR::make_IntegerBitNot_t(
                            al, x.base.base.loc, operand, operand_type, value);
                        return;
                    } else {
                        throw SemanticError("Argument of `not` intrinsic must be INTEGER",
                                            x.base.base.loc);
                    }
                }
            }
            Vec<ASR::call_arg_t> args;
            Vec<ASR::call_arg_t> args_with_mdt;
            visit_expr_list(x.m_args, x.n_args, args);
            if (x.n_member >= 1) {
                args_with_mdt.reserve(al, x.n_args + 1);
                ASR::call_arg_t v_expr_call_arg;
                v_expr_call_arg.loc = v_expr->base.loc;
                v_expr_call_arg.m_value = v_expr;
                args_with_mdt.push_back(al, v_expr_call_arg);
                for( size_t i = 0; i < args.size(); i++ ) {
                    args_with_mdt.push_back(al, args[i]);
                }
            }
            if (x.n_keywords > 0) {
                if (ASR::is_a<ASR::Function_t>(*f2)) {
                    ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(f2);
                    diag::Diagnostics diags;
                    visit_kwargs(args, x.m_keywords, x.n_keywords,
                        f->m_args, f->n_args, x.base.base.loc, f,
                        diags, x.n_member);
                    if( diags.has_error() ) {
                        diag.diagnostics.insert(diag.diagnostics.end(),
                            diags.diagnostics.begin(), diags.diagnostics.end());
                        throw SemanticAbort();
                    }
                } else {
                    LCOMPILERS_ASSERT(ASR::is_a<ASR::GenericProcedure_t>(*f2))
                    ASR::GenericProcedure_t* gp = ASR::down_cast<ASR::GenericProcedure_t>(f2);
                    bool function_found = false;
                    for( int i = 0; i < (int) gp->n_procs; i++ ) {
                        Vec<ASR::call_arg_t> args_copy;
                        args_copy.reserve(al, args.size() + x.n_keywords);
                        for( size_t j = 0; j < args.size(); j++ ) {
                            args_copy.push_back(al, args[j]);
                        }
                        ASR::symbol_t* f4 = gp->m_procs[i];
                        if( !ASR::is_a<ASR::Function_t>(*f4) ) {
                            throw SemanticError(std::string(ASRUtils::symbol_name(f4)) +
                                                " is not a function.",
                                                x.base.base.loc);
                        }
                        ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(f4);
                        diag::Diagnostics diags;
                        visit_kwargs(args_copy, x.m_keywords, x.n_keywords,
                            f->m_args, f->n_args, x.base.base.loc, f,
                            diags, x.n_member);
                        if( diags.has_error() ) {
                            continue ;
                        }
                        int idx = ASRUtils::select_generic_procedure(args_copy, *gp, x.base.base.loc,
                                        [&](const std::string &msg, const Location &loc) { throw SemanticError(msg, loc); },
                                        false);
                        if( idx == i ) {
                            function_found = true;
                            for( size_t j = args.size(); j < args_copy.size(); j++ ) {
                                args.push_back(al, args_copy[j]);
                            }
                            break;
                        }
                    }
                    if( !function_found ) {
                        bool is_function = true;
                        v = intrinsic_as_node(x, is_function);
                        if( !is_function ) {
                            return ;
                        }
                    }
                    if( v == nullptr ) {
                        throw SemanticError("Unable to find a function to bind for generic procedure call, " + std::string(gp->m_name),
                                            x.base.base.loc);
                    }
                }
            }
            // check whether the requirement function is included in the template
            if (ASR::is_a<ASR::Function_t>(*v)) {
                ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(v);
                if (f->m_is_restriction) {
                    if (!is_template) {
                        throw SemanticError("A requirement function must be called from a template",
                                            x.base.base.loc);
                    }
                    bool requirement_found = false;
                    std::string f_name = f->m_name;
                    if (called_requirement.find(f_name) != called_requirement.end()
                            && ASR::is_a<ASR::symbol_t>(*called_requirement[f_name])) {
                        requirement_found = true;
                    }
                    if (!requirement_found) {
                        throw SemanticError("The template did not declare this requirement function",
                                            x.base.base.loc);
                    }
                    rt_vec.push_back(v);
                }
            }
            if (x.n_member >= 1) {
                tmp = create_FunctionCallWithASTNode(x, v, args_with_mdt);
            } else {
                tmp = create_FunctionCallWithASTNode(x, v, args);
            }
        } else {
            switch (f2->type) {
            case(ASR::symbolType::Variable): {
                // TODO: Make create_StringRef for character (non-array) variables.
                tmp = create_ArrayRef(x.base.base.loc, x.m_args, x.n_args, v_expr, v, f2);
                break;
            }
            case(ASR::symbolType::StructType):
                tmp = create_DerivedTypeConstructor(x.base.base.loc, x.m_args, x.n_args, v); break;
            case(ASR::symbolType::ClassProcedure):
                tmp = create_ClassProcedure(x.base.base.loc, x.m_args, x.n_args, v, v_expr); break;
            default: throw SemanticError("Symbol '" + var_name
                        + "' is not a function or an array", x.base.base.loc);
            }
        }
    }

    ASR::symbol_t* resolve_intrinsic_function(const Location &loc, const std::string &remote_sym) {
        if (!intrinsic_procedures.is_intrinsic(remote_sym)) {
            if (compiler_options.implicit_interface) {
                return nullptr;
            } else {
                throw SemanticError("Function '" + remote_sym + "' not found"
                    " or not implemented yet (if it is intrinsic)",
                    loc);
            }
        }
        std::string module_name = intrinsic_procedures.get_module(remote_sym, loc);

        SymbolTable *tu_symtab = ASRUtils::get_tu_symtab(current_scope);
        LCompilers::PassOptions pass_options;
        pass_options.runtime_library_dir = compiler_options.runtime_library_dir;
        pass_options.mod_files_dir = compiler_options.mod_files_dir;
        pass_options.include_dirs = compiler_options.include_dirs;

        ASR::Module_t *m = ASRUtils::load_module(al, tu_symtab, module_name,
                loc, true, pass_options, true,
                [&](const std::string &msg, const Location &loc) { throw SemanticError(msg, loc); }
                );

        ASR::symbol_t *t = m->m_symtab->resolve_symbol(remote_sym);
        if (!t) {
            throw SemanticError("The symbol '" + remote_sym
                + "' not found in the module '" + module_name + "'",
                loc);
        } else if (! (ASR::is_a<ASR::GenericProcedure_t>(*t)
                    || ASR::is_a<ASR::Function_t>(*t)
                    )) {
            throw SemanticError("The symbol '" + remote_sym
                + "' found in the module '" + module_name + "', "
                + "but it is not a function, subroutine or a generic procedure.",
                loc);
        }
        char *fn_name = ASRUtils::symbol_name(t);
        ASR::asr_t *fn = ASR::make_ExternalSymbol_t(
            al, t->base.loc,
            /* a_symtab */ current_scope,
            /* a_name */ fn_name,
            t,
            m->m_name, nullptr, 0, fn_name,
            ASR::accessType::Private
            );
        std::string sym = fn_name;

        current_scope->add_symbol(sym, ASR::down_cast<ASR::symbol_t>(fn));
        ASR::symbol_t *v = ASR::down_cast<ASR::symbol_t>(fn);
        if (current_module) {
            // We are in body visitor
            // Add the module `m` to current module dependencies
            Vec<char*> vec;
            vec.from_pointer_n_copy(al, current_module->m_dependencies,
                        current_module->n_dependencies);
            if (!present(vec, m->m_name)) {
                vec.push_back(al, m->m_name);
                current_module->m_dependencies = vec.p;
                current_module->n_dependencies = vec.size();
            }
        } else {
            // We are in the symtab visitor or body visitor (the
            // current_module_dependencies is not used in body visitor)
            if (!present(current_module_dependencies, m->m_name)) {
                current_module_dependencies.push_back(al, m->m_name);
            }
        }
        return v;
    }

    bool is_integer(ASR::ttype_t &t) {
        return ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_pointer(&t));
    }

    bool is_real(ASR::ttype_t &t) {
        return ASR::is_a<ASR::Real_t>(*ASRUtils::type_get_past_pointer(&t));
    }

    bool assignment_types_agree(ASR::ttype_t *target, ASR::ttype_t *value) {
        // For now we will just check basic type mismatch
        if (target->type == value->type) {
            return true;
        }
        if (is_integer(*target) && is_integer(*value)) {
            return true;
        }
        if (is_real(*target) && is_real(*value)) {
            return true;
        }
        return false;
    }

    void visit_BinOp2(Allocator &al, const AST::BinOp_t &x,
                                    ASR::expr_t *&left, ASR::expr_t *&right,
                                    ASR::asr_t *&asr, std::string& intrinsic_op_name,
                                    SymbolTable* curr_scope) {
        ASR::binopType op;
        switch (x.m_op) {
            case (AST::Add):
                op = ASR::Add;
                break;
            case (AST::Sub):
                op = ASR::Sub;
                break;
            case (AST::Mul):
                op = ASR::Mul;
                break;
            case (AST::Div):
                op = ASR::Div;
                break;
            case (AST::Pow):
                op = ASR::Pow;
                break;
            // Fix compiler warning:
            default: {
                LCOMPILERS_ASSERT(false);
                op = ASR::binopType::Pow;
            }
        }

        // Cast LHS or RHS if necessary
        ASR::ttype_t *left_type = ASRUtils::expr_type(left);
        ASR::ttype_t *right_type = ASRUtils::expr_type(right);
        ASR::expr_t *overloaded = nullptr;
        if ( ASRUtils::use_overloaded(left, right, op,
            intrinsic_op_name, curr_scope, asr, al,
            x.base.base.loc, current_function_dependencies,
            current_module_dependencies,
            [&](const std::string &msg, const Location &loc) { throw SemanticError(msg, loc); }) ) {
            overloaded = ASRUtils::EXPR(asr);
        }

        ASR::expr_t **conversion_cand = &left;
        ASR::ttype_t *source_type = left_type;
        ASR::ttype_t *dest_type = right_type;
        if(!ASRUtils::is_generic(*left_type) && !ASRUtils::is_generic(*right_type)){
            ImplicitCastRules::find_conversion_candidate(&left, &right, left_type,
                                                    right_type, conversion_cand,
                                                    &source_type, &dest_type);
        }

        ImplicitCastRules::set_converted_value(al, x.base.base.loc, conversion_cand,
                                            source_type, dest_type);

        if (!ASRUtils::check_equal_type(ASRUtils::expr_type(left),
                                    ASRUtils::expr_type(right))) {
            std::string ltype = ASRUtils::type_to_str(ASRUtils::expr_type(left));
            std::string rtype = ASRUtils::type_to_str(ASRUtils::expr_type(right));
            diag.add(Diagnostic(
                "Type mismatch in binary operator, the types must be compatible",
                Level::Error, Stage::Semantic, {
                    Label("type mismatch (" + ltype + " and " + rtype + ")",
                            {left->base.loc, right->base.loc})
                })
            );
            throw SemanticAbort();
        }
        ASR::expr_t *value = nullptr;

        if (ASRUtils::is_integer(*dest_type)) {

            if (ASRUtils::expr_value(left) != nullptr && ASRUtils::expr_value(right) != nullptr) {
                int64_t left_value = ASR::down_cast<ASR::IntegerConstant_t>(
                                        ASRUtils::expr_value(left))
                                        ->m_n;
                int64_t right_value = ASR::down_cast<ASR::IntegerConstant_t>(
                                        ASRUtils::expr_value(right))
                                        ->m_n;
                int64_t result;
                switch (op) {
                    case (ASR::Add):
                        result = left_value + right_value;
                        break;
                    case (ASR::Sub):
                        result = left_value - right_value;
                        break;
                    case (ASR::Mul):
                        result = left_value * right_value;
                        break;
                    case (ASR::Div):
                        result = left_value / right_value;
                        break;
                    case (ASR::Pow):
                        result = std::pow(left_value, right_value);
                        break;
                    // Reconsider
                    default: {
                        LCOMPILERS_ASSERT(false);
                        op = ASR::binopType::Pow;
                    }
                }
                value = ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(
                    al, x.base.base.loc, result, dest_type));
            }

            asr = ASR::make_IntegerBinOp_t(al, x.base.base.loc, left, op, right, dest_type, value);

        } else if (ASRUtils::is_generic(*left_type) || ASRUtils::is_generic(*right_type)){
            asr = ASR::make_TemplateBinOp_t(al, x.base.base.loc, left, op, right, dest_type, value);
        } else if (ASRUtils::is_real(*dest_type)) {

            if (ASRUtils::expr_value(left) != nullptr && ASRUtils::expr_value(right) != nullptr) {
                double left_value = ASR::down_cast<ASR::RealConstant_t>(
                                        ASRUtils::expr_value(left))
                                        ->m_r;
                double right_value = ASR::down_cast<ASR::RealConstant_t>(
                                        ASRUtils::expr_value(right))
                                        ->m_r;
                double result;
                switch (op) {
                    case (ASR::Add):
                        result = left_value + right_value;
                        break;
                    case (ASR::Sub):
                        result = left_value - right_value;
                        break;
                    case (ASR::Mul):
                        result = left_value * right_value;
                        break;
                    case (ASR::Div):
                        result = left_value / right_value;
                        break;
                    case (ASR::Pow):
                        result = std::pow(left_value, right_value);
                        break;
                    // Reconsider
                    default: {
                        LCOMPILERS_ASSERT(false);
                        op = ASR::binopType::Pow;
                    }
                }
                value = ASR::down_cast<ASR::expr_t>(
                    ASR::make_RealConstant_t(al, x.base.base.loc, result, dest_type));
            }

            asr = ASR::make_RealBinOp_t(al, x.base.base.loc, left, op, right, dest_type, value);

        } else if (ASRUtils::is_complex(*dest_type)) {

            if (ASRUtils::expr_value(left) != nullptr && ASRUtils::expr_value(right) != nullptr) {
                ASR::ComplexConstant_t *left0
                    = ASR::down_cast<ASR::ComplexConstant_t>(
                            ASRUtils::expr_value(left));
                ASR::ComplexConstant_t *right0
                    = ASR::down_cast<ASR::ComplexConstant_t>(
                            ASRUtils::expr_value(right));
                std::complex<double> left_value(left0->m_re, left0->m_im);
                std::complex<double> right_value(right0->m_re, right0->m_im);
                std::complex<double> result;
                switch (op) {
                    case (ASR::Add):
                        result = left_value + right_value;
                        break;
                    case (ASR::Sub):
                        result = left_value - right_value;
                        break;
                    case (ASR::Mul):
                        result = left_value * right_value;
                        break;
                    case (ASR::Div):
                        result = left_value / right_value;
                        break;
                    case (ASR::Pow):
                        result = std::pow(left_value, right_value);
                        break;
                    // Reconsider
                    default: {
                        LCOMPILERS_ASSERT(false);
                        op = ASR::binopType::Pow;
                    }
                }
                value = ASR::down_cast<ASR::expr_t>(
                    ASR::make_ComplexConstant_t(al, x.base.base.loc,
                        std::real(result), std::imag(result), dest_type));
            }

            asr = ASR::make_ComplexBinOp_t(al, x.base.base.loc, left, op, right, dest_type, value);

        }

        if (overloaded != nullptr) {
            asr = ASR::make_OverloadedBinOp_t(al, x.base.base.loc, left, op, right, dest_type, value, overloaded);
        }

    }


    void visit_BinOp(const AST::BinOp_t &x) {
        this->visit_expr(*x.m_left);
        ASR::expr_t *left = ASRUtils::EXPR(tmp);
        this->visit_expr(*x.m_right);
        ASR::expr_t *right = ASRUtils::EXPR(tmp);
        visit_BinOp2(al, x, left, right, tmp, binop2str[x.m_op], current_scope);
    }

    void visit_DefBinOp(const AST::DefBinOp_t &x) {
        this->visit_expr(*x.m_left);
        ASR::expr_t *left = ASRUtils::EXPR(tmp);
        this->visit_expr(*x.m_right);
        ASR::expr_t *right = ASRUtils::EXPR(tmp);

        ASR::ttype_t *left_type = ASRUtils::expr_type(left);
        ASR::ttype_t *right_type = ASRUtils::expr_type(right);

        ASR::StructType_t *left_struct = nullptr;
        if ( ASR::is_a<ASR::Struct_t>(*left_type) ) {
            left_struct = ASR::down_cast<ASR::StructType_t>(
                ASRUtils::symbol_get_past_external(ASR::down_cast<ASR::Struct_t>(
                left_type)->m_derived_type));
        } else if ( ASR::is_a<ASR::Class_t>(*left_type) ) {
            left_struct = ASR::down_cast<ASR::StructType_t>(
                ASRUtils::symbol_get_past_external(ASR::down_cast<ASR::Class_t>(
                left_type)->m_class_type));
        }

        ASR::symbol_t* sym = current_scope->resolve_symbol(x.m_op);
        ASR::symbol_t *op_sym = ASRUtils::symbol_get_past_external(sym);
        if ( left_struct != nullptr && op_sym == nullptr) {
            op_sym = left_struct->m_symtab->resolve_symbol(
                "~def_op~" + std::string(x.m_op));
        }
        if (op_sym == nullptr) {
            throw SemanticError("`" + std::string(x.m_op)
                + "` is not defined in the struct: `" + left_struct->m_name
                + "`", x.base.base.loc);
        }

        ASR::CustomOperator_t* gen_proc = ASR::down_cast<ASR::CustomOperator_t>(op_sym);
        LCOMPILERS_ASSERT(gen_proc->n_procs == 1)
        ASR::symbol_t* proc;
        if ( ASR::is_a<ASR::ClassProcedure_t>(*gen_proc->m_procs[0]) ) {
            proc =  ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::ClassProcedure_t>(
                gen_proc->m_procs[0])->m_proc);
        } else {
            proc = gen_proc->m_procs[0];
        }
        switch(proc->type) {
            case ASR::symbolType::Function: {
                ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(proc);
                std::string matched_func_name = "";
                if( func->n_args == 2 ) {
                    ASR::ttype_t* left_arg_type = ASRUtils::expr_type(func->m_args[0]);
                    ASR::ttype_t* right_arg_type = ASRUtils::expr_type(func->m_args[1]);
                    if( (left_arg_type->type == left_type->type &&
                            right_arg_type->type == right_type->type)
                        || (ASR::is_a<ASR::Class_t>(*left_arg_type) &&
                            ASR::is_a<ASR::Struct_t>(*left_type))
                        || (ASR::is_a<ASR::Class_t>(*right_arg_type) &&
                            ASR::is_a<ASR::Struct_t>(*right_type)) ) {
                        Vec<ASR::call_arg_t> a_args;
                        a_args.reserve(al, 2);
                        ASR::call_arg_t left_call_arg, right_call_arg;

                        left_call_arg.loc = left->base.loc;
                        left_call_arg.m_value = left;
                        a_args.push_back(al, left_call_arg);

                        right_call_arg.loc = right->base.loc;
                        right_call_arg.m_value = right;
                        a_args.push_back(al, right_call_arg);

                        std::string func_name = to_lower(func->m_name);
                        if( current_scope->resolve_symbol(func_name) ) {
                            matched_func_name = func_name;
                        } else {
                            std::string mangled_name = func_name + "@" + std::string(x.m_op);
                            matched_func_name = mangled_name;
                        }
                        ASR::symbol_t* a_name = current_scope->resolve_symbol(matched_func_name);
                        if( a_name == nullptr ) {
                            throw SemanticError("Unable to resolve matched function: `"
                                + matched_func_name + "` for defined binary operation",
                                x.base.base.loc);
                        }
                        ASR::ttype_t *return_type = nullptr;
                        if( func->m_elemental && func->n_args == 1
                                && ASRUtils::is_array(ASRUtils::expr_type(a_args[0].m_value)) ) {
                            return_type = ASRUtils::duplicate_type(
                                al, ASRUtils::expr_type(a_args[0].m_value));
                        } else {
                            return_type = ASRUtils::expr_type(func->m_return_var);
                        }
                        current_function_dependencies.insert(matched_func_name);
                        if( ASR::is_a<ASR::ExternalSymbol_t>(*a_name) ) {
                            current_module_dependencies.push_back(al,
                                ASR::down_cast<ASR::ExternalSymbol_t>(a_name)->m_module_name);
                        }
                        tmp = ASR::make_FunctionCall_t(al, x.base.base.loc,
                            a_name, sym, a_args.p, 2, return_type,
                            nullptr, nullptr);
                    }
                }
                break;
            }
            default: {
                throw SemanticError("Only function can be used in the defined binary operators",
                                    proc->base.loc);
            }
        }
    }

    void visit_BoolOp(const AST::BoolOp_t &x) {
        this->visit_expr(*x.m_left);
        ASR::expr_t *left = ASRUtils::EXPR(tmp);
        this->visit_expr(*x.m_right);
        ASR::expr_t *right = ASRUtils::EXPR(tmp);
        CommonVisitorMethods::visit_BoolOp(al, x, left, right, tmp, diag);
    }

    void visit_StrOp(const AST::StrOp_t &x) {
        this->visit_expr(*x.m_left);
        ASR::expr_t *left = ASRUtils::EXPR(tmp);
        this->visit_expr(*x.m_right);
        ASR::expr_t *right = ASRUtils::EXPR(tmp);
        CommonVisitorMethods::visit_StrOp(al, x, left, right, tmp);
    }

    void visit_UnaryOp(const AST::UnaryOp_t &x) {
        this->visit_expr(*x.m_operand);
        ASR::expr_t *operand = ASRUtils::EXPR(tmp);
        CommonVisitorMethods::visit_UnaryOp(al, x, operand, tmp);
    }

    void visit_Compare(const AST::Compare_t &x) {
        this->visit_expr(*x.m_left);
        ASR::expr_t *left = ASRUtils::EXPR(tmp);
        this->visit_expr(*x.m_right);
        ASR::expr_t *right = ASRUtils::EXPR(tmp);
        CommonVisitorMethods::visit_Compare(al, x, left, right, tmp,
                                            cmpop2str[x.m_op], current_scope,
                                            current_function_dependencies,
                                            current_module_dependencies);
    }

    void visit_Parenthesis(const AST::Parenthesis_t &x) {
        this->visit_expr(*x.m_operand);
    }

    void visit_Logical(const AST::Logical_t &x) {
        ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Logical_t(al, x.base.base.loc,
                4, nullptr, 0));
        tmp = ASR::make_LogicalConstant_t(al, x.base.base.loc, x.m_value, type);
    }

    void visit_String(const AST::String_t &x) {
        int s_len = strlen(x.m_s);
        ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Character_t(al, x.base.base.loc,
                1, s_len, nullptr, nullptr, 0));
        tmp = ASR::make_StringConstant_t(al, x.base.base.loc, x.m_s, type);
    }

    void visit_BOZ(const AST::BOZ_t& x) {
        std::string s = std::string(x.m_s);
        int base = -1;
        ASR::integerbozType boz_type;
        if( s[0] == 'b' || s[0] == 'B' ) {
            boz_type = ASR::integerbozType::Binary;
            base = 2;
        } else if( s[0] == 'z' || s[0] == 'Z' ) {
            boz_type = ASR::integerbozType::Hex;
            base = 16;
        } else if( s[0] == 'o' || s[0] == 'O' ) {
            boz_type = ASR::integerbozType::Octal;
            base = 8;
        } else {
            throw SemanticError(R"""(Only 'b', 'o' and 'z'
                                are accepted as prefixes of
                                BOZ literal constants.)""",
                                x.base.base.loc);
        }
        std::string boz_str = s.substr(2, s.size() - 2);
        int boz_int = std::stoi(boz_str, nullptr, base);
        tmp = ASR::make_IntegerBOZ_t(al, x.base.base.loc, boz_int,
                                boz_type, nullptr);
    }

    void visit_Num(const AST::Num_t &x) {
        int ikind = 4;
        if (x.m_kind) {
            ikind = std::atoi(x.m_kind);
            if (ikind == 0) {
                std::string var_name = x.m_kind;
                ASR::symbol_t *v = current_scope->resolve_symbol(var_name);
                if (v) {
                    const ASR::symbol_t *v3 = ASRUtils::symbol_get_past_external(v);
                    if (ASR::is_a<ASR::Variable_t>(*v3)) {
                        ASR::Variable_t *v2 = ASR::down_cast<ASR::Variable_t>(v3);
                        if (v2->m_value) {
                            if (ASR::is_a<ASR::IntegerConstant_t>(*v2->m_value)) {
                                ikind = ASR::down_cast<ASR::IntegerConstant_t>(v2->m_value)->m_n;
                            } else {
                                throw SemanticError("Variable '" + var_name + "' is constant but not an integer",
                                    x.base.base.loc);
                            }
                        } else {
                            throw SemanticError("Variable '" + var_name + "' is not constant",
                                x.base.base.loc);
                        }
                    } else {
                        throw SemanticError("Symbol '" + var_name + "' is not a variable",
                            x.base.base.loc);
                    }
                } else {
                    throw SemanticError("Variable '" + var_name + "' not declared",
                        x.base.base.loc);
                }
            }
        }
        ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Integer_t(al,
                x.base.base.loc, ikind, nullptr, 0));
        if (BigInt::is_int_ptr(x.m_n)) {
            std::string str_repr = BigInt::largeint_to_string(x.m_n);
            if( !BigInt::is_int64(str_repr) ) {
                throw SemanticError("Integer constants larger than 2^64-1 are not implemented yet",
                                    x.base.base.loc);
            }
            int64_t m_n = std::stoll(str_repr);
            tmp = ASR::make_IntegerConstant_t(al, x.base.base.loc,
                                                m_n, type);
        } else {
            tmp = ASR::make_IntegerConstant_t(al, x.base.base.loc, x.m_n, type);
        }
    }

    void visit_Real(const AST::Real_t &x) {
        double r = ASRUtils::extract_real(x.m_n);
        char* s_kind;
        int r_kind = ASRUtils::extract_kind_str(x.m_n, s_kind);
        if (r_kind == 0) {
            std::string var_name = s_kind;
            ASR::symbol_t *v = current_scope->resolve_symbol(var_name);
            if (v) {
                const ASR::symbol_t *v3 = ASRUtils::symbol_get_past_external(v);
                if (ASR::is_a<ASR::Variable_t>(*v3)) {
                    ASR::Variable_t *v2 = ASR::down_cast<ASR::Variable_t>(v3);
                    if (v2->m_value) {
                        if (ASR::is_a<ASR::IntegerConstant_t>(*v2->m_value)) {
                            r_kind = ASR::down_cast<ASR::IntegerConstant_t>(v2->m_value)->m_n;
                        } else {
                            throw SemanticError("Variable '" + var_name + "' is constant but not an integer",
                                x.base.base.loc);
                        }
                    } else {
                        throw SemanticError("Variable '" + var_name + "' is not constant",
                            x.base.base.loc);
                    }
                } else {
                    throw SemanticError("Symbol '" + var_name + "' is not a variable",
                        x.base.base.loc);
                }
            } else {
                throw SemanticError("Variable '" + var_name + "' not declared",
                    x.base.base.loc);
            }
        }
        ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Real_t(al, x.base.base.loc,
                r_kind, nullptr, 0));
        tmp = ASR::make_RealConstant_t(al, x.base.base.loc, r, type);
    }

    void visit_Complex(const AST::Complex_t &x) {
        this->visit_expr(*x.m_re);
        ASR::expr_t *re = ASRUtils::EXPR(tmp);
        ASR::expr_t *re_value = ASRUtils::expr_value(re);
        int a_kind_r = ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(re));
        this->visit_expr(*x.m_im);
        ASR::expr_t *im = ASRUtils::EXPR(tmp);
        ASR::expr_t *im_value = ASRUtils::expr_value(im);
        int a_kind_i = ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(im));
        // TODO: Add semantic checks what type are allowed
        ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Complex_t(al, x.base.base.loc,
                std::max(a_kind_r, a_kind_i), nullptr, 0));
        ASR::expr_t *value = nullptr;
        if (re_value && im_value) {
            double re_double;
            if (ASR::is_a<ASR::RealConstant_t>(*re_value)) {
                re_double = ASR::down_cast<ASR::RealConstant_t>(re_value)->m_r;
            } else if (ASR::is_a<ASR::IntegerConstant_t>(*re_value)) {
                re_double = ASR::down_cast<ASR::IntegerConstant_t>(re_value)->m_n;
            } else {
                throw SemanticError("Argument `a` in a ComplexConstructor `(a,b)` must be either Real or Integer", x.base.base.loc);
            }
            double im_double;
            if (ASR::is_a<ASR::RealConstant_t>(*im_value)) {
                im_double = ASR::down_cast<ASR::RealConstant_t>(im_value)->m_r;
            } else if (ASR::is_a<ASR::IntegerConstant_t>(*im_value)) {
                im_double = ASR::down_cast<ASR::IntegerConstant_t>(im_value)->m_n;
            } else {
                throw SemanticError("Argument `b` in a ComplexConstructor `(a,b)` must be either Real or Integer", x.base.base.loc);
            }
            value = ASR::down_cast<ASR::expr_t>(ASR::make_ComplexConstant_t(al, x.base.base.loc, re_double, im_double, type));
        }
        tmp = ASR::make_ComplexConstructor_t(al, x.base.base.loc,
                re, im, type, value);
    }

    void visit_Procedure(const AST::Procedure_t&) {
        // To Be Implemented
    }

    Vec<ASR::expr_t*> visit_expr_list(AST::fnarg_t *ast_list, size_t n) {
        Vec<ASR::expr_t*> asr_list;
        asr_list.reserve(al, n);
        for (size_t i=0; i<n; i++) {
            LCOMPILERS_ASSERT(ast_list[i].m_end != nullptr);
            this->visit_expr(*ast_list[i].m_end);
            ASR::expr_t *expr = ASRUtils::EXPR(tmp);
            asr_list.push_back(al, expr);
        }
        return asr_list;
    }

    void visit_expr_list(AST::fnarg_t *ast_list, size_t n, Vec<ASR::call_arg_t>& call_args) {
        call_args.reserve(al, n);
        for (size_t i = 0; i < n; i++) {
            LCOMPILERS_ASSERT(ast_list[i].m_end != nullptr);
            this->visit_expr(*ast_list[i].m_end);
            ASR::expr_t *expr = ASRUtils::EXPR(tmp);
            ASR::call_arg_t call_arg;
            call_arg.loc = expr->base.loc;
            call_arg.m_value = expr;
            call_args.push_back(al, call_arg);
        }
    }

    std::vector<std::string> convert_fn_args_to_string(
            ASR::expr_t **expr_list, size_t n, const Location &loc) {
        std::vector<std::string> result;
        for (size_t i=0; i < n; i++) {
            ASR::Variable_t *v = ASRUtils::EXPR2VAR(expr_list[i]);
            if (v->m_presence == ASR::presenceType::Optional) {
                SemanticError("Keyword arguments with optional arguments are not implemented yet", loc);
            }
            result.push_back(v->m_name);
        }
        return result;
    }

    template <typename T>
    void visit_kwargs(Vec<ASR::call_arg_t>& args, AST::keyword_t *kwargs, size_t n,
                ASR::expr_t **fn_args, size_t fn_n_args, const Location &loc, T* fn,
                diag::Diagnostics& diag, size_t type_bound=0) {
        size_t n_args = args.size();
        std::string fn_name = fn->m_name;
        std::vector<std::string> optional_args;
        std::vector<int> optional_args_idx;
        for( auto itr = fn->m_symtab->get_scope().begin(); itr != fn->m_symtab->get_scope().end();
             itr++ ) {
            ASR::symbol_t* fn_sym = itr->second;
            if( ASR::is_a<ASR::Variable_t>(*fn_sym) ) {
                ASR::Variable_t* fn_var = ASR::down_cast<ASR::Variable_t>(fn_sym);
                if( fn_var->m_presence == ASR::presenceType::Optional ) {
                    optional_args.push_back(itr->first);
                    for( size_t i = 0; i < fn_n_args; i++ ) {
                        if( ASR::down_cast<ASR::Var_t>(fn_args[i])->m_v == fn_sym ) {
                            optional_args_idx.push_back(i);
                            break;
                        }
                    }
                }
            }
        }

        if (n_args + n > fn_n_args) {
            diag.semantic_error_label(
                "Procedure accepts " + std::to_string(fn_n_args)
                + " arguments, but " + std::to_string(n_args + n)
                + " were provided",
                {loc},
                "incorrect number of arguments to " + std::string(fn_name)
            );
            return ;
        }

        std::vector<std::string> fn_args2 = convert_fn_args_to_string(
                fn_args, fn_n_args, loc);

        for (size_t i=0; i < n; i++) {
            std::string str = std::string(kwargs[i].m_arg);
            if( std::find(optional_args.begin(), optional_args.end(), str) == optional_args.end() ) {
                ASR::call_arg_t empty_arg;
                Location loc;
                loc.first = 1, loc.last = 1;
                empty_arg.loc = loc;
                empty_arg.m_value = nullptr;
                args.push_back(al, empty_arg);
            }
        }


        size_t offset = args.size();
        for (size_t i = 0; i < fn_n_args - offset - type_bound; i++) {
            ASR::call_arg_t call_arg;
            call_arg.loc = loc;
            call_arg.m_value = nullptr;
            args.push_back(al, call_arg);
        }
        for (size_t i = 0; i < n; i++) {
            this->visit_expr(*kwargs[i].m_value);
            ASR::expr_t *expr = ASRUtils::EXPR(tmp);
            std::string name = to_lower(kwargs[i].m_arg);
            auto search_optional = std::find(optional_args.begin(), optional_args.end(), name);
            if( search_optional != optional_args.end() ) {
                size_t kwarg_idx = std::distance(optional_args.begin(), search_optional);
                if (args[kwarg_idx + offset].m_value != nullptr) {
                    diag.semantic_error_label(
                        "Keyword argument is already specified as another keyword argument",
                        {loc},
                        "`" + name + "` keyword argument is already specified.");
                    return ;
                }
                args.p[kwarg_idx + offset].m_value = expr;
                args.p[kwarg_idx + offset].loc = expr->base.loc;
            } else {
                auto search = std::find(fn_args2.begin(), fn_args2.end(), name);
                if (search != fn_args2.end()) {
                    size_t idx = std::distance(fn_args2.begin(), search);
                    if (idx < n_args) {
                        diag.semantic_error_label(
                            "Keyword argument is already specified as a non-keyword argument",
                            {loc},
                            "`" + name + "` keyword argument is already specified.");
                        return ;
                    }
                    if (args[idx].m_value != nullptr) {
                        idx = n_args + (idx - offset) - 1;
                        if (args[idx].m_value != nullptr) {
                            diag.semantic_error_label(
                                "Keyword argument is already specified as another keyword argument",
                                {loc},
                                "`" + name + "` keyword argument is already specified.");
                            return ;
                        }
                    }
                    args.p[idx].loc = expr->base.loc;
                    args.p[idx].m_value = expr;
                } else {
                    diag.semantic_error_label(
                        "Keyword argument not found " + name,
                        {loc},
                        name + " keyword argument not found.");
                    return ;
                }
            }
        }

        for (size_t i=0; i < args.size(); i++) {
            if (args[i].m_value == nullptr &&
                std::find(optional_args_idx.begin(), optional_args_idx.end(), i)
                    == optional_args_idx.end()) {
                diag.semantic_error_label(
                    "Argument was not specified",
                    {loc},
                    std::to_string(i) +
                    "-th argument not specified for " + fn_name);
                return ;
            }
        }
    }

    void visit_NameUtil(AST::struct_member_t* x_m_member, size_t x_n_member,
                        char* x_m_id, const Location& loc) {
        if (x_n_member == 0) {
            tmp = resolve_variable(loc, to_lower(x_m_id));
        } else if (x_n_member == 1) {
            if (x_m_member[0].n_args == 0) {
                SymbolTable* scope = current_scope;
                tmp = this->resolve_variable2(loc, to_lower(x_m_id),
                    to_lower(x_m_member[0].m_name), scope);
            } else {
                // TODO: incorporate m_args
                SymbolTable* scope = current_scope;
                tmp = this->resolve_variable2(loc, to_lower(x_m_id),
                    to_lower(x_m_member[0].m_name), scope);
            }
        } else {
            SymbolTable* scope = current_scope;
            tmp = this->resolve_variable2(loc, to_lower(x_m_member[1].m_name),
                                          to_lower(x_m_member[0].m_name), scope);
            ASR::StructInstanceMember_t* tmp2;
            std::uint32_t i;
            for( i = 2; i < x_n_member; i++ ) {
                tmp2 = (ASR::StructInstanceMember_t*) this->resolve_variable2(loc,
                        to_lower(x_m_member[i].m_name), to_lower(x_m_member[i - 1].m_name),
                        scope);
                ASR::ttype_t* tmp2_mem_type = tmp2->m_type;
                ASR::symbol_t* tmp2_m_m_ext = ASRUtils::import_struct_instance_member(al,
                                                    tmp2->m_m, current_scope, tmp2_mem_type);
                tmp = ASR::make_StructInstanceMember_t(al, loc, ASRUtils::EXPR(tmp),
                                                       tmp2_m_m_ext, tmp2_mem_type, nullptr);
            }
            i = x_n_member - 1;
            tmp2 = (ASR::StructInstanceMember_t*) this->resolve_variable2(loc, to_lower(x_m_id),
                        to_lower(x_m_member[i].m_name), scope);
            ASR::ttype_t* tmp2_mem_type = tmp2->m_type;
            ASR::symbol_t* tmp2_m_m_ext = ASRUtils::import_struct_instance_member(al, tmp2->m_m,
                                            current_scope, tmp2_mem_type);
            tmp = ASR::make_StructInstanceMember_t(al, loc, ASRUtils::EXPR(tmp), tmp2_m_m_ext,
                                                   tmp2_mem_type, nullptr);
        }
    }

    void visit_Name(const AST::Name_t &x) {
        visit_NameUtil(x.m_member, x.n_member, x.m_id, x.base.base.loc);
    }


};

} // namespace LCompilers::LFortran

#endif /* LFORTRAN_SEMANTICS_AST_COMMON_VISITOR_H */
