#ifndef LFORTRAN_SEMANTICS_AST_COMMON_VISITOR_H
#define LFORTRAN_SEMANTICS_AST_COMMON_VISITOR_H

#include <lfortran/asr.h>
#include <lfortran/ast.h>
#include <lfortran/bigint.h>
#include <lfortran/string_utils.h>
#include <lfortran/utils.h>

namespace LFortran {

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
        LFORTRAN_STMT_LABEL_TYPE(FormTeam)
        LFORTRAN_STMT_LABEL_TYPE(GoTo)
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
        default : throw LFortranException("Not implemented");
    }
}


class CommonVisitorMethods {
public:

  inline static void visit_BinOp(Allocator &al, const AST::BinOp_t &x,
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
      LFORTRAN_ASSERT(false);
      op = ASR::binopType::Pow;
    }
    }

    // Cast LHS or RHS if necessary
    ASR::ttype_t *left_type = LFortran::ASRUtils::expr_type(left);
    ASR::ttype_t *right_type = LFortran::ASRUtils::expr_type(right);
    ASR::expr_t *overloaded = nullptr;
    if( LFortran::ASRUtils::use_overloaded(left, right, op,
        intrinsic_op_name, curr_scope, asr, al,
        x.base.base.loc) ) {
        overloaded = LFortran::ASRUtils::EXPR(asr);
    }

    ASR::expr_t **conversion_cand = &left;
    ASR::ttype_t *source_type = left_type;
    ASR::ttype_t *dest_type = right_type;

    ImplicitCastRules::find_conversion_candidate(&left, &right, left_type,
                                                 right_type, conversion_cand,
                                                 &source_type, &dest_type);
    ImplicitCastRules::set_converted_value(al, x.base.base.loc, conversion_cand,
                                           source_type, dest_type);

    LFORTRAN_ASSERT(
        ASRUtils::check_equal_type(LFortran::ASRUtils::expr_type(left),
                                   LFortran::ASRUtils::expr_type(right)));
    ASR::expr_t *value = nullptr;
    // Assign evaluation to `value` if possible, otherwise leave nullptr
    if (LFortran::ASRUtils::expr_value(left) != nullptr &&
        LFortran::ASRUtils::expr_value(right) != nullptr) {
      if (ASR::is_a<LFortran::ASR::Integer_t>(*dest_type)) {
        int64_t left_value = ASR::down_cast<ASR::ConstantInteger_t>(
                                 LFortran::ASRUtils::expr_value(left))
                                 ->m_n;
        int64_t right_value = ASR::down_cast<ASR::ConstantInteger_t>(
                                  LFortran::ASRUtils::expr_value(right))
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
          LFORTRAN_ASSERT(false);
          op = ASR::binopType::Pow;
        }
        }
        value = ASR::down_cast<ASR::expr_t>(ASR::make_ConstantInteger_t(
            al, x.base.base.loc, result, dest_type));
      } else if (ASR::is_a<LFortran::ASR::Real_t>(*dest_type)) {
        double left_value = ASR::down_cast<ASR::ConstantReal_t>(
                                LFortran::ASRUtils::expr_value(left))
                                ->m_r;
        double right_value = ASR::down_cast<ASR::ConstantReal_t>(
                                 LFortran::ASRUtils::expr_value(right))
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
          LFORTRAN_ASSERT(false);
          op = ASR::binopType::Pow;
        }
        }
        value = ASR::down_cast<ASR::expr_t>(
            ASR::make_ConstantReal_t(al, x.base.base.loc, result, dest_type));
      }
    }
    asr = ASR::make_BinOp_t(al, x.base.base.loc, left, op, right, dest_type,
                            value, overloaded);
  }

  inline static void visit_Compare(Allocator &al, const AST::Compare_t &x,
                                   ASR::expr_t *&left, ASR::expr_t *&right,
                                   ASR::asr_t *&asr) {
    // Cast LHS or RHS if necessary
    ASR::ttype_t *left_type = LFortran::ASRUtils::expr_type(left);
    ASR::ttype_t *right_type = LFortran::ASRUtils::expr_type(right);
    if ((left_type->type != ASR::ttypeType::Real &&
         left_type->type != ASR::ttypeType::Integer) &&
        (right_type->type != ASR::ttypeType::Real &&
         right_type->type != ASR::ttypeType::Integer) &&
        ((left_type->type != ASR::ttypeType::Complex ||
          right_type->type != ASR::ttypeType::Complex) &&
         x.m_op != AST::cmpopType::Eq && x.m_op != AST::cmpopType::NotEq)) {
      throw SemanticError(
          "Compare: only Integer or Real can be on the LHS and RHS. "
          "If operator is .eq. or .neq. then Complex type is also acceptable",
          x.base.base.loc);
    } else {
      ASR::expr_t **conversion_cand = &left;
      ASR::ttype_t *dest_type = right_type;
      ASR::ttype_t *source_type = left_type;
      ImplicitCastRules::find_conversion_candidate(&left, &right, left_type,
                                                   right_type, conversion_cand,
                                                   &source_type, &dest_type);

      ImplicitCastRules::set_converted_value(
          al, x.base.base.loc, conversion_cand, source_type, dest_type);
    }

    LFORTRAN_ASSERT(
        ASRUtils::check_equal_type(LFortran::ASRUtils::expr_type(left),
                                   LFortran::ASRUtils::expr_type(right)));
    ASR::ttype_t *type = LFortran::ASRUtils::TYPE(
        ASR::make_Logical_t(al, x.base.base.loc, 4, nullptr, 0));
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

    ASR::expr_t *value = nullptr;
    ASR::ttype_t *source_type = left_type;
    // Assign evaluation to `value` if possible, otherwise leave nullptr
    if (LFortran::ASRUtils::expr_value(left) != nullptr &&
        LFortran::ASRUtils::expr_value(right) != nullptr) {
      if (ASR::is_a<LFortran::ASR::Integer_t>(*source_type)) {
        int64_t left_value = ASR::down_cast<ASR::ConstantInteger_t>(
                                 LFortran::ASRUtils::expr_value(left))
                                 ->m_n;
        int64_t right_value = ASR::down_cast<ASR::ConstantInteger_t>(
                                  LFortran::ASRUtils::expr_value(right))
                                  ->m_n;
        bool result;
        switch (asr_op) {
            case (ASR::cmpopType::Eq): {
                result = left_value == right_value;
                break;
            }
            case (ASR::cmpopType::Gt): {
                result = left_value > right_value;
                break;
            }
            case (ASR::cmpopType::GtE): {
                result = left_value >= right_value;
                break;
            }
            case (ASR::cmpopType::Lt): {
                result = left_value < right_value;
                break;
            }
            case (ASR::cmpopType::LtE): {
                result = left_value <= right_value;
                break;
            }
            case (ASR::cmpopType::NotEq): {
                result = left_value != right_value;
                break;
            }
            default: {
                throw SemanticError("Comparison operator not implemented",
                                    x.base.base.loc);
            }
        }
        value = ASR::down_cast<ASR::expr_t>(ASR::make_ConstantLogical_t(
            al, x.base.base.loc, result, source_type));
      } else if (ASR::is_a<LFortran::ASR::Real_t>(*source_type)) {
        double left_value = ASR::down_cast<ASR::ConstantReal_t>(
                                LFortran::ASRUtils::expr_value(left))
                                ->m_r;
        double right_value = ASR::down_cast<ASR::ConstantReal_t>(
                                 LFortran::ASRUtils::expr_value(right))
                                 ->m_r;
        bool result;
        switch (asr_op) {
            case (ASR::cmpopType::Eq): {
                result = left_value == right_value;
                break;
            }
            case (ASR::cmpopType::Gt): {
                result = left_value > right_value;
                break;
            }
            case (ASR::cmpopType::GtE): {
                result = left_value >= right_value;
                break;
            }
            case (ASR::cmpopType::Lt): {
                result = left_value < right_value;
                break;
            }
            case (ASR::cmpopType::LtE): {
                result = left_value <= right_value;
                break;
            }
            case (ASR::cmpopType::NotEq): {
                result = left_value != right_value;
                break;
            }
            default: {
                throw SemanticError("Comparison operator not implemented",
                                    x.base.base.loc);
            }
        }
        value = ASR::down_cast<ASR::expr_t>(ASR::make_ConstantLogical_t(
            al, x.base.base.loc, result, source_type));
      }
    }
    asr = ASR::make_Compare_t(al, x.base.base.loc, left, asr_op, right, type,
                              value);
  }

  inline static void visit_BoolOp(Allocator &al, const AST::BoolOp_t &x,
                                  ASR::expr_t *&left, ASR::expr_t *&right,
                                  ASR::asr_t *&asr) {
    ASR::boolopType op;
    switch (x.m_op) {
    case (AST::And):
      op = ASR::And;
      break;
    case (AST::Or):
      op = ASR::Or;
      break;
    case (AST::NEqv):
      op = ASR::NEqv;
      break;
    case (AST::Eqv):
      op = ASR::Eqv;
      break;
    default:
      throw SemanticError(R"""(Only .and., .or., .neqv., .eqv.
                                    implemented for logical type operands.)""",
                          x.base.base.loc);
    }

    // Cast LHS or RHS if necessary
    ASR::ttype_t *left_type = LFortran::ASRUtils::expr_type(left);
    ASR::ttype_t *right_type = LFortran::ASRUtils::expr_type(right);
    ASR::expr_t **conversion_cand = &left;
    ASR::ttype_t *source_type = left_type;
    ASR::ttype_t *dest_type = right_type;

    ImplicitCastRules::find_conversion_candidate(&left, &right, left_type,
                                                 right_type, conversion_cand,
                                                 &source_type, &dest_type);
    ImplicitCastRules::set_converted_value(al, x.base.base.loc, conversion_cand,
                                           source_type, dest_type);

    LFORTRAN_ASSERT(
        ASRUtils::check_equal_type(LFortran::ASRUtils::expr_type(left),
                                   LFortran::ASRUtils::expr_type(right)));

    ASR::expr_t *value = nullptr;
    // Assign evaluation to `value` if possible, otherwise leave nullptr
    if (LFortran::ASRUtils::expr_value(left) != nullptr &&
        LFortran::ASRUtils::expr_value(right) != nullptr) {
        LFORTRAN_ASSERT(ASR::is_a<LFortran::ASR::Logical_t>(*dest_type))

        bool left_value = ASR::down_cast<ASR::ConstantLogical_t>(
                                 LFortran::ASRUtils::expr_value(left))
                                 ->m_value;
        bool right_value = ASR::down_cast<ASR::ConstantLogical_t>(
                                  LFortran::ASRUtils::expr_value(right))
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
        value = ASR::down_cast<ASR::expr_t>(ASR::make_ConstantLogical_t(
            al, x.base.base.loc, result, dest_type));
    }
    asr = ASR::make_BoolOp_t(al, x.base.base.loc, left, op, right, dest_type,
                             value);
  }

  inline static void visit_UnaryOp(Allocator &al, const AST::UnaryOp_t &x,
                                   ASR::expr_t *&operand, ASR::asr_t *&asr) {
    ASR::unaryopType op;
    switch (x.m_op) {
    case (AST::unaryopType::Invert):
      op = ASR::unaryopType::Invert;
      break;
    case (AST::unaryopType::Not):
      op = ASR::unaryopType::Not;
      break;
    case (AST::unaryopType::UAdd):
      op = ASR::unaryopType::UAdd;
      break;
    case (AST::unaryopType::USub):
      op = ASR::unaryopType::USub;
      break;
    // Fix compiler warning:
    default: {
      LFORTRAN_ASSERT(false);
      op = ASR::unaryopType::Invert;
    }
    }
    ASR::ttype_t *operand_type = LFortran::ASRUtils::expr_type(operand);
    ASR::expr_t *value = nullptr;
    // Assign evaluation to `value` if possible, otherwise leave nullptr
    if (LFortran::ASRUtils::expr_value(operand) != nullptr) {
      if (ASR::is_a<LFortran::ASR::Integer_t>(*operand_type)) {
        int64_t op_value = ASR::down_cast<ASR::ConstantInteger_t>(
                                  LFortran::ASRUtils::expr_value(operand))
                                  ->m_n;
        int64_t result;
        switch (op) {
        case (ASR::unaryopType::UAdd):
          result = op_value;
          break;
        case (ASR::unaryopType::USub):
          result = -op_value;
          break;
        default: {
            throw SemanticError("Unary operator not implemented yet for compile time evaluation",
                x.base.base.loc);
        }
        }
        value = ASR::down_cast<ASR::expr_t>(ASR::make_ConstantInteger_t(
            al, x.base.base.loc, result, operand_type));
      } else if (ASR::is_a<LFortran::ASR::Real_t>(*operand_type)) {
        double op_value = ASR::down_cast<ASR::ConstantReal_t>(
                                LFortran::ASRUtils::expr_value(operand))
                                ->m_r;
        double result;
        switch (op) {
        case (ASR::unaryopType::UAdd):
          result = op_value;
          break;
        case (ASR::unaryopType::USub):
          result = -op_value;
          break;
        default: {
            throw SemanticError("Unary operator not implemented yet for compile time evaluation",
                x.base.base.loc);
        }
        }
        value = ASR::down_cast<ASR::expr_t>(
            ASR::make_ConstantReal_t(al, x.base.base.loc, result, operand_type));
      }
    }
    asr = ASR::make_UnaryOp_t(al, x.base.base.loc, op, operand, operand_type,
                              value);
  }

  static inline void visit_StrOp(Allocator &al, const AST::StrOp_t &x,
                                 ASR::expr_t *&left, ASR::expr_t *&right,
                                 ASR::asr_t *&asr) {
    ASR::stropType op;
    LFORTRAN_ASSERT(x.m_op == AST::Concat)
    switch (x.m_op) {
    case (AST::Concat):
      op = ASR::Concat;
    }
    ASR::ttype_t *left_type = LFortran::ASRUtils::expr_type(left);
    ASR::ttype_t *right_type = LFortran::ASRUtils::expr_type(right);
    LFORTRAN_ASSERT(ASR::is_a<ASR::Character_t>(*left_type))
    LFORTRAN_ASSERT(ASR::is_a<ASR::Character_t>(*right_type))
    ASR::Character_t *left_type2 = ASR::down_cast<ASR::Character_t>(left_type);
    ASR::Character_t *right_type2 = ASR::down_cast<ASR::Character_t>(right_type);
    LFORTRAN_ASSERT(left_type2->n_dims == 0);
    LFORTRAN_ASSERT(right_type2->n_dims == 0);
    ASR::ttype_t *dest_type = ASR::down_cast<ASR::ttype_t>(ASR::make_Character_t(al, x.base.base.loc, left_type2->m_kind,
        left_type2->m_len + right_type2->m_len, nullptr, nullptr, 0));

    ASR::expr_t *value = nullptr;
    // Assign evaluation to `value` if possible, otherwise leave nullptr
    if (LFortran::ASRUtils::expr_value(left) != nullptr &&
        LFortran::ASRUtils::expr_value(right) != nullptr) {
        char* left_value = ASR::down_cast<ASR::ConstantString_t>(
                                 LFortran::ASRUtils::expr_value(left))
                                 ->m_s;
        char* right_value = ASR::down_cast<ASR::ConstantString_t>(
                                  LFortran::ASRUtils::expr_value(right))
                                  ->m_s;
        char* result;
        std::string result_s = std::string(left_value)+std::string(right_value);
        Str s; s.from_str_view(result_s);
        result = s.c_str(al);
        LFORTRAN_ASSERT((int64_t)strlen(result) == ASR::down_cast<ASR::Character_t>(dest_type)->m_len)
        value = ASR::down_cast<ASR::expr_t>(ASR::make_ConstantString_t(
            al, x.base.base.loc, result, dest_type));
      }
    asr = ASR::make_StrOp_t(al, x.base.base.loc, left, op, right, dest_type,
                            value);
  }

static ASR::asr_t* comptime_intrinsic_real(ASR::expr_t *A,
        ASR::expr_t * kind,
        Allocator &al, const Location &loc) {
    int kind_int = 4;
    if (kind) {
        ASR::expr_t* kind_value = LFortran::ASRUtils::expr_value(kind);
        if (kind_value) {
            if (ASR::is_a<ASR::ConstantInteger_t>(*kind_value)) {
                kind_int = ASR::down_cast<ASR::ConstantInteger_t>(kind_value)->m_n;
            } else {
                throw SemanticError("kind argument to real(a, kind) is not a constant integer", loc);
            }
        } else {
            throw SemanticError("kind argument to real(a, kind) is not constant", loc);
        }
    }
    ASR::expr_t *result = A;
    ASR::ttype_t *dest_type = LFortran::ASRUtils::TYPE(ASR::make_Real_t(al, loc,
                kind_int, nullptr, 0));
    ASR::ttype_t *source_type = LFortran::ASRUtils::expr_type(A);

    // TODO: this is explicit cast, use ExplicitCast
    ImplicitCastRules::set_converted_value(al, loc, &result,
                                           source_type, dest_type);
    return (ASR::asr_t*)result;
}

}; // class CommonVisitorMethods


template <class Derived>
class CommonVisitor : public AST::BaseVisitor<Derived> {
public:
    std::map<std::string, std::string> intrinsic_procedures = {
        {"kind", "lfortran_intrinsic_kind"},
        {"selected_int_kind", "lfortran_intrinsic_kind"},
        {"selected_real_kind", "lfortran_intrinsic_kind"},
        {"size", "lfortran_intrinsic_array"},
        {"present", "lfortran_intrinsic_array"},
        {"lbound", "lfortran_intrinsic_array"},
        {"ubound", "lfortran_intrinsic_array"},
        {"min", "lfortran_intrinsic_array"},
        {"max", "lfortran_intrinsic_array"},
        {"allocated", "lfortran_intrinsic_array"},
        {"minval", "lfortran_intrinsic_array"},
        {"maxval", "lfortran_intrinsic_array"},
        {"real", "lfortran_intrinsic_array"},
        {"char", "lfortran_intrinsic_array"},
        {"floor", "lfortran_intrinsic_array"},
        {"sum", "lfortran_intrinsic_array"},
        {"len", "lfortran_intrinsic_array"},
        {"abs", "lfortran_intrinsic_math2"},
        {"aimag", "lfortran_intrinsic_math2"},
        {"modulo", "lfortran_intrinsic_math2"},
        {"exp", "lfortran_intrinsic_math"},
        {"log", "lfortran_intrinsic_math"},
        {"erf", "lfortran_intrinsic_math"},
        {"sin", "lfortran_intrinsic_trig"},
        {"cos", "lfortran_intrinsic_math"},
        {"tan", "lfortran_intrinsic_math"},
        {"sinh", "lfortran_intrinsic_math"},
        {"cosh", "lfortran_intrinsic_math"},
        {"tanh", "lfortran_intrinsic_math"},
        {"asin", "lfortran_intrinsic_math"},
        {"acos", "lfortran_intrinsic_math"},
        {"atan", "lfortran_intrinsic_math"},
        {"atan2", "lfortran_intrinsic_math"},
        {"asinh", "lfortran_intrinsic_math"},
        {"acosh", "lfortran_intrinsic_math"},
        {"atanh", "lfortran_intrinsic_math"},
        {"sqrt", "lfortran_intrinsic_math2"},
        {"int", "lfortran_intrinsic_array"},
        {"real", "lfortran_intrinsic_array"},
        {"tiny", "lfortran_intrinsic_array"},
        {"len_trim", "lfortran_intrinsic_string"},
        {"trim", "lfortran_intrinsic_string"},
        {"iand", "lfortran_intrinsic_bit"},
    };

    std::map<AST::operatorType, std::string> binop2str = {
        {AST::operatorType::Mul, "~mul"},
        {AST::operatorType::Add, "~add"},
    };

    ASR::asr_t *tmp;
    Allocator &al;
    SymbolTable *current_scope;
    ASR::Module_t *current_module = nullptr;
    Vec<char *> current_module_dependencies;

    CommonVisitor(Allocator &al, SymbolTable *symbol_table) : al{al}, current_scope{symbol_table} {
        current_module_dependencies.reserve(al, 4);
    }

    ASR::asr_t* resolve_variable(const Location &loc, const std::string &var_name) {
        SymbolTable *scope = current_scope;
        ASR::symbol_t *v = scope->resolve_symbol(var_name);
        if (!v) {
            throw SemanticError("Variable '" + var_name + "' not declared", loc);
        }
        if( v->type == ASR::symbolType::Variable ) {
            ASR::Variable_t* v_var = ASR::down_cast<ASR::Variable_t>(v);
            if( v_var->m_type == nullptr &&
                v_var->m_intent == ASR::intentType::AssociateBlock ) {
                return (ASR::asr_t*)(v_var->m_symbolic_value);
            }
        }
        return ASR::make_Var_t(al, loc, v);
    }

    ASR::asr_t* resolve_variable2(const Location &loc, const std::string &var_name,
            const std::string &dt_name, SymbolTable*& scope) {
        ASR::symbol_t *v = scope->resolve_symbol(dt_name);
        if (!v) {
            throw SemanticError("Variable '" + dt_name + "' not declared", loc);
        }
        ASR::Variable_t* v_variable = ((ASR::Variable_t*)(&(v->base)));
        if ( v_variable->m_type->type == ASR::ttypeType::Derived ||
             v_variable->m_type->type == ASR::ttypeType::DerivedPointer ||
             v_variable->m_type->type == ASR::ttypeType::Class ) {
            ASR::ttype_t* v_type = v_variable->m_type;
            ASR::Derived_t* der = (ASR::Derived_t*)(&(v_type->base));
            ASR::DerivedType_t *der_type;
            if( der->m_derived_type->type == ASR::symbolType::ExternalSymbol ) {
                ASR::ExternalSymbol_t* der_ext = (ASR::ExternalSymbol_t*)(&(der->m_derived_type->base));
                ASR::symbol_t* der_sym = der_ext->m_external;
                if( der_sym == nullptr ) {
                    throw SemanticError("'" + std::string(der_ext->m_name) + "' isn't a Derived type.", loc);
                } else {
                    der_type = (ASR::DerivedType_t*)(&(der_sym->base));
                }
            } else {
                der_type = (ASR::DerivedType_t*)(&(der->m_derived_type->base));
            }
            ASR::DerivedType_t *par_der_type = der_type;
            // scope = der_type->m_symtab;
            // ASR::symbol_t* member = der_type->m_symtab->resolve_symbol(var_name);
            ASR::symbol_t* member = nullptr;
            while( par_der_type != nullptr && member == nullptr ) {
                scope = par_der_type->m_symtab;
                member = par_der_type->m_symtab->resolve_symbol(var_name);
                if( par_der_type->m_parent != nullptr ) {
                    par_der_type = (ASR::DerivedType_t*)(LFortran::ASRUtils::symbol_get_past_external(par_der_type->m_parent));
                } else {
                    par_der_type = nullptr;
                }
            }
            if( member != nullptr ) {
                ASR::asr_t* v_var = ASR::make_Var_t(al, loc, v);
                return ASRUtils::getDerivedRef_t(al, loc, v_var, member, current_scope);
            } else {
                throw SemanticError("Variable '" + dt_name + "' doesn't have any member named, '" + var_name + "'.", loc);
            }
        } else {
            throw SemanticError("Variable '" + dt_name + "' is not a derived type", loc);
        }
    }

    ASR::symbol_t* resolve_intrinsic_function(const Location &loc, std::string &remote_sym) {
        std::string module_name = intrinsic_procedures[remote_sym];

        SymbolTable *tu_symtab = ASRUtils::get_tu_symtab(current_scope);
        ASR::Module_t *m = ASRUtils::load_module(al, tu_symtab, module_name,
                loc, true);

        ASR::symbol_t *t = m->m_symtab->resolve_symbol(remote_sym);
        if (!t) {
            throw SemanticError("The symbol '" + remote_sym
                + "' not found in the module '" + module_name + "'",
                loc);
        } else if (! (ASR::is_a<ASR::GenericProcedure_t>(*t)
                    || ASR::is_a<ASR::Function_t>(*t))) {
            throw SemanticError("The symbol '" + remote_sym
                + "' found in the module '" + module_name + "', "
                + "but it is not a function or a generic function.",
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

        current_scope->scope[sym] = ASR::down_cast<ASR::symbol_t>(fn);
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

    void visit_BinOp(const AST::BinOp_t &x) {
        this->visit_expr(*x.m_left);
        ASR::expr_t *left = LFortran::ASRUtils::EXPR(tmp);
        this->visit_expr(*x.m_right);
        ASR::expr_t *right = LFortran::ASRUtils::EXPR(tmp);
        CommonVisitorMethods::visit_BinOp(al, x, left, right, tmp, binop2str[x.m_op], current_scope);
    }

    void visit_BoolOp(const AST::BoolOp_t &x) {
        this->visit_expr(*x.m_left);
        ASR::expr_t *left = LFortran::ASRUtils::EXPR(tmp);
        this->visit_expr(*x.m_right);
        ASR::expr_t *right = LFortran::ASRUtils::EXPR(tmp);
        CommonVisitorMethods::visit_BoolOp(al, x, left, right, tmp);
    }

    void visit_StrOp(const AST::StrOp_t &x) {
        this->visit_expr(*x.m_left);
        ASR::expr_t *left = LFortran::ASRUtils::EXPR(tmp);
        this->visit_expr(*x.m_right);
        ASR::expr_t *right = LFortran::ASRUtils::EXPR(tmp);
        CommonVisitorMethods::visit_StrOp(al, x, left, right, tmp);
    }

    void visit_UnaryOp(const AST::UnaryOp_t &x) {
        this->visit_expr(*x.m_operand);
        ASR::expr_t *operand = LFortran::ASRUtils::EXPR(tmp);
        CommonVisitorMethods::visit_UnaryOp(al, x, operand, tmp);
    }

    void visit_Compare(const AST::Compare_t &x) {
        this->visit_expr(*x.m_left);
        ASR::expr_t *left = LFortran::ASRUtils::EXPR(tmp);
        this->visit_expr(*x.m_right);
        ASR::expr_t *right = LFortran::ASRUtils::EXPR(tmp);
        CommonVisitorMethods::visit_Compare(al, x, left, right, tmp);
    }

    void visit_Parenthesis(const AST::Parenthesis_t &x) {
        this->visit_expr(*x.m_operand);
    }

    void visit_Logical(const AST::Logical_t &x) {
        ASR::ttype_t *type = LFortran::ASRUtils::TYPE(ASR::make_Logical_t(al, x.base.base.loc,
                4, nullptr, 0));
        tmp = ASR::make_ConstantLogical_t(al, x.base.base.loc, x.m_value, type);
    }

    void visit_String(const AST::String_t &x) {
        int s_len = strlen(x.m_s);
        ASR::ttype_t *type = LFortran::ASRUtils::TYPE(ASR::make_Character_t(al, x.base.base.loc,
                1, s_len, nullptr, nullptr, 0));
        tmp = ASR::make_ConstantString_t(al, x.base.base.loc, x.m_s, type);
    }

    void visit_Num(const AST::Num_t &x) {
        int ikind = 4;
        if (x.m_kind) {
            ikind = std::atoi(x.m_kind);
            if (ikind == 0) {
                std::string var_name = x.m_kind;
                ASR::symbol_t *v = current_scope->resolve_symbol(var_name);
                if (v) {
                    const ASR::symbol_t *v3 = LFortran::ASRUtils::symbol_get_past_external(v);
                    if (ASR::is_a<ASR::Variable_t>(*v3)) {
                        ASR::Variable_t *v2 = ASR::down_cast<ASR::Variable_t>(v3);
                        if (v2->m_value) {
                            if (ASR::is_a<ASR::ConstantInteger_t>(*v2->m_value)) {
                                ikind = ASR::down_cast<ASR::ConstantInteger_t>(v2->m_value)->m_n;
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
        ASR::ttype_t *type = LFortran::ASRUtils::TYPE(ASR::make_Integer_t(al,
                x.base.base.loc, ikind, nullptr, 0));
        if (BigInt::is_int_ptr(x.m_n)) {
            throw SemanticError("Integer constants larger than 2^62-1 are not implemented yet", x.base.base.loc);
        } else {
            LFORTRAN_ASSERT(!BigInt::is_int_ptr(x.m_n));
            tmp = ASR::make_ConstantInteger_t(al, x.base.base.loc, x.m_n, type);
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
                const ASR::symbol_t *v3 = LFortran::ASRUtils::symbol_get_past_external(v);
                if (ASR::is_a<ASR::Variable_t>(*v3)) {
                    ASR::Variable_t *v2 = ASR::down_cast<ASR::Variable_t>(v3);
                    if (v2->m_value) {
                        if (ASR::is_a<ASR::ConstantInteger_t>(*v2->m_value)) {
                            r_kind = ASR::down_cast<ASR::ConstantInteger_t>(v2->m_value)->m_n;
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
        ASR::ttype_t *type = LFortran::ASRUtils::TYPE(ASR::make_Real_t(al, x.base.base.loc,
                r_kind, nullptr, 0));
        tmp = ASR::make_ConstantReal_t(al, x.base.base.loc, r, type);
    }

    void visit_Complex(const AST::Complex_t &x) {
        this->visit_expr(*x.m_re);
        ASR::expr_t *re = LFortran::ASRUtils::EXPR(tmp);
        int a_kind_r = LFortran::ASRUtils::extract_kind_from_ttype_t(LFortran::ASRUtils::expr_type(re));
        this->visit_expr(*x.m_im);
        ASR::expr_t *im = LFortran::ASRUtils::EXPR(tmp);
        int a_kind_i = LFortran::ASRUtils::extract_kind_from_ttype_t(LFortran::ASRUtils::expr_type(im));
        ASR::ttype_t *type = LFortran::ASRUtils::TYPE(ASR::make_Complex_t(al, x.base.base.loc,
                std::max(a_kind_r, a_kind_i), nullptr, 0));
        tmp = ASR::make_ConstantComplex_t(al, x.base.base.loc,
                re, im, type);
    }

    Vec<ASR::expr_t*> visit_expr_list(AST::fnarg_t *ast_list, size_t n) {
        Vec<ASR::expr_t*> asr_list;
        asr_list.reserve(al, n);
        for (size_t i=0; i<n; i++) {
            LFORTRAN_ASSERT(ast_list[i].m_end != nullptr);
            this->visit_expr(*ast_list[i].m_end);
            ASR::expr_t *expr = LFortran::ASRUtils::EXPR(tmp);
            asr_list.push_back(al, expr);
        }
        return asr_list;
    }

    void visit_Name(const AST::Name_t &x) {
        if (x.n_member == 0) {
            tmp = resolve_variable(x.base.base.loc, to_lower(x.m_id));
        } else if (x.n_member == 1) {
            if (x.m_member[0].n_args == 0) {
                SymbolTable* scope = current_scope;
                tmp = this->resolve_variable2(x.base.base.loc, to_lower(x.m_id),
                    to_lower(x.m_member[0].m_name), scope);
            } else {
                // TODO: incorporate m_args
                SymbolTable* scope = current_scope;
                tmp = this->resolve_variable2(x.base.base.loc, to_lower(x.m_id),
                    to_lower(x.m_member[0].m_name), scope);
            }
        } else {
            SymbolTable* scope = current_scope;
            tmp = this->resolve_variable2(x.base.base.loc, to_lower(x.m_member[1].m_name), to_lower(x.m_member[0].m_name), scope);
            ASR::DerivedRef_t* tmp2;
            std::uint32_t i;
            for( i = 2; i < x.n_member; i++ ) {
                tmp2 = (ASR::DerivedRef_t*)this->resolve_variable2(x.base.base.loc,
                                            to_lower(x.m_member[i].m_name), to_lower(x.m_member[i - 1].m_name), scope);
                tmp = ASR::make_DerivedRef_t(al, x.base.base.loc, LFortran::ASRUtils::EXPR(tmp), tmp2->m_m, tmp2->m_type, nullptr);
            }
            i = x.n_member - 1;
            tmp2 = (ASR::DerivedRef_t*)this->resolve_variable2(x.base.base.loc, to_lower(x.m_id), to_lower(x.m_member[i].m_name), scope);
            tmp = ASR::make_DerivedRef_t(al, x.base.base.loc, LFortran::ASRUtils::EXPR(tmp), tmp2->m_m, tmp2->m_type, nullptr);
        }
    }

    // It tries to evaluate intrinsic function calls if it can be done.
    // If it cannot be done at compile time, it returns a nullptr.
    // `f` must be an intrinsic function
    ASR::expr_t *intrinsic_function_evaluation(const Location &loc,
            const ASR::Function_t &f, Vec<ASR::expr_t*> &args) {
        LFORTRAN_ASSERT(ASRUtils::is_intrinsic_function(&f));
        std::string var_name = f.m_name;
        ASR::expr_t *value = nullptr;
        switch(args.n) {
            case 1: { // Single argument intrinsics
                if (var_name=="kind") {
                    // TODO: Refactor to allow early return
                    // kind_num --> value {4, 8, etc.}
                    int64_t kind_num = 4; // Default
                    ASR::expr_t* kind_expr = args[0];
                    // TODO: Check that the expression reduces to a valid constant expression (10.1.12)
                    switch( kind_expr->type ) {
                        case ASR::exprType::ConstantInteger: {
                            kind_num = ASR::down_cast<ASR::Integer_t>(ASR::down_cast<ASR::ConstantInteger_t>(kind_expr)->m_type)->m_kind;
                            break;
                        }
                        case ASR::exprType::ConstantReal:{
                            kind_num = ASR::down_cast<ASR::Real_t>(ASR::down_cast<ASR::ConstantReal_t>(kind_expr)->m_type)->m_kind;
                            break;
                        }
                        case ASR::exprType::ConstantLogical:{
                            kind_num = ASR::down_cast<ASR::Logical_t>(ASR::down_cast<ASR::ConstantLogical_t>(kind_expr)->m_type)->m_kind;
                            break;
                        }
                        case ASR::exprType::Var : {
                            kind_num = ASRUtils::extract_kind(kind_expr, loc);
                            break;
                        }
                    default: {
                        std::string msg = R"""(Only Integer literals or expressions which reduce to constant Integer are accepted as kind parameters.)""";
                        throw SemanticError(msg, loc);
                        break;
                    }
                    }
                    ASR::ttype_t *type = LFortran::ASRUtils::TYPE(
                            ASR::make_Integer_t(al, loc,
                                4, nullptr, 0));
                    value = ASR::down_cast<ASR::expr_t>(ASR::make_ConstantInteger_t(al, loc, kind_num,
                        type));
                }
                else if (var_name=="tiny") {
                    // We assume the input is valid
                    // ASR::expr_t* tiny_expr = args[0];
                    ASR::ttype_t* tiny_type = LFortran::ASRUtils::expr_type(args[0]);
                    // TODO: Arrays of reals are a valid argument for tiny
                    if (LFortran::ASRUtils::is_array(tiny_type)){
                        throw SemanticError("Array values not implemented yet",
                                            loc);
                    }
                    // TODO: Figure out how to deal with higher precision later
                    if (ASR::is_a<LFortran::ASR::Real_t>(*tiny_type)) {
                        // We don't actually need the value yet, it is enough to know it is a double
                        // but it might provide further information later (precision)
                        // double tiny_val = ASR::down_cast<ASR::ConstantReal_t>(LFortran::ASRUtils::expr_value(tiny_expr))->m_r;
                        int tiny_kind = LFortran::ASRUtils::extract_kind_from_ttype_t(tiny_type);
                        if (tiny_kind == 4){
                            float low_val = std::numeric_limits<float>::min();
                            value = ASR::down_cast<ASR::expr_t>(ASR::make_ConstantReal_t(al, loc,
                                                                                         low_val, // value
                                                                                         tiny_type));
                        } else {
                            double low_val = std::numeric_limits<double>::min();
                            value = ASR::down_cast<ASR::expr_t>(ASR::make_ConstantReal_t(al, loc,
                                                                                         low_val, // value
                                                                                         tiny_type));
                                }
                    }
                    else {
                        throw SemanticError("Argument for tiny must be Real",
                                            loc);
                    }
                }
                else if (var_name=="real") {
                    value = ASR::down_cast<ASR::expr_t>(CommonVisitorMethods::comptime_intrinsic_real(args[0], nullptr, al, loc));
                }
                else if (var_name=="floor") {
                    // TODO: Implement optional kind; J3/18-007r1 --> FLOOR(A, [KIND])
                    // TODO: Rip out switch to work with optional arguments
                    ASR::expr_t* func_expr = args[0];
                    ASR::ttype_t* func_type = LFortran::ASRUtils::expr_type(func_expr);
                    int func_kind = ASRUtils::extract_kind_from_ttype_t(func_type);
                    int64_t ival {0};
                    if (LFortran::ASR::is_a<LFortran::ASR::Real_t>(*func_type)) {
                        if (func_kind == 4){
                            float rv = ASR::down_cast<ASR::ConstantReal_t>(
                                LFortran::ASRUtils::expr_value(func_expr))->m_r;
                            if (rv<0) {
                                // negative number
                                // floor -> integer(|x|+1)
                                ival = static_cast<int64_t>(rv-1);
                            } else {
                                    // positive, floor -> integer(x)
                                    ival = static_cast<int64_t>(rv);
                                }
                                value = ASR::down_cast<ASR::expr_t>(ASR::make_ConstantInteger_t(al, loc, ival,func_type));
                            } else {
                                double rv = ASR::down_cast<ASR::ConstantReal_t>(LFortran::ASRUtils::expr_value(func_expr))->m_r;
                                int64_t ival = static_cast<int64_t>(rv);
                                if (rv<0) {
                                    // negative number
                                    // floor -> integer(x+1)
                                    ival = static_cast<int64_t>(rv+1);
                                } else {
                                    // positive, floor -> integer(x)
                                    ival = static_cast<int64_t>(rv);
                                }
                                value = ASR::down_cast<ASR::expr_t>(ASR::make_ConstantInteger_t(al, loc, ival,func_type));
                        }
                    } else {
                        throw SemanticError("floor must have one real argument", loc);
                    }
                }
                else if (var_name=="int") {
                    ASR::expr_t* int_expr = args[0];
                    ASR::ttype_t* int_type = LFortran::ASRUtils::expr_type(int_expr);
                    int int_kind = ASRUtils::extract_kind_from_ttype_t(int_type);
                    if (LFortran::ASR::is_a<LFortran::ASR::Integer_t>(*int_type)) {
                        if (int_kind == 4){
                            int64_t ival = ASR::down_cast<ASR::ConstantInteger_t>(LFortran::ASRUtils::expr_value(int_expr))->m_n;
                            value = ASR::down_cast<ASR::expr_t>(ASR::make_ConstantInteger_t(al, loc, ival, int_type));
                        } else {
                            int64_t ival = ASR::down_cast<ASR::ConstantInteger_t>(LFortran::ASRUtils::expr_value(int_expr))->m_n;
                            value = ASR::down_cast<ASR::expr_t>(ASR::make_ConstantInteger_t(al, loc, ival, int_type));
                        }
                    }
                    else if (LFortran::ASR::is_a<LFortran::ASR::Real_t>(*int_type)) {
                        if (int_kind == 4){
                            float rv = ASR::down_cast<ASR::ConstantReal_t>(
                                LFortran::ASRUtils::expr_value(int_expr))->m_r;
                            int64_t ival = static_cast<int64_t>(rv);
                            value = ASR::down_cast<ASR::expr_t>(ASR::make_ConstantInteger_t(al, loc, ival, int_type));
                        } else {
                            double rv = ASR::down_cast<ASR::ConstantReal_t>(LFortran::ASRUtils::expr_value(int_expr))->m_r;
                            int64_t ival = static_cast<int64_t>(rv);
                            value = ASR::down_cast<ASR::expr_t>(ASR::make_ConstantInteger_t(al, loc, ival, int_type));
                        }
                    }
                    // TODO: Handle BOZ later
                    // else if () {

                    // }
                    else {
                        throw SemanticError("int must have only one argument", loc);
                    }
                }
                else if (var_name=="char") {
                    ASR::expr_t* real_expr = args[0];
                    ASR::ttype_t* real_type = LFortran::ASRUtils::expr_type(real_expr);
                    if (LFortran::ASR::is_a<LFortran::ASR::Integer_t>(*real_type)) {
                        int64_t c = ASR::down_cast<ASR::ConstantInteger_t>(
                            LFortran::ASRUtils::expr_value(real_expr))->m_n;
                        ASR::ttype_t* str_type =
                            LFortran::ASRUtils::TYPE(ASR::make_Character_t(al,
                            loc, 1, 1, nullptr, nullptr, 0));
                        if (! (c >= 0 && c <= 127) ) {
                            throw SemanticError("The argument 'x' in char(x) must be in the range 0 <= x <= 127.", loc);
                        }
                        char cc = c;
                        std::string svalue;
                        svalue += cc;
                        Str s;
                        s.from_str_view(svalue);
                        char *str_val = s.c_str(al);
                        value = ASR::down_cast<ASR::expr_t>(
                            ASR::make_ConstantString_t(al, loc,
                            str_val, str_type));
                    } else {
                        throw SemanticError("char() must have one integer argument", loc);
                    }
                }
                else if (var_name=="selected_int_kind") {
                    ASR::expr_t* real_expr = args[0];
                    ASR::ttype_t* real_type = LFortran::ASRUtils::expr_type(real_expr);
                    if (LFortran::ASR::is_a<LFortran::ASR::Integer_t>(*real_type)) {
                        int64_t R = ASR::down_cast<ASR::ConstantInteger_t>(
                            LFortran::ASRUtils::expr_value(real_expr))->m_n;
                        int a_kind = 4;
                        if (R < 10) {
                            a_kind = 4;
                        } else {
                            a_kind = 8;
                        }
                        value = ASR::down_cast<ASR::expr_t>(
                            ASR::make_ConstantInteger_t(al, loc,
                            a_kind, real_type));
                    } else {
                        throw SemanticError("integer_int_kind() must have one integer argument", loc);
                    }
                }
                else if (var_name=="selected_real_kind") {
                    // TODO: Be more standards compliant 16.9.170
                    // e.g. selected_real_kind(6, 70)
                    ASR::expr_t* real_expr = args[0];
                    ASR::ttype_t* real_type = LFortran::ASRUtils::expr_type(real_expr);
                    if (LFortran::ASR::is_a<LFortran::ASR::Integer_t>(*real_type)) {
                        int64_t R = ASR::down_cast<ASR::ConstantInteger_t>(
                            LFortran::ASRUtils::expr_value(real_expr))->m_n;
                        int a_kind = 4;
                        if (R < 7) {
                            a_kind = 4;
                        } else {
                            a_kind = 8;
                        }
                        value = ASR::down_cast<ASR::expr_t>(
                            ASR::make_ConstantInteger_t(al, loc,
                            a_kind, real_type));
                    } else {
                        throw SemanticError("integer_real_kind() must have one integer argument", loc);
                    }
                }
                break;
            }
            case 2: {
                if (var_name=="real") {
                    value = ASR::down_cast<ASR::expr_t>(CommonVisitorMethods::comptime_intrinsic_real(args[0], args[1], al, loc));
                } else {
                    throw SemanticError("Function '" + var_name + "' with " + std::to_string(args.n) +
                            " arguments not supported yet",
                            loc);
                }
                break;
            }
            default:  { // Not implemented
                throw SemanticError("Function '" + var_name + "' with " + std::to_string(args.n) +
                        " arguments not supported yet",
                        loc);
            }
        }
        return value;
    }

};

} // namespace LFortran

#endif /* LFORTRAN_SEMANTICS_AST_COMMON_VISITOR_H */
