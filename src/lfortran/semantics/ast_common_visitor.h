#ifndef LFORTRAN_SEMANTICS_AST_COMMON_VISITOR_H
#define LFORTRAN_SEMANTICS_AST_COMMON_VISITOR_H

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <lfortran/ast.h>
#include <libasr/bigint.h>
#include <libasr/string_utils.h>
#include <lfortran/utils.h>
#include <lfortran/semantics/comptime_eval.h>

#include <string>

using LFortran::diag::Level;
using LFortran::diag::Stage;
using LFortran::diag::Label;
using LFortran::diag::Diagnostic;

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

  inline static void visit_Compare(Allocator &al, const AST::Compare_t &x,
                                   ASR::expr_t *&left, ASR::expr_t *&right,
                                   ASR::asr_t *&asr, std::string& intrinsic_op_name,
                                   SymbolTable* curr_scope) {
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
    ASR::ttype_t *left_type = LFortran::ASRUtils::expr_type(left);
    ASR::ttype_t *right_type = LFortran::ASRUtils::expr_type(right);

    ASR::expr_t *overloaded = nullptr;
    if ( ASRUtils::use_overloaded(left, right, asr_op,
        intrinsic_op_name, curr_scope, asr, al,
        x.base.base.loc, [&](const std::string &msg, const Location &loc) { throw SemanticError(msg, loc); }) ) {
        overloaded = LFortran::ASRUtils::EXPR(asr);
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
      ASR::expr_t **conversion_cand = &left;
      dest_type = right_type;
      source_type = left_type;
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

    ASR::expr_t *value = nullptr;
    // Assign evaluation to `value` if possible, otherwise leave nullptr
    if (LFortran::ASRUtils::expr_value(left) != nullptr &&
        LFortran::ASRUtils::expr_value(right) != nullptr) {
      if (ASR::is_a<LFortran::ASR::Integer_t>(*dest_type)) {
        int64_t left_value = ASR::down_cast<ASR::IntegerConstant_t>(
                                 LFortran::ASRUtils::expr_value(left))
                                 ->m_n;
        int64_t right_value = ASR::down_cast<ASR::IntegerConstant_t>(
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
        value = ASR::down_cast<ASR::expr_t>(ASR::make_LogicalConstant_t(
            al, x.base.base.loc, result, type));
      } else if (ASR::is_a<LFortran::ASR::Real_t>(*dest_type)) {
        double left_value = ASR::down_cast<ASR::RealConstant_t>(
                                LFortran::ASRUtils::expr_value(left))
                                ->m_r;
        double right_value = ASR::down_cast<ASR::RealConstant_t>(
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
        value = ASR::down_cast<ASR::expr_t>(ASR::make_LogicalConstant_t(
            al, x.base.base.loc, result, type));
      }
    }
    asr = ASR::make_Compare_t(al, x.base.base.loc, left, asr_op, right, type,
                              value, overloaded);
  }

  inline static void visit_BoolOp(Allocator &al, const AST::BoolOp_t &x,
                                  ASR::expr_t *&left, ASR::expr_t *&right,
                                  ASR::asr_t *&asr, diag::Diagnostics &diag) {
    ASR::boolopType op;
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
            "please help us: what is the Fortran way to specify xor?"
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

        bool left_value = ASR::down_cast<ASR::LogicalConstant_t>(
                                 LFortran::ASRUtils::expr_value(left))
                                 ->m_value;
        bool right_value = ASR::down_cast<ASR::LogicalConstant_t>(
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
        value = ASR::down_cast<ASR::expr_t>(ASR::make_LogicalConstant_t(
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
        int64_t op_value = ASR::down_cast<ASR::IntegerConstant_t>(
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
        value = ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(
            al, x.base.base.loc, result, operand_type));
      } else if (ASR::is_a<LFortran::ASR::Real_t>(*operand_type)) {
        double op_value = ASR::down_cast<ASR::RealConstant_t>(
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
            ASR::make_RealConstant_t(al, x.base.base.loc, result, operand_type));
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
        char* left_value = ASR::down_cast<ASR::StringConstant_t>(
                                 LFortran::ASRUtils::expr_value(left))
                                 ->m_s;
        char* right_value = ASR::down_cast<ASR::StringConstant_t>(
                                  LFortran::ASRUtils::expr_value(right))
                                  ->m_s;
        char* result;
        std::string result_s = std::string(left_value)+std::string(right_value);
        Str s; s.from_str_view(result_s);
        result = s.c_str(al);
        LFORTRAN_ASSERT((int64_t)strlen(result) == ASR::down_cast<ASR::Character_t>(dest_type)->m_len)
        value = ASR::down_cast<ASR::expr_t>(ASR::make_StringConstant_t(
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
    ASR::ttype_t *dest_type = LFortran::ASRUtils::TYPE(ASR::make_Real_t(al, loc,
                kind_int, nullptr, 0));
    ASR::ttype_t *source_type = LFortran::ASRUtils::expr_type(A);

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
        ASR::expr_t* kind_value = LFortran::ASRUtils::expr_value(kind);
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
    ASR::ttype_t *dest_type = LFortran::ASRUtils::TYPE(ASR::make_Integer_t(al, loc,
                kind_int, nullptr, 0));
    ASR::ttype_t *source_type = LFortran::ASRUtils::expr_type(A);

    // TODO: this is implicit cast, use ExplicitCast
    ImplicitCastRules::set_converted_value(al, loc, &result,
                                           source_type, dest_type);
    return (ASR::asr_t*)result;
}

}; // class CommonVisitorMethods


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
    SymbolTable *current_scope;
    ASR::Module_t *current_module = nullptr;
    Vec<char *> current_module_dependencies;
    IntrinsicProcedures intrinsic_procedures;
    IntrinsicProceduresAsASRNodes intrinsic_procedures_as_asr_nodes;

    CommonVisitor(Allocator &al, SymbolTable *symbol_table,
            diag::Diagnostics &diagnostics)
        : diag{diagnostics}, al{al}, current_scope{symbol_table} {
        current_module_dependencies.reserve(al, 4);
    }

    ASR::asr_t* resolve_variable(const Location &loc, const std::string &var_name) {
        SymbolTable *scope = current_scope;
        ASR::symbol_t *v = scope->resolve_symbol(var_name);
        if (!v) {
            diag.semantic_error_label("Variable '" + var_name
                + "' is not declared", {loc},
                "'" + var_name + "' is undeclared");
            throw SemanticAbort();
        }
        return ASR::make_Var_t(al, loc, v);
    }

    ASR::asr_t* create_DerivedTypeConstructor(const Location &loc,
            AST::fnarg_t* m_args, size_t n_args, ASR::symbol_t *v) {
        Vec<ASR::expr_t*> vals = visit_expr_list(m_args, n_args);
        ASR::ttype_t* der = LFortran::ASRUtils::TYPE(
                            ASR::make_Derived_t(al, loc, v,
                                                nullptr, 0));
        return ASR::make_DerivedTypeConstructor_t(al, loc,
                v, vals.p, vals.size(), der);
    }

    ASR::asr_t* create_ArrayRef(const Location &loc,
                AST::fnarg_t* m_args, size_t n_args,
                    ASR::symbol_t *v,
                    ASR::symbol_t *f2) {
        Vec<ASR::array_index_t> args;
        args.reserve(al, n_args);
        for (size_t i=0; i<n_args; i++) {
            ASR::array_index_t ai;
            ai.loc = loc;
            ASR::expr_t *m_start, *m_end, *m_step;
            m_start = m_end = m_step = nullptr;
            if (m_args[i].m_start != nullptr) {
                this->visit_expr(*(m_args[i].m_start));
                m_start = LFortran::ASRUtils::EXPR(tmp);
                ai.loc = m_start->base.loc;
            }
            if (m_args[i].m_end != nullptr) {
                this->visit_expr(*(m_args[i].m_end));
                m_end = LFortran::ASRUtils::EXPR(tmp);
                ai.loc = m_end->base.loc;
            }
            if (m_args[i].m_step != nullptr) {
                this->visit_expr(*(m_args[i].m_step));
                m_step = LFortran::ASRUtils::EXPR(tmp);
                ai.loc = m_step->base.loc;
            }
            ai.m_left = m_start;
            ai.m_right = m_end;
            ai.m_step = m_step;
            args.push_back(al, ai);
        }

        ASR::ttype_t *type;
        type = ASR::down_cast<ASR::Variable_t>(f2)->m_type;
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
        return ASR::make_ArrayRef_t(al, loc,
            v, args.p, args.size(), type, arr_ref_val);
    }

    void visit_ArrayInitializer(const AST::ArrayInitializer_t &x) {
        Vec<ASR::expr_t*> body;
        body.reserve(al, x.n_args);
        ASR::ttype_t *type = nullptr;
        for (size_t i=0; i<x.n_args; i++) {
            this->visit_expr(*x.m_args[i]);
            ASR::expr_t *expr = LFortran::ASRUtils::EXPR(tmp);
            if (type == nullptr) {
                type = LFortran::ASRUtils::expr_type(expr);
            } else {
                if (LFortran::ASRUtils::expr_type(expr)->type != type->type) {
                    throw SemanticError("Type mismatch in array initializer",
                        x.base.base.loc);
                }
            }
            body.push_back(al, expr);
        }
        tmp = ASR::make_ArrayConstant_t(al, x.base.base.loc, body.p,
            body.size(), type);
    }

    void fill_func_calls_ttype_t(std::vector<ASR::expr_t*>& func_calls, ASR::dimension_t* dims, size_t n_dims) {
        for( size_t i = 0; i < n_dims; i++ ) {
            func_calls.push_back(dims[i].m_start);
            func_calls.push_back(dims[i].m_end);
        }
    }

    void fix_function_calls_ttype_t(std::vector<ASR::expr_t*>& func_calls,
                                    Vec<ASR::call_arg_t>& orig_args,
                                    ASR::Function_t* orig_func=nullptr,
                                    bool is_external_func=false) {
        for( size_t i = 0; i < func_calls.size(); i++ ) {
            ASR::expr_t* potential_call = func_calls[i];
            if (potential_call) {
                // The case when expression in return type (like len_expr of character)
                // is a function call.
                if (ASR::is_a<ASR::FunctionCall_t>(*potential_call)) {
                    ASR::FunctionCall_t *fc = ASR::down_cast<ASR::FunctionCall_t>(potential_call);
                    ASR::symbol_t *new_es = fc->m_name;
                    // Import a function as external only if necessary
                    if( is_external_func ) {
                        ASR::Function_t *f = nullptr;
                        if (ASR::is_a<ASR::Function_t>(*fc->m_name)) {
                            f = ASR::down_cast<ASR::Function_t>(fc->m_name);
                        } else if( ASR::is_a<ASR::ExternalSymbol_t>(*fc->m_name) ) {
                            ASR::symbol_t* f_sym = ASRUtils::symbol_get_past_external(fc->m_name);
                            if( ASR::is_a<ASR::Function_t>(*f_sym) ) {
                                f = ASR::down_cast<ASR::Function_t>(f_sym);
                            }
                        }
                        ASR::Module_t *m = ASR::down_cast2<ASR::Module_t>(f->m_symtab->parent->asr_owner);
                        char *modname = m->m_name;
                        ASR::symbol_t *maybe_f = current_scope->resolve_symbol(std::string(f->m_name));
                        std::string maybe_modname = "";
                        if( maybe_f && ASR::is_a<ASR::ExternalSymbol_t>(*maybe_f) ) {
                            maybe_modname = ASR::down_cast<ASR::ExternalSymbol_t>(maybe_f)->m_module_name;
                        }
                        // If the Function to be imported is already present
                        // then do not import.
                        if( maybe_modname == std::string(modname) ) {
                            new_es = maybe_f;
                        } else {
                            // Import while assigning a new name to avoid conflicts
                            // For example, if someone is using `len` from a user
                            // define module then `get_unique_name` will avoid conflict
                            std::string unique_name = current_scope->get_unique_name(f->m_name);
                            Str s; s.from_str_view(unique_name);
                            char *unique_name_c = s.c_str(al);
                            LFORTRAN_ASSERT(current_scope->get_symbol(unique_name) == nullptr);
                            new_es = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(
                                al, f->base.base.loc,
                                /* a_symtab */ current_scope,
                                /* a_name */ unique_name_c,
                                (ASR::symbol_t*)f,
                                modname, nullptr, 0,
                                f->m_name,
                                ASR::accessType::Private
                                ));
                            current_scope->add_symbol(unique_name, new_es);
                        }
                    }
                    Vec<ASR::call_arg_t> args;
                    args.reserve(al, fc->n_args);
                    // The following substitutes args from the current scope
                    for (size_t i = 0; i < fc->n_args; i++) {
                        ASR::expr_t *arg = fc->m_args[i].m_value;
                        size_t arg_idx = i;
                        bool idx_found = false;
                        if (ASR::is_a<ASR::Var_t>(*arg)) {
                            std::string arg_name = ASRUtils::symbol_name(ASR::down_cast<ASR::Var_t>(arg)->m_v);
                            // Finds the index of the argument to be used for substitution
                            // Basically if we are calling maybe(string, ret_type=character(len=len(s)))
                            // where string is a variable in current scope and s is one of the arguments
                            // accepted by maybe i.e., maybe has a signature maybe(s). Then, we will
                            // replace s with string. So, the call would become,
                            // maybe(string, ret_type=character(len=len(string)))
                            for( size_t j = 0; j < orig_func->n_args && !idx_found; j++ ) {
                                if( ASR::is_a<ASR::Var_t>(*(orig_func->m_args[j])) ) {
                                    std::string arg_name_2 = std::string(ASRUtils::symbol_name(ASR::down_cast<ASR::Var_t>(orig_func->m_args[j])->m_v));
                                    arg_idx = j;
                                    idx_found = arg_name_2 == arg_name;
                                }
                            }
                        }
                        ASR::call_arg_t call_arg;
                        call_arg.loc = arg->base.loc;
                        if( idx_found ) {
                            arg = orig_args[arg_idx].m_value;
                        }
                        call_arg.m_value = arg;
                        args.push_back(al, call_arg);
                    }
                    ASR::expr_t *new_call_expr = ASR::down_cast<ASR::expr_t>(ASR::make_FunctionCall_t(
                        al, fc->base.base.loc, new_es, nullptr, args.p, args.n, fc->m_type, fc->m_value, fc->m_dt));
                    func_calls[i] = new_call_expr;
                } else {
                    // If the potential_call is not a call but any other expression
                    ASR::expr_t *arg = potential_call;
                    size_t arg_idx = 0;
                    bool idx_found = false;
                    if (ASR::is_a<ASR::Var_t>(*arg)) {
                        std::string arg_name = ASRUtils::symbol_name(ASR::down_cast<ASR::Var_t>(arg)->m_v);
                        // Finds the index of the argument to be used for substitution
                        // Basically if we are calling maybe(3, ret_type=character(len=n))
                        // where 3 is an argument to be maybe and n is one of the arguments
                        // accepted by maybe i.e., maybe has a signature maybe(n). Then, we will
                        // replace n with 3. So, the call would become,
                        // maybe(string, ret_type=character(len=3))
                        for( size_t j = 0; j < orig_func->n_args && !idx_found; j++ ) {
                            if( ASR::is_a<ASR::Var_t>(*(orig_func->m_args[j])) ) {
                                std::string arg_name_2 = std::string(ASRUtils::symbol_name(ASR::down_cast<ASR::Var_t>(orig_func->m_args[j])->m_v));
                                arg_idx = j;
                                idx_found = arg_name_2 == arg_name;
                            }
                        }
                    }

                    if( idx_found ) {
                        func_calls[i] = orig_args[arg_idx].m_value;
                    }
                }
            }
        }
    }

    ASR::ttype_t* handle_return_type(ASR::ttype_t *return_type, const Location &loc,
                                     Vec<ASR::call_arg_t>& args, bool is_external_func_=true,
                                     ASR::Function_t* f=nullptr) {
        // Rebuild the return type if needed and make FunctionCalls use ExternalSymbol
        std::vector<ASR::expr_t*> func_calls;
        switch( return_type->type ) {
            case ASR::ttypeType::Character: {
                ASR::Character_t *t = ASR::down_cast<ASR::Character_t>(return_type);
                func_calls.push_back(t->m_len_expr);
                fill_func_calls_ttype_t(func_calls, t->m_dims, t->n_dims);
                fix_function_calls_ttype_t(func_calls, args, f, is_external_func_);
                Vec<ASR::dimension_t> new_dims;
                new_dims.reserve(al, t->n_dims);
                for( size_t i = 1; i < func_calls.size(); i += 2 ) {
                    ASR::dimension_t new_dim;
                    new_dim.loc = func_calls[i]->base.loc;
                    new_dim.m_start = func_calls[i];
                    new_dim.m_end = func_calls[i + 1];
                    new_dims.push_back(al, new_dim);
                }
                int64_t a_len = t->m_len;
                if( func_calls[0] ) {
                    a_len = ASRUtils::extract_len<SemanticError>(func_calls[0], loc);
                }
                return ASRUtils::TYPE(ASR::make_Character_t(al, loc, t->m_kind, a_len, func_calls[0], new_dims.p, new_dims.size()));
            }
            case ASR::ttypeType::Integer: {
                ASR::Integer_t *t = ASR::down_cast<ASR::Integer_t>(return_type);
                fill_func_calls_ttype_t(func_calls, t->m_dims, t->n_dims);
                fix_function_calls_ttype_t(func_calls, args, f, is_external_func_);
                Vec<ASR::dimension_t> new_dims;
                new_dims.reserve(al, t->n_dims);
                for( size_t i = 0; i < func_calls.size(); i += 2 ) {
                    ASR::dimension_t new_dim;
                    new_dim.loc = func_calls[i]->base.loc;
                    new_dim.m_start = func_calls[i];
                    new_dim.m_end = func_calls[i + 1];
                    new_dims.push_back(al, new_dim);
                }
                return ASRUtils::TYPE(ASR::make_Integer_t(al, loc, t->m_kind, new_dims.p, new_dims.size()));
            }
            case ASR::ttypeType::Real: {
                ASR::Real_t *t = ASR::down_cast<ASR::Real_t>(return_type);
                fill_func_calls_ttype_t(func_calls, t->m_dims, t->n_dims);
                fix_function_calls_ttype_t(func_calls, args, f, is_external_func_);
                Vec<ASR::dimension_t> new_dims;
                new_dims.reserve(al, t->n_dims);
                for( size_t i = 0; i < func_calls.size(); i += 2 ) {
                    ASR::dimension_t new_dim;
                    new_dim.loc = func_calls[i]->base.loc;
                    new_dim.m_start = func_calls[i];
                    new_dim.m_end = func_calls[i + 1];
                    new_dims.push_back(al, new_dim);
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

    ASR::asr_t* symbol_resolve_external_generic_procedure(
                const Location &loc,
                ASR::symbol_t *v, Vec<ASR::call_arg_t>& args) {
        ASR::ExternalSymbol_t *p = ASR::down_cast<ASR::ExternalSymbol_t>(v);
        ASR::symbol_t *f2 = ASR::down_cast<ASR::ExternalSymbol_t>(v)->m_external;
        ASR::GenericProcedure_t *g = ASR::down_cast<ASR::GenericProcedure_t>(f2);
        int idx = ASRUtils::select_generic_procedure(args, *g, loc,
                    [&](const std::string &msg, const Location &loc) { throw SemanticError(msg, loc); });
        ASR::symbol_t *final_sym;
        final_sym = g->m_procs[idx];
        if (!ASR::is_a<ASR::Function_t>(*final_sym)) {
            throw SemanticError("ExternalSymbol must point to a Function", loc);
        }
        ASR::ttype_t *return_type = LFortran::ASRUtils::EXPR2VAR(ASR::down_cast<ASR::Function_t>(final_sym)->m_return_var)->m_type;
        return_type = handle_return_type(return_type, loc, args, ASR::is_a<ASR::ExternalSymbol_t>(*v), ASR::down_cast<ASR::Function_t>(final_sym));
        // Create ExternalSymbol for the final subroutine:
        // We mangle the new ExternalSymbol's local name as:
        //   generic_procedure_local_name @
        //     specific_procedure_remote_name
        std::string local_sym = std::string(p->m_name) + "@"
            + LFortran::ASRUtils::symbol_name(final_sym);
        if (current_scope->get_symbol(local_sym)
            == nullptr) {
            Str name;
            name.from_str(al, local_sym);
            char *cname = name.c_str(al);
            ASR::asr_t *sub = ASR::make_ExternalSymbol_t(
                al, g->base.base.loc,
                /* a_symtab */ current_scope,
                /* a_name */ cname,
                final_sym,
                p->m_module_name, nullptr, 0, LFortran::ASRUtils::symbol_name(final_sym),
                ASR::accessType::Private
                );
            final_sym = ASR::down_cast<ASR::symbol_t>(sub);
            current_scope->add_symbol(local_sym, final_sym);
        } else {
            final_sym = current_scope->get_symbol(local_sym);
        }
        ASR::expr_t *value = nullptr;
        ASR::symbol_t* final_sym2 = LFortran::ASRUtils::symbol_get_past_external(final_sym);
        if (ASR::is_a<ASR::Function_t>(*final_sym2)) {
            ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(final_sym2);
            if (ASRUtils::is_intrinsic_function(f)) {
                ASR::symbol_t* v2 = LFortran::ASRUtils::symbol_get_past_external(v);
                ASR::GenericProcedure_t *gp = ASR::down_cast<ASR::GenericProcedure_t>(v2);

                ASR::asr_t *result = intrinsic_function_transformation(al, loc, gp->m_name, args);
                if (result) {
                    return result;
                } else {
                    value = intrinsic_procedures.comptime_eval(gp->m_name, al, loc, args);
                }
            }
        }
        return ASR::make_FunctionCall_t(al, loc,
            final_sym, v, args.p, args.size(), return_type,
            value, nullptr);
    }

    ASR::asr_t* create_ClassProcedure(const Location &loc,
                AST::fnarg_t* m_args, size_t n_args,
                    ASR::symbol_t *v,
                    ASR::expr_t *v_expr) {
        Vec<ASR::call_arg_t> args;
        visit_expr_list(m_args, n_args, args);
        ASR::ttype_t *type = nullptr;
        ASR::ClassProcedure_t *v_class_proc = ASR::down_cast<ASR::ClassProcedure_t>(v);
        type = LFortran::ASRUtils::EXPR2VAR(ASR::down_cast<ASR::Function_t>(v_class_proc->m_proc)->m_return_var)->m_type;
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
                    [&](const std::string &msg, const Location &loc) { throw SemanticError(msg, loc); });
            ASR::symbol_t *final_sym = p->m_procs[idx];

            ASR::ttype_t *type;
            type = LFortran::ASRUtils::EXPR2VAR(ASR::down_cast<ASR::Function_t>(final_sym)->m_return_var)->m_type;
            return ASR::make_FunctionCall_t(al, loc,
                final_sym, v, args.p, args.size(), type,
                nullptr, nullptr);
        }
    }

    ASR::asr_t* create_Function(const Location &loc,
                Vec<ASR::call_arg_t>& args, ASR::symbol_t *v) {
        ASR::symbol_t *f2 = ASRUtils::symbol_get_past_external(v);
        ASR::ttype_t *return_type = ASRUtils::EXPR2VAR(ASR::down_cast<ASR::Function_t>(f2)->m_return_var)->m_type;
        return_type = handle_return_type(return_type, loc, args, ASR::is_a<ASR::ExternalSymbol_t>(*v),
                                         ASR::down_cast<ASR::Function_t>(f2));
        ASR::expr_t* value = nullptr;
        if (ASR::is_a<ASR::ExternalSymbol_t>(*v)) {
            // Populate value
            ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(f2);
            if (ASRUtils::is_intrinsic_function(f)) {
                ASR::asr_t* result = intrinsic_function_transformation(al, loc, f->m_name, args);
                if (result) {
                    return result;
                } else {
                    value = intrinsic_procedures.comptime_eval(f->m_name, al, loc, args);
                }
            }
        }
        return ASR::make_FunctionCall_t(al, loc, v, nullptr,
            args.p, args.size(), return_type, value, nullptr);
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
                ASR::symbol_t *v, Vec<ASR::call_arg_t>& args) {
        ASR::symbol_t *f2 = ASRUtils::symbol_get_past_external(v);
        if (ASR::is_a<ASR::Function_t>(*f2)) {
            return create_Function(loc, args, v);
        } else {
            LFORTRAN_ASSERT(ASR::is_a<ASR::GenericProcedure_t>(*f2))
            return create_GenericProcedure(loc, args, v);
        }
    }

    ASR::asr_t* resolve_variable2(const Location &loc, const std::string &var_name,
            const std::string &dt_name, SymbolTable*& scope) {
        ASR::symbol_t *v = scope->resolve_symbol(dt_name);
        if (!v) {
            throw SemanticError("Variable '" + dt_name + "' not declared", loc);
        }
        ASR::Variable_t* v_variable = ASR::down_cast<ASR::Variable_t>(v);
        if (ASR::is_a<ASR::Derived_t>(*ASRUtils::type_get_past_pointer(v_variable->m_type)) ||
                ASR::is_a<ASR::Class_t>(*v_variable->m_type)) {
            ASR::ttype_t* v_type = ASRUtils::type_get_past_pointer(v_variable->m_type);
            ASR::symbol_t *derived_type = nullptr;
            if (ASR::is_a<ASR::Derived_t>(*v_type)) {
                derived_type = ASR::down_cast<ASR::Derived_t>(v_type)->m_derived_type;
            } else if (ASR::is_a<ASR::Class_t>(*v_type)) {
                derived_type = ASR::down_cast<ASR::Class_t>(v_type)->m_class_type;
            }
            ASR::DerivedType_t *der_type;
            if (ASR::is_a<ASR::ExternalSymbol_t>(*derived_type)) {
                ASR::ExternalSymbol_t* der_ext = ASR::down_cast<ASR::ExternalSymbol_t>(derived_type);
                ASR::symbol_t* der_sym = der_ext->m_external;
                if (der_sym == nullptr) {
                    throw SemanticError("'" + std::string(der_ext->m_name) + "' isn't a Derived type.", loc);
                } else {
                    der_type = ASR::down_cast<ASR::DerivedType_t>(der_sym);
                }
            } else {
                der_type = ASR::down_cast<ASR::DerivedType_t>(derived_type);
            }
            ASR::DerivedType_t *par_der_type = der_type;
            // scope = der_type->m_symtab;
            // ASR::symbol_t* member = der_type->m_symtab->resolve_symbol(var_name);
            ASR::symbol_t* member = nullptr;
            while( par_der_type != nullptr && member == nullptr ) {
                scope = par_der_type->m_symtab;
                member = par_der_type->m_symtab->resolve_symbol(var_name);
                if( par_der_type->m_parent != nullptr ) {
                    par_der_type = ASR::down_cast<ASR::DerivedType_t>(LFortran::ASRUtils::symbol_get_past_external(par_der_type->m_parent));
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
            const std::string dt_name, SymbolTable*& scope, ASR::symbol_t* parent=nullptr) {
        ASR::symbol_t* v = nullptr;
        ASR::DerivedType_t* der_type = nullptr;
        if( parent == nullptr ) {
            v = scope->resolve_symbol(dt_name);
            if (!v) {
                throw SemanticError("Variable '" + dt_name + "' not declared", loc);
            }
            ASR::Variable_t* v_variable = ASR::down_cast<ASR::Variable_t>(v);
            ASR::ttype_t* v_type = ASRUtils::type_get_past_pointer(v_variable->m_type);
            if ( ASR::is_a<ASR::Derived_t>(*v_type) || ASR::is_a<ASR::Class_t>(*v_type)) {
                ASR::Derived_t* der = (ASR::Derived_t*)(&(v_type->base));
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
            } else {
                throw SemanticError("Variable '" + dt_name + "' is not a derived type", loc);
            }
        } else {
            v = ASRUtils::symbol_get_past_external(parent);
            der_type = ASR::down_cast<ASR::DerivedType_t>(v);
        }
        ASR::symbol_t* member = der_type->m_symtab->resolve_symbol(var_name);
        if( member != nullptr ) {
            scope = der_type->m_symtab;
        } else if( der_type->m_parent != nullptr ) {
            member = resolve_deriv_type_proc(loc, var_name, "", scope, der_type->m_parent);
        } else {
            throw SemanticError("Variable '" + dt_name + "' doesn't have any member named, '" + var_name + "'.", loc);
        }
        return member;
    }

    ASR::asr_t* create_ArraySize(const AST::FuncCallOrArray_t& x) {
        if( !(x.n_args + x.n_keywords <= 3 && x.n_args >= 1)  ) {
            throw SemanticError("Incorrect number of arguments "
                                "to the array size() intrinsic "
                                "function; It accepts the array "
                                "and optionally the dimension and "
                                "the result kind.",
                                x.base.base.loc);
        }

        ASR::expr_t* dim = nullptr;
        ASR::expr_t* kind = nullptr;
        ASR::ttype_t* type = ASRUtils::TYPE(ASR::make_Integer_t(
                                                al, x.base.base.loc, 4,
                                                nullptr, 0));;

        LFORTRAN_ASSERT(x.m_args[0].m_end != nullptr);
        this->visit_expr(*x.m_args[0].m_end);
        ASR::expr_t* v_Var = ASRUtils::EXPR(tmp);
        AST::expr_t *dim_expr = nullptr, *kind_expr = nullptr;
        if( x.n_keywords == 0 ) {
            if( x.n_args >= 2 ) {
                dim_expr = x.m_args[1].m_end;
                LFORTRAN_ASSERT(dim_expr != nullptr);
            }
            if( x.n_args >= 3 ) {
                kind_expr = x.m_args[2].m_end;
                LFORTRAN_ASSERT(kind_expr != nullptr);
            }
        } else if( x.n_keywords == 1 ) {
            if( std::string(x.m_keywords[0].m_arg) == "dim" ) {
                dim_expr = x.m_keywords[0].m_value;
                if( x.n_args != 1 ) {
                    throw SemanticError("dim argument has been "
                                        "specified both as keyword "
                                        "and positional argument.",
                                        x.base.base.loc);
                }
            } else if( std::string(x.m_keywords[0].m_arg) == "kind" ) {
                kind_expr = x.m_keywords[0].m_value;
                if( x.n_args == 2 ) {
                    dim_expr = x.m_args[1].m_end;
                    LFORTRAN_ASSERT(dim_expr != nullptr);
                }
            } else {
                throw SemanticError("Unrecognized keyword argument, " +
                                    std::string(x.m_keywords[0].m_arg) +
                                    ".", x.base.base.loc);
            }
        } else if( x.n_keywords == 2 ) {
            std::string keyword0_name = x.m_keywords[0].m_arg;
            std::string keyword1_name = x.m_keywords[1].m_arg;
            LFORTRAN_ASSERT(keyword0_name != keyword1_name);
            if( keyword0_name == "dim" && keyword1_name == "kind" ) {
                dim_expr = x.m_keywords[0].m_value;
                kind_expr = x.m_keywords[1].m_value;
            } else if( keyword0_name == "kind" && keyword1_name == "dim" ) {
                dim_expr = x.m_keywords[1].m_value;
                kind_expr = x.m_keywords[0].m_value;
            } else {
                throw SemanticError("Unrecognized keyword arguments, " +
                                    keyword0_name + " and " + keyword1_name +
                                    ".", x.base.base.loc);
            }
        }
        if( dim_expr ) {
            this->visit_expr(*dim_expr);
            dim = ASRUtils::EXPR(tmp);
        }
        if( kind_expr ) {
            this->visit_expr(*kind_expr);
            kind = ASRUtils::EXPR(tmp);
            if( ASRUtils::expr_value(kind) ) {
                type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc,
                                        ASR::down_cast<ASR::IntegerConstant_t>(kind)->m_n ,
                                        nullptr, 0));
            }
        }

        return ASR::make_ArraySize_t(al, x.base.base.loc, v_Var, dim, type, nullptr);
    }

    void visit_FuncCallOrArray(const AST::FuncCallOrArray_t &x) {
        SymbolTable *scope = current_scope;
        std::string var_name = to_lower(x.m_func);
        ASR::symbol_t *v = nullptr;
        ASR::expr_t *v_expr = nullptr;
        // If this is a type bound procedure (in a class) it won't be in the
        // main symbol table. Need to check n_member.
        if (x.n_member == 1) {
            ASR::symbol_t *obj = current_scope->resolve_symbol(x.m_member[0].m_name);
            ASR::asr_t *obj_var = ASR::make_Var_t(al, x.base.base.loc, obj);
            v_expr = LFortran::ASRUtils::EXPR(obj_var);
            v = resolve_deriv_type_proc(x.base.base.loc, var_name,
                x.m_member[0].m_name, scope);
        } else {
            v = current_scope->resolve_symbol(var_name);
        }
        if (!v) {
            if( intrinsic_procedures_as_asr_nodes.is_intrinsic_present_in_ASR(var_name) ) {
                if( var_name == "size" ) {
                    tmp = create_ArraySize(x);
                }
                return ;
            }
            v = resolve_intrinsic_function(x.base.base.loc, var_name);
        }
        ASR::symbol_t *f2 = ASRUtils::symbol_get_past_external(v);
        if (ASR::is_a<ASR::Function_t>(*f2) || ASR::is_a<ASR::GenericProcedure_t>(*f2)) {
            Vec<ASR::call_arg_t> args;
            visit_expr_list(x.m_args, x.n_args, args);
            if (x.n_keywords > 0) {
                if (ASR::is_a<ASR::Function_t>(*f2)) {
                    ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(f2);
                    visit_kwargs(args, x.m_keywords, x.n_keywords,
                        f->m_args, f->n_args, x.base.base.loc, f);
                } else {
                    LFORTRAN_ASSERT(ASR::is_a<ASR::GenericProcedure_t>(*f2))
                    throw SemanticError("Keyword arguments are not implemented for generic functions yet",
                        x.base.base.loc);
                }
            }
            tmp = create_FunctionCall(x.base.base.loc, v, args);
        } else {
            switch (f2->type) {
            case(ASR::symbolType::Variable):
                tmp = create_ArrayRef(x.base.base.loc, x.m_args, x.n_args, v, f2); break;
            case(ASR::symbolType::DerivedType):
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
            throw SemanticError("Function '" + remote_sym + "' not found"
                " or not implemented yet (if it is intrinsic)",
                loc);
        }
        std::string module_name = intrinsic_procedures.get_module(remote_sym, loc);

        SymbolTable *tu_symtab = ASRUtils::get_tu_symtab(current_scope);
        std::string rl_path = get_runtime_library_dir();
        ASR::Module_t *m = ASRUtils::load_module(al, tu_symtab, module_name,
                loc, true, rl_path, true,
                [&](const std::string &msg, const Location &loc) { throw SemanticError(msg, loc); }
                );

        ASR::symbol_t *t = m->m_symtab->resolve_symbol(remote_sym);
        if (!t) {
            throw SemanticError("The symbol '" + remote_sym
                + "' not found in the module '" + module_name + "'",
                loc);
        } else if (! (ASR::is_a<ASR::GenericProcedure_t>(*t)
                    || ASR::is_a<ASR::Function_t>(*t)
                    || ASR::is_a<ASR::Subroutine_t>(*t))) {
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
                LFORTRAN_ASSERT(false);
                op = ASR::binopType::Pow;
            }
        }

        // Cast LHS or RHS if necessary
        ASR::ttype_t *left_type = LFortran::ASRUtils::expr_type(left);
        ASR::ttype_t *right_type = LFortran::ASRUtils::expr_type(right);
        ASR::expr_t *overloaded = nullptr;
        if ( ASRUtils::use_overloaded(left, right, op,
            intrinsic_op_name, curr_scope, asr, al,
            x.base.base.loc, [&](const std::string &msg, const Location &loc) { throw SemanticError(msg, loc); }) ) {
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
        // Assign evaluation to `value` if possible, otherwise leave nullptr
        if (LFortran::ASRUtils::expr_value(left) != nullptr &&
                    LFortran::ASRUtils::expr_value(right) != nullptr) {
            if (ASR::is_a<LFortran::ASR::Integer_t>(*dest_type)) {
                int64_t left_value = ASR::down_cast<ASR::IntegerConstant_t>(
                                        LFortran::ASRUtils::expr_value(left))
                                        ->m_n;
                int64_t right_value = ASR::down_cast<ASR::IntegerConstant_t>(
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
                value = ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(
                    al, x.base.base.loc, result, dest_type));
            } else if (ASR::is_a<LFortran::ASR::Real_t>(*dest_type)) {
                double left_value = ASR::down_cast<ASR::RealConstant_t>(
                                        LFortran::ASRUtils::expr_value(left))
                                        ->m_r;
                double right_value = ASR::down_cast<ASR::RealConstant_t>(
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
                    ASR::make_RealConstant_t(al, x.base.base.loc, result, dest_type));
            } else if (ASR::is_a<LFortran::ASR::Complex_t>(*dest_type)) {
                ASR::ComplexConstant_t *left0
                    = ASR::down_cast<ASR::ComplexConstant_t>(
                            LFortran::ASRUtils::expr_value(left));
                ASR::ComplexConstant_t *right0
                    = ASR::down_cast<ASR::ComplexConstant_t>(
                            LFortran::ASRUtils::expr_value(right));
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
                        LFORTRAN_ASSERT(false);
                        op = ASR::binopType::Pow;
                    }
                }
                value = ASR::down_cast<ASR::expr_t>(
                    ASR::make_ComplexConstant_t(al, x.base.base.loc,
                        std::real(result), std::imag(result), dest_type));
            }
        }
        asr = ASR::make_BinOp_t(al, x.base.base.loc, left, op, right, dest_type,
                                value, overloaded);
    }


    void visit_BinOp(const AST::BinOp_t &x) {
        this->visit_expr(*x.m_left);
        ASR::expr_t *left = LFortran::ASRUtils::EXPR(tmp);
        this->visit_expr(*x.m_right);
        ASR::expr_t *right = LFortran::ASRUtils::EXPR(tmp);
        visit_BinOp2(al, x, left, right, tmp, binop2str[x.m_op], current_scope);
    }

    void visit_BoolOp(const AST::BoolOp_t &x) {
        this->visit_expr(*x.m_left);
        ASR::expr_t *left = LFortran::ASRUtils::EXPR(tmp);
        this->visit_expr(*x.m_right);
        ASR::expr_t *right = LFortran::ASRUtils::EXPR(tmp);
        CommonVisitorMethods::visit_BoolOp(al, x, left, right, tmp, diag);
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
        CommonVisitorMethods::visit_Compare(al, x, left, right, tmp,
                                            cmpop2str[x.m_op], current_scope);
    }

    void visit_Parenthesis(const AST::Parenthesis_t &x) {
        this->visit_expr(*x.m_operand);
    }

    void visit_Logical(const AST::Logical_t &x) {
        ASR::ttype_t *type = LFortran::ASRUtils::TYPE(ASR::make_Logical_t(al, x.base.base.loc,
                4, nullptr, 0));
        tmp = ASR::make_LogicalConstant_t(al, x.base.base.loc, x.m_value, type);
    }

    void visit_String(const AST::String_t &x) {
        int s_len = strlen(x.m_s);
        ASR::ttype_t *type = LFortran::ASRUtils::TYPE(ASR::make_Character_t(al, x.base.base.loc,
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
                    const ASR::symbol_t *v3 = LFortran::ASRUtils::symbol_get_past_external(v);
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
        ASR::ttype_t *type = LFortran::ASRUtils::TYPE(ASR::make_Integer_t(al,
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
                const ASR::symbol_t *v3 = LFortran::ASRUtils::symbol_get_past_external(v);
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
        ASR::ttype_t *type = LFortran::ASRUtils::TYPE(ASR::make_Real_t(al, x.base.base.loc,
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
            LFORTRAN_ASSERT(ast_list[i].m_end != nullptr);
            this->visit_expr(*ast_list[i].m_end);
            ASR::expr_t *expr = LFortran::ASRUtils::EXPR(tmp);
            asr_list.push_back(al, expr);
        }
        return asr_list;
    }

    void visit_expr_list(AST::fnarg_t *ast_list, size_t n, Vec<ASR::call_arg_t>& call_args) {
        call_args.reserve(al, n);
        for (size_t i = 0; i < n; i++) {
            LFORTRAN_ASSERT(ast_list[i].m_end != nullptr);
            this->visit_expr(*ast_list[i].m_end);
            ASR::expr_t *expr = LFortran::ASRUtils::EXPR(tmp);
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
                ASR::expr_t **fn_args, size_t fn_n_args, const Location &loc, T* fn) {
        size_t n_args = args.size();
        std::vector<std::string> optional_args;
        for( auto itr = fn->m_symtab->get_scope().begin(); itr != fn->m_symtab->get_scope().end();
             itr++ ) {
            ASR::symbol_t* fn_sym = itr->second;
            if( ASR::is_a<ASR::Variable_t>(*fn_sym) ) {
                ASR::Variable_t* fn_var = ASR::down_cast<ASR::Variable_t>(fn_sym);
                if( fn_var->m_presence == ASR::presenceType::Optional ) {
                    optional_args.push_back(itr->first);
                }
            }
        }
        size_t n_optional = optional_args.size();
        if (n_args + n > fn_n_args + n_optional) {
            throw SemanticError(
                "Procedure accepts " + std::to_string(fn_n_args + n_optional)
                + " arguments, but " + std::to_string(n_args + n)
                + " were provided",
                loc
            );
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
        for (size_t i = 0; i < n_optional; i++) {
            ASR::call_arg_t call_arg;
            call_arg.loc = loc;
            call_arg.m_value = nullptr;
            args.push_back(al, call_arg);
        }
        for (size_t i=0; i<n; i++) {
            this->visit_expr(*kwargs[i].m_value);
            ASR::expr_t *expr = LFortran::ASRUtils::EXPR(tmp);
            std::string name = to_lower(kwargs[i].m_arg);
            auto search_optional = std::find(optional_args.begin(), optional_args.end(), name);
            if( search_optional != optional_args.end() ) {
                size_t kwarg_idx = std::distance(optional_args.begin(), search_optional);
                args.p[kwarg_idx + offset].m_value = expr;
                args.p[kwarg_idx + offset].loc = expr->base.loc;
            } else {
                auto search = std::find(fn_args2.begin(), fn_args2.end(), name);
                if (search != fn_args2.end()) {
                    size_t idx = std::distance(fn_args2.begin(), search);
                    if (idx < n_args) {
                        throw SemanticError("Keyword argument is already specified as a non-keyword argument", loc);
                    }
                    if (args[idx].m_value != nullptr) {
                        throw SemanticError("Keyword argument is already specified as another keyword argument ", loc);
                    }
                    args.p[idx].loc = expr->base.loc;
                    args.p[idx].m_value = expr;
                } else {
                    throw SemanticError("Keyword argument not found " + name, loc);
                }
            }
        }
        for (size_t i=0; i < offset; i++) {
            if (args[i].m_value == nullptr) {
                throw SemanticError("Argument was not specified", loc);
            }
        }
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


};

} // namespace LFortran

#endif /* LFORTRAN_SEMANTICS_AST_COMMON_VISITOR_H */
