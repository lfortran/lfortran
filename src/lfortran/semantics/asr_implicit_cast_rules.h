#ifndef LFORTRAN_SEMANTICS_ASR_IMPLICIT_CAST_RULES_H
#define LFORTRAN_SEMANTICS_ASR_IMPLICIT_CAST_RULES_H

#include <libasr/asr.h>
#include <lfortran/ast.h>
#include <lfortran/semantics/semantic_exception.h>


#define num_types 6

namespace LCompilers::LFortran {
class ImplicitCastRules {
private:
  //! Default case when no conversion is needed.
  static const int default_case = -1;
  //! Error case when conversion is not possible or is illegal.
  static const int error_case = -2;
  static const int integer_to_real = ASR::cast_kindType::IntegerToReal;
  static const int integer_to_integer = ASR::cast_kindType::IntegerToInteger;
  static const int real_to_integer = ASR::cast_kindType::RealToInteger;
  static const int real_to_complex = ASR::cast_kindType::RealToComplex;
  static const int integer_to_complex = ASR::cast_kindType::IntegerToComplex;
  static const int integer_to_logical = ASR::cast_kindType::IntegerToLogical;
  static const int complex_to_complex = ASR::cast_kindType::ComplexToComplex;
  static const int complex_to_real = ASR::cast_kindType::ComplexToReal;
  static const int real_to_real = ASR::cast_kindType::RealToReal;
  static const int complex_to_integer = ASR::cast_kindType::ComplexToInteger;

  //! Stores the variable part of error messages to be passed to SemanticError.
  static constexpr const char *type_names[num_types][2] = {
      {"Integer", "Integer Pointer"},
      {"Real", "Integer or Real or Real Pointer"},
      {"Complex", "Integer, Real or Complex or Complex Pointer"},
      {"Character", "Character Pointer"},
      {"Logical", "Integer or Logical Pointer"},
      {"Derived", "Derived Pointer"}
  };

  /*
   * Rule map for performing implicit cast represented by a 2D integer array.
   *
   * Key is the pair of indices with row index denoting the source type
   * and column index denoting the destination type.
   */
  static constexpr const int rule_map[num_types][num_types] = {

      // Integer
      {integer_to_integer, integer_to_real, integer_to_complex, error_case,
       integer_to_logical, error_case},

      // Real
      {real_to_integer, real_to_real, real_to_complex, default_case,
       default_case, default_case},

      // Complex
      {complex_to_integer, complex_to_real, complex_to_complex, default_case,
       default_case, default_case},

      // Character
      {default_case, default_case, default_case, default_case, default_case,
       default_case},

      // Logical
      {default_case, default_case, default_case, default_case, default_case,
       default_case},

      // Derived
      {default_case, default_case, default_case, default_case, default_case,
       default_case},
  };

  /*
   * Priority of different types to be used in conversion
   * when source and destination are directly not deducible.
   */
  static constexpr const int type_priority[num_types] = {
      4,  // Integer
      5,  // Real
      6,  // Complex
      -1, // Character
      -1, // Logical
      -1  // Derived
  };

public:
  /*
   * Adds ImplicitCast node if necessary.
   *
   * @param al Allocator&
   * @param a_loc Location&
   * @param convert_can ASR::expr_t** Address of the pointer to
   *                                 conversion candidate.
   * @param source_type ASR::ttype_t* Source type.
   * @param dest_type AST::ttype_t* Destination type.
   */
  static void set_converted_value(Allocator &al, const Location &a_loc,
                                  ASR::expr_t **convert_can,
                                  ASR::ttype_t *source_type,
                                  ASR::ttype_t *dest_type) {
    if (source_type->type == dest_type->type ||
        ASRUtils::is_same_type_pointer(source_type, dest_type)) {
      bool is_source_pointer = ASRUtils::is_pointer(source_type);
      bool is_dest_pointer = ASRUtils::is_pointer(dest_type);
      if (is_source_pointer && !is_dest_pointer) {
        ASR::ttype_t *temp = source_type;
        source_type = dest_type;
        dest_type = temp;
      }
      int source_kind = 0, dest_kind = 1;
      source_kind = ASRUtils::extract_kind_from_ttype_t(source_type);
      dest_kind = ASRUtils::extract_kind_from_ttype_t(dest_type);
      if (source_kind == dest_kind) {
        return;
      }
    }
    ASR::ttype_t *source_type2 = ASRUtils::type_get_past_pointer(source_type);
    ASR::ttype_t *dest_type2 = ASRUtils::type_get_past_pointer(dest_type);
    LCOMPILERS_ASSERT(source_type2->type < num_types);
    LCOMPILERS_ASSERT(dest_type2->type < num_types);
    int cast_kind = rule_map[source_type2->type][dest_type2->type];
    if (cast_kind == error_case) {
      std::string allowed_types_str = type_names[dest_type2->type][1];
      std::string dest_type_str = type_names[dest_type2->type][0];
      std::string error_msg =
          "Only " + allowed_types_str + " can be assigned to " + dest_type_str;
      throw SemanticError(error_msg, a_loc);
    } else if (cast_kind != default_case) {
        ASR::expr_t *value=nullptr;
        if ((ASR::cast_kindType)cast_kind == ASR::cast_kindType::RealToInteger) {
            if (ASRUtils::expr_value(*convert_can)) {
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Integer_t>(*dest_type2))
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Real_t>(*ASRUtils::expr_type(*convert_can)))
                value = ASRUtils::expr_value(*convert_can);
                LCOMPILERS_ASSERT(ASR::is_a<ASR::RealConstant_t>(*value))
                ASR::RealConstant_t *r = ASR::down_cast<ASR::RealConstant_t>(value);
                int64_t i = r->m_r;
                value = (ASR::expr_t *)ASR::make_IntegerConstant_t(al, a_loc,
                    i, dest_type2);
            }
        } else if ((ASR::cast_kindType)cast_kind == ASR::cast_kindType::IntegerToReal) {
            if (ASRUtils::expr_value(*convert_can)) {
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Real_t>(*ASRUtils::type_get_past_pointer(dest_type)))
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Integer_t>(*ASRUtils::expr_type(*convert_can)))
                value = ASRUtils::expr_value(*convert_can);
                if (ASR::is_a<ASR::IntegerConstant_t>(*value)) {
                  ASR::IntegerConstant_t *i = ASR::down_cast<ASR::IntegerConstant_t>(value);
                  double rval = static_cast<double>(i->m_n);
                  value = (ASR::expr_t *)ASR::make_RealConstant_t(al, a_loc,
                                                                 rval, dest_type2);
                } else {
                  // TODO: Handle cases where this is say, a constant Array
                  // See https://gitlab.com/lfortran/lfortran/-/merge_requests/1162#note_647992506
                  value = nullptr; // Reset
                }
            }

        } else if ((ASR::cast_kindType)cast_kind == ASR::cast_kindType::RealToReal) {
            if (ASRUtils::expr_value(*convert_can)) {
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Real_t>(*ASRUtils::type_get_past_pointer(dest_type)))
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Real_t>(*ASRUtils::expr_type(*convert_can)))
                value = ASRUtils::expr_value(*convert_can);
                LCOMPILERS_ASSERT(ASR::is_a<ASR::RealConstant_t>(*value))
                ASR::RealConstant_t *r = ASR::down_cast<ASR::RealConstant_t>(value);
                double rval = r->m_r;
                value = (ASR::expr_t *)ASR::make_RealConstant_t(al, a_loc,
                    rval, dest_type2);
            }
        } else if ((ASR::cast_kindType)cast_kind == ASR::cast_kindType::RealToComplex) {
            if (ASRUtils::expr_value(*convert_can)) {
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Complex_t>(*ASRUtils::type_get_past_pointer(dest_type)))
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Real_t>(*ASRUtils::expr_type(*convert_can)))
                value = ASRUtils::expr_value(*convert_can);
                LCOMPILERS_ASSERT(ASR::is_a<ASR::RealConstant_t>(*value))
                ASR::RealConstant_t *r = ASR::down_cast<ASR::RealConstant_t>(value);
                double rval = r->m_r;
                value = (ASR::expr_t *)ASR::make_ComplexConstant_t(al, a_loc,
                    rval, 0, dest_type2);
            }
        } else if ((ASR::cast_kindType)cast_kind == ASR::cast_kindType::ComplexToReal) {
            if (ASRUtils::expr_value(*convert_can)) {
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Real_t>(*ASRUtils::type_get_past_pointer(dest_type)))
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Complex_t>(*ASRUtils::expr_type(*convert_can)))
                value = ASRUtils::expr_value(*convert_can);
                LCOMPILERS_ASSERT(ASR::is_a<ASR::ComplexConstant_t>(*value))
                ASR::ComplexConstant_t *r = ASR::down_cast<ASR::ComplexConstant_t>(value);
                double rval = r->m_re;
                value = (ASR::expr_t *)ASR::make_RealConstant_t(al, a_loc,
                    rval, dest_type2);
            }
        } else if ((ASR::cast_kindType)cast_kind == ASR::cast_kindType::ComplexToInteger) {
            if (ASRUtils::expr_value(*convert_can)) {
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_pointer(dest_type)))
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Complex_t>(*ASRUtils::expr_type(*convert_can)))
                value = ASRUtils::expr_value(*convert_can);
                LCOMPILERS_ASSERT(ASR::is_a<ASR::ComplexConstant_t>(*value))
                ASR::ComplexConstant_t *r = ASR::down_cast<ASR::ComplexConstant_t>(value);
                double rval = r->m_re;
                int64_t ival = (int64_t)rval;
                value = (ASR::expr_t *)ASR::make_IntegerConstant_t(al, a_loc,
                    ival, dest_type2);
            }
        } else if ((ASR::cast_kindType)cast_kind == ASR::cast_kindType::IntegerToInteger) {
            if (ASRUtils::expr_value(*convert_can)) {
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Integer_t>(*dest_type2))
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Integer_t>(*ASRUtils::expr_type(*convert_can)))
                value = ASRUtils::expr_value(*convert_can);
                LCOMPILERS_ASSERT(ASR::is_a<ASR::IntegerConstant_t>(*value))
                ASR::IntegerConstant_t *i = ASR::down_cast<ASR::IntegerConstant_t>(value);
                int64_t ival = i->m_n;
                value = (ASR::expr_t *)ASR::make_IntegerConstant_t(al, a_loc,
                    ival, dest_type2);
            }

        }

      *convert_can = (ASR::expr_t *)ASR::make_Cast_t(
          al, a_loc, *convert_can, (ASR::cast_kindType)cast_kind, dest_type,
          value);
    }
  }

  /*
   * Deduces the candidate which is to be casted
   * based on the priority of types.
   *
   * @param left ASR::expr_t** Address of the pointer to left
   *                           element in the operation.
   * @param right ASR::expr_t** Address of the pointer to right
   *                            element in the operation.
   * @param left_type ASR::ttype_t* Pointer to the type of left element.
   * @param right_type ASR::ttype_t* Pointer to the type of right element.
   * @param conversion_cand ASR::expr_t**& Reference to the address of
   *                                      the pointer of conversion
   *                                      candidate.
   * @param source_type ASR::ttype_t** For storing the address of pointer
   *                                  to source type.
   * @param dest_type ASR::ttype_t** For stroing the address of pointer to
   *                                destination type.
   *
   * Note
   * ====
   *
   * Address of pointer have been used so that the contents
   * of the pointer variables are modified which are then
   * used in making the node of different operations. If directly
   * the pointer values are used, then no effect on left or right
   * is observed and ASR construction fails.
   */
  static void find_conversion_candidate(ASR::expr_t **left, ASR::expr_t **right,
                                        ASR::ttype_t *left_type,
                                        ASR::ttype_t *right_type,
                                        ASR::expr_t **&conversion_cand,
                                        ASR::ttype_t **source_type,
                                        ASR::ttype_t **dest_type) {

    ASR::ttype_t *left_type2 = ASRUtils::type_get_past_pointer(left_type);
    ASR::ttype_t *right_type2 = ASRUtils::type_get_past_pointer(right_type);
    LCOMPILERS_ASSERT(left_type2->type < num_types);
    LCOMPILERS_ASSERT(right_type2->type < num_types);
    int left_type_p = type_priority[left_type2->type];
    int right_type_p = type_priority[right_type2->type];

    if (left_type_p >= right_type_p) {
      conversion_cand = right;
      *source_type = right_type;
      *dest_type = left_type;
    } else {
      conversion_cand = left;
      *source_type = left_type;
      *dest_type = right_type;
    }
  }

  static int get_type_priority(ASR::ttypeType type_kind) {
    return type_priority[(int) type_kind];
  }
};
} // namespace LCompilers::LFortran

#endif /* LFORTRAN_SEMANTICS_ASR_IMPLICIT_CAST_RULES_H */
