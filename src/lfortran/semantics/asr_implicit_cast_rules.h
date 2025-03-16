#ifndef LFORTRAN_SEMANTICS_ASR_IMPLICIT_CAST_RULES_H
#define LFORTRAN_SEMANTICS_ASR_IMPLICIT_CAST_RULES_H

#include <libasr/asr.h>
#include <lfortran/ast.h>
#include <lfortran/semantics/semantic_exception.h>
#include <libasr/asr_utils.h>


#define num_types 7

using LCompilers::diag::Level;
using LCompilers::diag::Stage;
using LCompilers::diag::Label;
using LCompilers::diag::Diagnostic;
namespace LCompilers::LFortran {
class ImplicitCastRules {
private:
  //! Default case when no conversion is needed.
  static const int no_cast_required = -1;
  //! Error case when conversion is not possible or is illegal.
  static const int error_case = -2;
  static const int integer_to_real = ASR::cast_kindType::IntegerToReal;
  static const int integer_to_integer = ASR::cast_kindType::IntegerToInteger;
  static const int integer_to_unsigned_integer = ASR::cast_kindType::IntegerToUnsignedInteger;
  static const int real_to_integer = ASR::cast_kindType::RealToInteger;
  static const int real_to_complex = ASR::cast_kindType::RealToComplex;
  static const int integer_to_complex = ASR::cast_kindType::IntegerToComplex;
  static const int integer_to_logical = ASR::cast_kindType::IntegerToLogical;
  static const int complex_to_complex = ASR::cast_kindType::ComplexToComplex;
  static const int complex_to_real = ASR::cast_kindType::ComplexToReal;
  static const int real_to_real = ASR::cast_kindType::RealToReal;
  static const int complex_to_integer = ASR::cast_kindType::ComplexToInteger;
  static const int logical_to_integer = ASR::cast_kindType::LogicalToInteger;
  static const int logical_to_real = ASR::cast_kindType::LogicalToReal;

  //! Stores the variable part of error messages to be passed to SemanticError.
  static constexpr const char *type_names[num_types][2] = {
      {"Integer", "Integer Pointer"},
      {"UnsignedInteger", "Unsigned Integer Pointer"},
      {"Real", "Integer or Real or Real Pointer"},
      {"Complex", "Integer, Real or Complex or Complex Pointer"},
      {"String", "String Pointer"},
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
      {integer_to_integer, integer_to_unsigned_integer, integer_to_real, integer_to_complex,
       error_case, integer_to_logical, error_case},

      // Unsigned Integer
      {error_case, error_case, error_case, error_case,
       error_case, error_case, error_case},

      // Real
      {real_to_integer, error_case, real_to_real, real_to_complex,
       no_cast_required, no_cast_required, no_cast_required},

      // Complex
      {complex_to_integer, error_case, complex_to_real, complex_to_complex,
       no_cast_required, no_cast_required, no_cast_required},

      // String
      {no_cast_required, no_cast_required, no_cast_required, no_cast_required,
       no_cast_required, no_cast_required, no_cast_required},

      // Logical
      {no_cast_required, no_cast_required, no_cast_required, no_cast_required,
       no_cast_required, no_cast_required, no_cast_required},

      // Derived
      {no_cast_required, no_cast_required, no_cast_required, no_cast_required,
       no_cast_required, no_cast_required, no_cast_required},
  };

  /*
   * Priority of different types to be used in conversion
   * when source and destination are directly not deducible.
   */
  static constexpr const int type_priority[num_types] = {
      4,  // Integer
      -1,  // Unsigned Integer
      5,  // Real
      6,  // Complex
      -1, // String
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
                                  ASR::ttype_t *dest_type, diag::Diagnostics &diag) {
    if( ASRUtils::types_equal(source_type, dest_type, true) ) {
        return;
    }

    ASR::ttype_t *source_type2 = ASRUtils::type_get_past_array(
      ASRUtils::type_get_past_pointer(source_type));
    ASR::ttype_t *dest_type2 = ASRUtils::type_get_past_array(
      ASRUtils::type_get_past_pointer(dest_type));
    if (source_type2->type == dest_type2->type ||
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

    LCOMPILERS_ASSERT(source_type2->type < num_types);
    LCOMPILERS_ASSERT(dest_type2->type < num_types);
    int cast_kind = rule_map[source_type2->type][dest_type2->type];
    if (cast_kind == error_case) {
      std::string allowed_types_str = type_names[dest_type2->type][1];
      std::string dest_type_str = type_names[dest_type2->type][0];
      std::string error_msg = "Only " + allowed_types_str + " can be assigned to " + dest_type_str;
      diag.add(Diagnostic(error_msg,
          Level::Error, Stage::Semantic, {Label("", {a_loc})}));
      throw SemanticAbort();
    } else if (cast_kind != no_cast_required) {
        ASR::expr_t *value=nullptr;
        if ((ASR::cast_kindType)cast_kind == ASR::cast_kindType::RealToInteger) {
            if (ASRUtils::expr_value(*convert_can)) {
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Integer_t>(*dest_type2))
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Real_t>(*ASRUtils::extract_type(ASRUtils::expr_type(*convert_can))))
                value = ASRUtils::expr_value(*convert_can);
                if (ASR::is_a<ASR::RealConstant_t>(*value)) {
                    ASR::RealConstant_t *r = ASR::down_cast<ASR::RealConstant_t>(value);
                    int64_t i = r->m_r;
                    value = (ASR::expr_t *)ASR::make_IntegerConstant_t(al, a_loc,
                        i, dest_type2);
                } else {
                  LCOMPILERS_ASSERT(ASR::is_a<ASR::ArrayConstant_t>(*value));
                  ASR::ArrayConstant_t* array = ASR::down_cast<ASR::ArrayConstant_t>(value);
                  ASR::Array_t* array_type = ASR::down_cast<ASR::Array_t>(array->m_type);
                  void *data = array->m_data;
                  size_t array_size = ASRUtils::get_fixed_size_of_array(array->m_type);
                  int kind = ASRUtils::extract_kind_from_ttype_t(array->m_type);
                  int dest_kind = ASRUtils::extract_kind_from_ttype_t(dest_type2);
                  void *new_data = nullptr;
                  if (dest_kind == 4) {
                    int *new_array = al.allocate<int>(array_size);
                    for (size_t i = 0; i < array_size; i++) {
                      if ( kind == 4 ) {
                        new_array[i] = ((float*) data)[i];
                      } else {
                        new_array[i] = ((double*) data)[i];
                      }
                    }
                    new_data = new_array;
                  } else if (dest_kind == 8) {
                    int64_t *new_array = al.allocate<int64_t>(array_size);
                    for (size_t i = 0; i < array_size; i++) {
                      if ( kind == 4 ) {
                        new_array[i] = ((float*) data)[i];
                      } else {
                        new_array[i] = ((double*) data)[i];
                      }
                    }
                    new_data = new_array;
                  }
                  if (new_data) {
                    ASR::ttype_t* new_array_type = ASRUtils::TYPE(ASR::make_Array_t(al, dest_type2->base.loc, dest_type2,
                                              array_type->m_dims, array_type->n_dims, ASR::array_physical_typeType::FixedSizeArray));
                    value = ASRUtils::EXPR(ASR::make_ArrayConstant_t(al, value->base.loc, array_size * dest_kind,
                            new_data, new_array_type, array->m_storage_format));
                  }
                }
            }
        } else if ((ASR::cast_kindType)cast_kind == ASR::cast_kindType::IntegerToReal) {
            if (ASRUtils::expr_value(*convert_can)) {
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Real_t>(*dest_type2))
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Integer_t>(
                  *ASRUtils::extract_type(ASRUtils::expr_type(*convert_can))))
                value = ASRUtils::expr_value(*convert_can);
                if (ASR::is_a<ASR::IntegerConstant_t>(*value)) {
                    ASR::IntegerConstant_t *i = ASR::down_cast<ASR::IntegerConstant_t>(value);
                    double rval = static_cast<double>(i->m_n);
                    value = (ASR::expr_t *)ASR::make_RealConstant_t(al, a_loc,
                                                                 rval, dest_type2);
                } else {
                    LCOMPILERS_ASSERT(ASR::is_a<ASR::ArrayConstant_t>(*value));
                    ASR::ArrayConstant_t* array = ASR::down_cast<ASR::ArrayConstant_t>(value);
                    ASR::Array_t* array_type = ASR::down_cast<ASR::Array_t>(array->m_type);
                    int kind = ASRUtils::extract_kind_from_ttype_t(array->m_type);
                    void *data = array->m_data;
                    size_t array_size = ASRUtils::get_fixed_size_of_array(array->m_type);
                    int dest_kind = ASRUtils::extract_kind_from_ttype_t(dest_type2);
                    void *new_data = nullptr;
                    if (dest_kind == 4) {
                        float *new_array = al.allocate<float>(array_size);
                        for (size_t i = 0; i < array_size; i++) {
                          if ( kind == 4) {
                            new_array[i] = float(((int*) data)[i]);
                          } else if ( kind == 8 ) {
                            new_array[i] = float(((int64_t*) data)[i]);
                          } else if ( kind == 2 ) {
                            new_array[i] = float(((int16_t*) data)[i]);
                          } else if ( kind == 1 ) {
                            new_array[i] = float(((int8_t*) data)[i]);
                          }
                        }
                        new_data = new_array;
                    } else if ( dest_kind == 8 ) {
                        double *new_array = al.allocate<double>(array_size);
                        for (size_t i = 0; i < array_size; i++) {
                          if ( kind == 4) {
                            new_array[i] = double(((int*) data)[i]);
                          } else if ( kind == 8 ) {
                            new_array[i] = double(((int64_t*) data)[i]);
                          } else if ( kind == 2 ) {
                            new_array[i] = double(((int16_t*) data)[i]);
                          } else if ( kind == 1 ) {
                            new_array[i] = double(((int8_t*) data)[i]);
                          }
                        }
                        new_data = new_array;
                    }
                    if (new_data) {
                        ASR::ttype_t* new_array_type = ASRUtils::TYPE(ASR::make_Array_t(al, dest_type2->base.loc, dest_type2,
                                                  array_type->m_dims, array_type->n_dims, ASR::array_physical_typeType::FixedSizeArray));
                        value = ASRUtils::EXPR(ASR::make_ArrayConstant_t(al, value->base.loc, array_size * dest_kind,
                                new_data, new_array_type, array->m_storage_format));
                    }
                }
            }
        } else if ((ASR::cast_kindType)cast_kind == ASR::cast_kindType::RealToReal) {
            if (ASRUtils::expr_value(*convert_can)) {
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Real_t>(*dest_type2));
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Real_t>(*ASRUtils::extract_type(ASRUtils::expr_type(*convert_can))))
                value = ASRUtils::expr_value(*convert_can);
                if (ASR::is_a<ASR::RealConstant_t>(*value)) {
                    ASR::RealConstant_t *r = ASR::down_cast<ASR::RealConstant_t>(value);
                    double rval = r->m_r;
                    value = (ASR::expr_t *)ASR::make_RealConstant_t(al, a_loc,
                        rval, dest_type2);
              } else {
                  LCOMPILERS_ASSERT(ASR::is_a<ASR::ArrayConstant_t>(*value));
                  ASR::ArrayConstant_t* array = ASR::down_cast<ASR::ArrayConstant_t>(value);
                  ASR::Array_t* array_type = ASR::down_cast<ASR::Array_t>(array->m_type);
                  void *data = array->m_data;
                  size_t array_size = ASRUtils::get_fixed_size_of_array(array->m_type);
                  int dest_kind = ASRUtils::extract_kind_from_ttype_t(dest_type2);
                  void *new_data = nullptr;
                  if (dest_kind == 4) {
                      float *new_array = al.allocate<float>(array_size);
                      for (size_t i = 0; i < array_size; i++) {
                        new_array[i] = ((double*) data)[i];
                      }
                      new_data = new_array;
                  } else if (dest_kind == 8) {
                      double *new_array = al.allocate<double>(array_size);
                      for (size_t i = 0; i < array_size; i++) {
                        new_array[i] = ((float*) data)[i];
                      }
                      new_data = new_array;
                  }
                  if (new_data) {
                      ASR::ttype_t* new_array_type = ASRUtils::TYPE(ASR::make_Array_t(al, dest_type2->base.loc, dest_type2,
                                                array_type->m_dims, array_type->n_dims, ASR::array_physical_typeType::FixedSizeArray));
                      value = ASRUtils::EXPR(ASR::make_ArrayConstant_t(al, value->base.loc, array_size * dest_kind,
                              new_data, new_array_type, array->m_storage_format));
                  }
              }
            }
        } else if ((ASR::cast_kindType)cast_kind == ASR::cast_kindType::RealToComplex) {
            if (ASRUtils::expr_value(*convert_can)) {
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Complex_t>(*ASRUtils::type_get_past_pointer(dest_type2)))
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Real_t>(*ASRUtils::extract_type(ASRUtils::expr_type(*convert_can))))
                value = ASRUtils::expr_value(*convert_can);
                if( ASR::is_a<ASR::RealConstant_t>(*value) ) {
                    ASR::RealConstant_t *r = ASR::down_cast<ASR::RealConstant_t>(value);
                    double rval = r->m_r;
                    value = (ASR::expr_t *)ASR::make_ComplexConstant_t(al, a_loc,
                      rval, 0, dest_type2);
                } else {
                    LCOMPILERS_ASSERT(ASR::is_a<ASR::ArrayConstant_t>(*value));
                    ASR::ArrayConstant_t* array = ASR::down_cast<ASR::ArrayConstant_t>(value);
                    ASR::Array_t* array_type = ASR::down_cast<ASR::Array_t>(array->m_type);
                    int kind = ASRUtils::extract_kind_from_ttype_t(array->m_type);
                    void *data = array->m_data;
                    size_t array_size = ASRUtils::get_fixed_size_of_array(array->m_type);
                    int dest_kind = ASRUtils::extract_kind_from_ttype_t(dest_type2);
                    void *new_data = nullptr;
                    if (dest_kind == 8) {
                        std::complex<double> *new_array = al.allocate<std::complex<double>>(array_size);
                        for (size_t i = 0; i < array_size; i++) {
                            if (kind == 4) {
                                new_array[i] = std::complex<double>(((float*)data)[i], 0);
                            } else if (kind == 8) {
                                new_array[i] = std::complex<double>(((double*)data)[i], 0);
                            }
                        }
                        new_data = new_array;
                    } else if (dest_kind == 4) {
                        std::complex<float> *new_array = al.allocate<std::complex<float>>(array_size);
                        for (size_t i = 0; i < array_size; i++) {
                            if (kind == 4) {
                                new_array[i] = std::complex<float>(((float*)data)[i], 0);
                            } else if (kind == 8) {
                                new_array[i] = std::complex<float>(((double*)data)[i], 0);
                            }
                        }
                        new_data = new_array;
                    }
                    if (new_data) {
                        ASR::ttype_t* new_array_type = ASRUtils::TYPE(ASR::make_Array_t(al, dest_type2->base.loc, dest_type2,
                                                  array_type->m_dims, array_type->n_dims, ASR::array_physical_typeType::FixedSizeArray));
                        value = ASRUtils::EXPR(ASR::make_ArrayConstant_t(al, value->base.loc, array_size * dest_kind,
                                new_data, new_array_type, array->m_storage_format));
                    }
                }
            }
        } else if ((ASR::cast_kindType)cast_kind == ASR::cast_kindType::ComplexToReal) {
            if (ASRUtils::expr_value(*convert_can)) {
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Real_t>(*ASRUtils::type_get_past_pointer(dest_type2)))
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Complex_t>(*ASRUtils::extract_type(ASRUtils::expr_type(*convert_can))))
                value = ASRUtils::expr_value(*convert_can);
                if (ASR::is_a<ASR::ComplexConstant_t>(*value)) {
                  ASR::ComplexConstant_t *r = ASR::down_cast<ASR::ComplexConstant_t>(value);
                  double rval = r->m_re;
                  value = (ASR::expr_t *)ASR::make_RealConstant_t(al, a_loc,
                      rval, dest_type2);
                } else {
                  value = nullptr;
                }
            }
        } else if ((ASR::cast_kindType)cast_kind == ASR::cast_kindType::ComplexToInteger) {
            if (ASRUtils::expr_value(*convert_can)) {
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_pointer(dest_type2)))
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Complex_t>(*ASRUtils::extract_type(ASRUtils::expr_type(*convert_can))))
                value = ASRUtils::expr_value(*convert_can);
                if (ASR::is_a<ASR::ComplexConstant_t>(*value)) {
                  ASR::ComplexConstant_t *r = ASR::down_cast<ASR::ComplexConstant_t>(value);
                  double rval = r->m_re;
                  int64_t ival = (int64_t)rval;
                  value = (ASR::expr_t *)ASR::make_IntegerConstant_t(al, a_loc,
                      ival, dest_type2);
                } else {
                  value = nullptr;
                }
            }
        } else if ((ASR::cast_kindType)cast_kind == ASR::cast_kindType::IntegerToInteger) {
            if (ASRUtils::expr_value(*convert_can)) {
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Integer_t>(*dest_type2))
                LCOMPILERS_ASSERT(ASRUtils::is_integer(*ASRUtils::extract_type(ASRUtils::expr_type(*convert_can))))
                value = ASRUtils::expr_value(*convert_can);
                if( ASR::is_a<ASR::IntegerConstant_t>(*value) ) {
                    ASR::IntegerConstant_t *i = ASR::down_cast<ASR::IntegerConstant_t>(value);
                    int64_t ival = i->m_n;
                    value = (ASR::expr_t *)ASR::make_IntegerConstant_t(al, a_loc,
                        ival, dest_type2);
                } else {
                    LCOMPILERS_ASSERT(ASR::is_a<ASR::ArrayConstant_t>(*value));
                    ASR::ArrayConstant_t* array = ASR::down_cast<ASR::ArrayConstant_t>(value);
                    ASR::Array_t* array_type = ASR::down_cast<ASR::Array_t>(array->m_type);
                    int kind = ASRUtils::extract_kind_from_ttype_t(array->m_type);
                    void *data = array->m_data;
                    size_t array_size = ASRUtils::get_fixed_size_of_array(array->m_type);
                    int dest_kind = ASRUtils::extract_kind_from_ttype_t(dest_type2);
                    void *new_data = nullptr;
                    if (dest_kind == 4) {
                        int *new_array = al.allocate<int>(array_size);
                        for (size_t i = 0; i < array_size; i++) {
                          if ( kind == 8 ) {
                            new_array[i] = ((int64_t*) data)[i];
                          } else if ( kind == 2 ) {
                            new_array[i] = ((int16_t*) data)[i];
                          } else if ( kind == 1 ) {
                            new_array[i] = ((int8_t*) data)[i];
                          }
                        }
                        new_data = new_array;
                    } else if (dest_kind == 8) {
                        int64_t *new_array = al.allocate<int64_t>(array_size);
                        for (size_t i = 0; i < array_size; i++) {
                          if ( kind == 4) {
                            new_array[i] = ((int*) data)[i];
                          } else if ( kind == 2 ) {
                            new_array[i] = ((int16_t*) data)[i];
                          } else if ( kind == 1 ) {
                            new_array[i] = ((int8_t*) data)[i];
                          }
                        }
                        new_data = new_array;
                    } else if (dest_kind == 2) {
                        int16_t *new_array = al.allocate<int16_t>(array_size);
                        for (size_t i = 0; i < array_size; i++) {
                          if ( kind == 4) {
                            new_array[i] = ((int*) data)[i];
                          } else if ( kind == 8 ) {
                            new_array[i] = ((int64_t*) data)[i];
                          } else if ( kind == 1 ) {
                            new_array[i] = ((int8_t*) data)[i];
                          }
                        }
                        new_data = new_array;
                    } else if (dest_kind == 1) {
                        int8_t *new_array = al.allocate<int8_t>(array_size);
                        for (size_t i = 0; i < array_size; i++) {
                          if ( kind == 4) {
                            new_array[i] = ((int*) data)[i];
                          } else if ( kind == 8 ) {
                            new_array[i] = ((int64_t*) data)[i];
                          } else if ( kind == 2) {
                            new_array[i] = ((int16_t*) data)[i];
                          }
                        }
                        new_data = new_array;
                    }
                    if (new_data) {
                        ASR::ttype_t* new_array_type = ASRUtils::TYPE(ASR::make_Array_t(al, dest_type2->base.loc, dest_type2,
                                                  array_type->m_dims, array_type->n_dims, ASR::array_physical_typeType::FixedSizeArray));
                        value = ASRUtils::EXPR(ASR::make_ArrayConstant_t(al, value->base.loc, array_size * dest_kind,
                                new_data, new_array_type, array->m_storage_format));
                    }
                }
            }
        } else if ((ASR::cast_kindType)cast_kind == ASR::cast_kindType::IntegerToComplex) {
            if (ASRUtils::expr_value(*convert_can)) {
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Complex_t>(*dest_type2))
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Integer_t>(*ASRUtils::extract_type(ASRUtils::expr_type(*convert_can))))
                value = ASRUtils::expr_value(*convert_can);
                if( ASR::is_a<ASR::IntegerConstant_t>(*value) ) {
                    ASR::IntegerConstant_t *i = ASR::down_cast<ASR::IntegerConstant_t>(value);
                    int64_t ival = i->m_n;
                    value = (ASR::expr_t *)ASR::make_ComplexConstant_t(al, a_loc,
                      ival, 0, dest_type2);
                } else {
                    LCOMPILERS_ASSERT(ASR::is_a<ASR::ArrayConstant_t>(*value));
                    ASR::ArrayConstant_t* array = ASR::down_cast<ASR::ArrayConstant_t>(value);
                    ASR::Array_t* array_type = ASR::down_cast<ASR::Array_t>(array->m_type);
                    int kind = ASRUtils::extract_kind_from_ttype_t(array->m_type);
                    void *data = array->m_data;
                    size_t array_size = ASRUtils::get_fixed_size_of_array(array->m_type);
                    int dest_kind = ASRUtils::extract_kind_from_ttype_t(dest_type2);
                    void *new_data = nullptr;
                    if (dest_kind == 8) {
                        std::complex<double> *new_array = al.allocate<std::complex<double>>(array_size);
                        for (size_t i = 0; i < array_size; i++) {
                            if (kind == 4) {
                                new_array[i] = std::complex<double>(((int*)data)[i], 0);
                            } else if (kind == 8) {
                                new_array[i] = std::complex<double>(((int64_t*)data)[i], 0);
                            } else if (kind == 2) {
                                new_array[i] = std::complex<double>(((int16_t*)data)[i], 0);
                            } else {
                                new_array[i] = std::complex<double>(((int8_t*)data)[i], 0);
                            }
                        }
                        new_data = new_array;
                    } else if (dest_kind == 4) {
                        std::complex<float> *new_array = al.allocate<std::complex<float>>(array_size);
                        for (size_t i = 0; i < array_size; i++) {
                            if (kind == 4) {
                                new_array[i] = std::complex<float>(((int*)data)[i], 0);
                            } else if (kind == 8) {
                                new_array[i] = std::complex<float>(((int64_t*)data)[i], 0);
                            } else if (kind == 2) {
                                new_array[i] = std::complex<float>(((int16_t*)data)[i], 0);
                            } else {
                                new_array[i] = std::complex<float>(((int8_t*)data)[i], 0);
                            }
                        }
                        new_data = new_array;
                    }
                    if (new_data) {
                        ASR::ttype_t* new_array_type = ASRUtils::TYPE(ASR::make_Array_t(al, dest_type2->base.loc, dest_type2,
                                                  array_type->m_dims, array_type->n_dims, ASR::array_physical_typeType::FixedSizeArray));
                        value = ASRUtils::EXPR(ASR::make_ArrayConstant_t(al, value->base.loc, array_size * dest_kind,
                                new_data, new_array_type, array->m_storage_format));
                    }
                }
            }
        } else if ((ASR::cast_kindType)cast_kind == ASR::cast_kindType::ComplexToComplex) {
            if (ASRUtils::expr_value(*convert_can)) {
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Complex_t>(*dest_type2))
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Complex_t>(*ASRUtils::extract_type(ASRUtils::expr_type(*convert_can))))
                value = ASRUtils::expr_value(*convert_can);
                if( ASR::is_a<ASR::ComplexConstant_t>(*value) ) {
                    ASR::ComplexConstant_t *c = ASR::down_cast<ASR::ComplexConstant_t>(value);
                    double re = c->m_re;
                    double im = c->m_im;
                    value = (ASR::expr_t *)ASR::make_ComplexConstant_t(al, a_loc,
                      re, im, dest_type2);
                } else {
                    LCOMPILERS_ASSERT(ASR::is_a<ASR::ArrayConstant_t>(*value));
                    ASR::ArrayConstant_t* array = ASR::down_cast<ASR::ArrayConstant_t>(value);
                    ASR::Array_t* array_type = ASR::down_cast<ASR::Array_t>(array->m_type);
                    void *data = array->m_data;
                    size_t array_size = ASRUtils::get_fixed_size_of_array(array->m_type);
                    int dest_kind = ASRUtils::extract_kind_from_ttype_t(dest_type2);
                    void *new_data = nullptr;

                    if (dest_kind == 8) {
                        std::complex<double> *new_array = al.allocate<std::complex<double>>(array_size);
                        for (size_t i = 0; i < array_size; i++) {
                            new_array[i] = std::complex<double>(((std::complex<float>*)data)[i]);
                        }
                        new_data = new_array;
                    } else if (dest_kind == 4) {
                        std::complex<float> *new_array = al.allocate<std::complex<float>>(array_size);
                        for (size_t i = 0; i < array_size; i++) {
                            new_array[i] = std::complex<float>(((std::complex<double>*)data)[i]);
                        }
                        new_data = new_array;
                    }

                    if (new_data) {
                        ASR::ttype_t* new_array_type = ASRUtils::TYPE(ASR::make_Array_t(al, dest_type2->base.loc, dest_type2,
                                                          array_type->m_dims, array_type->n_dims, ASR::array_physical_typeType::FixedSizeArray));
                        value = ASRUtils::EXPR(ASR::make_ArrayConstant_t(al, value->base.loc, array_size * dest_kind,
                                                new_data, new_array_type, array->m_storage_format));
                    }
                }
            }
        }

      if( !ASRUtils::is_array(source_type) ) {
          dest_type = dest_type2;
      }
      if( ASRUtils::is_array(source_type) && !ASRUtils::is_array(dest_type) ) {
          ASR::dimension_t* m_dims = nullptr;
          size_t n_dims = ASRUtils::extract_dimensions_from_ttype(source_type, m_dims);
          dest_type = ASRUtils::make_Array_t_util(
            al, source_type->base.loc, dest_type, m_dims, n_dims);
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

    ASR::ttype_t *left_type2 = ASRUtils::type_get_past_array(
      ASRUtils::type_get_past_pointer(left_type));
    ASR::ttype_t *right_type2 = ASRUtils::type_get_past_array(
      ASRUtils::type_get_past_pointer(right_type));
    LCOMPILERS_ASSERT(left_type2->type < num_types);
    LCOMPILERS_ASSERT(right_type2->type < num_types);
    int left_type_p = type_priority[left_type2->type];
    int right_type_p = type_priority[right_type2->type];
    int left_kind = 0, right_kind = 1;
    left_kind = ASRUtils::extract_kind_from_ttype_t(left_type2);
    right_kind = ASRUtils::extract_kind_from_ttype_t(right_type2);

    if (left_type_p > right_type_p) {
      conversion_cand = right;
      *source_type = right_type;
      *dest_type = left_type;
    } else if (left_type_p == right_type_p) {
      // both are of same priority, then the one with lower kind is chosen to be cast to the higher kind
      if (left_kind >= right_kind) {
        conversion_cand = right;
        *source_type = right_type;
        *dest_type = left_type;
      } else if (left_kind < right_kind) {
        conversion_cand = left;
        *source_type = left_type;
        *dest_type = right_type;
      }
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
