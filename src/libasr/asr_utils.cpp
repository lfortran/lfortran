#include "libasr/asr.h"
#include <algorithm>
#include <limits>
#include <set>
#include <unordered_set>
#include <map>
#include <libasr/asr_utils.h>
#include <libasr/string_utils.h>
#include <libasr/serialization.h>
#include <libasr/assert.h>
#include <libasr/asr_verify.h>
#include <libasr/utils.h>
#include <libasr/modfile.h>
#include <libasr/runtime/lfortran_float128_quadmath.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/intrinsic_subroutine_registry.h>
#include <libasr/pass/intrinsic_array_function_registry.h>

#include <libasr/asr_builder.h>

namespace LCompilers {

    namespace ASRUtils  {


/**
 * @param string_expr string to be casted
 * @param array_type type of array to cast to
 * @return ASR Cast node with cast_kind = `StringToArray`
*/
inline ASR::Cast_t* cast_string_to_array(Allocator &al, ASR::expr_t* const string_expr, ASR::ttype_t* const array_type){
    LCOMPILERS_ASSERT(is_string_only(expr_type(string_expr)))
    LCOMPILERS_ASSERT(is_array_of_strings(array_type))

    ASR::ttype_t*  const array_type_dup = ASRUtils::ExprStmtDuplicator(al).duplicate_ttype(array_type);
    ASR::Array_t*  const array_t = ASR::down_cast<ASR::Array_t>(type_get_past_allocatable_pointer(array_type_dup));
    ASR::String_t* const dest_string_t = get_string_type(array_type_dup);
    if (array_t->n_dims != 1) {
        ASRBuilder b(al, string_expr->base.loc);
        ASR::expr_t* known_size = b.i32(1);
        for (size_t i = 0; i < array_t->n_dims; i++) {
            if (array_t->m_dims[i].m_start == nullptr) {
                array_t->m_dims[i].m_start = b.i32(1);
            }
            if (array_t->m_dims[i].m_length != nullptr) {
                known_size = b.Mul(known_size, array_t->m_dims[i].m_length);
            }
        }
        ASR::expr_t* string_len = b.StringLen(string_expr);
        ASR::expr_t* elem_count = b.Div(string_len, dest_string_t->m_len);
        for (size_t i = 0; i < array_t->n_dims; i++) {
            if (array_t->m_dims[i].m_length == nullptr) {
                array_t->m_dims[i].m_length = b.Div(elem_count, known_size);
                known_size = b.Mul(known_size, array_t->m_dims[i].m_length);
            }
        }
       return ASR::down_cast2<ASR::Cast_t>(
                ASR::make_Cast_t(al, string_expr->base.loc, string_expr
                            , ASR::StringToArray, array_type_dup, nullptr, nullptr));
    }
    const bool is_length_present = dest_string_t->m_len_kind == ASR::ExpressionLength;
    const bool is_size_present = !is_dimension_empty(array_type_dup); 

    // Notice We're transforming a string into array;
    // Resulting array should have proper length + size. 
    if(is_length_present && is_size_present){ // character()
       return ASR::down_cast2<ASR::Cast_t>(
                ASR::make_Cast_t(al, string_expr->base.loc, string_expr
                            , ASR::StringToArray, array_type_dup, nullptr, nullptr));
    } else if(is_length_present && !is_size_present){
        ASRBuilder b(al, string_expr->base.loc);
        ASR::expr_t* const castedString_len = extract_kind_from_ttype_t(expr_type(dest_string_t->m_len)) != 4? 
                                                b.i2i_t(b.StringLen(string_expr), expr_type(dest_string_t->m_len)) 
                                              : b.StringLen(string_expr);
        ASR::expr_t* const arr_size = b.Div(castedString_len, dest_string_t->m_len);
        array_t->m_dims[0].m_start  = b.i32(1);
        array_t->m_dims[0].m_length = arr_size;
       return ASR::down_cast2<ASR::Cast_t>(
                ASR::make_Cast_t(al, string_expr->base.loc, string_expr
                , ASR::StringToArray, array_type_dup, nullptr, nullptr));
    } else if(!is_length_present && is_size_present){
        ASRBuilder b(al, string_expr->base.loc);
        dest_string_t->m_len = b.StringLen(string_expr);
        dest_string_t->m_len_kind = ASR::ExpressionLength;
       return ASR::down_cast2<ASR::Cast_t>(
                ASR::make_Cast_t(al, string_expr->base.loc, string_expr
                , ASR::StringToArray, array_type_dup, nullptr, nullptr));
    } else {
        ASRBuilder b(al, string_expr->base.loc);
        dest_string_t->m_len = b.StringLen(string_expr);
        dest_string_t->m_len_kind = ASR::ExpressionLength;
        array_t->m_dims[0].m_start  = b.i32(1);
        array_t->m_dims[0].m_length = b.i32(1);
       return ASR::down_cast2<ASR::Cast_t>(
                ASR::make_Cast_t(al, string_expr->base.loc, string_expr
                , ASR::StringToArray, array_type_dup, nullptr, nullptr));
    }

}

// depth-first graph traversal
void visit(
    std::string const& a,
    std::map<std::string, std::vector<std::string>> const& deps,
    std::unordered_set<std::string>& visited,
    std::vector<std::string>& result
) {
    visited.insert(a);
    auto it = deps.find(a);
    if (it != deps.end()) {
        for (auto n : it->second) {
            if (!visited.count(n)) visit(n, deps, visited, result);
        }
    }
    result.push_back(a);
}

std::vector<std::string> order_deps(std::map<std::string, std::vector<std::string>> const& deps) {
    // Compute ordering: depth-first graph traversal, inserting nodes on way back

    // set containing the visited nodes
    std::unordered_set<std::string> visited;

    // vector containing result
    std::vector<std::string> result;

    for (auto d : deps) {
        if (!visited.count(d.first)) {
            visit(d.first, deps, visited, result);
        }
    }
    return result;
}

std::vector<std::string> determine_module_dependencies(
        const ASR::TranslationUnit_t &unit)
{
    std::map<std::string, std::vector<std::string>> deps;
    for (auto &item : unit.m_symtab->get_scope()) {
        if (ASR::is_a<ASR::Module_t>(*item.second)) {
            std::string name = item.first;
            ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(item.second);
            deps[name] = std::vector<std::string>();
            for (size_t i=0; i < m->n_dependencies; i++) {
                std::string dep = m->m_dependencies[i];
                deps[name].push_back(dep);
            }
        }
    }
    return order_deps(deps);
}

std::vector<std::string> determine_function_definition_order(
        SymbolTable* symtab) {
    std::map<std::string, std::vector<std::string>> func_dep_graph;
    ASR::symbol_t *sym;
    for( auto itr: symtab->get_scope() ) {
        if( ASR::is_a<ASR::Function_t>(*itr.second) ) {
            std::vector<std::string> deps;
            ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(itr.second);
            for( size_t i = 0; i < func->n_dependencies; i++ ) {
                std::string dep = func->m_dependencies[i];
                // Check if the dependent variable is present in the symtab.
                // This will help us to include only local dependencies, and we
                // assume that dependencies in the parent symtab are already declared
                // earlier.
                sym = symtab->get_symbol(dep);
                if (sym != nullptr && ASR::is_a<ASR::Function_t>(*sym))
                    deps.push_back(dep);
            }
            func_dep_graph[itr.first] = deps;
        }
    }
    return ASRUtils::order_deps(func_dep_graph);
}

std::vector<std::string> determine_class_procedure_declaration_order(
        SymbolTable* symtab) {
    std::map<std::string, std::vector<std::string>> func_dep_graph;
    ASR::symbol_t *sym;
    for( auto itr: symtab->get_scope() ) {
        if( ASR::is_a<ASR::StructMethodDeclaration_t>(*itr.second) ) {
            std::vector<std::string> deps;
            ASR::symbol_t* cp = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::StructMethodDeclaration_t>(itr.second)->m_proc);
            if (ASR::is_a<ASR::Function_t>(*cp)) {
                ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(cp);
                for( size_t i = 0; i < func->n_dependencies; i++ ) {
                    std::string dep = func->m_dependencies[i];
                    // Check if the dependent variable is present in the symtab.
                    // This will help us to include only local dependencies, and we
                    // assume that dependencies in the parent symtab are already declared
                    // earlier.
                    sym = symtab->get_symbol(dep);
                    if (sym != nullptr && ASR::is_a<ASR::Function_t>(*sym))
                        deps.push_back(dep);
                }
                func_dep_graph[itr.first] = deps;
            }
        }
    }
    return ASRUtils::order_deps(func_dep_graph);
}

ASR::symbol_t* get_struct_sym_from_struct_expr(ASR::expr_t* expression)
{
    // The idea behind this utility function is that every struct expression
    // must eventually resolve to either `Var` or `StructInstanceMember`.
    switch (expression->type) {
        case ASR::exprType::Var: {
            // The symbol m_v has to be `Variable` or 'Function' for a Struct expression.
            if (ASR::is_a<ASR::Variable_t>(*ASRUtils::symbol_get_past_external(ASR::down_cast<ASR::Var_t>(expression)->m_v))) {
                ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(ASRUtils::symbol_get_past_external(ASR::down_cast<ASR::Var_t>(expression)->m_v));
                if (var->m_type_declaration == nullptr) {
                    return nullptr;
                }
                return ASRUtils::symbol_get_past_external(var->m_type_declaration);
            } else if (ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(ASR::down_cast<ASR::Var_t>(expression)->m_v))) {
                ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(ASRUtils::symbol_get_past_external(ASR::down_cast<ASR::Var_t>(expression)->m_v));
                if (func->m_return_var != nullptr && ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(func->m_return_var))) {
                   return ASRUtils::get_struct_sym_from_struct_expr(func->m_return_var);
                } else {
                    for (size_t i = 0; i < func->n_args; i++) {
                        ASR::expr_t* arg = func->m_args[i];
                        if (arg != nullptr) {
                            ASR::symbol_t* struct_sym = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(arg));
                            if (struct_sym != nullptr) {
                                return struct_sym;
                            }
                        }
                    }
                    return nullptr;
                }
            } else if (ASR::is_a<ASR::Struct_t>(*ASRUtils::symbol_get_past_external(ASR::down_cast<ASR::Var_t>(expression)->m_v))) {
                // If the Var is a Struct, we return the symbol of the Struct.
                return ASR::down_cast<ASR::Var_t>(expression)->m_v;
            } else {
                throw LCompilersException("Expected Var to be either Variable or Function type, but found: " +
                                    std::to_string(ASR::down_cast<ASR::Var_t>(expression)->m_v->type));
            }
        }
        case ASR::exprType::StructInstanceMember: {
            ASR::StructInstanceMember_t* struct_instance_member = ASR::down_cast<ASR::StructInstanceMember_t>(expression);
            if (ASR::is_a<ASR::Struct_t>(*ASRUtils::symbol_get_past_external(struct_instance_member->m_m))) {
                // Special case: Can have `StructInstanceMember` like `var%member` where `member` is
                // parent struct of the struct used to declare `var`.
                // Please see assignment `c%parent_t = p` in
                // `integration_tests/derived_types_73.f90` for an example.
                return struct_instance_member->m_m;
            } else {
                LCOMPILERS_ASSERT(ASR::is_a<ASR::Variable_t>(*ASRUtils::symbol_get_past_external(struct_instance_member->m_m)));
                ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(ASRUtils::symbol_get_past_external(struct_instance_member->m_m));
                return var->m_type_declaration;
            }
        }
        case ASR::exprType::ArrayConstructor: {
            ASR::ArrayConstructor_t* array_constructor = ASR::down_cast<ASR::ArrayConstructor_t>(expression);
            // First check if type_declaration is available
            if (array_constructor->m_struct_var != nullptr) {
                return get_struct_sym_from_struct_expr(array_constructor->m_struct_var);
            }
            for (size_t i = 0; i < array_constructor->n_args; i++) {
                ASR::expr_t* arg = array_constructor->m_args[i];
                if (arg != nullptr) {
                    ASR::symbol_t* struct_sym = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(arg));
                    if (struct_sym != nullptr) {
                        return struct_sym;
                    }
                }
            }
            if (array_constructor->m_value != nullptr) {
                // If `m_value` is not null, it means that the array constructor
                // is returning a struct type.
                return ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(array_constructor->m_value));
            }
            return nullptr; // If no struct symbol found in arguments or value
        }
        case ASR::exprType::ArrayConstant: {
            ASR::ArrayConstant_t* array_constant =
                ASR::down_cast<ASR::ArrayConstant_t>(expression);
            if (!ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::type_get_past_array(array_constant->m_type))) {
                return nullptr;
            }
            if (ASRUtils::get_fixed_size_of_array(array_constant->m_type) < 1) {
                return nullptr;
            }
            ASR::expr_t* first = ((ASR::expr_t**)array_constant->m_data)[0];
            if (first == nullptr) {
                return nullptr;
            }
            return ASRUtils::get_struct_sym_from_struct_expr(first);
        }
        case ASR::exprType::ArrayItem: {
            ASR::ArrayItem_t* array_item = ASR::down_cast<ASR::ArrayItem_t>(expression);
            return ASRUtils::get_struct_sym_from_struct_expr(array_item->m_v);
        }
        case ASR::exprType::ArraySection: {
            ASR::ArraySection_t* array_section = ASR::down_cast<ASR::ArraySection_t>(expression);
            return ASRUtils::get_struct_sym_from_struct_expr(array_section->m_v);
        }
        case ASR::exprType::FunctionCall: {
            ASR::FunctionCall_t* func_call = ASR::down_cast<ASR::FunctionCall_t>(expression);
            ASR::Function_t* func = get_function(func_call->m_name);
            if (func == nullptr || func->m_return_var == nullptr) {
                return nullptr;
            }
            return ASRUtils::get_struct_sym_from_struct_expr(func->m_return_var);
        }
        case ASR::exprType::StructConstant: {
            ASR::StructConstant_t* struct_constant = ASR::down_cast<ASR::StructConstant_t>(expression);
            return struct_constant->m_dt_sym;
        }
        case ASR::exprType::GetPointer: {
            ASR::GetPointer_t* get_pointer = ASR::down_cast<ASR::GetPointer_t>(expression);
            return ASRUtils::get_struct_sym_from_struct_expr(get_pointer->m_arg);
        }
        case ASR::exprType::ArrayPhysicalCast: {
            ASR::ArrayPhysicalCast_t* array_physical_cast = ASR::down_cast<ASR::ArrayPhysicalCast_t>(expression);
            // `array_physical_cast->m_arg` will be non-null for Struct expressions
            LCOMPILERS_ASSERT(array_physical_cast->m_arg != nullptr);
            return ASRUtils::get_struct_sym_from_struct_expr(array_physical_cast->m_arg);
        }
        case ASR::exprType::IntegerCompare: {
            ASR::IntegerCompare_t* int_compare = ASR::down_cast<ASR::IntegerCompare_t>(expression);
            ASR::symbol_t* left_struct_sym = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(int_compare->m_left));
            ASR::symbol_t* right_struct_sym = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(int_compare->m_right));
            if (left_struct_sym != nullptr) {
                return left_struct_sym;
            } else if (right_struct_sym != nullptr) {
                return right_struct_sym;
            } else {
                return nullptr; // If no struct symbol found in either side
            }
        }
        case ASR::exprType::RealCompare: {
            ASR::RealCompare_t* real_compare = ASR::down_cast<ASR::RealCompare_t>(expression);
            ASR::symbol_t* left_struct_sym = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(real_compare->m_left));
            ASR::symbol_t* right_struct_sym = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(real_compare->m_right));
            if (left_struct_sym != nullptr) {
                return left_struct_sym;
            } else if (right_struct_sym != nullptr) {
                return right_struct_sym;
            } else {
                return nullptr; // If no struct symbol found in either side
            }
        }
        case ASR::exprType::StringCompare: {
            ASR::StringCompare_t* string_compare = ASR::down_cast<ASR::StringCompare_t>(expression);
            ASR::symbol_t* left_struct_sym = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(string_compare->m_left));
            ASR::symbol_t* right_struct_sym = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(string_compare->m_right));
            if (left_struct_sym != nullptr) {
                return left_struct_sym;
            } else if (right_struct_sym != nullptr) {
                return right_struct_sym;
            } else {
                return nullptr; // If no struct symbol found in either side
            }
        }
        case ASR::exprType::ComplexCompare: {
            ASR::ComplexCompare_t* complex_compare = ASR::down_cast<ASR::ComplexCompare_t>(expression);
            ASR::symbol_t* left_struct_sym = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(complex_compare->m_left));
            ASR::symbol_t* right_struct_sym = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(complex_compare->m_right));
            if (left_struct_sym != nullptr) {
                return left_struct_sym;
            } else if (right_struct_sym != nullptr) {
                return right_struct_sym;
            } else {
                return nullptr; // If no struct symbol found in either side
            }
        }
        case ASR::exprType::IntrinsicArrayFunction: {
            ASR::IntrinsicArrayFunction_t* intrinsic_array_function = ASR::down_cast<ASR::IntrinsicArrayFunction_t>(expression);
            for (size_t i = 0; i < intrinsic_array_function->n_args; i++) {
                ASR::expr_t* arg = intrinsic_array_function->m_args[i];
                if (arg != nullptr) {
                    ASR::symbol_t* struct_sym = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(arg));
                    if (struct_sym != nullptr) {
                        return struct_sym;
                    }
                }
            }
            // If no struct symbol found in arguments, return nullptr
            return nullptr;
        }
        case ASR::exprType::StringPhysicalCast: {
            ASR::StringPhysicalCast_t* string_physical_cast = ASR::down_cast<ASR::StringPhysicalCast_t>(expression);
            return ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(string_physical_cast->m_arg));
        }
        case ASR::exprType::IntrinsicImpureFunction: {
            ASR::IntrinsicImpureFunction_t* intrinsic_impure_function = ASR::down_cast<ASR::IntrinsicImpureFunction_t>(expression);
            for (size_t i = 0; i < intrinsic_impure_function->n_args; i++) {
                ASR::expr_t* arg = intrinsic_impure_function->m_args[i];
                if (arg != nullptr) {
                    ASR::symbol_t* struct_sym = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(arg));
                    if (struct_sym != nullptr) {
                        return struct_sym;
                    }
                }
            }
            // If no struct symbol found in arguments, return nullptr
            return nullptr;
        }
        case ASR::exprType::IntrinsicElementalFunction: {
            ASR::IntrinsicElementalFunction_t* intrinsic_elemental_function = ASR::down_cast<ASR::IntrinsicElementalFunction_t>(expression);
            for (size_t i = 0; i < intrinsic_elemental_function->n_args; i++) {
                ASR::expr_t* arg = intrinsic_elemental_function->m_args[i];
                if (arg != nullptr) {
                    ASR::symbol_t* struct_sym = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(arg));
                    if (struct_sym != nullptr) {
                        return struct_sym;
                    }
                }
            }
            // If no struct symbol found in arguments, return nullptr
            return nullptr;
        }
        case ASR::exprType::TypeInquiry: {
            ASR::TypeInquiry_t* type_inquiry = ASR::down_cast<ASR::TypeInquiry_t>(expression);
            return ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(type_inquiry->m_arg));
        }
        case ASR::exprType::RealUnaryMinus: {
            ASR::RealUnaryMinus_t* real_unary_minus = ASR::down_cast<ASR::RealUnaryMinus_t>(expression);
            return ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(real_unary_minus->m_arg));
        }
        case ASR::exprType::IntegerUnaryMinus: {
            ASR::IntegerUnaryMinus_t* int_unary_minus = ASR::down_cast<ASR::IntegerUnaryMinus_t>(expression);
            return ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(int_unary_minus->m_arg));
        }
        case ASR::exprType::RealBinOp: {
            ASR::RealBinOp_t* real_bin_op = ASR::down_cast<ASR::RealBinOp_t>(expression);
            ASR::symbol_t* left_struct_sym = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(real_bin_op->m_left));
            ASR::symbol_t* right_struct_sym = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(real_bin_op->m_right));
            if (left_struct_sym != nullptr) {
                return left_struct_sym;
            } else if (right_struct_sym != nullptr) {
                return right_struct_sym;
            } else {
                return nullptr; // If no struct symbol found in either side
            }
        }
        case ASR::exprType::IntegerBinOp: {
            ASR::IntegerBinOp_t* int_bin_op = ASR::down_cast<ASR::IntegerBinOp_t>(expression);
            ASR::symbol_t* left_struct_sym = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(int_bin_op->m_left));
            ASR::symbol_t* right_struct_sym = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(int_bin_op->m_right));
            if (left_struct_sym != nullptr) {
                return left_struct_sym;
            } else if (right_struct_sym != nullptr) {
                return right_struct_sym;
            } else {
                return nullptr; // If no struct symbol found in either side
            }
        }
        case ASR::exprType::LogicalBinOp: {
            ASR::LogicalBinOp_t* logical_bin_op = ASR::down_cast<ASR::LogicalBinOp_t>(expression);
            ASR::symbol_t* left_struct_sym = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(logical_bin_op->m_left));
            ASR::symbol_t* right_struct_sym = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(logical_bin_op->m_right));
            if (left_struct_sym != nullptr) {
                return left_struct_sym;
            } else if (right_struct_sym != nullptr) {
                return right_struct_sym;
            } else {
                return nullptr; // If no struct symbol found in either side
            }
        }
        case ASR::exprType::ComplexBinOp: {
            ASR::ComplexBinOp_t* complex_bin_op = ASR::down_cast<ASR::ComplexBinOp_t>(expression);
            ASR::symbol_t* left_struct_sym = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(complex_bin_op->m_left));
            ASR::symbol_t* right_struct_sym = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(complex_bin_op->m_right));
            if (left_struct_sym != nullptr) {
                return left_struct_sym;
            } else if (right_struct_sym != nullptr) {
                return right_struct_sym;
            } else {
                return nullptr; // If no struct symbol found in either side
            }
        }
        case ASR::exprType::StringConcat: {
            ASR::StringConcat_t* string_concat = ASR::down_cast<ASR::StringConcat_t>(expression);
            ASR::symbol_t* left_struct_sym = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(string_concat->m_left));
            ASR::symbol_t* right_struct_sym = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(string_concat->m_right));
            if (left_struct_sym != nullptr) {
                return left_struct_sym;
            } else if (right_struct_sym != nullptr) {
                return right_struct_sym;
            } else {
                return nullptr; // If no struct symbol found in either side
            }
        }
        case ASR::exprType::RealConstant:
        case ASR::exprType::StringConstant:
        case ASR::exprType::IntegerConstant:
        case ASR::exprType::LogicalConstant:
        case ASR::exprType::UnsignedIntegerConstant:
        case ASR::exprType::ComplexConstant:
        {
            // These do not have a struct symbol, return nullptr
            return nullptr;
        }
        case ASR::exprType::PointerNullConstant: {
            ASR::PointerNullConstant_t* pnc =
                ASR::down_cast<ASR::PointerNullConstant_t>(expression);
            return pnc->m_var_expr != nullptr
                ? ASRUtils::get_struct_sym_from_struct_expr(pnc->m_var_expr)
                : nullptr;
        }
        case ASR::exprType::BitCast: {
            ASR::BitCast_t* bit_cast = ASR::down_cast<ASR::BitCast_t>(expression);
            return ASRUtils::symbol_get_past_external(
                ASRUtils::get_struct_sym_from_struct_expr(bit_cast->m_mold));
        }
        case ASR::exprType::ComplexConstructor: {
            ASR::ComplexConstructor_t* complex_constructor = ASR::down_cast<ASR::ComplexConstructor_t>(expression);
            ASR::symbol_t* real_struct_sym = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(complex_constructor->m_re));
            ASR::symbol_t* imag_struct_sym = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(complex_constructor->m_im));
            if (real_struct_sym != nullptr) {
                return real_struct_sym;
            } else if (imag_struct_sym != nullptr) {
                return imag_struct_sym;
            } else {
                return nullptr; // If no struct symbol found in either part
            }
        }
        case ASR::exprType::Cast: {
            ASR::Cast_t* cast = ASR::down_cast<ASR::Cast_t>(expression);
            if (cast->m_dest) {
                return ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(cast->m_dest));
            }
            return ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(cast->m_arg));
        }
        case ASR::exprType::ArrayReshape: {
            ASR::ArrayReshape_t* array_reshape = ASR::down_cast<ASR::ArrayReshape_t>(expression);
            return ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(array_reshape->m_array));
        }
        case ASR::exprType::ArraySize: {
            return nullptr;
        }
        case ASR::exprType::LogicalNot: {
            ASR::LogicalNot_t* logical_not = ASR::down_cast<ASR::LogicalNot_t>(expression);
            return ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(logical_not->m_arg));
        }
        case ASR::exprType::ImpliedDoLoop: {
            ASR::ImpliedDoLoop_t* imp_dl = ASR::down_cast<ASR::ImpliedDoLoop_t>(expression);
            for (size_t i = 0; i < imp_dl->n_values; i++) {
                ASR::expr_t* arg = imp_dl->m_values[i];
                if (arg != nullptr) {
                    ASR::symbol_t* struct_sym = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(arg));
                    if (struct_sym != nullptr) {
                        return struct_sym;
                    }
                }
            }
            if (imp_dl->m_value != nullptr) {
                return ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(imp_dl->m_value));
            }
            return nullptr;
        }
        case ASR::exprType::StringSection: {
            ASR::StringSection_t* string_section = ASR::down_cast<ASR::StringSection_t>(expression);
            return ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(string_section->m_arg));
        }
        case ASR::exprType::ArrayBroadcast: {
            ASR::ArrayBroadcast_t* array_broadcast = ASR::down_cast<ASR::ArrayBroadcast_t>(expression);
            // `array_broadcast->m_v` will be non-null for Struct expressions
            return ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(array_broadcast->m_array));
        }
        case ASR::exprType::ArrayBound: {
            return nullptr;
        }
        case ASR::exprType::ArrayRank: {
            return nullptr;
        }
        case ASR::exprType::StructConstructor: {
            ASR::StructConstructor_t* struct_constructor = ASR::down_cast<ASR::StructConstructor_t>(expression);
            return struct_constructor->m_dt_sym;
        }
        case ASR::exprType::OverloadedBinOp: {
            ASR::OverloadedBinOp_t* overloaded_bin_op = ASR::down_cast<ASR::OverloadedBinOp_t>(expression);
            return ASRUtils::get_struct_sym_from_struct_expr(overloaded_bin_op->m_overloaded);
        }
        case ASR::exprType::OverloadedUnaryMinus: {
            ASR::OverloadedUnaryMinus_t* overloaded_unary_minus = ASR::down_cast<ASR::OverloadedUnaryMinus_t>(expression);
            return ASRUtils::get_struct_sym_from_struct_expr(overloaded_unary_minus->m_overloaded);
        }
        case ASR::exprType::OverloadedCompare: {
            ASR::OverloadedCompare_t* overloaded_compare = ASR::down_cast<ASR::OverloadedCompare_t>(expression);
            return ASRUtils::get_struct_sym_from_struct_expr(overloaded_compare->m_overloaded);
        }
        case ASR::exprType::OverloadedStringConcat: {
            ASR::OverloadedStringConcat_t* overloaded_string_concat = ASR::down_cast<ASR::OverloadedStringConcat_t>(expression);
            return ASRUtils::get_struct_sym_from_struct_expr(overloaded_string_concat->m_overloaded);
        }
        case ASR::exprType::OverloadedBoolOp: {
            ASR::OverloadedBoolOp_t* overloaded_bool_op = ASR::down_cast<ASR::OverloadedBoolOp_t>(expression);
            return ASRUtils::get_struct_sym_from_struct_expr(overloaded_bool_op->m_overloaded);
        }
        case ASR::exprType::StringItem: {
            ASR::StringItem_t* string_item = ASR::down_cast<ASR::StringItem_t>(expression);
            return ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(string_item->m_arg));
        }
        case ASR::exprType::ComplexRe: {
            ASR::ComplexRe_t* complex_re = ASR::down_cast<ASR::ComplexRe_t>(expression);
            return ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(complex_re->m_arg));
        }
        case ASR::exprType::ComplexIm: {
            ASR::ComplexIm_t* complex_im = ASR::down_cast<ASR::ComplexIm_t>(expression);
            return ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(complex_im->m_arg));
        }
        case ASR::exprType::StringLen: {
            ASR::StringLen_t* string_len = ASR::down_cast<ASR::StringLen_t>(expression);
            return ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(string_len->m_arg));
        }
        case ASR::exprType::ComplexUnaryMinus: {
            ASR::ComplexUnaryMinus_t* complex_unary_minus = ASR::down_cast<ASR::ComplexUnaryMinus_t>(expression);
            return ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(complex_unary_minus->m_arg));
        }
        case ASR::exprType::Iachar: {
            ASR::Iachar_t* iachar = ASR::down_cast<ASR::Iachar_t>(expression);
            return ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(iachar->m_arg));
        }
        case ASR::exprType::ListLen:
        case ASR::exprType::ListConstant:
        case ASR::exprType::ListConcat:
        case ASR::exprType::PointerAssociated:
        case ASR::exprType::PointerToCPtr:
        case ASR::exprType::CLoc:
        case ASR::exprType::FunctionParam: {
            return nullptr;
        }
        case ASR::exprType::UnionInstanceMember: {
            ASR::UnionInstanceMember_t* union_instance_member = ASR::down_cast<ASR::UnionInstanceMember_t>(expression);
            LCOMPILERS_ASSERT(ASR::is_a<ASR::Variable_t>(*ASRUtils::symbol_get_past_external(union_instance_member->m_m)));
            ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(ASRUtils::symbol_get_past_external(union_instance_member->m_m));
            return var->m_type_declaration;
        }
        case ASR::exprType::IfExp: {
            // Both arms of a conditional expression have the same declared
            // type, so either of them names the derived type. The first arm
            // that resolves to one wins, since an arm can be, say, a null
            // pointer that carries no declaration of its own.
            ASR::IfExp_t* if_exp = ASR::down_cast<ASR::IfExp_t>(expression);
            ASR::symbol_t* struct_sym = get_struct_sym_from_struct_expr(if_exp->m_body);
            if (struct_sym == nullptr) {
                struct_sym = get_struct_sym_from_struct_expr(if_exp->m_orelse);
            }
            return struct_sym;
        }
        default: {
            throw LCompilersException("get_struct_sym_from_struct_expr() not implemented for "
                                + std::to_string(expression->type));
            return nullptr;
        }
    }
}

void set_struct_sym_to_struct_expr(ASR::expr_t* expression, ASR::symbol_t* struct_sym) {
    // The idea behind this utility function is that every struct expression
    // must eventually resolve to either `Var` or `StructInstanceMember`.
    switch (expression->type) {
        case ASR::exprType::Var: {
            ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(expression);
            LCOMPILERS_ASSERT(ASR::is_a<ASR::Variable_t>(*ASRUtils::symbol_get_past_external(var->m_v)));
            ASR::Variable_t* variable = ASR::down_cast<ASR::Variable_t>(ASRUtils::symbol_get_past_external(var->m_v));
            variable->m_type_declaration = struct_sym;
            return;
        } 
        case ASR::exprType::StructInstanceMember: {
            ASR::StructInstanceMember_t* struct_instance_member = ASR::down_cast<ASR::StructInstanceMember_t>(expression);
            // A component reached through an `ExternalSymbol` is declared in
            // another scope, typically a module, and that one declaration is
            // shared by every user of the module. Rewriting it to a symbol
            // that is only visible here would corrupt it for everyone else,
            // so leave it alone -- it already names its type in its own scope.
            if( ASR::is_a<ASR::ExternalSymbol_t>(*struct_instance_member->m_m) ) {
                return;
            }
            LCOMPILERS_ASSERT(ASR::is_a<ASR::Variable_t>(*struct_instance_member->m_m));
            ASR::Variable_t* variable = ASR::down_cast<ASR::Variable_t>(struct_instance_member->m_m);
            variable->m_type_declaration = struct_sym;
            return;
        } 
        case ASR::exprType::ArrayItem: {
            ASR::ArrayItem_t* array_item = ASR::down_cast<ASR::ArrayItem_t>(expression);
            return set_struct_sym_to_struct_expr(array_item->m_v, struct_sym);
        }
        case ASR::exprType::ArraySection: {
            ASR::ArraySection_t* array_section = ASR::down_cast<ASR::ArraySection_t>(expression);
            return set_struct_sym_to_struct_expr(array_section->m_v, struct_sym);
        }
        case ASR::exprType::FunctionCall: {
            ASR::FunctionCall_t* func_call = ASR::down_cast<ASR::FunctionCall_t>(expression);
            // `func_call->m_dt` will be non-null for Struct expressions
            LCOMPILERS_ASSERT(func_call->m_dt != nullptr);
            return set_struct_sym_to_struct_expr(func_call->m_dt, struct_sym);
        }
        case ASR::exprType::StructConstant: {
            ASR::StructConstant_t* struct_constant = ASR::down_cast<ASR::StructConstant_t>(expression);
            struct_constant->m_dt_sym = struct_sym;
            return;
        }
        default: {
            throw LCompilersException("set_struct_sym_to_struct_expr() not implemented for "
                                + std::to_string(expression->type));
        }
    }

}

ASR::cptr_kindType get_cptr_kind_from_type(ASR::ttype_t* type)
{
    if (type == nullptr) {
        return ASR::cptr_kindType::CPtrUnspecified;
    }
    ASR::ttype_t* scalar = ASRUtils::extract_type(type);
    if (!ASR::is_a<ASR::CPtr_t>(*scalar)) {
        return ASR::cptr_kindType::CPtrUnspecified;
    }
    return ASR::down_cast<ASR::CPtr_t>(scalar)->m_kind;
}

ASR::Module_t* get_loaded_iso_c_binding_module(SymbolTable* scope)
{
    SymbolTable *tu_scope = ASRUtils::get_tu_symtab(scope);
    if (tu_scope == nullptr) {
        return nullptr;
    }
    ASR::symbol_t *mod = tu_scope->resolve_symbol("iso_c_binding");
    if (mod == nullptr || !ASR::is_a<ASR::Module_t>(*mod)) {
        return nullptr;
    }
    ASR::Module_t *iso_c_binding = ASR::down_cast<ASR::Module_t>(mod);
    return iso_c_binding->m_intrinsic ? iso_c_binding : nullptr;
}

ASR::symbol_t* get_iso_c_binding_symbol(SymbolTable* scope,
        const std::string &original_name)
{
    ASR::Module_t *iso_c_binding = get_loaded_iso_c_binding_module(scope);
    if (iso_c_binding == nullptr) {
        return nullptr;
    }
    return iso_c_binding->m_symtab->resolve_symbol(original_name);
}

bool is_iso_c_binding_symbol(SymbolTable* scope, ASR::symbol_t *sym,
        const std::string &original_name)
{
    if (sym == nullptr) {
        return false;
    }
    ASR::symbol_t *expected = get_iso_c_binding_symbol(scope, original_name);
    if (expected == nullptr) {
        return false;
    }
    return ASRUtils::symbol_get_past_external(sym)
        == ASRUtils::symbol_get_past_external(expected);
}

bool is_iso_c_null_symbol(SymbolTable* scope, ASR::symbol_t *sym)
{
    return is_iso_c_binding_symbol(scope, sym, "c_null_ptr")
        || is_iso_c_binding_symbol(scope, sym, "c_null_funptr");
}

bool is_iso_c_binding_type_symbol(SymbolTable* scope, ASR::symbol_t *sym,
        const std::string &original_name)
{
    if (sym == nullptr) {
        return false;
    }
    ASR::symbol_t* sym_orig = ASRUtils::symbol_get_past_external(sym);
    if (!ASR::is_a<ASR::Struct_t>(*sym_orig)) {
        return false;
    }
    return is_iso_c_binding_symbol(scope, sym, original_name);
}

bool is_iso_c_ptr_type_symbol(SymbolTable* scope, ASR::symbol_t *sym)
{
    return is_iso_c_binding_type_symbol(scope, sym, "c_ptr");
}

bool is_iso_c_funptr_type_symbol(SymbolTable* scope, ASR::symbol_t *sym)
{
    return is_iso_c_binding_type_symbol(scope, sym, "c_funptr");
}

bool is_iso_fortran_env_team_type_symbol(SymbolTable* scope, ASR::symbol_t *sym)
{
    if (sym == nullptr) {
        return false;
    }
    SymbolTable *tu_scope = ASRUtils::get_tu_symtab(scope);
    LCOMPILERS_ASSERT(tu_scope != nullptr);
    ASR::symbol_t *mod = tu_scope->resolve_symbol("iso_fortran_env");
    if (mod == nullptr) {
        // iso_fortran_env is not loaded, so team_type cannot be in use
        return false;
    }
    LCOMPILERS_ASSERT(ASR::is_a<ASR::Module_t>(*mod));
    ASR::Module_t *iso_fortran_env = ASR::down_cast<ASR::Module_t>(mod);
    if (!iso_fortran_env->m_intrinsic) {
        return false;
    }
    ASR::symbol_t *expected = iso_fortran_env->m_symtab->resolve_symbol("team_type");
    LCOMPILERS_ASSERT(expected != nullptr);
    return ASRUtils::symbol_get_past_external(sym)
        == ASRUtils::symbol_get_past_external(expected);
}

ASR::cptr_kindType cptr_kind_from_type_declaration(SymbolTable* scope,
        ASR::symbol_t* type_decl)
{
    if (is_iso_c_funptr_type_symbol(scope, type_decl)) {
        return ASR::cptr_kindType::CFunPointer;
    }
    if (is_iso_c_ptr_type_symbol(scope, type_decl)) {
        return ASR::cptr_kindType::CPointer;
    }
    return ASR::cptr_kindType::CPtrUnspecified;
}

ASR::ttype_t* make_cptr_type(Allocator& al, const Location& loc,
        SymbolTable* scope, ASR::symbol_t* type_decl)
{
    return ASRUtils::TYPE(ASR::make_CPtr_t(
        al, loc, cptr_kind_from_type_declaration(scope, type_decl)));
}

ASR::expr_t* make_iso_c_null_constant(Allocator& al, const Location& loc,
        SymbolTable* scope, ASR::symbol_t* null_sym)
{
    ASR::symbol_t* type_decl = ASRUtils::is_iso_c_binding_symbol(
        scope, null_sym, "c_null_funptr")
        ? ASRUtils::get_iso_c_binding_symbol(scope, "c_funptr")
        : ASRUtils::get_iso_c_binding_symbol(scope, "c_ptr");
    type_decl = ASRUtils::import_type_declaration(al, type_decl, scope);
    ASR::ttype_t *type_ = ASRUtils::make_cptr_type(
        al, loc, scope, type_decl);
    ASR::expr_t* null_sym_expr = nullptr;
    if (null_sym != nullptr) {
        null_sym_expr = ASRUtils::EXPR(ASR::make_Var_t(al, loc, null_sym));
    }
    return ASRUtils::EXPR(ASR::make_PointerNullConstant_t(
        al, loc, type_, null_sym_expr));
}

void anchor_cptr_null_constant_to_type_declaration(Allocator& al,
        ASR::expr_t* expr, ASR::Variable_t* var)
{
    if (expr == nullptr || var == nullptr ||
            !ASR::is_a<ASR::PointerNullConstant_t>(*expr)) {
        return;
    }
    ASR::PointerNullConstant_t* pnc =
        ASR::down_cast<ASR::PointerNullConstant_t>(expr);
    if (!ASR::is_a<ASR::CPtr_t>(
            *ASRUtils::extract_type(ASRUtils::expr_type(expr)))) {
        return;
    }
    if (var->m_type_declaration == nullptr) {
        return;
    }
    pnc->m_var_expr = ASRUtils::EXPR(ASR::make_Var_t(
        al, expr->base.loc, var->m_type_declaration));
}

ASR::Variable_t* get_variable_from_symbol(ASR::symbol_t* sym)
{
    if (sym == nullptr) {
        return nullptr;
    }
    sym = ASRUtils::symbol_get_past_external(sym);
    if (ASR::is_a<ASR::Variable_t>(*sym)) {
        return ASR::down_cast<ASR::Variable_t>(sym);
    }
    return nullptr;
}

bool array_indices_have_vector_subscript(ASR::array_index_t* args,
        size_t n_args)
{
    for (size_t i = 0; i < n_args; i++) {
        if (args[i].m_left == nullptr && args[i].m_right != nullptr
                && args[i].m_step == nullptr
                && ASRUtils::is_array(ASRUtils::expr_type(args[i].m_right))) {
            return true;
        }
    }
    return false;
}

ASR::FunctionType_t* get_procedure_type_from_expr(ASR::expr_t* expr)
{
    if (expr == nullptr) {
        return nullptr;
    }
    if (ASR::is_a<ASR::Var_t>(*expr)) {
        ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(expr)->m_v);
        if (ASR::is_a<ASR::Function_t>(*sym)
                || ASRUtils::is_symbol_procedure_variable(sym)) {
            return ASRUtils::get_FunctionType(sym);
        }
        return nullptr;
    }
    if (ASR::is_a<ASR::StructInstanceMember_t>(*expr)) {
        ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::StructInstanceMember_t>(expr)->m_m);
        if (ASRUtils::is_symbol_procedure_variable(sym)) {
            return ASRUtils::get_FunctionType(sym);
        }
        return nullptr;
    }
    ASR::ttype_t* expr_type = ASRUtils::type_get_past_allocatable_pointer(
        ASRUtils::expr_type(expr));
    if (ASR::is_a<ASR::FunctionType_t>(*expr_type)) {
        return ASR::down_cast<ASR::FunctionType_t>(expr_type);
    }
    return nullptr;
}

ASR::FunctionType_t* get_procedure_pointer_component_type(
        ASR::Variable_t* member_var)
{
    if (member_var == nullptr) {
        return nullptr;
    }
    ASR::ttype_t* target_type = ASRUtils::type_get_past_pointer(
        member_var->m_type);
    if (ASR::is_a<ASR::FunctionType_t>(*target_type)) {
        return ASR::down_cast<ASR::FunctionType_t>(target_type);
    }
    return nullptr;
}

bool is_procedure_pointer_value(ASR::Variable_t* member_var,
        ASR::expr_t* value)
{
    return get_procedure_pointer_component_type(member_var) != nullptr
        && get_procedure_type_from_expr(value) != nullptr;
}

bool is_valid_pointer_assignment_target(ASR::expr_t* expr)
{
    if (expr == nullptr || ASR::is_a<ASR::IfExp_t>(*expr)) {
        return false;
    }
    if (ASR::is_a<ASR::Var_t>(*expr)) {
        ASR::Variable_t* var = get_variable_from_symbol(
            ASR::down_cast<ASR::Var_t>(expr)->m_v);
        return var != nullptr && (ASRUtils::is_pointer(var->m_type)
            || var->m_target_attr);
    } else if (ASR::is_a<ASR::StructInstanceMember_t>(*expr)) {
        ASR::StructInstanceMember_t* member =
            ASR::down_cast<ASR::StructInstanceMember_t>(expr);
        ASR::Variable_t* member_var = get_variable_from_symbol(member->m_m);
        if (member_var != nullptr && (ASRUtils::is_pointer(member_var->m_type)
                || member_var->m_target_attr
                || get_procedure_pointer_component_type(member_var) != nullptr)) {
            return true;
        }
        return is_valid_pointer_assignment_target(member->m_v);
    } else if (ASR::is_a<ASR::ArrayItem_t>(*expr)) {
        ASR::ArrayItem_t* item = ASR::down_cast<ASR::ArrayItem_t>(expr);
        if (array_indices_have_vector_subscript(item->m_args, item->n_args)) {
            return false;
        }
        return is_valid_pointer_assignment_target(item->m_v);
    } else if (ASR::is_a<ASR::ArraySection_t>(*expr)) {
        ASR::ArraySection_t* section = ASR::down_cast<ASR::ArraySection_t>(expr);
        if (array_indices_have_vector_subscript(section->m_args,
                section->n_args)) {
            return false;
        }
        return is_valid_pointer_assignment_target(section->m_v);
    } else if (ASR::is_a<ASR::Cast_t>(*expr)) {
        return is_valid_pointer_assignment_target(
            ASR::down_cast<ASR::Cast_t>(expr)->m_arg);
    } else if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*expr)) {
        return is_valid_pointer_assignment_target(
            ASR::down_cast<ASR::ArrayPhysicalCast_t>(expr)->m_arg);
    }
    return ASRUtils::is_pointer(ASRUtils::expr_type(expr));
}

void set_null_context_from_variable(Allocator& al, const Location& loc,
        SymbolTable* scope, ASR::Variable_t* var,
        ASR::ttype_t*& current_variable_type,
        ASR::expr_t*& current_struct_type_var_expr)
{
    current_variable_type = nullptr;
    current_struct_type_var_expr = nullptr;
    if (var == nullptr) {
        return;
    }
    current_variable_type = var->m_type;
    ASR::ttype_t* var_type = ASRUtils::extract_type(var->m_type);
    if (ASR::is_a<ASR::StructType_t>(*var_type) ||
            ASR::is_a<ASR::FunctionType_t>(*var_type)) {
        ASR::symbol_t* type_decl = ASRUtils::import_type_declaration(
            al, var->m_type_declaration, scope);
        if (type_decl != nullptr) {
            current_struct_type_var_expr = ASRUtils::EXPR(
                ASR::make_Var_t(al, loc, type_decl));
        }
    }
}

ASR::symbol_t* get_cptr_type_declaration_from_expr(ASR::expr_t* expression)
{
    if (expression == nullptr) {
        return nullptr;
    }
    switch (expression->type) {
        case ASR::exprType::PointerNullConstant: {
            ASR::PointerNullConstant_t* pnc =
                ASR::down_cast<ASR::PointerNullConstant_t>(expression);
            return get_cptr_type_declaration_from_expr(pnc->m_var_expr);
        }
        case ASR::exprType::PointerToCPtr: {
            return nullptr;
        }
        case ASR::exprType::Var: {
            ASR::symbol_t* sym =
                ASR::down_cast<ASR::Var_t>(expression)->m_v;
            ASR::symbol_t* sym_orig = ASRUtils::symbol_get_past_external(sym);
            if (ASR::is_a<ASR::Struct_t>(*sym_orig)) {
                return sym_orig;
            }
            if (ASR::is_a<ASR::Variable_t>(*sym_orig)) {
                ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(sym_orig);
                return ASRUtils::symbol_get_past_external(var->m_type_declaration);
            }
            if (ASR::is_a<ASR::Function_t>(*sym_orig)) {
                ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(sym_orig);
                return func->m_return_var != nullptr
                    ? get_cptr_type_declaration_from_expr(func->m_return_var)
                    : nullptr;
            }
            return nullptr;
        }
        case ASR::exprType::StructInstanceMember: {
            ASR::StructInstanceMember_t* member =
                ASR::down_cast<ASR::StructInstanceMember_t>(expression);
            ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(member->m_m);
            if (!ASR::is_a<ASR::Variable_t>(*sym)) {
                return nullptr;
            }
            ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(sym);
            return ASRUtils::symbol_get_past_external(var->m_type_declaration);
        }
        case ASR::exprType::ArrayItem: {
            return get_cptr_type_declaration_from_expr(
                ASR::down_cast<ASR::ArrayItem_t>(expression)->m_v);
        }
        case ASR::exprType::ArraySection: {
            return get_cptr_type_declaration_from_expr(
                ASR::down_cast<ASR::ArraySection_t>(expression)->m_v);
        }
        case ASR::exprType::Cast: {
            ASR::Cast_t* cast = ASR::down_cast<ASR::Cast_t>(expression);
            return get_cptr_type_declaration_from_expr(cast->m_arg);
        }
        case ASR::exprType::ArrayPhysicalCast: {
            return get_cptr_type_declaration_from_expr(
                ASR::down_cast<ASR::ArrayPhysicalCast_t>(expression)->m_arg);
        }
        case ASR::exprType::FunctionCall: {
            ASR::FunctionCall_t* func_call =
                ASR::down_cast<ASR::FunctionCall_t>(expression);
            ASR::Function_t* func = get_function(func_call->m_name);
            return func != nullptr && func->m_return_var != nullptr
                ? get_cptr_type_declaration_from_expr(func->m_return_var)
                : nullptr;
        }
        default: {
            return nullptr;
        }
    }
}

ASR::symbol_t* get_union_sym_from_union_expr(ASR::expr_t* expression)
{
    switch (expression->type) {
        case ASR::exprType::Var: {
            // The symbol m_v has to be `Variable` or 'Function' for a Union expression.
            if (ASR::is_a<ASR::Variable_t>(*ASRUtils::symbol_get_past_external(ASR::down_cast<ASR::Var_t>(expression)->m_v))) {
                ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(ASRUtils::symbol_get_past_external(ASR::down_cast<ASR::Var_t>(expression)->m_v));
                return var->m_type_declaration;
            } else if (ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(ASR::down_cast<ASR::Var_t>(expression)->m_v))) {
                ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(ASRUtils::symbol_get_past_external(ASR::down_cast<ASR::Var_t>(expression)->m_v));
                if (func->m_return_var != nullptr && ASRUtils::symbol_get_past_external(ASRUtils::get_union_sym_from_union_expr(func->m_return_var))) {
                   return ASRUtils::symbol_get_past_external(ASRUtils::get_union_sym_from_union_expr(func->m_return_var));
                } else {
                    for (size_t i = 0; i < func->n_args; i++) {
                        ASR::expr_t* arg = func->m_args[i];
                        if (arg != nullptr) {
                            ASR::symbol_t* union_sym = ASRUtils::symbol_get_past_external(ASRUtils::get_union_sym_from_union_expr(arg));
                            if (union_sym != nullptr) {
                                return union_sym;
                            }
                        }
                    }
                    return nullptr;
                }
            } else if (ASR::is_a<ASR::Union_t>(*ASRUtils::symbol_get_past_external(ASR::down_cast<ASR::Var_t>(expression)->m_v))) {
                // If the Var is a Union, we return the symbol of the Union.
                return ASRUtils::symbol_get_past_external(ASR::down_cast<ASR::Var_t>(expression)->m_v);
            } else {
                throw LCompilersException("Expected Var to be either Variable or Function type, but found: " +
                                    std::to_string(ASR::down_cast<ASR::Var_t>(expression)->m_v->type));
            }
        }
        case ASR::exprType::UnionInstanceMember: {
            ASR::UnionInstanceMember_t* union_instance_member = ASR::down_cast<ASR::UnionInstanceMember_t>(expression);
            ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(ASRUtils::symbol_get_past_external(union_instance_member->m_m));
            return var->m_type_declaration;
        }
        case ASR::exprType::FunctionCall: {
            ASR::FunctionCall_t* func_call = ASR::down_cast<ASR::FunctionCall_t>(expression);
            // `func_call->m_dt` will be non-null for Union expressions
            if ( func_call->m_dt != nullptr ){
                // If `func_call->m_dt` is not null, it means that the function call
                // is returning a union type.
                return ASRUtils::symbol_get_past_external(ASRUtils::get_union_sym_from_union_expr(func_call->m_dt));
            } else {
                ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(ASRUtils::symbol_get_past_external(func_call->m_name));
                return ASRUtils::symbol_get_past_external(ASRUtils::get_union_sym_from_union_expr(func->m_return_var));
            }
        }
        case ASR::exprType::UnionConstructor: {
            ASR::UnionConstructor_t* union_constructor = ASR::down_cast<ASR::UnionConstructor_t>(expression);
            return union_constructor->m_dt_sym;
        }
        default: {
            throw LCompilersException("get_union_sym_from_union_expr() not implemented for "
                                + std::to_string(expression->type));
            return nullptr;
        }
    }
}


// Recursively fetch `ASR::Function_t` from an `ASR::expr_t` if it has `FunctionType`.
const ASR::Function_t* get_function_from_expr(ASR::expr_t* expr) {
    if (!expr) {
        throw LCompilersException("Passed `ASR::expr_t expr` is nullptr.");
    }
    if (!ASR::is_a<ASR::FunctionType_t>(*ASRUtils::extract_type(ASRUtils::expr_type(expr)))) {
        throw LCompilersException("`ttype_t` of passed `ASR::expr_t expr` is not `ASR::exprType::FunctionType`.");
    }

    switch (expr->type) {
        case ASR::exprType::Var: {
            ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(expr);
            ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(var->m_v);
            if (ASR::is_a<ASR::Function_t>(*sym)) {
                return ASR::down_cast<ASR::Function_t>(sym);
            } else if ( ASR::is_a<ASR::Variable_t>(*sym) ) {
                // case: procedure variables
                ASR::Variable_t* var_sym = ASR::down_cast<ASR::Variable_t>(sym);
                ASR::symbol_t* type_decl = ASRUtils::symbol_get_past_external(var_sym->m_type_declaration);
                if (type_decl != nullptr && 
                    ASR::is_a<ASR::Function_t>(*type_decl)) {
                    return ASR::down_cast<ASR::Function_t>(type_decl);
                } else {
                    throw LCompilersException("`ASR::Var_t` symbol is a `ASR::Variable_t` without type declared");
                }
            } else {
                throw LCompilersException("`ASR::Var_t` symbol is not `ASR::Function_t`, rather: " + std::to_string(sym->type));
            }
        }
        case ASR::exprType::StructInstanceMember: {
            ASR::StructInstanceMember_t* sim = ASR::down_cast<ASR::StructInstanceMember_t>(expr);
            ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(sim->m_m);
            if (ASR::is_a<ASR::Function_t>(*sym)) {
                return ASR::down_cast<ASR::Function_t>(sym);
            } else if ( ASR::is_a<ASR::Variable_t>(*sym) ) {
                // case: procedure variables
                ASR::Variable_t* var_sym = ASR::down_cast<ASR::Variable_t>(sym);
                ASR::symbol_t* type_decl = ASRUtils::symbol_get_past_external(var_sym->m_type_declaration);
                if (type_decl != nullptr && 
                    ASR::is_a<ASR::Function_t>(*type_decl)) {
                    return ASR::down_cast<ASR::Function_t>(type_decl);
                } else {
                    throw LCompilersException("`ASR::Var_t` symbol is a `ASR::Variable_t` without type declared");
                }
            } else {
                throw LCompilersException("`ASR::Var_t` symbol is not `ASR::Function_t`, rather: " + std::to_string(sym->type));
            }
            return get_function_from_expr(sim->m_v);
        }
        case ASR::exprType::ArrayItem: {
            ASR::ArrayItem_t* ai = ASR::down_cast<ASR::ArrayItem_t>(expr);
            return get_function_from_expr(ai->m_v);
        }
        case ASR::exprType::ArraySection: {
            ASR::ArraySection_t* as = ASR::down_cast<ASR::ArraySection_t>(expr);
            return get_function_from_expr(as->m_v);
        }
        case ASR::exprType::FunctionCall: {
            ASR::FunctionCall_t* fc = ASR::down_cast<ASR::FunctionCall_t>(expr);
            ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(fc->m_name);
            if (ASR::is_a<ASR::Function_t>(*sym)) {
                ASR::Function_t* called_func = ASR::down_cast<ASR::Function_t>(sym);
                if (called_func->m_return_var != nullptr) {
                    ASR::ttype_t* ret_type = ASRUtils::extract_type(
                        ASRUtils::expr_type(called_func->m_return_var));
                    if (ASR::is_a<ASR::FunctionType_t>(*ret_type)
                            && ASR::is_a<ASR::Var_t>(*called_func->m_return_var)) {
                        // The call returns a procedure pointer. Resolve it
                        // directly (without recursing into get_function_from_expr)
                        // to avoid an infinite loop when the return variable's
                        // type_declaration points back at `called_func` itself.
                        ASR::Var_t* rv = ASR::down_cast<ASR::Var_t>(called_func->m_return_var);
                        ASR::symbol_t* rv_sym = ASRUtils::symbol_get_past_external(rv->m_v);
                        if (ASR::is_a<ASR::Variable_t>(*rv_sym)) {
                            ASR::Variable_t* rv_var = ASR::down_cast<ASR::Variable_t>(rv_sym);
                            ASR::symbol_t* td = rv_var->m_type_declaration;
                            if (td != nullptr) {
                                td = ASRUtils::symbol_get_past_external(td);
                                if (td != nullptr
                                        && ASR::is_a<ASR::Function_t>(*td)
                                        && td != (ASR::symbol_t*)called_func) {
                                    return ASR::down_cast<ASR::Function_t>(td);
                                }
                            }
                        }
                    }
                }
                return called_func;
            } else if (ASR::is_a<ASR::Variable_t>(*sym)) {
                ASR::Variable_t* var_sym = ASR::down_cast<ASR::Variable_t>(sym);
                ASR::symbol_t* type_decl = ASRUtils::symbol_get_past_external(var_sym->m_type_declaration);
                if (type_decl != nullptr &&
                    ASR::is_a<ASR::Function_t>(*type_decl)) {
                    return ASR::down_cast<ASR::Function_t>(type_decl);
                }
            }
            return nullptr;
        }
        case ASR::exprType::PointerNullConstant: {
            // PointerNullConstant is a special case where it does not have a function
            // associated with it, so we return nullptr.
            return nullptr;
        }
        case ASR::exprType::FunctionPointerCast: {
            // A cast without `to` targets the opaque procedure type, which
            // has no interface symbol.
            ASR::symbol_t* to = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::FunctionPointerCast_t>(expr)->m_to);
            if (to && ASR::is_a<ASR::Function_t>(*to)) {
                return ASR::down_cast<ASR::Function_t>(to);
            }
            return nullptr;
        }
        default:
            throw LCompilersException("get_function_from_expr() not implemented for "
                                + std::to_string(expr->type));
    }
}

std::vector<std::string> determine_variable_declaration_order(
         SymbolTable* symtab) {
    std::map<std::string, std::vector<std::string>> var_dep_graph;
    for( auto itr: symtab->get_scope() ) {
        if( ASR::is_a<ASR::Variable_t>(*itr.second) ) {
            std::vector<std::string> deps;
            ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(itr.second);
            for( size_t i = 0; i < var->n_dependencies; i++ ) {
                std::string dep = var->m_dependencies[i];
                // Check if the dependent variable is present in the symtab.
                // This will help us to include only local dependencies, and we
                // assume that dependencies in the parent symtab are already declared
                // earlier.
                if (symtab->get_symbol(dep) != nullptr)
                    deps.push_back(dep);
            }
            var_dep_graph[itr.first] = deps;
        }
    }
    return ASRUtils::order_deps(var_dep_graph);
}

void extract_module_python(const ASR::TranslationUnit_t &m,
                std::vector<std::pair<std::string, ASR::Module_t*>>& children_modules,
                std::string module_name) {
    bool module_found = false;
    for (auto &a : m.m_symtab->get_scope()) {
        if( ASR::is_a<ASR::Module_t>(*a.second) ) {
            if( a.first == "__main__" ) {
                module_found = true;
                children_modules.push_back(std::make_pair(module_name,
                    ASR::down_cast<ASR::Module_t>(a.second)));
            } else {
                children_modules.push_back(std::make_pair(a.first,
                    ASR::down_cast<ASR::Module_t>(a.second)));
            }
        }
    }
    if( !module_found ) {
        throw LCompilersException("ICE: Module not found");
    }
}

void update_call_args(Allocator &al, SymbolTable *current_scope, bool implicit_interface) {
    /*
        Iterate over body of program, check if there are any subroutine calls if yes, iterate over its args
        and update the args if they are equal to the old symbol
        For example:
            function func(f)
                double precision c
                call sub2(c)
                print *, c(d)
            end function
        This function updates `sub2` to use the new symbol `c` that is now a function, not a variable.
        Along with this, it also updates the args of `sub2` to use the new symbol `c` instead of the old one.
    */

    class ArgsReplacer : public ASR::BaseExprReplacer<ArgsReplacer> {
    public:
        Allocator &al;
        ASR::symbol_t* new_sym;

        ArgsReplacer(Allocator &al_) : al(al_) {}

        void replace_Var(ASR::Var_t* x) {
            *current_expr = ASRUtils::EXPR(ASR::make_Var_t(al, x->base.base.loc, new_sym));
        }
    };

    class ArgsVisitor : public ASR::CallReplacerOnExpressionsVisitor<ArgsVisitor>
    {
        public:
        Allocator &al;
        SymbolTable* scope = current_scope;
        ArgsReplacer replacer;
        ArgsVisitor(Allocator &al_) : al(al_), replacer(al_) {}

        void call_replacer_(ASR::symbol_t* new_sym) {
            replacer.current_expr = current_expr;
            replacer.new_sym = new_sym;
            replacer.replace_expr(*current_expr);
        }

        ASR::symbol_t* fetch_sym(ASR::symbol_t* arg_sym_underlying) {
            ASR::symbol_t* sym = nullptr;
            if (ASR::is_a<ASR::Variable_t>(*arg_sym_underlying)) {
                ASR::Variable_t* arg_variable = ASR::down_cast<ASR::Variable_t>(arg_sym_underlying);
                std::string arg_variable_name = std::string(arg_variable->m_name);
                sym = arg_variable->m_parent_symtab->get_symbol(arg_variable_name);
            } else if (ASR::is_a<ASR::Function_t>(*arg_sym_underlying)) {
                ASR::Function_t* arg_function = ASR::down_cast<ASR::Function_t>(arg_sym_underlying);
                std::string arg_function_name = std::string(arg_function->m_name);
                sym = arg_function->m_symtab->parent->get_symbol(arg_function_name);
            }
            return sym;
        }

        void handle_Var(ASR::expr_t* arg_expr, ASR::expr_t** expr_to_replace) {
            if (arg_expr && ASR::is_a<ASR::Var_t>(*arg_expr)) {
                ASR::Var_t* arg_var = ASR::down_cast<ASR::Var_t>(arg_expr);
                ASR::symbol_t* arg_sym = arg_var->m_v;
                ASR::symbol_t* arg_sym_underlying = ASRUtils::symbol_get_past_external(arg_sym);
                ASR::symbol_t* sym = fetch_sym(arg_sym_underlying);
                if (sym != arg_sym_underlying) {
                    ASR::expr_t** current_expr_copy = current_expr;
                    current_expr = const_cast<ASR::expr_t**>((expr_to_replace));
                    this->call_replacer_(sym);
                    current_expr = current_expr_copy;
                }
            }
        }


        void visit_SubroutineCall(const ASR::SubroutineCall_t& x) {
            ASR::SubroutineCall_t* subrout_call = (ASR::SubroutineCall_t*)(&x);
            for (size_t j = 0; j < subrout_call->n_args; j++) {
                ASR::call_arg_t arg = subrout_call->m_args[j];
                ASR::expr_t* arg_expr = arg.m_value;
                handle_Var(arg_expr, &(subrout_call->m_args[j].m_value));
            }
        }

        void visit_FunctionCall(const ASR::FunctionCall_t& x) {
            ASR::FunctionCall_t* func_call = (ASR::FunctionCall_t*)(&x);
            for (size_t j = 0; j < func_call->n_args; j++) {
                ASR::call_arg_t arg = func_call->m_args[j];
                ASR::expr_t* arg_expr = arg.m_value;
                handle_Var(arg_expr, &(func_call->m_args[j].m_value));
            }
        }

        void visit_Function(const ASR::Function_t& x) {
            ASR::Function_t* func = (ASR::Function_t*)(&x);
            scope = func->m_symtab;
            for (size_t i = 0; i < func->n_args; i++) {
                ASR::expr_t* arg_expr = func->m_args[i];
                if (ASR::is_a<ASR::Var_t>(*arg_expr)) {
                    ASR::Var_t* arg_var = ASR::down_cast<ASR::Var_t>(arg_expr);
                    ASR::symbol_t* arg_sym = arg_var->m_v;
                    ASR::symbol_t* arg_sym_underlying = ASRUtils::symbol_get_past_external(arg_sym);
                    ASR::symbol_t* sym = fetch_sym(arg_sym_underlying);
                    if (sym != arg_sym_underlying) {
                        ASR::expr_t** current_expr_copy = current_expr;
                        current_expr = const_cast<ASR::expr_t**>(&(func->m_args[i]));
                        this->call_replacer_(sym);
                        current_expr = current_expr_copy;
                    }
                }
            }
            scope = func->m_symtab;
            for (auto &it: scope->get_scope()) {
                visit_symbol(*it.second);
            }
            scope = func->m_symtab;
            for (size_t i = 0; i < func->n_body; i++) {
                visit_stmt(*func->m_body[i]);
            }
            scope = func->m_symtab;
        }
    };

    if (implicit_interface) {
        ArgsVisitor v(al);
        SymbolTable *tu_symtab = ASRUtils::get_tu_symtab(current_scope);
        ASR::asr_t* asr_ = tu_symtab->asr_owner;
        ASR::TranslationUnit_t* tu = ASR::down_cast2<ASR::TranslationUnit_t>(asr_);
        v.visit_TranslationUnit(*tu);
    }
}

ASR::Module_t* extract_module(const ASR::TranslationUnit_t &m) {
    LCOMPILERS_ASSERT(m.m_symtab->get_scope().size()== 1);
    for (auto &a : m.m_symtab->get_scope()) {
        LCOMPILERS_ASSERT(ASR::is_a<ASR::Module_t>(*a.second));
        return ASR::down_cast<ASR::Module_t>(a.second);
    }
    throw LCompilersException("ICE: Module not found");
}

void fix_translation_unit(Allocator &al, ASR::TranslationUnit_t *tu, SymbolTable *symtab, bool run_verify) {
    fix_external_symbols(*tu, *symtab);
    PassUtils::UpdateDependenciesVisitor v(al);
    v.visit_TranslationUnit(*tu);
    if (run_verify) {
#if defined(WITH_LFORTRAN_ASSERT)
        diag::Diagnostics diagnostics;
        if (!asr_verify(*tu, true, diagnostics)) {
            std::cerr << diagnostics.render2();
            throw LCompilersException("Verify failed");
        };
#endif
    }
}

ASR::Module_t* load_module(Allocator &al, SymbolTable *symtab,
                            const std::string &module_name,
                            const Location &loc, bool intrinsic,
                            std::set<std::string> &loaded_submodules,
                            LCompilers::PassOptions& pass_options,
                            bool run_verify,
                            const std::function<void (const std::string &, const Location &)> err,
                            LCompilers::LocationManager &lm,
                            bool generate_object_code,
                            bool load_submodules) {
    LCOMPILERS_ASSERT(symtab);
    // In interactive mode each cell is a TranslationUnit chained to the
    // previous one, so a module declared in an earlier cell lives in a parent
    // scope. Resolve through the chain: without this the module is not found
    // here and a modfile is looked for on disk, which there is none of.
    if (ASR::symbol_t *m = symtab->resolve_symbol(module_name)) {
        if (ASR::is_a<ASR::Module_t>(*m)) {
            return ASR::down_cast<ASR::Module_t>(m);
        } else {
            err("The symbol '" + module_name + "' is not a module", loc);
        }
    }
    LCOMPILERS_ASSERT(symtab->parent == nullptr ||
        ASRUtils::is_tu_scope(symtab->parent));
    ASR::TranslationUnit_t* mod1 = nullptr;
    Result<ASR::TranslationUnit_t*, ErrorMessage> res
        = find_and_load_module(al, module_name, *symtab, intrinsic, pass_options, lm);
    std::string error_message = "Module '" + module_name + "' not declared in the current source and the modfile was not found";
    if (res.ok) {
        mod1 = res.result;
    } else {
        error_message = res.error.message;
        if (!intrinsic) {
            // Module not found as a regular module. Try intrinsic module
            if (module_name == "iso_c_binding"
                ||module_name == "iso_fortran_env"
                ||module_name == "ieee_arithmetic") {
                Result<ASR::TranslationUnit_t*, ErrorMessage> res
                    = find_and_load_module(al, "lfortran_intrinsic_" + module_name,
                        *symtab, true, pass_options, lm);
                if (res.ok) {
                    mod1 = res.result;
                } else {
                    error_message = res.error.message;
                }
            }
        }
    }
    if (mod1 == nullptr) {
        err(error_message, loc);
    }
    ASR::Module_t *mod2 = extract_module(*mod1);
    symtab->add_symbol(module_name, (ASR::symbol_t*)mod2);
    mod2->m_symtab->parent = symtab;
    mod2->m_loaded_from_mod = true;
    if ( generate_object_code && !startswith(mod2->m_name, "lfortran_intrinsic") ) {
        mod2->m_symtab->mark_all_variables_external(al);
    }
    LCOMPILERS_ASSERT(symtab->resolve_symbol(module_name));

    // Create a temporary TranslationUnit just for fixing the symbols
    ASR::asr_t *orig_asr_owner = symtab->asr_owner;
    ASR::TranslationUnit_t *tu
        = ASR::down_cast2<ASR::TranslationUnit_t>(ASR::make_TranslationUnit_t(al, loc,
            symtab, nullptr, 0, nullptr));

    // Load any dependent modules recursively
    bool rerun = true;
    while (rerun) {
        rerun = false;
        std::vector<std::string> modules_list
            = determine_module_dependencies(*tu);
        for (auto &item : modules_list) {
            if (symtab->get_symbol(item)
                    == nullptr) {
                // A module that was loaded requires to load another
                // module

                // This is not very robust, we should store that information
                // in the ASR itself, or encode in the name in a robust way,
                // such as using `module_name@intrinsic`:
                bool is_intrinsic = startswith(item, "lfortran_intrinsic");
                ASR::TranslationUnit_t *mod1 = nullptr;
                Result<ASR::TranslationUnit_t*, ErrorMessage> res
                    = find_and_load_module(al, item, *symtab, is_intrinsic, pass_options, lm);
                std::string error_message = "Module '" + item + "' modfile was not found";
                if (res.ok) {
                    mod1 = res.result;
                } else {
                    error_message =  res.error.message;
                    if (!is_intrinsic) {
                        // Module not found as a regular module. Try intrinsic module
                        if (item == "iso_c_binding"
                            ||item == "iso_fortran_env") {
                            Result<ASR::TranslationUnit_t*, ErrorMessage> res
                                = find_and_load_module(al, "lfortran_intrinsic_" + item,
                                *symtab, true, pass_options, lm);
                            if (res.ok) {
                                mod1 = res.result;
                            } else {
                                error_message =  res.error.message;
                            }
                        }
                    }
                }

                if (mod1 == nullptr) {
                    err(error_message, loc);
                }
                ASR::Module_t *mod2 = extract_module(*mod1);
                symtab->add_symbol(item, (ASR::symbol_t*)mod2);
                mod2->m_symtab->parent = symtab;
                mod2->m_loaded_from_mod = true;
                if ( generate_object_code && !startswith(mod2->m_name, "lfortran_intrinsic") ) {
                    mod2->m_symtab->mark_all_variables_external(al);
                }
                rerun = true;
            }
        }
    }

    if (load_submodules) {
        load_dependent_submodules(al, symtab, mod2, loc,
                                  loaded_submodules, pass_options,
                                  run_verify, err, lm);
    }

    // Check that all modules are included in ASR now
    std::vector<std::string> modules_list
        = determine_module_dependencies(*tu);
    for (auto &item : modules_list) {
        if (symtab->get_symbol(item) == nullptr) {
            err("ICE: Module '" + item + "' modfile was not found, but should have", loc);
        }
    }

    // Fix all external symbols and update dependencies
    fix_translation_unit(al, tu, symtab, run_verify);
    symtab->asr_owner = orig_asr_owner;

    return mod2;
}

void load_dependent_submodules(Allocator &al, SymbolTable *symtab,
                               ASR::Module_t* mod, const Location &loc,
                               std::set<std::string> &loaded_submodules,
                               LCompilers::PassOptions& pass_options,
                               bool run_verify,
                               const std::function<void (const std::string &, const Location &)> err,
                               LCompilers::LocationManager &lm) {
    if (startswith(std::string(mod->m_name), "lfortran_intrinsic")) {
        return ;
    }

    if (loaded_submodules.count(std::string(mod->m_name))) {
        return ;
    }
    loaded_submodules.insert(std::string(mod->m_name));

    for (size_t i=0;i<mod->n_dependencies;i++) {
        std::string dep_name = std::string(mod->m_dependencies[i]);
        ASR::symbol_t *dep_sym = symtab->get_symbol(dep_name);
        if (dep_sym == nullptr) {
            bool is_intrinsic = startswith(dep_name, "lfortran_intrinsic");
            load_module(al, symtab, dep_name, loc, is_intrinsic, loaded_submodules,
                        pass_options, run_verify, err, lm, false, true);
            dep_sym = symtab->get_symbol(dep_name);
        }
        if (dep_sym == nullptr) {
            continue;
        }
        if (!ASR::is_a<ASR::Module_t>(*dep_sym)) {
            err("The symbol '" + dep_name + "' must be a module", loc);
            continue;
        }
        ASR::Module_t* dep_mod = ASR::down_cast<ASR::Module_t>(dep_sym);
        load_dependent_submodules(al, symtab, dep_mod, loc,
                                  loaded_submodules, pass_options,
                                  run_verify, err, lm);
    }

    if (mod->m_has_submodules) {
        std::vector<ASR::Module_t*> pending;
        pending.push_back(mod);
        while (!pending.empty()) {
            ASR::Module_t* current = pending.back();
            pending.pop_back();
            std::vector<ASR::TranslationUnit_t*> submods;
            Result<std::vector<ASR::TranslationUnit_t*>, ErrorMessage> res
                = ASRUtils::find_and_load_submodules(al, std::string(current->m_name), *symtab, pass_options, lm);
            if (res.ok) {
                submods = res.result;
            } else {
                std::string error_message = res.error.message;
                err(error_message, loc);
            }
            for (size_t i=0;i<submods.size();i++) {
                ASR::Module_t *submod = ASRUtils::extract_module(*submods[i]);
                if (symtab->get_symbol(std::string(submod->m_name)) == nullptr) {
                    symtab->add_symbol(std::string(submod->m_name), (ASR::symbol_t*)submod);
                    submod->m_symtab->parent = symtab;
                    submod->m_loaded_from_mod = true;
                    pending.push_back(submod);
                }
            }
        }
    }

    // Create a temporary TranslationUnit just for fixing the symbols
    ASR::asr_t *orig_asr_owner = symtab->asr_owner;
    ASR::TranslationUnit_t *tu
        = ASR::down_cast2<ASR::TranslationUnit_t>(ASR::make_TranslationUnit_t(al, loc,
            symtab, nullptr, 0, nullptr));

    // Keeps track of loaded dependent modules whose submodules are not yet loaded
    std::vector<ASR::Module_t*> dependent_modules_with_not_yet_loaded_submodules;

    // Load any dependent modules recursively
    bool rerun = true;
    while (rerun) {
        rerun = false;
        std::vector<std::string> modules_list
            = determine_module_dependencies(*tu);
        for (auto &item : modules_list) {
            // In interactive mode a module a dependency names may have been
            // declared by an earlier cell, so it lives in a parent scope.
            // Resolve through the chain, or a modfile is looked for on disk,
            // of which a notebook has none.
            if (symtab->resolve_symbol(item)
                    == nullptr) {
                bool is_intrinsic = startswith(item, "lfortran_intrinsic");
                ASR::TranslationUnit_t *mod1 = nullptr;
                Result<ASR::TranslationUnit_t*, ErrorMessage> res
                    = find_and_load_module(al, item, *symtab, is_intrinsic, pass_options, lm);
                std::string error_message = "Module '" + item + "' modfile was not found";
                if (res.ok) {
                    mod1 = res.result;
                } else {
                    error_message =  res.error.message;
                    if (!is_intrinsic) {
                        // Module not found as a regular module. Try intrinsic module
                        if (item == "iso_c_binding"
                            ||item == "iso_fortran_env") {
                            Result<ASR::TranslationUnit_t*, ErrorMessage> res
                                = find_and_load_module(al, "lfortran_intrinsic_" + item,
                                *symtab, true, pass_options, lm);
                            if (res.ok) {
                                mod1 = res.result;
                            } else {
                                error_message =  res.error.message;
                            }
                        }
                    }
                }

                if (mod1 == nullptr) {
                    err(error_message, loc);
                }
                ASR::Module_t *mod2 = extract_module(*mod1);
                symtab->add_symbol(item, (ASR::symbol_t*)mod2);
                mod2->m_symtab->parent = symtab;
                mod2->m_loaded_from_mod = true;
                dependent_modules_with_not_yet_loaded_submodules.push_back(mod2);
                rerun = true;
            }
        }
    }

    // Load all the submodules of dependent modules recursively
    for (size_t i=0;i<dependent_modules_with_not_yet_loaded_submodules.size();i++) {
        load_dependent_submodules(al, symtab, dependent_modules_with_not_yet_loaded_submodules[i],
                                  loc, loaded_submodules, pass_options, run_verify, err, lm);
    }

    symtab->asr_owner = orig_asr_owner;
}

ASR::asr_t* make_Assignment_t_util(Allocator &al, const Location &a_loc,
    ASR::expr_t* a_target, ASR::expr_t* a_value,
    ASR::stmt_t* a_overloaded, bool a_realloc_lhs, bool a_move,
    bool a_new_target, bool a_new_allocation) {
    bool is_allocatable = ASRUtils::is_allocatable(a_target);
    if ( !is_allocatable && ASR::is_a<ASR::ArrayPhysicalCast_t>(*a_target) ) {
        is_allocatable = ASRUtils::is_allocatable(
            ASRUtils::get_past_array_physical_cast(a_target));
    }
    if ( ASR::is_a<ASR::StructInstanceMember_t>(*a_target) ) {
        ASR::StructInstanceMember_t* a_target_struct = ASR::down_cast<ASR::StructInstanceMember_t>(a_target);
        is_allocatable |= ASRUtils::is_allocatable(a_target_struct->m_v);
    }
    a_realloc_lhs = a_realloc_lhs && is_allocatable;
    return ASR::make_Assignment_t(al, a_loc, a_target, a_value,
        a_overloaded, a_realloc_lhs, a_move, a_new_target, a_new_allocation);
}

void set_intrinsic(ASR::symbol_t* sym) {
    switch( sym->type ) {
        case ASR::symbolType::Module: {
            ASR::Module_t* module_sym = ASR::down_cast<ASR::Module_t>(sym);
            module_sym->m_intrinsic = true;
            for( auto& itr: module_sym->m_symtab->get_scope() ) {
                set_intrinsic(itr.second);
            }
            break;
        }
        case ASR::symbolType::Function: {
            ASR::Function_t* function_sym = ASR::down_cast<ASR::Function_t>(sym);
            ASR::FunctionType_t* func_sym_type = ASR::down_cast<ASR::FunctionType_t>(function_sym->m_function_signature);
            func_sym_type->m_abi = ASR::abiType::Intrinsic;
            break;
        }
        case ASR::symbolType::Struct: {
            ASR::Struct_t* derived_type_sym = ASR::down_cast<ASR::Struct_t>(sym);
            derived_type_sym->m_abi = ASR::abiType::Intrinsic;
            break;
        }
        case ASR::symbolType::Variable: {
            ASR::Variable_t* derived_type_sym = ASR::down_cast<ASR::Variable_t>(sym);
            derived_type_sym->m_abi = ASR::abiType::Intrinsic;
            break;
        }
        default: {
            break;
        }
    }
}

void set_intrinsic(ASR::TranslationUnit_t* trans_unit) {
    for( auto& itr: trans_unit->m_symtab->get_scope() ) {
        set_intrinsic(itr.second);
    }
}

ASR::Variable_t* get_storage_designator_offset(ASR::expr_t *designator,
        int64_t &element_index, int64_t &character_index) {
    auto constant = [](ASR::expr_t *e, int64_t &n) {
        ASR::expr_t *value = e ? ASRUtils::expr_value(e) : nullptr;
        return value != nullptr && ASRUtils::extract_value(value, n);
    };
    element_index = 0;
    character_index = 0;
    // A substring is given by its first character, the start, which defaults
    // to 1. Its end, which defaults to the length, and its stride, which is
    // 1, only have to describe a substring that is not empty.
    ASR::expr_t *substring = designator;
    if (ASR::is_a<ASR::StringSection_t>(*designator)) {
        designator = ASR::down_cast<ASR::StringSection_t>(designator)->m_arg;
    } else if (ASR::is_a<ASR::StringItem_t>(*designator)) {
        designator = ASR::down_cast<ASR::StringItem_t>(designator)->m_arg;
    } else {
        substring = nullptr;
    }
    ASR::ArrayItem_t *element = nullptr;
    if (ASR::is_a<ASR::ArrayItem_t>(*designator)) {
        element = ASR::down_cast<ASR::ArrayItem_t>(designator);
        designator = element->m_v;
    }
    if (!ASR::is_a<ASR::Var_t>(*designator)) return nullptr;
    ASR::symbol_t *sym = ASR::down_cast<ASR::Var_t>(designator)->m_v;
    if (!ASR::is_a<ASR::Variable_t>(*sym)) return nullptr;
    ASR::Variable_t *base = ASR::down_cast<ASR::Variable_t>(sym);
    ASR::ttype_t *type = base->m_type;
    if (ASRUtils::is_pointer(type) || ASRUtils::is_allocatable(type)) {
        return nullptr;
    }
    ASR::dimension_t *dims = nullptr;
    size_t n_dims = ASRUtils::extract_dimensions_from_ttype(type, dims);
    if (element != nullptr) {
        if (element->n_args != n_dims || n_dims == 0) return nullptr;
        // The arithmetic is done on offsets already known to lie within the
        // array, so that no bound, however large, can overflow.
        int64_t stride = 1;
        for (size_t i = 0; i < n_dims; i++) {
            const ASR::array_index_t &index = element->m_args[i];
            int64_t subscript, lower = 1, extent;
            if (index.m_left != nullptr || index.m_step != nullptr
                    || !constant(index.m_right, subscript)
                    || (dims[i].m_start != nullptr
                        && !constant(dims[i].m_start, lower))
                    || !constant(dims[i].m_length, extent)
                    || extent <= 0 || subscript < lower) {
                return nullptr;
            }
            uint64_t offset = static_cast<uint64_t>(subscript)
                - static_cast<uint64_t>(lower);
            if (offset >= static_cast<uint64_t>(extent)
                    || offset > static_cast<uint64_t>(
                        (std::numeric_limits<int64_t>::max() - element_index)
                        / stride)) {
                return nullptr;
            }
            element_index += static_cast<int64_t>(offset) * stride;
            if (i + 1 < n_dims) {
                if (extent > std::numeric_limits<int64_t>::max() / stride) {
                    return nullptr;
                }
                stride *= extent;
            }
        }
    }
    if (substring != nullptr) {
        int64_t length;
        if (!ASRUtils::is_character(*type)
                || !constant(ASRUtils::get_string_type(type)->m_len, length)) {
            return nullptr;
        }
        int64_t first = 1, last = length, step = 1;
        if (ASR::is_a<ASR::StringItem_t>(*substring)) {
            if (!constant(ASR::down_cast<ASR::StringItem_t>(substring)->m_idx,
                    first)) {
                return nullptr;
            }
            last = first;
        } else {
            ASR::StringSection_t *s = ASR::down_cast<ASR::StringSection_t>(substring);
            if ((s->m_start != nullptr && !constant(s->m_start, first))
                    || (s->m_end != nullptr && !constant(s->m_end, last))
                    || (s->m_step != nullptr && !constant(s->m_step, step))) {
                return nullptr;
            }
        }
        if (step != 1 || first < 1 || last < first || last > length) {
            return nullptr;
        }
        character_index = first - 1;
    }
    return base;
}

Result<ASR::TranslationUnit_t*, ErrorMessage> find_and_load_module(Allocator &al, const std::string &msym,
                                                SymbolTable &symtab, bool intrinsic,
                                                LCompilers::PassOptions& pass_options,
                                                LCompilers::LocationManager &lm) {
    std::filesystem::path runtime_library_dir { pass_options.runtime_library_dir };
    std::filesystem::path filename {msym + ".mod"};
    std::vector<std::filesystem::path> mod_files_dirs;

    mod_files_dirs.push_back( runtime_library_dir );
    mod_files_dirs.push_back( pass_options.mod_files_dir );
    mod_files_dirs.insert(mod_files_dirs.end(),
                          pass_options.include_dirs.begin(),
                          pass_options.include_dirs.end());

    bool found_empty_mod = false;
    for (auto path : mod_files_dirs) {
        std::string modfile;
        std::filesystem::path full_path = path / filename;
        if (read_file(full_path.string(), modfile)) {
            if (modfile.empty()) {
                found_empty_mod = true;
                continue;
            }
            Result<ASR::TranslationUnit_t*, ErrorMessage> res = load_modfile(al, modfile, false, symtab, lm);
            if (res.ok) {
                ASR::TranslationUnit_t* asr = res.result;
                if (intrinsic) {
                    set_intrinsic(asr);
                }
                return asr;
            } else {
                return res.error;
            }
        }
    }

    if (found_empty_mod) {
        std::string smod_suffix = "@" + msym + ".smod";
        for (auto &path : mod_files_dirs) {
            std::filesystem::path search_dir = path.empty() ? std::filesystem::path(".") : path;
            if (!std::filesystem::is_directory(search_dir)) continue;
            for (auto &entry : std::filesystem::directory_iterator(search_dir)) {
                std::string fname = entry.path().filename().string();
                if (fname.size() > smod_suffix.size() &&
                    fname.compare(fname.size() - smod_suffix.size(),
                                  smod_suffix.size(), smod_suffix) == 0) {
                    std::string smodfile;
                    if (read_file(entry.path().string(), smodfile) && !smodfile.empty()) {
                        Result<ASR::TranslationUnit_t*, ErrorMessage> res = load_modfile(al, smodfile, false, symtab, lm);
                        if (res.ok) {
                            ASR::TranslationUnit_t* asr = res.result;
                            if (intrinsic) {
                                set_intrinsic(asr);
                            }
                            return asr;
                        } else {
                            return res.error;
                        }
                    }
                }
            }
        }
    }

    return ErrorMessage("Module '" + msym + "' modfile was not found");
}

Result<std::vector<ASR::TranslationUnit_t*>, ErrorMessage> find_and_load_submodules(Allocator &al, const std::string &parent_module_name,
                                                            SymbolTable &symtab,
                                                            LCompilers::PassOptions &pass_options,
                                                            LCompilers::LocationManager &lm) {
    std::vector<ASR::TranslationUnit_t*> submodules_collector;
    std::filesystem::path runtime_library_dir { pass_options.runtime_library_dir };
    std::vector<std::filesystem::path> mod_files_dirs;

    mod_files_dirs.push_back(runtime_library_dir);
    mod_files_dirs.push_back(pass_options.mod_files_dir);
    mod_files_dirs.insert(mod_files_dirs.end(),
                          pass_options.include_dirs.begin(),
                          pass_options.include_dirs.end());

    std::string smod_prefix = parent_module_name + "@";
    for (auto &path : mod_files_dirs) {
        if (path.empty()) path = ".";
        if (!std::filesystem::is_directory(path)) continue;
        for (auto &file : std::filesystem::directory_iterator(path)) {
            std::string submod_filename = file.path().filename().string();
            if (startswith(submod_filename, smod_prefix) && endswith(submod_filename, ".smod")) {
                std::string submodfile;
                if (read_file(file.path().string(), submodfile) && !submodfile.empty()) {
                    Result<ASR::TranslationUnit_t*, ErrorMessage> sub_res = load_modfile(al, submodfile, false, symtab, lm);
                    if (sub_res.ok) {
                        ASR::TranslationUnit_t* asr = sub_res.result;
                        submodules_collector.push_back(asr);
                    } else {
                        return sub_res.error;
                    }
                }
            }
        }
    }

    return submodules_collector;
}

bool struct_constant_names_procedure(ASR::expr_t* value) {
    if (value == nullptr || !ASR::is_a<ASR::StructConstant_t>(*value)) {
        return false;
    }
    ASR::StructConstant_t* sc = ASR::down_cast<ASR::StructConstant_t>(value);
    for (size_t i = 0; i < sc->n_args; i++) {
        ASR::expr_t* arg = sc->m_args[i].m_value;
        if (arg == nullptr) {
            continue;
        }
        if (ASR::is_a<ASR::Var_t>(*arg) && ASR::is_a<ASR::Function_t>(
                *ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(arg)->m_v))) {
            return true;
        }
        if (struct_constant_names_procedure(arg)) {
            return true;
        }
    }
    return false;
}

bool constant_names_procedure_outside(ASR::expr_t* value,
        SymbolTable* scope) {
    if (value == nullptr) {
        return false;
    }
    switch (value->type) {
        case ASR::exprType::Var: {
            ASR::symbol_t* v = ASR::down_cast<ASR::Var_t>(value)->m_v;
            return ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(v))
                && !ASRUtils::is_visible_from(v, scope);
        }
        case ASR::exprType::StructConstant: {
            ASR::StructConstant_t* sc = ASR::down_cast<ASR::StructConstant_t>(value);
            for (size_t i = 0; i < sc->n_args; i++) {
                if (constant_names_procedure_outside(sc->m_args[i].m_value, scope)) {
                    return true;
                }
            }
            return false;
        }
        case ASR::exprType::ArrayConstructor: {
            ASR::ArrayConstructor_t* ac = ASR::down_cast<ASR::ArrayConstructor_t>(value);
            for (size_t i = 0; i < ac->n_args; i++) {
                if (constant_names_procedure_outside(ac->m_args[i], scope)) {
                    return true;
                }
            }
            return false;
        }
        case ASR::exprType::ArrayConstant: {
            ASR::ArrayConstant_t* ac = ASR::down_cast<ASR::ArrayConstant_t>(value);
            if (!ASR::is_a<ASR::StructType_t>(*ASRUtils::type_get_past_array(ac->m_type))) {
                return false;
            }
            ASR::expr_t** elements = static_cast<ASR::expr_t**>(ac->m_data);
            int64_t n = ASRUtils::get_fixed_size_of_array(ac->m_type);
            for (int64_t i = 0; i < n; i++) {
                if (constant_names_procedure_outside(elements[i], scope)) {
                    return true;
                }
            }
            return false;
        }
        case ASR::exprType::ArrayBroadcast:
            return constant_names_procedure_outside(
                ASR::down_cast<ASR::ArrayBroadcast_t>(value)->m_array, scope);
        default:
            return false;
    }
}

ASR::expr_t* import_folded_constant_value(Allocator& al,
        ASR::expr_t* value, SymbolTable* scope) {
    if (constant_names_procedure_outside(value, scope)) {
        return nullptr;
    }
    return externalize_struct_refs_in_init(al, value, scope);
}

ASR::expr_t* get_struct_member_value_from_constant(ASR::expr_t* value,
                            ASR::symbol_t* member) {
    if (value == nullptr) {
        return nullptr;
    }
    member = ASRUtils::symbol_get_past_external(member);
    if (ASR::is_a<ASR::ArrayBroadcast_t>(*value)) {
        ASR::ArrayBroadcast_t *array_broadcast = ASR::down_cast<ASR::ArrayBroadcast_t>(value);
        ASR::expr_t* scalar_value = ASRUtils::expr_value(array_broadcast->m_array);
        if (scalar_value == nullptr) {
            scalar_value = array_broadcast->m_array;
        }
        return get_struct_member_value_from_constant(scalar_value, member);
    }
    ASR::expr_t* expr_value = ASRUtils::expr_value(value);
    if (expr_value != nullptr && expr_value != value) {
        value = expr_value;
    }
    if (!ASR::is_a<ASR::StructConstant_t>(*value)) {
        return nullptr;
    }

    ASR::StructConstant_t *stc = ASR::down_cast<ASR::StructConstant_t>(value);
    ASR::Struct_t *struct_s = ASR::down_cast<ASR::Struct_t>(
        ASRUtils::symbol_get_past_external(stc->m_dt_sym));
    std::string mem_name = ASRUtils::symbol_name(member);
    // The arguments of a StructConstant hold the members of the parent types
    // first, so find the index i of the member in that order.
    std::vector<ASR::Struct_t*> struct_chain;
    for (ASR::Struct_t* s = struct_s; s != nullptr;
            s = s->m_parent ? ASR::down_cast<ASR::Struct_t>(
                ASRUtils::symbol_get_past_external(s->m_parent)) : nullptr) {
        struct_chain.push_back(s);
    }
    size_t i = 0;
    bool found = false;
    for (auto it = struct_chain.rbegin(); it != struct_chain.rend() && !found; ++it) {
        for (size_t j = 0; j < (*it)->n_members; j++, i++) {
            if ((*it)->m_members[j] == mem_name) {
                found = true;
                break;
            }
        }
    }

    LCOMPILERS_ASSERT(found && i < stc->n_args);
    return stc->m_args[i].m_value;
}

ASR::expr_t* get_struct_member_value_from_constant_array(Allocator &al,
        const Location& loc, ASR::expr_t* value, ASR::symbol_t* member,
        ASR::ttype_t* result_type) {
    if (value == nullptr) {
        return nullptr;
    }
    ASR::expr_t* scalar_value = get_struct_member_value_from_constant(value, member);
    if (scalar_value) {
        if (ASRUtils::is_array(result_type)
                && !ASRUtils::is_array(ASRUtils::expr_type(scalar_value))) {
            ASR::expr_t* broadcast = ASRUtils::broadcast_scalar_constant_to_array(
                al, scalar_value->base.loc, scalar_value, result_type);
            if (broadcast) {
                return broadcast;
            }
        }
        return scalar_value;
    }

    ASR::expr_t* expr_value = ASRUtils::expr_value(value);
    if (expr_value && expr_value != value) {
        value = expr_value;
    }

    Vec<ASR::expr_t*> elements;
    if (ASR::is_a<ASR::ArrayConstant_t>(*value)) {
        ASR::ArrayConstant_t* array_constant =
            ASR::down_cast<ASR::ArrayConstant_t>(value);
        if (!ASR::is_a<ASR::StructType_t>(
                *ASRUtils::type_get_past_array(array_constant->m_type))) {
            return nullptr;
        }
        int64_t size = ASRUtils::get_fixed_size_of_array(array_constant->m_type);
        if (size < 0) {
            return nullptr;
        }
        elements.reserve(al, size);
        for (int64_t i = 0; i < size; i++) {
            ASR::expr_t* element =
                ASRUtils::fetch_ArrayConstant_value(al, array_constant, i);
            ASR::expr_t* member_value =
                get_struct_member_value_from_constant(element, member);
            if (member_value == nullptr) {
                return nullptr;
            }
            elements.push_back(al, member_value);
        }
    } else if (ASR::is_a<ASR::ArrayConstructor_t>(*value)) {
        ASR::ArrayConstructor_t* array_constructor =
            ASR::down_cast<ASR::ArrayConstructor_t>(value);
        elements.reserve(al, array_constructor->n_args);
        for (size_t i = 0; i < array_constructor->n_args; i++) {
            ASR::expr_t* element =
                ASRUtils::expr_value(array_constructor->m_args[i]);
            if (element == nullptr) {
                element = array_constructor->m_args[i];
            }
            ASR::expr_t* member_value =
                get_struct_member_value_from_constant(element, member);
            if (member_value == nullptr) {
                return nullptr;
            }
            elements.push_back(al, member_value);
        }
    } else {
        return nullptr;
    }

    if (!ASRUtils::is_array(result_type)) {
        return nullptr;
    }
    return ASRUtils::EXPR(ASRUtils::make_ArrayConstructor_t_util(al, loc,
        elements.p, elements.size(), result_type,
        ASR::arraystorageType::ColMajor));
}

ASR::expr_t* designator_constant_value(Allocator& al, ASR::expr_t* expr) {
    ASR::expr_t* value = ASRUtils::expr_value(expr);
    if (value != nullptr) {
        return value;
    }
    if (ASR::is_a<ASR::ArrayItem_t>(*expr)) {
        ASR::ArrayItem_t* item = ASR::down_cast<ASR::ArrayItem_t>(expr);
        return fold_ArrayItem_value(al, item->m_v, item->m_args, item->n_args);
    }
    if (ASR::is_a<ASR::StructInstanceMember_t>(*expr)) {
        ASR::StructInstanceMember_t* member =
            ASR::down_cast<ASR::StructInstanceMember_t>(expr);
        if (!ASR::is_a<ASR::Variable_t>(
                *ASRUtils::symbol_get_past_external(member->m_m))) {
            return nullptr;
        }
        return get_struct_member_value_from_constant_array(al, expr->base.loc,
            designator_constant_value(al, member->m_v), member->m_m,
            member->m_type);
    }
    return nullptr;
}

ASR::asr_t* getStructInstanceMember_t(Allocator& al, const Location& loc,
                            ASR::asr_t* v_var, ASR::symbol_t *v,
                            ASR::symbol_t* member, SymbolTable* current_scope) {
    member = ASRUtils::symbol_get_past_external(member);
    if (ASR::is_a<ASR::Struct_t>(*member)) {
        ASR::Struct_t* member_variable = ASR::down_cast<ASR::Struct_t>(member);
        ASR::symbol_t *mem_es = nullptr;
        std::string mem_name = "1_" + std::string(member_variable->m_name) +
            "_" + std::string(ASRUtils::symbol_name(member));
        if (current_scope->resolve_symbol(mem_name)) {
            mem_es = current_scope->resolve_symbol(mem_name);
        } else {
            mem_es = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(al,
                member->base.loc, current_scope, s2c(al, mem_name), member,
                ASRUtils::symbol_name(ASRUtils::get_asr_owner(member)),
                nullptr, 0, member_variable->m_name, ASR::accessType::Public));
            current_scope->add_symbol(mem_name, mem_es);
        }
        ASR::ttype_t* member_type = ASRUtils::make_StructType_t_util(al,
            member_variable->base.base.loc, mem_es, true);
        return ASR::make_StructInstanceMember_t(al, loc, ASRUtils::EXPR(v_var),
            mem_es, member_type, nullptr);
    } else {
        LCOMPILERS_ASSERT(ASR::is_a<ASR::Variable_t>(*member));
        ASR::Variable_t* member_variable = ASR::down_cast<ASR::Variable_t>(member);
        ASR::ttype_t* member_type = ASRUtils::type_get_past_pointer(
            ASRUtils::type_get_past_allocatable(member_variable->m_type));
        ASR::ttype_t* member_type_ = ASRUtils::type_get_past_array(member_type);
        ASR::dimension_t* m_dims = nullptr;
        size_t n_dims = ASRUtils::extract_dimensions_from_ttype(member_type, m_dims);

        if( ASR::is_a<ASR::Allocatable_t>(*member_variable->m_type) ) {
            member_type = ASRUtils::TYPE(ASRUtils::make_Allocatable_t_util(al,
            member_variable->base.base.loc, member_type));
        } else if( ASR::is_a<ASR::Pointer_t>(*member_variable->m_type) ) {
            member_type = ASRUtils::TYPE(ASR::make_Pointer_t(al,
            member_variable->base.base.loc, member_type));
        }
        member_type = ASRUtils::import_procedure_type(al, member_type, current_scope);

        if (ASR::is_a<ASR::ArrayItem_t>(*ASRUtils::EXPR(v_var))) {
            ASR::ArrayItem_t *t = ASR::down_cast<ASR::ArrayItem_t>(ASRUtils::EXPR(v_var));
            if (ASRUtils::is_array_indexed_with_array_indices(t->m_args, t->n_args)) {
                n_dims = ASRUtils::extract_dimensions_from_ttype(
                    ASRUtils::type_get_past_allocatable_pointer(
                        ASRUtils::expr_type(ASRUtils::EXPR(v_var))), m_dims);
                member_type = ASRUtils::make_Array_t_util(al, loc, member_type_, m_dims, n_dims);
            }
        }
        ASR::symbol_t* base_symbol = v ? ASRUtils::symbol_get_past_external(v) : nullptr;
        bool base_is_parameter = base_symbol != nullptr &&
            ASR::is_a<ASR::Variable_t>(*base_symbol) &&
            ASR::down_cast<ASR::Variable_t>(base_symbol)->m_storage ==
                ASR::storage_typeType::Parameter;
        ASR::ttype_t* base_expr_type = ASRUtils::expr_type(ASRUtils::EXPR(v_var));
        if (base_is_parameter && ASRUtils::is_array(base_expr_type) &&
                !ASRUtils::is_array(member_type)) {
            n_dims = ASRUtils::extract_dimensions_from_ttype(base_expr_type, m_dims);
            Vec<ASR::dimension_t> m_dims_vec;
            m_dims_vec.from_pointer_n(m_dims, n_dims);
            member_type = ASRUtils::duplicate_type(al, member_type, &m_dims_vec);
        }

        ASR::symbol_t* member_ext = ASRUtils::import_struct_instance_member(al, member, current_scope);
        ASR::expr_t* value = nullptr;
        ASR::expr_t* base_value = base_is_parameter ?
            designator_constant_value(al, ASRUtils::EXPR(v_var)) : nullptr;
        if (base_is_parameter && base_value != nullptr) {
            value = get_struct_member_value_from_constant_array(
                al, loc, base_value, member, member_type);
        }
        v = base_symbol;
        if (base_is_parameter) {
            ASR::Variable_t *v_variable_s = ASR::down_cast<ASR::Variable_t>(v);
            ASR::expr_t* parameter_value = v_variable_s->m_value ?
                v_variable_s->m_value : v_variable_s->m_symbolic_value;
            if (value == nullptr) {
                ASR::expr_t* init_value = get_struct_member_value_from_constant_array(
                    al, loc, parameter_value, member, member_type);
                if (init_value != nullptr) {
                    value = init_value;
                }
            }
            if (value == nullptr && member_variable->m_symbolic_value != nullptr) {
                value = expr_value(member_variable->m_symbolic_value);
            }
            if (value != nullptr && ASRUtils::is_array(ASRUtils::expr_type(value))
                    && ASRUtils::is_character(*ASRUtils::expr_type(value))) {
                value = nullptr;
            }
        }
        value = import_folded_constant_value(al, value, current_scope);
        return ASR::make_StructInstanceMember_t(al, loc, ASRUtils::EXPR(v_var),
            member_ext, member_type, value);
    }
}

// Whether an operand of a defined binary operator can be passed to the
// matching dummy argument of one specific function of that operator.
static bool operand_matches_dummy(ASR::expr_t* operand, ASR::expr_t* dummy,
        bool is_elemental) {
    ASR::ttype_t* operand_type = ASRUtils::expr_type(operand);
    ASR::ttype_t* dummy_type = ASRUtils::expr_type(dummy);
    // A class(*) dummy takes an operand of any type and rank.
    bool any_type = ASRUtils::is_unlimited_polymorphic_type(dummy_type);

    // An allocatable or pointer dummy needs the same attribute on the operand.
    if ((ASRUtils::is_allocatable(dummy_type) && !ASRUtils::is_allocatable(operand_type)) ||
        (ASRUtils::is_pointer(dummy_type) && !ASRUtils::is_pointer(operand_type))) {
        return false;
    }
    operand_type = ASRUtils::type_get_past_allocatable_pointer(operand_type);
    dummy_type = ASRUtils::type_get_past_allocatable_pointer(dummy_type);

    // Arrays match arrays and scalars match scalars, unless the function is elemental.
    if (!any_type && !is_elemental &&
        ASR::is_a<ASR::Array_t>(*operand_type) != ASR::is_a<ASR::Array_t>(*dummy_type)) {
        return false;
    }
    operand_type = ASRUtils::type_get_past_array(operand_type);
    dummy_type = ASRUtils::type_get_past_array(dummy_type);

    // The element types are of the same category (integer, real, ...).
    if (!any_type && operand_type->type != dummy_type->type) {
        return false;
    }

    // A derived type matches a dummy of the same type or of a type it extends.
    if (ASR::is_a<ASR::StructType_t>(*operand_type) &&
        ASR::is_a<ASR::StructType_t>(*dummy_type)) {
        ASR::Struct_t* operand_struct = ASR::down_cast<ASR::Struct_t>(
            ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(operand)));
        ASR::Struct_t* dummy_struct = ASR::down_cast<ASR::Struct_t>(
            ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(dummy)));
        return can_pass_derviedtype_arg_to_parameter(operand_struct, dummy_struct);
    }

    // Any other type is equal, including the kind and the deferred type of a template.
    return check_equal_type(operand_type, dummy_type, operand, dummy);
}

bool use_overloaded(ASR::expr_t* left, ASR::expr_t* right,
    ASR::binopType op, std::string& intrinsic_op_name,
    SymbolTable* curr_scope, ASR::asr_t*& asr,
    Allocator &al, const Location& loc,
    SetChar& current_function_dependencies,
    SetChar& current_module_dependencies,
    const std::function<void (const std::string &, const Location &)> err) {
    ASR::ttype_t *left_type = ASRUtils::expr_type(left);
    ASR::ttype_t *right_type = ASRUtils::expr_type(right);
    ASR::Struct_t *left_struct = nullptr, *right_struct = nullptr;

    if (ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(left_type))) {
        left_struct = ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(left)));
    }
    if (ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(right_type))) {
        right_struct = ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(right)));
    }
    bool found = false;
    bool found_on_right = false;
    bool left_has_op = is_op_overloaded(op, intrinsic_op_name, curr_scope, left_struct);
    bool right_has_op = right_struct != nullptr &&
                        is_op_overloaded(op, intrinsic_op_name, curr_scope, right_struct);
    if (!left_has_op && right_has_op) {
        found_on_right = true;
    }
    ASR::Struct_t* op_struct = found_on_right ? right_struct : left_struct;
    (void)op_struct;
    if (left_has_op || right_has_op) {
        ASR::symbol_t* sym = curr_scope->resolve_symbol(intrinsic_op_name);
        ASR::symbol_t* orig_sym = ASRUtils::symbol_get_past_external(sym);
        auto find_struct_op_sym = [&](ASR::Struct_t* s) -> ASR::symbol_t* {
            ASR::symbol_t* result = nullptr;
            ASR::Struct_t* temp_struct = s;
            while (temp_struct) {
                if (temp_struct->m_symtab->get_symbol(intrinsic_op_name) != nullptr) {
                    result = temp_struct->m_symtab->get_symbol(intrinsic_op_name);
                    break;
                } else if (temp_struct->m_parent) {
                    temp_struct = ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(temp_struct->m_parent));
                } else {
                    temp_struct = nullptr;
                }
            }
            return result;
        };
        ASR::symbol_t* orig_sym2 = nullptr;
        ASR::symbol_t* orig_sym2_right = nullptr;
        if (left_has_op && left_struct != nullptr) {
            orig_sym2 = find_struct_op_sym(left_struct);
        }
        if (right_has_op && right_struct != nullptr) {
            orig_sym2_right = find_struct_op_sym(right_struct);
        }
        Vec<ASR::symbol_t*> op_overloading_procs; op_overloading_procs.reserve(al, 1);
        size_t n_interface_proc = 0;
        size_t n_left_struct_end = 0;
        // functions that are declared by interface operator(+)
        if (orig_sym) {
            ASR::CustomOperator_t* gen_proc = ASR::down_cast<ASR::CustomOperator_t>(orig_sym);
            for( size_t i = 0; i < gen_proc->n_procs; i++ ) {
                op_overloading_procs.push_back(al, gen_proc->m_procs[i]);
            }
            n_interface_proc = gen_proc->n_procs;
        }
        // functions that are type-bound on the left operand's type
        if (orig_sym2) {
            ASR::CustomOperator_t* gen_proc = ASR::down_cast<ASR::CustomOperator_t>(orig_sym2);
            for( size_t i = 0; i < gen_proc->n_procs; i++ ) {
                op_overloading_procs.push_back(al, gen_proc->m_procs[i]);
            }
        }
        n_left_struct_end = op_overloading_procs.size();
        // functions that are type-bound on the right operand's type
        if (orig_sym2_right) {
            ASR::CustomOperator_t* gen_proc = ASR::down_cast<ASR::CustomOperator_t>(orig_sym2_right);
            for( size_t i = 0; i < gen_proc->n_procs; i++ ) {
                op_overloading_procs.push_back(al, gen_proc->m_procs[i]);
            }
        }
        for( size_t i = 0; i < op_overloading_procs.size() && !found; i++ ) {
            bool proc_from_right_struct = (i >= n_left_struct_end);
            ASR::Struct_t* proc_op_struct = proc_from_right_struct ? right_struct : left_struct;
            // For the call-site bookkeeping below, treat as found_on_right
            // when the procedure is type-bound on the right operand or when
            // there is no left-side operator at all.
            bool iter_found_on_right = proc_from_right_struct || found_on_right;
            ASR::symbol_t* proc;
            ASR::symbol_t* orig_proc = ASRUtils::symbol_get_past_external(op_overloading_procs[i]);
            if ( ASR::is_a<ASR::StructMethodDeclaration_t>(*op_overloading_procs[i]) ) {
                ASR::StructMethodDeclaration_t* cp = ASR::down_cast<ASR::StructMethodDeclaration_t>(op_overloading_procs[i]);
                if (proc_op_struct != nullptr && cp->m_parent_symtab->get_counter() != proc_op_struct->m_symtab->get_counter()) {
                    ASR::Struct_t* temp_struct = proc_op_struct;
                    while (temp_struct->m_parent) {
                        if (temp_struct->m_symtab->get_symbol(cp->m_name) != nullptr) {
                            cp = ASR::down_cast<ASR::StructMethodDeclaration_t>(temp_struct->m_symtab->get_symbol(cp->m_name));
                            orig_proc = temp_struct->m_symtab->get_symbol(cp->m_name);
                            break;
                        }
                        temp_struct = ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(temp_struct->m_parent));
                    }
                }
                proc =  ASRUtils::symbol_get_past_external(cp->m_proc);
            } else {
                proc = ASRUtils::symbol_get_past_external(op_overloading_procs[i]);
            }
            if( (void*) proc == (void*) curr_scope->asr_owner ) {
                continue ;
            }
            switch(proc->type) {
                case ASR::symbolType::Function: {
                    ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(proc);
                    bool is_elemental = ASRUtils::is_elemental(proc);
                    std::string matched_func_name = "";
                    if( func->n_args == 2 ) {
                        if (operand_matches_dummy(left, func->m_args[0], is_elemental) &&
                            operand_matches_dummy(right, func->m_args[1], is_elemental)) {
                            found = true;
                            Vec<ASR::call_arg_t> a_args;
                            a_args.reserve(al, 1);
                            ASR::call_arg_t left_call_arg, right_call_arg;
                            ASR::expr_t* self_arg = nullptr;
                            if (i < n_interface_proc) {
                                left_call_arg.loc = left->base.loc, left_call_arg.m_value = left;
                                a_args.push_back(al, left_call_arg);
                                right_call_arg.loc = right->base.loc, right_call_arg.m_value = right;
                                a_args.push_back(al, right_call_arg);
                            } else if (iter_found_on_right) {
                                // Operator is a type-bound procedure on the right
                                // operand's type. Include both operands as regular
                                // call arguments (not as self_arg) so that argument
                                // order matches the function's formal parameters.
                                left_call_arg.loc = left->base.loc, left_call_arg.m_value = left;
                                a_args.push_back(al, left_call_arg);
                                right_call_arg.loc = right->base.loc, right_call_arg.m_value = right;
                                a_args.push_back(al, right_call_arg);
                            } else {
                                right_call_arg.loc = right->base.loc, right_call_arg.m_value = right;
                                a_args.push_back(al, right_call_arg);
                            }
                            ASR::symbol_t* a_name = nullptr;
                            if (i < n_interface_proc && !is_type_bound_generic(sym)) {
                                a_name = op_overloading_procs[i];
                            } else if (i >= n_interface_proc && !iter_found_on_right) {
                                a_name = import_class_procedure(al, loc, orig_proc, curr_scope);
                                self_arg = left;
                            } else {
                                a_name = import_type_bound_specific(al, curr_scope, proc,
                                    intrinsic_op_name);
                            }
                            matched_func_name = ASRUtils::symbol_name(a_name);
                            ASR::ttype_t *return_type = nullptr;
                            // The passed object of a type-bound operator is not
                            // in a_args, so the shape comes from both operands.
                            Vec<ASR::call_arg_t> operands;
                            operands.reserve(al, 2);
                            operands.push_back(al, ASR::call_arg_t{ left->base.loc, left });
                            operands.push_back(al, ASR::call_arg_t{ right->base.loc, right });
                            ASR::expr_t* first_array_arg =
                                ASRUtils::find_first_array_arg_if_elemental(func, operands);
                            if( first_array_arg != nullptr ) {
                                ASR::dimension_t* array_dims;
                                size_t array_n_dims = ASRUtils::extract_dimensions_from_ttype(
                                ASRUtils::expr_type(first_array_arg), array_dims);
                                Vec<ASR::dimension_t> new_dims;
                                new_dims.from_pointer_n_copy(al, array_dims, array_n_dims);
                                return_type = ASRUtils::duplicate_type(al,
                                                ASRUtils::get_FunctionType(func)->m_return_var_type,
                                                &new_dims);
                            } else {
                                return_type = ASRUtils::expr_type(func->m_return_var);
                            }
                            if (ASRUtils::symbol_parent_symtab(a_name)->get_counter() != curr_scope->get_counter()) {
                                ADD_ASR_DEPENDENCIES_WITH_NAME(curr_scope, a_name, current_function_dependencies, s2c(al, matched_func_name));
                            }
                            ASRUtils::insert_module_dependency(a_name, al, current_module_dependencies);
                            ASRUtils::set_absent_optional_arguments_to_null(a_args, func, al, self_arg);
                            ASR::call_arg_t* call_args1 = a_args.p;
                            size_t n_call_args1 = a_args.n;
                            ASRUtils::insert_self_arg(al, a_name, call_args1, n_call_args1, self_arg);
                            {
                                ASR::FunctionType_t* ftype = ASRUtils::get_FunctionType(func);
                                // An elemental reference with an array argument
                                // has the shape of the argument, set above.
                                if (ftype->m_return_var_type && first_array_arg == nullptr) {
                                    return_type = ASRUtils::duplicate_type(al, ftype->m_return_var_type);
                                    ASR::dimension_t* ret_dims = nullptr;
                                    size_t n_ret_dims = ASRUtils::extract_dimensions_from_ttype(return_type, ret_dims);
                                    if (n_ret_dims > 0) {
                                       // Keep whichever 'r' initialization is currently there, for example:
                                       ReplaceFunctionParamWithArg r(al, a_args.p, a_args.n); // OR call_args1, n_call_args1

                                       // 1. Initialize the caller's duplicator OUTSIDE the loop
                                       ASRUtils::ExprStmtDuplicator caller_dup(al);
                                       caller_dup.allow_procedure_calls = true;
                                       caller_dup.success = true;

                                       for (size_t di = 0; di < n_ret_dims; di++) {
                                          if (ret_dims[di].m_length) {
                                          // 2. Extract and safely duplicate m_length
                                          ASR::expr_t* replaced_length = r.replace_FunctionParam_with_arg(ret_dims[di].m_length);
                                          ret_dims[di].m_length = (replaced_length == ret_dims[di].m_length) ? caller_dup.duplicate_expr(replaced_length) : replaced_length;
                                           }
        
                                          if (ret_dims[di].m_start) {
                                          // 3. Extract and safely duplicate m_start
                                          ASR::expr_t* replaced_start = r.replace_FunctionParam_with_arg(ret_dims[di].m_start);
                                          ret_dims[di].m_start = (replaced_start == ret_dims[di].m_start) ? caller_dup.duplicate_expr(replaced_start) : replaced_start;
                                           }
                                        }
                                    }
                                }
                            }
                            asr = ASRUtils::make_FunctionCall_t_util(al, loc, a_name, sym,
                                                            call_args1, n_call_args1,
                                                            return_type,
                                                            nullptr, self_arg
                                                            );
                        }
                    }
                    break;
                }
                default: {
                    err("While overloading binary operators only functions can be used",
                                        proc->base.loc);
                }
            }
        }
    }
    return found;
}

void process_overloaded_unary_function(ASR::symbol_t* proc, ASR::expr_t* operand,
    ASR::ttype_t* operand_type, const std::string& op_name, bool& found, Allocator& al, SymbolTable* curr_scope,
    const Location& loc, SetChar& current_function_dependencies,
    SetChar& current_module_dependencies, ASR::asr_t*& asr,
    bool elemental_pass,
    const std::function<void (const std::string &, const Location &)> /*err*/,
    ASR::symbol_t* specific=nullptr) {
    ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(proc);
    std::string matched_func_name = "";
    if( func->n_args == 1 ) {
        ASR::ttype_t* operand_arg_type = ASRUtils::expr_type(func->m_args[0]);
        // An elemental specific applies to each element of an array operand;
        // any other specific needs a dummy of the operand's rank.
        bool is_elemental_call = ASRUtils::get_FunctionType(func)->m_elemental &&
            ASRUtils::is_array(operand_type);
        if (is_elemental_call != elemental_pass) {
            return;
        }
        bool rank_matches = is_elemental_call ||
            ASRUtils::is_assumed_rank_array(operand_arg_type) ||
            ASRUtils::extract_n_dims_from_ttype(operand_arg_type) ==
                ASRUtils::extract_n_dims_from_ttype(operand_type);
        if (rank_matches && ASRUtils::check_equal_type(operand_arg_type,
                operand_type, func->m_args[0], operand)) {
            found = true;
            Vec<ASR::call_arg_t> a_args;
            a_args.reserve(al, 1);
            ASR::call_arg_t operand_call_arg;
            operand_call_arg.loc = operand->base.loc;
            operand_call_arg.m_value = operand;
            a_args.push_back(al, operand_call_arg);
            ASR::symbol_t* a_name = specific != nullptr
                ? specific : import_type_bound_specific(al, curr_scope, proc, op_name);
            matched_func_name = ASRUtils::symbol_name(a_name);
            ASR::ttype_t *return_type = nullptr;
            ASR::expr_t* first_array_arg =
                ASRUtils::find_first_array_arg_if_elemental(func, a_args);
            if( first_array_arg != nullptr ) {
                ASR::dimension_t* array_dims;
                size_t array_n_dims = ASRUtils::extract_dimensions_from_ttype(
                    ASRUtils::expr_type(first_array_arg), array_dims);
                Vec<ASR::dimension_t> new_dims;
                new_dims.from_pointer_n_copy(al, array_dims, array_n_dims);
                return_type = ASRUtils::duplicate_type(al,
                                ASRUtils::get_FunctionType(func)->m_return_var_type,
                                &new_dims);
            } else {
                return_type = ASRUtils::expr_type(func->m_return_var);
                bool is_array = ASRUtils::is_array(return_type);
                ASR::dimension_t* m_dims = nullptr;
                size_t n_dims = ASRUtils::extract_dimensions_from_ttype(return_type, m_dims);
                return_type = ASRUtils::type_get_past_array(return_type);
                if( ASR::is_a<ASR::StructType_t>(*return_type) ) {
                    return_type = ASRUtils::duplicate_type(al, return_type);
                    if( is_array ) {
                        return_type = ASRUtils::make_Array_t_util(
                            al, loc, return_type, m_dims, n_dims);
                    }
                }
            }
            if (ASRUtils::symbol_parent_symtab(a_name)->get_counter() != curr_scope->get_counter()) {
                ADD_ASR_DEPENDENCIES_WITH_NAME(curr_scope, a_name, current_function_dependencies, s2c(al, matched_func_name));
            }
            ASRUtils::insert_module_dependency(a_name, al, current_module_dependencies);
            ASRUtils::set_absent_optional_arguments_to_null(a_args, func, al);
            {
                ASR::FunctionType_t* ftype = ASRUtils::get_FunctionType(func);
                // An elemental reference with an array argument
                // has the shape of the argument, set above.
                if (ftype->m_return_var_type && first_array_arg == nullptr) {
                    return_type = ASRUtils::duplicate_type(al, ftype->m_return_var_type);
                    ASR::dimension_t* ret_dims = nullptr;
                    size_t n_ret_dims = ASRUtils::extract_dimensions_from_ttype(return_type, ret_dims);
                    if (n_ret_dims > 0) {
                       // Keep whichever 'r' initialization is currently there, for example:
                       ReplaceFunctionParamWithArg r(al, a_args.p, a_args.n); // OR call_args1, n_call_args1

                       // 1. Initialize the caller's duplicator OUTSIDE the loop
                       ASRUtils::ExprStmtDuplicator caller_dup(al);
                       caller_dup.allow_procedure_calls = true;
                       caller_dup.success = true;

                       for (size_t di = 0; di < n_ret_dims; di++) {
                           if (ret_dims[di].m_length) {
                              // 2. Extract and safely duplicate m_length
                              ASR::expr_t* replaced_length = r.replace_FunctionParam_with_arg(ret_dims[di].m_length);
                              ret_dims[di].m_length = (replaced_length == ret_dims[di].m_length) ? caller_dup.duplicate_expr(replaced_length) : replaced_length;
                            }
        
                            if (ret_dims[di].m_start) {
                               // 3. Extract and safely duplicate m_start
                               ASR::expr_t* replaced_start = r.replace_FunctionParam_with_arg(ret_dims[di].m_start);
                               ret_dims[di].m_start = (replaced_start == ret_dims[di].m_start) ? caller_dup.duplicate_expr(replaced_start) : replaced_start;
                            }
                        }
                    }
                }
            }
            ASR::symbol_t* original_name = a_name;
            asr = ASRUtils::make_FunctionCall_t_util(al, loc, a_name, original_name,
                                            a_args.p, 1,
                                            return_type,
                                            nullptr, nullptr
                                            );
        }
    }
}

bool use_overloaded_unary_op(ASR::expr_t* operand, const std::string& op_name,
    SymbolTable* curr_scope, ASR::asr_t*& asr,
    Allocator &al, const Location& loc, SetChar& current_function_dependencies,
    SetChar& current_module_dependencies,
    const std::function<void (const std::string &, const Location &)> err) {
    ASR::ttype_t *operand_type = ASRUtils::type_get_past_allocatable_pointer(
        ASRUtils::expr_type(operand));
    ASR::ttype_t *element_type = ASRUtils::type_get_past_array(operand_type);
    ASR::symbol_t* sym = curr_scope->resolve_symbol(op_name);

    if (!sym) {
        if (ASR::is_a<ASR::StructType_t>(*element_type)) {
            ASR::symbol_t* struct_t_sym = ASRUtils::symbol_get_past_external(
                ASRUtils::get_struct_sym_from_struct_expr(operand));
            if (ASR::is_a<ASR::Struct_t>(*struct_t_sym)) {
                ASR::Struct_t* struct_type_t = ASR::down_cast<ASR::Struct_t>(struct_t_sym);
                sym = struct_type_t->m_symtab->resolve_symbol(op_name);
                while (sym == nullptr && struct_type_t->m_parent != nullptr) {
                    struct_type_t = ASR::down_cast<ASR::Struct_t>(
                        ASRUtils::symbol_get_past_external(struct_type_t->m_parent));
                    sym = struct_type_t->m_symtab->resolve_symbol(op_name);
                }
                if (sym == nullptr) {
                    return false;
                }
                sym = ASR::down_cast<ASR::symbol_t>(
                    ASR::make_ExternalSymbol_t(al,
                                               loc,
                                               curr_scope,
                                               s2c(al, op_name),
                                               sym,
                                               struct_type_t->m_name,
                                               nullptr,
                                               0,
                                               s2c(al, op_name),
                                               ASR::accessType::Public));
                curr_scope->add_symbol(op_name, sym);
            } else {
                LCOMPILERS_ASSERT(false);
            }
        } else {
            return false;
        }
    }

    ASR::symbol_t* orig_sym = ASRUtils::symbol_get_past_external(sym);
    ASR::CustomOperator_t* gen_proc = ASR::down_cast<ASR::CustomOperator_t>(orig_sym);
    ASR::CustomOperator_t* struct_gen = nullptr;
    if (ASR::is_a<ASR::StructType_t>(*element_type) &&
            !ASRUtils::is_class_type(element_type)) {
        ASR::symbol_t* struct_t_sym = ASRUtils::symbol_get_past_external(
            ASRUtils::get_struct_sym_from_struct_expr(operand));
        if (ASR::is_a<ASR::Struct_t>(*struct_t_sym)) {
            ASR::Struct_t* struct_type_t = ASR::down_cast<ASR::Struct_t>(struct_t_sym);
            ASR::symbol_t* struct_sub = struct_type_t->m_symtab->resolve_symbol(op_name);
            while (struct_sub == nullptr && struct_type_t->m_parent != nullptr) {
                struct_type_t = ASR::down_cast<ASR::Struct_t>(
                    ASRUtils::symbol_get_past_external(struct_type_t->m_parent));
                struct_sub = struct_type_t->m_symtab->resolve_symbol(op_name);
            }
            if (struct_sub != nullptr) {
                ASR::symbol_t* struct_orig = ASRUtils::symbol_get_past_external(struct_sub);
                if (ASR::is_a<ASR::CustomOperator_t>(*struct_orig)) {
                    struct_gen = ASR::down_cast<ASR::CustomOperator_t>(struct_orig);
                }
            }
        }
    }

    bool found = false;
    // A matching nonelemental specific takes priority over an elemental one
    // (F2018 15.5.5.2), so the elemental specifics are only tried afterwards.
    for (bool elemental_pass : {false, true}) {
        for( size_t i = 0; i < gen_proc->n_procs && !found; i++ ) {
            ASR::symbol_t* proc = ASRUtils::symbol_get_past_external(gen_proc->m_procs[i]);
            switch(proc->type) {
                case ASR::symbolType::Function: {
                    process_overloaded_unary_function(proc, operand, operand_type, op_name,
                        found, al, curr_scope, loc, current_function_dependencies,
                        current_module_dependencies, asr, elemental_pass, err,
                        is_type_bound_generic(sym) ? nullptr : gen_proc->m_procs[i]);
                    break;
                }
                case ASR::symbolType::StructMethodDeclaration: {
                    ASR::StructMethodDeclaration_t* class_procedure_t = ASR::down_cast<ASR::StructMethodDeclaration_t>(proc);
                    process_overloaded_unary_function(
                        ASRUtils::symbol_get_past_external(class_procedure_t->m_proc),
                        operand, operand_type, op_name, found, al, curr_scope, loc,
                        current_function_dependencies, current_module_dependencies, asr,
                        elemental_pass, err);
                    break;
                }
                default: {
                    if (!elemental_pass) {
                        err("While overloading binary operators only functions can be used",
                                            proc->base.loc);
                    }
                }
            }
        }
        for (size_t i = 0; struct_gen && i < struct_gen->n_procs && !found; i++) {
            ASR::symbol_t* proc = struct_gen->m_procs[i];
            switch(proc->type) {
                case ASR::symbolType::Function: {
                    process_overloaded_unary_function(proc, operand, operand_type, op_name,
                        found, al, curr_scope, loc, current_function_dependencies,
                        current_module_dependencies, asr, elemental_pass, err);
                    break;
                }
                case ASR::symbolType::StructMethodDeclaration: {
                    ASR::StructMethodDeclaration_t* class_procedure_t =
                        ASR::down_cast<ASR::StructMethodDeclaration_t>(proc);
                    process_overloaded_unary_function(
                        ASRUtils::symbol_get_past_external(class_procedure_t->m_proc),
                        operand, operand_type, op_name, found, al, curr_scope, loc,
                        current_function_dependencies, current_module_dependencies, asr,
                        elemental_pass, err);
                    break;
                }
                default:
                    break;
            }
        }
        if (found) {
            break;
        }
    }
    return found;
}

bool is_op_overloaded(ASR::binopType op, std::string& intrinsic_op_name,
                      SymbolTable* curr_scope, ASR::Struct_t* left_struct) {
    bool result = true;
    switch(op) {
        case ASR::binopType::Add: {
            if(intrinsic_op_name != "~add") {
                result = false;
            }
            break;
        }
        case ASR::binopType::Sub: {
            if(intrinsic_op_name != "~sub") {
                result = false;
            }
            break;
        }
        case ASR::binopType::Mul: {
            if(intrinsic_op_name != "~mul") {
                result = false;
            }
            break;
        }
        case ASR::binopType::Div: {
            if(intrinsic_op_name != "~div") {
                result = false;
            }
            break;
        }
        case ASR::binopType::Pow: {
            if(intrinsic_op_name != "~pow") {
                result = false;
            }
            break;
        }
        default: {
            throw LCompilersException("Binary operator '" + ASRUtils::binop_to_str_python(op) + "' not supported yet");
        }
    }

    if( result && curr_scope->resolve_symbol(intrinsic_op_name) == nullptr ) {
        if ( left_struct != nullptr ) {
            ASR::Struct_t* temp_struct = left_struct;
            result = false;
            while (temp_struct) {
                if (temp_struct->m_symtab->resolve_symbol(intrinsic_op_name) != nullptr) {
                    result = true;
                    break;
                } else if (temp_struct->m_parent) {
                    temp_struct = ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(temp_struct->m_parent));
                } else {
                    temp_struct = nullptr;
                }
            }
        } else {
            result = false;
        }
    }
    return result;
}

void process_overloaded_assignment_function(ASR::symbol_t* proc, ASR::expr_t* target, ASR::expr_t* value,
    ASR::ttype_t* target_type, ASR::ttype_t* value_type, bool& found, Allocator& al, const Location& target_loc,
    const Location& value_loc, SymbolTable* curr_scope, SetChar& current_function_dependencies,
    SetChar& current_module_dependencies, ASR::asr_t*& asr, ASR::symbol_t* sym, const Location& loc, ASR::expr_t* expr_dt,
    const std::function<void (const std::string &, const Location &)> err, char* pass_arg=nullptr,
    ASR::symbol_t* specific=nullptr) {
    ASR::Function_t* subrout = ASR::down_cast<ASR::Function_t>(proc);
    std::string matched_subrout_name = "";
    if( subrout->n_args == 2 ) {
        ASR::ttype_t* target_arg_type = ASRUtils::expr_type(subrout->m_args[0]);
        ASR::ttype_t* value_arg_type = ASRUtils::expr_type(subrout->m_args[1]);
        // F2023 10.2.1.4 / 15.5.2.4: non-elemental defined assignment requires
        // matching ranks of actuals and dummies. types_equal peels arrays, so
        // enforce ranks explicitly unless the procedure is elemental.
        bool is_elemental = ASRUtils::is_elemental(proc);
        if (!is_elemental) {
            int target_rank = ASRUtils::extract_n_dims_from_ttype(target_type);
            int value_rank = ASRUtils::extract_n_dims_from_ttype(value_type);
            int arg0_rank = ASRUtils::extract_n_dims_from_ttype(target_arg_type);
            int arg1_rank = ASRUtils::extract_n_dims_from_ttype(value_arg_type);
            if (target_rank != arg0_rank || value_rank != arg1_rank) {
                return;
            }
        }
        if( ASRUtils::types_equal(target_arg_type, target_type, subrout->m_args[0], target) &&
            ASRUtils::types_equal(value_arg_type, value_type, subrout->m_args[1], value) ) {
            std::string arg0_name = ASRUtils::symbol_name(ASR::down_cast<ASR::Var_t>(subrout->m_args[0])->m_v);
            std::string arg1_name = ASRUtils::symbol_name(ASR::down_cast<ASR::Var_t>(subrout->m_args[1])->m_v);
            if( pass_arg != nullptr ) {
                std::string pass_arg_str = std::string(pass_arg);
                if( arg0_name != pass_arg_str && arg1_name != pass_arg_str ) {
                    err(pass_arg_str + " argument is not present in " + std::string(subrout->m_name),
                        proc->base.loc);
                }
                if( (arg0_name == pass_arg_str && target != expr_dt) ) {
                    err(std::string(subrout->m_name) + " is not a procedure of " +
                        ASRUtils::type_to_str_fortran_expr(target_type, target),
                        loc);
                }
                if( (arg1_name == pass_arg_str && value != expr_dt) ) {
                    err(std::string(subrout->m_name) + " is not a procedure of " +
                        ASRUtils::type_to_str_fortran_expr(value_type, value),
                        loc);
                }
            }
            found = true;
            Vec<ASR::call_arg_t> a_args;
            a_args.reserve(al, 2);
            ASR::call_arg_t target_arg, value_arg;
            target_arg.loc = target_loc, target_arg.m_value = target;
            a_args.push_back(al, target_arg);
            value_arg.loc = value_loc, value_arg.m_value = value;
            a_args.push_back(al, value_arg);
            std::string subrout_name = to_lower(subrout->m_name);
            ASR::symbol_t* resolved = curr_scope->resolve_symbol(subrout_name);
            ASR::symbol_t *a_name = nullptr;
            if (specific != nullptr) {
                a_name = specific;
                matched_subrout_name = ASRUtils::symbol_name(a_name);
            } else if( resolved && ASRUtils::symbol_get_past_external(resolved) == proc ) {
                matched_subrout_name = subrout_name;
            } else {
                std::string mangled_name = subrout_name + "@~assign";
                matched_subrout_name = mangled_name;
            }
            if (a_name == nullptr) {
                a_name = curr_scope->get_symbol(matched_subrout_name);
                // When another procedure took the name, use a numbered name that is free or refers to `proc`.
                std::string taken_name = matched_subrout_name;
                for( int i = 1; a_name != nullptr &&
                        ASRUtils::symbol_get_past_external(a_name) != proc; i++ ) {
                    matched_subrout_name = taken_name + std::to_string(i);
                    a_name = curr_scope->get_symbol(matched_subrout_name);
                }
            }
            ASR::symbol_t *proc_owner = ASRUtils::get_asr_owner(proc);
            if( a_name == nullptr && matched_subrout_name == subrout_name &&
                    !(proc_owner && (ASR::is_a<ASR::Module_t>(*proc_owner) ||
                                     ASR::is_a<ASR::Struct_t>(*proc_owner))) ) {
                // An ExternalSymbol can only refer into a module or a struct,
                // so a procedure host associated from another scope (such as
                // a template) is referenced through its host symbol.
                a_name = resolved;
            }
            if( a_name == nullptr ) {
                a_name = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(
                            al, loc, curr_scope, s2c(al, matched_subrout_name), proc,
                            ASRUtils::symbol_name(ASRUtils::get_asr_owner(proc)),
                            nullptr, 0, subrout->m_name, ASR::accessType::Public));
                curr_scope->add_symbol(matched_subrout_name, a_name);
            }
            if (ASRUtils::symbol_parent_symtab(a_name)->get_counter() != curr_scope->get_counter()) {
                ADD_ASR_DEPENDENCIES_WITH_NAME(curr_scope, a_name, current_function_dependencies, s2c(al, matched_subrout_name));
            }
            ASRUtils::insert_module_dependency(a_name, al, current_module_dependencies);
            ASRUtils::set_absent_optional_arguments_to_null(a_args, subrout, al);
            ASR::symbol_t* a_original_name = ASR::is_a<ASR::ExternalSymbol_t>(*a_name) ? a_name : sym;
            asr = ASRUtils::make_SubroutineCall_t_util(al, loc, a_name, a_original_name,
                                            a_args.p, 2, nullptr, nullptr, false);
        }
    }
}

ASR::symbol_t* resolve_struct_assign_symbol(ASR::Struct_t* s) {
    while (s != nullptr) {
        ASR::symbol_t* result = s->m_symtab->resolve_symbol("~assign");
        if (result != nullptr) {
            return result;
        }
        if (s->m_parent == nullptr) {
            break;
        }
        s = ASR::down_cast<ASR::Struct_t>(
            ASRUtils::symbol_get_past_external(s->m_parent));
    }
    return nullptr;
}

ASR::symbol_t* resolve_struct_defined_assignment_proc(ASR::Struct_t* s) {
    ASR::symbol_t* da_sym = resolve_struct_assign_symbol(s);
    if (da_sym == nullptr) {
        return nullptr;
    }
    da_sym = ASRUtils::symbol_get_past_external(da_sym);
    if (!ASR::is_a<ASR::CustomOperator_t>(*da_sym)) {
        return nullptr;
    }
    ASR::CustomOperator_t* custom_op = ASR::down_cast<ASR::CustomOperator_t>(da_sym);
    for (size_t ip = 0; ip < custom_op->n_procs; ip++) {
        ASR::symbol_t* assign_proc =
            ASRUtils::symbol_get_past_external(custom_op->m_procs[ip]);
        ASR::symbol_t* candidate;
        if (ASR::is_a<ASR::StructMethodDeclaration_t>(*assign_proc)) {
            candidate = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::StructMethodDeclaration_t>(
                    assign_proc)->m_proc);
        } else {
            candidate = assign_proc;
        }
        if (!ASR::is_a<ASR::Function_t>(*candidate)) {
            continue;
        }
        ASR::Function_t* cand_func = ASR::down_cast<ASR::Function_t>(candidate);
        if (cand_func->n_args < 2) {
            continue;
        }
        // Both formals must be type/class of s (type_declaration).
        auto formal_matches = [&](ASR::expr_t* arg) {
            ASR::Variable_t* var = ASRUtils::EXPR2VAR(arg);
            ASR::ttype_t* t = ASRUtils::type_get_past_array(
                ASRUtils::type_get_past_allocatable(
                    ASRUtils::type_get_past_pointer(var->m_type)));
            if (!ASR::is_a<ASR::StructType_t>(*t) ||
                    var->m_type_declaration == nullptr) {
                return false;
            }
            return ASRUtils::symbol_get_past_external(var->m_type_declaration)
                == &s->base;
        };
        if (formal_matches(cand_func->m_args[0]) &&
                formal_matches(cand_func->m_args[1])) {
            return candidate;
        }
    }
    return nullptr;
}

bool struct_assignment_is_more_than_a_copy(ASR::symbol_t* struct_sym) {
    if (struct_sym == nullptr) {
        return false;
    }
    ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(struct_sym);
    if (!ASR::is_a<ASR::Struct_t>(*sym)) {
        return false;
    }
    ASR::Struct_t* struct_t = ASR::down_cast<ASR::Struct_t>(sym);
    // The variable is finalized if finalizing it does anything: a final
    // procedure of its own type, of a type it extends (the parent component
    // is finalized, F2018 7.5.6.2), or of one of its components.
    if (struct_needs_finalization(sym)) {
        return true;
    }
    for (size_t i = 0; i < struct_t->n_members; i++) {
        ASR::symbol_t* member = struct_t->m_symtab->get_symbol(
            struct_t->m_members[i]);
        if (member == nullptr || !ASR::is_a<ASR::Variable_t>(*member)) {
            continue;
        }
        ASR::Variable_t* member_var = ASR::down_cast<ASR::Variable_t>(member);
        if (ASRUtils::is_pointer(member_var->m_type) ||
                member_var->m_type_declaration == nullptr ||
                !ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::extract_type(member_var->m_type))) {
            continue;
        }
        ASR::symbol_t* member_struct = ASRUtils::symbol_get_past_external(
            member_var->m_type_declaration);
        if (!ASR::is_a<ASR::Struct_t>(*member_struct)) {
            continue;
        }
        if (resolve_struct_defined_assignment_proc(
                ASR::down_cast<ASR::Struct_t>(member_struct)) != nullptr) {
            return true;
        }
    }
    return false;
}

// `visited` guards against a malformed cyclic type or parent chain.
static bool struct_needs_finalization_util(ASR::Struct_t* st,
        std::set<ASR::Struct_t*>& visited) {
    for (ASR::Struct_t* level = st; level != nullptr; ) {
        if (visited.find(level) != visited.end()) {
            return false;
        }
        visited.insert(level);
        if (level->n_member_functions > 0) {
            return true;
        }
        for (auto& m : level->m_symtab->get_scope()) {
            if (!ASR::is_a<ASR::Variable_t>(*m.second)) continue;
            ASR::Variable_t* m_var = ASR::down_cast<ASR::Variable_t>(m.second);
            if (ASRUtils::is_array(m_var->m_type) ||
                    ASRUtils::is_allocatable(m_var->m_type) ||
                    ASRUtils::is_pointer(m_var->m_type) ||
                    ASRUtils::is_class_type(m_var->m_type) ||
                    !ASR::is_a<ASR::StructType_t>(*m_var->m_type) ||
                    m_var->m_type_declaration == nullptr) {
                continue;
            }
            ASR::symbol_t* m_struct = ASRUtils::symbol_get_past_external(
                m_var->m_type_declaration);
            if (ASR::is_a<ASR::Struct_t>(*m_struct) &&
                    struct_needs_finalization_util(
                        ASR::down_cast<ASR::Struct_t>(m_struct), visited)) {
                return true;
            }
        }
        if (level->m_parent == nullptr) {
            break;
        }
        ASR::symbol_t* parent = ASRUtils::symbol_get_past_external(
            level->m_parent);
        level = ASR::is_a<ASR::Struct_t>(*parent)
            ? ASR::down_cast<ASR::Struct_t>(parent) : nullptr;
    }
    return false;
}

bool struct_needs_finalization(ASR::symbol_t* struct_sym) {
    if (struct_sym == nullptr) {
        return false;
    }
    ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(struct_sym);
    if (!ASR::is_a<ASR::Struct_t>(*sym)) {
        return false;
    }
    std::set<ASR::Struct_t*> visited;
    return struct_needs_finalization_util(ASR::down_cast<ASR::Struct_t>(sym),
        visited);
}

bool is_finalizable_function_result(ASR::ttype_t* type,
        ASR::symbol_t* struct_sym) {
    if (type == nullptr || ASRUtils::is_array(type) ||
            ASRUtils::is_allocatable(type) || ASRUtils::is_pointer(type) ||
            !ASR::is_a<ASR::StructType_t>(*type) ||
            ASRUtils::is_class_type(type)) {
        return false;
    }
    return struct_needs_finalization(struct_sym);
}

bool is_finalizable_function_reference(ASR::expr_t* expr) {
    if (expr == nullptr) {
        return false;
    }
    expr = ASRUtils::get_past_array_physical_cast(expr);
    if (!ASR::is_a<ASR::FunctionCall_t>(*expr)) {
        return false;
    }
    return is_finalizable_function_result(ASRUtils::expr_type(expr),
        ASRUtils::get_struct_sym_from_struct_expr(expr));
}

class ContainsFinalizableFunctionReference:
    public ASR::BaseWalkVisitor<ContainsFinalizableFunctionReference> {
public:
    bool found = false;
    void visit_expr(const ASR::expr_t &x) {
        if (found) return;
        if (is_finalizable_function_reference(const_cast<ASR::expr_t*>(&x))) {
            found = true;
            return;
        }
        ASR::BaseWalkVisitor<ContainsFinalizableFunctionReference>::visit_expr(x);
    }
    void visit_ttype(const ASR::ttype_t & /*x*/) {}
};

bool contains_finalizable_function_reference(ASR::expr_t* expr) {
    if (expr == nullptr) {
        return false;
    }
    ContainsFinalizableFunctionReference check;
    check.visit_expr(*expr);
    return check.found;
}

class ContainsFunctionReference:
    public ASR::BaseWalkVisitor<ContainsFunctionReference> {
public:
    bool found = false;
    void visit_FunctionCall(const ASR::FunctionCall_t & /*x*/) {
        found = true;
    }
    void visit_ttype(const ASR::ttype_t & /*x*/) {}
};

bool contains_function_reference(ASR::expr_t* expr) {
    if (expr == nullptr) {
        return false;
    }
    ContainsFunctionReference check;
    check.visit_expr(*expr);
    return check.found;
}

ASR::symbol_t* resolve_struct_assign_symbol(ASR::expr_t* expression) {
    ASR::symbol_t* struct_sym = ASRUtils::get_struct_sym_from_struct_expr(expression);
    if (struct_sym == nullptr) {
        return nullptr;
    }
    return resolve_struct_assign_symbol(ASR::down_cast<ASR::Struct_t>(
        ASRUtils::symbol_get_past_external(struct_sym)));
}

bool use_overloaded_assignment(ASR::expr_t* target, ASR::expr_t* value,
                               SymbolTable* curr_scope, ASR::asr_t*& asr,
                               Allocator &al, const Location& loc,
                               SetChar& current_function_dependencies,
                               SetChar& current_module_dependencies,
                               const std::function<void (const std::string &, const Location &)> err) {
    ASR::ttype_t *target_type = ASRUtils::type_get_past_allocatable(ASRUtils::expr_type(target));
    ASR::ttype_t *value_type = ASRUtils::type_get_past_allocatable(ASRUtils::expr_type(value));
    bool found = false;
    ASR::symbol_t* sym = curr_scope->resolve_symbol("~assign");
    ASR::expr_t* expr_dt = nullptr;
    if (!sym) {
        // extract_type peels array/pointer/allocatable so both scalar and
        // array-of-struct assignments share one lookup path.
        ASR::ttype_t* target_elem = ASRUtils::extract_type(target_type);
        ASR::ttype_t* value_elem = ASRUtils::extract_type(value_type);
        if (ASR::is_a<ASR::StructType_t>(*target_elem)) {
            sym = resolve_struct_assign_symbol(target);
            if (sym) {
                expr_dt = target;
            }
        } else if (ASR::is_a<ASR::StructType_t>(*value_elem)) {
            sym = resolve_struct_assign_symbol(value);
            if (sym) {
                expr_dt = value;
            }
        }
    }
    if (sym) {
        ASR::symbol_t* orig_sym = ASRUtils::symbol_get_past_external(sym);
        ASR::CustomOperator_t* gen_proc = ASR::down_cast<ASR::CustomOperator_t>(orig_sym);
        // Non-elemental specifics take precedence over elemental ones.
        std::vector<ASR::symbol_t*> procs(gen_proc->m_procs, gen_proc->m_procs + gen_proc->n_procs);
        std::stable_partition(procs.begin(), procs.end(),
            [](ASR::symbol_t* p) { return !ASRUtils::is_elemental(p); });
        for( size_t i = 0; i < procs.size() && !found; i++ ) {
            ASR::symbol_t* proc = ASRUtils::symbol_get_past_external(procs[i]);
            switch( proc->type ) {
                case ASR::symbolType::Function: {
                    process_overloaded_assignment_function(proc, target, value, target_type,
                        value_type, found, al, target->base.loc, value->base.loc, curr_scope,
                        current_function_dependencies, current_module_dependencies, asr, sym,
                        loc, expr_dt, err, nullptr,
                        is_type_bound_generic(sym) ? nullptr : procs[i]);
                    break;
                }
                case ASR::symbolType::StructMethodDeclaration: {
                    ASR::StructMethodDeclaration_t* class_proc = ASR::down_cast<ASR::StructMethodDeclaration_t>(proc);
                    ASR::symbol_t* proc_func = ASRUtils::symbol_get_past_external(class_proc->m_proc);
                    process_overloaded_assignment_function(proc_func, target, value, target_type,
                        value_type, found, al, target->base.loc, value->base.loc, curr_scope,
                        current_function_dependencies, current_module_dependencies, asr, proc_func, loc,
                        expr_dt, err, class_proc->m_self_argument);
                    if (found && expr_dt && ASRUtils::is_class_type(
                            ASRUtils::type_get_past_allocatable(ASRUtils::expr_type(expr_dt)))) {
                        // For polymorphic (class) types, create a type-bound call
                        // referencing the StructMethodDeclaration with m_dt set,
                        // so codegen can use vtable dispatch.
                        std::string method_name = std::string(class_proc->m_name) + "@~assign";
                        ASR::symbol_t* ext_sym = curr_scope->get_symbol(method_name);
                        if (ext_sym == nullptr) {
                            ext_sym = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(
                                al, loc, curr_scope, s2c(al, method_name), proc,
                                ASRUtils::symbol_name(ASRUtils::get_asr_owner(proc)),
                                nullptr, 0, class_proc->m_name, ASR::accessType::Public));
                            curr_scope->add_symbol(method_name, ext_sym);
                        }
                        if (ASRUtils::symbol_parent_symtab(ext_sym)->get_counter() != curr_scope->get_counter()) {
                            ADD_ASR_DEPENDENCIES_WITH_NAME(curr_scope, ext_sym, current_function_dependencies, s2c(al, method_name));
                        }
                        ASRUtils::insert_module_dependency(ext_sym, al, current_module_dependencies);

                        ASR::expr_t* non_pass_arg = (expr_dt == target) ? value : target;
                        Vec<ASR::call_arg_t> a_args;
                        a_args.reserve(al, 1);
                        ASR::call_arg_t arg;
                        arg.loc = non_pass_arg->base.loc;
                        arg.m_value = non_pass_arg;
                        a_args.push_back(al, arg);

                        ASR::call_arg_t* call_args = a_args.p;
                        size_t n_call_args = 1;
                        ASRUtils::insert_self_arg(al, ext_sym, call_args, n_call_args, expr_dt);
                        asr = ASRUtils::make_SubroutineCall_t_util(al, loc, ext_sym, ext_sym,
                            call_args, n_call_args, expr_dt, nullptr, false);
                    }
                    break;
                }
                default: {
                    err("Only functions and class procedures can be used for generic assignment statement, found " + std::to_string(proc->type), loc);
                }
            }
        }
    }
    return found;
}

// If `proc`, a specific procedure of the defined input/output generic
// `read_write`, accepts `item` as its dtv dummy, imports it into `curr_scope`
// (recording the dependencies a call needs) and returns the symbol a call
// names. `a_original_name` receives the call's original name. For a
// type-bound procedure, `binding` is its binding in the generic, which the
// call names.
static ASR::symbol_t* import_defined_io_procedure(const std::string &read_write,
    ASR::symbol_t* proc, ASR::expr_t* item, ASR::ttype_t* arg_type, Allocator& al,
    SymbolTable* curr_scope, SetChar& current_function_dependencies,
    SetChar& current_module_dependencies, ASR::symbol_t* sym, const Location& loc,
    ASR::expr_t* expr_dt, const std::function<void (const std::string &, const Location &)> err,
    char* pass_arg, ASR::symbol_t* binding, ASR::symbol_t*& a_original_name,
    ASR::symbol_t* specific=nullptr) {
    ASR::Function_t* subrout = ASR::down_cast<ASR::Function_t>(proc);
    std::string matched_subrout_name = "";
    ASR::ttype_t* func_arg_type = ASRUtils::expr_type(subrout->m_args[0]);
    if( !ASRUtils::types_equal(func_arg_type, arg_type, subrout->m_args[0], item) ) {
        return nullptr;
    }
    std::string arg0_name = ASRUtils::symbol_name(ASR::down_cast<ASR::Var_t>(subrout->m_args[0])->m_v);
    if( pass_arg != nullptr ) {
        std::string pass_arg_str = std::string(pass_arg);
        if( (arg0_name == pass_arg_str && item != expr_dt) ) {
            err(std::string(subrout->m_name) + " is not a procedure of " +
                ASRUtils::type_to_str_fortran_expr(arg_type, item),
                loc);
        }
    }
    ASR::symbol_t *a_name = nullptr;
    if (expr_dt != nullptr) {
        a_name = ASRUtils::import_class_procedure(al, loc, binding, curr_scope);
    } else if (specific != nullptr) {
        a_name = specific;
    } else {
        a_name = import_type_bound_specific(al, curr_scope, proc, read_write);
    }
    matched_subrout_name = ASRUtils::symbol_name(a_name);
    if (ASRUtils::symbol_parent_symtab(a_name)->get_counter() != curr_scope->get_counter()) {
        ADD_ASR_DEPENDENCIES_WITH_NAME(curr_scope, a_name, current_function_dependencies, s2c(al, matched_subrout_name));
    }
    ASRUtils::insert_module_dependency(a_name, al, current_module_dependencies);
    a_original_name = ASR::is_a<ASR::ExternalSymbol_t>(*a_name) ? a_name : sym;
    return a_name;
}

ASR::symbol_t* resolve_defined_io_procedure(std::string &read_write, ASR::expr_t* item,
                               SymbolTable* curr_scope, Allocator &al, const Location& loc,
                               SetChar& current_function_dependencies,
                               SetChar& current_module_dependencies,
                               const std::function<void (const std::string &, const Location &)> err,
                               ASR::symbol_t*& a_original_name, ASR::expr_t*& expr_dt,
                               bool elementwise) {
    ASR::ttype_t *arg_type = ASRUtils::type_get_past_allocatable_pointer(ASRUtils::expr_type(item));
    if (elementwise) {
        arg_type = ASRUtils::type_get_past_array(arg_type);
    }
    ASR::symbol_t* a_name = nullptr;
    a_original_name = nullptr;
    expr_dt = nullptr;
    // The specific procedure of the generic `sym` that takes `item`.
    auto resolve_generic = [&](ASR::symbol_t* sym) {
        ASR::symbol_t* orig_sym = ASRUtils::symbol_get_past_external(sym);
        auto process_proc_list = [&](ASR::symbol_t** procs, size_t n_procs) {
            for( size_t i = 0; i < n_procs && a_name == nullptr; i++ ) {
                ASR::symbol_t* proc = ASRUtils::symbol_get_past_external(procs[i]);
                switch( proc->type ) {
                    case ASR::symbolType::Function: {
                        a_name = import_defined_io_procedure(read_write, proc, item, arg_type,
                            al, curr_scope, current_function_dependencies,
                            current_module_dependencies, sym, loc, expr_dt, err, nullptr,
                            nullptr, a_original_name,
                            is_type_bound_generic(sym) ? nullptr : procs[i]);
                        break;
                    }
                    case ASR::symbolType::StructMethodDeclaration: {
                        ASR::StructMethodDeclaration_t* class_proc = ASR::down_cast<ASR::StructMethodDeclaration_t>(proc);
                        ASR::symbol_t* proc_func = ASRUtils::symbol_get_past_external(class_proc->m_proc);
                        a_name = import_defined_io_procedure(read_write, proc_func, item, arg_type,
                            al, curr_scope, current_function_dependencies,
                            current_module_dependencies, proc_func, loc, expr_dt, err,
                            class_proc->m_self_argument, proc, a_original_name);
                        break;
                    }
                    default: {
                        err("Only functions and class procedures can be used for generic read/write statement, found " + std::to_string(proc->type), loc);
                    }
                }
            }
        };
        if (ASR::is_a<ASR::CustomOperator_t>(*orig_sym)) {
            ASR::CustomOperator_t* gen_proc = ASR::down_cast<ASR::CustomOperator_t>(orig_sym);
            process_proc_list(gen_proc->m_procs, gen_proc->n_procs);
        } else if (ASR::is_a<ASR::GenericProcedure_t>(*orig_sym)) {
            ASR::GenericProcedure_t* gen_proc = ASR::down_cast<ASR::GenericProcedure_t>(orig_sym);
            process_proc_list(gen_proc->m_procs, gen_proc->n_procs);
        } else {
            err("Only generic procedures can be used for overloaded read/write statement, found " + std::to_string(orig_sym->type), loc);
        }
    };
    // The generic interface visible in the scope, then the type-bound generic
    // of the item's type.
    ASR::symbol_t* sym = curr_scope->resolve_symbol(read_write);
    if (sym) {
        resolve_generic(sym);
    }
    if (a_name == nullptr && ASR::is_a<ASR::StructType_t>(*arg_type)) {
        ASR::Struct_t* arg_struct = ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(item)));
        // The type-bound generic of the item's type, or else the one it
        // inherits from an ancestor, then the scopes around the type.
        for (ASR::Struct_t* s = arg_struct; s != nullptr && a_name == nullptr;
                s = s->m_parent ? ASR::down_cast<ASR::Struct_t>(
                    ASRUtils::symbol_get_past_external(s->m_parent)) : nullptr) {
            sym = s->m_symtab->get_symbol(read_write);
            if (sym) {
                expr_dt = item;
                resolve_generic(sym);
            }
        }
        if (a_name == nullptr) {
            sym = arg_struct->m_symtab->parent->resolve_symbol(read_write);
            if (sym) {
                expr_dt = item;
                resolve_generic(sym);
            }
        }
        if (a_name == nullptr) {
            expr_dt = nullptr;
        }
    }
    return a_name;
}

bool use_overloaded_file_read_write(std::string &read_write, Vec<ASR::expr_t*> args,
                               SymbolTable* curr_scope, ASR::asr_t*& asr,
                               Allocator &al, const Location& loc,
                               SetChar& current_function_dependencies,
                               SetChar& current_module_dependencies,
                               const std::function<void (const std::string &, const Location &)> err) {
    ASR::symbol_t* a_original_name = nullptr;
    ASR::expr_t* expr_dt = nullptr;
    ASR::symbol_t* a_name = resolve_defined_io_procedure(read_write, args[0],
        curr_scope, al, loc, current_function_dependencies,
        current_module_dependencies, err, a_original_name, expr_dt);
    if (a_name == nullptr) {
        return false;
    }
    ASR::Function_t* subrout = ASRUtils::get_function(a_name);
    Vec<ASR::call_arg_t> a_args;
    a_args.reserve(al, 1);
    for (size_t i = 0; i < args.size(); i++) {
        if (i == 0 && (expr_dt != nullptr)) {
            continue;
        }
        ASR::call_arg_t arg;
        arg.loc = args[0]->base.loc;
        arg.m_value = args[i];
        a_args.push_back(al, arg);
    }
    ASRUtils::set_absent_optional_arguments_to_null(a_args, subrout, al, expr_dt);
    ASR::call_arg_t* call_args = a_args.p;
    size_t n_call_args = a_args.n;
    ASRUtils::insert_self_arg(al, a_name, call_args, n_call_args, expr_dt);
    asr = ASRUtils::make_SubroutineCall_t_util(al, loc, a_name, a_original_name,
                                    call_args, n_call_args, expr_dt, nullptr, false);
    return true;
}

bool use_overloaded(ASR::expr_t* left, ASR::expr_t* right,
                    ASR::cmpopType op, std::string& intrinsic_op_name,
                    SymbolTable* curr_scope, ASR::asr_t*& asr,
                    Allocator &al, const Location& loc,
                    SetChar& current_function_dependencies,
                    SetChar& current_module_dependencies,
                    const std::function<void (const std::string &, const Location &)> err) {
    ASR::ttype_t *left_type = ASRUtils::expr_type(left);
    ASR::ttype_t *right_type = ASRUtils::expr_type(right);
    ASR::Struct_t *left_struct = nullptr, *right_struct = nullptr;

    if (ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(left_type))) {
        left_struct = ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(left)));
    }
    if (ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(right_type))) {
        right_struct = ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(right)));
    }
    bool found = false;
    // Try left_struct first; if not found, try right_struct (for pass(rhs) operators)
    ASR::Struct_t* active_struct = left_struct;
    bool pass_right = false;
    bool op_overloaded_found = is_op_overloaded(op, intrinsic_op_name, curr_scope, left_struct);
    if (!op_overloaded_found && right_struct != nullptr) {
        op_overloaded_found = is_op_overloaded(op, intrinsic_op_name, curr_scope, right_struct);
        if (op_overloaded_found) {
            active_struct = right_struct;
            pass_right = true;
        }
    }
    if (op_overloaded_found) {
        ASR::symbol_t* sym = curr_scope->resolve_symbol(intrinsic_op_name);
        ASR::symbol_t* orig_sym = ASRUtils::symbol_get_past_external(sym);
        ASR::symbol_t* orig_sym2 = nullptr;
        if ( active_struct != nullptr) {
            ASR::Struct_t* temp_struct = active_struct;
            while (temp_struct) {
                if (temp_struct->m_symtab->get_symbol(intrinsic_op_name) != nullptr) {
                    orig_sym2 = temp_struct->m_symtab->get_symbol(intrinsic_op_name);
                    break;
                } else if (temp_struct->m_parent) {
                    temp_struct = ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(temp_struct->m_parent));
                } else {
                    temp_struct = nullptr;
                }
            }
        }
        // We need to compare all functions available (Class Procedures + interface declared)
        Vec<ASR::symbol_t*> op_overloading_procs; op_overloading_procs.reserve(al, 1);
        size_t n_interface_proc = 0;
        // functions that are declared by interface operator(==)
        if (orig_sym) {
            ASR::CustomOperator_t* gen_proc = ASR::down_cast<ASR::CustomOperator_t>(orig_sym);
            for( size_t i = 0; i < gen_proc->n_procs; i++ ) {
                op_overloading_procs.push_back(al, gen_proc->m_procs[i]);
            }
            n_interface_proc = gen_proc->n_procs;
        }
        // functions that are class procedure
        if (orig_sym2) {
            ASR::CustomOperator_t* gen_proc = ASR::down_cast<ASR::CustomOperator_t>(orig_sym2);
            for( size_t i = 0; i < gen_proc->n_procs; i++ ) {
                op_overloading_procs.push_back(al, gen_proc->m_procs[i]);
            }
        }
        for( size_t i = 0; i < op_overloading_procs.size() && !found; i++ ) {
            ASR::symbol_t* proc;
            ASR::symbol_t* orig_proc = ASRUtils::symbol_get_past_external(op_overloading_procs[i]);
            if ( ASR::is_a<ASR::StructMethodDeclaration_t>(*op_overloading_procs[i]) ) {
                ASR::StructMethodDeclaration_t* cp = ASR::down_cast<ASR::StructMethodDeclaration_t>(op_overloading_procs[i]);
                if (cp->m_parent_symtab->get_counter() != active_struct->m_symtab->get_counter()) {
                    // It may be overided in the derived class
                    ASR::Struct_t* temp_struct = active_struct;
                    while (temp_struct->m_parent) {
                        if (temp_struct->m_symtab->get_symbol(cp->m_name) != nullptr) {
                            cp = ASR::down_cast<ASR::StructMethodDeclaration_t>(temp_struct->m_symtab->get_symbol(cp->m_name));
                            orig_proc = temp_struct->m_symtab->get_symbol(cp->m_name);
                            break;
                        }
                        temp_struct = ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(temp_struct->m_parent));
                    }
                }
                proc =  ASRUtils::symbol_get_past_external(cp->m_proc);
            } else {
                proc = ASRUtils::symbol_get_past_external(op_overloading_procs[i]);
            }
            switch(proc->type) {
                case ASR::symbolType::Function: {
                    ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(proc);
                    bool is_elemental = ASRUtils::is_elemental(proc);
                    std::string matched_func_name = "";
                    if( func->n_args == 2 ) {
                        if (operand_matches_dummy(left, func->m_args[0], is_elemental) &&
                            operand_matches_dummy(right, func->m_args[1], is_elemental)) {
                            found = true;
                            Vec<ASR::call_arg_t> a_args;
                            a_args.reserve(al, 1);
                            ASR::call_arg_t left_call_arg, right_call_arg;
                            ASR::expr_t* self_arg = nullptr;
                            if (i < n_interface_proc || pass_right) {
                                // For interface procs or pass(rhs) class procs,
                                // both args go in a_args in proper order
                                left_call_arg.loc = left->base.loc, left_call_arg.m_value = left;
                                a_args.push_back(al, left_call_arg);
                                right_call_arg.loc = right->base.loc, right_call_arg.m_value = right;
                                a_args.push_back(al, right_call_arg);
                            } else {
                                // pass(lhs) or default: self_arg is left, right is the regular argument
                                right_call_arg.loc = right->base.loc, right_call_arg.m_value = right;
                                a_args.push_back(al, right_call_arg);
                            }
                            ASR::symbol_t* a_name = nullptr;
                            if (i < n_interface_proc && !is_type_bound_generic(sym)) {
                                a_name = op_overloading_procs[i];
                            } else if (i >= n_interface_proc && !pass_right) {
                                a_name = import_class_procedure(al, loc, orig_proc, curr_scope);
                                self_arg = left;
                            } else {
                                a_name = import_type_bound_specific(al, curr_scope, proc,
                                    intrinsic_op_name);
                            }
                            matched_func_name = ASRUtils::symbol_name(a_name);
                            ASR::ttype_t *return_type = nullptr;
                            // The passed object of a type-bound operator is not
                            // in a_args, so the shape comes from both operands.
                            Vec<ASR::call_arg_t> operands;
                            operands.reserve(al, 2);
                            operands.push_back(al, ASR::call_arg_t{ left->base.loc, left });
                            operands.push_back(al, ASR::call_arg_t{ right->base.loc, right });
                            ASR::expr_t* first_array_arg =
                                ASRUtils::find_first_array_arg_if_elemental(func, operands);
                            if( first_array_arg != nullptr ) {
                                ASR::dimension_t* array_dims;
                                size_t array_n_dims = ASRUtils::extract_dimensions_from_ttype(
                                ASRUtils::expr_type(first_array_arg), array_dims);
                                Vec<ASR::dimension_t> new_dims;
                                new_dims.from_pointer_n_copy(al, array_dims, array_n_dims);
                                return_type = ASRUtils::duplicate_type(al,
                                                ASRUtils::get_FunctionType(func)->m_return_var_type,
                                                &new_dims);
                            }
                            if (ASRUtils::symbol_parent_symtab(a_name)->get_counter() != curr_scope->get_counter()) {
                                ADD_ASR_DEPENDENCIES_WITH_NAME(curr_scope, a_name, current_function_dependencies, s2c(al, matched_func_name));
                            }
                            ASRUtils::insert_module_dependency(a_name, al, current_module_dependencies);
                            ASRUtils::set_absent_optional_arguments_to_null(a_args, func, al, self_arg);
                            ASR::call_arg_t* call_args2 = a_args.p;
                            size_t n_call_args2 = a_args.n;
                            ASRUtils::insert_self_arg(al, a_name, call_args2, n_call_args2, self_arg);
                            if (return_type == nullptr) {
                                return_type = ASRUtils::EXPR2VAR(func->m_return_var)->m_type;
                                if (ASR::is_a<ASR::Array_t>(*return_type)) {
                                    ASR::Array_t* arr_t = ASR::down_cast<ASR::Array_t>(return_type);
                                    ASRUtils::ExprStmtDuplicator expr_duplicator(al);
                                    expr_duplicator.allow_procedure_calls = true;
                                    Vec<ASR::call_arg_t> oa_vec;
                                    oa_vec.from_pointer_n_copy(al, call_args2, n_call_args2);
                                    ASRUtils::ReplaceArgVisitor arg_replacer(al, curr_scope, func,
                                        oa_vec, current_function_dependencies, current_module_dependencies);
                                    Vec<ASR::dimension_t> new_dims;
                                    new_dims.reserve(al, arr_t->n_dims);
                                    for (size_t di = 0; di < arr_t->n_dims; di++) {
                                        ASR::dimension_t new_dim;
                                        new_dim.loc = arr_t->m_dims[di].loc;
                                        new_dim.m_start = nullptr;
                                        new_dim.m_length = nullptr;
                                        if (arr_t->m_dims[di].m_start) {
                                            expr_duplicator.success = true;
                                            ASR::expr_t* sc = expr_duplicator.duplicate_expr(arr_t->m_dims[di].m_start);
                                            LCOMPILERS_ASSERT(expr_duplicator.success);
                                            arg_replacer.current_expr = &sc;
                                            arg_replacer.replace_expr(sc);
                                            new_dim.m_start = sc;
                                        }
                                        if (arr_t->m_dims[di].m_length) {
                                            expr_duplicator.success = true;
                                            ASR::expr_t* lc = expr_duplicator.duplicate_expr(arr_t->m_dims[di].m_length);
                                            LCOMPILERS_ASSERT(expr_duplicator.success);
                                            arg_replacer.current_expr = &lc;
                                            arg_replacer.replace_expr(lc);
                                            new_dim.m_length = lc;
                                        }
                                        new_dims.push_back(al, new_dim);
                                    }
                                    bool for_type = true;
                                    for (size_t k = 0; k < new_dims.size(); k++) {
                                        if (new_dims[k].m_length && ASR::is_a<ASR::ArraySize_t>(*new_dims[k].m_length)) {
                                            ASR::ArraySize_t* as_t = ASR::down_cast<ASR::ArraySize_t>(new_dims[k].m_length);
                                            if ((ASR::is_a<ASR::FunctionCall_t>(*as_t->m_v) && ASRUtils::is_allocatable(as_t->m_v)) ||
                                                ASR::is_a<ASR::IntrinsicArrayFunction_t>(*as_t->m_v)) {
                                                for_type = false;
                                                break;
                                            }
                                        }
                                    }
                                    return_type = ASRUtils::make_Array_t_util(
                                        al, loc, arr_t->m_type, new_dims.p, new_dims.size(),
                                        ASR::abiType::Source, false,
                                        ASR::array_physical_typeType::DescriptorArray,
                                        false, false, for_type);
                                }
                            }
                            asr = ASRUtils::make_FunctionCall_t_util(al, loc, a_name, sym,
                                                            call_args2, n_call_args2,
                                                            return_type,
                                                            nullptr, self_arg
                                                            );
                        }
                    }
                    break;
                }
                default: {
                    err("While overloading binary operators only functions can be used",
                                        proc->base.loc);
                }
            }
        }
    }
    return found;
}

bool use_overloaded(ASR::expr_t* left, ASR::expr_t* right,
                            ASR::logicalbinopType op, std::string& intrinsic_op_name,
                            SymbolTable* curr_scope, ASR::asr_t*& asr,
                            Allocator& al, const Location& loc,
                            SetChar& current_function_dependencies,
                            SetChar& current_module_dependencies,
                            const std::function<void(const std::string&, const Location&)> err) {
    ASR::ttype_t* left_type = ASRUtils::expr_type(left);
    ASR::Struct_t* left_struct = nullptr;
    if (ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(left_type))) {
        left_struct = ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(left))));
    }
    bool found = false;
    if (is_op_overloaded(op, intrinsic_op_name, curr_scope, left_struct)) {
        ASR::symbol_t* sym = curr_scope->resolve_symbol(intrinsic_op_name);
        ASR::symbol_t* orig_sym = ASRUtils::symbol_get_past_external(sym);
        if (left_struct != nullptr && orig_sym == nullptr) {
            orig_sym = left_struct->m_symtab->resolve_symbol(intrinsic_op_name);
        }
        ASR::CustomOperator_t* gen_proc = ASR::down_cast<ASR::CustomOperator_t>(orig_sym);
        for (size_t i = 0; i < gen_proc->n_procs && !found; i++) {
            ASR::symbol_t* proc;
            if (ASR::is_a<ASR::StructMethodDeclaration_t>(*gen_proc->m_procs[i])) {
                proc = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::StructMethodDeclaration_t>(gen_proc->m_procs[i])->m_proc);
            } else {
                proc = ASRUtils::symbol_get_past_external(gen_proc->m_procs[i]);
            }
            switch (proc->type) {
                case ASR::symbolType::Function: {
                    ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(proc);
                    bool is_elemental = ASRUtils::is_elemental(proc);
                    std::string matched_func_name = "";
                    if (func->n_args == 2) {
                        if (operand_matches_dummy(left, func->m_args[0], is_elemental) &&
                            operand_matches_dummy(right, func->m_args[1], is_elemental)) {
                            found = true;
                            Vec<ASR::call_arg_t> a_args;
                            a_args.reserve(al, 2);
                            a_args.push_back(al, ASR::call_arg_t{ left->base.loc, left });
                            a_args.push_back(al, ASR::call_arg_t{ right->base.loc, right });
                            ASR::symbol_t* a_name = nullptr;
                            if (sym != nullptr && !is_type_bound_generic(sym)
                                    && !ASR::is_a<ASR::StructMethodDeclaration_t>(*gen_proc->m_procs[i])) {
                                a_name = gen_proc->m_procs[i];
                            } else {
                                a_name = import_type_bound_specific(al, curr_scope, proc,
                                    intrinsic_op_name);
                            }
                            matched_func_name = ASRUtils::symbol_name(a_name);
                            ASR::ttype_t* return_type = nullptr;
                            ASR::expr_t* first_array_arg =
                                ASRUtils::find_first_array_arg_if_elemental(func, a_args);
                            if (first_array_arg != nullptr) {
                                ASR::dimension_t* array_dims;
                                size_t array_n_dims = ASRUtils::extract_dimensions_from_ttype(
                                    ASRUtils::expr_type(first_array_arg), array_dims);
                                Vec<ASR::dimension_t> new_dims;
                                new_dims.from_pointer_n_copy(al, array_dims, array_n_dims);
                                return_type = ASRUtils::duplicate_type(
                                    al,
                                    ASRUtils::get_FunctionType(func)->m_return_var_type,
                                    &new_dims);
                            } else {
                                return_type = ASRUtils::expr_type(func->m_return_var);
                            }
                            if (ASRUtils::symbol_parent_symtab(a_name)->get_counter()
                                != curr_scope->get_counter()) {
                                ADD_ASR_DEPENDENCIES_WITH_NAME(curr_scope,
                                                               a_name,
                                                               current_function_dependencies,
                                                               s2c(al, matched_func_name));
                            }
                            ASRUtils::insert_module_dependency(
                                a_name, al, current_module_dependencies);
                            ASRUtils::set_absent_optional_arguments_to_null(a_args, func, al);
                            {
                                ASR::FunctionType_t* ftype = ASRUtils::get_FunctionType(func);
                                // An elemental reference with an array argument
                                // has the shape of the argument, set above.
                                if (ftype->m_return_var_type && first_array_arg == nullptr) {
                                    return_type = ASRUtils::duplicate_type(al, ftype->m_return_var_type);
                                    ASR::dimension_t* ret_dims = nullptr;
                                    size_t n_ret_dims = ASRUtils::extract_dimensions_from_ttype(return_type, ret_dims);
                                     if (n_ret_dims > 0) {
                                        // Keep whichever 'r' initialization is currently there, for example:
                                        ReplaceFunctionParamWithArg r(al, a_args.p, a_args.n); // OR call_args1, n_call_args1

                                        // 1. Initialize the caller's duplicator OUTSIDE the loop
                                        ASRUtils::ExprStmtDuplicator caller_dup(al);
                                        caller_dup.allow_procedure_calls = true;
                                        caller_dup.success = true;

                                     for (size_t di = 0; di < n_ret_dims; di++) {
                                         if (ret_dims[di].m_length) {
                                            // 2. Extract and safely duplicate m_length
                                            ASR::expr_t* replaced_length = r.replace_FunctionParam_with_arg(ret_dims[di].m_length);
                                            ret_dims[di].m_length = (replaced_length == ret_dims[di].m_length) ? caller_dup.duplicate_expr(replaced_length) : replaced_length;
                                         }
        
                                         if (ret_dims[di].m_start) {
                                            // 3. Extract and safely duplicate m_start
                                            ASR::expr_t* replaced_start = r.replace_FunctionParam_with_arg(ret_dims[di].m_start);
                                            ret_dims[di].m_start = (replaced_start == ret_dims[di].m_start) ? caller_dup.duplicate_expr(replaced_start) : replaced_start;
                                         }
                                        }
                                     }
                                }
                            }
                            asr = ASRUtils::make_FunctionCall_t_util(
                                al, loc, a_name, sym, a_args.p, 2, return_type, nullptr, nullptr);
                        }
                    }
                    break;
                }
                default: {
                    err("Only functions can be used to overload logical binary operators",
                        proc->base.loc);
                }
            }
        }
    }
    return found;
}

bool is_op_overloaded(ASR::cmpopType op, std::string& intrinsic_op_name,
                      SymbolTable* curr_scope, ASR::Struct_t *left_struct) {
    bool result = true;
    switch(op) {
        case ASR::cmpopType::Eq: {
            if(intrinsic_op_name != "~eq") {
                result = false;
            }
            break;
        }
        case ASR::cmpopType::NotEq: {
            if(intrinsic_op_name != "~noteq") {
                result = false;
            }
            break;
        }
        case ASR::cmpopType::Lt: {
            if(intrinsic_op_name != "~lt") {
                result = false;
            }
            break;
        }
        case ASR::cmpopType::LtE: {
            if(intrinsic_op_name != "~lte") {
                result = false;
            }
            break;
        }
        case ASR::cmpopType::Gt: {
            if(intrinsic_op_name != "~gt") {
                result = false;
            }
            break;
        }
        case ASR::cmpopType::GtE: {
            if(intrinsic_op_name != "~gte") {
                result = false;
            }
            break;
        }
    }
    if( result && curr_scope->resolve_symbol(intrinsic_op_name) == nullptr ) {
        bool is_op_in_scope = false;
        ASR::Struct_t* temp_struct = left_struct;
        while (temp_struct) {
            // Try to resolve intrinsic_op_name till its last ancestor
            if (temp_struct->m_symtab->get_symbol(
                intrinsic_op_name) != nullptr) {
                is_op_in_scope = true;
                break;
            } else if (temp_struct->m_parent) {
                temp_struct = ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(temp_struct->m_parent));
            } else {
                temp_struct = nullptr;
            }
        }
        if (is_op_in_scope) {
            result = true;
        } else {
            result = false;
        }
    }

    return result;
}

bool is_op_overloaded(ASR::logicalbinopType op, std::string& intrinsic_op_name,
                      SymbolTable* curr_scope, ASR::Struct_t* left_struct) {
    bool result = true;
    switch (op) {
        case ASR::logicalbinopType::And: {
            if (intrinsic_op_name != "~and") {
                result = false;
            }
            break;
        }
        case ASR::logicalbinopType::Or: {
            if (intrinsic_op_name != "~or") {
                result = false;
            }
            break;
        }
        case ASR::logicalbinopType::Xor: {
            if (intrinsic_op_name != "~xor") {
                result = false;
            }
            break;
        }
        case ASR::logicalbinopType::NEqv: {
            if (intrinsic_op_name != "~neqv") {
                result = false;
            }
            break;
        }
        case ASR::logicalbinopType::Eqv: {
            if (intrinsic_op_name != "~eqv") {
                result = false;
            }
            break;
        }
        default: {
            result = false;
            break;
        }
    }

    if (result && curr_scope->resolve_symbol(intrinsic_op_name) == nullptr) {
        if (left_struct != nullptr) {
            ASR::Struct_t* temp_struct = left_struct;
            result = false;
            while (temp_struct) {
                if (temp_struct->m_symtab->resolve_symbol(intrinsic_op_name) != nullptr) {
                    result = true;
                    break;
                } else if (temp_struct->m_parent) {
                    temp_struct = ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(temp_struct->m_parent));
                } else {
                    temp_struct = nullptr;
                }
            }
        } else {
            result = false;
        }
    }

    return result;
}

template <typename T>
bool argument_types_match(const Vec<ASR::call_arg_t>& args,
        const T &sub) {
    if (args.size() <= sub.n_args) {
        size_t i;
        for (i = 0; i < args.size(); i++) {
            ASR::symbol_t* sub_arg_sym = symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(sub.m_args[i])->m_v
            );
            if (ASR::is_a<ASR::Variable_t>(*sub_arg_sym)) {
                ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(sub_arg_sym);
                if (args[i].m_value == nullptr &&
                    v->m_presence == ASR::presenceType::Optional) {
                    // If it's optional and argument is empty
                    // continue to next argument.
                    continue;
                }
                if (args[i].m_value == nullptr) {
                    // Required argument is not provided — overload doesn't match.
                    return false;
                }
                ASR::ttype_t *arg1 = ASRUtils::expr_type(args[i].m_value);
                ASR::ttype_t *arg2 = v->m_type;

                // Check if formal argument is an implicit interface procedure (e.g. procedure() :: f)
                // If it is, it accepts any explicit interface procedure as actual argument.
                if (v->m_type_declaration && ASR::is_a<ASR::FunctionType_t>(*arg1)) {
                    ASR::symbol_t* type_decl = ASRUtils::symbol_get_past_external(v->m_type_declaration);
                    if (ASR::is_a<ASR::Function_t>(*type_decl)) {
                        std::string decl_name = ASR::down_cast<ASR::Function_t>(type_decl)->m_name;
                        if (decl_name.find("__") == 0 && decl_name.find("_iface_implicit") != std::string::npos) {
                            continue;
                        }
                    }
                }

                // A procedure declared `external` with no interface (e.g.
                // `external :: f`) has an unknown signature, so it matches any
                // explicit interface formal argument, and vice versa.
                if (ASR::is_a<ASR::FunctionType_t>(*arg1) && ASR::is_a<ASR::FunctionType_t>(*arg2)) {
                    ASR::FunctionType_t* arg1_func_type = ASR::down_cast<ASR::FunctionType_t>(arg1);
                    ASR::FunctionType_t* arg2_func_type = ASR::down_cast<ASR::FunctionType_t>(arg2);
                    if (ASRUtils::is_bare_implicit_interface(*arg1_func_type) ||
                        ASRUtils::is_bare_implicit_interface(*arg2_func_type)) {
                        continue;
                    }
                }

                ASR::symbol_t* s1 = nullptr;
                ASR::symbol_t* s2 = nullptr;
                ASR::ttype_t* arg2_ext = ASRUtils::extract_type(arg2);
                bool is_elemental = ASRUtils::get_FunctionType(sub)->m_elemental;
                if (ASR::is_a<ASR::StructType_t>(*arg2_ext)) {
                    if ((ASRUtils::is_allocatable(arg2) && !ASRUtils::is_allocatable(arg1)) ||
                            (ASRUtils::is_array(arg2) && !ASRUtils::is_array(arg1) && !ASRUtils::is_assumed_rank_array(arg2)) ||
                            (!is_elemental && !ASRUtils::is_array(arg2) && ASRUtils::is_array(arg1))) {
                        return false;
                    }
                    if (ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(arg1)) || ASRUtils::is_class_type(ASRUtils::extract_type(arg1))) {
                        s1 = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(args[i].m_value));
                    }
                    s2 = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(sub.m_args[i]));
                }
                if (s1 && s2) {
                    bool is_arg_class = ASRUtils::is_class_type(arg2_ext);
                    if (!is_arg_class) {     // if argument is c_struct we need exact matching types
                        if (s1 != s2) {
                            return false;
                        }
                    } else if (!ASRUtils::check_class_assignment_compatibility(s2, s1)) {
                        return false;
                    }
                    if (!is_elemental) {
                        int rank1 = ASRUtils::extract_n_dims_from_ttype(arg1);
                        int rank2 = ASRUtils::extract_n_dims_from_ttype(arg2);
                        if (rank1 != rank2
                                && !ASRUtils::is_assumed_rank_array(arg2)) {
                            return false;
                        }
                    }
                } else {
                    if (!types_equal(arg1, arg2, args[i].m_value, sub.m_args[i], !ASRUtils::get_FunctionType(sub)->m_elemental)) {
                        return false;
                    }
                }
            } else if (ASR::is_a<ASR::Function_t>(*sub_arg_sym)) {
                if (args[i].m_value == nullptr) {
                    // Required procedure argument is not provided — overload doesn't match.
                    return false;
                }
                ASR::Function_t* f = ASR::down_cast<ASR::Function_t>(sub_arg_sym);

                ASR::ttype_t *arg1 = ASRUtils::expr_type(args[i].m_value);
                ASR::ttype_t *arg2 = f->m_function_signature;
                // A procedure pointer actual has the type
                // Pointer(FunctionType); compare its target's signature.
                ASR::ttype_t *arg1_proc = ASRUtils::type_get_past_pointer(arg1);

                // A procedure declared `external` with no interface (e.g.
                // `external :: f`) has an unknown signature, so it matches any
                // explicit interface formal argument.
                if (ASR::is_a<ASR::FunctionType_t>(*arg1_proc) && ASR::is_a<ASR::FunctionType_t>(*arg2)) {
                    ASR::FunctionType_t* arg1_func_type = ASR::down_cast<ASR::FunctionType_t>(arg1_proc);
                    if (ASRUtils::is_bare_implicit_interface(*arg1_func_type)) {
                        continue;
                    }
                    // A formal procedure with an implicit interface (e.g.
                    // `external :: f` or `real(8), external :: f`) has no
                    // argument list to compare. Only its result type may be
                    // known: a typed formal is a function, so the actual must
                    // be a function with the same result type.
                    ASR::FunctionType_t* arg2_func_type = ASR::down_cast<ASR::FunctionType_t>(arg2);
                    if (ASRUtils::is_bare_implicit_interface(*arg2_func_type)) {
                        ASR::ttype_t* actual_result = arg1_func_type->m_return_var_type;
                        ASR::ttype_t* formal_result = arg2_func_type->m_return_var_type;
                        if (formal_result && (actual_result == nullptr ||
                                !types_equal(actual_result, formal_result,
                                    nullptr, nullptr, true))) {
                            return false;
                        }
                        continue;
                    }
                }

                Allocator al(512);
                if (!types_equal(arg1, arg2, args[i].m_value,
                    ASRUtils::get_expr_from_sym(al, &f->base), false)) {
                    return false;
                }
            }
        }
        for( ; i < sub.n_args; i++ ) {
            ASR::symbol_t* sub_arg_sym = symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(sub.m_args[i])->m_v);
            if (ASR::is_a<ASR::Variable_t>(*sub_arg_sym)) {
                ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(sub_arg_sym);
                if (v->m_presence != ASR::presenceType::Optional) {
                    return false;
                }
            } else if (ASR::is_a<ASR::Function_t>(*sub_arg_sym)) {
                return false;
            }
        }
        return true;
    } else {
        return false;
    }
}

bool select_func_subrout(const ASR::symbol_t* proc, const Vec<ASR::call_arg_t>& args,
                         Location& loc, const std::function<void (const std::string &, const Location &)> err) {
    bool result = false;
    proc = ASRUtils::symbol_get_past_external(proc);
    if (ASR::is_a<ASR::StructMethodDeclaration_t>(*proc)) {
        proc = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::StructMethodDeclaration_t>(proc)->m_proc);
    }
    if (ASR::is_a<ASR::Function_t>(*proc)) {
        ASR::Function_t *fn
            = ASR::down_cast<ASR::Function_t>(proc);
        if (argument_types_match(args, *fn)) {
            result = true;
        }
    } else {
        err("Only Subroutine and Function supported in generic procedure", loc);
    }
    return result;
}

ASR::asr_t* symbol_resolve_external_generic_procedure_without_eval(
            const Location &loc,
            ASR::symbol_t *v, Vec<ASR::call_arg_t>& args,
            SymbolTable* current_scope, Allocator& al,
            const std::function<void (const std::string &, const Location &)> err) {
    ASR::ExternalSymbol_t *p = ASR::down_cast<ASR::ExternalSymbol_t>(v);
    ASR::symbol_t *f2 = ASR::down_cast<ASR::ExternalSymbol_t>(v)->m_external;
    ASR::GenericProcedure_t *g = ASR::down_cast<ASR::GenericProcedure_t>(f2);
    int idx = select_generic_procedure(args, *g, loc, err);
    if (idx < 0) {
        return nullptr;
    }
    ASR::symbol_t *final_sym = g->m_procs[idx];
    ASR::symbol_t *final_sym_unwrapped = ASRUtils::symbol_get_past_external(final_sym);
    if (ASR::is_a<ASR::StructMethodDeclaration_t>(*final_sym_unwrapped)) {
        // Import class procedures into the current scope so call symbols stay local.
        final_sym = ASRUtils::import_class_procedure(al, loc, final_sym_unwrapped, current_scope);
    }

    if (ASRUtils::symbol_parent_symtab(final_sym)->get_counter() != current_scope->get_counter()) {
        // Create a local ExternalSymbol for non-local specific procedures.
        ASR::symbol_t *final_sym_past_ext = ASRUtils::symbol_get_past_external(final_sym);
        std::string local_sym = std::string(p->m_name) + "@"
            + ASRUtils::symbol_name(final_sym_past_ext);
        if (current_scope->get_symbol(local_sym) == nullptr) {
            ASR::symbol_t *owner_sym = ASRUtils::get_asr_owner(final_sym_past_ext);
            Str name;
            name.from_str(al, local_sym);
            ASR::asr_t *sub = ASR::make_ExternalSymbol_t(
                al, g->base.base.loc,
                current_scope,
                name.c_str(al),
                final_sym_past_ext,
                s2c(al, ASRUtils::symbol_name(owner_sym)), nullptr, 0,
                s2c(al, ASRUtils::symbol_name(final_sym_past_ext)),
                ASR::accessType::Private
            );
            current_scope->add_symbol(local_sym, ASR::down_cast<ASR::symbol_t>(sub));
        }
        final_sym = current_scope->get_symbol(local_sym);
    }

    ASR::symbol_t *called_proc = ASRUtils::symbol_get_past_external(final_sym);
    if (ASR::is_a<ASR::StructMethodDeclaration_t>(*called_proc)) {
        called_proc = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::StructMethodDeclaration_t>(called_proc)->m_proc);
    }
    if (!ASR::is_a<ASR::Function_t>(*called_proc)) {
        err("Only Subroutine and Function supported in generic procedure", loc);
        return nullptr;
    }

    ASR::Function_t *func = ASR::down_cast<ASR::Function_t>(called_proc);
    bool is_subroutine = func->m_return_var == nullptr;
    ASR::ttype_t *return_type = nullptr;
    if (func->m_return_var) {
        if( ASRUtils::get_FunctionType(func)->m_elemental &&
            func->n_args >= 1 &&
            ASRUtils::is_array(ASRUtils::expr_type(args[0].m_value)) ) {
            ASR::dimension_t* array_dims;
            size_t array_n_dims = ASRUtils::extract_dimensions_from_ttype(
            ASRUtils::expr_type(args[0].m_value), array_dims);
            Vec<ASR::dimension_t> new_dims;
            new_dims.from_pointer_n_copy(al, array_dims, array_n_dims);
            return_type = ASRUtils::duplicate_type(al,
                            ASRUtils::get_FunctionType(func)->m_return_var_type,
                            &new_dims);
        } else {
            return_type = ASRUtils::EXPR2VAR(func->m_return_var)->m_type;
        }
    }
    // ASRUtils::insert_module_dependency(v, al, current_module_dependencies);
    if( is_subroutine ) {
        ASRUtils::set_absent_optional_arguments_to_null(args, func, al);
        return ASRUtils::make_SubroutineCall_t_util(al, loc, final_sym,
                                        v, args.p, args.size(),
                                        nullptr, nullptr, false);
    } else {
        ASRUtils::set_absent_optional_arguments_to_null(args, func, al);
        return ASRUtils::make_FunctionCall_t_util(al, loc, final_sym,
                                        v, args.p, args.size(),
                                        return_type,
                                        nullptr, nullptr
                                        );
    }
}

ASR::asr_t* make_Cast_t_value(Allocator &al, const Location &a_loc,
            ASR::expr_t* a_arg, ASR::cast_kindType a_kind, ASR::ttype_t* a_type) {

    ASR::expr_t* value = nullptr;

    if (ASRUtils::expr_value(a_arg)) {
        // calculate value
        if (a_kind == ASR::cast_kindType::RealToInteger) {
            ASR::RealConstant_t* rc = ASR::down_cast<ASR::RealConstant_t>(
                        ASRUtils::expr_value(a_arg));
            // kind=16 m_r is a pointer-encoded payload, not a double — skip
            // compile-time folding and let the runtime fptosi handle it.
            // kind=10 m_r is a pointer to the long double bytes, which
            // real_constant_get_r10 reads.
            int src_kind = ASRUtils::extract_kind_from_ttype_t(rc->m_type);
            if (src_kind != 16) {
                int64_t v = src_kind == 10
                    ? static_cast<int64_t>(ASRUtils::real_constant_get_r10(rc))
                    : static_cast<int64_t>(rc->m_r);
                value = ASR::down_cast<ASR::expr_t>(
                        ASR::make_IntegerConstant_t(al, a_loc, v, a_type));
            }
        } else if (a_kind == ASR::cast_kindType::RealToReal) {
            ASR::RealConstant_t* rc = ASR::down_cast<ASR::RealConstant_t>(
                       ASRUtils::expr_value(a_arg));
            int src_kind = ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(a_arg));
            int dest_kind = ASRUtils::extract_kind_from_ttype_t(a_type);
            // Skip compile-time folding when either side is real(16): m_r is
            // a pointer payload for kind=16 (see real_constant_get_r16_bytes),
            // and folding through double would silently truncate. Runtime
            // cast handles it.
            if (src_kind != 16 && dest_kind != 16) {
                // kind=10 m_r is a pointer to the long double bytes, so
                // read it with real_constant_get_r10 and build a kind=10
                // result with make_RealConstant_util.
                double v = src_kind == 10
                    ? ASRUtils::real_constant_get_r10(rc) : rc->m_r;
                value = ASRUtils::make_RealConstant_util(al, a_loc, v, a_type);
            }
        } else if (a_kind == ASR::cast_kindType::RealToComplex) {
            ASR::RealConstant_t* rc = ASR::down_cast<ASR::RealConstant_t>(
                                  ASRUtils::expr_value(a_arg));
            int src_kind = ASRUtils::extract_kind_from_ttype_t(rc->m_type);
            if (src_kind != 16) {
                double re = src_kind == 10
                    ? ASRUtils::real_constant_get_r10(rc) : rc->m_r;
                value = ASR::down_cast<ASR::expr_t>(ASR::make_ComplexConstant_t(
                            al, a_loc, re, 0, a_type));
            }
        } else if (a_kind == ASR::cast_kindType::IntegerToReal) {
            // TODO: Clashes with the pow functions
            int64_t int_value = ASR::down_cast<ASR::IntegerConstant_t>(ASRUtils::expr_value(a_arg))->m_n;
            int dest_kind = ASRUtils::extract_kind_from_ttype_t(a_type);
            if (dest_kind == 10) {
                value = ASRUtils::make_RealConstant_r10(al, a_loc,
                    (long double)int_value, a_type);
            } else if (dest_kind != 16) {
                value = ASR::down_cast<ASR::expr_t>(ASR::make_RealConstant_t(al, a_loc, (double)int_value, a_type));
            } else {
                // int -> real(16) is always exact for int64; build the binary128
                // bytes directly on the arena and pack them into m_r.
                uint8_t* bytes = (uint8_t*)al.alloc(16);
                lf_float128 v = lf_f128_from_int64(int_value);
                std::memcpy(bytes, v.bytes, 16);
                double m_r = ASRUtils::real_constant_pack_r16(bytes);
                value = ASR::down_cast<ASR::expr_t>(
                    ASR::make_RealConstant_t(al, a_loc, m_r, a_type));
            }
        } else if (a_kind == ASR::cast_kindType::IntegerToComplex) {
            int64_t int_value = ASR::down_cast<ASR::IntegerConstant_t>(
                                ASRUtils::expr_value(a_arg))->m_n;
            value = ASR::down_cast<ASR::expr_t>(ASR::make_ComplexConstant_t(al, a_loc,
                        (double)int_value, 0, a_type));
        } else if (a_kind == ASR::cast_kindType::IntegerToInteger) {
            int64_t int_value = ASR::down_cast<ASR::IntegerConstant_t>(
                                ASRUtils::expr_value(a_arg))->m_n;
            value = ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(al, a_loc, int_value, a_type));
        } else if (a_kind == ASR::cast_kindType::IntegerToUnsignedInteger) {
            int64_t int_value = ASR::down_cast<ASR::IntegerConstant_t>(
                                ASRUtils::expr_value(a_arg))->m_n;
            value = ASR::down_cast<ASR::expr_t>(ASR::make_UnsignedIntegerConstant_t(al, a_loc, int_value, a_type));
        } else if (a_kind == ASR::cast_kindType::UnsignedIntegerToInteger) {
            int64_t int_value = ASR::down_cast<ASR::UnsignedIntegerConstant_t>(
                                ASRUtils::expr_value(a_arg))->m_n;
            value = ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(al, a_loc, int_value, a_type));
        } else if (a_kind == ASR::cast_kindType::UnsignedIntegerToUnsignedInteger) {
            int64_t int_value = ASR::down_cast<ASR::UnsignedIntegerConstant_t>(
                                ASRUtils::expr_value(a_arg))->m_n;
            value = ASR::down_cast<ASR::expr_t>(ASR::make_UnsignedIntegerConstant_t(al, a_loc, int_value, a_type));
        } else if (a_kind == ASR::cast_kindType::IntegerToLogical) {
            int64_t int_value = ASR::down_cast<ASR::IntegerConstant_t>(
                                ASRUtils::expr_value(a_arg))->m_n;
            value = ASR::down_cast<ASR::expr_t>(ASR::make_LogicalConstant_t(al, a_loc, int_value, a_type));
        } else if (a_kind == ASR::cast_kindType::ComplexToComplex) {
            ASR::ComplexConstant_t* value_complex = ASR::down_cast<ASR::ComplexConstant_t>(
                        ASRUtils::expr_value(a_arg));
            double real = value_complex->m_re;
            double imag = value_complex->m_im;
            value = ASR::down_cast<ASR::expr_t>(
                    ASR::make_ComplexConstant_t(al, a_loc, real, imag, a_type));
        } else if (a_kind == ASR::cast_kindType::ComplexToReal) {
            ASR::ComplexConstant_t* value_complex = ASR::down_cast<ASR::ComplexConstant_t>(
                        ASRUtils::expr_value(a_arg));
            double real = value_complex->m_re;
            value = ASR::down_cast<ASR::expr_t>(
                    ASR::make_RealConstant_t(al, a_loc, real, a_type));
        } else if (a_kind == ASR::cast_kindType::IntegerToSymbolicExpression) {
            Vec<ASR::expr_t*> args;
            args.reserve(al, 1);
            args.push_back(al, a_arg);
            LCompilers::ASRUtils::create_intrinsic_function create_function =
                LCompilers::ASRUtils::IntrinsicElementalFunctionRegistry::get_create_function("SymbolicInteger");
            diag::Diagnostics diag;
            value = ASR::down_cast<ASR::expr_t>(create_function(al, a_loc, args, diag));
        }
    }

    return ASR::make_Cast_t(al, a_loc, a_arg, a_kind, a_type, value, nullptr);
}

template<typename T>
static T perform_binop(T left_value, T right_value, ASR::binopType op) {
    T result;
    switch (op) {
        case ASR::Add:
            result = left_value + right_value;
            break;
        case ASR::Sub:
            result = left_value - right_value;
            break;
        case ASR::Mul:
            result = left_value * right_value;
            break;
        case ASR::Div:
            result = left_value / right_value;
            break;
        case ASR::Pow:
            result = static_cast<T>(std::pow(left_value, right_value));
            break;
        default:
            LCOMPILERS_ASSERT(false);
            result = 0;
    }
    return result;
}

// `(re, im)**n` by repeated multiplication (binary exponentiation), with a
// negative `n` giving `1/(re, im)**(-n)`. The backends compute complex**integer
// with the same algorithm, so that a folded constant equals the value computed
// at run time.
template<typename T>
static std::complex<double> complex_integer_pow(T re, T im, int64_t n) {
    if (n == 0) {
        return std::complex<double>(1, 0);
    }
    auto mul = [](T &p, T &q, T r, T s) {
        T re_ = p*r - q*s;
        T im_ = p*s + q*r;
        p = re_;
        q = im_;
    };
    uint64_t u = n < 0 ? 0 - static_cast<uint64_t>(n) : static_cast<uint64_t>(n);
    T base_re = re, base_im = im;
    while ((u & 1) == 0) {
        mul(base_re, base_im, base_re, base_im);
        u >>= 1;
    }
    T res_re = base_re, res_im = base_im;
    u >>= 1;
    while (u != 0) {
        mul(base_re, base_im, base_re, base_im);
        if (u & 1) {
            mul(res_re, res_im, base_re, base_im);
        }
        u >>= 1;
    }
    if (n < 0) {
        // 1/(r + s i) by Smith's algorithm, as complex division is lowered
        T p = 1, q = 0, r = res_re, s = res_im;
        if (std::fabs(r) >= std::fabs(s)) {
            T ratio = s / r;
            T denom = r + s * ratio;
            res_re = (p + q * ratio) / denom;
            res_im = (q - p * ratio) / denom;
        } else {
            T ratio = r / s;
            T denom = s + r * ratio;
            res_re = (p * ratio + q) / denom;
            res_im = (q * ratio - p) / denom;
        }
    }
    return std::complex<double>(res_re, res_im);
}

ASR::expr_t* fold_binop_constants(Allocator &al, ASR::expr_t* left,
        ASR::expr_t* right, ASR::binopType op, const Location& loc,
        ASR::ttype_t* dest_type, bool &division_by_zero) {
    LCOMPILERS_ASSERT((left != nullptr) && (right != nullptr));
    division_by_zero = false;
    if (ASR::is_a<ASR::RealConstant_t>(*left) && ASR::is_a<ASR::RealConstant_t>(*right)) {
        ASR::RealConstant_t* lc = ASR::down_cast<ASR::RealConstant_t>(left);
        ASR::RealConstant_t* rc = ASR::down_cast<ASR::RealConstant_t>(right);
        if (ASRUtils::extract_kind_from_ttype_t(dest_type) == 16) {
            lf_float128 lv = ASRUtils::real_constant_get_r16(lc);
            lf_float128 rv = ASRUtils::real_constant_get_r16(rc);
            lf_float128 res;
            switch (op) {
                case ASR::Add: res = lf_f128_add(lv, rv); break;
                case ASR::Sub: res = lf_f128_sub(lv, rv); break;
                case ASR::Mul: res = lf_f128_mul(lv, rv); break;
                case ASR::Div: res = lf_f128_div(lv, rv); break;
                case ASR::Pow: res = lf_f128_pow(lv, rv); break;
                default: LCOMPILERS_ASSERT(false); res = lv;
            }
            return ASRUtils::make_RealConstant_r16(al, left->base.loc, res, dest_type);
        } else if (ASRUtils::extract_kind_from_ttype_t(dest_type) == 10) {
            const uint8_t* l_bytes = ASRUtils::real_constant_get_r10_bytes(lc);
            const uint8_t* r_bytes = ASRUtils::real_constant_get_r10_bytes(rc);
            long double lv{}, rv{};
            std::memcpy(&lv, l_bytes, sizeof(long double));
            std::memcpy(&rv, r_bytes, sizeof(long double));
            long double res;
            switch (op) {
                case ASR::Add: res = lv + rv; break;
                case ASR::Sub: res = lv - rv; break;
                case ASR::Mul: res = lv * rv; break;
                case ASR::Div: res = lv / rv; break;
                case ASR::Pow: res = std::pow(lv, rv); break;
                default: LCOMPILERS_ASSERT(false); res = lv;
            }
            return ASRUtils::make_RealConstant_r10(al, left->base.loc, res, dest_type);
        }
        if (ASRUtils::extract_kind_from_ttype_t(dest_type) == 4) {
            // Evaluate in single precision, as the program would.
            float left_value = static_cast<float>(lc->m_r);
            float right_value = static_cast<float>(rc->m_r);
            float result = perform_binop(left_value, right_value, op);
            return ASRUtils::EXPR(ASR::make_RealConstant_t(al, left->base.loc,
                result, dest_type));
        }
        double left_value = lc->m_r;
        double right_value = rc->m_r;
        return ASRUtils::EXPR(ASR::make_RealConstant_t(al, left->base.loc,
        perform_binop(left_value, right_value, op), dest_type));
    } else if (ASR::is_a<ASR::RealConstant_t>(*left) && ASR::is_a<ASR::IntegerConstant_t>(*right)){
        LCOMPILERS_ASSERT(op == ASR::binopType::Pow);
        ASR::RealConstant_t* lc = ASR::down_cast<ASR::RealConstant_t>(left);
        int64_t right_value = ASR::down_cast<ASR::IntegerConstant_t>(right)->m_n;
        if (ASRUtils::extract_kind_from_ttype_t(dest_type) == 16) {
            lf_float128 lv = ASRUtils::real_constant_get_r16(lc);
            lf_float128 res = lf_f128_pow(lv, lf_f128_from_double((double)right_value));
            return ASRUtils::make_RealConstant_r16(al, left->base.loc, res, dest_type);
        } else if (ASRUtils::extract_kind_from_ttype_t(dest_type) == 10) {
            const uint8_t* l_bytes = ASRUtils::real_constant_get_r10_bytes(lc);
            long double lv{};
            std::memcpy(&lv, l_bytes, sizeof(long double));
            long double res = std::pow(lv, (long double)right_value);
            return ASRUtils::make_RealConstant_r10(al, left->base.loc, res, dest_type);
        }
        double left_value = lc->m_r;
        double result = std::pow(left_value, right_value);
        if (ASRUtils::extract_kind_from_ttype_t(dest_type) == 4) {
            result = (float) result;
        }
        return ASRUtils::EXPR(ASR::make_RealConstant_t(al, left->base.loc,
                result, dest_type));
    } else if (ASR::is_a<ASR::IntegerConstant_t>(*left) && ASR::is_a<ASR::IntegerConstant_t>(*right)) {
        int64_t left_value = ASR::down_cast<ASR::IntegerConstant_t>(left)->m_n;
        int64_t right_value = ASR::down_cast<ASR::IntegerConstant_t>(right)->m_n;

        if (op == ASR::Div && right_value == 0) {
            division_by_zero = true;
            return nullptr;
        }

        return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, left->base.loc,
                perform_binop(left_value, right_value, op), dest_type));
    } else if (ASR::is_a<ASR::ComplexConstant_t>(*left) && ASR::is_a<ASR::ComplexConstant_t>(*right)) {
        ASR::ComplexConstant_t *left_value
            = ASR::down_cast<ASR::ComplexConstant_t>(
                    ASRUtils::expr_value(left));
        ASR::ComplexConstant_t *right_value
            = ASR::down_cast<ASR::ComplexConstant_t>(
                    ASRUtils::expr_value(right));
        std::complex<double> left_value_(left_value->m_re, left_value->m_im);
        std::complex<double> right_value_(right_value->m_re, right_value->m_im);
        std::complex<double> result = perform_binop(left_value_, right_value_, op);
        return ASRUtils::EXPR( ASR::make_ComplexConstant_t(al, loc,
                std::real(result), std::imag(result), dest_type));
    } else if (ASR::is_a<ASR::ComplexConstant_t>(*left) && ASR::is_a<ASR::IntegerConstant_t>(*right)) {
        LCOMPILERS_ASSERT(op == ASR::binopType::Pow);
        ASR::ComplexConstant_t *lc = ASR::down_cast<ASR::ComplexConstant_t>(left);
        int64_t n = ASR::down_cast<ASR::IntegerConstant_t>(right)->m_n;
        std::complex<double> result;
        if (ASRUtils::extract_kind_from_ttype_t(dest_type) == 4) {
            // Evaluate in single precision, as the program would.
            result = complex_integer_pow<float>(static_cast<float>(lc->m_re),
                static_cast<float>(lc->m_im), n);
        } else {
            result = complex_integer_pow<double>(lc->m_re, lc->m_im, n);
        }
        return ASRUtils::EXPR( ASR::make_ComplexConstant_t(al, loc,
                std::real(result), std::imag(result), dest_type));
    }
    return nullptr;
}

template<typename T>
static bool perform_compare(T left, T right, ASR::cmpopType op) {
    switch (op) {
        case ASR::cmpopType::Eq: return left == right;
        case ASR::cmpopType::NotEq: return left != right;
        case ASR::cmpopType::Gt: return left > right;
        case ASR::cmpopType::GtE: return left >= right;
        case ASR::cmpopType::Lt: return left < right;
        case ASR::cmpopType::LtE: return left <= right;
    }
    LCOMPILERS_ASSERT(false);
    return false;
}

ASR::expr_t* fold_compare_constants(Allocator &al, ASR::expr_t* left,
        ASR::expr_t* right, ASR::cmpopType op, const Location& loc,
        ASR::ttype_t* logical_type) {
    bool result;
    if (ASR::is_a<ASR::IntegerConstant_t>(*left)
            && ASR::is_a<ASR::IntegerConstant_t>(*right)) {
        result = perform_compare(ASR::down_cast<ASR::IntegerConstant_t>(left)->m_n,
            ASR::down_cast<ASR::IntegerConstant_t>(right)->m_n, op);
    } else if (ASR::is_a<ASR::RealConstant_t>(*left)
            && ASR::is_a<ASR::RealConstant_t>(*right)) {
        ASR::RealConstant_t* lc = ASR::down_cast<ASR::RealConstant_t>(left);
        ASR::RealConstant_t* rc = ASR::down_cast<ASR::RealConstant_t>(right);
        if (ASRUtils::extract_kind_from_ttype_t(lc->m_type) == 16) {
            lf_float128 lv = ASRUtils::real_constant_get_r16(lc);
            lf_float128 rv = ASRUtils::real_constant_get_r16(rc);
            if (op == ASR::cmpopType::Eq || op == ASR::cmpopType::NotEq) {
                result = (lf_f128_eq(lv, rv) != 0) == (op == ASR::cmpopType::Eq);
            } else {
                result = perform_compare(lf_f128_cmp(lv, rv), 0, op);
            }
        } else if (ASRUtils::extract_kind_from_ttype_t(lc->m_type) == 10) {
            result = perform_compare(ASRUtils::real_constant_get_r10(lc),
                ASRUtils::real_constant_get_r10(rc), op);
        } else {
            result = perform_compare(lc->m_r, rc->m_r, op);
        }
    } else if (ASR::is_a<ASR::LogicalConstant_t>(*left)
            && ASR::is_a<ASR::LogicalConstant_t>(*right)) {
        result = perform_compare(ASR::down_cast<ASR::LogicalConstant_t>(left)->m_value,
            ASR::down_cast<ASR::LogicalConstant_t>(right)->m_value, op);
    } else {
        return nullptr;
    }
    return ASRUtils::EXPR(ASR::make_LogicalConstant_t(al, loc, result,
        logical_type));
}

bool fold_logical_binop(ASR::logicalbinopType op, bool left, bool right,
        bool &result) {
    switch (op) {
        case ASR::And: result = left && right; return true;
        case ASR::Or: result = left || right; return true;
        case ASR::NEqv: result = left != right; return true;
        case ASR::Eqv: result = left == right; return true;
        default: return false;
    }
}

bool reads_valueless_parameter(ASR::expr_t* e) {
    class Finder : public ASR::BaseWalkVisitor<Finder> {
        public:
            bool found = false;
            void visit_Var(const ASR::Var_t &x) {
                ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(x.m_v);
                if (ASR::is_a<ASR::Variable_t>(*sym)) {
                    ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
                    if (var->m_storage == ASR::storage_typeType::Parameter
                            && var->m_value == nullptr) {
                        found = true;
                    }
                }
            }
    };
    if (e == nullptr) {
        return false;
    }
    Finder finder;
    finder.visit_expr(*e);
    return finder.found;
}

bool reads_variable_value(ASR::expr_t* e) {
    class Finder : public ASR::BaseWalkVisitor<Finder> {
        public:
            bool found = false;
            void visit_expr(const ASR::expr_t &x) {
                if (!ASRUtils::is_value_constant(
                        ASRUtils::expr_value(const_cast<ASR::expr_t*>(&x)))) {
                    ASR::BaseWalkVisitor<Finder>::visit_expr(x);
                }
            }
            // An inquiry about a variable, e.g. `size(a)`, `kind(x)` or
            // `len(s)`, does not read its value.
            void visit_inquired(ASR::expr_t *arg) {
                if (!ASR::is_a<ASR::Var_t>(*arg)) {
                    visit_expr(*arg);
                }
            }
            void visit_ArraySize(const ASR::ArraySize_t &x) {
                visit_inquired(x.m_v);
                if (x.m_dim) visit_expr(*x.m_dim);
            }
            void visit_ArrayBound(const ASR::ArrayBound_t &x) {
                visit_inquired(x.m_v);
                if (x.m_dim) visit_expr(*x.m_dim);
            }
            void visit_StringLen(const ASR::StringLen_t &x) {
                visit_inquired(x.m_arg);
            }
            void visit_ArrayRank(const ASR::ArrayRank_t &/*x*/) {
            }
            void visit_TypeInquiry(const ASR::TypeInquiry_t &/*x*/) {
            }
            void visit_Var(const ASR::Var_t &x) {
                ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(x.m_v);
                if (ASR::is_a<ASR::Variable_t>(*sym)
                        && ASR::down_cast<ASR::Variable_t>(sym)->m_storage
                            != ASR::storage_typeType::Parameter) {
                    found = true;
                }
            }
    };
    if (e == nullptr) {
        return false;
    }
    Finder finder;
    finder.visit_expr(*e);
    return finder.found;
}

bool depends_on_deferred_constant(const ASR::Variable_t* v) {
    if (v->m_storage != ASR::storage_typeType::Parameter
            || v->m_value != nullptr || v->m_symbolic_value == nullptr) {
        return false;
    }
    class Finder : public ASR::BaseWalkVisitor<Finder> {
        public:
            bool found = false;
            void visit_Var(const ASR::Var_t &x) {
                ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(x.m_v);
                if (ASR::is_a<ASR::Variable_t>(*sym)) {
                    ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
                    if (is_deferred_constant(var)
                            || depends_on_deferred_constant(var)) {
                        found = true;
                    }
                }
            }
    };
    Finder finder;
    finder.visit_expr(*v->m_symbolic_value);
    return finder.found;
}

ASR::symbol_t* import_class_procedure(Allocator &al, const Location& loc,
        ASR::symbol_t* original_sym, SymbolTable *current_scope) {
    if (original_sym && ASR::is_a<ASR::ExternalSymbol_t>(*original_sym)) {
        ASR::symbol_t* original_sym_past_ext = ASRUtils::symbol_get_past_external(original_sym);
        if (ASR::is_a<ASR::StructMethodDeclaration_t>(*original_sym_past_ext) ||
            (ASR::is_a<ASR::Variable_t>(*original_sym_past_ext) &&
             ASR::is_a<ASR::FunctionType_t>(*ASRUtils::type_get_past_pointer(ASRUtils::symbol_type(original_sym_past_ext))))) {
            original_sym = original_sym_past_ext;
        }
    }
    if( original_sym && (ASR::is_a<ASR::StructMethodDeclaration_t>(*original_sym) ||
        (ASR::is_a<ASR::Variable_t>(*original_sym) &&
         ASR::is_a<ASR::FunctionType_t>(*ASRUtils::type_get_past_pointer(ASRUtils::symbol_type(original_sym))))) ) {
        std::string class_proc_name;
        if (ASR::is_a<ASR::StructMethodDeclaration_t>(*original_sym)) {
            class_proc_name = std::string(ASR::down_cast<ASR::StructMethodDeclaration_t>(original_sym)->m_name);
        } else {
            class_proc_name = ASRUtils::symbol_name(original_sym);
        }
        if( original_sym != current_scope->resolve_symbol(class_proc_name) ) {
            std::string struct_name = ASRUtils::symbol_name(ASRUtils::get_asr_owner(original_sym));
            std::string imported_proc_name = "1_" + struct_name + "_" + class_proc_name;
            if( current_scope->resolve_symbol(imported_proc_name) == nullptr ) {
                ASR::symbol_t* module_sym = ASRUtils::get_asr_owner(original_sym);
                std::string module_name = ASRUtils::symbol_name(module_sym);
                if( current_scope->resolve_symbol(module_name) == nullptr ) {
                    std::string imported_module_name = "1_" + module_name;
                    if( current_scope->resolve_symbol(imported_module_name) == nullptr ) {
                        LCOMPILERS_ASSERT(ASR::is_a<ASR::Module_t>(
                            *ASRUtils::get_asr_owner(module_sym)));
                        ASR::symbol_t* imported_module = ASR::down_cast<ASR::symbol_t>(
                            ASR::make_ExternalSymbol_t(
                                al, loc, current_scope, s2c(al, imported_module_name),
                                module_sym, ASRUtils::symbol_name(ASRUtils::get_asr_owner(module_sym)),
                                nullptr, 0, s2c(al, module_name), ASR::accessType::Public
                            )
                        );
                        current_scope->add_symbol(imported_module_name, imported_module);
                    }
                    module_name = imported_module_name;
                }
                // Use the owning module's name as module_name and encode
                // the struct path in scope_names. This avoids ambiguity
                // during deserialization when a struct and a top-level module
                // share the same name.
                std::string ext_module_name = module_name;
                char** scope_names_arr = nullptr;
                size_t n_scope_names = 0;
                ASR::symbol_t* struct_owner = ASRUtils::get_asr_owner(
                    ASRUtils::symbol_get_past_external(module_sym));
                if (struct_owner != nullptr && ASR::is_a<ASR::Module_t>(*struct_owner)) {
                    ext_module_name = ASRUtils::symbol_name(struct_owner);
                    scope_names_arr = al.allocate<char*>(1);
                    scope_names_arr[0] = s2c(al, struct_name);
                    n_scope_names = 1;
                }
                ASR::symbol_t* imported_sym = ASR::down_cast<ASR::symbol_t>(
                    ASR::make_ExternalSymbol_t(
                        al, loc, current_scope, s2c(al, imported_proc_name),
                        original_sym, s2c(al, ext_module_name), scope_names_arr, n_scope_names,
                        ASRUtils::symbol_name(original_sym), ASR::accessType::Public
                    )
                );
                current_scope->add_symbol(imported_proc_name, imported_sym);
                original_sym = imported_sym;
            } else {
                original_sym = current_scope->resolve_symbol(imported_proc_name);
            }
        }
    }
    return original_sym;
}

ASR::asr_t* make_Binop_util(Allocator &al, const Location& loc, ASR::binopType binop,
                            ASR::expr_t* lexpr, ASR::expr_t* rexpr, ASR::ttype_t* ttype) {
    ASRUtils::make_ArrayBroadcast_t_util(al, loc, lexpr, rexpr);

    ASR::ttype_t* underlying_type = ASRUtils::type_get_past_allocatable_pointer(ttype);
    ASR::ttype_t* element_type = ASRUtils::type_get_past_array(underlying_type);

    switch (element_type->type) {
        case ASR::ttypeType::Real: {
            return ASR::make_RealBinOp_t(al, loc, lexpr, binop, rexpr,
                ASRUtils::duplicate_type(al, ttype), nullptr);
        }
        case ASR::ttypeType::Integer: {
            return ASR::make_IntegerBinOp_t(al, loc, lexpr, binop, rexpr,
                ASRUtils::duplicate_type(al, ttype), nullptr);
        }
        case ASR::ttypeType::Complex: {
            return ASR::make_ComplexBinOp_t(al, loc, lexpr, binop, rexpr,
                ASRUtils::duplicate_type(al, ttype), nullptr);
        }
        default:
            throw LCompilersException("Not implemented " + ASRUtils::type_to_str_python_expr(ttype, lexpr));
    }
}

ASR::asr_t* make_Cmpop_util(Allocator &al, const Location& loc, ASR::cmpopType cmpop,
                        ASR::expr_t* lexpr, ASR::expr_t* rexpr, ASR::ttype_t* ttype) {
    ASR::ttype_t* expr_type = ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4));
    switch (ttype->type) {
        case ASR::ttypeType::Real: {
            return ASR::make_RealCompare_t(al, loc, lexpr, cmpop, rexpr, expr_type, nullptr);
        }
        case ASR::ttypeType::Integer: {
            return ASR::make_IntegerCompare_t(al, loc, lexpr, cmpop, rexpr, expr_type, nullptr);
        }
        case ASR::ttypeType::Complex: {
            return ASR::make_ComplexCompare_t(al, loc, lexpr, cmpop, rexpr, expr_type, nullptr);
        }
        case ASR::ttypeType::String: {
            return ASR::make_StringCompare_t(al, loc, lexpr, cmpop, rexpr, expr_type, nullptr);
        }
        default:
            throw LCompilersException("Not implemented " + ASRUtils::type_to_str_python_expr(ttype, lexpr));
    }
}

void mark_modules_as_external(const LCompilers::ASR::TranslationUnit_t &u)
{
    Allocator al(4*1024);
    for (auto &item : u.m_symtab->get_scope()) {
        if (LCompilers::ASR::is_a<LCompilers::ASR::Module_t>(*item.second)) {
            LCompilers::ASR::Module_t *m = LCompilers::ASR::down_cast<LCompilers::ASR::Module_t>(item.second);
            m->m_symtab->mark_all_variables_external(al);
        }
    }
}

void make_ArrayBroadcast_t_util(Allocator& al, const Location& loc,
    ASR::expr_t*& expr1, ASR::expr_t*& expr2, ASR::dimension_t* expr1_mdims,
    size_t expr1_ndims) {
    ASR::ttype_t* expr1_type = ASRUtils::expr_type(expr1);
    Vec<ASR::expr_t*> shape_args;
    shape_args.reserve(al, 1);
    shape_args.push_back(al, expr1);
    bool is_value_character_array = ASRUtils::is_character(*ASRUtils::expr_type(expr2));

    Vec<ASR::dimension_t> dims;
    dims.reserve(al, 1);
    ASR::dimension_t dim;
    dim.loc = loc;
    dim.m_length = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
        expr1_ndims, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4))));
    dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
        1, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4))));
    dims.push_back(al, dim);
    // The shape is a constant-size array of integers, whatever the type of
    // the broadcast value is.
    ASR::ttype_t* dest_shape_type = ASRUtils::make_Array_t_util(al, loc,
        ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)), dims.p, dims.size());

    ASR::expr_t* dest_shape = nullptr;
    ASR::expr_t* value = nullptr;
    ASR::ttype_t* ret_type = nullptr;
    if( ASRUtils::is_fixed_size_array(expr1_mdims, expr1_ndims) ) {
        Vec<ASR::expr_t*> lengths; lengths.reserve(al, expr1_ndims);
        for( size_t i = 0; i < expr1_ndims; i++ ) {
            lengths.push_back(al, ASRUtils::expr_value(expr1_mdims[i].m_length));
        }
        dest_shape = EXPR(ASRUtils::make_ArrayConstructor_t_util(al, loc, lengths.p,
            lengths.size(), dest_shape_type, ASR::arraystorageType::ColMajor));
        Vec<ASR::dimension_t> dims;
        dims.reserve(al, 1);
        ASR::dimension_t dim;
        dim.loc = loc;
        dim.m_length = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
            ASRUtils::get_fixed_size_of_array(expr1_mdims, expr1_ndims),
            ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4))));
        dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
            1, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4))));
        dims.push_back(al, dim);

        if( ASRUtils::is_value_constant(expr2) &&
            ASRUtils::get_fixed_size_of_array(expr1_mdims, expr1_ndims) <= 256 ) {
            ASR::ttype_t* value_type = ASRUtils::TYPE(ASR::make_Array_t(al, loc,
                ASRUtils::type_get_past_array(ASRUtils::expr_type(expr2)), dims.p, dims.size(),
                is_value_character_array ? ASR::array_physical_typeType::PointerArray: ASR::array_physical_typeType::FixedSizeArray, ASR::memory_spaceType::Global));
            Vec<ASR::expr_t*> values;
            values.reserve(al, ASRUtils::get_fixed_size_of_array(expr1_mdims, expr1_ndims));
            for( int64_t i = 0; i < ASRUtils::get_fixed_size_of_array(expr1_mdims, expr1_ndims); i++ ) {
                values.push_back(al, expr2);
            }
            value = EXPR(ASRUtils::make_ArrayConstructor_t_util(al, loc, values.p,
                values.size(), value_type, ASR::arraystorageType::ColMajor));
            if (ASR::is_a<ASR::ArrayConstructor_t>(*value) && ASRUtils::expr_value(value)) {
                value = ASRUtils::expr_value(value);
            }
            ret_type = value_type;
        }
    } else {
        dest_shape = ASRUtils::EXPR(ASR::make_IntrinsicArrayFunction_t(al, loc,
            static_cast<int64_t>(ASRUtils::IntrinsicArrayFunctions::Shape), shape_args.p,
            shape_args.size(), 0, dest_shape_type, nullptr));
    }

    if (ret_type == nullptr) {
        // TODO: Construct appropriate return type here
        // For now simply coping the type from expr1
        if (ASRUtils::is_simd_array(expr1)) {
            // The broadcast is an ordinary array of the target's constant
            // shape, which is cast to the SIMD target below.
            // TODO: Make this more general; do not check for SIMDArray
            ret_type = ASRUtils::duplicate_type(al, expr1_type, nullptr,
                ASR::array_physical_typeType::FixedSizeArray, true);
        } else {
            ret_type = expr1_type;
        }
    }
    expr2 = ASRUtils::EXPR(ASR::make_ArrayBroadcast_t(al, loc, expr2, dest_shape, ret_type, value));

    if (ASRUtils::extract_physical_type(expr1_type) != ASRUtils::extract_physical_type(ret_type)) {
        expr2 = ASRUtils::EXPR(ASRUtils::make_ArrayPhysicalCast_t_util(al, loc, expr2,
            ASRUtils::extract_physical_type(ret_type),
            ASRUtils::extract_physical_type(expr1_type), expr1_type, nullptr));
    }
}

void make_ArrayBroadcast_t_util(Allocator& al, const Location& loc,
    ASR::expr_t*& expr1, ASR::expr_t*& expr2) {
    ASR::ttype_t* expr1_type = ASRUtils::expr_type(expr1);
    ASR::ttype_t* expr2_type = ASRUtils::expr_type(expr2);
    ASR::dimension_t *expr1_mdims = nullptr, *expr2_mdims = nullptr;
    size_t expr1_ndims = ASRUtils::extract_dimensions_from_ttype(expr1_type, expr1_mdims);
    size_t expr2_ndims = ASRUtils::extract_dimensions_from_ttype(expr2_type, expr2_mdims);
    if( expr1_ndims == expr2_ndims ) {
        // TODO: Always broadcast both the expressions
        return ;
    }

    if( expr1_ndims > expr2_ndims ) {
        if( ASR::is_a<ASR::ArrayReshape_t>(*expr2) ) {
            return ;
        }
        make_ArrayBroadcast_t_util(al, loc, expr1, expr2, expr1_mdims, expr1_ndims);
    } else {
        if( ASR::is_a<ASR::ArrayReshape_t>(*expr1) ) {
            return ;
        }
        make_ArrayBroadcast_t_util(al, loc, expr2, expr1, expr2_mdims, expr2_ndims);
    }
}

int64_t compute_trailing_zeros(int64_t number, int64_t kind) {
    int64_t trailing_zeros = 0;
    if (number == 0 && kind == 4) {
        return 32;
    } else if (number == 0 && kind == 8) {
        return 64;
    }
    while (number % 2 == 0) {
        number = number / 2;
        trailing_zeros++;
    }
    return trailing_zeros;
}

int64_t compute_leading_zeros(int64_t number, int64_t kind) {
    int64_t leading_zeros = 0;
    int64_t total_bits = 32;
    if (kind == 8) total_bits = 64;
    if (number < 0) return 0;
    while (total_bits > 0) {
        if (number%2 == 0) {
            leading_zeros++;
        } else {
            leading_zeros = 0;
        }
        number = number/2;
        total_bits--;
    }
    return leading_zeros;
}

void append_error(diag::Diagnostics& diag, const std::string& msg,
                const Location& loc) {
    diag.add(diag::Diagnostic(msg, diag::Level::Error,
        diag::Stage::Semantic, {diag::Label("", { loc })}));
}

size_t get_constant_ArrayConstant_size(ASR::ArrayConstant_t* x) {
    return ASRUtils::get_fixed_size_of_array(x->m_type);
}

ASR::expr_t* get_ArrayConstant_size(Allocator& al, ASR::ArrayConstant_t* x) {
    ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x->base.base.loc, 4));
    return make_ConstantWithType(make_IntegerConstant_t,
            ASRUtils::get_fixed_size_of_array(x->m_type), int_type, x->base.base.loc);
}

ASR::expr_t* get_ImpliedDoLoop_size(Allocator& al, ASR::ImpliedDoLoop_t* implied_doloop) {
    const Location& loc = implied_doloop->base.base.loc;
    ASRUtils::ASRBuilder builder(al, loc);
    ASR::expr_t* start = implied_doloop->m_start;
    ASR::expr_t* end = implied_doloop->m_end;
    ASR::expr_t* d = implied_doloop->m_increment;
    ASR::expr_t* implied_doloop_size = nullptr;
    int kind = ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(end));
    start = builder.i2i_t(start, ASRUtils::expr_type(end));
    if( d == nullptr ) {
        implied_doloop_size = ASRUtils::compute_length_from_start_end(al, start, end);
    } else {
        d = builder.i2i_t(d, ASRUtils::expr_type(end));
        implied_doloop_size = builder.Add(builder.Div(
            builder.Sub(end, start), d),
            make_ConstantWithKind(make_IntegerConstant_t, make_Integer_t, 1, kind, loc));
    }
    int const_elements = 0;
    ASR::expr_t* implied_doloop_size_ = nullptr;
    for( size_t i = 0; i < implied_doloop->n_values; i++ ) {
        if( ASR::is_a<ASR::ImpliedDoLoop_t>(*implied_doloop->m_values[i]) ) {
            if( implied_doloop_size_ == nullptr ) {
                implied_doloop_size_ = get_ImpliedDoLoop_size(al,
                    ASR::down_cast<ASR::ImpliedDoLoop_t>(implied_doloop->m_values[i]));
            } else {
                implied_doloop_size_ = builder.Add(get_ImpliedDoLoop_size(al,
                    ASR::down_cast<ASR::ImpliedDoLoop_t>(implied_doloop->m_values[i])),
                    implied_doloop_size_);
            }
        } else if( ASR::is_a<ASR::ArrayConstructor_t>(*implied_doloop->m_values[i]) ) {
            ASR::expr_t* ac_size = get_ArrayConstructor_size(al,
                ASR::down_cast<ASR::ArrayConstructor_t>(implied_doloop->m_values[i]));
            if( implied_doloop_size_ == nullptr ) {
                implied_doloop_size_ = ac_size;
            } else {
                implied_doloop_size_ = builder.Add(ac_size, implied_doloop_size_);
            }
        } else {
            const_elements += 1;
        }
    }

    ASR::symbol_t* loop_var_sym = nullptr;
    if (ASR::is_a<ASR::Var_t>(*implied_doloop->m_var)) {
        loop_var_sym = ASR::down_cast<ASR::Var_t>(implied_doloop->m_var)->m_v;
    }
    bool inner_depends_on_var = (loop_var_sym != nullptr &&
        implied_doloop_size_ != nullptr && d == nullptr &&
        expr_references_symbol(implied_doloop_size_, loop_var_sym));

    if (inner_depends_on_var) {
        ASR::expr_t* two = make_ConstantWithKind(
            make_IntegerConstant_t, make_Integer_t, 2, kind, loc);
        ASR::expr_t* arith_sum = builder.Div(
            builder.Mul(implied_doloop_size, builder.Add(start, end)), two);
        if (const_elements > 0) {
            ASR::expr_t* const_total = builder.Mul(
                make_ConstantWithKind(make_IntegerConstant_t,
                    make_Integer_t, const_elements, kind, loc),
                implied_doloop_size);
            implied_doloop_size = builder.Add(arith_sum, const_total);
        } else {
            implied_doloop_size = arith_sum;
        }
    } else {
        if( const_elements > 0 ) {
            if( implied_doloop_size_ == nullptr ) {
                implied_doloop_size_ = make_ConstantWithKind(make_IntegerConstant_t,
                    make_Integer_t, const_elements, kind, loc);
            } else {
                implied_doloop_size_ = builder.Add(
                    make_ConstantWithKind(make_IntegerConstant_t,
                        make_Integer_t, const_elements, kind, loc),
                    implied_doloop_size_);
            }
        }
        if( implied_doloop_size_ ) {
            implied_doloop_size = builder.Mul(implied_doloop_size_, implied_doloop_size);
        }
    }
    return implied_doloop_size;
}

ASR::expr_t* get_ArrayConstructor_size(Allocator& al, ASR::ArrayConstructor_t* x) {
    ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x->base.base.loc, 4));
    ASR::expr_t* array_size = nullptr;
    int64_t constant_size = 0;
    const Location& loc = x->base.base.loc;
    ASRUtils::ASRBuilder builder(al, loc);
    for( size_t i = 0; i < x->n_args; i++ ) {
        ASR::expr_t* element = x->m_args[i];
        if( ASR::is_a<ASR::ArrayConstant_t>(*element) ) {
            if( ASRUtils::is_value_constant(element) ) {
                constant_size += get_constant_ArrayConstant_size(
                    ASR::down_cast<ASR::ArrayConstant_t>(element));
            } else {
                ASR::expr_t* element_array_size = get_ArrayConstant_size(al,
                    ASR::down_cast<ASR::ArrayConstant_t>(element));
                if( array_size == nullptr ) {
                    array_size = element_array_size;
                } else {
                    array_size = builder.Add(array_size,
                                    element_array_size);
                }
            }
        } else if( ASR::is_a<ASR::ArrayConstructor_t>(*element) ) {
            ASR::expr_t* element_array_size = get_ArrayConstructor_size(al,
                ASR::down_cast<ASR::ArrayConstructor_t>(element));
            if( array_size == nullptr ) {
                array_size = element_array_size;
            } else {
                array_size = builder.Add(array_size,
                                element_array_size);
            }
        } else if( ASR::is_a<ASR::Var_t>(*element) ) {
            ASR::ttype_t* element_type = ASRUtils::type_get_past_allocatable(
                ASRUtils::expr_type(element));
            if( ASRUtils::is_array(element_type) ) {
                if( ASRUtils::is_fixed_size_array(element_type) ) {
                    ASR::dimension_t* m_dims = nullptr;
                    size_t n_dims = ASRUtils::extract_dimensions_from_ttype(element_type, m_dims);
                    constant_size += ASRUtils::get_fixed_size_of_array(m_dims, n_dims);
                } else {
                    ASR::expr_t* element_array_size = ASRUtils::get_size(element, al);
                    if( array_size == nullptr ) {
                        array_size = element_array_size;
                    } else {
                        array_size = builder.Add(array_size,
                                        element_array_size);
                    }
                }
            } else {
                constant_size += 1;
            }
        } else if( ASR::is_a<ASR::ImpliedDoLoop_t>(*element) ) {
            ASR::expr_t* implied_doloop_size = get_ImpliedDoLoop_size(al,
                ASR::down_cast<ASR::ImpliedDoLoop_t>(element));
            if( array_size ) {
                array_size = builder.Add(implied_doloop_size, array_size);
            } else {
                array_size = implied_doloop_size;
            }
        } else if( ASR::is_a<ASR::ArraySection_t>(*element) ) {
            ASR::ArraySection_t* array_section_t = ASR::down_cast<ASR::ArraySection_t>(element);
            ASR::expr_t* array_section_size = nullptr;
            for( size_t j = 0; j < array_section_t->n_args; j++ ) {
                ASR::expr_t* start = array_section_t->m_args[j].m_left;
                ASR::expr_t* end = array_section_t->m_args[j].m_right;
                ASR::expr_t* d = array_section_t->m_args[j].m_step;
                if( d == nullptr ) {
                    continue;
                }
                ASR::expr_t* dim_size = builder.Add(builder.Div(
                    builder.Sub(end, start), d),
                    make_ConstantWithKind(make_IntegerConstant_t, make_Integer_t, 1, 4, loc));
                if( array_section_size == nullptr ) {
                    array_section_size = dim_size;
                } else {
                    array_section_size = builder.Mul(array_section_size, dim_size);
                }
            }
            if( array_size == nullptr ) {
                array_size = array_section_size;
            } else {
                array_size = builder.Add(array_section_size, array_size);
            }
        } else {
            ASR::ttype_t* element_type = ASRUtils::type_get_past_allocatable(
                ASRUtils::type_get_past_pointer(ASRUtils::expr_type(element)));
            if( ASRUtils::is_array(element_type) ) {
                if( ASRUtils::is_fixed_size_array(element_type) ) {
                    ASR::dimension_t* m_dims = nullptr;
                    size_t n_dims = ASRUtils::extract_dimensions_from_ttype(element_type, m_dims);
                    constant_size += ASRUtils::get_fixed_size_of_array(m_dims, n_dims);
                } else {
                    ASR::expr_t* element_array_size = ASRUtils::get_size(element, al);
                    if( array_size == nullptr ) {
                        array_size = element_array_size;
                    } else {
                        array_size = builder.Add(array_size, element_array_size);
                    }
                }
            } else {
                constant_size += 1;
            }
        }
    }
    ASR::expr_t* constant_size_asr = nullptr;
    if (constant_size == 0 && array_size == nullptr) {
        constant_size = ASRUtils::get_fixed_size_of_array(x->m_type);
    }
    if( constant_size > 0 ) {
        constant_size_asr = make_ConstantWithType(make_IntegerConstant_t,
                                constant_size, int_type, x->base.base.loc);
        if( array_size == nullptr ) {
            return constant_size_asr;
        }
    }
    if( constant_size_asr ) {
        array_size = builder.Add(array_size, constant_size_asr);
    }

    if( array_size == nullptr ) {
        array_size = make_ConstantWithKind(make_IntegerConstant_t,
            make_Integer_t, 0, 4, x->base.base.loc);
    }
    return array_size;
}

// The shape of `matmul` and of `transpose` follows entirely from the
// shapes of their operands, and neither carries dimensions of its own in
// its result type, so a `size(...)` query on one has to be answered from
// the operands: matmul is (n,k)x(k,m) -> (n,m), (n,k)x(k) -> (n) and
// (k)x(k,m) -> (m), and transpose swaps the two extents of its argument.
// `spread(source, dim, ncopies)` inserts the extent `ncopies` into the
// shape of `source` at position `dim`; its extents are built by the same
// `Spread::result_extent` as its result type, but with the extents of
// `source` answered by `make_ArraySize_t_util` and a negative `ncopies`
// taken as 0.
// The general rule in `make_ArraySize_t_util` -- the size of the first
// array argument, which holds for the intrinsics whose result has the
// shape of their argument -- would answer `size(matmul(a, b))` with the
// size of `a`.  Returns nullptr for every other intrinsic, and for an
// operand rank combination that is not one of matmul's three.
static ASR::asr_t* array_intrinsic_shape_size(Allocator &al,
    const Location &a_loc, ASR::IntrinsicArrayFunction_t* af,
    ASR::expr_t* a_dim, ASR::ttype_t* a_type, bool for_type) {
    // Each dimension of the result, as the operand and the 1-based
    // dimension of that operand whose extent it is; for `spread` the
    // operand is null and the extent is built by `Spread::result_extent`.
    std::vector<std::pair<ASR::expr_t*, int64_t>> shape;
    ASRUtils::IntrinsicArrayFunctions id =
        static_cast<ASRUtils::IntrinsicArrayFunctions>(af->m_arr_intrinsic_id);
    if( id == ASRUtils::IntrinsicArrayFunctions::MatMul ) {
        if( af->n_args != 2 ) return nullptr;
        ASR::expr_t* a = ASRUtils::get_past_array_physical_cast(af->m_args[0]);
        ASR::expr_t* b = ASRUtils::get_past_array_physical_cast(af->m_args[1]);
        size_t ra = ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(a));
        size_t rb = ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(b));
        if( ra == 2 && rb == 2 ) {
            shape.push_back({a, 1});
            shape.push_back({b, 2});
        } else if( ra == 2 && rb == 1 ) {
            shape.push_back({a, 1});
        } else if( ra == 1 && rb == 2 ) {
            shape.push_back({b, 2});
        } else {
            return nullptr;
        }
    } else if( id == ASRUtils::IntrinsicArrayFunctions::Transpose ) {
        if( af->n_args != 1 ) return nullptr;
        ASR::expr_t* a = ASRUtils::get_past_array_physical_cast(af->m_args[0]);
        if( ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(a)) != 2 ) {
            return nullptr;
        }
        shape.push_back({a, 2});
        shape.push_back({a, 1});
    } else if( id == ASRUtils::IntrinsicArrayFunctions::Spread ) {
        if( af->n_args != 3 ) return nullptr;
        ASR::expr_t* source = ASRUtils::get_past_array_physical_cast(af->m_args[0]);
        size_t rs = ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(source));
        size_t rr = ASRUtils::extract_n_dims_from_ttype(af->m_type);
        // A scalar `source` is stored as a one-element array, so the
        // result has the rank of the stored `source`.
        if( rr != rs && rr != rs + 1 ) return nullptr;
        for( size_t i = 1; i <= rr; i++ ) {
            shape.push_back({nullptr, i});
        }
    } else {
        return nullptr;
    }
    ASRBuilder builder(al, a_loc);
    ASR::ttype_t* int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, a_loc, 4));
    // `make_ArraySize_t_util` can answer with the declared length of an
    // explicit-shape dimension, which keeps its own kind, so the extents
    // are cast to the kind `a_type` that is asked for.
    auto size_of_type = [&](ASR::expr_t* size) {
        return CastingUtil::perform_casting(size, a_type, al, a_loc);
    };
    auto extent = [&](size_t i) {
        if( shape[i].first == nullptr ) {
            // The extent `ncopies` of `spread`, which is zero when negative.
            ASR::expr_t* ncopies = builder.Max(ASRUtils::EXPR(
                ASR::make_IntegerConstant_t(al, a_loc, 0, a_type)),
                CastingUtil::perform_casting(af->m_args[2], a_type, al, a_loc));
            ASR::expr_t* source = ASRUtils::get_past_array_physical_cast(
                af->m_args[0]);
            if( static_cast<size_t>(ASRUtils::extract_n_dims_from_ttype(
                    ASRUtils::expr_type(source))) == shape.size() ) {
                return ncopies;
            }
            ASR::expr_t* dim = af->m_args[1];
            diag::Diagnostics diag;
            ASR::expr_t* e = ASRUtils::Spread::result_extent(al, a_loc, source,
                ncopies, i, dim, ASRUtils::is_value_constant(dim),
                [&](ASR::expr_t* j) {
                    return size_of_type(ASRUtils::EXPR(make_ArraySize_t_util(
                        al, a_loc, source, j, a_type, nullptr, for_type)));
                }, diag);
            LCOMPILERS_ASSERT(!diag.has_error());
            return e;
        }
        ASR::expr_t* d = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
            al, a_loc, shape[i].second, int32_type));
        return size_of_type(ASRUtils::EXPR(make_ArraySize_t_util(al, a_loc,
            shape[i].first, d, a_type, nullptr, for_type)));
    };
    if( a_dim == nullptr ) {
        ASR::expr_t* size = extent(0);
        for( size_t i = 1; i < shape.size(); i++ ) {
            size = builder.Mul(size, extent(i));
        }
        return &(size->base);
    }
    if( shape.size() == 1 ) {
        // A rank-one result has one dimension whatever `dim` evaluates to.
        return &(extent(0)->base);
    }
    int dim = -1;
    if( ASRUtils::extract_value(ASRUtils::expr_value(a_dim), dim) ) {
        if( dim < 1 || (size_t) dim > shape.size() ) return nullptr;
        return &(extent((size_t) dim - 1)->base);
    }
    // merge(extent 1, merge(extent 2, ..., dim == 2), dim == 1)
    ASR::asr_t *merged = &(extent(shape.size() - 1)->base);
    for( size_t i = shape.size() - 1; i-- > 0; ) {
        Vec<ASR::expr_t*> merge_args; merge_args.reserve(al, 3);
        merge_args.push_back(al, extent(i));
        merge_args.push_back(al, ASRUtils::EXPR(merged));
        merge_args.push_back(al, builder.Eq(a_dim,
            builder.i_t(i + 1, ASRUtils::expr_type(a_dim))));
        diag::Diagnostics diag;
        merged = ASRUtils::Merge::create_Merge(al, a_loc, merge_args, diag);
        // The three arguments are two integer extents and a logical test
        // constructed here, so Merge cannot reject them. A failure is an
        // invariant break, not a user diagnostic to drop.
        LCOMPILERS_ASSERT(merged != nullptr);
        LCOMPILERS_ASSERT(!diag.has_error());
        if (merged == nullptr || diag.has_error()) return nullptr;
    }
    return merged;
}

// The number of elements of the subscript triplet `start:end:step`,
// max(0, (end - start + step) / step) (F2018 9.5.3.3.2). Each extent is
// clamped on its own: `(end - start) / step + 1` is one for the empty
// triplet `1:0:2`, and the product of two negative extents is positive.
static ASR::expr_t* triplet_extent(Allocator &al, const Location &a_loc,
    ASR::expr_t* start, ASR::expr_t* end, ASR::expr_t* step,
    ASR::ttype_t* a_type) {
    ASRBuilder builder(al, a_loc);
    start = CastingUtil::perform_casting(start, a_type, al, a_loc);
    end = CastingUtil::perform_casting(end, a_type, al, a_loc);
    step = CastingUtil::perform_casting(step, a_type, al, a_loc);
    ASR::expr_t* count = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, a_loc,
        ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, a_loc,
            ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, a_loc, end,
                ASR::binopType::Sub, start, a_type, nullptr)),
            ASR::binopType::Add, step, a_type, nullptr)),
        ASR::binopType::Div, step, a_type, nullptr));
    return builder.Max(ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, a_loc,
        0, a_type)), count);
}

ASR::asr_t* make_ArraySize_t_util(
    Allocator &al, const Location &a_loc, ASR::expr_t* a_v,
    ASR::expr_t* a_dim, ASR::ttype_t* a_type, ASR::expr_t* a_value,
    bool for_type) {
    int dim = -1;
    bool is_dimension_constant = (a_dim != nullptr) && ASRUtils::extract_value(
        ASRUtils::expr_value(a_dim), dim);
    ASR::ttype_t* array_func_type = nullptr;
    if( ASR::is_a<ASR::ArrayPhysicalCast_t>(*a_v) ) {
        ASR::expr_t* inner = ASR::down_cast<ASR::ArrayPhysicalCast_t>(a_v)->m_arg;
        if (ASRUtils::is_array(ASRUtils::expr_type(inner))) {
            a_v = inner;
        }
    }
    if ( ASR::is_a<ASR::IntrinsicArrayFunction_t>(*a_v) && for_type ) {
        ASR::IntrinsicArrayFunction_t* af = ASR::down_cast<ASR::IntrinsicArrayFunction_t>(a_v);
        ASR::asr_t* shape_size = array_intrinsic_shape_size(al, a_loc, af,
            a_dim, a_type, for_type);
        if( shape_size != nullptr ) return shape_size;
        int64_t dim_arg_index = ASRUtils::IntrinsicArrayFunctionRegistry::get_dim_arg_index(
            static_cast<ASRUtils::IntrinsicArrayFunctions>(af->m_arr_intrinsic_id));
        ASR::expr_t* af_dim = nullptr;
        if( dim_arg_index >= 1 && (size_t) dim_arg_index < af->n_args && af->m_args[dim_arg_index] != nullptr ) {
            af_dim = af->m_args[dim_arg_index];
        }
        if ( ASRUtils::is_array(af->m_type) ) {
            array_func_type = af->m_type;
        }
        for ( size_t i = 0; i < af->n_args; i++ ) {
            if ( ASRUtils::is_array(ASRUtils::expr_type(af->m_args[i])) ) {
                a_v = af->m_args[i];
                if ( ASR::is_a<ASR::ArrayPhysicalCast_t>(*a_v)) {
                    a_v = ASR::down_cast<ASR::ArrayPhysicalCast_t>(a_v)->m_arg;
                }
                break;
            }
        }

        if( af_dim != nullptr ) {
            ASRBuilder builder(al, a_loc);
            ASR::ttype_t* int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, a_loc, 4));
            if( a_dim == nullptr ) {
                size_t rank = ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(a_v));
                Vec<ASR::expr_t*> array_sizes; array_sizes.reserve(al, rank);
                for( size_t i = 1; i <= rank; i++ ) {
                    ASR::expr_t* i_a_dim = ASRUtils::EXPR(
                        ASR::make_IntegerConstant_t(al, a_loc, i, int32_type));
                    ASR::expr_t* i_dim_size = ASRUtils::EXPR(make_ArraySize_t_util(
                        al, a_loc, a_v, i_a_dim, a_type, nullptr, for_type));
                    array_sizes.push_back(al, i_dim_size);
                }

                rank--;
                Vec<ASR::expr_t*> merged_sizes; merged_sizes.reserve(al, rank);
                for( size_t i = 0; i < rank; i++ ) {
                    Vec<ASR::expr_t*> merge_args; merge_args.reserve(al, 3);
                    merge_args.push_back(al, array_sizes[i]);
                    merge_args.push_back(al, array_sizes[i + 1]);
                    merge_args.push_back(al, builder.Lt(builder.i32(i+1), af_dim));
                    diag::Diagnostics diag;
                    merged_sizes.push_back(al, ASRUtils::EXPR(
                        ASRUtils::Merge::create_Merge(al, a_loc, merge_args, diag)));
                }

                ASR::expr_t* size = merged_sizes[0];
                for( size_t i = 1; i < rank; i++ ) {
                    size = builder.Mul(merged_sizes[i], size);
                }

                return &(size->base);
            } else {
                ASR::expr_t *dim_size_lt = ASRUtils::EXPR(make_ArraySize_t_util(
                    al, a_loc, a_v, a_dim, a_type, nullptr, for_type));
                ASR::expr_t *dim_size_gte = ASRUtils::EXPR(make_ArraySize_t_util(
                    al, a_loc, a_v, builder.Add(a_dim, builder.i_t(1, ASRUtils::expr_type(a_dim))),
                    a_type, nullptr, for_type));
                Vec<ASR::expr_t*> merge_args; merge_args.reserve(al, 3);
                merge_args.push_back(al, dim_size_lt); merge_args.push_back(al, dim_size_gte);
                merge_args.push_back(al, builder.Lt(a_dim, af_dim));
                diag::Diagnostics diag;
                return ASRUtils::Merge::create_Merge(al, a_loc, merge_args, diag);
            }
        } else if( array_func_type != nullptr && is_dimension_constant ) {
            ASR::dimension_t* af_dims = nullptr;
            size_t af_n_dims = ASRUtils::extract_dimensions_from_ttype(
                array_func_type, af_dims);
            if( (size_t) dim >= 1 && (size_t) dim <= af_n_dims &&
                af_dims[dim - 1].m_length != nullptr ) {
                return (ASR::asr_t*) af_dims[dim - 1].m_length;
            } else if ((size_t) dim >= 1 && (size_t) dim <= af_n_dims) {
                return ASR::make_ArraySize_t(al, a_loc, a_v, a_dim, a_type, a_value);
            }
        }
    } else if( ASR::is_a<ASR::FunctionCall_t>(*a_v) && for_type ) {
        ASR::FunctionCall_t* function_call = ASR::down_cast<ASR::FunctionCall_t>(a_v);
        ASR::dimension_t* m_dims = nullptr;
        size_t n_dims = ASRUtils::extract_dimensions_from_ttype(function_call->m_type, m_dims);
        if( ASRUtils::is_fixed_size_array(function_call->m_type) ) {
            if( a_dim == nullptr ) {
                return ASR::make_IntegerConstant_t(al, a_loc,
                    ASRUtils::get_fixed_size_of_array(function_call->m_type), a_type);
            } else if( is_dimension_constant ) {
                return &(m_dims[dim - 1].m_length->base);
            }
        } else {
            if( a_dim == nullptr ) {
                if( n_dims == 0 || m_dims[0].m_length == nullptr ) {
                    return ASR::make_ArraySize_t(al, a_loc, a_v, a_dim, a_type, a_value);
                }
                ASR::expr_t* result = m_dims[0].m_length;
                for( size_t i = 1; i < n_dims; i++ ) {
                    if( m_dims[i].m_length == nullptr ) {
                        return ASR::make_ArraySize_t(al, a_loc, a_v, a_dim, a_type, a_value);
                    }
                    result = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, a_loc,
                        result, ASR::binopType::Mul, m_dims[i].m_length, a_type, nullptr));
                }
                return &(result->base);
            } else if( is_dimension_constant ) {
                if( m_dims[dim - 1].m_length == nullptr ) {
                    // For Deferred-shape allocatable return, calculate size at runtime
                    return ASR::make_ArraySize_t(al, a_loc, a_v, a_dim, a_type, a_value);
                }
                LCOMPILERS_ASSERT(m_dims[dim - 1].m_length);
                return &(m_dims[dim - 1].m_length->base);
            }
        }
    } else if( ASR::is_a<ASR::ArrayReshape_t>(*a_v) && for_type ) {
        ASR::ArrayReshape_t* array_reshape = ASR::down_cast<ASR::ArrayReshape_t>(a_v);
        ASR::dimension_t* m_dims = nullptr;
        size_t n_dims = ASRUtils::extract_dimensions_from_ttype(array_reshape->m_type, m_dims);
        if( ASRUtils::is_fixed_size_array(array_reshape->m_type) ) {
            if( a_dim == nullptr ) {
                return ASR::make_IntegerConstant_t(al, a_loc,
                    ASRUtils::get_fixed_size_of_array(array_reshape->m_type), a_type);
            } else if( is_dimension_constant ) {
                return &(m_dims[dim - 1].m_length->base);
            }
        } else {
            if( a_dim == nullptr ) {
                if( n_dims == 0 || m_dims[0].m_length == nullptr ) {
                    return ASR::make_ArraySize_t(al, a_loc, a_v, a_dim, a_type, a_value);
                }
                ASR::expr_t* result = m_dims[0].m_length;
                for( size_t i = 1; i < n_dims; i++ ) {
                    if( m_dims[i].m_length == nullptr ) {
                        return ASR::make_ArraySize_t(al, a_loc, a_v, a_dim, a_type, a_value);
                    }
                    result = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, a_loc,
                        result, ASR::binopType::Mul, m_dims[i].m_length, a_type, nullptr));
                }
                return &(result->base);
            } else if( is_dimension_constant ) {
                if( m_dims[dim - 1].m_length == nullptr ) {
                    return ASR::make_ArraySize_t(al, a_loc, a_v, a_dim, a_type, a_value);
                }
                LCOMPILERS_ASSERT(m_dims[dim - 1].m_length);
                return &(m_dims[dim - 1].m_length->base);
            }
        }
    } else if( ASR::is_a<ASR::IntrinsicElementalFunction_t>(*a_v) && for_type ) {
        ASR::IntrinsicElementalFunction_t* elemental = ASR::down_cast<ASR::IntrinsicElementalFunction_t>(a_v);
        for( size_t i = 0; i < elemental->n_args; i++ ) {
            if( ASRUtils::is_array(ASRUtils::expr_type(elemental->m_args[i])) ) {
                return make_ArraySize_t_util(al, a_loc, elemental->m_args[i], a_dim, a_type, a_value, for_type);
            }
        }
    }
    if( ASR::is_a<ASR::ArraySection_t>(*a_v) ) {
        ASR::ArraySection_t* array_section_t = ASR::down_cast<ASR::ArraySection_t>(a_v);
        if( a_dim == nullptr ) {
            ASR::asr_t* const1 = ASR::make_IntegerConstant_t(al, a_loc, 1, a_type);
            ASR::asr_t* size = const1;
            for( size_t i = 0; i < array_section_t->n_args; i++ ) {
                ASR::expr_t* start = array_section_t->m_args[i].m_left;
                ASR::expr_t* end = array_section_t->m_args[i].m_right;
                ASR::expr_t* d = array_section_t->m_args[i].m_step;
                if( (start == nullptr || end == nullptr || d == nullptr) &&
                    !ASRUtils::is_array(ASRUtils::expr_type(end))){
                    continue;
                }
                ASR::expr_t* plus1 = nullptr;
                // Case: A(:, iact) where iact is an array
                if( ASRUtils::is_array(ASRUtils::expr_type(end)) ) {
                    ASR::ttype_t* arr_type = ASRUtils::expr_type(end);
                    bool is_func_with_unknown_return = (ASR::is_a<ASR::FunctionCall_t>(*end) &&
                        ASRUtils::is_allocatable(ASRUtils::expr_type(end))) || ASR::is_a<ASR::IntrinsicArrayFunction_t>(*end);
                    if( ASRUtils::is_fixed_size_array(arr_type) ) {
                        int64_t arr_size = ASRUtils::get_fixed_size_of_array(arr_type);
                        plus1 = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, a_loc, arr_size, a_type));
                    } else {
                        plus1 = ASRUtils::EXPR(ASRUtils::make_ArraySize_t_util(al, end->base.loc, end,
                            nullptr, a_type, ASRUtils::expr_value(end), !is_func_with_unknown_return));
                    }
                } else {
                    plus1 = triplet_extent(al, a_loc, start, end, d, a_type);
                }
                size = ASR::make_IntegerBinOp_t(al, a_loc, ASRUtils::EXPR(size),
                    ASR::binopType::Mul, plus1, a_type, nullptr);
            }
            return size;
        } else if( is_dimension_constant ) {
            ASR::asr_t* const1 = ASR::make_IntegerConstant_t(al, a_loc, 1, a_type);

            // ArraySection arguments include scalar indices as well. For
            // ArraySize(section, dim=k), `k` is over the resulting section
            // rank, so we must skip scalar-indexed dimensions when mapping
            // to section args.
            ASR::array_index_t* section_arg = nullptr;
            int64_t logical_dim = 0;
            for (size_t i = 0; i < array_section_t->n_args; i++) {
                ASR::expr_t* i_start = array_section_t->m_args[i].m_left;
                ASR::expr_t* i_end = array_section_t->m_args[i].m_right;
                ASR::expr_t* i_step = array_section_t->m_args[i].m_step;
                bool is_scalar_index = (i_start == nullptr) && (i_step == nullptr)
                    && !ASRUtils::is_array(ASRUtils::expr_type(i_end));
                if (is_scalar_index) {
                    continue;
                }
                logical_dim += 1;
                if (logical_dim == dim) {
                    section_arg = &array_section_t->m_args[i];
                    break;
                }
            }

            if (section_arg == nullptr) {
                return const1;
            }

            ASR::expr_t* start = section_arg->m_left;
            ASR::expr_t* end = section_arg->m_right;
            ASR::expr_t* d = section_arg->m_step;

            // Case: A(:, iact) where iact is an array and dim = 2
            if( ASRUtils::is_array(ASRUtils::expr_type(end)) ) {
                bool is_func_with_unknown_return = (ASR::is_a<ASR::FunctionCall_t>(*end) &&
                        ASRUtils::is_allocatable(ASRUtils::expr_type(end))) || ASR::is_a<ASR::IntrinsicArrayFunction_t>(*end);
                ASR::ttype_t* arr_type = ASRUtils::expr_type(end);
                if( ASRUtils::is_fixed_size_array(arr_type) ) {
                    int64_t arr_size = ASRUtils::get_fixed_size_of_array(arr_type);
                    return ASR::make_IntegerConstant_t(al, a_loc, arr_size, a_type);
                } else {
                    return ASRUtils::make_ArraySize_t_util(al, end->base.loc, end,
                        nullptr, a_type, ASRUtils::expr_value(end), !is_func_with_unknown_return);
                }
            }

            if( start == nullptr && d == nullptr ) {
                return const1;
            }
            return &(triplet_extent(al, a_loc, start, end, d, a_type)->base);
        }
    }
    if( ASR::is_a<ASR::ArrayItem_t>(*a_v) ) {
        ASR::ArrayItem_t* array_item_t = ASR::down_cast<ASR::ArrayItem_t>(a_v);
        LCOMPILERS_ASSERT(ASRUtils::is_array(array_item_t->m_type));
        // `w%u(2)` with `w` an array is an array although its subscripts are
        // scalar: it takes one element of the component from every element of
        // the base, so it has as many elements as the base. The subscripts
        // below are the sizes of vector subscripts, which these are not.
        ASR::expr_t* shape_base = ASRUtils::struct_base_lending_shape(array_item_t);
        if( shape_base != nullptr ) {
            return make_ArraySize_t_util(al, a_loc, shape_base, a_dim, a_type,
                a_value, for_type);
        }
        if( for_type ) {
            LCOMPILERS_ASSERT(!ASRUtils::is_allocatable(array_item_t->m_type) &&
                              !ASRUtils::is_pointer(array_item_t->m_type));
        }
        // Only the vector subscripts are dimensions of the result; a
        // scalar subscript selects a single position.
        std::vector<ASR::expr_t*> vector_subscripts;
        for( size_t i = 0; i < array_item_t->n_args; i++ ) {
            ASR::expr_t* subscript = array_item_t->m_args[i].m_right;
            if( subscript && ASRUtils::is_array(ASRUtils::expr_type(subscript)) ) {
                vector_subscripts.push_back(subscript);
            }
        }
        if( a_dim == nullptr ) {
            ASR::asr_t* const1 = ASR::make_IntegerConstant_t(al, a_loc, 1, a_type);
            ASR::asr_t* size = const1;
            for( ASR::expr_t* subscript: vector_subscripts ) {
                ASR::expr_t* end = ASRUtils::EXPR(make_ArraySize_t_util(al, a_loc,
                    subscript, a_dim, a_type, nullptr, for_type));
                size = ASR::make_IntegerBinOp_t(al, a_loc, ASRUtils::EXPR(size),
                    ASR::binopType::Mul, end, a_type, nullptr);
            }
            return size;
        } else if( is_dimension_constant && dim >= 1
                && (size_t) dim <= vector_subscripts.size() ) {
            return make_ArraySize_t_util(al, a_loc, vector_subscripts[dim - 1],
                nullptr, a_type, nullptr, for_type);
        }
    }
    if( is_binop_expr(a_v) && for_type ) {
        ASR::expr_t* lhs = extract_member_from_binop(a_v, 0);
        ASR::expr_t* rhs = extract_member_from_binop(a_v, 1);
        // A scalar operand, either bare (e.g. `x > t`) or wrapped in an
        // ArrayBroadcast, has no shape; take the size from the other side.
        if( ASR::is_a<ASR::ArrayBroadcast_t>(*rhs) ||
                !ASRUtils::is_array(ASRUtils::expr_type(rhs)) ) {
            return make_ArraySize_t_util(al, a_loc, lhs, a_dim, a_type, a_value, for_type);
        } else if( ASR::is_a<ASR::ArrayBroadcast_t>(*lhs) ||
                !ASRUtils::is_array(ASRUtils::expr_type(lhs)) ) {
            return make_ArraySize_t_util(al, a_loc, rhs, a_dim, a_type, a_value, for_type);
        } else if( ASR::is_a<ASR::Var_t>(*rhs) ||
                   ASR::is_a<ASR::ArraySection_t>(*rhs) ) {
            return make_ArraySize_t_util(al, a_loc, rhs, a_dim, a_type, a_value, for_type);
        } else {
            return make_ArraySize_t_util(al, a_loc, lhs, a_dim, a_type, a_value, for_type);
        }
    } else if( is_unaryop_expr(a_v) && for_type ) {
        return make_ArraySize_t_util(al, a_loc, extract_member_from_unaryop(a_v), a_dim, a_type, a_value, for_type);
    } else if( ASR::is_a<ASR::Cast_t>(*a_v) && for_type ) {
        // Cast preserves array shape; recurse into its argument.
        return make_ArraySize_t_util(al, a_loc,
            ASR::down_cast<ASR::Cast_t>(a_v)->m_arg, a_dim, a_type, a_value, for_type);
    } else if( ASR::is_a<ASR::ArrayConstructor_t>(*a_v) && for_type ) {
        ASR::ArrayConstructor_t* array_constructor = ASR::down_cast<ASR::ArrayConstructor_t>(a_v);
        return &(get_ArrayConstructor_size(al, array_constructor)->base);
    } else {
        ASR::dimension_t* m_dims = nullptr;
        size_t n_dims = 0;
        if (array_func_type != nullptr) n_dims = ASRUtils::extract_dimensions_from_ttype(array_func_type, m_dims);
        else n_dims = ASRUtils::extract_dimensions_from_ttype(ASRUtils::expr_type(a_v), m_dims);
        bool is_dimension_dependent_only_on_arguments_ = is_dimension_dependent_only_on_arguments(m_dims, n_dims);

        bool compute_size = (is_dimension_dependent_only_on_arguments_ &&
            (is_dimension_constant || a_dim == nullptr));
        if( compute_size && for_type ) {
            ASR::dimension_t* m_dims = nullptr;
            if (array_func_type != nullptr) n_dims = ASRUtils::extract_dimensions_from_ttype(array_func_type, m_dims);
            else n_dims = ASRUtils::extract_dimensions_from_ttype(ASRUtils::expr_type(a_v), m_dims);
            if( a_dim == nullptr ) {
                ASR::asr_t* size = ASR::make_IntegerConstant_t(al, a_loc, 1, a_type);
                for( size_t i = 0; i < n_dims; i++ ) {
                    size = ASR::make_IntegerBinOp_t(al, a_loc, ASRUtils::EXPR(size),
                        ASR::binopType::Mul, m_dims[i].m_length, a_type, nullptr);
                }
                return size;
            } else if( is_dimension_constant ) {
                return (ASR::asr_t*) m_dims[dim - 1].m_length;
            }
        }
    }


    if( for_type ) {
        LCOMPILERS_ASSERT_MSG(
            ASR::is_a<ASR::Var_t>(*a_v) ||
            ASR::is_a<ASR::StructInstanceMember_t>(*a_v) ||
            ASR::is_a<ASR::FunctionParam_t>(*a_v),
            "Found ASR::exprType::" + std::to_string(a_v->type));
    }

    return ASR::make_ArraySize_t(al, a_loc, a_v, a_dim, a_type, a_value);
}

ASR::ttype_t* substitute_class_type_parameter(Allocator& al,
                                              ASR::TypeParameter_t* param,
                                              ASR::ttype_t* subs,
                                              ASR::symbol_t* subs_sym)
{
    if (!param->m_is_class || subs_sym == nullptr) return subs;
    if (!ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(subs))) return subs;
    ASR::dimension_t* m_dims = nullptr;
    size_t n_dims = ASRUtils::extract_dimensions_from_ttype(subs, m_dims);
    ASR::ttype_t* t = ASRUtils::make_StructType_t_util(al, subs->base.loc,
        subs_sym, false);
    if (n_dims > 0) {
        t = ASRUtils::make_Array_t_util(al, subs->base.loc, t, m_dims, n_dims);
    }
    return t;
}

ASR::ttype_t* make_StructType_t_util(Allocator& al,
                                     Location loc,
                                     ASR::symbol_t* derived_type_sym,
                                     bool is_cstruct)
{
    ASR::Struct_t* derived_type = ASR::down_cast<ASR::Struct_t>(
        ASRUtils::symbol_get_past_external(derived_type_sym));

    if ( derived_type->m_struct_signature != nullptr ) {
        ASR::StructType_t* struct_type = ASR::down_cast<ASR::StructType_t>(derived_type->m_struct_signature);
        // if ( struct_type->m_is_cstruct == is_cstruct ) {
        //     // return already constructed struct type
        //     return derived_type->m_struct_signature;
        // }

        if ( struct_type->m_is_cstruct != is_cstruct ) {
            /*
                This if for the cases where a struct type is declared as class(XX) in Fortran
                For the example given below, in subroutine method, `list` is used as a class type

                module list_module

                type :: list
                    type(list), pointer :: child => null()
                ! contains
                !    procedure :: method
                end type list

                contains

                subroutine method(self)
                    class(list), intent(inout) :: self
                    print *, 'associated: ', associated(self%child)
                end subroutine method

                end module list_module
            */
            ASR::StructType_t* new_struct_type = ASR::down_cast<ASR::StructType_t>(ASRUtils::duplicate_type(al, (ASR::ttype_t*) struct_type));
            new_struct_type->m_is_cstruct = is_cstruct;
            return (ASR::ttype_t*) new_struct_type;
        }
        return derived_type->m_struct_signature;
    }

    std::string derived_type_name = derived_type->m_name;

    Vec<ASR::ttype_t*> member_types;
    member_types.reserve(al, derived_type->n_members);

    for (size_t i = 0; i < derived_type->n_members; i++) {
        std::string member_name = derived_type->m_members[i];
        ASR::symbol_t* sym = derived_type->m_symtab->get_symbol(member_name);
        if (sym && ASR::is_a<ASR::Variable_t>(*sym)) {
            ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(
                ASRUtils::symbol_get_past_external(sym));
            if (ASRUtils::symbol_get_past_external(derived_type_sym) == ASRUtils::symbol_get_past_external(var->m_type_declaration)) {
                // this is self referential, so we can directly take it
                ASR::StructType_t* stype = ASR::down_cast<ASR::StructType_t>(ASRUtils::extract_type(var->m_type));
                return ASRUtils::TYPE(
                    ASR::make_StructType_t(al, loc, stype->m_data_member_types,
                                           stype->n_data_member_types,
                                           nullptr,
                                           0,
                                           is_cstruct == false ? false : stype->m_is_cstruct,
                                           stype->m_is_unlimited_polymorphic
                                           )
                );
            }
            ASR::ttype_t* member_type = var->m_type;
            // The signature is shared by every scope that uses the type, so
            // a procedure component is recorded with the opaque procedure
            // type: its interface can reference symbols of its own scope.
            if (ASR::is_a<ASR::FunctionType_t>(*ASRUtils::type_get_past_pointer(member_type))) {
                member_type = ASRUtils::make_opaque_procedure_type(al,
                    member_type->base.loc, nullptr);
                if (ASR::is_a<ASR::Pointer_t>(*var->m_type)) {
                    member_type = ASRUtils::TYPE(ASR::make_Pointer_t(al,
                        member_type->base.loc, member_type));
                }
            }
            member_types.push_back(al, member_type);
        }
    }
    return ASRUtils::TYPE(
        ASR::make_StructType_t(al,
                               loc,
                               member_types.p,
                               member_types.n,
                               nullptr,
                               0,
                               is_cstruct,
                               (derived_type_name == "~unlimited_polymorphic_type" || derived_type_name == "~assumed_type") ? true : false));
}

ASR::expr_t* get_compile_time_array_size(Allocator& al, ASR::ttype_t* array_type){
    LCOMPILERS_ASSERT(ASR::is_a<ASR::Array_t>(*
        ASRUtils::type_get_past_allocatable_pointer(array_type)));
    ASR::Array_t* array_t = ASR::down_cast<ASR::Array_t>(
        ASRUtils::type_get_past_allocatable_pointer(array_type));
    if (array_t->m_physical_type == ASR::array_physical_typeType::AssumedRankArray) {
        return nullptr;
    }
    int64_t array_size = ASRUtils::get_fixed_size_of_array(array_type);
    if(array_size != -1){
            return ASRUtils::EXPR(
                ASR::make_IntegerConstant_t(al, array_type->base.loc, array_size,
                ASRUtils::TYPE(ASR::make_Integer_t(al, array_type->base.loc, 8))));
    }
    return nullptr;
}

template<typename T>
ASR::expr_t* get_binop_size_var(ASR::expr_t* x) {
    ASR::expr_t* left = get_expr_size_expr(ASR::down_cast<T>(x)->m_left, true);
    ASR::expr_t* right = get_expr_size_expr(ASR::down_cast<T>(x)->m_right, true);
    if (left && ASRUtils::is_array(ASRUtils::expr_type(left))) {
        return get_expr_size_expr(left, true);
    } else if (right && ASRUtils::is_array(ASRUtils::expr_type(right))) {
        return get_expr_size_expr(right, true);
    }
    return nullptr;
}

// Get past expressions to get the expr which will be used to calculate ArraySize.
// This should only return one of Var, ArrayPhysicalCast, StructInstanceMember, BitCast, or ArrayConstant.
// This should never return nullptr for regular calls, nullptr is only used for traversing binop to find an array.
ASR::expr_t* get_expr_size_expr(ASR::expr_t* x, bool inside_binop /* = false*/) {
    if (ASR::is_a<ASR::Var_t>(*x) ||
        ASR::is_a<ASR::ArrayPhysicalCast_t>(*x) ||
        ASR::is_a<ASR::BitCast_t>(*x) ||
        ASR::is_a<ASR::ArrayConstant_t>(*x) ||
        ASR::is_a<ASR::ArrayConstructor_t>(*x) ||
        ASR::is_a<ASR::ArraySection_t>(*x)) {
        return x;
    }

    if (ASR::is_a<ASR::IntegerBinOp_t>(*x)) {
        return get_binop_size_var<ASR::IntegerBinOp_t>(x);
    } else if (ASR::is_a<ASR::RealBinOp_t>(*x)) {
        return get_binop_size_var<ASR::RealBinOp_t>(x);
    } else if (ASR::is_a<ASR::ComplexBinOp_t>(*x)) {
        return get_binop_size_var<ASR::ComplexBinOp_t>(x);
    } else if (ASR::is_a<ASR::LogicalBinOp_t>(*x)) {
        return get_binop_size_var<ASR::LogicalBinOp_t>(x);
    } else if (ASR::is_a<ASR::IntegerCompare_t>(*x)) {
        return get_binop_size_var<ASR::IntegerCompare_t>(x);
    } else if (ASR::is_a<ASR::RealCompare_t>(*x)) {
        return get_binop_size_var<ASR::RealCompare_t>(x);
    } else if (ASR::is_a<ASR::ComplexCompare_t>(*x)) {
        return get_binop_size_var<ASR::ComplexCompare_t>(x);
    } else if (ASR::is_a<ASR::StringCompare_t>(*x)) {
        return get_binop_size_var<ASR::StringCompare_t>(x);
    } else if (ASR::is_a<ASR::OverloadedCompare_t>(*x)) {
        return get_binop_size_var<ASR::OverloadedCompare_t>(x);
    } else if (ASR::is_a<ASR::StringConcat_t>(*x)) {
        return get_binop_size_var<ASR::StringConcat_t>(x);
    } else if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*x)) {
        ASR::expr_t* arg = ASR::down_cast<ASR::IntegerUnaryMinus_t>(x)->m_arg;
        if (ASRUtils::is_array(ASRUtils::expr_type(arg))) {
            return get_expr_size_expr(arg);
        }
    } else if (ASR::is_a<ASR::RealUnaryMinus_t>(*x)) {
        ASR::expr_t* arg = ASR::down_cast<ASR::RealUnaryMinus_t>(x)->m_arg;
        if (ASRUtils::is_array(ASRUtils::expr_type(arg))) {
            return get_expr_size_expr(arg);
        }
    } else if (ASR::is_a<ASR::Cast_t>(*x)) {
        ASR::expr_t* arg = ASR::down_cast<ASR::Cast_t>(x)->m_arg;
        if (ASRUtils::is_array(ASRUtils::expr_type(arg))) {
            return get_expr_size_expr(arg);
        }
    } else if (ASR::is_a<ASR::LogicalNot_t>(*x)) {
        ASR::expr_t* arg = ASR::down_cast<ASR::LogicalNot_t>(x)->m_arg;
        if (ASRUtils::is_array(ASRUtils::expr_type(arg))) {
            return get_expr_size_expr(arg);
        }
    } else if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*x)) {
        ASR::IntrinsicElementalFunction_t* elemental_f = ASR::down_cast<ASR::IntrinsicElementalFunction_t>(x);

        // If any argument is an array other arguments must be the same shape
        for (size_t i = 0; i < elemental_f->n_args; i++) {
            if (ASRUtils::is_array(ASRUtils::expr_type(elemental_f->m_args[i]))) {
                return get_expr_size_expr(elemental_f->m_args[i]);
            }
        }
    } else if (ASR::is_a<ASR::FunctionCall_t>(*x)) {
        ASR::FunctionCall_t* func_call = ASR::down_cast<ASR::FunctionCall_t>(x);
        if (ASRUtils::is_elemental(func_call->m_name)) {
            // If any argument is an array other arguments must be the same shape
            for (size_t i = 0; i < func_call->n_args; i++) {
                if (ASRUtils::is_array(ASRUtils::expr_type(func_call->m_args[i].m_value))) {
                    return get_expr_size_expr(func_call->m_args[i].m_value);
                }
            }
            // m_dt is also an argument
            if (func_call->m_dt && ASRUtils::is_array(ASRUtils::expr_type(func_call->m_dt))) {
                return get_expr_size_expr(func_call->m_dt);
            }
        }
    } else if (ASR::is_a<ASR::ComplexRe_t>(*x)) {
        ASR::expr_t* arg = ASR::down_cast<ASR::ComplexRe_t>(x)->m_arg;
        if (ASRUtils::is_array(ASRUtils::expr_type(arg))) {
            return get_expr_size_expr(arg);
        }
    } else if (ASR::is_a<ASR::ComplexIm_t>(*x)) {
        ASR::expr_t* arg = ASR::down_cast<ASR::ComplexIm_t>(x)->m_arg;
        if (ASRUtils::is_array(ASRUtils::expr_type(arg))) {
            return get_expr_size_expr(arg);
        }
    } else if (ASR::is_a<ASR::StructInstanceMember_t>(*x)) {
        ASR::StructInstanceMember_t* sim = ASR::down_cast<ASR::StructInstanceMember_t>(x);
        if (ASRUtils::is_array(ASRUtils::expr_type(sim->m_v))) {
            return get_expr_size_expr(sim->m_v);
        }
    }

    if (ASR::is_a<ASR::StructInstanceMember_t>(*x)) {
        return x;
    } else if (inside_binop) {
        return nullptr;
    } else {
        return nullptr;
    }
}


ASR::ttype_t* typed_expr_type(const ASR::expr_t *e)
{
    if (e == nullptr) return nullptr;
    if (!ASR::is_a<ASR::Var_t>(*e)) return ASRUtils::expr_type(e);
    ASR::symbol_t *s = ASR::down_cast<ASR::Var_t>(e)->m_v;
    if (s == nullptr) return nullptr;
    if (ASR::is_a<ASR::ExternalSymbol_t>(*s)) {
        s = ASR::down_cast<ASR::ExternalSymbol_t>(s)->m_external;
        if (s == nullptr || ASR::is_a<ASR::ExternalSymbol_t>(*s)) return nullptr;
    }
    if (!ASR::is_a<ASR::Function_t>(*s) && !ASR::is_a<ASR::Variable_t>(*s)
            && !ASR::is_a<ASR::Struct_t>(*s)) {
        return nullptr;
    }
    return ASRUtils::expr_type(e);
}

bool is_procedure_type(ASR::ttype_t *t)
{
    return t != nullptr
        && ASR::is_a<ASR::FunctionType_t>(*ASRUtils::type_get_past_array(
            ASRUtils::type_get_past_allocatable_pointer(t)));
}

bool is_struct_like_type(ASR::ttype_t *t)
{
    if (t == nullptr) return false;
    ASR::ttype_t *t2 = ASRUtils::type_get_past_array(
        ASRUtils::type_get_past_allocatable_pointer(t));
    return ASR::is_a<ASR::StructType_t>(*t2) || ASRUtils::is_class_type(t2);
}

// `check_equal_type` does not look at a character length, but a caller
// compiled against `character(len=5)` reserves five bytes for a result the
// implementation writes ten into.
static InterfaceMismatch string_length_mismatch(const std::string &what,
    ASR::ttype_t *impl, ASR::ttype_t *decl, const std::string &code)
{
    if (impl == nullptr || decl == nullptr) return {};
    if (!ASRUtils::is_character(*impl) || !ASRUtils::is_character(*decl)) {
        return {};
    }
    ASR::String_t *a = ASRUtils::get_string_type(impl);
    ASR::String_t *b = ASRUtils::get_string_type(decl);
    if (a == nullptr || b == nullptr) return {};
    int64_t a_len = -1, b_len = -1;
    if (a->m_len == nullptr || b->m_len == nullptr) return {};
    if (!ASRUtils::extract_value(ASRUtils::expr_value(a->m_len), a_len) ||
            !ASRUtils::extract_value(ASRUtils::expr_value(b->m_len), b_len)) {
        return {};
    }
    if (a_len == b_len) return {};
    return {true, code, what + " must have character length " +
        std::to_string(b_len) + ", not " + std::to_string(a_len)};
}

// The dummy argument `impl` declares at position `i`, compared against the one
// `decl` declares in the same position.
static InterfaceMismatch argument_mismatch(const std::string &what, size_t i,
    ASR::Function_t *impl, ASR::Function_t *decl, bool use_expr_context)
{
    if (!ASR::is_a<ASR::Var_t>(*impl->m_args[i]) ||
            !ASR::is_a<ASR::Var_t>(*decl->m_args[i])) {
        return {};
    }
    ASR::symbol_t *sym = ASR::down_cast<ASR::Var_t>(impl->m_args[i])->m_v;
    ASR::symbol_t *decl_sym = ASR::down_cast<ASR::Var_t>(decl->m_args[i])->m_v;
    if (!ASR::is_a<ASR::Variable_t>(*sym) ||
            !ASR::is_a<ASR::Variable_t>(*decl_sym)) {
        return {};
    }
    ASR::Variable_t *arg = ASR::down_cast<ASR::Variable_t>(sym);
    ASR::Variable_t *decl_arg = ASR::down_cast<ASR::Variable_t>(decl_sym);
    std::string which = what + ", argument " + std::to_string(i + 1) +
        " '" + std::string(arg->m_name) + "',";
    if (arg->m_intent != decl_arg->m_intent) {
        return {true, "argument_intent_matches",
            which + " must have the same intent as '" +
            std::string(decl_arg->m_name) + "'"};
    }
    if (arg->m_presence != decl_arg->m_presence) {
        return {true, "argument_presence_matches",
            which + " must agree with '" + std::string(decl_arg->m_name) +
            "' on the OPTIONAL attribute"};
    }
    ASR::ttype_t *type = arg->m_type;
    ASR::ttype_t *decl_type = decl_arg->m_type;
    if (type == nullptr || decl_type == nullptr) return {};
    if (ASRUtils::is_allocatable(type) != ASRUtils::is_allocatable(decl_type)) {
        return {true, "argument_allocatable_matches",
            which + " must agree with '" + std::string(decl_arg->m_name) +
            "' on the ALLOCATABLE attribute"};
    }
    if (ASRUtils::is_pointer(type) != ASRUtils::is_pointer(decl_type)) {
        return {true, "argument_pointer_matches",
            which + " must agree with '" + std::string(decl_arg->m_name) +
            "' on the POINTER attribute"};
    }
    if (ASRUtils::extract_n_dims_from_ttype(type) !=
            ASRUtils::extract_n_dims_from_ttype(decl_type)) {
        return {true, "argument_rank_matches",
            which + " must have rank " + std::to_string(
                ASRUtils::extract_n_dims_from_ttype(decl_type))};
    }
    // A derived type argument spells its members out inline, so two
    // structurally different types can name the same type; those are compared
    // by the dedicated struct checks instead.
    if (is_struct_like_type(type) || is_struct_like_type(decl_type) ||
            is_procedure_type(type) || is_procedure_type(decl_type)) {
        return {};
    }
    if (!ASRUtils::check_equal_type(type, decl_type,
            use_expr_context ? impl->m_args[i] : nullptr,
            use_expr_context ? decl->m_args[i] : nullptr)) {
        return {true, "argument_type_matches",
            which + " must have type " +
            ASRUtils::get_type_code(decl_type) + ", not " +
            ASRUtils::get_type_code(type)};
    }
    return {};
}

InterfaceMismatch interface_mismatch(const std::string &what,
    ASR::Function_t *impl, ASR::Function_t *decl, size_t skip,
    bool use_expr_context)
{
    bool decl_is_function = decl->m_return_var != nullptr;
    bool is_function = impl->m_return_var != nullptr;
    if (decl_is_function != is_function) {
        return {true, "result_kind_matches", what + " must be a " +
            std::string(decl_is_function ? "function" : "subroutine")};
    }
    if (decl_is_function && is_function) {
        ASR::ttype_t *decl_type = typed_expr_type(decl->m_return_var);
        ASR::ttype_t *type = typed_expr_type(impl->m_return_var);
        if (decl_type != nullptr && type != nullptr &&
                !is_struct_like_type(decl_type) && !is_struct_like_type(type)) {
            if (!ASRUtils::check_equal_type(type, decl_type,
                    use_expr_context ? impl->m_return_var : nullptr,
                    use_expr_context ? decl->m_return_var : nullptr)) {
                return {true, "result_type_matches", what + " must return " +
                    ASRUtils::get_type_code(decl_type) + ", not " +
                    ASRUtils::get_type_code(type)};
            }
            InterfaceMismatch m = string_length_mismatch(what + " result",
                type, decl_type, "result_type_matches");
            if (m.mismatch) return m;
        }
    }
    if (impl->n_args != decl->n_args) {
        return {true, "argument_count_matches", what + " must take " +
            std::to_string(decl->n_args) + " arguments, not " +
            std::to_string(impl->n_args)};
    }
    for (size_t i = 0; i < impl->n_args; i++) {
        if (i == skip) continue;
        InterfaceMismatch m = argument_mismatch(what, i, impl, decl,
            use_expr_context);
        if (m.mismatch) return m;
    }
    return {};
}

size_t passed_object_index(const ASR::StructMethodDeclaration_t &x,
    ASR::Function_t *proc)
{
    if (x.m_is_nopass) return proc->n_args;
    if (x.m_self_argument == nullptr) return 0;
    std::string self_name = x.m_self_argument;
    for (size_t i = 0; i < proc->n_args; i++) {
        if (!ASR::is_a<ASR::Var_t>(*proc->m_args[i])) continue;
        if (self_name == std::string(ASRUtils::symbol_name(
                ASR::down_cast<ASR::Var_t>(proc->m_args[i])->m_v))) {
            return i;
        }
    }
    return proc->n_args;
}

ASR::StructMethodDeclaration_t* overridden_binding(
    const ASR::StructMethodDeclaration_t &x)
{
    SymbolTable *symtab = x.m_parent_symtab;
    if (symtab == nullptr || symtab->asr_owner == nullptr ||
            !ASR::is_a<ASR::symbol_t>(*symtab->asr_owner)) {
        return nullptr;
    }
    ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(symtab->asr_owner);
    if (!ASR::is_a<ASR::Struct_t>(*owner)) return nullptr;
    ASR::symbol_t *parent = ASR::down_cast<ASR::Struct_t>(owner)->m_parent;
    // A parent cycle is diagnosed on its own; here it must only not loop.
    std::set<const ASR::Struct_t*> seen;
    while (parent != nullptr) {
        parent = ASRUtils::symbol_get_past_external(parent);
        if (parent == nullptr || !ASR::is_a<ASR::Struct_t>(*parent)) {
            return nullptr;
        }
        ASR::Struct_t *s = ASR::down_cast<ASR::Struct_t>(parent);
        if (!seen.insert(s).second) return nullptr;
        ASR::symbol_t *sym = s->m_symtab->get_symbol(std::string(x.m_name));
        if (sym != nullptr &&
                ASR::is_a<ASR::StructMethodDeclaration_t>(*sym)) {
            return ASR::down_cast<ASR::StructMethodDeclaration_t>(sym);
        }
        parent = s->m_parent;
    }
    return nullptr;
}

InterfaceMismatch binding_override_mismatch(
    const ASR::StructMethodDeclaration_t &x, ASR::Function_t *proc,
    const std::string &what, bool use_expr_context)
{
    ASR::StructMethodDeclaration_t *base_decl = overridden_binding(x);
    if (base_decl == nullptr) return {};
    ASR::symbol_t *base_sym =
        ASRUtils::symbol_get_past_external(base_decl->m_proc);
    if (base_sym == nullptr || !ASR::is_a<ASR::Function_t>(*base_sym)) {
        return {};
    }
    ASR::Function_t *base = ASR::down_cast<ASR::Function_t>(base_sym);
    // An inherited binding names the very same procedure; only a binding that
    // names a different one overrides anything.
    if (base == proc) return {};

    if (x.m_is_nopass != base_decl->m_is_nopass) {
        return {true, "nopass_matches",
            what + " must agree on the NOPASS attribute"};
    }
    size_t self_index = passed_object_index(x, proc);
    size_t base_self_index = passed_object_index(*base_decl, base);
    if (self_index != base_self_index) {
        return {true, "passed_object_matches",
            what + " must take its passed-object dummy argument in the same "
            "position"};
    }
    // The passed-object dummy argument is declared with the type it is bound
    // to, so the two deliberately differ there.
    return interface_mismatch(what, proc, base, self_index, use_expr_context);
}

std::string get_format_type_code(ASR::ttype_t* type) {
    type = type_get_past_allocatable(type_get_past_pointer(type));
    switch (type->type) {
        case ASR::ttypeType::Integer: {
            return "I" + std::to_string(extract_kind_from_ttype_t(type));
        }
        case ASR::ttypeType::UnsignedInteger: {
            return "U" + std::to_string(extract_kind_from_ttype_t(type));
        }
        case ASR::ttypeType::Real: {
            return "R" + std::to_string(extract_kind_from_ttype_t(type));
        }
        case ASR::ttypeType::Complex: {
            std::string real = "R" + std::to_string(extract_kind_from_ttype_t(type));
            return "{" + real + "," + real + "}";
        }
        case ASR::ttypeType::Logical: {
            return "L" + std::to_string(
                ASR::down_cast<ASR::Logical_t>(type)->m_kind * 8);
        }
        case ASR::ttypeType::String: {
            ASR::String_t* str_type = ASR::down_cast<ASR::String_t>(type);
            std::string res = "S-";
            if (str_type->m_physical_type == ASR::DescriptorString) {
                res += "DESC";
            } else if (str_type->m_physical_type == ASR::CChar) {
                res += "CCHAR";
            } else {
                throw LCompilersException("Unhandled string physical type");
            }
            // The character kind tells the runtime how many bytes one
            // character occupies, so that it can transcode a kind 4 value to
            // UTF-8 and count field widths in characters rather than bytes.
            return res + "-K" + std::to_string(str_type->m_kind);
        }
        case ASR::ttypeType::CPtr: {
            return "CPtr";
        }
        default: {
            return "";
        }
    }
}

namespace {

// Replaces the variable `var` by the expression `value`.
class ImpliedDoLoopVarSubstituter :
        public ASR::BaseExprReplacer<ImpliedDoLoopVarSubstituter> {
public:
    ASR::symbol_t* var;
    ASR::expr_t* value;

    ImpliedDoLoopVarSubstituter(ASR::symbol_t* var, ASR::expr_t* value)
        : var(var), value(value) {}

    void replace_Var(ASR::Var_t* x) {
        if (x->m_v == var) {
            *current_expr = value;
        }
    }
};

}

// Whether the bounds and steps of `idl` and of the loops nested in it are
// all integer literals.
static bool implied_do_loop_has_literal_bounds(ASR::ImpliedDoLoop_t* idl) {
    if (!ASR::is_a<ASR::IntegerConstant_t>(*idl->m_start) ||
        !ASR::is_a<ASR::IntegerConstant_t>(*idl->m_end) ||
        (idl->m_increment &&
         !ASR::is_a<ASR::IntegerConstant_t>(*idl->m_increment))) {
        return false;
    }
    for (size_t i = 0; i < idl->n_values; i++) {
        if (ASR::is_a<ASR::ImpliedDoLoop_t>(*idl->m_values[i]) &&
            !implied_do_loop_has_literal_bounds(
                ASR::down_cast<ASR::ImpliedDoLoop_t>(idl->m_values[i]))) {
            return false;
        }
    }
    return true;
}

// Collects in `types` the derived types of the values of `idl` and of the
// loops nested in it.
static void implied_do_loop_derived_types(ASR::ImpliedDoLoop_t* idl,
        std::set<ASR::symbol_t*> &types) {
    for (size_t i = 0; i < idl->n_values; i++) {
        ASR::expr_t* value = idl->m_values[i];
        if (ASR::is_a<ASR::ImpliedDoLoop_t>(*value)) {
            implied_do_loop_derived_types(
                ASR::down_cast<ASR::ImpliedDoLoop_t>(value), types);
        } else if (ASR::is_a<ASR::StructType_t>(*extract_type(expr_type(value)))) {
            if (ASR::symbol_t* type = get_struct_sym_from_struct_expr(value)) {
                types.insert(symbol_get_past_external(type));
            }
        }
    }
}

// Appends the values of `idl` to `result`; `iterations` and `values` are the
// numbers of iterations and of values that may still be expanded.
static ImpliedDoExpansion append_implied_do_loop_values(Allocator &al,
        ASR::ImpliedDoLoop_t* idl, Vec<ASR::expr_t*> &result,
        uint64_t &iterations, uint64_t &values) {
    // A nested loop that reuses an enclosing loop's variable had its own
    // variable substituted by the enclosing loop's value.
    if (!ASR::is_a<ASR::Var_t>(*idl->m_var)) {
        return {ImpliedDoExpansionBlocker::ReusedVariable, &idl->base};
    }
    // The bounds only have to be integer compile-time constants,
    // e.g. `size(a)` of a fixed size array or a named constant.
    int64_t start_val = 0, end_val = 0, step_val = 1;
    if (!is_value_constant(expr_value(idl->m_start), start_val)) {
        return {ImpliedDoExpansionBlocker::RunTimeBounds, idl->m_start};
    }
    if (!is_value_constant(expr_value(idl->m_end), end_val)) {
        return {ImpliedDoExpansionBlocker::RunTimeBounds, idl->m_end};
    }
    if (idl->m_increment &&
        !is_value_constant(expr_value(idl->m_increment), step_val)) {
        return {ImpliedDoExpansionBlocker::RunTimeBounds, idl->m_increment};
    }

    if (step_val == 0 || (step_val > 0 && start_val > end_val) ||
        (step_val < 0 && start_val < end_val)) {
        return {ImpliedDoExpansionBlocker::None, nullptr};
    }

    // The trip count is (end - start) / step + 1. It is computed in
    // unsigned arithmetic, which cannot overflow here, and the loop
    // runs on a counter, so bounds near huge() are safe.
    uint64_t distance = step_val > 0
        ? uint64_t(end_val) - uint64_t(start_val)
        : uint64_t(start_val) - uint64_t(end_val);
    uint64_t abs_step = step_val > 0
        ? uint64_t(step_val) : uint64_t(0) - uint64_t(step_val);
    uint64_t last_trip = distance / abs_step;
    if (last_trip >= iterations) {
        return {ImpliedDoExpansionBlocker::TooManyIterations, nullptr};
    }
    iterations -= last_trip + 1;

    ASR::symbol_t* loop_var_symbol = ASR::down_cast<ASR::Var_t>(idl->m_var)->m_v;

    for (uint64_t trip = 0; trip <= last_trip; trip++) {
        // start + trip * step, which lies between start and end.
        int64_t i = int64_t(uint64_t(start_val) + trip * uint64_t(step_val));
        ASR::expr_t* loop_var_val = EXPR(ASR::make_IntegerConstant_t(
            al, idl->base.base.loc, i, expr_type(idl->m_var)));

        for (size_t j = 0; j < idl->n_values; j++) {
            // A nested loop is expanded with this loop's variable
            // substituted, both in its values and in its bounds.
            ExprStmtDuplicator duplicator(al);
            ASR::expr_t* substituted = duplicator.duplicate_expr(idl->m_values[j]);
            ImpliedDoLoopVarSubstituter substituter(loop_var_symbol, loop_var_val);
            substituter.current_expr = &substituted;
            substituter.replace_expr(substituted);

            if (ASR::is_a<ASR::ImpliedDoLoop_t>(*substituted)) {
                ImpliedDoExpansion nested = append_implied_do_loop_values(al,
                    ASR::down_cast<ASR::ImpliedDoLoop_t>(substituted),
                    result, iterations, values);
                if (nested.blocker != ImpliedDoExpansionBlocker::None) {
                    return nested;
                }
            } else {
                if (values == 0) {
                    return {ImpliedDoExpansionBlocker::TooManyValues, nullptr};
                }
                values--;
                result.push_back(al, substituted);
            }
        }
    }
    return {ImpliedDoExpansionBlocker::None, nullptr};
}

ImpliedDoExpansion expand_implied_do_loop(Allocator &al,
        ASR::ImpliedDoLoop_t* idl, Vec<ASR::expr_t*> &result) {
    // Loops whose bounds are all integer literals are expanded in full.
    uint64_t iterations = implied_do_loop_has_literal_bounds(idl)
        ? std::numeric_limits<uint64_t>::max()
        : implied_do_loop_folded_bounds_expansion_limit;
    // Only a loop with values of two or more different derived types is
    // limited. A loop with values of one derived type and of intrinsic types
    // keeps the expansion that it had before loops over different derived
    // types were expanded.
    std::set<ASR::symbol_t*> derived_types;
    implied_do_loop_derived_types(idl, derived_types);
    uint64_t values = derived_types.size() > 1
        ? implied_do_loop_expansion_limit : std::numeric_limits<uint64_t>::max();
    return append_implied_do_loop_values(al, idl, result, iterations, values);
}

//Initialize pointer to zero so that it can be initialized in first call to get_instance
ASRUtils::LabelGenerator* ASRUtils::LabelGenerator::label_generator = nullptr;

bool is_single_statement(const ASR::stmt_t &x) {
    switch (x.type) {
        case ASR::stmtType::DoConcurrentLoop:
        case ASR::stmtType::OMPRegion:
        case ASR::stmtType::DoLoop:
        case ASR::stmtType::ForAllSingle:
        case ASR::stmtType::ForEach:
        case ASR::stmtType::If:
        case ASR::stmtType::IfArithmetic:
        case ASR::stmtType::Select:
        case ASR::stmtType::SelectType:
        case ASR::stmtType::SelectRank:
        case ASR::stmtType::Where:
        case ASR::stmtType::WhileLoop:
        case ASR::stmtType::ChangeTeam:
        case ASR::stmtType::AssociateBlockCall:
        case ASR::stmtType::BlockCall:
        case ASR::stmtType::GoTo:
        case ASR::stmtType::GoToTarget:
        case ASR::stmtType::Cycle:
        case ASR::stmtType::Exit:
        case ASR::stmtType::Return:
            return false;
        default:
            return true;
    }
}

bool runs_on_device(const ASR::Function_t& x) {
    switch (ASRUtils::get_exec_space(x)) {
        case ASR::exec_spaceType::Device:
        case ASR::exec_spaceType::HostDevice:
        case ASR::exec_spaceType::Kernel:
            return true;
        case ASR::exec_spaceType::Host:
            return false;
    }
    return false;
}

ASR::FunctionType_t* get_FunctionType(ASR::symbol_t* x) {
    ASR::symbol_t* a_name_ = ASRUtils::symbol_get_past_external(x);
    ASR::FunctionType_t* func_type = nullptr;
    if( ASR::is_a<ASR::Function_t>(*a_name_) ) {
        func_type = ASR::down_cast<ASR::FunctionType_t>(
            ASR::down_cast<ASR::Function_t>(a_name_)->m_function_signature);
    } else if( ASR::is_a<ASR::Variable_t>(*a_name_) ) {
        ASR::ttype_t* var_type = ASR::down_cast<ASR::Variable_t>(a_name_)->m_type;
        if (ASR::is_a<ASR::Pointer_t>(*var_type)) {
            var_type = ASR::down_cast<ASR::Pointer_t>(var_type)->m_type;
        }
        func_type = ASR::down_cast<ASR::FunctionType_t>(var_type);
    } else if( ASR::is_a<ASR::StructMethodDeclaration_t>(*a_name_) ) {
        ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(
            ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::StructMethodDeclaration_t>(a_name_)->m_proc));
        func_type = ASR::down_cast<ASR::FunctionType_t>(func->m_function_signature);
    } else {
        LCOMPILERS_ASSERT(false);
    }
    return func_type;
}

ASR::Function_t* get_function(ASR::symbol_t* x)
{
    ASR::symbol_t* a_name = ASRUtils::symbol_get_past_external(x);

    if (ASR::is_a<ASR::Function_t>(*a_name)) {
        return ASR::down_cast<ASR::Function_t>(a_name);
    } else if (ASR::is_a<ASR::Variable_t>(*a_name)) {
        ASR::symbol_t* type_decl = ASR::down_cast<ASR::Variable_t>(a_name)->m_type_declaration;
        if (type_decl == nullptr) {
            return nullptr;
        }
        return ASR::down_cast<ASR::Function_t>(ASRUtils::symbol_get_past_external(type_decl));
    } else if (ASR::is_a<ASR::StructMethodDeclaration_t>(*a_name)) {
        return ASR::down_cast<ASR::Function_t>(ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::StructMethodDeclaration_t>(a_name)->m_proc));
    } else {
        LCOMPILERS_ASSERT(false);
    }
    
    return nullptr;
}

int extract_kind_from_ttype_t(const ASR::ttype_t* type) {
    if (type == nullptr) {
        return -1;
    }
    switch (type->type) {
        case ASR::ttypeType::Array: {
            return extract_kind_from_ttype_t(ASR::down_cast<ASR::Array_t>(type)->m_type);
        }
        case ASR::ttypeType::Integer : {
            return checked_integer_cast<int>(ASR::down_cast<ASR::Integer_t>(type)->m_kind);
        }
        case ASR::ttypeType::UnsignedInteger : {
            return checked_integer_cast<int>(ASR::down_cast<ASR::UnsignedInteger_t>(type)->m_kind);
        }
        case ASR::ttypeType::Real : {
            return checked_integer_cast<int>(ASR::down_cast<ASR::Real_t>(type)->m_kind);
        }
        case ASR::ttypeType::Complex: {
            return checked_integer_cast<int>(ASR::down_cast<ASR::Complex_t>(type)->m_kind);
        }
        case ASR::ttypeType::String: {
            return checked_integer_cast<int>(ASR::down_cast<ASR::String_t>(type)->m_kind);
        }
        case ASR::ttypeType::Logical: {
            return checked_integer_cast<int>(ASR::down_cast<ASR::Logical_t>(type)->m_kind);
        }
        case ASR::ttypeType::Pointer: {
            return extract_kind_from_ttype_t(ASR::down_cast<ASR::Pointer_t>(type)->m_type);
        }
        case ASR::ttypeType::Allocatable: {
            return extract_kind_from_ttype_t(ASR::down_cast<ASR::Allocatable_t>(type)->m_type);
        }
        case ASR::ttypeType::StructType: {
            return 4;
        }
        default : {
            return -1;
        }
    }
}

void set_kind_to_ttype_t(ASR::ttype_t* type, int kind) {
    if (type == nullptr) {
        return;
    }
    switch (type->type) {
        case ASR::ttypeType::Array: {
            set_kind_to_ttype_t(ASR::down_cast<ASR::Array_t>(type)->m_type, kind);
            break;
        }
        case ASR::ttypeType::Integer : {
            ASR::down_cast<ASR::Integer_t>(type)->m_kind = kind;
            break;
        }
        case ASR::ttypeType::UnsignedInteger : {
            ASR::down_cast<ASR::UnsignedInteger_t>(type)->m_kind = kind;
            break;
        }
        case ASR::ttypeType::Real : {
            ASR::down_cast<ASR::Real_t>(type)->m_kind = kind;
            break;
        }
        case ASR::ttypeType::Complex: {
            ASR::down_cast<ASR::Complex_t>(type)->m_kind = kind;
            break;
        }
        case ASR::ttypeType::String: {
            ASR::down_cast<ASR::String_t>(type)->m_kind = kind;
            break;
        }
        case ASR::ttypeType::Logical: {
            ASR::down_cast<ASR::Logical_t>(type)->m_kind = kind;
            break;
        }
        case ASR::ttypeType::Pointer: {
            set_kind_to_ttype_t(ASR::down_cast<ASR::Pointer_t>(type)->m_type, kind);
            break;
        }
        case ASR::ttypeType::Allocatable: {
            set_kind_to_ttype_t(ASR::down_cast<ASR::Allocatable_t>(type)->m_type, kind);
            break;
        }
        default : {
            return;
        }
    }
}

ASR::Variable_t* expr_to_variable_or_null(ASR::expr_t* expr) {
    if (!expr || !(ASR::is_a<ASR::Var_t>(*expr) || ASR::is_a<ASR::Cast_t>(*expr))) {
        return nullptr;
    }

    if (ASR::is_a<ASR::Cast_t>(*expr)) {
        return expr_to_variable_or_null(ASR::down_cast<ASR::Cast_t>(expr)->m_arg);
    }

    ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(expr)->m_v);

    if (!ASR::is_a<ASR::Variable_t>(*sym)) {
        return nullptr;
    }

    return ASR::down_cast<ASR::Variable_t>(sym);
}

ASR::ttype_t* symbol_type(const ASR::symbol_t *f)
{
    switch( f->type ) {
        case ASR::symbolType::Variable: {
            return ASR::down_cast<ASR::Variable_t>(f)->m_type;
        }
        case ASR::symbolType::Enum: {
            return ASR::down_cast<ASR::Enum_t>(f)->m_type;
        }
        case ASR::symbolType::ExternalSymbol: {
            return symbol_type(ASRUtils::symbol_get_past_external(f));
        }
        case ASR::symbolType::Function: {
            return ASRUtils::expr_type(
                ASR::down_cast<ASR::Function_t>(f)->m_return_var);
        }
        case ASR::symbolType::Struct: {
            return ASR::down_cast<ASR::Struct_t>(f)->m_struct_signature;
        }
        case ASR::symbolType::StructMethodDeclaration :{
            return symbol_type(ASR::down_cast<ASR::StructMethodDeclaration_t>(f)->m_proc);
        }
        default: {
            throw LCompilersException("Cannot return type of, " +
                                    std::to_string(f->type) + " symbol.");
        }
    }
    return nullptr;
}

ASR::expr_t* get_expr_from_sym(Allocator& al, ASR::symbol_t* sym) {
    sym = ASRUtils::symbol_get_past_external(sym);
    switch (sym->type) {
        case ASR::symbolType::Variable: {
            return ASRUtils::EXPR(ASR::make_Var_t(al, sym->base.loc, sym));
        }
        case ASR::symbolType::Function: {
            return ASRUtils::EXPR(ASR::make_Var_t(al, sym->base.loc, sym));
        }
        default: {
            throw LCompilersException("get_expr_from_sym: Only Variable and Function symbols are supported.");
        }
    }
}

std::string symbol_type_name(const ASR::symbol_t &s)
{
    switch( s.type ) {
        case ASR::symbolType::Program: return "Program";
        case ASR::symbolType::Module: return "Module";
        case ASR::symbolType::Function: return "Function";
        case ASR::symbolType::GenericProcedure: return "GenericProcedure";
        case ASR::symbolType::CustomOperator: return "CustomOperator";
        case ASR::symbolType::ExternalSymbol: return "ExternalSymbol";
        case ASR::symbolType::Struct: return "Struct";
        case ASR::symbolType::Enum: return "Enum";
        case ASR::symbolType::Union: return "Union";
        case ASR::symbolType::Variable: return "Variable";
        case ASR::symbolType::StructMethodDeclaration: return "StructMethodDeclaration";
        case ASR::symbolType::AssociateBlock: return "AssociateBlock";
        case ASR::symbolType::Block: return "Block";
        case ASR::symbolType::Requirement: return "Requirement";
        case ASR::symbolType::Template: return "Template";
        case ASR::symbolType::Namelist: return "Namelist";
        case ASR::symbolType::BlockData: return "BlockData";
        case ASR::symbolType::CommonBlock: return "CommonBlock";
        default: {
            LCOMPILERS_ASSERT(false);
        }
    }
    return "";
}

ASR::abiType symbol_abi(const ASR::symbol_t *f)
{
    switch( f->type ) {
        case ASR::symbolType::Variable: {
            return ASR::down_cast<ASR::Variable_t>(f)->m_abi;
        }
        case ASR::symbolType::Enum: {
            return ASR::down_cast<ASR::Enum_t>(f)->m_abi;
        }
        case ASR::symbolType::ExternalSymbol: {
            return symbol_abi(ASR::down_cast<ASR::ExternalSymbol_t>(f)->m_external);
        }
        case ASR::symbolType::Function: {
            return ASRUtils::get_FunctionType(*ASR::down_cast<ASR::Function_t>(f))->m_abi;
        }
        case ASR::symbolType::CommonBlock: {
            return ASR::down_cast<ASR::CommonBlock_t>(f)->m_abi;
        }
        default: {
            throw LCompilersException("Cannot return ABI of, " +
                                    std::to_string(f->type) + " symbol.");
        }
    }
    return ASR::abiType::Source;
}

// Helper function to convert intentType to Fortran intent string
static std::string intent_to_str(ASR::intentType intent) {
    switch (intent) {
        case ASR::intentType::In: return "in";
        case ASR::intentType::Out: return "out";
        case ASR::intentType::InOut: return "inout";
        case ASR::intentType::ReturnVar: return "return";
        default: return "";
    }
}

std::string symbol_to_str_fortran(const ASR::symbol_t &s, bool add_symbol_access_specifiers) {
    switch (s.type) {
        case ASR::symbolType::Variable: {
            const ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(&s);
            ASR::ttype_t *base_type = ASRUtils::extract_type(v->m_type);
            std::string res = type_to_str_fortran_symbol(base_type, v->m_type_declaration, true);
            if (ASR::is_a<ASR::StructType_t>(*base_type) &&
                    !ASR::down_cast<ASR::StructType_t>(base_type)->m_is_unlimited_polymorphic) {
                const ASR::StructType_t *stype = ASR::down_cast<ASR::StructType_t>(base_type);
                if (stype->m_is_cstruct) {
                    res = "type(" + res + ")";
                } else {
                    res = "class(" + res + ")";
                }
            }
            // Add dimension attribute for array types
            ASR::ttype_t *type_past_alloc = ASRUtils::type_get_past_allocatable_pointer(v->m_type);
            if (ASR::is_a<ASR::Array_t>(*type_past_alloc)) {
                ASR::Array_t *array_t = ASR::down_cast<ASR::Array_t>(type_past_alloc);
                res += ", dimension(";
                for (size_t i = 0; i < array_t->n_dims; i++) {
                    if (i > 0) res += ", ";
                    res += ":";
                }
                res += ")";
            }
            // Add allocatable/pointer attributes
            if (ASR::is_a<ASR::Allocatable_t>(*v->m_type)) {
                res += ", allocatable";
            } else if (ASR::is_a<ASR::Pointer_t>(*v->m_type)) {
                res += ", pointer";
            }
            // Collect attributes
            if (v->m_storage == ASR::storage_typeType::Parameter) {
                res += ", parameter";
            }
            if (add_symbol_access_specifiers) {
                if (v->m_access == ASR::accessType::Private) {
                    res += ", private";
                } else if (v->m_access == ASR::accessType::Public) {
                    res += ", public";
                }
            }
            if (v->m_presence == ASR::presenceType::Optional) {
                res += ", optional";
            }
            if (v->m_intent != ASR::intentType::Unspecified
                && v->m_intent != ASR::intentType::ReturnVar) {
                res += ", intent(" + intent_to_str(v->m_intent) + ")";
            }
            res += " :: " + std::string(v->m_name);
            if (v->m_value) {
                // TODO:
                //res += " = " + expr_to_str_fortran(v->m_value);
            }
            return res;
        }
        case ASR::symbolType::Function: {
            const ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(&s);
            std::string res = "function " + std::string(f->m_name) + "(";
            std::vector<std::string> arg_names;
            // Extract argument names
            for (size_t i = 0; i < f->n_args; i++) {
                const ASR::expr_t *arg = f->m_args[i];
                if (ASR::is_a<ASR::Var_t>(*arg)) {
                    const ASR::Var_t *var = ASR::down_cast<ASR::Var_t>(arg);
                    const ASR::symbol_t *v_sym = var->m_v;
                    if (ASR::is_a<ASR::Variable_t>(*v_sym)) {
                        const ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(v_sym);
                        arg_names.push_back(std::string(v->m_name));
                    }
                }
            }
            res += join(", ", arg_names) + ")";
            // Handle return variable
            if (f->m_return_var) {
                const ASR::expr_t *ret = f->m_return_var;
                if (ASR::is_a<ASR::Var_t>(*ret)) {
                    const ASR::Var_t *var = ASR::down_cast<ASR::Var_t>(ret);
                    const ASR::symbol_t *v_sym = var->m_v;
                    if (ASR::is_a<ASR::Variable_t>(*v_sym)) {
                        const ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(v_sym);
                        res += " result(" + std::string(v->m_name) + ")";
                    }
                }
            }
            res += "\n";
            // Add argument declarations
            for (const std::string &arg_name : arg_names) {
                ASR::symbol_t *arg_sym = f->m_symtab->get_symbol(arg_name);
                if (arg_sym && ASR::is_a<ASR::Variable_t>(*arg_sym)) {
                    const ASR::Variable_t *arg_var = ASR::down_cast<ASR::Variable_t>(arg_sym);
                    res += "    " + symbol_to_str_fortran(arg_var->base, false) + "\n";
                }
            }
            // Add return variable declaration
            if (f->m_return_var) {
                const ASR::expr_t *ret = f->m_return_var;
                if (ASR::is_a<ASR::Var_t>(*ret)) {
                    const ASR::Var_t *var = ASR::down_cast<ASR::Var_t>(ret);
                    const ASR::symbol_t *v_sym = var->m_v;
                    if (ASR::is_a<ASR::Variable_t>(*v_sym)) {
                        const ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(v_sym);
                        res += "    " + symbol_to_str_fortran(v->base, false) + "\n";
                    }
                }
            }
            res += "end function";
            return res;
        }
        case ASR::symbolType::Module: {
            const ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(&s);
            std::string res = "module " + std::string(m->m_name);
            // Optionally add more details, e.g., intrinsic status
            if (m->m_intrinsic) {
                res += "\n    ! intrinsic module";
            }
            return res;
        }
        case ASR::symbolType::Program: {
            const ASR::Program_t *p = ASR::down_cast<ASR::Program_t>(&s);
            return "program " + std::string(p->m_name);
        }
        case ASR::symbolType::ExternalSymbol: {
            const ASR::ExternalSymbol_t *ext = ASR::down_cast<ASR::ExternalSymbol_t>(&s);
            ASR::symbol_t *orig_sym = ASRUtils::symbol_get_past_external(ext->m_external);
            if (orig_sym) {
                std::string orig_str = symbol_to_str_fortran(*orig_sym, true);
                return orig_str + "  ! from module " + std::string(ext->m_module_name);
            } else {
                return "external symbol " + std::string(ext->m_name);
            }
        }
        case ASR::symbolType::Struct: {
            const ASR::Struct_t *st = ASR::down_cast<ASR::Struct_t>(&s);
            return "type " + std::string(st->m_name);
        }
        case ASR::symbolType::Enum: {
            const ASR::Enum_t *en = ASR::down_cast<ASR::Enum_t>(&s);
            return "enum " + std::string(en->m_name);
        }
        case ASR::symbolType::Union: {
            const ASR::Union_t *un = ASR::down_cast<ASR::Union_t>(&s);
            return "type " + std::string(un->m_name) + "  ! union";
        }
        case ASR::symbolType::GenericProcedure: {
            const ASR::GenericProcedure_t *gp = ASR::down_cast<ASR::GenericProcedure_t>(&s);
            std::string res = "interface " + std::string(gp->m_name) + "\n";
            for (size_t i = 0; i < gp->n_procs; i++) {
                ASR::symbol_t *proc = gp->m_procs[i];
                res += "    procedure " + std::string(ASRUtils::symbol_name(proc)) + "\n";
            }
            res += "end interface";
            return res;
        }
        case ASR::symbolType::CustomOperator: {
            const ASR::CustomOperator_t *co = ASR::down_cast<ASR::CustomOperator_t>(&s);
            std::string res = "interface operator(" + std::string(co->m_name) + ")\n";
            for (size_t i = 0; i < co->n_procs; i++) {
                ASR::symbol_t *proc = co->m_procs[i];
                res += "    procedure " + std::string(ASRUtils::symbol_name(proc)) + "\n";
            }
            res += "end interface";
            return res;
        }
        case ASR::symbolType::StructMethodDeclaration: {
            const ASR::StructMethodDeclaration_t *cp = ASR::down_cast<ASR::StructMethodDeclaration_t>(&s);
            return "procedure " + std::string(cp->m_name) + "  ! class-bound";
        }
        case ASR::symbolType::AssociateBlock: {
            const ASR::AssociateBlock_t *ab = ASR::down_cast<ASR::AssociateBlock_t>(&s);
            return "associate (" + std::string(ab->m_name) + ")";
        }
        case ASR::symbolType::Block: {
            const ASR::Block_t *b = ASR::down_cast<ASR::Block_t>(&s);
            return "block " + std::string(b->m_name);
        }
        case ASR::symbolType::Requirement: {
            const ASR::Requirement_t *req = ASR::down_cast<ASR::Requirement_t>(&s);
            return "requirement " + std::string(req->m_name);
        }
        case ASR::symbolType::Template: {
            const ASR::Template_t *tpl = ASR::down_cast<ASR::Template_t>(&s);
            return "template " + std::string(tpl->m_name);
        }
        default: {
            return "symbol " + std::string(ASRUtils::symbol_name(&s));
        }
    }
}

ASR::ttype_t* get_contained_type(ASR::ttype_t* asr_type, int overload) {
    switch( asr_type->type ) {
        case ASR::ttypeType::List: {
            return ASR::down_cast<ASR::List_t>(asr_type)->m_type;
        }
        case ASR::ttypeType::Set: {
            return ASR::down_cast<ASR::Set_t>(asr_type)->m_type;
        }
        case ASR::ttypeType::Dict: {
            switch( overload ) {
                case 0:
                    return ASR::down_cast<ASR::Dict_t>(asr_type)->m_key_type;
                case 1:
                    return ASR::down_cast<ASR::Dict_t>(asr_type)->m_value_type;
                default:
                    return asr_type;
            }
        }
        case ASR::ttypeType::EnumType: {
            ASR::EnumType_t* enum_asr = ASR::down_cast<ASR::EnumType_t>(asr_type);
            ASR::Enum_t* enum_type = ASR::down_cast<ASR::Enum_t>(
                ASRUtils::symbol_get_past_external(enum_asr->m_enum_type));
            return enum_type->m_type;
        }
        case ASR::ttypeType::Pointer: {
            ASR::Pointer_t* pointer_asr = ASR::down_cast<ASR::Pointer_t>(asr_type);
            return pointer_asr->m_type;
        }
        case ASR::ttypeType::Allocatable: {
            ASR::Allocatable_t* pointer_asr = ASR::down_cast<ASR::Allocatable_t>(asr_type);
            return pointer_asr->m_type;
        }
        default: {
            return asr_type;
        }
    }
}

ASR::array_physical_typeType extract_physical_type(ASR::ttype_t* e) {
    switch( e->type ) {
        case ASR::ttypeType::Array: {
            return ASR::down_cast<ASR::Array_t>(e)->m_physical_type;
        }
        case ASR::ttypeType::Pointer: {
            return extract_physical_type(ASRUtils::type_get_past_pointer(e));
        }
        case ASR::ttypeType::Allocatable: {
            return extract_physical_type(ASRUtils::type_get_past_allocatable(e));
        }
        default:
            throw LCompilersException("Cannot extract the physical type of " +
                    std::to_string(e->type) + " type.");
    }
}

ASR::abiType expr_abi(ASR::expr_t* e) {
    switch( e->type ) {
        case ASR::exprType::Var: {
            return ASRUtils::symbol_abi(ASR::down_cast<ASR::Var_t>(e)->m_v);
        }
        case ASR::exprType::StructInstanceMember: {
            return ASRUtils::symbol_abi(ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_m);
        }
        case ASR::exprType::ArrayReshape: {
            return ASRUtils::expr_abi(ASR::down_cast<ASR::ArrayReshape_t>(e)->m_array);
        }
        case ASR::exprType::GetPointer: {
            return ASRUtils::expr_abi(ASR::down_cast<ASR::GetPointer_t>(e)->m_arg);
        }
        case ASR::exprType::ComplexIm: {
            return ASRUtils::expr_abi(ASR::down_cast<ASR::ComplexIm_t>(e)->m_arg);
        }
        case ASR::exprType::ComplexRe: {
            return ASRUtils::expr_abi(ASR::down_cast<ASR::ComplexRe_t>(e)->m_arg);
        }
        case ASR::exprType::ArrayPhysicalCast: {
            return ASRUtils::expr_abi(ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg);
        }
        case ASR::exprType::ArrayConstant: {
            return ASR::abiType::Source;
        }
        case ASR::exprType::ArraySection: {
            return ASRUtils::expr_abi(ASR::down_cast<ASR::ArraySection_t>(e)->m_v);
        }
        default:
            throw LCompilersException(std::string("Cannot extract the ABI of ") +
                "ASR::exprType::" + std::to_string(e->type) + " expression.");
    }
}

char *symbol_name(const ASR::symbol_t *f)
{
    switch (f->type) {
        case ASR::symbolType::Program: {
            return ASR::down_cast<ASR::Program_t>(f)->m_name;
        }
        case ASR::symbolType::Module: {
            return ASR::down_cast<ASR::Module_t>(f)->m_name;
        }
        case ASR::symbolType::Function: {
            return ASR::down_cast<ASR::Function_t>(f)->m_name;
        }
        case ASR::symbolType::GenericProcedure: {
            return ASR::down_cast<ASR::GenericProcedure_t>(f)->m_name;
        }
        case ASR::symbolType::Struct: {
            return ASR::down_cast<ASR::Struct_t>(f)->m_name;
        }
        case ASR::symbolType::Enum: {
            return ASR::down_cast<ASR::Enum_t>(f)->m_name;
        }
        case ASR::symbolType::Union: {
            return ASR::down_cast<ASR::Union_t>(f)->m_name;
        }
        case ASR::symbolType::Variable: {
            return ASR::down_cast<ASR::Variable_t>(f)->m_name;
        }
        case ASR::symbolType::ExternalSymbol: {
            return ASR::down_cast<ASR::ExternalSymbol_t>(f)->m_name;
        }
        case ASR::symbolType::StructMethodDeclaration: {
            return ASR::down_cast<ASR::StructMethodDeclaration_t>(f)->m_name;
        }
        case ASR::symbolType::CustomOperator: {
            return ASR::down_cast<ASR::CustomOperator_t>(f)->m_name;
        }
        case ASR::symbolType::AssociateBlock: {
            return ASR::down_cast<ASR::AssociateBlock_t>(f)->m_name;
        }
        case ASR::symbolType::Block: {
            return ASR::down_cast<ASR::Block_t>(f)->m_name;
        }
        case ASR::symbolType::Requirement: {
            return ASR::down_cast<ASR::Requirement_t>(f)->m_name;
        }
        case ASR::symbolType::Template: {
            return ASR::down_cast<ASR::Template_t>(f)->m_name;
        }
        case ASR::symbolType::Namelist: {
            return ASR::down_cast<ASR::Namelist_t>(f)->m_group_name;
        }
        case ASR::symbolType::BlockData: {
            return ASR::down_cast<ASR::BlockData_t>(f)->m_name;
        }
        case ASR::symbolType::CommonBlock: {
            return ASR::down_cast<ASR::CommonBlock_t>(f)->m_name;
        }
        default : throw LCompilersException("Not implemented");
    }
}

bool get_class_proc_nopass_val(ASR::symbol_t* func_sym) {
    func_sym = ASRUtils::symbol_get_past_external(func_sym);
    bool nopass = false;
    if (ASR::is_a<ASR::StructMethodDeclaration_t>(*func_sym)) {
        nopass = ASR::down_cast<ASR::StructMethodDeclaration_t>(func_sym)->m_is_nopass;
    } else if (ASR::is_a<ASR::Variable_t>(*func_sym)) {
        ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(func_sym);
        nopass = (v->m_pass_attr == ASR::pass_attrType::NoPass ||
                  v->m_pass_attr == ASR::pass_attrType::NotMethod);
    }
    return nopass;
}

size_t get_pass_arg_index(ASR::symbol_t* a_name) {
    ASR::symbol_t* sym = symbol_get_past_external(a_name);
    if (ASR::is_a<ASR::StructMethodDeclaration_t>(*sym)) {
        ASR::StructMethodDeclaration_t* clss_proc =
            ASR::down_cast<ASR::StructMethodDeclaration_t>(sym);
        if (clss_proc->m_self_argument == nullptr) {
            return 0;
        }
        ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(
            symbol_get_past_external(clss_proc->m_proc));
        for (size_t i = 0; i < func->n_args; i++) {
            ASR::Variable_t* v = EXPR2VAR(func->m_args[i]);
            if (strcmp(v->m_name, clss_proc->m_self_argument) == 0) {
                return i;
            }
        }
        return 0;
    } else if (ASR::is_a<ASR::Variable_t>(*sym)) {
        ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(sym);
        if (var->m_self_argument == nullptr) {
            return 0;
        }
        if (var->m_type_declaration != nullptr) {
            ASR::symbol_t* decl = symbol_get_past_external(var->m_type_declaration);
            if (ASR::is_a<ASR::Function_t>(*decl)) {
                ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(decl);
                for (size_t i = 0; i < func->n_args; i++) {
                    ASR::Variable_t* v = EXPR2VAR(func->m_args[i]);
                    if (strcmp(v->m_name, var->m_self_argument) == 0) {
                        return i;
                    }
                }
            }
        }
        return 0;
    }
    return 0;
}

void encode_dimensions(size_t n_dims, std::string& res,
    bool use_underscore_sep) {
    if( n_dims == 0 ) {
        return ;
    }

    if( use_underscore_sep ) {
        res += "_";
    } else {
        res += "[";
    }

    for( size_t i = 0; i < n_dims; i++ ) {
        if( use_underscore_sep ) {
            res += "_";
        } else {
            res += ":";
        }
        if( i == n_dims - 1 ) {
            if( use_underscore_sep ) {
                res += "_";
            } else {
                res += "]";
            }
        } else {
            if( use_underscore_sep ) {
                res += "_";
            } else {
                res += ", ";
            }
        }
    }
}

std::string type_to_str_fortran_symbol(const ASR::ttype_t* t,
                                                     ASR::symbol_t* struct_sym,
                                                     bool show_kind)
{
    switch (t->type) {
        case ASR::ttypeType::Integer: {
            if (show_kind) {
                ASR::Integer_t* int_t = ASR::down_cast<ASR::Integer_t>(t);
                return "integer(" + std::to_string(int_t->m_kind) + ")";
            }
            return "integer";
        }
        case ASR::ttypeType::UnsignedInteger: {
            if (show_kind) {
                ASR::UnsignedInteger_t* uint_t = ASR::down_cast<ASR::UnsignedInteger_t>(t);
                return "type(unsigned(" + std::to_string(uint_t->m_kind) + "))";
            }
            return "type(unsigned)";
        }
        case ASR::ttypeType::Real: {
            if (show_kind) {
                ASR::Real_t* real_t = ASR::down_cast<ASR::Real_t>(t);
                return "real(" + std::to_string(real_t->m_kind) + ")";
            }
            return "real";
        }
        case ASR::ttypeType::Complex: {
            if (show_kind) {
                ASR::Complex_t* complex_t = ASR::down_cast<ASR::Complex_t>(t);
                return "complex(" + std::to_string(complex_t->m_kind) + ")";
            }
            return "complex";
        }
        case ASR::ttypeType::String: {
            if (show_kind) {
                ASR::String_t* str_t = ASR::down_cast<ASR::String_t>(t);
                std::string res = "character";
                if (str_t->m_len_kind == ASR::string_length_kindType::AssumedLength) {
                    res += "(len=*)";
                } else if (str_t->m_len_kind == ASR::string_length_kindType::DeferredLength) {
                    res += "(len=:)";
                } else if (str_t->m_len) {
                    if (ASR::is_a<ASR::IntegerConstant_t>(*str_t->m_len)) {
                        ASR::IntegerConstant_t* ic = ASR::down_cast<ASR::IntegerConstant_t>(str_t->m_len);
                        res += "(len=" + std::to_string(ic->m_n) + ")";
                    }
                }
                return res;
            }
            return "string";
        }
        case ASR::ttypeType::Logical: {
            if (show_kind) {
                ASR::Logical_t* log_t = ASR::down_cast<ASR::Logical_t>(t);
                return "logical(" + std::to_string(log_t->m_kind) + ")";
            }
            return "logical";
        }
        case ASR::ttypeType::Set: {
            return "set";
        }
        case ASR::ttypeType::List: {
            return "list";
        }
        case ASR::ttypeType::Tuple: {
            return "tuple";
        }
        case ASR::ttypeType::StructType: {
            if (ASR::down_cast<ASR::StructType_t>(t)->m_is_unlimited_polymorphic) {
                if (struct_sym != nullptr && std::string(ASRUtils::symbol_name(
                        ASRUtils::symbol_get_past_external(struct_sym))) == "~assumed_type") {
                    return "type(*)";
                }
                return "class(*)";
            }
            if (struct_sym == nullptr) {
                return "derived_type";
            }
            return ASRUtils::symbol_name(struct_sym);
        }
        case ASR::ttypeType::EnumType: {
            ASR::EnumType_t* enum_type = ASR::down_cast<ASR::EnumType_t>(t);
            return ASRUtils::symbol_name(enum_type->m_enum_type);
        }
        case ASR::ttypeType::UnionType: {
            return "union";
        }
        case ASR::ttypeType::Dict: {
            return "dict";
        }
        case ASR::ttypeType::Pointer: {
            return type_to_str_fortran_symbol(ASRUtils::type_get_past_pointer(
                        const_cast<ASR::ttype_t*>(t)), struct_sym, show_kind) + " pointer";
        }
        case ASR::ttypeType::Allocatable: {
            return type_to_str_fortran_symbol(ASRUtils::type_get_past_allocatable(
                        const_cast<ASR::ttype_t*>(t)), struct_sym, show_kind) + " allocatable";
        }
        case ASR::ttypeType::CPtr: {
            ASR::cptr_kindType kind = ASRUtils::get_cptr_kind_from_type(
                const_cast<ASR::ttype_t*>(t));
            if (kind == ASR::cptr_kindType::CFunPointer) {
                return "type(c_funptr)";
            }
            if (kind == ASR::cptr_kindType::CPointer) {
                return "type(c_ptr)";
            }
            if (struct_sym != nullptr) {
                return "type(" + std::string(ASRUtils::symbol_name(
                    ASRUtils::symbol_get_past_external(struct_sym))) + ")";
            }
            return "type(c_ptr)";
        }
        case ASR::ttypeType::SymbolicExpression: {
            return "type(symbolic)";
        }
        case ASR::ttypeType::TypeParameter: {
            ASR::TypeParameter_t* tp = ASR::down_cast<ASR::TypeParameter_t>(t);
            return tp->m_param;
        }
        case ASR::ttypeType::Array: {
            ASR::Array_t* array_t = ASR::down_cast<ASR::Array_t>(t);
            std::string res = type_to_str_fortran_symbol(array_t->m_type, struct_sym, show_kind);
            encode_dimensions(array_t->n_dims, res, false);
            return res;
        }
        case ASR::ttypeType::FunctionType: {
            ASR::FunctionType_t* ftp = ASR::down_cast<ASR::FunctionType_t>(t);
            std::string result = "FunctionType(";
            for( size_t i = 0; i < ftp->n_arg_types; i++ ) {
                result += type_to_str_fortran_symbol(ftp->m_arg_types[i], nullptr, show_kind) + ", ";
            }
            if( ftp->m_return_var_type ) {
                result += "return_type: ";
                result += type_to_str_fortran_symbol(ftp->m_return_var_type, nullptr, show_kind);
            }
            result += ")";
            return result;
        }
        default : throw LCompilersException("Type number " +
              std::to_string(t->type) + " not implemented.");
    }
}

std::string type_to_str_fortran_expr(const ASR::ttype_t* t, ASR::expr_t* expr)
{
    ASR::symbol_t* struct_sym = nullptr;

    if (ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(const_cast<ASR::ttype_t*>(t)))) {
        LCOMPILERS_ASSERT_MSG(expr != nullptr, "`expr` should be non-null for `StructType`");
        struct_sym = ASRUtils::get_struct_sym_from_struct_expr(expr);
    } else if (ASR::is_a<ASR::CPtr_t>(*ASRUtils::extract_type(
                   const_cast<ASR::ttype_t*>(t)))) {
        if (ASRUtils::get_cptr_kind_from_type(const_cast<ASR::ttype_t*>(t))
                == ASR::cptr_kindType::CPtrUnspecified && expr != nullptr) {
            struct_sym = ASRUtils::get_cptr_type_declaration_from_expr(expr);
        }
    }

    return type_to_str_fortran_symbol(t, struct_sym);
}

std::string type_to_str_with_kind(const ASR::ttype_t* t, ASR::expr_t* expr)
{
    ASR::symbol_t* struct_sym = nullptr;

    if (ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(const_cast<ASR::ttype_t*>(t)))) {
        if (expr != nullptr) {
            struct_sym = ASRUtils::get_struct_sym_from_struct_expr(expr);
        }
    } else if (ASR::is_a<ASR::CPtr_t>(*ASRUtils::extract_type(
                   const_cast<ASR::ttype_t*>(t)))) {
        if (ASRUtils::get_cptr_kind_from_type(const_cast<ASR::ttype_t*>(t))
                == ASR::cptr_kindType::CPtrUnspecified && expr != nullptr) {
            struct_sym = ASRUtils::get_cptr_type_declaration_from_expr(expr);
        }
    }

    switch (t->type) {
        case ASR::ttypeType::Integer: {
            ASR::Integer_t* int_t = ASR::down_cast<ASR::Integer_t>(t);
            return "integer(" + std::to_string(int_t->m_kind) + ")";
        }
        case ASR::ttypeType::Real: {
            ASR::Real_t* real_t = ASR::down_cast<ASR::Real_t>(t);
            return "real(" + std::to_string(real_t->m_kind) + ")";
        }
        case ASR::ttypeType::Complex: {
            ASR::Complex_t* complex_t = ASR::down_cast<ASR::Complex_t>(t);
            return "complex(" + std::to_string(complex_t->m_kind) + ")";
        }
        case ASR::ttypeType::Logical: {
            ASR::Logical_t* logical_t = ASR::down_cast<ASR::Logical_t>(t);
            return "logical(" + std::to_string(logical_t->m_kind) + ")";
        }
        case ASR::ttypeType::Pointer: {
            return type_to_str_with_kind(ASRUtils::type_get_past_pointer(
                        const_cast<ASR::ttype_t*>(t)), expr) + " pointer";
        }
        case ASR::ttypeType::Allocatable: {
            return type_to_str_with_kind(ASRUtils::type_get_past_allocatable(
                        const_cast<ASR::ttype_t*>(t)), expr) + " allocatable";
        }
        case ASR::ttypeType::Array: {
            ASR::Array_t* array_t = ASR::down_cast<ASR::Array_t>(t);
            std::string res = type_to_str_with_kind(array_t->m_type, expr);
            encode_dimensions(array_t->n_dims, res, false);
            return res;
        }
        default:
            // For other types, fall back to the standard function
            return type_to_str_fortran_symbol(t, struct_sym);
    }
}

std::string type_to_str_with_substitution(ASR::expr_t* expr, const ASR::ttype_t *t,
    std::map<std::string, std::pair<ASR::ttype_t*, ASR::symbol_t*>> subs)
{
    if (ASR::is_a<ASR::TypeParameter_t>(*t)) {
        ASR::TypeParameter_t* t_tp = ASR::down_cast<ASR::TypeParameter_t>(t);
        t = subs[t_tp->m_param].first;
    }
    switch (t->type) {
        case ASR::ttypeType::Pointer: {
            return type_to_str_with_substitution(expr, ASRUtils::type_get_past_pointer(
                        const_cast<ASR::ttype_t*>(t)), subs) + " pointer";
        }
        case ASR::ttypeType::Allocatable: {
            return type_to_str_with_substitution(expr, ASRUtils::type_get_past_allocatable(
                        const_cast<ASR::ttype_t*>(t)), subs) + " allocatable";
        }
        case ASR::ttypeType::Array: {
            ASR::Array_t* array_t = ASR::down_cast<ASR::Array_t>(t);
            std::string res = type_to_str_with_substitution(expr, array_t->m_type, subs);
            encode_dimensions(array_t->n_dims, res, false);
            return res;
        }
        case ASR::ttypeType::FunctionType: {
            ASR::FunctionType_t* ftp = ASR::down_cast<ASR::FunctionType_t>(t);
            std::string result = "(";
            for( size_t i = 0; i < ftp->n_arg_types; i++ ) {
                result += type_to_str_with_substitution(nullptr, ftp->m_arg_types[i], subs) + ", ";
            }
            result += "return_type: ";
            if( ftp->m_return_var_type ) {
                result += type_to_str_with_substitution(nullptr, ftp->m_return_var_type, subs);
            } else {
                result += "void";
            }
            result += ")";
            return result;
        }
        default : return type_to_str_fortran_expr(t, expr);
    }
}

std::string cmpop_to_str(const ASR::cmpopType t) {
    switch (t) {
        case (ASR::cmpopType::Eq): { return " == "; }
        case (ASR::cmpopType::NotEq): { return " != "; }
        case (ASR::cmpopType::Lt): { return " < "; }
        case (ASR::cmpopType::LtE): { return " <= "; }
        case (ASR::cmpopType::Gt): { return " > "; }
        case (ASR::cmpopType::GtE): { return " >= "; }
        default : throw LCompilersException("Cannot represent the comparison as a string");
    }
}

std::pair<char**, size_t> symbol_dependencies(const ASR::symbol_t *f)
{
    switch (f->type) {
        case ASR::symbolType::Program: {
            ASR::Program_t* sym = ASR::down_cast<ASR::Program_t>(f);
            return std::make_pair(sym->m_dependencies, sym->n_dependencies);
        }
        case ASR::symbolType::Module: {
            ASR::Module_t* sym = ASR::down_cast<ASR::Module_t>(f);
            return std::make_pair(sym->m_dependencies, sym->n_dependencies);
        }
        case ASR::symbolType::Function: {
            ASR::Function_t* sym = ASR::down_cast<ASR::Function_t>(f);
            return std::make_pair(sym->m_dependencies, sym->n_dependencies);
        }
        case ASR::symbolType::Struct: {
            ASR::Struct_t* sym = ASR::down_cast<ASR::Struct_t>(f);
            char** deps = sym->m_dependencies;
            size_t n_deps = sym->n_dependencies;

            // we store dependencies in this
            std::set<std::string> unique_deps;
            // first we add the dependencies of sym itself
            for (size_t i = 0; i < n_deps; ++i) {
                if (deps[i]) unique_deps.insert(deps[i]);
            }

            // if there exists a parent, add parent and it's dependencies
            if (sym->m_parent) {
                ASR::Struct_t* parent = ASR::down_cast<ASR::Struct_t>(sym->m_parent);
                unique_deps.insert(std::string(parent->m_name));

                auto parent_deps = symbol_dependencies(sym->m_parent);
                char** parent_dep_array = parent_deps.first;
                size_t parent_dep_count = parent_deps.second;

                for (size_t i = 0; i < parent_dep_count; ++i) {
                    unique_deps.insert(parent_dep_array[i]);
                }
            }

            size_t total_deps = unique_deps.size();
            char** merged_deps = new char*[total_deps];
            size_t idx = 0;
            for (const auto& dep : unique_deps) {
                merged_deps[idx] = static_cast<char*>(std::malloc(dep.size() + 1));
                if (merged_deps[idx] == nullptr) {
                    throw std::bad_alloc();
                }
                std::memcpy(merged_deps[idx], dep.c_str(), dep.size() + 1);
                ++idx;
            }
            return std::make_pair(merged_deps, total_deps);
        }
        case ASR::symbolType::Enum: {
            ASR::Enum_t* sym = ASR::down_cast<ASR::Enum_t>(f);
            return std::make_pair(sym->m_dependencies, sym->n_dependencies);
        }
        case ASR::symbolType::Union: {
            ASR::Union_t* sym = ASR::down_cast<ASR::Union_t>(f);
            return std::make_pair(sym->m_dependencies, sym->n_dependencies);
        }
        case ASR::symbolType::BlockData: {
            ASR::BlockData_t* sym = ASR::down_cast<ASR::BlockData_t>(f);
            return std::make_pair(sym->m_dependencies, sym->n_dependencies);
        }
        default : throw LCompilersException("Not implemented");
    }
}

SymbolTable *symbol_parent_symtab(const ASR::symbol_t *f)
{
    switch (f->type) {
        case ASR::symbolType::Program: {
            return ASR::down_cast<ASR::Program_t>(f)->m_symtab->parent;
        }
        case ASR::symbolType::Module: {
            return ASR::down_cast<ASR::Module_t>(f)->m_symtab->parent;
        }
        case ASR::symbolType::Function: {
            return ASR::down_cast<ASR::Function_t>(f)->m_symtab->parent;
        }
        case ASR::symbolType::GenericProcedure: {
            return ASR::down_cast<ASR::GenericProcedure_t>(f)->m_parent_symtab;
        }
        case ASR::symbolType::Struct: {
            return ASR::down_cast<ASR::Struct_t>(f)->m_symtab->parent;
        }
        case ASR::symbolType::Enum: {
            return ASR::down_cast<ASR::Enum_t>(f)->m_symtab->parent;
        }
        case ASR::symbolType::Union: {
            return ASR::down_cast<ASR::Union_t>(f)->m_symtab->parent;
        }
        case ASR::symbolType::Variable: {
            return ASR::down_cast<ASR::Variable_t>(f)->m_parent_symtab;
        }
        case ASR::symbolType::ExternalSymbol: {
            return ASR::down_cast<ASR::ExternalSymbol_t>(f)->m_parent_symtab;
        }
        case ASR::symbolType::StructMethodDeclaration: {
            return ASR::down_cast<ASR::StructMethodDeclaration_t>(f)->m_parent_symtab;
        }
        case ASR::symbolType::CustomOperator: {
            return ASR::down_cast<ASR::CustomOperator_t>(f)->m_parent_symtab;
        }
        case ASR::symbolType::AssociateBlock: {
            return ASR::down_cast<ASR::AssociateBlock_t>(f)->m_symtab->parent;
        }
        case ASR::symbolType::Block: {
            return ASR::down_cast<ASR::Block_t>(f)->m_symtab->parent;
        }
        case ASR::symbolType::Requirement: {
            return ASR::down_cast<ASR::Requirement_t>(f)->m_symtab->parent;
        }
        case ASR::symbolType::Template: {
            return ASR::down_cast<ASR::Template_t>(f)->m_symtab->parent;
        }
        case ASR::symbolType::Namelist: {
            return ASR::down_cast<ASR::Namelist_t>(f)->m_parent_symtab;
        }
        case ASR::symbolType::BlockData: {
            return ASR::down_cast<ASR::BlockData_t>(f)->m_symtab->parent;
        }
        case ASR::symbolType::CommonBlock: {
            return ASR::down_cast<ASR::CommonBlock_t>(f)->m_parent_symtab;
        }
        default : throw LCompilersException("Not implemented for type " +
              std::to_string(f->type));
    }
}

SymbolTable *scope_past_constructs(SymbolTable *scope)
{
    while (scope->parent && scope->asr_owner &&
            ASR::is_a<ASR::symbol_t>(*scope->asr_owner)) {
        ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(scope->asr_owner);
        if (!ASR::is_a<ASR::AssociateBlock_t>(*owner) &&
                !ASR::is_a<ASR::Block_t>(*owner)) {
            break;
        }
        scope = scope->parent;
    }
    return scope;
}

SymbolTable *symbol_symtab(const ASR::symbol_t *f)
{
    switch (f->type) {
        case ASR::symbolType::Program: {
            return ASR::down_cast<ASR::Program_t>(f)->m_symtab;
        }
        case ASR::symbolType::Module: {
            return ASR::down_cast<ASR::Module_t>(f)->m_symtab;
        }
        case ASR::symbolType::Function: {
            return ASR::down_cast<ASR::Function_t>(f)->m_symtab;
        }
        case ASR::symbolType::GenericProcedure: {
            return nullptr;
            //throw LCompilersException("GenericProcedure does not have a symtab");
        }
        case ASR::symbolType::CustomOperator: {
            return nullptr;
        }
        case ASR::symbolType::Namelist: {
            return nullptr;
        }
        case ASR::symbolType::Struct: {
            return ASR::down_cast<ASR::Struct_t>(f)->m_symtab;
        }
        case ASR::symbolType::Enum: {
            return ASR::down_cast<ASR::Enum_t>(f)->m_symtab;
        }
        case ASR::symbolType::Union: {
            return ASR::down_cast<ASR::Union_t>(f)->m_symtab;
        }
        case ASR::symbolType::Variable: {
            return nullptr;
            //throw LCompilersException("Variable does not have a symtab");
        }
        case ASR::symbolType::ExternalSymbol: {
            return nullptr;
            //throw LCompilersException("ExternalSymbol does not have a symtab");
        }
        case ASR::symbolType::StructMethodDeclaration: {
            return nullptr;
            //throw LCompilersException("StructMethodDeclaration does not have a symtab");
        }
        case ASR::symbolType::AssociateBlock: {
            return ASR::down_cast<ASR::AssociateBlock_t>(f)->m_symtab;
        }
        case ASR::symbolType::Block: {
            return ASR::down_cast<ASR::Block_t>(f)->m_symtab;
        }
        case ASR::symbolType::Requirement: {
            return ASR::down_cast<ASR::Requirement_t>(f)->m_symtab;
        }
        case ASR::symbolType::Template: {
            return ASR::down_cast<ASR::Template_t>(f)->m_symtab;
        }
        case ASR::symbolType::BlockData: {
            return ASR::down_cast<ASR::BlockData_t>(f)->m_symtab;
        }
        case ASR::symbolType::CommonBlock: {
            return nullptr;
        }
        default : throw LCompilersException("Not implemented");
    }
}

std::string cell_prefix(const SymbolTable *s) {
    const SymbolTable *tu = s;
    while (tu != nullptr && !is_tu_scope(tu)) tu = tu->parent;
    if (tu == nullptr) return "";
    int depth = 1;
    for (const SymbolTable *p = tu->parent; p != nullptr; p = p->parent) {
        if (is_tu_scope(p)) depth++;
    }
    if (depth == 1) return "";
    return "__cell" + std::to_string(depth) + "_";
}

ASR::symbol_t *import_type_bound_specific(Allocator &al, SymbolTable *scope,
        ASR::symbol_t *proc, const std::string &generic) {
    ASR::symbol_t *target = symbol_get_past_external(proc);
    std::string name = std::string(symbol_name(target)) + "@" + generic;
    for (const std::string &n : {name, std::string(symbol_name(target))}) {
        ASR::symbol_t *s = scope->resolve_symbol(n);
        if (s != nullptr && symbol_get_past_external(s) == target) {
            return s;
        }
    }
    // Two types can bind procedures of the same name to the same operator.
    std::string local_name = name;
    for (int i = 1; scope->get_symbol(local_name) != nullptr; i++) {
        if (symbol_get_past_external(scope->get_symbol(local_name)) == target) {
            return scope->get_symbol(local_name);
        }
        local_name = name + std::to_string(i);
    }
    ASR::Module_t *owner = get_sym_module(target);
    LCOMPILERS_ASSERT(owner != nullptr);
    ASR::symbol_t *s = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(
        al, target->base.loc, scope, s2c(al, local_name), target, owner->m_name,
        nullptr, 0, symbol_name(target), ASR::accessType::Private));
    scope->add_symbol(local_name, s);
    return s;
}

ASR::symbol_t *get_asr_owner(const ASR::expr_t *expr) {
    switch( expr->type ) {
        case ASR::exprType::Var: {
            return ASRUtils::get_asr_owner(ASR::down_cast<ASR::Var_t>(expr)->m_v);
        }
        case ASR::exprType::StructInstanceMember: {
            return ASRUtils::get_asr_owner(ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::StructInstanceMember_t>(expr)->m_m));
        }
        case ASR::exprType::GetPointer: {
            return ASRUtils::get_asr_owner(ASR::down_cast<ASR::GetPointer_t>(expr)->m_arg);
        }
        case ASR::exprType::FunctionCall: {
            return ASRUtils::get_asr_owner(ASR::down_cast<ASR::FunctionCall_t>(expr)->m_name);
        }
        default: {
            return nullptr;
        }
    }
    return nullptr;
}

ASR::Module_t *get_sym_module0(const ASR::symbol_t *sym) {
    const SymbolTable *s = symbol_parent_symtab(sym);
    while (s->parent != nullptr && !is_tu_scope(s)) {
        if (s->asr_owner != nullptr) {
            ASR::symbol_t *asr_owner = ASR::down_cast<ASR::symbol_t>(s->asr_owner);
            if (ASR::is_a<ASR::Module_t>(*asr_owner)) {
                return ASR::down_cast<ASR::Module_t>(asr_owner);
            }
        }
        s = s->parent;
    }
    return nullptr;
}

bool is_intrinsic_symbol(const ASR::symbol_t *fn) {
    if(!fn) return false;
    const ASR::symbol_t *sym = fn;
    ASR::Module_t *m = get_sym_module0(sym);
    if (m != nullptr) {
        if (m->m_intrinsic) {
            return true;
        }
        if (startswith(m->m_name, "lfortran_intrinsic")) return true;
    }
    return false;
}

bool is_intrinsic_function2(const ASR::Function_t *fn) {
    ASR::symbol_t *sym = (ASR::symbol_t*)fn;
    ASR::Module_t *m = get_sym_module0(sym);
    if (m != nullptr) {
        if (m->m_intrinsic ||
            ASRUtils::get_FunctionType(fn)->m_abi ==
            ASR::abiType::Intrinsic) {
                return true;
        }
    }
    return false;
}

bool is_variable(ASR::expr_t* a_value) {
    if (a_value == nullptr) {
        return false;
    }
    switch (a_value->type) {
        case ASR::exprType::ArrayItem: {
            ASR::ArrayItem_t* a_item = ASR::down_cast<ASR::ArrayItem_t>(a_value);
            ASR::expr_t* array_expr = a_item->m_v;
            ASR::Variable_t* array_var = ASRUtils::EXPR2VAR(array_expr);
            // a constant array's item isn't a "variable"
            return array_var->m_storage != ASR::storage_typeType::Parameter;
        }
        case ASR::exprType::Var: {
            ASR::Variable_t* variable_t = ASRUtils::EXPR2VAR(a_value);
            return variable_t->m_storage != ASR::storage_typeType::Parameter;
        }
        case ASR::exprType::StringItem:
        case ASR::exprType::StringSection:
        case ASR::exprType::ArraySection:
        case ASR::exprType::StructInstanceMember: {
            return true;
        }
        default: {
            return false;
        }
    }
}

bool is_pointer_association_initializer(ASR::expr_t* init) {
    if (init == nullptr) {
        return false;
    }
    switch (init->type) {
        case ASR::exprType::Var: {
            ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(init)->m_v);
            return ASR::is_a<ASR::Variable_t>(*sym) &&
                ASR::down_cast<ASR::Variable_t>(sym)->m_storage
                    != ASR::storage_typeType::Parameter;
        }
        case ASR::exprType::ArrayItem: {
            return is_pointer_association_initializer(
                ASR::down_cast<ASR::ArrayItem_t>(init)->m_v);
        }
        case ASR::exprType::ArraySection: {
            return is_pointer_association_initializer(
                ASR::down_cast<ASR::ArraySection_t>(init)->m_v);
        }
        case ASR::exprType::StructInstanceMember: {
            return is_pointer_association_initializer(
                ASR::down_cast<ASR::StructInstanceMember_t>(init)->m_v);
        }
        default: {
            return false;
        }
    }
}

bool is_named_constant_designator(ASR::expr_t* expr) {
    switch (expr->type) {
        case ASR::exprType::Var: {
            ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(expr)->m_v);
            return ASR::is_a<ASR::Variable_t>(*sym) &&
                ASR::down_cast<ASR::Variable_t>(sym)->m_storage ==
                    ASR::storage_typeType::Parameter;
        }
        case ASR::exprType::ArrayItem: {
            return is_named_constant_designator(
                ASR::down_cast<ASR::ArrayItem_t>(expr)->m_v);
        }
        case ASR::exprType::ArraySection: {
            return is_named_constant_designator(
                ASR::down_cast<ASR::ArraySection_t>(expr)->m_v);
        }
        case ASR::exprType::StructInstanceMember: {
            return is_named_constant_designator(
                ASR::down_cast<ASR::StructInstanceMember_t>(expr)->m_v);
        }
        case ASR::exprType::StringItem: {
            return is_named_constant_designator(
                ASR::down_cast<ASR::StringItem_t>(expr)->m_arg);
        }
        case ASR::exprType::StringSection: {
            return is_named_constant_designator(
                ASR::down_cast<ASR::StringSection_t>(expr)->m_arg);
        }
        default: {
            return false;
        }
    }
}

bool is_modifiable_actual_argument_expr(ASR::expr_t* a_value) {
    if (a_value == nullptr) {
        return false;
    }
    switch (a_value->type) {
        case ASR::exprType::Var: {
            ASR::symbol_t* v_sym = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(a_value)->m_v);
            if (!ASR::is_a<ASR::Variable_t>(*v_sym)) {
                return false;
            }
            ASR::Variable_t* variable_t = ASR::down_cast<ASR::Variable_t>(v_sym);
            return variable_t->m_storage != ASR::storage_typeType::Parameter;
        }
        case ASR::exprType::ArrayItem: {
            ASR::ArrayItem_t* a_item = ASR::down_cast<ASR::ArrayItem_t>(a_value);
            return is_modifiable_actual_argument_expr(a_item->m_v);
        }
        case ASR::exprType::StringItem:
        case ASR::exprType::StringSection:
        case ASR::exprType::ArraySection:
        case ASR::exprType::StructInstanceMember: {
            return !is_named_constant_designator(a_value);
        }
        case ASR::exprType::IntrinsicArrayFunction:
        case ASR::exprType::ListItem: {
            return true;
        }
        case ASR::exprType::Cast: {
            ASR::Cast_t* cast = ASR::down_cast<ASR::Cast_t>(a_value);
            return is_modifiable_actual_argument_expr(cast->m_arg);
        }
        case ASR::exprType::ArrayPhysicalCast: {
            ASR::ArrayPhysicalCast_t* cast = ASR::down_cast<ASR::ArrayPhysicalCast_t>(a_value);
            return is_modifiable_actual_argument_expr(cast->m_arg);
        }
        case ASR::exprType::GetPointer: {
            ASR::GetPointer_t* get_ptr = ASR::down_cast<ASR::GetPointer_t>(a_value);
            return is_modifiable_actual_argument_expr(get_ptr->m_arg);
        }
        case ASR::exprType::StringPhysicalCast: {
            ASR::StringPhysicalCast_t* cast = ASR::down_cast<ASR::StringPhysicalCast_t>(a_value);
            return is_modifiable_actual_argument_expr(cast->m_arg);
        }
        case ASR::exprType::ComplexRe: {
            ASR::ComplexRe_t* re = ASR::down_cast<ASR::ComplexRe_t>(a_value);
            return is_modifiable_actual_argument_expr(re->m_arg);
        }
        case ASR::exprType::ComplexIm: {
            ASR::ComplexIm_t* im = ASR::down_cast<ASR::ComplexIm_t>(a_value);
            return is_modifiable_actual_argument_expr(im->m_arg);
        }
        case ASR::exprType::DictItem: {
            return true;
        }
        case ASR::exprType::FunctionCall: {
            ASR::FunctionCall_t* func_call = ASR::down_cast<ASR::FunctionCall_t>(a_value);
            return ASR::is_a<ASR::Pointer_t>(*func_call->m_type);
        }
        default:
            return false;
    }
}

bool is_value_constant(ASR::expr_t *a_value) {
    if( a_value == nullptr ) {
        return false;
    }
    switch ( a_value->type ) {
        case ASR::exprType::IntegerConstant:
        case ASR::exprType::UnsignedIntegerConstant:
        case ASR::exprType::RealConstant:
        case ASR::exprType::ComplexConstant:
        case ASR::exprType::LogicalConstant:
        case ASR::exprType::ImpliedDoLoop:
        case ASR::exprType::PointerNullConstant:
        case ASR::exprType::ArrayConstant:
        case ASR::exprType::StringConstant:
        case ASR::exprType::StructConstant: {
            return true;
        }
        case ASR::exprType::RealBinOp:
        case ASR::exprType::IntegerUnaryMinus:
        case ASR::exprType::RealUnaryMinus:
        case ASR::exprType::IntegerBinOp:
        case ASR::exprType::StructInstanceMember:
        case ASR::exprType::StringLen:
        case ASR::exprType::ArrayItem: {
            return is_value_constant(expr_value(a_value));
        }
        case ASR::exprType::ArrayConstructor: {
            if (is_value_constant(expr_value(a_value))) {
                return true;
            }
            ASR::ArrayConstructor_t* ac = ASR::down_cast<ASR::ArrayConstructor_t>(a_value);
            for (size_t i = 0; i < ac->n_args; i++) {
                if (!is_value_constant(ac->m_args[i]) &&
                    !is_value_constant(expr_value(ac->m_args[i]))) {
                    return false;
                }
            }
            return true;
        } case ASR::exprType::ListConstant: {
            ASR::ListConstant_t* list_constant = ASR::down_cast<ASR::ListConstant_t>(a_value);
            for( size_t i = 0; i < list_constant->n_args; i++ ) {
                if( !ASRUtils::is_value_constant(list_constant->m_args[i]) &&
                    !ASRUtils::is_value_constant(ASRUtils::expr_value(list_constant->m_args[i])) ) {
                    return false;
                }
            }
            return true;
        } case ASR::exprType::TypeInquiry: {
            ASR::TypeInquiry_t* ti = ASR::down_cast<ASR::TypeInquiry_t>(a_value);
            if (ti->m_value != nullptr) {
                return ASRUtils::is_value_constant(ti->m_value);
            }
            return true;
        } case ASR::exprType::IntrinsicElementalFunction: {
            ASR::IntrinsicElementalFunction_t* intrinsic_elemental_function =
                ASR::down_cast<ASR::IntrinsicElementalFunction_t>(a_value);

            if (ASRUtils::is_value_constant(intrinsic_elemental_function->m_value)) {
                return true;
            }

            for( size_t i = 0; i < intrinsic_elemental_function->n_args; i++ ) {
                if( !ASRUtils::is_value_constant(intrinsic_elemental_function->m_args[i]) ) {
                    return false;
                }
            }
            return true;
        } case ASR::exprType::IntrinsicArrayFunction: {
            ASR::IntrinsicArrayFunction_t* intrinsic_array_function =
                ASR::down_cast<ASR::IntrinsicArrayFunction_t>(a_value);

            if (ASRUtils::is_value_constant(intrinsic_array_function->m_value)) {
                return true;
            }

            for( size_t i = 0; i < intrinsic_array_function->n_args; i++ ) {
                if( !ASRUtils::is_value_constant(intrinsic_array_function->m_args[i]) ) {
                    return false;
                }
            }
            return true;
        } case ASR::exprType::FunctionCall: {
            ASR::FunctionCall_t* func_call_t = ASR::down_cast<ASR::FunctionCall_t>(a_value);
            if( !ASRUtils::is_intrinsic_symbol(ASRUtils::symbol_get_past_external(func_call_t->m_name)) ) {
                return false;
            }

            ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(
                ASRUtils::symbol_get_past_external(func_call_t->m_name));
            for( size_t i = 0; i < func_call_t->n_args; i++ ) {
                if (func_call_t->m_args[i].m_value == nullptr &&
                    ASRUtils::EXPR2VAR(func->m_args[i])->m_presence == ASR::presenceType::Optional) {
                    continue;
                }
                if( !ASRUtils::is_value_constant(func_call_t->m_args[i].m_value) ) {
                    return false;
                }
            }
            return true;
        } case ASR::exprType::ArrayBroadcast: {
            ASR::ArrayBroadcast_t* array_broadcast = ASR::down_cast<ASR::ArrayBroadcast_t>(a_value);
            return is_value_constant(array_broadcast->m_value) ||
                is_value_constant(array_broadcast->m_array);
        } case ASR::exprType::Var: {
            ASR::Var_t* var_t = ASR::down_cast<ASR::Var_t>(a_value);
            if( ASR::is_a<ASR::Variable_t>(*ASRUtils::symbol_get_past_external(var_t->m_v)) ) {
                ASR::Variable_t* variable_t = ASR::down_cast<ASR::Variable_t>(
                    ASRUtils::symbol_get_past_external(var_t->m_v));
                return variable_t->m_storage == ASR::storage_typeType::Parameter;
            } else if(ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(var_t->m_v))){
                return true;
            } else {
                return false;
            }
        } case ASR::exprType::Cast: {
            ASR::Cast_t* cast_t = ASR::down_cast<ASR::Cast_t>(a_value);
            return is_value_constant(cast_t->m_arg);
        } case ASR::exprType::ArrayReshape: {
            ASR::ArrayReshape_t*
                array_reshape = ASR::down_cast<ASR::ArrayReshape_t>(a_value);
            return is_value_constant(array_reshape->m_array) && is_value_constant(array_reshape->m_shape);
        } case ASR::exprType::ArrayIsContiguous: {
            ASR::ArrayIsContiguous_t*
                array_is_contiguous = ASR::down_cast<ASR::ArrayIsContiguous_t>(a_value);
            return is_value_constant(array_is_contiguous->m_array);
        } case ASR::exprType::ArrayPhysicalCast: {
            ASR::ArrayPhysicalCast_t*
                array_physical_t = ASR::down_cast<ASR::ArrayPhysicalCast_t>(a_value);
            return is_value_constant(array_physical_t->m_arg);
        } case ASR::exprType::StructConstructor: {
            ASR::StructConstructor_t* struct_type_constructor =
                ASR::down_cast<ASR::StructConstructor_t>(a_value);
            bool is_constant = true;
            for( size_t i = 0; i < struct_type_constructor->n_args; i++ ) {
                if( struct_type_constructor->m_args[i].m_value ) {
                    is_constant = is_constant &&
                                (is_value_constant(
                                    struct_type_constructor->m_args[i].m_value) ||
                                is_value_constant(
                                    ASRUtils::expr_value(
                                        struct_type_constructor->m_args[i].m_value)));
                }
            }
            return is_constant;
        } default: {
            return false;
        }
    }
}

void update_common_block_role(ASR::CommonBlock_t *block) {
    if (block->m_role == ASR::common_block_roleType::External) {
        return;
    }
    bool initialized = false;
    auto check = [&](ASR::symbol_t *s) {
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
        initialized = initialized || v->m_symbolic_value != nullptr
            || v->m_value != nullptr;
    };
    for (size_t i = 0; i < block->n_members; i++) {
        check(block->m_members[i]);
    }
    for (size_t i = 0; i < block->n_associations; i++) {
        check(block->m_associations[i].m_variable);
    }
    block->m_role = initialized ? ASR::common_block_roleType::Initialized
        : ASR::common_block_roleType::Tentative;
}

bool is_value_constant(ASR::expr_t *a_value, double& const_value) {
    if( a_value == nullptr ) {
        return false;
    }
    if (ASR::is_a<ASR::IntegerConstant_t>(*a_value)) {
        ASR::IntegerConstant_t* const_int = ASR::down_cast<ASR::IntegerConstant_t>(a_value);
        const_value = static_cast<double>(const_int->m_n);
    } else if (ASR::is_a<ASR::RealConstant_t>(*a_value)) {
        ASR::RealConstant_t* const_real = ASR::down_cast<ASR::RealConstant_t>(a_value);
        const_value = const_real->m_r;
    } else {
        return false;
    }
    return true;
}

bool is_value_equal(ASR::expr_t* test_expr, ASR::expr_t* desired_expr) {
    ASR::expr_t* test_value = expr_value(test_expr);
    ASR::expr_t* desired_value = expr_value(desired_expr);
    if( !is_value_constant(test_value) ||
        !is_value_constant(desired_value) ||
        test_value->type != desired_value->type ) {
        return false;
    }

    switch( desired_value->type ) {
        case ASR::exprType::IntegerConstant: {
            ASR::IntegerConstant_t* test_int = ASR::down_cast<ASR::IntegerConstant_t>(test_value);
            ASR::IntegerConstant_t* desired_int = ASR::down_cast<ASR::IntegerConstant_t>(desired_value);
            return test_int->m_n == desired_int->m_n;
        }
        case ASR::exprType::StringConstant: {
            ASR::StringConstant_t* test_str = ASR::down_cast<ASR::StringConstant_t>(test_value);
            ASR::StringConstant_t* desired_str = ASR::down_cast<ASR::StringConstant_t>(desired_value);
            return std::string(test_str->m_s) == std::string(desired_str->m_s);
        }
        default: {
            return false;
        }
    }
}

bool is_value_in_range(ASR::expr_t* start, ASR::expr_t* end, ASR::expr_t* value) {
    ASR::expr_t *start_value = nullptr, *end_value = nullptr;
    if( start ) {
        start_value = expr_value(start);
    }
    if( end ) {
        end_value = expr_value(end);
    }
    ASR::expr_t* test_value = expr_value(value);


    double start_double = std::numeric_limits<double>::min();
    double end_double = std::numeric_limits<double>::max();
    double value_double;
    bool start_const = is_value_constant(start_value, start_double);
    bool end_const = is_value_constant(end_value, end_double);
    bool value_const = is_value_constant(test_value, value_double);
    if( !value_const || (!start_const && !end_const) ) {
        return false;
    }
    return value_double >= start_double && value_double <= end_double;
}

bool all_args_evaluated(const Vec<ASR::array_index_t> &args) {
    for (auto &a : args) {
        bool is_m_left_const, is_m_right_const, is_m_step_const;
        is_m_left_const = is_m_right_const = is_m_step_const = false;
        if( a.m_left != nullptr ) {
            ASR::expr_t *m_left_value = ASRUtils::expr_value(a.m_left);
            is_m_left_const = is_value_constant(m_left_value);
        } else {
            is_m_left_const = true;
        }
        if( a.m_right != nullptr ) {
            ASR::expr_t *m_right_value = ASRUtils::expr_value(a.m_right);
            is_m_right_const = is_value_constant(m_right_value);
        } else {
            is_m_right_const = true;
        }
        if( a.m_step != nullptr ) {
            ASR::expr_t *m_step_value = ASRUtils::expr_value(a.m_step);
            is_m_step_const = is_value_constant(m_step_value);
        } else {
            is_m_step_const = true;
        }
        if( !(is_m_left_const && is_m_right_const && is_m_step_const) ) {
            return false;
        }
    }
    return true;
}

ASR::Variable_t* extract_ExternalSymbol_Variable(ASR::expr_t* a_expr) {
    ASR::expr_t* variable_expr = nullptr;
    if (ASR::is_a<ASR::ArrayItem_t>(*a_expr)) {
        ASR::ArrayItem_t* array_item = ASR::down_cast<ASR::ArrayItem_t>(a_expr);
        variable_expr = array_item->m_v;
    } else if (ASR::is_a<ASR::Var_t>(*a_expr)) {
        variable_expr = a_expr;
    } else if (ASR::is_a<ASR::ArraySection_t>(*a_expr)) {
        ASR::ArraySection_t* array_section = ASR::down_cast<ASR::ArraySection_t>(a_expr);
        variable_expr = array_section->m_v;
    } else if (ASR::is_a<ASR::StructInstanceMember_t>(*a_expr)) {
        ASR::StructInstanceMember_t* struct_instance_mem = ASR::down_cast<ASR::StructInstanceMember_t>(a_expr);
        variable_expr = struct_instance_mem->m_v;
    }

    if (variable_expr && ASR::is_a<ASR::Var_t>(*variable_expr)) {
        bool is_external_sym = ASR::is_a<ASR::ExternalSymbol_t>(*ASR::down_cast<ASR::Var_t>(variable_expr)->m_v);
        if (is_external_sym) {
            return ASRUtils::EXPR2VAR(variable_expr);
        }
    }
    return nullptr;
}

bool extract_value(ASR::expr_t* value_expr,
    std::complex<double>& value) {
    if ( ASR::is_a<ASR::ComplexConstructor_t>(*value_expr) ) {
        value_expr = ASR::down_cast<ASR::ComplexConstructor_t>(value_expr)->m_value;
        if (!value_expr) {
            return false;
        }
    }
    if( !ASR::is_a<ASR::ComplexConstant_t>(*value_expr) ) {
        return false;
    }

    ASR::ComplexConstant_t* value_const = ASR::down_cast<ASR::ComplexConstant_t>(value_expr);
    value = std::complex(value_const->m_re, value_const->m_im);
    return true;
}

bool extract_string_value(ASR::expr_t* value_expr,
    std::string& value) {
    if( !is_value_constant(value_expr) ) {
        return false;
    }
    switch (value_expr->type)
    {
        case ASR::exprType::StringConstant: {
            ASR::StringConstant_t* const_string = ASR::down_cast<ASR::StringConstant_t>(value_expr);
            value = std::string(const_string->m_s);
            break;
        }
        case ASR::exprType::Var: {
            ASR::Variable_t* var = EXPR2VAR(value_expr);
            if (var->m_storage == ASR::storage_typeType::Parameter
                    && !extract_string_value(var->m_value, value)) {
                return false;
            }
            break;
        }
        case ASR::exprType::FunctionCall: {
            ASR::FunctionCall_t* func_call = ASR::down_cast<ASR::FunctionCall_t>(value_expr);
            if (!extract_string_value(func_call->m_value, value)) {
                return false;
            }
            break;
        }
        default:
            return false;
    }
    return true;
}

std::string type_encode_dims(size_t n_dims, ASR::dimension_t* m_dims )
{
    std::string dims_str = "[";
    for( size_t i = 0; i < n_dims; i++ ) {
        ASR::dimension_t dim = m_dims[i];
        dims_str += extract_dim_value(dim.m_length);
        if (i + 1 < n_dims) {
            dims_str += ",";
        }
    }
    dims_str += "]";
    return dims_str;
}

std::string get_type_code(const ASR::ttype_t *t, bool use_underscore_sep,
    bool encode_dimensions_, bool set_dimensional_hint, ASR::expr_t* expr,
    ASR::symbol_t* type_declaration)
{
    bool is_dimensional = false;
    std::string res = "";
    switch (t->type) {
        case ASR::ttypeType::Array: {
            ASR::Array_t* array_t = ASR::down_cast<ASR::Array_t>(t);
            res = get_type_code(array_t->m_type, use_underscore_sep, false, false, expr,
                type_declaration);
            if( encode_dimensions_ ) {
                encode_dimensions(array_t->n_dims, res, use_underscore_sep);
                return res;
            }
            is_dimensional = true;
            break;
        }
        case ASR::ttypeType::Integer: {
            ASR::Integer_t *integer = ASR::down_cast<ASR::Integer_t>(t);
            res = "i" + std::to_string(integer->m_kind * 8);
            break;
        }
        case ASR::ttypeType::UnsignedInteger: {
            ASR::UnsignedInteger_t *integer = ASR::down_cast<ASR::UnsignedInteger_t>(t);
            res = "u" + std::to_string(integer->m_kind * 8);
            break;
        }
        case ASR::ttypeType::Real: {
            ASR::Real_t *real = ASR::down_cast<ASR::Real_t>(t);
            res = "r" + std::to_string(real->m_kind * 8);
            break;
        }
        case ASR::ttypeType::Complex: {
            ASR::Complex_t *complx = ASR::down_cast<ASR::Complex_t>(t);
            res = "c" + std::to_string(complx->m_kind * 8);
            break;
        }
        case ASR::ttypeType::Logical: {
            ASR::Logical_t *logical_type = ASR::down_cast<ASR::Logical_t>(t);
            res = "l" + std::to_string(logical_type->m_kind * 8);
            break;
        }
        case ASR::ttypeType::String: {
            return "str";
        }
        case ASR::ttypeType::Tuple: {
            ASR::Tuple_t *tup = ASR::down_cast<ASR::Tuple_t>(t);
            std::string result = "tuple";
            if( use_underscore_sep ) {
                result += "_";
            } else {
                result += "[";
            }
            for (size_t i = 0; i < tup->n_type; i++) {
                result += get_type_code(tup->m_type[i], use_underscore_sep,
                                        encode_dimensions_, set_dimensional_hint);
                if (i + 1 != tup->n_type) {
                    if( use_underscore_sep ) {
                        result += "_";
                    } else {
                        result += ", ";
                    }
                }
            }
            if( use_underscore_sep ) {
                result += "_";
            } else {
                result += "]";
            }
            return result;
        }
        case ASR::ttypeType::Set: {
            ASR::Set_t *s = ASR::down_cast<ASR::Set_t>(t);
            if( use_underscore_sep ) {
                return "set_" + get_type_code(s->m_type, use_underscore_sep,
                                              encode_dimensions_, set_dimensional_hint) + "_";
            }
            return "set[" + get_type_code(s->m_type, use_underscore_sep,
                                          encode_dimensions_, set_dimensional_hint) + "]";
        }
        case ASR::ttypeType::Dict: {
            ASR::Dict_t *d = ASR::down_cast<ASR::Dict_t>(t);
            if( use_underscore_sep ) {
                return "dict_" + get_type_code(d->m_key_type, use_underscore_sep,
                                               encode_dimensions_, set_dimensional_hint) +
                    "_" + get_type_code(d->m_value_type, use_underscore_sep,
                                        encode_dimensions_, set_dimensional_hint) + "_";
            }
            return "dict[" + get_type_code(d->m_key_type, use_underscore_sep,
                                           encode_dimensions_, set_dimensional_hint) +
                    ", " + get_type_code(d->m_value_type, use_underscore_sep,
                                         encode_dimensions_, set_dimensional_hint) + "]";
        }
        case ASR::ttypeType::List: {
            ASR::List_t *l = ASR::down_cast<ASR::List_t>(t);
            if( use_underscore_sep ) {
                return "list_" + get_type_code(l->m_type, use_underscore_sep,
                                               encode_dimensions_, set_dimensional_hint) + "_";
            }
            return "list[" + get_type_code(l->m_type, use_underscore_sep,
                                           encode_dimensions_, set_dimensional_hint) + "]";
        }
        case ASR::ttypeType::CPtr: {
            ASR::cptr_kindType kind = ASRUtils::get_cptr_kind_from_type(
                const_cast<ASR::ttype_t*>(t));
            if (kind != ASR::cptr_kindType::CPtrUnspecified) {
                std::string name = kind == ASR::cptr_kindType::CFunPointer
                    ? "c_funptr" : "c_ptr";
                return use_underscore_sep ? "CPtr_" + name + "_" : "CPtr[" + name + "]";
            }
            return "CPtr";
        }
        case ASR::ttypeType::StructType: {
            ASR::StructType_t* struct_type = ASR::down_cast<ASR::StructType_t>(t);
            if ( expr != nullptr || type_declaration != nullptr ) {
                ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(expr != nullptr
                    ? ASRUtils::get_struct_sym_from_struct_expr(expr) : type_declaration);
                res = symbol_name(sym);
                if (sym != nullptr && !res.empty() && res[0] != '~' &&
                        ASR::is_a<ASR::Struct_t>(*sym)) {
                    ASR::Struct_t* struct_decl = ASR::down_cast<ASR::Struct_t>(sym);
                    if (struct_decl->m_symtab && struct_decl->m_symtab->parent &&
                            struct_decl->m_symtab->parent->asr_owner &&
                            ASR::is_a<ASR::symbol_t>(*struct_decl->m_symtab->parent->asr_owner)) {
                        ASR::symbol_t* parent_sym = ASR::down_cast<ASR::symbol_t>(
                            struct_decl->m_symtab->parent->asr_owner);
                        res = std::string(symbol_name(parent_sym)) + "." + res;
                    }
                }
            } else {
                res = "StructType";
            }
            if (!struct_type->m_is_cstruct) {
                res = res + "_Class";
            }
            break;
        }
        case ASR::ttypeType::EnumType: {
            ASR::EnumType_t* enum_type = ASR::down_cast<ASR::EnumType_t>(t);
            res = ASRUtils::symbol_name(
                ASRUtils::symbol_get_past_external(enum_type->m_enum_type));
            break;
        }
        case ASR::ttypeType::UnionType: {
            if ( expr != nullptr || type_declaration != nullptr ) {
                ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(expr != nullptr
                    ? ASRUtils::get_union_sym_from_union_expr(expr) : type_declaration);
                res = symbol_name(sym);
            } else {
                res = "UnionType";
            }
            break;
        }
        case ASR::ttypeType::Pointer: {
            ASR::Pointer_t* p = ASR::down_cast<ASR::Pointer_t>(t);
            if( use_underscore_sep ) {
                return "Pointer_" + get_type_code(p->m_type, use_underscore_sep,
                                                  encode_dimensions_, set_dimensional_hint) + "_";
            }
            return "Pointer[" + get_type_code(p->m_type, use_underscore_sep,
                                              encode_dimensions_, set_dimensional_hint) + "]";
        }
        case ASR::ttypeType::Allocatable: {
            ASR::Allocatable_t* p = ASR::down_cast<ASR::Allocatable_t>(t);
            if( use_underscore_sep ) {
                return "Allocatable_" + get_type_code(p->m_type, use_underscore_sep,
                                                  encode_dimensions_, set_dimensional_hint) + "_";
            }
            return "Allocatable[" + get_type_code(p->m_type, use_underscore_sep,
                                              encode_dimensions_, set_dimensional_hint) + "]";
        }
        case ASR::ttypeType::SymbolicExpression: {
            return "S";
        }
        case ASR::ttypeType::TypeParameter: {
            ASR::TypeParameter_t *tp = ASR::down_cast<ASR::TypeParameter_t>(t);
            return tp->m_param;
        }
        default: {
            throw LCompilersException("Type encoding not implemented for "
                                      + ASRUtils::type_to_str_python_expr(t, expr));
        }
    }
    if( is_dimensional && set_dimensional_hint ) {
        res += "dim";
    }
    return res;
}

std::string type_to_str_python_symbol(const ASR::ttype_t *t, ASR::symbol_t* struct_sym)
{
    switch (t->type) {
        case ASR::ttypeType::Array: {
            ASR::Array_t* array_t = ASR::down_cast<ASR::Array_t>(t);
            std::string res = type_to_str_python_symbol(array_t->m_type, struct_sym);
            std::string dim_info = type_encode_dims(array_t->n_dims, array_t->m_dims);
            res += dim_info;
            return res;
        }
        case ASR::ttypeType::Integer: {
            ASR::Integer_t *i = ASR::down_cast<ASR::Integer_t>(t);
            std::string res = "";
            switch (i->m_kind) {
                case 1: { res = "i8"; break; }
                case 2: { res = "i16"; break; }
                case 4: { res = "i32"; break; }
                case 8: { res = "i64"; break; }
                default: { throw LCompilersException("Integer kind not supported"); }
            }
            return res;
        }
        case ASR::ttypeType::UnsignedInteger: {
            ASR::UnsignedInteger_t *i = ASR::down_cast<ASR::UnsignedInteger_t>(t);
            std::string res = "";
            switch (i->m_kind) {
                case 1: { res = "u8"; break; }
                case 2: { res = "u16"; break; }
                case 4: { res = "u32"; break; }
                case 8: { res = "u64"; break; }
                default: { throw LCompilersException("UnsignedInteger kind not supported"); }
            }
            return res;
        }
        case ASR::ttypeType::Real: {
            ASR::Real_t *r = (ASR::Real_t*)t;
            std::string res = "";
            switch (r->m_kind) {
                case 4: { res = "f32"; break; }
                case 8: { res = "f64"; break; }
                case 16: { res = "f128"; break; }
                default: { throw LCompilersException("Float kind not supported"); }
            }
            return res;
        }
        case ASR::ttypeType::Complex: {
            ASR::Complex_t *c = (ASR::Complex_t*)t;
            switch (c->m_kind) {
                case 4: { return "c32"; }
                case 8: { return "c64"; }
                default: { throw LCompilersException("Complex kind not supported"); }
            }
        }
        case ASR::ttypeType::Logical: {
            return "bool";
        }
        case ASR::ttypeType::String: {
            return "str";
        }
        case ASR::ttypeType::Tuple: {
            ASR::Tuple_t *tup = ASR::down_cast<ASR::Tuple_t>(t);
            std::string result = "tuple[";
            for (size_t i=0; i<tup->n_type; i++) {
                result += type_to_str_python_symbol(tup->m_type[i], nullptr);
                if (i+1 != tup->n_type) {
                    result += ", ";
                }
            }
            result += "]";
            return result;
        }
        case ASR::ttypeType::Set: {
            ASR::Set_t *s = (ASR::Set_t *)t;
            return "set[" + type_to_str_python_symbol(s->m_type, nullptr) + "]";
        }
        case ASR::ttypeType::Dict: {
            ASR::Dict_t *d = (ASR::Dict_t *)t;
            std::string key = type_to_str_python_symbol(d->m_key_type, nullptr);
            std::string value = type_to_str_python_symbol(d->m_value_type, nullptr);
            return "dict[" + key + ", " + value + "]";
        }
        case ASR::ttypeType::List: {
            ASR::List_t *l = (ASR::List_t *)t;
            return "list[" + type_to_str_python_symbol(l->m_type, nullptr) + "]";
        }
        case ASR::ttypeType::CPtr: {
            ASR::cptr_kindType kind = ASRUtils::get_cptr_kind_from_type(
                const_cast<ASR::ttype_t*>(t));
            if (kind != ASR::cptr_kindType::CPtrUnspecified) {
                return kind == ASR::cptr_kindType::CFunPointer
                    ? "CPtr[c_funptr]" : "CPtr[c_ptr]";
            }
            return "CPtr";
        }
        case ASR::ttypeType::StructType: {
            if (struct_sym != nullptr) {
                return ASRUtils::symbol_name(struct_sym);
            }
            ASR::StructType_t* st = ASR::down_cast<ASR::StructType_t>(t);
            std::string s = "struct";
            for (size_t i = 0; i < st->n_data_member_types; i++) {
                s += "_" + type_to_str_python_symbol(st->m_data_member_types[i], nullptr);
            }
            return s;
        }
        case ASR::ttypeType::EnumType: {
            ASR::EnumType_t* d = ASR::down_cast<ASR::EnumType_t>(t);
            return "enum " + std::string(symbol_name(d->m_enum_type));
        }
        case ASR::ttypeType::UnionType: {
            /*ASR::UnionType_t* d = ASR::down_cast<ASR::UnionType_t>(t);*/
            return "UnionType";
        }
        case ASR::ttypeType::Pointer: {
            ASR::Pointer_t* p = ASR::down_cast<ASR::Pointer_t>(t);
            return "Pointer[" + type_to_str_python_symbol(p->m_type, struct_sym) + "]";
        }
        case ASR::ttypeType::Allocatable: {
            ASR::Allocatable_t* p = ASR::down_cast<ASR::Allocatable_t>(t);
            return "Allocatable[" + type_to_str_python_symbol(p->m_type, struct_sym) + "]";
        }
        case ASR::ttypeType::TypeParameter: {
            ASR::TypeParameter_t *p = ASR::down_cast<ASR::TypeParameter_t>(t);
            return p->m_param;
        }
        case ASR::ttypeType::SymbolicExpression: {
            return "S";
        }
        case ASR::ttypeType::FunctionType: {
            ASR::FunctionType_t* ft = ASR::down_cast<ASR::FunctionType_t>(t);
            std::string result = "Func[";
            for (size_t i = 0; i < ft->n_arg_types; i++) {
                if (i > 0) {
                    result += ",";
                }
                if (ft->m_arg_types[i] != nullptr) {
                    result += type_to_str_python_symbol(ft->m_arg_types[i], nullptr);
                } else {
                    result += "null";
                }
            }
            result += "]->";
            if (ft->m_return_var_type != nullptr) {
                result += type_to_str_python_symbol(ft->m_return_var_type, nullptr);
            } else {
                result += "null";
            }
            return result;
        }
        default : throw LCompilersException("Not implemented " + std::to_string(t->type));
    }
}

std::string binop_to_str_python(const ASR::binopType t) {
    switch (t) {
        case (ASR::binopType::Add): { return " + "; }
        case (ASR::binopType::Sub): { return " - "; }
        case (ASR::binopType::Mul): { return "*"; }
        case (ASR::binopType::Div): { return "/"; }
        case (ASR::binopType::BitAnd): { return "&"; }
        case (ASR::binopType::BitOr): { return "|"; }
        case (ASR::binopType::BitXor): { return "^"; }
        case (ASR::binopType::BitLShift): { return "<<"; }
        case (ASR::binopType::BitRShift): { return ">>"; }
        case (ASR::binopType::LBitRShift): { return ">>"; }
        default : throw LCompilersException("Cannot represent the binary operator as a string");
    }
}

Vec<ASR::call_arg_t> get_arg_values(Allocator &al, const Vec<ASR::call_arg_t>& args) {
    Vec<ASR::call_arg_t> values;
    values.reserve(al, args.size());
    for (auto &a : args) {
        ASR::expr_t *v = expr_value(a.m_value);
        if (v == nullptr) return values;
        ASR::call_arg_t v_arg;
        v_arg.loc = v->base.loc, v_arg.m_value = v;
        values.push_back(al, v_arg);
    }
    return values;
}

Vec<char*> get_scope_names(Allocator &al, const SymbolTable *symtab) {
    Vec<char*> scope_names;
    scope_names.reserve(al, 4);
    const SymbolTable *s = symtab;
    while (s->parent != nullptr && !is_tu_scope(s)) {
        char *owner_name = symbol_name(ASR::down_cast<ASR::symbol_t>(s->asr_owner));
        scope_names.push_back(al, owner_name);
        s = s->parent;
    }
    return scope_names;
}

ASR::expr_t* get_constant_zero_with_given_type(Allocator& al, ASR::ttype_t* asr_type) {
    asr_type = ASRUtils::type_get_past_pointer(asr_type);
    asr_type = ASRUtils::type_get_past_array(asr_type);
    switch (asr_type->type) {
        case ASR::ttypeType::Integer: {
            return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, asr_type->base.loc, 0, asr_type));
        }
        case ASR::ttypeType::Real: {
            return ASRUtils::make_RealConstant_util(al, asr_type->base.loc, 0.0, asr_type);
        }
        case ASR::ttypeType::Complex: {
            return ASRUtils::EXPR(ASR::make_ComplexConstant_t(al, asr_type->base.loc, 0.0, 0.0, asr_type));
        }
        case ASR::ttypeType::Logical: {
            return ASRUtils::EXPR(ASR::make_LogicalConstant_t(al, asr_type->base.loc, false, asr_type));
        }
        case ASR::ttypeType::String: {
            ASR::String_t* str_type = ASR::down_cast<ASR::String_t>(asr_type);
            int64_t len = 1;
            if (str_type->m_len) {
                ASRUtils::extract_value(str_type->m_len, len);
            }
            std::string blank(len, ' ');
            return ASRUtils::EXPR(ASR::make_StringConstant_t(al, asr_type->base.loc,
                s2c(al, blank), asr_type));
        }
        default: {
            throw LCompilersException("get_constant_zero_with_given_type: Not implemented " + std::to_string(asr_type->type));
        }
    }
    return nullptr;
}

ASR::expr_t* get_constant_one_with_given_type(Allocator& al, ASR::ttype_t* asr_type) {
    asr_type = ASRUtils::type_get_past_array(asr_type);
    switch (asr_type->type) {
        case ASR::ttypeType::Integer: {
            return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, asr_type->base.loc, 1, asr_type));
        }
        case ASR::ttypeType::Real: {
            return ASRUtils::make_RealConstant_util(al, asr_type->base.loc, 1.0, asr_type);
        }
        case ASR::ttypeType::Complex: {
            return ASRUtils::EXPR(ASR::make_ComplexConstant_t(al, asr_type->base.loc, 1.0, 0.0, asr_type));
        }
        case ASR::ttypeType::Logical: {
            return ASRUtils::EXPR(ASR::make_LogicalConstant_t(al, asr_type->base.loc, true, asr_type));
        }
        default: {
            throw LCompilersException("get_constant_one_with_given_type: Not implemented " + std::to_string(asr_type->type));
        }
    }
    return nullptr;
}

ASR::expr_t* get_string_filled_with_char(Allocator& al,
        ASR::ttype_t* string_type, unsigned char fill) {
    ASR::String_t* str_type = ASR::down_cast<ASR::String_t>(string_type);
    int64_t len = -1;
    if (!str_type->m_len || !ASRUtils::extract_value(str_type->m_len, len) || len < 0) {
        return nullptr;
    }
    std::string value((size_t) len, (char) fill);
    return ASRUtils::EXPR(ASR::make_StringConstant_t(al, string_type->base.loc,
        s2c(al, value), string_type));
}

ASR::expr_t* get_minimum_value_with_given_type(Allocator& al, ASR::ttype_t* asr_type) {
    asr_type = ASRUtils::type_get_past_array(asr_type);
    int kind = ASRUtils::extract_kind_from_ttype_t(asr_type);
    switch (asr_type->type) {
        case ASR::ttypeType::Integer: {
            int64_t val;
            switch (kind) {
                case 1: val = std::numeric_limits<int8_t>::min(); break;
                case 2: val = std::numeric_limits<int16_t>::min(); break;
                case 4: val = std::numeric_limits<int32_t>::min(); break;
                case 8: val = std::numeric_limits<int64_t>::min(); break;
                default:
                    // integer kinds are restricted to 1, 2, 4 and 8 in semantics
                    LCOMPILERS_ASSERT(false);
                    val = 0;
            }
            return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, asr_type->base.loc, val, asr_type));
        }
        case ASR::ttypeType::Real: {
            double val;
            switch (kind) {
                case 4: val = std::numeric_limits<float>::lowest(); break;
                case 8: val = std::numeric_limits<double>::lowest(); break;
                default:
                    throw LCompilersException("get_minimum_value_with_given_type: Unsupported real kind " + std::to_string(kind));
            }
            return ASRUtils::EXPR(ASR::make_RealConstant_t(al, asr_type->base.loc, val, asr_type));
        }
        default: {
            // MaxVal and MinVal only accept integer, real and character arrays,
            // and a character reduction is seeded with its identity instead
            LCOMPILERS_ASSERT(false);
        }
    }
    return nullptr;
}

ASR::expr_t* get_maximum_value_with_given_type(Allocator& al, ASR::ttype_t* asr_type) {
    asr_type = ASRUtils::type_get_past_array(asr_type);
    int kind = ASRUtils::extract_kind_from_ttype_t(asr_type);
    switch (asr_type->type) {
        case ASR::ttypeType::Integer: {
            int64_t val;
            switch (kind) {
                case 1: val = std::numeric_limits<int8_t>::max(); break;
                case 2: val = std::numeric_limits<int16_t>::max(); break;
                case 4: val = std::numeric_limits<int32_t>::max(); break;
                case 8: val = std::numeric_limits<int64_t>::max(); break;
                default:
                    // integer kinds are restricted to 1, 2, 4 and 8 in semantics
                    LCOMPILERS_ASSERT(false);
                    val = 0;
            }
            return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, asr_type->base.loc, val, asr_type));
        }
        case ASR::ttypeType::Real: {
            double val;
            switch (kind) {
                case 4: val = std::numeric_limits<float>::max(); break;
                case 8: val = std::numeric_limits<double>::max(); break;
                default:
                    throw LCompilersException("get_maximum_value_with_given_type: Unsupported real kind " + std::to_string(kind));
            }
            return ASRUtils::EXPR(ASR::make_RealConstant_t(al, asr_type->base.loc, val, asr_type));
        }
        default: {
            // MaxVal and MinVal only accept integer, real and character arrays,
            // and a character reduction is seeded with its identity instead
            LCOMPILERS_ASSERT(false);
        }
    }
    return nullptr;
}

bool has_hidden_string_lengths(const ASR::FunctionType_t &ft) {
    switch (ft.m_abi) {
        case ASR::abiType::Source:
        case ASR::abiType::LFortranModule:
        case ASR::abiType::GFortranModule:
        case ASR::abiType::ExternalUndefined:
            break;
        default:
            return false;
    }
    return ft.m_exec_space == ASR::exec_spaceType::Host;
}

bool is_string_dummy_with_hidden_length(
        const ASR::FunctionType_t &ft, const ASR::Variable_t &v) {
    if (!has_hidden_string_lengths(ft) || !is_arg_dummy(v.m_intent) ||
            v.m_value_attr || v.n_codims > 0) {
        return false;
    }
    ASR::ttype_t *t = v.m_type;
    if (ASR::is_a<ASR::Array_t>(*t)) {
        ASR::Array_t *array = ASR::down_cast<ASR::Array_t>(t);
        if (array->m_physical_type != ASR::array_physical_typeType::PointerArray &&
                array->m_physical_type !=
                    ASR::array_physical_typeType::UnboundedPointerArray) {
            return false;
        }
        t = array->m_type;
    }
    if (!ASR::is_a<ASR::String_t>(*t)) {
        return false;
    }
    ASR::String_t *str = ASR::down_cast<ASR::String_t>(t);
    return str->m_physical_type == ASR::string_physical_typeType::DescriptorString &&
        str->m_len_kind != ASR::string_length_kindType::DeferredLength;
}

bool is_string_dummy_with_hidden_length(
        const ASR::Function_t &fn, ASR::expr_t *x) {
    if (!ASR::is_a<ASR::Var_t>(*x)) {
        return false;
    }
    ASR::symbol_t *sym = symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(x)->m_v);
    return ASR::is_a<ASR::Variable_t>(*sym) && is_string_dummy_with_hidden_length(
        *ASR::down_cast<ASR::FunctionType_t>(fn.m_function_signature),
        *ASR::down_cast<ASR::Variable_t>(sym));
}

bool procedure_types_identical(ASR::FunctionType_t *a,
        ASR::FunctionType_t *b) {
    if (is_bare_implicit_interface(*a) != is_bare_implicit_interface(*b)
            || a->n_arg_types != b->n_arg_types || a->m_abi != b->m_abi
            || (a->m_return_var_type == nullptr)
                != (b->m_return_var_type == nullptr)) {
        return false;
    }
    if (a->m_return_var_type && !types_equal(a->m_return_var_type,
            b->m_return_var_type, nullptr, nullptr, true)) {
        return false;
    }
    for (size_t i = 0; i < a->n_arg_types; i++) {
        if (!types_equal(a->m_arg_types[i], b->m_arg_types[i], nullptr,
                nullptr, true)) {
            return false;
        }
    }
    return true;
}

std::vector<std::string> string_value_characters(
        const char* s, int64_t kind) {
    std::string str(s);
    if (kind == 1) {
        std::vector<std::string> characters;
        characters.reserve(str.size());
        for (char c : str) characters.push_back(std::string(1, c));
        return characters;
    }
    return utf8_split(str);
}

bool is_type_parameter(ASR::ttype_t &x) {
    switch (x.type) {
        case ASR::ttypeType::List: {
            ASR::List_t *list_type = ASR::down_cast<ASR::List_t>(type_get_past_pointer(&x));
            return is_type_parameter(*list_type->m_type);
        }
        case ASR::ttypeType::Array: {
            ASR::Array_t *arr_type = ASR::down_cast<ASR::Array_t>(type_get_past_pointer(&x));
            return is_type_parameter(*arr_type->m_type);
        }
        default : return ASR::is_a<ASR::TypeParameter_t>(*type_get_past_pointer(&x));
    }
}

bool is_generic_function(ASR::symbol_t *x) {
    ASR::symbol_t* x2 = symbol_get_past_external(x);
    switch (x2->type) {
        case ASR::symbolType::Function: {
            if (is_requirement_function(x2)) {
                return false;
            }
            ASR::Function_t *func = ASR::down_cast<ASR::Function_t>(x2);
            ASR::FunctionType_t *func_type
                = ASR::down_cast<ASR::FunctionType_t>(func->m_function_signature);
            for (size_t i=0; i<func_type->n_arg_types; i++) {
                if (is_type_parameter(*func_type->m_arg_types[i])) {
                    return true;
                }
            }
            return func_type->m_return_var_type
                   && is_type_parameter(*func_type->m_return_var_type);
        }
        default: return false;
    }
}

bool is_template_arg(ASR::symbol_t *x, std::string arg_name) {
    switch (x->type) {
        case ASR::symbolType::Template: {
            ASR::Template_t *t = ASR::down_cast<ASR::Template_t>(x);
            for (size_t i=0; i < t->n_args; i++) {
                std::string arg = t->m_args[i];
                if (arg.compare(arg_name) == 0) {
                    return true;
                }
            }
            break;
        }
        default: {
            return false;
        }
    }
    return false;
}

int get_body_size(ASR::symbol_t* s) {
    int n_body = 0;
    switch (s->type) {
        case ASR::symbolType::Function: {
            ASR::Function_t* f = ASR::down_cast<ASR::Function_t>(s);
            n_body = checked_integer_cast<int>(f->n_body);
            break;
        }
        case ASR::symbolType::Program: {
            ASR::Program_t* p = ASR::down_cast<ASR::Program_t>(s);
            n_body = checked_integer_cast<int>(p->n_body);
            break;
        }
        default: {
            n_body = -1;
        }
    }
    return n_body;
}

size_t extract_dimensions_from_ttype(ASR::ttype_t *x,
                                         ASR::dimension_t*& m_dims) {
    size_t n_dims {};
    switch (x->type) {
        case ASR::ttypeType::Array: {
            ASR::Array_t* array_t = ASR::down_cast<ASR::Array_t>(x);
            n_dims = array_t->n_dims;
            m_dims = array_t->m_dims;
            break;
        }
        case ASR::ttypeType::Pointer: {
            n_dims = extract_dimensions_from_ttype(ASR::down_cast<ASR::Pointer_t>(x)->m_type, m_dims);
            break;
        }
        case ASR::ttypeType::Allocatable: {
            n_dims = extract_dimensions_from_ttype(ASR::down_cast<ASR::Allocatable_t>(x)->m_type, m_dims);
            break;
        }
        case ASR::ttypeType::SymbolicExpression:
        case ASR::ttypeType::Integer:
        case ASR::ttypeType::UnsignedInteger:
        case ASR::ttypeType::Real:
        case ASR::ttypeType::Complex:
        case ASR::ttypeType::String:
        case ASR::ttypeType::Logical:
        case ASR::ttypeType::StructType:
        case ASR::ttypeType::EnumType:
        case ASR::ttypeType::UnionType:
        case ASR::ttypeType::List:
        case ASR::ttypeType::Tuple:
        case ASR::ttypeType::Dict:
        case ASR::ttypeType::Set:
        case ASR::ttypeType::CPtr:
        case ASR::ttypeType::TypeParameter:
        case ASR::ttypeType::FunctionType: {
            n_dims = 0;
            m_dims = nullptr;
            break;
        }
        default:
            throw LCompilersException("Not implemented " + std::to_string(x->type) + ".");
    }
    return n_dims;
}

bool is_fixed_size_array(ASR::dimension_t* m_dims, size_t n_dims) {
    if( n_dims == 0 ) {
        return false;
    }
    for( size_t i = 0; i < n_dims; i++ ) {
        int64_t dim_size = -1;
        if( m_dims[i].m_length == nullptr ) {
            return false;
        }
        if( !ASRUtils::extract_value(ASRUtils::expr_value(m_dims[i].m_length), dim_size) ) {
            return false;
        }
    }
    return true;
}

int64_t get_fixed_size_of_array(ASR::dimension_t* m_dims, size_t n_dims) {
    if( n_dims == 0 ) {
        return 0;
    }
    int64_t array_size = 1;
    for( size_t i = 0; i < n_dims; i++ ) {
        int64_t dim_size = -1;
        if( (m_dims[i].m_length == nullptr) ||
            !ASRUtils::extract_value(ASRUtils::expr_value(m_dims[i].m_length), dim_size) ) {
            return -1;
        }
        array_size *= dim_size;
    }
    return array_size;
}

int64_t get_fixed_size_of_ArraySection(ASR::ArraySection_t* x) {
    if (x->n_args == 0) {
        return 0;
    }
    int64_t array_size = 1;
    for (size_t i = 0; i < x->n_args; i++) {
        if (x->m_args[i].m_left && x->m_args[i].m_right && ASRUtils::is_value_constant(x->m_args[i].m_right) &&
            ASRUtils::is_value_constant(x->m_args[i].m_left)) {
            ASR::IntegerConstant_t* start = ASR::down_cast<ASR::IntegerConstant_t>(ASRUtils::expr_value(x->m_args[i].m_left));
            ASR::IntegerConstant_t* end = ASR::down_cast<ASR::IntegerConstant_t>(ASRUtils::expr_value(x->m_args[i].m_right));
            array_size = array_size * (end->m_n - start->m_n + 1);
        } else {
            return -1;
        }
    }
    return array_size;
}

std::pair<int64_t, int64_t> compute_type_size_align(ASR::ttype_t* type) {
    if (ASR::is_a<ASR::Array_t>(*type)) {
        ASR::Array_t* arr = ASR::down_cast<ASR::Array_t>(type);
        int64_t n_elem = get_fixed_size_of_array(arr->m_dims, arr->n_dims);
        if (n_elem <= 0) return {-1, -1};
        auto [elem_size, elem_align] = compute_type_size_align(arr->m_type);
        if (elem_size < 0) return {-1, -1};
        return {n_elem * elem_size, elem_align};
    }
    type = type_get_past_array(
               type_get_past_allocatable(
                   type_get_past_pointer(type)));
    if (ASR::is_a<ASR::Integer_t>(*type) ||
        ASR::is_a<ASR::UnsignedInteger_t>(*type) ||
        ASR::is_a<ASR::Real_t>(*type) ||
        ASR::is_a<ASR::Logical_t>(*type)) {
        int64_t kind = extract_kind_from_ttype_t(type);
        if (kind == 10 && ASR::is_a<ASR::Real_t>(*type)) {
#if defined(__APPLE__) && !defined(__aarch64__)
            // macOS x86-64: long double mapped to 64-bit double by Apple
            return {8, 8};
#else
            // Linux x86-64 (x86_fp80, padded to 16) or Apple Silicon (fp128)
            return {16, 16};
#endif
        }
        if (kind > 0) return {kind, kind};
        return {-1, -1};
    } else if (ASR::is_a<ASR::Complex_t>(*type)) {
        int64_t kind = extract_kind_from_ttype_t(type);
        if (kind <= 0) return {-1, -1};
        if (kind == 10) return {32, 16};
        // LFortran maps complex to a packed LLVM struct {float,float}
        // or {double,double}, so alignment is 1 (packed).
        return {2 * kind, 1};
    } else if (ASR::is_a<ASR::CPtr_t>(*type)) {
        return {8, 8};
    } else if (ASR::is_a<ASR::String_t>(*type)) {
        ASR::String_t* st = ASR::down_cast<ASR::String_t>(type);
        int64_t kind = st->m_kind;
        if (kind <= 0) return {-1, -1};
        int64_t len = 1;
        if (st->m_len == nullptr ||
            !ASRUtils::extract_value(st->m_len, len)) {
            return {-1, -1};
        }
        if (len < 0) return {-1, -1};
        return {kind * len, 1};
    } else if (ASR::is_a<ASR::StructType_t>(*type)) {
        ASR::StructType_t* st = ASR::down_cast<ASR::StructType_t>(type);
        int64_t offset = 0;
        int64_t max_align = 1;
        for (size_t i = 0; i < st->n_data_member_types; i++) {
            auto [size, align] = compute_struct_member_size_align(
                st->m_data_member_types[i]);
            if (size < 0) return {-1, -1};
            offset = ((offset + align - 1) / align) * align;
            offset += size;
            if (align > max_align) max_align = align;
        }
        // Pad struct to its alignment
        offset = ((offset + max_align - 1) / max_align) * max_align;
        if (offset == 0) offset = 1;
        return {offset, max_align};
    } else {
        return {-1, -1};
    }
}

std::pair<int64_t, int64_t>
    compute_struct_type_size_align(ASR::Struct_t* struct_type) {
        int64_t offset = 0;
        int64_t max_align = 1;

        if (struct_type->m_parent != nullptr) {
            ASR::symbol_t* parent_sym = ASRUtils::symbol_get_past_external(
                struct_type->m_parent);
            if (parent_sym == nullptr || !ASR::is_a<ASR::Struct_t>(*parent_sym)) {
                return {-1, -1};
            }
            auto [size, align] = compute_struct_type_size_align(
                ASR::down_cast<ASR::Struct_t>(parent_sym));
            if (size < 0) return {-1, -1};
            offset += size;
            max_align = align;
        }

        for (size_t i = 0; i < struct_type->n_members; i++) {
            ASR::symbol_t* member_sym = struct_type->m_symtab->get_symbol(
                struct_type->m_members[i]);
            member_sym = ASRUtils::symbol_get_past_external(member_sym);
            if (member_sym == nullptr || !ASR::is_a<ASR::Variable_t>(*member_sym)) {
                return {-1, -1};
            }
            ASR::Variable_t* member_var = ASR::down_cast<ASR::Variable_t>(member_sym);
            ASR::ttype_t* member_type = member_var->m_type;
            ASR::symbol_t* member_struct = member_var->m_type_declaration == nullptr
                ? nullptr
                : ASRUtils::symbol_get_past_external(member_var->m_type_declaration);
            std::pair<int64_t, int64_t> member_layout;
            if (member_struct != nullptr && ASR::is_a<ASR::Struct_t>(*member_struct) &&
                    !ASR::is_a<ASR::Pointer_t>(*member_type) &&
                    !ASR::is_a<ASR::Allocatable_t>(*member_type) &&
                    ASR::is_a<ASR::StructType_t>(*type_get_past_array(member_type))) {
                // A component of derived type is laid out from its own declared
                // type, which is the only place its inherited components live.
                member_layout = compute_struct_type_size_align(
                    ASR::down_cast<ASR::Struct_t>(member_struct));
                if (member_layout.first < 0) return {-1, -1};
                if (ASR::is_a<ASR::Array_t>(*member_type)) {
                    int64_t n_elem = get_fixed_size_of_array(member_type);
                    if (n_elem <= 0) return {-1, -1};
                    member_layout.first *= n_elem;
                }
            } else {
                member_layout = compute_struct_member_size_align(member_type);
            }
            int64_t size = member_layout.first;
            int64_t align = member_layout.second;
            if (size < 0) return {-1, -1};
            if (!struct_type->m_is_packed) {
                offset = ((offset + align - 1) / align) * align;
                if (align > max_align) max_align = align;
            }
            offset += size;
        }

        if (!struct_type->m_is_packed) {
            offset = ((offset + max_align - 1) / max_align) * max_align;
        }
        if (offset == 0) offset = 1;
        return {offset, struct_type->m_is_packed ? 1 : max_align};
    }

int64_t get_struct_expr_byte_size(ASR::expr_t* expr) {
        if (expr == nullptr) {
            return -1;
        }
        ASR::symbol_t* struct_sym = ASRUtils::symbol_get_past_external(
            ASRUtils::get_struct_sym_from_struct_expr(expr));
        if (struct_sym == nullptr || !ASR::is_a<ASR::Struct_t>(*struct_sym)) {
            return -1;
        }
        auto [size, _align] = compute_struct_type_size_align(
            ASR::down_cast<ASR::Struct_t>(struct_sym));
        (void)_align;
        return size;
    }

int64_t get_type_byte_size(ASR::ttype_t* type) {
    if (type == nullptr) {
        return -1;
    }
    if (ASR::is_a<ASR::Array_t>(*type)) {
        ASR::Array_t* arr = ASR::down_cast<ASR::Array_t>(type);
        int64_t n_elem = get_fixed_size_of_array(
            arr->m_dims, arr->n_dims);
        if (n_elem <= 0) return -1;
        int64_t elem_size = get_type_byte_size(arr->m_type);
        if (elem_size < 0) return -1;
        return n_elem * elem_size;
    }
    auto [size, _align] = compute_type_size_align(type);
    (void)_align;
    return size;
}

size_t rectangular_loop_nest_depth(
        const std::vector<ASR::DoLoop_t*> &loops) {
    for (size_t level = 0; level < loops.size(); level++) {
        const ASR::do_loop_head_t &head = loops[level]->m_head;
        for (size_t outer = 0; outer < level; outer++) {
            ASR::expr_t *index = loops[outer]->m_head.m_v;
            if (index == nullptr || !ASR::is_a<ASR::Var_t>(*index)) continue;
            ASR::symbol_t *index_sym = ASR::down_cast<ASR::Var_t>(index)->m_v;
            if (expr_references_symbol(head.m_start, index_sym)
                    || expr_references_symbol(head.m_end, index_sym)
                    || expr_references_symbol(head.m_increment, index_sym)) {
                return level;
            }
        }
    }
    return loops.size();
}

bool is_dimension_dependent_only_on_arguments(ASR::dimension_t* m_dims, size_t n_dims, bool only_intent_in_args,
    bool allow_deferred_shape_members) {
    ExprDependentOnlyOnArguments visitor;
    visitor.only_intent_in_args = only_intent_in_args;
    visitor.allow_deferred_shape_members = allow_deferred_shape_members;
    for( size_t i = 0; i < n_dims; i++ ) {
        visitor.is_dependent_only_on_argument = true;
        if( m_dims[i].m_length == nullptr ) {
            return false;
        }
        visitor.visit_expr(*m_dims[i].m_length);
        if( !visitor.is_dependent_only_on_argument ) {
            return false;
        }
    }
    return true;
}

bool is_binop_expr(ASR::expr_t* x) {
    switch( x->type ) {
        case ASR::exprType::IntegerBinOp:
        case ASR::exprType::RealBinOp:
        case ASR::exprType::ComplexBinOp:
        case ASR::exprType::LogicalBinOp:
        case ASR::exprType::UnsignedIntegerBinOp:
        case ASR::exprType::IntegerCompare:
        case ASR::exprType::RealCompare:
        case ASR::exprType::ComplexCompare:
        case ASR::exprType::LogicalCompare:
        case ASR::exprType::UnsignedIntegerCompare:
        case ASR::exprType::StringCompare:
        case ASR::exprType::StringConcat: {
            return true;
        }
        default: {
            return false;
        }
    }
}

bool is_unaryop_expr(ASR::expr_t* x) {
    switch( x->type ) {
        case ASR::exprType::IntegerUnaryMinus:
        case ASR::exprType::RealUnaryMinus:
        case ASR::exprType::ComplexUnaryMinus:
        case ASR::exprType::LogicalNot: {
            return true;
        }
        default: {
            return false;
        }
    }
}

ASR::expr_t* extract_member_from_unaryop(ASR::expr_t* x) {
    #define UNARYOP_MEMBER_CASE(X, X_t) \
    case (ASR::exprType::X) : { \
        return ASR::down_cast<ASR::X_t>(x)->m_arg; \
    }

    switch (x->type) {
        UNARYOP_MEMBER_CASE(IntegerUnaryMinus, IntegerUnaryMinus_t)
        UNARYOP_MEMBER_CASE(RealUnaryMinus, RealUnaryMinus_t)
        UNARYOP_MEMBER_CASE(ComplexUnaryMinus, ComplexUnaryMinus_t)
        UNARYOP_MEMBER_CASE(LogicalNot, LogicalNot_t)
        default: {
            LCOMPILERS_ASSERT(false)
        }
    }

    return nullptr;
}

ASR::expr_t* extract_member_from_binop(ASR::expr_t* x, int8_t member) {
    #define BINOP_MEMBER_CASE(X, X_t) \
    case (ASR::exprType::X) : { \
        if( member == 0 ) { \
            return ASR::down_cast<ASR::X_t>(x)->m_left; \
        } else { \
            return ASR::down_cast<ASR::X_t>(x)->m_right; \
        } \
    }

    switch (x->type) {
        BINOP_MEMBER_CASE(IntegerBinOp, IntegerBinOp_t)
        BINOP_MEMBER_CASE(RealBinOp, RealBinOp_t)
        BINOP_MEMBER_CASE(ComplexBinOp, ComplexBinOp_t)
        BINOP_MEMBER_CASE(LogicalBinOp, LogicalBinOp_t)
        BINOP_MEMBER_CASE(UnsignedIntegerBinOp, UnsignedIntegerBinOp_t)
        BINOP_MEMBER_CASE(IntegerCompare, IntegerCompare_t)
        BINOP_MEMBER_CASE(RealCompare, RealCompare_t)
        BINOP_MEMBER_CASE(ComplexCompare, ComplexCompare_t)
        BINOP_MEMBER_CASE(LogicalCompare, LogicalCompare_t)
        BINOP_MEMBER_CASE(UnsignedIntegerCompare, UnsignedIntegerCompare_t)
        BINOP_MEMBER_CASE(StringCompare, StringCompare_t)
        BINOP_MEMBER_CASE(StringConcat, StringConcat_t)
        default: {
            LCOMPILERS_ASSERT(false)
        }
    }

    return nullptr;
}

ASR::symbol_t* import_symbol(Allocator &al, ASR::symbol_t* sym,
        SymbolTable* scope, ASR::accessType access,
        const std::string &name_prefix, const std::string &name_suffix) {
    if (is_named_from(sym, scope)) return sym;
    ASR::symbol_t* definition = symbol_get_past_external(sym);
    if (definition == nullptr || symbol_parent_symtab(definition) == nullptr) {
        return sym;
    }
    if (is_named_from(definition, scope)) return definition;
    std::string name = symbol_name(definition);
    ASR::symbol_t* existing = scope->resolve_symbol(name);
    if (existing != nullptr && symbol_get_past_external(existing) == definition &&
            is_named_from(existing, scope)) {
        return existing;
    }
    for (auto &item : scope->get_scope()) {
        if (ASR::is_a<ASR::ExternalSymbol_t>(*item.second) &&
                symbol_get_past_external(item.second) == definition &&
                is_named_from(item.second, scope)) {
            return item.second;
        }
    }
    ASR::symbol_t* module_sym = get_asr_owner(definition);
    if (module_sym == nullptr || (!ASR::is_a<ASR::Module_t>(*module_sym) &&
            !ASR::is_a<ASR::Struct_t>(*module_sym))) {
        return sym;
    }
    std::string local_name = name_prefix + name + name_suffix;
    for (int i = 1; scope->resolve_symbol(local_name) != nullptr; i++) {
        local_name = name_prefix + name + name_suffix + std::to_string(i);
    }
    ASR::symbol_t* imported = ASR::down_cast<ASR::symbol_t>(
        ASR::make_ExternalSymbol_t(al, definition->base.loc, scope,
            s2c(al, local_name), definition, symbol_name(module_sym),
            nullptr, 0, s2c(al, name), access));
    scope->add_symbol(local_name, imported);
    return imported;
}

ASR::ttype_t* import_procedure_type(Allocator &al,
        ASR::ttype_t* type, SymbolTable* scope) {
    if (type == nullptr || scope == nullptr ||
            !ASR::is_a<ASR::FunctionType_t>(*type_get_past_pointer(type))) {
        return type;
    }
    ProcedureTypeImporter checker(al, scope, false);
    checker.replace_ttype(type);
    if (!checker.needed) return type;
    ASR::ttype_t* new_type = ExprStmtDuplicator(al).duplicate_ttype(type);
    ProcedureTypeImporter(al, scope, true).replace_ttype(new_type);
    return new_type;
}

ASR::symbol_t* import_type_declaration(Allocator &al,
        ASR::symbol_t* type_declaration, SymbolTable* scope) {
    if (type_declaration == nullptr || scope == nullptr) {
        return type_declaration;
    }
    ASR::symbol_t* definition = symbol_get_past_external(type_declaration);
    if (definition == nullptr) return type_declaration;
    // An unlimited polymorphic type is synthesised per scope rather than
    // declared anywhere, so there is no module to import it from; every
    // scope that needs one already has its own.
    if (ASR::is_a<ASR::Struct_t>(*definition)) {
        ASR::ttype_t* signature =
            ASR::down_cast<ASR::Struct_t>(definition)->m_struct_signature;
        if (signature != nullptr && ASR::is_a<ASR::StructType_t>(*signature) &&
                ASR::down_cast<ASR::StructType_t>(
                    signature)->m_is_unlimited_polymorphic) {
            return import_struct_type(al, definition, scope);
        }
    }
    // A type declaration names either a type or, for `procedure(iface) ::`,
    // the procedure whose interface it takes. Both are imported the same way.
    if (!ASR::is_a<ASR::Struct_t>(*definition) &&
            !ASR::is_a<ASR::Enum_t>(*definition) &&
            !ASR::is_a<ASR::Union_t>(*definition) &&
            !ASR::is_a<ASR::Function_t>(*definition)) {
        return type_declaration;
    }
    return import_symbol(al, type_declaration, scope, ASR::accessType::Public);
}

void set_cptr_type_declaration(ASR::ttype_t* type,
        ASR::symbol_t* type_declaration) {
    if (type == nullptr || type_declaration == nullptr) {
        return;
    }
    ASR::ttype_t* scalar = ASRUtils::extract_type(type);
    if (ASR::is_a<ASR::CPtr_t>(*scalar)) {
        ASR::CPtr_t* cptr = ASR::down_cast<ASR::CPtr_t>(scalar);
        if (cptr->m_kind == ASR::cptr_kindType::CPtrUnspecified) {
            std::string name = ASRUtils::symbol_name(
                ASRUtils::symbol_get_past_external(type_declaration));
            if (name == "c_funptr") {
                cptr->m_kind = ASR::cptr_kindType::CFunPointer;
            } else if (name == "c_ptr") {
                cptr->m_kind = ASR::cptr_kindType::CPointer;
            }
        }
    }
}

ASR::asr_t* make_Variable_t_util(Allocator &al, const Location &a_loc,
    SymbolTable* a_parent_symtab, char* a_name, char** a_dependencies, size_t n_dependencies,
    ASR::intentType a_intent, ASR::expr_t* a_symbolic_value, ASR::expr_t* a_value, ASR::storage_typeType a_storage,
    ASR::ttype_t* a_type, ASR::symbol_t* a_type_declaration, ASR::abiType a_abi, ASR::accessType a_access, ASR::presenceType a_presence,
    bool a_value_attr, bool a_target_attr, bool a_contiguous_attr,
    char* a_bindc_name, bool a_is_volatile, bool a_is_protected,
    ASR::pass_attrType a_pass_attr, char* a_self_argument,
    ASR::codimension_t* a_codims, size_t n_codims,
    ASR::symbol_t* a_common_block) {
    a_type_declaration = import_type_declaration(
        al, a_type_declaration, a_parent_symtab);
    a_type = import_procedure_type(al, a_type, a_parent_symtab);
    set_cptr_type_declaration(a_type, a_type_declaration);
    return ASR::make_Variable_t(al, a_loc, a_parent_symtab, a_name, a_dependencies,
        n_dependencies, a_intent, a_symbolic_value,  a_value,  a_storage, a_type,
        a_type_declaration,  a_abi, a_access, a_presence, a_value_attr,
        a_target_attr, a_contiguous_attr, a_bindc_name, a_is_volatile, a_is_protected,
        a_pass_attr, a_self_argument, a_codims, n_codims, a_common_block
    );
}

ASR::ttype_t* make_Array_t_util(Allocator& al, const Location& loc,
    ASR::ttype_t* type, ASR::dimension_t* m_dims, size_t n_dims,
    ASR::abiType abi, bool is_argument,
    ASR::array_physical_typeType physical_type,
    bool override_physical_type, bool is_dimension_star, bool for_type,
    ASR::memory_spaceType memory_space) {
    if( n_dims == 0 ) {
        // An Array with no dimensions is a scalar, with one exception: an
        // assumed-rank array is represented as an Array carrying no
        // dimensions, because its rank is unknown until a `select rank`
        // selects one. Asking for that physical type is therefore a request
        // for an assumed-rank array, not for the element type.
        if( physical_type != ASR::array_physical_typeType::AssumedRankArray ) {
            return type;
        }
        // The detection below infers the physical type from the dimensions,
        // and there are none here, so keep the one the caller asked for.
        override_physical_type = true;
    }

    for( size_t i = 0; i < n_dims; i++ ) {
        if( m_dims[i].m_length && ASR::is_a<ASR::ArraySize_t>(*m_dims[i].m_length) ) {
            ASR::ArraySize_t* as = ASR::down_cast<ASR::ArraySize_t>(m_dims[i].m_length);
            int64_t compile_time_size = -1;
            if( ASRUtils::extract_value(as->m_value, compile_time_size) ) {
                // Rebuilding the size from the operand's dimensions would
                // drop its compile-time value and turn a fixed-size array,
                // e.g. the result of `spread(x, 1, size(a))` for a parameter
                // `a`, into one of unknown size. Keep the extent constant.
                m_dims[i].m_length = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                    al, as->base.base.loc, compile_time_size, as->m_type));
                continue;
            }
            m_dims[i].m_length = ASRUtils::EXPR(ASRUtils::make_ArraySize_t_util(
                al, as->base.base.loc, as->m_v, as->m_dim, as->m_type, nullptr, for_type));
        }
    }

    if( !override_physical_type ) {
        if( abi == ASR::abiType::BindC ) {
            if( ASRUtils::is_fixed_size_array(m_dims, n_dims) && !is_argument ) {
                // bind(C) module/global fixed-size arrays use inline storage
                // to match C layout (e.g., int32_t arr[4] is 16 bytes inline)
                physical_type = ASR::array_physical_typeType::FixedSizeArray;
            } else if( is_dimension_star ||
                ASRUtils::is_fixed_size_array(m_dims, n_dims) ||
                !ASRUtils::is_dimension_empty(m_dims, n_dims) ) {
                physical_type = ASR::array_physical_typeType::PointerArray;
            } else {
                physical_type = ASR::array_physical_typeType::DescriptorArray;
            }
        } else {
            if( ASRUtils::is_fixed_size_array(m_dims, n_dims) ) {
                if( is_argument || (type && ASRUtils::is_character(*type)) ) {
                    physical_type = ASR::array_physical_typeType::PointerArray;
                } else {
                    physical_type = ASR::array_physical_typeType::FixedSizeArray;
                }
            } else if( !ASRUtils::is_dimension_empty(m_dims, n_dims) ) {
                physical_type = ASR::array_physical_typeType::PointerArray;
            } else if (is_dimension_star && is_argument) {
                physical_type = ASR::array_physical_typeType::UnboundedPointerArray;
            }
        }
    }

    // Compile-time-know-size Array of strings must be `PointerArray` physical type
    if(type && is_character(*type) && (physical_type == ASR::FixedSizeArray)){physical_type = ASR::PointerArray;}

    // A `StringArraySinglePointer` array is one flat character buffer, so its
    // elements are plain C characters rather than string descriptors. Keeping
    // the two physical types consistent here, at the single place Array types
    // are built, means no consumer has to reconcile a contradictory pair.
    // Rebuild the element type instead of mutating it, as the String node may
    // be shared with other types.
    if(type && ASR::is_a<ASR::String_t>(*type) &&
            physical_type == ASR::StringArraySinglePointer) {
        ASR::String_t* str = ASR::down_cast<ASR::String_t>(type);
        if(str->m_physical_type != ASR::CChar) {
            type = ASRUtils::TYPE(ASR::make_String_t(al, str->base.base.loc,
                str->m_kind, str->m_len, str->m_len_kind, ASR::CChar));
        }
    }

    return ASRUtils::TYPE(ASR::make_Array_t(
        al, loc, type, m_dims, n_dims, physical_type, memory_space));
}

bool ttype_set_dimensions(ASR::ttype_t** x,
            ASR::dimension_t *m_dims, int64_t n_dims,
            Allocator& al, ASR::abiType abi,
            bool is_argument, bool is_dimension_star) {
    switch ((*x)->type) {
        case ASR::ttypeType::Array: {
            ASR::Array_t* array_t = ASR::down_cast<ASR::Array_t>(*x);
            array_t->n_dims = n_dims;
            array_t->m_dims = m_dims;
            return true;
        }
        case ASR::ttypeType::Pointer: {
            return ttype_set_dimensions(
                &(ASR::down_cast<ASR::Pointer_t>(*x)->m_type), m_dims, n_dims, al);
        }
        case ASR::ttypeType::Allocatable: {
            return ttype_set_dimensions(
                &(ASR::down_cast<ASR::Allocatable_t>(*x)->m_type), m_dims, n_dims, al);
        }
        case ASR::ttypeType::Integer:
        case ASR::ttypeType::UnsignedInteger:
        case ASR::ttypeType::Real:
        case ASR::ttypeType::Complex:
        case ASR::ttypeType::String:
        case ASR::ttypeType::Logical:
        case ASR::ttypeType::StructType:
        case ASR::ttypeType::EnumType:
        case ASR::ttypeType::UnionType:
        case ASR::ttypeType::TypeParameter: {
            *x = ASRUtils::make_Array_t_util(al,
                (*x)->base.loc, *x, m_dims, n_dims, abi, is_argument, ASR::array_physical_typeType::DescriptorArray, false, is_dimension_star);
            return true;
        }
        default:
            return false;
    }
    return false;
}

bool is_aggregate_type(ASR::ttype_t* asr_type) {
    return ASRUtils::is_array(asr_type) ||
            !(ASR::is_a<ASR::Integer_t>(*asr_type) ||
              ASR::is_a<ASR::UnsignedInteger_t>(*asr_type) ||
              ASR::is_a<ASR::Real_t>(*asr_type) ||
              ASR::is_a<ASR::Complex_t>(*asr_type) ||
              ASR::is_a<ASR::Logical_t>(*asr_type) ||
              ASR::is_a<ASR::String_t>(
                *ASRUtils::type_get_past_pointer(
                    ASRUtils::type_get_past_allocatable(asr_type))) ||
              ASR::is_a<ASR::TypeParameter_t>(*asr_type));
}

Vec<ASR::dimension_t> make_complex_dimensions_bounds(Allocator& al,
        const Location &loc, ASR::dimension_t* m_dims, int n_dims) {
    Vec<ASR::dimension_t> dim_vec;
    dim_vec.reserve(al, n_dims);
    ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
    for (int i = 0; i < n_dims; i++) {
        ASR::dimension_t dim;
        dim.loc = loc;
        dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 1, int_type));
        dim.m_length = m_dims[i].m_length;
        dim_vec.push_back(al, dim);
    }
    return dim_vec;
}

ASR::ttype_t* duplicate_type(Allocator& al, const ASR::ttype_t* t,
    Vec<ASR::dimension_t>* dims,
    ASR::array_physical_typeType physical_type,
    bool override_physical_type) {
    size_t dimsn = 0;
    ASR::dimension_t* dimsp = nullptr;
    if (dims != nullptr) {
        dimsp = dims->p;
        dimsn = dims->n;
    }
    ASR::ttype_t* t_ = nullptr;
    switch (t->type) {
        case ASR::ttypeType::Array: {
            ASR::Array_t* tnew = ASR::down_cast<ASR::Array_t>(t);
            ASR::ttype_t* duplicated_element_type = duplicate_type(al, tnew->m_type);
            if (dims == nullptr) {
                dimsp = duplicate_dimensions(al, tnew->m_dims, tnew->n_dims);
                dimsn = tnew->n_dims;
                // A duplicate with the same dimensions is the same type, so
                // it keeps the physical type, which the dimensions alone do
                // not determine: an explicit-shape dummy and a local of the
                // same shape differ in it, and UnboundedPointerArray needs
                // flags only semantics has.
                if (!override_physical_type) {
                    physical_type = tnew->m_physical_type;
                    override_physical_type = true;
                }
            }
            // An assumed-rank array is the one Array that carries no
            // dimensions, so duplicating it would otherwise hand
            // make_Array_t_util zero dimensions and collapse it to its element
            // type. Its physical type cannot be re-derived from dimensions
            // that do not exist either, which is the same reason
            // UnboundedPointerArray is preserved above. Only preserve it when
            // the duplicate really ends up rankless: a caller supplying
            // dimensions is selecting a rank, as `select rank` does.
            if (!override_physical_type && dimsn == 0 &&
                tnew->m_physical_type == ASR::array_physical_typeType::AssumedRankArray) {
                physical_type = ASR::array_physical_typeType::AssumedRankArray;
                override_physical_type = true;
            }
            return ASRUtils::make_Array_t_util(al, tnew->base.base.loc,
                duplicated_element_type, dimsp, dimsn, ASR::abiType::Source,
                false, physical_type, override_physical_type, false, true,
                tnew->m_memory_space);
        }
        case ASR::ttypeType::Integer: {
            ASR::Integer_t* tnew = ASR::down_cast<ASR::Integer_t>(t);
            t_ = ASRUtils::TYPE(ASR::make_Integer_t(al, t->base.loc, tnew->m_kind));
            break;
        }
        case ASR::ttypeType::UnsignedInteger: {
            ASR::UnsignedInteger_t* tnew = ASR::down_cast<ASR::UnsignedInteger_t>(t);
            t_ = ASRUtils::TYPE(ASR::make_UnsignedInteger_t(al, t->base.loc, tnew->m_kind));
            break;
        }
        case ASR::ttypeType::Real: {
            ASR::Real_t* tnew = ASR::down_cast<ASR::Real_t>(t);
            t_ = ASRUtils::TYPE(ASR::make_Real_t(al, t->base.loc, tnew->m_kind));
            break;
        }
        case ASR::ttypeType::Complex: {
            ASR::Complex_t* tnew = ASR::down_cast<ASR::Complex_t>(t);
            t_ = ASRUtils::TYPE(ASR::make_Complex_t(al, t->base.loc, tnew->m_kind));
            break;
        }
        case ASR::ttypeType::Logical: {
            ASR::Logical_t* tnew = ASR::down_cast<ASR::Logical_t>(t);
            t_ = ASRUtils::TYPE(ASR::make_Logical_t(al, t->base.loc, tnew->m_kind));
            break;
        }
        case ASR::ttypeType::String: {
            ASR::String_t* tnew = ASR::down_cast<ASR::String_t>(t);
            ASR::expr_t* len_expr;
            if(tnew->m_len){
                ASRUtils::ExprStmtDuplicator duplicator_instance(al);
                len_expr = duplicator_instance.duplicate_expr(tnew->m_len);
            } else {
                len_expr = nullptr;
            }
            t_ = ASRUtils::TYPE(ASR::make_String_t(al, t->base.loc,
                    tnew->m_kind, len_expr, 
                    tnew->m_len_kind,
                    tnew->m_physical_type));
            break;
        }
        case ASR::ttypeType::StructType: {
            ASR::StructType_t* tnew = ASR::down_cast<ASR::StructType_t>(t);

            t_ = ASRUtils::TYPE(ASR::make_StructType_t(al,
                                                       t->base.loc,
                                                       tnew->m_data_member_types,
                                                       tnew->n_data_member_types,
                                                       nullptr,
                                                       0,
                                                       tnew->m_is_cstruct,
                                                       tnew->m_is_unlimited_polymorphic));
            break;
        }
        case ASR::ttypeType::UnionType: {
            ASR::UnionType_t* tnew = ASR::down_cast<ASR::UnionType_t>(t);
            t_ = ASRUtils::TYPE(ASR::make_UnionType_t(al, t->base.loc,
                tnew->m_data_member_types, tnew->n_data_member_types));
            break;
        }
        case ASR::ttypeType::Pointer: {
            ASR::Pointer_t* ptr = ASR::down_cast<ASR::Pointer_t>(t);
            ASR::ttype_t* dup_type = duplicate_type(al, ptr->m_type, dims,
                physical_type, override_physical_type);
            if( override_physical_type &&
                (physical_type == ASR::array_physical_typeType::FixedSizeArray ||
                (physical_type == ASR::array_physical_typeType::PointerArray && 
                dims != nullptr) || 
                (physical_type == ASR::array_physical_typeType::StringArraySinglePointer &&
                dims != nullptr) ) ) {
                return dup_type;
            }
            return ASRUtils::TYPE(ASR::make_Pointer_t(al, ptr->base.base.loc,
                ASRUtils::type_get_past_allocatable(dup_type)));
        }
        case ASR::ttypeType::Allocatable: {
            ASR::Allocatable_t* alloc_ = ASR::down_cast<ASR::Allocatable_t>(t);
            ASR::ttype_t* dup_type = duplicate_type(al, alloc_->m_type, dims,
                physical_type, override_physical_type);
            if( override_physical_type &&
                physical_type == ASR::array_physical_typeType::FixedSizeArray ) {
                return dup_type;
            }
            return ASRUtils::TYPE(ASR::make_Allocatable_t(al, alloc_->base.base.loc,
                ASRUtils::type_get_past_allocatable(dup_type)));
        }
        case ASR::ttypeType::CPtr: {
            ASR::CPtr_t* ptr = ASR::down_cast<ASR::CPtr_t>(t);
            return ASRUtils::TYPE(ASR::make_CPtr_t(al, ptr->base.base.loc, ptr->m_kind));
        }
        case ASR::ttypeType::List: {
            ASR::List_t* l = ASR::down_cast<ASR::List_t>(t);
            ASR::ttype_t* dup_type = duplicate_type(al, l->m_type);
            return ASRUtils::TYPE(ASR::make_List_t(al, l->base.base.loc,
                        dup_type));
        }
        case ASR::ttypeType::Dict: {
            ASR::Dict_t* d = ASR::down_cast<ASR::Dict_t>(t);
            ASR::ttype_t* dup_key_type = duplicate_type(al, d->m_key_type);
            ASR::ttype_t* dup_value_type = duplicate_type(al, d->m_value_type);
            return ASRUtils::TYPE(ASR::make_Dict_t(al, d->base.base.loc,
                        dup_key_type, dup_value_type));
        }
        case ASR::ttypeType::TypeParameter: {
            ASR::TypeParameter_t* tp = ASR::down_cast<ASR::TypeParameter_t>(t);
            t_ = ASRUtils::TYPE(ASR::make_TypeParameter_t(al, t->base.loc,
                tp->m_param, tp->m_deferred_attr, tp->m_is_class));
            break;
        }
        case ASR::ttypeType::FunctionType: {
            ASR::FunctionType_t* ft = ASR::down_cast<ASR::FunctionType_t>(t);
            //ASR::ttype_t* dup_type = duplicate_type(al, c->m_type, dims);
            Vec<ASR::ttype_t*> arg_types;
            arg_types.reserve(al, ft->n_arg_types);
            for( size_t i = 0; i < ft->n_arg_types; i++ ) {
                auto const t = ft->m_arg_types[i];
                ASR::ttype_t *duplicate_t = 
                    ASRUtils::duplicate_type(al, t, nullptr, 
                        is_array_t(t) ? extract_physical_type(t) : physical_type
                        , true);
                arg_types.push_back(al, duplicate_t);
            }
            return ASRUtils::TYPE(ASR::make_FunctionType_t(al, ft->base.base.loc,
                arg_types.p, arg_types.size(), ft->m_return_var_type, ft->m_abi,
                ft->m_deftype, ft->m_bindc_name, ft->m_elemental, ft->m_pure, ft->m_module, ft->m_inline,
                ft->m_static, ft->m_restrictions, ft->n_restrictions,
                ft->m_is_restriction, ft->m_exec_space));
        }
        case ASR::ttypeType::SymbolicExpression: {
            return ASRUtils::TYPE(ASR::make_SymbolicExpression_t(al, t->base.loc));
        }
        case ASR::ttypeType::Tuple: {
            ASR::Tuple_t* tup = ASR::down_cast<ASR::Tuple_t>(t);
            Vec<ASR::ttype_t*> types;
            types.reserve(al, tup->n_type);
            for( size_t i = 0; i < tup->n_type; i++ ) {
                ASR::ttype_t *t = ASRUtils::duplicate_type(al, tup->m_type[i],
                    nullptr, physical_type, override_physical_type);
                types.push_back(al, t);
            }
            return ASRUtils::TYPE(ASR::make_Tuple_t(al, tup->base.base.loc,
                types.p, types.size()));
        }
        default : throw LCompilersException("Not implemented " + std::to_string(t->type));
    }
    LCOMPILERS_ASSERT(t_ != nullptr);
    return ASRUtils::make_Array_t_util(
        al, t_->base.loc, t_, dimsp, dimsn,
        ASR::abiType::Source, false, physical_type,
        override_physical_type);
}

void set_absent_optional_arguments_to_null(
    Vec<ASR::call_arg_t>& args, ASR::Function_t* func, Allocator& al,
    ASR::expr_t* dt, bool nopass) {
    int offset = (dt != nullptr) && (!nopass);
    for( size_t i = args.size(); i + offset < func->n_args; i++ ) {
        if( ASR::is_a<ASR::Variable_t>(
                *ASR::down_cast<ASR::Var_t>(func->m_args[i + offset])->m_v) ) {
            LCOMPILERS_ASSERT(ASRUtils::EXPR2VAR(func->m_args[i + offset])->m_presence ==
                                ASR::presenceType::Optional);
            ASR::call_arg_t empty_arg;
            Location loc;
            loc.first = 1, loc.last = 1;
            empty_arg.loc = loc;
            empty_arg.m_value = nullptr;
            args.push_back(al, empty_arg);
        }
    }
    LCOMPILERS_ASSERT(args.size() + offset == (func->n_args));
}

bool is_allocatable_descriptor_string(ASR::ttype_t* t) {
    if (!ASR::is_a<ASR::Allocatable_t>(*t))
        return false;

    ASR::ttype_t* t1 = ASR::down_cast<ASR::Allocatable_t>(t)->m_type;

    if (!ASR::is_a<ASR::String_t>(*t1))
        return false;
    ASR::String_t* string = ASR::down_cast<ASR::String_t>(t1);
    return string->m_physical_type == ASR::string_physical_typeType::DescriptorString &&
            string->m_len_kind == ASR::string_length_kindType::DeferredLength;
}

int64_t inline_character_storage_size(ASR::ttype_t* member_type) {
    ASR::String_t* string_type = get_string_type(member_type);
    int64_t length = 1;
    if (string_type->m_len) {
        extract_value(string_type->m_len, length);
    }
    if (length < 1) {
        length = 1;
    }
    int64_t count = is_array(member_type)
        ? get_fixed_size_of_array(member_type) : 1;
    return length * string_type->m_kind * count;
}

bool is_inline_character_struct_member(ASR::expr_t* expr) {
    if (!ASR::is_a<ASR::StructInstanceMember_t>(*expr)) {
        return false;
    }
    ASR::StructInstanceMember_t* sim =
        ASR::down_cast<ASR::StructInstanceMember_t>(expr);
    ASR::symbol_t* member_sym = symbol_get_past_external(sim->m_m);
    if (!ASR::is_a<ASR::Variable_t>(*member_sym)) {
        return false;
    }
    ASR::Variable_t* member_var = ASR::down_cast<ASR::Variable_t>(member_sym);
    if (!member_var->m_parent_symtab
            || !member_var->m_parent_symtab->asr_owner) {
        return false;
    }
    ASR::symbol_t* struct_sym = symbol_get_past_external(
        ASR::down_cast<ASR::symbol_t>(member_var->m_parent_symtab->asr_owner));
    if (!ASR::is_a<ASR::Struct_t>(*struct_sym)) {
        return false;
    }
    return is_inline_character_struct_member(
        ASR::down_cast<ASR::Struct_t>(struct_sym), sim->m_type);
}

ASR::expr_t* create_string_physical_cast(Allocator& al, ASR::expr_t* string,
        ASR::string_physical_typeType to, bool make_allocatable){
    LCOMPILERS_ASSERT(is_character(*ASRUtils::expr_type(string)))
    ASR::String_t* str_type = ASR::down_cast<ASR::String_t>(ASRUtils::extract_type(expr_type(string)));
    LCOMPILERS_ASSERT(to != str_type->m_physical_type)
    ASR::ttype_t* cast_expr_type = ASRUtils::TYPE(
        ASR::make_String_t(al, string->base.loc,
            1, nullptr,
            ASR::ImplicitLength, to));
    if (make_allocatable) {
        cast_expr_type = ASRUtils::TYPE(
            ASR::make_Allocatable_t(al, string->base.loc, cast_expr_type));
    }
    return ASRUtils::EXPR(
        ASR::make_StringPhysicalCast_t(al, string->base.loc , string,
        str_type->m_physical_type, to, cast_expr_type, nullptr));
}

ASR::ttype_t* duplicate_type_with_empty_dims(Allocator& al, ASR::ttype_t* t,
    ASR::array_physical_typeType physical_type,
    bool override_physical_type) {
    size_t n_dims = ASRUtils::extract_n_dims_from_ttype(t);
    Vec<ASR::dimension_t> empty_dims;
    empty_dims.reserve(al, n_dims);
    for( size_t i = 0; i < n_dims; i++ ) {
        ASR::dimension_t empty_dim;
        empty_dim.loc = t->base.loc;
        empty_dim.m_start = nullptr;
        empty_dim.m_length = nullptr;
        empty_dims.push_back(al, empty_dim);
    }
    return duplicate_type(al, t, &empty_dims, physical_type, override_physical_type);
}

ASR::ttype_t* duplicate_type_without_dims(Allocator& al, const ASR::ttype_t* t, const Location& loc) {
    switch (t->type) {
        case ASR::ttypeType::Array: {
            return duplicate_type_without_dims(al, ASR::down_cast<ASR::Array_t>(t)->m_type, loc);
        }
        case ASR::ttypeType::Integer: {
            ASR::Integer_t* tnew = ASR::down_cast<ASR::Integer_t>(t);
            return ASRUtils::TYPE(ASR::make_Integer_t(al, loc, tnew->m_kind));
        }
        case ASR::ttypeType::UnsignedInteger: {
            ASR::UnsignedInteger_t* tnew = ASR::down_cast<ASR::UnsignedInteger_t>(t);
            return ASRUtils::TYPE(ASR::make_UnsignedInteger_t(al, loc, tnew->m_kind));
        }
        case ASR::ttypeType::Real: {
            ASR::Real_t* tnew = ASR::down_cast<ASR::Real_t>(t);
            return ASRUtils::TYPE(ASR::make_Real_t(al, loc, tnew->m_kind));
        }
        case ASR::ttypeType::Complex: {
            ASR::Complex_t* tnew = ASR::down_cast<ASR::Complex_t>(t);
            return ASRUtils::TYPE(ASR::make_Complex_t(al, loc, tnew->m_kind));
        }
        case ASR::ttypeType::Logical: {
            ASR::Logical_t* tnew = ASR::down_cast<ASR::Logical_t>(t);
            return ASRUtils::TYPE(ASR::make_Logical_t(al, loc, tnew->m_kind));
        }
        case ASR::ttypeType::String: {
            ASR::String_t* tnew = ASR::down_cast<ASR::String_t>(t);
            return ASRUtils::TYPE(ASR::make_String_t(al, loc,
                        tnew->m_kind, tnew->m_len, 
                        tnew->m_len_kind,
                        ASR::string_physical_typeType::DescriptorString));
        }
        case ASR::ttypeType::StructType: {
            ASR::StructType_t* tstruct = ASR::down_cast<ASR::StructType_t>(t);
            return ASRUtils::TYPE(ASR::make_StructType_t(al, t->base.loc,
                tstruct->m_data_member_types,
                tstruct->n_data_member_types,
                nullptr,
                0,
                tstruct->m_is_cstruct,
                tstruct->m_is_unlimited_polymorphic));
        }
        case ASR::ttypeType::Pointer: {
            ASR::Pointer_t* ptr = ASR::down_cast<ASR::Pointer_t>(t);
            ASR::ttype_t* dup_type = duplicate_type_without_dims(al, ptr->m_type, loc);
            return ASRUtils::TYPE(ASR::make_Pointer_t(al, ptr->base.base.loc,
                ASRUtils::type_get_past_allocatable(dup_type)));
        }
        case ASR::ttypeType::Allocatable: {
            ASR::Allocatable_t* alloc_ = ASR::down_cast<ASR::Allocatable_t>(t);
            ASR::ttype_t* dup_type = duplicate_type_without_dims(al, alloc_->m_type, loc);
            return ASRUtils::TYPE(ASR::make_Allocatable_t(al, alloc_->base.base.loc,
                ASRUtils::type_get_past_allocatable(dup_type)));
        }
        case ASR::ttypeType::TypeParameter: {
            ASR::TypeParameter_t* tp = ASR::down_cast<ASR::TypeParameter_t>(t);
            return ASRUtils::TYPE(ASR::make_TypeParameter_t(al, loc,
                tp->m_param, tp->m_deferred_attr, tp->m_is_class));
        }
        case ASR::ttypeType::CPtr: {
            ASR::CPtr_t* ptr = ASR::down_cast<ASR::CPtr_t>(t);
            return ASRUtils::TYPE(ASR::make_CPtr_t(al, loc, ptr->m_kind));
        }
        default : throw LCompilersException("Not implemented " + std::to_string(t->type));
    }
}

bool is_same_type_pointer(ASR::ttype_t* source, ASR::ttype_t* dest) {
    bool is_source_pointer = is_pointer(source), is_dest_pointer = is_pointer(dest);
    if( (!is_source_pointer && !is_dest_pointer) ||
        (is_source_pointer && is_dest_pointer) ) {
        return false;
    }
    if( is_source_pointer && !is_dest_pointer ) {
        ASR::ttype_t* temp = source;
        source = dest;
        dest = temp;
    }
    dest = ASRUtils::type_get_past_array(ASR::down_cast<ASR::Pointer_t>(dest)->m_type);
    if( (ASRUtils::is_class_type(source) || ASR::is_a<ASR::StructType_t>(*source)) &&
        (ASRUtils::is_class_type(dest) || ASR::is_a<ASR::StructType_t>(*dest)) ) {
        return true;
    }
    bool res = source->type == dest->type;
    return res;
}

int extract_kind_str(char* m_n, char *&kind_str) {
    char *p = m_n;
    while (*p != '\0') {
        if (*p == '_') {
            p++;
            int ikind = std::atoi(p);
            if (ikind == 0) {
                // Not an integer, return a string
                kind_str = p;
                return 0;
            } else {
                return ikind;
            }
        }
        if (*p == 'd' || *p == 'D') {
            // Double precision
            return 8;
        }
        if (*p == 'q' || *p == 'Q') {
            // Quad precision (real(16))
            return 16;
        }
        p++;
    }
    return 4;
}

void collect_allocatable_array_members(ASR::Struct_t* st,
        std::vector<std::pair<std::string, ASR::Variable_t*>>& members,
        std::set<ASR::Struct_t*>& visited) {
    if( !st || !visited.insert(st).second ) {
        return;
    }
    if( st->m_parent ) {
        ASR::symbol_t* parent = symbol_get_past_external(st->m_parent);
        if( ASR::is_a<ASR::Struct_t>(*parent) ) {
            collect_allocatable_array_members(
                ASR::down_cast<ASR::Struct_t>(parent), members, visited);
        }
    }
    for( size_t i = 0; i < st->n_members; i++ ) {
        ASR::symbol_t* mem = st->m_symtab->get_symbol(st->m_members[i]);
        if( !mem || !ASR::is_a<ASR::Variable_t>(*mem) ) {
            continue;
        }
        ASR::Variable_t* mv = ASR::down_cast<ASR::Variable_t>(mem);
        if( !ASR::is_a<ASR::Allocatable_t>(*mv->m_type) ) {
            continue;
        }
        if( !ASR::is_a<ASR::Array_t>(
                *type_get_past_allocatable(mv->m_type)) ) {
            continue;
        }
        members.push_back(std::make_pair(std::string(st->m_members[i]), mv));
    }
}

bool is_parent(ASR::Struct_t* a, ASR::Struct_t* b) {
    ASR::symbol_t* current_parent = b->m_parent;
    while( current_parent ) {
        current_parent = ASRUtils::symbol_get_past_external(current_parent);
        if( current_parent == (ASR::symbol_t*) a ) {
            return true;
        }
        LCOMPILERS_ASSERT(ASR::is_a<ASR::Struct_t>(*current_parent));
        current_parent = ASR::down_cast<ASR::Struct_t>(current_parent)->m_parent;
    }
    return false;
}

ASR::expr_t* get_identity_operand(ASR::IntegerBinOp_t* binop) {
    int64_t val = 0;
    if (binop->m_op == ASR::binopType::Add || binop->m_op == ASR::binopType::Sub) {
        // x+0, 0+x, x-0 are identity
        if (ASR::is_a<ASR::IntegerConstant_t>(*binop->m_right)) {
            if (extract_value(binop->m_right, val) && val == 0) {
                return binop->m_left;
            }
        }
        if (binop->m_op == ASR::binopType::Add &&
            ASR::is_a<ASR::IntegerConstant_t>(*binop->m_left)) {
            if (extract_value(binop->m_left, val) && val == 0) {
                return binop->m_right;
            }
        }
    }
    return nullptr;
}

bool expr_equal(ASR::expr_t* x, ASR::expr_t* y) {
    if( x->type != y->type ) {
        // Handle identity operations: compare x with y+0, y-0, etc.
        if (ASR::is_a<ASR::IntegerBinOp_t>(*y)) {
            ASR::expr_t* y_simple = get_identity_operand(
                ASR::down_cast<ASR::IntegerBinOp_t>(y));
            if (y_simple) return expr_equal(x, y_simple);
        }
        if (ASR::is_a<ASR::IntegerBinOp_t>(*x)) {
            ASR::expr_t* x_simple = get_identity_operand(
                ASR::down_cast<ASR::IntegerBinOp_t>(x));
            if (x_simple) return expr_equal(x_simple, y);
        }
        return false;
    }

    switch( x->type ) {
        case ASR::exprType::IntegerBinOp: {
            ASR::IntegerBinOp_t* intbinop_x = ASR::down_cast<ASR::IntegerBinOp_t>(x);
            ASR::IntegerBinOp_t* intbinop_y = ASR::down_cast<ASR::IntegerBinOp_t>(y);
            if( intbinop_x->m_op != intbinop_y->m_op ) {
                return false;
            }
            bool left_left = expr_equal(intbinop_x->m_left, intbinop_y->m_left);
            bool left_right = expr_equal(intbinop_x->m_left, intbinop_y->m_right);
            bool right_left = expr_equal(intbinop_x->m_right, intbinop_y->m_left);
            bool right_right = expr_equal(intbinop_x->m_right, intbinop_y->m_right);
            switch( intbinop_x->m_op ) {
                case ASR::binopType::Add:
                case ASR::binopType::Mul:
                case ASR::binopType::BitAnd:
                case ASR::binopType::BitOr:
                case ASR::binopType::BitXor: {
                    return (left_left && right_right) || (left_right && right_left);
                }
                case ASR::binopType::Sub:
                case ASR::binopType::Div:
                case ASR::binopType::Pow:
                case ASR::binopType::BitLShift:
                case ASR::binopType::BitRShift:
                case ASR::binopType::LBitRShift: {
                    return (left_left && right_right);
                }
            }
            break;
        }
        case ASR::exprType::Var: {
            ASR::Var_t* var_x = ASR::down_cast<ASR::Var_t>(x);
            ASR::Var_t* var_y = ASR::down_cast<ASR::Var_t>(y);
            return var_x->m_v == var_y->m_v;
        }
        case ASR::exprType::IntegerConstant: {
            ASR::IntegerConstant_t* intconst_x = ASR::down_cast<ASR::IntegerConstant_t>(x);
            ASR::IntegerConstant_t* intconst_y = ASR::down_cast<ASR::IntegerConstant_t>(y);
            return intconst_x->m_n == intconst_y->m_n;
        }
        case ASR::exprType::RealConstant: {
            ASR::RealConstant_t* realconst_x = ASR::down_cast<ASR::RealConstant_t>(x);
            ASR::RealConstant_t* realconst_y = ASR::down_cast<ASR::RealConstant_t>(y);
            return realconst_x->m_r == realconst_y->m_r;
        }
        default: {
            // Let it pass for now.
            return true;
        }
    }

    // Let it pass for now.
    return true;
}

bool dimension_expr_equal(
    ASR::expr_t* dim_a,
    ASR::expr_t* dim_b
) {
    // If either dimension is null, consider them equal by default.
    if (!(dim_a && dim_b)) {
        return true;
    }
    int dim_a_int {-1};
    int dim_b_int {-1};

    if (ASRUtils::extract_value(ASRUtils::expr_value(dim_a), dim_a_int) &&
        ASRUtils::extract_value(ASRUtils::expr_value(dim_b), dim_b_int)) {
        return dim_a_int == dim_b_int;
    }

    if (!ASRUtils::expr_equal(dim_a, dim_b)) {
        return false;
    }

    return true;
}

bool cptr_type_declarations_match(ASR::ttype_t* left_type,
        ASR::ttype_t* right_type, ASR::expr_t* left_expr,
        ASR::expr_t* right_expr) {
    ASR::cptr_kindType left_kind = ASRUtils::get_cptr_kind_from_type(left_type);
    ASR::cptr_kindType right_kind = ASRUtils::get_cptr_kind_from_type(right_type);
    if (left_kind != ASR::cptr_kindType::CPtrUnspecified
            && right_kind != ASR::cptr_kindType::CPtrUnspecified) {
        return left_kind == right_kind;
    }
    ASR::symbol_t* left = left_expr != nullptr
        ? ASRUtils::get_cptr_type_declaration_from_expr(left_expr) : nullptr;
    ASR::symbol_t* right = right_expr != nullptr
        ? ASRUtils::get_cptr_type_declaration_from_expr(right_expr) : nullptr;
    if (left == nullptr || right == nullptr) {
        return true;
    }
    return ASRUtils::symbol_get_past_external(left)
        == ASRUtils::symbol_get_past_external(right);
}

bool types_equal(ASR::ttype_t *a, ASR::ttype_t *b, ASR::expr_t* a_expr, ASR::expr_t* b_expr,
    bool check_for_dimensions) {
    // TODO: If anyone of the input or argument is derived type then
    // add support for checking member wise types and do not compare
    // directly. From stdlib_string len(pattern) error
    if( a == nullptr && b == nullptr ) {
        return true;
    }
    a = ASRUtils::type_get_past_allocatable(ASRUtils::type_get_past_pointer(a));
    b = ASRUtils::type_get_past_allocatable(ASRUtils::type_get_past_pointer(b));
    if( !check_for_dimensions ) {
        a = ASRUtils::type_get_past_array(a);
        b = ASRUtils::type_get_past_array(b);
    }
    // If either argument is a polymorphic type, return true.
    if (ASRUtils::is_class_type(a)) {
        if (ASR::down_cast<ASR::StructType_t>(a)->m_is_unlimited_polymorphic) {
            return true;
        }
    } else if (ASRUtils::is_class_type(b)) {
        if (ASR::down_cast<ASR::StructType_t>(b)->m_is_unlimited_polymorphic) {
            return true;
        }
    }
    // Assumed-rank arrays (dimension(..)) accept any rank, including scalar
    if (check_for_dimensions) {
        if (ASR::is_a<ASR::Array_t>(*b) && is_assumed_rank_array(b)) {
            return types_equal(a, ASR::down_cast<ASR::Array_t>(b)->m_type,
                               a_expr, b_expr, false);
        }
        if (ASR::is_a<ASR::Array_t>(*a) && is_assumed_rank_array(a)) {
            return types_equal(ASR::down_cast<ASR::Array_t>(a)->m_type, b,
                               a_expr, b_expr, false);
        }
    }
    if (a->type == b->type) {
        // TODO: check dims
        // TODO: check all types
        switch (a->type) {
            case (ASR::ttypeType::Array): {
                ASR::Array_t* a2 = ASR::down_cast<ASR::Array_t>(a);
                ASR::Array_t* b2 = ASR::down_cast<ASR::Array_t>(b);
                if( !types_equal(a2->m_type, b2->m_type, a_expr, b_expr) ) {
                    return false;
                }

                return ASRUtils::dimensions_compatible(
                            a2->m_dims, a2->n_dims,
                            b2->m_dims, b2->n_dims);
            }
            case (ASR::ttypeType::TypeParameter) : {
                ASR::TypeParameter_t* left_tp = ASR::down_cast<ASR::TypeParameter_t>(a);
                ASR::TypeParameter_t* right_tp = ASR::down_cast<ASR::TypeParameter_t>(b);
                std::string left_param = left_tp->m_param;
                std::string right_param = right_tp->m_param;
                return left_param == right_param;
            }
            case (ASR::ttypeType::Integer) : {
                ASR::Integer_t *a2 = ASR::down_cast<ASR::Integer_t>(a);
                ASR::Integer_t *b2 = ASR::down_cast<ASR::Integer_t>(b);
                return (a2->m_kind == b2->m_kind);
            }
            case (ASR::ttypeType::UnsignedInteger) : {
                ASR::UnsignedInteger_t *a2 = ASR::down_cast<ASR::UnsignedInteger_t>(a);
                ASR::UnsignedInteger_t *b2 = ASR::down_cast<ASR::UnsignedInteger_t>(b);
                return (a2->m_kind == b2->m_kind);
            }
            case ASR::ttypeType::CPtr: {
                return cptr_type_declarations_match(a, b, a_expr, b_expr);
            }
            case ASR::ttypeType::SymbolicExpression: {
                return true;
            }
            case (ASR::ttypeType::Real) : {
                ASR::Real_t *a2 = ASR::down_cast<ASR::Real_t>(a);
                ASR::Real_t *b2 = ASR::down_cast<ASR::Real_t>(b);
                return (a2->m_kind == b2->m_kind);
            }
            case (ASR::ttypeType::Complex) : {
                ASR::Complex_t *a2 = ASR::down_cast<ASR::Complex_t>(a);
                ASR::Complex_t *b2 = ASR::down_cast<ASR::Complex_t>(b);
                return (a2->m_kind == b2->m_kind);
            }
            case (ASR::ttypeType::Logical) : {
                ASR::Logical_t *a2 = ASR::down_cast<ASR::Logical_t>(a);
                ASR::Logical_t *b2 = ASR::down_cast<ASR::Logical_t>(b);
                return (a2->m_kind == b2->m_kind);
            }
            case (ASR::ttypeType::String) : {
                ASR::String_t *a2 = ASR::down_cast<ASR::String_t>(a);
                ASR::String_t *b2 = ASR::down_cast<ASR::String_t>(b);
                return (a2->m_kind == b2->m_kind);
            }
            case (ASR::ttypeType::List) : {
                ASR::List_t *a2 = ASR::down_cast<ASR::List_t>(a);
                ASR::List_t *b2 = ASR::down_cast<ASR::List_t>(b);
                return types_equal(a2->m_type, b2->m_type, a_expr, b_expr);
            }
            case (ASR::ttypeType::StructType) : {
                ASR::symbol_t* a_struct_sym = (a_expr != nullptr)
                    ? ASRUtils::symbol_get_past_external(
                          ASRUtils::get_struct_sym_from_struct_expr(a_expr))
                    : nullptr;
                ASR::symbol_t* b_struct_sym = (b_expr != nullptr)
                    ? ASRUtils::symbol_get_past_external(
                          ASRUtils::get_struct_sym_from_struct_expr(b_expr))
                    : nullptr;
                if (a_struct_sym != nullptr && b_struct_sym != nullptr
                    && ASR::is_a<ASR::Struct_t>(*a_struct_sym)
                    && ASR::is_a<ASR::Struct_t>(*b_struct_sym)) {
                    ASR::Struct_t* x_struct = ASR::down_cast<ASR::Struct_t>(a_struct_sym);
                    ASR::Struct_t* y_struct = ASR::down_cast<ASR::Struct_t>(b_struct_sym);
                    return is_derived_type_similar(x_struct, y_struct);
                }
                // Structural comparison when struct symbols unavailable
                ASR::StructType_t* a2 = ASR::down_cast<ASR::StructType_t>(a);
                ASR::StructType_t* b2 = ASR::down_cast<ASR::StructType_t>(b);
                if (a2->m_is_cstruct != b2->m_is_cstruct) return false;
                if (a2->m_is_unlimited_polymorphic != b2->m_is_unlimited_polymorphic) return false;
                if (a2->n_data_member_types != b2->n_data_member_types) return false;
                for (size_t i = 0; i < a2->n_data_member_types; i++) {
                    if (!types_equal(a2->m_data_member_types[i], b2->m_data_member_types[i],
                                     nullptr, nullptr, check_for_dimensions)) {
                        return false;
                    }
                }
                if (a2->n_member_function_types != b2->n_member_function_types) return false;
                for (size_t i = 0; i < a2->n_member_function_types; i++) {
                    if (!types_equal(a2->m_member_function_types[i], b2->m_member_function_types[i],
                                     nullptr, nullptr, check_for_dimensions)) {
                        return false;
                    }
                }
                return true;
            }
            case (ASR::ttypeType::UnionType) : {
                if (a_expr == nullptr || b_expr == nullptr) {
                    ASR::UnionType_t* a2 = ASR::down_cast<ASR::UnionType_t>(a);
                    ASR::UnionType_t* b2 = ASR::down_cast<ASR::UnionType_t>(b);
                    if (a2->n_data_member_types != b2->n_data_member_types) return false;
                    for (size_t i = 0; i < a2->n_data_member_types; i++) {
                        if (!types_equal(a2->m_data_member_types[i], b2->m_data_member_types[i],
                                         nullptr, nullptr, check_for_dimensions)) {
                            return false;
                        }
                    }
                    return true;
                }
                ASR::Union_t* x_union = ASR::down_cast<ASR::Union_t>(ASRUtils::symbol_get_past_external(
                    ASRUtils::get_union_sym_from_union_expr(a_expr)));
                ASR::Union_t* y_union = ASR::down_cast<ASR::Union_t>(ASRUtils::symbol_get_past_external(
                    ASRUtils::get_union_sym_from_union_expr(b_expr)));
                
                if (x_union == y_union) return true;

                return false;
            }
            case ASR::ttypeType::FunctionType: {
                ASR::FunctionType_t* a2 = ASR::down_cast<ASR::FunctionType_t>(a);
                ASR::FunctionType_t* b2 = ASR::down_cast<ASR::FunctionType_t>(b);

                if (a_expr == nullptr || b_expr == nullptr) {
                    if( a2->n_arg_types != b2->n_arg_types ||
                        (a2->m_return_var_type != nullptr && b2->m_return_var_type == nullptr) ||
                        (a2->m_return_var_type == nullptr && b2->m_return_var_type != nullptr) ) {
                        return false;
                    }
                    for (size_t i = 0; i < a2->n_arg_types; i++) {
                        if (!types_equal(a2->m_arg_types[i], b2->m_arg_types[i], nullptr, nullptr, true)) {
                            return false;
                        }
                    }
                    if (!types_equal(a2->m_return_var_type, b2->m_return_var_type, nullptr, nullptr, true)) {
                        return false;
                    }
                    return true;
                }

                ASR::Function_t* left_func = const_cast<ASR::Function_t*>(ASRUtils::get_function_from_expr(a_expr));
                ASR::Function_t* right_func = const_cast<ASR::Function_t*>(ASRUtils::get_function_from_expr(b_expr));

                // A Function's argument count should always match the
                // FunctionType_t signature we're comparing against. Mismatches
                // here indicate an inconsistency somewhere upstream; fall back
                // to a type-only comparison but flag it in debug builds.
                LCOMPILERS_ASSERT(left_func == nullptr || left_func->n_args == a2->n_arg_types);
                LCOMPILERS_ASSERT(right_func == nullptr || right_func->n_args == b2->n_arg_types);
                if (left_func == nullptr || right_func == nullptr
                    || left_func->n_args != a2->n_arg_types
                    || right_func->n_args != b2->n_arg_types) {
                    return types_equal(a, b, nullptr, nullptr, true);
                }

                if( a2->n_arg_types != b2->n_arg_types ||
                    (a2->m_return_var_type != nullptr && b2->m_return_var_type == nullptr) ||
                    (a2->m_return_var_type == nullptr && b2->m_return_var_type != nullptr) ) {
                    return false;
                }
                for( size_t i = 0; i < a2->n_arg_types; i++ ) {
                    if( !types_equal(a2->m_arg_types[i], b2->m_arg_types[i], left_func->m_args[i],
                         right_func->m_args[i], true) ) {
                        return false;
                    }
                }
                if( !types_equal(a2->m_return_var_type, b2->m_return_var_type, left_func->m_return_var,
                     right_func->m_return_var, true) ) {
                    return false;
                }
                return true;
            }
            default : return false;
        }
    }
    return false;
}

bool types_equal_with_substitution(ASR::ttype_t *a, ASR::ttype_t *b,
    std::map<std::string, std::pair<ASR::ttype_t*, ASR::symbol_t*>> subs, ASR::expr_t* a_expr, ASR::expr_t* b_expr,
    bool check_for_dimensions) {
    ASR::Struct_t* a_struct = nullptr;
    // TODO: If anyone of the input or argument is derived type then
    // add support for checking member wise types and do not compare
    // directly. From stdlib_string len(pattern) error
    if( a == nullptr && b == nullptr ) {
        return true;
    }
    a = ASRUtils::type_get_past_allocatable(ASRUtils::type_get_past_pointer(a));
    b = ASRUtils::type_get_past_allocatable(ASRUtils::type_get_past_pointer(b));
    if( !check_for_dimensions ) {
        a = ASRUtils::type_get_past_array(a);
        b = ASRUtils::type_get_past_array(b);
    }
    if (ASR::is_a<ASR::TypeParameter_t>(*a)) {
        ASR::TypeParameter_t* a_tp = ASR::down_cast<ASR::TypeParameter_t>(a);
        a = subs[a_tp->m_param].first;
        if (ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(a))) {
            a_struct = ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(subs[a_tp->m_param].second));
        }
    }
    if (a->type == b->type) {
        // TODO: check dims
        // TODO: check all types
        switch (a->type) {
            case (ASR::ttypeType::Array): {
                ASR::Array_t* a2 = ASR::down_cast<ASR::Array_t>(a);
                ASR::Array_t* b2 = ASR::down_cast<ASR::Array_t>(b);
                if( !types_equal_with_substitution(a2->m_type, b2->m_type, subs, a_expr, b_expr) ) {
                    return false;
                }

                return ASRUtils::dimensions_compatible(
                            a2->m_dims, a2->n_dims,
                            b2->m_dims, b2->n_dims);
            }
            case (ASR::ttypeType::TypeParameter) : {
                ASR::TypeParameter_t* left_tp = ASR::down_cast<ASR::TypeParameter_t>(a);
                ASR::TypeParameter_t* right_tp = ASR::down_cast<ASR::TypeParameter_t>(b);
                std::string left_param = left_tp->m_param;
                std::string right_param = right_tp->m_param;
                return left_param == right_param;
            }
            case (ASR::ttypeType::Integer) : {
                ASR::Integer_t *a2 = ASR::down_cast<ASR::Integer_t>(a);
                ASR::Integer_t *b2 = ASR::down_cast<ASR::Integer_t>(b);
                return (a2->m_kind == b2->m_kind);
            }
            case (ASR::ttypeType::UnsignedInteger) : {
                ASR::UnsignedInteger_t *a2 = ASR::down_cast<ASR::UnsignedInteger_t>(a);
                ASR::UnsignedInteger_t *b2 = ASR::down_cast<ASR::UnsignedInteger_t>(b);
                return (a2->m_kind == b2->m_kind);
            }
            case ASR::ttypeType::CPtr: {
                return cptr_type_declarations_match(a, b, a_expr, b_expr);
            }
            case ASR::ttypeType::SymbolicExpression: {
                return true;
            }
            case (ASR::ttypeType::Real) : {
                ASR::Real_t *a2 = ASR::down_cast<ASR::Real_t>(a);
                ASR::Real_t *b2 = ASR::down_cast<ASR::Real_t>(b);
                return (a2->m_kind == b2->m_kind);
            }
            case (ASR::ttypeType::Complex) : {
                ASR::Complex_t *a2 = ASR::down_cast<ASR::Complex_t>(a);
                ASR::Complex_t *b2 = ASR::down_cast<ASR::Complex_t>(b);
                return (a2->m_kind == b2->m_kind);
            }
            case (ASR::ttypeType::Logical) : {
                ASR::Logical_t *a2 = ASR::down_cast<ASR::Logical_t>(a);
                ASR::Logical_t *b2 = ASR::down_cast<ASR::Logical_t>(b);
                return (a2->m_kind == b2->m_kind);
            }
            case (ASR::ttypeType::String) : {
                ASR::String_t *a2 = ASR::down_cast<ASR::String_t>(a);
                ASR::String_t *b2 = ASR::down_cast<ASR::String_t>(b);
                return (a2->m_kind == b2->m_kind);
            }
            case (ASR::ttypeType::List) : {
                ASR::List_t *a2 = ASR::down_cast<ASR::List_t>(a);
                ASR::List_t *b2 = ASR::down_cast<ASR::List_t>(b);
                return types_equal_with_substitution(a2->m_type, b2->m_type, subs, nullptr, nullptr);
            }
            case (ASR::ttypeType::StructType) : {
                ASR::Struct_t* x_struct = nullptr;
                if (a_struct) {
                    x_struct = a_struct;
                } else if (a_expr != nullptr) {
                    ASR::symbol_t* a_sym = ASRUtils::symbol_get_past_external(
                        ASRUtils::get_struct_sym_from_struct_expr(a_expr));
                    if (a_sym != nullptr && ASR::is_a<ASR::Struct_t>(*a_sym)) {
                        x_struct = ASR::down_cast<ASR::Struct_t>(a_sym);
                    }
                }
                ASR::Struct_t* y_struct = nullptr;
                if (b_expr != nullptr) {
                    ASR::symbol_t* b_sym = ASRUtils::symbol_get_past_external(
                        ASRUtils::get_struct_sym_from_struct_expr(b_expr));
                    if (b_sym != nullptr && ASR::is_a<ASR::Struct_t>(*b_sym)) {
                        y_struct = ASR::down_cast<ASR::Struct_t>(b_sym);
                    }
                }

                if (x_struct != nullptr && y_struct != nullptr) {
                    return is_derived_type_similar(x_struct, y_struct);
                }
                return types_equal(a, b, nullptr, nullptr, check_for_dimensions);
            }
            case (ASR::ttypeType::UnionType) : {
                ASR::Union_t* x_union = ASR::down_cast<ASR::Union_t>(ASRUtils::symbol_get_past_external(
                    ASRUtils::get_union_sym_from_union_expr(a_expr)));
                ASR::Union_t* y_union = ASR::down_cast<ASR::Union_t>(ASRUtils::symbol_get_past_external(
                    ASRUtils::get_union_sym_from_union_expr(b_expr)));
                
                if (x_union == y_union) return true;

                return false;
            }
            case ASR::ttypeType::FunctionType: {
                ASR::FunctionType_t* a2 = ASR::down_cast<ASR::FunctionType_t>(a);
                ASR::FunctionType_t* b2 = ASR::down_cast<ASR::FunctionType_t>(b);

                ASR::Function_t* left_func = const_cast<ASR::Function_t*>(ASRUtils::get_function_from_expr(a_expr));
                ASR::Function_t* right_func = const_cast<ASR::Function_t*>(ASRUtils::get_function_from_expr(b_expr));

                LCOMPILERS_ASSERT(left_func == nullptr || left_func->n_args == a2->n_arg_types);
                LCOMPILERS_ASSERT(right_func == nullptr || right_func->n_args == b2->n_arg_types);
                if (left_func == nullptr || right_func == nullptr
                    || left_func->n_args != a2->n_arg_types
                    || right_func->n_args != b2->n_arg_types) {
                    return types_equal(a, b, nullptr, nullptr, true);
                }

                if( a2->n_arg_types != b2->n_arg_types ||
                    (a2->m_return_var_type != nullptr && b2->m_return_var_type == nullptr) ||
                    (a2->m_return_var_type == nullptr && b2->m_return_var_type != nullptr) ) {
                    return false;
                }
                for( size_t i = 0; i < a2->n_arg_types; i++ ) {
                    if( !types_equal_with_substitution(a2->m_arg_types[i], b2->m_arg_types[i], subs, left_func->m_args[i], right_func->m_args[i], true) ) {
                        return false;
                    }
                }
                if( !types_equal_with_substitution(a2->m_return_var_type, b2->m_return_var_type, subs, left_func->m_return_var, right_func->m_return_var, true) ) {
                    return false;
                }
                return true;
            }
            default : return false;
        }
    }
    return false;
}

bool check_equal_type(ASR::ttype_t* x, ASR::ttype_t* y, ASR::expr_t* x_expr, ASR::expr_t* y_expr, bool check_for_dimensions) {
    ASR::ttype_t *x_underlying, *y_underlying;
    x_underlying = nullptr;
    y_underlying = nullptr;
    if( ASR::is_a<ASR::EnumType_t>(*x) ) {
        ASR::EnumType_t *x_enum = ASR::down_cast<ASR::EnumType_t>(x);
        ASR::Enum_t *x_enum_type = ASR::down_cast<ASR::Enum_t>(
            ASRUtils::symbol_get_past_external(x_enum->m_enum_type));
        x_underlying = x_enum_type->m_type;
    }
    if( ASR::is_a<ASR::EnumType_t>(*y) ) {
        ASR::EnumType_t *y_enum = ASR::down_cast<ASR::EnumType_t>(y);
        ASR::Enum_t *y_enum_type = ASR::down_cast<ASR::Enum_t>(
            ASRUtils::symbol_get_past_external(y_enum->m_enum_type));
        y_underlying = y_enum_type->m_type;
    }
    if( x_underlying || y_underlying ) {
        if( x_underlying ) {
            x = x_underlying;
        }
        if( y_underlying ) {
            y = y_underlying;
        }
        return check_equal_type(x, y, x_expr, y_expr);
    }
    if( ASR::is_a<ASR::Pointer_t>(*x) ||
        ASR::is_a<ASR::Pointer_t>(*y) ) {
        x = ASRUtils::type_get_past_pointer(x);
        y = ASRUtils::type_get_past_pointer(y);
        return check_equal_type(x, y, x_expr, y_expr);
    } else if( ASR::is_a<ASR::Allocatable_t>(*x) ||
               ASR::is_a<ASR::Allocatable_t>(*y) ) {
        x = ASRUtils::type_get_past_allocatable(x);
        y = ASRUtils::type_get_past_allocatable(y);
        return check_equal_type(x, y, x_expr, y_expr);
    } else if (ASR::is_a<ASR::List_t>(*x) && ASR::is_a<ASR::List_t>(*y)) {
        x = ASR::down_cast<ASR::List_t>(x)->m_type;
        y = ASR::down_cast<ASR::List_t>(y)->m_type;
        return check_equal_type(x, y, x_expr, y_expr);
    } else if (ASR::is_a<ASR::Set_t>(*x) && ASR::is_a<ASR::Set_t>(*y)) {
        x = ASR::down_cast<ASR::Set_t>(x)->m_type;
        y = ASR::down_cast<ASR::Set_t>(y)->m_type;
        return check_equal_type(x, y, x_expr, y_expr);
    } else if (ASR::is_a<ASR::Dict_t>(*x) && ASR::is_a<ASR::Dict_t>(*y)) {
        ASR::ttype_t *x_key_type = ASR::down_cast<ASR::Dict_t>(x)->m_key_type;
        ASR::ttype_t *y_key_type = ASR::down_cast<ASR::Dict_t>(y)->m_key_type;
        ASR::ttype_t *x_value_type = ASR::down_cast<ASR::Dict_t>(x)->m_value_type;
        ASR::ttype_t *y_value_type = ASR::down_cast<ASR::Dict_t>(y)->m_value_type;
        return (check_equal_type(x_key_type, y_key_type, nullptr, nullptr) &&
                check_equal_type(x_value_type, y_value_type, nullptr, nullptr));
    } else if (ASR::is_a<ASR::Tuple_t>(*x) && ASR::is_a<ASR::Tuple_t>(*y)) {
        ASR::Tuple_t *a = ASR::down_cast<ASR::Tuple_t>(x);
        ASR::Tuple_t *b = ASR::down_cast<ASR::Tuple_t>(y);
        if(a->n_type != b->n_type) {
            return false;
        }
        bool result = true;
        for (size_t i=0; i<a->n_type; i++) {
            result = result && check_equal_type(a->m_type[i], b->m_type[i], nullptr, nullptr);
            if (!result) {
                return false;
            }
        }
        return result;
    } else if (ASR::is_a<ASR::TypeParameter_t>(*x) && ASR::is_a<ASR::TypeParameter_t>(*y)) {
        ASR::TypeParameter_t* left_tp = ASR::down_cast<ASR::TypeParameter_t>(x);
        ASR::TypeParameter_t* right_tp = ASR::down_cast<ASR::TypeParameter_t>(y);
        std::string left_param = left_tp->m_param;
        std::string right_param = right_tp->m_param;
        return left_param.compare(right_param) == 0;
    } else if (ASR::is_a<ASR::FunctionType_t>(*x) && ASR::is_a<ASR::FunctionType_t>(*y)) {
        ASR::FunctionType_t* left_ft = ASR::down_cast<ASR::FunctionType_t>(x);
        ASR::FunctionType_t* right_ft = ASR::down_cast<ASR::FunctionType_t>(y);

        if (x_expr == nullptr || y_expr == nullptr) {
            return types_equal(x, y, nullptr, nullptr, true);
        }

        ASR::Function_t* left_func = const_cast<ASR::Function_t*>(ASRUtils::get_function_from_expr(x_expr));
        ASR::Function_t* right_func = const_cast<ASR::Function_t*>(ASRUtils::get_function_from_expr(y_expr));

        LCOMPILERS_ASSERT(left_func == nullptr || left_func->n_args == left_ft->n_arg_types);
        LCOMPILERS_ASSERT(right_func == nullptr || right_func->n_args == right_ft->n_arg_types);
        if (left_func == nullptr || right_func == nullptr
            || left_func->n_args != left_ft->n_arg_types
            || right_func->n_args != right_ft->n_arg_types) {
            return types_equal(x, y, nullptr, nullptr, true);
        }

        if (left_ft->n_arg_types != right_ft->n_arg_types) {
            return false;
        }
        bool result;
        for (size_t i=0; i<left_ft->n_arg_types; i++) {
            result = check_equal_type(left_ft->m_arg_types[i],
                        right_ft->m_arg_types[i], left_func->m_args[i], right_func->m_args[i]);
            if (!result) return false;
        }
        if (left_ft->m_return_var_type == nullptr &&
                right_ft->m_return_var_type == nullptr) {
                return true;
        } else if (left_ft->m_return_var_type != nullptr &&
                right_ft->m_return_var_type != nullptr) {
                return check_equal_type(left_ft->m_return_var_type,
                        right_ft->m_return_var_type, left_func->m_return_var, right_func->m_return_var);
        }
        return false;
    }

    return types_equal(x, y, x_expr, y_expr, check_for_dimensions);
}

bool check_class_assignment_compatibility(ASR::expr_t* target, ASR::expr_t* value) {
    ASR::ttype_t* tar_ext = ASRUtils::extract_type(ASRUtils::expr_type(target));
    ASR::ttype_t* val_ext = ASRUtils::extract_type(ASRUtils::expr_type(value));
    bool is_class_same = false;
    if (ASR::is_a<ASR::StructType_t>(*tar_ext) && ASR::is_a<ASR::StructType_t>(*val_ext)) {
        ASR::symbol_t* sym_target = ASRUtils::get_struct_sym_from_struct_expr(target);
        sym_target = ASRUtils::symbol_get_past_external(sym_target);
        ASR::symbol_t* sym_value = ASRUtils::get_struct_sym_from_struct_expr(value);
        sym_value = ASRUtils::symbol_get_past_external(sym_value);
        ASR::Struct_t* tar_struct = ASR::down_cast<ASR::Struct_t>(sym_target);
        ASR::Struct_t* val_struct = ASR::down_cast<ASR::Struct_t>(sym_value);
        bool tar_is_upoly = tar_struct->m_struct_signature != nullptr &&
            ASR::down_cast<ASR::StructType_t>(
                tar_struct->m_struct_signature)->m_is_unlimited_polymorphic;
        is_class_same = (sym_target == sym_value);
        is_class_same = is_class_same || tar_is_upoly;
        is_class_same = is_class_same || ASRUtils::is_parent(tar_struct, val_struct);
    }
    return is_class_same;
}

bool check_class_assignment_compatibility(ASR::symbol_t* target, ASR::symbol_t* value) {
    target = ASRUtils::symbol_get_past_external(target);
    value = ASRUtils::symbol_get_past_external(value);
    bool is_class_same = false;
    if (ASR::is_a<ASR::Struct_t>(*target) && ASR::is_a<ASR::Struct_t>(*value)) {
        ASR::Struct_t* tar_struct = ASR::down_cast<ASR::Struct_t>(target);
        ASR::Struct_t* val_struct = ASR::down_cast<ASR::Struct_t>(value);
        bool tar_is_upoly = tar_struct->m_struct_signature != nullptr &&
            ASR::down_cast<ASR::StructType_t>(
                tar_struct->m_struct_signature)->m_is_unlimited_polymorphic;
        is_class_same = (target == value)
            || tar_is_upoly
            || ASRUtils::is_parent(tar_struct, val_struct);
    }
    return is_class_same;
}

bool is_elemental(ASR::symbol_t* x) {
    ASR::symbol_t* proc = ASRUtils::symbol_get_past_external(x);
    if (ASR::is_a<ASR::StructMethodDeclaration_t>(*proc)) {
        proc = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::StructMethodDeclaration_t>(proc)->m_proc);
    }
    if( !ASR::is_a<ASR::Function_t>(*proc) ) {
        return false;
    }
    return ASRUtils::get_FunctionType(
        ASR::down_cast<ASR::Function_t>(proc))->m_elemental;
}

void insert_module_dependency(ASR::symbol_t* a,
    Allocator& al, SetChar& module_dependencies) {
    if( ASR::is_a<ASR::ExternalSymbol_t>(*a) ) {
        ASR::ExternalSymbol_t* a_ext = ASR::down_cast<ASR::ExternalSymbol_t>(a);
        ASR::symbol_t* a_sym_module = ASRUtils::get_asr_owner(a_ext->m_external);
        if( a_sym_module ) {
            while( a_sym_module && !ASR::is_a<ASR::Module_t>(*a_sym_module) ) {
                a_sym_module = ASRUtils::get_asr_owner(a_sym_module);
            }
            if( a_sym_module ) {
                module_dependencies.push_back(al, ASRUtils::symbol_name(a_sym_module));
            }
        }
    }
}

ASR::ttype_t* get_type_parameter(ASR::ttype_t* t) {
    switch (t->type) {
        case ASR::ttypeType::TypeParameter: {
            return t;
        }
        case ASR::ttypeType::List: {
            ASR::List_t *tl = ASR::down_cast<ASR::List_t>(t);
            return get_type_parameter(tl->m_type);
        }
        case ASR::ttypeType::Array: {
            ASR::Array_t* array_t = ASR::down_cast<ASR::Array_t>(t);
            return get_type_parameter(array_t->m_type);
        }
        default: throw LCompilersException("Cannot get type parameter from this type.");
    }
}

bool set_struct_member_owner_path(Allocator& al,
        ASR::ExternalSymbol_t* e) {
    ASR::symbol_t* struct_t = ASRUtils::get_asr_owner(
        ASRUtils::symbol_get_past_external(e->m_external));
    std::vector<ASR::symbol_t*> path = {struct_t};
    ASR::symbol_t* owner = ASRUtils::get_asr_owner(struct_t);
    while (owner != nullptr && !ASR::is_a<ASR::Module_t>(*owner)) {
        path.push_back(owner);
        owner = ASRUtils::get_asr_owner(owner);
    }
    if (owner == nullptr) {
        return false;
    }
    e->m_module_name = s2c(al, ASRUtils::symbol_name(owner));
    e->n_scope_names = path.size();
    e->m_scope_names = al.allocate<char*>(e->n_scope_names);
    for (size_t i = 0; i < e->n_scope_names; i++) {
        e->m_scope_names[i] = s2c(al,
            ASRUtils::symbol_name(path[e->n_scope_names - 1 - i]));
    }
    return true;
}

ASR::symbol_t* make_struct_member_external_symbol(Allocator& al,
        const Location& loc, SymbolTable* scope, const std::string& name,
        ASR::symbol_t* struct_member, const std::string& struct_local_name,
        ASR::accessType access) {
    struct_member = ASRUtils::symbol_get_past_external(struct_member);
    ASR::symbol_t* e = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(al,
        loc, scope, s2c(al, name), struct_member, s2c(al, struct_local_name),
        nullptr, 0, s2c(al, ASRUtils::symbol_name(struct_member)), access));
    set_struct_member_owner_path(al, ASR::down_cast<ASR::ExternalSymbol_t>(e));
    return e;
}

ASR::symbol_t* import_struct_instance_member(Allocator& al,
                                                           ASR::symbol_t* struct_member,
                                                           SymbolTable* scope)
{
    struct_member = ASRUtils::symbol_get_past_external(struct_member);
    std::string struct_member_name = ASRUtils::symbol_name(struct_member);

    ASR::symbol_t* struct_t = ASRUtils::get_asr_owner(struct_member);
    std::string struct_t_name = ASRUtils::symbol_name(struct_t);

    std::string struct_external_symbol_name = struct_t_name;

    if (ASRUtils::symbol_get_past_external(scope->resolve_symbol(struct_t_name)) != struct_t) {
        struct_external_symbol_name = "1_" + struct_external_symbol_name;
    }

    if (scope->resolve_symbol(struct_external_symbol_name) == nullptr) {
        ASR::symbol_t* struct_t_module = ASRUtils::get_asr_owner(
                                            ASRUtils::symbol_get_past_external(struct_t));
        LCOMPILERS_ASSERT(struct_t_module != nullptr);

        if (ASR::is_a<ASR::Program_t>(*struct_t_module) ||
                ASR::is_a<ASR::Block_t>(*struct_t_module)) {
            return struct_member;
        }

        SymbolTable* struct_t_import_scope = scope;
        while (struct_t_import_scope->asr_owner == nullptr
               || !(ASR::is_a<ASR::Module_t>(
                       *ASR::down_cast<ASR::symbol_t>(struct_t_import_scope->asr_owner)) ||
                    ASRUtils::is_device_kernel(
                       ASR::down_cast<ASR::symbol_t>(struct_t_import_scope->asr_owner)))) {
            struct_t_import_scope = struct_t_import_scope->parent;
            if (struct_t_import_scope->asr_owner != nullptr
                && !ASR::is_a<ASR::symbol_t>(*struct_t_import_scope->asr_owner)) {
                break;
            }
        }
        LCOMPILERS_ASSERT(struct_t_import_scope != nullptr);

        ASR::symbol_t* struct_external_symbol = ASR::down_cast<ASR::symbol_t>(
                                                    ASR::make_ExternalSymbol_t(al,
                                                                            struct_member->base.loc,
                                                                            struct_t_import_scope,
                                                                            s2c(al, struct_external_symbol_name),
                                                                            struct_t,
                                                                            ASRUtils::symbol_name(struct_t_module),
                                                                            nullptr,
                                                                            0,
                                                                            s2c(al, struct_t_name),
                                                                            ASR::accessType::Public));

        struct_t_import_scope->add_symbol(struct_external_symbol_name, struct_external_symbol);
    }

    std::string struct_member_external_symbol_name = "1_" + struct_t_name + "_" + struct_member_name;
    if (scope->get_symbol(struct_member_external_symbol_name) == nullptr) {
        ASR::symbol_t* struct_member_external_symbol = make_struct_member_external_symbol(
            al, struct_member->base.loc, scope, struct_member_external_symbol_name,
            struct_member, struct_external_symbol_name, ASR::accessType::Public);
        scope->add_symbol(struct_member_external_symbol_name, struct_member_external_symbol);
    }

    return scope->get_symbol(struct_member_external_symbol_name);
}

ASR::symbol_t* import_enum_member(Allocator& al, ASR::symbol_t* v,
    SymbolTable* scope) {
    v = ASRUtils::symbol_get_past_external(v);
    ASR::symbol_t* enum_t = ASRUtils::get_asr_owner(v);
    std::string v_name = ASRUtils::symbol_name(v);
    std::string enum_t_name = ASRUtils::symbol_name(enum_t);
    std::string enum_ext_name = enum_t_name;
    if( scope->resolve_symbol(enum_t_name) != enum_t ) {
        enum_ext_name = "1_" + enum_ext_name;
    }
    if( scope->resolve_symbol(enum_ext_name) == nullptr ) {
        ASR::symbol_t* enum_t_module = ASRUtils::get_asr_owner(
            ASRUtils::symbol_get_past_external(enum_t));
        LCOMPILERS_ASSERT(enum_t_module != nullptr);
        SymbolTable* import_enum_t_scope = scope;
        while( import_enum_t_scope->asr_owner == nullptr ||
               !ASR::is_a<ASR::Module_t>(*ASR::down_cast<ASR::symbol_t>(
                import_enum_t_scope->asr_owner)) ) {
            import_enum_t_scope = import_enum_t_scope->parent;
        }
        LCOMPILERS_ASSERT(import_enum_t_scope != nullptr);
        ASR::symbol_t* enum_ext = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(al,
                                    v->base.loc, import_enum_t_scope, s2c(al, enum_ext_name), enum_t,
                                    ASRUtils::symbol_name(enum_t_module),
                                    nullptr, 0, s2c(al, enum_t_name), ASR::accessType::Public));
        import_enum_t_scope->add_symbol(enum_ext_name, enum_ext);
    }
    std::string v_ext_name = "1_" + enum_t_name + "_" + v_name;
    if( scope->get_symbol(v_ext_name) == nullptr ) {
        ASR::symbol_t* v_ext = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(al,
                                    v->base.loc, scope, s2c(al, v_ext_name), ASRUtils::symbol_get_past_external(v),
                                    s2c(al, enum_ext_name), nullptr, 0, s2c(al, v_name), ASR::accessType::Public));
        scope->add_symbol(v_ext_name, v_ext);
    }

    return scope->get_symbol(v_ext_name);
}

ASR::symbol_t* import_struct_type(Allocator& al, ASR::symbol_t* struct_sym,
    SymbolTable* scope) {
    struct_sym = ASRUtils::symbol_get_past_external(struct_sym);
    if (!ASR::is_a<ASR::Struct_t>(*struct_sym)) {
        return struct_sym;
    }
    std::string struct_name = ASRUtils::symbol_name(struct_sym);
    ASR::Struct_t* struct_t = ASR::down_cast<ASR::Struct_t>(struct_sym);
    if (struct_t->m_struct_signature != nullptr &&
        ASR::down_cast<ASR::StructType_t>(
            struct_t->m_struct_signature)->m_is_unlimited_polymorphic) {
        // Unlimited polymorphic is a per-scope synthetic type that cannot
        // be imported as an ExternalSymbol.  Find a local instance in the
        // scope chain or create one so the modfile serializer never writes
        // a dangling symtab reference from the foreign module.
        ASR::symbol_t* local = scope->resolve_symbol(struct_name);
        if (local != nullptr) {
            return local;
        }
        SymbolTable* upt_symtab = al.make_new<SymbolTable>(scope);
        ASR::asr_t* dtype = ASR::make_Struct_t(al, struct_sym->base.loc,
            upt_symtab, s2c(al, struct_name), nullptr, nullptr, 0,
            nullptr, 0, nullptr, 0, ASR::abiType::Source,
            ASR::accessType::Public, false, true, false, nullptr, 0,
            nullptr, nullptr, nullptr, 0);
        ASR::symbol_t* new_sym = ASR::down_cast<ASR::symbol_t>(dtype);
        ASR::ttype_t* sig = ASRUtils::make_StructType_t_util(
            al, struct_sym->base.loc, new_sym, false);
        ASR::down_cast<ASR::Struct_t>(new_sym)->m_struct_signature = sig;
        scope->add_symbol(struct_name, new_sym);
        return new_sym;
    }

    // Get the module that owns this struct
    ASR::symbol_t* struct_module = ASRUtils::get_asr_owner(struct_sym);
    if (struct_module == nullptr) {
        return struct_sym;
    }

    // Get the module symtab for the target scope
    SymbolTable* target_module_scope = scope;
    while (target_module_scope->asr_owner == nullptr ||
           !ASR::is_a<ASR::Module_t>(*ASR::down_cast<ASR::symbol_t>(target_module_scope->asr_owner))) {
        target_module_scope = target_module_scope->parent;
        if (target_module_scope == nullptr || target_module_scope->asr_owner == nullptr) {
            return struct_sym;
        }
        if (!ASR::is_a<ASR::symbol_t>(*target_module_scope->asr_owner)) {
            return struct_sym;
        }
    }

    // Check if the struct is from the same module as target scope
    ASR::symbol_t* target_module = ASR::down_cast<ASR::symbol_t>(target_module_scope->asr_owner);
    if (struct_module == target_module) {
        return struct_sym;  // Local symbol, no need to externalize
    }

    // Check if ExternalSymbol already exists
    std::string ext_sym_name = struct_name;
    ASR::symbol_t* existing = target_module_scope->resolve_symbol(ext_sym_name);
    if (existing && ASRUtils::symbol_get_past_external(existing) == struct_sym) {
        return existing;
    }
    if (existing) {
        ext_sym_name = "1_" + struct_name;
        existing = target_module_scope->get_symbol(ext_sym_name);
        if (existing && ASRUtils::symbol_get_past_external(existing) == struct_sym) {
            return existing;
        }
    }

    // Create ExternalSymbol if not exists
    if (target_module_scope->get_symbol(ext_sym_name) == nullptr) {
        ASR::symbol_t* ext_sym = ASR::down_cast<ASR::symbol_t>(
            ASR::make_ExternalSymbol_t(al, struct_sym->base.loc, target_module_scope,
                s2c(al, ext_sym_name), struct_sym, ASRUtils::symbol_name(struct_module),
                nullptr, 0, s2c(al, struct_name), ASR::accessType::Public));
        target_module_scope->add_symbol(ext_sym_name, ext_sym);
    }

    return target_module_scope->get_symbol(ext_sym_name);
}

ASR::expr_t* externalize_struct_refs_in_init(Allocator& al,
    ASR::expr_t* init_expr, SymbolTable* scope) {
    if (init_expr == nullptr) return nullptr;

    if (ASR::is_a<ASR::StructConstant_t>(*init_expr)) {
        ASR::StructConstant_t* sc = ASR::down_cast<ASR::StructConstant_t>(init_expr);
        ASR::symbol_t* ext_sym = import_struct_type(al, sc->m_dt_sym, scope);

        // Recursively process arguments
        Vec<ASR::call_arg_t> new_args;
        new_args.reserve(al, sc->n_args);
        for (size_t i = 0; i < sc->n_args; i++) {
            ASR::call_arg_t arg;
            arg.loc = sc->m_args[i].loc;
            arg.m_value = externalize_struct_refs_in_init(al, sc->m_args[i].m_value, scope);
            new_args.push_back(al, arg);
        }

        ASR::ttype_t* new_type = ASRUtils::make_StructType_t_util(al, init_expr->base.loc, ext_sym, true);
        return ASRUtils::EXPR(ASR::make_StructConstant_t(al, init_expr->base.loc,
            ext_sym, new_args.p, new_args.size(), new_type));
    } else if (ASR::is_a<ASR::StructConstructor_t>(*init_expr)) {
        ASR::StructConstructor_t* sc = ASR::down_cast<ASR::StructConstructor_t>(init_expr);
        ASR::symbol_t* ext_sym = import_struct_type(al, sc->m_dt_sym, scope);

        // Recursively process arguments
        Vec<ASR::call_arg_t> new_args;
        new_args.reserve(al, sc->n_args);
        for (size_t i = 0; i < sc->n_args; i++) {
            ASR::call_arg_t arg;
            arg.loc = sc->m_args[i].loc;
            arg.m_value = externalize_struct_refs_in_init(al, sc->m_args[i].m_value, scope);
            new_args.push_back(al, arg);
        }

        ASR::ttype_t* new_type = ASRUtils::make_StructType_t_util(al, init_expr->base.loc, ext_sym, true);
        ASR::expr_t* new_value = externalize_struct_refs_in_init(al, sc->m_value, scope);
        return ASRUtils::EXPR(ASR::make_StructConstructor_t(al, init_expr->base.loc,
            ext_sym, new_args.p, new_args.size(), new_type, new_value));
    } else if (ASR::is_a<ASR::PointerNullConstant_t>(*init_expr)) {
        ASR::PointerNullConstant_t* pnc = ASR::down_cast<ASR::PointerNullConstant_t>(init_expr);
        ASR::ttype_t* pnc_type = import_procedure_type(al, pnc->m_type, scope);
        if (pnc->m_var_expr != nullptr) {
            // The var_expr references a type-identity symbol in another scope
            // which may not be accessible from the current scope. Replace it
            // with that symbol as imported or otherwise visible here.
            ASR::symbol_t* struct_sym = ASRUtils::get_struct_sym_from_struct_expr(pnc->m_var_expr);
            if (struct_sym != nullptr) {
                ASR::symbol_t* resolved = ASRUtils::import_type_declaration(
                    al, struct_sym, scope);
                if (resolved != nullptr) {
                    ASR::expr_t* new_var_expr = ASRUtils::EXPR(
                        ASR::make_Var_t(al, init_expr->base.loc, resolved));
                    return ASRUtils::EXPR(ASR::make_PointerNullConstant_t(
                        al, init_expr->base.loc, pnc_type, new_var_expr));
                }
            }
            // If we can't resolve, strip var_expr to avoid scope violations
            return ASRUtils::EXPR(ASR::make_PointerNullConstant_t(
                al, init_expr->base.loc, pnc_type, nullptr));
        } else if (pnc_type != pnc->m_type) {
            return ASRUtils::EXPR(ASR::make_PointerNullConstant_t(
                al, init_expr->base.loc, pnc_type, nullptr));
        }
    }

    // For other expression types, return as-is (they don't contain struct references in their construction)
    return init_expr;
}

ASR::asr_t* make_FunctionType_t_util(Allocator &al,
    const Location &a_loc, ASR::expr_t** a_args, size_t n_args,
    ASR::expr_t* a_return_var, ASR::abiType a_abi, ASR::deftypeType a_deftype,
    char* a_bindc_name, bool a_elemental, bool a_pure, bool a_module, bool a_inline,
    bool a_static,
    ASR::symbol_t** a_restrictions, size_t n_restrictions, bool a_is_restriction, SymbolTable* current_scope,
    ASR::exec_spaceType a_exec_space) {
    Vec<ASR::ttype_t*> arg_types;
    arg_types.reserve(al, n_args);
    ReplaceWithFunctionParamVisitor replacer(al, a_args, n_args);
    for( size_t i = 0; i < n_args; i++ ) {
        // We need to substitute all direct argument variable references with
        // FunctionParam.
        ASR::ttype_t *t = replacer.replace_args_with_FunctionParam(
                            expr_type(a_args[i]), current_scope);
        arg_types.push_back(al, t);
    }
    ASR::ttype_t* return_var_type = nullptr;
    if( a_return_var ) {
        return_var_type = replacer.replace_args_with_FunctionParam(
                            ASRUtils::expr_type(a_return_var), current_scope);
    }

    LCOMPILERS_ASSERT(arg_types.size() == n_args);
    return ASR::make_FunctionType_t(
        al, a_loc, arg_types.p, arg_types.size(), return_var_type, a_abi, a_deftype,
        a_bindc_name, a_elemental, a_pure, a_module, a_inline,
        a_static, a_restrictions, n_restrictions,
        a_is_restriction, a_exec_space);
}

ASR::asr_t* make_Function_t_util(Allocator& al, const Location& loc,
    SymbolTable* m_symtab, char* m_name, char** m_dependencies, size_t n_dependencies,
    ASR::expr_t** a_args, size_t n_args, ASR::stmt_t** m_body, size_t n_body,
    ASR::expr_t* m_return_var, ASR::abiType m_abi, ASR::accessType m_access,
    ASR::deftypeType m_deftype, char* m_bindc_name, bool m_elemental, bool m_pure,
    bool m_module, bool m_inline, bool m_static,
    ASR::symbol_t** m_restrictions, size_t n_restrictions, bool m_is_restriction,
    bool m_deterministic, bool m_side_effect_free, char *m_c_header, Location* m_start_name,
    Location* m_end_name,
    ASR::exec_spaceType m_exec_space) {
    ASR::ttype_t* func_type = ASRUtils::TYPE(ASRUtils::make_FunctionType_t_util(
        al, loc, a_args, n_args, m_return_var, m_abi, m_deftype, m_bindc_name,
        m_elemental, m_pure, m_module, m_inline, m_static,
        m_restrictions, n_restrictions, m_is_restriction, m_symtab, m_exec_space));
    return ASR::make_Function_t(
        al, loc, m_symtab, m_name, func_type, m_dependencies, n_dependencies,
        a_args, n_args, m_body, n_body, m_return_var, m_access, m_deterministic,
        m_side_effect_free, m_c_header, nullptr, m_start_name, m_end_name);
}

void cast_to_wider_kind(Allocator& al, ASR::expr_t*& a, ASR::expr_t*& b) {
    ASR::ttype_t* a_type = ASRUtils::type_get_past_allocatable_pointer(ASRUtils::expr_type(a));
    ASR::ttype_t* b_type = ASRUtils::type_get_past_allocatable_pointer(ASRUtils::expr_type(b));
    if( !ASRUtils::is_integer(*a_type) || !ASRUtils::is_integer(*b_type) ) {
        return;
    }
    int a_kind = ASRUtils::extract_kind_from_ttype_t(a_type);
    int b_kind = ASRUtils::extract_kind_from_ttype_t(b_type);
    if( a_kind < b_kind ) {
        a = ASRUtils::EXPR(ASRUtils::make_Cast_t_value(al, a->base.loc, a,
            ASR::cast_kindType::IntegerToInteger, b_type));
    } else if( b_kind < a_kind ) {
        b = ASRUtils::EXPR(ASRUtils::make_Cast_t_value(al, b->base.loc, b,
            ASR::cast_kindType::IntegerToInteger, a_type));
    }
}

ASR::expr_t* compute_length_from_start_end(Allocator& al, ASR::expr_t* start, ASR::expr_t* end) {
    ASR::expr_t* start_value = nullptr;
    ASR::expr_t* end_value = nullptr;
    if (start != nullptr) {
        start_value = ASRUtils::expr_value(start);
    }
    if (end != nullptr) {
        end_value = ASRUtils::expr_value(end);
    }

    // If both start and end have compile time values
    // then length can be computed easily by extracting
    // compile time values of end and start.
    if( start_value && end_value ) {
        int64_t start_int = -1, end_int = -1, size = 0;
        ASRUtils::extract_value(start_value, start_int);
        ASRUtils::extract_value(end_value, end_int);
        if (end_int - start_int + 1 > 0) {
            size = end_int - start_int + 1;
        }
        return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, start->base.loc,
                              size,
                              ASRUtils::expr_type(start)));
    }

    // If start has a compile time value and
    // end is a variable then length can be
    // simplified by computing 1 - start as a constant
    // and then analysing the end expression.
    if( start_value && !end_value ) {
        int64_t start_int = -1;
        ASRUtils::extract_value(start_value, start_int);
        int64_t remaining_portion = 1 - start_int;

        // If 1 - start is 0 then length is clearly the
        // end expression.
        if( remaining_portion == 0 ) {
            return end;
        }

        // If end is a binary expression of Add, Sub
        // type.
        if( ASR::is_a<ASR::IntegerBinOp_t>(*end) ) {
            ASR::IntegerBinOp_t* end_binop = ASR::down_cast<ASR::IntegerBinOp_t>(end);
            if( end_binop->m_op == ASR::binopType::Add ||
                end_binop->m_op == ASR::binopType::Sub) {
                ASR::expr_t* end_left = end_binop->m_left;
                ASR::expr_t* end_right = end_binop->m_right;
                ASR::expr_t* end_leftv = ASRUtils::expr_value(end_left);
                ASR::expr_t* end_rightv = ASRUtils::expr_value(end_right);
                if( end_leftv ) {
                    // If left part of end is a compile time constant
                    // then it can be merged with 1 - start.
                    int64_t el_int = -1;
                    ASRUtils::extract_value(end_leftv, el_int);
                    remaining_portion += el_int;

                    // If 1 - start + end_left is 0
                    // and end is an addition operation
                    // then clearly end_right is the length.
                    if( remaining_portion == 0 &&
                        end_binop->m_op == ASR::binopType::Add ) {
                        return end_right;
                    }

                    // In all other cases the length would be (1 - start + end_left) endop end_right
                    // endop is the operation of end expression and 1 - start + end_left is a constant.
                    ASR::expr_t* remaining_expr = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al,
                                                    end->base.loc, remaining_portion,
                                                    ASRUtils::expr_type(end)));
                    return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, end->base.loc, remaining_expr,
                                end_binop->m_op, end_right, end_binop->m_type, end_binop->m_value));
                } else if( end_rightv ) {
                    // If right part of end is a compile time constant
                    // then it can be merged with 1 - start. The sign
                    // of end_right depends on the operation in
                    // end expression.
                    int64_t er_int = -1;
                    ASRUtils::extract_value(end_rightv, er_int);
                    if( end_binop->m_op == ASR::binopType::Sub ) {
                        er_int = -er_int;
                    }
                    remaining_portion += er_int;

                    // If (1 - start endop end_right) is 0
                    // then clearly end_left is the length expression.
                    if( remaining_portion == 0 ) {
                        return end_left;
                    }

                    // Otherwise, length is end_left Add (1 - start endop end_right)
                    // where endop is the operation in end expression and
                    // (1 - start endop end_right) is a compile time constant.
                    ASR::expr_t* remaining_expr = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al,
                                                    end->base.loc, remaining_portion,
                                                    ASRUtils::expr_type(end)));
                    return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, end->base.loc, end_left,
                                ASR::binopType::Add, remaining_expr, end_binop->m_type, end_binop->m_value));
                }
            }
        }

        // If start is a variable and end is a compile time constant
        // then compute (end + 1) as a constant and then return
        // (end + 1) - start as the length expression.
        if( !start_value && end_value ) {
            int64_t end_int = -1;
            ASRUtils::extract_value(end_value, end_int);
            int64_t remaining_portion = end_int + 1;
            ASR::expr_t* remaining_expr = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al,
                                                end->base.loc, remaining_portion,
                                                ASRUtils::expr_type(end)));
            return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, end->base.loc, remaining_expr,
                        ASR::binopType::Sub, start, ASRUtils::expr_type(end), nullptr));
        }

        // For all the other cases
        ASR::expr_t* remaining_expr = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al,
                                                end->base.loc, remaining_portion,
                                                ASRUtils::expr_type(end)));
        return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, end->base.loc, end,
                    ASR::binopType::Add, remaining_expr, ASRUtils::expr_type(end),
                    nullptr));
    }

    cast_to_wider_kind(al, start, end);
    ASR::expr_t* diff = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, end->base.loc, end,
                                       ASR::binopType::Sub, start, ASRUtils::expr_type(end),
                                       nullptr));
    ASR::expr_t *constant_one = ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(
                                            al, diff->base.loc, 1, ASRUtils::expr_type(diff)));
    return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, end->base.loc, diff,
                          ASR::binopType::Add, constant_one, ASRUtils::expr_type(end),
                          nullptr));
}

ASR::expr_t* compute_length_from_start_end_step(Allocator& al,
        ASR::expr_t* start, ASR::expr_t* end, ASR::expr_t* step) {
    if( start == nullptr || end == nullptr ) {
        return compute_length_from_start_end(al, start, end);
    }
    int64_t step_int = 1;
    bool step_is_known = true;
    if( step != nullptr ) {
        ASR::expr_t* step_value = ASRUtils::expr_value(step);
        step_is_known = step_value != nullptr &&
            ASRUtils::extract_value(step_value, step_int);
    }
    if( step_is_known && step_int == 1 ) {
        return compute_length_from_start_end(al, start, end);
    }
    ASR::ttype_t* int_type = ASRUtils::expr_type(end);
    if( step_is_known && step_int == 0 ) {
        // A zero step is not a valid section. Nothing can be computed from it,
        // so give it no elements rather than an extent that is not a constant,
        // which later phases require for a section with constant bounds.
        return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, end->base.loc,
            0, int_type));
    } else if( step_is_known ) {
        ASR::expr_t* start_value = ASRUtils::expr_value(start);
        ASR::expr_t* end_value = ASRUtils::expr_value(end);
        int64_t start_int = 0, end_int = 0;
        if( start_value != nullptr && end_value != nullptr &&
            ASRUtils::extract_value(start_value, start_int) &&
            ASRUtils::extract_value(end_value, end_int) ) {
            int64_t size = (end_int - start_int + step_int) / step_int;
            if( size < 0 ) {
                size = 0;
            }
            return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, end->base.loc,
                size, int_type));
        }
    }
    ASR::expr_t* end_minus_start = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(
        al, end->base.loc, end, ASR::binopType::Sub, start, int_type, nullptr));
    ASR::expr_t* plus_step = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(
        al, end->base.loc, end_minus_start, ASR::binopType::Add, step, int_type,
        nullptr));
    return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, end->base.loc, plus_step,
        ASR::binopType::Div, step, int_type, nullptr));
}

bool is_pass_array_by_data_possible(ASR::Function_t* x, std::vector<size_t>& v) {
    // BindC interfaces already pass array by data pointer so we don't need to track
    // them and use extra variables for their dimensional information. Only those functions
    // need to be tracked which by default pass arrays by using descriptors.
    if ((ASRUtils::get_FunctionType(x)->m_abi == ASR::abiType::BindC
         || ASRUtils::get_FunctionType(x)->m_abi == ASR::abiType::BindPython)
        && (ASRUtils::is_declaration_deftype(ASRUtils::get_FunctionType(x)->m_deftype)
            || ASRUtils::get_FunctionType(x)->m_bindc_name)) {
        return false;
    }

    ASR::ttype_t* typei = nullptr;
    ASR::dimension_t* dims = nullptr;
    for( size_t i = 0; i < x->n_args; i++ ) {
        typei = ASRUtils::expr_type(x->m_args[i]);
        if (ASR::is_a<ASR::Array_t>(*typei)) {
            ASR::Array_t* arr_type = ASR::down_cast<ASR::Array_t>(typei);
            if (arr_type->m_physical_type == ASR::array_physical_typeType::AssumedRankArray) return false;
        }
        ASR::Var_t* arg_Var = ASR::down_cast<ASR::Var_t>(x->m_args[i]);
        if( !ASR::is_a<ASR::Variable_t>(*arg_Var->m_v) ) {
            continue;
        }
        ASR::ttype_t* typei_noarray = ASRUtils::type_get_past_array(typei);
        if( ASRUtils::is_class_type(typei_noarray) ||
            ASR::is_a<ASR::FunctionType_t>(*typei_noarray) ) {
            continue ;
        }
        int n_dims = checked_integer_cast<int>(
            ASRUtils::extract_dimensions_from_ttype(typei, dims));
        ASR::Variable_t* argi = ASRUtils::EXPR2VAR(x->m_args[i]);
        if( ASR::is_a<ASR::Pointer_t>(*argi->m_type) ) {
            return false;
        }

        // The following if check determines whether the i-th argument
        // can be called by just passing the data pointer and
        // dimensional information spearately via extra arguments.
        // Only descriptor arguments qualify: assumed-size and bind(C)
        // arrays (UnboundedPointerArray, PointerArray) are already passed
        // as a bare data pointer and carry no dimension info by design.
        ASR::array_physical_typeType phys_type = ASR::array_physical_typeType::DescriptorArray;
        if (ASR::is_a<ASR::Array_t>(*ASRUtils::type_get_past_allocatable_pointer(argi->m_type))) {
            phys_type = ASR::down_cast<ASR::Array_t>(
                ASRUtils::type_get_past_allocatable_pointer(argi->m_type))->m_physical_type;
        }
        if( n_dims > 0 && ASRUtils::is_dimension_empty(dims, n_dims) &&
            phys_type == ASR::array_physical_typeType::DescriptorArray &&
            (argi->m_intent == ASRUtils::intent_in ||
             argi->m_intent == ASRUtils::intent_out ||
             argi->m_intent == ASRUtils::intent_inout) &&
            !ASR::is_a<ASR::Allocatable_t>(*argi->m_type) &&
            !ASR::is_a<ASR::StructType_t>(*argi->m_type) &&
            !ASRUtils::is_class_type(ASRUtils::type_get_past_array(argi->m_type)) &&
            !ASR::is_a<ASR::String_t>(*argi->m_type) &&
            !argi->m_target_attr &&
            argi->m_presence != ASR::presenceType::Optional) {
            v.push_back(i);
        }
    }
    return v.size() > 0;
}

ASR::ttype_t* get_union_type(Allocator &al, const Location &loc, ASR::symbol_t* union_sym) {
    Vec<ASR::ttype_t *> member_type_vec;
    member_type_vec.reserve(al, 1);
    ASR::Union_t* union_sym_ = ASR::down_cast<ASR::Union_t>(union_sym);
    for (size_t i=0;i<union_sym_->n_members;i++) {
        ASR::symbol_t* member_sym = union_sym_->m_symtab->resolve_symbol(union_sym_->m_members[i]);
        ASR::Variable_t* member_var = ASR::down_cast<ASR::Variable_t>(member_sym);
        member_type_vec.push_back(al, member_var->m_type);
    }
    return ASRUtils::TYPE(ASR::make_UnionType_t(al, loc, member_type_vec.p, member_type_vec.n));
}

void set_enum_value_type(ASR::enumtypeType &enum_value_type,
        SymbolTable *scope) {
    int8_t IntegerConsecutiveFromZero = 1;
    int8_t IntegerNotUnique = 0;
    int8_t IntegerUnique = 1;
    std::map<int64_t, int64_t> value2count;
    for( auto sym: scope->get_scope() ) {
        ASR::Variable_t* member_var = ASR::down_cast<ASR::Variable_t>(sym.second);
        ASR::expr_t* value = ASRUtils::expr_value(member_var->m_symbolic_value);
        int64_t value_int64 = -1;
        ASRUtils::extract_value(value, value_int64);
        if( value2count.find(value_int64) == value2count.end() ) {
            value2count[value_int64] = 0;
        }
        value2count[value_int64] += 1;
    }
    int64_t prev = -1;
    for( auto itr: value2count ) {
        if( itr.second > 1 ) {
            IntegerNotUnique = 1;
            IntegerUnique = 0;
            IntegerConsecutiveFromZero = 0;
            break ;
        }
        if( itr.first - prev != 1 ) {
            IntegerConsecutiveFromZero = 0;
        }
        prev = itr.first;
    }
    if( IntegerConsecutiveFromZero ) {
        if( value2count.find(0) == value2count.end() ) {
            IntegerConsecutiveFromZero = 0;
            IntegerUnique = 1;
        } else {
            IntegerUnique = 0;
        }
    }
    LCOMPILERS_ASSERT(IntegerConsecutiveFromZero + IntegerNotUnique + IntegerUnique == 1);
    if( IntegerConsecutiveFromZero ) {
        enum_value_type = ASR::enumtypeType::IntegerConsecutiveFromZero;
    } else if( IntegerNotUnique ) {
        enum_value_type = ASR::enumtypeType::IntegerNotUnique;
    } else if( IntegerUnique ) {
        enum_value_type = ASR::enumtypeType::IntegerUnique;
    }
}

void collect_variable_dependencies(Allocator& al, SetChar& deps_vec,
    ASR::ttype_t* type, ASR::expr_t* init_expr,
    ASR::expr_t* value, std::string current_name) {
    ASRUtils::CollectIdentifiersFromASRExpression collector(al, deps_vec, current_name);
    if( init_expr ) {
        collector.visit_expr(*init_expr);
    }
    if( value ) {
        collector.visit_expr(*value);
    }
    if( type ) {
        collector.visit_ttype(*type);
    }
}

int KMP_string_match(std::string &s_var, std::string &sub) {
    int str_len = checked_integer_cast<int>(s_var.size());
    int sub_len = checked_integer_cast<int>(sub.size());
    bool flag = 0;
    int res = -1;
    std::vector<int> lps(sub_len, 0);
    if (str_len == 0 || sub_len == 0) {
        res = (!sub_len || (sub_len == str_len))? 0: -1;
    } else {
        for(int i = 1, len = 0; i < sub_len;) {
            if (sub[i] == sub[len]) {
                lps[i++] = ++len;
            } else {
                if (len != 0) {
                    len = lps[len - 1];
                } else {
                    lps[i++] = 0;
                }
            }
        }
        for (int i = 0, j = 0; (str_len - i) >= (sub_len - j) && !flag;) {
            if (sub[j] == s_var[i]) {
                j++, i++;
            }
            if (j == sub_len) {
                res = i - j;
                flag = 1;
                j = lps[j - 1];
            } else if (i < str_len && sub[j] != s_var[i]) {
                if (j != 0) {
                    j = lps[j - 1];
                } else {
                    i = i + 1;
                }
            }
        }
    }
    return res;
}

int KMP_string_match_count(std::string &s_var, std::string &sub) {
    int str_len = checked_integer_cast<int>(s_var.size());
    int sub_len = checked_integer_cast<int>(sub.size());
    int count = 0;
    std::vector<int> lps(sub_len, 0);
    if (sub_len == 0) {
        count = str_len + 1;
    } else {
        for(int i = 1, len = 0; i < sub_len;) {
            if (sub[i] == sub[len]) {
                lps[i++] = ++len;
            } else {
                if (len != 0) {
                    len = lps[len - 1];
                } else {
                    lps[i++] = 0;
                }
            }
        }
        for (int i = 0, j = 0; (str_len - i) >= (sub_len - j);) {
            if (sub[j] == s_var[i]) {
                j++, i++;
            }
            if (j == sub_len) {
                count++;
                j = lps[j - 1];
            } else if (i < str_len && sub[j] != s_var[i]) {
                if (j != 0) {
                    j = lps[j - 1];
                } else {
                    i = i + 1;
                }
            }
        }
    }
    return count;
}

ASR::dimension_t* duplicate_dimensions(Allocator& al, ASR::dimension_t* m_dims, size_t n_dims) {
    Vec<ASR::dimension_t> dims;
    dims.reserve(al, n_dims);
    ASRUtils::ExprStmtDuplicator expr_duplicator(al);
    for (size_t i = 0; i < n_dims; i++) {
        ASR::expr_t* start = m_dims[i].m_start;
        if( start != nullptr ) {
            start = expr_duplicator.duplicate_expr(start);
        }
        ASR::expr_t* length = m_dims[i].m_length;
        if( length != nullptr ) {
            length = expr_duplicator.duplicate_expr(length);
        }
        ASR::dimension_t t;
        t.loc = m_dims[i].loc;
        t.m_start = start;
        t.m_length = length;
        dims.push_back(al, t);
    }
    return dims.p;
}

bool is_move_alloc_type_compatible(ASR::expr_t* from,
        ASR::expr_t* to) {
    ASR::ttype_t* from_type = extract_type(expr_type(from));
    ASR::ttype_t* to_type = extract_type(expr_type(to));
    if (is_unlimited_polymorphic_type(to_type)) {
        return true;
    }
    if (is_unlimited_polymorphic_type(from_type)) {
        return false;
    }
    bool is_from_struct = ASR::is_a<ASR::StructType_t>(*from_type);
    bool is_to_struct = ASR::is_a<ASR::StructType_t>(*to_type);
    if (is_from_struct != is_to_struct) {
        return false;
    }
    if (!is_to_struct) {
        return check_equal_type(to_type, from_type, to, from);
    }
    if (is_class_type(to_type)) {
        return check_class_assignment_compatibility(to, from);
    }
    ASR::symbol_t* from_sym = symbol_get_past_external(
        get_struct_sym_from_struct_expr(from));
    ASR::symbol_t* to_sym = symbol_get_past_external(
        get_struct_sym_from_struct_expr(to));
    if (ASR::is_a<ASR::Struct_t>(*from_sym) && ASR::is_a<ASR::Struct_t>(*to_sym)) {
        return is_same_derived_type(ASR::down_cast<ASR::Struct_t>(from_sym),
            ASR::down_cast<ASR::Struct_t>(to_sym));
    }
    return from_sym == to_sym;
}

bool is_move_allocation_pair(ASR::expr_t* target,
        ASR::expr_t* value) {
    ASR::ttype_t* target_type = expr_type(target);
    ASR::ttype_t* value_type = expr_type(value);
    if (!is_allocatable(target_type) || !is_allocatable(value_type)) {
        return false;
    }
    if (is_array(target_type) || is_array(value_type)) {
        return is_array(target_type) && is_array(value_type) &&
            extract_physical_type(target_type) == ASR::array_physical_typeType::DescriptorArray &&
            extract_physical_type(value_type) == ASR::array_physical_typeType::DescriptorArray;
    }
    if (is_character(*target_type) || is_character(*value_type)) {
        return false;
    }
    if (is_class_type(extract_type(value_type)) &&
            !is_class_type(extract_type(target_type))) {
        return false;
    }
    return is_move_alloc_type_compatible(value, target);
}

bool is_byte_representable_struct(ASR::Struct_t* st,
        std::set<ASR::Struct_t*>& visited) {
    if( !st ) {
        return false;
    }
    if( !visited.insert(st).second ) {
        // Already on the chain being checked. A type can only reach itself
        // through a pointer or an allocatable component, and that component
        // is rejected where it is declared.
        return true;
    }
    if( st->m_parent ) {
        ASR::symbol_t* parent = symbol_get_past_external(st->m_parent);
        // `m_parent` names the inherited type, so it resolves to a Struct.
        // The assert states that invariant; the test below is still kept
        // because this function only gates a constant fold, so on a malformed
        // symbol it should decline to fold rather than down_cast through the
        // wrong type, which a -DNDEBUG build would do with the assert gone.
        LCOMPILERS_ASSERT(parent && ASR::is_a<ASR::Struct_t>(*parent))
        if( !parent || !ASR::is_a<ASR::Struct_t>(*parent) ||
            !is_byte_representable_struct(
                ASR::down_cast<ASR::Struct_t>(parent), visited) ) {
            return false;
        }
    }
    for( size_t i = 0; i < st->n_members; i++ ) {
        ASR::symbol_t* mem = st->m_symtab->get_symbol(st->m_members[i]);
        if( !mem || !ASR::is_a<ASR::Variable_t>(*mem) ) {
            return false;
        }
        ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(mem);
        if( is_allocatable_or_pointer(var->m_type) ) {
            return false;
        }
        ASR::ttype_t* element_type = type_get_past_array(var->m_type);
        if( !ASR::is_a<ASR::StructType_t>(*element_type) ) {
            continue;
        }
        if( !ASR::down_cast<ASR::StructType_t>(element_type)->m_is_cstruct ) {
            // A `class(...)` component.
            return false;
        }
        ASR::symbol_t* mem_struct = var->m_type_declaration ?
            symbol_get_past_external(var->m_type_declaration) : nullptr;
        if( !mem_struct || !ASR::is_a<ASR::Struct_t>(*mem_struct) ||
            !is_byte_representable_struct(
                ASR::down_cast<ASR::Struct_t>(mem_struct), visited) ) {
            return false;
        }
    }
    return true;
}

int64_t expr_corank(const ASR::expr_t* expr) {
    if (!expr) return 0;
    while (expr) {
        if (ASR::is_a<ASR::Var_t>(*expr)) {
            ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(ASR::down_cast<ASR::Var_t>(expr)->m_v);
            return symbol_corank(sym);
        } else if (ASR::is_a<ASR::StructInstanceMember_t>(*expr)) {
            ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(ASR::down_cast<ASR::StructInstanceMember_t>(expr)->m_m);
            return symbol_corank(sym);
        } else if (ASR::is_a<ASR::CoarrayRef_t>(*expr)) {
            const ASR::CoarrayRef_t* ref = ASR::down_cast<ASR::CoarrayRef_t>(expr);
            int64_t r = expr_corank(ref->m_var);
            if (r > 0) return r;
            return ref->n_coindices;
        } else if (ASR::is_a<ASR::ArrayItem_t>(*expr)) {
            expr = ASR::down_cast<ASR::ArrayItem_t>(expr)->m_v;
        } else if (ASR::is_a<ASR::ArraySection_t>(*expr)) {
            expr = ASR::down_cast<ASR::ArraySection_t>(expr)->m_v;
        } else if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*expr)) {
            expr = ASR::down_cast<ASR::ArrayPhysicalCast_t>(expr)->m_arg;
        } else if (ASR::is_a<ASR::Cast_t>(*expr)) {
            expr = ASR::down_cast<ASR::Cast_t>(expr)->m_arg;
        } else {
            break;
        }
    }
    return 0;
}

void import_struct_t(Allocator& al,
    const Location& loc, ASR::ttype_t*& var_type,
    ASR::intentType intent, SymbolTable* current_scope, ASR::expr_t* var) {
    bool is_pointer = ASRUtils::is_pointer(var_type);
    bool is_allocatable = ASRUtils::is_allocatable(var_type);
    bool is_array = ASRUtils::is_array(var_type);
    ASR::dimension_t* m_dims = nullptr;
    size_t n_dims = ASRUtils::extract_dimensions_from_ttype(var_type, m_dims);
    ASR::array_physical_typeType ptype = ASR::array_physical_typeType::DescriptorArray;
    if( is_array ) {
        ptype = ASRUtils::extract_physical_type(var_type);
    }
    ASR::ttype_t* var_type_unwrapped = ASRUtils::type_get_past_allocatable(
        ASRUtils::type_get_past_pointer(ASRUtils::type_get_past_array(var_type)));
    if( var && ASR::is_a<ASR::StructType_t>(*var_type_unwrapped) && !ASRUtils::is_class_type(var_type_unwrapped) ) {
        ASR::symbol_t* der_sym = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(var));
        if( (ASR::asr_t*) ASRUtils::get_asr_owner(der_sym) != current_scope->asr_owner ) {
            std::string sym_name = ASRUtils::symbol_name(ASRUtils::symbol_get_past_external(der_sym));
            if( current_scope->resolve_symbol(sym_name) == nullptr ) {
                std::string unique_name = current_scope->get_unique_name(sym_name);
                der_sym = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(
                    al, loc, current_scope, s2c(al, unique_name), ASRUtils::symbol_get_past_external(der_sym),
                    ASRUtils::symbol_name(ASRUtils::get_asr_owner(ASRUtils::symbol_get_past_external(der_sym))), nullptr, 0,
                    ASRUtils::symbol_name(ASRUtils::symbol_get_past_external(der_sym)), ASR::accessType::Public));
                current_scope->add_symbol(unique_name, der_sym);
            } else {
                ASR::symbol_t* resolved = current_scope->resolve_symbol(sym_name);
                ASR::symbol_t* resolved_underlying = ASRUtils::symbol_get_past_external(resolved);
                if (resolved_underlying && ASR::is_a<ASR::Struct_t>(*resolved_underlying)) {
                    der_sym = resolved;
                } else {
                    std::string unique_name = current_scope->get_unique_name(sym_name);
                    der_sym = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(
                        al, loc, current_scope, s2c(al, unique_name), ASRUtils::symbol_get_past_external(der_sym),
                        ASRUtils::symbol_name(ASRUtils::get_asr_owner(ASRUtils::symbol_get_past_external(der_sym))), nullptr, 0,
                        ASRUtils::symbol_name(ASRUtils::symbol_get_past_external(der_sym)), ASR::accessType::Public));
                    current_scope->add_symbol(unique_name, der_sym);
                }
            }
            var_type = ASRUtils::make_StructType_t_util(al, loc, der_sym, true);
            if( is_array ) {
                var_type = ASRUtils::make_Array_t_util(al, loc, var_type, m_dims, n_dims,
                    ASR::abiType::Source, false, ptype, true);
            }
            if( is_pointer ) {
                var_type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc, var_type));
            } else if( is_allocatable ) {
                var_type = ASRUtils::TYPE(ASRUtils::make_Allocatable_t_util(al, loc, var_type));
            }
        }
    } else if( ASR::is_a<ASR::String_t>(*var_type_unwrapped) ) {
        ASR::String_t* char_t = ASR::down_cast<ASR::String_t>(var_type_unwrapped);
        if( char_t->m_len_kind == ASR::string_length_kindType::AssumedLength &&
            intent == ASR::intentType::Local ) {
            var_type = ASRUtils::TYPE(ASR::make_String_t(al, loc, char_t->m_kind,
                ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 0,
                    ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)))),
                ASR::string_length_kindType::ExpressionLength,
                ASR::string_physical_typeType::DescriptorString));
            if( is_array ) {
                var_type = ASRUtils::make_Array_t_util(al, loc, var_type, m_dims, n_dims,
                    ASR::abiType::Source, false, ptype, true);
            }
            if( is_pointer ) {
                var_type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc, var_type));
            } else if( is_allocatable ) {
                var_type = ASRUtils::TYPE(ASRUtils::make_Allocatable_t_util(al, loc, var_type));
            }
        }
    }
}

ASR::symbol_t* import_struct_sym_as_external(Allocator& al,
    const Location& loc, ASR::expr_t* v_expr, SymbolTable* current_scope) {
    ASR::symbol_t* struct_sym = get_struct_sym_from_struct_expr(v_expr);
    if (struct_sym == nullptr) return nullptr;
    // ExternalSymbol.m_external must point at the original definition (e.g.
    // Struct), never another ExternalSymbol. Expressions like s%component can
    // yield a use-associated ExternalSymbol when the component type is only
    // reached transitively (not imported into the current scope).
    ASR::symbol_t* original_struct = symbol_get_past_external(struct_sym);
    if (original_struct == nullptr) return nullptr;
    std::string struct_name = symbol_name(original_struct);
    if (current_scope->resolve_symbol(struct_name) == nullptr) {
        struct_sym = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(
            al, loc, current_scope, s2c(al, struct_name), original_struct,
            ASRUtils::symbol_name(ASRUtils::get_asr_owner(original_struct)), nullptr, 0,
            s2c(al, struct_name), ASR::accessType::Public));
        current_scope->add_symbol(struct_name, struct_sym);
    } else {
        ASR::symbol_t* resolved = current_scope->resolve_symbol(struct_name);
        ASR::symbol_t* resolved_underlying = symbol_get_past_external(resolved);
        if (resolved_underlying && ASR::is_a<ASR::Struct_t>(*resolved_underlying)) {
            struct_sym = resolved;
        } else {
            std::string unique_name = current_scope->get_unique_name(struct_name);
            struct_sym = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(
                al, loc, current_scope, s2c(al, unique_name), original_struct,
                ASRUtils::symbol_name(ASRUtils::get_asr_owner(original_struct)), nullptr, 0,
                s2c(al, struct_name), ASR::accessType::Public));
            current_scope->add_symbol(unique_name, struct_sym);
        }
    }
    return struct_sym;
}

ASR::asr_t* make_ArrayPhysicalCast_t_util(Allocator &al, const Location &a_loc,
    ASR::expr_t* a_arg, ASR::array_physical_typeType a_old, ASR::array_physical_typeType a_new,
    ASR::ttype_t* a_type, ASR::expr_t* a_value) {
    if( ASR::is_a<ASR::ArrayPhysicalCast_t>(*a_arg) ) {
        ASR::ArrayPhysicalCast_t* a_arg_ = ASR::down_cast<ASR::ArrayPhysicalCast_t>(a_arg);
        a_arg = a_arg_->m_arg;
        a_old = ASRUtils::extract_physical_type(ASRUtils::expr_type(a_arg_->m_arg));
    }

    LCOMPILERS_ASSERT(ASRUtils::extract_physical_type(ASRUtils::expr_type(a_arg)) == a_old);
    // Only a descriptor can be allocatable or a pointer, so the result of a
    // cast to any other physical type is a plain array.
    if( a_new != ASR::array_physical_typeType::DescriptorArray &&
        a_new != ASR::array_physical_typeType::AssumedRankArray ) {
        a_type = ASRUtils::type_get_past_allocatable_pointer(a_type);
    }
    // TODO: Allow for DescriptorArray to DescriptorArray physical cast for allocatables
    // later on
    if( a_old == a_new ) {
        if( a_old != ASR::array_physical_typeType::DescriptorArray ) {
            return (ASR::asr_t*) a_arg;
        }
        if( (ASR::is_a<ASR::Allocatable_t>(*ASRUtils::expr_type(a_arg)) ||
             ASR::is_a<ASR::Pointer_t>(*ASRUtils::expr_type(a_arg))) &&
            ASRUtils::check_equal_type(ASRUtils::expr_type(a_arg), a_type, nullptr, nullptr) ) {
            // Keep cast when narrowing from class(*) to concrete type
            bool arg_is_poly = ASRUtils::is_unlimited_polymorphic_type(ASRUtils::expr_type(a_arg));
            bool type_is_poly = ASRUtils::is_unlimited_polymorphic_type(a_type);
            // Keep cast when dimension counts differ (e.g., assumed-rank target
            // with rank-15 descriptor vs a lower-rank source descriptor).
            int arg_n_dims = ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(a_arg));
            int type_n_dims = ASRUtils::extract_n_dims_from_ttype(a_type);
            if (arg_is_poly == type_is_poly && arg_n_dims == type_n_dims) {
                return (ASR::asr_t*) a_arg;
            }
        }
    }

    return ASR::make_ArrayPhysicalCast_t(al, a_loc, a_arg, a_old, a_new, a_type, a_value);
}

void set_ArrayConstant_value(ASR::ArrayConstant_t* x, ASR::expr_t* value, int64_t i) {
    value = ASRUtils::expr_value(value);

    ASR::ttype_t* type = ASRUtils::type_get_past_array(ASRUtils::type_get_past_allocatable(ASRUtils::expr_type(value)));
    int kind = ASRUtils::extract_kind_from_ttype_t(type);

    switch (type->type) {
        case ASR::ttypeType::Integer: {
            ASR::IntegerConstant_t* value_int = ASR::down_cast<ASR::IntegerConstant_t>(value);
            switch (kind) {
                case 1: ((int8_t*)x->m_data)[i] = checked_bit_pattern_cast<int8_t>(value_int->m_n); break;
                case 2: ((int16_t*)x->m_data)[i] = checked_bit_pattern_cast<int16_t>(value_int->m_n); break;
                case 4: ((int32_t*)x->m_data)[i] = checked_bit_pattern_cast<int32_t>(value_int->m_n); break;
                case 8: ((int64_t*)x->m_data)[i] = value_int->m_n; break;
                default:
                    throw LCompilersException("Unsupported kind for integer array constant.");
            }
            break;
        }
        case ASR::ttypeType::Real: {
            ASR::RealConstant_t* value_real = ASR::down_cast<ASR::RealConstant_t>(value);
            switch (kind) {
                case 4: ((float*)x->m_data)[i] = static_cast<float>(value_real->m_r); break;
                case 8: ((double*)x->m_data)[i] = value_real->m_r; break;
                default:
                    throw LCompilersException("Unsupported kind for real array constant.");
            }
            break;
        }
        case ASR::ttypeType::UnsignedInteger: {
            ASR::UnsignedIntegerConstant_t* value_int = ASR::down_cast<ASR::UnsignedIntegerConstant_t>(value);
            switch (kind) {
                case 1: ((uint8_t*)x->m_data)[i] = checked_bit_pattern_cast<uint8_t>(value_int->m_n); break;
                case 2: ((uint16_t*)x->m_data)[i] = checked_bit_pattern_cast<uint16_t>(value_int->m_n); break;
                case 4: ((uint32_t*)x->m_data)[i] = checked_bit_pattern_cast<uint32_t>(value_int->m_n); break;
                case 8: ((uint64_t*)x->m_data)[i] = value_int->m_n; break;
                default:
                    throw LCompilersException("Unsupported kind for unsigned integer array constant.");
            }
            break;
        }
        case ASR::ttypeType::Complex: {
            ASR::ComplexConstant_t* value_complex = ASR::down_cast<ASR::ComplexConstant_t>(value);
            switch (kind) {
                case 4:
                    ((float*)x->m_data)[i] = static_cast<float>(value_complex->m_re);
                    ((float*)x->m_data)[i+1] = static_cast<float>(value_complex->m_im); break;
                case 8:
                    ((double*)x->m_data)[i] = value_complex->m_re;
                    ((double*)x->m_data)[i+1] = value_complex->m_im; break;
                default:
                    throw LCompilersException("Unsupported kind for complex array constant.");
            }
            break;
        }
        case ASR::ttypeType::Logical: {
            ASR::LogicalConstant_t* value_logical = ASR::down_cast<ASR::LogicalConstant_t>(value);
            ((bool*)x->m_data)[i] = value_logical->m_value;
            break;
        }
        case ASR::ttypeType::String: {
            ASR::String_t* char_type = ASR::down_cast<ASR::String_t>(type);
            int len = 0;
            ASRUtils::extract_value(char_type->m_len, len);
            ASR::StringConstant_t* value_str = ASR::down_cast<ASR::StringConstant_t>(value);
            char* data = value_str->m_s;
            int src_len = len;
            ASR::String_t* src_type = ASR::down_cast<ASR::String_t>(
                extract_type(expr_type((ASR::expr_t*)value)));
            extract_value(src_type->m_len, src_len);
            int copy_len = std::min(len, src_len);
            for (int j = 0; j < len; j++) {
                *(((char*)x->m_data) + i*len + j) = (j < copy_len) ? data[j] : ' ';
            }
            break;
        }
        default:
            throw LCompilersException("Unsupported type for array constant.");
    }
}

std::string fetch_ArrayConstant_value(void *data, ASR::ttype_t* type, int64_t i) {
   int kind = ASRUtils::extract_kind_from_ttype_t(type);

    switch (type->type) {
        case ASR::ttypeType::Integer: {
            switch (kind) {
                case 1: return std::to_string(((int8_t*)data)[i]);
                case 2: return std::to_string(((int16_t*)data)[i]);
                case 4: return std::to_string(((int32_t*)data)[i]);
                case 8: return std::to_string(((int64_t*)data)[i]);
                default:
                    throw LCompilersException("Unsupported kind for integer array constant.");
            }
        }
        case ASR::ttypeType::Real: {
            switch (kind) {
                case 4: return to_string_with_precision(((float*)data)[i], 8);
                case 8: return to_string_with_precision(((double*)data)[i], 16);
                default:
                    throw LCompilersException("Unsupported kind for real array constant.");
            }
        }
        case ASR::ttypeType::UnsignedInteger: {
            switch (kind) {
                case 1: return std::to_string(((uint8_t*)data)[i]);
                case 2: return std::to_string(((uint16_t*)data)[i]);
                case 4: return std::to_string(((uint32_t*)data)[i]);
                case 8: return std::to_string(((uint64_t*)data)[i]);
                default:
                    throw LCompilersException("Unsupported kind for unsigned integer array constant.");
            }
        }
        case ASR::ttypeType::Complex: {
            switch (kind) {
                case 4: return "("+(to_string_with_precision(*(((float*)data) + 2*i), 8))+", "+ (to_string_with_precision(*(((float*)data) + 2*i + 1), 8)) + ")";
                case 8: return "("+(to_string_with_precision(*(((double*)data) + 2*i), 16))+"_8, "+ (to_string_with_precision(*(((double*)data) + 2*i + 1), 16)) + "_8)";
                default:
                    throw LCompilersException("Unsupported kind for complex array constant.");
            }
        }
        case ASR::ttypeType::Logical: {
            if (((bool*)data)[i] == 1) return ".true.";
            return ".false.";
        }
        case ASR::ttypeType::String: {
            ASR::String_t* char_type = ASR::down_cast<ASR::String_t>(type);
            int len = 0;
            ASRUtils::extract_value(char_type->m_len, len);
            char* data_char = (char*)data + i*len;
            // take first len characters
            char* new_char = new char[len + 1];
            for (int j = 0; j < len; j++) {
                new_char[j] = data_char[j];
            }
            new_char[len] = '\0';
            return '\"' + std::string(new_char) + '\"';
        }
        case ASR::ttypeType::StructType:
        case ASR::ttypeType::CPtr: {
            ASR::expr_t* value = ((ASR::expr_t**)data)[i];
            if (value == nullptr) {
                return "()";
            }
            switch (value->type) {
                case ASR::exprType::StructConstant: {
                    return "StructConstant";
                }
                case ASR::exprType::PointerNullConstant: {
                    return "PointerNullConstant";
                }
                default: {
                    return "Constant";
                }
            }
        }
        default:
            throw LCompilersException("Unsupported type for array constant.");
    }
}

ASR::expr_t* fetch_ArrayConstant_value_helper(Allocator &al, const Location& loc, void *data, ASR::ttype_t* type, int64_t i) {
    int kind = ASRUtils::extract_kind_from_ttype_t(type);
    ASR::expr_t* value = nullptr;
    switch (type->type) {
        case ASR::ttypeType::Integer : {
            switch (kind) {
                case 1: value = EXPR(ASR::make_IntegerConstant_t(al, loc,
                                    ((int8_t*)data)[i], type)); break;
                case 2: value = EXPR(ASR::make_IntegerConstant_t(al, loc,
                                    ((int16_t*)data)[i], type)); break;
                case 4: value = EXPR(ASR::make_IntegerConstant_t(al, loc,
                                    ((int32_t*)data)[i], type)); break;
                case 8: value = EXPR(ASR::make_IntegerConstant_t(al, loc,
                                    ((int64_t*)data)[i], type)); break;
                default:
                    throw LCompilersException("Unsupported kind for integer array constant.");
            }
            return value;
        }
        case ASR::ttypeType::Real: {
            switch (kind) {
                case 4: value = EXPR(ASR::make_RealConstant_t(al, loc,
                                    ((float*)data)[i], type)); break;
                case 8: value = EXPR(ASR::make_RealConstant_t(al, loc,
                                    ((double*)data)[i], type)); break;
                default:
                    throw LCompilersException("Unsupported kind for real array constant.");
            }
            return value;
        }
        case ASR::ttypeType::UnsignedInteger: {
            switch (kind) {
                case 1: value = EXPR(ASR::make_UnsignedIntegerConstant_t(al, loc,
                                    ((uint8_t*)data)[i], type)); break;
                case 2: value = EXPR(ASR::make_UnsignedIntegerConstant_t(al, loc,
                                    ((uint16_t*)data)[i], type)); break;
                case 4: value = EXPR(ASR::make_UnsignedIntegerConstant_t(al, loc,
                                    ((uint32_t*)data)[i], type)); break;
                case 8: value = EXPR(ASR::make_UnsignedIntegerConstant_t(al, loc,
                                    ((uint64_t*)data)[i], type)); break;
                default:
                    throw LCompilersException("Unsupported kind for unsigned integer array constant.");
            }
            return value;
        }
        case ASR::ttypeType::Complex: {
            switch (kind) {
                case 4: value = EXPR(ASR::make_ComplexConstant_t(al, loc,
                                    *(((float*)data) + 2*i), *(((float*)data) + 2*i + 1), type)); break;
                case 8: value = EXPR(ASR::make_ComplexConstant_t(al, loc,
                                    *(((double*)data) + 2*i), *(((double*)data) + 2*i + 1), type)); break;
                default:
                    throw LCompilersException("Unsupported kind for complex array constant.");
            }
            return value;
        }
        case ASR::ttypeType::Logical: {
            value = EXPR(ASR::make_LogicalConstant_t(al, loc,
                                ((bool*)data)[i], type));
            return value;
        }
        case ASR::ttypeType::String: {
            ASR::String_t* char_type = ASR::down_cast<ASR::String_t>(type);
            LCOMPILERS_ASSERT(ASRUtils::is_value_constant(char_type->m_len))
            int len {};
            ASRUtils::extract_value(char_type->m_len, len);
            char* data_char = (char*)data;
            std::string str = std::string(data_char + i*len, len);
            value = EXPR(ASR::make_StringConstant_t(al, loc,
                                s2c(al, str), type));
            return value;
        }
        case ASR::ttypeType::StructType:
        case ASR::ttypeType::CPtr: {
            value = ((ASR::expr_t**)data)[i];
            return value;
        }
        default:
            throw LCompilersException("Unsupported type for array constant.");
    }
}

ASR::expr_t* fold_ArrayItem_value(Allocator &al, ASR::expr_t* array,
        ASR::array_index_t* indices, size_t n_indices) {
    if (n_indices == 0) {
        return nullptr;
    }
    ASR::expr_t* base_value = ASRUtils::designator_constant_value(al, array);
    if (base_value == nullptr || !ASR::is_a<ASR::ArrayConstant_t>(*base_value)) {
        return nullptr;
    }
    ASR::ArrayConstant_t* array_constant =
        ASR::down_cast<ASR::ArrayConstant_t>(base_value);
    ASR::dimension_t* dims = nullptr;
    size_t n_dims = ASRUtils::extract_dimensions_from_ttype(
        ASRUtils::expr_type(array), dims);
    if (n_dims != n_indices) {
        return nullptr;
    }
    int64_t flat_index = 0;
    int64_t stride = 1;
    for (size_t i = 0; i < n_indices; i++) {
        if (indices[i].m_left != nullptr
                || indices[i].m_step != nullptr
                || indices[i].m_right == nullptr) {
            return nullptr;
        }
        ASR::expr_t* index_value =
            ASRUtils::expr_value(indices[i].m_right);
        if (index_value == nullptr ||
                !ASR::is_a<ASR::IntegerConstant_t>(*index_value)) {
            return nullptr;
        }
        int64_t lower_bound = 1;
        if (dims[i].m_start) {
            ASR::expr_t* lb_value =
                ASRUtils::expr_value(dims[i].m_start);
            if (lb_value &&
                    !ASRUtils::extract_value(lb_value, lower_bound)) {
                return nullptr;
            }
        }
        int64_t length = 0;
        if (dims[i].m_length == nullptr ||
                !ASRUtils::extract_value(dims[i].m_length, length)) {
            return nullptr;
        }
        int64_t index = ASR::down_cast<ASR::IntegerConstant_t>(
            index_value)->m_n - lower_bound;
        flat_index += index * stride;
        stride *= length;
    }
    if (flat_index < 0 || flat_index >=
            ASRUtils::get_fixed_size_of_array(array_constant->m_type)) {
        return nullptr;
    }
    return ASRUtils::fetch_ArrayConstant_value(al, array_constant,
        flat_index);
}

void* set_ArrayConstant_data(ASR::expr_t** a_args, size_t n_args, ASR::ttype_t* a_type) {
    int kind = ASRUtils::extract_kind_from_ttype_t(a_type);
    switch (a_type->type) {
        case ASR::ttypeType::Integer: {
            switch (kind) {
                case 1: return set_data_int(new int8_t[n_args], a_args, n_args);
                case 2: return set_data_int(new int16_t[n_args], a_args, n_args);
                case 4: return set_data_int(new int32_t[n_args], a_args, n_args);
                case 8: return set_data_int(new int64_t[n_args], a_args, n_args);
                default:
                    throw LCompilersException("Unsupported kind for integer array constant.");
            }
        }
        case ASR::ttypeType::Real: {
            switch (kind) {
                case 4: return set_data_real(new float[n_args], a_args, n_args);
                case 8: return set_data_real(new double[n_args], a_args, n_args);
                default:
                    throw LCompilersException("Unsupported kind for real array constant.");
            }
        }
        case ASR::ttypeType::UnsignedInteger: {
            switch (kind) {
                case 1: return set_data_uint(new uint8_t[n_args], a_args, n_args);
                case 2: return set_data_uint(new uint16_t[n_args], a_args, n_args);
                case 4: return set_data_uint(new uint32_t[n_args], a_args, n_args);
                case 8: return set_data_uint(new uint64_t[n_args], a_args, n_args);
                default:
                    throw LCompilersException("Unsupported kind for unsigned integer array constant.");
            }
        }
        case ASR::ttypeType::Complex: {
            switch (kind) {
                case 4: return set_data_complex(new float[2*n_args], a_args, n_args);
                case 8: return set_data_complex(new double[2*n_args], a_args, n_args);
                default:
                    throw LCompilersException("Unsupported kind for complex array constant.");
            }
        }
        case ASR::ttypeType::Logical: {
            bool* data = new bool[n_args];
            for (size_t i = 0; i < n_args; i++) {
                data[i] = ASR::down_cast<ASR::LogicalConstant_t>(ASRUtils::expr_value(a_args[i]))->m_value;
            }
            return (void*) data;
        }
        case ASR::ttypeType::String: {
            int len = -1;
            ASRUtils::extract_value(
                ASR::down_cast<ASR::String_t>(a_type)->m_len, len);
            char* data = new char[len*n_args + 1];
            for (size_t i = 0; i < n_args; i++) {
                ASR::StringConstant_t* sc = ASR::down_cast<ASR::StringConstant_t>(
                    ASRUtils::expr_value(a_args[i]));
                char* value = sc->m_s;
                int src_len = len;
                ASR::String_t* src_type = ASR::down_cast<ASR::String_t>(
                    extract_type(expr_type((ASR::expr_t*)sc)));
                extract_value(src_type->m_len, src_len);
                int copy_len = std::min(len, src_len);
                for (int j = 0; j < len; j++) {
                    data[i*len + j] = (j < copy_len) ? value[j] : ' ';
                }
            }
            data[len*n_args] = '\0';
            return (void*) data;
        }
        case ASR::ttypeType::StructType: {
            ASR::expr_t** data = new ASR::expr_t*[n_args];
            for (size_t i = 0; i < n_args; i++) {
                data[i] = ASRUtils::expr_value(a_args[i]);
                LCOMPILERS_ASSERT(ASR::is_a<ASR::StructConstant_t>(*data[i]));
            }
            return (void*) data;
        }
        case ASR::ttypeType::CPtr: {
            ASR::expr_t** data = new ASR::expr_t*[n_args];
            for (size_t i = 0; i < n_args; i++) {
                data[i] = ASRUtils::expr_value(a_args[i]);
            }
            return (void*) data;
        }
        default:
            throw LCompilersException("Unsupported type for array constant.");
    }
}

int64_t get_ArrayConstant_data_size(size_t n_args, ASR::ttype_t* a_type) {
    if (ASRUtils::is_character(*a_type)) {
        int len = 0;
        if(!ASRUtils::extract_value(ASR::down_cast<ASR::String_t>(a_type)->m_len, len)){LCOMPILERS_ASSERT(false);}
        return n_args * len;
    }
    if (ASR::is_a<ASR::StructType_t>(*a_type)) {
        return n_args * sizeof(ASR::expr_t*);
    }
    if (ASR::is_a<ASR::CPtr_t>(*a_type)) {
        return n_args * sizeof(void*);
    }
    // Logical data is stored as one `bool` per element, whatever the kind.
    if (ASR::is_a<ASR::Logical_t>(*a_type)) {
        return n_args * sizeof(bool);
    }
    return n_args * ASRUtils::extract_kind_from_ttype_t(a_type);
}

ASR::expr_t* adjust_character_length(ASR::expr_t* value, int64_t lhs_len, int64_t rhs_len, const Location& loc, Allocator& al) {
    value = ASRUtils::expr_value(value);
    ASR::StringConstant_t* string_constant = ASR::down_cast<ASR::StringConstant_t>(value);
    char* original_str = string_constant->m_s;
    size_t new_length = static_cast<size_t>(lhs_len);
    char* adjusted_str = al.allocate<char>(new_length + 1);

    if (lhs_len < rhs_len) { // trim
        std::memcpy(adjusted_str, original_str, new_length);
    } else { // pad
        std::memcpy(adjusted_str, original_str, rhs_len);
        std::memset(adjusted_str + rhs_len, ' ', new_length - rhs_len);
    }
    adjusted_str[new_length] = '\0'; // null-terminate the string

    ASR::ttype_t* value_type = ASRUtils::TYPE(ASR::make_String_t(
        al, loc, 1,
        ASRUtils::EXPR(ASR::make_IntegerConstant_t(
            al, loc, new_length, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)))),
            ASR::string_length_kindType::ExpressionLength,
            ASR::string_physical_typeType::DescriptorString));

    return ASRUtils::EXPR(ASR::make_StringConstant_t(
        al, loc, adjusted_str, value_type));
}

ASR::expr_t* adjust_array_character_length(ASR::expr_t* value, int64_t lhs_len, int64_t rhs_len, Allocator& al) {
    ASR::ArrayConstant_t* array_constant = ASR::down_cast<ASR::ArrayConstant_t>(ASRUtils::expr_value(value));
    Vec<ASR::expr_t*> body;
    size_t array_size = ASRUtils::get_fixed_size_of_array(array_constant->m_type);

    body.reserve(al, array_size);

    for (size_t i = 0; i < array_size; i++) {
        ASR::expr_t* item = ASRUtils::fetch_ArrayConstant_value(al, array_constant, i);

        if (ASR::is_a<ASR::ArrayConstant_t>(*item)) {
            item = adjust_array_character_length(item, lhs_len, rhs_len,  al);
        } else {
            item = adjust_character_length(item, lhs_len, rhs_len, item->base.loc, al);
        }

        body.push_back(al, item);
    }

    {
        ASR::String_t* str_t = ASRUtils::get_string_type(array_constant->m_type);
        LCOMPILERS_ASSERT(ASRUtils::is_value_constant(str_t->m_len))
        ASR::down_cast<ASR::IntegerConstant_t>(str_t->m_len)->m_n = lhs_len;
    }
    array_constant->m_data = ASRUtils::set_ArrayConstant_data(
            body.p, body.size(), ASRUtils::extract_type(array_constant->m_type));
    array_constant->m_n_data = ASRUtils::get_ArrayConstant_data_size(array_constant->m_type);

    return value;
}

ASR::expr_t* conform_character_constant_length(ASR::expr_t* value,
        int64_t len, const Location& loc, Allocator& al) {
    ASR::String_t* value_str = ASRUtils::get_string_type(ASRUtils::expr_type(value));
    int64_t value_len = 0;
    if (value_str == nullptr || value_str->m_len == nullptr
            || !ASRUtils::extract_value(ASRUtils::expr_value(value_str->m_len), value_len)
            || value_len == len) {
        return nullptr;
    }
    if (ASR::is_a<ASR::StringConstant_t>(*value)) {
        return adjust_character_length(value, len, value_len, loc, al);
    }
    if (!ASR::is_a<ASR::ArrayConstant_t>(*value)) {
        return nullptr;
    }
    ASR::ArrayConstant_t* array = ASR::down_cast<ASR::ArrayConstant_t>(value);
    if (ASRUtils::get_fixed_size_of_array(array->m_type) < 0
            || !ASR::is_a<ASR::Array_t>(*array->m_type)) {
        return nullptr;
    }
    // `adjust_array_character_length` rewrites the constant and the
    // length in its type in place, and `value` may be a named constant's
    // value, so adjust a copy with its own length.
    ASR::ttype_t* adjusted_type = ASRUtils::duplicate_type(al, array->m_type);
    ASRUtils::get_string_type(adjusted_type)->m_len = ASRUtils::EXPR(
        ASR::make_IntegerConstant_t(al, loc, value_len,
            ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4))));
    ASR::expr_t* adjusted = ASRUtils::EXPR(ASRUtils::make_ArrayConstant_t_util(al, loc,
        array->m_data, adjusted_type, array->m_storage_format));
    return adjust_array_character_length(adjusted, len, value_len, al);
}

void flatten_ArrayConstant_data(Allocator &al, Vec<ASR::expr_t*> &data, ASR::expr_t** a_args, size_t n_args, ASR::ttype_t* a_type, int &curr_idx, ASR::ArrayConstant_t* x) {
    if (x != nullptr) {
        // this is array constant, we have it's data available
        void* x_data = x->m_data;
        for (size_t i = 0; i < (size_t) ASRUtils::get_fixed_size_of_array(x->m_type); i++) {
            ASR::expr_t* value = fetch_ArrayConstant_value_helper(al, x->base.base.loc, x_data, a_type, i);
            if (ASR::is_a<ASR::ArrayConstant_t>(*value)) {
                ASR::ArrayConstant_t* value_ = ASR::down_cast<ASR::ArrayConstant_t>(value);
                flatten_ArrayConstant_data(al, data, a_args, n_args, a_type, curr_idx, value_);
            } else {
                data.push_back(al, value);
                curr_idx++;
            }
        }
        return;
    }
    for (size_t i = 0; i < n_args; i++) {
        ASR::expr_t* a_value = ASRUtils::expr_value(a_args[i]);
        if (ASR::is_a<ASR::ArrayConstant_t>(*a_value)) {
            ASR::ArrayConstant_t* a_value_ = ASR::down_cast<ASR::ArrayConstant_t>(a_value);
            flatten_ArrayConstant_data(al, data, a_args, n_args, a_type, curr_idx, a_value_);
        } else {
            data.push_back(al, a_value);
            curr_idx++;
        }
    }
}

ASR::asr_t* make_ArrayConstructor_t_util(Allocator &al, const Location &a_loc,
    ASR::expr_t** a_args, size_t n_args, ASR::ttype_t* a_type, ASR::arraystorageType a_storage_format,
    ASR::expr_t* a_struct_var) {
    if( !ASRUtils::is_array(a_type) ) {
        Vec<ASR::dimension_t> dims;
        dims.reserve(al, 1);
        ASR::dimension_t dim;
        dim.loc = a_loc;
        dim.m_length = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
            al, a_loc, n_args, ASRUtils::TYPE(ASR::make_Integer_t(al, a_loc, 4))));
        dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
            al, a_loc, 1, ASRUtils::TYPE(ASR::make_Integer_t(al, a_loc, 4))));
        dims.push_back(al, dim);
        a_type = ASRUtils::make_Array_t_util(al, dim.loc,
            a_type, dims.p, dims.size(), ASR::abiType::Source,
            false, ASR::array_physical_typeType::PointerArray, true);
    } else if( ASR::is_a<ASR::Allocatable_t>(*a_type) ) {
        ASR::dimension_t* m_dims = nullptr;
        size_t n_dims = ASRUtils::extract_dimensions_from_ttype(a_type, m_dims);
        if( !ASRUtils::is_dimension_empty(m_dims, n_dims) ) {
            a_type = ASRUtils::duplicate_type_with_empty_dims(al, a_type);
        }
    }

    LCOMPILERS_ASSERT(ASRUtils::is_array(a_type));
    bool all_expr_evaluated = true;
    // Compile-time aggregation of real(16) arrays into a packed ArrayConstant
    // is not supported: set_ArrayConstant_data / fetch_ArrayConstant_value only
    // pack float/double element buffers, whereas a kind=16 RealConstant stores
    // its binary128 value behind a pointer in m_r (see real_constant_get_r16_bytes
    // in asr_utils.h). Let the array initialise at runtime instead — emits an
    // ArrayConstructor that lowers element-by-element via fp128 store.
    {
        ASR::ttype_t* elt_type = ASRUtils::type_get_past_pointer(
            ASRUtils::type_get_past_allocatable(a_type));
        if (ASRUtils::is_array(elt_type)) {
            elt_type = ASR::down_cast<ASR::Array_t>(elt_type)->m_type;
        }
        if (ASRUtils::is_real(*elt_type) &&
            ASRUtils::extract_kind_from_ttype_t(elt_type) == 16) {
            all_expr_evaluated = false;
        }
    }
    bool is_array_item_constant = n_args > 0 && (ASR::is_a<ASR::IntegerConstant_t>(*a_args[0]) ||
                                ASR::is_a<ASR::UnsignedIntegerConstant_t>(*a_args[0]) ||
                                ASR::is_a<ASR::RealConstant_t>(*a_args[0]) ||
                                ASR::is_a<ASR::ComplexConstant_t>(*a_args[0]) ||
                                ASR::is_a<ASR::LogicalConstant_t>(*a_args[0]) ||
                                ASR::is_a<ASR::StringConstant_t>(*a_args[0]) ||
                                ASR::is_a<ASR::StructConstant_t>(*a_args[0]) ||
                                ASR::is_a<ASR::IntegerUnaryMinus_t>(*a_args[0]) ||
                                ASR::is_a<ASR::RealUnaryMinus_t>(*a_args[0]));
    if( n_args > 0 ) {
        is_array_item_constant = is_array_item_constant || ASR::is_a<ASR::ArrayConstant_t>(*a_args[0]);
    }
    ASR::expr_t* value = nullptr;
    for (size_t i = 0; i < n_args; i++) {
        ASR::expr_t* a_value = ASRUtils::expr_value(a_args[i]);
        if (!is_value_constant(a_value)) {
            all_expr_evaluated = false;
        }
    }
    if (all_expr_evaluated && n_args == 0) {
        ASR::ttype_t* array_type = ASRUtils::type_get_past_pointer(a_type);
        ASR::Array_t* array = ASR::down_cast<ASR::Array_t>(array_type);
        if (is_character(*array->m_type)) {
            int len = 0;
            if (!ASRUtils::extract_value(ASR::down_cast<ASR::String_t>(array->m_type)->m_len, len)) {
                all_expr_evaluated = false;
            }
        }
    }
    if (all_expr_evaluated) {
        Vec<ASR::expr_t*> a_args_values; a_args_values.reserve(al, n_args);
        int curr_idx = 0;
        a_type = ASRUtils::type_get_past_pointer(a_type);
        ASR::Array_t* a_type_ = ASR::down_cast<ASR::Array_t>(a_type);
        flatten_ArrayConstant_data(al, a_args_values, a_args, n_args, a_type_->m_type, curr_idx, nullptr);
        Vec<ASR::dimension_t> dims; dims.reserve(al, 1);
        ASR::dimension_t dim; dim.loc = a_type_->m_dims[0].loc; dim.m_start = a_type_->m_dims[0].m_start;
        dim.m_length = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, a_type_->m_dims[0].loc,
                        curr_idx,
                        ASRUtils::TYPE(ASR::make_Integer_t(al, a_loc, 4))));
        dims.push_back(al, dim);
        ASR::ttype_t* new_type = ASRUtils::TYPE(ASR::make_Array_t(al, a_type->base.loc, a_type_->m_type,
            dims.p, dims.n, a_type_->m_physical_type, a_type_->m_memory_space));
        void *data = set_ArrayConstant_data(a_args_values.p, curr_idx, a_type_->m_type);
        value = ASRUtils::EXPR(ASRUtils::make_ArrayConstant_t_util(
            al, a_loc, data, new_type, a_storage_format));
    }

    if (is_array_item_constant && all_expr_evaluated) {
        return (ASR::asr_t*) value;
    }

    ASR::asr_t* arr_ctor_asr = ASR::make_ArrayConstructor_t(
        al, a_loc, a_args, n_args, a_type, value, a_storage_format, a_struct_var);
    ASR::ArrayConstructor_t* arr_ctor = ASR::down_cast<ASR::ArrayConstructor_t>(
        ASRUtils::EXPR(arr_ctor_asr));
    int64_t ctor_size_val = 0;
    bool size_known = false;
    bool all_scalars = true;
    bool can_compute_size = true;
    for (size_t i = 0; i < n_args; i++) {
        ASR::expr_t* arg = a_args[i];
        if (ASR::is_a<ASR::ImpliedDoLoop_t>(*arg) ||
            ASR::is_a<ASR::ArrayConstructor_t>(*arg) ||
            ASR::is_a<ASR::ArrayConstant_t>(*arg)) {
            all_scalars = false;
        }
        if (ASRUtils::is_array(ASRUtils::expr_type(arg))) {
            all_scalars = false;
            if (!ASRUtils::is_fixed_size_array(ASRUtils::expr_type(arg))) {
                can_compute_size = false;
                break;
            }
        }
    }
    if (all_scalars) {
        ctor_size_val = static_cast<int64_t>(n_args);
        size_known = true;
    } else if (can_compute_size) {
        ASR::expr_t* ctor_size_expr = ASRUtils::get_ArrayConstructor_size(al, arr_ctor);
        if (ASRUtils::extract_value(ASRUtils::expr_value(ctor_size_expr), ctor_size_val)) {
            size_known = true;
        }
    }
    if (size_known && ctor_size_val > 0) {
        ASR::ttype_t* ctor_type = ASRUtils::type_get_past_pointer(arr_ctor->m_type);
        if (ASR::is_a<ASR::Array_t>(*ctor_type)) {
            ASR::Array_t* ctor_arr = ASR::down_cast<ASR::Array_t>(ctor_type);
            if (ctor_arr->n_dims == 1) {
                int64_t dim_len = 0;
                bool has_len = ctor_arr->m_dims[0].m_length != nullptr &&
                    ASRUtils::extract_value(ASRUtils::expr_value(ctor_arr->m_dims[0].m_length), dim_len);
                if (!has_len) {
                    Vec<ASR::dimension_t> dims;
                    dims.reserve(al, 1);
                    ASR::dimension_t dim = ctor_arr->m_dims[0];
                    ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, a_loc, 4));
                    if (dim.m_start == nullptr) {
                        dim.m_start = ASRUtils::EXPR(
                            ASR::make_IntegerConstant_t(al, a_loc, 1, int_type));
                    }
                    dim.m_length = ASRUtils::EXPR(
                        ASR::make_IntegerConstant_t(al, a_loc, ctor_size_val, int_type));
                    dims.push_back(al, dim);
                    arr_ctor->m_type = ASRUtils::TYPE(ASR::make_Array_t(
                        al, a_loc, ctor_arr->m_type, dims.p, dims.size(),
                        ctor_arr->m_physical_type, ctor_arr->m_memory_space));
                }
            }
        }
    }

    return arr_ctor_asr;
}

ASR::expr_t* broadcast_scalar_constant_to_array(Allocator& al,
        const Location& loc, ASR::expr_t* value, ASR::ttype_t* array_type) {
    int64_t size = ASRUtils::get_fixed_size_of_array(array_type);
    if (size < 0) {
        return nullptr;
    }
    array_type = ASRUtils::duplicate_type(al, array_type);
    if (size == 0) {
        return ASRUtils::EXPR(ASRUtils::make_ArrayConstant_t_util(al, loc,
            nullptr, array_type, ASR::arraystorageType::ColMajor));
    }
    Vec<ASR::expr_t*> elements;
    elements.reserve(al, size);
    for (int64_t j = 0; j < size; j++) {
        elements.push_back(al, value);
    }
    ASR::expr_t* broadcast = ASRUtils::expr_value(ASRUtils::EXPR(
        ASRUtils::make_ArrayConstructor_t_util(al, loc, elements.p,
            elements.n, array_type, ASR::arraystorageType::ColMajor)));
    if (broadcast == nullptr || !ASR::is_a<ASR::ArrayConstant_t>(*broadcast)) {
        return nullptr;
    }
    // The elements are stored in column-major order; keep the rank and
    // bounds of `array_type`.
    ASR::down_cast<ASR::ArrayConstant_t>(broadcast)->m_type = array_type;
    return broadcast;
}

ASR::asr_t* make_print_t_util(Allocator& al, const Location& loc,
    ASR::expr_t** a_args, size_t n_args){
    LCOMPILERS_ASSERT(n_args > 0);
    if(n_args == 1 && ASRUtils::is_character(*ASRUtils::expr_type(a_args[0]))){
        return ASR::make_Print_t(al, loc, a_args[0]);
    } else {
        ASR::ttype_t *char_type = ASRUtils::TYPE(
            ASR::make_Allocatable_t(al, loc, ASRUtils::TYPE(ASR::make_String_t(
                al, loc, 1, nullptr,
                ASR::string_length_kindType::DeferredLength,
                ASR::string_physical_typeType::DescriptorString))));
        return ASR::make_Print_t(al, loc,
            ASRUtils::EXPR(ASR::make_StringFormat_t(al, loc, nullptr, a_args,n_args,
            ASR::string_format_kindType::FormatFortran, nullptr, 0, nullptr,
            char_type, nullptr)));
    }
}

void Call_t_body(Allocator& al, ASR::symbol_t* a_name,
    ASR::call_arg_t* a_args, size_t n_args, ASR::expr_t* a_dt, ASR::stmt_t** cast_stmt,
    bool implicit_argument_casting, SymbolTable* current_scope, std::optional<std::reference_wrapper<SetChar>> current_function_dependencies) {
    ASR::symbol_t* a_name_ = ASRUtils::symbol_get_past_external(a_name);
    ASR::FunctionType_t* func_type = get_FunctionType(a_name);
    // Skip arg processing for implicit interfaces (no declared parameter types)
    if (func_type->n_arg_types == 0) {
        return;
    }
    ASR::Function_t* func = ASRUtils::get_function(a_name);

    for( size_t i = 0; i < n_args; i++ ) {
        if( i >= func_type->n_arg_types ) {
            break;
        }
        if( a_args[i].m_value == nullptr ) {
            continue;
        }
        ASR::expr_t* arg = a_args[i].m_value;
        [[maybe_unused]] ASR::expr_t* orig_arg = func->m_args[i];
        ASR::ttype_t* arg_type = ASRUtils::type_get_past_allocatable(
            ASRUtils::type_get_past_pointer(ASRUtils::expr_type(arg)));
        ASR::ttype_t* orig_arg_type = ASRUtils::type_get_past_allocatable(
            ASRUtils::type_get_past_pointer(func_type->m_arg_types[i]));


        if (ASR::is_a<ASR::FunctionType_t>(*arg_type) && ASR::is_a<ASR::FunctionType_t>(*orig_arg_type)) {
            // Implicit interface reconciliation is handled at the LLVM codegen level.
            // No ASR-level cast is needed for procedure arguments.
            continue;
        }
        // cast string source based on the dest
        if( ASRUtils::is_string_only(orig_arg_type) &&
            ASRUtils::is_string_only(arg_type) &&
            !is_character_phsyical_types_matched(orig_arg_type, arg_type)){
            arg = a_args[i].m_value = 
                create_string_physical_cast(al, arg, 
                    get_string_type(orig_arg_type)->m_physical_type,
                    is_allocatable(func_type->m_arg_types[i]));
        }

        if( func_type->m_abi != ASR::abiType::BindC &&
            ASRUtils::is_string_only(orig_arg_type) &&
            ASRUtils::is_string_only(arg_type) ) {
            ASR::String_t* arg_str_type = get_string_type(arg_type);
            ASR::String_t* orig_arg_str_type = get_string_type(orig_arg_type);
            
            if (arg_str_type->m_len_kind == ASR::string_length_kindType::ExpressionLength &&
                orig_arg_str_type->m_len_kind == ASR::string_length_kindType::ExpressionLength &&
                arg_str_type->m_len && orig_arg_str_type->m_len &&
                ASRUtils::is_value_constant(arg_str_type->m_len) &&
                ASRUtils::is_value_constant(orig_arg_str_type->m_len)) {
                
                int64_t arg_len = ASRUtils::extract_dim_value_int(arg_str_type->m_len);
                int64_t orig_arg_len = ASRUtils::extract_dim_value_int(orig_arg_str_type->m_len);
                
                if (arg_len > orig_arg_len) {
                    ASR::expr_t* one = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                        al, arg->base.loc, 1, ASRUtils::TYPE(ASR::make_Integer_t(al, arg->base.loc, 4))));
                    ASR::expr_t* end = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                        al, arg->base.loc, orig_arg_len, ASRUtils::TYPE(ASR::make_Integer_t(al, arg->base.loc, 4))));
                    
                    ASR::ttype_t* section_type = ASRUtils::TYPE(ASR::make_String_t(
                        al, arg->base.loc, orig_arg_str_type->m_kind,
                        orig_arg_str_type->m_len, orig_arg_str_type->m_len_kind,
                        arg_str_type->m_physical_type));
                    
                    arg = a_args[i].m_value = ASRUtils::EXPR(ASR::make_StringSection_t(
                        al, arg->base.loc, arg, one, end, one, section_type, nullptr));
                }
            } else if (orig_arg_str_type->m_len_kind == ASR::string_length_kindType::ExpressionLength &&
                     orig_arg_str_type->m_len &&
                     ASRUtils::is_value_constant(orig_arg_str_type->m_len)) {
                int64_t orig_arg_len = ASRUtils::extract_dim_value_int(orig_arg_str_type->m_len);
                
                ASR::expr_t* one = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                    al, arg->base.loc, 1, ASRUtils::TYPE(ASR::make_Integer_t(al, arg->base.loc, 4))));
                ASR::expr_t* end = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                    al, arg->base.loc, orig_arg_len, ASRUtils::TYPE(ASR::make_Integer_t(al, arg->base.loc, 4))));
                
                ASR::ttype_t* section_type = ASRUtils::TYPE(ASR::make_String_t(
                    al, arg->base.loc, orig_arg_str_type->m_kind,
                    orig_arg_str_type->m_len, orig_arg_str_type->m_len_kind,
                    arg_str_type->m_physical_type));
                
                arg = a_args[i].m_value = ASRUtils::EXPR(ASR::make_StringSection_t(
                    al, arg->base.loc, arg, one, end, one, section_type, nullptr));
            }
        }
        if( !ASRUtils::is_intrinsic_symbol(a_name_) &&
            !(ASRUtils::is_class_type(ASRUtils::type_get_past_array(arg_type)) ||
              ASRUtils::is_class_type(ASRUtils::type_get_past_array(orig_arg_type))) &&
            !(ASR::is_a<ASR::FunctionType_t>(*ASRUtils::type_get_past_array(arg_type)) ||
              ASR::is_a<ASR::FunctionType_t>(*ASRUtils::type_get_past_array(orig_arg_type))) &&
            a_dt == nullptr ) {
                // Pass actual argument expression to `check_equal_type()` if the expression is
                // `ASR::FunctionParam_t`.
                ASR::expr_t* arg_expr = arg;
                ASR::expr_t* orig_arg_expr = func->m_args[i];
                if (ASR::is_a<ASR::FunctionParam_t>(*arg_expr)) {
                    ASR::FunctionParam_t* func_param = ASR::down_cast<ASR::FunctionParam_t>(arg_expr);
                    arg_expr = func->m_args[func_param->m_param_number];
                } 
                if (ASR::is_a<ASR::FunctionParam_t>(*orig_arg_expr)) {
                    ASR::FunctionParam_t* func_param = ASR::down_cast<ASR::FunctionParam_t>(orig_arg_expr);
                    orig_arg_expr = func->m_args[func_param->m_param_number];
                }
            if (implicit_argument_casting && !ASRUtils::check_equal_type(arg_type, orig_arg_type, arg_expr, orig_arg_expr)) {
                // Handle scalar integer kind mismatch (e.g., integer(8) to integer(4))
                if (ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_array(arg_type)) &&
                    ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_array(orig_arg_type)) &&
                    !ASRUtils::is_array(arg_type) && !ASRUtils::is_array(orig_arg_type)) {
                    int arg_kind = ASRUtils::extract_kind_from_ttype_t(arg_type);
                    int orig_kind = ASRUtils::extract_kind_from_ttype_t(orig_arg_type);
                    if (arg_kind != orig_kind) {
                        // Create IntegerToInteger cast to convert argument to expected kind
                        a_args[i].m_value = ASRUtils::EXPR(ASR::make_Cast_t(
                            al, arg->base.loc, arg,
                            ASR::cast_kindType::IntegerToInteger,
                            orig_arg_type, nullptr, nullptr));
                        continue;
                    }
                }
                if (ASR::is_a<ASR::Function_t>(*a_name_)) {
                    // get current_scope
                    SymbolTable* current_scope = nullptr;
                    std::string sym_name = "";
                    if (ASR::is_a<ASR::Var_t>(*arg)) {
                        ASR::Var_t* arg_var = ASR::down_cast<ASR::Var_t>(arg);
                        if (ASR::is_a<ASR::Variable_t>(*(arg_var->m_v))) {
                            ASR::Variable_t* arg_var_ = ASR::down_cast<ASR::Variable_t>(arg_var->m_v);
                            current_scope = arg_var_->m_parent_symtab;
                            sym_name = arg_var_->m_name;
                        }
                    } else if (ASR::is_a<ASR::ArrayItem_t>(*arg)) {
                        ASR::expr_t* arg_expr = ASR::down_cast<ASR::ArrayItem_t>(arg)->m_v;
                        ASR::Variable_t* arg_var = ASRUtils::EXPR2VAR(arg_expr);
                        current_scope = arg_var->m_parent_symtab;
                        sym_name = arg_var->m_name;
                    }
                    if (current_scope) {
                        ASR::Array_t* orig_arg_array_t = nullptr;
                        ASR::Array_t* arg_array_t = nullptr;
                        if (orig_arg_type->type == ASR::ttypeType::Array) {
                            orig_arg_array_t = ASR::down_cast<ASR::Array_t>(orig_arg_type);
                            Vec<ASR::dimension_t> dim;
                            dim.reserve(al, 1);
                            ASR::dimension_t dim_;
                            dim_.m_start = nullptr;
                            dim_.m_length = nullptr;
                            dim_.loc = arg->base.loc;
                            dim.push_back(al, dim_);
                            arg_array_t = (ASR::Array_t*) ASR::make_Array_t(al, arg->base.loc, orig_arg_array_t->m_type,
                            dim.p, dim.size(), ASR::array_physical_typeType::DescriptorArray, ASR::memory_spaceType::Global);
                        }
                        ASR::ttype_t* arg_array_type = (ASR::ttype_t*) arg_array_t;
                        ASR::ttype_t* pointer_type = ASRUtils::TYPE(ASR::make_Pointer_t(al, orig_arg_type->base.loc, arg_array_type));

                        std::string cast_sym_name = current_scope->get_unique_name(sym_name + "_cast", false);
                        ASR::asr_t* cast_ = ASRUtils::make_Variable_t_util(al, arg->base.loc,
                                        current_scope, s2c(al, cast_sym_name), nullptr,
                                        0, ASR::intentType::Local, nullptr, nullptr,
                                        ASR::storage_typeType::Default, pointer_type, nullptr,
                                        ASR::abiType::Source, ASR::accessType::Public,
                                        ASR::presenceType::Required, false,
                                        false, false, nullptr, false, false,
                                        ASR::pass_attrType::NotMethod, nullptr);

                        ASR::symbol_t* cast_sym = ASR::down_cast<ASR::symbol_t>(cast_);
                        current_scope->add_symbol(cast_sym_name, cast_sym);

                        ASR::expr_t* cast_expr = ASRUtils::EXPR(ASR::make_Var_t(al,arg->base.loc, cast_sym));

                        ASR::ttype_t* pointer_type_ = ASRUtils::TYPE(ASR::make_Pointer_t(al, arg->base.loc, ASRUtils::type_get_past_array(arg_type)));

                        ASR::asr_t* get_pointer = ASR::make_GetPointer_t(al, arg->base.loc, arg, pointer_type_, nullptr);

                        ASR::ttype_t* cptr = ASRUtils::TYPE(ASR::make_CPtr_t(al, arg->base.loc, ASR::cptr_kindType::CPtrUnspecified));

                        ASR::asr_t* pointer_to_cptr = ASR::make_PointerToCPtr_t(al, arg->base.loc, ASRUtils::EXPR(get_pointer), cptr, nullptr);

                        Vec<ASR::expr_t*> args_;
                        args_.reserve(al, 1);

                        ASR::ttype_t *int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, arg->base.loc, 4));
                        ASR::expr_t* thousand = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, arg->base.loc, 1000, int32_type));
                        ASR::expr_t* one = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, arg->base.loc, 1, int32_type));

                        args_.push_back(al, thousand);

                        Vec<ASR::dimension_t> dim;
                        dim.reserve(al, 1);
                        ASR::dimension_t dim_;
                        dim_.m_start = one;
                        dim_.m_length = one;
                        dim_.loc = arg->base.loc;
                        dim.push_back(al, dim_);

                        ASR::ttype_t* array_type = ASRUtils::TYPE(ASR::make_Array_t(al, arg->base.loc, int32_type, dim.p, dim.size(), ASR::array_physical_typeType::FixedSizeArray, ASR::memory_spaceType::Global));
                        ASR::asr_t* array_constant = ASRUtils::make_ArrayConstructor_t_util(al, arg->base.loc, args_.p, args_.size(), array_type, ASR::arraystorageType::ColMajor);

                        ASR::asr_t* cptr_to_pointer = ASR::make_CPtrToPointer_t(al, arg->base.loc, ASRUtils::EXPR(pointer_to_cptr), cast_expr, ASRUtils::EXPR(array_constant), nullptr);
                        *cast_stmt = ASRUtils::STMT(cptr_to_pointer);

                        ASR::asr_t* array_t = nullptr;

                        Vec<ASR::dimension_t> dims;
                        dims.reserve(al, 1);
                        ASR::dimension_t dims_;
                        dims_.m_start = nullptr;
                        dims_.m_length = nullptr;
                        dims_.loc = arg->base.loc;
                        dims.push_back(al, dims_);

                        array_t = ASR::make_Array_t(al, arg->base.loc, orig_arg_array_t->m_type,
                        dims.p, dims.size(), ASR::array_physical_typeType::PointerArray, ASR::memory_spaceType::Global);
                        ASR::asr_t* array_physical_cast = ASR::make_ArrayPhysicalCast_t(al, arg->base.loc, cast_expr, ASR::array_physical_typeType::DescriptorArray,
                                                        ASR::array_physical_typeType::PointerArray, ASRUtils::TYPE(array_t), nullptr);

                        a_args[i].m_value = ASRUtils::EXPR(array_physical_cast);
                    }
                }
            } else {
                // TODO: Make this a regular error. The current asr_utils.h is
                // not setup to return errors, so we need to refactor things.
                // For now we just do an assert.
                /*TODO: Remove this if check once intrinsic procedures are implemented correctly*/
                LCOMPILERS_ASSERT_MSG( ASRUtils::check_equal_type(arg_type, orig_arg_type, arg_expr, orig_arg_expr),
                    "ASRUtils::check_equal_type(" + ASRUtils::get_type_code(arg_type) + ", " +
                        ASRUtils::get_type_code(orig_arg_type) + ")");
            }
        }
        if( ASRUtils::is_array(arg_type) && ASRUtils::is_array(orig_arg_type) ) {
            ASR::Array_t* arg_array_t = ASR::down_cast<ASR::Array_t>(
                ASRUtils::type_get_past_pointer(arg_type));
            ASR::Array_t* orig_arg_array_t = ASR::down_cast<ASR::Array_t>(
                ASRUtils::type_get_past_pointer(orig_arg_type));
            bool is_orig_assumed_rank = false;
            if (orig_arg_array_t->m_physical_type == ASR::array_physical_typeType::AssumedRankArray) {
                is_orig_assumed_rank = true;
            }
            if( (arg_array_t->m_physical_type != orig_arg_array_t->m_physical_type) ||
                (arg_array_t->m_physical_type == ASR::array_physical_typeType::DescriptorArray &&
                 arg_array_t->m_physical_type == orig_arg_array_t->m_physical_type &&
                 !ASRUtils::is_intrinsic_symbol(a_name_)) ) {
                ASR::call_arg_t physical_cast_arg;
                physical_cast_arg.loc = arg->base.loc;
                Vec<ASR::dimension_t>* dimensions = nullptr;
                Vec<ASR::dimension_t> dimension_;
                dimension_.from_pointer_n_copy(al, orig_arg_array_t->m_dims, orig_arg_array_t->n_dims);
                ASR::ttype_t* physical_cast_type = ASRUtils::type_get_past_allocatable(ASRUtils::expr_type(arg));
                // An assumed-rank dummy receives a descriptor of the actual
                // argument's rank. The dummy's own type is shared by every
                // call, so the target physical type is kept separately.
                ASR::array_physical_typeType cast_physical_type = orig_arg_array_t->m_physical_type;

                if (ASRUtils::is_pointer(physical_cast_type) &&
                    orig_arg_array_t->m_physical_type == ASR::array_physical_typeType::AssumedRankArray) {
                    // Use the actual argument's dimensions for the descriptor.
                    // Assumed-rank descriptors are passed by pointer, so the
                    // callee reads the rank field — no need for 15-slot descriptors.
                    dimension_.from_pointer_n_copy(al, arg_array_t->m_dims, arg_array_t->n_dims);
                    dimensions = &dimension_;
                    cast_physical_type = ASR::array_physical_typeType::DescriptorArray;
                } else if (ASRUtils::is_pointer(physical_cast_type) &&
                           orig_arg_array_t->m_physical_type != ASR::array_physical_typeType::FixedSizeArray) {
                    dimensions = nullptr;
                } else if (ASRUtils::is_fixed_size_array(orig_arg_array_t->m_dims, orig_arg_array_t->n_dims)) {
                    dimensions = &dimension_;
                } else if (orig_arg_array_t->m_physical_type == ASR::array_physical_typeType::AssumedRankArray) {
                    // Use the actual argument's dimensions for the descriptor.
                    // Assumed-rank descriptors are passed by pointer, so the
                    // callee reads the rank field — no need for 15-slot descriptors.
                    dimension_.from_pointer_n_copy(al, arg_array_t->m_dims, arg_array_t->n_dims);
                    dimensions = &dimension_;
                    cast_physical_type = ASR::array_physical_typeType::DescriptorArray;
                } else if (arg_array_t->m_physical_type == ASR::array_physical_typeType::AssumedRankArray) {
                    // Assumed-rank actual argument (0 compile-time dims) passed
                    // to a known-rank formal parameter (e.g. inside select rank).
                    // Use the formal parameter's dimensions so the cast target
                    // type is a proper Array, not a bare element type.
                    dimensions = &dimension_;
                } else if (current_scope) {
                      // Replace FunctionParam in dimensions and check whether its symbols are accessible from current_scope
                      // Self is already in a_args (at the PASS position), so
                      // FunctionParam indices align 1:1 with a_args indices.
                      ReplaceFunctionParamWithArg r(al, a_args, n_args);
                      SetChar temp_function_dependencies;
                      CheckSymbolReplacer c(al, current_scope, temp_function_dependencies);
    
                      // 1. Initialize the caller's duplicator
                      ASRUtils::ExprStmtDuplicator caller_dup(al);
                      caller_dup.allow_procedure_calls = true;
                      caller_dup.success = true;

                      bool valid_symbols = true;
                      for (size_t i = 0; i < dimension_.size(); i++) {
                          dimension_.p[i].loc = arg->base.loc;
        
                          // 2. Safely extract the replaced memory (optimized or mutated)
                          ASR::expr_t* replaced_length = r.replace_FunctionParam_with_arg(dimension_[i].m_length);
                          ASR::expr_t* replaced_start = r.replace_FunctionParam_with_arg(dimension_[i].m_start);
        
                          // 3. Caller takes responsibility for duplication to prevent the DAG crash
                          dimension_.p[i].m_length = (replaced_length == dimension_.p[i].m_length) ? caller_dup.duplicate_expr(replaced_length) : replaced_length;
                          dimension_.p[i].m_start = (replaced_start == dimension_.p[i].m_start) ? caller_dup.duplicate_expr(replaced_start) : replaced_start;
                          valid_symbols = c.check_and_update_symbols(dimension_[i].m_length) && c.check_and_update_symbols(dimension_[i].m_start);

                          if (!valid_symbols) {
                            break;
                          }
                        }
                          // If the symbols are valid then include the dimension in ArrayPhysicalCast
                       if (valid_symbols) {
                          dimensions = &dimension_;
                           if (current_function_dependencies.has_value()) {
                            for (size_t j = 0; j < temp_function_dependencies.n; j++) {
                                current_function_dependencies->get().push_back(al, temp_function_dependencies[j]);
                            }
                          }
                        } else {
                        dimensions = nullptr;
                    }
                }

                //TO DO : Add appropriate errors in 'asr_uttils.h'.
                LCOMPILERS_ASSERT_MSG(
                    (ASR::is_a<ASR::String_t>(*ASRUtils::type_get_past_array(orig_arg_type)) &&
                    ASR::down_cast<ASR::String_t>(ASRUtils::type_get_past_array(orig_arg_type))->m_len_kind ==
                        ASR::string_length_kindType::AssumedLength &&
                    ASRUtils::is_fixed_size_array(orig_arg_array_t->m_dims, orig_arg_array_t->n_dims)) ||
                    dimensions_compatible(arg_array_t->m_dims, arg_array_t->n_dims,
                        orig_arg_array_t->m_dims, orig_arg_array_t->n_dims, false),
                    "Incompatible dimensions passed to " + (std::string)(ASR::down_cast<ASR::Function_t>(a_name_)->m_name)
                    + "(" + std::to_string(get_fixed_size_of_array(arg_array_t->m_dims,arg_array_t->n_dims)) + "/" + std::to_string(get_fixed_size_of_array(orig_arg_array_t->m_dims,orig_arg_array_t->n_dims))+")");

                ASR::ttype_t* cast_target_type = ASRUtils::duplicate_type(al,
                                                                            physical_cast_type,
                                                                            dimensions,
                                                                            cast_physical_type,
                                                                            true);
                // For assumed-rank allocatable/pointer parameters, preserve the
                // Allocatable/Pointer wrapper so the LLVM backend knows to use
                // pointer-to-pointer semantics (%array.15**).
                // Only wrap if not already wrapped (physical_cast_type may
                // already include Pointer/Allocatable).
                // Skip for BindC — CFI descriptors use single-pointer semantics.
                ASR::ttype_t* raw_param_type = func_type->m_arg_types[i];
                if (is_orig_assumed_rank &&
                    func_type->m_abi != ASR::abiType::BindC &&
                    ASR::is_a<ASR::Allocatable_t>(*raw_param_type) &&
                    ASR::is_a<ASR::Allocatable_t>(*ASRUtils::expr_type(arg)) &&
                    !ASR::is_a<ASR::Allocatable_t>(*cast_target_type)) {
                    cast_target_type = ASRUtils::TYPE(ASR::make_Allocatable_t(al,
                        arg->base.loc, cast_target_type));
                } else if (is_orig_assumed_rank &&
                           func_type->m_abi != ASR::abiType::BindC &&
                           ASR::is_a<ASR::Pointer_t>(*raw_param_type) &&
                           ASR::is_a<ASR::Pointer_t>(*ASRUtils::expr_type(arg)) &&
                           !ASR::is_a<ASR::Pointer_t>(*cast_target_type)) {
                    cast_target_type = ASRUtils::TYPE(ASR::make_Pointer_t(al,
                        arg->base.loc, cast_target_type));
                }
                physical_cast_arg.m_value = ASRUtils::EXPR(
                                                ASRUtils::make_ArrayPhysicalCast_t_util(
                                                    al,
                                                    arg->base.loc,
                                                    arg,
                                                    arg_array_t->m_physical_type,
                                                    cast_physical_type,
                                                    cast_target_type,
                                                    nullptr));
                a_args[i] = physical_cast_arg;
            }
        }
        if(is_stringToArray_cast_needed(arg_type, orig_arg_type)){
            ASR::ttype_t* string_array_target = orig_arg_type;
            //handle assumed rank case also
            if (ASRUtils::is_assumed_rank_array(orig_arg_type)) {
                Vec<ASR::dimension_t> one_dim; one_dim.reserve(al, 1);
                one_dim.push_back(al, {arg->base.loc, nullptr, nullptr});
                string_array_target = ASRUtils::duplicate_type(al, orig_arg_type,&one_dim, ASR::array_physical_typeType::DescriptorArray, true);
            }
            a_args[i].m_value = &cast_string_to_array(al, arg, string_array_target)->base;
        }
    }
}

void insert_self_arg(
    Allocator& al, ASR::symbol_t* a_name,
    ASR::call_arg_t*& a_args, size_t& n_args,
    ASR::expr_t* a_dt) {
    if (a_dt == nullptr) return;
    bool nopass = get_class_proc_nopass_val(a_name);
    if (nopass) return;

    size_t pass_idx = get_pass_arg_index(a_name);
    Vec<ASR::call_arg_t> new_args;
    new_args.reserve(al, n_args + 1);
    ASR::call_arg_t self_arg;
    self_arg.loc = a_dt->base.loc;
    if (ASR::is_a<ASR::Variable_t>(*symbol_get_past_external(a_name)) &&
        ASR::is_a<ASR::StructInstanceMember_t>(*a_dt)) {
        self_arg.m_value = ASR::down_cast<ASR::StructInstanceMember_t>(a_dt)->m_v;
    } else {
        self_arg.m_value = a_dt;
    }
    size_t explicit_i = 0;
    for (size_t i = 0; i < n_args + 1; i++) {
        if (i == pass_idx) {
            new_args.push_back(al, self_arg);
        } else {
            new_args.push_back(al, a_args[explicit_i]);
            explicit_i++;
        }
    }
    a_args = new_args.p;
    n_args = new_args.size();
}

// The type of a function reference whose result is an explicit-shape array
// with a constant length below zero, which the actual arguments can give
// (`f(-3)` for a result `r(n)`), has the extent zero there.
static ASR::ttype_t* clamp_constant_result_extents(Allocator &al,
        ASR::ttype_t* type) {
    if (!ASR::is_a<ASR::Array_t>(*type)) {
        return type;
    }
    ASR::Array_t* array = ASR::down_cast<ASR::Array_t>(type);
    Vec<ASR::dimension_t> dims;
    dims.reserve(al, array->n_dims);
    bool clamped = false;
    for (size_t i = 0; i < array->n_dims; i++) {
        ASR::dimension_t dim = array->m_dims[i];
        ASR::expr_t* value = dim.m_length ? ASRUtils::expr_value(dim.m_length) : nullptr;
        int64_t length = 0;
        if (value && ASRUtils::extract_value(value, length) && length < 0) {
            dim.m_length = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al,
                dim.m_length->base.loc, 0, ASRUtils::expr_type(dim.m_length)));
            clamped = true;
        }
        dims.push_back(al, dim);
    }
    if (!clamped) {
        return type;
    }
    return ASRUtils::TYPE(ASR::make_Array_t(al, type->base.loc, array->m_type,
        dims.p, dims.size(), array->m_physical_type, array->m_memory_space));
}

ASR::asr_t* make_FunctionCall_t_util(
    Allocator &al, const Location &a_loc, ASR::symbol_t* a_name,
    ASR::symbol_t* a_original_name, ASR::call_arg_t* a_args, size_t n_args,
    ASR::ttype_t* a_type, ASR::expr_t* a_value, ASR::expr_t* a_dt, SymbolTable* current_scope, std::optional<std::reference_wrapper<SetChar>> current_function_dependencies,
    bool implicit_argument_casting) {

    Call_t_body(al, a_name, a_args, n_args, a_dt, nullptr, implicit_argument_casting,
        current_scope, current_function_dependencies);

    if( ASRUtils::is_array(a_type) && ASRUtils::is_elemental(a_name) &&
        !ASRUtils::is_fixed_size_array(a_type) &&
        !ASRUtils::is_dimension_dependent_only_on_arguments(a_type) ) {
        ASR::ttype_t* type_ = ASRUtils::extract_type(a_type);
        #define i32j(j) ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, a_loc, j, \
            ASRUtils::TYPE(ASR::make_Integer_t(al, a_loc, 4))))
        ASR::expr_t* i32one = i32j(1);
        for( size_t i = 0; i < n_args; i++ ) {
            if (a_args[i].m_value == nullptr) {
                continue;
            }

            ASR::ttype_t* type = ASRUtils::expr_type(a_args[i].m_value);
            if (ASRUtils::is_array(type)) {
                ASR::dimension_t* m_dims = nullptr;
                size_t n_dims = ASRUtils::extract_dimensions_from_ttype(type, m_dims);
                if( ASRUtils::is_dimension_empty(m_dims, n_dims) ) {
                    Vec<ASR::dimension_t> m_dims_vec; m_dims_vec.reserve(al, n_dims);
                    for( size_t j = 0; j < n_dims; j++ ) {
                        ASR::dimension_t m_dim_vec;
                        m_dim_vec.loc = m_dims[j].loc;
                        m_dim_vec.m_start = i32one;
                        // `dim` of ArraySize is a dimension of the argument
                        // itself; for a section such as `x(1,:)` that skips
                        // the scalar-subscripted dimensions of `x`.
                        m_dim_vec.m_length = ASRUtils::EXPR(ASRUtils::make_ArraySize_t_util(al, m_dims[j].loc,
                            a_args[i].m_value, i32j(j + 1), ASRUtils::expr_type(i32one), nullptr));
                        m_dims_vec.push_back(al, m_dim_vec);
                    }
                    m_dims = m_dims_vec.p;
                    n_dims = m_dims_vec.size();
                }
                a_type = ASRUtils::make_Array_t_util(al, type->base.loc, type_, m_dims, n_dims,
                    ASR::abiType::Source, false, ASR::array_physical_typeType::DescriptorArray, true);
                break;
            }
        }
    }

    return ASR::make_FunctionCall_t(al, a_loc, a_name, a_original_name,
            a_args, n_args, clamp_constant_result_extents(al, a_type), a_value, a_dt);
}

ASR::asr_t* make_SubroutineCall_t_util(
    Allocator &al, const Location &a_loc, ASR::symbol_t* a_name,
    ASR::symbol_t* a_original_name, ASR::call_arg_t* a_args, size_t n_args,
    ASR::expr_t* a_dt, ASR::stmt_t** cast_stmt, bool implicit_argument_casting, SymbolTable* current_scope, std::optional<std::reference_wrapper<SetChar>> current_function_dependencies, bool a_strict_bounds_checking) {

    // Wrap a_dt as StructInstanceMember for procedure pointer components
    if( a_dt && ASR::is_a<ASR::Variable_t>(
        *ASRUtils::symbol_get_past_external(a_name)) &&
        ASR::is_a<ASR::FunctionType_t>(*ASRUtils::type_get_past_pointer(ASRUtils::symbol_type(a_name))) &&
        !ASR::is_a<ASR::StructInstanceMember_t>(*a_dt) ) {
        a_dt = ASRUtils::EXPR(ASR::make_StructInstanceMember_t(al, a_loc,
            a_dt, a_name, import_procedure_type(al, ASRUtils::duplicate_type(al,
                ASRUtils::symbol_type(a_name)), current_scope), nullptr));
    }

    Call_t_body(al, a_name, a_args, n_args, a_dt, cast_stmt, implicit_argument_casting,
         current_scope, current_function_dependencies);

    return ASR::make_SubroutineCall_t(al, a_loc, a_name, a_original_name, a_args, n_args, a_dt, a_strict_bounds_checking);
}

ASR::expr_t* find_first_array_arg_if_elemental(
    const ASR::Function_t* f,
    const Vec<ASR::call_arg_t>& args
) {
    ASR::expr_t* first_array_argument = nullptr;
    bool is_elemental = ASRUtils::get_FunctionType(f)->m_elemental;
    if (!is_elemental || f->n_args == 0) {
        return first_array_argument;
    }
    for (size_t i=0; i < args.size(); i++) {
        if (args[i].m_value && is_array(ASRUtils::expr_type(args[i].m_value))) {
            // return the very first *array* argument
            first_array_argument = args[i].m_value;
            break;
        }
    }
    return first_array_argument;
}

ASR::asr_t* make_StringFormat_t_util(Allocator &al, const Location &a_loc,
        ASR::expr_t* a_fmt, ASR::expr_t** a_args, size_t n_args, ASR::string_format_kindType a_kind,
        ASR::dtio_binding_t* a_dtio_bindings, size_t n_dtio_bindings, ASR::expr_t* a_dtio_unit,
        ASR::ttype_t* a_type, ASR::expr_t* a_value) {
    if (a_fmt && ASR::is_a<ASR::Var_t>(*a_fmt)) {
        ASR::Variable_t* fmt_str = ASR::down_cast<ASR::Variable_t>(ASRUtils::symbol_get_past_external(ASR::down_cast<ASR::Var_t>(a_fmt)->m_v));
        if (ASR::is_a<ASR::String_t>(
                *ASRUtils::extract_type(fmt_str->m_type))) {
            ASR::String_t* str_type = ASR::down_cast<ASR::String_t>(
                ASRUtils::extract_type(fmt_str->m_type));
            if (str_type->m_physical_type != ASR::string_physical_typeType::DescriptorString) {
                a_fmt = ASRUtils::EXPR(ASR::make_StringPhysicalCast_t(
                    al,
                    a_fmt->base.loc,
                    a_fmt,
                    str_type->m_physical_type,
                    ASR::string_physical_typeType::DescriptorString,
                    a_type,
                    nullptr));
            }
        }
    }
    return ASR::make_StringFormat_t(al, a_loc, a_fmt, a_args, n_args, a_kind,
        a_dtio_bindings, n_dtio_bindings, a_dtio_unit, a_type, a_value);
}

ASR::expr_t* cast_to_descriptor(Allocator& al, ASR::expr_t* arg) {
    ASR::ttype_t* arg_type = ASRUtils::expr_type(arg);
    ASR::Array_t* arg_array_t = ASR::down_cast<ASR::Array_t>(
        ASRUtils::type_get_past_pointer(
            ASRUtils::type_get_past_allocatable(arg_type)));
    if( arg_array_t->m_physical_type != ASR::array_physical_typeType::DescriptorArray ) {
        return ASRUtils::EXPR(ASRUtils::make_ArrayPhysicalCast_t_util(
            al, arg->base.loc, arg, arg_array_t->m_physical_type,
            ASR::array_physical_typeType::DescriptorArray,
            ASRUtils::duplicate_type(al, ASRUtils::expr_type(arg),
                nullptr, ASR::array_physical_typeType::DescriptorArray, true),
            nullptr));
    }
    return arg;
}

ASR::asr_t* make_IntrinsicElementalFunction_t_util(
    Allocator &al, const Location &a_loc, int64_t a_intrinsic_id,
    ASR::expr_t** a_args, size_t n_args, int64_t a_overload_id,
    ASR::ttype_t* a_type, ASR::expr_t* a_value) {

    for( size_t i = 0; i < n_args; i++ ) {
        if( a_args[i] == nullptr ) {
            continue;
        }
        ASR::expr_t* arg = a_args[i];
        ASR::ttype_t* arg_type = ASRUtils::type_get_past_allocatable(
            ASRUtils::type_get_past_pointer(ASRUtils::expr_type(arg)));

        if( ASRUtils::is_array(arg_type) ) {
            a_args[i] = cast_to_descriptor(al, arg);
            if( !ASRUtils::is_array(a_type) ) {
                ASR::Array_t* e = ASR::down_cast<ASR::Array_t>(
                    ASRUtils::type_get_past_allocatable(
                        ASRUtils::type_get_past_pointer(arg_type)));
                a_type = TYPE(ASR::make_Array_t(al, a_type->base.loc, a_type,
                                    e->m_dims, e->n_dims, e->m_physical_type, e->m_memory_space));
            }
        }
    }

    return ASR::make_IntrinsicElementalFunction_t(al, a_loc, a_intrinsic_id,
        a_args, n_args, a_overload_id, a_type, a_value);
}

ASR::asr_t* make_IntrinsicArrayFunction_t_util(
    Allocator &al, const Location &a_loc, int64_t arr_intrinsic_id,
    ASR::expr_t** a_args, size_t n_args, int64_t a_overload_id,
    ASR::ttype_t* a_type, ASR::expr_t* a_value) {

    for( size_t i = 0; i < n_args; i++ ) {
        if( a_args[i] == nullptr ) {
            continue;
        }
        ASR::expr_t* arg = a_args[i];
        ASR::ttype_t* arg_type = ASRUtils::type_get_past_allocatable(
            ASRUtils::type_get_past_pointer(ASRUtils::expr_type(arg)));

        if( ASRUtils::is_array(arg_type) ) {
            a_args[i] = cast_to_descriptor(al, arg);
        }
    }

    return ASR::make_IntrinsicArrayFunction_t(al, a_loc, arr_intrinsic_id,
        a_args, n_args, a_overload_id, a_type, a_value);
}

ASR::Variable_t* rightmost_part_variable(ASR::expr_t* x) {
    while (ASR::is_a<ASR::ArrayPhysicalCast_t>(*x)) {
        x = ASR::down_cast<ASR::ArrayPhysicalCast_t>(x)->m_arg;
    }
    ASR::symbol_t* sym = nullptr;
    if (ASR::is_a<ASR::Var_t>(*x)) {
        sym = ASR::down_cast<ASR::Var_t>(x)->m_v;
    } else if (ASR::is_a<ASR::StructInstanceMember_t>(*x)) {
        sym = ASR::down_cast<ASR::StructInstanceMember_t>(x)->m_m;
    } else {
        return nullptr;
    }
    sym = symbol_get_past_external(sym);
    if (!ASR::is_a<ASR::Variable_t>(*sym)) {
        return nullptr;
    }
    return ASR::down_cast<ASR::Variable_t>(sym);
}

bool is_simply_contiguous(ASR::expr_t* x) {
    switch (x->type) {
        case ASR::exprType::ArrayPhysicalCast:
            return is_simply_contiguous(ASR::down_cast<ASR::ArrayPhysicalCast_t>(x)->m_arg);
        case ASR::exprType::FunctionCall: {
            // A function, a type-bound procedure, a dummy procedure or a
            // procedure pointer
            ASR::symbol_t* fn = symbol_get_past_external(
                ASR::down_cast<ASR::FunctionCall_t>(x)->m_name);
            if (!ASR::is_a<ASR::Function_t>(*fn) &&
                    !ASR::is_a<ASR::StructMethodDeclaration_t>(*fn) &&
                    !(ASR::is_a<ASR::Variable_t>(*fn) &&
                        ASR::down_cast<ASR::Variable_t>(fn)->m_type_declaration != nullptr &&
                        ASR::is_a<ASR::Function_t>(*symbol_get_past_external(
                            ASR::down_cast<ASR::Variable_t>(fn)->m_type_declaration)))) {
                return false;
            }
            ASR::Function_t* function = get_function(fn);
            ASR::expr_t* result = function != nullptr ? function->m_return_var : nullptr;
            if (result == nullptr || !ASR::is_a<ASR::Var_t>(*result)) {
                return false;
            }
            ASR::symbol_t* result_sym = symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(result)->m_v);
            return ASR::is_a<ASR::Variable_t>(*result_sym) &&
                ASR::down_cast<ASR::Variable_t>(result_sym)->m_contiguous_attr;
        }
        case ASR::exprType::Var:
        case ASR::exprType::StructInstanceMember:
            return is_contiguous_array_designator(x);
        case ASR::exprType::ArraySection: {
            ASR::ArraySection_t* section = ASR::down_cast<ASR::ArraySection_t>(x);
            ASR::Variable_t* v = rightmost_part_variable(section->m_v);
            if (v == nullptr || !is_array(v->m_type) ||
                    is_possibly_discontiguous_part(v)) {
                return false;
            }
            // Only the last subscript triplet may be other than `:`, it has
            // no stride and no subscript precedes a triplet.
            bool after_subscript = false;
            bool after_partial_triplet = false;
            for (size_t i = 0; i < section->n_args; i++) {
                ASR::array_index_t& idx = section->m_args[i];
                if (idx.m_left == nullptr && idx.m_step == nullptr) {
                    if (idx.m_right != nullptr && is_array(expr_type(idx.m_right))) {
                        return false;
                    }
                    after_subscript = true;
                    continue;
                }
                if (after_subscript || after_partial_triplet) {
                    return false;
                }
                if (idx.m_step != nullptr && !(ASR::is_a<ASR::IntegerConstant_t>(*idx.m_step) &&
                        ASR::down_cast<ASR::IntegerConstant_t>(idx.m_step)->m_n == 1)) {
                    return false;
                }
                auto is_bound = [&](ASR::expr_t* e, ASR::arrayboundType kind) {
                    if (e == nullptr) {
                        return true;
                    }
                    if (!ASR::is_a<ASR::ArrayBound_t>(*e)) {
                        // A bound of an explicit-shape array, as a constant
                        ASR::dimension_t* dims = nullptr;
                        size_t n_dims = extract_dimensions_from_ttype(v->m_type, dims);
                        int64_t value, start = 1, length;
                        return i < n_dims && dims[i].m_length != nullptr &&
                            extract_value(expr_value(e), value) &&
                            (dims[i].m_start == nullptr ||
                                extract_value(expr_value(dims[i].m_start), start)) &&
                            extract_value(expr_value(dims[i].m_length), length) &&
                            value == (kind == ASR::arrayboundType::LBound ?
                                start : start + length - 1);
                    }
                    ASR::ArrayBound_t* bound = ASR::down_cast<ASR::ArrayBound_t>(e);
                    return bound->m_bound == kind && rightmost_part_variable(bound->m_v) == v &&
                        bound->m_dim != nullptr &&
                        ASR::is_a<ASR::IntegerConstant_t>(*bound->m_dim) &&
                        ASR::down_cast<ASR::IntegerConstant_t>(bound->m_dim)->m_n ==
                            static_cast<int64_t>(i + 1);
                };
                if (!is_bound(idx.m_left, ASR::arrayboundType::LBound) ||
                        !is_bound(idx.m_right, ASR::arrayboundType::UBound)) {
                    after_partial_triplet = true;
                }
            }
            return true;
        }
        default:
            return false;
    }
}

ASR::asr_t* make_Associate_t_util(
    Allocator &al, const Location &a_loc,
    ASR::expr_t* a_target, ASR::expr_t* a_value,
    ASR::pointer_bounds_formType a_form) {
    ASR::ttype_t* target_type = ASRUtils::expr_type(a_target);
    ASR::ttype_t* value_type = ASRUtils::expr_type(a_value);
    if( ASRUtils::is_array(target_type) && ASRUtils::is_array(value_type) ) {
        ASR::array_physical_typeType target_ptype = ASRUtils::extract_physical_type(target_type);
        ASR::array_physical_typeType value_ptype = ASRUtils::extract_physical_type(value_type);
        if( target_ptype != value_ptype ) {
            ASR::dimension_t *target_m_dims = nullptr, *value_m_dims = nullptr;
            size_t target_n_dims = ASRUtils::extract_dimensions_from_ttype(target_type, target_m_dims);
            size_t value_n_dims = ASRUtils::extract_dimensions_from_ttype(value_type, value_m_dims);
            Vec<ASR::dimension_t> dim_vec;
            Vec<ASR::dimension_t>* dim_vec_ptr = nullptr;
            if( (!ASRUtils::is_dimension_empty(target_m_dims, target_n_dims) ||
                !ASRUtils::is_dimension_empty(value_m_dims, value_n_dims)) &&
                target_ptype == ASR::array_physical_typeType::FixedSizeArray ) {
                if( !ASRUtils::is_dimension_empty(target_m_dims, target_n_dims) ) {
                    dim_vec.from_pointer_n(target_m_dims, target_n_dims);
                } else {
                    dim_vec.from_pointer_n(value_m_dims, value_n_dims);
                }
                dim_vec_ptr = &dim_vec;
            }
            a_value = ASRUtils::EXPR(ASRUtils::make_ArrayPhysicalCast_t_util(al, a_loc, a_value,
                        value_ptype, target_ptype, ASRUtils::duplicate_type(al,
                        value_type, dim_vec_ptr, target_ptype, true), nullptr));
        }
    }
    return ASR::make_Associate_t(al, a_loc, a_target, a_value, a_form);
}

void extract_array_indices(ASR::expr_t* x, Allocator &al,
    Vec<ASR::array_index_t>& m_args, int& n_args) {
    if( x->type == ASR::exprType::ArrayItem ) {
        ASR::ArrayItem_t* arr = ASR::down_cast<ASR::ArrayItem_t>(x);
        for(size_t i = 0; i < arr->n_args; i++){
            if((arr->m_args[i].m_left && arr->m_args[i].m_right && arr->m_args[i].m_step) ||
                  (arr->m_args[i].m_right && ASRUtils::is_array(ASRUtils::expr_type(arr->m_args[i].m_right)))){
                m_args.push_back(al, arr->m_args[i]);
                n_args++;
            }
        }
        return ;
    } else if( x->type == ASR::exprType::ArraySection ) {
        ASR::ArraySection_t* arr = ASR::down_cast<ASR::ArraySection_t>(x);
        for(size_t i = 0; i < arr->n_args; i++){
            if((arr->m_args[i].m_left && arr->m_args[i].m_right && arr->m_args[i].m_step) ||
                  (arr->m_args[i].m_right && ASRUtils::is_array(ASRUtils::expr_type(arr->m_args[i].m_right)))){
                m_args.push_back(al, arr->m_args[i]);
                n_args++;
            }
        }
    }
}

void extract_indices(ASR::expr_t* x,
    ASR::array_index_t*& m_args, size_t& n_args) {
    if( x->type == ASR::exprType::ArrayItem ) {
        m_args = ASR::down_cast<ASR::ArrayItem_t>(x)->m_args;
        n_args = ASR::down_cast<ASR::ArrayItem_t>(x)->n_args;
        return ;
    } else if( x->type == ASR::exprType::ArraySection ) {
        m_args = ASR::down_cast<ASR::ArraySection_t>(x)->m_args;
        n_args = ASR::down_cast<ASR::ArraySection_t>(x)->n_args;
        return ;
    }

    m_args = nullptr;
    n_args = 0;
}

bool is_array_indexed_with_array_indices(ASR::array_index_t* m_args, size_t n_args) {
    for( size_t i = 0; i < n_args; i++ ) {
        if( m_args[i].m_left == nullptr &&
            m_args[i].m_right != nullptr &&
            m_args[i].m_step == nullptr &&
            ASRUtils::is_array(
                ASRUtils::expr_type(m_args[i].m_right)) ) {
            return true;
        }
    }

    return false;
}

ASR::expr_t* struct_base_lending_shape(ASR::ArrayItem_t* x) {
    if( is_array_indexed_with_array_indices(x->m_args, x->n_args) ||
        x->m_v == nullptr ||
        !ASR::is_a<ASR::StructInstanceMember_t>(*x->m_v) ) {
        return nullptr;
    }
    ASR::expr_t* base = ASR::down_cast<ASR::StructInstanceMember_t>(x->m_v)->m_v;
    if( base == nullptr || !ASRUtils::is_array(ASRUtils::expr_type(base)) ) {
        return nullptr;
    }
    // Every component between the base array and the one being indexed must
    // hold its value inline, or the reference denotes an array of
    // indirections, which this type representation cannot express. The part
    // of nonzero rank itself, such as the component `w` of a scalar in
    // `s%w%u(1)`, may be of any kind.
    ASR::expr_t* root = get_struct_member_chain_array_part(base);
    for( ASR::expr_t* e = base; e != root;
            e = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v ) {
        ASR::StructInstanceMember_t* member =
            ASR::down_cast<ASR::StructInstanceMember_t>(e);
        if( ASRUtils::is_allocatable(member->m_type) ||
            ASR::is_a<ASR::Pointer_t>(*member->m_type) ) {
            return nullptr;
        }
    }
    // The chain has to bottom out in an array variable or an array component
    // of a scalar, or in a section of one, as in `w(2:4)%u(1)`: a section is
    // an array in its own right, whose shape the reference takes. An element
    // underneath is a scalar. A section with a vector subscript is not
    // supported here and is left alone.
    if( ASR::is_a<ASR::ArraySection_t>(*root) ) {
        ASR::ArraySection_t* section = ASR::down_cast<ASR::ArraySection_t>(root);
        if( is_array_indexed_with_array_indices(section) ) {
            return nullptr;
        }
        root = section->m_v;
    }
    if( !ASR::is_a<ASR::Var_t>(*root) &&
        !ASR::is_a<ASR::StructInstanceMember_t>(*root) ) {
        return nullptr;
    }
    ASR::ttype_t* root_type = ASRUtils::expr_type(root);
    if( root_type == nullptr || !ASRUtils::is_array(root_type) ) {
        return nullptr;
    }
    return base;
}

ASR::ttype_t* create_array_type_with_empty_dims(Allocator& al,
    size_t value_n_dims, ASR::ttype_t* value_type) {
    Vec<ASR::dimension_t> empty_dims; empty_dims.reserve(al, value_n_dims);
    for( size_t i = 0; i < value_n_dims; i++ ) {
        ASR::dimension_t empty_dim;
        Location loc; loc.first = 1, loc.last = 1;
        empty_dim.loc = loc;
        empty_dim.m_length = nullptr;
        empty_dim.m_start = nullptr;
        empty_dims.push_back(al, empty_dim);
    }
    return ASRUtils::make_Array_t_util(al, value_type->base.loc,
        ASRUtils::extract_type(value_type), empty_dims.p, empty_dims.size());
}

ASR::asr_t* make_ArrayItem_t_util(Allocator &al, const Location &a_loc,
    ASR::expr_t* a_v, ASR::array_index_t* a_args, size_t n_args, ASR::ttype_t* a_type,
    ASR::arraystorageType a_storage_format, ASR::expr_t* a_value,
    bool a_in_bounds) {
    if( ASR::is_a<ASR::ArrayPhysicalCast_t>(*a_v) && !(ASR::down_cast<ASR::ArrayPhysicalCast_t>(a_v)->m_old == ASR::array_physical_typeType::AssumedRankArray)) {
        a_v = ASR::down_cast<ASR::ArrayPhysicalCast_t>(a_v)->m_arg;
    }

    if( !ASRUtils::is_array_indexed_with_array_indices(a_args, n_args) ) {
        a_type = ASRUtils::extract_type(a_type);
    }

    if( ASRUtils::is_allocatable(a_type) ) {
        ASR::dimension_t* m_dims = nullptr;
        size_t n_dims = ASRUtils::extract_dimensions_from_ttype(a_type, m_dims);
        if( !ASRUtils::is_dimension_empty(m_dims, n_dims) ) {
            a_type = ASRUtils::create_array_type_with_empty_dims(
                al, n_dims, ASRUtils::extract_type(a_type));
            a_type = ASRUtils::TYPE(ASR::make_Allocatable_t(al, a_loc, a_type));
        }
    }

    ASRUtils::ExprStmtDuplicator type_duplicator(al);
    a_type = type_duplicator.duplicate_ttype(a_type);
    return ASR::make_ArrayItem_t(al, a_loc, a_v, a_args,
        n_args, a_type, a_storage_format, a_value, a_in_bounds);
}

ASR::ttype_t* make_Pointer_t_util(Allocator& al, const Location& loc, ASR::ttype_t* type) {
    if( ASRUtils::is_array(type) && !ASRUtils::is_assumed_rank_array(type) ) {
        ASR::dimension_t* m_dims = nullptr;
        size_t n_dims = ASRUtils::extract_dimensions_from_ttype(type, m_dims);
        // A pointer to an array must have deferred shape, so drop the
        // dimensions if any of them still carries a length. Checking only
        // whether some dimension is empty is not enough: an assumed-size
        // array such as `a(1, *)` has an empty last dimension but an
        // explicit first one.
        for( size_t i = 0; i < n_dims; i++ ) {
            if( m_dims[i].m_length != nullptr ) {
                type = ASRUtils::duplicate_type_with_empty_dims(al, type);
                break;
            }
        }
    }

    return ASRUtils::TYPE(ASR::make_Pointer_t(al, loc, type));
}

bool is_argument_of_type_CPtr(ASR::expr_t *var) {
    bool is_argument = false;
    if (ASR::is_a<ASR::CPtr_t>(*expr_type(var))) {
        if (ASR::is_a<ASR::Var_t>(*var)) {
            ASR::symbol_t *var_sym = ASR::down_cast<ASR::Var_t>(var)->m_v;
            if (ASR::is_a<ASR::Variable_t>(*var_sym)) {
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(var_sym);
                if (v->m_intent == intent_local ||
                    v->m_intent == intent_return_var ||
                    !v->m_intent) {
                    is_argument = false;
                } else {
                    is_argument = true;
                }
            }
        }
    }
    return is_argument;
}

void promote_arguments_kinds(Allocator &al, const Location &loc,
        Vec<ASR::expr_t*> &args, diag::Diagnostics &diag,
        bool use_first_array_kind) {
    int target_kind = -1;
    for (size_t i = 0; i < args.size(); i++) {
        ASR::ttype_t *arg_type = ASRUtils::expr_type(args[i]);
        int kind = ASRUtils::extract_kind_from_ttype_t(arg_type);
        if (use_first_array_kind && is_array(arg_type)){
            target_kind = kind;
            break;
        }
        if (kind > target_kind) {
            target_kind = kind;
        }
    }

    for (size_t i = 0; i < args.size(); i++) {
        ASR::ttype_t *arg_type = ASRUtils::expr_type(args[i]);
        int kind = ASRUtils::extract_kind_from_ttype_t(arg_type);
        if (kind==target_kind) {
            continue;
        }
        ASR::ttype_t *arg_elem_type = ASRUtils::extract_type(arg_type);
        ASR::ttype_t *target_type = nullptr;
        ASR::cast_kindType cast_kind;
        if (ASR::is_a<ASR::Real_t>(*arg_elem_type)) {
            target_type = ASRUtils::TYPE(ASR::make_Real_t(al, loc, target_kind));
            cast_kind = ASR::cast_kindType::RealToReal;
        } else if (ASR::is_a<ASR::Integer_t>(*arg_elem_type)) {
            target_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, target_kind));
            cast_kind = ASR::cast_kindType::IntegerToInteger;
        } else {
            diag.semantic_error_label("Unsupported argument type for kind adjustment", {loc},
                "help: ensure all arguments are of a convertible type");
            continue;
        }
        if (is_array(arg_type)) {
            ASR::dimension_t *m_dims = nullptr;
            size_t n_dims = ASRUtils::extract_dimensions_from_ttype(arg_type, m_dims);
            target_type = ASRUtils::make_Array_t_util(al, loc, target_type, m_dims, n_dims);
            args.p[i] = EXPR(ASRUtils::make_Cast_t_value(
                al, loc, args[i], cast_kind, target_type));
        } else if (ASR::is_a<ASR::RealConstant_t>(*args[i])) {
            args.p[i] = EXPR(ASR::make_RealConstant_t(
                al, loc, ASR::down_cast<ASR::RealConstant_t>(args[i])->m_r,
                target_type));
        } else if (ASR::is_a<ASR::IntegerConstant_t>(*args[i])) {
            args.p[i] = EXPR(ASR::make_IntegerConstant_t(
                al, loc, ASR::down_cast<ASR::IntegerConstant_t>(args[i])->m_n,
                target_type));
        } else {
            args.p[i] = EXPR(ASR::make_Cast_t(
                al, loc, args.p[i], cast_kind, target_type, nullptr, nullptr));
        }
    }
}

std::set<ASR::Function_t*> get_called_functions(ASR::stmt_t **body,
        size_t n_body, bool procedure_values) {
    struct Collector : BlockBodyWalkVisitor<Collector> {
        std::set<ASR::Function_t*> callees;
        bool procedure_values;
        explicit Collector(bool values) : procedure_values(values) {}
        void add(ASR::symbol_t *symbol) {
            symbol = symbol_get_past_external(symbol);
            if (!symbol) return;
            symbol = symbol_get_past_StructMethodDeclaration(symbol);
            if (symbol && ASR::is_a<ASR::Function_t>(*symbol)) {
                callees.insert(ASR::down_cast<ASR::Function_t>(symbol));
            }
        }
        void visit_FunctionCall(const ASR::FunctionCall_t &x) {
            add(x.m_name);
            ASR::BaseWalkVisitor<Collector>::visit_FunctionCall(x);
        }
        void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
            add(x.m_name);
            ASR::BaseWalkVisitor<Collector>::visit_SubroutineCall(x);
        }
        void visit_Var(const ASR::Var_t &x) {
            if (procedure_values) add(x.m_v);
        }
    };
    Collector collector(procedure_values);
    for (size_t i = 0; i < n_body; i++) collector.visit_stmt(*body[i]);
    return collector.callees;
}

bool is_side_effect_free_function(ASR::Function_t &fn,
        std::set<ASR::Function_t*> &visiting) {
    ASR::FunctionType_t *type = get_FunctionType(fn);
    if (type->m_pure) return true;
    if (type->m_deftype != ASR::deftypeType::Implementation ||
            fn.m_return_var == nullptr || !visiting.insert(&fn).second) {
        return false;
    }
    for (size_t i = 0; i < fn.n_args; i++) {
        if (!ASR::is_a<ASR::Var_t>(*fn.m_args[i])) return false;
        ASR::symbol_t *arg = ASR::down_cast<ASR::Var_t>(fn.m_args[i])->m_v;
        if (!ASR::is_a<ASR::Variable_t>(*arg) ||
                ASR::down_cast<ASR::Variable_t>(arg)->m_intent !=
                    ASR::intentType::In) {
            return false;
        }
    }
    struct Effects : BlockBodyWalkVisitor<Effects> {
        ASR::Function_t &fn;
        std::set<ASR::Function_t*> &visiting;
        bool free = true;
        Effects(ASR::Function_t &fn, std::set<ASR::Function_t*> &visiting)
            : fn(fn), visiting(visiting) {}
        void visit_stmt(const ASR::stmt_t &x) {
            switch (x.type) {
                case ASR::stmtType::Assignment:
                case ASR::stmtType::DoLoop:
                case ASR::stmtType::WhileLoop:
                case ASR::stmtType::If:
                case ASR::stmtType::Select:
                case ASR::stmtType::Exit:
                case ASR::stmtType::Cycle:
                case ASR::stmtType::Return:
                case ASR::stmtType::BlockCall:
                case ASR::stmtType::AssociateBlockCall:
                    BlockBodyWalkVisitor<Effects>::visit_stmt(x);
                    break;
                default:
                    free = false;
            }
        }
        void visit_Assignment(const ASR::Assignment_t &x) {
            ASR::expr_t *target = x.m_target;
            while (true) {
                if (ASR::is_a<ASR::ArrayItem_t>(*target)) {
                    target = ASR::down_cast<ASR::ArrayItem_t>(target)->m_v;
                } else if (ASR::is_a<ASR::ArraySection_t>(*target)) {
                    target = ASR::down_cast<ASR::ArraySection_t>(target)->m_v;
                } else if (ASR::is_a<ASR::StructInstanceMember_t>(*target)) {
                    target = ASR::down_cast<ASR::StructInstanceMember_t>(
                        target)->m_v;
                } else {
                    break;
                }
            }
            if (!ASR::is_a<ASR::Var_t>(*target)) {
                free = false;
            } else {
                ASR::symbol_t *s = ASR::down_cast<ASR::Var_t>(target)->m_v;
                if (!ASR::is_a<ASR::Variable_t>(*s)) {
                    free = false;
                } else {
                    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
                    SymbolTable *scope = v->m_parent_symtab;
                    while (scope != nullptr && scope != fn.m_symtab) {
                        scope = scope->parent;
                    }
                    if (scope == nullptr || (v->m_intent !=
                            ASR::intentType::Local && v->m_intent !=
                            ASR::intentType::ReturnVar) ||
                            v->m_storage == ASR::storage_typeType::Save) {
                        free = false;
                    }
                }
            }
            BlockBodyWalkVisitor<Effects>::visit_Assignment(x);
        }
        void visit_FunctionCall(const ASR::FunctionCall_t &x) {
            ASR::symbol_t *callee = symbol_get_past_external(x.m_name);
            if (callee == nullptr || !ASR::is_a<ASR::Function_t>(*callee) ||
                    !is_side_effect_free_function(
                        *ASR::down_cast<ASR::Function_t>(callee), visiting)) {
                free = false;
            }
            BlockBodyWalkVisitor<Effects>::visit_FunctionCall(x);
        }
    };
    Effects effects(fn, visiting);
    for (size_t i = 0; i < fn.n_body && effects.free; i++) {
        effects.visit_stmt(*fn.m_body[i]);
    }
    visiting.erase(&fn);
    return effects.free;
}

bool struct_needs_member_init(ASR::Struct_t* s,
        std::set<ASR::Struct_t*>& visited) {
    if (!visited.insert(s).second) {
        return false;
    }
    for (ASR::Struct_t* c = s; c != nullptr;
            c = c->m_parent == nullptr ? nullptr
                : ASR::down_cast<ASR::Struct_t>(
                    symbol_get_past_external(c->m_parent))) {
        for (size_t i = 0; i < c->n_members; i++) {
            ASR::symbol_t* sym = symbol_get_past_external(
                c->m_symtab->get_symbol(c->m_members[i]));
            if (!ASR::is_a<ASR::Variable_t>(*sym)) {
                continue;
            }
            ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(sym);
            if (is_array(v->m_type)) {
                return true;
            }
            if (is_character(*v->m_type)
                    && !is_inline_character_struct_member(c, v->m_type)) {
                return true;
            }
            ASR::ttype_t* member_type = extract_type(v->m_type);
            if (is_class_type(member_type)) {
                return true;
            }
            // A pointer or allocatable member holds the address of another
            // object, whose members are not set up from here.
            if (ASR::is_a<ASR::StructType_t>(*member_type)
                    && !is_pointer(v->m_type) && !is_allocatable(v->m_type)) {
                ASR::symbol_t* member_struct = symbol_get_past_external(
                    v->m_type_declaration);
                if (member_struct != nullptr
                        && ASR::is_a<ASR::Struct_t>(*member_struct)
                        && struct_needs_member_init(
                            ASR::down_cast<ASR::Struct_t>(member_struct), visited)) {
                    return true;
                }
            }
        }
    }
    return false;
}

bool needs_struct_array_member_init(ASR::expr_t* expr,
        ASR::ttype_t* type) {
    if (!is_array(type)) {
        return false;
    }
    ASR::ttype_t* el_type = type_get_past_array(type);
    if (!ASR::is_a<ASR::StructType_t>(*el_type) || is_class_type(el_type)) {
        return false;
    }
    ASR::symbol_t* struct_sym = symbol_get_past_external(
        get_struct_sym_from_struct_expr(expr));
    if (struct_sym == nullptr || !ASR::is_a<ASR::Struct_t>(*struct_sym)) {
        return false;
    }
    std::set<ASR::Struct_t*> visited;
    return struct_needs_member_init(
        ASR::down_cast<ASR::Struct_t>(struct_sym), visited);
}

int64_t constant_array_item_offset(ASR::ArrayItem_t* x) {
    ASR::dimension_t* dims = nullptr;
    size_t n_dims = extract_dimensions_from_ttype(expr_type(x->m_v), dims);
    if (n_dims == 0 || n_dims != x->n_args) {
        return -1;
    }
    int64_t offset = 0, stride = 1;
    for (size_t i = 0; i < n_dims; i++) {
        if (x->m_args[i].m_right == nullptr || dims[i].m_start == nullptr
                || dims[i].m_length == nullptr) {
            return -1;
        }
        int64_t index, start, length;
        if (!extract_value(expr_value(x->m_args[i].m_right), index)
                || !extract_value(expr_value(dims[i].m_start), start)
                || !extract_value(expr_value(dims[i].m_length), length)) {
            return -1;
        }
        if (index < start || index >= start + length) {
            return -1;
        }
        offset += (index - start) * stride;
        stride *= length;
    }
    return offset;
}

bool has_link_time_address(ASR::expr_t* d) {
    switch (d->type) {
        case ASR::exprType::Var: {
            ASR::symbol_t* sym = symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(d)->m_v);
            if (!ASR::is_a<ASR::Variable_t>(*sym)) {
                return false;
            }
            ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(sym);
            // A coarray's storage comes from `prif_allocate_coarray` at run
            // time, and an allocatable's from the allocation: the global that
            // names one holds an address the run time writes, rather than
            // being the storage itself.
            if (v->n_codims > 0 || is_allocatable_or_pointer(v->m_type)) {
                return false;
            }
            return is_module_variable(*v);
        }
        case ASR::exprType::ArrayItem: {
            ASR::ArrayItem_t* item = ASR::down_cast<ASR::ArrayItem_t>(d);
            return constant_array_item_offset(item) >= 0
                && has_link_time_address(item->m_v);
        }
        case ASR::exprType::StructInstanceMember: {
            ASR::StructInstanceMember_t* member =
                ASR::down_cast<ASR::StructInstanceMember_t>(d);
            // A pointer or allocatable component holds an address that is
            // read at run time rather than storage of its own.
            if (is_allocatable_or_pointer(expr_type(member->m_v))) {
                return false;
            }
            return has_link_time_address(member->m_v);
        }
        default: {
            return false;
        }
    }
}

bool has_only_static_component_targets(ASR::symbol_t* struct_sym) {
    ASR::Struct_t* s = ASR::down_cast<ASR::Struct_t>(
        symbol_get_past_external(struct_sym));
    if (s->m_parent != nullptr
            && !has_only_static_component_targets(s->m_parent)) {
        return false;
    }
    for (size_t i = 0; i < s->n_members; i++) {
        ASR::symbol_t* sym = s->m_symtab->get_symbol(s->m_members[i]);
        if (sym == nullptr || !ASR::is_a<ASR::Variable_t>(*sym)) {
            continue;
        }
        ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(sym);
        if (is_pointer(var->m_type)
                && is_pointer_association_initializer(var->m_symbolic_value)
                && !has_static_pointer_target(*var)) {
            return false;
        }
        if (ASR::is_a<ASR::StructType_t>(*var->m_type)
                && !is_class_type(var->m_type)
                && var->m_type_declaration != nullptr
                && !has_only_static_component_targets(var->m_type_declaration)) {
            return false;
        }
    }
    return true;
}

bool ExprDependentOnlyOnArguments::returns_non_primitive_type(ASR::ttype_t* t) {
    if (!t) return false;
    if (ASR::is_a<ASR::Allocatable_t>(*t) ||
        ASR::is_a<ASR::Array_t>(*t)) return true;
    ASR::ttype_t* base = t;
    while (ASR::is_a<ASR::Allocatable_t>(*base))
        base = ASR::down_cast<ASR::Allocatable_t>(base)->m_type;
    while (ASR::is_a<ASR::Pointer_t>(*base))
        base = ASR::down_cast<ASR::Pointer_t>(base)->m_type;
    if (ASR::is_a<ASR::StructType_t>(*base) ||
        ASR::is_a<ASR::SymbolicExpression_t>(*base) ||
        ASR::is_a<ASR::List_t>(*base) ||
        ASR::is_a<ASR::Dict_t>(*base) ||
        ASR::is_a<ASR::Set_t>(*base) ||
        ASR::is_a<ASR::Tuple_t>(*base)) return true;
    if (ASR::is_a<ASR::String_t>(*base)) {
        ASR::String_t* str = ASR::down_cast<ASR::String_t>(base);
        return str->m_physical_type != ASR::string_physical_typeType::CChar;
    }
    return false;
}

ASR::expr_t* ReplaceFunctionParamWithArg::replace_FunctionParam_with_arg(ASR::expr_t* t) {
    

    LCOMPILERS_ASSERT(replacements.empty()) 
    ASR::expr_t** current_copy = current_expr;
    current_expr = &t;
    
    replace_expr(t);
    current_expr = current_copy;

    ASRUtils::ExprStmtDuplicator duplicator(al);
    duplicator.allow_procedure_calls = true;
    duplicator.success = true;

    ASR::expr_t* tc = duplicator.duplicate_expr(t);
    LCOMPILERS_ASSERT(duplicator.success);

    for (auto& rep : replacements) {
        *(rep.first) = rep.second; 
    }
    replacements.clear();

    return tc;
}

void ReplaceFunctionParamWithArg::replace_type(ASR::ttype_t* t) {
    if (!t) return;
    t = ASRUtils::type_get_past_allocatable_pointer(t);

    if (ASR::is_a<ASR::String_t>(*t)) {
        ASR::String_t* s = ASR::down_cast<ASR::String_t>(t);
        if (s->m_len) {
            s->m_len = replace_FunctionParam_with_arg(s->m_len);
        }
    } else if (ASR::is_a<ASR::Array_t>(*t)) {
        ASR::Array_t* arr = ASR::down_cast<ASR::Array_t>(t);
        replace_type(arr->m_type);

        for (size_t i = 0; i < arr->n_dims; i++) {
            if (arr->m_dims[i].m_start) {
                arr->m_dims[i].m_start = replace_FunctionParam_with_arg(arr->m_dims[i].m_start);
            }

            if (arr->m_dims[i].m_length) {
                arr->m_dims[i].m_length = replace_FunctionParam_with_arg(arr->m_dims[i].m_length);
            }
        }
    }
}

void ReplaceArgVisitor::replace_FunctionCall(ASR::FunctionCall_t* x) {
    ASR::symbol_t* f = ASRUtils::symbol_get_past_external(x->m_name);
    ASR::symbol_t* owner = f ? ASRUtils::get_asr_owner(f) : nullptr;
    if (owner && ASR::is_a<ASR::Function_t>(*f)) {
        if (ASR::is_a<ASR::Module_t>(*owner)) {
            // Import a module function as external only if necessary
            ASR::symbol_t* new_es = import_symbol(al, x->m_name, current_scope);
            // The following substitutes args from the current scope
            for (size_t i = 0; i < x->n_args; i++) {
                ASR::expr_t** current_expr_copy_ = current_expr;
                current_expr = &(x->m_args[i].m_value);
                replace_expr(x->m_args[i].m_value);
                current_expr = current_expr_copy_;
            }
            replace_ttype(x->m_type);
            if (ASRUtils::symbol_parent_symtab(new_es)->get_counter() != current_scope->get_counter()) {
                ADD_ASR_DEPENDENCIES(current_scope, new_es, current_function_dependencies);
            }
            ASRUtils::insert_module_dependency(new_es, al, current_module_dependencies);
            x->m_name = new_es;
            if( x->m_original_name ) {
                // The original_name (e.g. a GenericProcedure) has to be
                // visible for modfile serialisation to find its table. A
                // generic is imported as `<generic>@<specific>`: its own
                // name would hide an entity the program names, such as the
                // intrinsic `len` for `len` of a module.
                ASR::symbol_t* original = ASRUtils::symbol_get_past_external(
                    x->m_original_name);
                std::string suffix = ASR::is_a<ASR::GenericProcedure_t>(*original) ||
                    ASR::is_a<ASR::CustomOperator_t>(*original)
                    ? "@" + std::string(ASRUtils::symbol_name(f)) : "";
                x->m_original_name = import_symbol(al,
                    x->m_original_name, current_scope,
                    ASR::accessType::Private, "", suffix);
                ASRUtils::insert_module_dependency(x->m_original_name,
                    al, current_module_dependencies);
            }
        } else {
            // A function of a program or procedure: nothing to import
            ASR::BaseExprReplacer<ReplaceArgVisitor>::replace_FunctionCall(x);
            ADD_ASR_DEPENDENCIES(current_scope, x->m_name, current_function_dependencies);
        }
        return;
    }
    // iterate over the arguments and replace them
    for (size_t i = 0; i < x->n_args; i++) {
        ASR::expr_t** current_expr_copy_ = current_expr;
        current_expr = &(x->m_args[i].m_value);
        if (x->m_args[i].m_value) {
            replace_expr(x->m_args[i].m_value);
        }
        current_expr = current_expr_copy_;
    }
}

void ReplaceArgVisitor::replace_Var(ASR::Var_t* x) {
    size_t arg_idx = 0;
    bool idx_found = false;
    // Finds the index of the argument to be used for substitution
    // Basically if we are calling maybe(string, ret_type=character(len=len(s)))
    // where string is a variable in current scope and s is one of the arguments
    // accepted by maybe i.e., maybe has a signature maybe(s). Then, we will
    // replace s with string. So, the call would become,
    // maybe(string, ret_type=character(len=len(string)))
    for( size_t j = 0; j < orig_func->n_args && !idx_found; j++ ) {
        if( ASR::is_a<ASR::Var_t>(*(orig_func->m_args[j])) ) {
            arg_idx = j;
            idx_found = ASR::down_cast<ASR::Var_t>(orig_func->m_args[j])->m_v == x->m_v;
        }
    }
    if( idx_found ) {
        LCOMPILERS_ASSERT(current_expr);
        *current_expr = orig_args[arg_idx].m_value;
    }  else if (ASRUtils::expr_value((ASR::expr_t*)x)){ //  Replace Constant args 
        *current_expr = ASRUtils::expr_value((ASR::expr_t*)x); 
    }
}

ASR::asr_t* ExprStmtWithScopeDuplicator::duplicate_FunctionPointerCast(ASR::FunctionPointerCast_t* x) {
    ASR::expr_t* m_arg = duplicate_expr(x->m_arg);
    ASR::symbol_t* m_to = x->m_to;
    if (m_to != nullptr) {
        std::string name = ASRUtils::symbol_name(m_to);
        ASR::symbol_t* resolved = use_resolve_symbol
            ? current_scope->resolve_symbol(name)
            : current_scope->get_symbol(name);
        if (resolved != nullptr) {
            m_to = resolved;
        }
    }
    ASR::ttype_t* m_type = duplicate_ttype(x->m_type);
    ASR::expr_t* m_value = duplicate_expr(x->m_value);
    return ASR::make_FunctionPointerCast_t(al, x->base.base.loc, m_arg, m_to,
        m_type, m_value);
}

ASR::asr_t* ExprStmtWithScopeDuplicator::duplicate_StructInstanceMember(ASR::StructInstanceMember_t* x) {
    ASR::expr_t* m_v = duplicate_expr(x->m_v);
    std::string member_name = ASRUtils::symbol_name(x->m_m);
    ASR::symbol_t* m_m = current_scope->resolve_symbol(member_name);
    if (m_m == nullptr) m_m = x->m_m;
    ASR::ttype_t* m_type = duplicate_ttype(x->m_type);
    ASR::expr_t* m_value = duplicate_expr(x->m_value);
    return ASR::make_StructInstanceMember_t(al, x->base.base.loc,
        m_v, m_m, m_type, m_value);
}

ASR::asr_t* ExprStmtWithScopeDuplicator::duplicate_FunctionCall(ASR::FunctionCall_t* x) {
    ASR::symbol_t* m_name = x->m_name;
    ASR::symbol_t* m_original_name = x->m_original_name;
    std::string name = ASRUtils::symbol_name(m_name);
    ASR::symbol_t* resolved = use_resolve_symbol
        ? current_scope->resolve_symbol(name)
        : current_scope->get_symbol(name);
    if (resolved) m_name = resolved;
    if (m_original_name) {
        std::string orig_name = ASRUtils::symbol_name(m_original_name);
        ASR::symbol_t* resolved_orig = use_resolve_symbol
            ? current_scope->resolve_symbol(orig_name)
            : current_scope->get_symbol(orig_name);
        if (resolved_orig) m_original_name = resolved_orig;
    }
    Vec<ASR::call_arg_t> m_args;
    m_args.reserve(al, x->n_args);
    for (size_t i = 0; i < x->n_args; i++) {
        ASR::call_arg_t call_arg_copy;
        call_arg_copy.loc = x->m_args[i].loc;
        call_arg_copy.m_value = duplicate_expr(x->m_args[i].m_value);
        m_args.push_back(al, call_arg_copy);
    }
    ASR::ttype_t* m_type = duplicate_ttype(x->m_type);
    ASR::expr_t* m_value = duplicate_expr(x->m_value);
    ASR::expr_t* m_dt = duplicate_expr(x->m_dt);
    return ASR::make_FunctionCall_t(al, x->base.base.loc, m_name,
        m_original_name, m_args.p, x->n_args, m_type, m_value, m_dt);
}

ASR::asr_t* ExprStmtWithScopeDuplicator::duplicate_SubroutineCall(ASR::SubroutineCall_t* x) {
    ASR::symbol_t* m_name = x->m_name;
    ASR::symbol_t* m_original_name = x->m_original_name;
    std::string name = ASRUtils::symbol_name(m_name);
    ASR::symbol_t* resolved = use_resolve_symbol
        ? current_scope->resolve_symbol(name)
        : current_scope->get_symbol(name);
    if (resolved) m_name = resolved;
    if (m_original_name) {
        std::string orig_name = ASRUtils::symbol_name(m_original_name);
        ASR::symbol_t* resolved_orig = use_resolve_symbol
            ? current_scope->resolve_symbol(orig_name)
            : current_scope->get_symbol(orig_name);
        if (resolved_orig) m_original_name = resolved_orig;
    }
    Vec<ASR::call_arg_t> m_args;
    m_args.reserve(al, x->n_args);
    for (size_t i = 0; i < x->n_args; i++) {
        ASR::call_arg_t call_arg_copy;
        call_arg_copy.loc = x->m_args[i].loc;
        call_arg_copy.m_value = duplicate_expr(x->m_args[i].m_value);
        m_args.push_back(al, call_arg_copy);
    }
    ASR::expr_t* m_dt = duplicate_expr(x->m_dt);
    return ASR::make_SubroutineCall_t(al, x->base.base.loc, m_name,
        m_original_name, m_args.p, x->n_args, m_dt,
        x->m_strict_bounds_checking);
}

void ReplaceWithFunctionParamVisitor::replace_Var(ASR::Var_t* x) {
    size_t arg_idx = 0;
    bool idx_found = false;
    std::string arg_name = ASRUtils::symbol_name(x->m_v);
    for( size_t j = 0; j < n_args && !idx_found; j++ ) {
        if( ASR::is_a<ASR::Var_t>(*(m_args[j])) ) {
            std::string arg_name_2 = std::string(ASRUtils::symbol_name(
                ASR::down_cast<ASR::Var_t>(m_args[j])->m_v));
            arg_idx = j;
            idx_found = arg_name_2 == arg_name;
        }
    }

    if( idx_found ) {
        LCOMPILERS_ASSERT(current_expr);
        ASR::ttype_t* t_ = replace_args_with_FunctionParam(
                            ASRUtils::symbol_type(x->m_v), current_scope);
        *current_expr = ASRUtils::EXPR(ASR::make_FunctionParam_t(
                            al, m_args[arg_idx]->base.loc, arg_idx,
                            t_, nullptr));
    } else if (ASR::is_a<ASR::Variable_t>(*x->m_v) &&
            is_entry_initialized_local(*ASR::down_cast<ASR::Variable_t>(x->m_v))) {
        // A local initialized on entry (e.g. the extent of an
        // explicit-shape dummy) stands for its initializer, which is
        // given in terms of the arguments.
        ASRUtils::ExprStmtDuplicator duplicator(al);
        *current_expr = duplicator.duplicate_expr(
            ASR::down_cast<ASR::Variable_t>(x->m_v)->m_symbolic_value);
        replace_expr(*current_expr);
    } else if (ASRUtils::expr_value((ASR::expr_t*)x)){//  Replace Constant args
        *current_expr = ASRUtils::expr_value((ASR::expr_t*)x); 
    }
}

ASR::ttype_t* ReplaceWithFunctionParamVisitor::replace_args_with_FunctionParam(ASR::ttype_t* t, SymbolTable* current_scope) {
    this->current_scope = current_scope;

    ASRUtils::ExprStmtDuplicator duplicator(al);
    duplicator.allow_procedure_calls = true;

    // We need to substitute all direct argument variable references with
    // FunctionParam.
    duplicator.success = true;
    t = duplicator.duplicate_ttype(t);
    LCOMPILERS_ASSERT(duplicator.success);
    replace_ttype(t);
    return t;
}

void SymbolDuplicator::duplicate_SymbolTable(SymbolTable* symbol_table,
    SymbolTable* destination_symtab) {
    std::vector<std::pair<ASR::Variable_t*, ASR::Variable_t*>> variables;
    for( auto& item: symbol_table->get_scope() ) {
        bool is_new = destination_symtab->get_symbol(item.first) == nullptr;
        duplicate_symbol(item.second, destination_symtab);
        ASR::symbol_t* copy = destination_symtab->get_symbol(item.first);
        if (is_new && copy && ASR::is_a<ASR::Variable_t>(*item.second)
                && ASR::is_a<ASR::Variable_t>(*copy)) {
            variables.push_back({ASR::down_cast<ASR::Variable_t>(item.second),
                ASR::down_cast<ASR::Variable_t>(copy)});
        }
    }
    remap_local_references(variables, destination_symtab);
    fixup_local_type_declarations(destination_symtab, symbol_table);
}

void SymbolDuplicator::remap_local_references(
        const std::vector<std::pair<ASR::Variable_t*, ASR::Variable_t*>> &variables,
        SymbolTable* destination_symtab) {
    for (auto &pair : variables) {
        ASR::Variable_t* orig = pair.first;
        ASR::Variable_t* copy = pair.second;
        ExprStmtWithScopeDuplicator node_duplicator(al, destination_symtab);
        node_duplicator.success = true;
        ASR::ttype_t* m_type = node_duplicator.duplicate_ttype(orig->m_type);
        ASR::expr_t* m_symbolic_value = node_duplicator.duplicate_expr(
            orig->m_symbolic_value);
        ASR::expr_t* m_value = node_duplicator.duplicate_expr(orig->m_value);
        if (!node_duplicator.success) {
            continue;
        }
        copy->m_type = m_type;
        copy->m_symbolic_value = m_symbolic_value;
        copy->m_value = m_value;
    }
}

void SymbolDuplicator::duplicate_symbol(ASR::symbol_t* symbol,
    SymbolTable* destination_symtab) {
    ASR::symbol_t* new_symbol = nullptr;
    std::string new_symbol_name = "";
    std::string symbol_name = ASRUtils::symbol_name(symbol);
    if (destination_symtab->get_symbol(symbol_name)) {
        return;
    }
    switch( symbol->type ) {
        case ASR::symbolType::Variable: {
            ASR::Variable_t* variable = ASR::down_cast<ASR::Variable_t>(symbol);
            new_symbol = duplicate_Variable(variable, destination_symtab);
            new_symbol_name = variable->m_name;
            break;
        }
        case ASR::symbolType::Module: {
            ASR::Module_t* module = ASR::down_cast<ASR::Module_t>(symbol);
            new_symbol = duplicate_Module(module, destination_symtab);
            new_symbol_name = module->m_name;
            break;
        }
        case ASR::symbolType::ExternalSymbol: {
            ASR::ExternalSymbol_t* external_symbol = ASR::down_cast<ASR::ExternalSymbol_t>(symbol);
            new_symbol = duplicate_ExternalSymbol(external_symbol, destination_symtab);
            new_symbol_name = external_symbol->m_name;
            break;
        }
        case ASR::symbolType::AssociateBlock: {
            ASR::AssociateBlock_t* associate_block = ASR::down_cast<ASR::AssociateBlock_t>(symbol);
            new_symbol = duplicate_AssociateBlock(associate_block, destination_symtab);
            new_symbol_name = associate_block->m_name;
            break;
        }
        case ASR::symbolType::Function: {
            ASR::Function_t* function = ASR::down_cast<ASR::Function_t>(symbol);
            new_symbol = duplicate_Function(function, destination_symtab);
            new_symbol_name = function->m_name;
            break;
        }
        case ASR::symbolType::Block: {
            ASR::Block_t* block = ASR::down_cast<ASR::Block_t>(symbol);
            new_symbol = duplicate_Block(block, destination_symtab);
            new_symbol_name = block->m_name;
            break;
        }
        case ASR::symbolType::Struct: {
            ASR::Struct_t* struct_type = ASR::down_cast<ASR::Struct_t>(symbol);
            new_symbol = duplicate_Struct(struct_type, destination_symtab);
            new_symbol_name = struct_type->m_name;
            break;
        }
        case ASR::symbolType::GenericProcedure: {
            ASR::GenericProcedure_t* generic_procedure = ASR::down_cast<ASR::GenericProcedure_t>(symbol);
            new_symbol = duplicate_GenericProcedure(generic_procedure, destination_symtab);
            new_symbol_name = generic_procedure->m_name;
            break;
        }
        case ASR::symbolType::CustomOperator: {
            ASR::CustomOperator_t* custom_operator = ASR::down_cast<ASR::CustomOperator_t>(symbol);
            new_symbol = duplicate_CustomOperator(custom_operator, destination_symtab);
            new_symbol_name = custom_operator->m_name;
            break;
        }
        case ASR::symbolType::StructMethodDeclaration: {
            ASR::StructMethodDeclaration_t* struct_method = ASR::down_cast<ASR::StructMethodDeclaration_t>(symbol);
            new_symbol = duplicate_StructMethodDeclaration(struct_method, destination_symtab);
            new_symbol_name = struct_method->m_name;
            break;
        }
        case ASR::symbolType::Namelist: {
            ASR::Namelist_t* namelist = ASR::down_cast<ASR::Namelist_t>(symbol);
            new_symbol = duplicate_Namelist(namelist, destination_symtab);
            new_symbol_name = namelist->m_group_name;
            break;
        }
        case ASR::symbolType::Requirement: {
            ASR::Requirement_t* requirement = ASR::down_cast<ASR::Requirement_t>(symbol);
            new_symbol = duplicate_Requirement(requirement, destination_symtab);
            new_symbol_name = requirement->m_name;
            break;
        }
        case ASR::symbolType::Template: {
            ASR::Template_t* template_sym = ASR::down_cast<ASR::Template_t>(symbol);
            new_symbol = duplicate_Template(template_sym, destination_symtab);
            new_symbol_name = template_sym->m_name;
            break;
        }
        case ASR::symbolType::CommonBlock: {
            ASR::CommonBlock_t* common_block = ASR::down_cast<ASR::CommonBlock_t>(symbol);
            new_symbol = duplicate_CommonBlock(common_block, destination_symtab);
            new_symbol_name = common_block->m_name;
            break;
        }
        default: {
            throw LCompilersException("Duplicating ASR::symbolType::" +
                    std::to_string(symbol->type) + " is not supported yet.");
        }
    }
    if( new_symbol ) {
        destination_symtab->add_symbol(new_symbol_name, new_symbol);
    }
}

ASR::symbol_t* SymbolDuplicator::duplicate_Variable(ASR::Variable_t* variable,
    SymbolTable* destination_symtab) {
    ExprStmtWithScopeDuplicator node_duplicator(al, destination_symtab);
    node_duplicator.success = true;
    ASR::expr_t* m_symbolic_value = node_duplicator.duplicate_expr(variable->m_symbolic_value);
    if( !node_duplicator.success ) {
        return nullptr;
    }
    node_duplicator.success = true;
    ASR::expr_t* m_value = node_duplicator.duplicate_expr(variable->m_value);
    if( !node_duplicator.success ) {
        return nullptr;
    }
    node_duplicator.success = true;
    ASR::ttype_t* m_type = node_duplicator.duplicate_ttype(variable->m_type);
    if( !node_duplicator.success ) {
        return nullptr;
    }
    return ASR::down_cast<ASR::symbol_t>(
        ASRUtils::make_Variable_t_util(al, variable->base.base.loc, destination_symtab,
            variable->m_name, variable->m_dependencies, variable->n_dependencies,
            variable->m_intent, m_symbolic_value, m_value, variable->m_storage,
            m_type, variable->m_type_declaration, variable->m_abi, variable->m_access,
            variable->m_presence, variable->m_value_attr, variable->m_target_attr,
            variable->m_contiguous_attr, variable->m_bindc_name, variable->m_is_volatile,
            variable->m_is_protected, variable->m_pass_attr, variable->m_self_argument,
            variable->m_codims, variable->n_codims
        ));
}

ASR::symbol_t* SymbolDuplicator::duplicate_AssociateBlock(ASR::AssociateBlock_t* associate_block,
    SymbolTable* destination_symtab) {
    SymbolTable* associate_block_symtab = al.make_new<SymbolTable>(destination_symtab);
    duplicate_SymbolTable(associate_block->m_symtab, associate_block_symtab);
    Vec<ASR::stmt_t*> new_body;
    new_body.reserve(al, associate_block->n_body);
    ASRUtils::ExprStmtDuplicator node_duplicator(al);
    node_duplicator.allow_procedure_calls = true;
    node_duplicator.allow_reshape = true;
    for( size_t i = 0; i < associate_block->n_body; i++ ) {
        node_duplicator.success = true;
        ASR::stmt_t* new_stmt = node_duplicator.duplicate_stmt(associate_block->m_body[i]);
        if( !node_duplicator.success ) {
            return nullptr;
        }
        new_body.push_back(al, new_stmt);
    }

    // node_duplicator_.allow_procedure_calls = true;

    return ASR::down_cast<ASR::symbol_t>(ASR::make_AssociateBlock_t(al,
                    associate_block->base.base.loc, associate_block_symtab,
                    associate_block->m_name, new_body.p, new_body.size()));
}

bool SymbolDuplicator::reduplicate_body_in_scope(SymbolTable *new_symtab,
        ASR::stmt_t **orig_body, size_t orig_n_body,
        Vec<ASR::stmt_t*> &new_body) {
    new_body.reserve(al, orig_n_body);
    ASRUtils::ExprStmtWithScopeDuplicator body_dup(al, new_symtab);
    body_dup.use_resolve_symbol = true;
    body_dup.allow_procedure_calls = true;
    body_dup.allow_reshape = true;
    for (size_t i = 0; i < orig_n_body; i++) {
        body_dup.success = true;
        ASR::stmt_t* s = body_dup.duplicate_stmt(orig_body[i]);
        if (!body_dup.success) return false;
        new_body.push_back(al, s);
    }
    return true;
}

void SymbolDuplicator::fixup_type_declarations_in_scope(SymbolTable *new_scope,
        SymbolTable *orig_scope,
        std::vector<std::pair<SymbolTable*, SymbolTable*>> &copied_scopes) {
    class RetargetToCopies : public ASR::BaseWalkVisitor<RetargetToCopies> {
    public:
        const std::vector<std::pair<SymbolTable*, SymbolTable*>> &scopes;
        RetargetToCopies(const std::vector<std::pair<SymbolTable*,
                SymbolTable*>> &scopes) : scopes(scopes) {}
        void visit_Var(const ASR::Var_t &x) {
            SymbolTable *parent = ASRUtils::symbol_parent_symtab(x.m_v);
            for (auto &s : scopes) {
                if (parent != s.first) continue;
                ASR::symbol_t *copy = s.second->get_symbol(
                    ASRUtils::symbol_name(x.m_v));
                if (copy != nullptr && copy->type == x.m_v->type) {
                    const_cast<ASR::Var_t&>(x).m_v = copy;
                }
                return;
            }
        }
    };
    copied_scopes.push_back({orig_scope, new_scope});
    RetargetToCopies retarget(copied_scopes);
    for (auto &item : new_scope->get_scope()) {
        if (ASR::is_a<ASR::Variable_t>(*item.second)) {
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(item.second);
            if (copied_scopes.size() > 1) {
                retarget.visit_symbol(*item.second);
            }
            if (v->m_type_declaration == nullptr) continue;
            SymbolTable *declared_in =
                ASRUtils::symbol_parent_symtab(v->m_type_declaration);
            for (auto &scopes : copied_scopes) {
                if (declared_in != scopes.first) continue;
                ASR::symbol_t *copy = scopes.second->get_symbol(
                    ASRUtils::symbol_name(v->m_type_declaration));
                if (copy != nullptr) {
                    v->m_type_declaration = copy;
                }
                break;
            }
            continue;
        }
        SymbolTable *new_nested = nullptr;
        if (ASR::is_a<ASR::Block_t>(*item.second) ||
                ASR::is_a<ASR::AssociateBlock_t>(*item.second) ||
                ASR::is_a<ASR::Function_t>(*item.second)) {
            new_nested = ASRUtils::symbol_symtab(item.second);
        }
        ASR::symbol_t *orig_sym = orig_scope->get_symbol(item.first);
        if (new_nested == nullptr || orig_sym == nullptr ||
                orig_sym->type != item.second->type) {
            continue;
        }
        SymbolTable *orig_nested = ASRUtils::symbol_symtab(orig_sym);
        if (orig_nested != nullptr && orig_nested != new_nested) {
            fixup_type_declarations_in_scope(new_nested, orig_nested,
                copied_scopes);
        }
    }
    copied_scopes.pop_back();
}

void SymbolDuplicator::fixup_nested_block_bodies(SymbolTable *new_scope,
        SymbolTable *orig_scope) {
    for (auto &item : new_scope->get_scope()) {
        ASR::symbol_t *orig_sym = orig_scope->get_symbol(
            item.first);
        if (!orig_sym) continue;
        if (ASR::is_a<ASR::AssociateBlock_t>(*item.second) &&
                ASR::is_a<ASR::AssociateBlock_t>(*orig_sym)) {
            ASR::AssociateBlock_t *new_ab =
                ASR::down_cast<ASR::AssociateBlock_t>(item.second);
            ASR::AssociateBlock_t *orig_ab =
                ASR::down_cast<ASR::AssociateBlock_t>(orig_sym);
            // Recurse into nested blocks first so their bodies are
            // fixed before we re-duplicate this level.
            fixup_nested_block_bodies(new_ab->m_symtab,
                orig_ab->m_symtab);
            Vec<ASR::stmt_t*> ab_body;
            if (reduplicate_body_in_scope(new_ab->m_symtab,
                    orig_ab->m_body, orig_ab->n_body, ab_body)) {
                new_ab->m_body = ab_body.p;
                new_ab->n_body = ab_body.n;
            }
        } else if (ASR::is_a<ASR::Block_t>(*item.second) &&
                ASR::is_a<ASR::Block_t>(*orig_sym)) {
            ASR::Block_t *new_block =
                ASR::down_cast<ASR::Block_t>(item.second);
            ASR::Block_t *orig_block =
                ASR::down_cast<ASR::Block_t>(orig_sym);
            fixup_nested_block_bodies(new_block->m_symtab,
                orig_block->m_symtab);
            Vec<ASR::stmt_t*> block_body;
            if (reduplicate_body_in_scope(new_block->m_symtab,
                    orig_block->m_body, orig_block->n_body,
                    block_body)) {
                new_block->m_body = block_body.p;
                new_block->n_body = block_body.n;
            }
        }
    }
}

ASR::symbol_t* SymbolDuplicator::duplicate_Function(ASR::Function_t* function,
    SymbolTable* destination_symtab) {
    SymbolTable* function_symtab = al.make_new<SymbolTable>(destination_symtab);
    duplicate_SymbolTable(function->m_symtab, function_symtab);

    fixup_nested_block_bodies(function_symtab, function->m_symtab);

    Vec<ASR::stmt_t*> new_body;
    new_body.reserve(al, function->n_body);

    ASRUtils::ExprStmtWithScopeDuplicator scoped_node_duplicator(al, function_symtab);
    scoped_node_duplicator.allow_procedure_calls = true;
    scoped_node_duplicator.allow_reshape = true;

    for( size_t i = 0; i < function->n_body; i++ ) {
        scoped_node_duplicator.success = true;
        ASR::stmt_t* new_stmt = scoped_node_duplicator.duplicate_stmt(function->m_body[i]);
        if (!scoped_node_duplicator.success) {
            return nullptr;
        }
        new_body.push_back(al, new_stmt);
    }

    Vec<ASR::expr_t*> new_args;
    new_args.reserve(al, function->n_args);
    for( size_t i = 0; i < function->n_args; i++ ) {
        scoped_node_duplicator.success = true;
        ASR::expr_t* new_arg = scoped_node_duplicator.duplicate_expr(function->m_args[i]);
        if (!scoped_node_duplicator.success) {
            return nullptr;
        }
        new_args.push_back(al, new_arg);
    }

    ASR::expr_t* new_return_var = function->m_return_var;
    if( new_return_var ) {
        scoped_node_duplicator.success = true;
        new_return_var = scoped_node_duplicator.duplicate_expr(function->m_return_var);
        if (!scoped_node_duplicator.success) {
            return nullptr;
        }
    }

    ASR::FunctionType_t* function_type = ASRUtils::get_FunctionType(function);

    return ASR::down_cast<ASR::symbol_t>(make_Function_t_util(al,
        function->base.base.loc, function_symtab, function->m_name,
        function->m_dependencies, function->n_dependencies, new_args.p,
        new_args.size(), new_body.p, new_body.size(), new_return_var,
        function_type->m_abi, function->m_access, function_type->m_deftype,
        function_type->m_bindc_name, function_type->m_elemental, function_type->m_pure,
        function_type->m_module, function_type->m_inline, function_type->m_static,
        function_type->m_restrictions, function_type->n_restrictions,
        function_type->m_is_restriction, function->m_deterministic,
        function->m_side_effect_free, nullptr, nullptr, nullptr,
        function_type->m_exec_space));
}

ASR::symbol_t* SymbolDuplicator::duplicate_Module(ASR::Module_t* module_t,
    SymbolTable* destination_symtab) {
    SymbolTable* module_symtab = al.make_new<SymbolTable>(destination_symtab);
    duplicate_SymbolTable(module_t->m_symtab, module_symtab);

    return ASR::down_cast<ASR::symbol_t>(ASR::make_Module_t(
        al, module_t->base.base.loc, module_symtab,
        module_t->m_name, module_t->m_parent_module, module_t->m_dependencies,
        module_t->n_dependencies, module_t->m_loaded_from_mod, module_t->m_intrinsic,
        module_t->m_has_submodules, module_t->m_global_init,
        module_t->m_start_name, module_t->m_end_name
    ));
}

ASR::symbol_t* SymbolDuplicator::duplicate_Requirement(ASR::Requirement_t* requirement_t,
    SymbolTable* destination_symtab) {
    SymbolTable* requirement_symtab = al.make_new<SymbolTable>(destination_symtab);
    duplicate_SymbolTable(requirement_t->m_symtab, requirement_symtab);

    return ASR::down_cast<ASR::symbol_t>(ASR::make_Requirement_t(
        al, requirement_t->base.base.loc, requirement_symtab,
        requirement_t->m_name, requirement_t->m_args, requirement_t->n_args,
        requirement_t->m_requires, requirement_t->n_requires
    ));
}

ASR::symbol_t* SymbolDuplicator::duplicate_Template(ASR::Template_t* template_t,
    SymbolTable* destination_symtab) {
    SymbolTable* template_symtab = al.make_new<SymbolTable>(destination_symtab);
    duplicate_SymbolTable(template_t->m_symtab, template_symtab);

    return ASR::down_cast<ASR::symbol_t>(ASR::make_Template_t(
        al, template_t->base.base.loc, template_symtab,
        template_t->m_name, template_t->m_args, template_t->n_args,
        template_t->m_requires, template_t->n_requires
    ));
}

ASR::symbol_t* SymbolDuplicator::duplicate_Block(ASR::Block_t* block_t,
    SymbolTable* destination_symtab) {
    SymbolTable* block_symtab = al.make_new<SymbolTable>(destination_symtab);
    duplicate_SymbolTable(block_t->m_symtab, block_symtab);

    Vec<ASR::stmt_t*> new_body;
    new_body.reserve(al, block_t->n_body);
    ASRUtils::ExprStmtDuplicator node_duplicator(al);
    node_duplicator.allow_procedure_calls = true;
    node_duplicator.allow_reshape = true;
    for( size_t i = 0; i < block_t->n_body; i++ ) {
        node_duplicator.success = true;
        ASR::stmt_t* new_stmt = node_duplicator.duplicate_stmt(block_t->m_body[i]);
        if( !node_duplicator.success ) {
            return nullptr;
        }
        new_body.push_back(al, new_stmt);
    }

    return ASR::down_cast<ASR::symbol_t>(ASR::make_Block_t(al,
            block_t->base.base.loc, block_symtab, block_t->m_name,
            new_body.p, new_body.size()));
}

ASR::symbol_t* SymbolDuplicator::duplicate_Struct(ASR::Struct_t* struct_type_t,
    SymbolTable* destination_symtab) {
    SymbolTable* struct_type_symtab = al.make_new<SymbolTable>(destination_symtab);
    duplicate_SymbolTable(struct_type_t->m_symtab, struct_type_symtab);
    return ASR::down_cast<ASR::symbol_t>(ASR::make_Struct_t(
        al, struct_type_t->base.base.loc, struct_type_symtab,
        struct_type_t->m_name, struct_type_t->m_struct_signature, struct_type_t->m_dependencies, struct_type_t->n_dependencies,
        struct_type_t->m_members, struct_type_t->n_members,
        struct_type_t->m_member_functions, struct_type_t->n_member_functions, struct_type_t->m_abi,
        struct_type_t->m_access, struct_type_t->m_is_packed, struct_type_t->m_is_abstract,
        struct_type_t->m_is_sequence,
        struct_type_t->m_initializers, struct_type_t->n_initializers, struct_type_t->m_alignment,
        struct_type_t->m_parent,
        struct_type_t->m_kind_params, struct_type_t->n_kind_params));
}

ASR::symbol_t* SymbolDuplicator::duplicate_StructMethodDeclaration(
    ASR::StructMethodDeclaration_t* structMethod,
    SymbolTable* destination_symtab) {
    return ASR::down_cast<ASR::symbol_t>(ASR::make_StructMethodDeclaration_t(
        al, structMethod->base.base.loc, destination_symtab,
        structMethod->m_name, structMethod->m_self_argument,
        structMethod->m_proc_name, structMethod->m_proc,
        structMethod->m_abi, structMethod->m_is_deferred,
        structMethod->m_is_nopass));
}

ASR::symbol_t* SymbolDuplicator::duplicate_Namelist(ASR::Namelist_t* namelist,
    SymbolTable* destination_symtab) {
    Vec<ASR::symbol_t*> var_list;
    var_list.reserve(al, namelist->n_var_list);
    for (size_t i = 0; i < namelist->n_var_list; i++) {
        ASR::symbol_t* var = namelist->m_var_list[i];
        std::string var_name = ASRUtils::symbol_name(var);
        ASR::symbol_t* new_var = destination_symtab->get_symbol(var_name);
        if (!new_var) {
            duplicate_symbol(var, destination_symtab);
            new_var = destination_symtab->get_symbol(var_name);
        }
        LCOMPILERS_ASSERT(new_var);
        var_list.push_back(al, new_var);
    }
    return ASR::down_cast<ASR::symbol_t>(ASR::make_Namelist_t(
        al, namelist->base.base.loc, destination_symtab,
        namelist->m_group_name, var_list.p, var_list.size()));
}

ASR::symbol_t* SymbolDuplicator::duplicate_CommonBlock(ASR::CommonBlock_t* common_block,
    SymbolTable* destination_symtab) {
    std::vector<ASR::Variable_t*> placed;
    auto copy_of = [&](ASR::symbol_t* variable) {
        std::string name = ASRUtils::symbol_name(variable);
        if (destination_symtab->get_symbol(name) == nullptr) {
            duplicate_symbol(variable, destination_symtab);
        }
        ASR::symbol_t* copy = destination_symtab->get_symbol(name);
        LCOMPILERS_ASSERT(copy != nullptr && ASR::is_a<ASR::Variable_t>(*copy));
        placed.push_back(ASR::down_cast<ASR::Variable_t>(copy));
        return copy;
    };
    Vec<ASR::symbol_t*> members;
    members.reserve(al, common_block->n_members);
    for (size_t i = 0; i < common_block->n_members; i++) {
        members.push_back(al, copy_of(common_block->m_members[i]));
    }
    Vec<ASR::common_association_t> associations;
    associations.reserve(al, common_block->n_associations);
    ExprStmtWithScopeDuplicator node_duplicator(al, destination_symtab);
    for (size_t i = 0; i < common_block->n_associations; i++) {
        ASR::common_association_t association = common_block->m_associations[i];
        association.m_variable = copy_of(association.m_variable);
        node_duplicator.success = true;
        association.m_designator = node_duplicator.duplicate_expr(
            association.m_designator);
        association.m_anchor = node_duplicator.duplicate_expr(
            association.m_anchor);
        LCOMPILERS_ASSERT(node_duplicator.success);
        associations.push_back(al, association);
    }
    ASR::symbol_t* copy = ASR::down_cast<ASR::symbol_t>(
        ASR::make_CommonBlock_t(al, common_block->base.base.loc,
            destination_symtab, common_block->m_name,
            common_block->m_block_name, common_block->m_link_name,
            members.p, members.size(), associations.p, associations.size(),
            common_block->m_abi, common_block->m_storage,
            common_block->m_role));
    for (ASR::Variable_t* variable : placed) {
        variable->m_common_block = copy;
    }
    return copy;
}

ASR::symbol_t* CheckSymbolReplacer::handle_symbol(ASR::symbol_t* x) {
    if (result) {
        ASR::symbol_t* y = current_scope->resolve_symbol(ASRUtils::symbol_name((x)));
        if (y == nullptr) {
            result = false;
        }
        if (y && ASR::is_a<ASR::ExternalSymbol_t>(*y)) {
            ASR::ExternalSymbol_t* ext = ASR::down_cast<ASR::ExternalSymbol_t>(y);
            if (ext->m_external == x) {
                return y;
            }
        }
        if (x != y) {
            result = false;
        }
    }
    return x;
}

void CheckSymbolReplacer::replace_StructInstanceMember(ASR::StructInstanceMember_t* x) {
    ASR::BaseExprReplacer<CheckSymbolReplacer>::replace_StructInstanceMember(x);
    if (!result) return;
    ASR::symbol_t* m_m = x->m_m;
    SymbolTable* m_m_scope = ASRUtils::symbol_parent_symtab(m_m);
    if (m_m_scope->get_counter() != current_scope->get_counter()) {
        ASR::symbol_t* imported = import_struct_instance_member(al, m_m, current_scope);
        if (imported) {
            x->m_m = imported;
        } else {
            result = false;
        }
    }
}

void RemoveArrayProcessingNodeReplacer::replace_ArrayBroadcast(ASR::ArrayBroadcast_t* x) {
    if (preserve_struct_array_broadcast
            && ASRUtils::is_array(x->m_type)
            && ASR::is_a<ASR::StructType_t>(
                *ASRUtils::type_get_past_array(x->m_type))) {
        ASR::BaseExprReplacer<RemoveArrayProcessingNodeReplacer>::
            replace_ArrayBroadcast(x);
        return;
    }
    *current_expr = x->m_array;
}

void RemoveArrayProcessingNodeVisitor::visit_Variable(const ASR::Variable_t& x) {
    ASR::Variable_t& xx = const_cast<ASR::Variable_t&>(x);
    bool preserve_copy = replacer.preserve_struct_array_broadcast;
    replacer.preserve_struct_array_broadcast = true;
    if (xx.m_symbolic_value != nullptr) {
        ASR::expr_t** current_expr_copy = current_expr;
        current_expr = &(xx.m_symbolic_value);
        call_replacer();
        current_expr = current_expr_copy;
    }
    if (xx.m_value != nullptr) {
        ASR::expr_t** current_expr_copy = current_expr;
        current_expr = &(xx.m_value);
        call_replacer();
        current_expr = current_expr_copy;
    }
    replacer.preserve_struct_array_broadcast = preserve_copy;
}

} // namespace ASRUtils


} // namespace LCompilers
