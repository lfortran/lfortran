#include "libasr/asr.h"
#include <unordered_set>
#include <map>
#include <libasr/asr_utils.h>
#include <libasr/string_utils.h>
#include <libasr/serialization.h>
#include <libasr/assert.h>
#include <libasr/asr_verify.h>
#include <libasr/utils.h>
#include <libasr/modfile.h>
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
    if(extract_n_dims_from_ttype(array_type) != 1) throw LCompilersException("Can't cast string to array of n_dims != 1");

    ASR::ttype_t*  const array_type_dup = ASRUtils::ExprStmtDuplicator(al).duplicate_ttype(array_type);
    ASR::Array_t*  const array_t = ASR::down_cast<ASR::Array_t>(type_get_past_allocatable_pointer(array_type_dup));
    ASR::String_t* const dest_string_t = get_string_type(array_type_dup);
    const bool is_length_present = dest_string_t->m_len_kind == ASR::ExpressionLength;
    const bool is_size_present = !is_dimension_empty(array_type_dup); 

    // Notice We're transforming a string into array;
    // Resulting array should have proper length + size. 
    if(is_length_present && is_size_present){ // character()
       return ASR::down_cast2<ASR::Cast_t>(
                ASR::make_Cast_t(al, string_expr->base.loc, string_expr
                            , ASR::StringToArray, array_type_dup, nullptr));
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
                , ASR::StringToArray, array_type_dup, nullptr));
    } else if(!is_length_present && is_size_present){
        ASRBuilder b(al, string_expr->base.loc);
        dest_string_t->m_len = b.StringLen(string_expr);
        dest_string_t->m_len_kind = ASR::ExpressionLength;
       return ASR::down_cast2<ASR::Cast_t>(
                ASR::make_Cast_t(al, string_expr->base.loc, string_expr
                , ASR::StringToArray, array_type_dup, nullptr));
    } else {
        ASRBuilder b(al, string_expr->base.loc);
        dest_string_t->m_len = b.StringLen(string_expr);
        dest_string_t->m_len_kind = ASR::ExpressionLength;
        array_t->m_dims[0].m_start  = b.i32(1);
        array_t->m_dims[0].m_length = b.i32(1);
       return ASR::down_cast2<ASR::Cast_t>(
                ASR::make_Cast_t(al, string_expr->base.loc, string_expr
                , ASR::StringToArray, array_type_dup, nullptr));
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
            ASR::symbol_t* cp = ASR::down_cast<ASR::StructMethodDeclaration_t>(itr.second)->m_proc;
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
            return ASRUtils::get_struct_sym_from_struct_expr(func->m_return_var);
        }
        case ASR::exprType::StructConstant: {
            ASR::StructConstant_t* struct_constant = ASR::down_cast<ASR::StructConstant_t>(expression);
            return struct_constant->m_dt_sym;
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
        case ASR::exprType::ArrayConstant: 
        case ASR::exprType::PointerNullConstant:
        case ASR::exprType::UnsignedIntegerConstant:
        case ASR::exprType::ComplexConstant:
        {
            // These do not have a struct symbol, return nullptr
            return nullptr;
        }
        case ASR::exprType::BitCast: {
            ASR::BitCast_t* bit_cast = ASR::down_cast<ASR::BitCast_t>(expression);
            return ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(bit_cast->m_source));
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
            return ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(cast->m_arg));
        }
        case ASR::exprType::ArrayReshape: {
            ASR::ArrayReshape_t* array_reshape = ASR::down_cast<ASR::ArrayReshape_t>(expression);
            return ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(array_reshape->m_array));
        }
        case ASR::exprType::ArraySize: {
            ASR::ArraySize_t* array_size = ASR::down_cast<ASR::ArraySize_t>(expression);
            return ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(array_size->m_v));
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
            ASR::ArrayBound_t* array_bound = ASR::down_cast<ASR::ArrayBound_t>(expression);
            return ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(array_bound->m_v));
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
        case ASR::exprType::PointerAssociated: {
            return nullptr;
        }
        case ASR::exprType::UnionInstanceMember: {
            ASR::UnionInstanceMember_t* union_instance_member = ASR::down_cast<ASR::UnionInstanceMember_t>(expression);
            LCOMPILERS_ASSERT(ASR::is_a<ASR::Variable_t>(*ASRUtils::symbol_get_past_external(union_instance_member->m_m)));
            ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(ASRUtils::symbol_get_past_external(union_instance_member->m_m));
            return var->m_type_declaration;
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
            return ASR::down_cast<ASR::Function_t>(fc->m_name);
        }
        case ASR::exprType::PointerNullConstant: {
            // PointerNullConstant is a special case where it does not have a function
            // associated with it, so we return nullptr.
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

void update_call_args(Allocator &al, SymbolTable *current_scope, bool implicit_interface,
        std::map<std::string, ASR::symbol_t*> changed_external_function_symbol) {
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
        std::map<std::string, ASR::symbol_t*> &changed_external_function_symbol;
        ArgsVisitor(Allocator &al_, std::map<std::string, ASR::symbol_t*> &changed_external_function_symbol_) : al(al_), replacer(al_),
                    changed_external_function_symbol(changed_external_function_symbol_) {}

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
            ASRUtils::SymbolDuplicator symbol_duplicator(al);
            std::map<std::string, ASR::symbol_t*> scope_ = scope->get_scope();
            std::vector<std::string> symbols_to_duplicate;
            for (auto it: scope_) {
                if (changed_external_function_symbol.find(it.first) != changed_external_function_symbol.end() &&
                    is_external_sym_changed(it.second, changed_external_function_symbol[it.first])) {
                    symbols_to_duplicate.push_back(it.first);
                }
            }

            for (auto it: symbols_to_duplicate) {
                scope->erase_symbol(it);
                symbol_duplicator.duplicate_symbol(changed_external_function_symbol[it], scope);
            }

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
        ArgsVisitor v(al, changed_external_function_symbol);
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
    if (symtab->get_symbol(module_name) != nullptr) {
        ASR::symbol_t *m = symtab->get_symbol(module_name);
        if (ASR::is_a<ASR::Module_t>(*m)) {
            return ASR::down_cast<ASR::Module_t>(m);
        } else {
            err("The symbol '" + module_name + "' is not a module", loc);
        }
    }
    LCOMPILERS_ASSERT(symtab->parent == nullptr);
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
            symtab, nullptr, 0));

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
        std::vector<ASR::TranslationUnit_t*> submods;
        Result<std::vector<ASR::TranslationUnit_t*>, ErrorMessage> res
            = ASRUtils::find_and_load_submodules(al, std::string(mod->m_name), *symtab, pass_options, lm);
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
            }
        }
    }

    // Create a temporary TranslationUnit just for fixing the symbols
    ASR::asr_t *orig_asr_owner = symtab->asr_owner;
    ASR::TranslationUnit_t *tu
        = ASR::down_cast2<ASR::TranslationUnit_t>(ASR::make_TranslationUnit_t(al, loc,
            symtab, nullptr, 0));

    // Keeps track of loaded dependent modules whose submodules are not yet loaded
    std::vector<ASR::Module_t*> dependent_modules_with_not_yet_loaded_submodules;

    // Load any dependent modules recursively
    bool rerun = true;
    while (rerun) {
        rerun = false;
        std::vector<std::string> modules_list
            = determine_module_dependencies(*tu);
        for (auto &item : modules_list) {
            if (symtab->get_symbol(item)
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
    ASR::stmt_t* a_overloaded, bool a_realloc_lhs, bool a_move) {
    bool is_allocatable = ASRUtils::is_allocatable(a_target);
    if ( ASR::is_a<ASR::StructInstanceMember_t>(*a_target) ) {
        ASR::StructInstanceMember_t* a_target_struct = ASR::down_cast<ASR::StructInstanceMember_t>(a_target);
        is_allocatable |= ASRUtils::is_allocatable(a_target_struct->m_v);
    }
    a_realloc_lhs = a_realloc_lhs && is_allocatable;
    return ASR::make_Assignment_t(al, a_loc, a_target, a_value,
        a_overloaded, a_realloc_lhs, a_move);
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

    for (auto path : mod_files_dirs) {
        std::string modfile;
        std::filesystem::path full_path = path / filename;
        if (read_file(full_path.string(), modfile)) {
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

    for (auto &path : mod_files_dirs) {
        if (path.empty()) path = ".";
        for (auto &file : std::filesystem::directory_iterator(path)) {
            std::string submod_filename = file.path().filename().string();
            if (startswith(submod_filename, parent_module_name) && endswith(submod_filename, ".smod")) {
                std::string submodfile;
                if (read_file(file.path().string(), submodfile)) {
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

        if (ASR::is_a<ASR::ArrayItem_t>(*ASRUtils::EXPR(v_var))) {
            ASR::ArrayItem_t *t = ASR::down_cast<ASR::ArrayItem_t>(ASRUtils::EXPR(v_var));
            if (ASRUtils::is_array_indexed_with_array_indices(t->m_args, t->n_args)) {
                n_dims = ASRUtils::extract_dimensions_from_ttype(
                    ASRUtils::type_get_past_allocatable_pointer(
                        ASRUtils::expr_type(ASRUtils::EXPR(v_var))), m_dims);
                member_type = ASRUtils::make_Array_t_util(al, loc, member_type_, m_dims, n_dims);
            }
        }

        ASR::symbol_t* member_ext = ASRUtils::import_struct_instance_member(al, member, current_scope);
        ASR::expr_t* value = nullptr;
        v = ASRUtils::symbol_get_past_external(v);
        if (v != nullptr &&
            ASR::down_cast<ASR::Variable_t>(v)->m_storage == ASR::storage_typeType::Parameter) {
            if (member_variable->m_symbolic_value != nullptr) {
                value = expr_value(member_variable->m_symbolic_value);
            }
            // Check for compile time value in StructConstant
            ASR::Variable_t *v_variable_s = ASR::down_cast<ASR::Variable_t>(v);
            if (v_variable_s->m_value != nullptr && ASR::is_a<ASR::StructConstant_t>(*v_variable_s->m_value)) {
                ASR::Struct_t *struct_s = ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(v_variable_s->m_type_declaration));
                std::string mem_name = ASRUtils::symbol_name(member);
                // Find the index i of the member in the Struct symbol and set value to ith argument of StructConstant
                size_t i = 0;
                for (i = 0; i < struct_s->n_members; i++) {
                    if (struct_s->m_members[i] == mem_name) {
                        break;
                    }
                }

                ASR::StructConstant_t *stc = ASR::down_cast<ASR::StructConstant_t>(v_variable_s->m_value);
                value = stc->m_args[i].m_value;
            }
        }
        return ASR::make_StructInstanceMember_t(al, loc, ASRUtils::EXPR(v_var),
            member_ext, member_type, value);
    }
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
    if (is_op_overloaded(op, intrinsic_op_name, curr_scope, left_struct)) {
        ASR::symbol_t* sym = curr_scope->resolve_symbol(intrinsic_op_name);
        ASR::symbol_t* orig_sym = ASRUtils::symbol_get_past_external(sym);
        ASR::symbol_t* orig_sym2 = nullptr;
        if (left_struct != nullptr) {
            ASR::Struct_t* temp_struct = left_struct;
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
        Vec<ASR::symbol_t*> op_overloading_procs; op_overloading_procs.reserve(al, 1);
        size_t n_interface_proc = 0;
        // functions that are declared by interface operator(+)
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
                if (cp->m_parent_symtab->get_counter() != left_struct->m_symtab->get_counter()) {
                    ASR::Struct_t* temp_struct = left_struct;
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
                        ASR::ttype_t* left_arg_type = ASRUtils::expr_type(func->m_args[0]);
                        ASR::ttype_t* right_arg_type = ASRUtils::expr_type(func->m_args[1]);
                        bool not_matching = (!ASRUtils::is_allocatable(left_type) && ASRUtils::is_allocatable(left_arg_type)) ||
                                            (!ASRUtils::is_allocatable(right_type) && ASRUtils::is_allocatable(right_arg_type)) ||
                                            (!ASRUtils::is_pointer(left_type) && ASRUtils::is_pointer(left_arg_type)) ||
                                            (!ASRUtils::is_pointer(right_type) && ASRUtils::is_pointer(right_arg_type));
                                            
                        ASR::ttype_t* left_type2 = ASRUtils::type_get_past_allocatable_pointer(left_type);
                        ASR::ttype_t* left_arg_type2 = ASRUtils::type_get_past_allocatable_pointer(left_arg_type);
                        ASR::ttype_t* right_type2 = ASRUtils::type_get_past_allocatable_pointer(right_type);
                        ASR::ttype_t* right_arg_type2 = ASRUtils::type_get_past_allocatable_pointer(right_arg_type);

                        not_matching = not_matching || (!is_elemental &&
                                       ((left_arg_type2->type != left_type2->type) ||
                                       (right_arg_type2->type != right_type2->type)));

                        left_type2 = ASRUtils::type_get_past_array(left_type2);
                        left_arg_type2 = ASRUtils::type_get_past_array(left_arg_type2);
                        right_type2 = ASRUtils::type_get_past_array(right_type2);
                        right_arg_type2 = ASRUtils::type_get_past_array(right_arg_type2);

                        if( !not_matching && (left_arg_type2->type == left_type2->type &&
                                                right_arg_type2->type == right_type2->type) ) {
                            // If all are StructTypes then the Struct symbols should match
                            if (ASR::is_a<ASR::StructType_t>(*left_type2) &&
                                ASR::is_a<ASR::StructType_t>(*right_type2) &&
                                ASR::is_a<ASR::StructType_t>(*left_arg_type2) &&
                                ASR::is_a<ASR::StructType_t>(*right_arg_type2)) {
                                    
                                ASR::Struct_t *left_sym = left_struct;
                                ASR::Struct_t *right_sym = right_struct;

                                ASR::Struct_t* left_arg_sym = ASR::down_cast<ASR::Struct_t>(
                                    ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(func->m_args[0])));
                                ASR::Struct_t* right_arg_sym = ASR::down_cast<ASR::Struct_t>(
                                    ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(func->m_args[1])));

                                if (!is_derived_type_similar(left_sym, left_arg_sym) ||
                                        !is_derived_type_similar(right_sym, right_arg_sym)) {
                                    break;
                                }
                            }
                            found = true;
                            Vec<ASR::call_arg_t> a_args;
                            a_args.reserve(al, 1);
                            ASR::call_arg_t left_call_arg, right_call_arg;
                            ASR::expr_t* self_arg = nullptr;
                            if (i < n_interface_proc) {
                                left_call_arg.loc = left->base.loc, left_call_arg.m_value = left;
                                a_args.push_back(al, left_call_arg);
                            }
                            right_call_arg.loc = right->base.loc, right_call_arg.m_value = right;
                            a_args.push_back(al, right_call_arg);
                            std::string func_name = to_lower(func->m_name);
                            if (i >= n_interface_proc) {
                                ASR::symbol_t* mem_ext = import_class_procedure(al, loc, orig_proc, curr_scope);
                                matched_func_name = ASRUtils::symbol_name(mem_ext);
                                self_arg = left;
                            } else if( curr_scope->resolve_symbol(func_name) ) {
                                matched_func_name = func_name;
                            } else {
                                std::string mangled_name = func_name + "@" + intrinsic_op_name;
                                matched_func_name = mangled_name;
                            }
                            ASR::symbol_t* a_name = curr_scope->resolve_symbol(matched_func_name);
                            if( a_name == nullptr ) {
                                if (i >= n_interface_proc) {
                                    err("Unable to resolve matched function for operator overloading, " + matched_func_name, loc);
                                } else {
                                    a_name = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(
                                                al, loc, curr_scope, s2c(al, matched_func_name), proc,
                                                ASRUtils::symbol_name(ASRUtils::get_asr_owner(proc)),
                                                nullptr, 0, func->m_name, ASR::accessType::Public));
                                    curr_scope->add_symbol(matched_func_name, a_name);
                                }
                            }
                            ASR::ttype_t *return_type = nullptr;
                            if( ASRUtils::get_FunctionType(func)->m_elemental &&
                                func->n_args >= 1 &&
                                ASRUtils::is_array(ASRUtils::expr_type(a_args[0].m_value)) ) {
                                ASR::dimension_t* array_dims;
                                size_t array_n_dims = ASRUtils::extract_dimensions_from_ttype(
                                ASRUtils::expr_type(a_args[0].m_value), array_dims);
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
                            asr = ASRUtils::make_FunctionCall_t_util(al, loc, a_name, sym,
                                                            a_args.p, a_args.n,
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

void process_overloaded_unary_minus_function(ASR::symbol_t* proc, ASR::expr_t* operand,
    ASR::ttype_t* operand_type, bool& found, Allocator& al, SymbolTable* curr_scope,
    const Location& loc, SetChar& current_function_dependencies,
    SetChar& current_module_dependencies, ASR::asr_t*& asr,
    const std::function<void (const std::string &, const Location &)> /*err*/) {
    ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(proc);
    std::string matched_func_name = "";
    if( func->n_args == 1 ) {
        ASR::ttype_t* operand_arg_type = ASRUtils::expr_type(func->m_args[0]);
        if (ASRUtils::check_equal_type(operand_arg_type, operand_type, func->m_args[0], operand)) {
            found = true;
            Vec<ASR::call_arg_t> a_args;
            a_args.reserve(al, 1);
            ASR::call_arg_t operand_call_arg;
            operand_call_arg.loc = operand->base.loc;
            operand_call_arg.m_value = operand;
            a_args.push_back(al, operand_call_arg);
            std::string func_name = to_lower(func->m_name);
            if( curr_scope->resolve_symbol(func_name) ) {
                matched_func_name = func_name;
            } else {
                std::string mangled_name = func_name + "@~sub";
                matched_func_name = mangled_name;
            }
            ASR::symbol_t* a_name = curr_scope->resolve_symbol(matched_func_name);
            if( a_name == nullptr ) {
                a_name = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(
                            al, loc, curr_scope, s2c(al, matched_func_name), proc,
                            ASRUtils::symbol_name(ASRUtils::get_asr_owner(proc)),
                            nullptr, 0, func->m_name, ASR::accessType::Public));
                curr_scope->add_symbol(matched_func_name, a_name);
            }
            ASR::ttype_t *return_type = nullptr;
            if( ASRUtils::get_FunctionType(func)->m_elemental &&
                func->n_args >= 1 &&
                ASRUtils::is_array(ASRUtils::expr_type(a_args[0].m_value)) ) {
                ASR::dimension_t* array_dims;
                size_t array_n_dims = ASRUtils::extract_dimensions_from_ttype(
                ASRUtils::expr_type(a_args[0].m_value), array_dims);
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
            asr = ASRUtils::make_FunctionCall_t_util(al, loc, a_name, proc,
                                            a_args.p, 1,
                                            return_type,
                                            nullptr, nullptr
                                            );
        }
    }
}

bool use_overloaded_unary_minus(ASR::expr_t* operand,
    SymbolTable* curr_scope, ASR::asr_t*& asr,
    Allocator &al, const Location& loc, SetChar& current_function_dependencies,
    SetChar& current_module_dependencies,
    const std::function<void (const std::string &, const Location &)> err) {
    ASR::ttype_t *operand_type = ASRUtils::expr_type(operand);
    ASR::symbol_t* sym = curr_scope->resolve_symbol("~sub");

    if (!sym) {
        if (ASR::is_a<ASR::StructType_t>(*operand_type) && !ASRUtils::is_class_type(operand_type)) {
            ASR::symbol_t* struct_t_sym = ASRUtils::symbol_get_past_external(
                ASRUtils::get_struct_sym_from_struct_expr(operand));
            if (ASR::is_a<ASR::Struct_t>(*struct_t_sym)) {
                ASR::Struct_t* struct_type_t = ASR::down_cast<ASR::Struct_t>(struct_t_sym);
                sym = struct_type_t->m_symtab->resolve_symbol("~sub");
                while (sym == nullptr && struct_type_t->m_parent != nullptr) {
                    struct_type_t = ASR::down_cast<ASR::Struct_t>(
                        ASRUtils::symbol_get_past_external(struct_type_t->m_parent));
                    sym = struct_type_t->m_symtab->resolve_symbol("~sub");
                }
                if (sym == nullptr) {
                    return false;
                }
                sym = ASR::down_cast<ASR::symbol_t>(
                    ASR::make_ExternalSymbol_t(al,
                                               loc,
                                               curr_scope,
                                               s2c(al, "~sub"),
                                               sym,
                                               struct_type_t->m_name,
                                               nullptr,
                                               0,
                                               s2c(al, "~sub"),
                                               ASR::accessType::Public));
                curr_scope->add_symbol("~sub", sym);
            } else {
                LCOMPILERS_ASSERT(false);
            }
        }
    }

    bool found = false;
    ASR::symbol_t* orig_sym = ASRUtils::symbol_get_past_external(sym);
    ASR::CustomOperator_t* gen_proc = ASR::down_cast<ASR::CustomOperator_t>(orig_sym);
    for( size_t i = 0; i < gen_proc->n_procs && !found; i++ ) {
        ASR::symbol_t* proc = gen_proc->m_procs[i];
        switch(proc->type) {
            case ASR::symbolType::Function: {
                process_overloaded_unary_minus_function(proc, operand, operand_type,
                    found, al, curr_scope, loc, current_function_dependencies,
                    current_module_dependencies, asr, err);
                break;
            }
            case ASR::symbolType::StructMethodDeclaration: {
                ASR::StructMethodDeclaration_t* class_procedure_t = ASR::down_cast<ASR::StructMethodDeclaration_t>(proc);
                process_overloaded_unary_minus_function(class_procedure_t->m_proc,
                    operand, operand_type, found, al, curr_scope, loc,
                    current_function_dependencies, current_module_dependencies, asr, err);
                break;
            }
            default: {
                err("While overloading binary operators only functions can be used",
                                    proc->base.loc);
            }
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
    const std::function<void (const std::string &, const Location &)> err, char* pass_arg=nullptr) {
    ASR::Function_t* subrout = ASR::down_cast<ASR::Function_t>(proc);
    std::string matched_subrout_name = "";
    if( subrout->n_args == 2 ) {
        ASR::ttype_t* target_arg_type = ASRUtils::expr_type(subrout->m_args[0]);
        ASR::ttype_t* value_arg_type = ASRUtils::expr_type(subrout->m_args[1]);
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
            if( curr_scope->resolve_symbol(subrout_name) ) {
                matched_subrout_name = subrout_name;
            } else {
                std::string mangled_name = subrout_name + "@~assign";
                matched_subrout_name = mangled_name;
            }
            ASR::symbol_t *a_name = curr_scope->resolve_symbol(matched_subrout_name);
            if( a_name == nullptr ) {
                err("Unable to resolve matched subroutine for assignment overloading, " + matched_subrout_name, loc);
            }
            if (ASRUtils::symbol_parent_symtab(a_name)->get_counter() != curr_scope->get_counter()) {
                ADD_ASR_DEPENDENCIES_WITH_NAME(curr_scope, a_name, current_function_dependencies, s2c(al, matched_subrout_name));
            }
            ASRUtils::insert_module_dependency(a_name, al, current_module_dependencies);
            ASRUtils::set_absent_optional_arguments_to_null(a_args, subrout, al);
            asr = ASRUtils::make_SubroutineCall_t_util(al, loc, a_name, sym,
                                            a_args.p, 2, nullptr, nullptr, false);
        }
    }
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
    if(!sym) {
        if( ASR::is_a<ASR::StructType_t>(*target_type) && !ASRUtils::is_class_type(target_type) ) {
            ASR::Struct_t* target_struct = ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(target)));
            sym = target_struct->m_symtab->resolve_symbol("~assign");
            expr_dt = target;
        } else if( ASR::is_a<ASR::StructType_t>(*value_type) && !ASRUtils::is_class_type(value_type) ) {
            ASR::Struct_t* value_struct = ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(value)));
            sym = value_struct->m_symtab->resolve_symbol("~assign");
            expr_dt = value;
        }
    }
    if (sym) {
        ASR::symbol_t* orig_sym = ASRUtils::symbol_get_past_external(sym);
        ASR::CustomOperator_t* gen_proc = ASR::down_cast<ASR::CustomOperator_t>(orig_sym);
        for( size_t i = 0; i < gen_proc->n_procs && !found; i++ ) {
            ASR::symbol_t* proc = ASRUtils::symbol_get_past_external(gen_proc->m_procs[i]);
            switch( proc->type ) {
                case ASR::symbolType::Function: {
                    process_overloaded_assignment_function(proc, target, value, target_type,
                        value_type, found, al, target->base.loc, value->base.loc, curr_scope,
                        current_function_dependencies, current_module_dependencies, asr, sym,
                        loc, expr_dt, err);
                    break;
                }
                case ASR::symbolType::StructMethodDeclaration: {
                    ASR::StructMethodDeclaration_t* class_proc = ASR::down_cast<ASR::StructMethodDeclaration_t>(proc);
                    ASR::symbol_t* proc_func = ASR::down_cast<ASR::StructMethodDeclaration_t>(proc)->m_proc;
                    process_overloaded_assignment_function(proc_func, target, value, target_type,
                        value_type, found, al, target->base.loc, value->base.loc, curr_scope,
                        current_function_dependencies, current_module_dependencies, asr, proc_func, loc,
                        expr_dt, err, class_proc->m_self_argument);
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

void process_overloaded_read_write_function(std::string &read_write, ASR::symbol_t* proc, Vec<ASR::expr_t*> &args,
    ASR::ttype_t* arg_type, bool& found, Allocator& al, const Location& arg_loc,
    SymbolTable* curr_scope, SetChar& current_function_dependencies,
    SetChar& current_module_dependencies, ASR::asr_t*& asr, ASR::symbol_t* sym, const Location& loc, ASR::expr_t* expr_dt,
    const std::function<void (const std::string &, const Location &)> err, char* pass_arg=nullptr) {
    ASR::Function_t* subrout = ASR::down_cast<ASR::Function_t>(proc);
    std::string matched_subrout_name = "";
    ASR::ttype_t* func_arg_type = ASRUtils::expr_type(subrout->m_args[0]);
    if( ASRUtils::types_equal(func_arg_type, arg_type, subrout->m_args[0], args[0]) ) {
        std::string arg0_name = ASRUtils::symbol_name(ASR::down_cast<ASR::Var_t>(subrout->m_args[0])->m_v);
        if( pass_arg != nullptr ) {
            std::string pass_arg_str = std::string(pass_arg);
            if( (arg0_name == pass_arg_str && args[0] != expr_dt) ) {
                err(std::string(subrout->m_name) + " is not a procedure of " +
                    ASRUtils::type_to_str_fortran_expr(arg_type, args[0]),
                    loc);
            }
        }
        found = true;
        Vec<ASR::call_arg_t> a_args;
        a_args.reserve(al, args.size());
        for (size_t i = 0; i < args.size(); i++) {
            ASR::call_arg_t arg;
            arg.loc = arg_loc;
            arg.m_value = args[i];
            a_args.push_back(al, arg);
        }
        std::string subrout_name = to_lower(subrout->m_name);
        if( curr_scope->resolve_symbol(subrout_name) ) {
            matched_subrout_name = subrout_name;
        } else {
            std::string mangled_name = subrout_name + "@" + read_write;
            matched_subrout_name = mangled_name;
        }
        ASR::symbol_t *a_name = curr_scope->resolve_symbol(matched_subrout_name);
        if( a_name == nullptr ) {
            err("Unable to resolve matched subroutine for read/write overloading, " + matched_subrout_name, loc);
        }
        if (ASRUtils::symbol_parent_symtab(a_name)->get_counter() != curr_scope->get_counter()) {
            ADD_ASR_DEPENDENCIES_WITH_NAME(curr_scope, a_name, current_function_dependencies, s2c(al, matched_subrout_name));
        }
        ASRUtils::insert_module_dependency(a_name, al, current_module_dependencies);
        ASRUtils::set_absent_optional_arguments_to_null(a_args, subrout, al);
        asr = ASRUtils::make_SubroutineCall_t_util(al, loc, a_name, sym,
                                        a_args.p, a_args.n, nullptr, nullptr, false);
    }
}

bool use_overloaded_file_read_write(std::string &read_write, Vec<ASR::expr_t*> args,
                               SymbolTable* curr_scope, ASR::asr_t*& asr,
                               Allocator &al, const Location& loc,
                               SetChar& current_function_dependencies,
                               SetChar& current_module_dependencies,
                               const std::function<void (const std::string &, const Location &)> err) {
    ASR::ttype_t *arg_type = ASRUtils::type_get_past_allocatable(ASRUtils::expr_type(args[0]));
    bool found = false;
    ASR::symbol_t* sym = curr_scope->resolve_symbol(read_write);
    ASR::expr_t* expr_dt = nullptr;
    if( sym == nullptr ) {
        if( ASR::is_a<ASR::StructType_t>(*arg_type) && !ASRUtils::is_class_type(arg_type) ) {
            ASR::Struct_t* arg_struct = ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(args[0])));
            sym = arg_struct->m_symtab->resolve_symbol(read_write);
            expr_dt = args[0];
        }
    }
    if (sym) {
        ASR::symbol_t* orig_sym = ASRUtils::symbol_get_past_external(sym);
        auto process_proc_list = [&](ASR::symbol_t** procs, size_t n_procs) {
            for( size_t i = 0; i < n_procs && !found; i++ ) {
                ASR::symbol_t* proc = ASRUtils::symbol_get_past_external(procs[i]);
                switch( proc->type ) {
                    case ASR::symbolType::Function: {
                        process_overloaded_read_write_function(read_write, proc, args, arg_type,
                            found, al, args[0]->base.loc, curr_scope,
                            current_function_dependencies, current_module_dependencies, asr, sym,
                            loc, expr_dt, err);
                        break;
                    }
                    case ASR::symbolType::StructMethodDeclaration: {
                        ASR::StructMethodDeclaration_t* class_proc = ASR::down_cast<ASR::StructMethodDeclaration_t>(proc);
                        ASR::symbol_t* proc_func = ASR::down_cast<ASR::StructMethodDeclaration_t>(proc)->m_proc;
                        process_overloaded_read_write_function(read_write, proc_func, args, arg_type,
                            found, al, args[0]->base.loc, curr_scope,
                            current_function_dependencies, current_module_dependencies, asr, proc_func, loc,
                            expr_dt, err, class_proc->m_self_argument);
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
    }
    return found;
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
    if (is_op_overloaded(op, intrinsic_op_name, curr_scope, left_struct)) {
        ASR::symbol_t* sym = curr_scope->resolve_symbol(intrinsic_op_name);
        ASR::symbol_t* orig_sym = ASRUtils::symbol_get_past_external(sym);
        ASR::symbol_t* orig_sym2 = nullptr;
        if ( left_struct != nullptr) {
            ASR::Struct_t* temp_struct = left_struct;
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
                if (cp->m_parent_symtab->get_counter() != left_struct->m_symtab->get_counter()) {
                    // It may be overided in the derived class
                    ASR::Struct_t* temp_struct = left_struct;
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
                        ASR::ttype_t* left_arg_type = ASRUtils::expr_type(func->m_args[0]);
                        ASR::ttype_t* right_arg_type = ASRUtils::expr_type(func->m_args[1]);
                        bool not_matching = (!ASRUtils::is_allocatable(left_type) && ASRUtils::is_allocatable(left_arg_type)) ||
                                            (!ASRUtils::is_allocatable(right_type) && ASRUtils::is_allocatable(right_arg_type)) ||
                                            (!ASRUtils::is_pointer(left_type) && ASRUtils::is_pointer(left_arg_type)) ||
                                            (!ASRUtils::is_pointer(right_type) && ASRUtils::is_pointer(right_arg_type));
                                            
                        ASR::ttype_t* left_type2 = ASRUtils::type_get_past_allocatable_pointer(left_type);
                        ASR::ttype_t* left_arg_type2 = ASRUtils::type_get_past_allocatable_pointer(left_arg_type);
                        ASR::ttype_t* right_type2 = ASRUtils::type_get_past_allocatable_pointer(right_type);
                        ASR::ttype_t* right_arg_type2 = ASRUtils::type_get_past_allocatable_pointer(right_arg_type);

                        // Check for array type
                        not_matching = not_matching || (!is_elemental &&
                                       ((left_arg_type2->type != left_type2->type) ||
                                       (right_arg_type2->type != right_type2->type)));

                        // Get element type and compare
                        left_type2 = ASRUtils::type_get_past_array(left_type2);
                        left_arg_type2 = ASRUtils::type_get_past_array(left_arg_type2);
                        right_type2 = ASRUtils::type_get_past_array(right_type2);
                        right_arg_type2 = ASRUtils::type_get_past_array(right_arg_type2);

                        if( !not_matching && (left_arg_type2->type == left_type2->type &&
                                                right_arg_type2->type == right_type2->type) ) {
                            // If all are StructTypes then the Struct symbols should match
                            if (ASR::is_a<ASR::StructType_t>(*left_type2) &&
                                ASR::is_a<ASR::StructType_t>(*right_type2) &&
                                ASR::is_a<ASR::StructType_t>(*left_arg_type2) &&
                                ASR::is_a<ASR::StructType_t>(*right_arg_type2)) {
                                    
                                ASR::Struct_t *left_sym = left_struct;
                                ASR::Struct_t *right_sym = right_struct;

                                ASR::Struct_t* left_arg_sym = ASR::down_cast<ASR::Struct_t>(
                                    ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(func->m_args[0])));
                                ASR::Struct_t* right_arg_sym = ASR::down_cast<ASR::Struct_t>(
                                    ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(func->m_args[1])));

                                if (!is_derived_type_similar(left_sym, left_arg_sym) ||
                                        !is_derived_type_similar(right_sym, right_arg_sym)) {
                                    break;
                                }
                            }
                            found = true;
                            Vec<ASR::call_arg_t> a_args;
                            a_args.reserve(al, 1);
                            ASR::call_arg_t left_call_arg, right_call_arg;
                            ASR::expr_t* self_arg = nullptr;
                            if (i < n_interface_proc) {
                                left_call_arg.loc = left->base.loc, left_call_arg.m_value = left;
                                a_args.push_back(al, left_call_arg);
                            }
                            right_call_arg.loc = right->base.loc, right_call_arg.m_value = right;
                            a_args.push_back(al, right_call_arg);
                            std::string func_name = to_lower(func->m_name);
                            if (i >= n_interface_proc) {
                                ASR::symbol_t* mem_ext = import_class_procedure(al, loc, orig_proc, curr_scope);
                                matched_func_name = ASRUtils::symbol_name(mem_ext);
                                self_arg = left;
                            } else if( curr_scope->resolve_symbol(func_name) ) {
                                matched_func_name = func_name;
                            } else {
                                std::string mangled_name = func_name + "@" + intrinsic_op_name;
                                matched_func_name = mangled_name;
                            }
                            ASR::symbol_t* a_name = curr_scope->resolve_symbol(matched_func_name);
                            if( a_name == nullptr ) {
                                if (i >= n_interface_proc) {
                                    err("Unable to resolve matched function for operator overloading, " + matched_func_name, loc);
                                } else {
                                    a_name = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(
                                                al, loc, curr_scope, s2c(al, matched_func_name), proc,
                                                ASRUtils::symbol_name(ASRUtils::get_asr_owner(proc)),
                                                nullptr, 0, func->m_name, ASR::accessType::Public));
                                    curr_scope->add_symbol(matched_func_name, a_name);
                                }
                            }
                            ASR::ttype_t *return_type = nullptr;
                            if( ASRUtils::get_FunctionType(func)->m_elemental &&
                                func->n_args >= 1 &&
                                ASRUtils::is_array(ASRUtils::expr_type(a_args[0].m_value)) ) {
                                ASR::dimension_t* array_dims;
                                size_t array_n_dims = ASRUtils::extract_dimensions_from_ttype(
                                ASRUtils::expr_type(a_args[0].m_value), array_dims);
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
                            asr = ASRUtils::make_FunctionCall_t_util(al, loc, a_name, sym,
                                                            a_args.p, a_args.n,
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
    ASR::ttype_t* left_type = ASRUtils::type_get_past_pointer(ASRUtils::expr_type(left));
    ASR::ttype_t* right_type = ASRUtils::type_get_past_pointer(ASRUtils::expr_type(right));
    ASR::Struct_t* left_struct = nullptr;
    if (ASR::is_a<ASR::StructType_t>(*left_type)) {
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
                    std::string matched_func_name = "";
                    if (func->n_args == 2) {
                        ASR::ttype_t* left_arg_type = ASRUtils::expr_type(func->m_args[0]);
                        ASR::ttype_t* right_arg_type = ASRUtils::expr_type(func->m_args[1]);
                        if ((left_arg_type->type == left_type->type
                             && right_arg_type->type == right_type->type)
                            || (ASRUtils::is_class_type(left_arg_type)
                                && ASR::is_a<ASR::StructType_t>(*left_type))
                            || (ASRUtils::is_class_type(right_arg_type)
                                && ASR::is_a<ASR::StructType_t>(*right_type))
                            || (ASR::is_a<ASR::StructType_t>(*left_arg_type)
                                && ASRUtils::is_class_type(left_type))
                            || (ASR::is_a<ASR::StructType_t>(*right_arg_type)
                                && ASRUtils::is_class_type(right_type))) {
                            if (ASR::is_a<ASR::StructType_t>(*left_type)
                                && ASR::is_a<ASR::StructType_t>(*right_type)
                                && ASR::is_a<ASR::StructType_t>(*left_arg_type)
                                && ASR::is_a<ASR::StructType_t>(*right_arg_type)) {
                                ASR::Struct_t* left_sym = ASR::down_cast<ASR::Struct_t>(
                                    ASRUtils::symbol_get_past_external(
                                        ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(left))));
                                ASR::Struct_t* right_sym = ASR::down_cast<ASR::Struct_t>(
                                    ASRUtils::symbol_get_past_external(ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(right))));
                                ASR::Struct_t* left_arg_sym = ASR::down_cast<ASR::Struct_t>(
                                    ASRUtils::symbol_get_past_external(ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(func->m_args[0]))));
                                ASR::Struct_t* right_arg_sym = ASR::down_cast<ASR::Struct_t>(
                                    ASRUtils::symbol_get_past_external(
                                        ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(func->m_args[1]))));
                                        
                                if (!is_derived_type_similar(left_sym, left_arg_sym) ||
                                        !is_derived_type_similar(right_sym, right_arg_sym)) {
                                    break;
                                }
                            }
                            found = true;
                            Vec<ASR::call_arg_t> a_args;
                            a_args.reserve(al, 2);
                            a_args.push_back(al, ASR::call_arg_t{ left->base.loc, left });
                            a_args.push_back(al, ASR::call_arg_t{ right->base.loc, right });
                            std::string func_name = to_lower(func->m_name);
                            if (curr_scope->resolve_symbol(func_name)) {
                                matched_func_name = func_name;
                            } else {
                                matched_func_name = func_name + "@" + intrinsic_op_name;
                            }
                            ASR::symbol_t* a_name = curr_scope->resolve_symbol(matched_func_name);
                            if (a_name == nullptr) {
                                err("Unable to resolve matched function for overloaded logical operator: "
                                        + matched_func_name,
                                    loc);
                            }
                            ASR::ttype_t* return_type = nullptr;
                            if (ASRUtils::get_FunctionType(func)->m_elemental && func->n_args >= 1
                                && ASRUtils::is_array(ASRUtils::expr_type(a_args[0].m_value))) {
                                ASR::dimension_t* array_dims;
                                size_t array_n_dims = ASRUtils::extract_dimensions_from_ttype(
                                    ASRUtils::expr_type(a_args[0].m_value), array_dims);
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
                // Otherwise this should not be nullptr
                ASR::ttype_t *arg1 = ASRUtils::expr_type(args[i].m_value);
                ASR::ttype_t *arg2 = v->m_type;
                ASR::symbol_t* s1 = ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(args[i].m_value));
                ASR::symbol_t* s2 = nullptr;
                ASR::ttype_t* arg2_ext = ASRUtils::extract_type(arg2);
                bool is_elemental = ASRUtils::get_FunctionType(sub)->m_elemental;
                if (ASR::is_a<ASR::StructType_t>(*arg2_ext)) {
                    if ((ASRUtils::is_allocatable(arg2) && !ASRUtils::is_allocatable(arg1)) ||
                            (ASRUtils::is_array(arg2) && !ASRUtils::is_array(arg1)) ||
                            (!is_elemental && !ASRUtils::is_array(arg2) && ASRUtils::is_array(arg1))) {
                        return false;
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
                } else if (!types_equal(arg1, arg2, args[i].m_value, sub.m_args[i], !ASRUtils::get_FunctionType(sub)->m_elemental)) {
                    return false;
                }
            } else if (ASR::is_a<ASR::Function_t>(*sub_arg_sym)) {
                ASR::Function_t* f = ASR::down_cast<ASR::Function_t>(sub_arg_sym);

                ASR::ttype_t *arg1 = ASRUtils::expr_type(args[i].m_value);
                ASR::ttype_t *arg2 = f->m_function_signature;

                // Check if actual argument is an implicit interface procedure
                // (FunctionType with no arg types and Interface deftype).
                // Implicit interfaces are compatible with any explicit interface
                // - actual compatibility is checked at call site.
                bool arg1_is_implicit = false;
                if (ASR::is_a<ASR::FunctionType_t>(*arg1)) {
                    ASR::FunctionType_t* ft = ASR::down_cast<ASR::FunctionType_t>(arg1);
                    arg1_is_implicit = (ft->n_arg_types == 0 &&
                        ft->m_deftype == ASR::deftypeType::Interface);
                }

                if (!arg1_is_implicit) {
                    Allocator al(512);
                    if (!types_equal(arg1, arg2, args[i].m_value,
                        ASRUtils::get_expr_from_sym(al, &f->base), false)) {
                        return false;
                    }
                }
            }
        }
        for( ; i < sub.n_args; i++ ) {
            ASR::Variable_t *v = ASRUtils::EXPR2VAR(sub.m_args[i]);
            if( v->m_presence != ASR::presenceType::Optional ) {
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
    ASR::symbol_t *final_sym;
    final_sym = g->m_procs[idx];
    LCOMPILERS_ASSERT(ASR::is_a<ASR::Function_t>(*final_sym));
    bool is_subroutine = ASR::down_cast<ASR::Function_t>(final_sym)->m_return_var == nullptr;
    ASR::ttype_t *return_type = nullptr;
    ASR::Function_t* func = nullptr;
    if( ASR::is_a<ASR::Function_t>(*final_sym) ) {
        func = ASR::down_cast<ASR::Function_t>(final_sym);
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
    }
    // Create ExternalSymbol for the final subroutine:
    // We mangle the new ExternalSymbol's local name as:
    //   generic_procedure_local_name @
    //     specific_procedure_remote_name
    std::string local_sym = std::string(p->m_name) + "@"
        + ASRUtils::symbol_name(final_sym);
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
            p->m_module_name, nullptr, 0, ASRUtils::symbol_name(final_sym),
            ASR::accessType::Private
            );
        final_sym = ASR::down_cast<ASR::symbol_t>(sub);
        current_scope->add_symbol(local_sym, final_sym);
    } else {
        final_sym = current_scope->get_symbol(local_sym);
    }
    // ASRUtils::insert_module_dependency(v, al, current_module_dependencies);
    if( is_subroutine ) {
        if( func ) {
            ASRUtils::set_absent_optional_arguments_to_null(args, func, al);
        }
        return ASRUtils::make_SubroutineCall_t_util(al, loc, final_sym,
                                        v, args.p, args.size(),
                                        nullptr, nullptr, false);
    } else {
        if( func ) {
            ASRUtils::set_absent_optional_arguments_to_null(args, func, al);
        }
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
            int64_t v = ASR::down_cast<ASR::RealConstant_t>(
                        ASRUtils::expr_value(a_arg))->m_r;
            value = ASR::down_cast<ASR::expr_t>(
                    ASR::make_IntegerConstant_t(al, a_loc, v, a_type));
        } else if (a_kind == ASR::cast_kindType::RealToReal) {
            double v = ASR::down_cast<ASR::RealConstant_t>(
                       ASRUtils::expr_value(a_arg))->m_r;
            value = ASR::down_cast<ASR::expr_t>(
                    ASR::make_RealConstant_t(al, a_loc, v, a_type));
        } else if (a_kind == ASR::cast_kindType::RealToComplex) {
            double double_value = ASR::down_cast<ASR::RealConstant_t>(
                                  ASRUtils::expr_value(a_arg))->m_r;
            value = ASR::down_cast<ASR::expr_t>(ASR::make_ComplexConstant_t(al, a_loc,
                        double_value, 0, a_type));
        } else if (a_kind == ASR::cast_kindType::IntegerToReal) {
            // TODO: Clashes with the pow functions
            int64_t int_value = ASR::down_cast<ASR::IntegerConstant_t>(ASRUtils::expr_value(a_arg))->m_n;
            value = ASR::down_cast<ASR::expr_t>(ASR::make_RealConstant_t(al, a_loc, (double)int_value, a_type));
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

    return ASR::make_Cast_t(al, a_loc, a_arg, a_kind, a_type, value);
}

ASR::symbol_t* import_class_procedure(Allocator &al, const Location& loc,
        ASR::symbol_t* original_sym, SymbolTable *current_scope) {
    if( original_sym && (ASR::is_a<ASR::StructMethodDeclaration_t>(*original_sym) ||
        (ASR::is_a<ASR::Variable_t>(*original_sym) &&
         ASR::is_a<ASR::FunctionType_t>(*ASRUtils::symbol_type(original_sym)))) ) {
        std::string class_proc_name;
        // StructMethodDeclaration name might be same if the procedure is overridden, use proc_name instead
        if (ASR::is_a<ASR::StructMethodDeclaration_t>(*original_sym)) {
            class_proc_name = std::string(ASR::down_cast<ASR::StructMethodDeclaration_t>(original_sym)->m_proc_name);
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
                ASR::symbol_t* imported_sym = ASR::down_cast<ASR::symbol_t>(
                    ASR::make_ExternalSymbol_t(
                        al, loc, current_scope, s2c(al, imported_proc_name),
                        original_sym, s2c(al, module_name), nullptr, 0,
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
    switch (ttype->type) {
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
    ASR::ttype_t* dest_shape_type = ASRUtils::TYPE(ASR::make_Array_t(al, loc,
        ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)), dims.p, dims.size(),
        is_value_character_array && !is_value_constant(expr2) ? ASR::array_physical_typeType::StringArraySinglePointer: ASR::array_physical_typeType::FixedSizeArray));

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
                is_value_character_array ? ASR::array_physical_typeType::PointerArray: ASR::array_physical_typeType::FixedSizeArray));
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
            // TODO: Make this more general; do not check for SIMDArray
            ret_type = ASRUtils::duplicate_type(al, expr1_type);
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

void get_sliced_indices(ASR::ArraySection_t* arr_sec, std::vector<size_t> &indices) {
    for (size_t i = 0; i < arr_sec->n_args; i++) {
        if (arr_sec->m_args[i].m_step != nullptr) {
            indices.push_back(i + 1);
        }
    }
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
        } else {
            const_elements += 1;
        }
    }
    // Include scalar elements in the implied-do body so mixed forms compute correctly per iteration.
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

ASR::asr_t* make_ArraySize_t_util(
    Allocator &al, const Location &a_loc, ASR::expr_t* a_v,
    ASR::expr_t* a_dim, ASR::ttype_t* a_type, ASR::expr_t* a_value,
    bool for_type) {
    int dim = -1;
    bool is_dimension_constant = (a_dim != nullptr) && ASRUtils::extract_value(
        ASRUtils::expr_value(a_dim), dim);
    ASR::ttype_t* array_func_type = nullptr;
    if( ASR::is_a<ASR::ArrayPhysicalCast_t>(*a_v) ) {
        a_v = ASR::down_cast<ASR::ArrayPhysicalCast_t>(a_v)->m_arg;
    }
    if ( ASR::is_a<ASR::IntrinsicArrayFunction_t>(*a_v) && for_type ) {
        ASR::IntrinsicArrayFunction_t* af = ASR::down_cast<ASR::IntrinsicArrayFunction_t>(a_v);
        int64_t dim_index = ASRUtils::IntrinsicArrayFunctionRegistry::get_dim_index(
            static_cast<ASRUtils::IntrinsicArrayFunctions>(af->m_arr_intrinsic_id));
        ASR::expr_t* af_dim = nullptr;
        if( dim_index == 1 && (size_t) dim_index < af->n_args && af->m_args[dim_index] != nullptr ) {
            af_dim = af->m_args[dim_index];
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
                LCOMPILERS_ASSERT(m_dims[0].m_length);
                ASR::expr_t* result = m_dims[0].m_length;
                for( size_t i = 1; i < n_dims; i++ ) {
                    LCOMPILERS_ASSERT(m_dims[i].m_length);
                    result = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, a_loc,
                        result, ASR::binopType::Mul, m_dims[i].m_length, a_type, nullptr));
                }
                return &(result->base);
            } else if( is_dimension_constant ) {
                LCOMPILERS_ASSERT(m_dims[dim - 1].m_length);
                return &(m_dims[dim - 1].m_length->base);
            }
        }
    } else if( ASR::is_a<ASR::IntrinsicElementalFunction_t>(*a_v) && for_type ) {
        ASR::IntrinsicElementalFunction_t* elemental = ASR::down_cast<ASR::IntrinsicElementalFunction_t>(a_v);
        for( size_t i = 0; i < elemental->n_args; i++ ) {
            if( ASRUtils::is_array(ASRUtils::expr_type(elemental->m_args[i])) ) {
                a_v = elemental->m_args[i];
                break;
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
                    start = CastingUtil::perform_casting(start, a_type, al, a_loc);
                    end = CastingUtil::perform_casting(end, a_type, al, a_loc);
                    d = CastingUtil::perform_casting(d, a_type, al, a_loc);
                    ASR::expr_t* endminusstart = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(
                        al, a_loc, end, ASR::binopType::Sub, start, a_type, nullptr));
                    ASR::expr_t* byd = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(
                        al, a_loc, endminusstart, ASR::binopType::Div, d, a_type, nullptr));
                    plus1 = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(
                        al, a_loc, byd, ASR::binopType::Add, ASRUtils::EXPR(const1), a_type, nullptr));
                }
                size = ASR::make_IntegerBinOp_t(al, a_loc, ASRUtils::EXPR(size),
                    ASR::binopType::Mul, plus1, a_type, nullptr);
            }
            // ArraySize should not be negative
            ASR::expr_t* const0 = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, a_loc, 0, a_type));
            ASRBuilder builder(al, a_loc);
            size = (ASR::asr_t *)builder.Max(const0, ASRUtils::EXPR(size));
            return size;
        } else if( is_dimension_constant ) {
            ASR::asr_t* const1 = ASR::make_IntegerConstant_t(al, a_loc, 1, a_type);
            ASR::expr_t* start = array_section_t->m_args[dim - 1].m_left;
            ASR::expr_t* end = array_section_t->m_args[dim - 1].m_right;
            ASR::expr_t* d = array_section_t->m_args[dim - 1].m_step;

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
            start = CastingUtil::perform_casting(start, a_type, al, a_loc);
            end = CastingUtil::perform_casting(end, a_type, al, a_loc);
            d = CastingUtil::perform_casting(d, a_type, al, a_loc);
            ASR::expr_t* endminusstart = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(
                al, a_loc, end, ASR::binopType::Sub, start, a_type, nullptr));
            ASR::expr_t* byd = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(
                al, a_loc, endminusstart, ASR::binopType::Div, d, a_type, nullptr));
            return ASR::make_IntegerBinOp_t(al, a_loc, byd, ASR::binopType::Add,
                ASRUtils::EXPR(const1), a_type, nullptr);
        }
    }
    if( ASR::is_a<ASR::ArrayItem_t>(*a_v) ) {
        ASR::ArrayItem_t* array_item_t = ASR::down_cast<ASR::ArrayItem_t>(a_v);
        LCOMPILERS_ASSERT(ASRUtils::is_array(array_item_t->m_type));
        if( for_type ) {
            LCOMPILERS_ASSERT(!ASRUtils::is_allocatable(array_item_t->m_type) &&
                              !ASRUtils::is_pointer(array_item_t->m_type));
        }
        if( a_dim == nullptr ) {
            ASR::asr_t* const1 = ASR::make_IntegerConstant_t(al, a_loc, 1, a_type);
            ASR::asr_t* size = const1;
            for( size_t i = 0; i < array_item_t->n_args; i++ ) {
                ASR::expr_t* end = ASRUtils::EXPR(make_ArraySize_t_util(al, a_loc,
                    array_item_t->m_args[i].m_right, a_dim, a_type, nullptr, for_type));
                size = ASR::make_IntegerBinOp_t(al, a_loc, ASRUtils::EXPR(size),
                    ASR::binopType::Mul, end, a_type, nullptr);
            }
            return size;
        } else if( is_dimension_constant ) {
            return make_ArraySize_t_util(al, a_loc,
                array_item_t->m_args[dim].m_right,
                nullptr, a_type, nullptr, for_type);
        }
    }
    if( is_binop_expr(a_v) && for_type ) {
        if( !ASR::is_a<ASR::ArrayBroadcast_t>(*extract_member_from_binop(a_v, 1)) &&
            (ASR::is_a<ASR::Var_t>(*extract_member_from_binop(a_v, 1)) ||
            ASR::is_a<ASR::ArraySection_t>(*extract_member_from_binop(a_v, 1))) ) {
            return make_ArraySize_t_util(al, a_loc,  extract_member_from_binop(a_v, 1),
                                         a_dim, a_type, a_value, for_type);
        } else {
            return make_ArraySize_t_util(al, a_loc, extract_member_from_binop(a_v, 0), a_dim, a_type, a_value, for_type);
        }
    } else if( is_unaryop_expr(a_v) && for_type ) {
        return make_ArraySize_t_util(al, a_loc, extract_member_from_unaryop(a_v), a_dim, a_type, a_value, for_type);
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
    member_types.reserve(al, derived_type->m_symtab->get_scope().size());

    for (auto const& sym : derived_type->m_symtab->get_scope()) {
        if (ASR::is_a<ASR::Variable_t>(*sym.second)) {
            ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(
                ASRUtils::symbol_get_past_external(sym.second));
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
            member_types.push_back(al, var->m_type);
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
                               derived_type_name == "~unlimited_polymorphic_type" ? true : false));
}

ASR::expr_t* get_compile_time_array_size(Allocator& al, ASR::ttype_t* array_type){
    LCOMPILERS_ASSERT(ASR::is_a<ASR::Array_t>(*
        ASRUtils::type_get_past_allocatable_pointer(array_type)));
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
        ASR::is_a<ASR::ArrayConstant_t>(*x)) {
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
        LCOMPILERS_ASSERT(false);
        return nullptr;
    }
}


//Initialize pointer to zero so that it can be initialized in first call to get_instance
ASRUtils::LabelGenerator* ASRUtils::LabelGenerator::label_generator = nullptr;

} // namespace ASRUtils


} // namespace LCompilers
