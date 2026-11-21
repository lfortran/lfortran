#ifndef LFORTRAN_PASS_INTRINSIC_FUNCTION_REGISTRY_H
#define LFORTRAN_PASS_INTRINSIC_FUNCTION_REGISTRY_H

#include <libasr/pass/intrinsic_function_ids.h>

#include <string>

// The registries are defined in intrinsic_function_registry.cpp, the only
// file that needs the implementations of all the intrinsic functions
// (intrinsic_functions.h). Include that header directly to use the functions
// of one intrinsic, such as `Abs::create_Abs`.

namespace LCompilers {

namespace ASRUtils {

std::string get_intrinsic_name(int64_t x);

namespace IntrinsicElementalFunctionRegistry {

    bool is_intrinsic_function(const std::string& name);

    bool is_intrinsic_function(int64_t id);

    create_intrinsic_function get_create_function(const std::string& name);

    create_intrinsic_function get_create_function(int64_t id);

    verify_function get_verify_function(int64_t id);

    impl_function get_instantiate_function(int64_t id);

    // The function that computes the compile-time value of the intrinsic
    // `id` from the values of its arguments (`m_args` of its
    // IntrinsicElementalFunction) and its type; nullptr for the intrinsics
    // without one.
    eval_intrinsic_function get_eval_function(int64_t id);

    std::string get_intrinsic_function_name(int64_t id);

} // namespace IntrinsicElementalFunctionRegistry

namespace IntrinsicImpureFunctionRegistry {

    bool is_intrinsic_function(const std::string& name);

    create_intrinsic_function get_create_function(const std::string& name);

    // Only impure intrinsic functions with observable side effects are
    // lowered by the intrinsic_function pass and have a verify function;
    // the others are lowered by the backends.
    verify_impure_function get_verify_function(int64_t id);

    instantiate_impure_function get_instantiate_function(int64_t id);

    // True if a reference to the intrinsic is not side-effect free, so it is
    // not allowed inside a PURE procedure.
    bool has_side_effects(int64_t id);

} // namespace IntrinsicImpureFunctionRegistry

std::string get_impure_intrinsic_name(int64_t x);

} // namespace ASRUtils

} // namespace LCompilers

#endif // LFORTRAN_PASS_INTRINSIC_FUNCTION_REGISTRY_H
