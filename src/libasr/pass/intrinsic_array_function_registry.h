#ifndef LFORTRAN_PASS_INTRINSIC_ARRAY_FUNCTION_REGISTRY_H
#define LFORTRAN_PASS_INTRINSIC_ARRAY_FUNCTION_REGISTRY_H

#include <libasr/pass/intrinsic_function_registry.h>

#include <string>

// The registry is defined in intrinsic_array_function_registry.cpp. Include
// intrinsic_array_functions.h to use the functions of one array intrinsic.

namespace LCompilers {

namespace ASRUtils {

std::string get_array_intrinsic_name(int64_t x);

namespace IntrinsicArrayFunctionRegistry {

    bool is_intrinsic_function(const std::string& name);

    create_intrinsic_function get_create_function(const std::string& name);

    impl_function get_instantiate_function(int64_t id);

    verify_array_function get_verify_function(int64_t id);

    // The function that computes the compile-time value of the intrinsic
    // `id` from the values of its arguments (`m_args` of its
    // IntrinsicArrayFunction) and its type; nullptr for the intrinsics
    // without one.
    eval_intrinsic_function get_eval_function(int64_t id);

    /*
        The function gives the index of the dim a.k.a axis argument
        for the intrinsic with the given id. Most of the time
        dim is specified via second argument (i.e., index 1) but
        still its better to encapsulate it in the following
        function and then call it to get the index of the dim
        argument whenever needed. This helps in limiting
        the API changes of the intrinsic to this function only.
    */
    static inline int get_dim_index(IntrinsicArrayFunctions id) {
        if( id == IntrinsicArrayFunctions::Any ||
            id == IntrinsicArrayFunctions::All ||
            id == IntrinsicArrayFunctions::Sum ||
            id == IntrinsicArrayFunctions::Product ||
            id == IntrinsicArrayFunctions::Iparity ||
            id == IntrinsicArrayFunctions::Iany ||
            id == IntrinsicArrayFunctions::Iall ||
            id == IntrinsicArrayFunctions::MaxVal ||
            id == IntrinsicArrayFunctions::MinVal ||
            id == IntrinsicArrayFunctions::Count ||
            id == IntrinsicArrayFunctions::Parity) {
            return 1; // dim argument index
        } else if( id == IntrinsicArrayFunctions::MatMul ||
            id == IntrinsicArrayFunctions::Transpose ||
            id == IntrinsicArrayFunctions::Pack ||
            id == IntrinsicArrayFunctions::Cshift ||
            id == IntrinsicArrayFunctions::Eoshift ||
            id == IntrinsicArrayFunctions::Spread ||
            id == IntrinsicArrayFunctions::Unpack ) {
            return 2; // return variable index
        }
        return -1;
    }

    /*
        Index of the `dim` argument in IntrinsicArrayFunction_t::m_args for
        intrinsics that take an optional dim. For `reduce`, `dim` follows `operation`.
    */
    static inline int get_dim_arg_index(IntrinsicArrayFunctions id) {
        if( id == IntrinsicArrayFunctions::Reduce ) {
            return 2;
        }
        int d = get_dim_index(id);
        if( d == 1 ) {
            return 1;
        }
        return -1;
    }

    static inline bool handle_dim(IntrinsicArrayFunctions id) {
        // Dim argument is already handled for the following
        if( id == IntrinsicArrayFunctions::Shape  ||
            id == IntrinsicArrayFunctions::Coshape ||
            id == IntrinsicArrayFunctions::MaxLoc ||
            id == IntrinsicArrayFunctions::MinLoc ||
            id == IntrinsicArrayFunctions::FindLoc ) {
            return false;
        } else {
            return true;
        }
    }

    static inline bool is_elemental(int64_t id) {
        // IntrinsicArrayFunctions id_ = static_cast<IntrinsicArrayFunctions>(id);
        // return (id_ == IntrinsicArrayFunctions::Merge);
        return id == -1;
    }
} // namespace IntrinsicArrayFunctionRegistry

} // namespace ASRUtils

} // namespace LCompilers

#endif // LFORTRAN_PASS_INTRINSIC_ARRAY_FUNCTION_REGISTRY_H
