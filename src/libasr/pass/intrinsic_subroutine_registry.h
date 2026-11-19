#ifndef LFORTRAN_PASS_INTRINSIC_SUBROUTINE_REGISTRY_H
#define LFORTRAN_PASS_INTRINSIC_SUBROUTINE_REGISTRY_H

#include <libasr/pass/intrinsic_function_registry.h>

#include <string>

// The registry is defined in intrinsic_subroutine_registry.cpp. Include
// intrinsic_subroutines.h to use the functions of one intrinsic subroutine.

namespace LCompilers {

namespace ASRUtils {

std::string get_intrinsic_subroutine_name(int64_t x);

namespace IntrinsicImpureSubroutineRegistry {

    bool is_intrinsic_subroutine(const std::string& name);

    bool is_intrinsic_subroutine(int64_t id);

    create_intrinsic_subroutine get_create_subroutine(const std::string& name);

    verify_subroutine get_verify_subroutine(int64_t id);

    impl_subroutine get_instantiate_subroutine(int64_t id);

    std::string get_intrinsic_subroutine_name_from_registry(int64_t id);

    // Whether `x` never defines its argument `x.m_args[i]`, because the
    // argument is `intent(in)`. The positions are those of `x.m_args`, which
    // some `create_*` functions compact by leaving absent optional arguments
    // out. Where a position does not say which argument it holds, the
    // argument is assumed to be defined.
    bool is_intent_in_argument(
        const ASR::IntrinsicImpureSubroutine_t& x, size_t i);

} // namespace IntrinsicImpureSubroutineRegistry

} // namespace ASRUtils

} // namespace LCompilers

#endif // LFORTRAN_PASS_INTRINSIC_SUBROUTINE_REGISTRY_H
