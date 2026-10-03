#ifndef LFORTRAN_CODEGEN_ASR_UTILS_H
#define LFORTRAN_CODEGEN_ASR_UTILS_H

#include <libasr/asr.h>
#include <libasr/asr_utils.h>

// Backend-neutral ASR classification shared by the code generators.
// These are pure queries over ASR with no backend (LLVM, Liric, Wasm,
// x86) types in their signatures, so every backend can share one
// definition instead of repeating the rule.

namespace LCompilers {
namespace CodeGen {

/**
 * Check if a function is an external interface function.
 * External interface functions are functions with:
 * - Interface or ImplicitInterface deftype
 * - Not intrinsic ABI
 * - Not in a module
 */
inline bool is_external_interface_function(ASR::FunctionType_t* ftype) {
    return ASRUtils::is_declaration_deftype(ftype->m_deftype) &&
           ftype->m_abi != ASR::abiType::Intrinsic &&
           !ftype->m_module;
}

} // namespace CodeGen
} // namespace LCompilers

#endif // LFORTRAN_CODEGEN_ASR_UTILS_H
