#ifndef LFORTRAN_ASR_TO_RUST_H
#define LFORTRAN_ASR_TO_RUST_H

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/codegen/asr_to_c_cpp.h>

namespace LFortran
{
Result<std::string>
asr_to_rust(Allocator& al, ASR::TranslationUnit_t& asr, diag::Diagnostics& diag);
}  // namespace LFortran

#endif  // LFORTRAN_ASR_TO_RUST_H
