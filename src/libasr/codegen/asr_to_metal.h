#ifndef LFORTRAN_ASR_TO_METAL_H
#define LFORTRAN_ASR_TO_METAL_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    Result<std::string> asr_to_metal(Allocator &al, ASR::TranslationUnit_t &asr,
        diag::Diagnostics &diagnostics, CompilerOptions &co);

    // What the Metal code generator raises for `kernel` alone, or an empty
    // string when it can write it.
    std::string asr_to_metal_kernel_error(const ASR::Function_t &kernel);

} // namespace LCompilers

#endif // LFORTRAN_ASR_TO_METAL_H
