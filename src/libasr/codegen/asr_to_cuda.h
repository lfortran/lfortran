#ifndef LFORTRAN_ASR_TO_CUDA_H
#define LFORTRAN_ASR_TO_CUDA_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    // emit_registrar controls the trailing kernel-registration shim, which is
    // only meaningful to the CUDA runtime's lookup-by-function-pointer launch
    // path. Consumers that resolve kernels by name want the kernels alone.
    Result<std::string> asr_to_cuda(Allocator &al, ASR::TranslationUnit_t &asr,
        diag::Diagnostics &diagnostics, CompilerOptions &co,
        bool emit_registrar = true);

} // namespace LCompilers

#endif // LFORTRAN_ASR_TO_CUDA_H
