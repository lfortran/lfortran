#ifndef LFORTRAN_ASR_TO_C_H
#define LFORTRAN_ASR_TO_C_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LFortran {

    Result<std::string> asr_to_c(Allocator &al, ASR::TranslationUnit_t &asr,
        diag::Diagnostics &diagnostics,
        int64_t default_lower_bound,
        const LFortran::CompilerOptions& compiler_options);

} // namespace LFortran

#endif // LFORTRAN_ASR_TO_C_H
