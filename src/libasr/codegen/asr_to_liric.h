#ifndef LFORTRAN_ASR_TO_LIRIC_H
#define LFORTRAN_ASR_TO_LIRIC_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    Result<int> asr_to_liric(ASR::TranslationUnit_t &asr,
        Allocator &al, const std::string &filename,
        CompilerOptions &co, diag::Diagnostics &diagnostics,
        int liric_backend);

} // namespace LCompilers

#endif // LFORTRAN_ASR_TO_LIRIC_H
