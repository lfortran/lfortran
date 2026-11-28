#ifndef LFORTRAN_ASR_TO_LEAN_H
#define LFORTRAN_ASR_TO_LEAN_H

#include <libasr/asr.h>
#include <libasr/asr_utils.h>

namespace LCompilers {

    // Prototype: translate a small Fortran subset to Lean 4 code that uses
    // the `LFortran.Prelude` floating-point model. `filename` is only used in
    // the generated header comment.
    Result<std::string> asr_to_lean(ASR::TranslationUnit_t &asr,
        diag::Diagnostics &diagnostics, const std::string &filename);

} // namespace LCompilers

#endif // LFORTRAN_ASR_TO_LEAN_H
