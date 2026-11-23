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

    // Executable translation. Unlike `asr_to_lean`, the result is computable
    // Lean 4: a program becomes `def main` that `lean --run` can execute.
    // Integers use their two's-complement width, real(4)/real(8) are Float32
    // and Float, and `stop` / `error stop` leave the process.
    Result<std::string> asr_to_lean_exec(ASR::TranslationUnit_t &asr,
        diag::Diagnostics &diagnostics, const std::string &filename);

} // namespace LCompilers

#endif // LFORTRAN_ASR_TO_LEAN_H
