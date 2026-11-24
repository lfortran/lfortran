#ifndef LFORTRAN_ASR_TO_LEAN_H
#define LFORTRAN_ASR_TO_LEAN_H

#include <libasr/asr.h>
#include <libasr/asr_utils.h>

namespace LCompilers {

    // Template mode of the Lean backend (`--show-lean`): the same translator
    // as `asr_to_lean_exec`, emitting each procedure once over
    // `[FInt ι] [FReal α]` (`LFortran.Arith`) plus a `_float` wrapper at its
    // kinds. Proofs instantiate the template at `ℤ` and an `FPModel`;
    // `lean --run` uses the wrapper. `filename` is only used in the header.
    Result<std::string> asr_to_lean(ASR::TranslationUnit_t &asr,
        diag::Diagnostics &diagnostics, const std::string &filename);

    // Executable mode. A program becomes `def main` that `lean --run` can
    // execute. Integers use their two's-complement width, real(4)/real(8)
    // are Float32 and Float, and `stop` / `error stop` leave the process.
    Result<std::string> asr_to_lean_exec(ASR::TranslationUnit_t &asr,
        diag::Diagnostics &diagnostics, const std::string &filename);

} // namespace LCompilers

#endif // LFORTRAN_ASR_TO_LEAN_H
