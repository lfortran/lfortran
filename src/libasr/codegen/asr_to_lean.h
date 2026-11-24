#ifndef LFORTRAN_ASR_TO_LEAN_H
#define LFORTRAN_ASR_TO_LEAN_H

#include <libasr/asr.h>
#include <libasr/asr_utils.h>

namespace LCompilers {

    // `--show-lean`: every procedure becomes one Lean template over
    // `K : Kinds` (`lean/LFortran/Arith.lean`). Proofs instantiate it at
    // `ℤ`/`ℝ` with an arithmetic model; a program's `main` instantiates it at
    // `LFortran.machine`. `filename` is only used in the header comment.
    Result<std::string> asr_to_lean(ASR::TranslationUnit_t &asr,
        diag::Diagnostics &diagnostics, const std::string &filename);

    // `--backend=lean`: the same code, for a file that has a program; its
    // `main` is what `lean --run` executes.
    Result<std::string> asr_to_lean_exec(ASR::TranslationUnit_t &asr,
        diag::Diagnostics &diagnostics, const std::string &filename);

} // namespace LCompilers

#endif // LFORTRAN_ASR_TO_LEAN_H
