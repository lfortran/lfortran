#ifndef LIBASR_PASS_GPU_RESIDUE_UNROLL_H
#define LIBASR_PASS_GPU_RESIDUE_UNROLL_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    // Unrolls a counted loop of device code by the constant divisor of the
    // quotients `(d + r)/K` and residues `modulo(d + r, K)` of its index,
    // so that each unrolled iteration has constant residues and the
    // quotient (and an element it subscripts) is computed once per K
    // iterations. Calls of small pure functions with a constant argument
    // are inlined there, so that their argument checks fold. See
    // gpu_residue_unroll.cpp.
    void pass_gpu_residue_unroll(Allocator &al,
        ASR::TranslationUnit_t &unit, const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_RESIDUE_UNROLL_H
