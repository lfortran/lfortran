#ifndef LFORTRAN_ASR_TO_JULIA_H
#define LFORTRAN_ASR_TO_JULIA_H

#include <libasr/asr.h>
#include <julia.h>

namespace LFortran {
    jl_expr_t asr_to_julia(ASR::TranslationUnit_t &asr);
} // namespace LFortran

#endif // LFORTRAN_ASR_TO_JULIA_H
