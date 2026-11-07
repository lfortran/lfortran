#ifndef LIBASR_PASS_GPU_INTEGER_POWERS_H
#define LIBASR_PASS_GPU_INTEGER_POWERS_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    // Writes a real power with a small constant integer exponent in device
    // code, such as `x**2`, as the products the host computes it with
    // (`x*x`), instead of a call of the device's `pow`. See
    // gpu_integer_powers.cpp.
    void pass_gpu_integer_powers(Allocator &al,
        ASR::TranslationUnit_t &unit, const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_INTEGER_POWERS_H
