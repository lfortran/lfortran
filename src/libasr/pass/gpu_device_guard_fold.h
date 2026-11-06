#ifndef LIBASR_PASS_GPU_DEVICE_GUARD_FOLD_H
#define LIBASR_PASS_GPU_DEVICE_GUARD_FOLD_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    // Removes the branches of device code whose tests the ranges of the
    // integers they compare decide, such as the check of a procedure's
    // argument that every call of it satisfies, so that a kernel whose
    // only `stop` is such a check does not stop. See
    // gpu_device_guard_fold.cpp.
    void pass_gpu_device_guard_fold(Allocator &al,
        ASR::TranslationUnit_t &unit, const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_DEVICE_GUARD_FOLD_H
