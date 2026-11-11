#ifndef LIBASR_PASS_GPU_WIDE_LOADS_H
#define LIBASR_PASS_GPU_WIDE_LOADS_H

#include <libasr/asr.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/utils.h>

namespace LCompilers {

    // Reads the elements of an array that device code reads side by side
    // in one statement as one vector. See gpu_wide_loads.cpp.
    // Whether the pass reads some elements of a kernel, or of the device
    // functions it calls, as vectors: 16 bytes at once per lane, which a
    // warp that reads along an array needs no more lanes for.
    bool gpu_reads_wide(const ASR::Function_t &kernel,
        const GpuDeviceCapabilities &caps);

    void pass_gpu_wide_loads(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_WIDE_LOADS_H
