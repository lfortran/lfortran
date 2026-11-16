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

    // The most vectors of 16 bytes an iteration of one loop of a kernel
    // reads (or the kernel outside its loops), each counted once: the loads
    // its threads have in flight together.
    int64_t gpu_wide_vectors_per_iteration(const ASR::Function_t &kernel);

    // The most rows of one array (elements that differ in a subscript after
    // the first) an iteration of a loop of the kernel reads 16 bytes of,
    // counted before the pass makes the vectors (gpu_vector_lanes.cpp sizes
    // the lanes of a kernel by it).
    int64_t gpu_wide_rows_per_iteration(const ASR::Function_t &kernel);

    void pass_gpu_wide_loads(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_WIDE_LOADS_H
