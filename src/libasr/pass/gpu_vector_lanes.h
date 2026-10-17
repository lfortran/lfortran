#ifndef LIBASR_PASS_GPU_VECTOR_LANES_H
#define LIBASR_PASS_GPU_VECTOR_LANES_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    // Runs each iteration of a kernel with few iterations on the lanes of
    // a SIMD-group, which share the loops inside the iteration. See
    // gpu_vector_lanes.cpp.
    void pass_gpu_vector_lanes(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_VECTOR_LANES_H
