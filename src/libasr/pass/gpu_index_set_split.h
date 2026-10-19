#ifndef LIBASR_PASS_GPU_INDEX_SET_SPLIT_H
#define LIBASR_PASS_GPU_INDEX_SET_SPLIT_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    // Splits a parallel loop nest assigned to the device whose body tests
    // its indices against the bounds (boundary rows of a stencil) into a
    // branch-free interior nest and a nest over the frame around it. See
    // gpu_index_set_split.cpp.
    void pass_gpu_index_set_split(Allocator &al, ASR::TranslationUnit_t &unit,
                                  const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_INDEX_SET_SPLIT_H
