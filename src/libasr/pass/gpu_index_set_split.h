#ifndef LIBASR_PASS_GPU_INDEX_SET_SPLIT_H
#define LIBASR_PASS_GPU_INDEX_SET_SPLIT_H

#include <libasr/asr.h>
#include <libasr/utils.h>

#include <set>

namespace LCompilers {

    // Splits a parallel loop nest assigned to the device whose body tests
    // its indices against the bounds (boundary rows of a stencil) into a
    // branch-free interior nest and a nest over the frame around it. See
    // gpu_index_set_split.cpp.
    void pass_gpu_index_set_split(Allocator &al, ASR::TranslationUnit_t &unit,
                                  const PassOptions &pass_options);

    // The splitting, leaving the loops in `rarely_run` as written.
    void gpu_index_set_split(Allocator &al, ASR::TranslationUnit_t &unit,
                             const PassOptions &pass_options,
                             const std::set<ASR::stmt_t*> &rarely_run);

    // The loop transformations of the pipeline: the fusion
    // (gpu_loop_fusion.cpp), then the splitting, which leaves as written the
    // loops the fusion keeps for an execution that rarely happens.
    void pass_gpu_loop_transforms(Allocator &al, ASR::TranslationUnit_t &unit,
                                  const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_INDEX_SET_SPLIT_H
