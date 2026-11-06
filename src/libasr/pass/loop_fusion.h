#ifndef LIBASR_PASS_LOOP_FUSION_H
#define LIBASR_PASS_LOOP_FUSION_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    // Fuses adjacent DO loop nests over the same iteration space into one
    // nest, when that does not change what the program computes. See
    // loop_fusion.cpp.
    void pass_loop_fusion(Allocator &al, ASR::TranslationUnit_t &unit,
                          const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_LOOP_FUSION_H
