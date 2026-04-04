#ifndef LIBASR_PASS_REPLACE_GPU_OFFLOAD_H
#define LIBASR_PASS_REPLACE_GPU_OFFLOAD_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    void pass_replace_gpu_offload(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_REPLACE_GPU_OFFLOAD_H
