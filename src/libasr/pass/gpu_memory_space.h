#ifndef LIBASR_PASS_GPU_MEMORY_SPACE_H
#define LIBASR_PASS_GPU_MEMORY_SPACE_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    void pass_gpu_memory_space(Allocator &al, ASR::TranslationUnit_t &unit,
                               const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_MEMORY_SPACE_H
