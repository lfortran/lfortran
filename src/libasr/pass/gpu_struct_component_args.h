#ifndef LIBASR_PASS_GPU_STRUCT_COMPONENT_ARGS_H
#define LIBASR_PASS_GPU_STRUCT_COMPONENT_ARGS_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    void pass_gpu_struct_component_args(Allocator &al,
        ASR::TranslationUnit_t &unit, const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_STRUCT_COMPONENT_ARGS_H
