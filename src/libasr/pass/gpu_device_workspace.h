#ifndef LIBASR_PASS_GPU_DEVICE_WORKSPACE_H
#define LIBASR_PASS_GPU_DEVICE_WORKSPACE_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    void pass_gpu_device_workspace(Allocator &al,
        ASR::TranslationUnit_t &unit, const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_DEVICE_WORKSPACE_H
