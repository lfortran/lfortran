#ifndef LIBASR_PASS_GPU_LAUNCH_SYNC_H
#define LIBASR_PASS_GPU_LAUNCH_SYNC_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    // Places the GpuSync statements that wait for asynchronous kernel
    // launches where the host next needs what a kernel wrote, and the
    // GpuDiscard statements for storage that a pending launch writes and
    // the host frees. See gpu_launch_sync.cpp.
    void pass_gpu_launch_sync(Allocator &al, ASR::TranslationUnit_t &unit,
                              const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_LAUNCH_SYNC_H
