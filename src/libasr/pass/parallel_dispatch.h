#ifndef LIBASR_PASS_PARALLEL_DISPATCH_H
#define LIBASR_PASS_PARALLEL_DISPATCH_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    void pass_parallel_dispatch(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &pass_options);

    // The target a loop falls back to once the device has declined it.
    ASR::exec_targetType host_exec_target(const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_PARALLEL_DISPATCH_H
