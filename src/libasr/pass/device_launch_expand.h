#ifndef LIBASR_PASS_DEVICE_LAUNCH_EXPAND_H
#define LIBASR_PASS_DEVICE_LAUNCH_EXPAND_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    void pass_device_launch_expand(Allocator &al, ASR::TranslationUnit_t &unit,
                                   const PassOptions &pass_options);

    // True when this pass can lay out every argument of a launch of `kernel`
    // exactly as the device code generator does. When it cannot, `reason`
    // says why, so that the caller can keep the loop on the host and warn.
    bool gpu_launch_is_supported(ASR::symbol_t *kernel, ASR::call_arg_t *args,
                                 size_t n_args, std::string &reason);

} // namespace LCompilers

#endif // LIBASR_PASS_DEVICE_LAUNCH_EXPAND_H
