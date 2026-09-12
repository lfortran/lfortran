#ifndef LIBASR_PASS_DEVICE_LAUNCH_EXPAND_H
#define LIBASR_PASS_DEVICE_LAUNCH_EXPAND_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    void pass_device_launch_expand(Allocator &al, ASR::TranslationUnit_t &unit,
                                   const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_DEVICE_LAUNCH_EXPAND_H
