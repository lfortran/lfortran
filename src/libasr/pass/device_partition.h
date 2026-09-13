#ifndef LIBASR_PASS_DEVICE_PARTITION_H
#define LIBASR_PASS_DEVICE_PARTITION_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    void pass_device_partition(Allocator &al, ASR::TranslationUnit_t &unit,
                               const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_DEVICE_PARTITION_H
