#ifndef LIBASR_PASS_EXTERNAL_ABI_H
#define LIBASR_PASS_EXTERNAL_ABI_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    void pass_finalize_external_abi(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_EXTERNAL_ABI_H
