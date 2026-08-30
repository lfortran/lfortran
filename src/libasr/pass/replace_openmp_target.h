#ifndef LIBASR_PASS_OPENMP_TARGET_H
#define LIBASR_PASS_OPENMP_TARGET_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    void pass_replace_openmp_target(Allocator &al, ASR::TranslationUnit_t &unit,
                                    const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_OPENMP_TARGET_H
