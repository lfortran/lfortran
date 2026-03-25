#ifndef LIBASR_PASS_DO_CONCURRENT_TO_OPENMP_H
#define LIBASR_PASS_DO_CONCURRENT_TO_OPENMP_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    void pass_do_concurrent_to_openmp(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_DO_CONCURRENT_TO_OPENMP_H
