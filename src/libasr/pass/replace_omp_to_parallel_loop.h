#ifndef LIBASR_PASS_OMP_TO_PARALLEL_LOOP_H
#define LIBASR_PASS_OMP_TO_PARALLEL_LOOP_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    void pass_replace_omp_to_parallel_loop(Allocator &al,
        ASR::TranslationUnit_t &unit, const PassOptions &pass_options);

    void pass_replace_parallel_loop_to_omp(Allocator &al,
        ASR::TranslationUnit_t &unit, const PassOptions &pass_options);

    void pass_flatten_omp_regions(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_OMP_TO_PARALLEL_LOOP_H
