#ifndef LIBASR_PASS_FUSE_ARRAY_REDUCTION_H
#define LIBASR_PASS_FUSE_ARRAY_REDUCTION_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    void pass_fuse_array_reduction(Allocator &al,
        ASR::TranslationUnit_t &unit,
        const LCompilers::PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_FUSE_ARRAY_REDUCTION_H
