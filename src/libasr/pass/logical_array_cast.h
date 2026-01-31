#ifndef LIBASR_PASS_LOGICAL_ARRAY_CAST_H
#define LIBASR_PASS_LOGICAL_ARRAY_CAST_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    void pass_logical_array_cast(Allocator &al, ASR::TranslationUnit_t &unit,
                                 const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_LOGICAL_ARRAY_CAST_H
