#ifndef LIBASR_PASS_EXPLICIT_SHAPE_EXTENT_H
#define LIBASR_PASS_EXPLICIT_SHAPE_EXTENT_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    void pass_explicit_shape_extent(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_EXPLICIT_SHAPE_EXTENT_H
