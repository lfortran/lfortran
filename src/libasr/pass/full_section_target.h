#ifndef LIBASR_PASS_FULL_SECTION_TARGET_H
#define LIBASR_PASS_FULL_SECTION_TARGET_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    // Assigns to the whole array the target of `x(:) = v` (every subscript
    // a full-range, unit-stride section), without reallocation. See
    // full_section_target.cpp.
    void pass_full_section_target(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_FULL_SECTION_TARGET_H
