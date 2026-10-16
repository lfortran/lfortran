#ifndef LIBASR_PASS_DEAD_ARRAY_STORE_H
#define LIBASR_PASS_DEAD_ARRAY_STORE_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    // Removes a whole-array assignment whose every element is assigned
    // again before anything can read it. See dead_array_store.cpp.
    void pass_dead_array_store(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_DEAD_ARRAY_STORE_H
