#ifndef LIBASR_PASS_ALLOCATE_UNALLOCATED_H
#define LIBASR_PASS_ALLOCATE_UNALLOCATED_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    // Turns a reallocation on assignment of an array that is certainly
    // unallocated where it executes into a plain allocation, without a test
    // of its allocation or shape (see allocate_unallocated.cpp).
    void pass_allocate_unallocated(Allocator &al, ASR::TranslationUnit_t &unit,
                                   const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_ALLOCATE_UNALLOCATED_H
