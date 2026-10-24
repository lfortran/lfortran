#ifndef LIBASR_PASS_REPLACE_ARRAY_OP_H
#define LIBASR_PASS_REPLACE_ARRAY_OP_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    void pass_replace_array_op(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &pass_options);

    // Allocate the unallocated allocatable scalar variable of an intrinsic
    // assignment explicitly before the assignment (F2018 10.2.1.3 p3).
    void pass_allocate_on_assignment(Allocator &al, ASR::TranslationUnit_t &unit,
                                     const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_REPLACE_ARRAY_OP_H
