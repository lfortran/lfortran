#ifndef LIBASR_PASS_PRINT_LIST_H
#define LIBASR_PASS_PRINT_LIST_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    void pass_replace_print_list(
    Allocator &al, ASR::TranslationUnit_t &unit,
    const LCompilers::PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_PRINT_LIST_H
