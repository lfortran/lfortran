#ifndef LIBASR_PASS_STRING_LENGTH_ARGUMENTS_H
#define LIBASR_PASS_STRING_LENGTH_ARGUMENTS_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    void pass_string_length_arguments(Allocator &al, ASR::TranslationUnit_t &unit,
                                      const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_STRING_LENGTH_ARGUMENTS_H
