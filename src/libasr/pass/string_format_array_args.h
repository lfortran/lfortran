#ifndef LIBASR_PASS_STRING_FORMAT_ARRAY_ARGS_H
#define LIBASR_PASS_STRING_FORMAT_ARRAY_ARGS_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    void pass_string_format_array_args(Allocator &al, ASR::TranslationUnit_t &unit,
                                       const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_STRING_FORMAT_ARRAY_ARGS_H
