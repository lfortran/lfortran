#ifndef LIBASR_PASS_HOIST_REALLOC_H
#define LIBASR_PASS_HOIST_REALLOC_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    void pass_hoist_realloc(Allocator &al, ASR::TranslationUnit_t &unit,
                            const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_HOIST_REALLOC_H
