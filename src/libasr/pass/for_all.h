#ifndef LIBASR_PASS_FOR_ALL
#define LIBASR_PASS_FOR_ALL

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    void pass_replace_forall(Allocator &al, ASR::TranslationUnit_t &unit,
                             const LCompilers::PassOptions& pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_FOR_ALL
