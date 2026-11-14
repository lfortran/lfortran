#ifndef LIBASR_PASS_REACHABLE_PROCEDURES_H
#define LIBASR_PASS_REACHABLE_PROCEDURES_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    void pass_reachable_procedures(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_REACHABLE_PROCEDURES_H
