#ifndef LIBASR_PASS_DTIO_CALLBACKS_H
#define LIBASR_PASS_DTIO_CALLBACKS_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    void pass_dtio_callbacks(Allocator &al, ASR::TranslationUnit_t &unit,
                             const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_DTIO_CALLBACKS_H
