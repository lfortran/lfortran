#ifndef LIBASR_PASS_CLOSURE_LOWERING_H
#define LIBASR_PASS_CLOSURE_LOWERING_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {
void pass_closure_lowering(Allocator &al, ASR::TranslationUnit_t &unit,
    const PassOptions &options);
}

#endif
