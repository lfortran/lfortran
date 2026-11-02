#ifndef LIBASR_PASS_SPECIALIZE_PROCEDURE_ARGUMENTS_H
#define LIBASR_PASS_SPECIALIZE_PROCEDURE_ARGUMENTS_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {

    // With --fast, a call that passes an internal procedure to a dummy
    // procedure that the callee only calls becomes a call of a copy of the
    // callee in the host, which calls the internal procedure directly. See
    // specialize_procedure_arguments.cpp.
    void pass_specialize_procedure_arguments(Allocator &al,
        ASR::TranslationUnit_t &unit, const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_SPECIALIZE_PROCEDURE_ARGUMENTS_H
