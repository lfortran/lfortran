#ifndef LFORTRAN_ASR_TO_MLIR_H
#define LFORTRAN_ASR_TO_MLIR_H

#include <libasr/asr.h>
// #include <libasr/codegen/evaluator.h>
// #include <libasr/pass/pass_manager.h>

namespace LCompilers {

    Result<std::string> asr_to_mlir(Allocator &al, ASR::TranslationUnit_t &asr);

} // namespace LCompilers

#endif // LFORTRAN_ASR_TO_MLIR_H
