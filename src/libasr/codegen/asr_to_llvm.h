#ifndef LFORTRAN_ASR_TO_LLVM_H
#define LFORTRAN_ASR_TO_LLVM_H

#include <libasr/asr.h>
#include <libasr/codegen/evaluator.h>
#include <libasr/pass/pass_manager.h>

namespace LCompilers {

    Result<bool> asr_to_llvm(ASR::TranslationUnit_t &asr,
        diag::Diagnostics &diagnostics,
        std::unique_ptr<LLVMEvaluator> &evaluator,
        Allocator &al,
        LCompilers::PassManager& pass_manager,
        CompilerOptions &co, const std::string &run_fn,
        const std::string &infile);

} // namespace LCompilers

#endif // LFORTRAN_ASR_TO_LLVM_H
