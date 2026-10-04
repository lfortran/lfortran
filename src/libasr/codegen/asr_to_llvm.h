#ifndef LFORTRAN_ASR_TO_LLVM_H
#define LFORTRAN_ASR_TO_LLVM_H

#include <libasr/asr.h>
#include <libasr/codegen/evaluator.h>
#include <libasr/pass/pass_manager.h>

namespace LCompilers {

    Result<std::unique_ptr<LLVMModule>> asr_to_llvm(ASR::TranslationUnit_t &asr,
            diag::Diagnostics &diagnostics,
            llvm::LLVMContext &context,
            const LLVMTargetConfig &target_config, Allocator &al,
            LCompilers::PassManager& pass_manager,
            CompilerOptions &compiler_options,
            const std::string &run_fn,
            const std::string &/*global_underscore*/,
            const std::string &infile,
            LocationManager &lm);

    // Give the startup initialization records `asr_to_llvm` emits, which are
    // independent of the object format, the form the loader or linker of the
    // module's target finds them in (runtime/lcompilers_init_abi.h). It is
    // applied once the module's target is set, before its code is emitted;
    // records already in that form are left alone.
    void lower_global_init_records(llvm::Module &module);

} // namespace LCompilers

#endif // LFORTRAN_ASR_TO_LLVM_H
