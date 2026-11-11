#ifndef LFORTRAN_ASR_TO_LLVM_H
#define LFORTRAN_ASR_TO_LLVM_H

#include <libasr/asr.h>
#include <libasr/codegen/evaluator.h>
#include <libasr/pass/pass_manager.h>
#include <functional>

namespace LCompilers {

    // Same-process native JIT only; never serialized into file compilation.
    //
    // The generated code polls cancellation as a safepoint: at procedure
    // entry, loop back-edges and after calls it loads the request word and
    // branches to its cleanup only when the word is nonzero. The words are
    // external i32 globals; the host defines each symbol at its own
    // storage, one set per JIT session.
    struct JITCancellation {
        // Nonzero once the host requests cancellation. Another host thread
        // may store it at any time; generated code only loads it.
        static constexpr const char *request_symbol = "__lcompilers_cancel_request";
        // Set once generated code starts to unwind for the request.
        static constexpr const char *observed_symbol = "__lcompilers_cancel_observed";
        // Depth of initialization and cleanup that has to complete.
        static constexpr const char *protect_symbol = "__lcompilers_cancel_protect";
        // Depth of calls into foreign code, which does not expect generated
        // code it calls back to return early. Cancellation is observed once
        // the foreign call returns.
        static constexpr const char *defer_symbol = "__lcompilers_cancel_defer";
        // Whether a call target, by its symbol, is generated code of an
        // earlier JIT module, which takes part in cancellation.
        std::function<bool(const std::string &)> cooperative_call;
        // Whether such an earlier definition never reaches a safepoint (it
        // does not use the request word), so a call needs no check after it.
        std::function<bool(const std::string &)> never_unwinds;
    };

    Result<std::unique_ptr<LLVMModule>> asr_to_llvm(ASR::TranslationUnit_t &asr,
            diag::Diagnostics &diagnostics,
            llvm::LLVMContext &context,
            const LLVMTargetConfig &target_config, Allocator &al,
            LCompilers::PassManager& pass_manager,
            CompilerOptions &compiler_options,
            const std::string &run_fn,
            const std::string &/*global_underscore*/,
            const std::string &infile,
            LocationManager &lm, const JITCancellation *cancellation = nullptr);

} // namespace LCompilers

#endif // LFORTRAN_ASR_TO_LLVM_H
