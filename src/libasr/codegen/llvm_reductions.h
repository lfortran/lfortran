#ifndef LIBASR_CODEGEN_LLVM_REDUCTIONS_H
#define LIBASR_CODEGEN_LLVM_REDUCTIONS_H

namespace llvm {
    class Function;
    class TargetMachine;
}

namespace LCompilers {

    // On x86 with AVX, replaces a four-way lane transpose sum of squares
    // with a pairwise horizontal reduction. See llvm_reductions.cpp. Runs on
    // optimized (vectorized) IR; `tm` decides the subtarget of `f`. Returns
    // whether `f` changed.
    bool recombine_horizontal_reductions(llvm::Function &f,
        const llvm::TargetMachine &tm);

} // namespace LCompilers

#endif // LIBASR_CODEGEN_LLVM_REDUCTIONS_H
