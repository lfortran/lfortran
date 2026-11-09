#ifndef LIBASR_CODEGEN_LLVM_REDUCTIONS_H
#define LIBASR_CODEGEN_LLVM_REDUCTIONS_H

namespace llvm {
    class Function;
    class TargetMachine;
}

namespace LCompilers {

    // On x86 with AVX, replaces vectorized sums of the squares of three
    // vector components with pairwise horizontal reductions. See
    // llvm_reductions.cpp. Runs on optimized (vectorized) IR; `tm` decides
    // the subtarget of `f`. Returns whether `f` changed.
    bool recombine_horizontal_reductions(llvm::Function &f,
        const llvm::TargetMachine &tm);

    // A cheap test, run before the late cleanup that precedes
    // recombine_horizontal_reductions: whether `f` is a definition for x86
    // with AVX that contains a fast addition of double vectors whose every
    // lane is a sum of three squares, where every rewrite starts. Only such
    // functions get the late cleanup and the rewrite. A rewrite that fires
    // then runs one module-level O1 cleanup, which also revisits the other
    // functions.
    bool may_recombine_horizontal_reductions(const llvm::Function &f,
        const llvm::TargetMachine &tm);

} // namespace LCompilers

#endif // LIBASR_CODEGEN_LLVM_REDUCTIONS_H
