#ifndef LIBASR_CODEGEN_LLVM_REDUCTIONS_H
#define LIBASR_CODEGEN_LLVM_REDUCTIONS_H

namespace llvm {
    class Function;
    class TargetMachine;
}

namespace LCompilers {

    // On x86 with AVX, replaces vectorized sums of the squares of components
    // 0, 1 and 2 of four element vectors with pairwise horizontal
    // reductions. See llvm_reductions.cpp. Runs on optimized (vectorized)
    // IR; `tm` decides the subtarget of `f`. Returns whether `f` changed.
    bool recombine_horizontal_reductions(llvm::Function &f,
        const llvm::TargetMachine &tm);

    // A cheap test, run before the late cleanup that precedes
    // recombine_horizontal_reductions: whether `f` is a definition for x86
    // with AVX that contains a four lane sum of three squares in every lane.
    // The cleanup may turn the squared values into the components of four
    // element vectors that the rewrite needs.
    bool may_recombine_horizontal_reductions(const llvm::Function &f,
        const llvm::TargetMachine &tm);

    // Whether recombine_horizontal_reductions would change `f`, which is
    // left unchanged. The --fast pipeline runs the late cleanup on a copy of
    // each function that may_recombine_horizontal_reductions accepts, and
    // only a function whose copy this accepts gets the cleanup and the
    // rewrite. A rewrite that fires then runs one module-level O1 cleanup,
    // which also revisits the other functions.
    bool can_recombine_horizontal_reductions(const llvm::Function &f,
        const llvm::TargetMachine &tm);

} // namespace LCompilers

#endif // LIBASR_CODEGEN_LLVM_REDUCTIONS_H
