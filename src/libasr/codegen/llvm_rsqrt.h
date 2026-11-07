#ifndef LIBASR_CODEGEN_LLVM_RSQRT_H
#define LIBASR_CODEGEN_LLVM_RSQRT_H

namespace llvm {
    class Function;
    class Triple;
}

namespace LCompilers {

    // On x86, rewrites the single precision `a / sqrt(x)` of `f` into the
    // reciprocal square root estimate followed by a Newton step written in
    // LLVM IR, with its constant factor moved out of sum reductions. See
    // llvm_rsqrt.cpp. Runs on optimized (vectorized) IR; returns whether `f`
    // changed.
    bool expand_reciprocal_sqrt(llvm::Function &f, const llvm::Triple &triple);

} // namespace LCompilers

#endif // LIBASR_CODEGEN_LLVM_RSQRT_H
