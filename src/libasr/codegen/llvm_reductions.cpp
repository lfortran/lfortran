#include <llvm/Config/llvm-config.h>
#if LLVM_VERSION_MAJOR >= 17
#include <llvm/TargetParser/Triple.h>
#else
#include <llvm/ADT/Triple.h>
#endif
#include <llvm/ADT/SmallPtrSet.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/CodeGen/TargetSubtargetInfo.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/Target/TargetMachine.h>
#ifdef HAVE_TARGET_X86
#include <llvm/IR/IntrinsicsX86.h>
#endif

#include <libasr/codegen/llvm_reductions.h>

/*
Pairwise horizontal reductions of four vector squares.

When a loop computes a sum of squares over the first two components of a
fixed-size, four element vector and the loop is unrolled by four, the SLP
vectorizer gathers each component of the four vectors with a chain of
shuffles, squares the gathered vectors and adds them:

    g0 = shuffle v0, v1, <0,4,undef,undef>
    ...
    g0 = shuffle g0, v3, <0,1,2,4>          ; component 0 of v0..v3
    ... the same for components 1 and 2 ...
    s = g0*g0 + g1*g1 + g2*g2               ; per vector: v[0]^2 + v[1]^2 + v[2]^2

That is a transpose of four vectors followed by a vertical sum: nine shuffles,
three multiplies and two adds per group of four vectors. The code generator
cannot recover a horizontal instruction from it, so the loop carries the
shuffles and the register pressure of the transpose.

The same result is computed pairwise, which is what a hand-written SIMD
kernel does:

    t0 = hadd(v0*v0, v1*v1)                 ; partial sums of v0 and v1
    t1 = hadd(v2*v2, v3*v3)
    s  = (t0[2],t0[3],t1[0],t1[1]) + (t0[0],t0[1],t1[2],t1[3])

Two horizontal adds, two shuffles and one add per group. The horizontal add
sums all four components, so the fourth component of the vectors is cleared
first unless it is known to be zero; the reduction above only sums the first
three.

This runs after the optimization pipeline, once the vectorizer has chosen the
transpose. It only rewrites groups whose three gathered components are
0, 1 and 2 of the same four vectors, and only on x86 with AVX.
*/

namespace LCompilers {

namespace {

using namespace llvm;

#if LLVM_VERSION_MAJOR >= 11

bool is_four_doubles(Type *type) {
    auto *vector = dyn_cast<FixedVectorType>(type);
    return vector && vector->getNumElements() == 4
        && vector->getElementType()->isDoubleTy();
}

bool is_fast(Value *v) {
    return isa<FPMathOperator>(v)
        && cast<Instruction>(v)->getFastMathFlags().isFast();
}

// If `v` is the end of the shuffle chain that gathers component `lane` of
// four vectors, store the four vectors in `bases` and return true. The chain
// is the one SLP emits when it vectorizes a component of four loads:
//   g1 = shuffle b0, b1, <lane,lane+4,undef,undef>
//   g2 = shuffle g1, b2, <0,1,lane+4,undef>
//   g3 = shuffle g2, b3, <0,1,2,lane+4>
bool match_gather(Value *v, unsigned lane, Value *bases[4]) {
    auto *g3 = dyn_cast<ShuffleVectorInst>(v);
    if (!g3 || !is_four_doubles(g3->getType())) {
        return false;
    }
    ArrayRef<int> m3 = g3->getShuffleMask();
    if (m3.size() != 4 || m3[0] != 0 || m3[1] != 1 || m3[2] != 2
            || m3[3] != (int)lane + 4) {
        return false;
    }
    auto *g2 = dyn_cast<ShuffleVectorInst>(g3->getOperand(0));
    if (!g2 || g2->getType() != g3->getType()) {
        return false;
    }
    ArrayRef<int> m2 = g2->getShuffleMask();
    if (m2.size() != 4 || m2[0] != 0 || m2[1] != 1
            || m2[2] != (int)lane + 4 || m2[3] != -1) {
        return false;
    }
    auto *g1 = dyn_cast<ShuffleVectorInst>(g2->getOperand(0));
    if (!g1 || g1->getType() != g3->getType()) {
        return false;
    }
    ArrayRef<int> m1 = g1->getShuffleMask();
    if (m1.size() != 4 || m1[0] != (int)lane || m1[1] != (int)lane + 4
            || m1[2] != -1 || m1[3] != -1) {
        return false;
    }
    bases[0] = g1->getOperand(0);
    bases[1] = g1->getOperand(1);
    bases[2] = g2->getOperand(1);
    bases[3] = g3->getOperand(1);
    for (unsigned i = 0; i < 4; i++) {
        if (!is_four_doubles(bases[i]->getType())) {
            return false;
        }
    }
    return true;
}

// A fast multiply of a value by itself.
bool match_square(Value *v, Value *&x) {
    auto *mul = dyn_cast<Instruction>(v);
    if (!mul || mul->getOpcode() != Instruction::FMul || !is_fast(mul)) {
        return false;
    }
    if (mul->getOperand(0) != mul->getOperand(1)) {
        return false;
    }
    x = mul->getOperand(0);
    return true;
}

// Collect the leaves of a tree of fast additions. Anything that is not such
// an addition is a leaf.
void collect_terms(Value *v, SmallVectorImpl<Value *> &terms,
        SmallPtrSetImpl<Value *> &seen) {
    if (!seen.insert(v).second) {
        return;
    }
    auto *i = dyn_cast<Instruction>(v);
    if (i && i->getOpcode() == Instruction::FAdd && is_fast(i)) {
        collect_terms(i->getOperand(0), terms, seen);
        collect_terms(i->getOperand(1), terms, seen);
    } else {
        terms.push_back(v);
    }
}

// Whether component 3 of `v` is known to be zero. Cycles through phis are
// assumed to be zero while they are checked, which is sound because every
// operation below preserves a zero component and every other input is
// checked.
bool lane_three_is_zero(Value *v, SmallPtrSetImpl<Value *> &visiting) {
    if (auto *c = dyn_cast<Constant>(v)) {
        if (isa<ConstantAggregateZero>(c)) {
            return true;
        }
        if (auto *cdv = dyn_cast<ConstantDataVector>(c)) {
            if (!cdv->getElementType()->isDoubleTy()) {
                return false;
            }
            return cast<ConstantFP>(cdv->getElementAsConstant(3))->isZero();
        }
        if (auto *cv = dyn_cast<ConstantVector>(c)) {
            if (!cv->getType()->getElementType()->isDoubleTy()) {
                return false;
            }
            auto *element = dyn_cast<ConstantFP>(cv->getAggregateElement(3));
            return element && element->isZero();
        }
        return false;
    }
    auto *i = dyn_cast<Instruction>(v);
    if (!i) {
        return false;
    }
    if (!visiting.insert(i).second) {
        return true;
    }
    switch (i->getOpcode()) {
        case Instruction::FAdd:
        case Instruction::FSub:
            return lane_three_is_zero(i->getOperand(0), visiting)
                && lane_three_is_zero(i->getOperand(1), visiting);
        case Instruction::FMul:
            // A zero times a value is zero with fast math; without it the
            // other value could be an infinity or a NaN.
            return (is_fast(i)
                    && (lane_three_is_zero(i->getOperand(0), visiting)
                        || lane_three_is_zero(i->getOperand(1), visiting)))
                || (lane_three_is_zero(i->getOperand(0), visiting)
                    && lane_three_is_zero(i->getOperand(1), visiting));
        case Instruction::FNeg:
            return lane_three_is_zero(i->getOperand(0), visiting);
        case Instruction::Select:
            return lane_three_is_zero(i->getOperand(1), visiting)
                && lane_three_is_zero(i->getOperand(2), visiting);
        case Instruction::PHI: {
            auto *phi = cast<PHINode>(i);
            for (unsigned k = 0; k < phi->getNumIncomingValues(); k++) {
                if (!lane_three_is_zero(phi->getIncomingValue(k), visiting)) {
                    return false;
                }
            }
            return phi->getNumIncomingValues() > 0;
        }
        case Instruction::ShuffleVector: {
            auto *shuffle = cast<ShuffleVectorInst>(i);
            unsigned n = cast<FixedVectorType>(shuffle->getType())
                ->getNumElements();
            int mask = shuffle->getShuffleMask()[3];
            if (mask < 0) {
                return false;
            }
            Value *source = mask < (int)n ? shuffle->getOperand(0)
                : shuffle->getOperand(1);
            return lane_three_is_zero(source, visiting);
        }
        case Instruction::BitCast:
            return lane_three_is_zero(i->getOperand(0), visiting);
        default:
            return false;
    }
}

bool lane_three_is_zero(Value *v) {
    SmallPtrSet<Value *, 16> visiting;
    return lane_three_is_zero(v, visiting);
}

Function *hadd_pd(Module &m) {
#ifdef HAVE_TARGET_X86
#if LLVM_VERSION_MAJOR >= 20
    return Intrinsic::getOrInsertDeclaration(&m,
        Intrinsic::x86_avx_hadd_pd_256);
#else
    return Intrinsic::getDeclaration(&m, Intrinsic::x86_avx_hadd_pd_256);
#endif
#else
    (void)m;
    return nullptr;
#endif
}

#endif // LLVM_VERSION_MAJOR >= 11

} // namespace

bool recombine_horizontal_reductions(llvm::Function &f,
        const llvm::TargetMachine &tm) {
#if LLVM_VERSION_MAJOR >= 11
    const Triple &triple = tm.getTargetTriple();
    if (triple.getArch() != Triple::x86 && triple.getArch() != Triple::x86_64) {
        return false;
    }
    // The subtarget the code generator uses for `f`: its target-cpu and
    // target-features attributes over the defaults of `tm`. A CPU implies
    // its features, so the attribute alone does not tell whether AVX is on.
    if (!tm.getSubtargetImpl(f)->checkFeatures("+avx")) {
        return false;
    }
    bool changed = false;
    // Collect candidates first: rewriting inserts instructions.
    llvm::SmallVector<llvm::Instruction *, 4> roots;
    for (llvm::BasicBlock &block : f) {
        for (llvm::Instruction &i : block) {
            if (i.getOpcode() != llvm::Instruction::FAdd || !is_fast(&i)) {
                continue;
            }
            llvm::SmallVector<llvm::Value *, 4> terms;
            llvm::SmallPtrSet<llvm::Value *, 8> seen;
            collect_terms(&i, terms, seen);
            if (terms.size() == 3) {
                roots.push_back(&i);
            }
        }
    }
    for (llvm::Instruction *root : roots) {
        llvm::SmallVector<llvm::Value *, 4> terms;
        llvm::SmallPtrSet<llvm::Value *, 8> seen;
        collect_terms(root, terms, seen);
        if (terms.size() != 3) {
            continue;
        }
        llvm::Value *bases[4] = {nullptr, nullptr, nullptr, nullptr};
        llvm::Value *squares[3] = {nullptr, nullptr, nullptr};
        bool used_lane[3] = {false, false, false};
        bool ok = true;
        for (unsigned k = 0; k < 3 && ok; k++) {
            llvm::Value *x;
            if (!match_square(terms[k], x)) {
                ok = false;
                break;
            }
            squares[k] = terms[k];
            bool matched = false;
            for (unsigned lane = 0; lane < 3 && !matched; lane++) {
                if (used_lane[lane]) {
                    continue;
                }
                llvm::Value *candidate[4];
                if (!match_gather(x, lane, candidate)) {
                    continue;
                }
                if (bases[0] == nullptr) {
                    for (unsigned j = 0; j < 4; j++) {
                        bases[j] = candidate[j];
                    }
                    matched = true;
                } else {
                    bool same = true;
                    for (unsigned j = 0; j < 4; j++) {
                        same = same && bases[j] == candidate[j];
                    }
                    if (same) {
                        matched = true;
                    }
                }
                if (matched) {
                    used_lane[lane] = true;
                }
            }
            if (!matched) {
                ok = false;
            }
        }
        if (!ok) {
            continue;
        }
        llvm::Module *module = f.getParent();
        llvm::Function *hadd = hadd_pd(*module);
        if (!hadd) {
            continue;
        }
        llvm::IRBuilder<> builder(root);
        builder.setFastMathFlags(root->getFastMathFlags());
        llvm::Type *vector_type = bases[0]->getType();
        llvm::Value *x[4];
        for (unsigned j = 0; j < 4; j++) {
            x[j] = builder.CreateFMul(bases[j], bases[j]);
        }
        bool zero = true;
        for (unsigned j = 0; j < 4; j++) {
            zero = zero && lane_three_is_zero(bases[j]);
        }
        if (!zero) {
            llvm::Type *i64 = llvm::Type::getInt64Ty(f.getContext());
            llvm::Constant *ones = llvm::ConstantInt::get(i64, -1);
            llvm::Constant *zeros = llvm::ConstantInt::get(i64, 0);
            llvm::Constant *mask = llvm::ConstantVector::get(
                llvm::ArrayRef<llvm::Constant *>{ones, ones, ones, zeros});
            for (unsigned j = 0; j < 4; j++) {
                llvm::Value *bits = builder.CreateBitCast(x[j],
                    llvm::FixedVectorType::get(i64, 4));
                x[j] = builder.CreateBitCast(
                    builder.CreateAnd(bits, mask), vector_type);
            }
        }
        llvm::Value *t0 = builder.CreateCall(hadd, {x[0], x[1]});
        llvm::Value *t1 = builder.CreateCall(hadd, {x[2], x[3]});
        llvm::Value *y0 = builder.CreateShuffleVector(t0, t1,
            llvm::ArrayRef<int>{2, 3, 4, 5});
        llvm::Value *y1 = builder.CreateShuffleVector(t0, t1,
            llvm::ArrayRef<int>{0, 1, 6, 7});
        llvm::Value *sum = builder.CreateFAdd(y0, y1);
        root->replaceAllUsesWith(sum);
        changed = true;
    }
    return changed;
#else
    (void)f;
    (void)tm;
    return false;
#endif
}

} // namespace LCompilers
