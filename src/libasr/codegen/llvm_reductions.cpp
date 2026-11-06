#include <llvm/Config/llvm-config.h>
#if LLVM_VERSION_MAJOR >= 17
#include <llvm/TargetParser/Triple.h>
#else
#include <llvm/ADT/Triple.h>
#endif
#include <llvm/ADT/DenseMap.h>
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

#include <utility>
#include <vector>

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

// Whether element `lane` of `v` is known to be +0.0 or -0.0 (a scalar is
// element 0 of itself).
//
// The facts are the greatest fixed point of the rules below: every element
// reachable from the query is first assumed to be zero, and the assumption is
// dropped for each element whose operands no longer justify it, until nothing
// changes. A cycle through phis is therefore zero only if every value that
// enters the cycle is zero and every operation on it keeps the element zero,
// which is induction over the iterations of the loop.
class LaneZero {
public:
    bool is_zero(Value *v, unsigned lane) {
        Element root{v, lane};
        if (!add(root)) {
            return constant_is_zero(v, lane);
        }
        bool changed = true;
        while (changed) {
            changed = false;
            for (Node &node : nodes) {
                if (node.zero && !holds(node)) {
                    node.zero = false;
                    changed = true;
                }
            }
        }
        return nodes[index[root]].zero;
    }

private:
    using Element = std::pair<Value *, unsigned>;

    // The element is zero if all operand elements are zero, or with `any`,
    // if one of them is.
    struct Node {
        SmallVector<Element, 3> operands;
        bool any = false;
        bool zero = true;
    };

    std::vector<Node> nodes;
    DenseMap<Element, unsigned> index;

    // A constant element that is +0.0 or -0.0, or an integer zero.
    static bool constant_is_zero(Value *v, unsigned lane) {
        auto *c = dyn_cast<Constant>(v);
        if (!c) {
            return false;
        }
        if (c->getType()->isVectorTy()) {
            c = c->getAggregateElement(lane);
        } else if (lane != 0) {
            return false;
        }
        if (!c || isa<UndefValue>(c)) {
            return false;
        }
        if (auto *fp = dyn_cast<ConstantFP>(c)) {
            return fp->isZero();
        }
        return c->isNullValue();
    }

    static unsigned width(Type *type) {
        auto *vector = dyn_cast<FixedVectorType>(type);
        return vector ? vector->getNumElements() : 1;
    }

    bool value_of(const Element &e) const {
        auto it = index.find(e);
        if (it == index.end()) {
            return constant_is_zero(e.first, e.second);
        }
        return nodes[it->second].zero;
    }

    bool holds(const Node &node) const {
        for (const Element &e : node.operands) {
            if (value_of(e) == node.any) {
                return node.any;
            }
        }
        return !node.any && !node.operands.empty();
    }

    // The operand elements that decide element `lane` of instruction `i`, or
    // false if the element is not known to be zero whatever its operands are.
    static bool rule(Instruction *i, unsigned lane, Node &node) {
        auto same = [&](Value *v) { node.operands.push_back({v, lane}); };
        switch (i->getOpcode()) {
            case Instruction::FAdd:
            case Instruction::FSub:
                same(i->getOperand(0));
                same(i->getOperand(1));
                return true;
            case Instruction::FMul:
                // With fast math a zero times any value is zero; without it
                // the other value could be an infinity or a NaN.
                node.any = is_fast(i);
                same(i->getOperand(0));
                same(i->getOperand(1));
                return true;
            case Instruction::FNeg:
                same(i->getOperand(0));
                return true;
            case Instruction::Select:
                same(i->getOperand(1));
                same(i->getOperand(2));
                return true;
            case Instruction::PHI:
                for (Value *incoming : cast<PHINode>(i)->incoming_values()) {
                    same(incoming);
                }
                return true;
            case Instruction::ShuffleVector: {
                auto *shuffle = cast<ShuffleVectorInst>(i);
                int m = shuffle->getShuffleMask()[lane];
                if (m < 0) {
                    return false;
                }
                unsigned n = width(shuffle->getOperand(0)->getType());
                node.operands.push_back({shuffle->getOperand(m < (int)n ? 0 : 1),
                    (unsigned)m % n});
                return true;
            }
            case Instruction::InsertElement: {
                auto *at = dyn_cast<ConstantInt>(i->getOperand(2));
                if (!at) {
                    return false;
                }
                if (at->getZExtValue() == lane) {
                    node.operands.push_back({i->getOperand(1), 0});
                } else {
                    same(i->getOperand(0));
                }
                return true;
            }
            case Instruction::ExtractElement: {
                auto *at = dyn_cast<ConstantInt>(i->getOperand(1));
                if (!at || lane != 0) {
                    return false;
                }
                node.operands.push_back({i->getOperand(0),
                    (unsigned)at->getZExtValue()});
                return true;
            }
            case Instruction::BitCast:
                // Only a cast between floating point elements of the same
                // width keeps the meaning of a zero element.
                if (width(i->getType()) != width(i->getOperand(0)->getType())
                        || !i->getType()->getScalarType()->isFloatingPointTy()
                        || !i->getOperand(0)->getType()->getScalarType()
                            ->isFloatingPointTy()) {
                    return false;
                }
                same(i->getOperand(0));
                return true;
            default:
                return false;
        }
    }

    // Add the elements reachable from `root` to the graph. Returns false if
    // `root` is not an instruction.
    bool add(const Element &root) {
        if (!isa<Instruction>(root.first)) {
            return false;
        }
        SmallVector<Element, 16> work;
        work.push_back(root);
        while (!work.empty()) {
            Element e = work.pop_back_val();
            auto *i = dyn_cast<Instruction>(e.first);
            if (!i || index.count(e)) {
                continue;
            }
            index[e] = nodes.size();
            Node node;
            if (!rule(i, e.second, node)) {
                node.operands.clear();
                node.any = false;
            }
            for (const Element &operand : node.operands) {
                work.push_back(operand);
            }
            nodes.push_back(node);
        }
        return true;
    }
};

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
        LaneZero lanes;
        bool zero = true;
        for (unsigned j = 0; j < 4; j++) {
            zero = zero && lanes.is_zero(bases[j], 3);
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
