#include <cmath>
#include <map>
#include <set>
#include <vector>

#include <llvm/ADT/PostOrderIterator.h>
#include <llvm/Config/llvm-config.h>
#if LLVM_VERSION_MAJOR >= 17
#include <llvm/TargetParser/Triple.h>
#else
#include <llvm/ADT/Triple.h>
#endif
#include <llvm/IR/CFG.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#ifdef HAVE_TARGET_X86
#include <llvm/IR/IntrinsicsX86.h>
#endif
#include <llvm/IR/Operator.h>

#include <libasr/codegen/llvm_rsqrt.h>

/*
Reciprocal square roots with the Newton step written in LLVM IR.

With fast math, the x86 code generator lowers a single precision
`a / sqrt(x)` to the hardware estimate `e` of `1/sqrt(x)` (`rsqrtps`,
`vrsqrt14ps`) refined by a Newton step, which computes
`-0.5 * e * (x*e*e - 3)`. The factor -0.5 costs a multiplication per vector
in every iteration of a loop such as

    do j = 1, n
        inv_r = 1 / sqrt(r2(j))
        s = m(j) * inv_r**3
        ax = ax + dx(j) * s
    end do

although it could be applied to `ax` once after the loop. The code generator
expands `1/sqrt` one basic block at a time, after all IR optimizations, so
nothing moves the factor out; and a Newton step written in the source is
folded back into `1/sqrt(x)` by the IR optimizations.

This transformation runs on optimized IR, after vectorization. It writes the
Newton step in IR without its constant factor,

    z = e * (x*e*e - 3),   1/sqrt(x) = -0.5 * z,

tells the code generator to emit the bare estimate `e` for `1/sqrt(x)` (the
function attribute `reciprocal-estimates` with no refinement steps) and
replaces `a / sqrt(x)` by `z` with the constant factor `-0.5 * a` (`a * z`
with `-0.5` when `a` is not constant). The factors are then carried through
the computation instead of being multiplied in:

* a product of values with factors `k1` and `k2` has the factor `k1*k2`;
* a sum `s = s + v` (a reduction, a `phi` of a start value and of the sum)
  accumulates `v / k` and has the factor `k` of `v`. It starts at the start
  value divided by `k`, computed before the loop: zero, the replacement of a
  value with the factor `k`, or the value multiplied by an exact power of two
  (otherwise the sum keeps its factor applied in every iteration). The
  remainder loops of a vectorized loop start at the partial sums of the
  vector loop;
* the sum of two values and the `phi` of values with the same factor `k`
  have the factor `k` (such as the `phi` after a loop and the sums combining
  the partial sums of a vectorized reduction);
* any other use multiplies the value by its factor first.

So the loop above computes `ax / k` and multiplies it by `k` after the loop.

One Newton step is what the code generator uses itself (the estimates have
12 or 14 correct bits). AArch64 is left to LLVM: its Newton step instruction
`frsqrts` includes the factor 0.5, and LLVM uses the estimate only on the
processors where it is faster than `fsqrt` and `fdiv` (not on Apple's, for
example). The single precision square roots of a function must all be
divisors of such divisions (and there must be no `pow`, which the code
generator may turn into square roots), since the attribute applies to every
square root of the function. Otherwise the function is left as it is. If the
code generator computes `1/sqrt(x)` exactly instead of estimating it, the
Newton step keeps the exact value, so the result is correct either way.

Double precision is handled separately. The x86 code generator never uses an
estimate for double, because a double estimate needs two Newton steps, so
the pass emits the AVX-512 estimate instruction itself and refines it twice:

    e = estimate(x),  t = x*e*e - 3,  u = e*t,
    t = x*(u/2)**2 - 3,  z = u*t,   1/sqrt(x) = z/4,

with the factor 1/4 carried as above. It only does this for fixed vectors of
2, 4 or 8 doubles when the function has the matching AVX-512 features, and
leaves other double square roots to the code generator.
*/

namespace LCompilers {

namespace {

using namespace llvm;

// A value equal to `v * k` for an original value.
struct Scaled {
    Value *v;
    double k;
};

bool is_x86(const Triple &triple) {
    return triple.getArch() == Triple::x86 || triple.getArch() == Triple::x86_64;
}

// Single precision scalars and fixed-length vectors of them.
bool is_float(Type *type) {
    if (type->isFloatTy()) {
        return true;
    }
#if LLVM_VERSION_MAJOR >= 11
    return isa<FixedVectorType>(type) && type->getScalarType()->isFloatTy();
#else
    return type->isVectorTy() && type->getScalarType()->isFloatTy();
#endif
}

// Fixed-length vectors of double that the AVX-512 estimate covers.
bool is_double_vector(Type *type) {
    if (!type->getScalarType()->isDoubleTy()) {
        return false;
    }
#if LLVM_VERSION_MAJOR >= 11
    if (!isa<FixedVectorType>(type)) {
        return false;
    }
    unsigned n = cast<FixedVectorType>(type)->getNumElements();
#else
    if (!type->isVectorTy()) {
        return false;
    }
    unsigned n = type->getVectorNumElements();
#endif
    return n == 2 || n == 4 || n == 8;
}

unsigned double_vector_size(Type *type) {
#if LLVM_VERSION_MAJOR >= 11
    return cast<FixedVectorType>(type)->getNumElements();
#else
    return type->getVectorNumElements();
#endif
}

// A feature listed in the function's target-features attribute.
bool has_target_feature(const Function &f, StringRef name) {
    StringRef features = f.getFnAttribute("target-features").getValueAsString();
    size_t pos = 0;
    while (pos < features.size()) {
        size_t end = features.find(',', pos);
        if (end == StringRef::npos) {
            end = features.size();
        }
        if (features.slice(pos, end) == name) {
            return true;
        }
        pos = end + 1;
    }
    return false;
}

// The AVX-512 reciprocal square root estimate for a vector of doubles.
bool has_double_estimate(const Function &f, Type *type) {
#ifdef HAVE_TARGET_X86
    if (!has_target_feature(f, "+avx512f")) {
        return false;
    }
    return double_vector_size(type) == 8 || has_target_feature(f, "+avx512vl");
#else
    (void)f;
    (void)type;
    return false;
#endif
}

Value *rsqrt_estimate(IRBuilder<> &b, Type *type, Value *x) {
#ifdef HAVE_TARGET_X86
    unsigned n = double_vector_size(type);
    Intrinsic::ID id = n == 8 ? Intrinsic::x86_avx512_rsqrt14_pd_512
        : n == 4 ? Intrinsic::x86_avx512_rsqrt14_pd_256
        : Intrinsic::x86_avx512_rsqrt14_pd_128;
#if LLVM_VERSION_MAJOR >= 20
    Function *callee = Intrinsic::getOrInsertDeclaration(
        b.GetInsertBlock()->getModule(), id);
#else
    Function *callee = Intrinsic::getDeclaration(
        b.GetInsertBlock()->getModule(), id);
#endif
    Value *mask = ConstantInt::get(Type::getInt8Ty(type->getContext()), -1);
    return b.CreateCall(callee, {x, UndefValue::get(type), mask});
#else
    (void)b;
    (void)type;
    (void)x;
    return nullptr;
#endif
}

bool is_fast(Value *v) {
    return isa<FPMathOperator>(v)
        && cast<Instruction>(v)->getFastMathFlags().isFast();
}

Intrinsic::ID intrinsic_id(Instruction &i) {
    if (CallInst *call = dyn_cast<CallInst>(&i)) {
        if (Function *callee = call->getCalledFunction()) {
            return callee->getIntrinsicID();
        }
    }
    return Intrinsic::not_intrinsic;
}

bool is_fdiv_by(Value *v, Value *divisor) {
    BinaryOperator *op = dyn_cast<BinaryOperator>(v);
    return op && op->getOpcode() == Instruction::FDiv
        && op->getOperand(1) == divisor && op->getOperand(0) != divisor
        && is_fast(op);
}

// The value of a floating point constant (or splat of one).
bool constant_value(Value *v, double &value) {
    Constant *c = dyn_cast<Constant>(v);
    if (!c) {
        return false;
    }
    if (c->getType()->isVectorTy()) {
        c = c->getSplatValue();
    }
    ConstantFP *fp = dyn_cast_or_null<ConstantFP>(c);
    if (!fp) {
        return false;
    }
    if (fp->getType()->isFloatTy()) {
        value = fp->getValueAPF().convertToFloat();
    } else if (fp->getType()->isDoubleTy()) {
        value = fp->getValueAPF().convertToDouble();
    } else {
        return false;
    }
    return true;
}

// +0.0 or -0.0 (or a splat of either).
bool is_zero(Constant *c) {
    if (c->isNullValue()) {
        return true;
    }
    if (c->getType()->isVectorTy()) {
        c = c->getSplatValue();
    }
    ConstantFP *fp = dyn_cast_or_null<ConstantFP>(c);
    return fp && fp->isZero();
}

PHINode *create_phi_before(PHINode *phi, unsigned n) {
#if LLVM_VERSION_MAJOR >= 19
    return PHINode::Create(phi->getType(), n, "", phi->getIterator());
#else
    return PHINode::Create(phi->getType(), n, "", phi);
#endif
}

// The square roots to expand, or none if the function has a single
// precision square root or power that is not such a divisor.
std::set<Instruction*> square_roots(Function &f) {
    std::set<Instruction*> roots;
    for (BasicBlock &bb: f) {
        for (Instruction &i: bb) {
            CallInst *call = dyn_cast<CallInst>(&i);
            if (!call) {
                continue;
            }
            Type *scalar = call->getType()->getScalarType();
            if (!scalar->isFloatTy() && !scalar->isDoubleTy()) {
                continue;
            }
            Intrinsic::ID id = intrinsic_id(i);
            Function *callee = call->getCalledFunction();
            if (!callee || id == Intrinsic::pow || callee->getName() == "sqrtf"
                    || callee->getName() == "powf"
                    || callee->getName() == "sqrt"
                    || callee->getName() == "pow") {
                return {};
            }
            if (id != Intrinsic::sqrt) {
                continue;
            }
            bool expandable = is_float(call->getType())
                || (is_double_vector(call->getType())
                    && has_double_estimate(f, call->getType()));
            if (!expandable) {
                continue;
            }
            if (!is_fast(call)) {
                return {};
            }
            for (User *u: call->users()) {
                if (!is_fdiv_by(u, call)) {
                    return {};
                }
            }
            roots.insert(call);
        }
    }
    return roots;
}

class Expansion {
public:
    explicit Expansion(Function &f) : f(f) {
        fmf.setFast();
    }

    bool run(const std::set<Instruction*> &roots) {
        ReversePostOrderTraversal<Function*> rpot(&f);
        for (BasicBlock *bb: rpot) {
            std::vector<Instruction*> insts;
            for (Instruction &i: *bb) {
                insts.push_back(&i);
            }
            for (Instruction *i: insts) {
                if (roots.count(i)) {
                    expand_square_root(i);
                } else if (PHINode *phi = dyn_cast<PHINode>(i)) {
                    visit_phi(phi);
                } else {
                    visit(i);
                }
            }
        }
        // A phi takes its incoming values at the end of the incoming
        // blocks, which a back edge may reach after the phi.
        for (BasicBlock &bb: f) {
            for (Instruction &i: bb) {
                PHINode *phi = dyn_cast<PHINode>(&i);
                if (!phi || created.count(phi) || scaled.count(phi)) {
                    continue;
                }
                for (unsigned k = 0; k < phi->getNumIncomingValues(); k++) {
                    auto it = scaled.find(phi->getIncomingValue(k));
                    if (it == scaled.end()) {
                        continue;
                    }
                    IRBuilder<> b(phi->getIncomingBlock(k)->getTerminator());
                    phi->setIncomingValue(k, materialize(b, it->second,
                        phi->getType()));
                }
            }
        }
        // Remove the replaced instructions; a replaced reduction is a cycle
        // of its phi and its sum.
        for (auto &r: reductions) {
            PHINode *phi = r.first;
            for (unsigned k = 0; k < phi->getNumIncomingValues(); k++) {
                if (phi->getIncomingValue(k) == r.second) {
                    phi->setIncomingValue(k, UndefValue::get(phi->getType()));
                }
            }
        }
        bool erased = true;
        while (erased) {
            erased = false;
            for (auto it = replaced.begin(); it != replaced.end(); ) {
                if ((*it)->use_empty()) {
                    (*it)->eraseFromParent();
                    it = replaced.erase(it);
                    erased = true;
                } else {
                    ++it;
                }
            }
        }
        return expanded;
    }

private:
    Function &f;
    FastMathFlags fmf;
    bool expanded = false;
    // The replacement of each original value with a factor.
    std::map<Value*, Scaled> scaled;
    // `1/sqrt(x) = k * z` for each expanded square root.
    std::map<Value*, Scaled> newton;
    std::vector<Instruction*> replaced;
    std::map<PHINode*, Instruction*> reductions;
    std::set<PHINode*> created;

    Scaled get(Value *v) {
        auto it = scaled.find(v);
        return it == scaled.end() ? Scaled{v, 1} : it->second;
    }

    Value *materialize(IRBuilder<> &b, const Scaled &s, Type *type) {
        b.setFastMathFlags(fmf);
        if (s.k == 1) {
            return s.v;
        }
        return b.CreateFMul(s.v, ConstantFP::get(type, s.k));
    }

    void expand_square_root(Instruction *sqrt) {
        Value *x = cast<CallInst>(sqrt)->getArgOperand(0);
        Type *type = sqrt->getType();
        IRBuilder<> b(sqrt->getNextNode());
        b.setFastMathFlags(fmf);
        if (type->getScalarType()->isFloatTy()) {
            Value *e = b.CreateFDiv(ConstantFP::get(type, 1.0), sqrt);
            Value *xee = b.CreateFMul(x, b.CreateFMul(e, e));
            Value *z = b.CreateFMul(e, b.CreateFAdd(xee,
                ConstantFP::get(type, -3.0)));
            newton[sqrt] = {z, -0.5};
        } else {
            // The estimate and two Newton steps, with the 1/4 factor out:
            // 1/sqrt(x) = z/4.
            Value *three = ConstantFP::get(type, -3.0);
            Value *e = rsqrt_estimate(b, type, x);
            Value *t1 = b.CreateFAdd(b.CreateFMul(x, b.CreateFMul(e, e)),
                three);
            Value *u = b.CreateFMul(e, t1);
            Value *quarter_x = b.CreateFMul(ConstantFP::get(type, 0.25), x);
            Value *t2 = b.CreateFAdd(b.CreateFMul(quarter_x,
                b.CreateFMul(u, u)), three);
            Value *z = b.CreateFMul(u, t2);
            newton[sqrt] = {z, 0.25};
        }
        expanded = true;
    }

    void visit(Instruction *i) {
        bool uses_scaled = false;
        for (Value *op: i->operands()) {
            uses_scaled = uses_scaled || scaled.count(op) || newton.count(op);
        }
        if (!uses_scaled) {
            return;
        }
        BinaryOperator *op = dyn_cast<BinaryOperator>(i);
        if (op && op->getOpcode() == Instruction::FDiv
                && newton.count(op->getOperand(1))) {
            // a / sqrt(x) = k * a * z
            Scaled n = newton[op->getOperand(1)];
            double a;
            if (constant_value(op->getOperand(0), a)) {
                replace(i, {n.v, a * n.k});
            } else {
                Scaled s = get(op->getOperand(0));
                IRBuilder<> b(i);
                b.setFastMathFlags(fmf);
                replace(i, {b.CreateFMul(s.v, n.v), s.k * n.k});
            }
            return;
        }
        if (op && is_fast(op) && op->getOpcode() == Instruction::FMul) {
            Scaled l = get(op->getOperand(0)), r = get(op->getOperand(1));
            IRBuilder<> b(i);
            b.setFastMathFlags(fmf);
            replace(i, {b.CreateFMul(l.v, r.v), l.k * r.k});
            return;
        }
        if (op && is_fast(op) && op->getOpcode() == Instruction::FAdd) {
            if (reduction(op, 0) || reduction(op, 1)) {
                return;
            }
            Scaled l = get(op->getOperand(0)), r = get(op->getOperand(1));
            if (scaled.count(op->getOperand(0)) && scaled.count(op->getOperand(1))
                    && l.k == r.k) {
                IRBuilder<> b(i);
                b.setFastMathFlags(fmf);
                replace(i, {b.CreateFAdd(l.v, r.v), l.k});
                return;
            }
        }
        // Any other use takes the value with its factor applied.
        IRBuilder<> b(i);
        for (unsigned k = 0; k < i->getNumOperands(); k++) {
            auto it = scaled.find(i->getOperand(k));
            if (it != scaled.end()) {
                i->setOperand(k, materialize(b, it->second,
                    i->getOperand(k)->getType()));
            }
        }
    }

    // A phi of values with the same factor, all already replaced (such as
    // the phi after a loop of the value the loop computes), takes the
    // replacements; any other phi applies the factors in the incoming
    // blocks, at the end.
    void visit_phi(PHINode *phi) {
        if (phi->getNumIncomingValues() == 0) {
            return;
        }
        double k = 0;
        for (unsigned n = 0; n < phi->getNumIncomingValues(); n++) {
            auto it = scaled.find(phi->getIncomingValue(n));
            if (it == scaled.end() || (n > 0 && it->second.k != k)) {
                return;
            }
            k = it->second.k;
        }
        PHINode *p = create_phi_before(phi, phi->getNumIncomingValues());
        for (unsigned n = 0; n < phi->getNumIncomingValues(); n++) {
            p->addIncoming(scaled[phi->getIncomingValue(n)].v,
                phi->getIncomingBlock(n));
        }
        created.insert(p);
        replace(phi, {p, k});
    }

    // `s = phi(0, s + v)`: accumulate `v` without its factor.
    bool reduction(BinaryOperator *sum, unsigned which) {
        PHINode *phi = dyn_cast<PHINode>(sum->getOperand(which));
        Value *v = sum->getOperand(1 - which);
        if (!phi || scaled.count(phi) || !scaled.count(v) || !phi->hasOneUse()
                || phi->getNumIncomingValues() != 2) {
            return false;
        }
        unsigned back = phi->getIncomingValue(0) == sum ? 0 : 1;
        if (phi->getIncomingValue(back) != sum) {
            return false;
        }
        // The sum starts at `init / k`: zero, the replacement of a value
        // with the same factor (the partial sum of a previous loop), or
        // the value divided by a power of two (exactly).
        Scaled s = scaled[v];
        Value *init = phi->getIncomingValue(1 - back);
        BasicBlock *entry = phi->getIncomingBlock(1 - back);
        Constant *c = dyn_cast<Constant>(init);
        Value *start;
        if (c && is_zero(c)) {
            start = init;
        } else {
            Scaled i = get(init);
            double ratio = i.k / s.k;
            int exponent;
            if (std::abs(std::frexp(ratio, &exponent)) != 0.5) {
                return false;
            }
            IRBuilder<> b(entry->getTerminator());
            start = materialize(b, {i.v, ratio}, phi->getType());
        }
        PHINode *acc = create_phi_before(phi, 2);
        acc->addIncoming(start, entry);
        IRBuilder<> b(sum);
        b.setFastMathFlags(fmf);
        Value *next = b.CreateFAdd(acc, s.v);
        acc->addIncoming(next, phi->getIncomingBlock(back));
        created.insert(acc);
        reductions[phi] = sum;
        scaled[phi] = {acc, s.k};
        replace(sum, {next, s.k});
        replaced.push_back(phi);
        return true;
    }

    void replace(Instruction *i, Scaled s) {
        scaled[i] = s;
        replaced.push_back(i);
    }
};

} // namespace

bool expand_reciprocal_sqrt(llvm::Function &f, const llvm::Triple &triple)
{
    if (!is_x86(triple) || f.isDeclaration()
            || f.hasFnAttribute("reciprocal-estimates")) {
        return false;
    }
    std::set<llvm::Instruction*> roots = square_roots(f);
    if (roots.empty()) {
        return false;
    }
    Expansion expansion(f);
    if (!expansion.run(roots)) {
        return false;
    }
    bool has_float = false;
    for (llvm::Instruction *i: roots) {
        has_float = has_float || i->getType()->getScalarType()->isFloatTy();
    }
    if (has_float) {
        f.addFnAttr("reciprocal-estimates", "sqrtf:0,vec-sqrtf:0");
    }
    return true;
}

} // namespace LCompilers
