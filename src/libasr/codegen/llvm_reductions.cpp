#include <llvm/Config/llvm-config.h>
#if LLVM_VERSION_MAJOR >= 17
#include <llvm/TargetParser/Triple.h>
#else
#include <llvm/ADT/Triple.h>
#endif
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/DenseSet.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/CodeGen/TargetSubtargetInfo.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/Target/TargetMachine.h>
#if defined(HAVE_TARGET_X86) && LLVM_VERSION_MAJOR >= 10
#include <llvm/IR/IntrinsicsX86.h>
#endif

#include <libasr/codegen/llvm_reductions.h>

#include <algorithm>
#include <utility>
#include <vector>

/*
Pairwise horizontal reductions of sums of squares.

A sum of the squares of components 0, 1 and 2 of a four element vector r,
computed for several vectors at once, is what `vhaddpd` computes, given the
squares r*r: two horizontal adds and one vertical add give the sums of four
vectors. SLP vectorizes such sums across the vectors instead, gathering one
component of each vector into a lane. How it places the vectors in lanes
depends on the LLVM version and the vector width: a transpose of four
vectors, two or eight lanes, partial sums of two lanes concatenated, or
differences taken before or after the gather.

So the rewrite does not match the shuffles. A root is a fast addition of
double vectors where every lane, followed through the additions and the
shuffles, is a sum of three squares. Each squared element is followed through
shuffles and fast elementwise operations to the vector elements it is
computed from. The three squares of a lane must be the same expression, one
element further on for each component; that expression on whole vectors is
r. The rewrite computes r for every lane and sums its squares with `vhaddpd`.

`vhaddpd` also adds component 3, so that component is replaced by a constant
zero unless every bit of it is known to be +0.0 or -0.0. Under fast math a
zero lane times a computed value is zero, which is what lets a loop-carried
component stay zero. A constant infinity or NaN is not: 0*Inf is NaN, and
the horizontal add would then read a lane the original code never used. The
rewrite runs after the vectorizer, only on x86 with AVX and only on a tree of
fast additions: an element added more than once is left unchanged.
*/

namespace LCompilers {

namespace {

using namespace llvm;

#if LLVM_VERSION_MAJOR >= 11

bool is_fast(Value *v) {
    return isa<FPMathOperator>(v)
        && cast<Instruction>(v)->getFastMathFlags().isFast();
}

// The squares that add up to one lane: each squared vector and its lane.
using Squares = SmallVector<std::pair<Value *, unsigned>, 3>;

// One node of a component expression, in preorder: a fast elementwise
// operation (`opcode`, its operands follow), or a leaf that reads element
// `lane` of the vector `leaf`.
struct Node {
    unsigned opcode = 0;
    Value *leaf = nullptr;
    unsigned lane = 0;
};

// A component expression is short, such as a difference of two positions.
// The bound keeps a shared subexpression from growing it.
const size_t max_component_nodes = 15;

// Express element `lane` of `v` as elementwise operations on elements of
// vectors of at least four doubles, looking through shuffles and element
// inserts and extracts. SLP places the elements of one component expression
// in different lanes and vectors, and sometimes subtracts before and
// sometimes after a shuffle; the expression is the same in every case.
bool lift(Value *v, unsigned lane, SmallVectorImpl<Node> &nodes) {
    if (nodes.size() >= max_component_nodes) {
        return false;
    }
    if (auto *shuffle = dyn_cast<ShuffleVectorInst>(v)) {
        int m = shuffle->getShuffleMask()[lane];
        auto *source = cast<FixedVectorType>(
            shuffle->getOperand(0)->getType());
        unsigned n = source->getNumElements();
        return m >= 0 && lift(shuffle->getOperand(m < (int)n ? 0 : 1),
            (unsigned)m % n, nodes);
    }
    if (auto *insert = dyn_cast<InsertElementInst>(v)) {
        auto *at = dyn_cast<ConstantInt>(insert->getOperand(2));
        if (!at) {
            return false;
        }
        return at->getZExtValue() == lane
            ? lift(insert->getOperand(1), 0, nodes)
            : lift(insert->getOperand(0), lane, nodes);
    }
    if (auto *extract = dyn_cast<ExtractElementInst>(v)) {
        auto *at = dyn_cast<ConstantInt>(extract->getOperand(1));
        return at && lift(extract->getOperand(0), at->getZExtValue(), nodes);
    }
    auto *i = dyn_cast<Instruction>(v);
    if (i && is_fast(i) && (i->getOpcode() == Instruction::FAdd
            || i->getOpcode() == Instruction::FSub
            || i->getOpcode() == Instruction::FMul)) {
        Node node;
        node.opcode = i->getOpcode();
        nodes.push_back(node);
        return lift(i->getOperand(0), lane, nodes)
            && lift(i->getOperand(1), lane, nodes);
    }
    auto *vector = dyn_cast<FixedVectorType>(v->getType());
    if (!vector || vector->getNumElements() < 4
            || !vector->getElementType()->isDoubleTy()) {
        return false;
    }
    Node node;
    node.leaf = v;
    node.lane = lane;
    nodes.push_back(node);
    return true;
}

// Given components 0, 1 and 2 of one four element vector, in some order,
// return the lane of each leaf that holds component 0. Each component is
// the same expression on the same vectors, one lane further on.
bool component_bases(const SmallVectorImpl<Node> *components[3],
        SmallVectorImpl<unsigned> &bases) {
    const SmallVectorImpl<Node> &a = *components[0];
    for (unsigned k = 1; k < 3; k++) {
        if (components[k]->size() != a.size()) {
            return false;
        }
    }
    // The component of each expression, from the lane of its first leaf.
    unsigned first = 0;
    while (first < a.size() && a[first].leaf == nullptr) {
        first++;
    }
    if (first == a.size()) {
        return false;
    }
    unsigned low = a[first].lane;
    for (unsigned k = 1; k < 3; k++) {
        low = std::min(low, (*components[k])[first].lane);
    }
    unsigned component[3];
    bool seen[3] = {false, false, false};
    for (unsigned k = 0; k < 3; k++) {
        component[k] = (*components[k])[first].lane - low;
        if (component[k] > 2 || seen[component[k]]) {
            return false;
        }
        seen[component[k]] = true;
    }
    bases.clear();
    for (size_t j = 0; j < a.size(); j++) {
        if (a[j].leaf == nullptr) {
            for (unsigned k = 1; k < 3; k++) {
                const Node &b = (*components[k])[j];
                if (b.leaf != nullptr || b.opcode != a[j].opcode) {
                    return false;
                }
            }
            bases.push_back(0);
            continue;
        }
        if (a[j].lane < component[0]) {
            return false;
        }
        unsigned base = a[j].lane - component[0];
        for (unsigned k = 1; k < 3; k++) {
            const Node &b = (*components[k])[j];
            if (b.leaf != a[j].leaf || b.lane != base + component[k]) {
                return false;
            }
        }
        bases.push_back(base);
    }
    return true;
}

// The second operand of a shuffle that reads only its first.
Value *unused(Value *v) {
#if LLVM_VERSION_MAJOR >= 12
    return PoisonValue::get(v->getType());
#else
    return UndefValue::get(v->getType());
#endif
}

// Build the four element vector whose components 0, 1 and 2 are the
// expression `nodes` (starting at `next`), with leaf `j` read from lanes
// bases[j] to bases[j] + 3. A lane past the end of a leaf is poison.
Value *emit(IRBuilder<> &builder, const SmallVectorImpl<Node> &nodes,
        const SmallVectorImpl<unsigned> &bases, size_t &next) {
    size_t j = next++;
    const Node &node = nodes[j];
    if (node.leaf == nullptr) {
        Value *a = emit(builder, nodes, bases, next);
        Value *b = emit(builder, nodes, bases, next);
        return builder.CreateBinOp((Instruction::BinaryOps)node.opcode, a, b);
    }
    unsigned n = cast<FixedVectorType>(node.leaf->getType())
        ->getNumElements();
    if (n == 4 && bases[j] == 0) {
        return node.leaf;
    }
    SmallVector<int, 4> mask;
    for (unsigned l = 0; l < 4; l++) {
        mask.push_back(bases[j] + l < n ? (int)(bases[j] + l) : -1);
    }
    return builder.CreateShuffleVector(node.leaf, unused(node.leaf), mask);
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

// Collect the squares that add up to element `lane` of `v`: through fast
// additions and shuffles down to fast squares. Each square is returned as the
// squared vector and the lane of it. Returns false if anything else is
// added, or if an element is reached twice: the sum is then a DAG, and
// counting that element once would drop a repeated term (s + s is 2*s).
bool collect_squares(Value *v, unsigned lane, Squares &squares,
        DenseSet<std::pair<Value *, unsigned>> &seen) {
    if (!seen.insert({v, lane}).second) {
        return false;
    }
    if (auto *shuffle = dyn_cast<ShuffleVectorInst>(v)) {
        int m = shuffle->getShuffleMask()[lane];
        unsigned n = cast<FixedVectorType>(
            shuffle->getOperand(0)->getType())->getNumElements();
        return m >= 0 && collect_squares(
            shuffle->getOperand(m < (int)n ? 0 : 1), (unsigned)m % n,
            squares, seen);
    }
    auto *i = dyn_cast<Instruction>(v);
    if (i && i->getOpcode() == Instruction::FAdd && is_fast(i)) {
        return collect_squares(i->getOperand(0), lane, squares, seen)
            && collect_squares(i->getOperand(1), lane, squares, seen);
    }
    Value *x = nullptr;
    if (!match_square(v, x)) {
        return false;
    }
    squares.push_back({x, lane});
    return true;
}

// A root is a fast addition of double vectors (two to eight lanes) where
// every lane is a sum of exactly three squares. The shuffles that feed it
// are part of the sum, so the additions below a root are not roots.
bool is_root(Instruction &i, SmallVectorImpl<Squares> &lanes) {
    auto *vector = dyn_cast<FixedVectorType>(i.getType());
    if (i.getOpcode() != Instruction::FAdd || !is_fast(&i) || !vector
            || !vector->getElementType()->isDoubleTy()
            || vector->getNumElements() < 2 || vector->getNumElements() > 8) {
        return false;
    }
    lanes.clear();
    for (unsigned lane = 0; lane < vector->getNumElements(); lane++) {
        Squares squares;
        DenseSet<std::pair<Value *, unsigned>> seen;
        if (!collect_squares(&i, lane, squares, seen) || squares.size() != 3) {
            return false;
        }
        lanes.push_back(squares);
    }
    return true;
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

    // The element is zero if every operand element is zero, or, when `any`
    // is set, if one of them is.
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

    // A constant element that is an infinity or a NaN. 0 times that element
    // is NaN, so it must not be treated as a zero lane.
    static bool nonfinite_constant_lane(Value *v, unsigned lane) {
        auto *c = dyn_cast<Constant>(v);
        if (!c) {
            return false;
        }
        if (c->getType()->isVectorTy()) {
            c = c->getAggregateElement(lane);
        } else if (lane != 0) {
            return false;
        }
        auto *fp = dyn_cast_or_null<ConstantFP>(c);
        if (!fp) {
            return false;
        }
        return fp->getValueAPF().isNaN() || fp->getValueAPF().isInfinity();
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
            case Instruction::FMul: {
                Value *a = i->getOperand(0);
                Value *b = i->getOperand(1);
                // Under fast math either factor being zero makes the lane
                // zero, including a computed weight. A constant infinity or
                // NaN does not: 0*Inf is NaN and vhaddpd would read it.
                bool unsafe = nonfinite_constant_lane(a, lane)
                    || nonfinite_constant_lane(b, lane);
                node.any = is_fast(i) && !unsafe;
                same(a);
                same(b);
                return true;
            }
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

// Whether `f` is compiled for x86 with AVX.
bool has_avx(const Function &f, const TargetMachine &tm) {
    const Triple &triple = tm.getTargetTriple();
    if (triple.getArch() != Triple::x86 && triple.getArch() != Triple::x86_64) {
        return false;
    }
    // The subtarget the code generator uses for `f`: its target-cpu and
    // target-features attributes over the defaults of `tm`. A CPU implies
    // its features, so the attribute alone does not tell whether AVX is on.
    return tm.getSubtargetImpl(f)->checkFeatures("+avx");
}

#endif // LLVM_VERSION_MAJOR >= 11

} // namespace

bool may_recombine_horizontal_reductions(const llvm::Function &f,
        const llvm::TargetMachine &tm) {
#if LLVM_VERSION_MAJOR >= 11
    if (f.isDeclaration() || !has_avx(f, tm)) {
        return false;
    }
    // Every rewrite starts from a root. The late cleanup only turns the
    // squared values into vectors the rewrite can read components from
    // (before it, SLP inserts scalars loaded from a stack slot), so only
    // the root is looked for here.
    llvm::SmallVector<Squares, 8> lanes;
    for (const llvm::BasicBlock &block : f) {
        for (const llvm::Instruction &i : block) {
            if (is_root(const_cast<llvm::Instruction &>(i), lanes)) {
                return true;
            }
        }
    }
    return false;
#else
    (void)f;
    (void)tm;
    return false;
#endif
}

bool recombine_horizontal_reductions(llvm::Function &f,
        const llvm::TargetMachine &tm) {
#if LLVM_VERSION_MAJOR >= 11
    if (!has_avx(f, tm)) {
        return false;
    }
    // Collect candidates first: rewriting inserts instructions.
    llvm::SmallVector<llvm::Instruction *, 4> roots;
    llvm::SmallVector<Squares, 8> lanes;
    for (llvm::BasicBlock &block : f) {
        for (llvm::Instruction &i : block) {
            if (is_root(i, lanes)) {
                roots.push_back(&i);
            }
        }
    }
    bool changed = false;
    for (llvm::Instruction *root : roots) {
        if (!is_root(*root, lanes)) {
            continue;
        }
        // Each lane of the root sums components 0, 1 and 2 of one vector
        // expression. Find that expression for every lane.
        llvm::SmallVector<llvm::SmallVector<Node, 8>, 8> expressions;
        llvm::SmallVector<llvm::SmallVector<unsigned, 8>, 8> bases;
        bool ok = true;
        for (Squares &squares : lanes) {
            llvm::SmallVector<Node, 8> components[3];
            const llvm::SmallVectorImpl<Node> *pointers[3];
            for (unsigned k = 0; k < 3 && ok; k++) {
                ok = lift(squares[k].first, squares[k].second, components[k]);
                pointers[k] = &components[k];
            }
            llvm::SmallVector<unsigned, 8> lane_bases;
            if (!ok || !component_bases(pointers, lane_bases)) {
                ok = false;
                break;
            }
            expressions.push_back(components[0]);
            bases.push_back(lane_bases);
        }
        llvm::Function *hadd = ok ? hadd_pd(*f.getParent()) : nullptr;
        if (!hadd) {
            continue;
        }
        llvm::IRBuilder<> builder(root);
        builder.setFastMathFlags(root->getFastMathFlags());
        LaneZero zeros;
        llvm::SmallVector<llvm::Value *, 8> squares;
        for (size_t j = 0; j < expressions.size(); j++) {
            size_t next = 0;
            llvm::Value *r = emit(builder, expressions[j], bases[j], next);
            llvm::Value *square = builder.CreateFMul(r, r);
            if (!zeros.is_zero(r, 3)) {
                // vhaddpd also adds component 3. A bitwise and does not
                // clear a poison lane, so take lanes 0, 1 and 2 from the
                // square and lane 3 from a constant zero.
                square = builder.CreateShuffleVector(square,
                    llvm::Constant::getNullValue(r->getType()),
                    llvm::ArrayRef<int>{0, 1, 2, 4});
            }
            squares.push_back(square);
        }
        // Four lanes at a time: two horizontal adds and one vertical add
        // give the four sums. A group with fewer lanes repeats its first.
        llvm::SmallVector<llvm::Value *, 2> groups;
        for (size_t g = 0; g < squares.size(); g += 4) {
            llvm::Value *x[4];
            for (size_t j = 0; j < 4; j++) {
                x[j] = squares[g + j < squares.size() ? g + j : g];
            }
            llvm::Value *t0 = builder.CreateCall(hadd, {x[0], x[1]});
            llvm::Value *t1 = builder.CreateCall(hadd, {x[2], x[3]});
            llvm::Value *y0 = builder.CreateShuffleVector(t0, t1,
                llvm::ArrayRef<int>{2, 3, 4, 5});
            llvm::Value *y1 = builder.CreateShuffleVector(t0, t1,
                llvm::ArrayRef<int>{0, 1, 6, 7});
            groups.push_back(builder.CreateFAdd(y0, y1));
        }
        llvm::Value *sum = groups[0];
        if (squares.size() != 4) {
            llvm::SmallVector<int, 8> mask;
            for (size_t j = 0; j < squares.size(); j++) {
                mask.push_back((int)j);
            }
            sum = builder.CreateShuffleVector(groups[0], groups.size() > 1
                ? groups[1] : unused(groups[0]), mask);
        }
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
