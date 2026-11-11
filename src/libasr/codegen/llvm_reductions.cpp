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
#include <llvm/IR/Operator.h>
#include <llvm/Target/TargetMachine.h>
#if defined(HAVE_TARGET_X86) && LLVM_VERSION_MAJOR >= 10
#include <llvm/IR/IntrinsicsX86.h>
#endif

#include <libasr/codegen/llvm_reductions.h>

#include <utility>
#include <vector>

/*
Pairwise horizontal reductions of sums of squares.

A sum of the squares of components 0, 1 and 2 of a four element vector r,
computed for several vectors at once, is what `vhaddpd` computes, given the
squares r*r: two horizontal adds and one vertical add give the sums of four
vectors. SLP vectorizes such sums across the vectors instead, gathering one
component of each vector into a lane. How it places the vectors in lanes
depends on the LLVM version: a transpose of four vectors, two lanes,
partial sums of two lanes concatenated, or differences taken before or
after the gather.

So the rewrite does not match the shuffles. A root is a fast addition of
two to four double lanes where every lane, followed through the additions
and the shuffles, is a sum of three squares. Each squared element is
followed through shuffles and fast elementwise operations to the vector
elements it is computed from. The three squares of a lane must be the same
expression, one element further on for each component; that expression on
whole vectors is r. The rewrite computes r for every lane and sums its
squares with `vhaddpd`.

Every vector the expression reads must be a four element double vector
that already holds components 0, 1 and 2 in lanes 0, 1 and 2, such as a
point stored as four doubles. The rewrite never builds such a vector from
other lanes: a vectorized loop over an array of three or four element
points loads several points into one wide vector, and shuffling each point
out of it costs more than the gather it replaces.

`vhaddpd` also adds component 3, so that component is replaced by a constant
zero unless every bit of it is known to be +0.0 or -0.0. The horizontal
add reads component 3 of values whose component 3 the original code may
never use, so the proof holds for any value there: a zero lane times a
finite constant is zero, but a zero lane times a computed value is not, as
the value may be an infinity or a NaN in that lane (0*Inf is NaN). The
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

// Whether the constant element index `at` is inside the vector type
// `type`. An index past the end makes the result poison.
bool in_range(ConstantInt *at, Type *type) {
    auto *vector = dyn_cast<FixedVectorType>(type);
    return vector && at->getValue().ult(vector->getNumElements());
}

// The squares that add up to one lane: each squared vector and its lane.
using Squares = SmallVector<std::pair<Value *, unsigned>, 3>;

// One node of a component expression, in preorder: a fast elementwise
// operation (`opcode`, its operands follow), a leaf that reads element
// `lane` of the vector `leaf`, or (with neither) a scalar loaded from a
// local variable, which the late cleanup may forward from a stored vector.
struct Node {
    unsigned opcode = 0;
    Value *leaf = nullptr;
    unsigned lane = 0;
};

// A component expression is short, such as a difference of two positions.
// The bound keeps a shared subexpression from growing it.
const size_t max_component_nodes = 15;

// Express element `lane` of `v` as elementwise operations on elements of
// four element double vectors, looking through shuffles and element inserts
// and extracts. SLP places the elements of one component expression in
// different lanes and vectors, and sometimes subtracts before and sometimes
// after a shuffle; the expression is the same in every case.
//
// Before the late cleanup, SLP still loads the scalars of a local array
// that its stores fill with vectors. `local_loads` accepts such a load as a
// leaf of unknown vector and lane.
bool lift(Value *v, unsigned lane, SmallVectorImpl<Node> &nodes,
        bool local_loads) {
    if (nodes.size() >= max_component_nodes) {
        return false;
    }
    if (local_loads && isa<LoadInst>(v) && v->getType()->isDoubleTy()) {
        Value *p = cast<LoadInst>(v)->getPointerOperand()->stripPointerCasts();
        while (auto *gep = dyn_cast<GEPOperator>(p)) {
            p = gep->getPointerOperand()->stripPointerCasts();
        }
        if (isa<AllocaInst>(p)) {
            nodes.push_back(Node());
            return true;
        }
    }
    if (auto *shuffle = dyn_cast<ShuffleVectorInst>(v)) {
        if (lane >= shuffle->getShuffleMask().size()) {
            return false;
        }
        int m = shuffle->getShuffleMask()[lane];
        auto *source = dyn_cast<FixedVectorType>(
            shuffle->getOperand(0)->getType());
        if (!source) {
            return false;
        }
        unsigned n = source->getNumElements();
        return m >= 0 && lift(shuffle->getOperand(m < (int)n ? 0 : 1),
            (unsigned)m % n, nodes, local_loads);
    }
    if (auto *insert = dyn_cast<InsertElementInst>(v)) {
        auto *at = dyn_cast<ConstantInt>(insert->getOperand(2));
        if (!at || !in_range(at, insert->getType())) {
            return false;
        }
        return at->getZExtValue() == lane
            ? lift(insert->getOperand(1), 0, nodes, local_loads)
            : lift(insert->getOperand(0), lane, nodes, local_loads);
    }
    if (auto *extract = dyn_cast<ExtractElementInst>(v)) {
        auto *at = dyn_cast<ConstantInt>(extract->getOperand(1));
        return at && in_range(at, extract->getOperand(0)->getType())
            && lift(extract->getOperand(0), at->getZExtValue(), nodes,
                local_loads);
    }
    auto *i = dyn_cast<Instruction>(v);
    if (i && is_fast(i) && (i->getOpcode() == Instruction::FAdd
            || i->getOpcode() == Instruction::FSub
            || i->getOpcode() == Instruction::FMul)) {
        Node node;
        node.opcode = i->getOpcode();
        nodes.push_back(node);
        return lift(i->getOperand(0), lane, nodes, local_loads)
            && lift(i->getOperand(1), lane, nodes, local_loads);
    }
    // The rewrite squares whole vectors that hold components 0, 1 and 2 of
    // a point in lanes 0, 1 and 2. A component read from a wider vector,
    // such as a vectorized load of several points, is not one.
    auto *vector = dyn_cast<FixedVectorType>(v->getType());
    if (!vector || vector->getNumElements() != 4
            || !vector->getElementType()->isDoubleTy() || lane > 2) {
        return false;
    }
    Node node;
    node.leaf = v;
    node.lane = lane;
    nodes.push_back(node);
    return true;
}

// Whether `components` are components 0, 1 and 2 of one four element vector
// expression, in some order: the same operations on the same vectors, with
// every leaf of component k read from lane k. A local load stands for any
// leaf.
bool same_expression(const SmallVectorImpl<Node> *components[3]) {
    const SmallVectorImpl<Node> &a = *components[0];
    for (unsigned k = 1; k < 3; k++) {
        if (components[k]->size() != a.size()) {
            return false;
        }
    }
    bool seen[3] = {false, false, false};
    for (unsigned k = 0; k < 3; k++) {
        const SmallVectorImpl<Node> &c = *components[k];
        unsigned component = 3;
        for (size_t j = 0; j < a.size(); j++) {
            if (c[j].opcode != a[j].opcode) {
                return false;
            }
            if (c[j].leaf == nullptr) {
                continue;
            }
            if ((a[j].leaf != nullptr && c[j].leaf != a[j].leaf)
                    || (component != 3 && c[j].lane != component)) {
                return false;
            }
            component = c[j].lane;
        }
        // A component of local loads only is decided after the cleanup.
        if (component == 3) {
            continue;
        }
        if (seen[component]) {
            return false;
        }
        seen[component] = true;
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
// expression `nodes` (starting at `next`), on the whole leaf vectors.
Value *emit(IRBuilder<> &builder, const SmallVectorImpl<Node> &nodes,
        size_t &next) {
    const Node &node = nodes[next++];
    if (node.leaf != nullptr) {
        return node.leaf;
    }
    Value *a = emit(builder, nodes, next);
    Value *b = emit(builder, nodes, next);
    return builder.CreateBinOp((Instruction::BinaryOps)node.opcode, a, b);
}

// A sum of three squares has two additions and the shuffles between them.
const size_t max_sum_nodes = 32;

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
// Stops at a fourth square or past `max_sum_nodes` elements, so a long chain
// of additions, each of which is tried as a root, is not walked to its end.
bool collect_squares(Value *v, unsigned lane, Squares &squares,
        DenseSet<std::pair<Value *, unsigned>> &seen) {
    if (seen.size() >= max_sum_nodes || !seen.insert({v, lane}).second) {
        return false;
    }
    if (auto *shuffle = dyn_cast<ShuffleVectorInst>(v)) {
        int m = shuffle->getShuffleMask()[lane];
        auto *source = dyn_cast<FixedVectorType>(
            shuffle->getOperand(0)->getType());
        if (!source) {
            return false;
        }
        unsigned n = source->getNumElements();
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
    return squares.size() <= 3;
}

// A root is a fast addition of double vectors (two to four lanes) where
// every lane is a sum of exactly three squares. The shuffles that feed it
// are part of the sum, so the additions below a root are not roots.
bool is_root(Instruction &i, SmallVectorImpl<Squares> &lanes) {
    auto *vector = dyn_cast<FixedVectorType>(i.getType());
    if (i.getOpcode() != Instruction::FAdd || !is_fast(&i) || !vector
            || !vector->getElementType()->isDoubleTy()
            || vector->getNumElements() < 2 || vector->getNumElements() > 4) {
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

// Each lane of a root sums components 0, 1 and 2 of one vector expression.
// Find that expression for every lane. `local_loads` is passed to `lift`.
bool expressions_of(const SmallVectorImpl<Squares> &lanes,
        SmallVectorImpl<SmallVector<Node, 8>> &expressions, bool local_loads) {
    expressions.clear();
    for (const Squares &squares : lanes) {
        SmallVector<Node, 8> components[3];
        const SmallVectorImpl<Node> *pointers[3];
        for (unsigned k = 0; k < 3; k++) {
            if (!lift(squares[k].first, squares[k].second, components[k],
                    local_loads)) {
                return false;
            }
            pointers[k] = &components[k];
        }
        if (!same_expression(pointers)) {
            return false;
        }
        expressions.push_back(components[0]);
    }
    return true;
}

// Whether element `lane` of `v` is known to be +0.0 or -0.0 (a scalar is
// element 0 of itself).
//
// The facts are the greatest fixed point of the rules below: every element
// reachable from the query is first assumed to be zero, and the assumption is
// dropped for each element with an operand element that is not zero, and
// then for each element that uses one whose assumption was dropped. A cycle
// through phis is therefore zero only if every value that enters the cycle
// is zero and every operation on it keeps the element zero, which is
// induction over the iterations of the loop.
class LaneZero {
public:
    bool is_zero(Value *v, unsigned lane) {
        Element root{v, lane};
        size_t first = nodes.size();
        if (!add(root)) {
            return constant_is_zero(v, lane);
        }
        // The elements of earlier queries are final, and none of them uses
        // a new one. A new element is not zero if an operand outside the new
        // ones is not, and then neither is any new element that uses it.
        SmallVector<unsigned, 16> work;
        for (size_t j = first; j < nodes.size(); j++) {
            Node &node = nodes[j];
            bool zero = !node.operands.empty();
            for (const Element &e : node.operands) {
                auto it = index.find(e);
                if (it == index.end()) {
                    zero = zero && constant_is_zero(e.first, e.second);
                } else if (it->second >= first) {
                    nodes[it->second].users.push_back(j);
                } else {
                    zero = zero && nodes[it->second].zero;
                }
            }
            if (!zero) {
                node.zero = false;
                work.push_back(j);
            }
        }
        while (!work.empty()) {
            for (unsigned user : nodes[work.pop_back_val()].users) {
                if (nodes[user].zero) {
                    nodes[user].zero = false;
                    work.push_back(user);
                }
            }
        }
        return nodes[index[root]].zero;
    }

private:
    using Element = std::pair<Value *, unsigned>;

    // The element is zero if it has operand elements and every one of them
    // is zero. `users` are the elements that have this one as an operand.
    struct Node {
        SmallVector<Element, 3> operands;
        SmallVector<unsigned, 2> users;
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

    // A constant element that is finite: 0 times it is zero.
    static bool finite_constant_lane(Value *v, unsigned lane) {
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
        return fp->getValueAPF().isFinite();
    }

    static unsigned width(Type *type) {
        auto *vector = dyn_cast<FixedVectorType>(type);
        return vector ? vector->getNumElements() : 1;
    }

    // The operand elements that decide element `lane` of instruction `i`, or
    // false if the element is not known to be zero whatever its operands are.
    bool rule(Instruction *i, unsigned lane, Node &node) const {
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
                // A zero times a finite constant is zero. A zero times a
                // computed value is not: the value may be an infinity or a
                // NaN in a lane the original code never reads, and vhaddpd
                // would read it. So the product is zero if both factors
                // are, or if one is and the other is a finite constant.
                if (finite_constant_lane(a, lane)) {
                    same(b);
                } else if (finite_constant_lane(b, lane)) {
                    same(a);
                } else {
                    same(a);
                    same(b);
                }
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
                if (lane >= shuffle->getShuffleMask().size()) {
                    return false;
                }
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
                if (!at || !in_range(at, i->getType())) {
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
                if (!at || lane != 0
                        || !in_range(at, i->getOperand(0)->getType())) {
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
    // A root whose lanes the rewrite can read from four element vectors.
    // The late cleanup forwards the vectors stored to a local array to the
    // scalars SLP loads from it, so such a load may still become one.
    llvm::SmallVector<Squares, 8> lanes;
    llvm::SmallVector<llvm::SmallVector<Node, 8>, 8> expressions;
    for (const llvm::BasicBlock &block : f) {
        for (const llvm::Instruction &i : block) {
            if (is_root(const_cast<llvm::Instruction &>(i), lanes)
                    && expressions_of(lanes, expressions, true)) {
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
        llvm::SmallVector<llvm::SmallVector<Node, 8>, 8> expressions;
        bool ok = expressions_of(lanes, expressions, false);
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
            llvm::Value *r = emit(builder, expressions[j], next);
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
        // Two horizontal adds and one vertical add give the sums of four
        // lanes. With fewer lanes, the missing ones repeat the first.
        llvm::Value *x[4];
        for (size_t j = 0; j < 4; j++) {
            x[j] = squares[j < squares.size() ? j : 0];
        }
        llvm::Value *t0 = builder.CreateCall(hadd, {x[0], x[1]});
        llvm::Value *t1 = builder.CreateCall(hadd, {x[2], x[3]});
        llvm::Value *y0 = builder.CreateShuffleVector(t0, t1,
            llvm::ArrayRef<int>{2, 3, 4, 5});
        llvm::Value *y1 = builder.CreateShuffleVector(t0, t1,
            llvm::ArrayRef<int>{0, 1, 6, 7});
        llvm::Value *sum = builder.CreateFAdd(y0, y1);
        if (squares.size() != 4) {
            llvm::SmallVector<int, 4> mask;
            for (size_t j = 0; j < squares.size(); j++) {
                mask.push_back((int)j);
            }
            sum = builder.CreateShuffleVector(sum, unused(sum), mask);
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
