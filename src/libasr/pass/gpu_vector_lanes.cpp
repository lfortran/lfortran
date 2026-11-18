#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/gpu_affine.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/pass/gpu_loop_utils.h>
#include <libasr/pass/gpu_offload_stop.h>
#include <libasr/pass/gpu_vector_lanes.h>
#include <libasr/pass/gpu_wide_loads.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/pass_utils.h>

#include <algorithm>
#include <functional>
#include <limits>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>

namespace LCompilers {

/*
The iterations of a kernel, each on the lanes of a SIMD-group.

A kernel runs one iteration of its loop on one thread. When the loop has
few iterations -- a training step over a mini-batch of one, a loop over the
layers of a network -- most of the GPU idles, and each iteration is a long
chain of dependent memory accesses on one thread: every element of every
array statement inside it, one after another, each waiting for the last.
A GPU thread does not run ahead of a load the way a CPU core does, so such
a chain costs a memory latency per element.

The loops inside an iteration that come from array statements (and the
other loops whose iterations are independent) can share that work out. So
a kernel this pass rewrites runs each iteration on `lanes` consecutive
threads, the *lanes* of the iteration, all in one SIMD-group: they wait for
each other with a barrier that costs a few cycles, and see each other's
writes after it. The thread index the kernel reads (GpuThreadIndex) then
numbers the iterations, the lanes of one iteration share it, and
GpuLaneIndex tells them apart. The kernel takes the lanes as its last
parameter (gpu_kernel_layout.lanes), and the host launches `lanes` threads
per iteration: the device's SIMD-group width (GpuDeviceCapabilities::
lanes_per_iteration) when the launch has at most `lanes_iterations_max`
iterations, and one otherwise, where the iterations alone fill the GPU and
the lanes would only repeat each other's work -- unless the lanes read along
the columns that the iterations take one each of, which one lane per
iteration would read 32 at a time: then up to `lanes_column_iterations_max`
(LaneRewriter::pays_at_any_size), and on a device whose lanes can be the
whole block (GpuDeviceCapabilities::lanes_column_block) the lanes of such a
kernel are all the threads of a block, one iteration per block: they
combine, broadcast and wait for each other through shared memory. A launch
of at most `lanes_block_iterations_max` iterations, which fills a small part
of the GPU even with a warp each, gives each iteration the lanes of a whole
block (`lanes_block`, where the device combines values across its
SIMD-groups): the warps of the block share the loops out and meet in shared
memory behind a block barrier.

Every lane of an iteration runs the body of the iteration, with the same
values in its private scalars, except for these statements:

* A *distributed* loop runs its iterations on the lanes in turn: lane k
  takes iterations k, k + lanes, ... (its start moves by the lane, its step
  is multiplied by the lanes), and the lanes wait for each other after it.
  A loop is distributed when its iterations are independent, which this
  pass checks rather than assumes:
  - its body has only assignments, `if`, loops, calls of pure procedures
    (or of functions that have no effect besides their value, as the
    compiler's own implementations of intrinsic functions, which are not
    declared pure: side_effect_free), and the `stop` handling of
    gpu_offload_stop.h;
  - every array it assigns an element of is shared by the lanes (a kernel
    argument, a workspace, what a pointer points to), and is assigned at
    one subscript list only, which has the loop variable, plus or minus
    something the loop does not change, as one of its subscripts: two
    iterations assign different elements;
  - it reads such an array only at that same subscript list, or at one
    that is a different element in every iteration (a subscript the loop
    does not change, plus another constant: `a(i, l)`, `a(k, l - 1)`), and
    asks about its shape (`size`, `lbound`) otherwise: no iteration reads
    what another assigns;
  - every scalar it assigns, its variable included, is assigned first in
    every iteration and is not read after the loop before it is assigned
    again: each lane may hold its own value. The site and the code of a
    stop are the exception: a lane that stops returns, so on any lane that
    goes on they still hold the zero they started with.
  No other loop of a distributed loop is distributed; inner loops run on
  each lane one after another. Before that, a distributed loop is joined
  with the loops right after it over the same iterations, as long as the
  joined loop can be distributed too and neither loop needs the value a
  scalar of the other has after it: one statement per loop becomes one
  statement per element, and the lanes wait once. And a distributed loop
  whose body is scalar assignments and one inner loop, which tells the
  elements it assigns apart as well, has the pairs of iterations of the two
  shared out instead of its own (`collapsible`).
* A *reduction* loop is distributed the same way where the device's lanes
  can combine values (GpuDeviceCapabilities::lane_reductions: an NVIDIA
  GPU, whose warps run the lanes, and Metal, whose SIMD-groups do, for
  values of at most lane_reduction_max_kind bytes): its iterations are
  independent but for private scalars each accumulates into, by statements
  `s = s op e` of its body (`+`, `-`, `*`, `max`, `min`, one operation per
  scalar, with an `e` that does not name `s`), and its body -- statements,
  and the blocks and loops in them -- only assigns private scalars --
  accumulators, scalars of its own in every iteration and dead after it,
  and inductions: integers its body adds constants to and reads otherwise,
  as the index into the second array of an array statement whose sections
  start at different subscripts -- and calls pure functions. The lanes but
  the first start every accumulator at the identity of its operation,
  every lane starts an induction at what it holds in the first iteration
  the lane takes, each accumulates its iterations, and after the loop
  every lane gets what they hold combined (GpuLaneReduce): the inner
  product of a matrix-vector product, the forces on one body. What the loop
  leaves in its variable and in an induction is set after it.
  An independent loop that is a `do concurrent` inside the iteration of at
  least a warp of iterations is the exception: it says its own iterations are
  the ones to share out (the positions of attention, each with the plain loop
  of its sum on one lane), so the phases of a step written as `do concurrent`
  loops after each other run on all the lanes, one after the other behind a
  barrier (and it is not asked to tell its iterations apart by the
  subscripts of what they assign: the `do concurrent` says they do). The
  loop is kept twice, and the iterations decide when the kernel
  runs.
  An independent loop whose body holds such a reduction loop of a number
  of iterations not known at compile time has that sum shared out instead
  of its own iterations, which may be fewer than the lanes (a sum split in
  a few parts); one whose sums have a fixed length (the groups of a
  quantized matrix-vector product) is shared out itself.
  Where a reduction loop of the iteration holds a reduction loop of a fixed
  number of iterations (GpuDeviceCapabilities::lane_inner_reductions_first:
  the 64 codes of a group of a quantized matrix-vector product, a loop of 16
  after gpu_residue_unroll), the outer one is not shared out: every lane
  goes through it, and the lanes share the inner one and combine its sum.
  Lane k then reads the k-th word of a group and the k-th elements of the
  vector, so the lanes of a warp read consecutive addresses, where one group
  per lane read 64 elements apart. Such a loop has fewer iterations than a
  warp has lanes, so the iteration gets as many lanes as that loop has
  iterations (16), and a warp runs two of them.
* A *single* statement runs on the first lane only: a statement outside a
  distributed loop that has an effect the lanes must not repeat -- one that
  assigns shared memory, calls a procedure that is not pure, takes elements
  of the device heap. The other lanes wait for it, and then take every
  private scalar it assigned from the first lane (GpuLaneBroadcast).
  Consecutive single statements run together.
* A loop, `if` or block that holds a distributed loop runs on every lane,
  which go through it together; what is inside is rewritten in turn. A
  block that holds none is a statement like any other: it runs on every
  lane, or on the first alone, without its own locals, which are gone
  after it, to hand on to the others.

The lanes run the statements between two barriers each at its own pace, so
before a single statement or a distributed loop that assigns shared memory
which the statements every lane ran since the last barrier read, the lanes
wait for each other: one lane would assign an element another still reads
(`s = a(1)` and then `a(j) = a(j) - s`). A loop whose next iteration assigns
what its body read last waits at the end of its body as well.

Where the lanes of an iteration may be the whole block (`lanes_block`), the
arrays of the iteration -- locals of the kernel or of a block it runs
outside every loop, of constant extents, of integers, reals or logicals,
named only by their elements and shape, block_array_bytes_max of them
together, those of fewer elements than a SIMD-group has lanes only with
one of as many -- can live in
the shared memory of the block (`__shared__`) instead of each lane's own:
then the lanes see the same of them, as of a kernel argument, and the loops
that fill them are distributed (each element assigned by one lane, at a
subscript that tells the iterations apart, also in a `do concurrent`, which
may define a scratch array of its own in each of its iterations), with the
barrier after them that every later read waits for. Scalars stay each
lane's own. The kernel is tried with such arrays first, and those that no
distributed loop assigns are left private (a lane-private copy each lane
fills itself), as are all of them when it cannot be rewritten so. A kernel
that keeps arrays in shared memory runs one iteration per block at every
launch, whatever its number of iterations, on blocks of as many threads as
the iteration has lanes: the smallest power of two from a SIMD-group to the
block that is at least the iterations of each distributed loop, known at
compile time, or the block. The lanes are then a constant of the kernel,
which is written once.

In such a kernel, a distributed loop of fewer iterations than the block has
lanes (128 columns of a state, 256 lanes) whose body holds long loops of
its own (the inner products of a column and its update) runs each of its
iterations on several lanes (`parts`, plan_parts): they share out those
inner loops whose iterations are independent but for private scalars they
accumulate into, and combine what they accumulated through shared memory,
behind a barrier. A lane counts the iterations it takes of such a loop,
and of two of them over the same iterations, the second takes the elements
of an array the first read from registers (hold_elements), as a
hand-written kernel keeps a column of the state.

A reduction loop right after a distributed loop over the same iterations,
which reads what that loop assigns only where it assigns it, is joined
into it (fold_reduction): each lane accumulates the elements it has just
computed (the norms of the vectors a phase works out) in the same pass,
and the lanes combine what they hold after it. And in a kernel with arrays
the block shares, the elements of the arguments the kernel only reads that
the iteration reads after earlier phases and their barriers (the gates of
a step, the weights of its last phase, the elements of a column a lane
keeps in registers) are read at the start of the iteration (EarlyLoads),
where the loads of the first phase hide them.

A kernel is rewritten only if it has a loop to distribute, and only if all
of the following holds of every single statement: it does not return (a
stop on the first lane alone would leave the others going on with values
it never broadcast), it assigns no private array (a lane-private copy the
others would miss) and every private scalar it assigns is an integer, a
real or a logical of 4 bytes. Nor is a kernel rewritten that assigns an
element of a kernel argument that every iteration assigns (no subscript
depends on the iteration, and no test such as `if (i == 1)` lets one
iteration alone assign it): an array declared outside the loop that each
iteration defines before using it, as a temporary. Fortran gives each
iteration its own, but a launch passes one array to all threads, so the
iterations race on it; lanes that wait for each other would make that show
more often than threads that do not. Otherwise the kernel runs one thread
per iteration, as before. A lane that stops inside a distributed loop records
the stop and returns, as a thread does (gpu_offload_stop.h); the others
return too as soon as the loop is done.

The pass runs after `gpu_kernel_finalize`: the host has already replayed
the kernel's prelude per iteration to size the workspaces, and a workspace
slice belongs to an iteration, shared by its lanes.
*/

namespace {

// Whether two expressions are the same, written the same way. False for
// anything this does not know.
bool same_expr(ASR::expr_t *a, ASR::expr_t *b) {
    if (a == b) return true;
    if (a == nullptr || b == nullptr || a->type != b->type) return false;
    switch (a->type) {
        case ASR::exprType::Var:
            return ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(a)->m_v)
                == ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(b)->m_v);
        case ASR::exprType::IntegerConstant:
            return ASR::down_cast<ASR::IntegerConstant_t>(a)->m_n
                == ASR::down_cast<ASR::IntegerConstant_t>(b)->m_n;
        case ASR::exprType::IntegerBinOp: {
            auto *x = ASR::down_cast<ASR::IntegerBinOp_t>(a);
            auto *y = ASR::down_cast<ASR::IntegerBinOp_t>(b);
            return x->m_op == y->m_op && same_expr(x->m_left, y->m_left)
                && same_expr(x->m_right, y->m_right);
        }
        case ASR::exprType::IntegerUnaryMinus:
            return same_expr(
                ASR::down_cast<ASR::IntegerUnaryMinus_t>(a)->m_arg,
                ASR::down_cast<ASR::IntegerUnaryMinus_t>(b)->m_arg);
        case ASR::exprType::Cast: {
            auto *x = ASR::down_cast<ASR::Cast_t>(a);
            auto *y = ASR::down_cast<ASR::Cast_t>(b);
            return x->m_kind == y->m_kind && same_expr(x->m_arg, y->m_arg)
                && ASRUtils::check_equal_type(x->m_type, y->m_type, nullptr,
                    nullptr);
        }
        case ASR::exprType::ArrayPhysicalCast:
            return same_expr(
                ASR::down_cast<ASR::ArrayPhysicalCast_t>(a)->m_arg,
                ASR::down_cast<ASR::ArrayPhysicalCast_t>(b)->m_arg);
        case ASR::exprType::StructInstanceMember: {
            auto *x = ASR::down_cast<ASR::StructInstanceMember_t>(a);
            auto *y = ASR::down_cast<ASR::StructInstanceMember_t>(b);
            return ASRUtils::symbol_get_past_external(x->m_m)
                    == ASRUtils::symbol_get_past_external(y->m_m)
                && same_expr(x->m_v, y->m_v);
        }
        case ASR::exprType::ArrayItem: {
            auto *x = ASR::down_cast<ASR::ArrayItem_t>(a);
            auto *y = ASR::down_cast<ASR::ArrayItem_t>(b);
            if (x->n_args != y->n_args || !same_expr(x->m_v, y->m_v)) {
                return false;
            }
            for (size_t i = 0; i < x->n_args; i++) {
                if (!same_expr(x->m_args[i].m_left, y->m_args[i].m_left)
                        || !same_expr(x->m_args[i].m_right,
                            y->m_args[i].m_right)
                        || !same_expr(x->m_args[i].m_step,
                            y->m_args[i].m_step)) {
                    return false;
                }
            }
            return true;
        }
        default:
            return false;
    }
}

// Whether two expressions of a loop body are the same, written the same way,
// with the operations and calls of values (same_expr, and the arithmetic, the
// intrinsic functions and the calls of functions on them).
bool same_value(ASR::expr_t *a, ASR::expr_t *b) {
    if (a == b) return true;
    if (a == nullptr || b == nullptr || a->type != b->type) return false;
    switch (a->type) {
        case ASR::exprType::RealBinOp: {
            auto *x = ASR::down_cast<ASR::RealBinOp_t>(a);
            auto *y = ASR::down_cast<ASR::RealBinOp_t>(b);
            return x->m_op == y->m_op && same_value(x->m_left, y->m_left)
                && same_value(x->m_right, y->m_right);
        }
        case ASR::exprType::RealUnaryMinus:
            return same_value(
                ASR::down_cast<ASR::RealUnaryMinus_t>(a)->m_arg,
                ASR::down_cast<ASR::RealUnaryMinus_t>(b)->m_arg);
        case ASR::exprType::IntegerBinOp: {
            auto *x = ASR::down_cast<ASR::IntegerBinOp_t>(a);
            auto *y = ASR::down_cast<ASR::IntegerBinOp_t>(b);
            return x->m_op == y->m_op && same_value(x->m_left, y->m_left)
                && same_value(x->m_right, y->m_right);
        }
        case ASR::exprType::Cast: {
            auto *x = ASR::down_cast<ASR::Cast_t>(a);
            auto *y = ASR::down_cast<ASR::Cast_t>(b);
            return x->m_kind == y->m_kind && same_value(x->m_arg, y->m_arg)
                && ASRUtils::check_equal_type(x->m_type, y->m_type, nullptr,
                    nullptr);
        }
        case ASR::exprType::IntrinsicElementalFunction: {
            auto *x = ASR::down_cast<ASR::IntrinsicElementalFunction_t>(a);
            auto *y = ASR::down_cast<ASR::IntrinsicElementalFunction_t>(b);
            if (x->m_intrinsic_id != y->m_intrinsic_id
                    || x->n_args != y->n_args) {
                return false;
            }
            for (size_t i = 0; i < x->n_args; i++) {
                if (!same_value(x->m_args[i], y->m_args[i])) return false;
            }
            return true;
        }
        case ASR::exprType::FunctionCall: {
            auto *x = ASR::down_cast<ASR::FunctionCall_t>(a);
            auto *y = ASR::down_cast<ASR::FunctionCall_t>(b);
            if (ASRUtils::symbol_get_past_external(x->m_name)
                    != ASRUtils::symbol_get_past_external(y->m_name)
                    || x->n_args != y->n_args) {
                return false;
            }
            for (size_t i = 0; i < x->n_args; i++) {
                if (!same_value(x->m_args[i].m_value,
                        y->m_args[i].m_value)) {
                    return false;
                }
            }
            return true;
        }
        case ASR::exprType::ArrayItem: {
            auto *x = ASR::down_cast<ASR::ArrayItem_t>(a);
            auto *y = ASR::down_cast<ASR::ArrayItem_t>(b);
            if (x->n_args != y->n_args || !same_expr(x->m_v, y->m_v)) {
                return false;
            }
            for (size_t i = 0; i < x->n_args; i++) {
                if (!same_value(x->m_args[i].m_right, y->m_args[i].m_right)
                        || x->m_args[i].m_left || y->m_args[i].m_left
                        || x->m_args[i].m_step || y->m_args[i].m_step) {
                    return false;
                }
            }
            return true;
        }
        default:
            return same_expr(a, b);
    }
}

// The variable a designator starts from, or nullptr.
ASR::symbol_t* root_of(ASR::expr_t *e) {
    while (true) {
        switch (e->type) {
            case ASR::exprType::Var:
                return ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(e)->m_v);
            case ASR::exprType::ArrayItem:
                e = ASR::down_cast<ASR::ArrayItem_t>(e)->m_v;
                break;
            case ASR::exprType::ArraySection:
                e = ASR::down_cast<ASR::ArraySection_t>(e)->m_v;
                break;
            case ASR::exprType::StructInstanceMember:
                e = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v;
                break;
            case ASR::exprType::ArrayPhysicalCast:
                e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
                break;
            default:
                return nullptr;
        }
    }
}

// The variable and components a designator names, its subscripts left out.
std::vector<ASR::symbol_t*> path_of(ASR::expr_t *e) {
    std::vector<ASR::symbol_t*> path;
    while (true) {
        switch (e->type) {
            case ASR::exprType::Var:
                path.insert(path.begin(), ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(e)->m_v));
                return path;
            case ASR::exprType::ArrayItem:
                e = ASR::down_cast<ASR::ArrayItem_t>(e)->m_v;
                break;
            case ASR::exprType::ArraySection:
                e = ASR::down_cast<ASR::ArraySection_t>(e)->m_v;
                break;
            case ASR::exprType::StructInstanceMember: {
                auto *m = ASR::down_cast<ASR::StructInstanceMember_t>(e);
                path.insert(path.begin(),
                    ASRUtils::symbol_get_past_external(m->m_m));
                e = m->m_v;
                break;
            }
            case ASR::exprType::ArrayPhysicalCast:
                e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
                break;
            default:
                return {};
        }
    }
}

// Whether one path is a prefix of the other: what they name overlaps.
bool overlapping(const std::vector<ASR::symbol_t*> &a,
        const std::vector<ASR::symbol_t*> &b) {
    size_t n = std::min(a.size(), b.size());
    for (size_t i = 0; i < n; i++) {
        if (a[i] != b[i]) return false;
    }
    return true;
}

bool side_effect_free(ASR::Function_t *f, std::set<ASR::Function_t*> &seen);

bool pure_callee(ASR::symbol_t *name, std::set<ASR::Function_t*> &seen) {
    ASR::symbol_t *s = ASRUtils::symbol_get_past_external(name);
    if (ASR::is_a<ASR::StructMethodDeclaration_t>(*s)) {
        s = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::StructMethodDeclaration_t>(s)->m_proc);
    }
    if (!ASR::is_a<ASR::Function_t>(*s)) return false;
    ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(s);
    ASR::FunctionType_t *t = ASRUtils::get_FunctionType(f);
    return t->m_pure || t->m_elemental || side_effect_free(f, seen);
}

bool pure_callee(ASR::symbol_t *name) {
    std::set<ASR::Function_t*> seen;
    return pure_callee(name, seen);
}

// Whether a call of `f` has no effect but its value, although `f` is not
// declared pure: every dummy is `intent(in)`, and its body only assigns its
// own variables, calls only procedures of the same kind, and does nothing
// else (no input or output, no stop). The compiler's own implementations of
// intrinsic functions (`mod`, `modulo`, `abs`, ...) are such functions.
bool side_effect_free(ASR::Function_t *f, std::set<ASR::Function_t*> &seen) {
    if (!seen.insert(f).second) return true;
    ASR::FunctionType_t *t = ASRUtils::get_FunctionType(f);
    if (t->m_deftype != ASR::deftypeType::Implementation
            || f->m_return_var == nullptr) {
        return false;
    }
    for (size_t i = 0; i < f->n_args; i++) {
        if (!ASR::is_a<ASR::Var_t>(*f->m_args[i])) return false;
        ASR::symbol_t *a = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(f->m_args[i])->m_v);
        if (!ASR::is_a<ASR::Variable_t>(*a)
                || ASR::down_cast<ASR::Variable_t>(a)->m_intent
                    != ASR::intentType::In) {
            return false;
        }
    }
    class Body : public ASR::BaseWalkVisitor<Body> {
    public:
        ASR::Function_t *f;
        std::set<ASR::Function_t*> &seen;
        bool ok = true;
        Body(ASR::Function_t *f_, std::set<ASR::Function_t*> &seen_)
            : f(f_), seen(seen_) {}
        bool own(ASR::expr_t *e) {
            ASR::symbol_t *r = root_of(e);
            if (r == nullptr || !ASR::is_a<ASR::Variable_t>(*r)) return false;
            for (SymbolTable *st = ASR::down_cast<ASR::Variable_t>(r)
                    ->m_parent_symtab; st != nullptr; st = st->parent) {
                if (st == f->m_symtab) return true;
            }
            return false;
        }
        void visit_stmt(const ASR::stmt_t &x) {
            switch (x.type) {
                case ASR::stmtType::Assignment: {
                    auto *a = ASR::down_cast<ASR::Assignment_t>(&x);
                    if (a->m_overloaded || !own(a->m_target)) ok = false;
                    break;
                }
                case ASR::stmtType::DoLoop: {
                    auto *d = ASR::down_cast<ASR::DoLoop_t>(&x);
                    if (d->m_head.m_v && !own(d->m_head.m_v)) ok = false;
                    break;
                }
                case ASR::stmtType::If:
                case ASR::stmtType::WhileLoop:
                case ASR::stmtType::Exit:
                case ASR::stmtType::Cycle:
                case ASR::stmtType::Return:
                    break;
                default:
                    ok = false;
                    return;
            }
            BaseWalkVisitor::visit_stmt(x);
        }
        void visit_FunctionCall(const ASR::FunctionCall_t &x) {
            if (!pure_callee(x.m_name, seen)) ok = false;
            BaseWalkVisitor::visit_FunctionCall(x);
        }
        void visit_IntrinsicImpureFunction(
                const ASR::IntrinsicImpureFunction_t &) {
            ok = false;
        }
        void visit_GpuAtomicFetchAdd(const ASR::GpuAtomicFetchAdd_t &) {
            ok = false;
        }
    } body(f, seen);
    for (size_t i = 0; i < f->n_body && body.ok; i++) {
        body.visit_stmt(*f->m_body[i]);
    }
    return body.ok;
}

// Whether `e` reads the variable `s`.
class Mentions : public ASR::BaseWalkVisitor<Mentions> {
public:
    ASR::symbol_t *s;
    bool found = false;
    std::set<ASR::symbol_t*> blocks;
    explicit Mentions(ASR::symbol_t *s_) : s(s_) {}
    void visit_Var(const ASR::Var_t &x) {
        if (ASRUtils::symbol_get_past_external(x.m_v) == s) found = true;
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!blocks.insert(b).second) return;
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(b);
        for (auto &item : block->m_symtab->get_scope()) {
            if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(item.second);
            visit_ttype(*v->m_type);
            if (v->m_symbolic_value) visit_expr(*v->m_symbolic_value);
        }
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
};

bool mentions(const ASR::stmt_t &x, ASR::symbol_t *s) {
    Mentions m(s);
    m.visit_stmt(x);
    return m.found;
}

bool mentions(ASR::expr_t *e, ASR::symbol_t *s) {
    if (e == nullptr) return false;
    Mentions m(s);
    m.visit_expr(*e);
    return m.found;
}

enum class Access { None, Read, Write };

// The first access of `s` in body[from, to): a write when the first
// statement that names it assigns it without reading it first. A block runs
// its statements in turn, so the first access in it is the first access.
Access first_access(ASR::stmt_t **body, size_t from, size_t to,
        ASR::symbol_t *s) {
    for (size_t i = from; i < to; i++) {
        ASR::stmt_t *x = body[i];
        if (ASR::is_a<ASR::BlockCall_t>(*x)) {
            ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(
                ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(x)->m_m));
            Access a = first_access(block->m_body, 0, block->n_body, s);
            if (a != Access::None) return a;
            continue;
        }
        if (ASR::is_a<ASR::Assignment_t>(*x)) {
            auto *a = ASR::down_cast<ASR::Assignment_t>(x);
            if (ASR::is_a<ASR::Var_t>(*a->m_target)
                    && root_of(a->m_target) == s) {
                return mentions(a->m_value, s) ? Access::Read
                    : Access::Write;
            }
        } else if (ASR::is_a<ASR::DoLoop_t>(*x)) {
            auto *d = ASR::down_cast<ASR::DoLoop_t>(x);
            bool header = mentions(d->m_head.m_start, s)
                || mentions(d->m_head.m_end, s)
                || mentions(d->m_head.m_increment, s);
            if (d->m_head.m_v && root_of(d->m_head.m_v) == s) {
                return header ? Access::Read : Access::Write;
            }
            // A loop whose body assigns `s` before it reads it, as a loop
            // inside it over `s` does, or reads it only in such loops of its
            // own: what `s` held before is not read in it, and if it runs no
            // iteration, is read only after it.
            if (!header && d->n_orelse == 0 && first_access(d->m_body, 0,
                    d->n_body, s) != Access::Read) {
                continue;
            }
        } else if (ASR::is_a<ASR::DoConcurrentLoop_t>(*x)) {
            auto *d = ASR::down_cast<ASR::DoConcurrentLoop_t>(x);
            bool index = false, header = false;
            for (size_t h = 0; h < d->n_head; h++) {
                index |= d->m_head[h].m_v && root_of(d->m_head[h].m_v) == s;
                header |= mentions(d->m_head[h].m_start, s)
                    || mentions(d->m_head[h].m_end, s)
                    || mentions(d->m_head[h].m_increment, s);
            }
            if (index) return header ? Access::Read : Access::Write;
            // As for a loop above: a body that assigns `s` first.
            if (!header && first_access(d->m_body, 0, d->n_body, s)
                    != Access::Read) {
                continue;
            }
        } else if (ASR::is_a<ASR::If_t>(*x)) {
            // An `if` both of whose branches assign `s` before they read it
            // (the cosine and sine of a rotation, by the part of a vector);
            // or that reads it in neither.
            auto *c = ASR::down_cast<ASR::If_t>(x);
            if (!mentions(c->m_test, s)) {
                Access a = first_access(c->m_body, 0, c->n_body, s);
                Access b = first_access(c->m_orelse, 0, c->n_orelse, s);
                if (a == Access::Write && b == Access::Write) {
                    return Access::Write;
                }
                if (a != Access::Read && b != Access::Read) continue;
            }
        }
        if (mentions(*x, s)) return Access::Read;
    }
    return Access::None;
}

// Where a statement is: in a list, at an index, and the statement owning
// the list (a loop, an `if`, a block), or nullptr for the kernel's body.
struct Frame {
    ASR::stmt_t **body;
    size_t n_body;
    size_t index;
    ASR::stmt_t *owner;
};

// Whether the value `s` has after the statement the innermost frame points
// at -- a loop that assigns `s` first in each of its iterations -- is never
// read: every way on from it assigns `s` before reading it, comes back to
// that loop, or leaves the kernel.
bool dead_after(const std::vector<Frame> &frames, ASR::symbol_t *s) {
    auto header_reads = [&](ASR::stmt_t *owner) {
        if (owner == nullptr) return false;
        if (ASR::is_a<ASR::WhileLoop_t>(*owner)) {
            return mentions(ASR::down_cast<ASR::WhileLoop_t>(owner)->m_test,
                s);
        }
        if (ASR::is_a<ASR::DoLoop_t>(*owner)) {
            auto *d = ASR::down_cast<ASR::DoLoop_t>(owner);
            return (d->m_head.m_v && root_of(d->m_head.m_v) == s)
                || mentions(d->m_head.m_start, s)
                || mentions(d->m_head.m_end, s)
                || mentions(d->m_head.m_increment, s);
        }
        return false;
    };
    for (size_t k = frames.size(); k-- > 0;) {
        const Frame &f = frames[k];
        Access a = first_access(f.body, f.index + 1, f.n_body, s);
        if (a == Access::Write) return true;
        if (a == Access::Read) return false;
        if (f.owner == nullptr || !(ASR::is_a<ASR::DoLoop_t>(*f.owner)
                || ASR::is_a<ASR::WhileLoop_t>(*f.owner))) {
            continue;
        }
        // The next iteration starts the body again, and goes down to the
        // loop of the innermost frame, which assigns `s` first. A loop on
        // the way that runs no iteration leads back to where this started.
        if (header_reads(f.owner)) return false;
        for (size_t j = k; j < frames.size(); j++) {
            const Frame &g = frames[j];
            if (j > k) {
                // The `if` may take its other branch instead, which then
                // goes on after the `if`, as checked above.
                if (g.owner && ASR::is_a<ASR::If_t>(*g.owner)) {
                    auto *c = ASR::down_cast<ASR::If_t>(g.owner);
                    if (mentions(c->m_test, s)) return false;
                    ASR::stmt_t **other;
                    size_t n_other;
                    if (g.body == c->m_body) {
                        other = c->m_orelse;
                        n_other = c->n_orelse;
                    } else if (g.body == c->m_orelse) {
                        other = c->m_body;
                        n_other = c->n_body;
                    } else {
                        return false;
                    }
                    if (first_access(other, 0, n_other, s) == Access::Read) {
                        return false;
                    }
                }
                if (header_reads(g.owner)) return false;
            }
            a = first_access(g.body, 0, g.index, s);
            if (a == Access::Write) break;
            if (a == Access::Read) return false;
        }
        // Or the loop ends, with the value its last iteration left in `s`,
        // which what comes after the loop must not read either.
    }
    return true;
}

// The stop handling of gpu_offload_stop.h: a record of a stop in the status
// buffer, and the test after a call that may have stopped.
bool stop_record(const ASR::stmt_t &x, ASR::symbol_t **status) {
    ASR::symbol_t *s = gpu_stop_record_status(x);
    if (s == nullptr) return false;
    if (status) *status = s;
    return true;
}

struct KernelInfo {
    ASR::Function_t *fn = nullptr;
    std::set<ASR::symbol_t*> params;
    std::set<ASR::symbol_t*> workspaces;
    // The site and code of a stop, which hold zero on every lane that goes
    // on.
    std::set<ASR::symbol_t*> stop_scalars;
    // The private scalars whose value depends on the iteration.
    std::set<ASR::symbol_t*> per_iteration;
    ASR::symbol_t *lanes = nullptr;
    // The device's lanes can combine what each of them accumulated
    // (GpuDeviceCapabilities::lane_reductions).
    bool lane_reductions = false;
    // The widest kind of a value they combine
    // (GpuDeviceCapabilities::lane_reduction_max_kind).
    int lane_reduction_max_kind = 8;
    // The device's lanes share loops whose iterations are independent
    // (GpuDeviceCapabilities::lane_independent_loops).
    bool independent_loops = true;
    // The device's lanes share out the sum of a short, fixed length inside
    // a reduction loop rather than the loop
    // (GpuDeviceCapabilities::lane_inner_reductions_first).
    bool inner_reductions_first = false;
    // Whether a reduction loop that reads an array across its columns (the
    // loop variable only in a later subscript) is shared out as well
    // (give_lanes: only for a launch of few iterations).
    bool scattered_reductions = false;
    // A kernel that reads vectors of 16 bytes gets lanes for a launch of
    // any size (GpuKernelLaunch below), so its shared-out loops are not
    // kept as they were for a launch with one lane: the shared-out form
    // runs one lane too, and the second copy only made nvcc schedule the
    // first worse (a quantized matrix-vector product took 5% longer).
    bool shared_only = false;
    int lanes_count = 1;
    // The values each private integer scalar of the kernel is assigned,
    // BLOCKs included (single_iteration_test).
    std::map<ASR::symbol_t*, std::vector<ASR::expr_t*>> definitions;
    // The contributions of a combination of array reductions
    // (GpuReduceCombine), which takes the first lane's: only the first lane
    // needs their values, so a statement that assigns nothing else runs on
    // it alone, and they are not handed to the others.
    std::set<ASR::symbol_t*> first_lane_scalars;
    // The arrays of the iteration that live in the shared memory of the
    // block, whose threads are all lanes of the one iteration
    // (block_array_candidates): every lane sees the same of them.
    std::set<ASR::symbol_t*> block_arrays;
    // The lanes of the block, all of which run the iteration, while the
    // iteration is tried with arrays the block shares (zero otherwise).
    int block_lanes = 0;
    // The lanes every launch gives an iteration, once they are known (zero
    // when a launch decides): above a SIMD-group, the lanes combine their
    // values (GpuLaneReduce) through shared memory behind a barrier of the
    // block, which every lane has reached when it has the result.
    int fixed_lanes = 0;

    bool local(ASR::symbol_t *s) const {
        if (!ASR::is_a<ASR::Variable_t>(*s)) return false;
        for (SymbolTable *t = ASR::down_cast<ASR::Variable_t>(s)
                ->m_parent_symtab; t != nullptr; t = t->parent) {
            if (t == fn->m_symtab) return true;
        }
        return false;
    }

    // Memory every lane of an iteration sees the same of.
    bool shared(ASR::symbol_t *s) const {
        if (s == nullptr || !local(s)) return true;
        ASR::ttype_t *t = ASRUtils::symbol_type(s);
        if (ASRUtils::is_pointer(t) || workspaces.count(s)
                || block_arrays.count(s)) {
            return true;
        }
        if (params.count(s)) {
            return ASRUtils::is_array(t)
                || ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(t));
        }
        return false;
    }

    // A variable each lane holds its own copy of, in a register.
    bool private_scalar(ASR::symbol_t *s) const {
        if (s == nullptr || !local(s) || shared(s)) return false;
        ASR::ttype_t *t = ASRUtils::symbol_type(s);
        return !ASRUtils::is_array(t)
            && !ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(t));
    }
};

// Whether an expression depends on the iteration: reads the thread index
// or a scalar that does.
class ReadsIteration : public ASR::BaseWalkVisitor<ReadsIteration> {
public:
    const std::set<ASR::symbol_t*> &per_iteration;
    bool found = false;
    explicit ReadsIteration(const std::set<ASR::symbol_t*> &p)
        : per_iteration(p) {}
    void visit_GpuThreadIndex(const ASR::GpuThreadIndex_t &) { found = true; }
    void visit_GpuBlockIndex(const ASR::GpuBlockIndex_t &) { found = true; }
    void visit_Var(const ASR::Var_t &x) {
        if (per_iteration.count(ASRUtils::symbol_get_past_external(x.m_v))) {
            found = true;
        }
    }
};

bool reads_iteration(const KernelInfo &k, ASR::expr_t *e) {
    if (e == nullptr) return false;
    ReadsIteration r(k.per_iteration);
    r.visit_expr(*e);
    return r.found;
}

// The private scalars of the kernel whose value depends on the iteration,
// to a fixed point.
class IterationScalars : public ASR::BaseWalkVisitor<IterationScalars> {
public:
    KernelInfo &k;
    bool added = false;
    std::set<ASR::symbol_t*> blocks;
    explicit IterationScalars(KernelInfo &k_) : k(k_) {}
    void define(ASR::expr_t *target, bool depends) {
        if (!depends || !ASR::is_a<ASR::Var_t>(*target)) return;
        ASR::symbol_t *s = root_of(target);
        if (k.private_scalar(s) && k.per_iteration.insert(s).second) {
            added = true;
        }
    }
    void visit_Assignment(const ASR::Assignment_t &x) {
        define(x.m_target, reads_iteration(k, x.m_value));
        BaseWalkVisitor::visit_Assignment(x);
    }
    void visit_DoLoop(const ASR::DoLoop_t &x) {
        if (x.m_head.m_v) {
            define(x.m_head.m_v, reads_iteration(k, x.m_head.m_start)
                || reads_iteration(k, x.m_head.m_end)
                || reads_iteration(k, x.m_head.m_increment));
        }
        BaseWalkVisitor::visit_DoLoop(x);
    }
    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        for (size_t h = 0; h < x.n_head; h++) {
            if (x.m_head[h].m_v == nullptr) continue;
            define(x.m_head[h].m_v, reads_iteration(k, x.m_head[h].m_start)
                || reads_iteration(k, x.m_head[h].m_end)
                || reads_iteration(k, x.m_head[h].m_increment));
        }
        BaseWalkVisitor::visit_DoConcurrentLoop(x);
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!blocks.insert(b).second) return;
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(b);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
};

// Whether `e` gives every iteration a value of its own: the number the
// kernel gives the iteration (GpuBlockIndex * GpuBlockSize +
// GpuThreadIndex), or such a value plus or minus something the iteration
// does not change, or a scalar assigned once, one of those (the loop index,
// `i = __flat_idx + 1`).
bool iteration_number(const KernelInfo &k, ASR::expr_t *e, int depth = 0) {
    if (e == nullptr || depth > 16) return false;
    auto past_casts = [](ASR::expr_t *x) {
        while (ASR::is_a<ASR::Cast_t>(*x) && ASR::down_cast<ASR::Cast_t>(x)
                ->m_kind == ASR::cast_kindType::IntegerToInteger) {
            x = ASR::down_cast<ASR::Cast_t>(x)->m_arg;
        }
        return x;
    };
    e = past_casts(e);
    if (ASR::is_a<ASR::Var_t>(*e)) {
        auto d = k.definitions.find(ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(e)->m_v));
        return d != k.definitions.end() && d->second.size() == 1
            && iteration_number(k, d->second[0], depth + 1);
    }
    if (!ASR::is_a<ASR::IntegerBinOp_t>(*e)) return false;
    auto *op = ASR::down_cast<ASR::IntegerBinOp_t>(e);
    ASR::expr_t *l = past_casts(op->m_left), *r = past_casts(op->m_right);
    if (op->m_op == ASR::binopType::Add) {
        auto block_start = [&](ASR::expr_t *x) {
            if (!ASR::is_a<ASR::IntegerBinOp_t>(*x)) return false;
            auto *m = ASR::down_cast<ASR::IntegerBinOp_t>(x);
            ASR::expr_t *a = past_casts(m->m_left), *b = past_casts(m->m_right);
            return m->m_op == ASR::binopType::Mul
                && ((ASR::is_a<ASR::GpuBlockIndex_t>(*a)
                        && ASR::is_a<ASR::GpuBlockSize_t>(*b))
                    || (ASR::is_a<ASR::GpuBlockSize_t>(*a)
                        && ASR::is_a<ASR::GpuBlockIndex_t>(*b)));
        };
        if ((ASR::is_a<ASR::GpuThreadIndex_t>(*l) && block_start(r))
                || (ASR::is_a<ASR::GpuThreadIndex_t>(*r) && block_start(l))) {
            return true;
        }
        return (iteration_number(k, l, depth + 1) && !reads_iteration(k, r))
            || (iteration_number(k, r, depth + 1) && !reads_iteration(k, l));
    }
    if (op->m_op == ASR::binopType::Sub) {
        return iteration_number(k, l, depth + 1) && !reads_iteration(k, r);
    }
    return false;
}

// Whether the test of an `if` holds in one iteration at most: an iteration
// number, as iteration_number, equal to something the iteration does not
// change (`if (i == 1)`).
bool single_iteration_test(const KernelInfo &k, ASR::expr_t *test) {
    if (!ASR::is_a<ASR::IntegerCompare_t>(*test)) return false;
    auto *c = ASR::down_cast<ASR::IntegerCompare_t>(test);
    if (c->m_op != ASR::cmpopType::Eq) return false;
    return (iteration_number(k, c->m_left) && !reads_iteration(k, c->m_right))
        || (iteration_number(k, c->m_right)
            && !reads_iteration(k, c->m_left));
}

// Whether a statement holds a loop.
bool holds_loop(const ASR::stmt_t &s) {
    class Loops : public ASR::BaseWalkVisitor<Loops> {
    public:
        bool found = false;
        std::set<ASR::symbol_t*> blocks;
        void visit_DoLoop(const ASR::DoLoop_t &) { found = true; }
        void visit_WhileLoop(const ASR::WhileLoop_t &) { found = true; }
        void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &) {
            found = true;
        }
        void visit_BlockCall(const ASR::BlockCall_t &x) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
            if (!blocks.insert(b).second) return;
            ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(b);
            for (size_t i = 0; i < block->n_body; i++) {
                visit_stmt(*block->m_body[i]);
            }
        }
    } loops;
    loops.visit_stmt(s);
    return loops.found;
}

// The values the private integer scalars of a kernel are assigned.
class ScalarDefinitions : public ASR::BaseWalkVisitor<ScalarDefinitions> {
public:
    KernelInfo &k;
    std::set<ASR::symbol_t*> blocks;
    explicit ScalarDefinitions(KernelInfo &k_) : k(k_) {}
    void visit_Assignment(const ASR::Assignment_t &x) {
        if (ASR::is_a<ASR::Var_t>(*x.m_target)) {
            ASR::symbol_t *s = root_of(x.m_target);
            if (k.private_scalar(s)
                    && ASRUtils::is_integer(*ASRUtils::symbol_type(s))) {
                k.definitions[s].push_back(x.m_value);
            }
        }
        BaseWalkVisitor::visit_Assignment(x);
    }
    void visit_DoLoop(const ASR::DoLoop_t &x) {
        // A loop variable takes many values.
        if (x.m_head.m_v) {
            ASR::symbol_t *s = root_of(x.m_head.m_v);
            if (s != nullptr) {
                k.definitions[s].push_back(nullptr);
                k.definitions[s].push_back(nullptr);
            }
        }
        BaseWalkVisitor::visit_DoLoop(x);
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!blocks.insert(b).second) return;
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(b);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
};

// Whether a statement may assign an element of a kernel argument at a
// place every iteration assigns: a temporary declared outside the loop,
// which each iteration defines before it reads it. Each iteration should
// have its own; a launch shares one between all threads (see
// gpu_vector_lanes.cpp), and lanes that wait for each other would read
// what another iteration wrote there more often than threads that do not.
class SharedTemporaryWrites
        : public ASR::BaseWalkVisitor<SharedTemporaryWrites> {
public:
    const KernelInfo &k;
    bool found = false;
    std::set<ASR::symbol_t*> blocks;
    explicit SharedTemporaryWrites(const KernelInfo &k_) : k(k_) {}

    bool argument_array(ASR::symbol_t *r) const {
        return r != nullptr && k.params.count(r) && !k.workspaces.count(r)
            && !ASRUtils::is_pointer(ASRUtils::symbol_type(r));
    }
    bool iteration_subscript(ASR::expr_t *e) const {
        while (e) {
            if (ASR::is_a<ASR::ArrayItem_t>(*e)) {
                auto *item = ASR::down_cast<ASR::ArrayItem_t>(e);
                for (size_t i = 0; i < item->n_args; i++) {
                    if (reads_iteration(k, item->m_args[i].m_left)
                            || reads_iteration(k, item->m_args[i].m_right)
                            || reads_iteration(k, item->m_args[i].m_step)) {
                        return true;
                    }
                }
                e = item->m_v;
            } else if (ASR::is_a<ASR::ArraySection_t>(*e)) {
                auto *sec = ASR::down_cast<ASR::ArraySection_t>(e);
                for (size_t i = 0; i < sec->n_args; i++) {
                    if (reads_iteration(k, sec->m_args[i].m_left)
                            || reads_iteration(k, sec->m_args[i].m_right)
                            || reads_iteration(k, sec->m_args[i].m_step)) {
                        return true;
                    }
                }
                e = sec->m_v;
            } else if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
                e = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v;
            } else if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
                e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
            } else {
                return false;
            }
        }
        return false;
    }
    void written(ASR::expr_t *e) {
        if (argument_array(root_of(e)) && !iteration_subscript(e)) {
            found = true;
        }
    }
    void visit_Assignment(const ASR::Assignment_t &x) {
        if (!ASR::is_a<ASR::Var_t>(*x.m_target)
                || !k.private_scalar(root_of(x.m_target))) {
            written(x.m_target);
        }
        BaseWalkVisitor::visit_Assignment(x);
    }
    void visit_If(const ASR::If_t &x) {
        if (stop_record(x.base, nullptr)) return;
        // What one iteration alone assigns (`if (i == 1) a(1) = 0`) is no
        // temporary every iteration defines.
        if (single_iteration_test(k, x.m_test)) {
            visit_expr(*x.m_test);
            for (size_t i = 0; i < x.n_orelse; i++) {
                visit_stmt(*x.m_orelse[i]);
            }
            return;
        }
        BaseWalkVisitor::visit_If(x);
    }
    void call_args(ASR::symbol_t *name, ASR::call_arg_t *args,
            size_t n_args) {
        ASR::symbol_t *callee = ASRUtils::symbol_get_past_external(name);
        if (ASR::is_a<ASR::StructMethodDeclaration_t>(*callee)) {
            callee = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::StructMethodDeclaration_t>(callee)
                    ->m_proc);
        }
        ASR::Function_t *fn = ASR::is_a<ASR::Function_t>(*callee)
            ? ASR::down_cast<ASR::Function_t>(callee) : nullptr;
        for (size_t i = 0; i < n_args; i++) {
            if (args[i].m_value == nullptr) continue;
            if (fn && i < fn->n_args && ASR::is_a<ASR::Var_t>(*fn->m_args[i])) {
                ASR::symbol_t *dummy = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(fn->m_args[i])->m_v);
                if (ASR::is_a<ASR::Variable_t>(*dummy)
                        && ASR::down_cast<ASR::Variable_t>(dummy)->m_intent
                            == ASR::intentType::In) {
                    continue;
                }
            }
            written(args[i].m_value);
        }
    }
    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        call_args(x.m_name, x.m_args, x.n_args);
        BaseWalkVisitor::visit_SubroutineCall(x);
    }
    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        call_args(x.m_name, x.m_args, x.n_args);
        BaseWalkVisitor::visit_FunctionCall(x);
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!blocks.insert(b).second) return;
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(b);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
};

bool writes_shared_temporary(const KernelInfo &k, const ASR::stmt_t &s) {
    SharedTemporaryWrites w(k);
    w.visit_stmt(s);
    return w.found;
}

// What a statement does, as far as the lanes are concerned.
class Effects : public ASR::BaseWalkVisitor<Effects> {
public:
    const KernelInfo &k;
    // Assigns shared memory, calls a procedure that is not pure, or takes
    // elements of the device heap: something the lanes must not repeat.
    bool shared_effect = false;
    // Assigns an element of an array each lane has its own copy of.
    bool private_array = false;
    bool returns = false;
    // Associates a pointer.
    bool associates = false;
    // A statement this pass does not know.
    bool unknown = false;
    // The private scalars it may assign, but for the locals of the blocks
    // inside it: Fortran gives such a local no value when its block starts,
    // so each run of the block, on each lane, has its own, and none is left
    // after the block.
    std::set<ASR::symbol_t*> scalars;
    // The status buffers of the stops it records.
    std::set<ASR::symbol_t*> statuses;
    std::set<ASR::symbol_t*> blocks;
    // The scopes of those blocks.
    std::set<SymbolTable*> block_scopes;

    explicit Effects(const KernelInfo &k_) : k(k_) {}

    // Whether `s` is a local of a block inside the statements walked, with
    // no value kept from one run of the block to the next.
    bool block_local(ASR::symbol_t *s) const {
        if (!ASR::is_a<ASR::Variable_t>(*s)) return false;
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
        if (v->m_storage == ASR::storage_typeType::Save
                || v->m_symbolic_value || v->m_value) {
            return false;
        }
        return block_scopes.count(v->m_parent_symtab) > 0;
    }

    void assigns_scalar(ASR::symbol_t *s) {
        if (!block_local(s)) scalars.insert(s);
    }

    void target(ASR::expr_t *e) {
        ASR::symbol_t *r = root_of(e);
        if (r == nullptr) {
            unknown = true;
            return;
        }
        if (ASR::is_a<ASR::Var_t>(*e) && k.private_scalar(r)) {
            assigns_scalar(r);
        } else if (k.shared(r)) {
            shared_effect = true;
        } else {
            private_array = true;
        }
    }

    void visit_stmt(const ASR::stmt_t &x) {
        switch (x.type) {
            case ASR::stmtType::Assignment: {
                auto *a = ASR::down_cast<ASR::Assignment_t>(&x);
                if (a->m_overloaded) {
                    unknown = true;
                    return;
                }
                target(a->m_target);
                visit_expr(*a->m_value);
                if (!ASR::is_a<ASR::Var_t>(*a->m_target)) {
                    // The subscripts.
                    visit_expr(*a->m_target);
                }
                return;
            }
            case ASR::stmtType::If: {
                ASR::symbol_t *status = nullptr;
                if (stop_record(x, &status)) {
                    // Every thread that stops may record its stop at once
                    // (gpu_offload_stop.h): no lane has to do it alone.
                    statuses.insert(status);
                    return;
                }
                BaseWalkVisitor::visit_stmt(x);
                return;
            }
            case ASR::stmtType::DoLoop: {
                auto *d = ASR::down_cast<ASR::DoLoop_t>(&x);
                if (d->n_orelse > 0 || d->m_head.m_v == nullptr) {
                    unknown = true;
                    return;
                }
                target(d->m_head.m_v);
                BaseWalkVisitor::visit_stmt(x);
                return;
            }
            case ASR::stmtType::DoConcurrentLoop: {
                auto *d = ASR::down_cast<ASR::DoConcurrentLoop_t>(&x);
                for (size_t h = 0; h < d->n_head; h++) {
                    if (d->m_head[h].m_v == nullptr) {
                        unknown = true;
                        return;
                    }
                    target(d->m_head[h].m_v);
                }
                BaseWalkVisitor::visit_stmt(x);
                return;
            }
            case ASR::stmtType::Return:
                returns = true;
                return;
            case ASR::stmtType::Allocate:
            case ASR::stmtType::ReAlloc:
            case ASR::stmtType::ExplicitDeallocate:
            case ASR::stmtType::ImplicitDeallocate:
                // device_no_op
                return;
            case ASR::stmtType::Associate:
                associates = true;
                BaseWalkVisitor::visit_stmt(x);
                return;
            case ASR::stmtType::SubroutineCall:
                shared_effect = true;
                BaseWalkVisitor::visit_stmt(x);
                return;
            case ASR::stmtType::WhileLoop:
            case ASR::stmtType::Exit:
            case ASR::stmtType::Cycle:
            case ASR::stmtType::BlockCall:
                BaseWalkVisitor::visit_stmt(x);
                return;
            case ASR::stmtType::GpuReduceCombine:
                // Every lane reaches it, and it takes the values of the
                // first lane of each iteration (asr_to_gpu_c.h): it runs on
                // all of them.
                BaseWalkVisitor::visit_stmt(x);
                return;
            default:
                unknown = true;
                return;
        }
    }

    void visit_WhileLoop(const ASR::WhileLoop_t &x) {
        if (x.n_orelse > 0) unknown = true;
        BaseWalkVisitor::visit_WhileLoop(x);
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!blocks.insert(b).second) return;
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(b);
        block_scopes.insert(block->m_symtab);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }

    // What a procedure may assign through its arguments: a private scalar
    // or a private array handed to a dummy that is not `intent(in)`.
    void call_args(ASR::symbol_t *name, ASR::call_arg_t *args,
            size_t n_args) {
        ASR::symbol_t *callee = ASRUtils::symbol_get_past_external(name);
        if (ASR::is_a<ASR::StructMethodDeclaration_t>(*callee)) {
            callee = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::StructMethodDeclaration_t>(callee)
                    ->m_proc);
        }
        ASR::Function_t *fn = ASR::is_a<ASR::Function_t>(*callee)
            ? ASR::down_cast<ASR::Function_t>(callee) : nullptr;
        for (size_t i = 0; i < n_args; i++) {
            if (args[i].m_value == nullptr) continue;
            if (fn && i < fn->n_args && ASR::is_a<ASR::Var_t>(*fn->m_args[i])) {
                ASR::symbol_t *dummy = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(fn->m_args[i])->m_v);
                if (ASR::is_a<ASR::Variable_t>(*dummy)
                        && ASR::down_cast<ASR::Variable_t>(dummy)->m_intent
                            == ASR::intentType::In) {
                    continue;
                }
            }
            ASR::symbol_t *r = root_of(args[i].m_value);
            if (r == nullptr) continue;
            if (ASR::is_a<ASR::Var_t>(*args[i].m_value)
                    && k.private_scalar(r)) {
                assigns_scalar(r);
            } else if (!k.shared(r) && !k.private_scalar(r)) {
                private_array = true;
            }
        }
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        if (!pure_callee(x.m_name)) shared_effect = true;
        call_args(x.m_name, x.m_args, x.n_args);
        BaseWalkVisitor::visit_FunctionCall(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        call_args(x.m_name, x.m_args, x.n_args);
        BaseWalkVisitor::visit_SubroutineCall(x);
    }

    void visit_GpuAtomicFetchAdd(const ASR::GpuAtomicFetchAdd_t &x) {
        shared_effect = true;
        BaseWalkVisitor::visit_GpuAtomicFetchAdd(x);
    }
};

// The memory every lane sees the same of (KernelInfo::shared) that the
// statements walked name, and that they may assign: an assignment's target,
// what a procedure is handed for a dummy that is not `intent(in)`, what an
// atomic adds to.
class SharedAccess : public ASR::BaseWalkVisitor<SharedAccess> {
public:
    const KernelInfo &k;
    std::set<ASR::symbol_t*> reads;
    std::set<ASR::symbol_t*> writes;
    std::set<ASR::symbol_t*> blocks;
    explicit SharedAccess(const KernelInfo &k_) : k(k_) {}

    bool shared_variable(ASR::symbol_t *s) const {
        return s != nullptr && ASR::is_a<ASR::Variable_t>(*s) && k.shared(s);
    }
    void written(ASR::expr_t *e) {
        ASR::symbol_t *r = root_of(e);
        if (shared_variable(r)) writes.insert(r);
    }
    void visit_Var(const ASR::Var_t &x) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_v);
        if (shared_variable(s)) reads.insert(s);
    }
    // An element assigned is not read: only its subscripts are.
    void visit_Assignment(const ASR::Assignment_t &x) {
        written(x.m_target);
        ASR::expr_t *e = x.m_target;
        while (e && !ASR::is_a<ASR::Var_t>(*e)) {
            if (ASR::is_a<ASR::ArrayItem_t>(*e)) {
                auto *item = ASR::down_cast<ASR::ArrayItem_t>(e);
                for (size_t i = 0; i < item->n_args; i++) {
                    if (item->m_args[i].m_left) {
                        visit_expr(*item->m_args[i].m_left);
                    }
                    if (item->m_args[i].m_right) {
                        visit_expr(*item->m_args[i].m_right);
                    }
                    if (item->m_args[i].m_step) {
                        visit_expr(*item->m_args[i].m_step);
                    }
                }
                e = item->m_v;
            } else if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
                e = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v;
            } else {
                visit_expr(*e);
                e = nullptr;
            }
        }
        visit_expr(*x.m_value);
    }
    void call_args(ASR::symbol_t *name, ASR::call_arg_t *args,
            size_t n_args) {
        ASR::symbol_t *callee = ASRUtils::symbol_get_past_external(name);
        if (ASR::is_a<ASR::StructMethodDeclaration_t>(*callee)) {
            callee = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::StructMethodDeclaration_t>(callee)
                    ->m_proc);
        }
        ASR::Function_t *fn = ASR::is_a<ASR::Function_t>(*callee)
            ? ASR::down_cast<ASR::Function_t>(callee) : nullptr;
        for (size_t i = 0; i < n_args; i++) {
            if (args[i].m_value == nullptr) continue;
            if (fn && i < fn->n_args && ASR::is_a<ASR::Var_t>(*fn->m_args[i])) {
                ASR::symbol_t *dummy = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(fn->m_args[i])->m_v);
                if (ASR::is_a<ASR::Variable_t>(*dummy)
                        && ASR::down_cast<ASR::Variable_t>(dummy)->m_intent
                            == ASR::intentType::In) {
                    continue;
                }
            }
            written(args[i].m_value);
        }
    }
    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        call_args(x.m_name, x.m_args, x.n_args);
        BaseWalkVisitor::visit_SubroutineCall(x);
    }
    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        call_args(x.m_name, x.m_args, x.n_args);
        BaseWalkVisitor::visit_FunctionCall(x);
    }
    void visit_GpuAtomicFetchAdd(const ASR::GpuAtomicFetchAdd_t &x) {
        written(x.m_target);
        BaseWalkVisitor::visit_GpuAtomicFetchAdd(x);
    }
    // The shape of an array is not its elements.
    void visit_ArraySize(const ASR::ArraySize_t &x) {
        if (x.m_dim) visit_expr(*x.m_dim);
    }
    void visit_ArrayBound(const ASR::ArrayBound_t &x) {
        if (x.m_dim) visit_expr(*x.m_dim);
    }
    // What device code lays out beforehand (device_no_op).
    void visit_Allocate(const ASR::Allocate_t &) {}
    void visit_ReAlloc(const ASR::ReAlloc_t &) {}
    void visit_ExplicitDeallocate(const ASR::ExplicitDeallocate_t &) {}
    void visit_ImplicitDeallocate(const ASR::ImplicitDeallocate_t &) {}
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!blocks.insert(b).second) return;
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(b);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
};

bool intersects(const std::set<ASR::symbol_t*> &a,
        const std::set<ASR::symbol_t*> &b) {
    for (ASR::symbol_t *s : a) {
        if (b.count(s)) return true;
    }
    return false;
}

// Whether the statements walked leave the loop they belong to: an unnamed
// `exit` or `cycle` outside every loop inside them, or a named one whose
// target is no loop inside them (the loop itself, or a loop around it).
class LeavesLoop : public ASR::BaseWalkVisitor<LeavesLoop> {
public:
    // The names of the loops being walked inside, outermost first; "" for
    // an unnamed one.
    std::vector<std::string> nested;
    bool found = false;
    void leave(const char *target) {
        if (target == nullptr) {
            if (nested.empty()) found = true;
        } else if (std::find(nested.begin(), nested.end(),
                std::string(target)) == nested.end()) {
            found = true;
        }
    }
    void visit_Exit(const ASR::Exit_t &x) { leave(x.m_stmt_name); }
    void visit_Cycle(const ASR::Cycle_t &x) { leave(x.m_stmt_name); }
    void visit_DoLoop(const ASR::DoLoop_t &x) {
        nested.push_back(x.m_name ? x.m_name : "");
        BaseWalkVisitor::visit_DoLoop(x);
        nested.pop_back();
    }
    void visit_WhileLoop(const ASR::WhileLoop_t &x) {
        nested.push_back(x.m_name ? x.m_name : "");
        BaseWalkVisitor::visit_WhileLoop(x);
        nested.pop_back();
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(
            ASRUtils::symbol_get_past_external(x.m_m));
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
};

// Every element designator, and every other naming of a variable, in the
// body of a loop: what the independence of its iterations is checked on.
class Designators : public ASR::BaseWalkVisitor<Designators> {
public:
    // Array elements: the whole ArrayItem.
    std::vector<ASR::expr_t*> items;
    // Anything else naming a variable or a component, whole.
    std::vector<ASR::expr_t*> wholes;
    std::set<ASR::symbol_t*> blocks;

    void visit_ArrayItem(const ASR::ArrayItem_t &x) {
        items.push_back(const_cast<ASR::expr_t*>(&x.base));
        // The subscripts, and what the base itself evaluates.
        for (size_t i = 0; i < x.n_args; i++) {
            if (x.m_args[i].m_left) visit_expr(*x.m_args[i].m_left);
            if (x.m_args[i].m_right) visit_expr(*x.m_args[i].m_right);
            if (x.m_args[i].m_step) visit_expr(*x.m_args[i].m_step);
        }
        base(x.m_v);
    }
    // The subscripts inside the base of an element, which is not itself a
    // naming of the whole.
    void base(ASR::expr_t *e) {
        while (true) {
            if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
                e = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v;
            } else if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
                e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
            } else if (ASR::is_a<ASR::ArrayItem_t>(*e)) {
                visit_ArrayItem(*ASR::down_cast<ASR::ArrayItem_t>(e));
                return;
            } else {
                return;
            }
        }
    }
    void visit_Var(const ASR::Var_t &x) {
        wholes.push_back(const_cast<ASR::expr_t*>(&x.base));
    }
    void visit_StructInstanceMember(const ASR::StructInstanceMember_t &x) {
        wholes.push_back(const_cast<ASR::expr_t*>(&x.base));
        base(x.m_v);
    }
    // The shape of an array is not its elements.
    void visit_ArraySize(const ASR::ArraySize_t &x) {
        if (x.m_dim) visit_expr(*x.m_dim);
    }
    void visit_ArrayBound(const ASR::ArrayBound_t &x) {
        if (x.m_dim) visit_expr(*x.m_dim);
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!blocks.insert(b).second) return;
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(b);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
};

// The assignments of a loop body, at any depth, outside stop records.
class Assignments : public ASR::BaseWalkVisitor<Assignments> {
public:
    std::vector<ASR::Assignment_t*> found;
    std::set<ASR::symbol_t*> blocks;
    void visit_Assignment(const ASR::Assignment_t &x) {
        found.push_back(const_cast<ASR::Assignment_t*>(&x));
    }
    void visit_If(const ASR::If_t &x) {
        if (stop_record(x.base, nullptr)) return;
        BaseWalkVisitor::visit_If(x);
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!blocks.insert(b).second) return;
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(b);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
};

// Whether a subscript is the loop variable, plus or minus something the
// loop does not change: different iterations give different values.
bool injective(ASR::expr_t *e, ASR::symbol_t *v,
        const std::set<ASR::symbol_t*> &changed) {
    if (e == nullptr) return false;
    if (ASR::is_a<ASR::Var_t>(*e)) return root_of(e) == v;
    if (ASR::is_a<ASR::Cast_t>(*e)) {
        auto *c = ASR::down_cast<ASR::Cast_t>(e);
        return c->m_kind == ASR::cast_kindType::IntegerToInteger
            && injective(c->m_arg, v, changed);
    }
    if (!ASR::is_a<ASR::IntegerBinOp_t>(*e)) return false;
    auto *op = ASR::down_cast<ASR::IntegerBinOp_t>(e);
    auto invariant = [&](ASR::expr_t *x) {
        for (ASR::symbol_t *s : changed) {
            if (mentions(x, s)) return false;
        }
        return !mentions(x, v);
    };
    if (op->m_op == ASR::binopType::Add) {
        return (injective(op->m_left, v, changed) && invariant(op->m_right))
            || (injective(op->m_right, v, changed) && invariant(op->m_left));
    }
    if (op->m_op == ASR::binopType::Sub) {
        return injective(op->m_left, v, changed) && invariant(op->m_right);
    }
    return false;
}

// Whether `e` multiplies `v` by a stride that is not a constant: then
// consecutive values of `v` are not next to each other in memory.
bool strided_by(ASR::expr_t *e, ASR::symbol_t *v) {
    class Muls : public ASR::BaseWalkVisitor<Muls> {
    public:
        ASR::symbol_t *v;
        bool found = false;
        static bool constant_one(ASR::expr_t *e) {
            int64_t c = 0;
            return ASRUtils::extract_value(ASRUtils::expr_value(e), c)
                && (c == 1 || c == -1);
        }
        void visit_IntegerBinOp(const ASR::IntegerBinOp_t &x) {
            if (x.m_op == ASR::binopType::Mul) {
                bool l = mentions(x.m_left, v);
                bool r = mentions(x.m_right, v);
                if ((l && !r && !constant_one(x.m_right)
                        && !ASRUtils::expr_value(x.m_right))
                        || (r && !l && !constant_one(x.m_left)
                        && !ASRUtils::expr_value(x.m_left))) {
                    found = true;
                }
            }
            BaseWalkVisitor::visit_IntegerBinOp(x);
        }
    } muls;
    muls.v = v;
    muls.visit_expr(*e);
    return muls.found;
}

// A subscript as something plus a constant.
// Whether evaluating `e` reads an element of an array.
bool reads_array_element(ASR::expr_t *e) {
    class Finder : public ASR::BaseWalkVisitor<Finder> {
    public:
        bool found = false;
        void visit_ArrayItem(const ASR::ArrayItem_t &/*x*/) {
            found = true;
        }
    };
    if (e == nullptr) return false;
    Finder f;
    f.visit_expr(*e);
    return f.found;
}

void offset_form(ASR::expr_t *e, ASR::expr_t *&base, int64_t &offset) {
    base = e;
    offset = 0;
    while (base) {
        if (ASR::is_a<ASR::IntegerConstant_t>(*base)) {
            offset += ASR::down_cast<ASR::IntegerConstant_t>(base)->m_n;
            base = nullptr;
        } else if (ASR::is_a<ASR::Cast_t>(*base) && ASR::down_cast<ASR::Cast_t>(
                base)->m_kind == ASR::cast_kindType::IntegerToInteger) {
            base = ASR::down_cast<ASR::Cast_t>(base)->m_arg;
        } else if (ASR::is_a<ASR::IntegerBinOp_t>(*base)) {
            auto *op = ASR::down_cast<ASR::IntegerBinOp_t>(base);
            int64_t c = 0;
            if (op->m_op == ASR::binopType::Add
                    && ASRUtils::extract_value(op->m_right, c)) {
                offset += c;
                base = op->m_left;
            } else if (op->m_op == ASR::binopType::Add
                    && ASRUtils::extract_value(op->m_left, c)) {
                offset += c;
                base = op->m_right;
            } else if (op->m_op == ASR::binopType::Sub
                    && ASRUtils::extract_value(op->m_right, c)) {
                offset -= c;
                base = op->m_left;
            } else {
                return;
            }
        } else {
            return;
        }
    }
}

// Whether two elements of one array are different elements in every
// iteration of a loop: at some subscript both are the same value that the
// loop does not change, plus different constants (`a(i, l)` and
// `a(k, l - 1)`).
bool disjoint(ASR::expr_t *a, ASR::expr_t *b,
        const std::set<ASR::symbol_t*> &changed) {
    if (!ASR::is_a<ASR::ArrayItem_t>(*a) || !ASR::is_a<ASR::ArrayItem_t>(*b)) {
        return false;
    }
    auto *x = ASR::down_cast<ASR::ArrayItem_t>(a);
    auto *y = ASR::down_cast<ASR::ArrayItem_t>(b);
    if (x->n_args != y->n_args || !same_expr(x->m_v, y->m_v)) return false;
    for (size_t i = 0; i < x->n_args; i++) {
        ASR::expr_t *p = x->m_args[i].m_right, *q = y->m_args[i].m_right;
        if (p == nullptr || q == nullptr || x->m_args[i].m_left
                || y->m_args[i].m_left) {
            continue;
        }
        bool invariant = true;
        for (ASR::symbol_t *s : changed) {
            if (mentions(p, s) || mentions(q, s)) invariant = false;
        }
        if (!invariant) continue;
        ASR::expr_t *pb, *qb;
        int64_t po, qo;
        offset_form(p, pb, po);
        offset_form(q, qb, qo);
        if (po != qo && ((pb == nullptr && qb == nullptr)
                || (pb && qb && same_expr(pb, qb)))) {
            return true;
        }
    }
    return false;
}

enum class Role { Replicate, Recurse, Distribute, Single };

// Device code has no heap of its own to allocate from: the storage of an
// allocatable local is laid out beforehand, and these statements do
// nothing there.
bool device_no_op(const ASR::stmt_t &s) {
    switch (s.type) {
        case ASR::stmtType::Allocate:
        case ASR::stmtType::ReAlloc:
        case ASR::stmtType::ExplicitDeallocate:
        case ASR::stmtType::ImplicitDeallocate:
            return true;
        default:
            return false;
    }
}

// The number of iterations of a loop the lanes share out, `end - start + 1`
// with a constant start, and an end that is a constant or a scalar
// parameter of the kernel (whose value at a launch is the argument).
struct TripBound {
    int64_t start;
    // The end when `param` is null.
    int64_t end;
    ASR::symbol_t *param;
};

class LaneRewriter {
public:
    LaneRewriter(Allocator &al_, KernelInfo &k_) : al(al_), k(k_) {}

    bool failed = false;
    bool distributed = false;
    // A loop of the kernel's own statements (not one inside a loop the
    // lanes share out) was left alone as a reduction only for reading
    // across the columns of an array (KernelInfo::scattered_reductions).
    bool scattered_declined = false;
    // The lanes of an iteration the reductions it shares out can all use:
    // the fewest iterations of one of the loops in which only the inner
    // sum is shared out (inner_reductions_first), rounded down to a power
    // of two. Zero when there is no such loop.
    int64_t lane_limit = 0;

    // Whether the lanes pay for themselves however many iterations a launch
    // has, as decided for the statements of `body`: every loop they share
    // out reads and assigns the arrays it names along their first
    // subscript, one after another, and at least one along a column that
    // belongs to the iteration (a later subscript depends on it), and is
    // not a loop of a few iterations known at compile time; and nothing
    // outside those loops that every lane (or the first) runs walks such a
    // column of a kernel argument itself, in a loop or by handing it to a
    // procedure. With one lane per iteration, the threads of a SIMD-group
    // would read 32 columns at once, each a few elements at a time;
    // lanes read one column together, so the more iterations the launch
    // has, the more it gains. Work that every lane repeats on a column
    // would cost the lanes as much as they save.
    bool pays_at_any_size(ASR::stmt_t **body, size_t n_body) {
        bool column = false;
        collect_aliases(body, n_body);
        return pays(body, n_body, column) && column;
    }

    // The numbers of iterations of the loops the lanes share out, when every
    // one is known at a launch: a constant, or a scalar parameter of the
    // kernel (none, when one loop is not). A whole block of lanes per iteration (the lanes of a column kernel,
    // or of a launch of few iterations) leaves most of its threads idle
    // when such a loop is short, and pays for the barriers its threads wait
    // at: a sum of 128 elements takes 29 microseconds on 256 lanes and 5 on
    // a warp.
    void loop_bounds(std::vector<TripBound> &bounds) {
        for (auto &r : roles) {
            if (r.second != Role::Distribute
                    || !ASR::is_a<ASR::DoLoop_t>(*r.first)
                    || collapse.count(r.first)) {
                if (r.second == Role::Distribute) {
                    bounds.clear();
                    return;
                }
                continue;
            }
            auto *d = ASR::down_cast<ASR::DoLoop_t>(r.first);
            int64_t start = 0, end = 0, step = 1;
            if (d->m_head.m_increment && (!ASRUtils::extract_value(
                    ASRUtils::expr_value(d->m_head.m_increment), step)
                    || step != 1)) {
                bounds.clear();
                return;
            }
            if (!ASRUtils::extract_value(ASRUtils::expr_value(
                    d->m_head.m_start), start)) {
                bounds.clear();
                return;
            }
            if (ASRUtils::extract_value(ASRUtils::expr_value(
                    d->m_head.m_end), end)) {
                bounds.push_back({start, end, nullptr});
            } else if (ASR::is_a<ASR::Var_t>(*d->m_head.m_end)
                    && k.params.count(root_of(d->m_head.m_end))
                    && ASRUtils::is_integer(*ASRUtils::expr_type(
                        d->m_head.m_end))) {
                bounds.push_back({start, 0, root_of(d->m_head.m_end)});
            } else {
                bounds.clear();
                return;
            }
        }
    }

    // Whether a loop the lanes share out runs each of its iterations on
    // several lanes (plan_parts), which needs all the lanes of the block.
    bool has_parts() const { return !parted.empty(); }

    // A loop the lanes share out, one iteration per lane in turn
    // (distribute): its start and step as they were, and its number of
    // iterations, known at compile time.
    struct LaneLoop {
        ASR::expr_t *start;
        ASR::expr_t *step;
        int64_t trips;
    };
    std::map<ASR::stmt_t*, LaneLoop> lane_loops;
    // The counted loops that read the elements a lane keeps in registers
    // (hold): the variable first, then the element into the register.
    std::set<ASR::stmt_t*> held_loops;

    // The arrays of the iteration that the block shares
    // (KernelInfo::block_arrays) which a loop the lanes share out assigns
    // elements of: the lanes assign them together.
    std::set<ASR::symbol_t*> distributed_block_arrays() {
        std::set<ASR::symbol_t*> found;
        for (auto &r : roles) {
            if (r.second != Role::Distribute) continue;
            Assignments assigns;
            assigns.visit_stmt(*r.first);
            for (ASR::Assignment_t *a : assigns.found) {
                ASR::symbol_t *root = root_of(a->m_target);
                if (k.block_arrays.count(root)) found.insert(root);
            }
        }
        return found;
    }

    // Puts back the `do concurrent` loops decide() tried as loops one
    // inside the other, for a kernel that keeps one lane per iteration.
    void restore() {
        for (auto &s : serialized) *s.first = s.second;
        serialized.clear();
    }

    // Decides what every statement of a list, and of the lists inside it,
    // does. True when it distributes a loop.
    bool decide(ASR::stmt_t **&body, size_t &n_body, ASR::stmt_t *owner,
            std::vector<Frame> &frames) {
        bool any = false;
        for (size_t i = 0; i < n_body; i++) {
            frames.push_back({body, n_body, i, owner});
            // A `do concurrent` is tried as the loops over its indices one
            // inside the other, which device code runs it as anyway, and
            // kept as such where the lanes share them out.
            ASR::stmt_t *concurrent = body[i];
            ASR::stmt_t *serial = serial_loops(body[i]);
            if (serial) {
                body[i] = serial;
                concurrent_loops.insert(serial);
            }
            Role r = role(body[i], frames);
            if (serial && r != Role::Distribute && r != Role::Recurse) {
                body[i] = concurrent;
                r = role(body[i], frames);
            } else if (serial) {
                serialized.push_back({&body[i], concurrent});
            }
            frames.pop_back();
            if (r == Role::Distribute && !reductions.count(body[i])
                    && !either.count(body[i])) {
                fuse_following(body, n_body, i, owner, frames);
                frames.push_back({body, n_body, i, owner});
                if (collapsible(ASR::down_cast<ASR::DoLoop_t>(body[i]))) {
                    collapse.insert(body[i]);
                } else {
                    plan_parts(ASR::down_cast<ASR::DoLoop_t>(body[i]), frames);
                }
                frames.pop_back();
                if (!collapse.count(body[i]) && !parted.count(body[i])) {
                    fold_reduction(body, n_body, i, owner, frames);
                }
            }
            roles[body[i]] = r;
            if (r == Role::Distribute || r == Role::Recurse) any = true;
        }
        // `distributed` is set where a loop is distributed (role): a
        // statement is recursed into only when it holds one, so a kernel
        // with nothing distributed keeps one lane per iteration.
        return any;
    }

    // Rewrites a list as decided.
    //
    // The lanes run the statements between two barriers each at its own
    // pace, so a lane may assign shared memory while another still reads
    // what was there before: the statements every lane runs read it where
    // they are, and a statement on the first lane or a distributed loop
    // after them may assign it. Such a statement or loop waits for the
    // lanes first. `unsettled` is the shared memory the lanes may still be
    // reading, named since their last barrier.
    void apply(ASR::stmt_t **&body, size_t &n_body) {
        Vec<ASR::stmt_t*> out;
        out.reserve(al, n_body + 4);
        std::vector<ASR::stmt_t*> single;
        for (size_t i = 0; i <= n_body; i++) {
            Role r = i < n_body ? roles.at(body[i]) : Role::Replicate;
            // A statement that does nothing on the device does not end the
            // statements the first lane runs together.
            if (i < n_body && (r == Role::Single
                    || (!single.empty() && device_no_op(*body[i])))) {
                single.push_back(body[i]);
                continue;
            }
            if (!single.empty()) {
                SharedAccess access(k);
                for (ASR::stmt_t *s : single) access.visit_stmt(*s);
                settle_before(access, single[0]->base.loc, out);
                emit_single(single, out);
                unsettled.clear();
                single.clear();
            }
            if (i == n_body) break;
            SharedAccess access(k);
            access.visit_stmt(*body[i]);
            if (r == Role::Distribute) {
                settle_before(access, body[i]->base.loc, out);
                auto copy = either.find(body[i]);
                if (copy != either.end()) {
                    // The loop shared out ends with a barrier, and the copy
                    // that shares its sums instead is written as a list of
                    // its own, which leaves `unsettled` as it ends.
                    emit_either(ASR::down_cast<ASR::DoLoop_t>(body[i]),
                        copy->second, out);
                    continue;
                }
                bool reduction = reductions.count(body[i]) > 0
                    && !folded.count(body[i]);
                bool deferred = deferred_inner.count(body[i]) > 0;
                distribute(ASR::down_cast<ASR::DoLoop_t>(body[i]), out);
                // A loop that only accumulates ends without a barrier, but
                // the lanes have read what they accumulate once they
                // combine it, unless the combination is deferred.
                if (deferred) {
                    unsettled.insert(access.reads.begin(),
                        access.reads.end());
                } else if (!reduction) {
                    unsettled.clear();
                }
                continue;
            }
            auto deferral = deferrals.find(body[i]);
            if (r == Role::Recurse && deferral != deferrals.end()) {
                settle_before(access, body[i]->base.loc, out);
                emit_deferred(body[i], deferral->second, out);
                unsettled.insert(access.reads.begin(), access.reads.end());
                continue;
            }
            if (r == Role::Recurse) {
                nested_apply(body[i]);
            } else {
                unsettled.insert(access.reads.begin(), access.reads.end());
            }
            out.push_back(al, body[i]);
        }
        body = out.p;
        n_body = out.n;
    }

private:
    Allocator &al;
    KernelInfo &k;
    std::map<ASR::stmt_t*, Role> roles;
    // The distributed loops whose iterations, together with those of the
    // one loop inside them, are shared out as one.
    std::set<ASR::stmt_t*> collapse;
    // A private scalar a loop accumulates into, and how.
    struct Accumulator {
        ASR::symbol_t *variable;
        ASR::reduction_opType op;
    };
    // The distributed loops that accumulate into private scalars, which
    // the lanes combine after the loop.
    std::map<ASR::stmt_t*, std::vector<Accumulator>> reductions;
    // A private integer that a loop adds the same constant to in every
    // iteration, and reads otherwise: the index into the second array of
    // an array statement whose sections start at different subscripts
    // (array_op's `j = j + 1`).
    struct Induction {
        ASR::symbol_t *variable;
        // What one iteration adds to it.
        int64_t step;
        // Whether what it holds after the loop is read.
        bool live;
    };
    // The inductions of the reduction loops.
    std::map<ASR::stmt_t*, std::vector<Induction>> inductions;
    // The reduction loops whose variable is read after them.
    std::set<ASR::stmt_t*> live_variables;
    // Where decide() put the loops of a `do concurrent` in its place, and
    // the `do concurrent`.
    std::vector<std::pair<ASR::stmt_t**, ASR::stmt_t*>> serialized;
    // The loops decide() made of a `do concurrent` of the iteration.
    std::set<ASR::stmt_t*> concurrent_loops;
    // The `do concurrent` loops of the iteration that have a sum inside
    // them, with a copy in which the lanes share the sum instead.
    std::map<ASR::stmt_t*, ASR::stmt_t*> either;

    // The lanes of the iteration share out a `do concurrent` of it when it
    // has at least a warp of iterations, and the sum of the plain loop
    // inside it otherwise (a long sum split in a few parts). Which one
    // is only known when the kernel runs, so the loop is kept twice.
    bool share_either(ASR::stmt_t *s, std::vector<Frame> &frames) {
        auto *l = ASR::down_cast<ASR::DoLoop_t>(s);
        bool copyable = true;
        SymbolTable *scope = block_scope(l->m_body, l->n_body, copyable);
        if (!copyable) return false;
        ASR::stmt_t *copy = nullptr;
        if (scope) {
            GpuDuplicator dup(al, scope);
            copy = dup.duplicate_stmt(s);
            if (!dup.ok) copy = nullptr;
        } else {
            ASRUtils::ExprStmtDuplicator dup(al);
            dup.success = true;
            copy = dup.duplicate_stmt(s);
            if (!dup.success) copy = nullptr;
        }
        if (!copy) return false;
        bool was_failed = failed;
        size_t n_serialized = serialized.size();
        int64_t was_limit = lane_limit;
        bool ok = nested_decide(copy, frames) && !failed;
        lane_limit = was_limit;
        if (!ok) {
            failed = was_failed;
            for (size_t i = serialized.size(); i-- > n_serialized;) {
                *serialized[i].first = serialized[i].second;
            }
            serialized.resize(n_serialized);
            return false;
        }
        either[s] = copy;
        return true;
    }

    void emit_either(ASR::DoLoop_t *d, ASR::stmt_t *copy,
            Vec<ASR::stmt_t*> &out) {
        const Location &loc = d->base.base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        ASR::ttype_t *type = ASRUtils::expr_type(d->m_head.m_v);
        ASR::expr_t *n = trip_count(b, d, type, out);
        Vec<ASR::stmt_t*> outer;
        outer.reserve(al, 4);
        distribute(d, outer);
        nested_apply(copy);
        std::vector<ASR::stmt_t*> shared_out(outer.p, outer.p + outer.n);
        out.push_back(al, b.If(b.GtE(n, b.i_t(32, type)), shared_out,
            {copy}));
    }

    // The loops over the indices of a nested `do concurrent`, the first
    // index outermost, with its body inside: they run its iterations in an
    // order the construct allows, as device code does (asr_to_gpu_c.h,
    // emit_concurrent_loops). Its `local` variables are assigned before
    // they are read in each iteration, and a `reduce` variable accumulates
    // by the statements of the body, so they need nothing else. Nullptr for
    // any other statement.
    ASR::stmt_t* serial_loops(ASR::stmt_t *s) {
        if (!ASR::is_a<ASR::DoConcurrentLoop_t>(*s)) return nullptr;
        auto *d = ASR::down_cast<ASR::DoConcurrentLoop_t>(s);
        if (d->n_head == 0) return nullptr;
        for (size_t h = 0; h < d->n_head; h++) {
            if (d->m_head[h].m_v == nullptr || d->m_head[h].m_start == nullptr
                    || d->m_head[h].m_end == nullptr) {
                return nullptr;
            }
        }
        ASR::stmt_t **inner = d->m_body;
        size_t n_inner = d->n_body;
        ASR::stmt_t *loop = nullptr;
        for (size_t h = d->n_head; h-- > 0;) {
            loop = ASRUtils::STMT(ASRUtils::make_DoLoop_t_util(al,
                s->base.loc, nullptr, d->m_head[h], inner, n_inner, nullptr,
                0));
            Vec<ASR::stmt_t*> one;
            one.reserve(al, 1);
            one.push_back(al, loop);
            inner = one.p;
            n_inner = one.n;
        }
        return loop;
    }

    // A bound of the loop at body[at]: the value a scalar it names was
    // last given in the list before, when that is all it is.
    ASR::expr_t* resolve(ASR::expr_t *e, ASR::stmt_t **body, size_t at) {
        if (e == nullptr || !ASR::is_a<ASR::Var_t>(*e)) return e;
        ASR::symbol_t *t = root_of(e);
        if (!k.private_scalar(t)) return e;
        for (size_t p = at; p-- > 0;) {
            if (!mentions(*body[p], t)) continue;
            if (!ASR::is_a<ASR::Assignment_t>(*body[p])) return e;
            auto *a = ASR::down_cast<ASR::Assignment_t>(body[p]);
            if (!ASR::is_a<ASR::Var_t>(*a->m_target)
                    || root_of(a->m_target) != t
                    || mentions(a->m_value, t)) {
                return e;
            }
            // Nothing in between changes what it reads.
            for (size_t q = p + 1; q < at; q++) {
                Effects between(k);
                between.visit_stmt(*body[q]);
                if (between.shared_effect || between.unknown) return e;
                for (ASR::symbol_t *x : between.scalars) {
                    if (mentions(a->m_value, x)) return e;
                }
            }
            return a->m_value;
        }
        return e;
    }

    bool same_step(ASR::expr_t *a, ASR::expr_t *b) {
        int64_t x = 1, y = 1;
        if (a && !ASRUtils::extract_value(ASRUtils::expr_value(a), x)) {
            return false;
        }
        if (b && !ASRUtils::extract_value(ASRUtils::expr_value(b), y)) {
            return false;
        }
        return x == y;
    }

    // The arrays a statement list assigns elements of.
    std::set<ASR::symbol_t*> assigned_arrays(ASR::stmt_t **body,
            size_t n_body) {
        Assignments assigns;
        for (size_t i = 0; i < n_body; i++) assigns.visit_stmt(*body[i]);
        std::set<ASR::symbol_t*> roots;
        for (ASR::Assignment_t *a : assigns.found) {
            if (!ASR::is_a<ASR::Var_t>(*a->m_target)) {
                roots.insert(root_of(a->m_target));
            }
        }
        return roots;
    }

    bool mentions_any(ASR::expr_t *e, const std::set<ASR::symbol_t*> &s) {
        for (ASR::symbol_t *x : s) {
            if (mentions(e, x)) return true;
        }
        return false;
    }

    bool has_block(ASR::DoLoop_t *d) {
        class Finder : public ASR::BaseWalkVisitor<Finder> {
        public:
            bool found = false;
            void visit_BlockCall(const ASR::BlockCall_t &) { found = true; }
        } f;
        for (size_t i = 0; i < d->n_body; i++) f.visit_stmt(*d->m_body[i]);
        return f.found;
    }

    // The loops that follow the distributed loop body[i], over the same
    // iterations, joined into it, one after another, as long as each
    // iteration of the result still reads and assigns only what it would
    // in the two loops: then running the loops one after another or the
    // iterations of the two together gives the same.
    void fuse_following(ASR::stmt_t **&body, size_t &n_body, size_t &i,
            ASR::stmt_t *owner, std::vector<Frame> &frames) {
        ASR::DoLoop_t *l1 = ASR::down_cast<ASR::DoLoop_t>(body[i]);
        if (has_block(l1)) return;
        while (true) {
            // Assignments of scalars, which the next loop's bounds may
            // name, move in front of the loop.
            std::vector<size_t> bridges;
            size_t j = i + 1;
            Effects e1(k);
            for (size_t q = 0; q < l1->n_body; q++) {
                e1.visit_stmt(*l1->m_body[q]);
            }
            std::set<ASR::symbol_t*> changed1 = e1.scalars;
            changed1.insert(root_of(l1->m_head.m_v));
            std::set<ASR::symbol_t*> arrays1 = assigned_arrays(l1->m_body,
                l1->n_body);
            for (; j < n_body; j++) {
                if (!ASR::is_a<ASR::Assignment_t>(*body[j])) break;
                auto *a = ASR::down_cast<ASR::Assignment_t>(body[j]);
                ASR::symbol_t *t = root_of(a->m_target);
                Effects eb(k);
                eb.visit_stmt(*body[j]);
                if (!ASR::is_a<ASR::Var_t>(*a->m_target)
                        || !k.private_scalar(t) || eb.shared_effect
                        || eb.unknown || changed1.count(t)
                        || mentions(*body[i], t)
                        || mentions_any(a->m_value, changed1)
                        || mentions_any(a->m_value, arrays1)) {
                    break;
                }
                bridges.push_back(j);
            }
            if (j >= n_body || !ASR::is_a<ASR::DoLoop_t>(*body[j])) return;
            ASR::DoLoop_t *l2 = ASR::down_cast<ASR::DoLoop_t>(body[j]);
            if (l2->n_orelse > 0 || l2->m_head.m_v == nullptr
                    || has_block(l2)) {
                return;
            }
            ASR::symbol_t *v1 = root_of(l1->m_head.m_v);
            ASR::symbol_t *v2 = root_of(l2->m_head.m_v);
            if (!k.private_scalar(v2)
                    || !same_step(l1->m_head.m_increment,
                        l2->m_head.m_increment)) {
                return;
            }
            ASR::expr_t *s1 = resolve(l1->m_head.m_start, body, i);
            ASR::expr_t *s2 = resolve(l2->m_head.m_start, body, j);
            ASR::expr_t *f1 = resolve(l1->m_head.m_end, body, i);
            ASR::expr_t *f2 = resolve(l2->m_head.m_end, body, j);
            if (!same_expr(s1, s2) || !same_expr(f1, f2)
                    || mentions_any(s2, changed1)
                    || mentions_any(f2, changed1)
                    || mentions_any(s2, arrays1)
                    || mentions_any(f2, arrays1)) {
                return;
            }
            // A scalar both loops name is the own of each iteration of
            // the one that comes second.
            Effects e2(k);
            for (size_t q = 0; q < l2->n_body; q++) {
                e2.visit_stmt(*l2->m_body[q]);
            }
            for (ASR::symbol_t *x : changed1) {
                if (k.stop_scalars.count(x)) continue;
                bool named = false;
                for (size_t q = 0; q < l2->n_body && !named; q++) {
                    named = mentions(*l2->m_body[q], x);
                }
                if (named && first_access(l2->m_body, 0, l2->n_body, x)
                        != Access::Write) {
                    return;
                }
            }
            for (ASR::symbol_t *x : e2.scalars) {
                if (k.stop_scalars.count(x)) continue;
                bool named = false;
                for (size_t q = 0; q < l1->n_body && !named; q++) {
                    named = mentions(*l1->m_body[q], x);
                }
                if ((named || x == v1) && first_access(l1->m_body, 0,
                        l1->n_body, x) != Access::Write) {
                    return;
                }
            }
            // The second loop's variable has no value after the loop.
            frames.push_back({body, n_body, j, owner});
            bool v2_dead = dead_after(frames, v2);
            frames.pop_back();
            if (!v2_dead) return;

            // The joined loop, tried in place.
            ASRUtils::ExprStmtDuplicator dup(al);
            dup.allow_procedure_calls = true;
            Vec<ASR::stmt_t*> joined;
            joined.reserve(al, l1->n_body + l2->n_body);
            for (size_t q = 0; q < l1->n_body; q++) {
                joined.push_back(al, l1->m_body[q]);
            }
            class Rename : public ASR::BaseWalkVisitor<Rename> {
            public:
                ASR::symbol_t *from, *to;
                void visit_Var(const ASR::Var_t &x) {
                    if (ASRUtils::symbol_get_past_external(x.m_v) == from) {
                        const_cast<ASR::Var_t&>(x).m_v = to;
                    }
                }
            } rename;
            rename.from = v2;
            rename.to = v1;
            for (size_t q = 0; q < l2->n_body; q++) {
                ASR::stmt_t *copy = dup.duplicate_stmt(l2->m_body[q]);
                if (copy == nullptr) return;
                rename.visit_stmt(*copy);
                joined.push_back(al, copy);
            }
            Vec<ASR::stmt_t*> list;
            list.reserve(al, n_body);
            for (size_t q = 0; q < i; q++) list.push_back(al, body[q]);
            for (size_t q : bridges) list.push_back(al, body[q]);
            size_t at = list.n;
            list.push_back(al, body[i]);
            for (size_t q = j + 1; q < n_body; q++) {
                list.push_back(al, body[q]);
            }
            ASR::stmt_t **old_body = l1->m_body;
            size_t old_n = l1->n_body;
            l1->m_body = joined.p;
            l1->n_body = joined.n;
            frames.push_back({list.p, list.n, at, owner});
            bool ok = distributable(l1, frames);
            frames.pop_back();
            if (!ok) {
                l1->m_body = old_body;
                l1->n_body = old_n;
                return;
            }
            for (size_t q : bridges) roles[body[q]] = Role::Replicate;
            body = list.p;
            n_body = list.n;
            i = at;
            // The loops inside, now side by side, may join in turn.
            frames.push_back({body, n_body, i, owner});
            fuse_inner(l1, frames);
            frames.pop_back();
        }
    }

    // A reduction loop over the iterations of the distributed loop body[i]
    // that follows it, past assignments of scalars (the starts of its
    // accumulators, which move in front of it), and reads what body[i]
    // assigns only where body[i] assigns it in the same iteration, joined
    // into body[i]: each lane accumulates the elements it has just
    // computed (the sums of squares of the vectors a phase works out, for
    // their norms), in one pass over them, with no barrier in between, and
    // the lanes combine what they hold after the loop, as after a
    // reduction loop, and wait for each other.
    void fold_reduction(ASR::stmt_t **&body, size_t &n_body, size_t &i,
            ASR::stmt_t *owner, std::vector<Frame> &frames) {
        if (!k.lane_reductions) return;
        ASR::DoLoop_t *l1 = ASR::down_cast<ASR::DoLoop_t>(body[i]);
        Effects e1(k);
        for (size_t q = 0; q < l1->n_body; q++) e1.visit_stmt(*l1->m_body[q]);
        if (!e1.statuses.empty()) return;
        std::set<ASR::symbol_t*> changed1 = e1.scalars;
        ASR::symbol_t *v1 = root_of(l1->m_head.m_v);
        changed1.insert(v1);
        std::set<ASR::symbol_t*> arrays1 = assigned_arrays(l1->m_body,
            l1->n_body);
        std::vector<size_t> bridges;
        size_t j = i + 1;
        for (; j < n_body; j++) {
            if (!ASR::is_a<ASR::Assignment_t>(*body[j])) break;
            auto *a = ASR::down_cast<ASR::Assignment_t>(body[j]);
            ASR::symbol_t *t = root_of(a->m_target);
            Effects eb(k);
            eb.visit_stmt(*body[j]);
            if (!ASR::is_a<ASR::Var_t>(*a->m_target) || !k.private_scalar(t)
                    || eb.shared_effect || eb.unknown || changed1.count(t)
                    || mentions(*body[i], t)
                    || mentions_any(a->m_value, changed1)
                    || mentions_any(a->m_value, arrays1)) {
                break;
            }
            bridges.push_back(j);
        }
        if (j >= n_body || !ASR::is_a<ASR::DoLoop_t>(*body[j])) return;
        ASR::DoLoop_t *l2 = ASR::down_cast<ASR::DoLoop_t>(body[j]);
        if (l2->n_orelse > 0 || l2->m_head.m_v == nullptr || has_block(l2)) {
            return;
        }
        ASR::symbol_t *v2 = root_of(l2->m_head.m_v);
        if (!k.private_scalar(v2) || !same_step(l1->m_head.m_increment,
                l2->m_head.m_increment)) {
            return;
        }
        ASR::expr_t *s1 = resolve(l1->m_head.m_start, body, i);
        ASR::expr_t *s2 = resolve(l2->m_head.m_start, body, j);
        ASR::expr_t *f1 = resolve(l1->m_head.m_end, body, i);
        ASR::expr_t *f2 = resolve(l2->m_head.m_end, body, j);
        if (!same_expr(s1, s2) || !same_expr(f1, f2)
                || mentions_any(s2, changed1) || mentions_any(f2, changed1)
                || mentions_any(s2, arrays1) || mentions_any(f2, arrays1)) {
            return;
        }
        std::vector<Accumulator> accumulators;
        std::vector<Induction> counters;
        frames.push_back({body, n_body, j, owner});
        bool was_scattered = declined_scattered;
        bool ok = reducible(l2, frames, accumulators, counters);
        declined_scattered = was_scattered;
        frames.pop_back();
        if (!ok || !counters.empty() || accumulators.empty()
                || live_variables.count(&l2->base)) {
            return;
        }
        // The joined body, with the second loop's variable the first's.
        ASRUtils::ExprStmtDuplicator dup(al);
        dup.allow_procedure_calls = true;
        class Rename : public ASR::BaseWalkVisitor<Rename> {
        public:
            ASR::symbol_t *from, *to;
            void visit_Var(const ASR::Var_t &x) {
                if (ASRUtils::symbol_get_past_external(x.m_v) == from) {
                    const_cast<ASR::Var_t&>(x).m_v = to;
                }
            }
        } rename;
        rename.from = v2;
        rename.to = v1;
        std::vector<ASR::stmt_t*> copies;
        for (size_t q = 0; q < l2->n_body; q++) {
            ASR::stmt_t *copy = dup.duplicate_stmt(l2->m_body[q]);
            if (copy == nullptr) return;
            rename.visit_stmt(*copy);
            copies.push_back(copy);
        }
        // It reads what the first assigns where the first assigns it, and
        // none of the first's scalars; the first names none of its own.
        Assignments assigns;
        for (size_t q = 0; q < l1->n_body; q++) assigns.visit_stmt(*l1->m_body[q]);
        Designators names;
        for (ASR::stmt_t *c : copies) names.visit_stmt(*c);
        for (ASR::expr_t *item : names.items) {
            if (!arrays1.count(root_of(item))) continue;
            bool same = false;
            for (ASR::Assignment_t *a : assigns.found) {
                if (same_expr(a->m_target, item)) same = true;
            }
            if (!same) return;
        }
        for (ASR::expr_t *w : names.wholes) {
            ASR::symbol_t *r = root_of(w);
            if (arrays1.count(r) || (r != v1 && changed1.count(r))) return;
        }
        Effects e2(k);
        for (ASR::stmt_t *c : copies) e2.visit_stmt(*c);
        for (ASR::symbol_t *x : e2.scalars) {
            if (x != v1 && mentions(*body[i], x)) return;
        }
        for (const Accumulator &a : accumulators) {
            if (mentions(*body[i], a.variable)) return;
        }
        bool copyable = true;
        block_scope(l1->m_body, l1->n_body, copyable);
        if (!copyable) return;
        Vec<ASR::stmt_t*> joined;
        joined.reserve(al, l1->n_body + copies.size());
        for (size_t q = 0; q < l1->n_body; q++) joined.push_back(al, l1->m_body[q]);
        for (ASR::stmt_t *c : copies) joined.push_back(al, c);
        l1->m_body = joined.p;
        l1->n_body = joined.n;
        Vec<ASR::stmt_t*> list;
        list.reserve(al, n_body);
        for (size_t q = 0; q < i; q++) list.push_back(al, body[q]);
        for (size_t q : bridges) {
            list.push_back(al, body[q]);
            roles[body[q]] = Role::Replicate;
        }
        for (size_t q = i; q < n_body; q++) {
            if (q == j || std::find(bridges.begin(), bridges.end(), q)
                    != bridges.end()) {
                continue;
            }
            list.push_back(al, body[q]);
        }
        body = list.p;
        n_body = list.n;
        i += bridges.size();
        reductions[&l1->base] = accumulators;
        folded.insert(&l1->base);
    }

    // The distributed loops a reduction loop was joined into
    // (fold_reduction).
    std::set<ASR::stmt_t*> folded;

    // Joins the loops of a distributed loop's body that run over the same
    // independent iterations: they run on one lane, one after another, and
    // only the order of independent iterations changes. `frames` ends with
    // the place of `l`.
    void fuse_inner(ASR::DoLoop_t *l, std::vector<Frame> &frames) {
        for (size_t i = 0; i < l->n_body; i++) {
            if (!ASR::is_a<ASR::DoLoop_t>(*l->m_body[i])) continue;
            frames.push_back({l->m_body, l->n_body, i, &l->base});
            bool saved = failed;
            bool ok = distributable(ASR::down_cast<ASR::DoLoop_t>(
                l->m_body[i]), frames);
            failed = saved;
            frames.pop_back();
            if (ok) fuse_following(l->m_body, l->n_body, i, &l->base, frames);
        }
    }

    ASR::expr_t* lanes_var(const Location &loc) {
        return ASRUtils::EXPR(ASR::make_Var_t(al, loc, k.lanes));
    }

    bool pays(ASR::stmt_t **body, size_t n_body, bool &column) {
        for (size_t i = 0; i < n_body; i++) {
            auto r = roles.find(body[i]);
            Role role = r == roles.end() ? Role::Replicate : r->second;
            if (role == Role::Distribute && either.count(body[i])) {
                // The copy that shares the sum instead decides the launch
                // size at which the lanes pay.
                auto *x = ASR::down_cast<ASR::DoLoop_t>(
                    either.find(body[i])->second);
                if (!pays(x->m_body, x->n_body, column)) return false;
            } else if (role == Role::Distribute) {
                if (!walks_columns(ASR::down_cast<ASR::DoLoop_t>(body[i]),
                        column)) {
                    return false;
                }
            } else if (role == Role::Recurse) {
                ASR::stmt_t *s = body[i];
                bool ok = true;
                switch (s->type) {
                    case ASR::stmtType::DoLoop: {
                        auto *x = ASR::down_cast<ASR::DoLoop_t>(s);
                        ok = pays(x->m_body, x->n_body, column);
                        break;
                    }
                    case ASR::stmtType::WhileLoop: {
                        auto *x = ASR::down_cast<ASR::WhileLoop_t>(s);
                        ok = pays(x->m_body, x->n_body, column);
                        break;
                    }
                    case ASR::stmtType::If: {
                        auto *x = ASR::down_cast<ASR::If_t>(s);
                        ok = pays(x->m_body, x->n_body, column)
                            && pays(x->m_orelse, x->n_orelse, column);
                        break;
                    }
                    case ASR::stmtType::BlockCall: {
                        ASR::Block_t *x = ASR::down_cast<ASR::Block_t>(
                            ASRUtils::symbol_get_past_external(
                                ASR::down_cast<ASR::BlockCall_t>(s)->m_m));
                        ok = pays(x->m_body, x->n_body, column);
                        break;
                    }
                    default:
                        ok = false;
                }
                if (!ok) return false;
            } else if (repeats_column(*body[i])) {
                return false;
            }
        }
        return true;
    }

    // Whether the distributed loop `d` reads and assigns the arrays it
    // names, by its variable, an induction of it or the variable of a loop
    // inside it, along their first subscript, at a stride of one, and is not a loop of a
    // few iterations known at compile time. Sets `column` when one of them
    // is a shared array whose later subscripts depend on the iteration.
    bool walks_columns(ASR::DoLoop_t *d, bool &column) {
        int64_t start = 0, end = 0, step = 1;
        if (ASRUtils::extract_value(ASRUtils::expr_value(d->m_head.m_start),
                    start)
                && ASRUtils::extract_value(ASRUtils::expr_value(
                    d->m_head.m_end), end)
                && (d->m_head.m_increment == nullptr
                    || ASRUtils::extract_value(ASRUtils::expr_value(
                        d->m_head.m_increment), step))
                && step != 0 && (end - start)/step + 1 < 64) {
            return false;
        }
        class Variables : public ASR::BaseWalkVisitor<Variables> {
        public:
            std::set<ASR::symbol_t*> found;
            std::set<ASR::symbol_t*> blocks;
            void visit_DoLoop(const ASR::DoLoop_t &x) {
                if (x.m_head.m_v) found.insert(root_of(x.m_head.m_v));
                BaseWalkVisitor::visit_DoLoop(x);
            }
            void visit_BlockCall(const ASR::BlockCall_t &x) {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
                if (!blocks.insert(b).second) return;
                ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(b);
                for (size_t i = 0; i < block->n_body; i++) {
                    visit_stmt(*block->m_body[i]);
                }
            }
        } variables;
        variables.visit_stmt(d->base);
        auto counters = inductions.find(&d->base);
        if (counters != inductions.end()) {
            for (const Induction &c : counters->second) {
                variables.found.insert(c.variable);
            }
        }
        Designators names;
        for (size_t i = 0; i < d->n_body; i++) names.visit_stmt(*d->m_body[i]);
        for (ASR::expr_t *e : names.items) {
            auto *item = ASR::down_cast<ASR::ArrayItem_t>(e);
            if (!k.shared(root_of(e))) continue;
            bool first = false, later = false, iteration = false;
            for (size_t i = 0; i < item->n_args; i++) {
                ASR::expr_t *sub = item->m_args[i].m_right;
                if (sub == nullptr) continue;
                for (ASR::symbol_t *v : variables.found) {
                    if (!mentions(sub, v)) continue;
                    if (i == 0) first = true; else later = true;
                    if (strided_by(sub, v)) return false;
                }
                if (i > 0 && reads_iteration(k, sub)) iteration = true;
            }
            if (later) return false;
            if (first && iteration) column = true;
        }
        return true;
    }

    // The pointers of the kernel associated with a section of an array,
    // as a section handed to a procedure is.
    std::map<ASR::symbol_t*, ASR::expr_t*> aliases;

    void collect_aliases(ASR::stmt_t **body, size_t n_body) {
        class Walk : public ASR::BaseWalkVisitor<Walk> {
        public:
            std::map<ASR::symbol_t*, ASR::expr_t*> &aliases;
            std::set<ASR::symbol_t*> blocks;
            explicit Walk(std::map<ASR::symbol_t*, ASR::expr_t*> &a)
                : aliases(a) {}
            void visit_Associate(const ASR::Associate_t &x) {
                if (ASR::is_a<ASR::Var_t>(*x.m_target)) {
                    aliases[root_of(x.m_target)] = x.m_value;
                }
            }
            void visit_BlockCall(const ASR::BlockCall_t &x) {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
                if (!blocks.insert(b).second) return;
                ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(b);
                for (size_t i = 0; i < block->n_body; i++) {
                    visit_stmt(*block->m_body[i]);
                }
            }
        } walk(aliases);
        for (size_t i = 0; i < n_body; i++) walk.visit_stmt(*body[i]);
    }

    // Whether a statement that every lane (or the first) runs walks a
    // column of a kernel argument that belongs to the iteration: names an
    // element of one inside a loop, or hands a section of one to a
    // procedure, itself or through a pointer associated with it.
    bool repeats_column(const ASR::stmt_t &s) {
        class Walk : public ASR::BaseWalkVisitor<Walk> {
        public:
            LaneRewriter &r;
            int loops = 0;
            bool found = false;
            std::set<ASR::symbol_t*> blocks;
            explicit Walk(LaneRewriter &r_) : r(r_) {}
            // A section of a kernel argument that belongs to the
            // iteration, or a pointer associated with one.
            bool section_column(ASR::expr_t *e) {
                while (ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
                    e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
                }
                if (ASR::is_a<ASR::Var_t>(*e)) {
                    auto a = r.aliases.find(root_of(e));
                    return a != r.aliases.end() && a->second != e
                        && section_column(a->second);
                }
                return ASR::is_a<ASR::ArraySection_t>(*e)
                    && argument_column(e);
            }
            bool argument_column(ASR::expr_t *e) {
                ASR::symbol_t *root = root_of(e);
                if (root == nullptr || !r.k.params.count(root)
                        || r.k.workspaces.count(root)) {
                    return false;
                }
                ASR::array_index_t *args = nullptr;
                size_t n = 0;
                if (ASR::is_a<ASR::ArrayItem_t>(*e)) {
                    auto *x = ASR::down_cast<ASR::ArrayItem_t>(e);
                    args = x->m_args;
                    n = x->n_args;
                } else if (ASR::is_a<ASR::ArraySection_t>(*e)) {
                    auto *x = ASR::down_cast<ASR::ArraySection_t>(e);
                    args = x->m_args;
                    n = x->n_args;
                }
                for (size_t i = 0; i < n; i++) {
                    if (reads_iteration(r.k, args[i].m_left)
                            || reads_iteration(r.k, args[i].m_right)
                            || reads_iteration(r.k, args[i].m_step)) {
                        return true;
                    }
                }
                return false;
            }
            void visit_ArrayItem(const ASR::ArrayItem_t &x) {
                if (loops > 0 && argument_column(
                        const_cast<ASR::expr_t*>(&x.base))) {
                    found = true;
                }
                BaseWalkVisitor::visit_ArrayItem(x);
            }
            void call_args(ASR::call_arg_t *args, size_t n) {
                for (size_t i = 0; i < n; i++) {
                    if (args[i].m_value && section_column(args[i].m_value)) {
                        found = true;
                    }
                }
            }
            void visit_FunctionCall(const ASR::FunctionCall_t &x) {
                call_args(x.m_args, x.n_args);
                BaseWalkVisitor::visit_FunctionCall(x);
            }
            void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
                call_args(x.m_args, x.n_args);
                BaseWalkVisitor::visit_SubroutineCall(x);
            }
            void visit_DoLoop(const ASR::DoLoop_t &x) {
                loops++;
                BaseWalkVisitor::visit_DoLoop(x);
                loops--;
            }
            void visit_WhileLoop(const ASR::WhileLoop_t &x) {
                loops++;
                BaseWalkVisitor::visit_WhileLoop(x);
                loops--;
            }
            void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
                loops++;
                BaseWalkVisitor::visit_DoConcurrentLoop(x);
                loops--;
            }
            void visit_BlockCall(const ASR::BlockCall_t &x) {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
                if (!blocks.insert(b).second) return;
                ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(b);
                for (size_t i = 0; i < block->n_body; i++) {
                    visit_stmt(*block->m_body[i]);
                }
            }
        } walk(*this);
        walk.visit_stmt(s);
        return walk.found;
    }

    ASR::expr_t* lane_index(const Location &loc) {
        return ASRUtils::EXPR(ASR::make_GpuLaneIndex_t(al, loc,
            ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)), nullptr));
    }

    // The first lane runs the statements, the others wait for it and take
    // the private scalars it assigned.
    void emit_single(const std::vector<ASR::stmt_t*> &stmts,
            Vec<ASR::stmt_t*> &out) {
        const Location &loc = stmts[0]->base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        Effects e(k);
        for (ASR::stmt_t *s : stmts) e.visit_stmt(*s);
        out.push_back(al, b.If(b.Eq(lane_index(loc), b.i32(0)), stmts, {}));
        // Statements that only add to contributions of the first lane have
        // nothing for the others to wait for.
        bool others_need = e.shared_effect || std::any_of(e.scalars.begin(),
            e.scalars.end(), [&](ASR::symbol_t *v) {
                return k.first_lane_scalars.count(v) == 0;
            });
        if (!others_need) return;
        out.push_back(al, ASRUtils::STMT(ASR::make_GpuLaneBarrier_t(al,
            loc)));
        for (ASR::symbol_t *s : e.scalars) {
            if (k.first_lane_scalars.count(s)) continue;
            ASR::expr_t *v = b.Var(s);
            out.push_back(al, b.Assignment(v, ASRUtils::EXPR(
                ASR::make_GpuLaneBroadcast_t(al, loc, v,
                    ASRUtils::symbol_type(s), nullptr))));
        }
    }

    // Lane j takes iterations j, j + lanes, ...; the lanes wait for each
    // other after the loop, and when one of them stopped, all return.
    void distribute(ASR::DoLoop_t *d, Vec<ASR::stmt_t*> &out) {
        if (collapse.count(&d->base)) {
            distribute_collapsed(d, out);
            return;
        }
        auto parts = parted.find(&d->base);
        if (parts != parted.end()) {
            distribute_parts(d, parts->second, out);
            return;
        }
        auto reduction = reductions.find(&d->base);
        if (reduction != reductions.end()) {
            distribute_reduction(d, reduction->second, out);
            if (folded.count(&d->base) && k.fixed_lanes <= k.lanes_count) {
                finish_distributed(d, out);
            }
            return;
        }
        const Location &loc = d->base.base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        ASR::ttype_t *type = ASRUtils::expr_type(d->m_head.m_v);
        ASR::expr_t *step = d->m_head.m_increment;
        ASR::expr_t *lane = b.i2i_t(lane_index(loc), type);
        ASR::expr_t *lanes = b.i2i_t(lanes_var(loc), type);
        int64_t trips = 0;
        if (constant_bounds(d, trips)) {
            lane_loops[&d->base] = {d->m_head.m_start, step, trips};
        }
        d->m_head.m_start = b.Add(d->m_head.m_start, b.Mul(lane, step));
        d->m_head.m_increment = b.Mul(step, lanes);
        out.push_back(al, &d->base);
        finish_distributed(d, out);
    }

    // The lists inside a loop, an `if` or a block.
    bool nested_decide(ASR::stmt_t *s, std::vector<Frame> &frames) {
        switch (s->type) {
            case ASR::stmtType::DoLoop: {
                auto *x = ASR::down_cast<ASR::DoLoop_t>(s);
                return decide(x->m_body, x->n_body, s, frames);
            }
            case ASR::stmtType::WhileLoop: {
                auto *x = ASR::down_cast<ASR::WhileLoop_t>(s);
                return decide(x->m_body, x->n_body, s, frames);
            }
            case ASR::stmtType::If: {
                auto *x = ASR::down_cast<ASR::If_t>(s);
                bool a = decide(x->m_body, x->n_body, s, frames);
                bool b = decide(x->m_orelse, x->n_orelse, s, frames);
                return a || b;
            }
            case ASR::stmtType::BlockCall: {
                ASR::Block_t *x = ASR::down_cast<ASR::Block_t>(
                    ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::BlockCall_t>(s)->m_m));
                return decide(x->m_body, x->n_body, s, frames);
            }
            default:
                return false;
        }
    }

    // The lanes wait for each other before `access`, when it may assign
    // shared memory they may still be reading.
    void settle_before(const SharedAccess &access, const Location &loc,
            Vec<ASR::stmt_t*> &out) {
        if (!intersects(access.writes, unsettled)) return;
        out.push_back(al, ASRUtils::STMT(ASR::make_GpuLaneBarrier_t(al,
            loc)));
        unsettled.clear();
    }

    // Shared memory the lanes may still be reading (apply).
    std::set<ASR::symbol_t*> unsettled;

    // What a loop's header reads, and, in its body, a barrier at the end
    // when the next iteration may assign what the lanes still read there.
    // What may be unsettled after the loop is what was before it (it may
    // run no iteration) and what is at the end of its body.
    void apply_loop(ASR::stmt_t *loop, ASR::stmt_t **&body, size_t &n_body,
            const std::vector<ASR::expr_t*> &header) {
        SharedAccess head(k);
        for (ASR::expr_t *e : header) {
            if (e) head.visit_expr(*e);
        }
        unsettled.insert(head.reads.begin(), head.reads.end());
        std::set<ASR::symbol_t*> before = unsettled;
        apply(body, n_body);
        unsettled.insert(head.reads.begin(), head.reads.end());
        SharedAccess inside(k);
        inside.visit_stmt(*loop);
        if (intersects(inside.writes, unsettled)) {
            Vec<ASR::stmt_t*> list;
            list.reserve(al, n_body + 1);
            for (size_t i = 0; i < n_body; i++) list.push_back(al, body[i]);
            list.push_back(al, ASRUtils::STMT(ASR::make_GpuLaneBarrier_t(al,
                loop->base.loc)));
            body = list.p;
            n_body = list.n;
            unsettled.clear();
        }
        unsettled.insert(before.begin(), before.end());
    }

    void nested_apply(ASR::stmt_t *s) {
        switch (s->type) {
            case ASR::stmtType::DoLoop: {
                auto *x = ASR::down_cast<ASR::DoLoop_t>(s);
                apply_loop(s, x->m_body, x->n_body, {x->m_head.m_start,
                    x->m_head.m_end, x->m_head.m_increment});
                break;
            }
            case ASR::stmtType::WhileLoop: {
                auto *x = ASR::down_cast<ASR::WhileLoop_t>(s);
                apply_loop(s, x->m_body, x->n_body, {x->m_test});
                break;
            }
            case ASR::stmtType::If: {
                auto *x = ASR::down_cast<ASR::If_t>(s);
                SharedAccess test(k);
                test.visit_expr(*x->m_test);
                unsettled.insert(test.reads.begin(), test.reads.end());
                std::set<ASR::symbol_t*> before = unsettled;
                apply(x->m_body, x->n_body);
                std::set<ASR::symbol_t*> taken = unsettled;
                unsettled = before;
                apply(x->m_orelse, x->n_orelse);
                unsettled.insert(taken.begin(), taken.end());
                break;
            }
            case ASR::stmtType::BlockCall: {
                ASR::Block_t *x = ASR::down_cast<ASR::Block_t>(
                    ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::BlockCall_t>(s)->m_m));
                apply(x->m_body, x->n_body);
                break;
            }
            default:
                LCOMPILERS_ASSERT(false);
        }
    }

    // Whether a distributed loop's body is assignments of scalars and then
    // one loop, whose iterations do not depend on the outer loop's and
    // tell apart the elements it assigns as well: then the lanes take the
    // pairs of iterations of the two, not only the outer ones.
    bool collapsible(ASR::DoLoop_t *l) {
        if (l->n_body == 0
                || !ASR::is_a<ASR::DoLoop_t>(*l->m_body[l->n_body - 1])) {
            return false;
        }
        ASR::DoLoop_t *m = ASR::down_cast<ASR::DoLoop_t>(
            l->m_body[l->n_body - 1]);
        ASR::symbol_t *v = root_of(l->m_head.m_v);
        if (m->n_orelse > 0 || m->m_head.m_v == nullptr
                || !k.private_scalar(root_of(m->m_head.m_v))
                || (m->m_head.m_increment != nullptr
                    && ASRUtils::expr_value(m->m_head.m_increment)
                        == nullptr)) {
            return false;
        }
        ASR::symbol_t *w = root_of(m->m_head.m_v);
        Effects em(k);
        for (size_t q = 0; q < m->n_body; q++) em.visit_stmt(*m->m_body[q]);
        std::set<ASR::symbol_t*> inner = em.scalars;
        inner.insert(w);
        for (size_t q = 0; q + 1 < l->n_body; q++) {
            if (!ASR::is_a<ASR::Assignment_t>(*l->m_body[q])) return false;
            auto *a = ASR::down_cast<ASR::Assignment_t>(l->m_body[q]);
            if (!ASR::is_a<ASR::Var_t>(*a->m_target)
                    || !k.private_scalar(root_of(a->m_target))
                    || mentions(a->m_value, v)
                    || mentions_any(a->m_value, inner)) {
                return false;
            }
        }
        for (ASR::expr_t *e : {m->m_head.m_start, m->m_head.m_end}) {
            if (mentions(e, v) || mentions_any(e, inner)) return false;
        }
        std::set<ASR::symbol_t*> changed = em.scalars;
        changed.insert(v);
        changed.insert(w);
        Assignments assigns;
        for (size_t q = 0; q < m->n_body; q++) assigns.visit_stmt(*m->m_body[q]);
        for (ASR::Assignment_t *a : assigns.found) {
            if (ASR::is_a<ASR::Var_t>(*a->m_target)) continue;
            ASR::ArrayItem_t *item = ASR::down_cast<ASR::ArrayItem_t>(
                a->m_target);
            bool by_v = false, by_w = false;
            for (size_t q = 0; q < item->n_args; q++) {
                by_v |= injective(item->m_args[q].m_right, v, changed);
                by_w |= injective(item->m_args[q].m_right, w, changed);
            }
            if (!by_v || !by_w) return false;
        }
        return true;
    }

    // The number of iterations of `do v = start, end, step`, at least zero.
    ASR::expr_t* trip_count(ASRUtils::ASRBuilder &b, ASR::DoLoop_t *d,
            ASR::ttype_t *type, Vec<ASR::stmt_t*> &out) {
        const Location &loc = d->base.base.loc;
        ASRUtils::ExprStmtDuplicator dup(al);
        dup.allow_procedure_calls = true;
        ASR::expr_t *step = d->m_head.m_increment
            ? dup.duplicate_expr(d->m_head.m_increment) : b.i_t(1, type);
        ASR::expr_t *n = b.Variable(k.fn->m_symtab,
            k.fn->m_symtab->get_unique_name("__gpu_lane_trips"), type,
            ASR::intentType::Local);
        out.push_back(al, b.Assignment(n, b.Add(b.Div(b.Sub(
            dup.duplicate_expr(d->m_head.m_end),
            dup.duplicate_expr(d->m_head.m_start)), step), b.i_t(1, type))));
        out.push_back(al, b.If(b.Lt(n, b.i_t(0, type)),
            {b.Assignment(n, b.i_t(0, type))}, {}));
        (void)loc;
        return n;
    }

    // The pairs of iterations of a collapsible loop and the loop inside
    // it, numbered from zero, shared out as the iterations of one loop.
    void distribute_collapsed(ASR::DoLoop_t *l, Vec<ASR::stmt_t*> &out) {
        const Location &loc = l->base.base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        ASR::DoLoop_t *m = ASR::down_cast<ASR::DoLoop_t>(
            l->m_body[l->n_body - 1]);
        ASR::ttype_t *type = ASRUtils::expr_type(l->m_head.m_v);
        ASR::expr_t *nv = trip_count(b, l, type, out);
        Vec<ASR::stmt_t*> inside;
        inside.reserve(al, l->n_body + 4);
        for (size_t q = 0; q + 1 < l->n_body; q++) {
            inside.push_back(al, l->m_body[q]);
        }
        ASR::expr_t *nw = trip_count(b, m, type, inside);
        ASR::expr_t *e = b.Variable(k.fn->m_symtab,
            k.fn->m_symtab->get_unique_name("__gpu_lane_pair"), type,
            ASR::intentType::Local);
        ASRUtils::ExprStmtDuplicator dup(al);
        dup.allow_procedure_calls = true;
        ASR::expr_t *step_v = l->m_head.m_increment
            ? l->m_head.m_increment : b.i_t(1, type);
        ASR::expr_t *step_w = m->m_head.m_increment
            ? m->m_head.m_increment : b.i_t(1, type);
        ASR::expr_t *mw = b.i2i_t(m->m_head.m_v, type);
        std::vector<ASR::stmt_t*> body = {
            b.Assignment(l->m_head.m_v, b.Add(l->m_head.m_start,
                b.Mul(b.Div(e, nw), step_v))),
            b.Assignment(m->m_head.m_v, b.Add(m->m_head.m_start,
                b.Mul(b.Sub(e, b.Mul(b.Div(e, nw), nw)), step_w))),
        };
        (void)mw;
        for (size_t q = 0; q < m->n_body; q++) body.push_back(m->m_body[q]);
        inside.push_back(al, b.DoLoop(e, b.i2i_t(lane_index(loc), type),
            b.Sub(b.Mul(nv, nw), b.i_t(1, type)), body,
            b.i2i_t(lanes_var(loc), type)));
        std::vector<ASR::stmt_t*> guarded(inside.p, inside.p + inside.n);
        out.push_back(al, b.If(b.Gt(nv, b.i_t(0, type)), guarded, {}));
        finish_distributed(l, out);
    }

    // A distributed loop of fewer iterations than the lanes of the block
    // (KernelInfo::block_lanes), each of which runs on `parts` of them, and
    // the loops of its body that those lanes share out, with the private
    // scalars each accumulates into (plan_parts).
    struct Parts {
        int64_t trips;
        int64_t parts;
        std::map<ASR::stmt_t*, std::vector<Accumulator>> loops;
        // The loops of the body that hold such loops, which every lane of
        // the iteration goes through (over the heads of a query).
        std::set<ASR::stmt_t*> containers;
        // Whether the lanes of an iteration are consecutive lanes of a
        // warp, which combine what they accumulate with shuffles: `parts`
        // is a power of two up to a SIMD-group.
        bool groups;
        // The variable of the loop and the scalars its body assigns, which
        // may differ between the lanes of the block.
        std::set<ASR::symbol_t*> varying;
    };
    std::map<ASR::stmt_t*, Parts> parted;
    // The fewest lanes per iteration that combine with shuffles rather than
    // through shared memory (Parts::groups): two lanes per column of a
    // state read along it at half the width of a warp, which costs more
    // than the barriers it saves.
    static constexpr int64_t parts_groups_min = 8;

    // The number of iterations of `d`, when its bounds and step are
    // constants.
    bool constant_bounds(ASR::DoLoop_t *d, int64_t &trips) {
        int64_t start = 0, end = 0, step = 1;
        if (!ASRUtils::extract_value(ASRUtils::expr_value(d->m_head.m_start),
                    start)
                || !ASRUtils::extract_value(ASRUtils::expr_value(
                    d->m_head.m_end), end)
                || (d->m_head.m_increment && !ASRUtils::extract_value(
                    ASRUtils::expr_value(d->m_head.m_increment), step))
                || step <= 0) {
            return false;
        }
        trips = end >= start ? (end - start)/step + 1 : 0;
        return true;
    }

    // An iteration of the distributed loop `l` -- a column of a matrix, the
    // state of a head -- whose body holds long loops of their own runs on
    // several lanes when the loop has fewer iterations than the block has
    // lanes (128 columns, 256 lanes): `parts` lanes take each iteration, and
    // share out the loops of its body whose iterations are independent but
    // for private scalars they accumulate into (sums over a column, and the
    // update of its elements), each lane taking every `parts`-th iteration
    // from its own, then combining what they accumulated through shared
    // memory. The other statements of the body run on each of those lanes,
    // with the same values, as on the lanes of an iteration: an element of
    // shared memory they assign is the iteration's, which its lanes all
    // assign the same value. The lanes of the block take one iteration each
    // (`trips` times `parts` is the block), and wait for each other inside
    // the body. Only for a kernel that runs one iteration per block (one
    // with arrays the block shares), whose lanes are known. The statements
    // several lanes run alike (all but the loops shared out; those too
    // when lanes past the last group repeat the last iteration) assign
    // memory the body does not read: two lanes would otherwise each add to
    // `acc(i) = acc(i) + s`, or one read what another assigned already.
    void plan_parts(ASR::DoLoop_t *l, std::vector<Frame> &frames) {
        if (k.block_lanes <= 0 || l->n_orelse > 0
                || l->m_head.m_v == nullptr) {
            return;
        }
        int64_t trips = 0;
        if (!constant_bounds(l, trips) || trips < 1
                || 2*trips > k.block_lanes) {
            return;
        }
        int64_t parts = std::min<int64_t>(k.block_lanes / trips,
            k.lanes_count);
        Effects e(k);
        for (size_t i = 0; i < l->n_body; i++) e.visit_stmt(*l->m_body[i]);
        if (e.unknown || e.returns || !e.statuses.empty()) return;
        Parts plan{trips, parts, {}, {},
            (parts & (parts - 1)) == 0 && parts >= parts_groups_min,
            e.scalars};
        plan.varying.insert(root_of(l->m_head.m_v));
        if (!plan_list(l->m_body, l->n_body, &l->base, frames, plan)
                || plan.loops.empty()) {
            return;
        }
        SharedAccess alike(k), split(k), all(k);
        parts_access(l->m_body, l->n_body, plan, alike, split);
        for (size_t i = 0; i < l->n_body; i++) all.visit_stmt(*l->m_body[i]);
        if (intersects(alike.writes, all.reads)) return;
        if (plan.groups && plan.trips*plan.parts < k.block_lanes
                && intersects(split.writes, all.reads)) {
            return;
        }
        parted[&l->base] = plan;
    }

    // What the statements of a list of a body planned by plan_parts access:
    // the loops shared out in `split`, the others, which several lanes run
    // alike, in `alike`.
    void parts_access(ASR::stmt_t **body, size_t n_body, const Parts &plan,
            SharedAccess &alike, SharedAccess &split) {
        for (size_t i = 0; i < n_body; i++) {
            ASR::stmt_t *s = body[i];
            if (plan.loops.count(s)) {
                split.visit_stmt(*s);
            } else if (plan.containers.count(s)) {
                auto *c = ASR::down_cast<ASR::DoLoop_t>(s);
                parts_access(c->m_body, c->n_body, plan, alike, split);
            } else {
                alike.visit_stmt(*s);
            }
        }
    }

    // The loops of a list of the body of a loop planned by plan_parts that
    // its lanes share out, and the loops that hold such loops, which every
    // lane goes through: false when the list holds other statements than
    // assignments and loops.
    bool plan_list(ASR::stmt_t **body, size_t n_body, ASR::stmt_t *owner,
            std::vector<Frame> &frames, Parts &plan) {
        for (size_t i = 0; i < n_body; i++) {
            ASR::stmt_t *s = body[i];
            if (ASR::is_a<ASR::Assignment_t>(*s)) continue;
            if (!ASR::is_a<ASR::DoLoop_t>(*s)) return false;
            auto *d = ASR::down_cast<ASR::DoLoop_t>(s);
            frames.push_back({body, n_body, i, owner});
            std::vector<Accumulator> accumulators;
            bool ok = true;
            if (splittable(d, plan.parts, frames, accumulators)) {
                plan.loops[s] = accumulators;
            } else if (d->n_orelse == 0 && d->m_head.m_v
                    && k.private_scalar(root_of(d->m_head.m_v))
                    && uniform_bounds(d, plan)) {
                size_t before = plan.loops.size();
                ok = plan_list(d->m_body, d->n_body, s, frames, plan);
                if (plan.loops.size() > before) plan.containers.insert(s);
            }
            frames.pop_back();
            if (!ok) return false;
        }
        return true;
    }

    // Whether every lane of the block runs the loop `d` of a body planned by
    // plan_parts as many times: its bounds name nothing that differs between
    // them. Its barriers and the shuffles of the loops it holds need them
    // all; the loop otherwise runs whole on each lane.
    bool uniform_bounds(ASR::DoLoop_t *d, const Parts &plan) {
        for (ASR::expr_t *e : {d->m_head.m_start, d->m_head.m_end,
                d->m_head.m_increment}) {
            if (e && mentions_any(e, plan.varying)) return false;
        }
        return true;
    }

    // Whether the lanes of an iteration share out the loop `d` of its body,
    // `parts` lanes each taking every `parts`-th iteration (plan_parts): a
    // loop of at least two iterations per lane, of assignments only, to
    // private scalars it accumulates into (`s = s op e`, as a reduction
    // loop), to private scalars of its own in every iteration that are dead
    // after it, and to elements of shared memory at one subscript list,
    // which tells its iterations apart and at which alone it reads them.
    bool splittable(ASR::DoLoop_t *d, int64_t parts,
            std::vector<Frame> &frames,
            std::vector<Accumulator> &accumulators) {
        int64_t trips = 0;
        if (d->n_orelse > 0 || d->m_head.m_v == nullptr
                || !constant_bounds(d, trips) || trips < 2*parts) {
            return false;
        }
        ASR::symbol_t *v = root_of(d->m_head.m_v);
        if (!k.private_scalar(v) || !dead_after(frames, v)) return false;
        Effects e(k);
        for (size_t i = 0; i < d->n_body; i++) {
            if (!ASR::is_a<ASR::Assignment_t>(*d->m_body[i])) return false;
            e.visit_stmt(*d->m_body[i]);
        }
        if (e.unknown || e.private_array || e.associates || e.returns
                || !e.statuses.empty()) {
            return false;
        }
        class Impure : public ASR::BaseWalkVisitor<Impure> {
        public:
            bool found = false;
            void visit_FunctionCall(const ASR::FunctionCall_t &x) {
                if (!pure_callee(x.m_name)) found = true;
                BaseWalkVisitor::visit_FunctionCall(x);
            }
            void visit_GpuAtomicFetchAdd(const ASR::GpuAtomicFetchAdd_t &) {
                found = true;
            }
        } impure;
        for (size_t i = 0; i < d->n_body; i++) impure.visit_stmt(*d->m_body[i]);
        if (impure.found) return false;
        std::set<ASR::symbol_t*> changed = e.scalars;
        changed.insert(v);
        std::vector<ASR::expr_t*> written;
        std::map<ASR::symbol_t*, ASR::reduction_opType> ops;
        for (size_t i = 0; i < d->n_body; i++) {
            auto *a = ASR::down_cast<ASR::Assignment_t>(d->m_body[i]);
            ASR::symbol_t *r = root_of(a->m_target);
            if (ASR::is_a<ASR::Var_t>(*a->m_target) && k.private_scalar(r)) {
                ASR::reduction_opType op;
                if (accumulation(r, a->m_value, op)) {
                    auto known = ops.find(r);
                    if (known != ops.end() && known->second != op) {
                        return false;
                    }
                    ops[r] = op;
                }
                continue;
            }
            if (!ASR::is_a<ASR::ArrayItem_t>(*a->m_target) || !k.shared(r)) {
                return false;
            }
            auto *item = ASR::down_cast<ASR::ArrayItem_t>(a->m_target);
            bool tells_apart = false;
            for (size_t j = 0; j < item->n_args; j++) {
                if (item->m_args[j].m_left || item->m_args[j].m_step) {
                    return false;
                }
                tells_apart |= injective(item->m_args[j].m_right, v, changed);
            }
            if (!tells_apart) return false;
            for (ASR::expr_t *w : written) {
                if (overlapping(path_of(w), path_of(a->m_target))
                        && !same_expr(w, a->m_target)) {
                    return false;
                }
            }
            written.push_back(a->m_target);
        }
        Designators names;
        for (size_t i = 0; i < d->n_body; i++) names.visit_stmt(*d->m_body[i]);
        for (ASR::expr_t *w : written) {
            for (ASR::expr_t *item : names.items) {
                if (overlapping(path_of(item), path_of(w))
                        && !same_expr(item, w)) {
                    return false;
                }
            }
            for (ASR::expr_t *whole : names.wholes) {
                if (overlapping(path_of(whole), path_of(w))) return false;
            }
        }
        for (ASR::symbol_t *s : e.scalars) {
            if (s == v) continue;
            for (ASR::expr_t *bound : {d->m_head.m_start, d->m_head.m_end,
                    d->m_head.m_increment}) {
                if (mentions(bound, s)) return false;
            }
            auto op = ops.find(s);
            if (op == ops.end()) {
                // Its own in every iteration.
                if (first_access(d->m_body, 0, d->n_body, s) != Access::Write
                        || !dead_after(frames, s)) {
                    return false;
                }
                continue;
            }
            // An accumulator: named only by its updates.
            for (size_t i = 0; i < d->n_body; i++) {
                auto *a = ASR::down_cast<ASR::Assignment_t>(d->m_body[i]);
                bool update = ASR::is_a<ASR::Var_t>(*a->m_target)
                    && root_of(a->m_target) == s;
                ASR::reduction_opType op2;
                if (update ? !accumulation(s, a->m_value, op2)
                        : mentions(*d->m_body[i], s)) {
                    return false;
                }
            }
            ASR::ttype_t *t = ASRUtils::symbol_type(s);
            int kind = ASRUtils::extract_kind_from_ttype_t(t);
            if (!(ASRUtils::is_integer(*t) || ASRUtils::is_real(*t))
                    || (kind != 4 && kind != 8)) {
                return false;
            }
            accumulators.push_back({s, op->second});
        }
        return true;
    }

    // Each lane takes the iteration `mod(lane, trips)` of a loop planned by
    // plan_parts, as the `lane / trips`-th of its lanes: the loops of its
    // body that they share out start at that part and step by `parts`
    // iterations, and the lanes combine what they accumulated through an
    // array of the block's shared memory per accumulator, one element per
    // lane, after a barrier; every lane folds the parts of its iteration in
    // the same order. Before a shared-out loop that assigns shared memory
    // the body read since the last barrier, and after one that assigns
    // shared memory, the lanes wait for each other.
    void distribute_parts(ASR::DoLoop_t *d, const Parts &plan,
            Vec<ASR::stmt_t*> &out) {
        const Location &loc = d->base.base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        ASR::ttype_t *type = ASRUtils::expr_type(d->m_head.m_v);
        ASR::ttype_t *int4 = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
        ASR::expr_t *step = d->m_head.m_increment ? d->m_head.m_increment
            : b.i_t(1, type);
        ASR::expr_t *slot = b.Variable(k.fn->m_symtab,
            k.fn->m_symtab->get_unique_name("__gpu_lane_slot"), int4,
            ASR::intentType::Local);
        ASR::expr_t *part = b.Variable(k.fn->m_symtab,
            k.fn->m_symtab->get_unique_name("__gpu_lane_part"), int4,
            ASR::intentType::Local);
        ASR::expr_t *trips = b.i32(static_cast<int32_t>(plan.trips));
        if (plan.groups) {
            // Lane `parts*slot + part`; the lanes past the last group take
            // the last iteration again, as the lanes of a warp all have to
            // run its shuffles.
            ASR::expr_t *parts = b.i32(static_cast<int32_t>(plan.parts));
            out.push_back(al, b.Assignment(slot, b.Min(b.Div(lane_index(loc),
                parts), b.i32(static_cast<int32_t>(plan.trips - 1)))));
            out.push_back(al, b.Assignment(part, b.Sub(lane_index(loc),
                b.Mul(b.Div(lane_index(loc), parts), parts))));
        } else {
            out.push_back(al, b.Assignment(slot, b.Sub(lane_index(loc),
                b.Mul(b.Div(lane_index(loc), trips), trips))));
            out.push_back(al, b.Assignment(part, b.Div(lane_index(loc),
                trips)));
        }
        out.push_back(al, b.Assignment(d->m_head.m_v, b.Add(d->m_head.m_start,
            b.Mul(b.i2i_t(slot, type), step))));
        share_parts(d->m_body, d->n_body, plan, part);
        std::set<ASR::symbol_t*> read;
        parts_list(d->m_body, d->n_body, plan, slot, part, read, false, out);
        finish_distributed(d, out);
    }

    // The statements of a list of the body of a loop planned by plan_parts
    // (distribute_parts), written to `out`: the loops the lanes share out
    // with the combination of what they accumulate after them (behind
    // another barrier inside a loop of the body, which writes the shared
    // memory of the combination again), and the loops that hold them with
    // their bodies written so. Where the lanes of the block are not a whole
    // number of parts of each iteration, the lanes past the last part
    // (`part` >= `parts`) skip the loops: they start their accumulators at
    // the identity, and take the combination as the others.
    void parts_list(ASR::stmt_t **body, size_t n_body, const Parts &plan,
            ASR::expr_t *slot, ASR::expr_t *part,
            std::set<ASR::symbol_t*> &read, bool nested,
            Vec<ASR::stmt_t*> &out) {
        bool idle = !plan.groups && plan.trips*plan.parts < k.block_lanes;
        for (size_t i = 0; i < n_body; i++) {
            ASR::stmt_t *s = body[i];
            const Location &loc = s->base.loc;
            ASRUtils::ASRBuilder b(al, loc);
            SharedAccess access(k);
            access.visit_stmt(*s);
            if (plan.containers.count(s)) {
                auto *c = ASR::down_cast<ASR::DoLoop_t>(s);
                Vec<ASR::stmt_t*> inner;
                inner.reserve(al, c->n_body + 8);
                std::set<ASR::symbol_t*> inner_read = read;
                parts_list(c->m_body, c->n_body, plan, slot, part,
                    inner_read, true, inner);
                c->m_body = inner.p;
                c->n_body = inner.n;
                read.insert(inner_read.begin(), inner_read.end());
                read.insert(access.reads.begin(), access.reads.end());
                out.push_back(al, s);
                continue;
            }
            auto split = plan.loops.find(s);
            if (split == plan.loops.end()) {
                read.insert(access.reads.begin(), access.reads.end());
                out.push_back(al, s);
                continue;
            }
            if (intersects(access.writes, read)) {
                out.push_back(al, ASRUtils::STMT(
                    ASR::make_GpuLaneBarrier_t(al, loc)));
                read.clear();
            }
            const std::vector<Accumulator> &accumulators = split->second;
            std::vector<ASR::stmt_t*> starts;
            for (const Accumulator &a : accumulators) {
                starts.push_back(b.Assignment(b.Var(a.variable), identity(a.op,
                    ASRUtils::symbol_type(a.variable), loc)));
            }
            if (!starts.empty()) {
                out.push_back(al, b.If(b.NotEq(part, b.i32(0)), starts, {}));
            }
            if (idle) {
                out.push_back(al, b.If(b.Lt(part,
                    b.i32(static_cast<int32_t>(plan.parts))), {s}, {}));
            } else {
                out.push_back(al, s);
            }
            if (accumulators.empty()) {
                if (!access.writes.empty()) {
                    out.push_back(al, ASRUtils::STMT(
                        ASR::make_GpuLaneBarrier_t(al, loc)));
                    read.clear();
                }
                continue;
            }
            if (plan.groups) {
                for (const Accumulator &a : accumulators) {
                    ASR::ttype_t *t = ASRUtils::symbol_type(a.variable);
                    out.push_back(al, b.Assignment(b.Var(a.variable),
                        ASRUtils::EXPR(ASR::make_GpuLaneReduce_t(al, loc,
                            b.Var(a.variable), a.op, plan.parts,
                            ASRUtils::duplicate_type(al, t), nullptr))));
                }
                if (!access.writes.empty()) {
                    out.push_back(al, ASRUtils::STMT(
                        ASR::make_GpuLaneBarrier_t(al, loc)));
                    read.clear();
                }
                continue;
            }
            std::vector<ASR::expr_t*> scratch;
            for (const Accumulator &a : accumulators) {
                ASR::ttype_t *t = ASRUtils::symbol_type(a.variable);
                Vec<ASR::dimension_t> dims;
                dims.reserve(al, 1);
                ASR::dimension_t dim;
                dim.loc = loc;
                dim.m_start = b.i32(1);
                dim.m_length = b.i32(static_cast<int32_t>(k.block_lanes));
                dims.push_back(al, dim);
                ASR::ttype_t *array = ASRUtils::TYPE(ASR::make_Array_t(al, loc,
                    ASRUtils::duplicate_type(al, t), dims.p, dims.n,
                    ASR::array_physical_typeType::FixedSizeArray,
                    ASR::memory_spaceType::Shared));
                ASR::expr_t *x = b.Variable(k.fn->m_symtab,
                    k.fn->m_symtab->get_unique_name("__gpu_lane_parts"),
                    array, ASR::intentType::Local);
                scratch.push_back(x);
                out.push_back(al, b.Assignment(b.ArrayItem_01(x,
                    {b.Add(lane_index(loc), b.i32(1))}), b.Var(a.variable)));
            }
            out.push_back(al, ASRUtils::STMT(ASR::make_GpuLaneBarrier_t(al,
                loc)));
            read.clear();
            for (size_t j = 0; j < accumulators.size(); j++) {
                const Accumulator &a = accumulators[j];
                ASR::expr_t *total = nullptr;
                for (int64_t q = 0; q < plan.parts; q++) {
                    ASR::expr_t *x = b.ArrayItem_01(scratch[j], {b.Add(slot,
                        b.i32(static_cast<int32_t>(q*plan.trips + 1)))});
                    total = total == nullptr ? x : combine(a.op, total, x, b);
                }
                out.push_back(al, b.Assignment(b.Var(a.variable), total));
            }
            if (nested) {
                out.push_back(al, ASRUtils::STMT(
                    ASR::make_GpuLaneBarrier_t(al, loc)));
            }
        }
    }

    ASR::expr_t* copy_expr(ASR::expr_t *e) {
        ASRUtils::ExprStmtDuplicator dup(al);
        dup.allow_procedure_calls = true;
        return dup.duplicate_expr(e);
    }

    // The loops of the body of `d` that its lanes share out (plan_parts),
    // each lane starting at its `part`. A loop whose iterations the lanes
    // share evenly counts the iterations of a lane, from one, and works out
    // its variable from that count first, each lane taking the `part`-th
    // contiguous run of them: each lane then runs the same number, known
    // at compile time, which the device compiler unrolls whole. Of two such loops over the same iterations, the second of
    // which assigns the elements of an array that the first reads (the
    // state of a column: its inner product, then its update), the second
    // takes what the first read from a private array, indexed by the count,
    // which the unrolled loops keep in registers, instead of reading it
    // again: the first stores each element it reads there, and the second
    // its new value, before it assigns the element.
    struct Counted {
        ASR::DoLoop_t *loop;
        ASR::expr_t *count;
        int64_t trips;
    };

    // Whether a lane of the block may take no part of an iteration (lanes
    // past the last part, which skip the shared-out loops), while
    // share_parts runs: their elements are not read outside those loops.
    bool idle_parts = false;

    std::vector<Counted> share_parts(ASR::stmt_t **&d_body, size_t &d_n,
            const Parts &plan, ASR::expr_t *part) {
        idle_parts = !plan.groups
            && plan.trips*plan.parts < k.block_lanes;
        std::vector<Counted> counted;
        std::vector<std::pair<size_t, ASR::stmt_t*>> before;
        // One count for the loops of a kind of variable: loops that count
        // the same iterations then work out their variable the same way.
        std::map<int, ASR::expr_t*> counts;
        for (size_t i = 0; i < d_n; i++) {
            if (plan.containers.count(d_body[i])) {
                auto *c = ASR::down_cast<ASR::DoLoop_t>(d_body[i]);
                std::vector<Counted> inner = share_parts(c->m_body, c->n_body,
                    plan, part);
                for (const Counted &x : inner) {
                    if (ASR::stmt_t *copy = hold_invariant(c, x)) {
                        before.push_back({i, copy});
                    }
                }
                continue;
            }
            if (!plan.loops.count(d_body[i])) continue;
            auto *l = ASR::down_cast<ASR::DoLoop_t>(d_body[i]);
            const Location &loc = l->base.base.loc;
            ASRUtils::ASRBuilder b(al, loc);
            ASR::ttype_t *lt = ASRUtils::expr_type(l->m_head.m_v);
            ASR::expr_t *lstep = l->m_head.m_increment
                ? l->m_head.m_increment : b.i_t(1, lt);
            ASR::expr_t *start = b.Add(l->m_head.m_start,
                b.Mul(b.i2i_t(part, lt), lstep));
            ASR::expr_t *stride = b.Mul(lstep, b.i_t(plan.parts, lt));
            int64_t trips = 0;
            constant_bounds(l, trips);
            if (trips % plan.parts != 0) {
                l->m_head.m_start = start;
                l->m_head.m_increment = stride;
                continue;
            }
            int kind = ASRUtils::extract_kind_from_ttype_t(lt);
            if (!counts.count(kind)) {
                counts[kind] = b.Variable(k.fn->m_symtab,
                    k.fn->m_symtab->get_unique_name("__gpu_lane_count"),
                    ASRUtils::duplicate_type(al, lt), ASR::intentType::Local);
            }
            ASR::expr_t *count = counts[kind];
            Vec<ASR::stmt_t*> body;
            body.reserve(al, l->n_body + 1);
            // A lane takes a contiguous part of the iterations: its
            // elements of a column are next to each other, as the lanes
            // of a hand-written kernel take halves of it (3% faster on an
            // H200 than every other element). The lanes of a group, which
            // are consecutive lanes of a warp (Parts::groups), take every
            // `parts`-th iteration instead, so that a warp reads
            // consecutive elements together.
            ASR::expr_t *first = plan.groups
                ? b.Add(copy_expr(l->m_head.m_start),
                    b.Mul(b.i2i_t(part, lt), copy_expr(lstep)))
                : b.Add(copy_expr(l->m_head.m_start),
                    b.Mul(b.i2i_t(part, lt), b.Mul(copy_expr(lstep),
                        b.i_t(trips / plan.parts, lt))));
            ASR::expr_t *by = plan.groups
                ? b.Mul(copy_expr(lstep), b.i_t(plan.parts, lt))
                : copy_expr(lstep);
            body.push_back(al, b.Assignment(l->m_head.m_v, b.Add(first,
                b.Mul(b.Sub(count, b.i_t(1, lt)), by))));
            for (size_t j = 0; j < l->n_body; j++) {
                body.push_back(al, l->m_body[j]);
            }
            counted.push_back({l, count, trips / plan.parts});
            l->m_head.m_v = count;
            l->m_head.m_start = b.i_t(1, lt);
            l->m_head.m_end = b.i_t(trips / plan.parts, lt);
            l->m_head.m_increment = b.i_t(1, lt);
            l->m_body = body.p;
            l->n_body = body.n;
        }
        for (size_t a = 0; a + 1 < counted.size(); a++) {
            for (size_t c = a + 1; c < counted.size(); c++) {
                hold_elements(d_body, d_n, counted[a].loop, counted[c].loop,
                    counted[a].count, counted[c].count, counted[a].trips);
            }
        }
        if (!before.empty()) {
            Vec<ASR::stmt_t*> list;
            list.reserve(al, d_n + before.size());
            size_t next = 0;
            for (size_t i = 0; i < d_n; i++) {
                while (next < before.size() && before[next].first == i) {
                    list.push_back(al, before[next++].second);
                }
                list.push_back(al, d_body[i]);
            }
            d_body = list.p;
            d_n = list.n;
        }
        return counted;
    }

    // The elements that the counted loop `x` (share_parts), which runs in
    // every iteration of the loop `c` of the body around it (over the heads
    // of a query), reads the same of in each of them -- a subscript list
    // that names neither the variable of `c` nor what `c` assigns, of an
    // array `c` does not assign -- read once ahead of `c` into a private
    // array, indexed by the count, which the unrolled loops keep in
    // registers: the key of a position, for the scores of all the heads.
    // Returns the loop that reads them, or nullptr.
    ASR::stmt_t* hold_invariant(ASR::DoLoop_t *c, const Counted &x) {
        ASR::DoLoop_t *l = x.loop;
        if (idle_parts || l->n_body < 2) return nullptr;
        Effects e(k);
        e.visit_stmt(c->base);
        std::set<ASR::symbol_t*> changed = e.scalars;
        changed.insert(root_of(c->m_head.m_v));
        auto *v = ASR::down_cast<ASR::Assignment_t>(l->m_body[0]);
        ASR::symbol_t *j = root_of(v->m_target);
        // The count and the variable of `x` itself are worked out the same
        // way ahead of `c`.
        changed.erase(root_of(x.count));
        changed.erase(j);
        if (mentions_any(v->m_value, changed)) return nullptr;
        std::set<ASR::symbol_t*> assigned = assigned_arrays(c->m_body,
            c->n_body);
        Designators names;
        for (size_t i = 1; i < l->n_body; i++) names.visit_stmt(*l->m_body[i]);
        for (ASR::expr_t *item : names.items) {
            ASR::symbol_t *array = root_of(item);
            if (array == nullptr || assigned.count(array)
                    || !ASR::is_a<ASR::Var_t>(*ASR::down_cast<ASR::ArrayItem_t>(
                        item)->m_v)) {
                continue;
            }
            ASR::expr_t *element = nullptr;
            bool written = false;
            if (!element_of(l, array, element, written) || written
                    || element != item) {
                continue;
            }
            auto *a = ASR::down_cast<ASR::ArrayItem_t>(element);
            bool invariant = true;
            for (size_t i = 0; i < a->n_args; i++) {
                for (ASR::symbol_t *s : changed) {
                    if (s != j && mentions(a->m_args[i].m_right, s)) {
                        invariant = false;
                    }
                }
            }
            ASR::ttype_t *t = ASRUtils::expr_type(element);
            if (!invariant || !(ASRUtils::is_real(*t)
                    || ASRUtils::is_integer(*t))
                    || x.trips*ASRUtils::extract_kind_from_ttype_t(t)
                        > held_bytes_max) {
                continue;
            }
            const Location &loc = element->base.loc;
            ASRUtils::ASRBuilder b(al, loc);
            Vec<ASR::dimension_t> dims;
            dims.reserve(al, 1);
            ASR::dimension_t dim;
            dim.loc = loc;
            dim.m_start = b.i32(1);
            dim.m_length = b.i32(static_cast<int32_t>(x.trips));
            dims.push_back(al, dim);
            ASR::expr_t *held = b.Variable(k.fn->m_symtab,
                k.fn->m_symtab->get_unique_name("__gpu_lane_held"),
                ASRUtils::TYPE(ASR::make_Array_t(al, loc,
                    ASRUtils::duplicate_type(al, t), dims.p, dims.n,
                    ASR::array_physical_typeType::FixedSizeArray,
                    ASR::memory_spaceType::Thread)), ASR::intentType::Local);
            ASRUtils::ExprStmtDuplicator dup(al);
            dup.allow_procedure_calls = true;
            ASR::expr_t *source = dup.duplicate_expr(element);
            class Replace : public ASR::BaseExprReplacer<Replace> {
            public:
                ASRUtils::ASRBuilder &b;
                ASR::expr_t *element, *held, *count;
                Replace(ASRUtils::ASRBuilder &b_, ASR::expr_t *e_,
                    ASR::expr_t *h_, ASR::expr_t *c_)
                    : b(b_), element(e_), held(h_), count(c_) {}
                void replace_ArrayItem(ASR::ArrayItem_t *y) {
                    if (same_expr(&y->base, element)) {
                        *current_expr = b.ArrayItem_01(held, {count});
                        return;
                    }
                    BaseExprReplacer::replace_ArrayItem(y);
                }
            } r(b, element, held, x.count);
            for (size_t i = 1; i < l->n_body; i++) {
                auto *s = ASR::down_cast<ASR::Assignment_t>(l->m_body[i]);
                r.current_expr = &s->m_value;
                r.replace_expr(s->m_value);
            }
            ASR::do_loop_head_t head = l->m_head;
            head.m_start = dup.duplicate_expr(head.m_start);
            head.m_end = dup.duplicate_expr(head.m_end);
            head.m_increment = dup.duplicate_expr(head.m_increment);
            Vec<ASR::stmt_t*> reads;
            reads.reserve(al, 2);
            reads.push_back(al, dup.duplicate_stmt(l->m_body[0]));
            reads.push_back(al, b.Assignment(b.ArrayItem_01(held, {x.count}),
                source));
            ASR::stmt_t *copy = ASRUtils::STMT(ASRUtils::make_DoLoop_t_util(al,
                loc, nullptr, head, reads.p, reads.n, nullptr, 0));
            held_loops.insert(copy);
            return copy;
        }
        return nullptr;
    }

    // The elements an array is read at in the body of a counted loop of
    // share_parts (its first statement works out the variable), when they
    // are all the same designator, and whether it assigns it there.
    bool element_of(ASR::DoLoop_t *l, ASR::symbol_t *array,
            ASR::expr_t *&element, bool &assigned) {
        element = nullptr;
        assigned = false;
        Designators names;
        for (size_t i = 1; i < l->n_body; i++) names.visit_stmt(*l->m_body[i]);
        for (ASR::expr_t *w : names.wholes) {
            if (root_of(w) == array) return false;
        }
        for (ASR::expr_t *item : names.items) {
            if (root_of(item) != array) continue;
            if (element && !same_expr(element, item)) return false;
            element = item;
        }
        for (size_t i = 1; i < l->n_body; i++) {
            auto *x = ASR::down_cast<ASR::Assignment_t>(l->m_body[i]);
            if (root_of(x->m_target) == array) assigned = true;
        }
        return element != nullptr;
    }

    // Keeps the elements of an array that the counted loop `first` reads
    // and the counted loop `second` (later in the body of `d`, over the
    // same iterations) assigns in a private array of registers
    // (share_parts), when nothing in between names the array or changes
    // what its subscripts read.
    void hold_elements(ASR::stmt_t **d_body, size_t d_n, ASR::DoLoop_t *first,
            ASR::DoLoop_t *second, ASR::expr_t *count1, ASR::expr_t *count2,
            int64_t trips) {
        auto *v1 = ASR::down_cast<ASR::Assignment_t>(first->m_body[0]);
        auto *v2 = ASR::down_cast<ASR::Assignment_t>(second->m_body[0]);
        if (!same_expr(v1->m_target, v2->m_target)
                || !same_value(v1->m_value, v2->m_value)) {
            return;
        }
        ASR::symbol_t *j = root_of(v1->m_target);
        Assignments assigns;
        for (size_t i = 1; i < second->n_body; i++) {
            assigns.visit_stmt(*second->m_body[i]);
        }
        for (ASR::Assignment_t *a : assigns.found) {
            if (!ASR::is_a<ASR::ArrayItem_t>(*a->m_target)) continue;
            ASR::symbol_t *array = root_of(a->m_target);
            ASR::expr_t *e1 = nullptr, *e2 = nullptr;
            bool assigned1 = false, assigned2 = false;
            if (!element_of(first, array, e1, assigned1) || assigned1
                    || !element_of(second, array, e2, assigned2)
                    || !same_expr(e1, e2)) {
                continue;
            }
            ASR::ttype_t *t = ASRUtils::expr_type(e1);
            if (!(ASRUtils::is_real(*t) || ASRUtils::is_integer(*t))
                    || trips*ASRUtils::extract_kind_from_ttype_t(t)
                        > held_bytes_max) {
                continue;
            }
            // What the subscripts read, but for the variable, is the same
            // in both loops.
            std::set<ASR::symbol_t*> changed;
            for (ASR::DoLoop_t *l : {first, second}) {
                Effects e(k);
                for (size_t i = 0; i < l->n_body; i++) {
                    e.visit_stmt(*l->m_body[i]);
                }
                changed.insert(e.scalars.begin(), e.scalars.end());
            }
            size_t at1 = d_n, at2 = d_n;
            for (size_t i = 0; i < d_n; i++) {
                if (d_body[i] == &first->base) at1 = i;
                if (d_body[i] == &second->base) at2 = i;
            }
            bool kept = at1 < at2 && at2 < d_n;
            for (size_t i = at1 + 1; kept && i < at2; i++) {
                Effects e(k);
                e.visit_stmt(*d_body[i]);
                changed.insert(e.scalars.begin(), e.scalars.end());
                if (mentions(*d_body[i], array)) kept = false;
            }
            auto *item = ASR::down_cast<ASR::ArrayItem_t>(e1);
            for (size_t i = 0; kept && i < item->n_args; i++) {
                for (ASR::symbol_t *x : changed) {
                    if (x != j && mentions(item->m_args[i].m_right, x)) {
                        kept = false;
                    }
                }
            }
            if (!kept) continue;
            hold(first, second, e1, count1, count2, trips, t);
        }
    }

    // The most bytes of the elements a lane keeps in registers
    // (hold_elements): 64 `float`s, as a hand-written update of a 128 x 128
    // state on two lanes per column keeps.
    static constexpr int64_t held_bytes_max = 256;

    void hold(ASR::DoLoop_t *first, ASR::DoLoop_t *second,
            ASR::expr_t *element, ASR::expr_t *count1, ASR::expr_t *count2,
            int64_t trips, ASR::ttype_t *t) {
        const Location &loc = element->base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        Vec<ASR::dimension_t> dims;
        dims.reserve(al, 1);
        ASR::dimension_t dim;
        dim.loc = loc;
        dim.m_start = b.i32(1);
        dim.m_length = b.i32(static_cast<int32_t>(trips));
        dims.push_back(al, dim);
        ASR::expr_t *held = b.Variable(k.fn->m_symtab,
            k.fn->m_symtab->get_unique_name("__gpu_lane_held"),
            ASRUtils::TYPE(ASR::make_Array_t(al, loc,
                ASRUtils::duplicate_type(al, t), dims.p, dims.n,
                ASR::array_physical_typeType::FixedSizeArray,
                ASR::memory_spaceType::Thread)), ASR::intentType::Local);
        class Replace : public ASR::BaseExprReplacer<Replace> {
        public:
            ASRUtils::ASRBuilder &b;
            ASR::expr_t *element, *held, *count;
            Replace(ASRUtils::ASRBuilder &b_, ASR::expr_t *e, ASR::expr_t *h,
                ASR::expr_t *c) : b(b_), element(e), held(h), count(c) {}
            ASR::expr_t* item() {
                return b.ArrayItem_01(held, {count});
            }
            void replace_ArrayItem(ASR::ArrayItem_t *x) {
                if (same_expr(&x->base, element)) {
                    *current_expr = item();
                    return;
                }
                BaseExprReplacer::replace_ArrayItem(x);
            }
            void value(ASR::expr_t *&e) {
                current_expr = &e;
                replace_expr(e);
            }
        };
        Replace r1(b, element, held, count1);
        for (size_t i = 1; i < first->n_body; i++) {
            r1.value(ASR::down_cast<ASR::Assignment_t>(first->m_body[i])
                ->m_value);
        }
        Vec<ASR::stmt_t*> body1;
        body1.reserve(al, first->n_body + 1);
        body1.push_back(al, first->m_body[0]);
        body1.push_back(al, b.Assignment(r1.item(), element));
        for (size_t i = 1; i < first->n_body; i++) {
            body1.push_back(al, first->m_body[i]);
        }
        first->m_body = body1.p;
        if (!idle_parts) held_loops.insert(&first->base);
        first->n_body = body1.n;
        Replace r2(b, element, held, count2);
        Vec<ASR::stmt_t*> body2;
        body2.reserve(al, second->n_body + 2);
        for (size_t i = 0; i < second->n_body; i++) {
            auto *a = ASR::down_cast<ASR::Assignment_t>(second->m_body[i]);
            if (i > 0) r2.value(a->m_value);
            if (i > 0 && same_expr(a->m_target, element)) {
                body2.push_back(al, b.Assignment(r2.item(), a->m_value));
                a->m_value = r2.item();
            }
            body2.push_back(al, second->m_body[i]);
        }
        second->m_body = body2.p;
        second->n_body = body2.n;
    }

    ASR::expr_t* combine(ASR::reduction_opType op, ASR::expr_t *x,
            ASR::expr_t *y, ASRUtils::ASRBuilder &b) {
        switch (op) {
            case ASR::reduction_opType::ReduceAdd: return b.Add(x, y);
            case ASR::reduction_opType::ReduceMul: return b.Mul(x, y);
            case ASR::reduction_opType::ReduceMAX: return b.Max(x, y);
            default: return b.Min(x, y);
        }
    }

    // After a distributed loop the lanes wait for each other, and when one
    // of them stopped, all return.
    void finish_distributed(ASR::DoLoop_t *d, Vec<ASR::stmt_t*> &out) {
        const Location &loc = d->base.base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        out.push_back(al, ASRUtils::STMT(ASR::make_GpuLaneBarrier_t(al,
            loc)));
        Effects e(k);
        for (size_t i = 0; i < d->n_body; i++) e.visit_stmt(*d->m_body[i]);
        for (ASR::symbol_t *status : e.statuses) {
            out.push_back(al, b.If(b.NotEq(b.ArrayItem_01(b.Var(status),
                {b.i32(1)}), b.i32(0)), {b.Return()}, {}));
        }
    }

    Role role(ASR::stmt_t *s, std::vector<Frame> &frames) {
        switch (s->type) {
            case ASR::stmtType::BlockCall:
                if (nested_decide(s, frames)) return Role::Recurse;
                break;
            case ASR::stmtType::Return:
            case ASR::stmtType::Exit:
            case ASR::stmtType::Cycle:
                return Role::Replicate;
            case ASR::stmtType::DoLoop: {
                if (k.independent_loops && distributable(
                        ASR::down_cast<ASR::DoLoop_t>(s), frames)) {
                    // A `do concurrent` of the iteration says its own
                    // iterations are the ones to share out, and the plain
                    // loop with the sum inside it stays on each lane.
                    // In a kernel whose iteration has the lanes of a block,
                    // one of at least a SIMD-group of iterations known at
                    // compile time (the dimensions of a head) is shared out
                    // as it is, with each sum inside it on one lane.
                    int64_t known = 0;
                    bool wide = k.block_lanes > 0 && concurrent_loops.count(s)
                        && constant_bounds(ASR::down_cast<ASR::DoLoop_t>(s),
                            known)
                        && known >= k.lanes_count;
                    if (!wide && prefers_inner_reduction(
                            ASR::down_cast<ASR::DoLoop_t>(s), frames)) {
                        if (concurrent_loops.count(s)
                                && share_either(s, frames)) {
                            distributed = true;
                            return Role::Distribute;
                        }
                        bool was_failed = failed;
                        size_t n_serialized = serialized.size();
                        if (nested_decide(s, frames) && !failed) {
                            return Role::Recurse;
                        }
                        failed = was_failed;
                        for (size_t i = serialized.size(); i-- > n_serialized;) {
                            *serialized[i].first = serialized[i].second;
                        }
                        serialized.resize(n_serialized);
                    }
                    distributed = true;
                    return Role::Distribute;
                }
                std::vector<Accumulator> accumulators;
                std::vector<Induction> counters;
                declined_scattered = false;
                if (!reducible(ASR::down_cast<ASR::DoLoop_t>(s), frames,
                        accumulators, counters)) {
                    if (declined_scattered) scattered_declined = true;
                } else {
                    if (k.inner_reductions_first
                            && inner_reduction(
                                ASR::down_cast<ASR::DoLoop_t>(s)->m_body,
                                ASR::down_cast<ASR::DoLoop_t>(s)->n_body,
                                s, frames, true)) {
                        bool was_failed = failed;
                        size_t n_serialized = serialized.size();
                        int64_t was_limit = lane_limit;
                        lane_limit = lane_limit == 0 ? fixed_trips
                            : std::min(lane_limit, fixed_trips);
                        if (nested_decide(s, frames) && !failed) {
                            plan_deferral(ASR::down_cast<ASR::DoLoop_t>(s),
                                accumulators, frames);
                            return Role::Recurse;
                        }
                        lane_limit = was_limit;
                        failed = was_failed;
                        for (size_t i = serialized.size(); i-- > n_serialized;) {
                            *serialized[i].first = serialized[i].second;
                        }
                        serialized.resize(n_serialized);
                    }
                    reductions[s] = accumulators;
                    if (!counters.empty()) inductions[s] = counters;
                    distributed = true;
                    return Role::Distribute;
                }
                if (nested_decide(s, frames)) return Role::Recurse;
                break;
            }
            case ASR::stmtType::If:
                if (stop_record(*s, nullptr)) return Role::Replicate;
                // What one iteration alone does (`if (i == 1) a(1) = 0`) is
                // a statement of its own, which its assignments, of places
                // no other iteration assigns, are not taken apart from.
                if (single_iteration_test(k,
                        ASR::down_cast<ASR::If_t>(s)->m_test)
                        && !holds_loop(*s)) {
                    break;
                }
                if (nested_decide(s, frames)) {
                    Effects test(k);
                    test.visit_expr(*ASR::down_cast<ASR::If_t>(s)->m_test);
                    if (test.shared_effect || test.unknown) {
                        failed = true;
                    }
                    return Role::Recurse;
                }
                break;
            case ASR::stmtType::WhileLoop:
                if (nested_decide(s, frames)) {
                    Effects test(k);
                    test.visit_expr(*ASR::down_cast<ASR::WhileLoop_t>(s)
                        ->m_test);
                    if (test.shared_effect || test.unknown) {
                        failed = true;
                    }
                    return Role::Recurse;
                }
                break;
            default:
                break;
        }
        Effects e(k);
        e.visit_stmt(*s);
        if (e.unknown) {
            failed = true;
            return Role::Replicate;
        }
        // A contribution to a combination of array reductions, such as the
        // sum of the squares of what the first lane just stored, which the
        // others would otherwise wait for before they read it.
        if (!e.shared_effect && !e.scalars.empty() && !e.returns
                && !e.private_array && !e.associates
                && std::all_of(e.scalars.begin(), e.scalars.end(),
                    [&](ASR::symbol_t *v) {
                        return k.first_lane_scalars.count(v) > 0;
                    })) {
            return Role::Single;
        }
        if (!e.shared_effect) return Role::Replicate;
        // Run on the first lane alone. What it assigns has to reach the
        // others, and a return would leave them without it.
        if (e.returns || e.private_array || e.associates) {
            failed = true;
            return Role::Single;
        }
        LeavesLoop leaves;
        leaves.visit_stmt(*s);
        if (leaves.found) failed = true;
        if (writes_shared_temporary(k, *s)) failed = true;
        for (ASR::symbol_t *v : e.scalars) {
            if (k.first_lane_scalars.count(v)) continue;
            ASR::ttype_t *t = ASRUtils::symbol_type(v);
            if (!(ASRUtils::is_integer(*t) || ASRUtils::is_real(*t)
                    || ASRUtils::is_logical(*t))
                    || ASRUtils::extract_kind_from_ttype_t(t) != 4) {
                failed = true;
            }
        }
        return Role::Single;
    }

    // Whether the lanes share out a sum inside the independent loop `l`
    // better than `l` itself: a loop of its body (or of a block or an `if`
    // in it) that they can share out as a reduction, and whose number of
    // iterations is not a constant. Lanes that share out `l` idle when it
    // has few iterations, as when a long sum is split in a few parts, while
    // a sum of a length not known at compile time keeps them all busy, and
    // has them read along the column together; the groups of a quantized
    // matrix-vector product are the other way round: a sum of a fixed,
    // short length (64 codes) in each of many iterations.
    bool prefers_inner_reduction(ASR::DoLoop_t *l,
            std::vector<Frame> &frames) {
        if (!k.lane_reductions) return false;
        return inner_reduction(l->m_body, l->n_body, &l->base, frames);
    }

    bool inner_reduction(ASR::stmt_t **body, size_t n_body,
            ASR::stmt_t *owner, std::vector<Frame> &frames,
            bool fixed_length = false) {
        for (size_t i = 0; i < n_body; i++) {
            ASR::stmt_t *s = body[i];
            frames.push_back({body, n_body, i, owner});
            bool found = false;
            if (ASR::is_a<ASR::DoLoop_t>(*s)) {
                auto *d = ASR::down_cast<ASR::DoLoop_t>(s);
                std::vector<Accumulator> accumulators;
                std::vector<Induction> counters;
                int64_t trips = 0;
                found = (fixed_length ? long_constant_trips(d, trips)
                        : !constant_trips(d))
                    && reducible(d, frames, accumulators, counters);
                if (found && fixed_length) fixed_trips = trips;
            } else if (ASR::is_a<ASR::BlockCall_t>(*s)) {
                ASR::Block_t *x = ASR::down_cast<ASR::Block_t>(
                    ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::BlockCall_t>(s)->m_m));
                found = inner_reduction(x->m_body, x->n_body, s, frames,
                    fixed_length);
            } else if (ASR::is_a<ASR::If_t>(*s)) {
                auto *x = ASR::down_cast<ASR::If_t>(s);
                found = inner_reduction(x->m_body, x->n_body, s, frames,
                    fixed_length)
                    || inner_reduction(x->m_orelse, x->n_orelse, s, frames,
                        fixed_length);
            }
            frames.pop_back();
            if (found) return true;
        }
        return false;
    }

    // Whether `d` has the same number of iterations in every run, and
    // enough of them for half of the lanes to have one: the number is then
    // in `trips`.
    bool long_constant_trips(ASR::DoLoop_t *d, int64_t &trips) {
        int64_t step = 1;
        if (d->m_head.m_increment && !ASRUtils::extract_value(
                ASRUtils::expr_value(d->m_head.m_increment), step)) {
            return false;
        }
        if (step <= 0) return false;
        GpuAffineBuilder affine;
        affine.invariant_variable = [](ASR::symbol_t *) { return true; };
        GpuAffine start, end;
        if (!affine.affine(d->m_head.m_start, start)
                || !affine.affine(d->m_head.m_end, end)) {
            return false;
        }
        GpuAffine span = end.plus(start, -1);
        if (!span.is_constant()) return false;
        trips = span.constant / step + 1;
        return trips >= k.lanes_count / 2;
    }

    // Whether the number of iterations of `d` is the same in every run of
    // it, and what it is.
    bool constant_trip_count(ASR::DoLoop_t *d, int64_t &trips) {
        int64_t step = 1;
        if (d->m_head.m_increment && !ASRUtils::extract_value(
                ASRUtils::expr_value(d->m_head.m_increment), step)) {
            return false;
        }
        if (step <= 0) return false;
        GpuAffineBuilder affine;
        affine.invariant_variable = [](ASR::symbol_t *) { return true; };
        GpuAffine start, end;
        if (!affine.affine(d->m_head.m_start, start)
                || !affine.affine(d->m_head.m_end, end)) {
            return false;
        }
        GpuAffine span = end.plus(start, -1);
        if (!span.is_constant() || span.constant < 0) return false;
        trips = span.constant / step + 1;
        return true;
    }

    // The number of iterations of the loop inner_reduction found last.
    int64_t fixed_trips = 0;

    // The last call of reducible() declined its loop only because it reads
    // across columns.
    bool declined_scattered = false;

    // Whether the number of iterations of `d` is the same in every run of
    // it: its bounds differ by a constant, and its step is a constant.
    bool constant_trips(ASR::DoLoop_t *d) {
        int64_t step = 1;
        if (d->m_head.m_increment && !ASRUtils::extract_value(
                ASRUtils::expr_value(d->m_head.m_increment), step)) {
            return false;
        }
        GpuAffineBuilder affine;
        affine.invariant_variable = [](ASR::symbol_t *) { return true; };
        GpuAffine start, end;
        return affine.affine(d->m_head.m_start, start)
            && affine.affine(d->m_head.m_end, end)
            && end.plus(start, -1).is_constant();
    }

    // A statement of a loop body that assigns the private scalar `s`: an
    // update `s = value`, which may only happen in some iterations
    // (`conditional`), or `if (e > s) s = e` (`compared`), a maximum or
    // minimum of operation `op`.
    struct Update {
        ASR::Assignment_t *assignment;
        bool conditional;
        bool compared;
        ASR::reduction_opType op;
    };

    // Whether `test` is `value > s`, `s < value`, `value >= s` or `s <=
    // value` (`op` is then a maximum), or the same with the order reversed
    // (a minimum), and `value` does not name `s`.
    bool comparison(ASR::symbol_t *s, ASR::expr_t *test, ASR::expr_t *value,
            ASR::reduction_opType &op) {
        ASR::expr_t *left = nullptr, *right = nullptr;
        ASR::cmpopType cmp;
        if (ASR::is_a<ASR::RealCompare_t>(*test)) {
            auto *c = ASR::down_cast<ASR::RealCompare_t>(test);
            left = c->m_left; right = c->m_right; cmp = c->m_op;
        } else if (ASR::is_a<ASR::IntegerCompare_t>(*test)) {
            auto *c = ASR::down_cast<ASR::IntegerCompare_t>(test);
            left = c->m_left; right = c->m_right; cmp = c->m_op;
        } else {
            return false;
        }
        auto is_s = [&](ASR::expr_t *e) {
            return ASR::is_a<ASR::Var_t>(*e) && root_of(e) == s;
        };
        bool greater = cmp == ASR::cmpopType::Gt || cmp == ASR::cmpopType::GtE;
        bool less = cmp == ASR::cmpopType::Lt || cmp == ASR::cmpopType::LtE;
        if (!greater && !less) return false;
        if (mentions(value, s)) return false;
        // The value on the larger side is the new maximum.
        ASR::expr_t *larger = greater ? left : right;
        ASR::expr_t *smaller = greater ? right : left;
        if (is_s(smaller) && same_value(larger, value)) {
            op = ASR::reduction_opType::ReduceMAX;
            return true;
        }
        if (is_s(larger) && same_value(smaller, value)) {
            op = ASR::reduction_opType::ReduceMIN;
            return true;
        }
        return false;
    }

    // The accumulator and operation of `value`, the value assigned to the
    // private scalar `s`, when it is s + e, e + s, s - e, s * e, e * s,
    // max(s, e), max(e, s), min(s, e) or min(e, s) with an `e` that does
    // not name s.
    bool accumulation(ASR::symbol_t *s, ASR::expr_t *value,
            ASR::reduction_opType &op) {
        auto is_s = [&](ASR::expr_t *e) {
            return ASR::is_a<ASR::Var_t>(*e) && root_of(e) == s;
        };
        auto other = [&](ASR::expr_t *a, ASR::expr_t *b, bool commutes) {
            if (is_s(a) && !mentions(b, s)) return true;
            return commutes && is_s(b) && !mentions(a, s);
        };
        ASR::binopType binop;
        ASR::expr_t *left = nullptr, *right = nullptr;
        if (ASR::is_a<ASR::RealBinOp_t>(*value)) {
            auto *x = ASR::down_cast<ASR::RealBinOp_t>(value);
            binop = x->m_op; left = x->m_left; right = x->m_right;
        } else if (ASR::is_a<ASR::IntegerBinOp_t>(*value)) {
            auto *x = ASR::down_cast<ASR::IntegerBinOp_t>(value);
            binop = x->m_op; left = x->m_left; right = x->m_right;
        } else if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*value)) {
            auto *f = ASR::down_cast<ASR::IntrinsicElementalFunction_t>(
                value);
            using IEF = ASRUtils::IntrinsicElementalFunctions;
            bool is_max = f->m_intrinsic_id == static_cast<int64_t>(IEF::Max);
            bool is_min = f->m_intrinsic_id == static_cast<int64_t>(IEF::Min);
            if (!(is_max || is_min) || f->n_args != 2
                    || !other(f->m_args[0], f->m_args[1], true)) {
                return false;
            }
            op = is_max ? ASR::reduction_opType::ReduceMAX
                : ASR::reduction_opType::ReduceMIN;
            return true;
        } else {
            return false;
        }
        // s + e1 + e2 ... (each + or - of the chain applied to what is
        // left of it): `value` is the sum of s and of the terms, in this
        // order, so a statement that adds several terms at once updates the
        // accumulator as the statements that add each would in turn.
        auto chain = [&](ASR::expr_t *e) {
            while (!is_s(e)) {
                ASR::expr_t *l = nullptr, *r = nullptr;
                ASR::binopType o;
                if (ASR::is_a<ASR::RealBinOp_t>(*e)) {
                    auto *x = ASR::down_cast<ASR::RealBinOp_t>(e);
                    o = x->m_op; l = x->m_left; r = x->m_right;
                } else if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
                    auto *x = ASR::down_cast<ASR::IntegerBinOp_t>(e);
                    o = x->m_op; l = x->m_left; r = x->m_right;
                } else {
                    return false;
                }
                if ((o != ASR::binopType::Add && o != ASR::binopType::Sub)
                        || mentions(r, s)) {
                    return false;
                }
                e = l;
            }
            return true;
        };
        switch (binop) {
            case ASR::binopType::Add:
                op = ASR::reduction_opType::ReduceAdd;
                return other(left, right, true)
                    || (!mentions(right, s) && chain(left));
            // s - e: each lane takes away from what it starts with, the
            // others from zero, so the lanes add up what they hold.
            case ASR::binopType::Sub:
                op = ASR::reduction_opType::ReduceAdd;
                return other(left, right, false)
                    || (!mentions(right, s) && chain(left));
            case ASR::binopType::Mul:
                op = ASR::reduction_opType::ReduceMul;
                return other(left, right, true);
            default:
                return false;
        }
    }

    // Whether the iterations of `d` are independent but for scalars each
    // accumulates into -- a sum, a product, a maximum or a minimum -- which
    // the lanes can then share out, each accumulating its iterations from
    // the identity, and combine afterwards (GpuLaneReduce). The body only
    // assigns private scalars and calls pure functions, in statements,
    // blocks and loops of its own; every scalar it assigns is either its
    // own in every iteration and dead after the loop, an accumulator: an
    // integer or real updated by statements `s = s op e` of the body, all
    // by the same operation, and named nowhere else in the loop, or an
    // induction: an integer the body adds constants to, by statements
    // `j = j + c` of its own, and otherwise only reads (`counters`).
    bool reducible(ASR::DoLoop_t *d, std::vector<Frame> &frames,
            std::vector<Accumulator> &accumulators,
            std::vector<Induction> &counters) {
        declined_scattered = false;
        if (!k.lane_reductions) return false;
        if (d->n_orelse > 0 || d->m_head.m_v == nullptr) return false;
        // The copies of the body of a loop unrolled by gpu_residue_unroll
        // are blocks that each update the accumulator. Only a loop the
        // lanes share out whatever its caller (inner_reductions_first) has
        // them read: the sum of any other loop is left in the order it was
        // written.
        int64_t fixed = 0;
        bool blocks_hold_updates = k.inner_reductions_first
            && long_constant_trips(d, fixed);
        ASR::symbol_t *v = root_of(d->m_head.m_v);
        if (!k.private_scalar(v)) return false;
        if (d->m_head.m_increment) {
            ASR::expr_t *inc = ASRUtils::expr_value(d->m_head.m_increment);
            int64_t step = 0;
            if (inc == nullptr || !ASRUtils::extract_value(inc, step)
                    || step == 0) {
                return false;
            }
        }
        // A loop of a few iterations known at compile time (a short filter)
        // is left to unroll whole on each thread: lanes would mostly idle.
        {
            int64_t start = 0, end = 0, step = 1;
            if (ASRUtils::extract_value(ASRUtils::expr_value(
                        d->m_head.m_start), start)
                    && ASRUtils::extract_value(ASRUtils::expr_value(
                        d->m_head.m_end), end)
                    && (d->m_head.m_increment == nullptr
                        || ASRUtils::extract_value(ASRUtils::expr_value(
                            d->m_head.m_increment), step))
                    && step != 0 && (end - start)/step + 1 < 64) {
                return false;
            }
        }
        // Nor is a loop whose bounds are read from an array, as the row of
        // a compressed sparse matrix (`rowptr(i):rowptr(i + 1) - 1`) is:
        // such rows usually hold a few elements.
        if (reads_array_element(d->m_head.m_start)
                || reads_array_element(d->m_head.m_end)) {
            return false;
        }
        Effects e(k);
        for (size_t i = 0; i < d->n_body; i++) e.visit_stmt(*d->m_body[i]);
        if (e.unknown || e.private_array || e.associates || e.returns
                || e.shared_effect || !e.statuses.empty()) {
            return false;
        }
        {
            class Impure : public ASR::BaseWalkVisitor<Impure> {
            public:
                bool found = false;
                void visit_FunctionCall(const ASR::FunctionCall_t &x) {
                    if (!pure_callee(x.m_name)) found = true;
                    BaseWalkVisitor::visit_FunctionCall(x);
                }
                void visit_SubroutineCall(const ASR::SubroutineCall_t &) {
                    found = true;
                }
                void visit_GpuAtomicFetchAdd(
                        const ASR::GpuAtomicFetchAdd_t &) {
                    found = true;
                }
                void visit_BlockCall(const ASR::BlockCall_t &x) {
                    ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(
                        ASRUtils::symbol_get_past_external(x.m_m));
                    for (size_t i = 0; i < block->n_body; i++) {
                        visit_stmt(*block->m_body[i]);
                    }
                }
            } impure;
            for (size_t i = 0; i < d->n_body; i++) {
                impure.visit_stmt(*d->m_body[i]);
            }
            if (impure.found) return false;
        }
        LeavesLoop leaves;
        for (size_t i = 0; i < d->n_body; i++) {
            leaves.visit_stmt(*d->m_body[i]);
        }
        if (leaves.found) return false;
        // A launch with one lane per iteration runs a copy of the loop as
        // it was (distribute_reduction).
        bool copyable = true;
        block_scope(d->m_body, d->n_body, copyable);
        if (!copyable) return false;
        // What the variable, and an induction, hold after the loop is
        // worked out from the bounds, which the body must leave as they
        // were (gpu_residue_unroll goes on from where its loop stopped).
        bool bounds_kept = true;
        for (ASR::symbol_t *x : e.scalars) {
            if (mentions(d->m_head.m_start, x) || mentions(d->m_head.m_end, x)) {
                bounds_kept = false;
            }
        }
        if (!dead_after(frames, v)) {
            if (!bounds_kept) return false;
            live_variables.insert(&d->base);
        }
        for (ASR::symbol_t *s : e.scalars) {
            if (s == v) continue;
            if (k.stop_scalars.count(s)) return false;
            if (first_access(d->m_body, 0, d->n_body, s) != Access::Read
                    && dead_after(frames, s)) {
                continue;
            }
            // The lanes but the first start it at the identity, or each at
            // its own iteration, before the loop, so its bounds must not
            // read it: `do j = 1, n` with `n = n - 1` in the body would run
            // no iteration there.
            if (mentions(d->m_head.m_start, s) || mentions(d->m_head.m_end, s)
                    || mentions(d->m_head.m_increment, s)) {
                return false;
            }
            // Its updates, statements of the body itself; nothing inside
            // the statements of the body assigns it.
            std::vector<Update> updates;
            bool read = false;
            if (!collect_updates(d->m_body, d->n_body, s, updates, read,
                    blocks_hold_updates, false)) {
                return false;
            }
            ASR::ttype_t *t = ASRUtils::symbol_type(s);
            int kind = ASRUtils::extract_kind_from_ttype_t(t);
            if (read) {
                Induction c{s, 0, !dead_after(frames, s)};
                if (!ASRUtils::is_integer(*t)) return false;
                for (const Update &u : updates) {
                    int64_t step = 0;
                    if (u.conditional || u.compared
                            || !increment(s, u.assignment->m_value, step)) {
                        return false;
                    }
                    c.step += step;
                }
                if (c.live && !bounds_kept) return false;
                counters.push_back(c);
                continue;
            }
            if (!(ASRUtils::is_integer(*t) || ASRUtils::is_real(*t))
                    || (kind != 4 && kind != 8)
                    || kind > k.lane_reduction_max_kind) {
                return false;
            }
            // A loop unrolled by hand, or by gpu_residue_unroll, updates
            // it once per element it takes, all by the same operation.
            Accumulator acc{s, ASR::reduction_opType::ReduceAdd};
            for (size_t i = 0; i < updates.size(); i++) {
                ASR::reduction_opType op = updates[i].op;
                if ((!updates[i].compared
                        && !accumulation(s, updates[i].assignment->m_value,
                            op))
                        || (i > 0 && op != acc.op)) {
                    return false;
                }
                acc.op = op;
            }
            accumulators.push_back(acc);
        }
        // Lanes take consecutive iterations of the loop, so they read
        // consecutive elements of an array where the loop variable (or an
        // induction) is its first subscript, or adds to its offset with a
        // stride of one. An array read where the variable is only in a
        // later subscript, or is multiplied by a stride that is not a
        // constant (`a(i, j)` over `j`, laid out as `(i - lb1) + n*(j -
        // lb2)`), is read along a column by consecutive iterations of the
        // outer loop instead -- the threads of a launch with one lane per
        // iteration -- which lanes would scatter over 32 columns: such a
        // matrix-vector product ran at two thirds of its speed with lanes.
        // Nor is a loop whose body works out a scalar from its variable
        // times such a stride (`i = t + k*(4*stride)`): the loop over the
        // iterations a thread of a reduction adds up, which the other
        // threads of the launch already interleave with.
        {
            class Scattered : public ASR::BaseWalkVisitor<Scattered> {
            public:
                std::vector<ASR::symbol_t*> vars;
                bool found = false;
                void visit_ArrayItem(const ASR::ArrayItem_t &x) {
                    for (ASR::symbol_t *v : vars) {
                        for (size_t i = 0; i < x.n_args; i++) {
                            ASR::expr_t *sub = x.m_args[i].m_right;
                            if (sub == nullptr || !mentions(sub, v)) continue;
                            if (i > 0 && !(x.m_args[0].m_right
                                    && mentions(x.m_args[0].m_right, v))) {
                                found = true;
                            }
                            if (strided_by(sub, v)) found = true;
                        }
                    }
                    BaseWalkVisitor::visit_ArrayItem(x);
                }
                void visit_BlockCall(const ASR::BlockCall_t &x) {
                    ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(
                        ASRUtils::symbol_get_past_external(x.m_m));
                    for (size_t i = 0; i < block->n_body; i++) {
                        visit_stmt(*block->m_body[i]);
                    }
                }
            } scattered;
            scattered.vars.push_back(v);
            for (const Induction &c : counters) {
                scattered.vars.push_back(c.variable);
            }
            Assignments assigns;
            for (size_t i = 0; i < d->n_body; i++) {
                assigns.visit_stmt(*d->m_body[i]);
            }
            for (ASR::Assignment_t *a : assigns.found) {
                for (ASR::symbol_t *x : scattered.vars) {
                    if (mentions(a->m_value, x) && strided_by(a->m_value, x)) {
                        return false;
                    }
                }
            }
            for (size_t i = 0; i < d->n_body; i++) {
                scattered.visit_stmt(*d->m_body[i]);
            }
            if (scattered.found && !k.scattered_reductions) {
                declined_scattered = true;
                return false;
            }
        }
        return !accumulators.empty();
    }

    // The statements `s = ...` of `body`, and, where `blocks`, of the blocks
    // directly in it (the copies of an unrolled loop body are blocks of
    // their own), in `updates`; `read` is set when anything else names `s`. False when
    // `s` is assigned anywhere else.
    bool collect_updates(ASR::stmt_t **body, size_t n_body,
            ASR::symbol_t *s, std::vector<Update> &updates,
            bool &read, bool blocks, bool conditional) {
        for (size_t i = 0; i < n_body; i++) {
            ASR::stmt_t *x = body[i];
            if (ASR::is_a<ASR::Assignment_t>(*x)) {
                auto *a = ASR::down_cast<ASR::Assignment_t>(x);
                if (ASR::is_a<ASR::Var_t>(*a->m_target)
                        && root_of(a->m_target) == s) {
                    updates.push_back({a, conditional, false,
                        ASR::reduction_opType::ReduceAdd});
                    continue;
                }
            }
            if (ASR::is_a<ASR::If_t>(*x)) {
                auto *f = ASR::down_cast<ASR::If_t>(x);
                // `if (e > s) s = e`: a maximum (a minimum for `<`).
                ASR::reduction_opType op;
                ASR::Assignment_t *a = nullptr;
                if (f->n_orelse == 0 && f->n_body == 1
                        && ASR::is_a<ASR::Assignment_t>(*f->m_body[0])) {
                    a = ASR::down_cast<ASR::Assignment_t>(f->m_body[0]);
                }
                if (a && ASR::is_a<ASR::Var_t>(*a->m_target)
                        && root_of(a->m_target) == s
                        && comparison(s, f->m_test, a->m_value, op)) {
                    updates.push_back({a, conditional, true, op});
                    continue;
                }
                // `if (c) s = s + e`: the accumulation happens in some
                // iterations only, which every lane's partial sum allows.
                if (!mentions(f->m_test, s)) {
                    if (!collect_updates(f->m_body, f->n_body, s, updates,
                                read, blocks, true)
                            || !collect_updates(f->m_orelse, f->n_orelse, s,
                                updates, read, blocks, true)) {
                        return false;
                    }
                    continue;
                }
            }
            if (blocks && ASR::is_a<ASR::BlockCall_t>(*x)) {
                ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(
                    ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::BlockCall_t>(x)->m_m));
                if (!collect_updates(block->m_body, block->n_body, s,
                        updates, read, blocks, conditional)) {
                    return false;
                }
                continue;
            }
            Effects inner(k);
            inner.visit_stmt(*x);
            if (inner.scalars.count(s)) return false;
            if (mentions(*x, s)) read = true;
        }
        return true;
    }

    // The constant `c` of `value`, the value assigned to the integer `s`,
    // when it is s + c or c + s.
    bool increment(ASR::symbol_t *s, ASR::expr_t *value, int64_t &c) {
        if (!ASR::is_a<ASR::IntegerBinOp_t>(*value)) return false;
        auto *x = ASR::down_cast<ASR::IntegerBinOp_t>(value);
        if (x->m_op != ASR::binopType::Add) return false;
        auto is_s = [&](ASR::expr_t *e) {
            return ASR::is_a<ASR::Var_t>(*e) && root_of(e) == s;
        };
        auto constant = [&](ASR::expr_t *e) {
            return ASRUtils::extract_value(ASRUtils::expr_value(e), c);
        };
        return (is_s(x->m_left) && constant(x->m_right))
            || (is_s(x->m_right) && constant(x->m_left));
    }

    // The identity of a reduction of values of type `t`.
    ASR::expr_t* identity(ASR::reduction_opType op, ASR::ttype_t *t,
            const Location &loc) {
        ASRUtils::ASRBuilder b(al, loc);
        int kind = ASRUtils::extract_kind_from_ttype_t(t);
        if (ASRUtils::is_real(*t)) {
            double big = kind == 4 ? std::numeric_limits<float>::max()
                : std::numeric_limits<double>::max();
            double value = op == ASR::reduction_opType::ReduceAdd ? 0
                : op == ASR::reduction_opType::ReduceMul ? 1
                : op == ASR::reduction_opType::ReduceMIN ? big : -big;
            return ASRUtils::EXPR(ASR::make_RealConstant_t(al, loc, value,
                ASRUtils::duplicate_type(al, t)));
        }
        int64_t big = kind == 4 ? std::numeric_limits<int32_t>::max()
            : std::numeric_limits<int64_t>::max();
        int64_t value = op == ASR::reduction_opType::ReduceAdd ? 0
            : op == ASR::reduction_opType::ReduceMul ? 1
            : op == ASR::reduction_opType::ReduceMIN ? big : -big - 1;
        return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, value,
            ASRUtils::duplicate_type(al, t), ASR::integerbozType::Decimal));
    }

    // The scope of the blocks the statements of `body` run (outside any
    // block of their own), which GpuDuplicator copies them into: nullptr
    // when they run none. `copyable` is cleared when they are in more than
    // one scope, or GpuDuplicator cannot copy one of them or of the blocks
    // inside them (a local that is an array or has an initial value).
    SymbolTable* block_scope(ASR::stmt_t **body, size_t n_body,
            bool &copyable) {
        class Blocks : public ASR::BaseWalkVisitor<Blocks> {
        public:
            SymbolTable *scope = nullptr;
            bool copyable = true;
            int depth = 0;
            void visit_BlockCall(const ASR::BlockCall_t &x) {
                ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_m);
                if (!ASR::is_a<ASR::Block_t>(*s)) {
                    copyable = false;
                    return;
                }
                ASR::Block_t *b = ASR::down_cast<ASR::Block_t>(s);
                if (depth == 0) {
                    SymbolTable *parent = b->m_symtab->parent;
                    if (scope && scope != parent) copyable = false;
                    scope = parent;
                }
                for (auto &item : b->m_symtab->get_scope()) {
                    ASR::symbol_t *v = item.second;
                    if (ASR::is_a<ASR::Block_t>(*v)) continue;
                    if (!ASR::is_a<ASR::Variable_t>(*v)) {
                        copyable = false;
                        continue;
                    }
                    ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(v);
                    if (var->m_symbolic_value || var->m_value
                            || ASRUtils::is_array(var->m_type)) {
                        copyable = false;
                    }
                }
                depth++;
                for (size_t i = 0; i < b->n_body; i++) {
                    visit_stmt(*b->m_body[i]);
                }
                depth--;
            }
        } blocks;
        for (size_t i = 0; i < n_body; i++) blocks.visit_stmt(*body[i]);
        copyable = blocks.copyable;
        return blocks.scope;
    }

    // Lane j accumulates iterations j, j + lanes, ... from the identity (the
    // first lane from what the accumulator held before the loop), and the
    // lanes then combine what they hold, which every lane gets. An
    // induction starts at what it holds in iteration j, and moves on by
    // the lanes' iterations in each; after the loop it, and the variable,
    // hold what the loop as it was leaves in them, worked out from the
    // bounds before the loop.
    //
    // A launch with one lane per iteration runs the loop as it was instead,
    // whose start and step the device compiler then knows (it unrolls the
    // loop), in the other branch of a test of the lanes.
    // The sum a loop over groups takes of a lane-shared inner sum scaled by
    // a per-group factor (`total = total + a(g)*inner_sum + b(g)`, the
    // groups of a quantized matrix-vector product) is linear in the inner
    // sum, so the lanes need not combine each group's sum: each lane adds
    // the factor times what it holds of it to a sum of its own, the term
    // that does not depend on the inner sum is added by the first lane
    // alone, and the lanes combine their sums once after the outer loop.
    struct Deferral {
        // The inner reduction loop that is left without its combination.
        ASR::stmt_t *inner;
        // The assignments after it that work out the term from its sum.
        std::vector<ASR::Assignment_t*> chain;
        // The update of the outer accumulator, `total = total + term`.
        ASR::Assignment_t *update;
        ASR::symbol_t *total;
        // The inner sum and the scalars worked out from it.
        std::set<ASR::symbol_t*> derived;
    };
    std::map<ASR::stmt_t*, Deferral> deferrals;
    std::set<ASR::stmt_t*> deferred_inner;

    // Whether `e` is linear in the scalars of `derived`, each named once
    // on one side: the other side of a sum is a term the first lane adds
    // alone (when `commit`, it is rewritten so).
    bool linear_in(ASR::expr_t *e, const std::set<ASR::symbol_t*> &derived,
            bool commit) {
        if (ASR::is_a<ASR::Var_t>(*e)) return derived.count(root_of(e)) > 0;
        if (!ASR::is_a<ASR::RealBinOp_t>(*e)) return false;
        auto *x = ASR::down_cast<ASR::RealBinOp_t>(e);
        bool l = mentions_any(x->m_left, derived);
        bool r = mentions_any(x->m_right, derived);
        if (l == r) return false;
        switch (x->m_op) {
            case ASR::binopType::Add:
            case ASR::binopType::Sub:
            case ASR::binopType::Mul:
                break;
            case ASR::binopType::Div:
                if (!l) return false;
                break;
            default:
                return false;
        }
        if (!linear_in(l ? x->m_left : x->m_right, derived, commit)) {
            return false;
        }
        if (commit && (x->m_op == ASR::binopType::Add
                || x->m_op == ASR::binopType::Sub)) {
            ASR::expr_t *&other = l ? x->m_right : x->m_left;
            const Location &loc = other->base.loc;
            ASRUtils::ASRBuilder b(al, loc);
            ASR::ttype_t *t = ASRUtils::expr_type(other);
            other = ASRUtils::EXPR(ASR::make_IfExp_t(al, loc,
                b.Eq(lane_index(loc), b.i32(0)), other, b.f_t(0.0, t), t,
                nullptr));
        }
        return true;
    }

    void plan_deferral(ASR::DoLoop_t *g,
            const std::vector<Accumulator> &outer,
            std::vector<Frame> &frames) {
        if (outer.size() != 1 || outer[0].op != ASR::reduction_opType::ReduceAdd) {
            return;
        }
        ASR::symbol_t *total = outer[0].variable;
        for (size_t i = 0; i < g->n_body; i++) {
            auto r = reductions.find(g->m_body[i]);
            if (r == reductions.end()) continue;
            if (r->second.size() != 1
                    || r->second[0].op != ASR::reduction_opType::ReduceAdd) {
                return;
            }
            ASR::symbol_t *inner_sum = r->second[0].variable;
            if (inner_sum == total || !dead_after(frames, inner_sum)) return;
            Deferral d;
            d.inner = g->m_body[i];
            d.total = total;
            d.update = nullptr;
            d.derived.insert(inner_sum);
            for (size_t j = i + 1; j < g->n_body; j++) {
                if (!ASR::is_a<ASR::Assignment_t>(*g->m_body[j])) return;
                auto *a = ASR::down_cast<ASR::Assignment_t>(g->m_body[j]);
                if (!ASR::is_a<ASR::Var_t>(*a->m_target)) return;
                ASR::symbol_t *v = root_of(a->m_target);
                if (v == total) {
                    if (j + 1 != g->n_body
                            || !ASR::is_a<ASR::RealBinOp_t>(*a->m_value)) {
                        return;
                    }
                    auto *u = ASR::down_cast<ASR::RealBinOp_t>(a->m_value);
                    if (u->m_op != ASR::binopType::Add) return;
                    auto is_total = [&](ASR::expr_t *e) {
                        return ASR::is_a<ASR::Var_t>(*e)
                            && root_of(e) == total;
                    };
                    ASR::expr_t *term = is_total(u->m_left) ? u->m_right
                        : is_total(u->m_right) ? u->m_left : nullptr;
                    if (term == nullptr || mentions(term, total)
                            || !linear_in(term, d.derived, false)) {
                        return;
                    }
                    d.update = a;
                    continue;
                }
                if (!k.private_scalar(v) || !dead_after(frames, v)
                        || !linear_in(a->m_value, d.derived, false)) {
                    return;
                }
                d.derived.insert(v);
                d.chain.push_back(a);
            }
            if (d.update == nullptr) return;
            // Nothing else of the loop names what is worked out from the
            // inner sum.
            for (size_t q = 0; q < i; q++) {
                for (ASR::symbol_t *v : d.derived) {
                    if (v != inner_sum && mentions(*g->m_body[q], v)) return;
                }
            }
            deferrals[&g->base] = d;
            deferred_inner.insert(d.inner);
            return;
        }
    }

    // Whether `x` is the test `lanes == n`, as distribute_reduction makes it.
    bool lanes_test(ASR::expr_t *x, int64_t &n) {
        if (!ASR::is_a<ASR::IntegerCompare_t>(*x)) return false;
        auto *c = ASR::down_cast<ASR::IntegerCompare_t>(x);
        return c->m_op == ASR::cmpopType::Eq
            && ASR::is_a<ASR::Var_t>(*c->m_left)
            && root_of(c->m_left) == k.lanes
            && ASRUtils::extract_value(ASRUtils::expr_value(c->m_right), n);
    }

    // The number of lanes the tests of distribute_reduction in a statement
    // name.
    class ExactLanes : public ASR::BaseWalkVisitor<ExactLanes> {
    public:
        LaneRewriter &owner;
        int64_t lanes = 0;
        explicit ExactLanes(LaneRewriter &o) : owner(o) {}
        void visit_If(const ASR::If_t &x) {
            int64_t n = 0;
            if (x.m_test && owner.lanes_test(x.m_test, n)) lanes = n;
            BaseWalkVisitor::visit_If(x);
        }
        void visit_BlockCall(const ASR::BlockCall_t &x) {
            ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(
                ASRUtils::symbol_get_past_external(x.m_m));
            for (size_t i = 0; i < block->n_body; i++) {
                visit_stmt(*block->m_body[i]);
            }
        }
    };

    // Replaces each `if (lanes == n) once else loop` of a list, and of the
    // lists inside it, by `once` or by `loop`.
    void choose_exact(ASR::stmt_t **&body, size_t &n_body, int64_t n,
            bool once) {
        Vec<ASR::stmt_t*> out;
        out.reserve(al, n_body);
        for (size_t i = 0; i < n_body; i++) {
            ASR::stmt_t *s = body[i];
            if (ASR::is_a<ASR::If_t>(*s)) {
                auto *x = ASR::down_cast<ASR::If_t>(s);
                int64_t m = 0;
                if (x->m_test && lanes_test(x->m_test, m) && m == n) {
                    ASR::stmt_t **list = once ? x->m_body : x->m_orelse;
                    size_t count = once ? x->n_body : x->n_orelse;
                    for (size_t j = 0; j < count; j++) {
                        out.push_back(al, list[j]);
                    }
                    continue;
                }
                choose_exact(x->m_body, x->n_body, n, once);
                choose_exact(x->m_orelse, x->n_orelse, n, once);
            } else if (ASR::is_a<ASR::DoLoop_t>(*s)) {
                auto *x = ASR::down_cast<ASR::DoLoop_t>(s);
                choose_exact(x->m_body, x->n_body, n, once);
            } else if (ASR::is_a<ASR::WhileLoop_t>(*s)) {
                auto *x = ASR::down_cast<ASR::WhileLoop_t>(s);
                choose_exact(x->m_body, x->n_body, n, once);
            } else if (ASR::is_a<ASR::BlockCall_t>(*s)) {
                ASR::Block_t *x = ASR::down_cast<ASR::Block_t>(
                    ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::BlockCall_t>(s)->m_m));
                choose_exact(x->m_body, x->n_body, n, once);
            }
            out.push_back(al, s);
        }
        body = out.p;
        n_body = out.n;
    }

    void emit_deferred(ASR::stmt_t *g, const Deferral &d,
            Vec<ASR::stmt_t*> &out) {
        const Location &loc = g->base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        auto *gloop = ASR::down_cast<ASR::DoLoop_t>(g);
        // The term of the group that does not depend on the sum of the
        // inner loop (`offsets(g)*sums(g)` of `scale(g)*sum + offsets(g)*
        // sums(g)`), when it is read from memory by the loop variable
        // alone: the lanes add up those of their own groups after the loop,
        // instead of the first lane waiting for them in every group.
        ASR::stmt_t *term_loop = nullptr;
        {
            auto *u = ASR::down_cast<ASR::RealBinOp_t>(d.update->m_value);
            auto is_total = [&](ASR::expr_t *e) {
                return ASR::is_a<ASR::Var_t>(*e) && root_of(e) == d.total;
            };
            ASR::expr_t *x = is_total(u->m_left) ? u->m_right : u->m_left;
            if (ASR::is_a<ASR::Var_t>(*x) && d.chain.size() > 0) {
                ASR::symbol_t *m = root_of(x);
                ASR::Assignment_t *def = nullptr;
                for (ASR::Assignment_t *a : d.chain) {
                    if (root_of(a->m_target) == m) def = a;
                }
                size_t readers = 0;
                for (size_t i = 0; i < gloop->n_body; i++) {
                    if (mentions(*gloop->m_body[i], m)) readers++;
                }
                Effects assigned(k);
                for (size_t i = 0; i < gloop->n_body; i++) {
                    assigned.visit_stmt(*gloop->m_body[i]);
                }
                if (def && readers == 2 && ASR::is_a<ASR::RealBinOp_t>(
                        *def->m_value) && gloop->m_head.m_v) {
                    auto *e = ASR::down_cast<ASR::RealBinOp_t>(def->m_value);
                    bool l = mentions_any(e->m_left, d.derived);
                    bool r = mentions_any(e->m_right, d.derived);
                    if (l != r && (e->m_op == ASR::binopType::Add
                            || (e->m_op == ASR::binopType::Sub && l))) {
                        ASR::expr_t *other = l ? e->m_right : e->m_left;
                        bool pure = !mentions(other, d.total);
                        ASR::symbol_t *gv = root_of(gloop->m_head.m_v);
                        for (ASR::symbol_t *v : assigned.scalars) {
                            if (v != gv && mentions(other, v)) pure = false;
                        }
                        if (pure) {
                            ASR::ttype_t *vt = ASRUtils::expr_type(
                                gloop->m_head.m_v);
                            ASR::expr_t *v2 = b.Variable(k.fn->m_symtab,
                                k.fn->m_symtab->get_unique_name(
                                    "__gpu_lane_group"),
                                ASRUtils::duplicate_type(al, vt),
                                ASR::intentType::Local);
                            GpuDuplicator dup(al, k.fn->m_symtab);
                            dup.value_map[gv] = v2;
                            ASR::expr_t *q = dup.duplicate_expr(other);
                            ASRUtils::ExprStmtDuplicator copy(al);
                            copy.allow_procedure_calls = true;
                            auto step = [&]() {
                                return gloop->m_head.m_increment
                                    ? copy.duplicate_expr(
                                        gloop->m_head.m_increment)
                                    : b.i_t(1, vt);
                            };
                            ASR::expr_t *lanes = b.i2i_t(lanes_var(loc), vt);
                            ASR::expr_t *lane = b.i2i_t(lane_index(loc), vt);
                            bool negative = e->m_op == ASR::binopType::Sub;
                            ASR::expr_t *sum = negative
                                ? b.Sub(b.Var(d.total), q)
                                : b.Add(b.Var(d.total), q);
                            if (dup.ok) {
                                term_loop = b.DoLoop(v2,
                                    b.Add(copy.duplicate_expr(
                                        gloop->m_head.m_start),
                                        b.Mul(lane, step())),
                                    copy.duplicate_expr(gloop->m_head.m_end),
                                    {b.Assignment(b.Var(d.total), sum)},
                                    b.Mul(step(), lanes));
                                def->m_value = l ? e->m_left : e->m_right;
                            }
                        }
                    }
                }
            }
        }
        for (ASR::Assignment_t *a : d.chain) {
            linear_in(a->m_value, d.derived, true);
        }
        {
            auto *u = ASR::down_cast<ASR::RealBinOp_t>(d.update->m_value);
            auto is_total = [&](ASR::expr_t *e) {
                return ASR::is_a<ASR::Var_t>(*e) && root_of(e) == d.total;
            };
            linear_in(is_total(u->m_left) ? u->m_right : u->m_left,
                d.derived, true);
        }
        ASR::ttype_t *t = ASRUtils::symbol_type(d.total);
        std::vector<ASR::stmt_t*> start, finish;
        start.push_back(b.If(b.NotEq(lane_index(loc), b.i32(0)),
            {b.Assignment(b.Var(d.total),
                identity(ASR::reduction_opType::ReduceAdd, t, loc))}, {}));
        out.push_back(al, b.If(b.Gt(lanes_var(loc), b.i32(1)), start, {}));
        nested_apply(g);
        // The launch with as many lanes as an inner loop has iterations
        // gets a copy of the loop over the groups in which each lane takes
        // its one iteration without the loop around it, ahead of the loop
        // over the groups, so that the compiler of the device code knows
        // the lanes and can issue the loads of several groups together.
        int64_t exact = 0;
        {
            ExactLanes found(*this);
            found.visit_stmt(*g);
            exact = found.lanes;
        }
        if (exact > 1) {
            bool copyable = true;
            auto *loop = ASR::down_cast<ASR::DoLoop_t>(g);
            SymbolTable *scope = block_scope(loop->m_body, loop->n_body,
                copyable);
            ASR::stmt_t *copy = nullptr;
            if (copyable) {
                if (scope) {
                    GpuDuplicator dup(al, scope);
                    copy = dup.duplicate_stmt(g);
                    if (!dup.ok) copy = nullptr;
                } else {
                    ASRUtils::ExprStmtDuplicator dup(al);
                    dup.success = true;
                    copy = dup.duplicate_stmt(g);
                    if (!dup.success) copy = nullptr;
                }
            }
            if (copy) {
                auto *c = ASR::down_cast<ASR::DoLoop_t>(copy);
                choose_exact(c->m_body, c->n_body, exact, true);
                choose_exact(loop->m_body, loop->n_body, exact, false);
                out.push_back(al, b.If(b.Eq(lanes_var(loc),
                    b.i32((int)exact)), {copy}, {g}));
            } else {
                choose_exact(loop->m_body, loop->n_body, exact, false);
                out.push_back(al, g);
            }
        } else {
            out.push_back(al, g);
        }
        if (term_loop) out.push_back(al, term_loop);
        finish.push_back(b.Assignment(b.Var(d.total),
            ASRUtils::EXPR(ASR::make_GpuLaneReduce_t(al, loc,
                b.Var(d.total), ASR::reduction_opType::ReduceAdd, 0,
                ASRUtils::duplicate_type(al, t), nullptr))));
        out.push_back(al, b.If(b.Gt(lanes_var(loc), b.i32(1)), finish, {}));
    }

    void distribute_reduction(ASR::DoLoop_t *d,
            const std::vector<Accumulator> &accumulators,
            Vec<ASR::stmt_t*> &out) {
        const Location &loc = d->base.base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        bool copyable = true;
        SymbolTable *scope = block_scope(d->m_body, d->n_body, copyable);
        LCOMPILERS_ASSERT(copyable);
        ASR::stmt_t *as_it_was = nullptr;
        if (scope) {
            GpuDuplicator dup(al, scope);
            as_it_was = dup.duplicate_stmt(&d->base);
            LCOMPILERS_ASSERT(dup.ok && as_it_was);
        } else {
            ASRUtils::ExprStmtDuplicator dup(al);
            dup.success = true;
            as_it_was = dup.duplicate_stmt(&d->base);
            LCOMPILERS_ASSERT(dup.success && as_it_was);
        }
        auto counters = inductions.find(&d->base);
        std::vector<Induction> none;
        const std::vector<Induction> &ind = counters == inductions.end()
            ? none : counters->second;
        Vec<ASR::stmt_t*> shared;
        shared.reserve(al, accumulators.size() + 3*ind.size() + 2);
        std::vector<ASR::stmt_t*> starts;
        for (const Accumulator &a : accumulators) {
            starts.push_back(b.Assignment(b.Var(a.variable),
                identity(a.op, ASRUtils::symbol_type(a.variable), loc)));
        }
        shared.push_back(al, b.If(b.NotEq(lane_index(loc), b.i32(0)), starts,
            {}));
        ASR::ttype_t *type = ASRUtils::expr_type(d->m_head.m_v);
        ASR::expr_t *step = d->m_head.m_increment ? d->m_head.m_increment
            : b.i_t(1, type);
        // What a variable holds after the loop, set before it: what it
        // holds before, plus `step` for each of the max(0, (end - start +
        // s)/s) iterations of `do v = start, end, s`.
        std::vector<std::pair<ASR::expr_t*, ASR::expr_t*>> finals;
        auto after = [&](ASR::expr_t *from, ASR::ttype_t *t,
                ASR::expr_t *by) {
            ASRUtils::ExprStmtDuplicator dup(al);
            dup.allow_procedure_calls = true;
            ASR::expr_t *trips = b.Max(b.Div(b.Add(b.Sub(
                dup.duplicate_expr(d->m_head.m_end),
                dup.duplicate_expr(d->m_head.m_start)),
                dup.duplicate_expr(step)), dup.duplicate_expr(step)),
                b.i_t(0, type));
            ASR::expr_t *x = b.Variable(k.fn->m_symtab,
                k.fn->m_symtab->get_unique_name("__gpu_lane_after"),
                ASRUtils::duplicate_type(al, t), ASR::intentType::Local);
            shared.push_back(al, b.Assignment(x, b.Add(from,
                b.Mul(b.i2i_t(trips, t), by))));
            return x;
        };
        if (live_variables.count(&d->base)) {
            ASRUtils::ExprStmtDuplicator dup(al);
            dup.allow_procedure_calls = true;
            ASR::expr_t *x = after(dup.duplicate_expr(d->m_head.m_start),
                type, dup.duplicate_expr(step));
            finals.push_back({dup.duplicate_expr(d->m_head.m_v), x});
        }
        // A loop of as many iterations as there are lanes (the 16 words of
        // a group of a quantized matrix-vector product, on 16 lanes) takes
        // one iteration on each lane. Compiled as a loop the iteration waits
        // on the test of its end, where the loop of the next group could
        // issue its loads: the launch with that many lanes gets the body
        // without the loop.
        int64_t exact_lanes = 0;
        constant_trip_count(d, exact_lanes);
        Vec<ASR::stmt_t*> body;
        body.reserve(al, d->n_body + ind.size());
        for (size_t i = 0; i < d->n_body; i++) body.push_back(al, d->m_body[i]);
        for (const Induction &c : ind) {
            ASR::expr_t *j = b.Var(c.variable);
            ASR::ttype_t *t = ASRUtils::symbol_type(c.variable);
            if (c.live) {
                finals.push_back({j, after(j, t, b.i_t(c.step, t))});
            }
            shared.push_back(al, b.Assignment(j, b.Add(j,
                b.Mul(b.i2i_t(lane_index(loc), t), b.i_t(c.step, t)))));
            body.push_back(al, b.Assignment(j, b.Add(j, b.Mul(b.Sub(
                b.i2i_t(lanes_var(loc), t), b.i_t(1, t)), b.i_t(c.step, t)))));
        }
        d->m_body = body.p;
        d->n_body = body.n;
        ASR::expr_t *lane = b.i2i_t(lane_index(loc), type);
        ASR::expr_t *lanes = b.i2i_t(lanes_var(loc), type);
        d->m_head.m_start = b.Add(d->m_head.m_start, b.Mul(lane, step));
        d->m_head.m_increment = b.Mul(step, lanes);
        if (deferred_inner.count(&d->base) && exact_lanes > 1
                && exact_lanes <= k.lanes_count
                && (exact_lanes & (exact_lanes - 1)) == 0) {
            ASR::stmt_t *copy = nullptr;
            if (scope) {
                GpuDuplicator dup(al, scope);
                copy = dup.duplicate_stmt(&d->base);
                if (!dup.ok) copy = nullptr;
            } else {
                ASRUtils::ExprStmtDuplicator dup(al);
                dup.success = true;
                copy = dup.duplicate_stmt(&d->base);
                if (!dup.success) copy = nullptr;
            }
            if (copy) {
                auto *c = ASR::down_cast<ASR::DoLoop_t>(copy);
                std::vector<ASR::stmt_t*> once;
                once.push_back(b.Assignment(c->m_head.m_v, c->m_head.m_start));
                for (size_t i = 0; i < c->n_body; i++) {
                    once.push_back(c->m_body[i]);
                }
                shared.push_back(al, b.If(
                    b.Eq(lanes_var(loc), b.i32((int)exact_lanes)), once,
                    {&d->base}));
            } else {
                shared.push_back(al, &d->base);
            }
        } else {
            shared.push_back(al, &d->base);
        }
        for (const Accumulator &a : accumulators) {
            if (deferred_inner.count(&d->base)) break;
            ASR::ttype_t *t = ASRUtils::symbol_type(a.variable);
            shared.push_back(al, b.Assignment(b.Var(a.variable),
                ASRUtils::EXPR(ASR::make_GpuLaneReduce_t(al, loc,
                    b.Var(a.variable), a.op, 0,
                    ASRUtils::duplicate_type(al, t), nullptr))));
        }
        for (auto &f : finals) {
            shared.push_back(al, b.Assignment(f.first, f.second));
        }
        if (k.shared_only) {
            for (size_t i = 0; i < shared.n; i++) out.push_back(al, shared[i]);
            return;
        }
        Vec<ASR::stmt_t*> alone;
        alone.reserve(al, 1);
        alone.push_back(al, as_it_was);
        out.push_back(al, ASRUtils::STMT(ASR::make_If_t(al, loc, nullptr,
            b.Gt(lanes_var(loc), b.i32(1)), shared.p, shared.n, alone.p,
            alone.n)));
    }

    // Whether every element `items` names of the array of `w` has, at one
    // subscript, the same expression as `w`, which tells the iterations of
    // the loop over `v` apart.
    bool own_slice(ASR::expr_t *w, const std::vector<ASR::expr_t*> &items,
            ASR::symbol_t *v, const std::set<ASR::symbol_t*> &changed) {
        auto *x = ASR::down_cast<ASR::ArrayItem_t>(w);
        std::vector<ASR::symbol_t*> wp = path_of(w);
        for (size_t k = 0; k < x->n_args; k++) {
            ASR::expr_t *sub = x->m_args[k].m_right;
            if (!injective(sub, v, changed)) continue;
            bool all = true;
            for (ASR::expr_t *item : items) {
                if (!overlapping(path_of(item), wp)) continue;
                auto *y = ASR::down_cast<ASR::ArrayItem_t>(item);
                if (y->n_args != x->n_args || !same_expr(y->m_v, x->m_v)
                        || y->m_args[k].m_left || y->m_args[k].m_step
                        || !same_expr(y->m_args[k].m_right, sub)) {
                    all = false;
                    break;
                }
            }
            if (all) return true;
        }
        return false;
    }

    bool distributable(ASR::DoLoop_t *d, std::vector<Frame> &frames) {
        std::set<ASR::expr_t*> sliced;
        if (d->n_orelse > 0 || d->m_head.m_v == nullptr) return false;
        ASR::symbol_t *v = root_of(d->m_head.m_v);
        if (!k.private_scalar(v)) return false;
        if (d->m_head.m_increment) {
            ASR::expr_t *inc = ASRUtils::expr_value(d->m_head.m_increment);
            int64_t step = 0;
            if (inc == nullptr || !ASRUtils::extract_value(inc, step)
                    || step == 0) {
                return false;
            }
        }
        Effects e(k);
        for (size_t i = 0; i < d->n_body; i++) e.visit_stmt(*d->m_body[i]);
        if (e.unknown || e.private_array || e.associates) return false;
        // Only pure calls, and no heap: the only shared effects left are
        // the assignments checked below.
        {
            class Impure : public ASR::BaseWalkVisitor<Impure> {
            public:
                bool found = false;
                void visit_FunctionCall(const ASR::FunctionCall_t &x) {
                    if (!pure_callee(x.m_name)) found = true;
                    BaseWalkVisitor::visit_FunctionCall(x);
                }
                void visit_SubroutineCall(const ASR::SubroutineCall_t &) {
                    found = true;
                }
                void visit_GpuAtomicFetchAdd(
                        const ASR::GpuAtomicFetchAdd_t &) {
                    found = true;
                }
                // The statements of a block (BaseWalkVisitor does not go
                // into them).
                void visit_BlockCall(const ASR::BlockCall_t &x) {
                    ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(
                        ASRUtils::symbol_get_past_external(x.m_m));
                    for (size_t i = 0; i < block->n_body; i++) {
                        visit_stmt(*block->m_body[i]);
                    }
                }
            } impure;
            for (size_t i = 0; i < d->n_body; i++) {
                impure.visit_stmt(*d->m_body[i]);
            }
            if (impure.found) return false;
        }
        if (e.returns && e.statuses.empty()) return false;
        LeavesLoop leaves;
        for (size_t i = 0; i < d->n_body; i++) {
            leaves.visit_stmt(*d->m_body[i]);
        }
        if (leaves.found) return false;
        for (size_t i = 0; i < d->n_body; i++) {
            if (writes_shared_temporary(k, *d->m_body[i])) {
                failed = true;
                return false;
            }
        }

        // The scalars the loop changes, its variable included.
        std::set<ASR::symbol_t*> changed = e.scalars;
        changed.insert(v);

        // Every shared array is assigned at one subscript list, which tells
        // the iterations apart; for a `do concurrent` of the iteration the
        // programmer says that the iterations do not depend on each other,
        // as the standard requires of them.
        const bool asserted = concurrent_loops.count(&d->base) > 0;
        Assignments assigns;
        for (size_t i = 0; i < d->n_body; i++) {
            assigns.visit_stmt(*d->m_body[i]);
        }
        std::vector<ASR::expr_t*> written;
        for (ASR::Assignment_t *a : assigns.found) {
            ASR::symbol_t *r = root_of(a->m_target);
            if (ASR::is_a<ASR::Var_t>(*a->m_target)
                    && k.private_scalar(r)) {
                continue;
            }
            if (!ASR::is_a<ASR::ArrayItem_t>(*a->m_target)
                    || !k.shared(r)) {
                return false;
            }
            ASR::ArrayItem_t *item = ASR::down_cast<ASR::ArrayItem_t>(
                a->m_target);
            bool tells_apart = false;
            for (size_t i = 0; i < item->n_args; i++) {
                if (item->m_args[i].m_left || item->m_args[i].m_step) {
                    return false;
                }
                tells_apart |= injective(item->m_args[i].m_right, v,
                    changed);
            }
            // A `do concurrent` says so itself, but not of an array of the
            // iteration that the block shares (KernelInfo::block_arrays): a
            // scratch array each of its iterations defines for itself,
            // which the standard allows, is private memory, which gives
            // every iteration its own, and the block has one.
            if (asserted && !k.block_arrays.count(r)) continue;
            if (!tells_apart) return false;
            bool same = false;
            for (ASR::expr_t *w : written) {
                if (overlapping(path_of(w), path_of(a->m_target))) {
                    if (!same_expr(w, a->m_target)) {
                        sliced.insert(w);
                    }
                    same = true;
                }
            }
            if (!same) written.push_back(a->m_target);
        }
        // Each of them is read at that subscript list only, or, for an
        // array assigned at several (`x(d, i)` and `x(d, 7)`), named only
        // at subscript lists that all have the same subscript, which tells
        // the iterations apart, in one place: each iteration has a slice
        // of its own.
        Designators names;
        names.visit_expr(*d->m_head.m_v);
        for (size_t i = 0; i < d->n_body; i++) names.visit_stmt(*d->m_body[i]);
        for (ASR::expr_t *w : written) {
            std::vector<ASR::symbol_t*> wp = path_of(w);
            if (sliced.count(w)) {
                if (!own_slice(w, names.items, v, changed)) return false;
                for (ASR::expr_t *whole : names.wholes) {
                    if (overlapping(path_of(whole), wp)) return false;
                }
                continue;
            }
            for (ASR::expr_t *item : names.items) {
                if (overlapping(path_of(item), wp) && !same_expr(item, w)
                        && !disjoint(item, w, changed)) {
                    return false;
                }
            }
            for (ASR::expr_t *whole : names.wholes) {
                if (overlapping(path_of(whole), wp)) return false;
            }
        }
        // Every scalar it changes is its own in every iteration, and dead
        // after it.
        for (ASR::symbol_t *s : changed) {
            if (k.stop_scalars.count(s)) continue;
            if (!dead_after(frames, s)) return false;
            if (s == v) continue;
            // Or assigned before it is read by every loop inside that
            // names it, which may run no iteration (the `local` of a
            // `do concurrent` over two indices, in its inner loop).
            if (first_access(d->m_body, 0, d->n_body, s) == Access::Read) {
                return false;
            }
        }
        return true;
    }
};

// Reads the elements of kernel arguments that the iteration of a kernel
// with arrays the block shares only reads at the start of the statement
// list they are read in, as soon as the scalars of their subscripts have
// their values (EarlyLoads::run): the gates of a gated delta rule, read
// once the phases before them and their barriers are done, or the
// elements of a phase of one iteration per lane, wait for memory there,
// where the loads of the first phase could have hidden them. An element is
// read early when
//
// * its array is an argument of the kernel that nothing before it in the
//   kernel assigns (an assignment, a procedure's dummy that is not
//   `intent(in)`, an atomic), nor the loop it is read in, and no pointer is
//   associated in the kernel: the lanes that may assign it only do so
//   after they read it (the state of a recurrence, which each lane reads
//   and then writes);
// * it is read in a statement of the list, in a block it runs, or in the
//   body of a loop the lanes share out (distribute) of at most as many
//   iterations as the block has lanes, which each lane runs once at most:
//   it is read by the lanes that have an iteration (`lane < trips`), with
//   the loop's variable worked out from the lane;
// * every scalar its subscripts name, but that variable, is a scalar
//   argument, or is assigned only by statements of the list before it, and
//   nothing between where it is read and where it is read early returns:
//   the element is the one the statement reads, and reading it cannot fail
//   where the statement would not.
//
// The value is held in a new scalar, at most early_loads_max of them.
class EarlyLoads {
public:
    EarlyLoads(Allocator &al_, const KernelInfo &k_,
        const std::map<ASR::stmt_t*, LaneRewriter::LaneLoop> &loops_,
        const std::set<ASR::stmt_t*> &held_, int64_t lanes_)
        : al(al_), k(k_), loops(loops_), held(held_), lanes(lanes_) {}

    void run(ASR::Function_t &kernel) {
        // Whether the kernel associates a pointer.
        bool associates = false;
        for (size_t i = 0; i < kernel.n_body; i++) {
            Effects e(k);
            e.visit_stmt(*kernel.m_body[i]);
            associates |= e.associates;
        }
        if (associates) return;
        class Assigned : public ASR::BaseWalkVisitor<Assigned> {
        public:
            std::map<ASR::symbol_t*, int> count;
            std::set<ASR::symbol_t*> blocks;
            void visit_Assignment(const ASR::Assignment_t &x) {
                if (ASR::is_a<ASR::Var_t>(*x.m_target)) count[root_of(x.m_target)]++;
                BaseWalkVisitor::visit_Assignment(x);
            }
            void visit_DoLoop(const ASR::DoLoop_t &x) {
                if (x.m_head.m_v) count[root_of(x.m_head.m_v)]++;
                BaseWalkVisitor::visit_DoLoop(x);
            }
            void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
                for (size_t h = 0; h < x.n_head; h++) {
                    if (x.m_head[h].m_v) count[root_of(x.m_head[h].m_v)]++;
                }
                BaseWalkVisitor::visit_DoConcurrentLoop(x);
            }
            void call_args(ASR::call_arg_t *args, size_t n) {
                for (size_t i = 0; i < n; i++) {
                    if (args[i].m_value && ASR::is_a<ASR::Var_t>(*args[i].m_value)) {
                        count[root_of(args[i].m_value)] += 2;
                    }
                }
            }
            void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
                call_args(x.m_args, x.n_args);
                BaseWalkVisitor::visit_SubroutineCall(x);
            }
            void visit_BlockCall(const ASR::BlockCall_t &x) {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
                if (!blocks.insert(b).second) return;
                ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(b);
                for (size_t i = 0; i < block->n_body; i++) visit_stmt(*block->m_body[i]);
            }
        } assigned;
        for (size_t i = 0; i < kernel.n_body; i++) assigned.visit_stmt(*kernel.m_body[i]);
        assignments = assigned.count;
        fn = &kernel;
        list(kernel.m_body, kernel.n_body, {});
    }

private:
    Allocator &al;
    const KernelInfo &k;
    const std::map<ASR::stmt_t*, LaneRewriter::LaneLoop> &loops;
    const std::set<ASR::stmt_t*> &held;
    int64_t lanes;
    ASR::Function_t *fn = nullptr;
    // The arrays the statements before the list being looked at assign.
    std::set<ASR::symbol_t*> prior;
    std::map<ASR::symbol_t*, int> assignments;
    int hoisted = 0;
    static constexpr int early_loads_max = 16;
    // The scalars that keep their value in the list being looked at.
    std::set<ASR::symbol_t*> stable;

    // An element read early: where in the list, and of which loop of one
    // iteration per lane (nullptr for none).
    struct Candidate {
        ASR::expr_t **at;
        size_t position;
        ASR::stmt_t *loop;
    };

    bool read_only(ASR::expr_t *e) const {
        if (!ASR::is_a<ASR::ArrayItem_t>(*e)) return false;
        auto *item = ASR::down_cast<ASR::ArrayItem_t>(e);
        if (!ASR::is_a<ASR::Var_t>(*item->m_v)) return false;
        ASR::symbol_t *a = root_of(item->m_v);
        ASR::ttype_t *t = ASRUtils::expr_type(e);
        return a && k.params.count(a) && !k.workspaces.count(a)
            && ASRUtils::is_array(ASRUtils::symbol_type(a))
            && !ASRUtils::is_pointer(ASRUtils::symbol_type(a))
            && (ASRUtils::is_integer(*t) || ASRUtils::is_real(*t));
    }

    // The elements an expression reads that may be read early, with the
    // places that hold them.
    void elements(ASR::expr_t *&e, std::vector<ASR::expr_t**> &found) {
        class Walk : public ASR::BaseExprReplacer<Walk> {
        public:
            EarlyLoads &o;
            std::vector<ASR::expr_t**> &found;
            Walk(EarlyLoads &o_, std::vector<ASR::expr_t**> &f) : o(o_), found(f) {}
            void replace_ArrayItem(ASR::ArrayItem_t *x) {
                if (o.read_only(&x->base)) {
                    found.push_back(current_expr);
                    return;
                }
                BaseExprReplacer::replace_ArrayItem(x);
            }
        } walk(*this, found);
        walk.current_expr = &e;
        walk.replace_expr(e);
    }

    // The values the assignments of a statement list (and the blocks it
    // runs) read.
    void assignments_of(ASR::stmt_t **body, size_t n,
            std::vector<ASR::expr_t**> &found) {
        for (size_t i = 0; i < n; i++) {
            if (ASR::is_a<ASR::Assignment_t>(*body[i])) {
                elements(ASR::down_cast<ASR::Assignment_t>(body[i])->m_value,
                    found);
            } else if (ASR::is_a<ASR::BlockCall_t>(*body[i])) {
                ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(
                    ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::BlockCall_t>(body[i])->m_m));
                assignments_of(block->m_body, block->n_body, found);
            }
        }
    }

    // Whether a statement may leave the iteration.
    static bool leaves(const ASR::stmt_t &s) {
        class Walk : public ASR::BaseWalkVisitor<Walk> {
        public:
            bool found = false;
            void visit_Return(const ASR::Return_t &) { found = true; }
            void visit_If(const ASR::If_t &x) {
                if (gpu_stop_record_status(x.base)) found = true;
                BaseWalkVisitor::visit_If(x);
            }
            void visit_BlockCall(const ASR::BlockCall_t &x) {
                ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(
                    ASRUtils::symbol_get_past_external(x.m_m));
                for (size_t i = 0; i < block->n_body; i++) visit_stmt(*block->m_body[i]);
            }
        } walk;
        walk.visit_stmt(s);
        return walk.found;
    }

    // The scalars assigned only by the statements of `body` before `p`.
    std::set<ASR::symbol_t*> assigned_before(ASR::stmt_t **body, size_t p) {
        std::map<ASR::symbol_t*, int> count;
        for (size_t q = 0; q < p; q++) {
            if (!ASR::is_a<ASR::Assignment_t>(*body[q])) continue;
            auto *a = ASR::down_cast<ASR::Assignment_t>(body[q]);
            if (ASR::is_a<ASR::Var_t>(*a->m_target)) count[root_of(a->m_target)]++;
        }
        std::set<ASR::symbol_t*> found;
        for (auto &c : count) {
            auto n = assignments.find(c.first);
            if (n != assignments.end() && n->second == c.second) {
                found.insert(c.first);
            }
        }
        return found;
    }

    // `stable`: the scalars that keep their value in the whole list.
    void list(ASR::stmt_t **&body, size_t &n_body,
            const std::set<ASR::symbol_t*> &stable_) {
        stable = stable_;
        std::set<ASR::symbol_t*> prior_ = prior;
        // The blocks of the list are lists of their own.
        for (size_t i = 0; i < n_body; i++) {
            if (ASR::is_a<ASR::BlockCall_t>(*body[i])) {
                ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(
                    ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::BlockCall_t>(body[i])->m_m));
                std::set<ASR::symbol_t*> inner = assigned_before(body, i);
                inner.insert(stable_.begin(), stable_.end());
                prior = prior_;
                for (size_t q = 0; q < i; q++) {
                    SharedAccess access(k);
                    access.visit_stmt(*body[q]);
                    prior.insert(access.writes.begin(), access.writes.end());
                }
                list(block->m_body, block->n_body, inner);
            }
        }
        stable = stable_;
        prior = prior_;
        lane_scalars_first(body, n_body);
        std::vector<std::vector<ASR::stmt_t*>> early(n_body + 1);
        bool any = false;
        // The arrays assigned before the statement looked at.
        std::set<ASR::symbol_t*> assigned = prior;
        for (size_t p = 0; p < n_body && hoisted < early_loads_max; p++) {
            ASR::stmt_t *s = body[p];
            if (p > 0) {
                SharedAccess access(k);
                access.visit_stmt(*body[p - 1]);
                assigned.insert(access.writes.begin(), access.writes.end());
            }
            std::vector<ASR::expr_t**> found;
            ASR::stmt_t *loop = nullptr;
            auto l = loops.find(s);
            if (ASR::is_a<ASR::Assignment_t>(*s)
                    || ASR::is_a<ASR::BlockCall_t>(*s)) {
                assignments_of(&body[p], 1, found);
            } else if (held.count(s)) {
                size_t from = 0;
                ASR::stmt_t *x = read_held_early(body, p, from, assigned);
                if (x) {
                    early[from].push_back(x);
                    any = true;
                }
                continue;
            } else if (l != loops.end() && l->second.trips <= lanes
                    && l->second.trips > 0) {
                auto *d = ASR::down_cast<ASR::DoLoop_t>(s);
                assignments_of(d->m_body, d->n_body, found);
                loop = s;
            }
            SharedAccess own(k);
            own.visit_stmt(*s);
            for (ASR::expr_t **at : found) {
                if (hoisted >= early_loads_max) break;
                // Nothing before it assigns its array, nor the loop it is
                // read in (where an iteration may assign the element before
                // it reads it).
                ASR::symbol_t *array = root_of(*at);
                if (assigned.count(array)
                        || (loop && own.writes.count(array))) {
                    continue;
                }
                size_t from = 0;
                if (!available(body, p, *at, loop, from)) continue;
                // Something to read it ahead of: a loop or a barrier.
                bool ahead = false;
                for (size_t q = from; q < p; q++) {
                    if (!ASR::is_a<ASR::Assignment_t>(*body[q])) ahead = true;
                }
                if (!ahead) continue;
                early[from].push_back(read_early(at, loop));
                any = true;
            }
        }
        if (!any) return;
        Vec<ASR::stmt_t*> out;
        out.reserve(al, n_body + hoisted);
        for (size_t i = 0; i <= n_body; i++) {
            for (ASR::stmt_t *x : early[i]) out.push_back(al, x);
            if (i < n_body) out.push_back(al, body[i]);
        }
        body = out.p;
        n_body = out.n;
    }

    // Moves the assignments of the list to scalars assigned nowhere else,
    // of values worked out from the lane alone (which lane of the block
    // takes which column of a phase, plan_parts), to the start of the
    // list: the elements a lane reads at those subscripts are then known
    // there.
    void lane_scalars_first(ASR::stmt_t **body, size_t n_body) {
        class Lane : public ASR::BaseWalkVisitor<Lane> {
        public:
            const std::set<ASR::symbol_t*> &moved;
            const KernelInfo &k;
            bool lane = false, other = false;
            Lane(const std::set<ASR::symbol_t*> &m, const KernelInfo &k_)
                : moved(m), k(k_) {}
            void visit_GpuLaneIndex(const ASR::GpuLaneIndex_t &) { lane = true; }
            void visit_Var(const ASR::Var_t &x) {
                ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_v);
                if (moved.count(s) || (k.params.count(s)
                        && !ASRUtils::is_array(ASRUtils::symbol_type(s)))) {
                    lane |= moved.count(s) > 0;
                    return;
                }
                other = true;
            }
            void visit_ArrayItem(const ASR::ArrayItem_t &) { other = true; }
            void visit_FunctionCall(const ASR::FunctionCall_t &) { other = true; }
        };
        std::set<ASR::symbol_t*> moved;
        std::vector<ASR::stmt_t*> first, rest;
        for (size_t q = 0; q < n_body; q++) {
            ASR::stmt_t *s = body[q];
            if (ASR::is_a<ASR::Assignment_t>(*s)) {
                auto *a = ASR::down_cast<ASR::Assignment_t>(s);
                ASR::symbol_t *x = ASR::is_a<ASR::Var_t>(*a->m_target)
                    ? root_of(a->m_target) : nullptr;
                auto n = x ? assignments.find(x) : assignments.end();
                Lane lane(moved, k);
                lane.visit_expr(*a->m_value);
                if (x && k.private_scalar(x) && n != assignments.end()
                        && n->second == 1 && lane.lane && !lane.other) {
                    moved.insert(x);
                    first.push_back(s);
                    continue;
                }
            }
            rest.push_back(s);
        }
        if (first.empty()) return;
        size_t q = 0;
        for (ASR::stmt_t *s : first) body[q++] = s;
        for (ASR::stmt_t *s : rest) body[q++] = s;
    }

    // The loop that reads the elements a lane keeps in registers, ahead of
    // the counted loop body[p] that reads them (held_loops): a copy of its
    // first two statements, when what their subscripts read is known
    // earlier and nothing in between assigns the array or names the loop's
    // variables. The counted loop then reads them no more.
    ASR::stmt_t* read_held_early(ASR::stmt_t **body, size_t p,
            size_t &from, const std::set<ASR::symbol_t*> &assigned) {
        auto *l = ASR::down_cast<ASR::DoLoop_t>(body[p]);
        if (l->n_body < 2 || hoisted >= early_loads_max) return nullptr;
        auto *v = ASR::down_cast<ASR::Assignment_t>(l->m_body[0]);
        auto *h = ASR::down_cast<ASR::Assignment_t>(l->m_body[1]);
        if (!ASR::is_a<ASR::ArrayItem_t>(*h->m_value)) return nullptr;
        ASR::symbol_t *j = root_of(v->m_target);
        ASR::symbol_t *count = root_of(l->m_head.m_v);
        ASR::symbol_t *array = root_of(h->m_value);
        class Scalars : public ASR::BaseWalkVisitor<Scalars> {
        public:
            std::set<ASR::symbol_t*> found;
            bool other = false;
            void visit_Var(const ASR::Var_t &x) {
                found.insert(ASRUtils::symbol_get_past_external(x.m_v));
            }
            void visit_FunctionCall(const ASR::FunctionCall_t &) { other = true; }
        } scalars;
        scalars.visit_expr(*v->m_value);
        auto *item = ASR::down_cast<ASR::ArrayItem_t>(h->m_value);
        for (size_t i = 0; i < item->n_args; i++) {
            if (item->m_args[i].m_left || item->m_args[i].m_step
                    || item->m_args[i].m_right == nullptr
                    || reads_array_element(item->m_args[i].m_right)) {
                return nullptr;
            }
            scalars.visit_expr(*item->m_args[i].m_right);
        }
        if (scalars.other || assigned.count(array)) return nullptr;
        from = 0;
        for (ASR::symbol_t *x : scalars.found) {
            if (x == j || x == count || x == array) continue;
            if (!ASR::is_a<ASR::Variable_t>(*x)) return nullptr;
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(x);
            if (var->m_storage == ASR::storage_typeType::Parameter
                    || stable.count(x)) {
                continue;
            }
            auto n = assignments.find(x);
            int total = n == assignments.end() ? 0 : n->second;
            if (total == 0) {
                if (k.params.count(x) || !k.local(x)) continue;
                return nullptr;
            }
            int before = 0;
            for (size_t q = 0; q < p; q++) {
                if (!ASR::is_a<ASR::Assignment_t>(*body[q])) continue;
                auto *a = ASR::down_cast<ASR::Assignment_t>(body[q]);
                if (ASR::is_a<ASR::Var_t>(*a->m_target)
                        && root_of(a->m_target) == x) {
                    before++;
                    from = std::max(from, q + 1);
                }
            }
            if (before != total) return nullptr;
        }
        bool ahead = false;
        for (size_t q = from; q < p; q++) {
            SharedAccess access(k);
            access.visit_stmt(*body[q]);
            if (access.writes.count(array) || leaves(*body[q])
                    || mentions(*body[q], j) || mentions(*body[q], count)) {
                return nullptr;
            }
            if (!ASR::is_a<ASR::Assignment_t>(*body[q])) ahead = true;
        }
        if (!ahead) return nullptr;
        hoisted++;
        ASRUtils::ExprStmtDuplicator dup(al);
        dup.allow_procedure_calls = true;
        ASR::do_loop_head_t head = l->m_head;
        head.m_start = dup.duplicate_expr(head.m_start);
        head.m_end = dup.duplicate_expr(head.m_end);
        if (head.m_increment) head.m_increment = dup.duplicate_expr(head.m_increment);
        Vec<ASR::stmt_t*> reads;
        reads.reserve(al, 2);
        reads.push_back(al, dup.duplicate_stmt(l->m_body[0]));
        reads.push_back(al, l->m_body[1]);
        Vec<ASR::stmt_t*> rest;
        rest.reserve(al, l->n_body - 1);
        rest.push_back(al, l->m_body[0]);
        for (size_t i = 2; i < l->n_body; i++) rest.push_back(al, l->m_body[i]);
        l->m_body = rest.p;
        l->n_body = rest.n;
        return ASRUtils::STMT(ASRUtils::make_DoLoop_t_util(al, l->base.base.loc,
            nullptr, head, reads.p, reads.n, nullptr, 0));
    }

    // Whether the element at `at`, read by body[p], may be read early, and
    // the first position it may be read at.
    bool available(ASR::stmt_t **body, size_t p, ASR::expr_t *e,
            ASR::stmt_t *loop, size_t &from) {
        ASR::symbol_t *v = loop
            ? root_of(ASR::down_cast<ASR::DoLoop_t>(loop)->m_head.m_v) : nullptr;
        class Scalars : public ASR::BaseWalkVisitor<Scalars> {
        public:
            std::set<ASR::symbol_t*> found;
            bool other = false;
            void visit_Var(const ASR::Var_t &x) {
                found.insert(ASRUtils::symbol_get_past_external(x.m_v));
            }
            void visit_ArrayItem(const ASR::ArrayItem_t &x) {
                other = true;
                BaseWalkVisitor::visit_ArrayItem(x);
            }
            void visit_FunctionCall(const ASR::FunctionCall_t &) { other = true; }
        } scalars;
        auto *item = ASR::down_cast<ASR::ArrayItem_t>(e);
        for (size_t i = 0; i < item->n_args; i++) {
            if (item->m_args[i].m_left || item->m_args[i].m_step
                    || item->m_args[i].m_right == nullptr) {
                return false;
            }
            scalars.visit_expr(*item->m_args[i].m_right);
        }
        if (scalars.other) return false;
        from = 0;
        for (ASR::symbol_t *x : scalars.found) {
            if (x == v || x == root_of(item->m_v)) continue;
            if (!ASR::is_a<ASR::Variable_t>(*x)) return false;
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(x);
            if (var->m_storage == ASR::storage_typeType::Parameter) continue;
            if (stable.count(x)) continue;
            auto n = assignments.find(x);
            int total = n == assignments.end() ? 0 : n->second;
            if (total == 0) {
                if (k.params.count(x) || !k.local(x)) continue;
                return false;
            }
            // Assigned only by statements of the list before it.
            int before = 0;
            for (size_t q = 0; q < p; q++) {
                if (ASR::is_a<ASR::Assignment_t>(*body[q])) {
                    auto *a = ASR::down_cast<ASR::Assignment_t>(body[q]);
                    if (ASR::is_a<ASR::Var_t>(*a->m_target)
                            && root_of(a->m_target) == x) {
                        before++;
                        from = std::max(from, q + 1);
                    }
                }
            }
            if (before != total) return false;
        }
        for (size_t q = from; q < p; q++) {
            if (leaves(*body[q])) return false;
        }
        return true;
    }

    // The statement that reads the element at `at` into a new scalar,
    // which then stands in its place.
    ASR::stmt_t* read_early(ASR::expr_t **at, ASR::stmt_t *loop) {
        ASR::expr_t *e = *at;
        const Location &loc = e->base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        ASR::expr_t *x = b.Variable(fn->m_symtab,
            fn->m_symtab->get_unique_name("__gpu_early"),
            ASRUtils::duplicate_type(al, ASRUtils::expr_type(e)),
            ASR::intentType::Local);
        hoisted++;
        *at = x;
        if (loop == nullptr) return b.Assignment(x, e);
        auto *d = ASR::down_cast<ASR::DoLoop_t>(loop);
        const LaneRewriter::LaneLoop &l = loops.at(loop);
        ASR::symbol_t *v = root_of(d->m_head.m_v);
        ASR::ttype_t *type = ASRUtils::expr_type(d->m_head.m_v);
        ASRUtils::ExprStmtDuplicator copy(al);
        copy.allow_procedure_calls = true;
        ASR::expr_t *lane = ASRUtils::EXPR(ASR::make_GpuLaneIndex_t(al, loc,
            ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)), nullptr));
        ASR::expr_t *value = b.Add(copy.duplicate_expr(l.start),
            b.Mul(b.i2i_t(lane, type), copy.duplicate_expr(l.step)));
        GpuDuplicator dup(al, fn->m_symtab);
        dup.value_map[v] = value;
        ASR::expr_t *early = dup.duplicate_expr(e);
        ASR::expr_t *lane2 = ASRUtils::EXPR(ASR::make_GpuLaneIndex_t(al, loc,
            ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)), nullptr));
        return b.If(b.Lt(lane2, b.i32(static_cast<int32_t>(l.trips))),
            {b.Assignment(x, early)}, {});
    }
};

// The site and code of every stop test of the kernel.
class StopScalars : public ASR::BaseWalkVisitor<StopScalars> {
public:
    std::set<ASR::symbol_t*> &found;
    std::set<ASR::symbol_t*> blocks;
    explicit StopScalars(std::set<ASR::symbol_t*> &found_) : found(found_) {}
    void visit_If(const ASR::If_t &x) {
        ASR::symbol_t *site = nullptr, *code = nullptr;
        if (gpu_stopped_check_scalars(x.base, &site, &code)) {
            if (site) found.insert(site);
            if (code) found.insert(code);
        }
        BaseWalkVisitor::visit_If(x);
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!blocks.insert(b).second) return;
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(b);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
};

// Gives `kernel` a last parameter, the lanes of an iteration, as a scalar
// of its layout.
ASR::symbol_t* add_lanes_parameter(Allocator &al, ASR::Function_t &kernel) {
    const Location &loc = kernel.base.base.loc;
    ASR::ttype_t *int4 = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
    ASRUtils::ASRBuilder b(al, loc);
    ASR::expr_t *var = b.Variable(kernel.m_symtab,
        kernel.m_symtab->get_unique_name("__gpu_lanes"), int4,
        ASR::intentType::In);
    ASR::symbol_t *sym = ASR::down_cast<ASR::Var_t>(var)->m_v;
    Vec<ASR::expr_t*> args;
    args.reserve(al, kernel.n_args + 1);
    for (size_t i = 0; i < kernel.n_args; i++) {
        args.push_back(al, kernel.m_args[i]);
    }
    args.push_back(al, var);
    kernel.m_args = args.p;
    kernel.n_args = args.n;
    ASR::FunctionType_t *t = ASRUtils::get_FunctionType(kernel);
    Vec<ASR::ttype_t*> types;
    types.reserve(al, t->n_arg_types + 1);
    for (size_t i = 0; i < t->n_arg_types; i++) {
        types.push_back(al, t->m_arg_types[i]);
    }
    types.push_back(al, int4);
    kernel.m_function_signature = ASRUtils::TYPE(ASR::make_FunctionType_t(al,
        t->base.base.loc, types.p, types.n, t->m_return_var_type, t->m_abi,
        t->m_deftype, t->m_bindc_name, t->m_elemental, t->m_pure,
        t->m_module, t->m_inline, t->m_static, t->m_restrictions,
        t->n_restrictions, t->m_is_restriction, t->m_exec_space));
    ASR::gpu_kernel_layout_t &layout = *kernel.m_gpu;
    // The scalars travel in one buffer, ahead of the workspaces: a kernel
    // that had none gets that buffer now.
    if (layout.n_scalars == 0) {
        for (size_t i = 0; i < layout.n_workspaces; i++) {
            layout.m_workspaces[i].m_buffer_index++;
        }
    }
    Vec<ASR::gpu_kernel_argument_t> scalars;
    scalars.reserve(al, layout.n_scalars + 1);
    for (size_t i = 0; i < layout.n_scalars; i++) {
        scalars.push_back(al, layout.m_scalars[i]);
    }
    ASR::gpu_kernel_argument_t arg;
    arg.loc = loc;
    arg.m_variable = sym;
    arg.m_member = nullptr;
    arg.m_type = int4;
    arg.m_kind = ASR::gpu_argument_kindType::GpuScalar;
    arg.m_argument_index = kernel.n_args - 1;
    arg.m_dimension = 0;
    arg.m_written = false;
    arg.m_noalias = false;
    scalars.push_back(al, arg);
    layout.m_scalars = scalars.p;
    layout.n_scalars = scalars.n;
    layout.m_lanes = sym;
    return sym;
}

// The most bytes of the arrays of an iteration that a kernel keeps in the
// shared memory of a block. A block has 48 KiB of it that the kernel's
// source declares, and the combinations and broadcasts of its lanes take a
// little of that.
constexpr int64_t block_array_bytes_max = 32 * 1024;

// The arrays of the iteration of `kernel` that may live in the shared
// memory of a block whose threads are all lanes of the one iteration: its
// locals, or those of a block it runs outside every loop (a block inside a
// loop has one of each per iteration of that loop), of constant extents,
// of integers, reals or logicals, with no initial value, and named only by
// their elements and their shape (not handed whole to a procedure or a
// pointer, whose memory space they would change). Together they fit
// block_array_bytes_max; share_block_arrays keeps the small ones only
// with a large one.
std::vector<ASR::symbol_t*> block_array_candidates(
        const ASR::Function_t &kernel, const KernelInfo &k) {
    std::vector<SymbolTable*> scopes = {kernel.m_symtab};
    std::function<void(ASR::stmt_t**, size_t)> outside_loops =
            [&](ASR::stmt_t **body, size_t n_body) {
        for (size_t i = 0; i < n_body; i++) {
            if (ASR::is_a<ASR::BlockCall_t>(*body[i])) {
                ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(
                    ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::BlockCall_t>(body[i])->m_m));
                scopes.push_back(block->m_symtab);
                outside_loops(block->m_body, block->n_body);
            } else if (ASR::is_a<ASR::If_t>(*body[i])) {
                auto *x = ASR::down_cast<ASR::If_t>(body[i]);
                outside_loops(x->m_body, x->n_body);
                outside_loops(x->m_orelse, x->n_orelse);
            }
        }
    };
    outside_loops(kernel.m_body, kernel.n_body);
    Designators names;
    for (size_t i = 0; i < kernel.n_body; i++) {
        names.visit_stmt(*kernel.m_body[i]);
    }
    std::set<ASR::symbol_t*> whole;
    for (ASR::expr_t *e : names.wholes) {
        ASR::symbol_t *r = root_of(e);
        if (r) whole.insert(r);
    }
    std::vector<ASR::symbol_t*> found;
    int64_t bytes = 0;
    for (SymbolTable *scope : scopes) {
        for (auto &item : scope->get_scope()) {
            ASR::symbol_t *s = item.second;
            if (!ASR::is_a<ASR::Variable_t>(*s) || k.params.count(s)
                    || k.workspaces.count(s) || whole.count(s)) {
                continue;
            }
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
            if (v->m_intent != ASR::intentType::Local
                    || v->m_storage != ASR::storage_typeType::Default
                    || v->m_symbolic_value || v->m_value
                    || !ASR::is_a<ASR::Array_t>(*v->m_type)) {
                continue;
            }
            ASR::Array_t *a = ASR::down_cast<ASR::Array_t>(v->m_type);
            if (a->m_physical_type
                        != ASR::array_physical_typeType::FixedSizeArray
                    || a->m_memory_space != ASR::memory_spaceType::Thread
                    || !(ASRUtils::is_integer(*a->m_type)
                        || ASRUtils::is_real(*a->m_type)
                        || ASRUtils::is_logical(*a->m_type))) {
                continue;
            }
            int64_t elements = 1;
            bool constant = a->n_dims > 0;
            for (size_t d = 0; d < a->n_dims && constant; d++) {
                int64_t start = 0, length = 0;
                constant = a->m_dims[d].m_start && a->m_dims[d].m_length
                    && ASRUtils::extract_value(ASRUtils::expr_value(
                        a->m_dims[d].m_start), start)
                    && ASRUtils::extract_value(ASRUtils::expr_value(
                        a->m_dims[d].m_length), length)
                    && length > 0;
                elements *= length;
            }
            if (!constant) continue;
            int64_t size = elements
                * ASRUtils::extract_kind_from_ttype_t(a->m_type);
            if (bytes + size > block_array_bytes_max) continue;
            bytes += size;
            found.push_back(s);
        }
    }
    return found;
}

// The arrays of a kernel's iteration placed in the shared memory of the
// block: the iteration runs on all the threads of a block, and those of
// its loops that assign them are shared out (see block_array_candidates).
// Each is tried first; the ones that no shared-out loop assigns are left
// private, and none when the kernel cannot be rewritten with them shared.
// Returns the rewriter that decided with them, or nullptr.
std::unique_ptr<LaneRewriter> share_block_arrays(Allocator &al,
        ASR::Function_t &kernel, KernelInfo &k,
        const GpuDeviceCapabilities &caps) {
    std::unique_ptr<LaneRewriter> rewrite;
    if (caps.lanes_block <= caps.lanes_per_iteration) return rewrite;
    std::vector<ASR::symbol_t*> candidates = block_array_candidates(kernel,
        k);
    std::set<ASR::symbol_t*> trial(candidates.begin(), candidates.end());
    bool shared_only = k.shared_only;
    while (!trial.empty()) {
        k.block_arrays = trial;
        k.block_lanes = caps.lanes_block;
        // The lanes of the one iteration of a block share a sum that reads
        // across the columns of an array as well: no other iteration reads
        // along its rows next to it.
        k.scattered_reductions = true;
        // The iteration never runs on one lane.
        k.shared_only = true;
        std::vector<Frame> frames;
        rewrite.reset(new LaneRewriter(al, k));
        rewrite->decide(kernel.m_body, kernel.n_body, nullptr, frames);
        std::set<ASR::symbol_t*> used;
        // A sum shared out on fewer lanes than a SIMD-group has
        // (lane_limit) is for many iterations per block.
        if (!rewrite->failed && rewrite->distributed
                && rewrite->lane_limit == 0) {
            used = rewrite->distributed_block_arrays();
        }
        // Arrays of fewer elements than a SIMD-group has lanes (the norms
        // of a few heads) only join one that has as many: the loops over
        // them alone do not keep a block's lanes busy.
        bool large = false;
        for (ASR::symbol_t *a : used) {
            large |= ASRUtils::get_fixed_size_of_array(
                ASRUtils::symbol_type(a)) >= caps.lanes_per_iteration;
        }
        if (!large) used.clear();
        if (used == trial) return rewrite;
        rewrite->restore();
        rewrite.reset();
        trial = used;
    }
    k.block_arrays.clear();
    k.block_lanes = 0;
    k.scattered_reductions = false;
    k.shared_only = shared_only;
    return rewrite;
}

// Rewrites `kernel` to run each iteration on lanes, when it has a loop to
// distribute and nothing in the way. `block` tells whether arrays of the
// iteration live in the shared memory of the block, whose threads are then
// the lanes of one iteration at every launch: `lanes` of them.
bool give_lanes(Allocator &al, ASR::Function_t &kernel,
        const GpuDeviceCapabilities &caps, bool &any_size, int &lanes,
        bool &scattered, std::vector<TripBound> &bounds, bool &block) {
    any_size = false;
    scattered = false;
    block = false;
    // A kernel whose warps already combine the accumulators of a reduction
    // (gpu_offload.cpp) runs every thread on its own iterations.
    {
        class WarpReduce : public ASR::BaseWalkVisitor<WarpReduce> {
        public:
            bool found = false;
            void visit_GpuLaneReduce(const ASR::GpuLaneReduce_t &) {
                found = true;
            }
        } warp_reduce;
        for (size_t i = 0; i < kernel.n_body; i++) {
            warp_reduce.visit_stmt(*kernel.m_body[i]);
        }
        if (warp_reduce.found) return false;
    }
    KernelInfo k;
    k.fn = &kernel;
    k.lane_reductions = caps.lane_reductions;
    k.lane_reduction_max_kind = caps.lane_reduction_max_kind;
    k.independent_loops = caps.lane_independent_loops;
    k.inner_reductions_first = caps.lane_inner_reductions_first;
    k.lanes_count = caps.lanes_per_iteration;
    k.shared_only = gpu_reads_wide(kernel, caps);
    // Where lanes share out only reductions (NVIDIA GPUs), a kernel with
    // workspaces or that takes memory from the device heap keeps one lane
    // per iteration: the lanes would share what the iteration builds there
    // as they share nothing else.
    if (!k.independent_loops) {
        if (kernel.m_gpu->n_workspaces > 0) return false;
        class Heap : public ASR::BaseWalkVisitor<Heap> {
        public:
            bool found = false;
            void visit_GpuAtomicFetchAdd(const ASR::GpuAtomicFetchAdd_t &) {
                found = true;
            }
        } heap;
        for (size_t i = 0; i < kernel.n_body; i++) {
            heap.visit_stmt(*kernel.m_body[i]);
        }
        if (heap.found) return false;
    }
    for (size_t i = 0; i < kernel.n_args; i++) {
        k.params.insert(ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(kernel.m_args[i])->m_v));
    }
    for (size_t i = 0; i < kernel.m_gpu->n_workspaces; i++) {
        k.workspaces.insert(kernel.m_gpu->m_workspaces[i].m_variable);
    }
    StopScalars stops(k.stop_scalars);
    for (size_t i = 0; i < kernel.n_body; i++) {
        stops.visit_stmt(*kernel.m_body[i]);
    }
    for (size_t i = 0; i < kernel.n_body; i++) {
        if (!ASR::is_a<ASR::GpuReduceCombine_t>(*kernel.m_body[i])) continue;
        ASR::GpuReduceCombine_t *c = ASR::down_cast<ASR::GpuReduceCombine_t>(
            kernel.m_body[i]);
        for (size_t j = 0; j < c->n_items; j++) {
            if (ASR::is_a<ASR::Var_t>(*c->m_items[j].m_value)) {
                k.first_lane_scalars.insert(ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(c->m_items[j].m_value)->m_v));
            }
        }
    }
    {
        ScalarDefinitions definitions(k);
        for (size_t i = 0; i < kernel.n_body; i++) {
            definitions.visit_stmt(*kernel.m_body[i]);
        }
    }
    for (bool added = true; added;) {
        IterationScalars iteration(k);
        for (size_t i = 0; i < kernel.n_body; i++) {
            iteration.visit_stmt(*kernel.m_body[i]);
        }
        added = iteration.added;
    }
    if (std::unique_ptr<LaneRewriter> shared = share_block_arrays(al, kernel,
            k, caps)) {
        // As many lanes as the longest loop they share out has iterations,
        // rounded up to a power of two, from a SIMD-group to a block (a
        // block of 256 lanes leaves half of them idle in loops over 128
        // elements, and waits for them at every barrier), unless a loop
        // runs its iterations on several lanes (plan_parts), which takes
        // the block.
        std::vector<TripBound> trips;
        shared->loop_bounds(trips);
        lanes = caps.lanes_block;
        bool known = !trips.empty();
        int64_t most = 0;
        for (const TripBound &t : trips) {
            if (t.param) known = false;
            most = std::max(most, t.end - t.start + 1);
        }
        if (known && !shared->has_parts()) {
            lanes = caps.lanes_per_iteration;
            while (lanes < most && lanes < caps.lanes_block) lanes *= 2;
        }
        k.lanes = add_lanes_parameter(al, kernel);
        kernel.m_gpu->m_lanes_max = lanes;
        k.fixed_lanes = lanes;
        shared->apply(kernel.m_body, kernel.n_body);
        EarlyLoads early(al, k, shared->lane_loops, shared->held_loops,
            lanes);
        early.run(kernel);
        for (ASR::symbol_t *s : k.block_arrays) {
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
            ASR::ttype_t *t = ASRUtils::duplicate_type(al, v->m_type);
            ASR::down_cast<ASR::Array_t>(t)->m_memory_space =
                ASR::memory_spaceType::Shared;
            v->m_type = t;
        }
        block = true;
        return true;
    }
    std::vector<Frame> frames;
    LaneRewriter first(al, k);
    first.decide(kernel.m_body, kernel.n_body, nullptr, frames);
    // A kernel with no loop to share out but a sum that reads across the
    // columns of an array (`do i = 1, n; s = 0; do k = 1, m; s = s +
    // x(i, k)`, the iterations along the column) is tried again with that
    // sum shared out. One lane per iteration reads the elements of a row
    // with consecutive threads, which pays when the iterations fill the
    // GPU; a launch of few iterations instead leaves each a long chain of
    // dependent loads on one thread, and such a launch gets lanes
    // (lanes_scattered_iterations_max): the sum over the 360 longitudes of
    // the 55 radii of a pole of POT3D goes from 40 to a few microseconds.
    std::unique_ptr<LaneRewriter> second;
    LaneRewriter *chosen = &first;
    if ((!first.distributed || first.scattered_declined) && k.lane_reductions
            && caps.lanes_scattered_iterations_max > 0) {
        first.restore();
        k.scattered_reductions = true;
        frames.clear();
        second.reset(new LaneRewriter(al, k));
        second->decide(kernel.m_body, kernel.n_body, nullptr, frames);
        chosen = second.get();
        scattered = true;
    }
    LaneRewriter &rewrite = *chosen;
    if (rewrite.failed || !rewrite.distributed) {
        rewrite.restore();
        scattered = false;
        return false;
    }
    any_size = !scattered
        && rewrite.pays_at_any_size(kernel.m_body, kernel.n_body)
        && !gpu_reads_wide(kernel, caps);
    lanes = caps.lanes_per_iteration;
    while (rewrite.lane_limit > 0 && lanes > rewrite.lane_limit && lanes > 2) {
        lanes /= 2;
    }
    k.lanes = add_lanes_parameter(al, kernel);
    // A kernel that gets fewer lanes than a warp has no use for the lanes of
    // a whole block.
    kernel.m_gpu->m_lanes_max = lanes == caps.lanes_per_iteration
        ? std::max({lanes, caps.lanes_block,
            any_size ? caps.lanes_column_block : 0}) : lanes;
    rewrite.loop_bounds(bounds);
    rewrite.apply(kernel.m_body, kernel.n_body);
    return true;
}

// Rewrites the kernels and gives each launch of one the lanes it runs an
// iteration on.
class VectorLanes : public PassUtils::PassVisitor<VectorLanes> {
public:
    VectorLanes(Allocator &al, const GpuDeviceCapabilities &caps_)
        : PassVisitor(al, nullptr), lanes(caps_.lanes_per_iteration),
          caps(caps_) {
        pass_result.reserve(al, 4);
    }

    void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &x) {
        ASR::Function_t &kernel =
            *ASR::down_cast<ASR::Function_t>(x.m_kernel);
        if (kernel.m_gpu == nullptr) return;
        if (decided.insert(&kernel).second && kernel.m_gpu->m_lanes == nullptr) {
            bool any_size = false, scattered = false, block = false;
            int kernel_lanes = lanes;
            give_lanes(al, kernel, caps, any_size, kernel_lanes, scattered,
                kernel_bounds[&kernel], block);
            kernel_lane_count[&kernel] = kernel_lanes;
            if (any_size) column_kernels.insert(&kernel);
            if (scattered) scattered_kernels.insert(&kernel);
            if (block) block_kernels.insert(&kernel);
            if (kernel.m_gpu->m_lanes != nullptr && !block
                    && gpu_reads_wide(kernel, caps)) {
                wide_kernels.insert(&kernel);
                if (!scattered && caps.lanes_wide > caps.lanes_per_iteration
                        && kernel_lanes == caps.lanes_per_iteration
                        && gpu_wide_rows_per_iteration(kernel)
                            >= wide_block_rows
                        && kernel.m_gpu->m_lanes_max >= caps.lanes_wide
                        && kernel.m_gpu->m_lanes_max % caps.lanes_wide == 0) {
                    kernel.m_gpu->m_lanes_max = caps.lanes_wide;
                }
            }
        }
        if (kernel.m_gpu->m_lanes == nullptr
                || x.n_args == kernel.n_args) {
            return;
        }
        LCOMPILERS_ASSERT(x.n_args + 1 == kernel.n_args);
        const Location &loc = x.base.base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        ASR::ttype_t *int4 = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
        ASR::expr_t *n = b.Variable(current_scope,
            current_scope->get_unique_name("__gpu_lanes"), int4,
            ASR::intentType::Local);
        ASR::ttype_t *count_type = ASRUtils::expr_type(x.m_thread_count);
        ASRUtils::ExprStmtDuplicator dup(al);
        dup.success = true;
        ASR::expr_t *count = dup.duplicate_expr(x.m_thread_count);
        LCOMPILERS_ASSERT(dup.success && count);
        // The arrays of the iteration of a kernel that the block shares
        // (give_lanes) belong to one iteration: every launch runs one
        // iteration per block, on all its threads.
        if (block_kernels.count(&kernel)) {
            int block_lanes = kernel.m_gpu->m_lanes_max;
            pass_result.push_back(al, b.Assignment(n, b.i32(block_lanes)));
            const_cast<ASR::GpuKernelLaunch_t&>(x).m_block_size = b.i_t(
                block_lanes, ASRUtils::expr_type(x.m_block_size));
            finish_launch(x, kernel, n);
            return;
        }
        pass_result.push_back(al, b.Assignment(n, b.i32(1)));
        bool scattered = scattered_kernels.count(&kernel) > 0;
        int64_t iterations_max = column_kernels.count(&kernel)
            ? std::max(caps.lanes_iterations_max,
                caps.lanes_column_iterations_max)
            : caps.lanes_iterations_max;
        int64_t block_iterations_max = caps.lanes_block_iterations_max;
        if (scattered) {
            iterations_max = std::min(iterations_max,
                caps.lanes_scattered_iterations_max);
            block_iterations_max = std::min(block_iterations_max,
                caps.lanes_scattered_iterations_max);
        }
        // A lane that reads vectors of 16 bytes (gpu_wide_loads.cpp) reads
        // 512 contiguous bytes with its warp; without lanes, the 32
        // threads of a warp read 16 bytes from 32 different rows. So a
        // launch of any size gets them: the language-model head of a
        // 248,000-word vocabulary read at a third of the bandwidth.
        if (wide_kernels.count(&kernel) && !scattered) {
            iterations_max = std::max(iterations_max, wide_iterations_max);
        }
        // A kernel whose lanes were cut to the length of a loop they share
        // keeps that many.
        int kernel_lanes = kernel_lane_count[&kernel];
        // A kernel that reads vectors of 16 bytes runs an iteration on each
        // block of caps.lanes_wide threads (see `decided` above).
        bool wide_block = wide_kernels.count(&kernel) && caps.lanes_wide > 0
            && kernel.m_gpu->m_lanes_max == caps.lanes_wide;
        if (column_kernels.count(&kernel) && kernel_lanes == lanes
                && caps.lanes_column_block > lanes) {
            kernel_lanes = caps.lanes_column_block;
        }
        pass_result.push_back(al, b.If(b.LtE(count,
            b.i_t(iterations_max, count_type)),
            {b.Assignment(n, b.i32(kernel_lanes))}, {}));
        // A launch of few iterations gives each a whole block, unless the
        // kernel has no use for more lanes than it gets.
        if (kernel.m_gpu->m_lanes_max > caps.lanes_per_iteration) {
            pass_result.push_back(al, b.If(b.LtE(count,
                b.i_t(wide_block ? iterations_max : block_iterations_max,
                    count_type)),
                {b.Assignment(n, b.i32(kernel.m_gpu->m_lanes_max))}, {}));
        }
        // When every loop the lanes share out is shorter than a few
        // iterations per thread of a block, they are the lanes of a warp: a
        // block would leave most of its threads idle.
        {
            ASR::expr_t *shorter = nullptr;
            for (const TripBound &t : kernel_bounds[&kernel]) {
                ASR::expr_t *end = nullptr;
                if (t.param == nullptr) {
                    end = b.i32(static_cast<int32_t>(t.end));
                } else {
                    for (size_t i = 0; i < x.n_args && i < kernel.n_args; i++) {
                        if (ASRUtils::symbol_get_past_external(
                                ASR::down_cast<ASR::Var_t>(kernel.m_args[i])
                                    ->m_v) == t.param) {
                            end = x.m_args[i].m_value;
                        }
                    }
                }
                if (end == nullptr) continue;
                ASR::expr_t *trips = b.Add(b.Sub(b.i2i_t(end, int4),
                    b.i32(static_cast<int32_t>(t.start))), b.i32(1));
                ASR::expr_t *test = b.Lt(trips, b.i32(
                    static_cast<int32_t>(short_loop_threads
                        * caps.threads_per_block)));
                shorter = shorter ? b.And(shorter, test) : test;
            }
            // Not for a launch of few iterations, which gives each a whole
            // block because a warp each would leave most of the GPU idle:
            // a loop that is short does not change that.
            if (shorter) {
                pass_result.push_back(al, b.If(b.And(b.And(shorter,
                    b.Gt(n, b.i32(lanes))),
                    b.Gt(count, b.i_t(block_iterations_max, count_type))),
                    {b.Assignment(n, b.i32(lanes))}, {}));
            }
        }
        // The kernel names those lanes as a constant (gpu_kernel_signature.h),
        // whatever the size of the launch.
        if (wide_block) {
            pass_result.push_back(al, b.Assignment(n, b.i32(caps.lanes_wide)));
        }
        auto &launch = const_cast<ASR::GpuKernelLaunch_t&>(x);
        // An iteration of a kernel that reads vectors of 16 bytes streams
        // whole rows, and the launch has few of them: blocks of
        // `threads_per_block` would share them out unevenly between the
        // multiprocessors (the 320 blocks of 8 warps of a matrix-vector
        // product of 5120 rows, two rows per iteration, are three on some
        // of the 132 of an H200 and two on the others, which then wait).
        // Its launches start blocks of caps.lanes_wide threads, one
        // iteration each, or of wide_block_iterations warps when the lanes
        // are a warp.
        int64_t block_threads = 0;
        if (wide_kernels.count(&kernel)
                && ASRUtils::extract_value(ASRUtils::expr_value(
                    x.m_block_size), block_threads)) {
            ASR::ttype_t *bt = ASRUtils::expr_type(x.m_block_size);
            if (wide_block && block_threads >= caps.lanes_wide) {
                launch.m_block_size = b.i_t(caps.lanes_wide, bt);
            } else if (block_threads > wide_block_iterations*lanes
                    && block_threads % (wide_block_iterations*lanes) == 0) {
                ASR::expr_t *block = b.Variable(current_scope,
                    current_scope->get_unique_name("__gpu_block_threads"),
                    ASRUtils::duplicate_type(al, bt), ASR::intentType::Local);
                pass_result.push_back(al, b.Assignment(block, x.m_block_size));
                pass_result.push_back(al, b.If(b.Eq(n, b.i32(lanes)),
                    {b.Assignment(block,
                        b.i_t(wide_block_iterations*lanes, bt))}, {}));
                launch.m_block_size = block;
            }
        }
        finish_launch(x, kernel, n);
    }

    // Hands the launch the lanes `n` as the kernel's last argument.
    void finish_launch(const ASR::GpuKernelLaunch_t &x,
            ASR::Function_t &kernel, ASR::expr_t *n) {
        const Location &loc = x.base.base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        // A tiled launch (gpu_offload.cpp) numbers an iteration in its tile
        // from the thread index along the tile's width and height, which
        // lanes narrow to the iterations' (the lanes of one iteration share
        // a thread index): only a fraction of each tile would run. A launch
        // with lanes runs flat instead, with the threads the launch started
        // for the tiles, of which the ones past the iterations return.
        for (size_t i = 0; i < x.n_args && i < kernel.n_args; i++) {
            ASR::symbol_t *parameter = ASR::down_cast<ASR::Var_t>(
                kernel.m_args[i])->m_v;
            if (std::string(ASRUtils::symbol_name(parameter)).rfind(
                    "__tiled", 0) != 0
                    || !ASR::is_a<ASR::Var_t>(*x.m_args[i].m_value)) {
                continue;
            }
            ASR::expr_t *tiled = x.m_args[i].m_value;
            pass_result.push_back(al, b.If(b.Gt(n, b.i32(1)),
                {b.Assignment(tiled, b.i_t(0, ASRUtils::expr_type(tiled)))},
                {}));
        }
        Vec<ASR::call_arg_t> args;
        args.reserve(al, x.n_args + 1);
        for (size_t i = 0; i < x.n_args; i++) args.push_back(al, x.m_args[i]);
        ASR::call_arg_t arg;
        arg.loc = loc;
        arg.m_value = n;
        args.push_back(al, arg);
        auto &launch = const_cast<ASR::GpuKernelLaunch_t&>(x);
        launch.m_args = args.p;
        launch.n_args = args.n;
        pass_result.push_back(al, &launch.base);
    }

private:
    int lanes;
    GpuDeviceCapabilities caps;
    std::set<ASR::Function_t*> decided;
    // The lanes of an iteration of each kernel that has them.
    std::map<ASR::Function_t*, int> kernel_lane_count;
    // The numbers of iterations of the loops each kernel shares out that a
    // launch knows (LaneRewriter::loop_bounds).
    std::map<ASR::Function_t*, std::vector<TripBound>> kernel_bounds;
    // The kernels whose lanes pay for themselves however many iterations a
    // launch has (LaneRewriter::pays_at_any_size).
    std::set<ASR::Function_t*> column_kernels;
    // The kernels that share out a sum read across columns, which get
    // lanes only for a launch of few iterations (give_lanes).
    std::set<ASR::Function_t*> scattered_kernels;
    // The kernels that read vectors of 16 bytes.
    std::set<ASR::Function_t*> wide_kernels;
    // The kernels whose arrays of the iteration the block shares, which run
    // one iteration per block (give_lanes).
    std::set<ASR::Function_t*> block_kernels;
    // The most iterations of a launch of such a kernel that get lanes:
    // 32 lanes of them stay within the 32-bit thread index.
    static constexpr int64_t wide_iterations_max = int64_t(1) << 24;
    // The iterations, a warp each, of a block of a launch of such a kernel.
    static constexpr int64_t wide_block_iterations = 2;
    // The rows of a matrix an iteration of a kernel that reads vectors must
    // read in one loop to get the lanes of caps.lanes_wide threads, as a
    // hand-written product gives a block to several rows at once. With
    // fewer rows a warp reads them faster: on an H200, a q8 product of
    // 5120 rows and 17408 columns, two rows per iteration, took 38.5 us
    // with a warp and 46.8 us with 64 lanes, and one row per iteration was
    // 12% slower with 64 lanes; four rows of 34816 by 5120 took 51.6 us
    // with 64 lanes (and their sums combined together, asr_to_gpu_c.h)
    // and 54.7 us with a warp.
    static constexpr int64_t wide_block_rows = 4;
    // A loop of fewer iterations than this many per thread of a block does
    // not get a block of lanes.
    static constexpr int64_t short_loop_threads = 8;
};

} // namespace

bool gpu_kernel_block_shared(const ASR::Function_t &kernel) {
    if (kernel.m_gpu == nullptr || kernel.m_gpu->m_lanes == nullptr) {
        return false;
    }
    std::function<bool(SymbolTable*)> holds = [&](SymbolTable *scope) {
        for (auto &item : scope->get_scope()) {
            ASR::symbol_t *s = item.second;
            if (ASR::is_a<ASR::Block_t>(*s)) {
                if (holds(ASR::down_cast<ASR::Block_t>(s)->m_symtab)) {
                    return true;
                }
            } else if (ASR::is_a<ASR::Variable_t>(*s)) {
                ASR::ttype_t *t = ASR::down_cast<ASR::Variable_t>(s)->m_type;
                if (ASR::is_a<ASR::Array_t>(*t)
                        && ASR::down_cast<ASR::Array_t>(t)->m_memory_space
                            == ASR::memory_spaceType::Shared) {
                    return true;
                }
            }
        }
        return false;
    };
    return holds(kernel.m_symtab);
}

void pass_gpu_vector_lanes(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options) {
    GpuDeviceCapabilities caps = gpu_device_capabilities(pass_options);
    if (!caps.device_selected() || caps.lanes_per_iteration <= 1) return;
    // A launch gets lanes only when it has at most lanes_iterations_max
    // iterations, so it starts at most this many threads, also when it runs
    // in chunks (device_launch_expand.cpp): few enough to be numbered in
    // the 32-bit thread index of a Metal kernel.
    LCOMPILERS_ASSERT(std::max(caps.lanes_per_iteration,
            caps.lanes_column_block)
        <= std::numeric_limits<int32_t>::max() / std::max(
            caps.lanes_iterations_max, caps.lanes_column_iterations_max));
    VectorLanes v(al, caps);
    v.visit_TranslationUnit(unit);
}

} // namespace LCompilers
