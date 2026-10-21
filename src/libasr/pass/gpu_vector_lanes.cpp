#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/pass/gpu_offload_stop.h>
#include <libasr/pass/gpu_vector_lanes.h>
#include <libasr/pass/pass_utils.h>

#include <map>
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
lanes_per_iteration) when the launch has at most `iterations_max`
iterations, and one otherwise, where the iterations alone fill the GPU and
the lanes would only repeat each other's work.

Every lane of an iteration runs the body of the iteration, with the same
values in its private scalars, except for these statements:

* A *distributed* loop runs its iterations on the lanes in turn: lane k
  takes iterations k, k + lanes, ... (its start moves by the lane, its step
  is multiplied by the lanes), and the lanes wait for each other after it.
  A loop is distributed when its iterations are independent, which this
  pass checks rather than assumes:
  - its body has only assignments, `if`, loops, calls of pure procedures,
    and the `stop` handling of gpu_offload_stop.h;
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
* A *single* statement runs on the first lane only: a statement outside a
  distributed loop that has an effect the lanes must not repeat -- one that
  assigns shared memory, calls a procedure that is not pure, takes elements
  of the device heap. The other lanes wait for it, and then take every
  private scalar it assigned from the first lane (GpuLaneBroadcast).
  Consecutive single statements run together.
* A loop or `if` that holds a distributed loop runs on every lane, which go
  through it together; what is inside is rewritten in turn.

A kernel is rewritten only if it has a loop to distribute, and only if all
of the following holds of every single statement: it does not return (a
stop on the first lane alone would leave the others going on with values
it never broadcast), it assigns no private array (a lane-private copy the
others would miss) and every private scalar it assigns is an integer, a
real or a logical of 4 bytes. Nor is a kernel rewritten that assigns an
element of a kernel argument that every iteration assigns (no subscript
depends on the iteration): an array declared outside the loop that each
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

// A launch with more iterations than this runs one thread per iteration.
const int64_t iterations_max = 1024;

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

bool pure_callee(ASR::symbol_t *name) {
    ASR::symbol_t *s = ASRUtils::symbol_get_past_external(name);
    if (ASR::is_a<ASR::StructMethodDeclaration_t>(*s)) {
        s = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::StructMethodDeclaration_t>(s)->m_proc);
    }
    if (!ASR::is_a<ASR::Function_t>(*s)) return false;
    ASR::FunctionType_t *t = ASRUtils::get_FunctionType(
        ASR::down_cast<ASR::Function_t>(s));
    return t->m_pure || t->m_elemental;
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
// statement that names it assigns it without reading it first.
Access first_access(ASR::stmt_t **body, size_t from, size_t to,
        ASR::symbol_t *s) {
    for (size_t i = from; i < to; i++) {
        ASR::stmt_t *x = body[i];
        if (ASR::is_a<ASR::Assignment_t>(*x)) {
            auto *a = ASR::down_cast<ASR::Assignment_t>(x);
            if (ASR::is_a<ASR::Var_t>(*a->m_target)
                    && root_of(a->m_target) == s) {
                return mentions(a->m_value, s) ? Access::Read
                    : Access::Write;
            }
        } else if (ASR::is_a<ASR::DoLoop_t>(*x)) {
            auto *d = ASR::down_cast<ASR::DoLoop_t>(x);
            if (d->m_head.m_v && root_of(d->m_head.m_v) == s) {
                return mentions(d->m_head.m_start, s)
                    || mentions(d->m_head.m_end, s)
                    || mentions(d->m_head.m_increment, s)
                    ? Access::Read : Access::Write;
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
                if (g.owner && ASR::is_a<ASR::If_t>(*g.owner)) return false;
                if (header_reads(g.owner)) return false;
            }
            a = first_access(g.body, 0, g.index, s);
            if (a == Access::Write) return true;
            if (a == Access::Read) return false;
        }
        return true;
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
        if (ASRUtils::is_pointer(t) || workspaces.count(s)) return true;
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
    // The private scalars it may assign.
    std::set<ASR::symbol_t*> scalars;
    // The status buffers of the stops it records.
    std::set<ASR::symbol_t*> statuses;
    std::set<ASR::symbol_t*> blocks;

    explicit Effects(const KernelInfo &k_) : k(k_) {}

    void target(ASR::expr_t *e) {
        ASR::symbol_t *r = root_of(e);
        if (r == nullptr) {
            unknown = true;
            return;
        }
        if (ASR::is_a<ASR::Var_t>(*e) && k.private_scalar(r)) {
            scalars.insert(r);
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
                scalars.insert(r);
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

// Whether a loop leaves itself by `exit` or `cycle` of its own, rather than
// of a loop inside it.
class OwnExitOrCycle : public ASR::BaseWalkVisitor<OwnExitOrCycle> {
public:
    const char *name;
    int depth = 0;
    bool found = false;
    explicit OwnExitOrCycle(const char *name_) : name(name_) {}
    void own(const char *target) {
        if (target == nullptr) {
            if (depth == 0) found = true;
        } else if (name != nullptr && std::string(target) == name) {
            found = true;
        }
    }
    void visit_Exit(const ASR::Exit_t &x) { own(x.m_stmt_name); }
    void visit_Cycle(const ASR::Cycle_t &x) { own(x.m_stmt_name); }
    void visit_DoLoop(const ASR::DoLoop_t &x) {
        depth++;
        BaseWalkVisitor::visit_DoLoop(x);
        depth--;
    }
    void visit_WhileLoop(const ASR::WhileLoop_t &x) {
        depth++;
        BaseWalkVisitor::visit_WhileLoop(x);
        depth--;
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

// A subscript as something plus a constant.
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

class LaneRewriter {
public:
    LaneRewriter(Allocator &al_, KernelInfo &k_) : al(al_), k(k_) {}

    bool failed = false;
    bool distributed = false;

    // Decides what every statement of a list, and of the lists inside it,
    // does. True when it distributes a loop.
    bool decide(ASR::stmt_t **&body, size_t &n_body, ASR::stmt_t *owner,
            std::vector<Frame> &frames) {
        bool any = false;
        for (size_t i = 0; i < n_body; i++) {
            frames.push_back({body, n_body, i, owner});
            Role r = role(body[i], frames);
            frames.pop_back();
            if (r == Role::Distribute) {
                fuse_following(body, n_body, i, owner, frames);
                frames.push_back({body, n_body, i, owner});
                if (collapsible(ASR::down_cast<ASR::DoLoop_t>(body[i]))) {
                    collapse.insert(body[i]);
                }
                frames.pop_back();
            }
            roles[body[i]] = r;
            if (r == Role::Distribute || r == Role::Recurse) any = true;
        }
        distributed |= any;
        return any;
    }

    // Rewrites a list as decided.
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
                emit_single(single, out);
                single.clear();
            }
            if (i == n_body) break;
            if (r == Role::Distribute) {
                distribute(ASR::down_cast<ASR::DoLoop_t>(body[i]), out);
                continue;
            }
            if (r == Role::Recurse) nested_apply(body[i]);
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
        out.push_back(al, ASRUtils::STMT(ASR::make_GpuLaneBarrier_t(al,
            loc)));
        for (ASR::symbol_t *s : e.scalars) {
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
        const Location &loc = d->base.base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        ASR::ttype_t *type = ASRUtils::expr_type(d->m_head.m_v);
        ASR::expr_t *step = d->m_head.m_increment
            ? d->m_head.m_increment : b.i_t(1, type);
        ASR::expr_t *lane = b.i2i_t(lane_index(loc), type);
        ASR::expr_t *lanes = b.i2i_t(lanes_var(loc), type);
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

    void nested_apply(ASR::stmt_t *s) {
        switch (s->type) {
            case ASR::stmtType::DoLoop: {
                auto *x = ASR::down_cast<ASR::DoLoop_t>(s);
                apply(x->m_body, x->n_body);
                break;
            }
            case ASR::stmtType::WhileLoop: {
                auto *x = ASR::down_cast<ASR::WhileLoop_t>(s);
                apply(x->m_body, x->n_body);
                break;
            }
            case ASR::stmtType::If: {
                auto *x = ASR::down_cast<ASR::If_t>(s);
                apply(x->m_body, x->n_body);
                apply(x->m_orelse, x->n_orelse);
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
                nested_decide(s, frames);
                return Role::Recurse;
            case ASR::stmtType::Return:
            case ASR::stmtType::Exit:
            case ASR::stmtType::Cycle:
                return Role::Replicate;
            case ASR::stmtType::DoLoop:
                if (distributable(ASR::down_cast<ASR::DoLoop_t>(s), frames)) {
                    distributed = true;
                    return Role::Distribute;
                }
                if (nested_decide(s, frames)) return Role::Recurse;
                break;
            case ASR::stmtType::If:
                if (stop_record(*s, nullptr)) return Role::Replicate;
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
        if (!e.shared_effect) return Role::Replicate;
        // Run on the first lane alone. What it assigns has to reach the
        // others, and a return would leave them without it.
        if (e.returns || e.private_array || e.associates) {
            failed = true;
            return Role::Single;
        }
        OwnExitOrCycle leaves(nullptr);
        leaves.visit_stmt(*s);
        if (leaves.found) failed = true;
        if (writes_shared_temporary(k, *s)) failed = true;
        for (ASR::symbol_t *v : e.scalars) {
            ASR::ttype_t *t = ASRUtils::symbol_type(v);
            if (!(ASRUtils::is_integer(*t) || ASRUtils::is_real(*t)
                    || ASRUtils::is_logical(*t))
                    || ASRUtils::extract_kind_from_ttype_t(t) != 4) {
                failed = true;
            }
        }
        return Role::Single;
    }

    bool distributable(ASR::DoLoop_t *d, std::vector<Frame> &frames) {
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
                void visit_BlockCall(const ASR::BlockCall_t &) {
                    found = true;
                }
            } impure;
            for (size_t i = 0; i < d->n_body; i++) {
                impure.visit_stmt(*d->m_body[i]);
            }
            if (impure.found) return false;
        }
        if (e.returns && e.statuses.empty()) return false;
        OwnExitOrCycle own(d->m_name);
        for (size_t i = 0; i < d->n_body; i++) own.visit_stmt(*d->m_body[i]);
        if (own.found) return false;
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
        // the iterations apart.
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
            if (!tells_apart) return false;
            bool same = false;
            for (ASR::expr_t *w : written) {
                if (overlapping(path_of(w), path_of(a->m_target))) {
                    if (!same_expr(w, a->m_target)) return false;
                    same = true;
                }
            }
            if (!same) written.push_back(a->m_target);
        }
        // Each of them is read at that subscript list only.
        Designators names;
        names.visit_expr(*d->m_head.m_v);
        for (size_t i = 0; i < d->n_body; i++) names.visit_stmt(*d->m_body[i]);
        for (ASR::expr_t *w : written) {
            std::vector<ASR::symbol_t*> wp = path_of(w);
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
            if (first_access(d->m_body, 0, d->n_body, s) != Access::Write) {
                return false;
            }
        }
        return true;
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

// Rewrites `kernel` to run each iteration on lanes, when it has a loop to
// distribute and nothing in the way.
bool give_lanes(Allocator &al, ASR::Function_t &kernel) {
    KernelInfo k;
    k.fn = &kernel;
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
    for (bool added = true; added;) {
        IterationScalars iteration(k);
        for (size_t i = 0; i < kernel.n_body; i++) {
            iteration.visit_stmt(*kernel.m_body[i]);
        }
        added = iteration.added;
    }
    std::vector<Frame> frames;
    LaneRewriter rewrite(al, k);
    rewrite.decide(kernel.m_body, kernel.n_body, nullptr, frames);
    if (rewrite.failed || !rewrite.distributed) return false;
    k.lanes = add_lanes_parameter(al, kernel);
    rewrite.apply(kernel.m_body, kernel.n_body);
    return true;
}

// Rewrites the kernels and gives each launch of one the lanes it runs an
// iteration on.
class VectorLanes : public PassUtils::PassVisitor<VectorLanes> {
public:
    VectorLanes(Allocator &al, int lanes_)
        : PassVisitor(al, nullptr), lanes(lanes_) {
        pass_result.reserve(al, 4);
    }

    void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &x) {
        ASR::Function_t &kernel =
            *ASR::down_cast<ASR::Function_t>(x.m_kernel);
        if (kernel.m_gpu == nullptr) return;
        if (decided.insert(&kernel).second && kernel.m_gpu->m_lanes == nullptr) {
            give_lanes(al, kernel);
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
        pass_result.push_back(al, b.Assignment(n, b.i32(1)));
        pass_result.push_back(al, b.If(b.LtE(count,
            b.i_t(iterations_max, count_type)),
            {b.Assignment(n, b.i32(lanes))}, {}));
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
    std::set<ASR::Function_t*> decided;
};

} // namespace

void pass_gpu_vector_lanes(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options) {
    GpuDeviceCapabilities caps = gpu_device_capabilities(pass_options);
    if (!caps.device_selected() || caps.lanes_per_iteration <= 1) return;
    VectorLanes v(al, caps.lanes_per_iteration);
    v.visit_TranslationUnit(unit);
}

} // namespace LCompilers
