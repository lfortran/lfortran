#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/device_launch_expand.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/pass/access_paths.h>
#include <libasr/pass/gpu_launch_sync.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/pass_utils.h>

#include <algorithm>
#include <map>
#include <set>
#include <tuple>
#include <vector>

namespace LCompilers {

/*
Where the host waits for the GPU.

A GpuKernelLaunch does not wait for its kernel, and the device keeps what a
kernel writes until the host waits (lfortran_gpu_runtime.h). This pass puts a
GpuSync -- wait for every launch, and copy what the kernels wrote back to the
host -- where the host next needs that, and nowhere else:

* before a host statement that may read or write, allocate or free storage a
  pending launch may write: a read would see stale values, and the copy back
  at the next wait would undo a write. Storage a launch only reads needs no
  wait: the runtime copies it when the launch is made;
* before a statement that acts outside the program -- input/output, `stop`,
  a call to a procedure that is not pure -- while a launch whose kernel can
  stop is pending, so that a stop the kernel recorded ends the program first,
  as it does on the CPU;
* before a launch of a kernel that can stop while another such launch is
  pending, so that the stop reported is the first one;
* before `return`, `goto` and the end of the procedure, when a launch is
  pending: what the caller sees is complete;
* before any statement this pass does not know.

After every GpuSync, the stops each pending launch may have recorded are
replayed (GpuKernelLaunch.stop_replay, see gpu_offload_stop.h), in launch
order; the status buffers are cleared once, on entry to the procedure.

What a statement accesses is worked out over access paths (access_paths.h).

Control flow is structured: `if`, `select case`, loops (to a fixed point),
`block` and `associate`, `exit` and `cycle`. A local of a `block` that a
pending launch writes, at the end of the block or where `exit` or `cycle`
leaves it, and an array the host deallocates, are not waited for: the host
does not need the values, so a GpuDiscard tells the runtime to forget them.
*/

namespace {

using namespace AccessPaths;

struct State {
    bool reachable = true;
    // Any launch may be pending.
    bool any = false;
    // What the pending launches may write, sorted.
    std::vector<Path> written;
    // The pending launches that may have recorded a stop, in launch order.
    std::vector<ASR::GpuKernelLaunch_t*> stops;

    static State unreachable() {
        State s;
        s.reachable = false;
        return s;
    }

    void clear() {
        any = false;
        written.clear();
        stops.clear();
    }

    void add_written(const Path &p) {
        auto it = std::lower_bound(written.begin(), written.end(), p);
        if (it == written.end() || !(*it == p)) written.insert(it, p);
    }

    void add_stop(ASR::GpuKernelLaunch_t *launch) {
        if (std::find(stops.begin(), stops.end(), launch) == stops.end()) {
            stops.push_back(launch);
        }
    }

    void join(const State &o) {
        if (!o.reachable) return;
        if (!reachable) {
            *this = o;
            return;
        }
        any = any || o.any;
        for (const Path &p : o.written) add_written(p);
        for (ASR::GpuKernelLaunch_t *l : o.stops) add_stop(l);
    }

    bool operator==(const State &o) const {
        return reachable == o.reachable && any == o.any
            && written == o.written && stops == o.stops;
    }
};


class LaunchFinder : public ASR::BaseWalkVisitor<LaunchFinder> {
public:
    bool found = false;
    void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &/*x*/) {
        found = true;
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(
            ASRUtils::symbol_get_past_external(x.m_m));
        for (size_t i = 0; i < block->n_body && !found; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::AssociateBlock_t *block = ASR::down_cast<ASR::AssociateBlock_t>(
            ASRUtils::symbol_get_past_external(x.m_m));
        for (size_t i = 0; i < block->n_body && !found; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
};

bool launches(ASR::stmt_t **body, size_t n_body) {
    LaunchFinder f;
    for (size_t i = 0; i < n_body && !f.found; i++) f.visit_stmt(*body[i]);
    return f.found;
}

class RoutineSync {
public:
    RoutineSync(Allocator &al_, SymbolTable *routine_,
            const PointerTargets &pointers_)
        : al(al_), routine(routine_), pointers(pointers_) {}

    // The new body of the routine.
    Vec<ASR::stmt_t*> run(ASR::stmt_t **body, size_t n_body,
            const Location &loc) {
        Vec<ASR::stmt_t*> out;
        out.reserve(al, n_body + 4);
        State end = process_body(body, n_body, State(), true, &out);
        if (end.reachable && end.any) sync(end, true, &out, loc);
        // Each status buffer is cleared once, before its first launch.
        Vec<ASR::stmt_t*> result;
        result.reserve(al, out.size() + status_buffers.size());
        ASRUtils::ASRBuilder b(al, loc);
        for (ASR::expr_t *status : status_buffers) {
            ASR::ttype_t *int4 = ASRUtils::TYPE(ASR::make_Integer_t(al, loc,
                4));
            Vec<ASR::array_index_t> first;
            first.reserve(al, 1);
            ASR::array_index_t ai;
            ai.loc = loc;
            ai.m_left = nullptr;
            ai.m_right = b.i32(1);
            ai.m_step = nullptr;
            first.push_back(al, ai);
            result.push_back(al, b.Assignment(ASRUtils::EXPR(
                ASR::make_ArrayItem_t(al, loc, status, first.p, first.n,
                    int4, ASR::arraystorageType::ColMajor, nullptr, false)),
                b.i32(0)));
        }
        for (size_t i = 0; i < out.size(); i++) result.push_back(al, out[i]);
        return result;
    }

private:
    Allocator &al;
    SymbolTable *routine;
    const PointerTargets &pointers;
    // Which procedures may act outside the program.
    std::map<ASR::Function_t*, bool> outside;
    std::vector<SymbolTable*> blocks;
    struct Loop {
        ASR::stmt_t *loop;
        std::string name;
        size_t blocks;
        // A while loop whose test reads what a launch in its body writes
        // waits at the end of every iteration, `cycle` included.
        bool sync_at_back_edge;
        State at_cycle;
        State at_exit;
    };
    std::vector<Loop> loops;
    std::vector<ASR::expr_t*> status_buffers;
    std::vector<ASR::symbol_t*> status_symbols;
    // How many waits have been placed, in either mode.
    size_t n_syncs = 0;

    // Waits for every pending launch, and replays the stops they recorded.
    void sync(State &s, bool emit, Vec<ASR::stmt_t*> *out,
            const Location &loc) {
        n_syncs++;
        if (emit) {
            out->push_back(al, ASRUtils::STMT(ASR::make_GpuSync_t(al, loc)));
            ASRUtils::ExprStmtDuplicator dup(al);
            for (ASR::GpuKernelLaunch_t *launch : s.stops) {
                for (size_t i = 0; i < launch->n_stop_replay; i++) {
                    out->push_back(al,
                        dup.duplicate_stmt(launch->m_stop_replay[i]));
                }
            }
        }
        s.clear();
    }

    bool conflicts(const Accesses &acc, const State &s) {
        if (s.written.empty()) return false;
        if (acc.everything) return true;
        for (const Path &w : s.written) {
            if (acc.globals && reachable_elsewhere(w.root, routine)) {
                return true;
            }
            for (const Path &a : acc.paths) {
                if (overlap(a, w)) return true;
            }
        }
        return false;
    }

    // Waits before a statement with these accesses when it needs to.
    void before(const Accesses &acc, State &s, bool emit,
            Vec<ASR::stmt_t*> *out, const Location &loc) {
        if (conflicts(acc, s) || (acc.observable && !s.stops.empty())) {
            sync(s, emit, out, loc);
        }
    }

    // Tells the runtime to forget what pending launches write into the
    // locals of `scope`, which end here.
    void discard_locals(SymbolTable *scope, State &s, bool emit,
            Vec<ASR::stmt_t*> *out, const Location &loc) {
        Vec<ASR::expr_t*> vars;
        vars.reserve(al, 1);
        std::vector<Path> kept;
        bool must_sync = false;
        for (const Path &p : s.written) {
            bool local = p.root != nullptr
                && ASR::is_a<ASR::Variable_t>(*p.root)
                && ASR::down_cast<ASR::Variable_t>(p.root)->m_parent_symtab
                    == scope
                && ASR::down_cast<ASR::Variable_t>(p.root)->m_storage
                    != ASR::storage_typeType::Save;
            if (!local) {
                kept.push_back(p);
                continue;
            }
            if (!p.members.empty() || p.shallow) {
                must_sync = true;
                kept.push_back(p);
                continue;
            }
            vars.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                p.root)));
        }
        if (must_sync) {
            sync(s, emit, out, loc);
            return;
        }
        if (vars.size() == 0) return;
        if (emit) {
            out->push_back(al, ASRUtils::STMT(ASR::make_GpuDiscard_t(al, loc,
                vars.p, vars.n)));
        }
        s.written = kept;
    }

    void leave_blocks(size_t depth, State &s, bool emit,
            Vec<ASR::stmt_t*> *out, const Location &loc) {
        for (size_t i = blocks.size(); i > depth; i--) {
            discard_locals(blocks[i - 1], s, emit, out, loc);
        }
    }

    State process_body(ASR::stmt_t **body, size_t n_body, State s,
            bool emit, Vec<ASR::stmt_t*> *out) {
        for (size_t i = 0; i < n_body; i++) {
            s = process(body[i], s, emit, out);
        }
        return s;
    }

    Vec<ASR::stmt_t*> new_body(size_t n) {
        Vec<ASR::stmt_t*> v;
        v.reserve(al, n + 2);
        return v;
    }

    Loop* find_loop(const char *name) {
        for (size_t i = loops.size(); i > 0; i--) {
            if (name == nullptr || loops[i - 1].name == name) {
                return &loops[i - 1];
            }
        }
        return nullptr;
    }

    // A loop body run to a fixed point of the state at its head, then once
    // more to place the waits.
    State loop(ASR::stmt_t *x, const char *name, ASR::stmt_t **&body,
            size_t &n_body, ASR::expr_t *test, State entry, bool emit,
            Vec<ASR::stmt_t*> *out) {
        const Location &loc = x->base.loc;
        Accesses test_acc = expression_accesses(test, routine, pointers,
            outside);
        if (test != nullptr) before(test_acc, entry, emit, out, loc);
        loops.push_back({x, name ? name : "", blocks.size(), false,
            State::unreachable(), State::unreachable()});
        if (entry.any && !launches(body, n_body)) {
            // A loop that launches nothing and waits in its first
            // iteration waits before it instead, once.
            size_t placed = n_syncs;
            process_body(body, n_body, entry, false, nullptr);
            if (n_syncs != placed) sync(entry, emit, out, loc);
        }
        State head = entry;
        for (int iteration = 0; ; iteration++) {
            Loop &l = loops.back();
            l.at_cycle = State::unreachable();
            l.at_exit = State::unreachable();
            State end = process_body(body, n_body, head, false, nullptr);
            State back = end;
            back.join(loops.back().at_cycle);
            if (test != nullptr && back.reachable
                    && (conflicts(test_acc, back)
                        || (test_acc.observable && !back.stops.empty()))) {
                loops.back().sync_at_back_edge = true;
            }
            if (loops.back().sync_at_back_edge) back.clear();
            State next = entry;
            next.join(back);
            if (next == head || iteration > 32) {
                head = next;
                break;
            }
            head = next;
        }
        Loop &l = loops.back();
        l.at_cycle = State::unreachable();
        l.at_exit = State::unreachable();
        Vec<ASR::stmt_t*> nb = new_body(n_body);
        State end = process_body(body, n_body, head, emit,
            emit ? &nb : nullptr);
        if (loops.back().sync_at_back_edge && end.reachable && end.any) {
            sync(end, emit, &nb, loc);
        }
        State after = head;
        after.join(loops.back().at_exit);
        loops.pop_back();
        if (emit) {
            body = nb.p;
            n_body = nb.n;
            out->push_back(al, x);
        }
        return after;
    }

    void enter_block(SymbolTable *scope, ASR::stmt_t **&body, size_t &n_body,
            State &s, bool emit, const Location &loc) {
        blocks.push_back(scope);
        Vec<ASR::stmt_t*> nb = new_body(n_body);
        s = process_body(body, n_body, s, emit, emit ? &nb : nullptr);
        if (s.reachable) discard_locals(scope, s, emit, &nb, loc);
        blocks.pop_back();
        if (emit) {
            body = nb.p;
            n_body = nb.n;
        }
    }

    void launch(ASR::GpuKernelLaunch_t &x, State &s, bool emit,
            Vec<ASR::stmt_t*> *out) {
        const Location &loc = x.base.base.loc;
        ASR::Function_t *kernel = ASR::down_cast<ASR::Function_t>(
            ASRUtils::symbol_get_past_external(x.m_kernel));
        const ASR::gpu_kernel_layout_t &layout = *kernel->m_gpu;
        // What the host itself reads for the launch: the scalars, the
        // geometry, and the arguments it copies.
        Accesses acc;
        AccessCollector c(acc, routine, pointers, outside);
        c.visit_expr(*x.m_thread_count);
        c.visit_expr(*x.m_block_size);
        for (size_t i = 0; i < layout.n_scalars; i++) {
            const ASR::gpu_kernel_argument_t &e = layout.m_scalars[i];
            if (e.m_kind != ASR::gpu_argument_kindType::GpuScalar) continue;
            if ((size_t)e.m_argument_index >= x.n_args) continue;
            ASR::expr_t *v = x.m_args[e.m_argument_index].m_value;
            if (v) c.visit_expr(*v);
        }
        std::vector<Path> writes;
        for (size_t i = 0; i < layout.n_buffers; i++) {
            const ASR::gpu_kernel_argument_t &e = layout.m_buffers[i];
            ASR::expr_t *arg = x.m_args[e.m_argument_index].m_value;
            GpuBufferHandling handling = gpu_launch_buffer_handling(x, e);
            if (handling == GpuBufferHandling::Scratch) continue;
            Path p;
            std::vector<ASR::expr_t*> subscripts;
            if (!path_of(pointers, arg, p, &subscripts)) {
                acc.everything = true;
                continue;
            }
            for (ASR::expr_t *sub : subscripts) c.visit_expr(*sub);
            if (e.m_member) p.members.push_back(
                ASRUtils::symbol_get_past_external(e.m_member));
            ASR::ttype_t *type = ASRUtils::expr_type(arg);
            // The bytes of a derived-type value, or of the elements of an
            // array of them; their allocatable components are separate
            // buffers.
            if (e.m_member == nullptr && ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::extract_type(type))) {
                p.shallow = true;
            }
            if (handling == GpuBufferHandling::HostCopy) {
                acc.paths.push_back(p);
            } else if (handling == GpuBufferHandling::HostCopyShallow) {
                Path q = p;
                q.shallow = true;
                acc.paths.push_back(q);
            }
            if (e.m_written) writes.push_back(p);
        }
        before(acc, s, emit, out, loc);
        if (x.m_stop_status != nullptr) {
            for (ASR::GpuKernelLaunch_t *other : s.stops) {
                if (other != &x) {
                    sync(s, emit, out, loc);
                    break;
                }
            }
            if (emit) {
                ASR::symbol_t *status = ASR::down_cast<ASR::Var_t>(
                    x.m_stop_status)->m_v;
                if (std::find(status_symbols.begin(), status_symbols.end(),
                        status) == status_symbols.end()) {
                    status_symbols.push_back(status);
                    status_buffers.push_back(x.m_stop_status);
                }
            }
        }
        if (emit) out->push_back(al, &x.base);
        s.any = true;
        for (const Path &p : writes) s.add_written(p);
        if (x.m_stop_status != nullptr) s.add_stop(&x);
    }

    // Deallocating storage a pending launch writes does not need the
    // values: the runtime forgets them instead.
    void deallocate(ASR::expr_t **vars, size_t n_vars, State &s, bool emit,
            Vec<ASR::stmt_t*> *out, const Location &loc) {
        Vec<ASR::expr_t*> forget;
        forget.reserve(al, n_vars);
        for (size_t i = 0; i < n_vars; i++) {
            Path p;
            if (!path_of(pointers, vars[i], p, nullptr)) continue;
            if (!ASRUtils::is_array(ASRUtils::expr_type(vars[i]))) continue;
            auto it = std::find(s.written.begin(), s.written.end(), p);
            if (it == s.written.end()) continue;
            forget.push_back(al, vars[i]);
            s.written.erase(it);
        }
        if (emit && forget.size() > 0) {
            out->push_back(al, ASRUtils::STMT(ASR::make_GpuDiscard_t(al, loc,
                forget.p, forget.n)));
        }
    }

    State process(ASR::stmt_t *x, State s, bool emit,
            Vec<ASR::stmt_t*> *out) {
        const Location &loc = x->base.loc;
        if (!s.reachable) {
            if (emit) out->push_back(al, x);
            return s;
        }
        switch (x->type) {
            case ASR::stmtType::GpuKernelLaunch: {
                launch(*ASR::down_cast<ASR::GpuKernelLaunch_t>(x), s, emit,
                    out);
                return s;
            }
            case ASR::stmtType::GpuSync: {
                sync(s, emit, out, loc);
                return s;
            }
            case ASR::stmtType::If: {
                ASR::If_t *i = ASR::down_cast<ASR::If_t>(x);
                before(expression_accesses(i->m_test, routine, pointers,
                    outside), s, emit, out, loc);
                Vec<ASR::stmt_t*> body = new_body(i->n_body);
                Vec<ASR::stmt_t*> orelse = new_body(i->n_orelse);
                State a = process_body(i->m_body, i->n_body, s, emit,
                    emit ? &body : nullptr);
                State b = process_body(i->m_orelse, i->n_orelse, s, emit,
                    emit ? &orelse : nullptr);
                a.join(b);
                if (emit) {
                    i->m_body = body.p;
                    i->n_body = body.n;
                    i->m_orelse = orelse.p;
                    i->n_orelse = orelse.n;
                    out->push_back(al, x);
                }
                return a;
            }
            case ASR::stmtType::Select: {
                ASR::Select_t *sel = ASR::down_cast<ASR::Select_t>(x);
                Accesses acc = expression_accesses(sel->m_test, routine,
                    pointers, outside);
                for (size_t i = 0; i < sel->n_body; i++) {
                    ASR::case_stmt_t *c = sel->m_body[i];
                    if (ASR::is_a<ASR::CaseStmt_t>(*c)) {
                        ASR::CaseStmt_t *cs = ASR::down_cast<ASR::CaseStmt_t>(
                            c);
                        for (size_t j = 0; j < cs->n_test; j++) {
                            Accesses t = expression_accesses(cs->m_test[j],
                                routine, pointers, outside);
                            acc.paths.insert(acc.paths.end(),
                                t.paths.begin(), t.paths.end());
                            acc.globals |= t.globals;
                            acc.everything |= t.everything;
                            acc.observable |= t.observable;
                        }
                    } else {
                        ASR::CaseStmt_Range_t *cr =
                            ASR::down_cast<ASR::CaseStmt_Range_t>(c);
                        for (ASR::expr_t *e : {cr->m_start, cr->m_end}) {
                            Accesses t = expression_accesses(e, routine,
                                pointers, outside);
                            acc.paths.insert(acc.paths.end(),
                                t.paths.begin(), t.paths.end());
                            acc.globals |= t.globals;
                            acc.everything |= t.everything;
                            acc.observable |= t.observable;
                        }
                    }
                }
                before(acc, s, emit, out, loc);
                State result = State::unreachable();
                State previous = State::unreachable();
                for (size_t i = 0; i < sel->n_body; i++) {
                    ASR::case_stmt_t *c = sel->m_body[i];
                    ASR::stmt_t **&cbody = ASR::is_a<ASR::CaseStmt_t>(*c)
                        ? ASR::down_cast<ASR::CaseStmt_t>(c)->m_body
                        : ASR::down_cast<ASR::CaseStmt_Range_t>(c)->m_body;
                    size_t &cn = ASR::is_a<ASR::CaseStmt_t>(*c)
                        ? ASR::down_cast<ASR::CaseStmt_t>(c)->n_body
                        : ASR::down_cast<ASR::CaseStmt_Range_t>(c)->n_body;
                    State in = s;
                    if (sel->m_enable_fall_through) in.join(previous);
                    Vec<ASR::stmt_t*> nb = new_body(cn);
                    State end = process_body(cbody, cn, in, emit,
                        emit ? &nb : nullptr);
                    if (emit) {
                        cbody = nb.p;
                        cn = nb.n;
                    }
                    previous = end;
                    result.join(end);
                }
                Vec<ASR::stmt_t*> nd = new_body(sel->n_default);
                State def = process_body(sel->m_default, sel->n_default, s,
                    emit, emit ? &nd : nullptr);
                if (emit) {
                    sel->m_default = nd.p;
                    sel->n_default = nd.n;
                    out->push_back(al, x);
                }
                result.join(def);
                return result;
            }
            case ASR::stmtType::DoLoop: {
                ASR::DoLoop_t *d = ASR::down_cast<ASR::DoLoop_t>(x);
                Accesses acc;
                AccessCollector c(acc, routine, pointers, outside);
                for (ASR::expr_t *e : {d->m_head.m_start, d->m_head.m_end,
                        d->m_head.m_increment}) {
                    if (e) c.visit_expr(*e);
                }
                before(acc, s, emit, out, loc);
                State after = loop(x, d->m_name, d->m_body, d->n_body,
                    nullptr, s, emit, out);
                if (d->n_orelse > 0) {
                    // A loop with an `else` part is not one this pass
                    // places waits in.
                    if (after.any) sync(after, emit, out, loc);
                }
                return after;
            }
            case ASR::stmtType::DoConcurrentLoop: {
                ASR::DoConcurrentLoop_t *d =
                    ASR::down_cast<ASR::DoConcurrentLoop_t>(x);
                Accesses acc;
                AccessCollector c(acc, routine, pointers, outside);
                for (size_t i = 0; i < d->n_head; i++) {
                    for (ASR::expr_t *e : {d->m_head[i].m_start,
                            d->m_head[i].m_end, d->m_head[i].m_increment}) {
                        if (e) c.visit_expr(*e);
                    }
                }
                before(acc, s, emit, out, loc);
                return loop(x, nullptr, d->m_body, d->n_body, nullptr, s,
                    emit, out);
            }
            case ASR::stmtType::WhileLoop: {
                ASR::WhileLoop_t *w = ASR::down_cast<ASR::WhileLoop_t>(x);
                State after = loop(x, w->m_name, w->m_body, w->n_body,
                    w->m_test, s, emit, out);
                if (w->n_orelse > 0 && after.any) {
                    sync(after, emit, out, loc);
                }
                return after;
            }
            case ASR::stmtType::BlockCall: {
                ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(x)->m_m);
                ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(sym);
                enter_block(block->m_symtab, block->m_body, block->n_body,
                    s, emit, loc);
                if (emit) out->push_back(al, x);
                return s;
            }
            case ASR::stmtType::AssociateBlockCall: {
                ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::AssociateBlockCall_t>(x)->m_m);
                ASR::AssociateBlock_t *block =
                    ASR::down_cast<ASR::AssociateBlock_t>(sym);
                enter_block(block->m_symtab, block->m_body, block->n_body,
                    s, emit, loc);
                if (emit) out->push_back(al, x);
                return s;
            }
            case ASR::stmtType::Exit:
            case ASR::stmtType::Cycle: {
                bool is_exit = ASR::is_a<ASR::Exit_t>(*x);
                const char *name = is_exit
                    ? ASR::down_cast<ASR::Exit_t>(x)->m_stmt_name
                    : ASR::down_cast<ASR::Cycle_t>(x)->m_stmt_name;
                Loop *l = find_loop(name);
                if (l == nullptr) {
                    // Leaving a construct that is not a loop: wait.
                    if (s.any) sync(s, emit, out, loc);
                } else {
                    leave_blocks(l->blocks, s, emit, out, loc);
                    if (!is_exit && l->sync_at_back_edge && s.any) {
                        sync(s, emit, out, loc);
                    }
                    (is_exit ? l->at_exit : l->at_cycle).join(s);
                }
                if (emit) out->push_back(al, x);
                return State::unreachable();
            }
            case ASR::stmtType::Return:
            case ASR::stmtType::GoTo: {
                if (s.any) sync(s, emit, out, loc);
                if (emit) out->push_back(al, x);
                return State::unreachable();
            }
            case ASR::stmtType::GoToTarget: {
                if (s.any) sync(s, emit, out, loc);
                if (emit) out->push_back(al, x);
                return State();
            }
            case ASR::stmtType::Stop:
            case ASR::stmtType::ErrorStop: {
                if (!s.stops.empty()) sync(s, emit, out, loc);
                if (emit) out->push_back(al, x);
                return State::unreachable();
            }
            case ASR::stmtType::ExplicitDeallocate:
            case ASR::stmtType::ImplicitDeallocate: {
                ASR::expr_t **vars;
                size_t n_vars;
                if (ASR::is_a<ASR::ExplicitDeallocate_t>(*x)) {
                    vars = ASR::down_cast<ASR::ExplicitDeallocate_t>(x)
                        ->m_vars;
                    n_vars = ASR::down_cast<ASR::ExplicitDeallocate_t>(x)
                        ->n_vars;
                } else {
                    vars = ASR::down_cast<ASR::ImplicitDeallocate_t>(x)
                        ->m_vars;
                    n_vars = ASR::down_cast<ASR::ImplicitDeallocate_t>(x)
                        ->n_vars;
                }
                deallocate(vars, n_vars, s, emit, out, loc);
                Accesses acc;
                AccessCollector c(acc, routine, pointers, outside);
                c.visit_stmt(*x);
                before(acc, s, emit, out, loc);
                if (emit) out->push_back(al, x);
                return s;
            }
            case ASR::stmtType::Associate: {
                // Binding a name accesses no values.
                if (emit) out->push_back(al, x);
                return s;
            }
            case ASR::stmtType::Assignment:
            case ASR::stmtType::Allocate:
            case ASR::stmtType::ReAlloc:
            case ASR::stmtType::Nullify:
            case ASR::stmtType::DebugCheckArrayBounds:
            case ASR::stmtType::SubroutineCall:
            case ASR::stmtType::IntrinsicImpureSubroutine:
            case ASR::stmtType::Expr:
            case ASR::stmtType::CPtrToPointer: {
                Accesses acc;
                AccessCollector c(acc, routine, pointers, outside);
                c.visit_stmt(*x);
                before(acc, s, emit, out, loc);
                if (emit) out->push_back(al, x);
                return s;
            }
            default: {
                // Input/output, and anything this pass does not know: wait
                // for everything.
                if (s.any) sync(s, emit, out, loc);
                if (emit) out->push_back(al, x);
                return s;
            }
        }
    }
};

// The launches have had their stop replays placed; nothing reads them from
// the launch any more.
class ReplayClearer : public ASR::BaseWalkVisitor<ReplayClearer> {
public:
    void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &x) {
        ASR::GpuKernelLaunch_t &xx = const_cast<ASR::GpuKernelLaunch_t&>(x);
        xx.m_stop_replay = nullptr;
        xx.n_stop_replay = 0;
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(
            ASRUtils::symbol_get_past_external(x.m_m));
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::AssociateBlock_t *block = ASR::down_cast<ASR::AssociateBlock_t>(
            ASRUtils::symbol_get_past_external(x.m_m));
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
};

void place_syncs(Allocator &al, SymbolTable *scope, ASR::stmt_t **&body,
        size_t &n_body, const Location &loc) {
    if (!launches(body, n_body)) return;
    PointerTargets targets;
    PointerCollector collector(targets);
    for (size_t i = 0; i < n_body; i++) collector.visit_stmt(*body[i]);
    RoutineSync sync(al, scope, targets);
    Vec<ASR::stmt_t*> result = sync.run(body, n_body, loc);
    body = result.p;
    n_body = result.n;
    ReplayClearer clearer;
    for (size_t i = 0; i < n_body; i++) clearer.visit_stmt(*body[i]);
}

void visit_scope(Allocator &al, SymbolTable *scope) {
    for (auto &item : scope->get_scope()) {
        ASR::symbol_t *sym = item.second;
        if (ASR::is_a<ASR::Function_t>(*sym)) {
            ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(sym);
            if (fn->m_gpu == nullptr) {
                place_syncs(al, fn->m_symtab, fn->m_body, fn->n_body,
                    fn->base.base.loc);
            }
            visit_scope(al, fn->m_symtab);
        } else if (ASR::is_a<ASR::Program_t>(*sym)) {
            ASR::Program_t *p = ASR::down_cast<ASR::Program_t>(sym);
            place_syncs(al, p->m_symtab, p->m_body, p->n_body,
                p->base.base.loc);
            visit_scope(al, p->m_symtab);
        } else if (ASR::is_a<ASR::Module_t>(*sym)) {
            visit_scope(al, ASR::down_cast<ASR::Module_t>(sym)->m_symtab);
        }
    }
}

} // namespace

void pass_gpu_launch_sync(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options) {
    if (!gpu_device_capabilities(pass_options).device_selected()) return;
    visit_scope(al, unit.m_symtab);
}

} // namespace LCompilers
