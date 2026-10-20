#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/gpu_affine.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/pass/gpu_loop_fusion.h>
#include <libasr/pass/gpu_loop_utils.h>
#include <libasr/pass/gpu_offload_designator.h>
#include <libasr/pass/intrinsic_function_registry.h>

#include <map>
#include <set>

namespace LCompilers {

/*
 * Producer-consumer fusion of parallel loops assigned to the device, by
 * recomputation.
 *
 * Two adjacent loop nests
 *
 *     do concurrent (j = 1:m, i = 1:n)           ! the producer P
 *         if (i <= n - 1) f(i, j) = <expression of i, j>
 *     end do
 *     do concurrent (j = 2:m, i = 2:n - 1)       ! the consumer C
 *         ... f(i - 1, j) ... f(i, j) ... (also inside pure functions)
 *     end do
 *
 * become one nest over the iterations of C in which every element of f that
 * C reads is the producer's expression evaluated at its subscripts. Each
 * array the producer writes is then never stored nor loaded: the bytes of
 * the intermediate array leave the memory traffic, and the producer's launch
 * goes. A value is computed again by every iteration that reads it, so the
 * transformation trades arithmetic for memory traffic; the cost model below
 * decides.
 *
 * Legality (anything not proven leaves the loops as they are):
 *
 * * The producer only assigns elements `f(i, j)` of real or integer arrays
 *   at its own iteration (the subscripts are its indices), each array once,
 *   possibly under IF tests (without ELSE); its expressions read neither
 *   these arrays nor anything the consumer assigns, except arrays that are
 *   renamed (below), nor its private variables.
 * * Neither an intermediate array nor a renamed one is a pointer, nor a
 *   component reached through a pointer, nor a variable that may be read
 *   or written through another name (may_be_aliased: TARGET, POINTER,
 *   EQUIVALENCE, a common block, ...): such reads would not be seen.
 * * Every function the producer's expressions and tests call is pure and
 *   self-contained: it names no variable but its own dummies and locals
 *   (gpu_function_self_contained), so its reads are those of its actual
 *   arguments. The consumer reads the producer's arrays only element by
 *   element at affine subscripts; a pure function the consumer passes such
 *   an array to is inlined first (gpu_loop_utils.h), and any other function
 *   it calls is self-contained. The consumer never writes those arrays.
 * * Every element the consumer reads is one the producer writes: on every
 *   path through the consumer's body (its loop bounds and the IF tests
 *   around the read, as affine constraints) the subscripts lie inside the
 *   producer's loop bounds and satisfy the producer's IF tests, which must
 *   be expressible exactly (gpu_affine.h). Every read is also inside the
 *   bounds of its array; when that is needed for the proof, the fused nest
 *   runs only when a run-time check finds the bounds of each intermediate
 *   array within the producer's loop bounds and the bounds its IF tests
 *   put on single indices.
 * * An array the consumer writes and the producer reads (the stage update
 *   `q(i, j) = ...` of a time step whose fluxes read q) would be read and
 *   written by different iterations of the fused nest. The consumer writes
 *   a copy of it instead, which is copied back after the nest, or which
 *   takes turns with the array (below). The consumer must write the array
 *   only at its own iteration, the same element on every path through its
 *   body, and read it only at that element before writing it. The array is
 *   allocatable; its copy is allocated before the outermost DO loop below,
 *   which must hold nothing that may change the array's allocation: the
 *   statements of those loops define neither the array as a whole nor the
 *   variable it is a component of (an assignment, also of a derived-type
 *   object, or an actual argument for a dummy that is not INTENT(IN), of a
 *   function too), and have no effects gpu_collect_effects does not
 *   model (ALLOCATE, ...).
 * * The producer's arrays keep the values the producer gave them, which the
 *   program may read after the loops. The two loops must be the body of DO
 *   loops (a time step and its stages), taken from the innermost outwards
 *   while each has a unit step, bounds without function calls, and no
 *   statement that leaves it early, and while the other statements of its
 *   body neither read nor write the producer's arrays, call only
 *   self-contained procedures, have effects gpu_collect_effects models
 *   (gpu_loop_utils.h), statements nested in BLOCKs included: they define
 *   variables only by assignment, as DO variables, as actual arguments of
 *   pure procedures (or of self-contained functions without input/output)
 *   for dummies that are not INTENT(IN), or as internal files; any other
 *   kind of statement or expression (input, IOSTAT=, ALLOCATE, a call of
 *   another procedure, an implied DO in input/output, ...) ends the chain.
 *   And they assign neither the loop's variable nor a variable the
 *   producer's bounds or tests read. The producer's bounds and tests must
 *   not read the loop's variable, and the bounds of the DO loops nested in
 *   it must read neither its variable nor anything assigned in it, so that
 *   they run the same iterations in each of its iterations. These bounds
 *   and tests read no variable that may change through another name of its
 *   storage (a pointer, a TARGET, a variable in EQUIVALENCE or a common
 *   block), which the assignments do not name. Then only the last iteration
 *   of those DO loops writes the values the program sees later, and the
 *   other iterations write values the next iteration overwrites unread. The
 *   original two loops therefore run in the last iteration and the fused
 *   nest in every other one. The last iteration is recognized by the loop's
 *   end, evaluated into a variable right before the loop, as the loop
 *   itself evaluates it once, when it starts: the statements of the loop
 *   may change what the end reads. Without such loops nothing is fused.
 * * A renamed array takes turns with its copy (ping-pong, `swap`) instead
 *   of being copied back when, in addition, no other statement of those DO
 *   loops reads or writes it and the consumer's bounds read neither their
 *   variables nor anything assigned in them or reached through another
 *   name, so that the consumer writes the same elements in every execution
 *   and the array and its copy agree on all the others.
 *
 * Cost model: every element of an intermediate array is computed by each
 * iteration that reads it instead of once. The fusion is made when the
 * extra arithmetic, the cost of the producer's expression (with the bodies
 * of the functions it calls) times the reads beyond the first, is at most
 * 64 operations per byte of memory traffic it removes (a store and a load
 * per element), the order of the ratio of arithmetic to memory throughput
 * of a GPU.
 */

namespace {

struct ProducerWrite {
    std::string key;
    ASR::ArrayItem_t *target = nullptr;
    ASR::expr_t *value = nullptr;
    // The loop of the producer each subscript of the target is the index
    // of.
    std::vector<int> loop_of_subscript;
    std::vector<ASR::expr_t*> guards;
    int64_t element_bytes = 4;
};

struct Read {
    std::string key;
    ASR::ArrayItem_t *item;
    std::vector<GpuAffine> subscripts;
    GpuDnf path;
    bool affine_ok = true;
};

// Whether a statement list holds a statement that leaves a loop or the
// procedure early.
bool has_early_exit(ASR::stmt_t **body, size_t n) {
    struct Find : public ASRUtils::BlockBodyWalkVisitor<Find> {
        bool found = false;
        void visit_Exit(const ASR::Exit_t &) { found = true; }
        void visit_Cycle(const ASR::Cycle_t &) { found = true; }
        void visit_Return(const ASR::Return_t &) { found = true; }
        void visit_GoTo(const ASR::GoTo_t &) { found = true; }
        void visit_GoToTarget(const ASR::GoToTarget_t &) { found = true; }
    } f;
    for (size_t i = 0; i < n; i++) f.visit_stmt(*body[i]);
    return f.found;
}

// Whether a variable may change through another name of its storage,
// which the analyses of a statement list do not see: a pointer, a variable
// with the TARGET attribute, a variable named where an address is taken or
// a pointer associated, on either side (`address_taken`; EQUIVALENCE is
// lowered so), a variable of a common block, or a volatile one. Named
// constants never change.
bool may_be_aliased(ASR::symbol_t *s,
        const std::set<ASR::symbol_t*> &address_taken) {
    s = ASRUtils::symbol_get_past_external(s);
    if (!ASR::is_a<ASR::Variable_t>(*s)) return false;
    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
    if (v->m_storage == ASR::storage_typeType::Parameter) return false;
    return v->m_target_attr || ASRUtils::is_pointer(v->m_type)
        || v->m_common_block || v->m_is_volatile || address_taken.count(s);
}

// Whether the array a designator names (a variable, or a component of one)
// may be read or written through another name, which the analyses do not
// see: a pointer component on the way, or a variable that may_be_aliased
// (a pointer, a TARGET, EQUIVALENCE, a common block, ...). A designator of
// another form (an element on the way, ...) is not analyzed and may be.
bool designator_may_be_aliased(ASR::expr_t *des,
        const std::set<ASR::symbol_t*> &address_taken) {
    while (ASR::is_a<ASR::StructInstanceMember_t>(*des)) {
        if (ASRUtils::is_pointer(ASRUtils::expr_type(des))) return true;
        des = ASR::down_cast<ASR::StructInstanceMember_t>(des)->m_v;
    }
    if (!ASR::is_a<ASR::Var_t>(*des)) return true;
    ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(des)->m_v);
    if (!ASR::is_a<ASR::Variable_t>(*s)) return true;
    return may_be_aliased(s, address_taken);
}

class LoopFusion {
public:
    Allocator &al;
    const PassOptions &po;
    // The variables involved in taking an address or associating a
    // pointer anywhere in the program (see pass_gpu_loop_fusion).
    std::set<ASR::symbol_t*> address_taken;
    GpuRarelyRun &rarely_run;

    // Statements to put before and after a statement of the list being
    // rebuilt: the allocation of a renamed array's copy goes before the
    // outermost DO loop the fusion relies on, its deallocation after it.
    std::map<ASR::stmt_t*, std::vector<ASR::stmt_t*>> before, after;

    LoopFusion(Allocator &al_, const PassOptions &po_,
            GpuRarelyRun &rarely_run_)
        : al(al_), po(po_), rarely_run(rarely_run_) {}

    struct Ancestor {
        ASR::stmt_t *stmt;          // a DoLoop, or another statement
        ASR::stmt_t **body;         // the list the child is in
        size_t n_body;
    };

    void visit_body(SymbolTable *scope, ASR::stmt_t **&body, size_t &n,
            std::vector<Ancestor> &ancestors) {
        // Nested lists first, so the innermost loops are fused first.
        for (size_t i = 0; i < n; i++) {
            visit_stmt(scope, body[i], ancestors);
        }
        Vec<ASR::stmt_t*> out;
        out.reserve(al, n);
        for (size_t i = 0; i < n; i++) {
            if (i + 1 < n) {
                ASR::stmt_t *r = try_fuse(scope, body, n, i, ancestors);
                if (r) {
                    out.push_back(al, r);
                    i++;
                    continue;
                }
            }
            out.push_back(al, body[i]);
        }
        Vec<ASR::stmt_t*> final_out;
        final_out.reserve(al, out.n);
        for (size_t i = 0; i < out.n; i++) {
            auto b = before.find(out[i]);
            if (b != before.end()) {
                for (auto *s : b->second) final_out.push_back(al, s);
                before.erase(b);
            }
            final_out.push_back(al, out[i]);
            auto a = after.find(out[i]);
            if (a != after.end()) {
                for (auto *s : a->second) final_out.push_back(al, s);
                after.erase(a);
            }
        }
        body = final_out.p;
        n = final_out.n;
    }

    void visit_stmt(SymbolTable *scope, ASR::stmt_t *s,
            std::vector<Ancestor> &ancestors) {
        switch (s->type) {
            case ASR::stmtType::DoLoop: {
                ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(s);
                ancestors.push_back({s, x->m_body, x->n_body});
                visit_body(scope, x->m_body, x->n_body, ancestors);
                ancestors.pop_back();
                break;
            }
            case ASR::stmtType::If: {
                ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                ancestors.push_back({s, nullptr, 0});
                visit_body(scope, x->m_body, x->n_body, ancestors);
                visit_body(scope, x->m_orelse, x->n_orelse, ancestors);
                ancestors.pop_back();
                break;
            }
            case ASR::stmtType::WhileLoop: {
                ASR::WhileLoop_t *x = ASR::down_cast<ASR::WhileLoop_t>(s);
                ancestors.push_back({s, nullptr, 0});
                visit_body(scope, x->m_body, x->n_body, ancestors);
                ancestors.pop_back();
                break;
            }
            case ASR::stmtType::Select: {
                ASR::Select_t *x = ASR::down_cast<ASR::Select_t>(s);
                ancestors.push_back({s, nullptr, 0});
                for (size_t i = 0; i < x->n_body; i++) {
                    if (ASR::is_a<ASR::CaseStmt_t>(*x->m_body[i])) {
                        ASR::CaseStmt_t *c = ASR::down_cast<ASR::CaseStmt_t>(
                            x->m_body[i]);
                        visit_body(scope, c->m_body, c->n_body, ancestors);
                    } else {
                        ASR::CaseStmt_Range_t *c =
                            ASR::down_cast<ASR::CaseStmt_Range_t>(
                                x->m_body[i]);
                        visit_body(scope, c->m_body, c->n_body, ancestors);
                    }
                }
                visit_body(scope, x->m_default, x->n_default, ancestors);
                ancestors.pop_back();
                break;
            }
            case ASR::stmtType::BlockCall: {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                if (ASR::is_a<ASR::Block_t>(*b)) {
                    ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                    ancestors.push_back({s, nullptr, 0});
                    visit_body(blk->m_symtab, blk->m_body, blk->n_body,
                        ancestors);
                    ancestors.pop_back();
                }
                break;
            }
            default:
                break;
        }
    }

    bool parse_producer(const GpuLoopNest &p,
            std::vector<ProducerWrite> &writes) {
        ASR::DoLoop_t *inner = p.innermost();
        std::function<bool(ASR::stmt_t**, size_t,
            std::vector<ASR::expr_t*>&)> walk =
            [&](ASR::stmt_t **body, size_t n,
                std::vector<ASR::expr_t*> &guards) -> bool {
            for (size_t i = 0; i < n; i++) {
                ASR::stmt_t *s = body[i];
                if (ASR::is_a<ASR::If_t>(*s)) {
                    ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                    if (x->n_orelse != 0) return false;
                    guards.push_back(x->m_test);
                    if (!walk(x->m_body, x->n_body, guards)) return false;
                    guards.pop_back();
                    continue;
                }
                if (!ASR::is_a<ASR::Assignment_t>(*s)) return false;
                ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(s);
                if (a->m_overloaded || a->m_realloc_lhs
                        || !ASR::is_a<ASR::ArrayItem_t>(*a->m_target)) {
                    return false;
                }
                ASR::ArrayItem_t *t = ASR::down_cast<ASR::ArrayItem_t>(
                    a->m_target);
                ProducerWrite w;
                w.key = gpu_array_key(t->m_v);
                if (w.key.empty() || t->n_args != p.rank()) return false;
                ASR::ttype_t *at = ASRUtils::expr_type(t->m_v);
                if (ASRUtils::is_pointer(at)
                        || designator_may_be_aliased(t->m_v, address_taken)) {
                    return false;
                }
                std::set<int> used;
                for (size_t d = 0; d < t->n_args; d++) {
                    ASR::expr_t *ix = t->m_args[d].m_right;
                    if (t->m_args[d].m_left || t->m_args[d].m_step
                            || ix == nullptr || !ASR::is_a<ASR::Var_t>(*ix)) {
                        return false;
                    }
                    int pos = p.position(ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::Var_t>(ix)->m_v));
                    if (pos < 0 || used.count(pos)) return false;
                    used.insert(pos);
                    w.loop_of_subscript.push_back(pos);
                }
                for (auto &o : writes) if (o.key == w.key) return false;
                ASR::ttype_t *et = ASRUtils::extract_type(at);
                if (!ASR::is_a<ASR::Real_t>(*et)
                        && !ASR::is_a<ASR::Integer_t>(*et)) {
                    return false;
                }
                w.element_bytes = ASRUtils::extract_kind_from_ttype_t(et);
                w.target = t;
                w.value = a->m_value;
                w.guards = guards;
                writes.push_back(w);
            }
            return true;
        };
        std::vector<ASR::expr_t*> guards;
        if (!walk(inner->m_body, inner->n_body, guards)) return false;
        return !writes.empty();
    }

    // Collects the reads of the producer's arrays in the (inlined)
    // consumer body with the path conditions they are made under.
    bool collect_reads(ASR::stmt_t **body, size_t n,
            const std::set<std::string> &keys, GpuAffineBuilder &b,
            const std::set<ASR::symbol_t*> &single, GpuDnf path,
            std::vector<Read> &reads) {
        struct Items : public ASR::BaseWalkVisitor<Items> {
            const std::set<std::string> *keys;
            std::vector<ASR::ArrayItem_t*> items;
            bool whole = false;
            void visit_ArrayItem(const ASR::ArrayItem_t &x) {
                std::string k = gpu_array_key(x.m_v);
                if (keys->count(k)) {
                    items.push_back(const_cast<ASR::ArrayItem_t*>(&x));
                    for (size_t i = 0; i < x.n_args; i++) {
                        if (x.m_args[i].m_right) visit_expr(*x.m_args[i].m_right);
                    }
                    return;
                }
                ASR::BaseWalkVisitor<Items>::visit_ArrayItem(x);
            }
            void visit_Var(const ASR::Var_t &x) {
                ASR::expr_t *e = const_cast<ASR::expr_t*>(&x.base);
                if (keys->count(gpu_array_key(e))) whole = true;
            }
            // The bounds and the size of an array are not its values.
            void visit_ArrayBound(const ASR::ArrayBound_t &x) {
                if (x.m_dim) visit_expr(*x.m_dim);
            }
            void visit_ArraySize(const ASR::ArraySize_t &x) {
                if (x.m_dim) visit_expr(*x.m_dim);
            }
            void visit_StructInstanceMember(
                    const ASR::StructInstanceMember_t &x) {
                ASR::expr_t *e = const_cast<ASR::expr_t*>(&x.base);
                if (keys->count(gpu_array_key(e))) {
                    whole = true;
                    return;
                }
                ASR::BaseWalkVisitor<Items>::visit_StructInstanceMember(x);
            }
        };
        auto scan = [&](ASR::expr_t *e) -> bool {
            if (e == nullptr) return true;
            // A derived-type object read as a whole reads its components.
            std::set<std::string> objects;
            gpu_collect_array_reads(e, objects);
            for (auto &k : keys) {
                if (gpu_key_touched_whole(objects, k)) return false;
            }
            Items it;
            it.keys = &keys;
            it.visit_expr(*e);
            if (it.whole) return false;
            for (ASR::ArrayItem_t *x : it.items) {
                Read r;
                r.key = gpu_array_key(x->m_v);
                r.item = x;
                r.path = path;
                for (size_t d = 0; d < x->n_args; d++) {
                    GpuAffine a;
                    if (x->m_args[d].m_left || x->m_args[d].m_step
                            || x->m_args[d].m_right == nullptr
                            || !b.affine(x->m_args[d].m_right, a)) {
                        return false;
                    }
                    r.subscripts.push_back(a);
                }
                reads.push_back(r);
            }
            return true;
        };
        for (size_t i = 0; i < n; i++) {
            ASR::stmt_t *s = body[i];
            switch (s->type) {
                case ASR::stmtType::Assignment: {
                    ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(s);
                    if (!scan(a->m_value)) return false;
                    if (ASR::is_a<ASR::ArrayItem_t>(*a->m_target)) {
                        ASR::ArrayItem_t *t = ASR::down_cast<ASR::ArrayItem_t>(
                            a->m_target);
                        if (keys.count(gpu_array_key(t->m_v))) return false;
                        for (size_t d = 0; d < t->n_args; d++) {
                            if (!scan(t->m_args[d].m_right)) return false;
                        }
                    } else if (ASR::is_a<ASR::Var_t>(*a->m_target)) {
                        ASR::symbol_t *v = ASRUtils::symbol_get_past_external(
                            ASR::down_cast<ASR::Var_t>(a->m_target)->m_v);
                        GpuAffine val;
                        if (single.count(v) && b.affine(a->m_value, val)) {
                            b.bindings[v] = val;
                        }
                    } else if (!scan(a->m_target)) {
                        return false;
                    }
                    break;
                }
                case ASR::stmtType::If: {
                    ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                    if (!scan(x->m_test)) return false;
                    if (!collect_reads(x->m_body, x->n_body, keys, b, single,
                            gpu_dnf_and(path, b.condition(x->m_test, true)),
                            reads)) {
                        return false;
                    }
                    if (!collect_reads(x->m_orelse, x->n_orelse, keys, b,
                            single, gpu_dnf_and(path,
                                b.condition(x->m_test, false)), reads)) {
                        return false;
                    }
                    break;
                }
                case ASR::stmtType::Select: {
                    ASR::Select_t *x = ASR::down_cast<ASR::Select_t>(s);
                    if (!scan(x->m_test)) return false;
                    for (size_t k = 0; k < x->n_body; k++) {
                        ASR::stmt_t **cb;
                        size_t cn;
                        if (ASR::is_a<ASR::CaseStmt_t>(*x->m_body[k])) {
                            ASR::CaseStmt_t *c = ASR::down_cast<ASR::CaseStmt_t>(
                                x->m_body[k]);
                            for (size_t t = 0; t < c->n_test; t++) {
                                if (!scan(c->m_test[t])) return false;
                            }
                            cb = c->m_body;
                            cn = c->n_body;
                        } else {
                            ASR::CaseStmt_Range_t *c =
                                ASR::down_cast<ASR::CaseStmt_Range_t>(
                                    x->m_body[k]);
                            if (!scan(c->m_start) || !scan(c->m_end)) {
                                return false;
                            }
                            cb = c->m_body;
                            cn = c->n_body;
                        }
                        if (!collect_reads(cb, cn, keys, b, single, path,
                                reads)) {
                            return false;
                        }
                    }
                    if (!collect_reads(x->m_default, x->n_default, keys, b,
                            single, path, reads)) {
                        return false;
                    }
                    break;
                }
                case ASR::stmtType::BlockCall: {
                    ASR::symbol_t *bs = ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                    if (!ASR::is_a<ASR::Block_t>(*bs)) return false;
                    ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(bs);
                    if (!collect_reads(blk->m_body, blk->n_body, keys, b,
                            single, path, reads)) {
                        return false;
                    }
                    break;
                }
                default: {
                    // Any other statement must not read the arrays.
                    std::set<std::string> r;
                    gpu_collect_array_reads(&s, 1, r);
                    for (auto &k : keys) {
                        if (gpu_key_touched(r, k)) return false;
                    }
                    break;
                }
            }
        }
        return true;
    }

    // Whether every path through `body` assigns `key` at the subscripts
    // `idx` (the same Var symbols), and the array is assigned nowhere
    // else, nor read.
    struct WriteShape {
        std::vector<ASR::symbol_t*> subscripts;
        bool consistent = true;
        bool read = false;
    };

    bool must_write(ASR::stmt_t **body, size_t n, const std::string &key,
            WriteShape &ws) {
        bool written = false;
        for (size_t i = 0; i < n; i++) {
            ASR::stmt_t *s = body[i];
            if (ASR::is_a<ASR::Assignment_t>(*s)) {
                ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(s);
                if (ASR::is_a<ASR::ArrayItem_t>(*a->m_target)
                        && gpu_array_key(ASR::down_cast<ASR::ArrayItem_t>(
                            a->m_target)->m_v) == key) {
                    ASR::ArrayItem_t *t = ASR::down_cast<ASR::ArrayItem_t>(
                        a->m_target);
                    std::vector<ASR::symbol_t*> sub;
                    for (size_t d = 0; d < t->n_args; d++) {
                        ASR::expr_t *ix = t->m_args[d].m_right;
                        if (t->m_args[d].m_left || t->m_args[d].m_step
                                || ix == nullptr
                                || !ASR::is_a<ASR::Var_t>(*ix)) {
                            ws.consistent = false;
                            return false;
                        }
                        sub.push_back(ASRUtils::symbol_get_past_external(
                            ASR::down_cast<ASR::Var_t>(ix)->m_v));
                    }
                    if (ws.subscripts.empty()) ws.subscripts = sub;
                    if (ws.subscripts != sub) ws.consistent = false;
                    written = true;
                    std::set<std::string> r;
                    gpu_collect_array_reads(a->m_value, r);
                    if (gpu_key_touched(r, key)) ws.read = true;
                    continue;
                }
                std::set<std::string> r;
                gpu_collect_array_reads(&s, 1, r);
                if (gpu_key_touched(r, key)) ws.read = true;
            } else if (ASR::is_a<ASR::If_t>(*s)) {
                ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                std::set<std::string> r;
                gpu_collect_array_reads(x->m_test, r);
                if (gpu_key_touched(r, key)) ws.read = true;
                bool a = must_write(x->m_body, x->n_body, key, ws);
                bool b = must_write(x->m_orelse, x->n_orelse, key, ws);
                if (a && b) written = true;
            } else if (ASR::is_a<ASR::Select_t>(*s)) {
                ASR::Select_t *x = ASR::down_cast<ASR::Select_t>(s);
                std::set<std::string> r;
                gpu_collect_array_reads(x->m_test, r);
                if (gpu_key_touched(r, key)) ws.read = true;
                bool all = x->n_default > 0;
                for (size_t k = 0; k < x->n_body; k++) {
                    ASR::stmt_t **cb;
                    size_t cn;
                    if (ASR::is_a<ASR::CaseStmt_t>(*x->m_body[k])) {
                        ASR::CaseStmt_t *c = ASR::down_cast<ASR::CaseStmt_t>(
                            x->m_body[k]);
                        cb = c->m_body;
                        cn = c->n_body;
                    } else {
                        ASR::CaseStmt_Range_t *c =
                            ASR::down_cast<ASR::CaseStmt_Range_t>(x->m_body[k]);
                        cb = c->m_body;
                        cn = c->n_body;
                    }
                    if (!must_write(cb, cn, key, ws)) all = false;
                }
                if (!must_write(x->m_default, x->n_default, key, ws)) {
                    all = false;
                }
                if (all) written = true;
            } else if (ASR::is_a<ASR::BlockCall_t>(*s)) {
                ASR::symbol_t *bs = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                if (!ASR::is_a<ASR::Block_t>(*bs)) {
                    ws.consistent = false;
                    return false;
                }
                ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(bs);
                if (must_write(blk->m_body, blk->n_body, key, ws)) {
                    written = true;
                }
            } else {
                std::set<std::string> r;
                gpu_collect_array_reads(&s, 1, r);
                GpuEffects e;
                gpu_collect_effects(&s, 1, e);
                if (gpu_key_touched(r, key) || gpu_key_touched(e.arrays, key)
                        || e.unknown) {
                    ws.consistent = false;
                }
            }
        }
        return written;
    }

    // Whether every read of `key` in `body` is of the element the body
    // writes (`subscripts`) and comes before every write on its path, in
    // the order the statements run: such a read sees the value from before
    // the loop, which the array keeps when the writes go to a copy. The
    // branches of an IF or a SELECT CASE are alternatives.
    bool reads_before_writes(ASR::stmt_t **body, size_t n,
            const std::string &key,
            const std::vector<ASR::symbol_t*> &subscripts) {
        bool ok = true;
        auto identity = [&](ASR::ArrayItem_t *x) {
            if (x->n_args != subscripts.size()) return false;
            for (size_t d = 0; d < x->n_args; d++) {
                ASR::expr_t *ix = x->m_args[d].m_right;
                if (x->m_args[d].m_left || x->m_args[d].m_step || !ix
                        || !ASR::is_a<ASR::Var_t>(*ix)
                        || ASRUtils::symbol_get_past_external(
                            ASR::down_cast<ASR::Var_t>(ix)->m_v)
                            != subscripts[d]) {
                    return false;
                }
            }
            return true;
        };
        auto reads = [&](ASR::expr_t *e, bool written) {
            if (e == nullptr) return;
            // A derived-type object read as a whole reads the array.
            std::set<std::string> objects;
            gpu_collect_array_reads(e, objects);
            if (gpu_key_touched_whole(objects, key)) ok = false;
            struct R : public ASR::BaseWalkVisitor<R> {
                const std::string *key;
                std::vector<ASR::ArrayItem_t*> items;
                bool whole = false;
                void visit_ArrayItem(const ASR::ArrayItem_t &x) {
                    if (gpu_array_key(x.m_v) == *key) {
                        items.push_back(const_cast<ASR::ArrayItem_t*>(&x));
                        return;
                    }
                    ASR::BaseWalkVisitor<R>::visit_ArrayItem(x);
                }
                void visit_Var(const ASR::Var_t &x) {
                    if (gpu_array_key(const_cast<ASR::expr_t*>(&x.base))
                            == *key) {
                        whole = true;
                    }
                }
                void visit_StructInstanceMember(
                        const ASR::StructInstanceMember_t &x) {
                    if (gpu_array_key(const_cast<ASR::expr_t*>(&x.base))
                            == *key) {
                        whole = true;
                        return;
                    }
                    ASR::BaseWalkVisitor<R>::visit_StructInstanceMember(x);
                }
            } r;
            r.key = &key;
            r.visit_expr(*e);
            if (r.whole) ok = false;
            for (auto *x : r.items) {
                if (written || !identity(x)) ok = false;
            }
        };
        // Returns whether the array may have been written once the list
        // has run, given whether it may have been before.
        std::function<bool(ASR::stmt_t**, size_t, bool)> walk =
                [&](ASR::stmt_t **b, size_t m, bool written) -> bool {
            for (size_t i = 0; i < m && ok; i++) {
                ASR::stmt_t *s = b[i];
                if (ASR::is_a<ASR::Assignment_t>(*s)) {
                    ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(s);
                    reads(a->m_value, written);
                    if (ASR::is_a<ASR::ArrayItem_t>(*a->m_target)) {
                        ASR::ArrayItem_t *t = ASR::down_cast<ASR::ArrayItem_t>(
                            a->m_target);
                        for (size_t d = 0; d < t->n_args; d++) {
                            reads(t->m_args[d].m_right, written);
                        }
                        if (gpu_array_key(t->m_v) == key) written = true;
                    }
                } else if (ASR::is_a<ASR::If_t>(*s)) {
                    ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                    reads(x->m_test, written);
                    bool w1 = walk(x->m_body, x->n_body, written);
                    bool w2 = walk(x->m_orelse, x->n_orelse, written);
                    written = w1 || w2;
                } else if (ASR::is_a<ASR::Select_t>(*s)) {
                    ASR::Select_t *x = ASR::down_cast<ASR::Select_t>(s);
                    reads(x->m_test, written);
                    bool out = walk(x->m_default, x->n_default, written);
                    for (size_t k = 0; k < x->n_body; k++) {
                        if (ASR::is_a<ASR::CaseStmt_t>(*x->m_body[k])) {
                            ASR::CaseStmt_t *c = ASR::down_cast<ASR::CaseStmt_t>(
                                x->m_body[k]);
                            for (size_t t = 0; t < c->n_test; t++) {
                                reads(c->m_test[t], written);
                            }
                            out = walk(c->m_body, c->n_body, written) || out;
                        } else {
                            ASR::CaseStmt_Range_t *c =
                                ASR::down_cast<ASR::CaseStmt_Range_t>(
                                    x->m_body[k]);
                            reads(c->m_start, written);
                            reads(c->m_end, written);
                            out = walk(c->m_body, c->n_body, written) || out;
                        }
                    }
                    written = out;
                } else if (ASR::is_a<ASR::BlockCall_t>(*s)) {
                    ASR::symbol_t *bs = ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                    if (!ASR::is_a<ASR::Block_t>(*bs)) {
                        ok = false;
                        return written;
                    }
                    ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(bs);
                    written = walk(blk->m_body, blk->n_body, written);
                } else {
                    std::set<std::string> r;
                    gpu_collect_array_reads(&s, 1, r);
                    if (gpu_key_touched(r, key)) ok = false;
                }
            }
            return written;
        };
        walk(body, n, false);
        return ok;
    }

    // The designator expression of an array key in a statement list.
    ASR::expr_t *find_designator(ASR::stmt_t **body, size_t n,
            const std::string &key) {
        struct Find : public ASRUtils::BlockBodyWalkVisitor<Find> {
            const std::string *key;
            ASR::expr_t *found = nullptr;
            void visit_ArrayItem(const ASR::ArrayItem_t &x) {
                if (!found && gpu_array_key(x.m_v) == *key) found = x.m_v;
                ASRUtils::BlockBodyWalkVisitor<Find>::visit_ArrayItem(x);
            }
        } f;
        f.key = &key;
        for (size_t i = 0; i < n && !f.found; i++) f.visit_stmt(*body[i]);
        return f.found;
    }

    ASR::stmt_t *try_fuse(SymbolTable *scope, ASR::stmt_t **body, size_t n,
            size_t i, std::vector<Ancestor> &ancestors) {
        GpuLoopNest p, c;
        if (!gpu_loop_nest(body[i], p) || !gpu_loop_nest(body[i + 1], c)
                || p.rank() != c.rank()) {
            return nullptr;
        }
        if (!gpu_simple_body(p.innermost()->m_body, p.innermost()->n_body)
                || !gpu_simple_body(c.innermost()->m_body,
                    c.innermost()->n_body)) {
            return nullptr;
        }
        std::vector<ProducerWrite> writes;
        if (!parse_producer(p, writes)) return nullptr;
        std::set<std::string> xkeys;
        for (auto &w : writes) xkeys.insert(w.key);

        // What the producer's expressions read.
        std::set<std::string> producer_reads;
        std::set<ASR::symbol_t*> producer_syms;
        for (auto &w : writes) {
            gpu_collect_array_reads(w.value, producer_reads);
            gpu_collect_symbols(w.value, producer_syms);
            for (auto *g : w.guards) gpu_collect_symbols(g, producer_syms);
            // A call of a function that names variables other than its
            // arguments and locals reads what the collections above do not
            // see.
            if (!gpu_calls_self_contained(w.value)) return nullptr;
            for (auto *g : w.guards) {
                if (!gpu_calls_self_contained(g)) return nullptr;
            }
            // An impure call.
            struct Calls : public ASR::BaseWalkVisitor<Calls> {
                bool ok = true;
                void visit_FunctionCall(const ASR::FunctionCall_t &x) {
                    ASR::symbol_t *f = ASRUtils::symbol_get_past_external(
                        x.m_name);
                    if (!ASR::is_a<ASR::Function_t>(*f)) {
                        ok = false;
                    } else {
                        ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(f);
                        ASR::FunctionType_t *ft = ASRUtils::get_FunctionType(fn);
                        if (!ft->m_pure && !ft->m_elemental) ok = false;
                    }
                    ASR::BaseWalkVisitor<Calls>::visit_FunctionCall(x);
                }
            } calls;
            calls.visit_expr(*w.value);
            if (!calls.ok) return nullptr;
        }
        for (auto &k : xkeys) {
            if (gpu_key_touched(producer_reads, k)) return nullptr;
        }
        for (ASR::symbol_t *s : p.privates) {
            if (producer_syms.count(s)) return nullptr;
        }

        // The consumer, copied, with the calls that take an intermediate
        // array inlined.
        ASR::DoLoop_t *cin = c.innermost();
        GpuDuplicator dup(al, scope);
        std::vector<std::pair<SymbolTable*, std::string>> added;
        dup.added = &added;
        Vec<ASR::stmt_t*> cbody;
        dup.duplicate_body(cin->m_body, cin->n_body, cbody);
        auto rollback = [&]() {
            for (auto it = added.rbegin(); it != added.rend(); ++it) {
                if (it->first->get_symbol(it->second)) {
                    it->first->erase_symbol(it->second);
                }
            }
        };
        if (!dup.ok) {
            rollback();
            return nullptr;
        }
        GpuInliner inl(al, [&](ASR::FunctionCall_t *call) {
            for (size_t k = 0; k < call->n_args; k++) {
                ASR::expr_t *a = call->m_args[k].m_value;
                if (a && xkeys.count(gpu_array_key(a))) return true;
            }
            return false;
        });
        ASR::stmt_t **cb = cbody.p;
        size_t cn = cbody.n;
        if (!inl.run(scope, cb, cn)) {
            inl.rollback();
            rollback();
            return nullptr;
        }
        auto fail = [&]() -> ASR::stmt_t* {
            inl.rollback();
            rollback();
            return nullptr;
        };

        // A call left in the consumer reads only what its arguments name,
        // so that the reads of the intermediate arrays are all visible.
        if (!gpu_calls_self_contained(cb, cn)) return fail();
        // What the consumer assigns.
        GpuEffects c_effects;
        gpu_collect_effects(cb, cn, c_effects);
        if (c_effects.unknown) return fail();
        const std::set<ASR::symbol_t*> &c_scalars = c_effects.scalars;
        const std::set<std::string> &c_arrays = c_effects.arrays;
        for (auto &k : xkeys) if (c_arrays.count(k)) return fail();
        for (ASR::symbol_t *s : producer_syms) {
            if (c_scalars.count(s)) return fail();
        }
        for (ASR::symbol_t *s : c.privates) {
            if (producer_syms.count(s)) return fail();
        }
        // In the fused nest a variable the producer's expressions, tests
        // or bounds name would be the consumer's index of that name (DO
        // CONCURRENT indices are the variables of the scope), not the
        // value it has outside the consumer. The producer's own indices
        // are replaced by the consumer's subscripts.
        {
            std::set<ASR::symbol_t*> named = producer_syms;
            for (size_t d = 0; d < p.rank(); d++) {
                gpu_collect_symbols(p.starts[d], named);
                gpu_collect_symbols(p.ends[d], named);
            }
            for (ASR::symbol_t *s : named) {
                if (c.position(s) >= 0 && p.position(s) < 0) return fail();
            }
        }
        // Arrays the consumer writes and the producer reads: renamed.
        std::vector<std::string> renamed;
        for (auto &k : c_arrays) {
            if (producer_reads.count(k)) renamed.push_back(k);
            // The copy would replace only the reads of the array itself.
            if (gpu_key_touched_whole(producer_reads, k)) return fail();
        }
        std::map<std::string, WriteShape> shapes;
        for (auto &k : renamed) {
            WriteShape ws;
            bool all = must_write(cb, cn, k, ws);
            if (!all || !ws.consistent) return fail();
            if (ws.read && !reads_before_writes(cb, cn, k, ws.subscripts)) {
                return fail();
            }
            for (ASR::symbol_t *s : ws.subscripts) {
                if (c.position(s) < 0) return fail();
            }
            shapes[k] = ws;
        }

        // The affine facts: the consumer's loop bounds.
        std::set<ASR::symbol_t*> single;
        for (auto &kv : c_effects.definitions) {
            if (kv.second == 1) single.insert(kv.first);
        }
        auto invariant = [&](ASR::symbol_t *s) {
            if (c.position(s) >= 0) return true;
            return c_scalars.count(s) == 0 && ASR::is_a<ASR::Variable_t>(*s);
        };
        GpuAffineBuilder cb_builder;
        cb_builder.invariant_variable = invariant;
        GpuConstraints bounds;
        for (size_t d = 0; d < c.rank(); d++) {
            GpuAffine v = GpuAffine::atom(gpu_affine_key(
                ASRUtils::EXPR(ASR::make_Var_t(al, cin->base.base.loc,
                    c.vars[d]))));
            GpuAffine lo, hi;
            if (!cb_builder.affine(c.starts[d], lo)
                    || !cb_builder.affine(c.ends[d], hi)) {
                return fail();
            }
            bounds.push_back(v.plus(lo, -1));
            bounds.push_back(hi.plus(v, -1));
        }
        std::vector<Read> reads;
        if (!collect_reads(cb, cn, xkeys, cb_builder, single,
                GpuDnf{bounds}, reads)) {
            return fail();
        }
        if (reads.empty()) return fail();

        // Every read is of an element the producer writes.
        std::vector<GpuAffine> plo(p.rank()), phi(p.rank());
        GpuAffineBuilder pb;
        pb.invariant_variable = [&](ASR::symbol_t *s) {
            return c_scalars.count(s) == 0 && p.position(s) < 0
                && ASR::is_a<ASR::Variable_t>(*s);
        };
        for (size_t d = 0; d < p.rank(); d++) {
            if (!pb.affine(p.starts[d], plo[d])
                    || !pb.affine(p.ends[d], phi[d])) {
                return fail();
            }
        }
        // A read of an element is inside the bounds of the array, as in any
        // valid program. When the producer writes every element of an
        // intermediate array, every read is covered; whether it does
        // depends on how the array was allocated, which the fused nest
        // checks at run time when the reads cannot be proven otherwise:
        // the bounds of the array lie within the loop bounds and the tests
        // on single indices of the producer (`box`).
        std::map<std::string, GpuConstraints> box;
        // Equalities of the lower bounds of the intermediate arrays with
        // the first index the producer writes, and the value of each
        // pinned bound (by its atom key).
        std::map<std::string, GpuConstraints> pins;
        std::map<std::string, GpuAffine> pinned;
        GpuAffineBuilder ab = cb_builder;
        ab.bindings.clear();
        auto bound_atom = [&](ASR::expr_t *des, size_t d, bool upper,
                GpuAffine &out) {
            ASR::expr_t *e = ASRUtils::EXPR(ASR::make_ArrayBound_t(al,
                des->base.loc, des, gpu_i32(al, des->base.loc, d + 1),
                ASRUtils::TYPE(ASR::make_Integer_t(al, des->base.loc, 4)),
                upper ? ASR::arrayboundType::UBound
                    : ASR::arrayboundType::LBound, nullptr));
            return ab.affine(e, out);
        };
        for (auto &w : writes) {
            GpuConstraints bx;
            GpuAffineBuilder g0;
            g0.invariant_variable = [&](ASR::symbol_t *s) {
                return p.position(s) >= 0 || pb.invariant_variable(s);
            };
            std::vector<std::vector<GpuAffine>> lowers(w.loop_of_subscript.size()),
                uppers(w.loop_of_subscript.size());
            for (size_t d = 0; d < w.loop_of_subscript.size(); d++) {
                int k = w.loop_of_subscript[d];
                lowers[d].push_back(plo[k]);
                uppers[d].push_back(phi[k]);
            }
            for (ASR::expr_t *g : w.guards) {
                if (!g0.exact_condition(g)) continue;
                GpuDnf gd = g0.condition(g, true);
                if (gd.size() != 1) continue;
                for (auto &c : gd[0]) {
                    for (size_t d = 0; d < w.loop_of_subscript.size(); d++) {
                        int k = w.loop_of_subscript[d];
                        std::string vk = gpu_affine_key(ASRUtils::EXPR(
                            ASR::make_Var_t(al, cin->base.base.loc,
                                p.vars[k])));
                        int64_t coef = c.coefficient(vk);
                        if (coef != 1 && coef != -1) continue;
                        bool only = true;
                        for (size_t m = 0; m < p.rank(); m++) {
                            if ((int)m == k) continue;
                            std::string vm = gpu_affine_key(ASRUtils::EXPR(
                                ASR::make_Var_t(al, cin->base.base.loc,
                                    p.vars[m])));
                            if (c.coefficient(vm) != 0) only = false;
                        }
                        if (!only) continue;
                        GpuAffine rest = c;
                        rest.terms.erase(vk);
                        if (coef == 1) {
                            lowers[d].push_back(rest.scaled(-1));
                        } else {
                            uppers[d].push_back(rest);
                        }
                    }
                }
            }
            GpuConstraints pn;
            for (size_t d = 0; d < w.loop_of_subscript.size(); d++) {
                GpuAffine lb, ub;
                if (!bound_atom(w.target->m_v, d, false, lb)
                        || !bound_atom(w.target->m_v, d, true, ub)) {
                    return fail();
                }
                for (auto &l : lowers[d]) bx.push_back(lb.plus(l, -1));
                for (auto &u : uppers[d]) bx.push_back(u.plus(ub, -1));
                // The lower bound pinned to the producer's first index.
                GpuAffine eq = lb.plus(plo[w.loop_of_subscript[d]], -1);
                pn.push_back(eq);
                pn.push_back(eq.scaled(-1));
                pinned[lb.terms.begin()->first] = plo[w.loop_of_subscript[d]];
            }
            box[w.key] = bx;
            pins[w.key] = pn;
        }
        auto prove_reads = [&](bool assume, bool pin) -> bool {
            for (auto &r : reads) {
                const ProducerWrite *w = nullptr;
                for (auto &x : writes) if (x.key == r.key) w = &x;
                if (w == nullptr || r.subscripts.size()
                        != w->loop_of_subscript.size()) {
                    return false;
                }
                GpuConstraints extra;
                for (size_t d = 0; d < r.subscripts.size(); d++) {
                    GpuAffine lb, ub;
                    if (!bound_atom(w->target->m_v, d, false, lb)
                            || !bound_atom(w->target->m_v, d, true, ub)) {
                        return false;
                    }
                    extra.push_back(r.subscripts[d].plus(lb, -1));
                    extra.push_back(ub.plus(r.subscripts[d], -1));
                }
                if (assume) {
                    for (auto &bk : box) {
                        extra.insert(extra.end(), bk.second.begin(),
                            bk.second.end());
                    }
                }
                if (pin) {
                    for (auto &bk : pins) {
                        extra.insert(extra.end(), bk.second.begin(),
                            bk.second.end());
                    }
                }
                GpuDnf facts;
                for (auto &conj : r.path) {
                    GpuConstraints cc = conj;
                    cc.insert(cc.end(), extra.begin(), extra.end());
                    facts.push_back(cc);
                }
                // Subscript d is the index of producer loop
                // loop_of_subscript[d].
                GpuAffineBuilder gb;
                gb.invariant_variable = pb.invariant_variable;
                for (size_t d = 0; d < r.subscripts.size(); d++) {
                    int k = w->loop_of_subscript[d];
                    const GpuAffine &e = r.subscripts[d];
                    if (!gpu_affine_proves(facts, e.plus(plo[k], -1))
                            || !gpu_affine_proves(facts, phi[k].plus(e, -1))) {
                        return false;
                    }
                    gb.bindings[p.vars[k]] = e;
                }
                for (ASR::expr_t *g : w->guards) {
                    if (!gb.exact_condition(g)) return false;
                    GpuDnf gd = gb.condition(g, true);
                    if (gd.size() != 1) return false;
                    for (auto &goal : gd[0]) {
                        if (!gpu_affine_proves(facts, goal)) return false;
                    }
                }
            }
            return true;
        };
        // Pinning the lower bounds makes the subscripts of the inlined
        // bodies constant offsets of the indices, which the passes after
        // this one can reason about; it is checked at run time like the
        // bounds (an array allocated otherwise runs the original loops).
        for (auto &kv : pb.atoms) ab.atoms.insert(kv);
        bool shifted = false;
        for (auto &r : reads) {
            for (auto &e : r.subscripts) {
                for (auto &t : e.terms) {
                    if (pinned.count(t.first)) shifted = true;
                }
            }
        }
        bool assume = true, pin = shifted;
        if (!(pin && prove_reads(true, true))) {
            pin = false;
            if (prove_reads(false, false)) {
                assume = false;
            } else if (!prove_reads(true, false)) {
                return fail();
            }
        }
        // The run-time check of the assumed bounds.
        std::vector<ASR::expr_t*> checks;
        auto add_checks = [&](std::map<std::string, GpuConstraints> &cs) {
            for (auto &bk : cs) {
                for (auto &c : bk.second) {
                    ASR::expr_t *e = gpu_affine_to_expr(al,
                        cin->base.base.loc, c, ab.atoms);
                    if (e == nullptr) return false;
                    checks.push_back(e);
                }
            }
            return true;
        };
        if (assume && !add_checks(box)) return fail();
        if (pin && !add_checks(pins)) return fail();
        if (!pin) pinned.clear();

        // The cost model.
        int64_t extra_ops = 0, saved_bytes = 0;
        for (auto &w : writes) {
            int64_t nreads = 0;
            for (auto &r : reads) if (r.key == w.key) nreads++;
            if (nreads == 0) continue;
            extra_ops += (nreads - 1) * gpu_expr_cost(w.value);
            saved_bytes += 2 * w.element_bytes;
        }
        if (extra_ops > 64 * saved_bytes) return fail();

        // The DO loops whose last iteration alone needs the intermediate
        // arrays stored.
        std::vector<ASR::DoLoop_t*> loops;
        std::vector<ASR::stmt_t*> loop_stmts;
        // What the statements of those loops other than the two assign and
        // read.
        std::set<std::string> other_writes, other_reads;
        // The variables they assign, and those that designators they assign
        // start from (`x` for `x(k) = ...` or `x%c = ...`).
        std::set<ASR::symbol_t*> assigned_in_loops, based_in_loops;
        {
            ASR::stmt_t *child_a = body[i], *child_b = body[i + 1];
            ASR::stmt_t **list = body;
            size_t list_n = n;
            for (size_t k = ancestors.size(); k-- > 0;) {
                Ancestor &an = ancestors[k];
                if (!ASR::is_a<ASR::DoLoop_t>(*an.stmt)) break;
                ASR::DoLoop_t *l = ASR::down_cast<ASR::DoLoop_t>(an.stmt);
                if (l->m_head.m_v == nullptr || l->m_head.m_end == nullptr
                        || !ASR::is_a<ASR::Var_t>(*l->m_head.m_v)) {
                    break;
                }
                if (l->m_head.m_increment) {
                    int64_t inc;
                    if (!ASRUtils::extract_value(ASRUtils::expr_value(
                            l->m_head.m_increment), inc) || inc != 1) {
                        break;
                    }
                }
                if (!gpu_expr_duplicable(l->m_head.m_end)
                        || (l->m_head.m_start
                            && !gpu_expr_duplicable(l->m_head.m_start))) {
                    break;
                }
                if (has_early_exit(list, list_n)) break;
                // Nothing else in the loop's body touches the intermediate
                // arrays, the variables the producer's bounds and tests
                // read or the loop's variable.
                GpuEffects other;
                std::set<std::string> ra;
                for (size_t m = 0; m < list_n; m++) {
                    if (list[m] == child_a || list[m] == child_b) continue;
                    gpu_collect_effects(&list[m], 1, other);
                    gpu_collect_array_reads(&list[m], 1, ra);
                    // What a call reads beyond its arguments is not seen.
                    if (!gpu_calls_self_contained(&list[m], 1)) {
                        other.unknown = true;
                    }
                }
                const std::set<std::string> &wa = other.arrays;
                // What the loops nested in it, which hold the fused pair,
                // assign.
                GpuEffects all = other;
                gpu_collect_effects(&child_a, 1, all);
                if (child_b != child_a) {
                    gpu_collect_effects(&child_b, 1, all);
                }
                const std::set<ASR::symbol_t*> &sc_all = all.scalars;
                const std::set<ASR::symbol_t*> &ba_all = all.bases;
                // A statement with effects the collections do not see
                // ends the chain, so that the levels kept have none.
                bool touched = all.unknown;
                for (auto &k2 : xkeys) {
                    if (gpu_key_touched(wa, k2) || gpu_key_touched(ra, k2)) {
                        touched = true;
                    }
                }
                // An expression changes when it reads a variable these
                // statements assign (`_syms`), or the value of one they
                // assign a part of (`_vals`).
                std::set<ASR::symbol_t*> bound_syms, bound_vals;
                for (size_t d = 0; d < p.rank(); d++) {
                    gpu_collect_symbols(p.starts[d], bound_syms);
                    gpu_collect_symbols(p.ends[d], bound_syms);
                    gpu_collect_value_symbols(p.starts[d], bound_vals);
                    gpu_collect_value_symbols(p.ends[d], bound_vals);
                }
                for (auto &w : writes) {
                    for (auto *g : w.guards) {
                        gpu_collect_symbols(g, bound_syms);
                        gpu_collect_value_symbols(g, bound_vals);
                    }
                }
                ASR::symbol_t *lv = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(l->m_head.m_v)->m_v);
                for (ASR::symbol_t *s : sc_all) {
                    if (s == lv) touched = true;
                    if (bound_syms.count(s) && p.position(s) < 0) {
                        touched = true;
                    }
                }
                for (ASR::symbol_t *s : ba_all) {
                    if (bound_vals.count(s) && p.position(s) < 0) {
                        touched = true;
                    }
                }
                // The loop's own variable changes from one iteration to
                // the next: the producer's bounds and tests must not read
                // it.
                if (bound_syms.count(lv)) touched = true;
                // Nor a variable that may change through another name.
                for (ASR::symbol_t *s : bound_syms) {
                    if (p.position(s) < 0
                            && may_be_aliased(s, address_taken)) {
                        touched = true;
                    }
                }
                // The DO loops nested in it run the same iterations in
                // each of its iterations, so that its last iteration runs
                // theirs: their bounds read neither its variable nor
                // anything assigned in it. Otherwise an inner loop may run
                // no iteration at all in the last one.
                for (ASR::DoLoop_t *inner : loops) {
                    std::set<ASR::symbol_t*> head_syms, head_vals;
                    gpu_collect_symbols(inner->m_head.m_end, head_syms);
                    gpu_collect_value_symbols(inner->m_head.m_end, head_vals);
                    if (inner->m_head.m_start) {
                        gpu_collect_symbols(inner->m_head.m_start, head_syms);
                        gpu_collect_value_symbols(inner->m_head.m_start,
                            head_vals);
                    }
                    for (ASR::symbol_t *s : head_syms) {
                        if (s == lv || sc_all.count(s)
                                || may_be_aliased(s, address_taken)) {
                            touched = true;
                        }
                    }
                    for (ASR::symbol_t *s : head_vals) {
                        if (ba_all.count(s)) touched = true;
                    }
                }
                if (touched) break;
                other_writes.insert(wa.begin(), wa.end());
                other_reads.insert(ra.begin(), ra.end());
                assigned_in_loops.insert(sc_all.begin(), sc_all.end());
                based_in_loops.insert(ba_all.begin(), ba_all.end());
                // The consumer's bounds are fixed only when they do not
                // read the loop's variable either.
                assigned_in_loops.insert(lv);
                loops.push_back(l);
                loop_stmts.push_back(an.stmt);
                // One level up the loop itself is the child.
                child_a = child_b = an.stmt;
                if (k == 0) break;
                Ancestor &up = ancestors[k - 1];
                if (!ASR::is_a<ASR::DoLoop_t>(*up.stmt)) break;
                list = up.body;
                list_n = up.n_body;
            }
        }
        if (loops.empty()) return fail();

        Location loc = body[i + 1]->base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        ASR::ttype_t *logical_type = ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4));

        // The copies of the renamed arrays.
        std::map<std::string, ASR::symbol_t*> copies;
        std::map<std::string, ASR::expr_t*> designators;
        std::vector<ASR::stmt_t*> hoisted_before, hoisted_after;
        SymbolTable *fn_scope = scope;
        while (fn_scope->asr_owner && ASR::is_a<ASR::symbol_t>(
                *fn_scope->asr_owner) && ASR::is_a<ASR::Block_t>(
                    *ASR::down_cast<ASR::symbol_t>(fn_scope->asr_owner))) {
            fn_scope = fn_scope->parent;
        }
        for (auto &k : renamed) {
            ASR::expr_t *des = find_designator(cin->m_body, cin->n_body, k);
            if (des == nullptr) return fail();
            ASR::ttype_t *t = ASRUtils::expr_type(des);
            if (!ASRUtils::is_allocatable(t) || ASRUtils::is_pointer(t)
                    || designator_may_be_aliased(des, address_taken)) {
                return fail();
            }
            // Its copy is allocated once, before the outermost of these
            // loops, which must not change its allocation. What may change
            // it is a definition of the array, or of a derived-type object
            // or component it is part of, as a whole (an assignment or an
            // actual argument), which gpu_collect_effects counts as a
            // definition of the variable it belongs to; or an effect it
            // does not model (ALLOCATE, ...), which ended the chain above.
            ASR::symbol_t *root = gpu_designator_base(des).root;
            if (root == nullptr || assigned_in_loops.count(root)) {
                return fail();
            }
            designators[k] = des;
        }
        // Renamed arrays whose copies can take turns with them instead of
        // being copied back (ping-pong buffers): a copy starts equal to its
        // array, and only the consumer writes either, always the same
        // elements, so the two agree on every element the consumer does not
        // write. The fused nest then reads one and writes the other, which
        // one alternating from one execution to the next; the original
        // loops, in the last iteration, first copy back what the latest
        // execution left in the copies. Between two executions the array
        // may hold the older values, so no other statement of the loops
        // may read it either.
        std::map<std::string, bool> swap;
        {
            std::set<ASR::symbol_t*> c_bound_syms, c_bound_vals;
            for (size_t d2 = 0; d2 < c.rank(); d2++) {
                gpu_collect_symbols(c.starts[d2], c_bound_syms);
                gpu_collect_symbols(c.ends[d2], c_bound_syms);
                gpu_collect_value_symbols(c.starts[d2], c_bound_vals);
                gpu_collect_value_symbols(c.ends[d2], c_bound_vals);
            }
            bool bounds_fixed = true;
            for (ASR::symbol_t *x : c_bound_syms) {
                if ((assigned_in_loops.count(x)
                        || may_be_aliased(x, address_taken))
                        && c.position(x) < 0) {
                    bounds_fixed = false;
                }
            }
            for (ASR::symbol_t *x : c_bound_vals) {
                if (based_in_loops.count(x) && c.position(x) < 0) {
                    bounds_fixed = false;
                }
            }
            for (auto &k : renamed) {
                swap[k] = gpu_loop_transform_enabled(po, GpuLoopSwap)
                    && bounds_fixed
                    && !gpu_key_touched(other_writes, k)
                    && !gpu_key_touched(other_reads, k);
            }
        }
        bool ping_pong = !renamed.empty();
        for (auto &k : renamed) if (!swap[k]) ping_pong = false;
        ASR::symbol_t *parity = nullptr;
        if (ping_pong) {
            parity = ASR::down_cast<ASR::Var_t>(b.Variable(fn_scope,
                fn_scope->get_unique_name("__gpu_parity", false),
                ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4)),
                ASR::intentType::Local))->m_v;
            hoisted_before.push_back(b.Assignment(b.Var(parity),
                b.bool_t(false, ASRUtils::TYPE(ASR::make_Logical_t(al, loc,
                    4)))));
        }
        for (auto &k : renamed) {
            ASR::expr_t *des = designators[k];
            ASR::ttype_t *t = ASRUtils::expr_type(des);
            std::string name = fn_scope->get_unique_name("__gpu_next_"
                + std::string(ASRUtils::symbol_name(
                    ASR::is_a<ASR::Var_t>(*des)
                    ? ASR::down_cast<ASR::Var_t>(des)->m_v
                    : ASR::down_cast<ASR::StructInstanceMember_t>(des)->m_m)),
                false);
            ASR::symbol_t *copy = ASR::down_cast<ASR::Var_t>(b.Variable(
                fn_scope, name, ASRUtils::duplicate_type(al, t),
                ASR::intentType::Local))->m_v;
            copies[k] = copy;
            size_t rank = ASRUtils::extract_n_dims_from_ttype(t);
            ASRUtils::ExprStmtDuplicator d(al);
            Vec<ASR::dimension_t> dims;
            dims.reserve(al, rank);
            for (size_t r = 0; r < rank; r++) {
                ASR::dimension_t dim;
                dim.loc = loc;
                dim.m_start = ASRUtils::EXPR(ASR::make_ArrayBound_t(al, loc,
                    d.duplicate_expr(des), gpu_i32(al, loc, r + 1),
                    ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)),
                    ASR::arrayboundType::LBound, nullptr));
                dim.m_length = ASRUtils::EXPR(ASR::make_ArraySize_t(al, loc,
                    d.duplicate_expr(des), gpu_i32(al, loc, r + 1),
                    ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)),
                    nullptr));
                dims.push_back(al, dim);
            }
            Vec<ASR::expr_t*> aargs;
            aargs.reserve(al, 1);
            aargs.push_back(al, d.duplicate_expr(des));
            ASR::expr_t *is_alloc = ASRUtils::EXPR(
                ASR::make_IntrinsicImpureFunction_t(al, loc,
                    static_cast<int64_t>(
                        ASRUtils::IntrinsicImpureFunctions::Allocated),
                    aargs.p, aargs.n, 0, logical_type, nullptr));
            if (ping_pong) {
                hoisted_before.push_back(b.If(is_alloc,
                    {b.Allocate(b.Var(copy), dims),
                     gpu_device_copy_nest(al, loc, fn_scope, b.Var(copy),
                        des)}, {}));
            } else {
                hoisted_before.push_back(b.If(is_alloc,
                    {b.Allocate(b.Var(copy), dims)}, {}));
            }
            Vec<ASR::expr_t*> cargs;
            cargs.reserve(al, 1);
            cargs.push_back(al, b.Var(copy));
            ASR::expr_t *copy_alloc = ASRUtils::EXPR(
                ASR::make_IntrinsicImpureFunction_t(al, loc,
                    static_cast<int64_t>(
                        ASRUtils::IntrinsicImpureFunctions::Allocated),
                    cargs.p, cargs.n, 0, logical_type, nullptr));
            hoisted_after.push_back(b.If(copy_alloc,
                {b.Deallocate(b.Var(copy))}, {}));
        }

        // The fused body: reads of an intermediate array are the
        // producer's expression at their subscripts, and the renamed
        // arrays are written into their copies.
        struct Rewrite : public ASR::BaseExprReplacer<Rewrite> {
            Allocator &al;
            SymbolTable *scope;
            const std::vector<ProducerWrite> *writes;
            const GpuLoopNest *p;
            Rewrite(Allocator &al_) : al(al_) {}
            void replace_ArrayItem(ASR::ArrayItem_t *x) {
                ASR::BaseExprReplacer<Rewrite>::replace_ArrayItem(x);
                std::string k = gpu_array_key(x->m_v);
                for (auto &w : *writes) {
                    if (w.key != k) continue;
                    GpuDuplicator d(al, scope);
                    for (size_t s = 0; s < x->n_args; s++) {
                        d.value_map[p->vars[w.loop_of_subscript[s]]] =
                            x->m_args[s].m_right;
                    }
                    *current_expr = d.duplicate_expr(w.value);
                    return;
                }
            }
        } rw(al);
        rw.scope = scope;
        rw.writes = &writes;
        rw.p = &p;
        struct Walk : public ASRUtils::BlockBodyWalkVisitor<Walk> {
            Rewrite *rw;
            const std::map<std::string, ASR::symbol_t*> *copies;
            Allocator *al;
            void expr(ASR::expr_t *&e) {
                if (!e) return;
                rw->current_expr = &e;
                rw->replace_expr(e);
            }
            void visit_Assignment(const ASR::Assignment_t &x) {
                ASR::Assignment_t &a = const_cast<ASR::Assignment_t&>(x);
                expr(a.m_value);
                if (ASR::is_a<ASR::ArrayItem_t>(*a.m_target)) {
                    ASR::ArrayItem_t *t = ASR::down_cast<ASR::ArrayItem_t>(
                        a.m_target);
                    for (size_t d = 0; d < t->n_args; d++) {
                        expr(t->m_args[d].m_right);
                    }
                    auto it = copies->find(gpu_array_key(t->m_v));
                    if (it != copies->end()) {
                        t->m_v = ASRUtils::EXPR(ASR::make_Var_t(*al,
                            t->m_v->base.loc, it->second));
                    }
                } else {
                    expr(a.m_target);
                }
            }
            void visit_If(const ASR::If_t &x) {
                expr(const_cast<ASR::If_t&>(x).m_test);
                for (size_t k = 0; k < x.n_body; k++) visit_stmt(*x.m_body[k]);
                for (size_t k = 0; k < x.n_orelse; k++) {
                    visit_stmt(*x.m_orelse[k]);
                }
            }
            void visit_Select(const ASR::Select_t &x) {
                expr(const_cast<ASR::Select_t&>(x).m_test);
                for (size_t k = 0; k < x.n_body; k++) {
                    if (ASR::is_a<ASR::CaseStmt_t>(*x.m_body[k])) {
                        ASR::CaseStmt_t *c = ASR::down_cast<ASR::CaseStmt_t>(
                            x.m_body[k]);
                        for (size_t t = 0; t < c->n_test; t++) {
                            expr(c->m_test[t]);
                        }
                        for (size_t t = 0; t < c->n_body; t++) {
                            visit_stmt(*c->m_body[t]);
                        }
                    } else {
                        ASR::CaseStmt_Range_t *c =
                            ASR::down_cast<ASR::CaseStmt_Range_t>(x.m_body[k]);
                        expr(c->m_start);
                        expr(c->m_end);
                        for (size_t t = 0; t < c->n_body; t++) {
                            visit_stmt(*c->m_body[t]);
                        }
                    }
                }
                for (size_t k = 0; k < x.n_default; k++) {
                    visit_stmt(*x.m_default[k]);
                }
            }
        } walk;
        walk.rw = &rw;
        walk.copies = &copies;
        walk.al = &al;
        for (size_t k = 0; k < cn; k++) walk.visit_stmt(*cb[k]);

        // The bounds of an intermediate array the subscripts of inlined
        // bodies read are read by the host before the launch, so the fused
        // nest does not take the array, which it no longer reads.
        std::vector<ASR::stmt_t*> bound_reads;
        {
            struct Hoist : public ASR::BaseExprReplacer<Hoist> {
                Allocator &al;
                SymbolTable *fn_scope;
                const std::set<std::string> *xkeys;
                std::map<std::string, ASR::symbol_t*> temps;
                std::vector<ASR::stmt_t*> *reads;
                Hoist(Allocator &al_) : al(al_) {}
                void hoist(ASR::expr_t *e, ASR::expr_t *array) {
                    if (!xkeys->count(gpu_array_key(array))) return;
                    std::string key = gpu_affine_key(e);
                    if (key.empty()) return;
                    auto it = temps.find(key);
                    if (it == temps.end()) {
                        ASRUtils::ASRBuilder b(al, e->base.loc);
                        std::string name = fn_scope->get_unique_name(
                            "__gpu_bound", false);
                        ASR::symbol_t *t = ASR::down_cast<ASR::Var_t>(
                            b.Variable(fn_scope, name,
                                ASRUtils::duplicate_type(al,
                                    ASRUtils::expr_type(e)),
                                ASR::intentType::Local))->m_v;
                        temps[key] = t;
                        reads->push_back(b.Assignment(b.Var(t), e));
                        it = temps.find(key);
                    }
                    *current_expr = ASRUtils::EXPR(ASR::make_Var_t(al,
                        e->base.loc, it->second));
                }
                const std::map<std::string, GpuAffine> *pinned;
                const std::map<std::string, ASR::expr_t*> *atoms;
                void replace_ArrayBound(ASR::ArrayBound_t *x) {
                    auto it = pinned->find(gpu_affine_key(&x->base));
                    if (it != pinned->end()) {
                        ASR::expr_t *v = gpu_affine_to_expr(al,
                            x->base.base.loc, it->second, *atoms);
                        if (v) {
                            *current_expr = v;
                            return;
                        }
                    }
                    hoist(&x->base, x->m_v);
                }
                void replace_ArraySize(ASR::ArraySize_t *x) {
                    hoist(&x->base, x->m_v);
                }
            } h(al);
            h.fn_scope = fn_scope;
            h.xkeys = &xkeys;
            h.reads = &bound_reads;
            h.pinned = &pinned;
            h.atoms = &ab.atoms;
            struct Walk2 : public ASRUtils::BlockBodyWalkVisitor<Walk2> {
                Hoist *h;
                void expr(ASR::expr_t *&e) {
                    if (!e) return;
                    h->current_expr = &e;
                    h->replace_expr(e);
                }
                void visit_Assignment(const ASR::Assignment_t &x) {
                    ASR::Assignment_t &a = const_cast<ASR::Assignment_t&>(x);
                    expr(a.m_value);
                    expr(a.m_target);
                }
                void visit_If(const ASR::If_t &x) {
                    expr(const_cast<ASR::If_t&>(x).m_test);
                    for (size_t k = 0; k < x.n_body; k++) {
                        visit_stmt(*x.m_body[k]);
                    }
                    for (size_t k = 0; k < x.n_orelse; k++) {
                        visit_stmt(*x.m_orelse[k]);
                    }
                }
            } w2;
            w2.h = &h;
            for (size_t k = 0; k < cn; k++) w2.visit_stmt(*cb[k]);
        }
        Vec<ASR::stmt_t*> fused_body;
        fused_body.reserve(al, cn);
        for (size_t k = 0; k < cn; k++) fused_body.push_back(al, cb[k]);
        ASRUtils::ExprStmtDuplicator d(al);
        std::vector<ASR::expr_t*> cs, ce;
        for (size_t k = 0; k < c.rank(); k++) {
            cs.push_back(d.duplicate_expr(c.starts[k]));
            ce.push_back(d.duplicate_expr(c.ends[k]));
        }
        std::vector<ASR::stmt_t*> fused_stmts = bound_reads;
        fused_stmts.push_back(gpu_loop_nest_with_body(al, c, cs, ce,
            fused_body));
        // The nests that copy back what the fused nest wrote into the
        // copies.
        auto copy_back = [&](const std::string &k) {
            const WriteShape &ws = shapes[k];
            Vec<ASR::array_index_t> idx;
            idx.reserve(al, ws.subscripts.size());
            Vec<ASR::array_index_t> idx2;
            idx2.reserve(al, ws.subscripts.size());
            for (ASR::symbol_t *sub : ws.subscripts) {
                ASR::array_index_t ai;
                ai.loc = loc;
                ai.m_left = nullptr;
                ai.m_step = nullptr;
                ai.m_right = b.Var(sub);
                idx.push_back(al, ai);
                ai.m_right = b.Var(sub);
                idx2.push_back(al, ai);
            }
            ASR::ttype_t *et = ASRUtils::duplicate_type(al,
                ASRUtils::extract_type(ASRUtils::expr_type(designators[k])));
            ASR::expr_t *lhs = ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc,
                d.duplicate_expr(designators[k]), idx.p, idx.n, et,
                ASR::arraystorageType::ColMajor, nullptr, false));
            ASR::expr_t *rhs = ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc,
                b.Var(copies[k]), idx2.p, idx2.n,
                ASRUtils::duplicate_type(al, et),
                ASR::arraystorageType::ColMajor, nullptr, false));
            Vec<ASR::stmt_t*> cbody2;
            cbody2.reserve(al, 1);
            cbody2.push_back(al, b.Assignment(lhs, rhs));
            std::vector<ASR::expr_t*> cs2, ce2;
            for (size_t m = 0; m < c.rank(); m++) {
                cs2.push_back(d.duplicate_expr(c.starts[m]));
                ce2.push_back(d.duplicate_expr(c.ends[m]));
            }
            return gpu_loop_nest_with_body(al, c, cs2, ce2, cbody2);
        };
        std::vector<ASR::stmt_t*> original = {body[i], body[i + 1]};
        if (ping_pong) {
            // The second fused nest reads the copies and writes the arrays.
            GpuDuplicator d2(al, scope);
            Vec<ASR::stmt_t*> other;
            d2.duplicate_body(fused_body.p, fused_body.n, other);
            if (!d2.ok) return fail();
            std::map<std::string, ASR::symbol_t*> copy_of;
            std::map<ASR::symbol_t*, std::string> array_of;
            for (auto &k : renamed) {
                copy_of[k] = copies[k];
                array_of[copies[k]] = k;
            }
            gpu_replace_exprs(other.p, other.n, [&](ASR::expr_t *e)
                    -> ASR::expr_t* {
                if (ASR::is_a<ASR::Var_t>(*e)) {
                    auto it = array_of.find(ASR::down_cast<ASR::Var_t>(e)->m_v);
                    if (it != array_of.end()) {
                        return d.duplicate_expr(designators[it->second]);
                    }
                }
                std::string k = gpu_array_key(e);
                auto it = copy_of.find(k);
                if (!k.empty() && it != copy_of.end()) {
                    return b.Var(it->second);
                }
                return nullptr;
            });
            std::vector<ASR::expr_t*> cs3, ce3;
            for (size_t m = 0; m < c.rank(); m++) {
                cs3.push_back(d.duplicate_expr(c.starts[m]));
                ce3.push_back(d.duplicate_expr(c.ends[m]));
            }
            ASR::stmt_t *second = gpu_loop_nest_with_body(al, c, cs3, ce3,
                other);
            ASR::stmt_t *first = fused_stmts.back();
            fused_stmts.pop_back();
            fused_stmts.push_back(b.If(b.Var(parity), {second}, {first}));
            fused_stmts.push_back(b.Assignment(b.Var(parity),
                b.Not(b.Var(parity))));
            std::vector<ASR::stmt_t*> back;
            for (auto &k : renamed) back.push_back(copy_back(k));
            back.push_back(b.Assignment(b.Var(parity),
                b.bool_t(false, ASRUtils::TYPE(ASR::make_Logical_t(al, loc,
                    4)))));
            original.insert(original.begin(), b.If(b.Var(parity), back, {}));
        } else {
            for (auto &k : renamed) fused_stmts.push_back(copy_back(k));
        }

        // The condition: not the last iteration of the DO loops, and the
        // assumed lower bounds hold. A DO loop evaluates its end once, when
        // it starts, and the statements of its body may change what the
        // expression reads: the end is copied into a variable right before
        // the loop, which the last iteration's variable equals.
        ASR::expr_t *last = nullptr;
        for (size_t k = 0; k < loops.size(); k++) {
            ASR::DoLoop_t *l = loops[k];
            ASR::symbol_t *end = ASR::down_cast<ASR::Var_t>(b.Variable(
                fn_scope, fn_scope->get_unique_name("__gpu_end", false),
                ASRUtils::duplicate_type(al,
                    ASRUtils::type_get_past_allocatable_pointer(
                        ASRUtils::expr_type(l->m_head.m_end))),
                ASR::intentType::Local))->m_v;
            before[loop_stmts[k]].push_back(b.Assignment(b.Var(end),
                d.duplicate_expr(l->m_head.m_end)));
            ASR::expr_t *t = b.Eq(d.duplicate_expr(l->m_head.m_v),
                b.Var(end));
            last = last ? b.And(last, t) : t;
        }
        ASR::expr_t *cond = b.Not(last);
        for (ASR::expr_t *e : checks) {
            cond = b.And(cond, b.GtE(e, gpu_i32(al, loc, 0)));
        }
        rarely_run.insert(body[i]);
        rarely_run.insert(body[i + 1]);
        ASR::stmt_t *result = b.If(cond, fused_stmts, original);
        if (!hoisted_before.empty()) {
            ASR::stmt_t *outer = loop_stmts.back();
            auto &bv = before[outer];
            bv.insert(bv.end(), hoisted_before.begin(), hoisted_before.end());
            auto &av = after[outer];
            av.insert(av.end(), hoisted_after.begin(), hoisted_after.end());
        }
        return result;
    }
};

} // namespace

void gpu_loop_fusion(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options, GpuRarelyRun &rarely_run) {
    if (!gpu_loop_transforms_apply(pass_options)
            || !gpu_loop_transform_enabled(pass_options, GpuLoopFuse)) {
        return;
    }
    LoopFusion f(al, pass_options, rarely_run);
    {
        // Every variable named in a statement, or in the declaration of
        // a variable (its initializer), that takes an address
        // (`GetPointer`, `c_loc`) or associates a pointer, and the
        // variable declared: on either side of such an association. That
        // is how EQUIVALENCE is lowered, in a procedure as a pointer set
        // from the address of the other variable, in a module as a
        // variable whose initializer is that address.
        struct AddressTaken : public ASR::BaseWalkVisitor<AddressTaken> {
            using Base = ASR::BaseWalkVisitor<AddressTaken>;
            std::set<ASR::symbol_t*> *syms;
            struct Frame {
                bool takes_address = false;
                std::set<ASR::symbol_t*> vars;
            };
            // The statement or declaration being visited, innermost last.
            std::vector<Frame> frames;
            void framed(ASR::symbol_t *declared,
                    const std::function<void()> &visit) {
                frames.emplace_back();
                if (declared) frames.back().vars.insert(declared);
                visit();
                Frame fr = std::move(frames.back());
                frames.pop_back();
                if (fr.takes_address) {
                    syms->insert(fr.vars.begin(), fr.vars.end());
                }
            }
            void taken(ASR::expr_t *arg) {
                if (!frames.empty()) frames.back().takes_address = true;
                // Also outside a statement or declaration.
                gpu_collect_symbols(arg, *syms);
            }
            void visit_stmt(const ASR::stmt_t &x) {
                framed(nullptr, [&]() { Base::visit_stmt(x); });
            }
            void visit_Variable(const ASR::Variable_t &x) {
                framed(const_cast<ASR::symbol_t*>(&x.base),
                    [&]() { Base::visit_Variable(x); });
            }
            void visit_Var(const ASR::Var_t &x) {
                if (!frames.empty()) {
                    frames.back().vars.insert(
                        ASRUtils::symbol_get_past_external(x.m_v));
                }
            }
            void visit_GetPointer(const ASR::GetPointer_t &x) {
                taken(x.m_arg);
                Base::visit_GetPointer(x);
            }
            void visit_CLoc(const ASR::CLoc_t &x) {
                taken(x.m_arg);
                Base::visit_CLoc(x);
            }
            void visit_Associate(const ASR::Associate_t &x) {
                taken(x.m_value);
                Base::visit_Associate(x);
            }
        } t;
        t.syms = &f.address_taken;
        t.visit_TranslationUnit(unit);
    }
    std::function<void(SymbolTable*)> visit_scope = [&](SymbolTable *st) {
        for (auto &item : st->get_scope()) {
            ASR::symbol_t *s = item.second;
            if (ASR::is_a<ASR::Function_t>(*s)) {
                ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(s);
                std::vector<LoopFusion::Ancestor> anc;
                f.visit_body(fn->m_symtab, fn->m_body, fn->n_body, anc);
                visit_scope(fn->m_symtab);
            } else if (ASR::is_a<ASR::Program_t>(*s)) {
                ASR::Program_t *pr = ASR::down_cast<ASR::Program_t>(s);
                std::vector<LoopFusion::Ancestor> anc;
                f.visit_body(pr->m_symtab, pr->m_body, pr->n_body, anc);
                visit_scope(pr->m_symtab);
            } else if (ASR::is_a<ASR::Module_t>(*s)) {
                visit_scope(ASR::down_cast<ASR::Module_t>(s)->m_symtab);
            }
        }
    };
    visit_scope(unit.m_symtab);
}

void pass_gpu_loop_fusion(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options) {
    GpuRarelyRun rarely_run;
    gpu_loop_fusion(al, unit, pass_options, rarely_run);
}

} // namespace LCompilers
