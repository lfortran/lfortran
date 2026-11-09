#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/gpu_affine.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/pass/gpu_loop_fusion.h>
#include <libasr/pass/gpu_loop_utils.h>
#include <libasr/pass/gpu_offload_designator.h>
#include <libasr/pass/intrinsic_array_function_registry.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/pass_utils.h>

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
 *   or written through another name (gpu_may_be_aliased: TARGET, POINTER,
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

// Whether the array a designator names (a variable, or a component of one)
// may be read or written through another name, which the analyses do not
// see: a pointer component on the way, or a variable that
// gpu_may_be_aliased (a pointer, a TARGET, EQUIVALENCE, a common block,
// ...). A designator of another form (an element on the way, ...) is not
// analyzed and may be.
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
    return gpu_may_be_aliased(s, address_taken);
}

// Replaces each read of an intermediate array by the producer's expression
// at its subscripts.
struct FusedReadRewriter
        : public ASR::BaseExprReplacer<FusedReadRewriter> {
    Allocator &al;
    SymbolTable *scope;
    const std::vector<ProducerWrite> *writes;
    const GpuLoopNest *p;
    FusedReadRewriter(Allocator &al_) : al(al_) {}
    void replace_ArrayItem(ASR::ArrayItem_t *x) {
        ASR::BaseExprReplacer<FusedReadRewriter>::replace_ArrayItem(x);
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
};

// Applies FusedReadRewriter to the expressions of the fused body, and
// writes the renamed arrays into their copies.
struct FusedBodyWalker
        : public ASRUtils::BlockBodyWalkVisitor<FusedBodyWalker> {
    FusedReadRewriter *rw;
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
};

// Replaces each bound or size of an intermediate array by a variable the
// host assigns before the launch (`reads`), one per distinct expression.
struct BoundHoister : public ASR::BaseExprReplacer<BoundHoister> {
    Allocator &al;
    SymbolTable *fn_scope;
    const std::set<std::string> *xkeys;
    std::map<std::string, ASR::symbol_t*> temps;
    std::vector<ASR::stmt_t*> *reads;
    BoundHoister(Allocator &al_) : al(al_) {}
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
                // The producer's first index, an integer(4) value, in the
                // kind of the bound (integer(8) when the subscripts it
                // shifts are; see GpuInliner).
                if (ASRUtils::extract_kind_from_ttype_t(x->m_type)
                        != ASRUtils::extract_kind_from_ttype_t(
                            ASRUtils::expr_type(v))) {
                    v = ASRUtils::ASRBuilder(al, x->base.base.loc).i2i_t(v,
                        ASRUtils::duplicate_type(al, x->m_type));
                }
                *current_expr = v;
                return;
            }
        }
        hoist(&x->base, x->m_v);
    }
    void replace_ArraySize(ASR::ArraySize_t *x) {
        hoist(&x->base, x->m_v);
    }
};

// Applies BoundHoister to the expressions of the fused body.
struct BoundHoistWalker
        : public ASRUtils::BlockBodyWalkVisitor<BoundHoistWalker> {
    BoundHoister *h;
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
};

class LoopFusion {
public:
    Allocator &al;
    const PassOptions &po;
    // The variables involved in taking an address or associating a
    // pointer anywhere in the program (gpu_collect_address_taken).
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

    // The state of one attempt to fuse a pair of loop nests, which each
    // phase of try_fuse fills in for the next ones.
    struct Fusion {
        // The producer and the consumer.
        GpuLoopNest p, c;
        std::vector<ProducerWrite> writes;
        // The keys of the intermediate arrays.
        std::set<std::string> xkeys;
        // What the producer's expressions read.
        std::set<std::string> producer_reads;
        std::set<ASR::symbol_t*> producer_syms;
        // The consumer's innermost loop, and its body copied, with the
        // calls that take an intermediate array inlined.
        ASR::DoLoop_t *cin = nullptr;
        ASR::stmt_t **cb = nullptr;
        size_t cn = 0;
        // What the consumer assigns.
        GpuEffects c_effects;
        // Arrays the consumer writes and the producer reads, and the
        // element each is written at.
        std::vector<std::string> renamed;
        std::map<std::string, WriteShape> shapes;
        // The affine facts of the consumer, and its reads of the
        // intermediate arrays.
        GpuAffineBuilder cb_builder;
        std::vector<Read> reads;
        // The producer's loop bounds, as affine forms of `pb`.
        std::vector<GpuAffine> plo, phi;
        GpuAffineBuilder pb;
        // The builder of the bounds of the intermediate arrays, the bounds
        // assumed for them (`box`) and their run-time checks.
        GpuAffineBuilder ab;
        std::map<std::string, GpuConstraints> box;
        // Equalities of the lower bounds of the intermediate arrays with
        // the first index the producer writes, and the value of each
        // pinned bound (by its atom key).
        std::map<std::string, GpuConstraints> pins;
        std::map<std::string, GpuAffine> pinned;
        std::vector<ASR::expr_t*> checks;
        // The DO loops whose last iteration alone needs the intermediate
        // arrays stored, innermost first, and their statements.
        std::vector<ASR::DoLoop_t*> loops;
        std::vector<ASR::stmt_t*> loop_stmts;
        // What the statements of those loops other than the two assign and
        // read.
        std::set<std::string> other_writes, other_reads;
        // The variables they assign, and those that designators they assign
        // start from (`x` for `x(k) = ...` or `x%c = ...`).
        std::set<ASR::symbol_t*> assigned_in_loops, based_in_loops;
        // The scope that receives the new variables: that of the procedure,
        // not of a BLOCK.
        SymbolTable *fn_scope = nullptr;
        // The designators of the renamed arrays, their copies, and whether
        // each takes turns with its copy.
        std::map<std::string, ASR::expr_t*> designators;
        std::map<std::string, ASR::symbol_t*> copies;
        std::map<std::string, bool> swap;
        bool ping_pong = false;
        ASR::symbol_t *parity = nullptr;
        // Statements before and after the outermost of `loops`.
        std::vector<ASR::stmt_t*> hoisted_before, hoisted_after;
        // The host's reads of the bounds of the intermediate arrays, before
        // the fused nest.
        std::vector<ASR::stmt_t*> bound_reads;
    };

    // Whether `producer` and `consumer` can be fused as far as their own
    // shape goes (`f.p`, `f.c`, `f.writes`): parallel loop nests of the
    // same rank with simple bodies, the producer only
    // assigning elements of intermediate arrays at its own iteration, with
    // expressions that read neither these arrays nor its private variables
    // and that call only pure, self-contained functions.
    bool parse_pair(ASR::stmt_t *producer, ASR::stmt_t *consumer,
            Fusion &f) {
        GpuLoopNest &p = f.p, &c = f.c;
        if (!gpu_loop_nest(producer, p) || !gpu_loop_nest(consumer, c)
                || p.rank() != c.rank()) {
            return false;
        }
        if (!gpu_simple_body(p.innermost()->m_body, p.innermost()->n_body)
                || !gpu_simple_body(c.innermost()->m_body,
                    c.innermost()->n_body)) {
            return false;
        }
        std::vector<ProducerWrite> &writes = f.writes;
        if (!parse_producer(p, writes)) return false;
        for (auto &w : writes) f.xkeys.insert(w.key);

        // What the producer's expressions read.
        for (auto &w : writes) {
            gpu_collect_array_reads(w.value, f.producer_reads);
            gpu_collect_symbols(w.value, f.producer_syms);
            for (auto *g : w.guards) gpu_collect_symbols(g, f.producer_syms);
            // A call of a function that names variables other than its
            // arguments and locals reads what the collections above do not
            // see.
            if (!gpu_calls_self_contained(w.value)) return false;
            for (auto *g : w.guards) {
                if (!gpu_calls_self_contained(g)) return false;
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
            if (!calls.ok) return false;
        }
        for (auto &k : f.xkeys) {
            if (gpu_key_touched(f.producer_reads, k)) return false;
        }
        for (ASR::symbol_t *s : p.privates) {
            if (f.producer_syms.count(s)) return false;
        }
        return true;
    }

    // Whether the (inlined) consumer can be fused with the producer as far
    // as the variables they name are concerned: the reads of the
    // intermediate arrays are all visible, the consumer writes neither
    // these arrays nor what the producer reads, and their names do not
    // clash in the fused nest.
    bool check_consumer(Fusion &f) {
        GpuLoopNest &p = f.p, &c = f.c;
        // A call left in the consumer reads only what its arguments name,
        // so that the reads of the intermediate arrays are all visible.
        if (!gpu_calls_self_contained(f.cb, f.cn)) return false;
        // What the consumer assigns.
        gpu_collect_effects(f.cb, f.cn, f.c_effects);
        if (f.c_effects.unknown) return false;
        const std::set<ASR::symbol_t*> &c_scalars = f.c_effects.scalars;
        const std::set<std::string> &c_arrays = f.c_effects.arrays;
        for (auto &k : f.xkeys) if (c_arrays.count(k)) return false;
        for (ASR::symbol_t *s : f.producer_syms) {
            if (c_scalars.count(s)) return false;
        }
        for (ASR::symbol_t *s : c.privates) {
            if (f.producer_syms.count(s)) return false;
        }
        // In the fused nest a variable the producer's expressions, tests
        // or bounds name would be the consumer's index of that name (DO
        // CONCURRENT indices are the variables of the scope), not the
        // value it has outside the consumer. The producer's own indices
        // are replaced by the consumer's subscripts.
        {
            std::set<ASR::symbol_t*> named = f.producer_syms;
            for (size_t d = 0; d < p.rank(); d++) {
                gpu_collect_symbols(p.starts[d], named);
                gpu_collect_symbols(p.ends[d], named);
            }
            for (ASR::symbol_t *s : named) {
                if (c.position(s) >= 0 && p.position(s) < 0) return false;
            }
        }
        return true;
    }

    // Finds the arrays the consumer writes and the producer reads, which
    // the fused nest writes into copies (`renamed`), and checks that the
    // consumer writes each at its own iteration on every path, reading it
    // only there before.
    bool find_renamed(Fusion &f) {
        // Arrays the consumer writes and the producer reads: renamed.
        for (auto &k : f.c_effects.arrays) {
            if (f.producer_reads.count(k)) f.renamed.push_back(k);
            // The copy would replace only the reads of the array itself.
            if (gpu_key_touched_whole(f.producer_reads, k)) return false;
        }
        for (auto &k : f.renamed) {
            WriteShape ws;
            bool all = must_write(f.cb, f.cn, k, ws);
            if (!all || !ws.consistent) return false;
            if (ws.read
                    && !reads_before_writes(f.cb, f.cn, k, ws.subscripts)) {
                return false;
            }
            for (ASR::symbol_t *s : ws.subscripts) {
                if (f.c.position(s) < 0) return false;
            }
            f.shapes[k] = ws;
        }
        return true;
    }

    // Collects the consumer's reads of the intermediate arrays, under the
    // affine facts of its loop bounds; there must be some.
    bool collect_consumer_reads(Fusion &f) {
        GpuLoopNest &c = f.c;
        // The affine facts: the consumer's loop bounds.
        std::set<ASR::symbol_t*> single;
        for (auto &kv : f.c_effects.definitions) {
            if (kv.second == 1) single.insert(kv.first);
        }
        auto invariant = [&f](ASR::symbol_t *s) {
            if (f.c.position(s) >= 0) return true;
            return f.c_effects.scalars.count(s) == 0
                && ASR::is_a<ASR::Variable_t>(*s);
        };
        GpuAffineBuilder &cb_builder = f.cb_builder;
        cb_builder.invariant_variable = invariant;
        GpuConstraints bounds;
        for (size_t d = 0; d < c.rank(); d++) {
            GpuAffine v = GpuAffine::atom(gpu_affine_key(
                ASRUtils::EXPR(ASR::make_Var_t(al, f.cin->base.base.loc,
                    c.vars[d]))));
            GpuAffine lo, hi;
            if (!cb_builder.affine(c.starts[d], lo)
                    || !cb_builder.affine(c.ends[d], hi)) {
                return false;
            }
            bounds.push_back(v.plus(lo, -1));
            bounds.push_back(hi.plus(v, -1));
        }
        if (!collect_reads(f.cb, f.cn, f.xkeys, cb_builder, single,
                GpuDnf{bounds}, f.reads)) {
            return false;
        }
        if (f.reads.empty()) return false;
        return true;
    }

    // The bound of dimension `d` of the array `des` as an affine form of
    // the builder `f.ab`.
    bool bound_atom(Fusion &f, ASR::expr_t *des, size_t d, bool upper,
            GpuAffine &out) {
        ASR::expr_t *e = ASRUtils::EXPR(ASR::make_ArrayBound_t(al,
            des->base.loc, des, gpu_i32(al, des->base.loc, d + 1),
            ASRUtils::TYPE(ASR::make_Integer_t(al, des->base.loc, 4)),
            upper ? ASR::arrayboundType::UBound
                : ASR::arrayboundType::LBound, nullptr));
        return f.ab.affine(e, out);
    }

    // The bounds each intermediate array must have for the producer to
    // write every element of it (`box`): within the loop bounds and the
    // tests on single indices of the producer.
    bool compute_box(Fusion &f) {
        GpuLoopNest &p = f.p;
        for (auto &w : f.writes) {
            GpuConstraints bx;
            GpuAffineBuilder g0;
            g0.invariant_variable = [&f](ASR::symbol_t *s) {
                return f.p.position(s) >= 0 || f.pb.invariant_variable(s);
            };
            std::vector<std::vector<GpuAffine>> lowers(w.loop_of_subscript.size()),
                uppers(w.loop_of_subscript.size());
            for (size_t d = 0; d < w.loop_of_subscript.size(); d++) {
                int k = w.loop_of_subscript[d];
                lowers[d].push_back(f.plo[k]);
                uppers[d].push_back(f.phi[k]);
            }
            for (ASR::expr_t *g : w.guards) {
                if (!g0.exact_condition(g)) continue;
                GpuDnf gd = g0.condition(g, true);
                if (gd.size() != 1) continue;
                for (auto &c : gd[0]) {
                    for (size_t d = 0; d < w.loop_of_subscript.size(); d++) {
                        int k = w.loop_of_subscript[d];
                        std::string vk = gpu_affine_key(ASRUtils::EXPR(
                            ASR::make_Var_t(al, f.cin->base.base.loc,
                                p.vars[k])));
                        int64_t coef = c.coefficient(vk);
                        if (coef != 1 && coef != -1) continue;
                        bool only = true;
                        for (size_t m = 0; m < p.rank(); m++) {
                            if ((int)m == k) continue;
                            std::string vm = gpu_affine_key(ASRUtils::EXPR(
                                ASR::make_Var_t(al, f.cin->base.base.loc,
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
                if (!bound_atom(f, w.target->m_v, d, false, lb)
                        || !bound_atom(f, w.target->m_v, d, true, ub)) {
                    return false;
                }
                for (auto &l : lowers[d]) bx.push_back(lb.plus(l, -1));
                for (auto &u : uppers[d]) bx.push_back(u.plus(ub, -1));
                // The lower bound pinned to the producer's first index.
                GpuAffine eq = lb.plus(f.plo[w.loop_of_subscript[d]], -1);
                pn.push_back(eq);
                pn.push_back(eq.scaled(-1));
                f.pinned[lb.terms.begin()->first]
                    = f.plo[w.loop_of_subscript[d]];
            }
            f.box[w.key] = bx;
            f.pins[w.key] = pn;
        }
        return true;
    }

    // Whether every read is of an element the producer writes, assuming
    // the bounds `box` of the intermediate arrays when `assume`, and their
    // lower bounds pinned (`pins`) when `pin`.
    bool prove_reads(Fusion &f, bool assume, bool pin) {
        GpuLoopNest &p = f.p;
        for (auto &r : f.reads) {
            const ProducerWrite *w = nullptr;
            for (auto &x : f.writes) if (x.key == r.key) w = &x;
            if (w == nullptr || r.subscripts.size()
                    != w->loop_of_subscript.size()) {
                return false;
            }
            GpuConstraints extra;
            for (size_t d = 0; d < r.subscripts.size(); d++) {
                GpuAffine lb, ub;
                if (!bound_atom(f, w->target->m_v, d, false, lb)
                        || !bound_atom(f, w->target->m_v, d, true, ub)) {
                    return false;
                }
                extra.push_back(r.subscripts[d].plus(lb, -1));
                extra.push_back(ub.plus(r.subscripts[d], -1));
            }
            if (assume) {
                for (auto &bk : f.box) {
                    extra.insert(extra.end(), bk.second.begin(),
                        bk.second.end());
                }
            }
            if (pin) {
                for (auto &bk : f.pins) {
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
            gb.invariant_variable = f.pb.invariant_variable;
            for (size_t d = 0; d < r.subscripts.size(); d++) {
                int k = w->loop_of_subscript[d];
                const GpuAffine &e = r.subscripts[d];
                if (!gpu_affine_proves(facts, e.plus(f.plo[k], -1))
                        || !gpu_affine_proves(facts, f.phi[k].plus(e, -1))) {
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
    }

    // Whether every read is of an element the producer writes, as proven
    // from the bounds of the arrays, or else from bounds the fused nest
    // checks at run time (`checks`).
    bool prove_coverage(Fusion &f) {
        GpuLoopNest &p = f.p;
        // Every read is of an element the producer writes.
        f.plo.resize(p.rank());
        f.phi.resize(p.rank());
        GpuAffineBuilder &pb = f.pb;
        pb.invariant_variable = [&f](ASR::symbol_t *s) {
            return f.c_effects.scalars.count(s) == 0 && f.p.position(s) < 0
                && ASR::is_a<ASR::Variable_t>(*s);
        };
        for (size_t d = 0; d < p.rank(); d++) {
            if (!pb.affine(p.starts[d], f.plo[d])
                    || !pb.affine(p.ends[d], f.phi[d])) {
                return false;
            }
        }
        // A read of an element is inside the bounds of the array, as in any
        // valid program. When the producer writes every element of an
        // intermediate array, every read is covered; whether it does
        // depends on how the array was allocated, which the fused nest
        // checks at run time when the reads cannot be proven otherwise:
        // the bounds of the array lie within the loop bounds and the tests
        // on single indices of the producer (`box`).
        f.ab = f.cb_builder;
        f.ab.bindings.clear();
        if (!compute_box(f)) return false;
        // Pinning the lower bounds makes the subscripts of the inlined
        // bodies constant offsets of the indices, which the passes after
        // this one can reason about; it is checked at run time like the
        // bounds (an array allocated otherwise runs the original loops).
        for (auto &kv : pb.atoms) f.ab.atoms.insert(kv);
        bool shifted = false;
        for (auto &r : f.reads) {
            for (auto &e : r.subscripts) {
                for (auto &t : e.terms) {
                    if (f.pinned.count(t.first)) shifted = true;
                }
            }
        }
        bool assume = true, pin = shifted;
        if (pin) {
            // Pins that contradict the assumed bounds (a test of the
            // producer starts its writes after its first index) would
            // prove anything and leave a fused loop that never runs.
            GpuConstraints all;
            for (auto *cs : {&f.box, &f.pins}) {
                for (auto &bk : *cs) {
                    all.insert(all.end(), bk.second.begin(), bk.second.end());
                }
            }
            if (gpu_affine_infeasible(all)) pin = false;
        }
        if (!(pin && prove_reads(f, true, true))) {
            pin = false;
            if (prove_reads(f, false, false)) {
                assume = false;
            } else if (!prove_reads(f, true, false)) {
                return false;
            }
        }
        // The run-time check of the assumed bounds.
        auto add_checks = [&](std::map<std::string, GpuConstraints> &cs) {
            for (auto &bk : cs) {
                for (auto &c : bk.second) {
                    ASR::expr_t *e = gpu_affine_to_expr(al,
                        f.cin->base.base.loc, c, f.ab.atoms);
                    if (e == nullptr) return false;
                    f.checks.push_back(e);
                }
            }
            return true;
        };
        if (assume && !add_checks(f.box)) return false;
        if (pin && !add_checks(f.pins)) return false;
        if (!pin) f.pinned.clear();
        return true;
    }

    // The cost model.
    bool within_cost(const Fusion &f) {
        int64_t extra_ops = 0, saved_bytes = 0;
        for (auto &w : f.writes) {
            int64_t nreads = 0;
            for (auto &r : f.reads) if (r.key == w.key) nreads++;
            if (nreads == 0) continue;
            extra_ops += (nreads - 1) * gpu_expr_cost(w.value);
            saved_bytes += 2 * w.element_bytes;
        }
        return extra_ops <= 64 * saved_bytes;
    }

    // Finds the DO loops whose last iteration alone needs the intermediate
    // arrays stored (`loops`), from the innermost loop holding the pair
    // `body[i]`, `body[i + 1]` outwards; there must be some.
    bool find_time_loops(ASR::stmt_t **body, size_t n, size_t i,
            std::vector<Ancestor> &ancestors, Fusion &f) {
        GpuLoopNest &p = f.p;
        std::vector<ASR::DoLoop_t*> &loops = f.loops;
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
                int64_t inc = 0;
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
            for (auto &k2 : f.xkeys) {
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
            for (auto &w : f.writes) {
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
                        && gpu_may_be_aliased(s, address_taken)) {
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
                            || gpu_may_be_aliased(s, address_taken)) {
                        touched = true;
                    }
                }
                for (ASR::symbol_t *s : head_vals) {
                    if (ba_all.count(s)) touched = true;
                }
            }
            if (touched) break;
            f.other_writes.insert(wa.begin(), wa.end());
            f.other_reads.insert(ra.begin(), ra.end());
            f.assigned_in_loops.insert(sc_all.begin(), sc_all.end());
            f.based_in_loops.insert(ba_all.begin(), ba_all.end());
            // The consumer's bounds are fixed only when they do not
            // read the loop's variable either.
            f.assigned_in_loops.insert(lv);
            loops.push_back(l);
            f.loop_stmts.push_back(an.stmt);
            // One level up the loop itself is the child.
            child_a = child_b = an.stmt;
            if (k == 0) break;
            Ancestor &up = ancestors[k - 1];
            if (!ASR::is_a<ASR::DoLoop_t>(*up.stmt)) break;
            list = up.body;
            list_n = up.n_body;
        }
        return !loops.empty();
    }

    // The scope of the procedure `scope` is in, past BLOCKs.
    static SymbolTable *function_scope(SymbolTable *scope) {
        SymbolTable *fn_scope = scope;
        while (fn_scope->asr_owner && ASR::is_a<ASR::symbol_t>(
                *fn_scope->asr_owner) && ASR::is_a<ASR::Block_t>(
                    *ASR::down_cast<ASR::symbol_t>(fn_scope->asr_owner))) {
            fn_scope = fn_scope->parent;
        }
        return fn_scope;
    }

    // Finds the designator of each renamed array, which must be an
    // allocatable array that is not aliased and whose allocation the DO
    // loops do not change.
    bool find_designators(Fusion &f) {
        for (auto &k : f.renamed) {
            ASR::expr_t *des = find_designator(f.cin->m_body, f.cin->n_body,
                k);
            if (des == nullptr) return false;
            ASR::ttype_t *t = ASRUtils::expr_type(des);
            if (!ASRUtils::is_allocatable(t) || ASRUtils::is_pointer(t)
                    || designator_may_be_aliased(des, address_taken)) {
                return false;
            }
            // Its copy is allocated once, before the outermost of these
            // loops, which must not change its allocation. What may change
            // it is a definition of the array, or of a derived-type object
            // or component it is part of, as a whole (an assignment or an
            // actual argument), which gpu_collect_effects counts as a
            // definition of the variable it belongs to; or an effect it
            // does not model (ALLOCATE, ...), which ended the chain above.
            ASR::symbol_t *root = gpu_designator_base(des).root;
            if (root == nullptr || f.assigned_in_loops.count(root)) {
                return false;
            }
            f.designators[k] = des;
        }
        return true;
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
    void decide_ping_pong(Fusion &f) {
        GpuLoopNest &c = f.c;
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
                if ((f.assigned_in_loops.count(x)
                        || gpu_may_be_aliased(x, address_taken))
                        && c.position(x) < 0) {
                    bounds_fixed = false;
                }
            }
            for (ASR::symbol_t *x : c_bound_vals) {
                if (f.based_in_loops.count(x) && c.position(x) < 0) {
                    bounds_fixed = false;
                }
            }
            for (auto &k : f.renamed) {
                f.swap[k] = gpu_loop_transform_enabled(po, GpuLoopSwap)
                    && bounds_fixed
                    && !gpu_key_touched(f.other_writes, k)
                    && !gpu_key_touched(f.other_reads, k);
            }
        }
        f.ping_pong = !f.renamed.empty();
        for (auto &k : f.renamed) if (!f.swap[k]) f.ping_pong = false;
    }

    // The copies of the renamed arrays, with their allocation (and, for
    // ping-pong, the parity and the initial copy) before the DO loops and
    // their deallocation after them.
    void make_copies(Fusion &f, const Location &loc) {
        ASRUtils::ASRBuilder b(al, loc);
        ASR::ttype_t *logical_type = ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4));
        SymbolTable *fn_scope = f.fn_scope;
        if (f.ping_pong) {
            f.parity = ASR::down_cast<ASR::Var_t>(b.Variable(fn_scope,
                fn_scope->get_unique_name("__gpu_parity", false),
                ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4)),
                ASR::intentType::Local))->m_v;
            f.hoisted_before.push_back(b.Assignment(b.Var(f.parity),
                b.bool_t(false, ASRUtils::TYPE(ASR::make_Logical_t(al, loc,
                    4)))));
        }
        for (auto &k : f.renamed) {
            ASR::expr_t *des = f.designators[k];
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
            f.copies[k] = copy;
            size_t rank = ASRUtils::extract_n_dims_from_ttype(t);
            ASRUtils::ExprStmtDuplicator d(al);
            Vec<ASR::dimension_t> dims;
            dims.reserve(al, rank);
            // The copy has the bounds of the array, read in the kind the
            // device computes element offsets in: with 64-bit offsets they
            // need not fit integer(4).
            int index_kind = gpu_device_capabilities(po).offset_kind;
            for (size_t r = 0; r < rank; r++) {
                ASR::dimension_t dim;
                dim.loc = loc;
                dim.m_start = ASRUtils::EXPR(ASR::make_ArrayBound_t(al, loc,
                    d.duplicate_expr(des), gpu_i32(al, loc, r + 1),
                    ASRUtils::TYPE(ASR::make_Integer_t(al, loc, index_kind)),
                    ASR::arrayboundType::LBound, nullptr));
                dim.m_length = ASRUtils::EXPR(ASR::make_ArraySize_t(al, loc,
                    d.duplicate_expr(des), gpu_i32(al, loc, r + 1),
                    ASRUtils::TYPE(ASR::make_Integer_t(al, loc, index_kind)),
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
            if (f.ping_pong) {
                f.hoisted_before.push_back(b.If(is_alloc,
                    {b.Allocate(b.Var(copy), dims),
                     gpu_device_copy_nest(al, loc, fn_scope, b.Var(copy),
                        des, index_kind)}, {}));
            } else {
                f.hoisted_before.push_back(b.If(is_alloc,
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
            f.hoisted_after.push_back(b.If(copy_alloc,
                {b.Deallocate(b.Var(copy))}, {}));
        }
    }

    // The fused body: reads of an intermediate array are the
    // producer's expression at their subscripts, and the renamed
    // arrays are written into their copies.
    void rewrite_fused_body(SymbolTable *scope, Fusion &f) {
        FusedReadRewriter rw(al);
        rw.scope = scope;
        rw.writes = &f.writes;
        rw.p = &f.p;
        FusedBodyWalker walk;
        walk.rw = &rw;
        walk.copies = &f.copies;
        walk.al = &al;
        for (size_t k = 0; k < f.cn; k++) walk.visit_stmt(*f.cb[k]);
    }

    // The bounds of an intermediate array the subscripts of inlined
    // bodies read are read by the host before the launch, so the fused
    // nest does not take the array, which it no longer reads.
    void hoist_bound_reads(Fusion &f) {
        BoundHoister h(al);
        h.fn_scope = f.fn_scope;
        h.xkeys = &f.xkeys;
        h.reads = &f.bound_reads;
        h.pinned = &f.pinned;
        h.atoms = &f.ab.atoms;
        BoundHoistWalker w2;
        w2.h = &h;
        for (size_t k = 0; k < f.cn; k++) w2.visit_stmt(*f.cb[k]);
    }

    // A nest that copies back what the fused nest wrote into the copy of
    // the renamed array `k`.
    ASR::stmt_t *copy_back(Fusion &f, const std::string &k,
            ASRUtils::ASRBuilder &b, ASRUtils::ExprStmtDuplicator &d,
            const Location &loc) {
        GpuLoopNest &c = f.c;
        const WriteShape &ws = f.shapes[k];
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
            ASRUtils::extract_type(ASRUtils::expr_type(f.designators[k])));
        ASR::expr_t *lhs = ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc,
            d.duplicate_expr(f.designators[k]), idx.p, idx.n, et,
            ASR::arraystorageType::ColMajor, nullptr, false));
        ASR::expr_t *rhs = ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc,
            b.Var(f.copies[k]), idx2.p, idx2.n,
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
    }

    // Makes the fused nest take turns: a second fused nest reads the
    // copies and writes the arrays, `fused_stmts` runs one or the other by
    // the parity, and the original loops (`original`) first copy back
    // what the latest execution left in the copies.
    bool add_ping_pong(SymbolTable *scope, Fusion &f,
            ASRUtils::ASRBuilder &b, ASRUtils::ExprStmtDuplicator &d,
            const Location &loc, Vec<ASR::stmt_t*> &fused_body,
            std::vector<ASR::stmt_t*> &fused_stmts,
            std::vector<ASR::stmt_t*> &original) {
        GpuLoopNest &c = f.c;
        // The second fused nest reads the copies and writes the arrays.
        GpuDuplicator d2(al, scope);
        Vec<ASR::stmt_t*> other;
        d2.duplicate_body(fused_body.p, fused_body.n, other);
        if (!d2.ok) return false;
        std::map<std::string, ASR::symbol_t*> copy_of;
        std::map<ASR::symbol_t*, std::string> array_of;
        for (auto &k : f.renamed) {
            copy_of[k] = f.copies[k];
            array_of[f.copies[k]] = k;
        }
        gpu_replace_exprs(other.p, other.n, [&](ASR::expr_t *e)
                -> ASR::expr_t* {
            if (ASR::is_a<ASR::Var_t>(*e)) {
                auto it = array_of.find(ASR::down_cast<ASR::Var_t>(e)->m_v);
                if (it != array_of.end()) {
                    return d.duplicate_expr(f.designators[it->second]);
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
        fused_stmts.push_back(b.If(b.Var(f.parity), {second}, {first}));
        fused_stmts.push_back(b.Assignment(b.Var(f.parity),
            b.Not(b.Var(f.parity))));
        std::vector<ASR::stmt_t*> back;
        for (auto &k : f.renamed) back.push_back(copy_back(f, k, b, d, loc));
        back.push_back(b.Assignment(b.Var(f.parity),
            b.bool_t(false, ASRUtils::TYPE(ASR::make_Logical_t(al, loc,
                4)))));
        original.insert(original.begin(), b.If(b.Var(f.parity), back, {}));
        return true;
    }

    // The fused statement: the fused nest (with the host's reads of
    // bounds, the copying back or the ping-pong) when not in the last
    // iteration of the DO loops and the assumed bounds hold, and the
    // original loops `producer`, `consumer` otherwise. Also queues the statements to
    // put around the DO loops. Null when the second fused nest of a
    // ping-pong cannot be made.
    ASR::stmt_t *assemble(SymbolTable *scope, ASR::stmt_t *producer,
            ASR::stmt_t *consumer, const Location &loc, Fusion &f) {
        GpuLoopNest &c = f.c;
        SymbolTable *fn_scope = f.fn_scope;
        ASRUtils::ASRBuilder b(al, loc);
        Vec<ASR::stmt_t*> fused_body;
        fused_body.reserve(al, f.cn);
        for (size_t k = 0; k < f.cn; k++) fused_body.push_back(al, f.cb[k]);
        ASRUtils::ExprStmtDuplicator d(al);
        std::vector<ASR::expr_t*> cs, ce;
        for (size_t k = 0; k < c.rank(); k++) {
            cs.push_back(d.duplicate_expr(c.starts[k]));
            ce.push_back(d.duplicate_expr(c.ends[k]));
        }
        std::vector<ASR::stmt_t*> fused_stmts = f.bound_reads;
        fused_stmts.push_back(gpu_loop_nest_with_body(al, c, cs, ce,
            fused_body));
        std::vector<ASR::stmt_t*> original = {producer, consumer};
        if (f.ping_pong) {
            if (!add_ping_pong(scope, f, b, d, loc, fused_body, fused_stmts,
                    original)) {
                return nullptr;
            }
        } else {
            // The nests that copy back what the fused nest wrote into the
            // copies.
            for (auto &k : f.renamed) {
                fused_stmts.push_back(copy_back(f, k, b, d, loc));
            }
        }

        // The condition: not the last iteration of the DO loops, and the
        // assumed lower bounds hold. A DO loop evaluates its end once, when
        // it starts, and the statements of its body may change what the
        // expression reads: the end is copied into a variable right before
        // the loop, which the last iteration's variable equals.
        ASR::expr_t *last = nullptr;
        for (size_t k = 0; k < f.loops.size(); k++) {
            ASR::DoLoop_t *l = f.loops[k];
            ASR::symbol_t *end = ASR::down_cast<ASR::Var_t>(b.Variable(
                fn_scope, fn_scope->get_unique_name("__gpu_end", false),
                ASRUtils::duplicate_type(al,
                    ASRUtils::type_get_past_allocatable_pointer(
                        ASRUtils::expr_type(l->m_head.m_end))),
                ASR::intentType::Local))->m_v;
            before[f.loop_stmts[k]].push_back(b.Assignment(b.Var(end),
                d.duplicate_expr(l->m_head.m_end)));
            ASR::expr_t *t = b.Eq(d.duplicate_expr(l->m_head.m_v),
                b.Var(end));
            last = last ? b.And(last, t) : t;
        }
        ASR::expr_t *cond = b.Not(last);
        for (ASR::expr_t *e : f.checks) {
            cond = b.And(cond, b.GtE(e, gpu_i32(al, loc, 0)));
        }
        rarely_run.insert(producer);
        rarely_run.insert(consumer);
        ASR::stmt_t *result = b.If(cond, fused_stmts, original);
        if (!f.hoisted_before.empty()) {
            ASR::stmt_t *outer = f.loop_stmts.back();
            auto &bv = before[outer];
            bv.insert(bv.end(), f.hoisted_before.begin(),
                f.hoisted_before.end());
            auto &av = after[outer];
            av.insert(av.end(), f.hoisted_after.begin(),
                f.hoisted_after.end());
        }
        return result;
    }

    // Fuses the producer `body[i]` with the consumer `body[i + 1]` (see
    // the comment at the top), returning the statement that replaces both,
    // or null, with nothing changed, when the fusion is not proven legal
    // or not profitable.
    ASR::stmt_t *try_fuse(SymbolTable *scope, ASR::stmt_t **body, size_t n,
            size_t i, std::vector<Ancestor> &ancestors) {
        Fusion f;
        if (!parse_pair(body[i], body[i + 1], f)) return nullptr;

        // The consumer, copied, with the calls that take an intermediate
        // array inlined.
        f.cin = f.c.innermost();
        GpuDuplicator dup(al, scope);
        std::vector<std::pair<SymbolTable*, std::string>> added;
        dup.added = &added;
        Vec<ASR::stmt_t*> cbody;
        dup.duplicate_body(f.cin->m_body, f.cin->n_body, cbody);
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
                if (a && f.xkeys.count(gpu_array_key(a))) return true;
            }
            return false;
        }, gpu_device_capabilities(po).offset_kind);
        f.cb = cbody.p;
        f.cn = cbody.n;
        if (!inl.run(scope, f.cb, f.cn)) {
            inl.rollback();
            rollback();
            return nullptr;
        }
        auto fail = [&]() -> ASR::stmt_t* {
            inl.rollback();
            rollback();
            return nullptr;
        };

        if (!check_consumer(f) || !find_renamed(f)
                || !collect_consumer_reads(f) || !prove_coverage(f)
                || !within_cost(f)
                || !find_time_loops(body, n, i, ancestors, f)) {
            return fail();
        }

        Location loc = body[i + 1]->base.loc;
        f.fn_scope = function_scope(scope);
        if (!find_designators(f)) return fail();
        decide_ping_pong(f);
        make_copies(f, loc);
        rewrite_fused_body(scope, f);
        hoist_bound_reads(f);
        ASR::stmt_t *result = assemble(scope, body[i], body[i + 1], loc, f);
        if (result == nullptr) return fail();
        return result;
    }
};

// Unlike the time-step fusion above, this does not recompute a value at
// multiple readers. A private map and its sole, full-domain ordered fold
// become one loop, retaining a typed scalar store before each fold update.
class PrivateReductionFusion {
    Allocator &al;
    LoopFusion &fusion;

    static ASR::symbol_t *variable(ASR::expr_t *e) {
        if (!e || !ASR::is_a<ASR::Var_t>(*e)) return nullptr;
        auto *s = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(e)->m_v);
        return ASR::is_a<ASR::Variable_t>(*s) ? s : nullptr;
    }

    static bool one(ASR::expr_t *e) {
        return !e || (ASR::is_a<ASR::IntegerConstant_t>(*e)
            && ASR::down_cast<ASR::IntegerConstant_t>(e)->m_n == 1);
    }

    static bool same(ASR::expr_t *a, ASR::expr_t *b) {
        return a && b && PassUtils::same_expression(a, b);
    }

    bool stable(ASR::expr_t *e, SymbolTable *scope) {
        std::set<ASR::symbol_t*> symbols;
        gpu_collect_symbols(e, symbols);
        for (auto *s : symbols) {
            if (gpu_may_be_aliased(s, fusion.address_taken)) return false;
            if (!ASR::is_a<ASR::Variable_t>(*s)) continue;
            auto *v = ASR::down_cast<ASR::Variable_t>(s);
            for (auto *st = scope; st; st = st->parent) {
                if (ASRUtils::has_scoped_variable_attribute(*v, st, false)) {
                    return false;
                }
            }
        }
        return true;
    }

    bool try_fuse(SymbolTable *scope, ASR::stmt_t **body, size_t n, size_t i) {
        if (!scope->asr_owner || !ASR::is_a<ASR::symbol_t>(*scope->asr_owner)
                || !ASR::is_a<ASR::Block_t>(
                    *ASR::down_cast<ASR::symbol_t>(scope->asr_owner))) return false;
        auto *owner = ASR::down_cast2<ASR::Block_t>(scope->asr_owner);
        if (owner->m_body != body || owner->n_body != n) return false;
        if (!ASR::is_a<ASR::DoConcurrentLoop_t>(*body[i])
                || !ASR::is_a<ASR::Assignment_t>(*body[i + 1])
                || !ASR::is_a<ASR::DoLoop_t>(*body[i + 2])) return false;
        auto *p = ASR::down_cast<ASR::DoConcurrentLoop_t>(body[i]);
        auto *init = ASR::down_cast<ASR::Assignment_t>(body[i + 1]);
        auto *c = ASR::down_cast<ASR::DoLoop_t>(body[i + 2]);
        if (p->n_head != 1 || p->n_local || p->n_shared || p->n_reduction
                || c->n_body != 1 || c->n_orelse
                || !ASR::is_a<ASR::Assignment_t>(*c->m_body[0])) return false;
        auto &head = p->m_head[0];
        auto *pi = variable(head.m_v), *ci = variable(c->m_head.m_v);
        if (!pi || !ci || pi == ci || !head.m_start || !c->m_head.m_start
                || !one(head.m_start)
                || !one(c->m_head.m_start) || !one(head.m_increment)
                || !one(c->m_head.m_increment)
                || !same(head.m_end, c->m_head.m_end)
                || !ASRUtils::types_equal(ASRUtils::expr_type(head.m_v),
                    ASRUtils::expr_type(c->m_head.m_v), head.m_v,
                    c->m_head.m_v)) return false;

        // Reuse the producer's store/alias/subscript proof on the serial
        // representation of this nested concurrent loop.
        GpuLoopNest nest;
        nest.loops.push_back(ASR::down_cast<ASR::DoLoop_t>(ASRUtils::STMT(
            ASRUtils::make_DoLoop_t_util(al, body[i]->base.loc, nullptr,
                head, p->m_body, p->n_body, nullptr, 0))));
        nest.vars.push_back(pi);
        std::vector<ProducerWrite> writes;
        if (!fusion.parse_producer(nest, writes) || writes.size() != 1
                || !writes[0].guards.empty()) return false;
        auto &w = writes[0];
        auto *array = variable(w.target->m_v);
        if (!array || ASRUtils::symbol_parent_symtab(array) != scope) return false;
        auto *v = ASR::down_cast<ASR::Variable_t>(array);
        if (v->m_intent != ASR::intentType::Local
                || v->m_storage != ASR::storage_typeType::Default
                || v->m_symbolic_value || v->m_value
                || !ASR::is_a<ASR::Array_t>(*v->m_type)) return false;
        auto *type = ASR::down_cast<ASR::Array_t>(v->m_type);
        if (type->n_dims != 1 || !type->m_dims[0].m_start
                || !one(type->m_dims[0].m_start)
                || !same(type->m_dims[0].m_length, head.m_end)) return false;

        auto *update = ASR::down_cast<ASR::Assignment_t>(c->m_body[0]);
        auto *acc = variable(update->m_target);
        if (!acc || acc != variable(init->m_target)
                || ASRUtils::symbol_parent_symtab(acc) != scope
                || ASRUtils::is_array(ASRUtils::symbol_type(acc))
                || init->m_overloaded || init->m_realloc_lhs
                || update->m_overloaded || update->m_realloc_lhs
                || (!ASR::is_a<ASR::RealConstant_t>(*init->m_value)
                    && !ASR::is_a<ASR::IntegerConstant_t>(*init->m_value))) return false;
        std::set<ASR::symbol_t*> bounds;
        gpu_collect_symbols(head.m_end, bounds);
        if (bounds.count(acc) || bounds.count(pi) || bounds.count(ci)
                || bounds.count(array) || !stable(head.m_end, scope)) return false;
        ASR::expr_t *left = nullptr, *term = nullptr;
        if (ASR::is_a<ASR::RealBinOp_t>(*update->m_value)) {
            auto *op = ASR::down_cast<ASR::RealBinOp_t>(update->m_value);
            if (op->m_op != ASR::binopType::Add) return false;
            left = op->m_left;
            term = op->m_right;
        } else if (ASR::is_a<ASR::IntegerBinOp_t>(*update->m_value)) {
            auto *op = ASR::down_cast<ASR::IntegerBinOp_t>(update->m_value);
            if (op->m_op != ASR::binopType::Add) return false;
            left = op->m_left;
            term = op->m_right;
        } else {
            return false;
        }
        if (variable(left) != acc || !gpu_expr_duplicable(term)
                || !stable(w.value, scope) || !stable(update->m_value, scope)
                || !gpu_calls_self_contained(w.value)) return false;
        struct PureCalls : public ASR::BaseWalkVisitor<PureCalls> {
            bool ok = true;
            void visit_IntrinsicImpureFunction(
                    const ASR::IntrinsicImpureFunction_t &) { ok = false; }
            void visit_FunctionCall(const ASR::FunctionCall_t &x) {
                auto *s = ASRUtils::symbol_get_past_external(x.m_name);
                if (!ASR::is_a<ASR::Function_t>(*s)) {
                    ok = false;
                } else {
                    auto *ft = ASRUtils::get_FunctionType(
                        ASR::down_cast<ASR::Function_t>(s));
                    if (!ft->m_pure && !ft->m_elemental) ok = false;
                }
                ASR::BaseWalkVisitor<PureCalls>::visit_FunctionCall(x);
            }
        } calls;
        calls.visit_expr(*w.value);
        calls.visit_expr(*term);
        if (!calls.ok) return false;
        std::set<ASR::symbol_t*> reads;
        gpu_collect_symbols(w.value, reads);
        if (reads.count(array) || reads.count(acc) || reads.count(ci)) return false;
        reads.clear();
        gpu_collect_symbols(body + i + 2, 1, reads);
        if (reads.count(pi)) return false;

        struct Use : public ASR::BaseWalkVisitor<Use> {
            ASR::symbol_t *array, *index, *acc;
            ASR::ArrayItem_t *item = nullptr;
            bool ok = true;
            void visit_ArrayItem(const ASR::ArrayItem_t &x) {
                if (variable(x.m_v) != array) {
                    ASR::BaseWalkVisitor<Use>::visit_ArrayItem(x);
                    return;
                }
                if (item || x.n_args != 1 || x.m_args[0].m_left
                        || x.m_args[0].m_step
                        || variable(x.m_args[0].m_right) != index) ok = false;
                item = const_cast<ASR::ArrayItem_t*>(&x);
            }
            void visit_Var(const ASR::Var_t &x) {
                if (x.m_v == array || x.m_v == acc) ok = false;
            }
        } use;
        use.array = array;
        use.index = ci;
        use.acc = acc;
        use.visit_expr(*term);
        if (!use.ok || !use.item) return false;

        // The entire owning scope, declarations included, must have no
        // other reader/escape of this storage or use of the producer index.
        for (size_t j = 0; j < n; j++) {
            if (j == i || j == i + 2) continue;
            std::set<ASR::symbol_t*> other;
            gpu_collect_symbols(body + j, 1, other);
            if (other.count(array) || other.count(pi)) return false;
        }
        for (auto &entry : scope->get_scope()) {
            if (entry.second == array) continue;
            struct References : public ASR::BaseWalkVisitor<References> {
                ASR::symbol_t *array;
                bool found = false;
                void visit_Var(const ASR::Var_t &x) {
                    if (x.m_v == array) found = true;
                }
            } refs;
            refs.array = array;
            refs.visit_symbol(*entry.second);
            if (refs.found) return false;
        }

        GpuDuplicator dup(al, scope);
        dup.value_map[pi] = c->m_head.m_v;
        ASR::expr_t *value = dup.duplicate_expr(w.value);
        if (!dup.ok) return false;
        ASRUtils::ASRBuilder b(al, body[i]->base.loc);
        ASR::expr_t *scalar = b.Variable(scope,
            scope->get_unique_name("__gpu_map_value", false),
            ASRUtils::duplicate_type(al, w.target->m_type),
            ASR::intentType::Local);
        gpu_replace_exprs(c->m_body, c->n_body, [&](ASR::expr_t *e) -> ASR::expr_t* {
            return e == &use.item->base ? scalar : nullptr;
        });
        Vec<ASR::stmt_t*> fused;
        fused.reserve(al, 2);
        fused.push_back(al, b.Assignment(scalar, value));
        fused.push_back(al, c->m_body[0]);
        c->m_body = fused.p;
        c->n_body = fused.n;
        scope->erase_symbol(v->m_name);
        return true;
    }

    // A private map `terms(1:n)` that a concurrent loop over 1:n assigns by
    // the last statement of each iteration, and that only the sum right
    // after the loop reads -- the groups of a quantized matrix-vector
    // product -- needs no storage either: the iterations, in order, add
    // what they would have stored to the sum. The sum is `t = sum(terms)`,
    // or a fold `t = c; do j = 1, n; t = t + e; end do` whose `e` reads
    // `terms(j)` once, as gpu_offload lowers `sum` (with an upper bound of
    // max(0, n)). The other statements of an iteration (the sum of a
    // group, say) stay as they are. The loop can be in a branch of an `if`
    // or in a loop of the BLOCK the map is a local of, which must name it
    // nowhere else. The sum then adds the elements in the order the shared
    // lowering of SUM does; the lanes of a SIMD-group combine their parts
    // in another (gpu_vector_lanes.cpp). Returns the number of statements
    // of `body` from `i` on it replaced by two, or zero.
    size_t try_fuse_sum(SymbolTable *scope, ASR::stmt_t **body, size_t n,
            size_t i) {
        if (!scope->asr_owner || !ASR::is_a<ASR::symbol_t>(*scope->asr_owner)
                || !ASR::is_a<ASR::Block_t>(
                    *ASR::down_cast<ASR::symbol_t>(scope->asr_owner))) return 0;
        auto *owner = ASR::down_cast2<ASR::Block_t>(scope->asr_owner);
        if (i + 1 >= n || !ASR::is_a<ASR::DoConcurrentLoop_t>(*body[i])
                || !ASR::is_a<ASR::Assignment_t>(*body[i + 1])) return 0;
        auto *p = ASR::down_cast<ASR::DoConcurrentLoop_t>(body[i]);
        if (p->n_head != 1 || p->n_local || p->n_shared || p->n_reduction
                || p->n_body == 0) return 0;
        auto &head = p->m_head[0];
        auto *pi = variable(head.m_v);
        if (!pi || !head.m_start || !head.m_end || !one(head.m_start)
                || !one(head.m_increment)) return 0;

        // The sum: `acc` and what to add to it, given the stored element.
        auto *first = ASR::down_cast<ASR::Assignment_t>(body[i + 1]);
        if (first->m_overloaded || first->m_realloc_lhs) return 0;
        ASR::symbol_t *array = nullptr, *acc = variable(first->m_target);
        ASR::Assignment_t *update = nullptr;
        ASR::DoLoop_t *fold = nullptr;
        ASR::ArrayItem_t *read = nullptr;
        size_t consumed = 0;
        if (ASR::is_a<ASR::IntrinsicArrayFunction_t>(*first->m_value)) {
            auto *sum = ASR::down_cast<ASR::IntrinsicArrayFunction_t>(
                first->m_value);
            if (static_cast<ASRUtils::IntrinsicArrayFunctions>(
                    sum->m_arr_intrinsic_id)
                        != ASRUtils::IntrinsicArrayFunctions::Sum
                    || sum->n_args != 1) return 0;
            ASR::expr_t *arg = sum->m_args[0];
            while (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arg)) {
                arg = ASR::down_cast<ASR::ArrayPhysicalCast_t>(arg)->m_arg;
            }
            array = variable(arg);
            consumed = 2;
        } else if (i + 2 < n && ASR::is_a<ASR::DoLoop_t>(*body[i + 2])
                && (ASR::is_a<ASR::RealConstant_t>(*first->m_value)
                    || ASR::is_a<ASR::IntegerConstant_t>(*first->m_value))) {
            fold = ASR::down_cast<ASR::DoLoop_t>(body[i + 2]);
            if (fold->n_body != 1 || fold->n_orelse
                    || !ASR::is_a<ASR::Assignment_t>(*fold->m_body[0])
                    || !one(fold->m_head.m_start)
                    || !one(fold->m_head.m_increment)
                    || !covers(fold->m_head.m_end, head.m_end)) return 0;
            update = ASR::down_cast<ASR::Assignment_t>(fold->m_body[0]);
            auto *ci = variable(fold->m_head.m_v);
            if (!ci || ci == pi || variable(update->m_target) != acc
                    || update->m_overloaded || update->m_realloc_lhs
                    || (!ASR::is_a<ASR::RealBinOp_t>(*update->m_value)
                        && !ASR::is_a<ASR::IntegerBinOp_t>(*update->m_value))) {
                return 0;
            }
            // `acc + e`, where `e` reads one element, terms(j).
            ASR::expr_t *left, *term;
            ASR::binopType op;
            if (ASR::is_a<ASR::RealBinOp_t>(*update->m_value)) {
                auto *x = ASR::down_cast<ASR::RealBinOp_t>(update->m_value);
                left = x->m_left; term = x->m_right; op = x->m_op;
            } else {
                auto *x = ASR::down_cast<ASR::IntegerBinOp_t>(update->m_value);
                left = x->m_left; term = x->m_right; op = x->m_op;
            }
            if (op != ASR::binopType::Add || variable(left) != acc) return 0;
            struct Items : public ASR::BaseWalkVisitor<Items> {
                std::vector<ASR::ArrayItem_t*> items;
                void visit_ArrayItem(const ASR::ArrayItem_t &x) {
                    items.push_back(const_cast<ASR::ArrayItem_t*>(&x));
                    ASR::BaseWalkVisitor<Items>::visit_ArrayItem(x);
                }
            } items;
            items.visit_expr(*term);
            for (auto *x : items.items) {
                if (x->n_args == 1 && !x->m_args[0].m_left
                        && !x->m_args[0].m_step
                        && variable(x->m_args[0].m_right) == ci
                        && variable(x->m_v)
                        && ASR::is_a<ASR::Array_t>(*ASRUtils::symbol_type(
                            variable(x->m_v)))
                        && ASRUtils::symbol_parent_symtab(variable(x->m_v))
                            == scope) {
                    if (read) return 0;
                    read = x;
                }
            }
            if (!read) return 0;
            array = variable(read->m_v);
            std::set<ASR::symbol_t*> named;
            gpu_collect_symbols(term, named);
            if (named.count(acc) || !gpu_expr_duplicable(term)
                    || !stable(term, scope)) return 0;
            // The other things the term reads are not the map.
            struct Count : public ASR::BaseWalkVisitor<Count> {
                ASR::symbol_t *array;
                size_t found = 0;
                void visit_Var(const ASR::Var_t &x) {
                    if (x.m_v == array) found++;
                }
            } count;
            count.array = array;
            count.visit_expr(*term);
            if (count.found != 1) return 0;
            consumed = 3;
        } else {
            return 0;
        }
        if (!array || !acc || acc == array || acc == pi
                || ASRUtils::symbol_parent_symtab(array) != scope
                || ASRUtils::is_array(ASRUtils::symbol_type(acc))) return 0;
        auto *v = ASR::down_cast<ASR::Variable_t>(array);
        if (v->m_intent != ASR::intentType::Local
                || v->m_storage != ASR::storage_typeType::Default
                || v->m_symbolic_value || v->m_value
                || !ASR::is_a<ASR::Array_t>(*v->m_type)) return 0;
        auto *type = ASR::down_cast<ASR::Array_t>(v->m_type);
        ASR::ttype_t *element = type->m_type;
        if (type->n_dims != 1 || !type->m_dims[0].m_start
                || !one(type->m_dims[0].m_start)
                || !covers(type->m_dims[0].m_length, head.m_end)
                || !(ASR::is_a<ASR::Real_t>(*element)
                    || ASR::is_a<ASR::Integer_t>(*element))
                || (!fold && !ASRUtils::types_equal(element,
                    ASRUtils::symbol_type(acc), nullptr, nullptr))) return 0;
        std::set<ASR::symbol_t*> bounds;
        gpu_collect_symbols(head.m_end, bounds);
        if (bounds.count(acc) || bounds.count(pi) || bounds.count(array)
                || !stable(head.m_end, scope)) return 0;

        // The last statement of an iteration stores its element, and no
        // other statement of the loop names the map or the sum.
        ASR::stmt_t *last = p->m_body[p->n_body - 1];
        if (!ASR::is_a<ASR::Assignment_t>(*last)) return 0;
        auto *store = ASR::down_cast<ASR::Assignment_t>(last);
        if (store->m_overloaded || store->m_realloc_lhs
                || !ASR::is_a<ASR::ArrayItem_t>(*store->m_target)) return 0;
        auto *item = ASR::down_cast<ASR::ArrayItem_t>(store->m_target);
        if (variable(item->m_v) != array || item->n_args != 1
                || item->m_args[0].m_left || item->m_args[0].m_step
                || variable(item->m_args[0].m_right) != pi
                || !ASRUtils::types_equal(ASRUtils::expr_type(store->m_value),
                    element, nullptr, nullptr)
                || has_early_exit(p->m_body, p->n_body)
                || !stable(store->m_value, scope)
                || !gpu_calls_self_contained(store->m_value)) return 0;
        std::set<ASR::symbol_t*> named;
        gpu_collect_symbols(store->m_value, named);
        gpu_collect_symbols(p->m_body, p->n_body - 1, named);
        if (named.count(array) || named.count(acc)) return 0;
        if (fold && named.count(variable(fold->m_head.m_v))) return 0;
        // Nor does anything else in the BLOCK, declarations included, nor
        // does the loop variable outlive the loop.
        struct References : public ASRUtils::BlockBodyWalkVisitor<References> {
            std::set<ASR::symbol_t*> symbols;
            std::set<const ASR::stmt_t*> skip;
            bool found = false;
            void visit_stmt(const ASR::stmt_t &x) {
                if (skip.count(&x)) return;
                ASRUtils::BlockBodyWalkVisitor<References>::visit_stmt(x);
            }
            void visit_Var(const ASR::Var_t &x) {
                if (symbols.count(x.m_v)) found = true;
            }
        } refs;
        refs.symbols = {array, pi};
        for (size_t j = i; j < i + consumed; j++) refs.skip.insert(body[j]);
        for (size_t j = 0; j < owner->n_body; j++) {
            refs.visit_stmt(*owner->m_body[j]);
        }
        if (refs.found) return 0;
        if (fold) {
            // The fold's variable is not read after it.
            refs.symbols = {variable(fold->m_head.m_v)};
            for (size_t j = 0; j < owner->n_body; j++) {
                refs.visit_stmt(*owner->m_body[j]);
            }
            if (refs.found) return 0;
        }
        for (auto &entry : scope->get_scope()) {
            if (entry.second == array) continue;
            struct Declared : public ASR::BaseWalkVisitor<Declared> {
                ASR::symbol_t *array;
                bool found = false;
                void visit_Var(const ASR::Var_t &x) {
                    if (x.m_v == array) found = true;
                }
            } declared;
            declared.array = array;
            declared.visit_symbol(*entry.second);
            if (declared.found) return 0;
        }

        const Location &loc = body[i]->base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        ASR::expr_t *scalar = b.Variable(scope,
            scope->get_unique_name("__gpu_map_value", false),
            ASRUtils::duplicate_type(al, element), ASR::intentType::Local);
        ASR::stmt_t *init, *add;
        if (fold) {
            // The fold's update, reading the stored value at the
            // producer's index.
            init = body[i + 1];
            ASR::symbol_t *ci = variable(fold->m_head.m_v);
            gpu_replace_exprs(fold->m_body, fold->n_body,
                    [&](ASR::expr_t *e) -> ASR::expr_t* {
                if (e == &read->base) return scalar;
                if (ASR::is_a<ASR::Var_t>(*e) && variable(e) == ci) {
                    return b.i2i_t(b.Var(pi), ASRUtils::symbol_type(ci));
                }
                return nullptr;
            });
            add = fold->m_body[0];
        } else {
            ASR::expr_t *zero = ASR::is_a<ASR::Real_t>(*element)
                ? ASRUtils::EXPR(ASR::make_RealConstant_t(al, loc, 0,
                    ASRUtils::duplicate_type(al, element)))
                : ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 0,
                    ASRUtils::duplicate_type(al, element),
                    ASR::integerbozType::Decimal));
            init = b.Assignment(first->m_target, zero);
            add = b.Assignment(b.Var(acc), b.Add(b.Var(acc), scalar));
        }
        Vec<ASR::stmt_t*> loop;
        loop.reserve(al, p->n_body + 1);
        for (size_t j = 0; j + 1 < p->n_body; j++) loop.push_back(al, p->m_body[j]);
        loop.push_back(al, b.Assignment(scalar, store->m_value));
        loop.push_back(al, add);
        body[i] = init;
        body[i + 1] = ASRUtils::STMT(ASRUtils::make_DoLoop_t_util(al, loc,
            nullptr, head, loop.p, loop.n, nullptr, 0));
        scope->erase_symbol(v->m_name);
        return consumed;
    }

    // Whether an array of `length` elements has one for each iteration of
    // a loop from 1 to `end`: the length is `end` or max(0, end), or a
    // variable declared with such a value, as the extent of an automatic
    // array is.
    bool covers(ASR::expr_t *length, ASR::expr_t *end) {
        if (same(length, end)) return true;
        if (auto *s = variable(length)) {
            auto *x = ASR::down_cast<ASR::Variable_t>(s);
            if (x->m_storage != ASR::storage_typeType::Default
                    || !x->m_symbolic_value) return false;
            length = x->m_symbolic_value;
            if (same(length, end)) return true;
        }
        if (!ASR::is_a<ASR::IntrinsicElementalFunction_t>(*length)) return false;
        auto *f = ASR::down_cast<ASR::IntrinsicElementalFunction_t>(length);
        if (f->m_intrinsic_id != static_cast<int64_t>(
                ASRUtils::IntrinsicElementalFunctions::Max)
                || f->n_args != 2) return false;
        auto zero = [](ASR::expr_t *e) {
            int64_t c = -1;
            return ASRUtils::extract_value(ASRUtils::expr_value(e), c) && c == 0;
        };
        return (zero(f->m_args[0]) && same(f->m_args[1], end))
            || (zero(f->m_args[1]) && same(f->m_args[0], end));
    }

public:
    PrivateReductionFusion(Allocator &al_, LoopFusion &fusion_)
        : al(al_), fusion(fusion_) {}

    void list(SymbolTable *scope, ASR::stmt_t **body, size_t &n) {
        for (size_t i = 0; i < n; i++) {
            ASR::stmt_t *s = body[i];
            if (ASR::is_a<ASR::BlockCall_t>(*s)) {
                auto *symbol = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                if (!ASR::is_a<ASR::Block_t>(*symbol)) continue;
                auto *block = ASR::down_cast<ASR::Block_t>(symbol);
                list(block->m_symtab, block->m_body, block->n_body);
            } else if (ASR::is_a<ASR::DoLoop_t>(*s)) {
                auto *loop = ASR::down_cast<ASR::DoLoop_t>(s);
                list(scope, loop->m_body, loop->n_body);
            } else if (ASR::is_a<ASR::DoConcurrentLoop_t>(*s)) {
                auto *loop = ASR::down_cast<ASR::DoConcurrentLoop_t>(s);
                list(scope, loop->m_body, loop->n_body);
            } else if (ASR::is_a<ASR::If_t>(*s)) {
                auto *branch = ASR::down_cast<ASR::If_t>(s);
                list(scope, branch->m_body, branch->n_body);
                list(scope, branch->m_orelse, branch->n_orelse);
            }
        }
        for (size_t i = 0; i + 2 < n; i++) {
            if (!try_fuse(scope, body, n, i)) continue;
            for (size_t j = i + 1; j < n; j++) body[j - 1] = body[j];
            n--;
        }
        for (size_t i = 0; i + 1 < n; i++) {
            size_t consumed = try_fuse_sum(scope, body, n, i);
            if (consumed < 3) continue;
            for (size_t j = i + 3; j < n; j++) body[j - 1] = body[j];
            n--;
        }
    }
};


// Forwards a block-private, full-domain map into the statements of the same
// BLOCK that read its elements at subscripts computed there. A private map
// such as `codes(1:k)` written by one concurrent loop and read element by
// element afterwards (after array_op turned the slices into loops) needs no
// storage of its own: each read recomputes the element it reads. The array,
// its store and its per-thread device storage disappear, which is what makes
// the consuming fold a plain loop over the map's element expression.
class PrivateMapForwarding {
    Allocator &al;
    LoopFusion &fusion;

    static ASR::symbol_t *variable(ASR::expr_t *e) {
        if (!e || !ASR::is_a<ASR::Var_t>(*e)) return nullptr;
        auto *s = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(e)->m_v);
        return ASR::is_a<ASR::Variable_t>(*s) ? s : nullptr;
    }

    // The statement a node is, for comparing against a walked statement.
    static ASR::stmt_t *stmt_of(ASR::Assignment_t *a) {
        return a ? &a->base : nullptr;
    }

    static ASR::stmt_t *stmt_of(ASR::If_t *i) {
        return i ? &i->base : nullptr;
    }

    static bool one(ASR::expr_t *e) {
        return !e || (ASR::is_a<ASR::IntegerConstant_t>(*e)
            && ASR::down_cast<ASR::IntegerConstant_t>(e)->m_n == 1);
    }

    // An expression that may be recomputed where it is read: no impure or
    // array-valued intrinsic and no call that may be impure or is not
    // elemental.
    static bool pure_expr(ASR::expr_t *e) {
        struct Check : public ASR::BaseWalkVisitor<Check> {
            bool ok = true;
            void visit_IntrinsicImpureFunction(
                    const ASR::IntrinsicImpureFunction_t &) {
                ok = false;
            }
            void visit_IntrinsicImpureSubroutine(
                    const ASR::IntrinsicImpureSubroutine_t &) {
                ok = false;
            }
            void visit_ImpliedDoLoop(const ASR::ImpliedDoLoop_t &) {
                ok = false;
            }
            void visit_FunctionCall(const ASR::FunctionCall_t &x) {
                ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_name);
                if (!ASR::is_a<ASR::Function_t>(*s)) {
                    ok = false;
                    return;
                }
                auto *ft = ASRUtils::get_FunctionType(
                    ASR::down_cast<ASR::Function_t>(s));
                if (!ft->m_pure && !ft->m_elemental) ok = false;
                ASR::BaseWalkVisitor<Check>::visit_FunctionCall(x);
            }
        } c;
        c.visit_expr(*e);
        return c.ok;
    }

    // The bound expression an array is declared with: an extent temporary
    // that is never assigned stands for its symbolic value, and `max(0, n)`
    // is the guard a pointer array carries for an empty extent.
    static ASR::expr_t *extent(ASR::expr_t *e,
            const std::set<ASR::symbol_t*> &assigned) {
        for (int depth = 0; e && depth < 4; depth++) {
            if (ASR::is_a<ASR::Var_t>(*e)) {
                ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(e)->m_v);
                if (!ASR::is_a<ASR::Variable_t>(*s) || assigned.count(s)) {
                    return e;
                }
                auto *v = ASR::down_cast<ASR::Variable_t>(s);
                ASR::expr_t *next = v->m_symbolic_value
                    ? v->m_symbolic_value : v->m_value;
                if (!next) return e;
                e = next;
                continue;
            }
            if (!ASR::is_a<ASR::IntrinsicElementalFunction_t>(*e)) break;
            auto *f = ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e);
            if (static_cast<ASRUtils::IntrinsicElementalFunctions>(
                        f->m_intrinsic_id)
                    != ASRUtils::IntrinsicElementalFunctions::Max
                    || f->n_args != 2) break;
            ASR::expr_t *zero = nullptr;
            ASR::expr_t *other = nullptr;
            for (size_t i = 0; i < 2; i++) {
                if (ASR::is_a<ASR::IntegerConstant_t>(*f->m_args[i])) {
                    auto *c = ASR::down_cast<ASR::IntegerConstant_t>(
                        f->m_args[i]);
                    if (c->m_n == 0) zero = f->m_args[i];
                } else {
                    other = f->m_args[i];
                }
            }
            if (!zero || !other) break;
            e = other;
        }
        return e;
    }

    bool stable(ASR::expr_t *e, SymbolTable *scope) {
        std::set<ASR::symbol_t*> symbols;
        gpu_collect_symbols(e, symbols);
        for (auto *s : symbols) {
            if (gpu_may_be_aliased(s, fusion.address_taken)) return false;
            if (!ASR::is_a<ASR::Variable_t>(*s)) continue;
            auto *v = ASR::down_cast<ASR::Variable_t>(s);
            for (auto *st = scope; st; st = st->parent) {
                if (ASRUtils::has_scoped_variable_attribute(*v, st, false)) {
                    return false;
                }
            }
        }
        return true;
    }

    // The variable a target designator assigns, through any subscripting
    // and component selection: `z%f_(k, j)` assigns `z`, the symbol reads
    // of `z%f_` are collected under.
    static ASR::symbol_t *assigned_variable(ASR::expr_t *e) {
        while (e) {
            if (ASR::is_a<ASR::ArrayItem_t>(*e)) {
                e = ASR::down_cast<ASR::ArrayItem_t>(e)->m_v;
            } else if (ASR::is_a<ASR::ArraySection_t>(*e)) {
                e = ASR::down_cast<ASR::ArraySection_t>(e)->m_v;
            } else if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
                e = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v;
            } else {
                break;
            }
        }
        return variable(e);
    }

    // Every variable a statement list assigns, or passes to a call.
    static void collect_written(ASR::stmt_t **body, size_t n,
            std::set<ASR::symbol_t*> &out) {
        for (size_t i = 0; i < n; i++) {
            ASR::stmt_t *s = body[i];
            if (ASR::is_a<ASR::Assignment_t>(*s)) {
                auto *a = ASR::down_cast<ASR::Assignment_t>(s);
                if (auto *v = assigned_variable(a->m_target)) out.insert(v);
            } else if (ASR::is_a<ASR::DoLoop_t>(*s)) {
                auto *l = ASR::down_cast<ASR::DoLoop_t>(s);
                if (auto *v = variable(l->m_head.m_v)) out.insert(v);
                collect_written(l->m_body, l->n_body, out);
            } else if (ASR::is_a<ASR::DoConcurrentLoop_t>(*s)) {
                auto *l = ASR::down_cast<ASR::DoConcurrentLoop_t>(s);
                for (size_t h = 0; h < l->n_head; h++) {
                    if (auto *v = variable(l->m_head[h].m_v)) out.insert(v);
                }
                collect_written(l->m_body, l->n_body, out);
            } else if (ASR::is_a<ASR::If_t>(*s)) {
                auto *b = ASR::down_cast<ASR::If_t>(s);
                collect_written(b->m_body, b->n_body, out);
                collect_written(b->m_orelse, b->n_orelse, out);
            } else if (ASR::is_a<ASR::BlockCall_t>(*s)) {
                auto *sym = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                if (ASR::is_a<ASR::Block_t>(*sym)) {
                    auto *b = ASR::down_cast<ASR::Block_t>(sym);
                    collect_written(b->m_body, b->n_body, out);
                }
            } else {
                std::set<ASR::symbol_t*> refs;
                gpu_collect_symbols(body + i, 1, refs);
                out.insert(refs.begin(), refs.end());
            }
        }
    }

    // A condition a statement runs under, with the polarity of an ELSE.
    struct Guard {
        ASR::expr_t *condition;
        bool positive;
    };

    static Guard guard_of(ASR::expr_t *e, bool positive) {
        if (e && ASR::is_a<ASR::LogicalNot_t>(*e)) {
            return {ASR::down_cast<ASR::LogicalNot_t>(e)->m_arg, !positive};
        }
        return {e, positive};
    }

    // Whether every condition of `need` holds in `have`.
    static bool implies(const std::vector<Guard> &have,
            const std::vector<Guard> &need) {
        for (auto &n : need) {
            bool found = false;
            for (auto &h : have) {
                if (h.positive == n.positive && h.condition && n.condition
                        && PassUtils::same_expression(
                            h.condition, n.condition)) {
                    found = true;
                    break;
                }
            }
            if (!found) return false;
        }
        return true;
    }

    // Element reads of `array` in a statement list, or any other use of it.
    struct Reader : public ASR::BaseWalkVisitor<Reader> {
        ASR::symbol_t *array;
        ASR::stmt_t *skip;
        std::vector<Guard> guards;
        std::vector<ASR::ArrayItem_t*> reads;
        std::vector<std::vector<Guard>> read_guards;
        std::vector<ASR::ArrayBound_t*> bounds;
        bool other_use = false;

        Reader(ASR::symbol_t *a, ASR::stmt_t *k) : array(a), skip(k) {}

        void visit_ArrayItem(const ASR::ArrayItem_t &x) {
            if (variable(x.m_v) != array) {
                ASR::BaseWalkVisitor<Reader>::visit_ArrayItem(x);
                return;
            }
            if (x.n_args != 1 || x.m_args[0].m_left
                    || x.m_args[0].m_step) {
                other_use = true;
            } else {
                reads.push_back(const_cast<ASR::ArrayItem_t*>(&x));
                read_guards.push_back(guards);
            }
        }

        void visit_Var(const ASR::Var_t &x) {
            if (ASRUtils::symbol_get_past_external(x.m_v) == array) {
                other_use = true;
            }
        }

        // `lbound`/`ubound` of the map is a bound of the loop the read runs
        // in, not a use of its values.
        void visit_ArrayBound(const ASR::ArrayBound_t &x) {
            if (variable(x.m_v) == array) {
                bounds.push_back(const_cast<ASR::ArrayBound_t*>(&x));
                return;
            }
            ASR::BaseWalkVisitor<Reader>::visit_ArrayBound(x);
        }

        void visit_If(const ASR::If_t &x) {
            if (x.m_test) visit_expr(*x.m_test);
            guards.push_back(guard_of(x.m_test, true));
            for (size_t i = 0; i < x.n_body; i++) visit_stmt(*x.m_body[i]);
            guards.back() = guard_of(x.m_test, false);
            for (size_t i = 0; i < x.n_orelse; i++) visit_stmt(*x.m_orelse[i]);
            guards.pop_back();
        }

        // A BLOCK's statements are in its own scope, not in the call.
        void visit_BlockCall(const ASR::BlockCall_t &x) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
            if (!ASR::is_a<ASR::Block_t>(*b)) {
                other_use = true;
                return;
            }
            auto *blk = ASR::down_cast<ASR::Block_t>(b);
            for (size_t i = 0; i < blk->n_body; i++) {
                visit_stmt(*blk->m_body[i]);
            }
        }

        void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &) {
            other_use = true;
        }

        // Any subscript of an assigned target that reads the array makes the
        // map's element per-iteration dependent in a way we do not lower.
        void visit_Assignment(const ASR::Assignment_t &x) {
            if (reinterpret_cast<const ASR::stmt_t*>(&x) == skip) return;
            Target t;
            t.array = array;
            t.use = &other_use;
            if (x.m_target) t.visit_expr(*x.m_target);
            if (x.m_value) visit_expr(*x.m_value);
            if (x.m_overloaded) visit_stmt(*x.m_overloaded);
        }

        struct Target : public ASR::BaseWalkVisitor<Target> {
            ASR::symbol_t *array;
            bool *use;
            void visit_ArrayItem(const ASR::ArrayItem_t &x) {
                if (variable(x.m_v) == array) {
                    *use = true;
                    return;
                }
                ASR::BaseWalkVisitor<Target>::visit_ArrayItem(x);
            }
            void visit_Var(const ASR::Var_t &x) {
                if (ASRUtils::symbol_get_past_external(x.m_v) == array) {
                    *use = true;
                }
            }
        };
    };

    void scan(ASR::stmt_t **body, size_t n, Reader &r) {
        for (size_t i = 0; i < n; i++) r.visit_stmt(*body[i]);
    }

    // Whether a statement is the map store `array(index) = value`.
    static ASR::Assignment_t *map_store(ASR::stmt_t *s,
            ASR::symbol_t *array, ASR::symbol_t *index) {
        if (!s || !ASR::is_a<ASR::Assignment_t>(*s)) return nullptr;
        auto *a = ASR::down_cast<ASR::Assignment_t>(s);
        if (a->m_overloaded || a->m_realloc_lhs
                || !ASR::is_a<ASR::ArrayItem_t>(*a->m_target)) return nullptr;
        auto *item = ASR::down_cast<ASR::ArrayItem_t>(a->m_target);
        if (variable(item->m_v) != array || item->n_args != 1
                || item->m_args[0].m_left || item->m_args[0].m_step
                || variable(item->m_args[0].m_right) != index) return nullptr;
        return a;
    }

    void find_stores(ASR::stmt_t **body, size_t n, ASR::symbol_t *index,
            std::set<ASR::symbol_t*> &arrays) {
        for (size_t k = 0; k < n; k++) {
            if (!ASR::is_a<ASR::Assignment_t>(*body[k])) continue;
            auto *a = ASR::down_cast<ASR::Assignment_t>(body[k]);
            if (!ASR::is_a<ASR::ArrayItem_t>(*a->m_target)) continue;
            auto *item = ASR::down_cast<ASR::ArrayItem_t>(a->m_target);
            ASR::symbol_t *v = variable(item->m_v);
            if (!v || v == index || item->n_args != 1
                    || item->m_args[0].m_left || item->m_args[0].m_step
                    || variable(item->m_args[0].m_right) != index) continue;
            arrays.insert(v);
        }
        // A map written under an IF is forwarded once the maps written
        // beside it are gone.
        if (!arrays.empty()) return;
        for (size_t k = 0; k < n; k++) {
            if (!ASR::is_a<ASR::If_t>(*body[k])) continue;
            auto *branch = ASR::down_cast<ASR::If_t>(body[k]);
            find_stores(branch->m_body, branch->n_body, index, arrays);
        }
    }

    // The one store of `array` in a statement list, at the loop's index,
    // which may sit under IF statements. Reports the IFs around it, or null
    // when the store is at the top level.
    ASR::Assignment_t *find_store(ASR::stmt_t **body, size_t n,
            ASR::symbol_t *array, ASR::symbol_t *index,
            std::vector<ASR::If_t*> &parents, std::vector<Guard> &guards) {
        ASR::Assignment_t *found = nullptr;
        for (size_t k = 0; k < n; k++) {
            ASR::Assignment_t *a = map_store(body[k], array, index);
            if (a) {
                if (found) return nullptr;
                found = a;
                continue;
            }
            if (ASR::is_a<ASR::If_t>(*body[k])) {
                auto *branch = ASR::down_cast<ASR::If_t>(body[k]);
                std::vector<ASR::If_t*> inner = parents;
                std::vector<Guard> inner_guards = guards;
                inner.push_back(branch);
                inner_guards.push_back(guard_of(branch->m_test, true));
                ASR::Assignment_t *a2 = find_store(branch->m_body,
                    branch->n_body, array, index, inner, inner_guards);
                if (a2) {
                    if (found) return nullptr;
                    found = a2;
                    parents = inner;
                    guards = inner_guards;
                }
                // A store in an ELSE would need the negated condition, and is
                // not this pattern.
                if (branch->n_orelse) {
                    if (map_store_in(branch->m_orelse, branch->n_orelse,
                                array, index)) return nullptr;
                }
            }
        }
        return found;
    }

    static bool map_store_in(ASR::stmt_t **body, size_t n,
            ASR::symbol_t *array, ASR::symbol_t *index) {
        for (size_t k = 0; k < n; k++) {
            if (map_store(body[k], array, index)) return true;
            if (ASR::is_a<ASR::If_t>(*body[k])) {
                auto *branch = ASR::down_cast<ASR::If_t>(body[k]);
                if (map_store_in(branch->m_body, branch->n_body, array, index)
                        || map_store_in(branch->m_orelse, branch->n_orelse,
                            array, index)) {
                    return true;
                }
            }
        }
        return false;
    }

    // Removes a statement from the list of the IF that holds it, and the IF
    // itself once its body is empty, with no ELSE.
    void drop_store(ASR::stmt_t **loop_body, size_t &loop_n,
            std::vector<ASR::If_t*> &parents, ASR::Assignment_t *store) {
        ASR::stmt_t **body = parents.empty() ? loop_body : parents.back()->m_body;
        size_t &n = parents.empty()
            ? loop_n : parents.back()->n_body;
        for (size_t k = 0; k < n; k++) {
            if (body[k] == stmt_of(store)) {
                for (size_t j = k; j + 1 < n; j++) body[j] = body[j + 1];
                n--;
                break;
            }
        }
        for (size_t p = parents.size(); p-- > 0; ) {
            ASR::If_t *branch = parents[p];
            if (branch->n_body != 0 || branch->n_orelse != 0) break;
            if (!branch->m_test || !pure_expr(branch->m_test)) break;
            ASR::stmt_t **holder = p == 0 ? loop_body
                : parents[p - 1]->m_body;
            size_t &hn = p == 0 ? loop_n : parents[p - 1]->n_body;
            for (size_t k = 0; k < hn; k++) {
                if (holder[k] == stmt_of(branch)) {
                    for (size_t j = k; j + 1 < hn; j++) holder[j] = holder[j + 1];
                    hn--;
                    break;
                }
            }
        }
    }

    bool try_forward(SymbolTable *scope, ASR::stmt_t **body, size_t &n,
            ASR::stmt_t **root, size_t &root_n, size_t i) {
        ASR::expr_t *end = nullptr;
        ASR::symbol_t *index = nullptr;
        ASR::stmt_t **loop_body = nullptr;
        size_t loop_n = 0;
        if (ASR::is_a<ASR::DoConcurrentLoop_t>(*body[i])) {
            auto *l = ASR::down_cast<ASR::DoConcurrentLoop_t>(body[i]);
            if (l->n_head != 1 || l->n_local || l->n_shared || l->n_reduction)
                return false;
            if (!one(l->m_head[0].m_start) || !one(l->m_head[0].m_increment))
                return false;
            end = l->m_head[0].m_end;
            index = variable(l->m_head[0].m_v);
            loop_body = l->m_body;
            loop_n = l->n_body;
        } else if (ASR::is_a<ASR::DoLoop_t>(*body[i])) {
            auto *l = ASR::down_cast<ASR::DoLoop_t>(body[i]);
            if (!one(l->m_head.m_start) || !one(l->m_head.m_increment))
                return false;
            end = l->m_head.m_end;
            index = variable(l->m_head.m_v);
            loop_body = l->m_body;
            loop_n = l->n_body;
        } else {
            return false;
        }
        if (!index || !end) return false;

        // The arrays this loop stores at its own index. One of them is
        // forwarded now; another one waits for the next round.
        std::set<ASR::symbol_t*> arrays;
        find_stores(loop_body, loop_n, index, arrays);
        if (arrays.empty()) return false;
        ASR::symbol_t *array = *arrays.begin();

        std::vector<ASR::If_t*> parents;
        std::vector<Guard> guards;
        ASR::Assignment_t *store = find_store(loop_body, loop_n, array,
            index, parents, guards);
        if (!store) {
            return false;
        }
        ASR::expr_t *value = store->m_value;

        // A local, rank-one array of exactly the loop's extent.
        std::set<ASR::symbol_t*> written;
        collect_written(body, n, written);
        if (ASRUtils::symbol_parent_symtab(array) != scope) {
            return false;
        }
        auto *v = ASR::down_cast<ASR::Variable_t>(array);
        if (v->m_intent != ASR::intentType::Local
                || v->m_storage != ASR::storage_typeType::Default
                || v->m_symbolic_value || v->m_value
                || gpu_may_be_aliased(array, fusion.address_taken)
                || !ASR::is_a<ASR::Array_t>(*v->m_type)) {
            return false;
        }
        auto *type = ASR::down_cast<ASR::Array_t>(v->m_type);
        if (type->n_dims != 1 || !one(type->m_dims[0].m_start)
                || !PassUtils::same_expression(
                    extent(type->m_dims[0].m_length, written), end)) {
            return false;
        }

        // The map's value is pure and recomputable from its own index.
        if (!pure_expr(value)) {
            return false;
        }
        if (!gpu_calls_self_contained(value)) {
            return false;
        }
        if (!stable(value, scope)) {
            return false;
        }
        std::set<ASR::symbol_t*> value_symbols;
        gpu_collect_symbols(value, value_symbols);
        if (value_symbols.count(array)) {
            return false;
        }

        // Nothing the value reads may be assigned in the block.
        written.erase(array);
        for (auto *sym : value_symbols) {
            if (sym == index || sym == array) continue;
            if (written.count(sym)) {
                return false;
            }
        }

        // No read of the map before its store, here or in the loop.
        Reader before(array, nullptr);
        scan(body, i, before);
        if (before.other_use || !before.reads.empty()) {
            return false;
        }
        size_t top = parents.empty() ? 0 : SIZE_MAX;
        for (size_t k = 0; k < loop_n; k++) {
            if (parents.empty()) {
                if (loop_body[k] == stmt_of(store)) {
                    top = k;
                    break;
                }
            } else if (loop_body[k] == stmt_of(parents.front())) {
                top = k;
                break;
            }
        }
        if (top == SIZE_MAX) return false;
        Reader early(array, stmt_of(store));
        for (size_t k = 0; k < top; k++) early.visit_stmt(*loop_body[k]);
        if (early.other_use || !early.reads.empty() || !early.bounds.empty()) {
            return false;
        }
        if (!parents.empty()) {
            Reader in_if(array, stmt_of(store));
            scan(parents.back()->m_body, parents.back()->n_body, in_if);
            if (in_if.other_use || !in_if.reads.empty()
                    || !in_if.bounds.empty()) {
                return false;
            }
        }

        // Every later read is an element read under the store's conditions.
        Reader late(array, stmt_of(store));
        scan(loop_body + top, loop_n - top, late);
        if (late.other_use) {
            return false;
        }
        Reader after(array, nullptr);
        scan(body + i + 1, n - i - 1, after);
        if (after.other_use) {
            return false;
        }
        // Every use in the block must be one of the reads replaced below:
        // the same map may be named before the loop or in another branch.
        Reader everywhere(array, stmt_of(store));
        scan(root, root_n, everywhere);
        if (everywhere.other_use) {
            return false;
        }
        std::vector<ASR::ArrayItem_t*> reads = late.reads;
        std::vector<std::vector<Guard>> read_guards = late.read_guards;
        std::vector<ASR::ArrayBound_t*> bounds = late.bounds;
        reads.insert(reads.end(), after.reads.begin(), after.reads.end());
        read_guards.insert(read_guards.end(), after.read_guards.begin(),
            after.read_guards.end());
        bounds.insert(bounds.end(), after.bounds.begin(), after.bounds.end());
        if (reads.empty()) {
            return false;
        }
        std::set<ASR::expr_t*> covered;
        for (auto *item : reads) covered.insert(&item->base);
        for (auto *b : bounds) covered.insert(&b->base);
        if (everywhere.reads.size() + everywhere.bounds.size()
                != covered.size()) {
            return false;
        }
        for (auto *item : everywhere.reads) {
            if (!covered.count(&item->base)) return false;
        }
        for (auto *b : everywhere.bounds) {
            if (!covered.count(&b->base)) return false;
        }
        for (auto &g : read_guards) {
            if (!implies(g, guards)) {
                return false;
            }
        }

        // Recompute every read before anything is changed.
        std::map<ASR::expr_t*, ASR::expr_t*> replacements;
        for (auto *item : reads) {
            GpuDuplicator dup(al, scope);
            dup.value_map[index] = item->m_args[0].m_right;
            ASR::expr_t *copy = dup.duplicate_expr(value);
            if (!dup.ok || copy == nullptr) {
                return false;
            }
            replacements[&item->base] = copy;
        }
        for (auto *b : bounds) {
            if (!b->m_dim || !ASR::is_a<ASR::IntegerConstant_t>(*b->m_dim)
                    || ASR::down_cast<ASR::IntegerConstant_t>(b->m_dim)->m_n
                        != 1) {
                return false;
            }
            if (b->m_bound == ASR::arrayboundType::LBound) {
                replacements[&b->base] = ASRUtils::EXPR(
                    ASR::make_IntegerConstant_t(al, b->base.base.loc, 1,
                        b->m_type));
            } else if (b->m_bound == ASR::arrayboundType::UBound) {
                GpuDuplicator dup(al, scope);
                ASR::expr_t *copy = dup.duplicate_expr(end);
                if (!dup.ok || copy == nullptr) {
                    return false;
                }
                replacements[&b->base] = copy;
            } else {
                return false;
            }
        }

        auto replace = [&](ASR::stmt_t **b, size_t count) {
            gpu_replace_exprs(b, count, [&](ASR::expr_t *e) -> ASR::expr_t* {
                auto it = replacements.find(e);
                return it == replacements.end() ? nullptr : it->second;
            });
        };
        if (loop_n > top + 1) {
            replace(loop_body + top + 1, loop_n - top - 1);
        }
        replace(body + i + 1, n - i - 1);
        if (!parents.empty() && store) {
            ASR::stmt_t **inner = parents.back()->m_body;
            size_t inn = parents.back()->n_body;
            for (size_t k = 0; k < inn; k++) {
                if (inner[k] == stmt_of(store)) {
                    replace(inner + k + 1, inn - k - 1);
                    break;
                }
            }
        }

        // Drop the store, and the loop if it held nothing else.
        drop_store(loop_body, loop_n, parents, store);
        if (ASR::is_a<ASR::DoConcurrentLoop_t>(*body[i])) {
            ASR::down_cast<ASR::DoConcurrentLoop_t>(body[i])->n_body = loop_n;
        } else {
            ASR::down_cast<ASR::DoLoop_t>(body[i])->n_body = loop_n;
        }
        if (loop_n == 0) {
            for (size_t k = i; k + 1 < n; k++) body[k] = body[k + 1];
            n--;
        }

        // The array is gone when nothing refers to it any more.
        struct References : public ASR::BaseWalkVisitor<References> {
            ASR::symbol_t *array;
            bool found = false;
            void visit_Var(const ASR::Var_t &x) {
                if (ASRUtils::symbol_get_past_external(x.m_v) == array) {
                    found = true;
                }
            }
            void visit_BlockCall(const ASR::BlockCall_t &x) {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
                if (!ASR::is_a<ASR::Block_t>(*b)) return;
                auto *blk = ASR::down_cast<ASR::Block_t>(b);
                for (size_t k = 0; k < blk->n_body; k++) {
                    visit_stmt(*blk->m_body[k]);
                }
            }
        } refs;
        refs.array = array;
        for (size_t k = 0; k < n; k++) refs.visit_stmt(*body[k]);
        if (!refs.found) scope->erase_symbol(v->m_name);
        return true;
    }

public:
    PrivateMapForwarding(Allocator &al_, LoopFusion &fusion_)
        : al(al_), fusion(fusion_) {}

    // `root` is the statement list of the block whose symbol table is
    // `scope`: a map may be named anywhere in it, not only beside its loop.
    void list(SymbolTable *scope, ASR::stmt_t **body, size_t &n,
            ASR::stmt_t **root, size_t &root_n) {
        for (size_t i = 0; i < n; i++) {
            ASR::stmt_t *s = body[i];
            if (ASR::is_a<ASR::BlockCall_t>(*s)) {
                auto *symbol = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                if (!ASR::is_a<ASR::Block_t>(*symbol)) continue;
                auto *block = ASR::down_cast<ASR::Block_t>(symbol);
                list(block->m_symtab, block->m_body, block->n_body,
                    block->m_body, block->n_body);
            } else if (ASR::is_a<ASR::DoLoop_t>(*s)) {
                auto *loop = ASR::down_cast<ASR::DoLoop_t>(s);
                list(scope, loop->m_body, loop->n_body, root, root_n);
            } else if (ASR::is_a<ASR::DoConcurrentLoop_t>(*s)) {
                auto *loop = ASR::down_cast<ASR::DoConcurrentLoop_t>(s);
                list(scope, loop->m_body, loop->n_body, root, root_n);
            } else if (ASR::is_a<ASR::If_t>(*s)) {
                auto *branch = ASR::down_cast<ASR::If_t>(s);
                list(scope, branch->m_body, branch->n_body, root, root_n);
                list(scope, branch->m_orelse, branch->n_orelse, root, root_n);
            }
        }
        for (size_t i = 0; i < n; ) {
            if (!try_forward(scope, body, n, root, root_n, i)) {
                i++;
            }
        }
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
    gpu_collect_address_taken(unit, f.address_taken);
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

void pass_gpu_private_reduction_fusion(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options) {
    if (gpu_device_selected(pass_options) == GpuDevice::None
            || !gpu_loop_transform_enabled(pass_options, GpuLoopFuse)) return;
    GpuRarelyRun rarely_run;
    LoopFusion fusion(al, pass_options, rarely_run);
    gpu_collect_address_taken(unit, fusion.address_taken);
    PrivateReductionFusion private_fusion(al, fusion);
    std::function<void(SymbolTable*)> visit = [&](SymbolTable *scope) {
        for (auto &entry : scope->get_scope()) {
            if (ASR::is_a<ASR::Function_t>(*entry.second)) {
                auto *fn = ASR::down_cast<ASR::Function_t>(entry.second);
                if (ASRUtils::is_device_only_function(*fn)) {
                    private_fusion.list(fn->m_symtab, fn->m_body, fn->n_body);
                }
                visit(fn->m_symtab);
            } else if (ASR::is_a<ASR::Module_t>(*entry.second)) {
                visit(ASR::down_cast<ASR::Module_t>(entry.second)->m_symtab);
            }
        }
    };
    visit(unit.m_symtab);
    PassUtils::UpdateDependenciesVisitor update(al);
    update.visit_TranslationUnit(unit);
}


void pass_gpu_private_map_forwarding(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options) {
    if (gpu_device_selected(pass_options) == GpuDevice::None
            || !gpu_loop_transform_enabled(pass_options, GpuLoopFuse)) return;
    GpuRarelyRun rarely_run;
    LoopFusion fusion(al, pass_options, rarely_run);
    gpu_collect_address_taken(unit, fusion.address_taken);
    PrivateMapForwarding forwarding(al, fusion);
    std::function<void(SymbolTable*)> visit = [&](SymbolTable *scope) {
        for (auto &entry : scope->get_scope()) {
            if (ASR::is_a<ASR::Function_t>(*entry.second)) {
                auto *fn = ASR::down_cast<ASR::Function_t>(entry.second);
                if (ASRUtils::is_device_only_function(*fn)) {
                    forwarding.list(fn->m_symtab, fn->m_body, fn->n_body,
                        fn->m_body, fn->n_body);
                }
                visit(fn->m_symtab);
            } else if (ASR::is_a<ASR::Module_t>(*entry.second)) {
                visit(ASR::down_cast<ASR::Module_t>(entry.second)->m_symtab);
            }
        }
    };
    visit(unit.m_symtab);
    PassUtils::UpdateDependenciesVisitor update(al);
    update.visit_TranslationUnit(unit);
}

} // namespace LCompilers
