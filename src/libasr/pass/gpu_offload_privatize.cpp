#include <algorithm>
#include <functional>
#include <map>
#include <set>
#include <string>
#include <utility>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/access_paths.h>
#include <libasr/pass/gpu_offload_collect.h>
#include <libasr/pass/gpu_offload_preflight.h>
#include <libasr/pass/gpu_offload_rewrite.h>
#include <libasr/pass/gpu_offload_visitor.h>
#include <libasr/pass/symbol_expr_substitution.h>

namespace LCompilers {

namespace {

using namespace AccessPaths;

// Whether statements and expressions of a procedure may access the values
// of one of its arrays (access_paths.h). Asking about its shape does not.
// The array is one `privatize_iteration_arrays` may make local: no pointer
// may point to it, and no procedure but this one names it. So what
// access_paths.h takes a call of a contained procedure, or a pointer it
// cannot resolve, to access -- anything -- includes the array only where
// the same statement names it as well (an associate name).
class GpuArrayAccess {
public:
    GpuArrayAccess(ASR::symbol_t *sym, SymbolTable *routine_,
            const PointerTargets &pointers_,
            std::map<ASR::Function_t*, bool> &outside_)
        : routine(routine_), pointers(pointers_), outside(outside_) {
        path.root = sym;
    }

    bool stmt(ASR::stmt_t *s) {
        return accesses([&](auto &v) { v.visit_stmt(*s); });
    }

    bool expr(ASR::expr_t *e) {
        return e != nullptr
            && accesses([&](auto &v) { v.visit_expr(*e); });
    }

    bool head(const ASR::do_loop_head_t &h) {
        return expr(h.m_v) || expr(h.m_start) || expr(h.m_end)
            || expr(h.m_increment);
    }

    bool clause(ASR::omp_clause_t *x) {
        return accesses([&](auto &v) { v.visit_omp_clause(*x); });
    }

    // What the declarations of a BLOCK evaluate on entry.
    bool declarations(SymbolTable *scope) {
        Accesses acc;
        AccessCollector c(acc, routine, pointers, outside);
        c.declarations(scope);
        if (acc.everything) {
            std::set<ASR::symbol_t*> names;
            VarSymbolCollector n(names);
            for (auto &item : scope->get_scope()) n.visit_symbol(*item.second);
            acc.everything = names.count(path.root) > 0;
        }
        return conflicts(acc, path, routine);
    }

    // The same question asked about another variable of the procedure.
    GpuArrayAccess of(ASR::symbol_t *other) {
        return GpuArrayAccess(other, routine, pointers, outside);
    }

    SymbolTable *procedure() const { return routine; }

private:
    SymbolTable *routine;
    const PointerTargets &pointers;
    std::map<ASR::Function_t*, bool> &outside;
    Path path;

    template <typename Visit>
    bool accesses(const Visit &visit) {
        Accesses acc;
        AccessCollector c(acc, routine, pointers, outside);
        visit(c);
        if (acc.everything) {
            std::set<ASR::symbol_t*> names;
            VarSymbolCollector n(names);
            visit(n);
            acc.everything = names.count(path.root) > 0;
        }
        return conflicts(acc, path, routine);
    }
};

// Whether a procedure has a label a jump may enter a list of statements
// at, in its middle, which the reading below, from the start of each list,
// does not follow. Jumps out of a statement are `Leaves` (access_paths.h).
class GpuJumpFinder : public ASRUtils::BlockBodyWalkVisitor<GpuJumpFinder> {
public:
    bool found = false;
    void visit_GoTo(const ASR::GoTo_t &) { found = true; }
    void visit_GoToTarget(const ASR::GoToTarget_t &) { found = true; }
    void visit_IfArithmetic(const ASR::IfArithmetic_t &) { found = true; }
};

class GpuAssociateFinder
        : public ASRUtils::BlockBodyWalkVisitor<GpuAssociateFinder> {
public:
    bool found = false;
    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &) {
        found = true;
    }
};

// Whether every ASSOCIATE in `stmts` is one the kernel inlines once the
// statements are a BLOCK's (rewrite_loop_body_for_kernel): a statement of
// that BLOCK or of a BLOCK it reaches through BLOCKs (`in_block`), or one
// directly in such an ASSOCIATE. Any other one would reach the kernel
// whole.
bool gpu_associates_inlined(ASR::stmt_t **stmts, size_t n_stmts,
        bool in_block) {
    for (size_t i = 0; i < n_stmts; i++) {
        ASR::stmt_t *s = stmts[i];
        if (in_block && ASR::is_a<ASR::BlockCall_t>(*s) &&
                ASR::is_a<ASR::Block_t>(
                    *ASR::down_cast<ASR::BlockCall_t>(s)->m_m)) {
            ASR::Block_t *b = ASR::down_cast<ASR::Block_t>(
                ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
            if (!gpu_associates_inlined(b->m_body, b->n_body, true)) {
                return false;
            }
            continue;
        }
        if (ASR::is_a<ASR::AssociateBlockCall_t>(*s) &&
                ASR::is_a<ASR::AssociateBlock_t>(
                    *ASR::down_cast<ASR::AssociateBlockCall_t>(s)->m_m)) {
            ASR::AssociateBlock_t *ab = ASR::down_cast<ASR::AssociateBlock_t>(
                ASR::down_cast<ASR::AssociateBlockCall_t>(s)->m_m);
            if (!gpu_associates_inlined(ab->m_body, ab->n_body, false)) {
                return false;
            }
            continue;
        }
        GpuAssociateFinder finder;
        finder.visit_stmt(*s);
        if (finder.found) return false;
    }
    return true;
}

// The BLOCK and ASSOCIATE constructs a loop body runs that belong to
// `scope`: those of the body itself, not those nested in one of them.
class GpuScopedConstructs
        : public ASRUtils::BlockBodyWalkVisitor<GpuScopedConstructs> {
public:
    explicit GpuScopedConstructs(SymbolTable *scope_) : scope(scope_) {}

    std::vector<ASR::symbol_t*> found;

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        add(x.m_m);
        ASRUtils::BlockBodyWalkVisitor<GpuScopedConstructs>
            ::visit_BlockCall(x);
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        add(x.m_m);
        ASRUtils::BlockBodyWalkVisitor<GpuScopedConstructs>
            ::visit_AssociateBlockCall(x);
    }

private:
    SymbolTable *scope;

    void add(ASR::symbol_t *s) {
        if ((ASR::is_a<ASR::Block_t>(*s) ||
                    ASR::is_a<ASR::AssociateBlock_t>(*s)) &&
                scope->get_symbol(ASRUtils::symbol_name(s)) == s &&
                std::find(found.begin(), found.end(), s) == found.end()) {
            found.push_back(s);
        }
    }
};

// What a run of statements does with the value an array held when the
// run was entered.
//   Untouched -- never reads it: the array is not accessed, or it is first
//                accessed by a definition of all of it on every path
//                that accesses it.
//   Defined   -- every path that goes on past the run defines all of it
//                before anything reads it.
//   Read      -- may read it, or this reading cannot tell.
// A path that leaves the run elsewhere -- `exit`, `cycle`, `return` -- is
// not one that goes on past it, and is reported apart (see `run`).
enum class GpuArrayFlow { Untouched, Defined, Read };

// Reads statements in the order they run, to find whether they may read
// the value `sym` held when they were entered. Every shape it does not
// know is taken to read it.
class GpuArrayFlowReader {
public:
    ASR::symbol_t *sym;
    ASR::Array_t *arr;
    GpuArrayAccess &access;

    GpuArrayFlowReader(ASR::symbol_t *sym_, GpuArrayAccess &access_,
            const std::set<ASR::symbol_t*> &host_associated_)
        : sym(sym_), arr(ASR::down_cast<ASR::Array_t>(
            ASRUtils::symbol_type(sym_))), access(access_),
            host_associated(host_associated_) {}

    // `escaped` is set when a path may leave the run elsewhere than past
    // its end, by a jump out of one of its statements, wherever that
    // statement nests it. Such a path skips what the rest of the run
    // defines.
    GpuArrayFlow run(ASR::stmt_t **body, size_t n_body, bool &escaped) {
        counters.clear();
        elements.clear();
        for (size_t i = 0; i < n_body; i++) {
            Leaves leaves;
            leaves.visit_stmt(*body[i]);
            if (leaves.found) escaped = true;
            GpuArrayFlow f = stmt(body[i]);
            if (f != GpuArrayFlow::Untouched) return f;
            count(body[i]);
        }
        return GpuArrayFlow::Untouched;
    }

private:
    const std::set<ASR::symbol_t*> &host_associated;

    // What the statements of the run read so far tell: the values of
    // integer locals known at this point of it, and the elements of the
    // array, by their offset in array element order, it has defined.
    // A run that defines every element one by one -- the array
    // constructor lowering `k = 1; t(k) = a; k = k + 1; t(k) = b; ...`,
    // or `t(1) = a; t(2) = b` -- defines all of it.
    std::map<ASR::symbol_t*, int64_t> counters;
    std::set<int64_t> elements;

    // An integer local that only a statement that names it can change.
    bool trackable(ASR::symbol_t *s) {
        if (s == sym || !ASR::is_a<ASR::Variable_t>(*s)) return false;
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
        return ASR::is_a<ASR::Integer_t>(*v->m_type)
            && v->m_intent == ASR::intentType::Local
            && !v->m_is_volatile && v->m_common_block == nullptr
            && !reachable_elsewhere(s, access.procedure())
            && host_associated.count(s) == 0;
    }

    bool evaluate(ASR::expr_t *e, int64_t &v) {
        if (e == nullptr) return false;
        if (constant(e, v)) return true;
        switch (e->type) {
            case ASR::exprType::Var: {
                auto it = counters.find(ASR::down_cast<ASR::Var_t>(e)->m_v);
                if (it == counters.end()) return false;
                v = it->second;
                return true;
            }
            case ASR::exprType::ArrayBound: {
                // `lbound(t, d)` of a dimension with constant bounds, as
                // the array constructor lowering starts its counter.
                ASR::ArrayBound_t *b = ASR::down_cast<ASR::ArrayBound_t>(e);
                int64_t dim = 0, start = 0, length = 0;
                if (!ASR::is_a<ASR::Var_t>(*b->m_v) ||
                        ASR::down_cast<ASR::Var_t>(b->m_v)->m_v != sym ||
                        !constant(b->m_dim, dim) || dim < 1 ||
                        dim > (int64_t)arr->n_dims ||
                        !constant(arr->m_dims[dim - 1].m_start, start) ||
                        !constant(arr->m_dims[dim - 1].m_length, length)) {
                    return false;
                }
                v = b->m_bound == ASR::arrayboundType::LBound
                    ? start : start + length - 1;
                return true;
            }
            case ASR::exprType::Cast: {
                ASR::Cast_t *c = ASR::down_cast<ASR::Cast_t>(e);
                return c->m_kind == ASR::cast_kindType::IntegerToInteger
                    && evaluate(c->m_arg, v);
            }
            case ASR::exprType::IntegerBinOp: {
                ASR::IntegerBinOp_t *b = ASR::down_cast<ASR::IntegerBinOp_t>(
                    e);
                int64_t l = 0, r = 0;
                if (!evaluate(b->m_left, l) || !evaluate(b->m_right, r)) {
                    return false;
                }
                if (b->m_op == ASR::binopType::Add) v = l + r;
                else if (b->m_op == ASR::binopType::Sub) v = l - r;
                else return false;
                return true;
            }
            default: return false;
        }
    }

    // Follows the integer locals a statement the run went on past
    // assigns. Any statement other than such an assignment, or the
    // definition of an element, may change them in ways not followed
    // here, and forgets them all.
    void count(ASR::stmt_t *s) {
        if (ASR::is_a<ASR::Assignment_t>(*s)) {
            ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(s);
            if (a->m_overloaded == nullptr && ASR::is_a<ASR::Var_t>(
                    *a->m_target)) {
                ASR::symbol_t *t = ASR::down_cast<ASR::Var_t>(
                    a->m_target)->m_v;
                int64_t v = 0;
                bool known = trackable(t) && evaluate(a->m_value, v);
                counters.erase(t);
                if (known) {
                    counters[t] = v;
                } else if (access.of(t).expr(a->m_value)) {
                    counters.clear();
                }
                return;
            }
            if (element(s, nullptr)) return;
        }
        counters.clear();
    }

    // Whether an element of the array, its subscripts values known here,
    // is one the run has defined; `offset` is set to its position in
    // array element order.
    bool known_element(const ASR::ArrayItem_t &item, int64_t &offset,
            bool defined) {
        if (!ASR::is_a<ASR::Var_t>(*item.m_v) ||
                ASR::down_cast<ASR::Var_t>(item.m_v)->m_v != sym ||
                item.n_args != arr->n_dims) {
            return false;
        }
        int64_t position = 0, stride = 1;
        for (size_t d = 0; d < item.n_args; d++) {
            const ASR::array_index_t &idx = item.m_args[d];
            int64_t start = 0, length = 0, v = 0;
            if (idx.m_left || idx.m_step ||
                    !constant(arr->m_dims[d].m_start, start) ||
                    !constant(arr->m_dims[d].m_length, length) ||
                    !evaluate(idx.m_right, v) ||
                    v < start || v >= start + length) {
                return false;
            }
            position += (v - start) * stride;
            stride *= length;
        }
        offset = position;
        return !defined || elements.count(position) > 0;
    }

    // Whether `e` reads the array only through elements the run has
    // defined, `t(2) + 1.0` after `t(2) = a`.
    bool reads_defined(ASR::expr_t *e) {
        if (!access.expr(e)) return true;
        struct Reads : public ASR::BaseWalkVisitor<Reads> {
            GpuArrayFlowReader &r;
            bool ok = true;
            explicit Reads(GpuArrayFlowReader &r_) : r(r_) {}
            void visit_ArrayItem(const ASR::ArrayItem_t &x) {
                int64_t offset = 0;
                if (ASR::is_a<ASR::Var_t>(*x.m_v) &&
                        ASR::down_cast<ASR::Var_t>(x.m_v)->m_v == r.sym) {
                    ok = ok && r.known_element(x, offset, true);
                    return;
                }
                ASR::BaseWalkVisitor<Reads>::visit_ArrayItem(x);
            }
            void visit_Var(const ASR::Var_t &x) {
                if (x.m_v == r.sym) ok = false;
            }
        } reads(*this);
        reads.visit_expr(*e);
        return reads.ok;
    }

    // `t(i1, ..., ir) = v`, each subscript a value known here within the
    // declared bounds of its dimension, where `v` reads `t` only through
    // elements defined before and names no integer local followed here.
    // `offset` is set to the element's position in array element order.
    bool element(ASR::stmt_t *s, int64_t *offset) {
        if (!ASR::is_a<ASR::Assignment_t>(*s)) return false;
        ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(s);
        if (a->m_overloaded != nullptr ||
                !ASR::is_a<ASR::ArrayItem_t>(*a->m_target) ||
                !reads_defined(a->m_value)) {
            return false;
        }
        for (auto &c : counters) {
            if (access.of(c.first).expr(a->m_value)) return false;
        }
        int64_t position = 0;
        if (!known_element(*ASR::down_cast<ASR::ArrayItem_t>(a->m_target),
                position, false)) {
            return false;
        }
        if (offset) *offset = position;
        return true;
    }

    // The number of elements of the array, when it is a constant.
    bool extent(int64_t &n) {
        n = 1;
        for (size_t d = 0; d < arr->n_dims; d++) {
            int64_t length = 0;
            if (!constant(arr->m_dims[d].m_length, length)) return false;
            n *= length;
        }
        return true;
    }

    static bool constant(ASR::expr_t *e, int64_t &v) {
        return e != nullptr
            && ASRUtils::extract_value(ASRUtils::expr_value(e), v);
    }

    bool is_lower_bound(size_t d, ASR::expr_t *e) {
        int64_t s = 0, v = 0;
        return constant(arr->m_dims[d].m_start, s) && constant(e, v)
            && v == s;
    }

    bool is_upper_bound(size_t d, ASR::expr_t *e) {
        int64_t s = 0, l = 0, v = 0;
        ASR::expr_t *length = arr->m_dims[d].m_length;
        // A dummy that cannot change still has the value used on entry
        // when the automatic array's shape was established.
        if (length && ASR::is_a<ASR::Var_t>(*length) &&
                ASR::is_a<ASR::Variable_t>(
                    *ASR::down_cast<ASR::Var_t>(length)->m_v) &&
                !ASRUtils::is_pointer(ASRUtils::symbol_type(
                    ASR::down_cast<ASR::Var_t>(length)->m_v)) &&
                ASR::down_cast<ASR::Variable_t>(
                    ASR::down_cast<ASR::Var_t>(length)->m_v)->m_intent
                    == ASR::intentType::In &&
                constant(arr->m_dims[d].m_start, s) && s == 1 &&
                PassUtils::same_expression(arr->m_dims[d].m_length, e)) {
            return true;
        }
        return constant(arr->m_dims[d].m_start, s)
            && constant(arr->m_dims[d].m_length, l) && constant(e, v)
            && v == s + l - 1;
    }

    static bool unit_step(ASR::expr_t *step) {
        int64_t v = 0;
        return step == nullptr || (ASRUtils::extract_value(
            ASRUtils::expr_value(step), v) && v == 1);
    }

    bool is_whole(ASR::expr_t *e) {
        if (ASR::is_a<ASR::Var_t>(*e)) {
            return ASR::down_cast<ASR::Var_t>(e)->m_v == sym;
        }
        if (!ASR::is_a<ASR::ArraySection_t>(*e)) return false;
        ASR::ArraySection_t *s = ASR::down_cast<ASR::ArraySection_t>(e);
        if (!ASR::is_a<ASR::Var_t>(*s->m_v) ||
                ASR::down_cast<ASR::Var_t>(s->m_v)->m_v != sym ||
                s->n_args != arr->n_dims) {
            return false;
        }
        for (size_t d = 0; d < s->n_args; d++) {
            const ASR::array_index_t &a = s->m_args[d];
            if (a.m_left == nullptr && a.m_step == nullptr &&
                    a.m_right != nullptr) {
                return false;
            }
            if ((a.m_left && !is_lower_bound(d, a.m_left)) ||
                    (a.m_right && !is_upper_bound(d, a.m_right)) ||
                    !unit_step(a.m_step)) {
                return false;
            }
        }
        return true;
    }

    // `t = v`, or `t(:, ...) = v` over all of `t`, where `v` does not
    // name `t`.
    bool defines_whole(ASR::stmt_t *s) {
        if (!ASR::is_a<ASR::Assignment_t>(*s)) return false;
        ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(s);
        return a->m_overloaded == nullptr && is_whole(a->m_target)
            && !access.expr(a->m_value);
    }

    // A perfect nest of DO loops, one per dimension, each running with
    // step 1 over the whole extent of its dimension, around one
    // assignment `t(i1, ..., ir) = v` that names each loop's variable as
    // the subscript of its dimension and whose `v` does not name `t`:
    //   do k = 1, m
    //     t(k) = ...
    //   end do
    bool defines_by_loops(ASR::DoLoop_t *outer) {
        std::vector<ASR::DoLoop_t*> loops;
        ASR::stmt_t *s = &outer->base;
        while (ASR::is_a<ASR::DoLoop_t>(*s)) {
            ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(s);
            if (dl->n_body != 1 || dl->n_orelse != 0 ||
                    dl->m_head.m_v == nullptr ||
                    !ASR::is_a<ASR::Var_t>(*dl->m_head.m_v) ||
                    access.head(dl->m_head) ||
                    !unit_step(dl->m_head.m_increment)) {
                return false;
            }
            loops.push_back(dl);
            s = dl->m_body[0];
        }
        if (!ASR::is_a<ASR::Assignment_t>(*s)) return false;
        ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(s);
        if (a->m_overloaded != nullptr || access.expr(a->m_value) ||
                !ASR::is_a<ASR::ArrayItem_t>(*a->m_target)) {
            return false;
        }
        ASR::ArrayItem_t *item = ASR::down_cast<ASR::ArrayItem_t>(
            a->m_target);
        if (!ASR::is_a<ASR::Var_t>(*item->m_v) ||
                ASR::down_cast<ASR::Var_t>(item->m_v)->m_v != sym ||
                item->n_args != arr->n_dims ||
                loops.size() != arr->n_dims) {
            return false;
        }
        std::vector<bool> covered(loops.size(), false);
        for (size_t d = 0; d < item->n_args; d++) {
            const ASR::array_index_t &idx = item->m_args[d];
            if (idx.m_left || idx.m_step || idx.m_right == nullptr ||
                    !ASR::is_a<ASR::Var_t>(*idx.m_right)) {
                return false;
            }
            ASR::symbol_t *iv = ASR::down_cast<ASR::Var_t>(
                idx.m_right)->m_v;
            bool matched = false;
            for (size_t l = 0; l < loops.size(); l++) {
                ASR::do_loop_head_t &h = loops[l]->m_head;
                if (ASR::down_cast<ASR::Var_t>(h.m_v)->m_v != iv) continue;
                if (covered[l] || !is_lower_bound(d, h.m_start) ||
                        !is_upper_bound(d, h.m_end)) {
                    return false;
                }
                covered[l] = true;
                matched = true;
                break;
            }
            if (!matched) return false;
        }
        return true;
    }

    // A list of statements nested in one of the run: a path that leaves it
    // early may go on past the statement that holds it without having
    // defined the array (`exit` of a BLOCK construct).
    GpuArrayFlow nested(ASR::stmt_t **body, size_t n_body) {
        // The list is read on its own; what the run around it knows is
        // taken up again after it.
        std::map<ASR::symbol_t*, int64_t> outer_counters;
        std::set<int64_t> outer_elements;
        std::swap(outer_counters, counters);
        std::swap(outer_elements, elements);
        bool escaped = false;
        GpuArrayFlow f = run(body, n_body, escaped);
        std::swap(outer_counters, counters);
        std::swap(outer_elements, elements);
        return escaped && f == GpuArrayFlow::Defined
            ? GpuArrayFlow::Untouched : f;
    }

    // A loop may run no iteration, so what its body defines is not
    // defined after it; what matters is only whether the body reads.
    GpuArrayFlow loop(bool head_names, ASR::stmt_t **body, size_t n_body,
            ASR::stmt_t **orelse, size_t n_orelse) {
        if (head_names || nested(body, n_body) == GpuArrayFlow::Read
                || nested(orelse, n_orelse) == GpuArrayFlow::Read) {
            return GpuArrayFlow::Read;
        }
        return GpuArrayFlow::Untouched;
    }

    GpuArrayFlow stmt(ASR::stmt_t *s) {
        switch (s->type) {
            case ASR::stmtType::Exit:
            case ASR::stmtType::Cycle:
            case ASR::stmtType::Return:
            case ASR::stmtType::GoTo: {
                // No path goes on past it; `run` notes where it goes.
                return GpuArrayFlow::Defined;
            }
            case ASR::stmtType::Stop:
            case ASR::stmtType::ErrorStop: {
                // Nothing runs after it that could read the array.
                return access.stmt(s) ? GpuArrayFlow::Read
                    : GpuArrayFlow::Defined;
            }
            default: break;
        }
        if (!access.stmt(s)) return GpuArrayFlow::Untouched;
        switch (s->type) {
            case ASR::stmtType::Assignment: {
                if (defines_whole(s)) return GpuArrayFlow::Defined;
                int64_t offset = 0, n = 0;
                if (!element(s, &offset)) return GpuArrayFlow::Read;
                elements.insert(offset);
                return extent(n) && (int64_t)elements.size() == n
                    ? GpuArrayFlow::Defined : GpuArrayFlow::Untouched;
            }
            case ASR::stmtType::If: {
                ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                if (access.expr(x->m_test)) return GpuArrayFlow::Read;
                GpuArrayFlow a = nested(x->m_body, x->n_body);
                GpuArrayFlow b = nested(x->m_orelse, x->n_orelse);
                if (a == GpuArrayFlow::Read || b == GpuArrayFlow::Read) {
                    return GpuArrayFlow::Read;
                }
                if (a == GpuArrayFlow::Defined && b == GpuArrayFlow::Defined) {
                    return GpuArrayFlow::Defined;
                }
                return GpuArrayFlow::Untouched;
            }
            case ASR::stmtType::BlockCall: {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                if (!ASR::is_a<ASR::Block_t>(*b)) return GpuArrayFlow::Read;
                ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                if (access.declarations(blk->m_symtab)) {
                    return GpuArrayFlow::Read;
                }
                return nested(blk->m_body, blk->n_body);
            }
            case ASR::stmtType::AssociateBlockCall: {
                // The selectors are bound by the leading statements of
                // the body, which `access` reads like any other: one that
                // names the array is taken to read it.
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::AssociateBlockCall_t>(s)->m_m);
                if (!ASR::is_a<ASR::AssociateBlock_t>(*b)) {
                    return GpuArrayFlow::Read;
                }
                ASR::AssociateBlock_t *ab =
                    ASR::down_cast<ASR::AssociateBlock_t>(b);
                if (access.declarations(ab->m_symtab)) {
                    return GpuArrayFlow::Read;
                }
                return nested(ab->m_body, ab->n_body);
            }
            case ASR::stmtType::DoLoop: {
                ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(s);
                if (defines_by_loops(x)) return GpuArrayFlow::Defined;
                return loop(access.head(x->m_head), x->m_body, x->n_body,
                    x->m_orelse, x->n_orelse);
            }
            case ASR::stmtType::WhileLoop: {
                ASR::WhileLoop_t *x = ASR::down_cast<ASR::WhileLoop_t>(s);
                return loop(access.expr(x->m_test), x->m_body, x->n_body,
                    x->m_orelse, x->n_orelse);
            }
            case ASR::stmtType::DoConcurrentLoop: {
                ASR::DoConcurrentLoop_t *x =
                    ASR::down_cast<ASR::DoConcurrentLoop_t>(s);
                bool head_names = false;
                for (size_t i = 0; i < x->n_head; i++) {
                    head_names = head_names || access.head(x->m_head[i]);
                }
                for (size_t i = 0; i < x->n_shared; i++) {
                    head_names = head_names || access.expr(x->m_shared[i]);
                }
                for (size_t i = 0; i < x->n_local; i++) {
                    head_names = head_names || access.expr(x->m_local[i]);
                }
                for (size_t i = 0; i < x->n_reduction; i++) {
                    head_names = head_names
                        || access.expr(x->m_reduction[i].m_arg);
                }
                return loop(head_names, x->m_body, x->n_body, nullptr, 0);
            }
            case ASR::stmtType::OMPRegion: {
                ASR::OMPRegion_t *x = ASR::down_cast<ASR::OMPRegion_t>(s);
                bool head_names = false;
                for (size_t i = 0; i < x->n_clauses; i++) {
                    head_names = head_names || access.clause(x->m_clauses[i]);
                }
                return loop(head_names, x->m_body, x->n_body, nullptr, 0);
            }
            default: return GpuArrayFlow::Read;
        }
    }
};

// One step of the way from a procedure's statements down to a statement
// nested in them: the statement list that holds the next step, the
// position in it, whether running that list again is possible (it is a
// loop body), and the condition of a `do while` evaluated before it runs
// again.
struct GpuStmtPathStep {
    ASR::stmt_t **body;
    size_t n_body;
    size_t index;
    bool repeats;
    ASR::expr_t *test;
};

bool find_stmt_path(ASR::stmt_t **body, size_t n_body, bool repeats,
        ASR::expr_t *test, const ASR::stmt_t *target,
        std::vector<GpuStmtPathStep> &path) {
    for (size_t i = 0; i < n_body; i++) {
        ASR::stmt_t *s = body[i];
        path.push_back({body, n_body, i, repeats, test});
        if (s == target) return true;
        bool found = false;
        switch (s->type) {
            case ASR::stmtType::If: {
                ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                found = find_stmt_path(x->m_body, x->n_body, false, nullptr,
                        target, path)
                    || find_stmt_path(x->m_orelse, x->n_orelse, false,
                        nullptr, target, path);
                break;
            }
            case ASR::stmtType::DoLoop: {
                ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(s);
                found = find_stmt_path(x->m_body, x->n_body, true, nullptr,
                    target, path);
                break;
            }
            case ASR::stmtType::WhileLoop: {
                ASR::WhileLoop_t *x = ASR::down_cast<ASR::WhileLoop_t>(s);
                found = find_stmt_path(x->m_body, x->n_body, true, x->m_test,
                    target, path);
                break;
            }
            case ASR::stmtType::DoConcurrentLoop: {
                ASR::DoConcurrentLoop_t *x =
                    ASR::down_cast<ASR::DoConcurrentLoop_t>(s);
                found = find_stmt_path(x->m_body, x->n_body, true, nullptr,
                    target, path);
                break;
            }
            case ASR::stmtType::OMPRegion: {
                ASR::OMPRegion_t *x = ASR::down_cast<ASR::OMPRegion_t>(s);
                found = find_stmt_path(x->m_body, x->n_body, true, nullptr,
                    target, path);
                break;
            }
            case ASR::stmtType::BlockCall: {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                if (ASR::is_a<ASR::Block_t>(*b)) {
                    ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                    found = find_stmt_path(blk->m_body, blk->n_body, false,
                        nullptr, target, path);
                }
                break;
            }
            case ASR::stmtType::AssociateBlockCall: {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::AssociateBlockCall_t>(s)->m_m);
                if (ASR::is_a<ASR::AssociateBlock_t>(*b)) {
                    ASR::AssociateBlock_t *ab =
                        ASR::down_cast<ASR::AssociateBlock_t>(b);
                    found = find_stmt_path(ab->m_body, ab->n_body, false,
                        nullptr, target, path);
                }
                break;
            }
            default: break;
        }
        if (found) return true;
        path.pop_back();
    }
    return false;
}

// Whether a statement run after `region` may read the value `sym` held
// when the region ended. `path` leads from the procedure's statements to
// the region. The rest of each enclosing list runs after the region, and
// all of an enclosing loop body may run again, after the condition of a
// `do while` is evaluated again; past the outermost list the procedure
// returns, and a local the procedure does not save is gone. Once every
// path past the rest of a list defines the array, the lists around it see
// only that definition -- unless a jump out of a list on the way, which
// may land past any of the lists around it, skips that definition.
bool read_after_region(GpuArrayFlowReader &reader,
        const std::vector<GpuStmtPathStep> &path) {
    bool escaped = false;
    for (size_t p = path.size(); p-- > 0;) {
        const GpuStmtPathStep &step = path[p];
        GpuArrayFlow after = reader.run(step.body + step.index + 1,
            step.n_body - step.index - 1, escaped);
        if (after == GpuArrayFlow::Read) return true;
        bool defined = after == GpuArrayFlow::Defined && !escaped;
        if (step.repeats) {
            if (!defined && reader.access.expr(step.test)) return true;
            bool again_escaped = false;
            if (reader.run(step.body, step.n_body, again_escaped)
                    == GpuArrayFlow::Read) {
                return true;
            }
        }
        if (defined) return false;
    }
    return false;
}

} // namespace

// An array of unspecified locality that every iteration of a DO
// CONCURRENT loop defines whole before it references it is a temporary of
// the iteration:
//
//   do concurrent (j = 1:n)
//     do k = 1, m
//       t(k) = x(j) + k
//     end do
//     g(:, j) = t(m:1:-1)
//   end do
//
// Fortran 2018, 11.1.7.5 lets such an iteration reference only the values
// it defined itself, and an array several iterations define is undefined
// once the loop ends. One buffer every thread writes would be a race, so
// the array becomes a local of a BLOCK around the body, just as if the
// source had declared it there; the kernel then gives each thread its own
// copy, a local array.
// The host's array keeps the value it had before the loop.
//
// An array is only made local where that cannot be told apart: every
// path through the body that accesses it first defines all of it -- by
// an assignment to the whole array, by a nest of DO loops over its full
// declared extent, or by assignments to each of its elements in turn, as
// the lowering of an array constructor fills its temporary -- and no statement that can run after the loop, however
// a jump gets there, may read it before it is defined again. So no
// iteration reads a value it did not define, whether every iteration,
// some or none define the array, and nothing after the loop reads what
// they defined. It is a local, non-saved variable that no pointer and no
// other procedure can reach
// (access_paths.h), that no clause of the loop and no ASSOCIATE around it
// names, and that no contained procedure names.
//
// Storage placement is independent: gpu_kernel_storage places the actual
// lowered locals on the stack or in per-thread global workspace slices.
// Other arrays remain shared, notably distinct elements per iteration
// (`y(i) = ...`).
std::vector<ASR::symbol_t*>
GpuOffloadVisitor::iteration_array_candidates(
        const ASR::OMPRegion_t &region, ParallelLoopNest &work,
        const std::map<ASR::symbol_t*, ASR::expr_t*> &enclosing_assoc_map) {
    std::vector<ASR::symbol_t*> candidates;
    // The procedure, or main program, the loop is in.
    SymbolTable *proc_scope = current_scope;
    while (proc_scope && proc_scope->asr_owner &&
            ASR::is_a<ASR::symbol_t>(*proc_scope->asr_owner)) {
        ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(
            proc_scope->asr_owner);
        if (!ASR::is_a<ASR::Block_t>(*owner) &&
                !ASR::is_a<ASR::AssociateBlock_t>(*owner)) {
            break;
        }
        proc_scope = proc_scope->parent;
    }
    if (proc_scope == nullptr || proc_scope->asr_owner == nullptr ||
            !ASR::is_a<ASR::symbol_t>(*proc_scope->asr_owner)) {
        return candidates;
    }
    ASR::symbol_t *proc = ASR::down_cast<ASR::symbol_t>(
        proc_scope->asr_owner);
    ASR::stmt_t **proc_body = nullptr;
    size_t proc_n_body = 0;
    if (ASR::is_a<ASR::Program_t>(*proc)) {
        proc_body = ASR::down_cast<ASR::Program_t>(proc)->m_body;
        proc_n_body = ASR::down_cast<ASR::Program_t>(proc)->n_body;
    } else if (ASR::is_a<ASR::Function_t>(*proc)) {
        proc_body = ASR::down_cast<ASR::Function_t>(proc)->m_body;
        proc_n_body = ASR::down_cast<ASR::Function_t>(proc)->n_body;
    } else {
        return candidates;
    }
    {
        GpuJumpFinder jumps;
        for (size_t i = 0; i < proc_n_body; i++) {
            jumps.visit_stmt(*proc_body[i]);
        }
        if (jumps.found) return candidates;
    }
    std::vector<GpuStmtPathStep> path;
    if (!find_stmt_path(proc_body, proc_n_body, false, nullptr,
            &region.base, path)) {
        return candidates;
    }

    // The scopes from the loop's own out to the procedure's: a variable
    // declared in one of them is declared outside the loop.
    std::set<SymbolTable*> outer_scopes;
    for (SymbolTable *s = current_scope; s; s = s->parent) {
        outer_scopes.insert(s);
        if (s == proc_scope) break;
    }

    std::set<ASR::symbol_t*> named;
    {
        VarSymbolCollector c(named);
        for (size_t i = 0; i < work.n_body; i++) {
            c.visit_stmt(*work.body[i]);
        }
    }

    // What the procedures contained in this one name, which they reach by
    // host association.
    std::set<ASR::symbol_t*> host_associated;
    {
        VarSymbolCollector c(host_associated);
        std::function<void(SymbolTable*)> scan_contained =
            [&](SymbolTable *st) {
            for (auto &item : st->get_scope()) {
                if (ASR::is_a<ASR::Function_t>(*item.second)) {
                    c.visit_symbol(*item.second);
                } else if (ASR::is_a<ASR::Block_t>(*item.second)) {
                    scan_contained(ASR::down_cast<ASR::Block_t>(
                        item.second)->m_symtab);
                }
            }
        };
        scan_contained(proc_scope);
    }

    PointerTargets pointers;
    {
        PointerCollector c(pointers);
        for (size_t i = 0; i < proc_n_body; i++) c.visit_stmt(*proc_body[i]);
    }
    std::map<ASR::Function_t*, bool> outside;

    for (ASR::symbol_t *sym : named) {
        if (!ASR::is_a<ASR::Variable_t>(*sym)) continue;
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
        if (!outer_scopes.count(v->m_parent_symtab) ||
                v->m_parent_symtab->asr_owner == nullptr ||
                !ASR::is_a<ASR::symbol_t>(*v->m_parent_symtab->asr_owner)) {
            continue;
        }
        ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(
            v->m_parent_symtab->asr_owner);
        if (ASR::is_a<ASR::AssociateBlock_t>(*owner)) continue;
        if (!ASR::is_a<ASR::Array_t>(*v->m_type) ||
                v->m_intent != ASR::intentType::Local ||
                v->m_storage != ASR::storage_typeType::Default ||
                v->m_symbolic_value || v->m_value ||
                v->m_is_volatile || v->m_common_block || v->n_codims > 0 ||
                reachable_elsewhere(sym, proc_scope) ||
                host_associated.count(sym)) {
            continue;
        }
        ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(v->m_type);
        ASR::ttype_t *elem = arr->m_type;
        if (!ASR::is_a<ASR::Integer_t>(*elem) &&
                !ASR::is_a<ASR::Real_t>(*elem) &&
                !ASR::is_a<ASR::Complex_t>(*elem) &&
                !ASR::is_a<ASR::Logical_t>(*elem)) {
            continue;
        }
        GpuArrayAccess access(sym, proc_scope, pointers, outside);
        bool named_outside_body = false;
        for (size_t d = 0; d < work.n_heads(); d++) {
            named_outside_body = named_outside_body
                || access.head(work.head(d));
        }
        for (size_t i = 0; i < region.n_clauses; i++) {
            named_outside_body = named_outside_body
                || access.clause(region.m_clauses[i]);
        }
        for (auto &assoc : enclosing_assoc_map) {
            named_outside_body = named_outside_body
                || assoc.first == sym || access.expr(assoc.second);
        }
        if (named_outside_body) continue;

        GpuArrayFlowReader reader(sym, access, host_associated);
        bool escaped = false;
        if (reader.run(work.body, work.n_body, escaped)
                == GpuArrayFlow::Read) {
            continue;
        }
        if (read_after_region(reader, path)) continue;
        candidates.push_back(sym);
    }
    // The body becomes the statements of a BLOCK (below).
    if (!gpu_associates_inlined(work.body, work.n_body, true)) {
        candidates.clear();
    }
    return candidates;
}

// A variable of LOCAL locality (the OMPPrivate clause of the canonical
// region, from `local(t)` or an OpenMP `private(t)`) is a construct
// entity: each iteration has its own, undefined on entry, and the
// variable outside the loop is not affected by it (Fortran 2018,
// 11.1.7.5). It becomes a local of the BLOCK around the body, with no
// analysis of the body. False, when an array the body names is not one
// the kernel can hold as a local of a thread: its one buffer would be
// shared by every thread.
bool GpuOffloadVisitor::locality_spec_variables(
        const ASR::OMPRegion_t &region, ParallelLoopNest &work,
        const std::map<ASR::symbol_t*, ASR::expr_t*> &enclosing_assoc_map,
        std::vector<ASR::symbol_t*> &local, std::string &rejected) {
    std::set<ASR::symbol_t*> named;
    {
        VarSymbolCollector c(named);
        for (size_t i = 0; i < work.n_body; i++) {
            c.visit_stmt(*work.body[i]);
        }
    }
    std::set<ASR::symbol_t*> indices;
    for (size_t d = 0; d < work.n_heads(); d++) {
        if (work.head(d).m_v && ASR::is_a<ASR::Var_t>(*work.head(d).m_v)) {
            indices.insert(ASR::down_cast<ASR::Var_t>(
                work.head(d).m_v)->m_v);
        }
    }
    std::set<ASR::symbol_t*> in_associates;
    {
        VarSymbolCollector c(in_associates);
        for (auto &assoc : enclosing_assoc_map) {
            in_associates.insert(assoc.first);
            c.visit_expr(*assoc.second);
        }
    }
    bool inlined = gpu_associates_inlined(work.body, work.n_body, true);
    for (size_t i = 0; i < region.n_clauses; i++) {
        if (!ASR::is_a<ASR::OMPPrivate_t>(*region.m_clauses[i])) continue;
        ASR::OMPPrivate_t *clause = ASR::down_cast<ASR::OMPPrivate_t>(
            region.m_clauses[i]);
        for (size_t k = 0; k < clause->n_vars; k++) {
            if (!ASR::is_a<ASR::Var_t>(*clause->m_vars[k])) continue;
            ASR::symbol_t *sym = ASR::down_cast<ASR::Var_t>(
                clause->m_vars[k])->m_v;
            if (!named.count(sym) || indices.count(sym) ||
                    std::find(local.begin(), local.end(), sym)
                        != local.end()) {
                continue;
            }
            bool array = ASR::is_a<ASR::Variable_t>(*sym) &&
                ASRUtils::is_array(ASRUtils::symbol_type(sym));
            ASR::ttype_t *elem = ASR::is_a<ASR::Variable_t>(*sym)
                ? ASRUtils::type_get_past_array(ASRUtils::symbol_type(sym))
                : nullptr;
            bool holdable = elem != nullptr && inlined &&
                !in_associates.count(sym) &&
                ASR::down_cast<ASR::Variable_t>(sym)->n_codims == 0 &&
                (ASR::is_a<ASR::Integer_t>(*elem) ||
                    ASR::is_a<ASR::Real_t>(*elem) ||
                    ASR::is_a<ASR::Complex_t>(*elem) ||
                    ASR::is_a<ASR::Logical_t>(*elem));
            if (!holdable) {
                // A scalar is still a local of each thread when the body
                // assigns it (see visit_OMPRegion); an array is a buffer.
                if (array) {
                    rejected = ASRUtils::symbol_name(sym);
                    return false;
                }
                continue;
            }
            local.push_back(sym);
        }
    }
    return true;
}

// Locality is a correctness decision. Storage is assigned only after
// shared lowering has materialized the kernel's arrays.
bool GpuOffloadVisitor::privatize_iteration_arrays(
        const ASR::OMPRegion_t &region, ParallelLoopNest &work,
        const std::map<ASR::symbol_t*, ASR::expr_t*> &enclosing_assoc_map,
        Vec<ASR::stmt_t*> &private_shapes) {
    std::vector<ASR::symbol_t*> privatized;
    std::string rejected;
    if (!locality_spec_variables(region, work, enclosing_assoc_map,
            privatized, rejected)) {
        report_not_offloaded(region.base.base.loc,
            GpuDecline(GpuDeclineReason::LocalityNotPrivate, rejected));
        return false;
    }
    for (ASR::symbol_t *sym : iteration_array_candidates(region, work,
            enclosing_assoc_map)) {
        if (std::find(privatized.begin(), privatized.end(), sym)
                == privatized.end()) {
            privatized.push_back(sym);
        }
    }
    if (privatized.empty()) return true;
    std::sort(privatized.begin(), privatized.end(),
        [](ASR::symbol_t *a, ASR::symbol_t *b) {
            return std::string(ASRUtils::symbol_name(a))
                < std::string(ASRUtils::symbol_name(b));
        });

    Location loc = region.base.base.loc;
    SymbolTable *block_scope = al.make_new<SymbolTable>(current_scope);
    // The BLOCK and ASSOCIATE constructs of the body move into the new
    // BLOCK with it, so that a name they refer to resolves to the local.
    {
        GpuScopedConstructs constructs(current_scope);
        for (size_t i = 0; i < work.n_body; i++) {
            constructs.visit_stmt(*work.body[i]);
        }
        for (ASR::symbol_t *s : constructs.found) {
            std::string name = ASRUtils::symbol_name(s);
            if (ASR::is_a<ASR::Block_t>(*s)) {
                ASR::down_cast<ASR::Block_t>(s)->m_symtab->parent =
                    block_scope;
            } else {
                ASR::down_cast<ASR::AssociateBlock_t>(s)->m_symtab->parent =
                    block_scope;
            }
            current_scope->erase_symbol(name);
            block_scope->add_symbol(name, s);
        }
    }
    std::map<ASR::symbol_t*, ASR::expr_t*> local_of;
    for (ASR::symbol_t *sym : privatized) {
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
        ASR::ttype_t *type = ASRUtils::duplicate_type(al, v->m_type);
        ASRUtils::ASRBuilder b(al, v->base.base.loc);
        ASR::expr_t *source = ASRUtils::EXPR(
            ASR::make_Var_t(al, v->base.base.loc, sym));
        size_t n_dims = ASR::is_a<ASR::Array_t>(*type)
            ? ASR::down_cast<ASR::Array_t>(type)->n_dims : 0;
        for (size_t d = 0; d < n_dims; d++) {
            auto *array = ASR::down_cast<ASR::Array_t>(type);
            auto capture = [&](ASR::expr_t *&dimension, ASR::expr_t *value) {
                int64_t constant_value;
                if (dimension && ASRUtils::extract_value(
                        ASRUtils::expr_value(dimension), constant_value)) return;
                // Capture the array's existing shape, not the expression
                // used to declare it, whose operands may since have changed.
                ASR::expr_t *extent = b.Variable(current_scope,
                    current_scope->get_unique_name("__gpu_private_shape"),
                    ASRUtils::expr_type(value), ASR::intentType::Local);
                private_shapes.push_back(al, b.Assignment(extent, value));
                dimension = ASRUtils::EXPR(ASR::make_Var_t(al,
                    v->base.base.loc, ASR::down_cast<ASR::Var_t>(extent)->m_v));
            };
            ASR::ttype_t *index_type = ASRUtils::TYPE(
                ASR::make_Integer_t(al, loc, 4));
            capture(array->m_dims[d].m_length,
                b.ArraySize(source, b.i32(d + 1), index_type));
            capture(array->m_dims[d].m_start,
                ASRUtils::EXPR(ASR::make_ArrayBound_t(al, v->base.base.loc,
                    source, b.i32(d + 1), index_type, ASR::arrayboundType::LBound,
                    nullptr)));
        }
        ASR::symbol_t *local = gpu_new_variable(al, v->base.base.loc,
            block_scope, block_scope->get_unique_name(v->m_name),
            type);
        local_of[sym] = ASRUtils::EXPR(ASR::make_Var_t(al,
            v->base.base.loc, local));
    }
    std::string block_name = current_scope->get_unique_name(
        "__gpu_iteration_scope");
    ASR::asr_t *block = ASR::make_Block_t(al, loc, block_scope,
        s2c(al, block_name), work.body, work.n_body);
    block_scope->asr_owner = block;
    ASR::symbol_t *block_sym = ASR::down_cast<ASR::symbol_t>(block);
    // The symbol owns all moved scopes, including bare ASSOCIATE bodies
    // that the statement visitor does not descend into.
    AssociateVarResolverVisitor redirect(al, local_of);
    redirect.visit_symbol(*block_sym);
    current_scope->add_symbol(block_name, block_sym);
    kernel_blocks.push_back(block_sym);
    Vec<ASR::stmt_t*> body;
    body.reserve(al, 1);
    body.push_back(al, ASRUtils::STMT(ASR::make_BlockCall_t(al, loc, -1,
        block_sym)));
    work.set_body(body.p, body.n);
    return true;
}

} // namespace LCompilers
