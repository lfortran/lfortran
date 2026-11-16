#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/gpu_affine.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/pass/gpu_wide_loads.h>
#include <libasr/pass/intrinsic_function_registry.h>

#include <algorithm>
#include <cstdint>
#include <functional>
#include <map>
#include <set>
#include <string>
#include <vector>

namespace LCompilers {

/*
Vector loads of the elements a statement reads side by side.

A thread of a kernel that reads `w(4*c-3, n)`, `w(4*c-2, n)`, `w(4*c-1, n)`
and `w(4*c, n)` -- the four packed words of a quantized weight, the eight
`bf16` values of an activation -- issues four loads of four bytes, and the
warp reads 128 bytes per instruction, each lane 16 bytes away from the next.
The memory system serves the same bytes with a quarter of the instructions
when each lane reads its 16 bytes at once, and a matrix-vector product
that is limited by the instructions of its loads (not by the bandwidth)
runs 1.2 to 1.8 times as fast. The device compiler does not make one load
of four, as it cannot know that the address is a multiple of 16: the array
starts wherever its caller's section starts, and its extents are run-time
values.

So this pass finds, in every statement of device code, the elements of one
array the kernel is given that the statement is certain to read (they are
not in an operand that need not be evaluated, as the second of `.and.` or
a branch of `merge`, nor an argument the callee may assign) at subscripts
that differ in the first one by the constants c, c + 1, ..., c + width - 1,
where `width` elements fill 16 bytes, and are the same otherwise; and
whose first subscript otherwise moves by multiples of `width`. The
statement then reads a new variable for each element, and a GpuWideLoad
before it assigns them: from the 16 bytes at the first element when they
start at a multiple of 16, and from the elements one by one when they do
not. Whether they do is not known when the program is compiled, but it is
the same for the whole launch: it depends on the address of the array and on
its lower bound and extents, so GpuWideLoad has the test (`aligned`), and the
device code tests it once per kernel. All the elements of the vector are
among the ones the statement reads, so a vector never reads past the array.

A kernel that reads such vectors needs no more than a warp per iteration to
keep the memory channels busy: a lane reads 16 bytes, a warp 512 contiguous
bytes (gpu_reads_wide; see gpu_vector_lanes.cpp).
*/

namespace {

// The number of elements of one array that fill a vector, or 0 when the
// elements are of a type that is not read as one.
int vector_width(ASR::ttype_t *t) {
    t = ASRUtils::type_get_past_allocatable_pointer(t);
    if (ASRUtils::is_array(t)) return 0;
    int kind = ASRUtils::extract_kind_from_ttype_t(t);
    if (!(ASRUtils::is_integer(*t) || ASRUtils::is_real(*t))) return 0;
    if (kind != 2 && kind != 4 && kind != 8) return 0;
    return 16 / kind;
}

struct Candidate {
    ASR::expr_t **slot;
    ASR::ArrayItem_t *item;
    int64_t offset;
    // Whether `offset` counts from the first lower bound of the array (the
    // subscript adds it: `a(lbound(a, 1) + e)`), not from 0.
    bool from_lower = false;
};

// The elements the expression reads for certain, as the places that hold
// them.
class Collector : public ASR::BaseExprReplacer<Collector> {
public:
    std::map<std::string, std::vector<Candidate>> groups;
    // The array whose elements are read.
    std::map<std::string, ASR::symbol_t*> arrays;
    int conditional = 0;

    void replace_LogicalBinOp(ASR::LogicalBinOp_t *x) {
        ASR::expr_t **cur = current_expr;
        current_expr = &x->m_left;
        replace_expr(x->m_left);
        conditional++;
        current_expr = &x->m_right;
        replace_expr(x->m_right);
        conditional--;
        current_expr = cur;
    }

    void replace_IfExp(ASR::IfExp_t *x) {
        ASR::expr_t **cur = current_expr;
        current_expr = &x->m_test;
        replace_expr(x->m_test);
        conditional++;
        current_expr = &x->m_body;
        replace_expr(x->m_body);
        current_expr = &x->m_orelse;
        replace_expr(x->m_orelse);
        conditional--;
        current_expr = cur;
    }

    void replace_IntrinsicElementalFunction(
            ASR::IntrinsicElementalFunction_t *x) {
        bool is_merge = x->m_intrinsic_id == static_cast<int64_t>(
            ASRUtils::IntrinsicElementalFunctions::Merge);
        if (is_merge) conditional++;
        ASR::BaseExprReplacer<Collector>::replace_IntrinsicElementalFunction(x);
        if (is_merge) conditional--;
    }

    // An argument is read where the callee takes it by value only.
    void replace_FunctionCall(ASR::FunctionCall_t *x) {
        ASR::symbol_t *callee = ASRUtils::symbol_get_past_external(x->m_name);
        ASR::Function_t *fn = ASR::is_a<ASR::Function_t>(*callee)
            ? ASR::down_cast<ASR::Function_t>(callee) : nullptr;
        ASR::expr_t **cur = current_expr;
        for (size_t i = 0; i < x->n_args; i++) {
            if (x->m_args[i].m_value == nullptr) continue;
            bool by_value = false;
            if (fn && i < fn->n_args && ASR::is_a<ASR::Var_t>(*fn->m_args[i])) {
                ASR::symbol_t *p = ASR::down_cast<ASR::Var_t>(
                    fn->m_args[i])->m_v;
                by_value = ASR::is_a<ASR::Variable_t>(*p)
                    && ASR::down_cast<ASR::Variable_t>(p)->m_intent
                        == ASR::intentType::In;
            }
            if (!by_value) conditional++;
            current_expr = &x->m_args[i].m_value;
            replace_expr(x->m_args[i].m_value);
            if (!by_value) conditional--;
        }
        current_expr = cur;
    }

    void replace_ArrayItem(ASR::ArrayItem_t *x) {
        ASR::expr_t **slot = current_expr;
        if (conditional == 0) consider(slot, x);
        ASR::BaseExprReplacer<Collector>::replace_ArrayItem(x);
    }

private:
    void consider(ASR::expr_t **slot, ASR::ArrayItem_t *x) {
        if (!ASR::is_a<ASR::Var_t>(*x->m_v) || x->n_args == 0) return;
        ASR::symbol_t *array = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(x->m_v)->m_v);
        // An array the kernel is given: it starts where the host put it.
        if (!ASR::is_a<ASR::Variable_t>(*array)
                || ASR::down_cast<ASR::Variable_t>(array)->m_intent
                    == ASR::intentType::Local) {
            return;
        }
        if (vector_width(x->m_type) == 0) return;
        for (size_t i = 0; i < x->n_args; i++) {
            if (x->m_args[i].m_right == nullptr || x->m_args[i].m_left
                    || x->m_args[i].m_step) {
                return;
            }
        }
        GpuAffineBuilder builder;
        builder.invariant_variable = [](ASR::symbol_t *) { return true; };
        GpuAffine first;
        if (!builder.affine(x->m_args[0].m_right, first)) return;
        // A subscript that adds the array's own first lower bound, as the
        // element of an array dummy inlined in a kernel does (GpuInliner:
        // `a(i + (lbound(a, 1) - 1))`), reads the element that many places
        // from the start of the array, whatever the lower bound is.
        // The lower bound is `lbound(a, 1)`, or, once the bounds of the
        // kernel's arrays are its parameters, what the type of `a` declares.
        ASR::dimension_t *dims = nullptr;
        size_t n_dims = ASRUtils::extract_dimensions_from_ttype(
            ASRUtils::symbol_type(array), dims);
        std::string declared_lower = n_dims > 0 && dims[0].m_start
            ? gpu_affine_key(dims[0].m_start) : std::string();
        auto first_lower = [&](const std::string &key, ASR::expr_t *e) {
            if (!declared_lower.empty() && key == declared_lower) return true;
            if (!ASR::is_a<ASR::ArrayBound_t>(*e)) return false;
            ASR::ArrayBound_t *b = ASR::down_cast<ASR::ArrayBound_t>(e);
            int64_t dim = 0;
            return b->m_bound == ASR::arrayboundType::LBound
                && ASR::is_a<ASR::Var_t>(*b->m_v)
                && ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(b->m_v)->m_v) == array
                && b->m_dim && ASRUtils::extract_value(
                    ASRUtils::expr_value(b->m_dim), dim) && dim == 1;
        };
        bool from_lower = false;
        for (auto it = first.terms.begin(); it != first.terms.end(); ++it) {
            auto atom = builder.atoms.find(it->first);
            if (it->second == 1 && atom != builder.atoms.end()
                    && first_lower(it->first, atom->second)) {
                first.terms.erase(it);
                from_lower = true;
                break;
            }
        }
        // The first subscript moves by whole vectors.
        int width = vector_width(x->m_type);
        for (auto &term : first.terms) {
            if (term.second % width != 0) return;
        }
        std::string key = std::to_string(reinterpret_cast<uintptr_t>(array))
            + (from_lower ? "@lower" : "");
        for (auto &term : first.terms) {
            key += "|" + term.first + "*" + std::to_string(term.second);
        }
        for (size_t i = 1; i < x->n_args; i++) {
            std::string k = gpu_affine_key(x->m_args[i].m_right);
            if (k.empty()) return;
            key += "#" + k;
        }
        groups[key].push_back(Candidate{slot, x, first.constant, from_lower});
        arrays[key] = array;
    }
};

// The condition under which the elements that start at the subscript
// `start` of the first dimension of `array` and are `width` apart start at a
// multiple of 16 bytes, when the array itself does: the first lower bound
// and the extents of the dimensions before the last, which the elements
// of the other subscripts are that many apart in, are multiples of it.
// Where `from_lower`, `start` counts from the first lower bound, which
// then does not matter. False when they never are; `condition` is null
// when they always are.
bool alignment_condition(Allocator &al, const Location &loc,
        ASR::expr_t *array, size_t n_subscripts, int64_t start, int width,
        bool from_lower, ASR::expr_t *&condition) {
    ASRUtils::ASRBuilder b(al, loc);
    ASR::ttype_t *int4 = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
    ASR::dimension_t *dims = nullptr;
    size_t n_dims = ASRUtils::extract_dimensions_from_ttype(
        ASRUtils::expr_type(array), dims);
    condition = nullptr;
    auto require = [&](ASR::expr_t *test) {
        condition = condition ? b.And(condition, test) : test;
    };
    ASR::expr_t *lower = b.ArrayLBound(array, 1);
    int64_t l = 0;
    if (from_lower) {
        if ((start % width + width) % width != 0) return false;
    } else if (ASRUtils::extract_value(ASRUtils::expr_value(lower), l)) {
        if (((start - l) % width + width) % width != 0) return false;
    } else {
        require(b.Eq(b.And(b.Sub(b.i32(start), lower), b.i32(width - 1)),
            b.i32(0)));
    }
    int64_t constant_stride = 1;
    ASR::expr_t *stride = nullptr;
    for (size_t i = 1; i < n_subscripts && i <= n_dims; i++) {
        int64_t length = 0;
        ASR::expr_t *size = nullptr;
        if (dims[i - 1].m_length && ASRUtils::extract_value(
                ASRUtils::expr_value(dims[i - 1].m_length), length)) {
            constant_stride *= length;
        } else {
            size = b.ArraySize(array, b.i32(static_cast<int>(i)), int4);
        }
        if (size) stride = stride ? b.Mul(stride, size) : size;
        if (stride == nullptr) {
            if (constant_stride % width != 0) return false;
        } else {
            ASR::expr_t *full = constant_stride == 1 ? stride
                : b.Mul(b.i32(static_cast<int>(constant_stride)), stride);
            require(b.Eq(b.And(full, b.i32(width - 1)), b.i32(0)));
        }
    }
    return true;
}

class WideLoads {
public:
    explicit WideLoads(Allocator &al_) : al(al_) {}

    void body(SymbolTable *scope, ASR::stmt_t **&stmts, size_t &n) {
        Vec<ASR::stmt_t*> out;
        out.reserve(al, n);
        bool changed = false;
        for (size_t i = 0; i < n; i++) {
            Vec<ASR::stmt_t*> pre;
            pre.reserve(al, 1);
            stmt(scope, stmts[i], pre);
            for (size_t j = 0; j < pre.n; j++) out.push_back(al, pre[j]);
            if (pre.n > 0) changed = true;
            out.push_back(al, stmts[i]);
        }
        if (changed) {
            stmts = out.p;
            n = out.n;
        }
    }

private:
    Allocator &al;

    // The variables and arrays a statement assigns, and whether it does
    // anything else that could (a call of a subroutine).
    struct Writes : public ASR::BaseWalkVisitor<Writes> {
        std::set<ASR::symbol_t*> symbols;
        bool unknown = false;
        void target(ASR::expr_t *e) {
            while (e) {
                if (ASR::is_a<ASR::Var_t>(*e)) {
                    symbols.insert(ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::Var_t>(e)->m_v));
                    return;
                } else if (ASR::is_a<ASR::ArrayItem_t>(*e)) {
                    e = ASR::down_cast<ASR::ArrayItem_t>(e)->m_v;
                } else {
                    unknown = true;
                    return;
                }
            }
        }
        void visit_Assignment(const ASR::Assignment_t &x) {
            target(x.m_target);
        }
        void visit_GpuWideLoad(const ASR::GpuWideLoad_t &x) {
            for (size_t i = 0; i < x.n_targets; i++) target(x.m_targets[i]);
        }
        void visit_DoLoop(const ASR::DoLoop_t &x) {
            if (x.m_head.m_v) target(x.m_head.m_v);
            BaseWalkVisitor::visit_DoLoop(x);
        }
        void visit_SubroutineCall(const ASR::SubroutineCall_t &) {
            unknown = true;
        }
        void visit_BlockCall(const ASR::BlockCall_t &x) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
            if (!ASR::is_a<ASR::Block_t>(*b)) {
                unknown = true;
                return;
            }
            ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
            for (size_t i = 0; i < blk->n_body; i++) visit_stmt(*blk->m_body[i]);
        }
    };

    struct Reads : public ASR::BaseWalkVisitor<Reads> {
        std::set<ASR::symbol_t*> symbols;
        void visit_Var(const ASR::Var_t &x) {
            symbols.insert(ASRUtils::symbol_get_past_external(x.m_v));
        }
    };

    // The slots of the elements of arrays that `e` reads for certain: not
    // under a branch (merge, a short-circuit operator) or in an argument
    // of a call to anything but a function of the device.
    static void certain_items(ASR::expr_t **e,
            std::vector<ASR::expr_t**> &items) {
        switch ((*e)->type) {
            case ASR::exprType::ArrayItem: {
                ASR::ArrayItem_t *a = ASR::down_cast<ASR::ArrayItem_t>(*e);
                if (ASR::is_a<ASR::Var_t>(*a->m_v)) items.push_back(e);
                return;
            }
            case ASR::exprType::RealBinOp: {
                ASR::RealBinOp_t *x = ASR::down_cast<ASR::RealBinOp_t>(*e);
                certain_items(&x->m_left, items);
                certain_items(&x->m_right, items);
                return;
            }
            case ASR::exprType::Cast: {
                certain_items(&ASR::down_cast<ASR::Cast_t>(*e)->m_arg, items);
                return;
            }
            case ASR::exprType::FunctionCall: {
                ASR::FunctionCall_t *f = ASR::down_cast<ASR::FunctionCall_t>(*e);
                ASR::symbol_t *fn = ASRUtils::symbol_get_past_external(f->m_name);
                if (!ASR::is_a<ASR::Function_t>(*fn)
                        || !ASRUtils::is_device_only_function(
                            *ASR::down_cast<ASR::Function_t>(fn))) {
                    return;
                }
                // Only an argument the callee takes by value: one it may
                // assign has to stay the element itself.
                ASR::Function_t *callee = ASR::down_cast<ASR::Function_t>(fn);
                for (size_t i = 0; i < f->n_args && i < callee->n_args; i++) {
                    if (f->m_args[i].m_value == nullptr
                            || !ASR::is_a<ASR::Var_t>(*callee->m_args[i])) {
                        continue;
                    }
                    ASR::symbol_t *p = ASR::down_cast<ASR::Var_t>(
                        callee->m_args[i])->m_v;
                    if (ASR::is_a<ASR::Variable_t>(*p)
                            && ASR::down_cast<ASR::Variable_t>(p)->m_intent
                                == ASR::intentType::In) {
                        certain_items(&f->m_args[i].m_value, items);
                    }
                }
                return;
            }
            default:
                return;
        }
    }

    // Whether the statements may leave the body before its end (an exit, a
    // cycle, a return, a stop, a jump): what a statement after such a
    // statement reads is then not read for certain in every iteration.
    static bool leaves(ASR::stmt_t **stmts, size_t n) {
        struct Leaves : public ASR::BaseWalkVisitor<Leaves> {
            bool found = false;
            void visit_Exit(const ASR::Exit_t &) { found = true; }
            void visit_Cycle(const ASR::Cycle_t &) { found = true; }
            void visit_Return(const ASR::Return_t &) { found = true; }
            void visit_Stop(const ASR::Stop_t &) { found = true; }
            void visit_ErrorStop(const ASR::ErrorStop_t &) { found = true; }
            void visit_GoTo(const ASR::GoTo_t &) { found = true; }
            void visit_BlockCall(const ASR::BlockCall_t &x) {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
                if (!ASR::is_a<ASR::Block_t>(*b)) return;
                ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                for (size_t i = 0; i < blk->n_body; i++) {
                    visit_stmt(*blk->m_body[i]);
                }
            }
        } l;
        for (size_t i = 0; i < n && !l.found; i++) l.visit_stmt(*stmts[i]);
        return l.found;
    }

    // The vector loads of a loop's body go to its start, ahead of the
    // arithmetic of the statements before theirs, so that all of an
    // iteration's loads are in flight at once (nvcc keeps them where they
    // are under a tight register bound: the last rows of a quantized
    // matrix-vector product of 4 rows were read halfway through the
    // iteration, 35 us instead of 32). A load moves only when nothing in
    // the body writes its array and nothing before it assigns a variable
    // its subscripts use, and nothing in the body leaves it early.
    void hoist(SymbolTable *scope, ASR::stmt_t **&stmts, size_t &n) {
        if (leaves(stmts, n)) return;
        Writes all;
        for (size_t i = 0; i < n; i++) all.visit_stmt(*stmts[i]);
        if (all.unknown) return;
        // The other elements a statement of the body reads for certain (the
        // scales of the rows of a quantized product) are read at its start
        // too, into new variables, once the body reads vectors.
        Vec<ASR::stmt_t*> scalars;
        scalars.reserve(al, 1);
        bool vectors = false;
        for (size_t i = 0; i < n; i++) {
            if (ASR::is_a<ASR::GpuWideLoad_t>(*stmts[i])) vectors = true;
        }
        if (vectors) {
            Writes before;
            for (size_t i = 0; i < n; i++) {
                if (ASR::is_a<ASR::Assignment_t>(*stmts[i])) {
                    ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(
                        stmts[i]);
                    std::vector<ASR::expr_t**> items;
                    if (!a->m_overloaded) certain_items(&a->m_value, items);
                    for (ASR::expr_t **slot : items) {
                        ASR::ArrayItem_t *item =
                            ASR::down_cast<ASR::ArrayItem_t>(*slot);
                        if (ASRUtils::is_array(item->m_type)) continue;
                        Reads r;
                        r.visit_expr(**slot);
                        bool ok = true;
                        ASR::symbol_t *array = ASRUtils::symbol_get_past_external(
                            ASR::down_cast<ASR::Var_t>(item->m_v)->m_v);
                        for (ASR::symbol_t *v : r.symbols) {
                            if (before.symbols.count(v)
                                    || (v == array && all.symbols.count(v))) {
                                ok = false;
                            }
                        }
                        if (!ok) continue;
                        Location loc = item->base.base.loc;
                        ASRUtils::ASRBuilder b(al, loc);
                        ASR::expr_t *t = b.Variable(scope,
                            scope->get_unique_name("__gpu_element", false),
                            ASRUtils::duplicate_type(al, item->m_type),
                            ASR::intentType::Local);
                        scalars.push_back(al, b.Assignment(t, *slot));
                        *slot = t;
                    }
                }
                before.visit_stmt(*stmts[i]);
            }
        }
        Vec<ASR::stmt_t*> front, rest;
        front.reserve(al, n);
        rest.reserve(al, n);
        Writes before;
        bool moved = false;
        for (size_t i = 0; i < n; i++) {
            ASR::stmt_t *s = stmts[i];
            if (ASR::is_a<ASR::GpuWideLoad_t>(*s)) {
                ASR::GpuWideLoad_t *w = ASR::down_cast<ASR::GpuWideLoad_t>(s);
                bool ok = ASR::is_a<ASR::Var_t>(*w->m_array);
                ASR::symbol_t *array = ok
                    ? ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::Var_t>(w->m_array)->m_v)
                    : nullptr;
                Reads r;
                r.visit_expr(*w->m_source);
                if (w->m_aligned) r.visit_expr(*w->m_aligned);
                for (ASR::symbol_t *v : r.symbols) {
                    if (before.symbols.count(v)
                            || (v == array && all.symbols.count(v))) {
                        ok = false;
                    }
                }
                if (ok) {
                    if (rest.n > 0) moved = true;
                    front.push_back(al, s);
                    continue;
                }
            }
            before.visit_stmt(*s);
            rest.push_back(al, s);
        }
        if (!moved && scalars.n == 0) return;
        for (size_t i = 0; i < scalars.n; i++) front.push_back(al, scalars[i]);
        for (size_t i = 0; i < rest.n; i++) front.push_back(al, rest[i]);
        stmts = front.p;
        n = front.n;
    }

    // The reads of `e` that sit side by side become reads of new variables,
    // which a statement appended to `pre` assigns.
    void rewrite(SymbolTable *scope, ASR::expr_t *&e, Vec<ASR::stmt_t*> &pre) {
        if (e == nullptr) return;
        Collector c;
        c.current_expr = &e;
        c.replace_expr(e);
        for (auto &entry : c.groups) {
            std::vector<Candidate> &loads = entry.second;
            int width = vector_width(loads[0].item->m_type);
            std::map<int64_t, std::vector<Candidate*>> by_offset;
            for (Candidate &l : loads) by_offset[l.offset].push_back(&l);
            std::set<int64_t> used;
            for (auto &first : by_offset) {
                int64_t o = first.first;
                bool run = true;
                for (int j = 0; j < width; j++) {
                    if (!by_offset.count(o + j) || used.count(o + j)) {
                        run = false;
                    }
                }
                if (!run) continue;
                ASR::ArrayItem_t *start = by_offset[o][0]->item;
                Location loc = start->base.base.loc;
                ASR::expr_t *aligned = nullptr;
                if (!alignment_condition(al, loc, start->m_v,
                        start->n_args, o, width, by_offset[o][0]->from_lower,
                        aligned)) {
                    continue;
                }
                Vec<ASR::expr_t*> targets;
                targets.reserve(al, width);
                for (int j = 0; j < width; j++) {
                    used.insert(o + j);
                    ASR::ArrayItem_t *item = by_offset[o + j][0]->item;
                    ASRUtils::ASRBuilder b(al, loc);
                    ASR::expr_t *v = b.Variable(scope,
                        scope->get_unique_name("__gpu_wide", false),
                        ASRUtils::duplicate_type(al, item->m_type),
                        ASR::intentType::Local);
                    targets.push_back(al, v);
                    for (Candidate *l : by_offset[o + j]) {
                        *l->slot = v;
                    }
                }
                pre.push_back(al, ASRUtils::STMT(ASR::make_GpuWideLoad_t(al,
                    loc, targets.p, targets.n,
                    ASRUtils::EXPR(&start->base.base), start->m_v, aligned)));
            }
        }
    }

    void stmt(SymbolTable *scope, ASR::stmt_t *s, Vec<ASR::stmt_t*> &pre) {
        switch (s->type) {
            case ASR::stmtType::Assignment: {
                ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(s);
                if (a->m_overloaded) return;
                rewrite(scope, a->m_value, pre);
                return;
            }
            case ASR::stmtType::If: {
                ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                rewrite(scope, x->m_test, pre);
                body(scope, x->m_body, x->n_body);
                body(scope, x->m_orelse, x->n_orelse);
                return;
            }
            case ASR::stmtType::DoLoop: {
                ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(s);
                body(scope, x->m_body, x->n_body);
                hoist(scope, x->m_body, x->n_body);
                body(scope, x->m_orelse, x->n_orelse);
                return;
            }
            case ASR::stmtType::DoConcurrentLoop: {
                ASR::DoConcurrentLoop_t *x =
                    ASR::down_cast<ASR::DoConcurrentLoop_t>(s);
                body(scope, x->m_body, x->n_body);
                return;
            }
            case ASR::stmtType::WhileLoop: {
                ASR::WhileLoop_t *x = ASR::down_cast<ASR::WhileLoop_t>(s);
                body(scope, x->m_body, x->n_body);
                body(scope, x->m_orelse, x->n_orelse);
                return;
            }
            case ASR::stmtType::Select: {
                ASR::Select_t *x = ASR::down_cast<ASR::Select_t>(s);
                for (size_t i = 0; i < x->n_body; i++) {
                    ASR::case_stmt_t *c = x->m_body[i];
                    if (ASR::is_a<ASR::CaseStmt_t>(*c)) {
                        ASR::CaseStmt_t *cs =
                            ASR::down_cast<ASR::CaseStmt_t>(c);
                        body(scope, cs->m_body, cs->n_body);
                    } else {
                        ASR::CaseStmt_Range_t *cs =
                            ASR::down_cast<ASR::CaseStmt_Range_t>(c);
                        body(scope, cs->m_body, cs->n_body);
                    }
                }
                body(scope, x->m_default, x->n_default);
                return;
            }
            case ASR::stmtType::BlockCall: {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                if (!ASR::is_a<ASR::Block_t>(*b)) return;
                ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                body(blk->m_symtab, blk->m_body, blk->n_body);
                return;
            }
            default:
                return;
        }
    }
};

// Whether some statement reads a run of elements side by side, as the pass
// would: a count, without changing anything.
class Scan {
public:
    bool found = false;
    std::set<ASR::Function_t*> seen;

    void body(ASR::stmt_t **stmts, size_t n) {
        for (size_t i = 0; i < n && !found; i++) stmt(stmts[i]);
    }

private:
    void expr(ASR::expr_t *e) {
        if (e == nullptr || found) return;
        Collector c;
        c.current_expr = &e;
        c.replace_expr(e);
        for (auto &entry : c.groups) {
            std::set<int64_t> offsets;
            for (Candidate &l : entry.second) offsets.insert(l.offset);
            int width = vector_width(entry.second[0].item->m_type);
            for (int64_t o : offsets) {
                bool run = true;
                for (int j = 0; j < width; j++) {
                    if (!offsets.count(o + j)) run = false;
                }
                if (run) found = true;
            }
        }
        // The functions a statement calls read what they are given.
        struct Calls : public ASR::BaseWalkVisitor<Calls> {
            std::vector<ASR::Function_t*> callees;
            void visit_FunctionCall(const ASR::FunctionCall_t &x) {
                ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_name);
                if (ASR::is_a<ASR::Function_t>(*s)) {
                    callees.push_back(ASR::down_cast<ASR::Function_t>(s));
                }
                BaseWalkVisitor::visit_FunctionCall(x);
            }
        } calls;
        calls.visit_expr(*e);
        for (ASR::Function_t *f : calls.callees) {
            if (seen.insert(f).second && ASRUtils::is_device_only_function(*f)) {
                body(f->m_body, f->n_body);
            }
        }
    }

    void stmt(ASR::stmt_t *s) {
        switch (s->type) {
            case ASR::stmtType::Assignment:
                expr(ASR::down_cast<ASR::Assignment_t>(s)->m_value);
                return;
            case ASR::stmtType::If: {
                ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                expr(x->m_test);
                body(x->m_body, x->n_body);
                body(x->m_orelse, x->n_orelse);
                return;
            }
            case ASR::stmtType::DoLoop: {
                ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(s);
                body(x->m_body, x->n_body);
                return;
            }
            case ASR::stmtType::DoConcurrentLoop:
                body(ASR::down_cast<ASR::DoConcurrentLoop_t>(s)->m_body,
                    ASR::down_cast<ASR::DoConcurrentLoop_t>(s)->n_body);
                return;
            case ASR::stmtType::WhileLoop:
                body(ASR::down_cast<ASR::WhileLoop_t>(s)->m_body,
                    ASR::down_cast<ASR::WhileLoop_t>(s)->n_body);
                return;
            case ASR::stmtType::BlockCall: {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                if (!ASR::is_a<ASR::Block_t>(*b)) return;
                ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                body(blk->m_body, blk->n_body);
                return;
            }
            default:
                return;
        }
    }
};

// The vectors of 16 bytes that an iteration of a loop of device code
// reads (its GpuWideLoad statements, and those of the device functions it
// calls, one set per call), counted once per element they start at: the
// statements of a matrix-vector product that takes several rows of a matrix
// per iteration each read the same vectors of `x`.
class Vectors {
public:
    // The most an iteration of one loop reads.
    int64_t most = 0;
    std::set<const ASR::Function_t*> active;

    // What the statements read outside the loops they hold, whose
    // iterations are counted by themselves.
    int64_t body(ASR::stmt_t **stmts, size_t n) {
        std::set<std::string> keys;
        int64_t other = 0;
        for (size_t i = 0; i < n; i++) stmt(stmts[i], keys, other);
        return static_cast<int64_t>(keys.size()) + other;
    }

private:
    void loop(ASR::stmt_t **stmts, size_t n) {
        most = std::max(most, body(stmts, n));
    }

    void calls(ASR::expr_t *e, int64_t &other) {
        if (e == nullptr) return;
        struct Calls : public ASR::BaseWalkVisitor<Calls> {
            std::vector<const ASR::Function_t*> callees;
            void visit_FunctionCall(const ASR::FunctionCall_t &x) {
                ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_name);
                if (ASR::is_a<ASR::Function_t>(*s)) {
                    callees.push_back(ASR::down_cast<ASR::Function_t>(s));
                }
                BaseWalkVisitor::visit_FunctionCall(x);
            }
        } c;
        c.visit_expr(*e);
        for (const ASR::Function_t *f : c.callees) {
            if (!ASRUtils::is_device_only_function(*f)
                    || !active.insert(f).second) {
                continue;
            }
            other += body(f->m_body, f->n_body);
            active.erase(f);
        }
    }

    void stmt(ASR::stmt_t *s, std::set<std::string> &keys, int64_t &other) {
        switch (s->type) {
            case ASR::stmtType::GpuWideLoad: {
                ASR::GpuWideLoad_t *w = ASR::down_cast<ASR::GpuWideLoad_t>(s);
                std::string key;
                if (ASR::is_a<ASR::ArrayItem_t>(*w->m_source)) {
                    ASR::ArrayItem_t *item = ASR::down_cast<ASR::ArrayItem_t>(
                        w->m_source);
                    if (ASR::is_a<ASR::Var_t>(*item->m_v)) {
                        key = std::to_string(reinterpret_cast<uintptr_t>(
                            ASRUtils::symbol_get_past_external(
                                ASR::down_cast<ASR::Var_t>(
                                    item->m_v)->m_v)));
                        for (size_t i = 0; i < item->n_args && !key.empty();
                                i++) {
                            std::string k = item->m_args[i].m_right
                                ? gpu_affine_key(item->m_args[i].m_right)
                                : std::string();
                            key = k.empty() ? std::string() : key + "#" + k;
                        }
                    }
                }
                if (key.empty()) {
                    other++;
                } else {
                    keys.insert(key);
                }
                return;
            }
            case ASR::stmtType::Assignment:
                calls(ASR::down_cast<ASR::Assignment_t>(s)->m_value, other);
                return;
            case ASR::stmtType::If: {
                ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                calls(x->m_test, other);
                for (size_t i = 0; i < x->n_body; i++) {
                    stmt(x->m_body[i], keys, other);
                }
                for (size_t i = 0; i < x->n_orelse; i++) {
                    stmt(x->m_orelse[i], keys, other);
                }
                return;
            }
            case ASR::stmtType::BlockCall: {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                if (!ASR::is_a<ASR::Block_t>(*b)) return;
                ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                for (size_t i = 0; i < blk->n_body; i++) {
                    stmt(blk->m_body[i], keys, other);
                }
                return;
            }
            case ASR::stmtType::DoLoop: {
                ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(s);
                loop(x->m_body, x->n_body);
                return;
            }
            case ASR::stmtType::DoConcurrentLoop: {
                ASR::DoConcurrentLoop_t *x =
                    ASR::down_cast<ASR::DoConcurrentLoop_t>(s);
                loop(x->m_body, x->n_body);
                return;
            }
            case ASR::stmtType::WhileLoop: {
                ASR::WhileLoop_t *x = ASR::down_cast<ASR::WhileLoop_t>(s);
                loop(x->m_body, x->n_body);
                return;
            }
            default:
                return;
        }
    }
};

} // namespace

// The rows of one array that the statements of a loop read runs of elements
// side by side in (as Scan finds them): the runs that differ in a subscript
// after the first.
class Runs {
public:
    int64_t most = 0;
    std::set<const ASR::Function_t*> active;

    void body(ASR::stmt_t **stmts, size_t n, std::set<std::string> &keys) {
        for (size_t i = 0; i < n; i++) stmt(stmts[i], keys);
    }

private:
    void loop(ASR::stmt_t **stmts, size_t n) {
        std::set<std::string> keys;
        body(stmts, n, keys);
        most = std::max(most, rows(keys));
    }

public:
    // `keys` are "array|later subscripts": the most rows of one array.
    static int64_t rows(const std::set<std::string> &keys) {
        std::map<std::string, int64_t> per_array;
        int64_t m = 0;
        for (const std::string &k : keys) {
            m = std::max(m, ++per_array[k.substr(0, k.find('|'))]);
        }
        return m;
    }

private:

    void expr(ASR::expr_t *e, std::set<std::string> &keys) {
        if (e == nullptr) return;
        Collector c;
        c.current_expr = &e;
        c.replace_expr(e);
        for (auto &entry : c.groups) {
            std::map<int64_t, Candidate*> by_offset;
            for (Candidate &l : entry.second) by_offset[l.offset] = &l;
            int width = vector_width(entry.second[0].item->m_type);
            for (auto &first : by_offset) {
                bool run = true;
                for (int j = 0; j < width; j++) {
                    if (!by_offset.count(first.first + j)) run = false;
                }
                if (!run) continue;
                ASR::ArrayItem_t *item = first.second->item;
                if (!ASR::is_a<ASR::Var_t>(*item->m_v)) continue;
                std::string key = std::to_string(reinterpret_cast<uintptr_t>(
                    ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::Var_t>(item->m_v)->m_v))) + "|";
                for (size_t i = 1; i < item->n_args; i++) {
                    key += "#" + (item->m_args[i].m_right
                        ? gpu_affine_key(item->m_args[i].m_right)
                        : std::string());
                }
                keys.insert(key);
            }
        }
        struct Calls : public ASR::BaseWalkVisitor<Calls> {
            std::vector<const ASR::Function_t*> callees;
            void visit_FunctionCall(const ASR::FunctionCall_t &x) {
                ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_name);
                if (ASR::is_a<ASR::Function_t>(*s)) {
                    callees.push_back(ASR::down_cast<ASR::Function_t>(s));
                }
                BaseWalkVisitor::visit_FunctionCall(x);
            }
        } calls;
        calls.visit_expr(*e);
        for (const ASR::Function_t *f : calls.callees) {
            if (!ASRUtils::is_device_only_function(*f)
                    || !active.insert(f).second) {
                continue;
            }
            body(f->m_body, f->n_body, keys);
            active.erase(f);
        }
    }

    void stmt(ASR::stmt_t *s, std::set<std::string> &keys) {
        switch (s->type) {
            case ASR::stmtType::Assignment:
                expr(ASR::down_cast<ASR::Assignment_t>(s)->m_value, keys);
                return;
            case ASR::stmtType::If: {
                ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                expr(x->m_test, keys);
                body(x->m_body, x->n_body, keys);
                body(x->m_orelse, x->n_orelse, keys);
                return;
            }
            case ASR::stmtType::BlockCall: {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                if (!ASR::is_a<ASR::Block_t>(*b)) return;
                ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                body(blk->m_body, blk->n_body, keys);
                return;
            }
            case ASR::stmtType::DoLoop: {
                ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(s);
                loop(x->m_body, x->n_body);
                return;
            }
            case ASR::stmtType::DoConcurrentLoop: {
                ASR::DoConcurrentLoop_t *x =
                    ASR::down_cast<ASR::DoConcurrentLoop_t>(s);
                body(x->m_body, x->n_body, keys);
                return;
            }
            case ASR::stmtType::WhileLoop: {
                ASR::WhileLoop_t *x = ASR::down_cast<ASR::WhileLoop_t>(s);
                loop(x->m_body, x->n_body);
                return;
            }
            default:
                return;
        }
    }
};

int64_t gpu_wide_rows_per_iteration(const ASR::Function_t &kernel) {
    Runs r;
    std::set<std::string> keys;
    r.body(kernel.m_body, kernel.n_body, keys);
    return std::max(r.most, Runs::rows(keys));
}

int64_t gpu_wide_vectors_per_iteration(const ASR::Function_t &kernel) {
    Vectors v;
    int64_t outside = v.body(kernel.m_body, kernel.n_body);
    return std::max(outside, v.most);
}

bool gpu_reads_wide(const ASR::Function_t &kernel,
        const GpuDeviceCapabilities &caps) {
    if (!caps.device_selected() || !caps.wide_loads) return false;
    Scan scan;
    scan.body(kernel.m_body, kernel.n_body);
    return scan.found;
}

void pass_gpu_wide_loads(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options) {
    GpuDeviceCapabilities caps = gpu_device_capabilities(pass_options);
    if (!caps.device_selected() || !caps.wide_loads) return;
    WideLoads loads(al);
    std::function<void(SymbolTable*)> visit = [&](SymbolTable *scope) {
        for (auto &entry : scope->get_scope()) {
            ASR::symbol_t *s = entry.second;
            if (ASR::is_a<ASR::Function_t>(*s)) {
                ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(s);
                if (ASRUtils::is_device_only_function(*fn)) {
                    loads.body(fn->m_symtab, fn->m_body, fn->n_body);
                }
                visit(fn->m_symtab);
            } else if (ASR::is_a<ASR::Module_t>(*s)) {
                visit(ASR::down_cast<ASR::Module_t>(s)->m_symtab);
            } else if (ASR::is_a<ASR::Program_t>(*s)) {
                visit(ASR::down_cast<ASR::Program_t>(s)->m_symtab);
            }
        }
    };
    visit(unit.m_symtab);
}

} // namespace LCompilers
