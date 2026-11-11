#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/gpu_affine.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/pass/gpu_wide_loads.h>
#include <libasr/pass/intrinsic_function_registry.h>

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
        // The first subscript moves by whole vectors.
        int width = vector_width(x->m_type);
        for (auto &term : first.terms) {
            if (term.second % width != 0) return;
        }
        std::string key = std::to_string(reinterpret_cast<uintptr_t>(array));
        for (auto &term : first.terms) {
            key += "|" + term.first + "*" + std::to_string(term.second);
        }
        for (size_t i = 1; i < x->n_args; i++) {
            std::string k = gpu_affine_key(x->m_args[i].m_right);
            if (k.empty()) return;
            key += "#" + k;
        }
        groups[key].push_back(Candidate{slot, x, first.constant});
        arrays[key] = array;
    }
};

// The condition under which the elements that start at the subscript
// `start` of the first dimension of `array` and are `width` apart start at a
// multiple of 16 bytes, when the array itself does: the first lower bound
// and the extents of the dimensions before the last, which the elements
// of the other subscripts are that many apart in, are multiples of it.
// False when they never are; `condition` is null when they always are.
bool alignment_condition(Allocator &al, const Location &loc,
        ASR::expr_t *array, size_t n_subscripts, int64_t start, int width,
        ASR::expr_t *&condition) {
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
    if (ASRUtils::extract_value(ASRUtils::expr_value(lower), l)) {
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
                        start->n_args, o, width, aligned)) {
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

} // namespace

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
