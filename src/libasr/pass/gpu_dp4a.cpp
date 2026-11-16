#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/pass/gpu_dp4a.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/intrinsic_function_registry.h>

#include <functional>
#include <vector>

namespace LCompilers {

/*
The packed dot product of four signed bytes in device code.

A sum of the form

    r = c + sb(a,0)*sb(b,0) + sb(a,1)*sb(b,1) + sb(a,2)*sb(b,2) + sb(a,3)*sb(b,3)
    sb(x,k) = shifta(shiftl(x, 24 - 8*k), 24)

is the dot product of the four bytes of a and of b, each read as an int8,
added to c. It is plain integer arithmetic that any compiler can run, and an
NVIDIA GPU has one instruction for it, `__dp4a(a, b, c)`, which gives the same
result (and wraps where the sum overflows, as the hardware add does). A
program that wants it need not call a CUDA function: it writes the sum above,
and this pass, which runs for `--gpu=cuda` on an NVIDIA GPU, turns each
occurrence into a call of `__dp4a`. Metal uses the same interface, implemented
by its device code generator with signed-byte vector casts and arithmetic.

The sum is matched on the ASR while shifta and shiftl are still intrinsic
functions, which is before any function is inlined or lowered, so that it is
found in a function of its own, in a loop of the kernel, and after a call is
inlined, alike:

- the additions may be associated any way, and every term that is not one of
  the four products is part of c;
- the factors of each product may be in either order;
- the four products have the left shifts 24, 16, 8 and 0 (a left shift by
  zero may be left out), once each;
- a and b are any two expressions, the same in the four products.

Anything else is left as it is. In particular only integer(4) is matched, and
a, b and c are compared node for node, so an operand that merely evaluates
to the same value is not recognized as the same.

On an NVIDIA GPU (not on Metal, which has no such instruction), the pass
also writes a byte of an integer(4) placed in the low byte of a constant
whose low byte is zero,

    ior(iand(shiftr(w, 8*k), 255), c)      (k = 0, 1, 2, 3)

with `iand(w, 255)`, `shiftr(w, 24)` or `ibits(w, 8*k, 8)` for the byte, as
one call of `__byte_perm(w, c, s)`, which picks each byte of its result from
the eight bytes of its first two operands: byte k of w for the lowest
(`s = 0x7650 + k`), the bytes of c for the others. A dequantization reads a
byte as a real this way (`transfer(ior(byte, 1258291200), 0.0)` is
2**23 + byte), and the instruction does in one what the shift and the mask
do in two (the two operands of ior may be in either order).
*/

namespace {

using IEF = ASRUtils::IntrinsicElementalFunctions;

bool is_int4(ASR::ttype_t *t) {
    t = ASRUtils::type_get_past_allocatable_pointer(t);
    return ASR::is_a<ASR::Integer_t>(*t) && !ASRUtils::is_array(t)
        && ASRUtils::extract_kind_from_ttype_t(t) == 4;
}

// `id(x, amount)` of an integer(4), with a constant amount.
bool shift_call(ASR::expr_t *e, IEF id, ASR::expr_t *&x, int64_t &amount) {
    if (e == nullptr || !ASR::is_a<ASR::IntrinsicElementalFunction_t>(*e)) {
        return false;
    }
    ASR::IntrinsicElementalFunction_t *f =
        ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e);
    if (f->m_intrinsic_id != static_cast<int64_t>(id) || f->n_args != 2
            || !is_int4(f->m_type)
            || !is_int4(ASRUtils::expr_type(f->m_args[0]))) {
        return false;
    }
    ASR::expr_t *n = f->m_args[1];
    if (!ASR::is_a<ASR::IntegerConstant_t>(*n)
            && !ASRUtils::extract_value(ASRUtils::expr_value(n), amount)) {
        return false;
    }
    if (ASR::is_a<ASR::IntegerConstant_t>(*n)) {
        amount = ASR::down_cast<ASR::IntegerConstant_t>(n)->m_n;
    }
    x = f->m_args[0];
    return true;
}

// `shifta(shiftl(x, left), 24)` or `shifta(x, 24)`: x, and the left shift
// that brings the byte to the top.
bool signed_byte(ASR::expr_t *e, ASR::expr_t *&x, int &left) {
    ASR::expr_t *inner = nullptr;
    int64_t amount = 0;
    if (!shift_call(e, IEF::Rshift, inner, amount) || amount != 24) {
        return false;
    }
    ASR::expr_t *shifted = nullptr;
    int64_t by = 0;
    if (shift_call(inner, IEF::Shiftl, shifted, by)
            && (by == 0 || by == 8 || by == 16 || by == 24)) {
        x = shifted;
        left = static_cast<int>(by);
    } else {
        x = inner;
        left = 0;
    }
    return true;
}

// Whether two operands are the same expression, node for node: variables,
// constants, array elements, arithmetic, casts, and the calls and intrinsic
// functions of those. Anything else compares unequal.
bool same(ASR::expr_t *a, ASR::expr_t *b) {
    if (a == nullptr || b == nullptr) return a == b;
    if (a == b) return true;
    if (a->type != b->type) return false;
    switch (a->type) {
        case ASR::exprType::Var:
            return ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(a)->m_v) ==
                ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(b)->m_v);
        case ASR::exprType::IntegerConstant:
            return ASR::down_cast<ASR::IntegerConstant_t>(a)->m_n ==
                ASR::down_cast<ASR::IntegerConstant_t>(b)->m_n
                && ASRUtils::extract_kind_from_ttype_t(
                    ASRUtils::expr_type(a)) ==
                ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(b));
        case ASR::exprType::IntegerBinOp: {
            ASR::IntegerBinOp_t *x = ASR::down_cast<ASR::IntegerBinOp_t>(a);
            ASR::IntegerBinOp_t *y = ASR::down_cast<ASR::IntegerBinOp_t>(b);
            return x->m_op == y->m_op
                && ASRUtils::extract_kind_from_ttype_t(x->m_type)
                    == ASRUtils::extract_kind_from_ttype_t(y->m_type)
                && same(x->m_left, y->m_left)
                && same(x->m_right, y->m_right);
        }
        case ASR::exprType::IntegerUnaryMinus:
            return same(ASR::down_cast<ASR::IntegerUnaryMinus_t>(a)->m_arg,
                ASR::down_cast<ASR::IntegerUnaryMinus_t>(b)->m_arg);
        case ASR::exprType::Cast: {
            ASR::Cast_t *x = ASR::down_cast<ASR::Cast_t>(a);
            ASR::Cast_t *y = ASR::down_cast<ASR::Cast_t>(b);
            return x->m_kind == y->m_kind
                && ASRUtils::extract_kind_from_ttype_t(x->m_type)
                    == ASRUtils::extract_kind_from_ttype_t(y->m_type)
                && same(x->m_arg, y->m_arg);
        }
        case ASR::exprType::ArrayItem: {
            ASR::ArrayItem_t *x = ASR::down_cast<ASR::ArrayItem_t>(a);
            ASR::ArrayItem_t *y = ASR::down_cast<ASR::ArrayItem_t>(b);
            if (x->n_args != y->n_args || !same(x->m_v, y->m_v)) return false;
            for (size_t i = 0; i < x->n_args; i++) {
                if (x->m_args[i].m_left != nullptr
                        || y->m_args[i].m_left != nullptr
                        || x->m_args[i].m_step != nullptr
                        || y->m_args[i].m_step != nullptr
                        || !same(x->m_args[i].m_right,
                            y->m_args[i].m_right)) {
                    return false;
                }
            }
            return true;
        }
        case ASR::exprType::IntrinsicElementalFunction: {
            ASR::IntrinsicElementalFunction_t *x =
                ASR::down_cast<ASR::IntrinsicElementalFunction_t>(a);
            ASR::IntrinsicElementalFunction_t *y =
                ASR::down_cast<ASR::IntrinsicElementalFunction_t>(b);
            if (x->m_intrinsic_id != y->m_intrinsic_id
                    || x->m_overload_id != y->m_overload_id
                    || x->n_args != y->n_args
                    || !is_int4(x->m_type) || !is_int4(y->m_type)) {
                return false;
            }
            for (size_t i = 0; i < x->n_args; i++) {
                if (!same(x->m_args[i], y->m_args[i])) return false;
            }
            return true;
        }
        case ASR::exprType::FunctionCall: {
            ASR::FunctionCall_t *x = ASR::down_cast<ASR::FunctionCall_t>(a);
            ASR::FunctionCall_t *y = ASR::down_cast<ASR::FunctionCall_t>(b);
            if (ASRUtils::symbol_get_past_external(x->m_name)
                    != ASRUtils::symbol_get_past_external(y->m_name)
                    || x->n_args != y->n_args || x->m_dt != nullptr
                    || y->m_dt != nullptr) {
                return false;
            }
            for (size_t i = 0; i < x->n_args; i++) {
                if (!same(x->m_args[i].m_value, y->m_args[i].m_value)) {
                    return false;
                }
            }
            return true;
        }
        default:
            return false;
    }
}

void flatten(ASR::expr_t *e, std::vector<ASR::expr_t*> &terms) {
    if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
        ASR::IntegerBinOp_t *op = ASR::down_cast<ASR::IntegerBinOp_t>(e);
        if (op->m_op == ASR::binopType::Add && op->m_value == nullptr
                && is_int4(op->m_type)) {
            flatten(op->m_left, terms);
            flatten(op->m_right, terms);
            return;
        }
    }
    terms.push_back(e);
}

// A constant integer(4) operand: its value.
bool int4_constant(ASR::expr_t *e, int64_t &v) {
    return is_int4(ASRUtils::expr_type(e))
        && ASRUtils::extract_value(ASRUtils::expr_value(e), v);
}

// Byte k of an integer(4) x in the low byte of an integer(4) whose other
// bits are zero: `iand(x, 255)`, `iand(shiftr(x, 8*k), 255)` (or shifta),
// `shiftr(x, 24)` or `ibits(x, 8*k, 8)`.
bool low_byte(ASR::expr_t *e, ASR::expr_t *&x, int &k) {
    if (e == nullptr || !ASR::is_a<ASR::IntrinsicElementalFunction_t>(*e)
            || !is_int4(ASRUtils::expr_type(e))) {
        return false;
    }
    ASR::IntrinsicElementalFunction_t *f =
        ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e);
    int64_t amount = 0;
    if (f->m_intrinsic_id == static_cast<int64_t>(IEF::Iand)
            && f->n_args == 2) {
        for (int side = 0; side < 2; side++) {
            int64_t mask = 0;
            ASR::expr_t *a = f->m_args[1 - side];
            if (!int4_constant(f->m_args[side], mask) || mask != 255
                    || !is_int4(ASRUtils::expr_type(a))) {
                continue;
            }
            ASR::expr_t *inner = nullptr;
            if ((shift_call(a, IEF::Shiftr, inner, amount)
                        || shift_call(a, IEF::Rshift, inner, amount))
                    && (amount == 0 || amount == 8 || amount == 16
                        || amount == 24)) {
                x = inner;
                k = static_cast<int>(amount / 8);
            } else {
                x = a;
                k = 0;
            }
            return true;
        }
        return false;
    }
    if (shift_call(e, IEF::Shiftr, x, amount) && amount == 24) {
        k = 3;
        return true;
    }
    if (f->m_intrinsic_id == static_cast<int64_t>(IEF::Ibits)
            && f->n_args == 3 && is_int4(ASRUtils::expr_type(f->m_args[0]))) {
        int64_t pos = 0, len = 0;
        if (int4_constant(f->m_args[1], pos)
                && int4_constant(f->m_args[2], len) && len == 8
                && (pos == 0 || pos == 8 || pos == 16 || pos == 24)) {
            x = f->m_args[0];
            k = static_cast<int>(pos / 8);
            return true;
        }
    }
    return false;
}

class Dp4aReplacer : public ASR::BaseExprReplacer<Dp4aReplacer> {
public:
    Allocator &al;
    ASR::symbol_t *&dp4a;
    ASR::symbol_t *&byte_perm;
    SymbolTable *global;

    // Whether the device has `__byte_perm`: an NVIDIA GPU, not Metal.
    bool has_byte_perm = false;

    Dp4aReplacer(Allocator &al_, ASR::symbol_t *&dp4a_,
            ASR::symbol_t *&byte_perm_, SymbolTable *global_)
        : al(al_), dp4a(dp4a_), byte_perm(byte_perm_), global(global_) {}

    // The device dot-product interface: a CUDA instruction or a Metal helper.
    ASR::symbol_t *function(const Location &loc) {
        return interface(loc, "__dp4a", dp4a);
    }

    // The interface of the instruction `name` of three integer(4) operands
    // and an integer(4) result, added to the global scope once.
    ASR::symbol_t *interface(const Location &loc, const std::string &name,
            ASR::symbol_t *&cached) {
        if (cached != nullptr) return cached;
        if (ASR::symbol_t *existing = global->get_symbol(name)) {
            return cached = existing;
        }
        ASRUtils::ASRBuilder b(al, loc);
        SymbolTable *fn_symtab = al.make_new<SymbolTable>(global);
        Vec<ASR::expr_t*> args;
        args.reserve(al, 3);
        for (const char *arg : {"a", "b", "c"}) {
            args.push_back(al, b.Variable(fn_symtab, arg,
                ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)),
                ASR::intentType::In, nullptr, ASR::abiType::BindC, true));
        }
        ASR::expr_t *return_var = b.Variable(fn_symtab, name,
            ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)),
            ASRUtils::intent_return_var, nullptr, ASR::abiType::BindC, false);
        ASR::asr_t *fn = ASRUtils::make_Function_t_util(al, loc, fn_symtab,
            s2c(al, name), nullptr, 0, args.p, args.n, nullptr, 0,
            return_var, ASR::abiType::BindC, ASR::accessType::Public,
            ASR::deftypeType::Interface, s2c(al, name), false, true, false,
            false, false, nullptr, 0, false, false, false, nullptr);
        cached = ASR::down_cast<ASR::symbol_t>(fn);
        global->add_symbol(name, cached);
        return cached;
    }

    // `ior(byte k of w, c)` with a constant c whose low byte is zero:
    // `__byte_perm(w, c, 0x7650 + k)`.
    void replace_IntrinsicElementalFunction(
            ASR::IntrinsicElementalFunction_t *x) {
        ASR::BaseExprReplacer<Dp4aReplacer>::
            replace_IntrinsicElementalFunction(x);
        if (!has_byte_perm
                || x->m_intrinsic_id != static_cast<int64_t>(IEF::Ior)
                || x->n_args != 2 || x->m_value != nullptr
                || !is_int4(x->m_type)) {
            return;
        }
        for (int side = 0; side < 2; side++) {
            int64_t c = 0;
            ASR::expr_t *w = nullptr;
            int k = 0;
            if (!int4_constant(x->m_args[1 - side], c) || (c & 255) != 0
                    || !low_byte(x->m_args[side], w, k)) {
                continue;
            }
            Location loc = x->base.base.loc;
            ASRUtils::ASRBuilder b(al, loc);
            Vec<ASR::call_arg_t> args;
            args.reserve(al, 3);
            for (ASR::expr_t *e : {w, x->m_args[1 - side],
                    b.i32(0x7650 + k)}) {
                ASR::call_arg_t arg;
                arg.loc = loc;
                arg.m_value = e;
                args.push_back(al, arg);
            }
            ASR::symbol_t *fn = interface(loc, "__byte_perm", byte_perm);
            *current_expr = ASRUtils::EXPR(ASR::make_FunctionCall_t(al, loc,
                fn, nullptr, args.p, args.n,
                ASRUtils::duplicate_type(al, x->m_type), nullptr, nullptr));
            return;
        }
    }

    void replace_IntegerBinOp(ASR::IntegerBinOp_t *x) {
        ASR::BaseExprReplacer<Dp4aReplacer>::replace_IntegerBinOp(x);
        if (x->m_op != ASR::binopType::Add || x->m_value != nullptr
                || !is_int4(x->m_type)) {
            return;
        }
        std::vector<ASR::expr_t*> terms;
        flatten(&x->base, terms);
        if (terms.size() < 4 || terms.size() > 64) return;

        // The product at each byte position: its two operands.
        ASR::expr_t *first[4] = {nullptr, nullptr, nullptr, nullptr};
        ASR::expr_t *second[4] = {nullptr, nullptr, nullptr, nullptr};
        size_t position[4] = {0, 0, 0, 0};
        int found = 0;
        for (size_t i = 0; i < terms.size(); i++) {
            if (!ASR::is_a<ASR::IntegerBinOp_t>(*terms[i])) continue;
            ASR::IntegerBinOp_t *mul =
                ASR::down_cast<ASR::IntegerBinOp_t>(terms[i]);
            if (mul->m_op != ASR::binopType::Mul || mul->m_value != nullptr
                    || !is_int4(mul->m_type)) {
                continue;
            }
            ASR::expr_t *u = nullptr, *v = nullptr;
            int lu = 0, lv = 0;
            if (!signed_byte(mul->m_left, u, lu)
                    || !signed_byte(mul->m_right, v, lv) || lu != lv) {
                continue;
            }
            int k = lu / 8;
            if (first[k] != nullptr) continue;
            first[k] = u;
            second[k] = v;
            position[k] = i;
            found++;
        }
        if (found != 4) return;
        for (int k = 1; k < 4; k++) {
            if (!((same(first[k], first[0]) && same(second[k], second[0]))
                    || (same(first[k], second[0])
                        && same(second[k], first[0])))) {
                return;
            }
        }

        // c: the terms that are not one of the four products.
        Location loc = x->base.base.loc;
        ASR::expr_t *c = nullptr;
        for (size_t i = 0; i < terms.size(); i++) {
            if (i == position[0] || i == position[1] || i == position[2]
                    || i == position[3]) {
                continue;
            }
            c = c == nullptr ? terms[i]
                : ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc, c,
                    ASR::binopType::Add, terms[i],
                    ASRUtils::duplicate_type(al, x->m_type), nullptr));
        }
        if (c == nullptr) {
            c = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 0,
                ASRUtils::duplicate_type(al, x->m_type)));
        }
        Vec<ASR::call_arg_t> args;
        args.reserve(al, 3);
        for (ASR::expr_t *e : {first[0], second[0], c}) {
            ASR::call_arg_t arg;
            arg.loc = loc;
            arg.m_value = e;
            args.push_back(al, arg);
        }
        ASR::symbol_t *fn = function(loc);
        *current_expr = ASRUtils::EXPR(ASR::make_FunctionCall_t(al, loc, fn,
            nullptr, args.p, args.n,
            ASRUtils::duplicate_type(al, x->m_type), nullptr, nullptr));
    }
};

class Dp4aVisitor : public ASR::CallReplacerOnExpressionsVisitor<Dp4aVisitor> {
public:
    Dp4aReplacer replacer;
    Dp4aVisitor(Allocator &al, ASR::symbol_t *&dp4a,
            ASR::symbol_t *&byte_perm, SymbolTable *global)
        : replacer(al, dp4a, byte_perm, global) {}

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }

    // The statements of a BLOCK of the body, such as the body of an
    // iteration of a `do concurrent` with locals of its own.
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_m);
        if (!ASR::is_a<ASR::Block_t>(*s)) return;
        ASR::Block_t *b = ASR::down_cast<ASR::Block_t>(s);
        for (size_t i = 0; i < b->n_body; i++) visit_stmt(*b->m_body[i]);
    }
};

} // namespace

void pass_gpu_dp4a(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options) {
    // Metal lowers the operation to vector arithmetic. CUDA requires native
    // hardware; its CPU emulation and HIP keep the portable arithmetic.
    const GpuDevice device = gpu_device_selected(pass_options);
    if (device != GpuDevice::Metal &&
            (device != GpuDevice::Cuda || !pass_options.gpu_cuda_warps)) {
        return;
    }
    ASR::symbol_t *dp4a = nullptr, *byte_perm = nullptr;
    Dp4aVisitor visitor(al, dp4a, byte_perm, unit.m_symtab);
    visitor.replacer.has_byte_perm = device == GpuDevice::Cuda;
    std::function<void(SymbolTable*)> visit = [&](SymbolTable *scope) {
        // The visit can add `__dp4a` to the scope that is being walked.
        std::vector<ASR::symbol_t*> symbols;
        for (auto &entry : scope->get_scope()) symbols.push_back(entry.second);
        for (ASR::symbol_t *s : symbols) {
            if (ASR::is_a<ASR::Function_t>(*s)) {
                ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(s);
                if (ASRUtils::is_device_only_function(*fn)) {
                    for (size_t i = 0; i < fn->n_body; i++) {
                        visitor.visit_stmt(*fn->m_body[i]);
                    }
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
    if (dp4a != nullptr || byte_perm != nullptr) {
        PassUtils::UpdateDependenciesVisitor update(al);
        update.visit_TranslationUnit(unit);
    }
}

} // namespace LCompilers
