#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/pass/gpu_integer_powers.h>
#include <libasr/pass/intrinsic_function_registry.h>

#include <functional>

namespace LCompilers {

/*
Real powers with small constant integer exponents in device code.

The host computes `x**k`, for a real `x` and a constant integer k from 0 to
gpu_power_max_exponent, as products of `x` (asr_to_llvm.cpp):

    x**0 = 1               x**5 = (x2*x2)*x
    x**1 = x               x**6 = (x2*x2)*x2
    x**2 = x*x             x**7 = ((x2*x2)*x2)*x
    x**3 = x*(x*x)         x**8 = (x2*x2)*(x2*x2)
    x**4 = x2*x2           where x2 = x*x

A device code generator would write `pow(x, k)`, which the device evaluates
as exp2(k*log2(x)) under fast math, more slowly and not always exactly. So
such a power in device code becomes the same products here, and the device
computes what the host does: `x*x` is the correctly rounded square.

`x` is evaluated once. A variable or a constant is written once per
factor. Anything else, in an operand that is always evaluated of an
assignment, a test of an IF or a SELECT, the bounds of a loop or an argument
of a subroutine call, is assigned to a variable of its own right before the
statement, and the products read that. Elsewhere, such as in the test of a
DO WHILE or in an operand that need not be evaluated (the second operand of
`.and.`, a branch of `merge`), an element of an array whose subscripts call
no function is written once per factor, and a power of anything else is
kept as it is.

The pass runs once the device code is complete -- after the intrinsic
functions and the array operations are lowered, which write such powers
too -- and before the device heap and the kernels are laid out.
*/

namespace {

constexpr int64_t gpu_power_max_exponent = 8;

// Whether `e` can be written several times for one evaluation: it reads a
// variable or an element of an array, and calls no function.
bool designator(ASR::expr_t *e) {
    switch (e->type) {
        case ASR::exprType::Var:
            return ASR::is_a<ASR::Variable_t>(*ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(e)->m_v));
        case ASR::exprType::RealConstant:
            return true;
        case ASR::exprType::StructInstanceMember:
            return designator(
                ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v);
        case ASR::exprType::ArrayItem: {
            ASR::ArrayItem_t *x = ASR::down_cast<ASR::ArrayItem_t>(e);
            if (!designator(x->m_v)) return false;
            struct Calls : public ASR::BaseWalkVisitor<Calls> {
                bool found = false;
                void visit_FunctionCall(const ASR::FunctionCall_t &) {
                    found = true;
                }
            } c;
            for (size_t i = 0; i < x->n_args; i++) {
                if (x->m_args[i].m_left) c.visit_expr(*x->m_args[i].m_left);
                if (x->m_args[i].m_right) c.visit_expr(*x->m_args[i].m_right);
                if (x->m_args[i].m_step) c.visit_expr(*x->m_args[i].m_step);
            }
            return !c.found;
        }
        default:
            return false;
    }
}

// Rewrites the powers of an expression. With `pre`, a base that is not a
// designator is assigned to a new variable of `scope`, by an assignment
// appended to `pre`.
class PowerReplacer : public ASR::BaseExprReplacer<PowerReplacer> {
public:
    Allocator &al;
    SymbolTable *scope;
    Vec<ASR::stmt_t*> *pre;
    // Inside an operand that need not be evaluated.
    int conditional = 0;

    PowerReplacer(Allocator &al_, SymbolTable *scope_,
            Vec<ASR::stmt_t*> *pre_) : al(al_), scope(scope_), pre(pre_) {}

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
        ASR::BaseExprReplacer<PowerReplacer>::
            replace_IntrinsicElementalFunction(x);
        if (is_merge) conditional--;
    }

    void replace_RealBinOp(ASR::RealBinOp_t *x) {
        ASR::BaseExprReplacer<PowerReplacer>::replace_RealBinOp(x);
        if (x->m_op != ASR::binopType::Pow || x->m_value != nullptr) return;
        ASR::ttype_t *rt = ASRUtils::expr_type(x->m_right);
        int64_t k = -1;
        if (!ASRUtils::is_integer(*rt) || ASRUtils::is_array(rt)
                || !ASRUtils::extract_value(x->m_right, k)
                || k < 0 || k > gpu_power_max_exponent) {
            return;
        }
        ASR::ttype_t *t = x->m_type;
        ASR::ttype_t *bt = ASRUtils::expr_type(x->m_left);
        if (ASRUtils::is_array(t) || !ASRUtils::is_real(*t)
                || ASRUtils::is_array(bt) || !ASRUtils::is_real(*bt)
                || ASRUtils::extract_kind_from_ttype_t(t)
                    != ASRUtils::extract_kind_from_ttype_t(bt)) {
            return;
        }
        Location loc = x->base.base.loc;
        ASR::expr_t *base = x->m_left;
        bool hoist = pre != nullptr && conditional == 0;
        if (k >= 2 && !ASR::is_a<ASR::Var_t>(*base)
                && !ASR::is_a<ASR::RealConstant_t>(*base)
                && (hoist || !designator(base))) {
            if (!hoist) return;
            ASRUtils::ASRBuilder b(al, loc);
            std::string name = scope->get_unique_name("__gpu_power_base",
                false);
            ASR::expr_t *v = b.Variable(scope, name,
                ASRUtils::duplicate_type(al,
                    ASRUtils::type_get_past_allocatable_pointer(bt)),
                ASR::intentType::Local);
            pre->push_back(al, b.Assignment(v, base));
            base = v;
        }
        *current_expr = power(loc, base, k, t);
    }

private:
    ASR::expr_t *copy(ASR::expr_t *e) {
        ASRUtils::ExprStmtDuplicator d(al);
        return d.duplicate_expr(e);
    }

    ASR::expr_t *mul(const Location &loc, ASR::expr_t *a, ASR::expr_t *b,
            ASR::ttype_t *t) {
        return ASRUtils::EXPR(ASR::make_RealBinOp_t(al, loc, a,
            ASR::binopType::Mul, b, ASRUtils::duplicate_type(al, t),
            nullptr));
    }

    // The products of asr_to_llvm.cpp for x**k, 0 <= k <= 8.
    ASR::expr_t *power(const Location &loc, ASR::expr_t *x, int64_t k,
            ASR::ttype_t *t) {
        auto x2 = [&]() { return mul(loc, copy(x), copy(x), t); };
        auto x4 = [&]() { return mul(loc, x2(), x2(), t); };
        switch (k) {
            case 0:
                return ASRUtils::EXPR(ASR::make_RealConstant_t(al, loc, 1.0,
                    ASRUtils::duplicate_type(al, t)));
            case 1: return x;
            case 2: return x2();
            case 3: return mul(loc, copy(x), x2(), t);
            case 4: return x4();
            case 5: return mul(loc, x4(), copy(x), t);
            case 6: return mul(loc, x4(), x2(), t);
            case 7: return mul(loc, mul(loc, x4(), x2(), t), copy(x), t);
            default: return mul(loc, x4(), x4(), t);
        }
    }
};

class PowerLowering {
public:
    explicit PowerLowering(Allocator &al_) : al(al_) {}

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

    void expr(SymbolTable *scope, ASR::expr_t *&e, Vec<ASR::stmt_t*> *pre) {
        if (e == nullptr) return;
        PowerReplacer r(al, scope, pre);
        r.current_expr = &e;
        r.replace_expr(e);
    }

    void head(SymbolTable *scope, ASR::do_loop_head_t &h,
            Vec<ASR::stmt_t*> &pre) {
        expr(scope, h.m_start, &pre);
        expr(scope, h.m_end, &pre);
        expr(scope, h.m_increment, &pre);
    }

    void stmt(SymbolTable *scope, ASR::stmt_t *s, Vec<ASR::stmt_t*> &pre) {
        switch (s->type) {
            case ASR::stmtType::Assignment: {
                ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(s);
                if (a->m_overloaded) return;
                expr(scope, a->m_target, nullptr);
                expr(scope, a->m_value, &pre);
                return;
            }
            case ASR::stmtType::If: {
                ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                expr(scope, x->m_test, &pre);
                body(scope, x->m_body, x->n_body);
                body(scope, x->m_orelse, x->n_orelse);
                return;
            }
            case ASR::stmtType::DoLoop: {
                ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(s);
                head(scope, x->m_head, pre);
                body(scope, x->m_body, x->n_body);
                body(scope, x->m_orelse, x->n_orelse);
                return;
            }
            case ASR::stmtType::DoConcurrentLoop: {
                ASR::DoConcurrentLoop_t *x =
                    ASR::down_cast<ASR::DoConcurrentLoop_t>(s);
                for (size_t i = 0; i < x->n_head; i++) {
                    head(scope, x->m_head[i], pre);
                }
                body(scope, x->m_body, x->n_body);
                return;
            }
            case ASR::stmtType::WhileLoop: {
                ASR::WhileLoop_t *x = ASR::down_cast<ASR::WhileLoop_t>(s);
                expr(scope, x->m_test, nullptr);
                body(scope, x->m_body, x->n_body);
                body(scope, x->m_orelse, x->n_orelse);
                return;
            }
            case ASR::stmtType::Select: {
                ASR::Select_t *x = ASR::down_cast<ASR::Select_t>(s);
                expr(scope, x->m_test, &pre);
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
            case ASR::stmtType::SubroutineCall: {
                ASR::SubroutineCall_t *x =
                    ASR::down_cast<ASR::SubroutineCall_t>(s);
                for (size_t i = 0; i < x->n_args; i++) {
                    expr(scope, x->m_args[i].m_value, &pre);
                }
                return;
            }
            default:
                return;
        }
    }
};

} // namespace

void pass_gpu_integer_powers(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options) {
    if (gpu_device_selected(pass_options) == GpuDevice::None) return;
    PowerLowering lowering(al);
    std::function<void(SymbolTable*)> visit = [&](SymbolTable *scope) {
        for (auto &entry : scope->get_scope()) {
            ASR::symbol_t *s = entry.second;
            if (ASR::is_a<ASR::Function_t>(*s)) {
                ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(s);
                if (ASRUtils::is_device_only_function(*fn)) {
                    lowering.body(fn->m_symtab, fn->m_body, fn->n_body);
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
