#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_builder.h>
#include <libasr/pass/gpu_residue_unroll.h>
#include <libasr/pass/gpu_affine.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/pass/gpu_loop_utils.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/pass_utils.h>

#include <functional>
#include <map>
#include <set>
#include <string>
#include <vector>

namespace LCompilers {

/*
Residue unrolling of device loops.

A loop of device code that decodes packed data reads, for each index `d`,
the word `w((d - 1)/4 + 1)` and the part `modulo(d - 1, 4)` of it, often
through a small pure function that checks the part:

    do d = a, b
        acc = acc + real(byte(w((d - 1)/4 + 1), modulo(d - 1, 4)))*x(d)
    end do

Four consecutive iterations read the same word, and each recomputes the
quotient, the residue and the check. Here the loop is unrolled by the
divisor K, in groups whose first index `G` makes `G + r` (the numerator
`d + r` at `d = G`) a non-negative multiple of K. In copy j of the body,
`(d + r)/K` is `(G + r)/K` and `modulo(d + r, K)` (or `mod`) is the
constant j:

    do G = a', b - (K - 1), K
        word = w((G - 1)/4 + 1)
        acc = acc + real(byte(word, 0))*x(G)
        acc = acc + real(byte(word, 1))*x(G + 1)
        acc = acc + real(byte(word, 2))*x(G + 2)
        acc = acc + real(byte(word, 3))*x(G + 3)
    end do

* A call of a small pure function with a constant argument in a copy is
  inlined (GpuInliner), and an IF whose test is then constant, such as the
  function's check of its argument, is folded. A check that still fails
  is kept: its STOP runs where the call did.
* An array element the copies read at the same subscripts, in a position
  of the first copy that is always evaluated, of an array the body does
  not define (or alias), is read once per group, before the copies.

The iterations run in their original order, so the transformation is
exact. The groups need the numerator to be non-negative, where the
truncating division equals the floor and `mod` equals `modulo`, and at
least K iterations. What the loop nest does not prove (its bounds as facts,
gpu_affine.h) is tested before the loop, which runs as written otherwise:

    if (a + r >= 0 .and. b - a >= K - 1) then
        a' = a + mod(K - mod(a + r, K), K)
        do d = a, a' - 1            ! peel, omitted when a + r is aligned
            body
        end do
        do G = a', b - (K - 1), K   ! the groups
            ...
        end do
        do d = G, b                 ! remainder, omitted when b - a + 1
            body                    ! is a multiple of K and a aligned
        end do
    else
        do d = a, b
            body
        end do
    end if

The loop must count up by one, with a 32-bit index that the body does not
define; its body must not leave it (EXIT, CYCLE, GO TO), and every variable
the numerator reads besides `d` must keep its value in it. Only quotients
and residues of the most frequent divisor (at most
residue_unroll_max_factor) and numerator are replaced; the others are
computed from `G + j` as before.

The pass runs before intrinsic functions are lowered, while `modulo` and
`mod` are still intrinsics, and before `stop` in device code is lowered
(gpu_device_heap), so that inlined checks are still IF and STOP
statements.
*/

namespace {

constexpr int64_t residue_unroll_max_factor = 8;
// The most gpu_stmts_cost of the body of an unrolled loop, times K.
constexpr int64_t residue_unroll_max_cost = 512;
// The most gpu_stmts_cost of the body of a function inlined in a copy.
constexpr int64_t residue_inline_max_cost = 32;

bool constant_integer(ASR::expr_t *e, int64_t &v) {
    if (e == nullptr) return false;
    if (ASR::is_a<ASR::IntegerConstant_t>(*e)) {
        v = ASR::down_cast<ASR::IntegerConstant_t>(e)->m_n;
        return true;
    }
    ASR::expr_t *value = ASRUtils::expr_value(e);
    if (value && ASR::is_a<ASR::IntegerConstant_t>(*value)) {
        v = ASR::down_cast<ASR::IntegerConstant_t>(value)->m_n;
        return true;
    }
    return false;
}

bool integer_kind_4(ASR::expr_t *e) {
    ASR::ttype_t *t = ASRUtils::expr_type(e);
    return ASR::is_a<ASR::Integer_t>(*t)
        && ASRUtils::extract_kind_from_ttype_t(t) == 4;
}

enum class ResidueForm { None, Quotient, Residue };

// Whether `e` is `n/K`, `modulo(n, K)` or `mod(n, K)` of 32-bit integers
// with a constant K >= 2.
ResidueForm residue_form(ASR::expr_t *e, ASR::expr_t *&numerator,
        int64_t &k) {
    if (!integer_kind_4(e)) return ResidueForm::None;
    if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
        ASR::IntegerBinOp_t *b = ASR::down_cast<ASR::IntegerBinOp_t>(e);
        if (b->m_op != ASR::binopType::Div
                || !constant_integer(b->m_right, k) || k < 2
                || !integer_kind_4(b->m_left)) {
            return ResidueForm::None;
        }
        numerator = b->m_left;
        return ResidueForm::Quotient;
    }
    if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*e)) {
        ASR::IntrinsicElementalFunction_t *f =
            ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e);
        if ((f->m_intrinsic_id != static_cast<int64_t>(
                    ASRUtils::IntrinsicElementalFunctions::Modulo)
                && f->m_intrinsic_id != static_cast<int64_t>(
                    ASRUtils::IntrinsicElementalFunctions::Mod))
                || f->n_args != 2 || !constant_integer(f->m_args[1], k)
                || k < 2 || !integer_kind_4(f->m_args[0])) {
            return ResidueForm::None;
        }
        numerator = f->m_args[0];
        return ResidueForm::Residue;
    }
    return ResidueForm::None;
}

// ---------------------------------------------------------------------------
// Folding of the tests a constant argument decides

bool fold_integer(ASR::expr_t *e, int64_t &v) {
    if (constant_integer(e, v)) return true;
    switch (e->type) {
        case ASR::exprType::IntegerBinOp: {
            ASR::IntegerBinOp_t *b = ASR::down_cast<ASR::IntegerBinOp_t>(e);
            int64_t l, r;
            if (!fold_integer(b->m_left, l) || !fold_integer(b->m_right, r)) {
                return false;
            }
            switch (b->m_op) {
                case ASR::binopType::Add: v = l + r; break;
                case ASR::binopType::Sub: v = l - r; break;
                case ASR::binopType::Mul: v = l * r; break;
                case ASR::binopType::Div:
                    if (r == 0) return false;
                    v = l / r;
                    break;
                default: return false;
            }
            // Only values of the kind the expression has.
            return ASRUtils::extract_kind_from_ttype_t(b->m_type) != 4
                || (v >= INT32_MIN && v <= INT32_MAX);
        }
        case ASR::exprType::IntegerUnaryMinus: {
            int64_t a;
            if (!fold_integer(
                    ASR::down_cast<ASR::IntegerUnaryMinus_t>(e)->m_arg, a)) {
                return false;
            }
            v = -a;
            return true;
        }
        default:
            return false;
    }
}

bool fold_logical(ASR::expr_t *e, bool &v) {
    switch (e->type) {
        case ASR::exprType::LogicalConstant:
            v = ASR::down_cast<ASR::LogicalConstant_t>(e)->m_value;
            return true;
        case ASR::exprType::LogicalNot: {
            bool a;
            if (!fold_logical(ASR::down_cast<ASR::LogicalNot_t>(e)->m_arg,
                    a)) {
                return false;
            }
            v = !a;
            return true;
        }
        case ASR::exprType::IntegerCompare: {
            ASR::IntegerCompare_t *c = ASR::down_cast<ASR::IntegerCompare_t>(e);
            int64_t l, r;
            if (!fold_integer(c->m_left, l) || !fold_integer(c->m_right, r)) {
                return false;
            }
            switch (c->m_op) {
                case ASR::cmpopType::Eq: v = l == r; break;
                case ASR::cmpopType::NotEq: v = l != r; break;
                case ASR::cmpopType::Lt: v = l < r; break;
                case ASR::cmpopType::LtE: v = l <= r; break;
                case ASR::cmpopType::Gt: v = l > r; break;
                case ASR::cmpopType::GtE: v = l >= r; break;
                default: return false;
            }
            return true;
        }
        case ASR::exprType::LogicalBinOp: {
            ASR::LogicalBinOp_t *b = ASR::down_cast<ASR::LogicalBinOp_t>(e);
            bool l, r;
            bool lk = fold_logical(b->m_left, l);
            bool rk = fold_logical(b->m_right, r);
            switch (b->m_op) {
                case ASR::logicalbinopType::And:
                    if ((lk && !l) || (rk && !r)) { v = false; return true; }
                    if (lk && rk) { v = true; return true; }
                    return false;
                case ASR::logicalbinopType::Or:
                    if ((lk && l) || (rk && r)) { v = true; return true; }
                    if (lk && rk) { v = false; return true; }
                    return false;
                case ASR::logicalbinopType::Eqv:
                    if (!lk || !rk) return false;
                    v = l == r;
                    return true;
                case ASR::logicalbinopType::NEqv:
                    if (!lk || !rk) return false;
                    v = l != r;
                    return true;
                default:
                    return false;
            }
        }
        default:
            return false;
    }
}

// Replaces every IF of a statement list (and of the constructs and BLOCKs
// it runs) whose test is a constant by the branch the test selects.
void fold_branches(Allocator &al, ASR::stmt_t **&body, size_t &n) {
    Vec<ASR::stmt_t*> out;
    out.reserve(al, n);
    for (size_t i = 0; i < n; i++) {
        ASR::stmt_t *s = body[i];
        switch (s->type) {
            case ASR::stmtType::If: {
                ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                fold_branches(al, x->m_body, x->n_body);
                fold_branches(al, x->m_orelse, x->n_orelse);
                bool v;
                // A named IF can be the target of an EXIT.
                if ((x->m_name == nullptr || x->m_name[0] == '\0')
                        && fold_logical(x->m_test, v)) {
                    ASR::stmt_t **taken = v ? x->m_body : x->m_orelse;
                    size_t taken_n = v ? x->n_body : x->n_orelse;
                    for (size_t k = 0; k < taken_n; k++) {
                        out.push_back(al, taken[k]);
                    }
                    continue;
                }
                break;
            }
            case ASR::stmtType::DoLoop: {
                ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(s);
                fold_branches(al, x->m_body, x->n_body);
                break;
            }
            case ASR::stmtType::BlockCall: {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                if (ASR::is_a<ASR::Block_t>(*b)) {
                    ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                    fold_branches(al, blk->m_body, blk->n_body);
                }
                break;
            }
            default:
                break;
        }
        out.push_back(al, s);
    }
    body = out.p;
    n = out.n;
}

// ---------------------------------------------------------------------------
// Inspection

// Whether a statement list can leave an iteration other than by STOP or
// RETURN, which end the whole thread in order: EXIT, CYCLE or GO TO.
bool leaves_iteration(ASR::stmt_t **body, size_t n) {
    struct Find : public ASRUtils::BlockBodyWalkVisitor<Find> {
        bool found = false;
        void visit_Exit(const ASR::Exit_t &) { found = true; }
        void visit_Cycle(const ASR::Cycle_t &) { found = true; }
        void visit_GoTo(const ASR::GoTo_t &) { found = true; }
        void visit_GoToTarget(const ASR::GoToTarget_t &) { found = true; }
    } f;
    for (size_t i = 0; i < n; i++) f.visit_stmt(*body[i]);
    return f.found;
}

bool mentions(ASR::stmt_t **body, size_t n, ASR::symbol_t *v) {
    std::set<ASR::symbol_t*> syms;
    gpu_collect_symbols(body, n, syms);
    return syms.count(v) > 0;
}

// Whether a variable is named anywhere in a statement list outside the DO
// loops whose variable it is (so that its value after such a loop is read).
bool named_outside_its_loops(ASR::stmt_t **body, size_t n,
        ASR::symbol_t *v) {
    struct Find : public ASRUtils::BlockBodyWalkVisitor<Find> {
        ASR::symbol_t *v = nullptr;
        bool found = false;
        void visit_DoLoop(const ASR::DoLoop_t &x) {
            if (x.m_head.m_v && ASR::is_a<ASR::Var_t>(*x.m_head.m_v)
                    && ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::Var_t>(x.m_head.m_v)->m_v) == v) {
                return;
            }
            ASRUtils::BlockBodyWalkVisitor<Find>::visit_DoLoop(x);
        }
        void visit_Var(const ASR::Var_t &x) {
            if (ASRUtils::symbol_get_past_external(x.m_v) == v) found = true;
        }
    } f;
    f.v = v;
    for (size_t i = 0; i < n && !f.found; i++) f.visit_stmt(*body[i]);
    return f.found;
}

// Whether a function's body holds a STOP or ERROR STOP.
bool function_stops(ASR::Function_t *fn) {
    struct Find : public ASR::BaseWalkVisitor<Find> {
        bool found = false;
        void visit_Stop(const ASR::Stop_t &) { found = true; }
        void visit_ErrorStop(const ASR::ErrorStop_t &) { found = true; }
    } f;
    for (size_t i = 0; i < fn->n_body; i++) f.visit_stmt(*fn->m_body[i]);
    return f.found;
}

// The calls of a statement list the inliner may move before the statement
// that evaluates them: in an assignment, outside the operands that need
// not be evaluated (GpuCallReplacer).
void collect_movable_calls(ASR::stmt_t **body, size_t n,
        std::set<ASR::FunctionCall_t*> &calls) {
    struct Movable : public ASR::BaseWalkVisitor<Movable> {
        std::set<ASR::FunctionCall_t*> *calls;
        void visit_LogicalBinOp(const ASR::LogicalBinOp_t &x) {
            visit_expr(*x.m_left);
        }
        void visit_IfExp(const ASR::IfExp_t &x) { visit_expr(*x.m_test); }
        void visit_IntrinsicElementalFunction(
                const ASR::IntrinsicElementalFunction_t &x) {
            if (x.m_intrinsic_id == static_cast<int64_t>(
                    ASRUtils::IntrinsicElementalFunctions::Merge)) {
                return;
            }
            ASR::BaseWalkVisitor<Movable>::visit_IntrinsicElementalFunction(
                x);
        }
        void visit_FunctionCall(const ASR::FunctionCall_t &x) {
            calls->insert(const_cast<ASR::FunctionCall_t*>(&x));
            ASR::BaseWalkVisitor<Movable>::visit_FunctionCall(x);
        }
    };
    for (size_t i = 0; i < n; i++) {
        ASR::stmt_t *s = body[i];
        if (ASR::is_a<ASR::Assignment_t>(*s)) {
            ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(s);
            if (a->m_overloaded) continue;
            Movable m;
            m.calls = &calls;
            m.visit_expr(*a->m_target);
            m.visit_expr(*a->m_value);
        } else if (ASR::is_a<ASR::BlockCall_t>(*s)) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
            if (ASR::is_a<ASR::Block_t>(*b)) {
                ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                collect_movable_calls(blk->m_body, blk->n_body, calls);
            }
        } else if (ASR::is_a<ASR::If_t>(*s)) {
            ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
            collect_movable_calls(x->m_body, x->n_body, calls);
            collect_movable_calls(x->m_orelse, x->n_orelse, calls);
        }
    }
}

// A structural key of an array element whose subscripts are scalar
// integer expressions gpu_affine_key can describe, or "".
std::string element_key(ASR::ArrayItem_t *x) {
    std::string k = gpu_array_key(x->m_v);
    if (k.empty()) return "";
    for (size_t i = 0; i < x->n_args; i++) {
        if (x->m_args[i].m_left || x->m_args[i].m_step
                || x->m_args[i].m_right == nullptr) {
            return "";
        }
        std::string s = gpu_affine_key(x->m_args[i].m_right);
        if (s.empty()) return "";
        k += "|" + s;
    }
    return k;
}

// The variable an array designator starts from.
ASR::symbol_t *designator_base(ASR::expr_t *e) {
    while (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
        e = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v;
    }
    if (!ASR::is_a<ASR::Var_t>(*e)) return nullptr;
    return ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(e)->m_v);
}

// The array elements the statements of a list always evaluate, in order,
// up to the first statement that may not run the rest: positions inside
// an IF or a loop, or in an operand that need not be evaluated, are left
// out.
void collect_evaluated_elements(ASR::stmt_t **body, size_t n,
        std::vector<ASR::ArrayItem_t*> &items, bool &stopped) {
    struct Evaluated : public ASR::BaseWalkVisitor<Evaluated> {
        std::vector<ASR::ArrayItem_t*> *items;
        void visit_LogicalBinOp(const ASR::LogicalBinOp_t &x) {
            visit_expr(*x.m_left);
        }
        void visit_IfExp(const ASR::IfExp_t &x) { visit_expr(*x.m_test); }
        void visit_IntrinsicElementalFunction(
                const ASR::IntrinsicElementalFunction_t &x) {
            if (x.m_intrinsic_id == static_cast<int64_t>(
                    ASRUtils::IntrinsicElementalFunctions::Merge)) {
                return;
            }
            ASR::BaseWalkVisitor<Evaluated>::visit_IntrinsicElementalFunction(
                x);
        }
        void visit_ArrayItem(const ASR::ArrayItem_t &x) {
            items->push_back(const_cast<ASR::ArrayItem_t*>(&x));
            ASR::BaseWalkVisitor<Evaluated>::visit_ArrayItem(x);
        }
    };
    for (size_t i = 0; i < n && !stopped; i++) {
        ASR::stmt_t *s = body[i];
        if (ASR::is_a<ASR::Assignment_t>(*s)) {
            ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(s);
            if (a->m_overloaded) {
                stopped = true;
                return;
            }
            Evaluated e;
            e.items = &items;
            e.visit_expr(*a->m_value);
            if (ASR::is_a<ASR::ArrayItem_t>(*a->m_target)) {
                ASR::ArrayItem_t *t = ASR::down_cast<ASR::ArrayItem_t>(
                    a->m_target);
                for (size_t k = 0; k < t->n_args; k++) {
                    if (t->m_args[k].m_right) {
                        e.visit_expr(*t->m_args[k].m_right);
                    }
                }
            }
        } else if (ASR::is_a<ASR::BlockCall_t>(*s)) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
            if (!ASR::is_a<ASR::Block_t>(*b)) {
                stopped = true;
                return;
            }
            ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
            collect_evaluated_elements(blk->m_body, blk->n_body, items,
                stopped);
        } else {
            stopped = true;
        }
    }
}

void collect_elements(ASR::stmt_t **body, size_t n,
        std::set<std::string> &keys) {
    struct All : public ASRUtils::BlockBodyWalkVisitor<All> {
        std::set<std::string> *keys;
        void visit_ArrayItem(const ASR::ArrayItem_t &x) {
            std::string k = element_key(const_cast<ASR::ArrayItem_t*>(&x));
            if (!k.empty()) keys->insert(k);
            ASRUtils::BlockBodyWalkVisitor<All>::visit_ArrayItem(x);
        }
    } a;
    a.keys = &keys;
    for (size_t i = 0; i < n; i++) a.visit_stmt(*body[i]);
}

// ---------------------------------------------------------------------------

class ResidueUnroll {
    Allocator &al;
    const PassOptions &pass_options;
    std::set<ASR::symbol_t*> address_taken;

    // The loops around the statement list being visited, outermost first.
    struct Enclosing {
        ASR::expr_t *v, *start, *end;
        int64_t step;
        ASR::stmt_t **body;
        size_t n;
    };
    std::vector<Enclosing> enclosing;
    // The function being visited.
    SymbolTable *function_scope = nullptr;
    ASR::stmt_t **function_body = nullptr;
    size_t function_n = 0;

    // Whether `s` is declared in `scope` or in a scope around it, up to
    // the function.
    bool local_to_function(ASR::symbol_t *s, SymbolTable *scope) {
        SymbolTable *home = ASRUtils::symbol_parent_symtab(s);
        for (SymbolTable *t = scope; t; t = t->parent) {
            if (t == home) return true;
            if (t == function_scope) break;
        }
        return false;
    }

    struct Match {
        ResidueForm form;
        int64_t k;
        GpuAffine r;
    };

    // A counted loop: a DO loop, or a DO CONCURRENT loop with one index.
    struct Loop {
        ASR::stmt_t *stmt;
        const ASR::do_loop_head_t *head;
        ASR::stmt_t **body;
        size_t n;
        // Set for a DO CONCURRENT loop, whose clauses the loops made from
        // it keep.
        ASR::DoConcurrentLoop_t *concurrent;
    };

    bool attempt(SymbolTable *scope, const Loop &x, bool inline_calls,
            Vec<ASR::stmt_t*> &out,
            std::vector<std::pair<SymbolTable*, std::string>> &added);

public:
    ResidueUnroll(Allocator &al_, const PassOptions &pass_options_,
            ASR::TranslationUnit_t &unit)
        : al(al_), pass_options(pass_options_) {
        gpu_collect_address_taken(unit, address_taken);
    }

    void function(ASR::Function_t *fn) {
        function_scope = fn->m_symtab;
        function_body = fn->m_body;
        function_n = fn->n_body;
        list(fn->m_symtab, fn->m_body, fn->n_body);
    }

    void list(SymbolTable *scope, ASR::stmt_t **&body, size_t &n);
    bool transform(SymbolTable *scope, const Loop &x,
            Vec<ASR::stmt_t*> &out);
};

void ResidueUnroll::list(SymbolTable *scope, ASR::stmt_t **&body,
        size_t &n) {
    Vec<ASR::stmt_t*> out;
    out.reserve(al, n);
    bool changed = false;
    for (size_t i = 0; i < n; i++) {
        ASR::stmt_t *s = body[i];
        switch (s->type) {
            case ASR::stmtType::DoLoop: {
                ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(s);
                int64_t step = 1;
                if (x->m_head.m_increment
                        && !constant_integer(x->m_head.m_increment, step)) {
                    step = 0;
                }
                enclosing.push_back({x->m_head.m_v, x->m_head.m_start,
                    x->m_head.m_end, step, x->m_body, x->n_body});
                list(scope, x->m_body, x->n_body);
                enclosing.pop_back();
                if (x->n_orelse == 0 && transform(scope,
                        {s, &x->m_head, x->m_body, x->n_body, nullptr},
                        out)) {
                    changed = true;
                    continue;
                }
                break;
            }
            case ASR::stmtType::DoConcurrentLoop: {
                ASR::DoConcurrentLoop_t *x =
                    ASR::down_cast<ASR::DoConcurrentLoop_t>(s);
                bool one = x->n_head == 1;
                if (one) {
                    int64_t step = 1;
                    if (x->m_head[0].m_increment && !constant_integer(
                            x->m_head[0].m_increment, step)) {
                        step = 0;
                    }
                    enclosing.push_back({x->m_head[0].m_v,
                        x->m_head[0].m_start, x->m_head[0].m_end, step,
                        x->m_body, x->n_body});
                }
                list(scope, x->m_body, x->n_body);
                if (one) enclosing.pop_back();
                if (one && x->n_reduction == 0 && transform(scope,
                        {s, &x->m_head[0], x->m_body, x->n_body, x}, out)) {
                    changed = true;
                    continue;
                }
                break;
            }
            case ASR::stmtType::If: {
                ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                list(scope, x->m_body, x->n_body);
                list(scope, x->m_orelse, x->n_orelse);
                break;
            }
            case ASR::stmtType::WhileLoop: {
                ASR::WhileLoop_t *x = ASR::down_cast<ASR::WhileLoop_t>(s);
                list(scope, x->m_body, x->n_body);
                break;
            }
            case ASR::stmtType::BlockCall: {
                ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                if (ASR::is_a<ASR::Block_t>(*b)) {
                    ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                    list(blk->m_symtab, blk->m_body, blk->n_body);
                }
                break;
            }
            default:
                break;
        }
        out.push_back(al, s);
    }
    if (changed) {
        body = out.p;
        n = out.n;
    }
}

bool ResidueUnroll::transform(SymbolTable *scope, const Loop &x,
        Vec<ASR::stmt_t*> &out) {
    const ASR::do_loop_head_t &h = *x.head;
    if (!h.m_v || !h.m_start || !h.m_end || x.n == 0
            || !ASR::is_a<ASR::Var_t>(*h.m_v) || !integer_kind_4(h.m_v)
            || !integer_kind_4(h.m_start) || !integer_kind_4(h.m_end)) {
        return false;
    }
    int64_t step = 1;
    if (h.m_increment && (!constant_integer(h.m_increment, step)
            || step != 1)) {
        return false;
    }
    for (bool inline_calls : {true, false}) {
        std::vector<std::pair<SymbolTable*, std::string>> added;
        size_t before = out.n;
        if (attempt(scope, x, inline_calls, out, added)) return true;
        out.n = before;
        for (auto it = added.rbegin(); it != added.rend(); ++it) {
            if (it->first->get_symbol(it->second)) {
                it->first->erase_symbol(it->second);
            }
        }
    }
    return false;
}

bool ResidueUnroll::attempt(SymbolTable *scope, const Loop &x,
        bool inline_calls, Vec<ASR::stmt_t*> &out,
        std::vector<std::pair<SymbolTable*, std::string>> &added) {
    const ASR::do_loop_head_t &h = *x.head;
    ASR::symbol_t *d = ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(h.m_v)->m_v);
    if (!ASR::is_a<ASR::Variable_t>(*d) || !local_to_function(d, scope)
            || gpu_may_be_aliased(d, address_taken)) {
        return false;
    }
    ASR::stmt_t **body = x.body;
    size_t n = x.n;
    if (leaves_iteration(body, n)) return false;
    GpuEffects effects;
    gpu_collect_effects(body, n, effects);
    if (effects.unknown || effects.scalars.count(d)
            || effects.bases.count(d)) {
        return false;
    }
    auto invariant = [&](ASR::symbol_t *s) {
        return !effects.scalars.count(s) && !effects.bases.count(s)
            && !gpu_may_be_aliased(s, address_taken);
    };
    GpuAffineBuilder builder;
    builder.invariant_variable = [&](ASR::symbol_t *s) {
        return s == d || invariant(s);
    };
    std::string dkey = gpu_affine_key(h.m_v);

    // The quotients and residues of `d + r` by a constant, grouped by
    // divisor and numerator.
    std::vector<Match> matches;
    gpu_replace_exprs(body, n, [&](ASR::expr_t *e) -> ASR::expr_t* {
        ASR::expr_t *num = nullptr;
        int64_t k = 0;
        ResidueForm form = residue_form(e, num, k);
        if (form == ResidueForm::None) return nullptr;
        GpuAffine a;
        if (!builder.affine(num, a) || a.coefficient(dkey) != 1) {
            return nullptr;
        }
        a.terms.erase(dkey);
        matches.push_back({form, k, a});
        return nullptr;
    });
    int64_t k = 0;
    GpuAffine r;
    size_t best = 0;
    for (const Match &m : matches) {
        if (m.k > residue_unroll_max_factor) continue;
        size_t count = 0;
        for (const Match &o : matches) {
            if (o.k == m.k && o.r == m.r) count++;
        }
        if (count > best || (count == best && m.k < k)) {
            best = count;
            k = m.k;
            r = m.r;
        }
    }
    if (best == 0) return false;
    if (gpu_stmts_cost(body, n) * k > residue_unroll_max_cost) return false;

    // Facts: the bounds of the loops around this one, whose atoms keep
    // their values in those loops.
    GpuConstraints facts;
    for (const Enclosing &l : enclosing) {
        if (l.step == 0 || l.v == nullptr || l.start == nullptr
                || l.end == nullptr) {
            continue;
        }
        GpuEffects outer;
        gpu_collect_effects(l.body, l.n, outer);
        if (outer.unknown) continue;
        std::set<ASR::symbol_t*> syms;
        gpu_collect_symbols(l.start, syms);
        gpu_collect_symbols(l.end, syms);
        bool stable = true;
        for (ASR::symbol_t *s : syms) {
            if (outer.scalars.count(s) || outer.bases.count(s)) {
                stable = false;
            }
        }
        GpuAffine v, start, end;
        if (!stable || !builder.affine(l.v, v)
                || !builder.affine(l.start, start)
                || !builder.affine(l.end, end)) {
            continue;
        }
        if (l.step > 0) {
            facts.push_back(v.plus(start, -1));
            facts.push_back(end.plus(v, -1));
        } else {
            facts.push_back(start.plus(v, -1));
            facts.push_back(v.plus(end, -1));
        }
    }
    GpuAffine a_start, a_end;
    bool start_affine = builder.affine(h.m_start, a_start);
    bool end_affine = builder.affine(h.m_end, a_end);
    auto divisible = [&](const GpuAffine &a) {
        if (a.constant % k != 0) return false;
        for (auto &t : a.terms) if (t.second % k != 0) return false;
        return true;
    };
    bool nonnegative = false, long_enough = false, aligned = false,
        whole_groups = false;
    if (start_affine) {
        GpuAffine u = a_start.plus(r);
        nonnegative = gpu_affine_proves(facts, u);
        aligned = divisible(u);
        if (end_affine) {
            GpuAffine trips = a_end.plus(a_start, -1);
            long_enough = gpu_affine_proves(facts,
                trips.plus(GpuAffine(-(k - 1))));
            whole_groups = divisible(trips.plus(GpuAffine(1)));
        }
    }

    Location loc = x.stmt->base.loc;
    ASRUtils::ASRBuilder b(al, loc);
    ASR::ttype_t *type = ASRUtils::expr_type(h.m_v);
    SymbolTable *home = ASRUtils::symbol_parent_symtab(d);
    auto new_var = [&](const std::string &base, ASR::ttype_t *t) {
        std::string name = home->get_unique_name(base, false);
        ASR::expr_t *v = b.Variable(home, name,
            ASRUtils::duplicate_type(al, t), ASR::intentType::Local);
        added.push_back({home, name});
        return v;
    };
    auto constant = [&](int64_t v) { return b.i_t(v, type); };
    ASRUtils::ExprStmtDuplicator copier(al);

    Vec<ASR::stmt_t*> pre;
    pre.reserve(al, 2);
    // A bound read again after the body ran is captured unless nothing the
    // body does can change it.
    std::set<std::string> written_arrays = effects.arrays;
    auto bound = [&](ASR::expr_t *e, const std::string &name)
            -> std::function<ASR::expr_t*()> {
        bool stable = gpu_expr_duplicable(e);
        std::set<ASR::symbol_t*> syms;
        gpu_collect_value_symbols(e, syms);
        for (ASR::symbol_t *s : syms) if (!invariant(s)) stable = false;
        std::set<std::string> reads;
        gpu_collect_array_reads(e, reads);
        for (const std::string &a : reads) {
            if (gpu_key_touched(written_arrays, a)) stable = false;
        }
        if (stable) {
            return [&copier, e]() { return copier.duplicate_expr(e); };
        }
        ASR::expr_t *v = new_var(name, type);
        pre.push_back(al, b.Assignment(v, copier.duplicate_expr(e)));
        return [&copier, v]() { return copier.duplicate_expr(v); };
    };
    std::function<ASR::expr_t*()> start = bound(h.m_start,
        "__gpu_residue_start");
    std::function<ASR::expr_t*()> end = bound(h.m_end, "__gpu_residue_end");
    ASR::expr_t *r_expr = gpu_affine_to_expr(al, loc, r, builder.atoms);
    if (r_expr == nullptr) return false;
    std::set<ASR::symbol_t*> r_symbols;
    gpu_collect_symbols(r_expr, r_symbols);

    // The first index of the groups.
    ASR::expr_t *group_start = nullptr;
    Vec<ASR::stmt_t*> then;
    then.reserve(al, 6);
    if (aligned) {
        group_start = start();
    } else {
        // a + mod(K - mod(a + r, K), K), where a + r >= 0, so that the
        // truncating division gives the remainders.
        auto mod_k = [&](const std::function<ASR::expr_t*()> &e) {
            return b.Sub(e(), b.Mul(b.Div(e(), constant(k)), constant(k)));
        };
        auto numerator = [&]() {
            return b.Add(start(), copier.duplicate_expr(r_expr));
        };
        ASR::expr_t *peel = mod_k([&]() {
            return b.Sub(constant(k), mod_k(numerator));
        });
        ASR::expr_t *first = new_var("__gpu_residue_first", type);
        then.push_back(al, b.Assignment(first, b.Add(start(), peel)));
        group_start = copier.duplicate_expr(first);
    }

    // The copies of the body, iteration G + j with constant residues.
    ASR::expr_t *g = new_var("__gpu_residue_group", type);
    ASR::symbol_t *g_sym = ASR::down_cast<ASR::Var_t>(g)->m_v;
    std::map<std::string, ASR::expr_t*> atoms = builder.atoms;
    std::string gkey = gpu_affine_key(g);
    atoms[gkey] = g;
    GpuAffine base = r.plus(GpuAffine::atom(gkey));
    std::vector<Vec<ASR::stmt_t*>> copies;
    for (int64_t j = 0; j < k; j++) {
        GpuDuplicator dup(al, scope);
        dup.added = &added;
        dup.value_map[d] = j == 0 ? g : b.Add(g, constant(j));
        Vec<ASR::stmt_t*> copy;
        dup.duplicate_body(body, n, copy);
        if (!dup.ok) return false;
        GpuAffineBuilder copy_builder;
        copy_builder.invariant_variable = [&](ASR::symbol_t *s) {
            return s == g_sym || r_symbols.count(s) > 0;
        };
        GpuAffine expected = base.plus(GpuAffine(j));
        bool ok = true;
        gpu_replace_exprs(copy.p, copy.n, [&](ASR::expr_t *e)
                -> ASR::expr_t* {
            ASR::expr_t *num = nullptr;
            int64_t kk = 0;
            ResidueForm form = residue_form(e, num, kk);
            if (form == ResidueForm::None || kk != k) return nullptr;
            GpuAffine a;
            if (!copy_builder.affine(num, a) || !(a == expected)) {
                return nullptr;
            }
            if (form == ResidueForm::Residue) {
                return b.i_t(j, ASRUtils::expr_type(e));
            }
            ASR::expr_t *u = gpu_affine_to_expr(al, loc, base, atoms);
            if (u == nullptr) {
                ok = false;
                return nullptr;
            }
            return b.Div(u, b.i_t(k, ASRUtils::expr_type(e)));
        });
        if (!ok || mentions(copy.p, copy.n, d)) return false;
        if (inline_calls) {
            std::set<ASR::FunctionCall_t*> movable;
            collect_movable_calls(copy.p, copy.n, movable);
            GpuInliner inl(al, [&](ASR::FunctionCall_t *call) {
                if (!movable.count(call)) return false;
                ASR::symbol_t *f = ASRUtils::symbol_get_past_external(
                    call->m_name);
                if (!ASR::is_a<ASR::Function_t>(*f)) return false;
                ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(f);
                if (!gpu_function_inlinable(fn, true) || !function_stops(fn)
                        || gpu_stmts_cost(fn->m_body, fn->n_body)
                            > residue_inline_max_cost) {
                    return false;
                }
                for (size_t i = 0; i < call->n_args; i++) {
                    int64_t v;
                    if (call->m_args[i].m_value
                            && fold_integer(call->m_args[i].m_value, v)) {
                        return true;
                    }
                }
                return false;
            }, gpu_device_capabilities(pass_options).offset_kind);
            inl.allow_stops = true;
            ASR::stmt_t **cp = copy.p;
            size_t cn = copy.n;
            bool inlined = inl.run(scope, cp, cn);
            added.insert(added.end(), inl.added.begin(), inl.added.end());
            if (!inlined) return false;
            fold_branches(al, cp, cn);
            copy.p = cp;
            copy.n = cn;
            copy.max = cn;
        }
        copies.push_back(copy);
    }

    // Elements of arrays the body does not define, read at the same
    // subscripts by the copies, are read once per group.
    Vec<ASR::stmt_t*> group;
    group.reserve(al, 4);
    {
        std::vector<ASR::ArrayItem_t*> items;
        bool stopped = false;
        collect_evaluated_elements(copies[0].p, copies[0].n, items, stopped);
        std::set<std::string> others;
        for (size_t j = 1; j < copies.size(); j++) {
            collect_elements(copies[j].p, copies[j].n, others);
        }
        std::map<std::string, ASR::expr_t*> hoisted;
        for (ASR::ArrayItem_t *item : items) {
            std::string key = element_key(item);
            if (key.empty() || hoisted.count(key) || !others.count(key)) {
                continue;
            }
            ASR::symbol_t *array = designator_base(item->m_v);
            std::string array_key = gpu_array_key(item->m_v);
            if (array == nullptr || gpu_may_be_aliased(array, address_taken)
                    || gpu_key_touched(effects.arrays, array_key)
                    || ASRUtils::is_array(item->m_type)) {
                continue;
            }
            bool stable = true;
            bool per_group = false;
            for (size_t i = 0; i < item->n_args; i++) {
                ASR::expr_t *sub = item->m_args[i].m_right;
                if (!gpu_expr_duplicable(sub)) stable = false;
                std::set<ASR::symbol_t*> syms;
                gpu_collect_value_symbols(sub, syms);
                for (ASR::symbol_t *s : syms) {
                    if (s == g_sym) {
                        per_group = true;
                    } else if (!invariant(s)) {
                        stable = false;
                    }
                }
            }
            if (!stable || !per_group) continue;
            ASR::expr_t *t = new_var("__gpu_residue_element", item->m_type);
            group.push_back(al, b.Assignment(t, copier.duplicate_expr(
                &item->base)));
            hoisted[key] = t;
        }
        if (!hoisted.empty()) {
            for (Vec<ASR::stmt_t*> &copy : copies) {
                gpu_replace_exprs(copy.p, copy.n, [&](ASR::expr_t *e)
                        -> ASR::expr_t* {
                    if (!ASR::is_a<ASR::ArrayItem_t>(*e)) return nullptr;
                    auto it = hoisted.find(element_key(
                        ASR::down_cast<ASR::ArrayItem_t>(e)));
                    if (it == hoisted.end()) return nullptr;
                    return copier.duplicate_expr(it->second);
                });
            }
        }
    }
    for (Vec<ASR::stmt_t*> &copy : copies) {
        for (size_t i = 0; i < copy.n; i++) group.push_back(al, copy[i]);
    }
    std::vector<ASR::stmt_t*> group_body(group.p, group.p + group.n);

    // A loop of the kind of the original one: a DO CONCURRENT loop keeps
    // its clauses.
    auto make_loop = [&](ASR::expr_t *v, ASR::expr_t *first,
            ASR::expr_t *last, const std::vector<ASR::stmt_t*> &stmts,
            ASR::expr_t *increment) -> ASR::stmt_t* {
        if (x.concurrent == nullptr) {
            return b.DoLoop(v, first, last, stmts, increment);
        }
        Vec<ASR::do_loop_head_t> heads;
        heads.reserve(al, 1);
        ASR::do_loop_head_t head;
        head.loc = loc;
        head.m_v = v;
        head.m_start = first;
        head.m_end = last;
        head.m_increment = increment;
        heads.push_back(al, head);
        auto copy_list = [&](ASR::expr_t **list, size_t count) {
            Vec<ASR::expr_t*> c;
            c.reserve(al, count);
            for (size_t i = 0; i < count; i++) {
                c.push_back(al, copier.duplicate_expr(list[i]));
            }
            return c;
        };
        Vec<ASR::expr_t*> shared = copy_list(x.concurrent->m_shared,
            x.concurrent->n_shared);
        Vec<ASR::expr_t*> local = copy_list(x.concurrent->m_local,
            x.concurrent->n_local);
        Vec<ASR::stmt_t*> loop_body;
        loop_body.reserve(al, stmts.size());
        for (ASR::stmt_t *st : stmts) loop_body.push_back(al, st);
        return ASRUtils::STMT(ASRUtils::make_DoConcurrentLoop_t_util(al, loc,
            heads.p, heads.n, shared.p, shared.n, local.p, local.n,
            nullptr, 0, loop_body.p, loop_body.n));
    };
    auto original_copy = [&]() -> std::vector<ASR::stmt_t*> {
        GpuDuplicator dup(al, scope);
        dup.added = &added;
        Vec<ASR::stmt_t*> c;
        dup.duplicate_body(body, n, c);
        if (!dup.ok) return {};
        return std::vector<ASR::stmt_t*>(c.p, c.p + c.n);
    };
    if (!aligned) {
        std::vector<ASR::stmt_t*> peel = original_copy();
        if (peel.empty()) return false;
        then.push_back(al, make_loop(copier.duplicate_expr(h.m_v), start(),
            b.Sub(copier.duplicate_expr(group_start), constant(1)), peel,
            nullptr));
    }
    then.push_back(al, make_loop(copier.duplicate_expr(g),
        copier.duplicate_expr(group_start), b.Sub(end(), constant(k - 1)),
        group_body, constant(k)));
    if (!(aligned && whole_groups)) {
        std::vector<ASR::stmt_t*> rest = original_copy();
        if (rest.empty()) return false;
        // After a DO loop its variable holds the first index the groups
        // did not run; a DO CONCURRENT index is undefined after the loop,
        // so the index is a' + K*((b - a' + 1)/K), where b - a' + 1 >= 0.
        ASR::expr_t *rest_start = x.concurrent == nullptr
            ? copier.duplicate_expr(g)
            : b.Add(copier.duplicate_expr(group_start), b.Mul(b.Div(
                b.Add(b.Sub(end(), copier.duplicate_expr(group_start)),
                    constant(1)), constant(k)), constant(k)));
        then.push_back(al, make_loop(copier.duplicate_expr(h.m_v),
            rest_start, end(), rest, nullptr));
    } else if (x.concurrent == nullptr
            && named_outside_its_loops(function_body, function_n, d)) {
        // The value the loop leaves in its variable.
        then.push_back(al, b.Assignment(copier.duplicate_expr(h.m_v),
            b.Add(end(), constant(1))));
    }

    for (size_t i = 0; i < pre.n; i++) out.push_back(al, pre[i]);
    ASR::expr_t *test = nullptr;
    if (!nonnegative) {
        test = b.GtE(b.Add(start(), copier.duplicate_expr(r_expr)),
            constant(0));
    }
    if (!long_enough) {
        ASR::expr_t *t = b.GtE(b.Sub(end(), start()), constant(k - 1));
        test = test ? b.And(test, t) : t;
    }
    if (test == nullptr) {
        for (size_t i = 0; i < then.n; i++) out.push_back(al, then[i]);
    } else {
        std::vector<ASR::stmt_t*> then_body(then.p, then.p + then.n);
        out.push_back(al, b.If(test, then_body, {x.stmt}));
    }
    return true;
}

} // namespace

void pass_gpu_residue_unroll(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options) {
    if (gpu_device_selected(pass_options) == GpuDevice::None) return;
    ResidueUnroll unroll(al, pass_options, unit);
    std::function<void(SymbolTable*)> visit = [&](SymbolTable *scope) {
        for (auto &entry : scope->get_scope()) {
            if (ASR::is_a<ASR::Function_t>(*entry.second)) {
                auto *fn = ASR::down_cast<ASR::Function_t>(entry.second);
                if (ASRUtils::is_device_only_function(*fn)) {
                    unroll.function(fn);
                }
                visit(fn->m_symtab);
            } else if (ASR::is_a<ASR::Module_t>(*entry.second)) {
                visit(ASR::down_cast<ASR::Module_t>(entry.second)->m_symtab);
            } else if (ASR::is_a<ASR::Program_t>(*entry.second)) {
                visit(ASR::down_cast<ASR::Program_t>(entry.second)->m_symtab);
            }
        }
    };
    visit(unit.m_symtab);
    PassUtils::UpdateDependenciesVisitor update(al);
    update.visit_TranslationUnit(unit);
}

} // namespace LCompilers
