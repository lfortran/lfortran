#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/access_paths.h>
#include <libasr/pass/loop_fusion.h>
#include <libasr/pass/pass_utils.h>

#include <algorithm>
#include <cstdint>
#include <functional>
#include <map>
#include <set>
#include <string>
#include <vector>

namespace LCompilers {

/*
Fusion of adjacent DO loop nests over the same iteration space.

The lowering of array expressions gives every array statement a loop nest of
its own. Consecutive statements over the same shape, as in

    delta = maxval(abs(xnew(1:n,1:m) - x(1:n,1:m)))
    x(1:n,1:m) = xnew(1:n,1:m)

then sweep the same arrays once per statement:

    acc = -huge(acc)
    do j = 1, m
        do i = 1, n
            e = abs(xnew(i,j) - x(i,j))
            acc = merge(acc, e, acc > e)
        end do
    end do
    delta = acc
    do j2 = 1, m
        do i2 = 1, n
            x(i2,j2) = xnew(i2,j2)
        end do
    end do

This pass runs the two bodies in one nest, so that each element is loaded
once (the scalar statements in between, `delta = acc`, run after it):

    do j = 1, m
        do i = 1, n
            e = abs(xnew(i,j) - x(i,j))
            acc = merge(acc, e, acc > e)
            x(i,j) = xnew(i,j)
        end do
    end do
    delta = acc

First, the auxiliary index variables of the lowering are written in terms of
the loop variable: in

    k = c
    do i = s, e
        ... k ...
        k = k + 1
    end do

each `k` in the body is `i + (c - s)` (`i` when `c` and `s` are the same
expression or the same constant), when `k` is a local of the procedure
referenced nowhere else and the body is one the fusion can analyze (below),
which then neither assigns `k` nor leaves an iteration early.

Two DO loops of one statement list, the second after the first and only
scalar assignments between them (the statements M), are fused when:

* Both loops have no construct name and the same start, end and step (made
  of integer constants, variables and integer operations), or constant
  bounds of unit step with as many iterations (`do i = 1, 4` and `do k = 2,
  5`), and their variables have the same type. Going inwards, while both
  bodies are one DO loop of such bounds as well, those loops are fused too;
  the variables of the second nest at the fused levels are replaced by those
  of the first, shifted by the difference of the starts (`k` by `i + 1`).
  Such a variable of the second nest is either the one of the first (with
  the same bounds) or a local of the procedure referenced nowhere outside
  the second nest, and the second nest does not reference the variable of
  the first.
* The bodies contain only assignments of intrinsic scalars (to variables or
  array elements), IF statements and DO loops, and their expressions only
  variables, array elements, constants, intrinsic operations, IF
  expressions and elemental intrinsic functions: no procedure is called. No
  variable is a pointer, volatile or asynchronous, or EQUIVALENCE-associated
  (through a common block): with no pointer and no call in the bodies, two
  different arrays are different storage.
* Iteration `I` of the second nest runs after iteration `I` of the first one
  and before the iterations after it. So nothing the second nest accesses
  may depend on a later iteration of the first nest, and vice versa: a
  scalar assigned by one nest is not referenced by the other one; for an
  array assigned by one nest and referenced by the other one, every
  reference in both nests has the same subscripts, each of which is an
  invariant or a fused loop variable plus a constant, every fused loop
  variable appearing (so that only the same iteration of both nests
  accesses the same element).
* The bounds reference nothing that the nests or M assign, and no fused
  loop variable is assigned. M assigns nothing the second nest references,
  and references nothing the second nest assigns, so it can run after it.
*/

namespace {

using SymbolMap = std::map<ASR::symbol_t*, ASR::symbol_t*>;
// For a loop variable of the second nest replaced by the one of the first,
// the difference of their values in the same iteration.
using Shifts = std::map<ASR::symbol_t*, int64_t>;
using UseCounts = std::map<ASR::symbol_t*, int64_t>;

ASR::symbol_t *mapped(ASR::symbol_t *s, const SymbolMap &map) {
    auto it = map.find(s);
    return it == map.end() ? s : it->second;
}

class VarUses : public ASR::BaseWalkVisitor<VarUses> {
public:
    UseCounts &uses;
    explicit VarUses(UseCounts &uses_) : uses(uses_) {}
    void visit_Var(const ASR::Var_t &x) {
        uses[ASRUtils::symbol_get_past_external(x.m_v)]++;
    }
};

int64_t uses_in(ASR::stmt_t *s, ASR::symbol_t *symbol) {
    UseCounts uses;
    VarUses v(uses);
    v.visit_stmt(*s);
    return uses[symbol];
}

int64_t uses_in(ASR::expr_t *e, ASR::symbol_t *symbol) {
    UseCounts uses;
    VarUses v(uses);
    v.visit_expr(*e);
    return uses[symbol];
}

int64_t uses_in(ASR::stmt_t **body, size_t n, ASR::symbol_t *symbol) {
    int64_t count = 0;
    for (size_t i = 0; i < n; i++) count += uses_in(body[i], symbol);
    return count;
}

ASR::Variable_t *integer_scalar_variable(ASR::expr_t *e) {
    if (e == nullptr || !ASR::is_a<ASR::Var_t>(*e)) return nullptr;
    ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(e)->m_v);
    if (!ASR::is_a<ASR::Variable_t>(*s)) return nullptr;
    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
    if (!ASR::is_a<ASR::Integer_t>(*v->m_type)) return nullptr;
    return v;
}

ASR::symbol_t *var_symbol(ASR::expr_t *e) {
    return ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(e)->m_v);
}

// A structural key of an expression made of variables, integer constants
// and integer operations, with the variables of `map` replaced; "" for any
// other expression, and for one with a variable that `shifts` shifts.
std::string expr_key(ASR::expr_t *e, const SymbolMap &map,
        const Shifts &shifts = Shifts()) {
    if (e == nullptr) return "";
    switch (e->type) {
        case ASR::exprType::Var: {
            auto shift = shifts.find(var_symbol(e));
            if (shift != shifts.end() && shift->second != 0) return "";
            ASR::symbol_t *s = mapped(var_symbol(e), map);
            if (!ASR::is_a<ASR::Variable_t>(*s)) return "";
            return "v" + std::to_string(reinterpret_cast<uintptr_t>(s));
        }
        case ASR::exprType::IntegerConstant: {
            ASR::IntegerConstant_t *c = ASR::down_cast<ASR::IntegerConstant_t>(e);
            return "c" + std::to_string(c->m_n) + "_"
                + std::to_string(ASRUtils::extract_kind_from_ttype_t(c->m_type));
        }
        case ASR::exprType::IntegerBinOp: {
            ASR::IntegerBinOp_t *b = ASR::down_cast<ASR::IntegerBinOp_t>(e);
            std::string l = expr_key(b->m_left, map, shifts);
            std::string r = expr_key(b->m_right, map, shifts);
            if (l.empty() || r.empty()) return "";
            return "(" + l + " " + std::to_string(static_cast<int>(b->m_op))
                + "_" + std::to_string(ASRUtils::extract_kind_from_ttype_t(b->m_type))
                + " " + r + ")";
        }
        case ASR::exprType::IntegerUnaryMinus: {
            ASR::IntegerUnaryMinus_t *u = ASR::down_cast<ASR::IntegerUnaryMinus_t>(e);
            std::string a = expr_key(u->m_arg, map, shifts);
            if (a.empty()) return "";
            return "(-" + a + ")";
        }
        default: return "";
    }
}

bool same_key(ASR::expr_t *a, ASR::expr_t *b, const SymbolMap &map,
        const Shifts &shifts = Shifts()) {
    std::string ka = expr_key(a, map, shifts);
    return !ka.empty() && ka == expr_key(b, map, shifts);
}

bool intrinsic_element_type(ASR::ttype_t *t) {
    t = ASRUtils::type_get_past_array(
        ASRUtils::type_get_past_allocatable_pointer(t));
    return ASR::is_a<ASR::Real_t>(*t) || ASR::is_a<ASR::Integer_t>(*t)
        || ASR::is_a<ASR::Complex_t>(*t) || ASR::is_a<ASR::Logical_t>(*t)
        || ASR::is_a<ASR::UnsignedInteger_t>(*t);
}

// Whether a variable can be analyzed by name: it is not reached through
// a pointer or another name, and no other agent accesses it.
bool analyzable_variable(ASR::symbol_t *s, SymbolTable *scope) {
    if (!ASR::is_a<ASR::Variable_t>(*s)) return false;
    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
    if (ASRUtils::is_pointer(v->m_type) || v->m_is_volatile
            || v->m_is_asynchronous || v->n_codims > 0
            || ASRUtils::is_assumed_rank_array(v->m_type)
            || !intrinsic_element_type(v->m_type)) {
        return false;
    }
    if (!ASRUtils::is_array(v->m_type) && ASRUtils::is_allocatable(v->m_type)) {
        return false;
    }
    if (v->m_common_block != nullptr) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(v->m_common_block);
        if (!ASR::is_a<ASR::CommonBlock_t>(*b)
                || ASR::down_cast<ASR::CommonBlock_t>(b)->n_associations > 0) {
            return false;
        }
    }
    for (SymbolTable *t = scope; t != nullptr; t = t->parent) {
        if (ASRUtils::has_scoped_variable_attribute(*v, t, false)) return false;
    }
    return true;
}

struct Access {
    bool read = false;
    bool written = false;
    bool array = false;
    // A reference whose subscripts are not of the forms the dependence
    // test knows.
    bool unknown_subscripts = false;
    // The subscripts of each reference, as a key, with the fused levels
    // whose variable they contain.
    std::map<std::string, uint64_t> subscripts;
};

using AccessMap = std::map<ASR::symbol_t*, Access>;

// Collects what statements and expressions access, with the variables of
// `map` replaced. `ok` is cleared by anything the fusion cannot analyze.
class Collector {
public:
    const SymbolMap &map;
    const Shifts &shifts;
    // The fused loop variables (of the first nest), by level.
    const std::vector<ASR::symbol_t*> &fused;
    SymbolTable *scope;
    AccessMap accesses;
    bool ok = true;

    Collector(const SymbolMap &map_, const Shifts &shifts_,
            const std::vector<ASR::symbol_t*> &fused_, SymbolTable *scope_)
        : map(map_), shifts(shifts_), fused(fused_), scope(scope_) {}

    void stmts(ASR::stmt_t **body, size_t n) {
        for (size_t i = 0; i < n && ok; i++) stmt(body[i]);
    }

    void stmt(ASR::stmt_t *s) {
        switch (s->type) {
            case ASR::stmtType::Assignment: {
                ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(s);
                if (a->m_overloaded || a->m_move_allocation
                        || ASRUtils::is_array(ASRUtils::expr_type(a->m_target))
                        || !intrinsic_element_type(ASRUtils::expr_type(a->m_target))) {
                    ok = false;
                    return;
                }
                reference(a->m_target, true);
                expr(a->m_value);
                return;
            }
            case ASR::stmtType::If: {
                ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                expr(x->m_test);
                stmts(x->m_body, x->n_body);
                stmts(x->m_orelse, x->n_orelse);
                return;
            }
            case ASR::stmtType::DoLoop: {
                ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(s);
                if (x->n_orelse > 0 || !integer_scalar_variable(x->m_head.m_v)
                        || !x->m_head.m_start || !x->m_head.m_end) {
                    ok = false;
                    return;
                }
                reference(x->m_head.m_v, true);
                expr(x->m_head.m_start);
                expr(x->m_head.m_end);
                if (x->m_head.m_increment) expr(x->m_head.m_increment);
                stmts(x->m_body, x->n_body);
                return;
            }
            default: {
                ok = false;
                return;
            }
        }
    }

    void expr(ASR::expr_t *e) {
        if (!ok) return;
        if (e == nullptr || ASRUtils::is_array(ASRUtils::expr_type(e))) {
            ok = false;
            return;
        }
        switch (e->type) {
            case ASR::exprType::Var:
            case ASR::exprType::ArrayItem: {
                reference(e, false);
                return;
            }
            case ASR::exprType::IntegerConstant:
            case ASR::exprType::UnsignedIntegerConstant:
            case ASR::exprType::RealConstant:
            case ASR::exprType::ComplexConstant:
            case ASR::exprType::LogicalConstant: return;
            case ASR::exprType::IntegerBinOp: {
                auto *x = ASR::down_cast<ASR::IntegerBinOp_t>(e);
                expr(x->m_left); expr(x->m_right); return;
            }
            case ASR::exprType::RealBinOp: {
                auto *x = ASR::down_cast<ASR::RealBinOp_t>(e);
                expr(x->m_left); expr(x->m_right); return;
            }
            case ASR::exprType::ComplexBinOp: {
                auto *x = ASR::down_cast<ASR::ComplexBinOp_t>(e);
                expr(x->m_left); expr(x->m_right); return;
            }
            case ASR::exprType::LogicalBinOp: {
                auto *x = ASR::down_cast<ASR::LogicalBinOp_t>(e);
                expr(x->m_left); expr(x->m_right); return;
            }
            case ASR::exprType::IntegerCompare: {
                auto *x = ASR::down_cast<ASR::IntegerCompare_t>(e);
                expr(x->m_left); expr(x->m_right); return;
            }
            case ASR::exprType::RealCompare: {
                auto *x = ASR::down_cast<ASR::RealCompare_t>(e);
                expr(x->m_left); expr(x->m_right); return;
            }
            case ASR::exprType::IntegerUnaryMinus: {
                expr(ASR::down_cast<ASR::IntegerUnaryMinus_t>(e)->m_arg); return;
            }
            case ASR::exprType::RealUnaryMinus: {
                expr(ASR::down_cast<ASR::RealUnaryMinus_t>(e)->m_arg); return;
            }
            case ASR::exprType::LogicalNot: {
                expr(ASR::down_cast<ASR::LogicalNot_t>(e)->m_arg); return;
            }
            case ASR::exprType::Cast: {
                auto *x = ASR::down_cast<ASR::Cast_t>(e);
                if (x->m_dest) { ok = false; return; }
                expr(x->m_arg); return;
            }
            case ASR::exprType::IfExp: {
                auto *x = ASR::down_cast<ASR::IfExp_t>(e);
                expr(x->m_test); expr(x->m_body); expr(x->m_orelse); return;
            }
            case ASR::exprType::IntrinsicElementalFunction: {
                auto *x = ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e);
                for (size_t i = 0; i < x->n_args; i++) expr(x->m_args[i]);
                return;
            }
            default: {
                ok = false;
                return;
            }
        }
    }

private:
    // Whether `e` is a fused loop variable plus a constant: the level of
    // the variable and the constant (in terms of the variable of the first
    // nest).
    bool affine(ASR::expr_t *e, int &l, int64_t &offset) {
        if (ASR::is_a<ASR::Var_t>(*e)) {
            ASR::symbol_t *s = var_symbol(e);
            auto shift = shifts.find(s);
            if (shift != shifts.end()) offset += shift->second;
            s = mapped(s, map);
            for (size_t i = 0; i < fused.size(); i++) {
                if (fused[i] == s) {
                    l = static_cast<int>(i);
                    return true;
                }
            }
            return false;
        }
        if (!ASR::is_a<ASR::IntegerBinOp_t>(*e)) return false;
        ASR::IntegerBinOp_t *b = ASR::down_cast<ASR::IntegerBinOp_t>(e);
        if ((b->m_op == ASR::binopType::Add || b->m_op == ASR::binopType::Sub)
                && ASR::is_a<ASR::IntegerConstant_t>(*b->m_right)) {
            int64_t c = ASR::down_cast<ASR::IntegerConstant_t>(b->m_right)->m_n;
            offset += b->m_op == ASR::binopType::Sub ? -c : c;
            return affine(b->m_left, l, offset);
        }
        if (b->m_op == ASR::binopType::Add
                && ASR::is_a<ASR::IntegerConstant_t>(*b->m_left)) {
            offset += ASR::down_cast<ASR::IntegerConstant_t>(b->m_left)->m_n;
            return affine(b->m_right, l, offset);
        }
        return false;
    }

    // The key of a subscript that is a fused loop variable plus a constant
    // (adding its level to `levels`) or that contains no fused loop
    // variable; "" for any other subscript.
    std::string subscript_key(ASR::expr_t *e, uint64_t &levels) {
        int l = -1;
        int64_t offset = 0;
        if (affine(e, l, offset)) {
            levels |= uint64_t(1) << l;
            return "L" + std::to_string(l) + "+" + std::to_string(offset);
        }
        std::string key = expr_key(e, map, shifts);
        if (key.empty()) return "";
        for (ASR::symbol_t *f: fused) {
            if (key.find("v" + std::to_string(reinterpret_cast<uintptr_t>(f)))
                    != std::string::npos) {
                return "";
            }
        }
        return "I" + key;
    }

    void reference(ASR::expr_t *e, bool write) {
        if (!ok) return;
        if (ASR::is_a<ASR::Var_t>(*e)) {
            ASR::symbol_t *s = mapped(var_symbol(e), map);
            if (!analyzable_variable(s, scope)
                    || ASRUtils::is_array(ASRUtils::symbol_type(s))) {
                ok = false;
                return;
            }
            Access &a = accesses[s];
            (write ? a.written : a.read) = true;
            return;
        }
        if (!ASR::is_a<ASR::ArrayItem_t>(*e)) {
            ok = false;
            return;
        }
        ASR::ArrayItem_t *item = ASR::down_cast<ASR::ArrayItem_t>(e);
        ASR::expr_t *base = ASRUtils::get_past_array_physical_cast(item->m_v);
        if (!ASR::is_a<ASR::Var_t>(*base)) {
            ok = false;
            return;
        }
        ASR::symbol_t *s = mapped(var_symbol(base), map);
        if (!analyzable_variable(s, scope)
                || !ASRUtils::is_array(ASRUtils::symbol_type(s))) {
            ok = false;
            return;
        }
        std::string key;
        uint64_t levels = 0;
        bool known = true;
        for (size_t i = 0; i < item->n_args && ok; i++) {
            ASR::array_index_t &index = item->m_args[i];
            if (index.m_left || index.m_step || !index.m_right) {
                ok = false;
                return;
            }
            expr(index.m_right);
            std::string k = subscript_key(index.m_right, levels);
            if (k.empty()) known = false;
            key += k + ",";
        }
        Access &a = accesses[s];
        a.array = true;
        (write ? a.written : a.read) = true;
        if (known) {
            a.subscripts[key] |= levels;
        } else {
            a.unknown_subscripts = true;
        }
    }
};

// Replaces each reference to a variable of `symbols` by `make(symbol)`.
class VarReplacer : public ASR::BaseExprReplacer<VarReplacer> {
public:
    std::map<ASR::symbol_t*, std::function<ASR::expr_t*(const Location&)>> make;
    int64_t replaced = 0;
    void replace_Var(ASR::Var_t *x) {
        auto it = make.find(ASRUtils::symbol_get_past_external(x->m_v));
        if (it == make.end()) return;
        *current_expr = it->second(x->base.base.loc);
        replaced++;
    }
};

class ReplaceInStmts : public ASR::CallReplacerOnExpressionsVisitor<ReplaceInStmts> {
public:
    VarReplacer replacer;
    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }
    void stmts(ASR::stmt_t **body, size_t n) {
        for (size_t i = 0; i < n; i++) visit_stmt(*body[i]);
    }
};

bool unit_step(ASR::DoLoop_t *loop) {
    ASR::expr_t *inc = loop->m_head.m_increment;
    return inc == nullptr || (ASR::is_a<ASR::IntegerConstant_t>(*inc)
        && ASR::down_cast<ASR::IntegerConstant_t>(inc)->m_n == 1);
}

// `v = v + 1` for an integer variable `v`, or nullptr.
ASR::symbol_t *unit_increment(ASR::stmt_t *s) {
    if (!ASR::is_a<ASR::Assignment_t>(*s)) return nullptr;
    ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(s);
    if (a->m_overloaded || !integer_scalar_variable(a->m_target)
            || !ASR::is_a<ASR::IntegerBinOp_t>(*a->m_value)) {
        return nullptr;
    }
    ASR::IntegerBinOp_t *b = ASR::down_cast<ASR::IntegerBinOp_t>(a->m_value);
    ASR::symbol_t *v = var_symbol(a->m_target);
    if (b->m_op != ASR::binopType::Add || !ASR::is_a<ASR::Var_t>(*b->m_left)
            || var_symbol(b->m_left) != v
            || !ASR::is_a<ASR::IntegerConstant_t>(*b->m_right)
            || ASR::down_cast<ASR::IntegerConstant_t>(b->m_right)->m_n != 1) {
        return nullptr;
    }
    return v;
}

class LoopFusion : public ASR::ASRPassBaseWalkVisitor<LoopFusion> {
public:
    Allocator &al;
    UseCounts uses;
    SymbolTable *routine = nullptr;

    explicit LoopFusion(Allocator &al_) : al(al_) {}

    template <typename T>
    void visit_routine(const T &x) {
        UseCounts uses_copy = uses;
        SymbolTable *routine_copy = routine;
        uses.clear();
        VarUses counter(uses);
        counter.visit_symbol(const_cast<ASR::symbol_t&>(x.base));
        routine = x.m_symtab;
        if constexpr (std::is_same_v<T, ASR::Program_t>) {
            ASR::ASRPassBaseWalkVisitor<LoopFusion>::visit_Program(x);
        } else {
            ASR::ASRPassBaseWalkVisitor<LoopFusion>::visit_Function(x);
        }
        uses = uses_copy;
        routine = routine_copy;
    }

    void visit_Program(const ASR::Program_t &x) { visit_routine(x); }
    void visit_Function(const ASR::Function_t &x) { visit_routine(x); }

    void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
        for (size_t i = 0; i < n_body; i++) visit_stmt(*m_body[i]);
        if (routine == nullptr) return;
        std::vector<ASR::stmt_t*> body(m_body, m_body + n_body);
        canonicalize_induction_variables(body);
        fuse(body);
        if (body.size() != n_body) {
            Vec<ASR::stmt_t*> v;
            v.reserve(al, body.size());
            for (ASR::stmt_t *s: body) v.push_back(al, s);
            m_body = v.p;
            n_body = v.size();
        }
    }

private:
    // A local variable of the procedure being transformed, whose value
    // nothing outside it can see.
    bool routine_local(ASR::symbol_t *s) {
        if (!ASR::is_a<ASR::Variable_t>(*s)) return false;
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
        return v->m_intent == ASR::intentType::Local
            && v->m_storage != ASR::storage_typeType::Parameter
            && AccessPaths::enclosing_routine(v->m_parent_symtab) == routine;
    }

    bool same_type(ASR::symbol_t *a, ASR::symbol_t *b) {
        ASR::ttype_t *ta = ASRUtils::symbol_type(a);
        ASR::ttype_t *tb = ASRUtils::symbol_type(b);
        return ASR::is_a<ASR::Integer_t>(*ta) && ASR::is_a<ASR::Integer_t>(*tb)
            && ASRUtils::extract_kind_from_ttype_t(ta)
                == ASRUtils::extract_kind_from_ttype_t(tb);
    }

    void canonicalize_induction_variables(std::vector<ASR::stmt_t*> &body) {
        std::vector<bool> removed(body.size(), false);
        for (size_t p = 0; p < body.size(); p++) {
            if (!ASR::is_a<ASR::DoLoop_t>(*body[p])) continue;
            ASR::DoLoop_t *loop = ASR::down_cast<ASR::DoLoop_t>(body[p]);
            ASR::Variable_t *index = integer_scalar_variable(loop->m_head.m_v);
            if (loop->n_orelse > 0 || !index || !unit_step(loop)
                    || !loop->m_head.m_start || !loop->m_head.m_end) {
                continue;
            }
            ASR::symbol_t *i = var_symbol(loop->m_head.m_v);
            // The initializations right before the loop.
            std::map<ASR::symbol_t*, size_t> inits;
            for (size_t q = p; q-- > 0 && !removed[q];) {
                if (!ASR::is_a<ASR::Assignment_t>(*body[q])) break;
                ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(body[q]);
                if (a->m_overloaded || !integer_scalar_variable(a->m_target)) break;
                inits.emplace(var_symbol(a->m_target), q);
            }
            // The increments at the end of the body.
            size_t n_rest = loop->n_body;
            std::map<ASR::symbol_t*, ASR::expr_t*> offsets;
            std::vector<ASR::symbol_t*> candidates;
            while (n_rest > 0) {
                ASR::symbol_t *v = unit_increment(loop->m_body[n_rest - 1]);
                if (!v || inits.count(v) == 0
                        || std::find(candidates.begin(), candidates.end(), v)
                            != candidates.end()) {
                    break;
                }
                candidates.push_back(v);
                n_rest--;
            }
            if (candidates.empty()) continue;
            // The rest of the body must be analyzable, and assign none of
            // the candidates.
            SymbolMap no_map;
            Shifts no_shifts;
            std::vector<ASR::symbol_t*> no_fused;
            Collector body_accesses(no_map, no_shifts, no_fused, current_scope);
            body_accesses.stmts(loop->m_body, n_rest);
            if (!body_accesses.ok) continue;
            std::map<ASR::symbol_t*, int64_t> substitution;
            for (ASR::symbol_t *v: candidates) {
                ASR::expr_t *init = ASR::down_cast<ASR::Assignment_t>(
                    body[inits[v]])->m_value;
                ASR::expr_t *start = loop->m_head.m_start;
                if (!routine_local(v) || !same_type(v, i) || v == i
                        || body_accesses.accesses[v].written) {
                    continue;
                }
                // Nothing but the initialization and the loop may
                // reference `v`; the initial value and the start reference
                // none of the variables initialized in between.
                if (uses[v] != uses_in(body[inits[v]], v) + uses_in(body[p], v)
                        || uses_in(loop->m_head.m_start, v)
                        || uses_in(loop->m_head.m_end, v)) {
                    continue;
                }
                bool independent = true;
                for (auto &other: inits) {
                    if (other.second >= inits[v] && other.second < p
                            && (uses_in(init, other.first) > 0
                                || uses_in(start, other.first) > 0)) {
                        independent = false;
                    }
                }
                if (!independent) continue;
                int64_t offset;
                if (same_key(init, start, no_map)) {
                    offset = 0;
                } else if (ASR::is_a<ASR::IntegerConstant_t>(*init)
                        && ASR::is_a<ASR::IntegerConstant_t>(*start)) {
                    offset = ASR::down_cast<ASR::IntegerConstant_t>(init)->m_n
                        - ASR::down_cast<ASR::IntegerConstant_t>(start)->m_n;
                } else {
                    continue;
                }
                substitution[v] = offset;
            }
            if (substitution.empty()) continue;
            ReplaceInStmts replace;
            ASR::ttype_t *type = index->m_type;
            for (auto &s: substitution) {
                int64_t offset = s.second;
                replace.replacer.make[s.first] = [this, i, type, offset](const Location &loc) {
                    ASR::expr_t *var = ASRUtils::EXPR(ASR::make_Var_t(al, loc, i));
                    if (offset == 0) return var;
                    return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc, var,
                        ASR::binopType::Add, ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                            al, loc, offset, type)), type, nullptr));
                };
            }
            // Drop the increments of the substituted variables (they are
            // the last statements of the body, in any order).
            Vec<ASR::stmt_t*> new_body;
            new_body.reserve(al, loop->n_body);
            for (size_t k = 0; k < loop->n_body; k++) {
                if (k >= n_rest && substitution.count(unit_increment(loop->m_body[k]))) {
                    continue;
                }
                new_body.push_back(al, loop->m_body[k]);
            }
            loop->m_body = new_body.p;
            loop->n_body = new_body.size();
            replace.stmts(loop->m_body, loop->n_body);
            uses[i] += replace.replacer.replaced;
            for (auto &s: substitution) removed[inits[s.first]] = true;
        }
        std::vector<ASR::stmt_t*> kept;
        for (size_t p = 0; p < body.size(); p++) {
            if (!removed[p]) kept.push_back(body[p]);
        }
        body = kept;
    }

    // Whether the loops `a` and `b` run the same number of iterations,
    // the variable of `b` exceeding that of `a` by `shift` in each.
    bool fusable_heads(ASR::DoLoop_t *a, ASR::DoLoop_t *b, const SymbolMap &map,
            const Shifts &shifts, int64_t &shift) {
        if (a->m_name || b->m_name || a->n_orelse > 0 || b->n_orelse > 0
                || !integer_scalar_variable(a->m_head.m_v)
                || !integer_scalar_variable(b->m_head.m_v)
                || !same_type(var_symbol(a->m_head.m_v), var_symbol(b->m_head.m_v))
                || !a->m_head.m_start || !a->m_head.m_end
                || !b->m_head.m_start || !b->m_head.m_end) {
            return false;
        }
        shift = 0;
        if (same_key(a->m_head.m_start, b->m_head.m_start, map, shifts)
                && same_key(a->m_head.m_end, b->m_head.m_end, map, shifts)) {
            if (a->m_head.m_increment && b->m_head.m_increment) {
                return same_key(a->m_head.m_increment, b->m_head.m_increment,
                    map, shifts);
            }
            return unit_step(a) && unit_step(b);
        }
        // Constant bounds of unit step, with as many iterations.
        ASR::expr_t *bounds[4] = {a->m_head.m_start, a->m_head.m_end,
            b->m_head.m_start, b->m_head.m_end};
        int64_t value[4];
        for (size_t i = 0; i < 4; i++) {
            if (!ASR::is_a<ASR::IntegerConstant_t>(*bounds[i])) return false;
            value[i] = ASR::down_cast<ASR::IntegerConstant_t>(bounds[i])->m_n;
        }
        if (!unit_step(a) || !unit_step(b)
                || value[1] - value[0] != value[3] - value[2]) {
            return false;
        }
        shift = value[2] - value[0];
        return true;
    }

    // Whether `s` is a scalar assignment that may run after the second
    // nest.
    bool movable(ASR::stmt_t *s) {
        if (!ASR::is_a<ASR::Assignment_t>(*s)) return false;
        ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(s);
        return ASR::is_a<ASR::Var_t>(*a->m_target)
            && !ASRUtils::is_array(ASRUtils::expr_type(a->m_target));
    }

    // Fuses the nest `second` into the nest `first`, with the statements
    // `between` them, when it is legal (see the top of the file).
    bool try_fuse(ASR::DoLoop_t *first, const std::vector<ASR::stmt_t*> &between,
            ASR::DoLoop_t *second) {
        SymbolMap map;
        Shifts shifts;
        std::vector<ASR::symbol_t*> fused;
        std::vector<ASR::DoLoop_t*> n1, n2;
        ASR::DoLoop_t *a = first, *b = second;
        int64_t shift;
        if (!fusable_heads(a, b, map, shifts, shift)) return false;
        while (true) {
            ASR::symbol_t *va = var_symbol(a->m_head.m_v);
            ASR::symbol_t *vb = var_symbol(b->m_head.m_v);
            if (va != vb || shift != 0) {
                if (va == vb || !routine_local(vb)
                        || uses[vb] != uses_in(&second->base, vb)
                        || uses_in(&second->base, va) > 0) {
                    return false;
                }
                map[vb] = va;
                shifts[vb] = shift;
            }
            fused.push_back(va);
            n1.push_back(a);
            n2.push_back(b);
            if (fused.size() < 64 && a->n_body == 1 && b->n_body == 1
                    && ASR::is_a<ASR::DoLoop_t>(*a->m_body[0])
                    && ASR::is_a<ASR::DoLoop_t>(*b->m_body[0])
                    && fusable_heads(ASR::down_cast<ASR::DoLoop_t>(a->m_body[0]),
                        ASR::down_cast<ASR::DoLoop_t>(b->m_body[0]), map, shifts,
                        shift)) {
                a = ASR::down_cast<ASR::DoLoop_t>(a->m_body[0]);
                b = ASR::down_cast<ASR::DoLoop_t>(b->m_body[0]);
                continue;
            }
            break;
        }

        SymbolMap no_map;
        Shifts no_shifts;
        Collector c1(no_map, no_shifts, fused, current_scope);
        Collector c2(map, shifts, fused, current_scope);
        Collector cm(no_map, no_shifts, fused, current_scope);
        Collector bounds(no_map, no_shifts, fused, current_scope);
        for (ASR::DoLoop_t *loop: n1) {
            bounds.expr(loop->m_head.m_start);
            bounds.expr(loop->m_head.m_end);
            if (loop->m_head.m_increment) bounds.expr(loop->m_head.m_increment);
        }
        c1.stmts(a->m_body, a->n_body);
        c2.stmts(b->m_body, b->n_body);
        for (ASR::stmt_t *s: between) cm.stmt(s);
        if (!c1.ok || !c2.ok || !cm.ok || !bounds.ok) return false;

        auto written = [&](ASR::symbol_t *s) {
            return (c1.accesses.count(s) && c1.accesses[s].written)
                || (c2.accesses.count(s) && c2.accesses[s].written)
                || (cm.accesses.count(s) && cm.accesses[s].written);
        };
        for (auto &x: bounds.accesses) {
            if (written(x.first)) return false;
        }
        for (ASR::symbol_t *f: fused) {
            if (written(f)) return false;
        }
        uint64_t all_levels = fused.size() == 64 ? ~uint64_t(0)
            : (uint64_t(1) << fused.size()) - 1;
        for (auto &x: c1.accesses) {
            if (c2.accesses.count(x.first) == 0) continue;
            Access &a1 = x.second;
            Access &a2 = c2.accesses[x.first];
            if (!a1.written && !a2.written) continue;
            if (!a1.array || a1.unknown_subscripts || a2.unknown_subscripts) {
                return false;
            }
            std::map<std::string, uint64_t> all = a1.subscripts;
            for (auto &k: a2.subscripts) all[k.first] |= k.second;
            if (all.size() != 1 || all.begin()->second != all_levels) {
                return false;
            }
        }
        for (auto &x: cm.accesses) {
            if (c2.accesses.count(x.first) == 0) continue;
            if (x.second.written || c2.accesses[x.first].written) return false;
        }

        // Fuse: the body of the second nest, with the variables of the
        // first, after the body of the first.
        ReplaceInStmts replace;
        for (auto &m: map) {
            ASR::symbol_t *to = m.second;
            int64_t d = shifts[m.first];
            ASR::ttype_t *type = ASRUtils::symbol_type(to);
            replace.replacer.make[m.first] = [this, to, d, type](const Location &loc) {
                ASR::expr_t *var = ASRUtils::EXPR(ASR::make_Var_t(al, loc, to));
                if (d == 0) return var;
                return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc, var,
                    ASR::binopType::Add, ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                        al, loc, d, type)), type, nullptr));
            };
        }
        replace.stmts(b->m_body, b->n_body);
        for (auto &m: map) uses[m.second] += uses_in(b->m_body, b->n_body, m.second);
        Vec<ASR::stmt_t*> new_body;
        new_body.reserve(al, a->n_body + b->n_body);
        for (size_t k = 0; k < a->n_body; k++) new_body.push_back(al, a->m_body[k]);
        for (size_t k = 0; k < b->n_body; k++) new_body.push_back(al, b->m_body[k]);
        a->m_body = new_body.p;
        a->n_body = new_body.size();
        return true;
    }

    void fuse(std::vector<ASR::stmt_t*> &body) {
        for (size_t p = 0; p < body.size(); p++) {
            if (!ASR::is_a<ASR::DoLoop_t>(*body[p])) continue;
            bool fused = true;
            while (fused) {
                fused = false;
                size_t q = p + 1;
                while (q < body.size() && movable(body[q])) q++;
                if (q >= body.size() || !ASR::is_a<ASR::DoLoop_t>(*body[q])) break;
                std::vector<ASR::stmt_t*> between(body.begin() + p + 1,
                    body.begin() + q);
                if (try_fuse(ASR::down_cast<ASR::DoLoop_t>(body[p]), between,
                        ASR::down_cast<ASR::DoLoop_t>(body[q]))) {
                    // The statements in between now run after the nest.
                    body.erase(body.begin() + q);
                    fused = true;
                }
            }
        }
    }
};

} // namespace

void pass_loop_fusion(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &/*pass_options*/) {
    LoopFusion v(al);
    v.visit_TranslationUnit(unit);
}

} // namespace LCompilers
