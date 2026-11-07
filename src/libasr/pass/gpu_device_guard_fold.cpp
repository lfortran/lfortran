#include <algorithm>
#include <cstdint>
#include <limits>
#include <map>
#include <optional>
#include <set>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/gpu_device_guard_fold.h>
#include <libasr/pass/intrinsic_function_registry.h>

namespace LCompilers {

/*
Removes the branches of device code that cannot run, judged by the ranges
of the integers their tests compare, so that a check that can never fail
costs nothing on the device, and a kernel whose only `stop` is such a check
is not a kernel that can stop (gpu_offload_stop.h): its launches get no
status buffer, and the host need not wait for them before it returns.

The typical case is a small pure function that checks its argument,

    pure elemental integer function byte_of(word, part)
        integer, intent(in) :: word, part
        if (part < 0 .or. part > 3) error stop "part must be in 0..3"
        byte_of = ibits(word, 8*part, 8)
    end function

called in a parallel loop as `byte_of(w((k-1)/4+1), modulo(k-1, 4))`. The
offload pass gives each kernel copies of the procedures it calls, declared in
the kernel's symbol table, so every call of such a copy is in the kernel or
in another copy. When every call of a copy passes, for an `intent(in)`
integer dummy, a value the ranges below place in [lo, hi], the copy's tests
are judged with the dummy in [lo, hi]: here `modulo(k - 1, 4)` is in [0, 3],
the test `part < 0 .or. part > 3` is false, and the `if` is replaced by its
`else` branch (none).

The range of an integer expression is that of its kind, narrowed by
* constants and named constants;
* `intent(in)` dummies of a copy, as above, and the index of a counted loop
  inside its body, from the ranges of its limits;
* `+`, `-`, `*`, division by a constant, unary minus, conversion between
  integer kinds, `iand` and bit masks with a non-negative operand, right
  shifts of a non-negative value, `modulo` and `mod`, `ibits`, `min`, `max`.
A test is decided when the ranges of the integers it compares do, through
`.not.`, `.and.`, `.or.`, `.eqv.` and `.neqv.`, or else when it is decided
for each value of one variable it reads that has at most 16 of them (so
`half /= 0 .and. half /= 1` is false for `half` in [0, 1]). A value outside
the range of its kind would have overflowed, so the range of an operation
that may overflow is that of its kind. An `if` that has a name, or whose
removed branch holds the target of a GO TO, is kept; a BLOCK that more than
one statement runs is judged with what is known everywhere in the routine.

The copies are judged in the order of the calls: a copy is judged once the
kernel and every copy that calls it have been, so the ranges of its dummies
are those of the calls that remain. A copy that calls itself, directly or
not, or is named other than by calling it, is judged without them.

This runs before the intrinsic functions are lowered, while `modulo` is
still one, and before `gpu_device_heap` lowers the `stop` statements device
code reaches, so a check removed here is not a site. A test the ranges do not
decide is kept, with its `stop`.
*/

namespace {

struct IntRange {
    int64_t lo;
    int64_t hi;
};

using Facts = std::map<ASR::symbol_t*, IntRange>;

constexpr int64_t int64_min = std::numeric_limits<int64_t>::min();
constexpr int64_t int64_max = std::numeric_limits<int64_t>::max();

// The values of an integer of the type `t`, or nothing when `t` is not a
// scalar integer.
std::optional<IntRange> kind_range(ASR::ttype_t *t) {
    if (t == nullptr || !ASR::is_a<ASR::Integer_t>(*t)) return std::nullopt;
    switch (ASR::down_cast<ASR::Integer_t>(t)->m_kind) {
        case 1: return IntRange{-128, 127};
        case 2: return IntRange{-32768, 32767};
        case 4: return IntRange{std::numeric_limits<int32_t>::min(),
            std::numeric_limits<int32_t>::max()};
        case 8: return IntRange{int64_min, int64_max};
        default: return std::nullopt;
    }
}

bool add(int64_t a, int64_t b, int64_t &r) {
    if ((b > 0 && a > int64_max - b) || (b < 0 && a < int64_min - b)) {
        return false;
    }
    r = a + b;
    return true;
}

bool sub(int64_t a, int64_t b, int64_t &r) {
    if ((b < 0 && a > int64_max + b) || (b > 0 && a < int64_min + b)) {
        return false;
    }
    r = a - b;
    return true;
}

bool mul(int64_t a, int64_t b, int64_t &r) {
    if (a == 0 || b == 0) {
        r = 0;
        return true;
    }
    if (a > 0) {
        if (b > 0 ? a > int64_max / b : b < int64_min / a) return false;
    } else {
        if (b > 0 ? a < int64_min / b : b < int64_max / a) return false;
    }
    r = a * b;
    return true;
}

bool constant_integer(ASR::expr_t *e, int64_t &v) {
    ASR::expr_t *value = ASRUtils::expr_value(e);
    if (value == nullptr || !ASR::is_a<ASR::IntegerConstant_t>(*value)) {
        return false;
    }
    v = ASR::down_cast<ASR::IntegerConstant_t>(value)->m_n;
    return true;
}

// The range of the integer expressions of a routine, given what is known of
// its variables (`facts`).
class RangeEval {
public:
    explicit RangeEval(const Facts &facts_) : facts(facts_) {}

    // The values `e` can have, or nothing when it is not a scalar integer.
    std::optional<IntRange> range(ASR::expr_t *e) {
        std::optional<IntRange> full = kind_range(ASRUtils::expr_type(e));
        if (!full) return std::nullopt;
        std::optional<IntRange> r = narrow(e);
        if (!r || r->lo > r->hi || r->lo < full->lo || r->hi > full->hi) {
            // A value outside its kind overflowed.
            return full;
        }
        return r;
    }

    // Whether the logical `e` is always true or always false, or nothing
    // when the ranges do not decide it.
    std::optional<bool> decide(ASR::expr_t *e) {
        ASR::expr_t *value = ASRUtils::expr_value(e);
        if (value != nullptr && ASR::is_a<ASR::LogicalConstant_t>(*value)) {
            return ASR::down_cast<ASR::LogicalConstant_t>(value)->m_value;
        }
        switch (e->type) {
            case ASR::exprType::LogicalNot: {
                std::optional<bool> a = decide(
                    ASR::down_cast<ASR::LogicalNot_t>(e)->m_arg);
                if (!a) return std::nullopt;
                return !*a;
            }
            case ASR::exprType::LogicalBinOp: {
                ASR::LogicalBinOp_t *x = ASR::down_cast<ASR::LogicalBinOp_t>(e);
                std::optional<bool> l = decide(x->m_left);
                std::optional<bool> r = decide(x->m_right);
                switch (x->m_op) {
                    case ASR::logicalbinopType::And:
                        if ((l && !*l) || (r && !*r)) return false;
                        if (l && r) return true;
                        return std::nullopt;
                    case ASR::logicalbinopType::Or:
                        if ((l && *l) || (r && *r)) return true;
                        if (l && r) return false;
                        return std::nullopt;
                    case ASR::logicalbinopType::Eqv:
                        if (!l || !r) return std::nullopt;
                        return *l == *r;
                    case ASR::logicalbinopType::NEqv:
                        if (!l || !r) return std::nullopt;
                        return *l != *r;
                    default:
                        return std::nullopt;
                }
            }
            case ASR::exprType::IntegerCompare: {
                ASR::IntegerCompare_t *x =
                    ASR::down_cast<ASR::IntegerCompare_t>(e);
                std::optional<IntRange> l = range(x->m_left);
                std::optional<IntRange> r = range(x->m_right);
                if (!l || !r) return std::nullopt;
                return compare(*l, x->m_op, *r);
            }
            default:
                return std::nullopt;
        }
    }

private:
    const Facts &facts;

    static std::optional<bool> compare(const IntRange &l, ASR::cmpopType op,
            const IntRange &r) {
        switch (op) {
            case ASR::cmpopType::Eq:
                if (l.lo == l.hi && r.lo == r.hi && l.lo == r.lo) return true;
                if (l.hi < r.lo || r.hi < l.lo) return false;
                return std::nullopt;
            case ASR::cmpopType::NotEq: {
                std::optional<bool> eq = compare(l, ASR::cmpopType::Eq, r);
                if (!eq) return std::nullopt;
                return !*eq;
            }
            case ASR::cmpopType::Lt:
                if (l.hi < r.lo) return true;
                if (l.lo >= r.hi) return false;
                return std::nullopt;
            case ASR::cmpopType::LtE:
                if (l.hi <= r.lo) return true;
                if (l.lo > r.hi) return false;
                return std::nullopt;
            case ASR::cmpopType::Gt:
                return compare(r, ASR::cmpopType::Lt, l);
            case ASR::cmpopType::GtE:
                return compare(r, ASR::cmpopType::LtE, l);
            default:
                return std::nullopt;
        }
    }

    std::optional<IntRange> binop(ASR::IntegerBinOp_t &x) {
        std::optional<IntRange> l = range(x.m_left);
        std::optional<IntRange> r = range(x.m_right);
        if (!l || !r) return std::nullopt;
        switch (x.m_op) {
            case ASR::binopType::Add: {
                IntRange s;
                if (!add(l->lo, r->lo, s.lo) || !add(l->hi, r->hi, s.hi)) {
                    return std::nullopt;
                }
                return s;
            }
            case ASR::binopType::Sub: {
                IntRange s;
                if (!sub(l->lo, r->hi, s.lo) || !sub(l->hi, r->lo, s.hi)) {
                    return std::nullopt;
                }
                return s;
            }
            case ASR::binopType::Mul: {
                int64_t p[4];
                if (!mul(l->lo, r->lo, p[0]) || !mul(l->lo, r->hi, p[1])
                        || !mul(l->hi, r->lo, p[2])
                        || !mul(l->hi, r->hi, p[3])) {
                    return std::nullopt;
                }
                IntRange s{p[0], p[0]};
                for (int64_t v : p) {
                    s.lo = std::min(s.lo, v);
                    s.hi = std::max(s.hi, v);
                }
                return s;
            }
            case ASR::binopType::Div: {
                // Division truncates, so it is monotonic in the dividend
                // for a constant divisor.
                if (r->lo != r->hi || r->lo == 0
                        || (r->lo == -1 && l->lo == int64_min)) {
                    return std::nullopt;
                }
                int64_t c = r->lo;
                if (c > 0) return IntRange{l->lo / c, l->hi / c};
                return IntRange{l->hi / c, l->lo / c};
            }
            case ASR::binopType::BitAnd:
                return bit_and(*l, *r);
            case ASR::binopType::BitRShift: {
                // An arithmetic shift of a non-negative value.
                if (l->lo < 0 || r->lo != r->hi || r->lo < 0 || r->lo > 62) {
                    return std::nullopt;
                }
                return IntRange{l->lo >> r->lo, l->hi >> r->lo};
            }
            default:
                return std::nullopt;
        }
    }

    // The bits of a non-negative operand bound the result.
    static std::optional<IntRange> bit_and(const IntRange &l,
            const IntRange &r) {
        if (l.lo >= 0 && r.lo >= 0) return IntRange{0, std::min(l.hi, r.hi)};
        if (l.lo >= 0) return IntRange{0, l.hi};
        if (r.lo >= 0) return IntRange{0, r.hi};
        return std::nullopt;
    }

    std::optional<IntRange> intrinsic(ASR::IntrinsicElementalFunction_t &x) {
        std::vector<IntRange> args;
        for (size_t i = 0; i < x.n_args; i++) {
            if (x.m_args[i] == nullptr) return std::nullopt;
            std::optional<IntRange> a = range(x.m_args[i]);
            if (!a) return std::nullopt;
            args.push_back(*a);
        }
        switch (static_cast<ASRUtils::IntrinsicElementalFunctions>(
                x.m_intrinsic_id)) {
            case ASRUtils::IntrinsicElementalFunctions::Modulo: {
                // The result has the sign of the divisor and is smaller
                // in magnitude.
                if (args.size() != 2) return std::nullopt;
                const IntRange &a = args[0], &p = args[1];
                if (p.lo > 0) {
                    if (a.lo >= 0 && a.hi < p.lo) return a;
                    return IntRange{0, p.hi - 1};
                }
                if (p.hi < 0) {
                    if (a.hi <= 0 && a.lo > p.hi) return a;
                    return IntRange{p.lo + 1, 0};
                }
                return std::nullopt;
            }
            case ASRUtils::IntrinsicElementalFunctions::Mod: {
                // The result has the sign of the dividend and is smaller
                // in magnitude than the divisor.
                if (args.size() != 2) return std::nullopt;
                const IntRange &a = args[0], &p = args[1];
                if ((p.lo <= 0 && p.hi >= 0) || p.lo == int64_min) {
                    return std::nullopt;
                }
                int64_t m = std::max(p.lo < 0 ? -p.lo : p.lo,
                    p.hi < 0 ? -p.hi : p.hi) - 1;
                if (a.lo >= 0) return IntRange{0, std::min(m, a.hi)};
                if (a.hi <= 0) return IntRange{std::max(-m, a.lo), 0};
                return IntRange{-m, m};
            }
            case ASRUtils::IntrinsicElementalFunctions::Iand: {
                if (args.size() != 2) return std::nullopt;
                return bit_and(args[0], args[1]);
            }
            case ASRUtils::IntrinsicElementalFunctions::Ibits: {
                // ibits(i, pos, len) is a non-negative number of len bits.
                if (args.size() != 3) return std::nullopt;
                if (!kind_range(x.m_type)) return std::nullopt;
                int64_t bits = 8 * ASR::down_cast<ASR::Integer_t>(
                    x.m_type)->m_kind;
                const IntRange &len = args[2];
                if (len.lo < 0 || len.hi >= bits || len.hi > 62) {
                    return std::nullopt;
                }
                return IntRange{0, (int64_t(1) << len.hi) - 1};
            }
            case ASRUtils::IntrinsicElementalFunctions::Min:
            case ASRUtils::IntrinsicElementalFunctions::Max: {
                if (args.empty()) return std::nullopt;
                bool is_min = static_cast<ASRUtils::IntrinsicElementalFunctions>(
                    x.m_intrinsic_id) == ASRUtils::IntrinsicElementalFunctions::Min;
                IntRange s = args[0];
                for (const IntRange &a : args) {
                    if (is_min) {
                        s.lo = std::min(s.lo, a.lo);
                        s.hi = std::min(s.hi, a.hi);
                    } else {
                        s.lo = std::max(s.lo, a.lo);
                        s.hi = std::max(s.hi, a.hi);
                    }
                }
                return s;
            }
            default:
                return std::nullopt;
        }
    }

    std::optional<IntRange> narrow(ASR::expr_t *e) {
        int64_t v = 0;
        if (constant_integer(e, v)) return IntRange{v, v};
        switch (e->type) {
            case ASR::exprType::Var: {
                ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(e)->m_v);
                auto known = facts.find(s);
                if (known != facts.end()) return known->second;
                if (s != nullptr && ASR::is_a<ASR::Variable_t>(*s)) {
                    ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(s);
                    if (var->m_storage == ASR::storage_typeType::Parameter
                            && constant_integer(var->m_value, v)) {
                        return IntRange{v, v};
                    }
                }
                return std::nullopt;
            }
            case ASR::exprType::IntegerBinOp:
                return binop(*ASR::down_cast<ASR::IntegerBinOp_t>(e));
            case ASR::exprType::IntegerUnaryMinus: {
                std::optional<IntRange> a = range(
                    ASR::down_cast<ASR::IntegerUnaryMinus_t>(e)->m_arg);
                if (!a || a->lo == int64_min) return std::nullopt;
                return IntRange{-a->hi, -a->lo};
            }
            case ASR::exprType::Cast: {
                ASR::Cast_t *x = ASR::down_cast<ASR::Cast_t>(e);
                if (x->m_kind != ASR::cast_kindType::IntegerToInteger) {
                    return std::nullopt;
                }
                // A value outside the new kind leaves only that kind.
                return range(x->m_arg);
            }
            case ASR::exprType::IntrinsicElementalFunction:
                return intrinsic(
                    *ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e));
            default:
                return std::nullopt;
        }
    }
};

// The procedure a call names, past external symbols and type bound
// procedure declarations.
ASR::Function_t* called_function(ASR::symbol_t *name) {
    ASR::symbol_t *s = ASRUtils::symbol_get_past_external(name);
    if (s == nullptr) return nullptr;
    s = ASRUtils::symbol_get_past_external(
        ASRUtils::symbol_get_past_StructMethodDeclaration(s));
    if (s == nullptr || !ASR::is_a<ASR::Function_t>(*s)) return nullptr;
    return ASR::down_cast<ASR::Function_t>(s);
}

ASR::symbol_t* var_symbol(ASR::expr_t *e) {
    if (e == nullptr || !ASR::is_a<ASR::Var_t>(*e)) return nullptr;
    return ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(e)->m_v);
}

// The variables an expression reads.
class ReadVariables : public ASR::BaseWalkVisitor<ReadVariables> {
public:
    std::set<ASR::symbol_t*> vars;

    void visit_Var(const ASR::Var_t &x) {
        vars.insert(ASRUtils::symbol_get_past_external(x.m_v));
    }
};

// Whether a statement list holds the target of a GO TO.
class LabelFinder : public ASRUtils::BlockBodyWalkVisitor<LabelFinder> {
public:
    bool found = false;

    void visit_GoToTarget(const ASR::GoToTarget_t &/*x*/) {
        found = true;
    }
};

bool holds_label(ASR::stmt_t **body, size_t n) {
    LabelFinder f;
    for (size_t i = 0; i < n; i++) f.visit_stmt(*body[i]);
    return f.found;
}

// The scalars a statement can define: assignment targets, loop indices and
// variables passed to a dummy that is not `intent(in)`.
class DefinedScalars
        : public ASRUtils::BlockBodyWalkVisitor<DefinedScalars> {
public:
    std::set<ASR::symbol_t*> defined;

    void visit_Assignment(const ASR::Assignment_t &x) {
        if (ASR::symbol_t *s = var_symbol(x.m_target)) defined.insert(s);
        ASR::BaseWalkVisitor<DefinedScalars>::visit_Assignment(x);
    }

    void visit_DoLoop(const ASR::DoLoop_t &x) {
        if (ASR::symbol_t *s = var_symbol(x.m_head.m_v)) defined.insert(s);
        ASR::BaseWalkVisitor<DefinedScalars>::visit_DoLoop(x);
    }

    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        for (size_t i = 0; i < x.n_head; i++) {
            if (ASR::symbol_t *s = var_symbol(x.m_head[i].m_v)) {
                defined.insert(s);
            }
        }
        ASR::BaseWalkVisitor<DefinedScalars>::visit_DoConcurrentLoop(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        actuals(x.m_name, x.m_args, x.n_args);
        ASR::BaseWalkVisitor<DefinedScalars>::visit_FunctionCall(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        actuals(x.m_name, x.m_args, x.n_args);
        ASR::BaseWalkVisitor<DefinedScalars>::visit_SubroutineCall(x);
    }

private:
    void actuals(ASR::symbol_t *name, ASR::call_arg_t *args, size_t n) {
        ASR::Function_t *fn = called_function(name);
        for (size_t i = 0; i < n; i++) {
            ASR::symbol_t *s = var_symbol(args[i].m_value);
            if (s == nullptr) continue;
            bool in = false;
            if (fn != nullptr && i < fn->n_args) {
                ASR::symbol_t *d = var_symbol(fn->m_args[i]);
                in = d != nullptr && ASR::is_a<ASR::Variable_t>(*d)
                    && ASR::down_cast<ASR::Variable_t>(d)->m_intent
                        == ASR::intentType::In;
            }
            if (!in) defined.insert(s);
        }
    }
};

std::set<ASR::symbol_t*> defined_in(ASR::stmt_t **body, size_t n) {
    DefinedScalars d;
    for (size_t i = 0; i < n; i++) d.visit_stmt(*body[i]);
    return d.defined;
}

// The statements and declarations of a routine, BLOCK and ASSOCIATE
// included, but not the routines it contains.
template <typename Visitor>
void visit_declarations(Visitor &v, SymbolTable *scope) {
    for (auto &item : scope->get_scope()) {
        ASR::symbol_t *s = item.second;
        if (ASR::is_a<ASR::Variable_t>(*s)) {
            v.visit_symbol(*s);
        } else if (ASR::is_a<ASR::Block_t>(*s)) {
            visit_declarations(v, ASR::down_cast<ASR::Block_t>(s)->m_symtab);
        } else if (ASR::is_a<ASR::AssociateBlock_t>(*s)) {
            visit_declarations(v,
                ASR::down_cast<ASR::AssociateBlock_t>(s)->m_symtab);
        }
    }
}

class KernelGuardFold;

// Hands every call of a copy, and every other use of one, to the fold.
class CallVisitor : public ASRUtils::BlockBodyWalkVisitor<CallVisitor> {
public:
    explicit CallVisitor(KernelGuardFold &fold_) : fold(fold_) {}

    void visit_FunctionCall(const ASR::FunctionCall_t &x);
    void visit_SubroutineCall(const ASR::SubroutineCall_t &x);
    void visit_Var(const ASR::Var_t &x);
    void visit_BlockCall(const ASR::BlockCall_t &x);

private:
    KernelGuardFold &fold;
};

// The fold of one kernel and of the copies of the procedures it calls.
class KernelGuardFold {
public:
    KernelGuardFold(Allocator &al_, ASR::Function_t &kernel_)
        : al(al_), kernel(kernel_) {
        collect_copies(kernel.m_symtab);
    }

    void run() {
        // Who calls each copy, and which copies are named other than by
        // calling them.
        graph_only = true;
        for (ASR::Function_t *fn : routines()) {
            caller = fn;
            CallVisitor v(*this);
            for (size_t i = 0; i < fn->n_body; i++) v.visit_stmt(*fn->m_body[i]);
            visit_declarations(v, fn->m_symtab);
        }
        graph_only = false;

        std::set<ASR::Function_t*> done;
        fold_routine(kernel, {});
        done.insert(&kernel);
        for (bool progress = true; progress;) {
            progress = false;
            for (ASR::Function_t *fn : copies) {
                if (done.count(fn) > 0 || escaped.count(fn) > 0) continue;
                bool ready = true;
                for (ASR::Function_t *c : callers[fn]) {
                    if (done.count(c) == 0) ready = false;
                }
                if (!ready) continue;
                fold_routine(*fn, dummy_facts(*fn));
                done.insert(fn);
                progress = true;
            }
        }
        // What is left is judged without the ranges of its dummies.
        for (ASR::Function_t *fn : copies) {
            if (done.count(fn) == 0) fold_routine(*fn, {});
        }
    }

    // A call of `callee` with `args`, in the routine being visited.
    void call(ASR::symbol_t *name, ASR::call_arg_t *args, size_t n) {
        ASR::Function_t *callee = called_function(name);
        if (callee == nullptr || copy_set.count(callee) == 0) return;
        if (graph_only) {
            callers[callee].insert(caller);
            return;
        }
        std::vector<std::optional<IntRange>> &seen = calls[callee];
        seen.resize(callee->n_args);
        bool first = called.insert(callee).second;
        RangeEval eval(facts);
        for (size_t i = 0; i < callee->n_args; i++) {
            std::optional<IntRange> r;
            if (i < n && args[i].m_value != nullptr) {
                r = eval.range(args[i].m_value);
            }
            if (!r) {
                r = kind_range(ASRUtils::expr_type(callee->m_args[i]));
                if (!r) r = IntRange{int64_min, int64_max};
            }
            if (first) {
                seen[i] = r;
            } else if (seen[i]) {
                seen[i]->lo = std::min(seen[i]->lo, r->lo);
                seen[i]->hi = std::max(seen[i]->hi, r->hi);
            }
        }
    }

    // A BLOCK is run, by `x`.
    void block_call(const ASR::BlockCall_t &x) {
        if (graph_only) block_calls[ASRUtils::symbol_get_past_external(x.m_m)]++;
    }

    // `x` names a routine other than by calling it.
    void named(const ASR::Var_t &x) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_v);
        if (s == nullptr || !ASR::is_a<ASR::Function_t>(*s)) return;
        ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(s);
        if (copy_set.count(fn) > 0) escaped.insert(fn);
    }

private:
    Allocator &al;
    ASR::Function_t &kernel;
    // The copies of the procedures the kernel calls: the routines declared
    // inside it.
    std::vector<ASR::Function_t*> copies;
    std::set<ASR::Function_t*> copy_set;
    std::map<ASR::Function_t*, std::set<ASR::Function_t*>> callers;
    std::set<ASR::Function_t*> escaped;
    // The range of each argument over the calls of a copy seen so far.
    std::map<ASR::Function_t*, std::vector<std::optional<IntRange>>> calls;
    std::set<ASR::Function_t*> called;

    bool graph_only = false;
    ASR::Function_t *caller = nullptr;
    // What is known where the routine being folded is.
    Facts facts;
    // What is known of the dummies of the routine being folded, which holds
    // everywhere in it.
    Facts dummy_ranges;
    // How many statements run each BLOCK. One run by several is folded
    // with what is known everywhere, not where one of them is.
    std::map<ASR::symbol_t*, size_t> block_calls;

    void collect_copies(SymbolTable *scope) {
        for (auto &item : scope->get_scope()) {
            ASR::symbol_t *s = item.second;
            if (ASR::is_a<ASR::Function_t>(*s)) {
                ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(s);
                if (copy_set.insert(fn).second) copies.push_back(fn);
                collect_copies(fn->m_symtab);
            } else if (ASR::is_a<ASR::Block_t>(*s)) {
                collect_copies(ASR::down_cast<ASR::Block_t>(s)->m_symtab);
            }
        }
    }

    std::vector<ASR::Function_t*> routines() {
        std::vector<ASR::Function_t*> all = {&kernel};
        all.insert(all.end(), copies.begin(), copies.end());
        return all;
    }

    // The range of each `intent(in)` scalar integer dummy of `fn` over its
    // calls, of a dummy the copy does not define.
    Facts dummy_facts(ASR::Function_t &fn) {
        Facts result;
        auto seen = calls.find(&fn);
        if (seen == calls.end()) return result;
        std::set<ASR::symbol_t*> defined = defined_in(fn.m_body, fn.n_body);
        for (size_t i = 0; i < fn.n_args && i < seen->second.size(); i++) {
            ASR::symbol_t *d = var_symbol(fn.m_args[i]);
            if (d == nullptr || !ASR::is_a<ASR::Variable_t>(*d)
                    || !seen->second[i] || defined.count(d) > 0) {
                continue;
            }
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(d);
            if (v->m_intent != ASR::intentType::In
                    || !kind_range(v->m_type)) {
                continue;
            }
            result[d] = *seen->second[i];
        }
        return result;
    }

    void fold_routine(ASR::Function_t &fn, const Facts &dummies) {
        caller = &fn;
        facts = dummies;
        dummy_ranges = dummies;
        CallVisitor v(*this);
        visit_declarations(v, fn.m_symtab);
        fold_body(fn.m_body, fn.n_body);
    }

    // Whether the test `e` is always true or always false. When the ranges
    // alone do not decide it, it is judged for each value of a variable
    // it reads that has at most `max_split` of them, as
    // `half /= 0 .and. half /= 1` is with `half` in [0, 1].
    std::optional<bool> decide(ASR::expr_t *e) {
        constexpr int64_t max_split = 16;
        std::optional<bool> whole = RangeEval(facts).decide(e);
        if (whole) return whole;
        ReadVariables reads;
        reads.visit_expr(*e);
        for (ASR::symbol_t *s : reads.vars) {
            auto known = facts.find(s);
            if (known == facts.end()) continue;
            IntRange r = known->second;
            int64_t width = 0;
            if (r.lo > r.hi || !sub(r.hi, r.lo, width) || width >= max_split) {
                continue;
            }
            std::optional<bool> all;
            bool agree = true;
            for (int64_t k = 0; agree && k <= width; k++) {
                Facts one = facts;
                one[s] = IntRange{r.lo + k, r.lo + k};
                std::optional<bool> d = RangeEval(one).decide(e);
                if (!d || (all && *all != *d)) agree = false;
                all = d;
            }
            if (agree && all) return all;
        }
        return std::nullopt;
    }

    void visit_calls(ASR::expr_t *e) {
        if (e == nullptr) return;
        CallVisitor v(*this);
        v.visit_expr(*e);
    }

    // The range of the index `v` of a counted loop inside its body, added
    // to `facts`, when the body does not define it.
    void loop_index(const ASR::do_loop_head_t &head, ASR::stmt_t **body,
            size_t n) {
        ASR::symbol_t *index = var_symbol(head.m_v);
        if (index == nullptr || head.m_start == nullptr
                || head.m_end == nullptr) {
            return;
        }
        int64_t step = 1;
        if (head.m_increment != nullptr
                && !constant_integer(head.m_increment, step)) {
            return;
        }
        RangeEval eval(facts);
        std::optional<IntRange> start = eval.range(head.m_start);
        std::optional<IntRange> end = eval.range(head.m_end);
        if (!start || !end || step == 0
                || defined_in(body, n).count(index) > 0) {
            return;
        }
        facts[index] = step > 0 ? IntRange{start->lo, end->hi}
            : IntRange{end->lo, start->hi};
    }

    void fold_body(ASR::stmt_t **&body, size_t &n) {
        Vec<ASR::stmt_t*> out;
        out.reserve(al, n);
        for (size_t i = 0; i < n; i++) {
            ASR::stmt_t *s = body[i];
            switch (s->type) {
                case ASR::stmtType::If: {
                    ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                    std::optional<bool> taken = decide(x->m_test);
                    // A named IF can be the target of an EXIT, and the
                    // branch that goes the target of a GO TO.
                    if (taken && (x->m_name == nullptr
                            || x->m_name[0] == '\0')
                            && !holds_label(*taken ? x->m_orelse : x->m_body,
                                *taken ? x->n_orelse : x->n_body)) {
                        ASR::stmt_t **branch = *taken ? x->m_body
                            : x->m_orelse;
                        size_t branch_n = *taken ? x->n_body : x->n_orelse;
                        fold_body(branch, branch_n);
                        for (size_t k = 0; k < branch_n; k++) {
                            out.push_back(al, branch[k]);
                        }
                        continue;
                    }
                    visit_calls(x->m_test);
                    fold_body(x->m_body, x->n_body);
                    fold_body(x->m_orelse, x->n_orelse);
                    break;
                }
                case ASR::stmtType::DoLoop: {
                    ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(s);
                    visit_calls(x->m_head.m_start);
                    visit_calls(x->m_head.m_end);
                    visit_calls(x->m_head.m_increment);
                    Facts saved = facts;
                    loop_index(x->m_head, x->m_body, x->n_body);
                    fold_body(x->m_body, x->n_body);
                    facts = saved;
                    fold_body(x->m_orelse, x->n_orelse);
                    break;
                }
                case ASR::stmtType::DoConcurrentLoop: {
                    ASR::DoConcurrentLoop_t *x =
                        ASR::down_cast<ASR::DoConcurrentLoop_t>(s);
                    Facts saved = facts;
                    for (size_t h = 0; h < x->n_head; h++) {
                        visit_calls(x->m_head[h].m_start);
                        visit_calls(x->m_head[h].m_end);
                        visit_calls(x->m_head[h].m_increment);
                    }
                    for (size_t h = 0; h < x->n_head; h++) {
                        loop_index(x->m_head[h], x->m_body, x->n_body);
                    }
                    for (size_t k = 0; k < x->n_shared; k++) {
                        visit_calls(x->m_shared[k]);
                    }
                    for (size_t k = 0; k < x->n_local; k++) {
                        visit_calls(x->m_local[k]);
                    }
                    for (size_t k = 0; k < x->n_reduction; k++) {
                        visit_calls(x->m_reduction[k].m_arg);
                    }
                    fold_body(x->m_body, x->n_body);
                    facts = saved;
                    break;
                }
                case ASR::stmtType::WhileLoop: {
                    ASR::WhileLoop_t *x = ASR::down_cast<ASR::WhileLoop_t>(s);
                    visit_calls(x->m_test);
                    fold_body(x->m_body, x->n_body);
                    fold_body(x->m_orelse, x->n_orelse);
                    break;
                }
                case ASR::stmtType::BlockCall: {
                    ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                    if (b != nullptr && ASR::is_a<ASR::Block_t>(*b)) {
                        ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
                        Facts saved = facts;
                        if (block_calls[b] != 1) facts = dummy_ranges;
                        fold_body(blk->m_body, blk->n_body);
                        facts = saved;
                    }
                    break;
                }
                default: {
                    // Everything inside is judged with what is known here.
                    CallVisitor v(*this);
                    v.visit_stmt(*s);
                    break;
                }
            }
            out.push_back(al, s);
        }
        body = out.p;
        n = out.n;
    }
};

void CallVisitor::visit_FunctionCall(const ASR::FunctionCall_t &x) {
    fold.call(x.m_name, x.m_args, x.n_args);
    ASR::BaseWalkVisitor<CallVisitor>::visit_FunctionCall(x);
}

void CallVisitor::visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
    fold.call(x.m_name, x.m_args, x.n_args);
    ASR::BaseWalkVisitor<CallVisitor>::visit_SubroutineCall(x);
}

void CallVisitor::visit_Var(const ASR::Var_t &x) {
    fold.named(x);
}

void CallVisitor::visit_BlockCall(const ASR::BlockCall_t &x) {
    fold.block_call(x);
    ASRUtils::BlockBodyWalkVisitor<CallVisitor>::visit_BlockCall(x);
}

} // namespace

void pass_gpu_device_guard_fold(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &/*pass_options*/) {
    for (auto &item : unit.m_symtab->get_scope()) {
        if (!ASRUtils::is_device_kernel(item.second)) continue;
        KernelGuardFold fold(al,
            *ASR::down_cast<ASR::Function_t>(item.second));
        fold.run();
    }
}

} // namespace LCompilers
