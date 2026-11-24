// Lean 4 backend. Every Fortran procedure becomes one Lean template over
// `K : Kinds` (`lean/LFortran/Arith.lean`): `integer(k)` is `K.Ik`,
// `real(k)` is `K.Rk`. A procedure requires the weakest class it needs:
// `FKinds` (arithmetic; what a proof instantiates), `FKindsMath` (math
// intrinsics) or `FKindsIO` (`print`). A program's `main` is one line that
// instantiates its template at `LFortran.machine`. `--show-lean` and
// `--backend=lean` emit the same code; `--backend=lean` requires a program.
//
// Constructs not translated yet are rejected with an error rather than
// translated approximately.

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <functional>
#include <map>
#include <set>
#include <string>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/codegen/asr_to_lean.h>
#include <libasr/pass/intrinsic_function_registry.h>

namespace LCompilers {

namespace {

class LeanError {
public:
    diag::Diagnostic d;

    LeanError(const std::string &msg, const Location &loc)
        : d{diag::Diagnostic(msg, diag::Level::Error, diag::Stage::CodeGen,
            {diag::Label("", {loc})})} {}
};

[[noreturn]] void unsupported(const std::string &what, const Location &loc) {
    throw LeanError(what + " not supported by the Lean backend", loc);
}

constexpr int pAtom = 1024;
constexpr int pApp = 1023;
constexpr int pCmp = 50;
constexpr int pAnd = 35;
constexpr int pOr = 30;

struct Val {
    std::string src;
    int prec = pAtom;
};

std::string atom(const Val &v) {
    return v.prec < pAtom ? "(" + v.src + ")" : v.src;
}

std::string int_lit(int64_t i) {
    return i < 0 ? "(" + std::to_string(i) + ")" : std::to_string(i);
}

std::string string_lit(const std::string &s) {
    std::string o = "\"";
    for (char c : s) {
        if (c == '\\' || c == '"') o += '\\';
        o += c;
    }
    return o + "\"";
}

// Fortran names that Lean 4 parses as keywords. They are emitted as `«name»`,
// which is the identifier `name`, so they cannot collide with another name.
const std::set<std::string> reserved = {
    "abbrev", "at", "attribute", "axiom", "break", "builtin_initialize", "by",
    "calc", "catch", "class", "coinductive", "continue", "decreasing_by",
    "declare_syntax_cat", "def", "deriving", "do", "elab", "elab_rules",
    "else", "end", "example", "exists", "export", "extends", "false",
    "finally", "for", "forall", "from", "fun", "have", "if", "import", "in",
    "include", "inductive", "infix", "infixl", "infixr", "initialize",
    "instance", "lemma", "let", "local", "macro", "macro_rules", "match",
    "meta", "mut", "mutual", "namespace", "noncomputable", "nofun",
    "nomatch", "nonrec", "notation", "omit", "opaque", "open", "partial",
    "postfix", "prefix", "private", "protected", "public", "rec", "return",
    "scoped", "section", "set_option", "show", "sorry", "structure",
    "suffices", "syntax", "termination_by", "then", "theorem", "true", "try",
    "unless", "universe", "unsafe", "using", "variable", "where", "while",
    "with"};

std::vector<ASR::symbol_t*> symbols_in_order(SymbolTable *symtab) {
    std::vector<ASR::symbol_t*> syms;
    for (auto &item : symtab->get_scope()) syms.push_back(item.second);
    std::stable_sort(syms.begin(), syms.end(),
        [](ASR::symbol_t *a, ASR::symbol_t *b) {
            return a->base.loc.first < b->base.loc.first;
        });
    return syms;
}

// The class a procedure needs: 0 `FKinds`, 1 `FKindsMath`, 2 `FKindsIO`.
const char *level_class(int level) {
    switch (level) {
        case 0: return "FKinds";
        case 1: return "FKindsMath";
        default: return "FKindsIO";
    }
}

struct Proc {
    ASR::Function_t *fn = nullptr;
    std::string sig;        // arguments and result type
    std::string body;
    std::string ret;        // the value returned at the end
    int level = 0;
    std::set<ASR::Function_t*> callees;
};

class LeanTemplate {
public:
    std::string filename;
    bool require_program = false;
    bool executable = false;
    std::string out;

private:
    std::map<ASR::Function_t*, Proc> procs;
    std::vector<ASR::Function_t*> order;
    Proc *cur = nullptr;
    bool in_program = false;
    int level = 0;
    int ind = 0;
    ASR::Variable_t *result = nullptr;
    std::set<ASR::Variable_t*> loop_vars;
    std::string ns;
    std::string *buf = nullptr;

    std::string indent() const { return std::string(2 * ind, ' '); }

    void line(const std::string &s) { *buf += indent() + s + "\n"; }

    std::string name(const std::string &n, const Location &loc) {
        for (unsigned char c : n) {
            if (!(std::isalnum(c) || c == '_')) {
                unsupported("the name '" + n + "' is", loc);
            }
        }
        // `K` is the kinds parameter of every template; Fortran names are
        // lower case in ASR, so it cannot occur, but never shadow it.
        if (n == "K") unsupported("the name 'K' is", loc);
        if (reserved.count(n)) return "«" + n + "»";
        return n;
    }

    // Types ----------------------------------------------------------------

    ASR::ttype_t *scalar(ASR::ttype_t *t, const Location &loc) {
        if (!t || ASR::is_a<ASR::Array_t>(*t) || ASR::is_a<ASR::Pointer_t>(*t)
                || ASR::is_a<ASR::Allocatable_t>(*t)) {
            unsupported("this type is", loc);
        }
        return t;
    }

    std::string type(ASR::ttype_t *t, const Location &loc) {
        t = scalar(t, loc);
        if (ASR::is_a<ASR::Integer_t>(*t)) {
            int k = ASR::down_cast<ASR::Integer_t>(t)->m_kind;
            if (k == 1 || k == 2 || k == 4 || k == 8) {
                return "K.I" + std::to_string(k);
            }
            unsupported("integer kind " + std::to_string(k) + " is", loc);
        }
        if (ASR::is_a<ASR::Real_t>(*t)) {
            int k = ASR::down_cast<ASR::Real_t>(t)->m_kind;
            if (k == 4 || k == 8) return "K.R" + std::to_string(k);
            unsupported("real kind " + std::to_string(k) + " is", loc);
        }
        if (ASR::is_a<ASR::Logical_t>(*t)) return "Bool";
        unsupported("this type is", loc);
    }

    std::string zero(ASR::ttype_t *t, const Location &loc) {
        t = scalar(t, loc);
        if (ASR::is_a<ASR::Integer_t>(*t)) return "FInt.ofInt 0";
        if (ASR::is_a<ASR::Real_t>(*t)) return "FReal.zero";
        if (ASR::is_a<ASR::Logical_t>(*t)) return "false";
        unsupported("this type is", loc);
    }

    bool is_int(ASR::expr_t *e) {
        return ASR::is_a<ASR::Integer_t>(*ASRUtils::expr_type(e));
    }

    bool is_real(ASR::expr_t *e) {
        return ASR::is_a<ASR::Real_t>(*ASRUtils::expr_type(e));
    }

    // Expressions ----------------------------------------------------------

    // A real constant: `FReal.ofInt n` for an integer below `2^53`, else
    // `FReal.lit m e` with the exact value `m · 2^e`, `m` odd.
    std::string real_lit(double v, int kind, const Location &loc) {
        if (!std::isfinite(v)) unsupported("a non-finite real constant is", loc);
        // `-0.0` keeps its sign bit, which `sign(a, b)` observes.
        if (v == 0 && std::signbit(v)) return "FReal.neg (FReal.ofInt 0)";
        if (v == std::trunc(v) && std::fabs(v) < 0x1p53) {
            return "FReal.ofInt " + int_lit((int64_t) v);
        }
        int e;
        double f = std::frexp(v, &e);
        int64_t m = (int64_t) std::ldexp(f, 53);
        e -= 53;
        while (m % 2 == 0) {
            m /= 2;
            e++;
        }
        int lo = kind == 4 ? -126 : -1022;
        int hi = kind == 4 ? 127 : 1023;
        if (e < lo || e > hi) unsupported("this real constant is", loc);
        return "FReal.lit " + int_lit(m) + " " + int_lit(e);
    }

    // An operand whose type the context does not fix (`FReal.signbit`,
    // comparisons, conversions, ...): a constant is ascribed its kind, since
    // `FReal.ofInt 1` alone does not determine the type.
    Val operand(ASR::expr_t *e) {
        Val v = lower(e);
        if (ASRUtils::expr_value(e)) {
            return {"(" + v.src + " : "
                + type(ASRUtils::expr_type(e), e->base.loc) + ")", pAtom};
        }
        return v;
    }

    Val app(const std::string &f, std::initializer_list<Val> args) {
        std::string s = f;
        for (const Val &a : args) s += " " + atom(a);
        return {s, pApp};
    }

    Val compare(ASR::expr_t *l, ASR::cmpopType op, ASR::expr_t *r) {
        std::string c = is_int(l) ? "FInt" : "FReal";
        Val a = operand(l), b = lower(r);
        switch (op) {
            case ASR::cmpopType::Eq: return app(c + ".beq", {a, b});
            case ASR::cmpopType::NotEq:
                return {"!" + atom(app(c + ".beq", {a, b})), pApp};
            case ASR::cmpopType::Lt: return app(c + ".lt", {a, b});
            case ASR::cmpopType::LtE: return app(c + ".le", {a, b});
            case ASR::cmpopType::Gt: return app(c + ".lt", {b, a});
            case ASR::cmpopType::GtE: return app(c + ".le", {b, a});
        }
        return app(c + ".beq", {a, b});
    }

    std::string call_name(ASR::symbol_t *sym) {
        sym = ASRUtils::symbol_get_past_external(sym);
        std::string n = name(ASRUtils::symbol_name(sym), sym->base.loc);
        if (ASR::Module_t *m = ASRUtils::get_sym_module(sym)) {
            return name(m->m_name, m->base.base.loc) + "." + n;
        }
        return n;
    }

    ASR::Function_t *callee(ASR::symbol_t *sym, const Location &loc) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(sym);
        if (!ASR::is_a<ASR::Function_t>(*s)) unsupported("this call is", loc);
        return ASR::down_cast<ASR::Function_t>(s);
    }

    Val call(ASR::symbol_t *sym, ASR::call_arg_t *args, size_t n,
            const Location &loc) {
        ASR::Function_t *fn = callee(sym, loc);
        cur->callees.insert(fn);
        std::string src = call_name((ASR::symbol_t*) fn) + " (K := K)";
        for (size_t i = 0; i < n; i++) {
            if (!args[i].m_value) unsupported("an omitted argument is", loc);
            src += " " + atom(lower(args[i].m_value));
        }
        return {src, pApp};
    }

    Val intrinsic(const ASR::IntrinsicElementalFunction_t &x, const Location &loc) {
        std::string f = ASRUtils::get_intrinsic_name(x.m_intrinsic_id);
        std::transform(f.begin(), f.end(), f.begin(), ::tolower);
        if (x.n_args == 1 && f == "abs") {
            Val a = operand(x.m_args[0]);
            if (is_real(x.m_args[0])) return app("FReal.abs", {a});
            return {"FInt.ofInt " + atom({"Int.natAbs " + atom(
                app("FInt.toInt", {a})), pApp}), pApp};
        }
        // `int(x)`: truncation toward zero, in the result's integer kind.
        if (x.n_args >= 1 && f == "int" && is_real(x.m_args[0])) {
            return {"(FInt.ofInt " + atom(app("FReal.toInt", {operand(x.m_args[0])}))
                + " : " + type(x.m_type, loc) + ")", pAtom};
        }
        if (x.n_args == 2 && (f == "modulo" || f == "mod") && is_int(x.m_args[0])) {
            return app(f == "modulo" ? "FInt.modulo" : "FInt.mod",
                {lower(x.m_args[0]), lower(x.m_args[1])});
        }
        // `sign(a, b)`: `|a|` with the sign bit of `b`, so `b = -0` gives
        // `-|a|`, as on every processor with signed zeros.
        if (x.n_args == 2 && f == "sign" && is_real(x.m_args[0])) {
            Val a = app("FReal.abs", {lower(x.m_args[0])});
            Val b = operand(x.m_args[1]);
            return {"(if " + app("FReal.signbit", {b}).src
                + " then " + app("FReal.neg", {a}).src + " else " + a.src + ")", pAtom};
        }
        static const std::set<std::string> math = {
            "sqrt", "exp", "log", "sin", "cos", "tan", "atan"};
        if (x.n_args == 1 && math.count(f) && is_real(x.m_args[0])) {
            level = std::max(level, 1);
            return app("FRealMath." + f, {operand(x.m_args[0])});
        }
        if (x.m_value) return lower(x.m_value);
        unsupported("the intrinsic '" + f + "' is", loc);
    }

    Val cast(const ASR::Cast_t &x, const Location &loc) {
        if (x.m_value) return lower(x.m_value);
        switch (x.m_kind) {
            case ASR::cast_kindType::IntegerToReal:
                // A procedure may have no integer kind in scope, so an
                // integer constant converted to real is `FReal.ofInt n`.
                if (ASR::is_a<ASR::IntegerConstant_t>(*x.m_arg)) {
                    return {"FReal.ofInt " + int_lit(ASR::down_cast<
                        ASR::IntegerConstant_t>(x.m_arg)->m_n), pApp};
                }
                return app("FReal.ofInt", {app("FInt.toInt", {operand(x.m_arg)})});
            case ASR::cast_kindType::IntegerToInteger:
                return {"(FInt.ofInt " + atom(app("FInt.toInt", {operand(x.m_arg)}))
                    + " : " + type(x.m_type, loc) + ")", pAtom};
            case ASR::cast_kindType::RealToInteger:
                return {"(FInt.ofInt " + atom(app("FReal.toInt", {operand(x.m_arg)}))
                    + " : " + type(x.m_type, loc) + ")", pAtom};
            case ASR::cast_kindType::RealToReal: {
                int k = ASR::down_cast<ASR::Real_t>(x.m_type)->m_kind;
                return app(k == 8 ? "FKinds.r4to8" : "FKinds.r8to4",
                    {operand(x.m_arg)});
            }
            default:
                unsupported("this conversion is", loc);
        }
    }

    Val lower(ASR::expr_t *e) {
        const Location &loc = e->base.loc;
        switch (e->type) {
            case ASR::exprType::Var: {
                ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(e)->m_v);
                if (!ASR::is_a<ASR::Variable_t>(*s)) unsupported("this name is", loc);
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
                if (v->m_storage == ASR::storage_typeType::Parameter && v->m_value) {
                    return lower(v->m_value);
                }
                if (v->m_parent_symtab != cur_scope()) {
                    unsupported("a variable from another scope is", loc);
                }
                return {name(v->m_name, v->base.base.loc), pAtom};
            }
            case ASR::exprType::IntegerConstant:
                return {"FInt.ofInt " + int_lit(
                    ASR::down_cast<ASR::IntegerConstant_t>(e)->m_n), pApp};
            case ASR::exprType::RealConstant: {
                auto &x = *ASR::down_cast<ASR::RealConstant_t>(e);
                return {real_lit(x.m_r, ASR::down_cast<ASR::Real_t>(x.m_type)->m_kind,
                    loc), pApp};
            }
            case ASR::exprType::LogicalConstant:
                return {ASR::down_cast<ASR::LogicalConstant_t>(e)->m_value
                    ? "true" : "false", pAtom};
            case ASR::exprType::IntegerBinOp: {
                auto &x = *ASR::down_cast<ASR::IntegerBinOp_t>(e);
                const char *f = nullptr;
                switch (x.m_op) {
                    case ASR::binopType::Add: f = "FInt.add"; break;
                    case ASR::binopType::Sub: f = "FInt.sub"; break;
                    case ASR::binopType::Mul: f = "FInt.mul"; break;
                    case ASR::binopType::Div: f = "FInt.div"; break;
                    default: unsupported("this integer operator is", loc);
                }
                return app(f, {lower(x.m_left), lower(x.m_right)});
            }
            case ASR::exprType::RealBinOp: {
                auto &x = *ASR::down_cast<ASR::RealBinOp_t>(e);
                const char *f = nullptr;
                switch (x.m_op) {
                    case ASR::binopType::Add: f = "FReal.add"; break;
                    case ASR::binopType::Sub: f = "FReal.sub"; break;
                    case ASR::binopType::Mul: f = "FReal.mul"; break;
                    case ASR::binopType::Div: f = "FReal.div"; break;
                    default: unsupported("this real operator is", loc);
                }
                return app(f, {lower(x.m_left), lower(x.m_right)});
            }
            case ASR::exprType::IntegerUnaryMinus:
                if (ASR::expr_t *v = ASRUtils::expr_value(e)) return lower(v);
                return app("FInt.sub", {{"FInt.ofInt 0", pApp},
                    lower(ASR::down_cast<ASR::IntegerUnaryMinus_t>(e)->m_arg)});
            case ASR::exprType::RealUnaryMinus:
                if (ASR::expr_t *v = ASRUtils::expr_value(e)) return lower(v);
                return app("FReal.neg",
                    {lower(ASR::down_cast<ASR::RealUnaryMinus_t>(e)->m_arg)});
            case ASR::exprType::IntegerCompare: {
                auto &x = *ASR::down_cast<ASR::IntegerCompare_t>(e);
                return compare(x.m_left, x.m_op, x.m_right);
            }
            case ASR::exprType::RealCompare: {
                auto &x = *ASR::down_cast<ASR::RealCompare_t>(e);
                return compare(x.m_left, x.m_op, x.m_right);
            }
            case ASR::exprType::LogicalBinOp: {
                auto &x = *ASR::down_cast<ASR::LogicalBinOp_t>(e);
                std::string op = "&&";
                int p = pAnd;
                switch (x.m_op) {
                    case ASR::logicalbinopType::And: break;
                    case ASR::logicalbinopType::Or: op = "||"; p = pOr; break;
                    case ASR::logicalbinopType::Eqv: op = "=="; p = pCmp; break;
                    default: op = "!="; p = pCmp; break;
                }
                return {atom(lower(x.m_left)) + " " + op + " "
                    + atom(lower(x.m_right)), p};
            }
            case ASR::exprType::LogicalNot:
                return {"!" + atom(lower(ASR::down_cast<ASR::LogicalNot_t>(e)->m_arg)),
                    pApp};
            case ASR::exprType::Cast:
                return cast(*ASR::down_cast<ASR::Cast_t>(e), loc);
            case ASR::exprType::FunctionCall: {
                auto &x = *ASR::down_cast<ASR::FunctionCall_t>(e);
                if (x.m_dt) unsupported("a type-bound call is", loc);
                return call(x.m_name, x.m_args, x.n_args, loc);
            }
            case ASR::exprType::IntrinsicElementalFunction:
                return intrinsic(*ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e),
                    loc);
            default:
                if (ASR::expr_t *v = ASRUtils::expr_value(e)) return lower(v);
                unsupported("this expression is", loc);
        }
    }

    // Statements -----------------------------------------------------------

    SymbolTable *scope = nullptr;
    SymbolTable *cur_scope() const { return scope; }

    std::string shown(ASR::expr_t *e) {
        const Location &loc = e->base.loc;
        if (ASR::is_a<ASR::StringConstant_t>(*e)) {
            return string_lit(ASR::down_cast<ASR::StringConstant_t>(e)->m_s);
        }
        if (ASR::is_a<ASR::IntegerConstant_t>(*e)) {
            return string_lit(std::to_string(
                ASR::down_cast<ASR::IntegerConstant_t>(e)->m_n));
        }
        ASR::ttype_t *t = scalar(ASRUtils::expr_type(e), loc);
        if (ASR::is_a<ASR::Integer_t>(*t)) {
            return "toString " + atom(app("FInt.toInt", {lower(e)}));
        }
        if (ASR::is_a<ASR::Real_t>(*t)) {
            int k = ASR::down_cast<ASR::Real_t>(t)->m_kind;
            std::string v = atom(lower(e));
            if (ASR::is_a<ASR::RealConstant_t>(*e)) {
                v = "(" + v + " : " + type(t, loc) + ")";
            }
            return "FKindsIO.fmtR" + std::to_string(k) + " " + v;
        }
        if (ASR::is_a<ASR::Logical_t>(*t)) {
            return "(if " + lower(e).src + " then \"T\" else \"F\")";
        }
        unsupported("printing this value is", loc);
    }

    void halt(ASR::expr_t *code, bool error, const Location &loc) {
        if (!in_program) unsupported("stop in a procedure is", loc);
        if (!code) {
            line(std::string("LFortran.halt ") + (error ? "1" : "0"));
            return;
        }
        if (ASR::is_a<ASR::IntegerConstant_t>(*code)) {
            line("LFortran.halt (LFortran.stopCode " + int_lit(
                ASR::down_cast<ASR::IntegerConstant_t>(code)->m_n) + ")");
            return;
        }
        unsupported("this stop code is", loc);
    }

    void stmts(ASR::stmt_t **body, size_t n) {
        for (size_t i = 0; i < n; i++) stmt(body[i]);
    }

    void stmt(ASR::stmt_t *s) {
        const Location &loc = s->base.loc;
        switch (s->type) {
            case ASR::stmtType::Assignment: {
                auto &x = *ASR::down_cast<ASR::Assignment_t>(s);
                if (!ASR::is_a<ASR::Var_t>(*x.m_target)) {
                    unsupported("this assignment is", loc);
                }
                line(lower(x.m_target).src + " := " + lower(x.m_value).src);
                break;
            }
            case ASR::stmtType::If: {
                auto &x = *ASR::down_cast<ASR::If_t>(s);
                line("if " + lower(x.m_test).src + " then");
                ind++;
                if (x.n_body == 0) line("pure ()");
                stmts(x.m_body, x.n_body);
                ind--;
                if (x.n_orelse) {
                    line("else");
                    ind++;
                    stmts(x.m_orelse, x.n_orelse);
                    ind--;
                }
                break;
            }
            case ASR::stmtType::DoLoop: {
                auto &x = *ASR::down_cast<ASR::DoLoop_t>(s);
                ASR::do_loop_head_t &h = x.m_head;
                bool unit = !h.m_increment
                    || (ASR::is_a<ASR::IntegerConstant_t>(*h.m_increment)
                        && ASR::down_cast<ASR::IntegerConstant_t>(
                            h.m_increment)->m_n == 1);
                if (!h.m_v || !ASR::is_a<ASR::Var_t>(*h.m_v) || !unit
                        || x.m_name) {
                    unsupported("this do loop is", loc);
                }
                // The index is not declared, so its kind is fixed on `lo`.
                line("for " + lower(h.m_v).src + " in FInt.range ("
                    + lower(h.m_start).src + " : "
                    + type(ASRUtils::expr_type(h.m_v), loc) + ") "
                    + atom(lower(h.m_end)) + " do");
                ind++;
                if (x.n_body == 0) line("pure ()");
                stmts(x.m_body, x.n_body);
                ind--;
                break;
            }
            case ASR::stmtType::Return:
                line(in_program ? std::string("return") : "return " + cur->ret);
                break;
            case ASR::stmtType::Print: {
                auto &x = *ASR::down_cast<ASR::Print_t>(s);
                if (!in_program) unsupported("print in a procedure is", loc);
                level = std::max(level, 2);
                if (!ASR::is_a<ASR::StringFormat_t>(*x.m_text)) {
                    unsupported("this print is", loc);
                }
                auto &f = *ASR::down_cast<ASR::StringFormat_t>(x.m_text);
                if (f.m_fmt) unsupported("formatted output is", loc);
                std::string items;
                for (size_t i = 0; i < f.n_args; i++) {
                    if (i) items += ", ";
                    items += shown(f.m_args[i]);
                }
                line("IO.println (String.intercalate \" \" [" + items + "])");
                break;
            }
            case ASR::stmtType::ErrorStop:
                halt(ASR::down_cast<ASR::ErrorStop_t>(s)->m_code, true, loc);
                break;
            case ASR::stmtType::Stop:
                halt(ASR::down_cast<ASR::Stop_t>(s)->m_code, false, loc);
                break;
            default:
                unsupported("this statement is", loc);
        }
    }

    void collect_loop_vars(ASR::stmt_t **body, size_t n) {
        for (size_t i = 0; i < n; i++) {
            ASR::stmt_t *s = body[i];
            if (ASR::is_a<ASR::DoLoop_t>(*s)) {
                auto &d = *ASR::down_cast<ASR::DoLoop_t>(s);
                if (d.m_head.m_v && ASR::is_a<ASR::Var_t>(*d.m_head.m_v)) {
                    loop_vars.insert(ASRUtils::EXPR2VAR(d.m_head.m_v));
                }
                collect_loop_vars(d.m_body, d.n_body);
            } else if (ASR::is_a<ASR::If_t>(*s)) {
                auto &d = *ASR::down_cast<ASR::If_t>(s);
                collect_loop_vars(d.m_body, d.n_body);
                collect_loop_vars(d.m_orelse, d.n_orelse);
            }
        }
    }

    // Locals of a procedure or program body: every variable that is not a
    // dummy, a parameter or a loop index.
    void locals(SymbolTable *st, const std::set<ASR::Variable_t*> &dummies) {
        for (ASR::symbol_t *s : symbols_in_order(st)) {
            if (!ASR::is_a<ASR::Variable_t>(*s)) continue;
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
            if (dummies.count(v) || loop_vars.count(v)) continue;
            if (v->m_storage == ASR::storage_typeType::Parameter) continue;
            const Location &vloc = v->base.base.loc;
            if (v->m_symbolic_value || v->m_value) {
                unsupported("an initialized local variable is", vloc);
            }
            line("let mut " + name(v->m_name, vloc) + " : " + type(v->m_type, vloc)
                + " := " + zero(v->m_type, vloc));
        }
    }

    void check_symbols(SymbolTable *st) {
        for (ASR::symbol_t *s : symbols_in_order(st)) {
            if (ASR::is_a<ASR::Variable_t>(*s) || ASR::is_a<ASR::ExternalSymbol_t>(*s)) {
                continue;
            }
            unsupported("this declaration is", s->base.loc);
        }
    }

    void function(ASR::Function_t *fn) {
        const Location &loc = fn->base.base.loc;
        Proc &p = procs[fn];
        p.fn = fn;
        cur = &p;
        level = 0;
        loop_vars.clear();
        scope = fn->m_symtab;
        in_program = false;
        check_symbols(fn->m_symtab);
        if (!fn->m_return_var) unsupported("a subroutine is", loc);
        result = ASRUtils::EXPR2VAR(fn->m_return_var);
        p.ret = name(result->m_name, result->base.base.loc);
        std::set<ASR::Variable_t*> dummies;
        for (size_t i = 0; i < fn->n_args; i++) {
            ASR::Variable_t *a = ASRUtils::EXPR2VAR(fn->m_args[i]);
            const Location &aloc = a->base.base.loc;
            if (a->m_intent != ASR::intentType::In) {
                unsupported("an argument that is not intent(in) is", aloc);
            }
            dummies.insert(a);
            p.sig += " (" + name(a->m_name, aloc) + " : " + type(a->m_type, aloc) + ")";
        }
        p.sig += " : " + type(result->m_type, result->base.base.loc);
        collect_loop_vars(fn->m_body, fn->n_body);
        buf = &p.body;
        ind = 1;
        locals(fn->m_symtab, dummies);
        stmts(fn->m_body, fn->n_body);
        bool returned = fn->n_body
            && ASR::is_a<ASR::Return_t>(*fn->m_body[fn->n_body - 1]);
        if (!returned) line("return " + p.ret);
        p.level = level;
        order.push_back(fn);
        cur = nullptr;
    }

    // A caller needs at least the class of each procedure it calls.
    void propagate_levels() {
        bool changed = true;
        while (changed) {
            changed = false;
            for (auto &kv : procs) {
                for (ASR::Function_t *c : kv.second.callees) {
                    auto it = procs.find(c);
                    if (it == procs.end()) {
                        unsupported("a call to a procedure outside this file is",
                            c->base.base.loc);
                    }
                    if (it->second.level > kv.second.level) {
                        kv.second.level = it->second.level;
                        changed = true;
                    }
                }
            }
        }
    }

    // A module's procedures, each after the ones it calls. Mutual recursion
    // is rejected; a procedure that calls itself is a `partial def`.
    std::vector<ASR::Function_t*> callees_first(const std::vector<ASR::Function_t*> &fns) {
        std::vector<ASR::Function_t*> sorted;
        std::map<ASR::Function_t*, int> state;  // 1 visiting, 2 done
        std::function<void(ASR::Function_t*)> visit = [&](ASR::Function_t *fn) {
            if (state[fn] == 2) return;
            if (state[fn] == 1) {
                unsupported("mutual recursion is", fn->base.base.loc);
            }
            state[fn] = 1;
            for (ASR::Function_t *c : procs[fn].callees) {
                if (c != fn && std::find(fns.begin(), fns.end(), c) != fns.end()) {
                    visit(c);
                }
            }
            state[fn] = 2;
            sorted.push_back(fn);
        };
        for (ASR::Function_t *fn : fns) visit(fn);
        return sorted;
    }

    std::string def(const Proc &p, const std::string &short_name) {
        std::string kw = p.callees.count(p.fn) ? "partial def " : "def ";
        return kw + short_name + " {K : Kinds} [" + level_class(p.level) + " K]"
            + p.sig + " := Id.run do\n" + p.body + "\n";
    }

public:
    void translate(ASR::TranslationUnit_t &tu) {
        std::vector<ASR::Module_t*> mods;
        ASR::Program_t *prog = nullptr;
        for (ASR::symbol_t *s : symbols_in_order(tu.m_symtab)) {
            if (ASR::is_a<ASR::Module_t>(*s)) {
                ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(s);
                if (m->m_intrinsic) continue;
                mods.push_back(m);
            } else if (ASR::is_a<ASR::Program_t>(*s)) {
                prog = ASR::down_cast<ASR::Program_t>(s);
            } else {
                unsupported("this top-level entity is", s->base.loc);
            }
        }
        if (require_program && !prog) {
            unsupported("a file with no program is", tu.base.base.loc);
        }
        std::vector<std::pair<std::string, std::vector<ASR::Function_t*>>> groups;
        for (ASR::Module_t *m : mods) {
            std::string mn = name(m->m_name, m->base.base.loc);
            std::vector<ASR::Function_t*> fns;
            for (ASR::symbol_t *s : symbols_in_order(m->m_symtab)) {
                if (ASR::is_a<ASR::Function_t>(*s)) {
                    ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(s);
                    function(fn);
                    fns.push_back(fn);
                } else if (ASR::is_a<ASR::Variable_t>(*s)) {
                    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
                    if (v->m_storage != ASR::storage_typeType::Parameter) {
                        unsupported("a module variable is", s->base.loc);
                    }
                } else if (!ASR::is_a<ASR::ExternalSymbol_t>(*s)) {
                    unsupported("this module entity is", s->base.loc);
                }
            }
            groups.push_back({mn, fns});
        }
        std::string prog_body;
        int prog_level = 2;
        Proc prog_proc;
        if (prog) {
            cur = &prog_proc;
            level = 2;
            loop_vars.clear();
            scope = prog->m_symtab;
            in_program = true;
            result = nullptr;
            check_symbols(prog->m_symtab);
            collect_loop_vars(prog->m_body, prog->n_body);
            buf = &prog_body;
            ind = 1;
            locals(prog->m_symtab, {});
            stmts(prog->m_body, prog->n_body);
            cur = nullptr;
        }
        propagate_levels();
        out = "-- Generated by LFortran (`lfortran "
            + std::string(executable ? "--backend=lean " : "--show-lean ") + filename
            + "`). Do not edit.\nimport LFortran.Runtime\n\nopen LFortran\n\n";
        for (auto &g : groups) {
            if (g.second.empty()) continue;
            out += "namespace " + g.first + "\n\n";
            for (ASR::Function_t *fn : callees_first(g.second)) {
                out += def(procs[fn], name(fn->m_name, fn->base.base.loc));
            }
            out += "end " + g.first + "\n\n";
        }
        if (prog) {
            for (ASR::Function_t *c : prog_proc.callees) {
                prog_level = std::max(prog_level, procs[c].level);
            }
            std::string pn = name(prog->m_name, prog->base.base.loc) + "_main";
            out += "def " + pn + " {K : Kinds} [" + level_class(prog_level)
                + " K] : IO Unit := do\n" + prog_body + "\n";
            out += "def main : IO UInt32 := LFortran.run (" + pn
                + " (K := LFortran.machine))\n";
        }
        while (out.size() >= 2 && out.back() == '\n' && out[out.size() - 2] == '\n') {
            out.pop_back();
        }
    }
};

Result<std::string> run(ASR::TranslationUnit_t &asr, diag::Diagnostics &diagnostics,
        const std::string &filename, bool executable) {
    LeanTemplate v;
    v.filename = filename;
    v.executable = executable;
    v.require_program = executable;
    try {
        v.translate(asr);
    } catch (const LeanError &e) {
        diagnostics.diagnostics.push_back(e.d);
        return Error();
    } catch (const LCompilersException &e) {
        diagnostics.diagnostics.push_back(diag::Diagnostic(
            std::string("Lean backend: ") + e.what(),
            diag::Level::Error, diag::Stage::CodeGen,
            {diag::Label("", {asr.base.base.loc})}));
        return Error();
    }
    return v.out;
}

} // namespace

Result<std::string> asr_to_lean(ASR::TranslationUnit_t &asr,
        diag::Diagnostics &diagnostics, const std::string &filename)
{
    return run(asr, diagnostics, filename, false);
}

Result<std::string> asr_to_lean_exec(ASR::TranslationUnit_t &asr,
        diag::Diagnostics &diagnostics, const std::string &filename)
{
    return run(asr, diagnostics, filename, true);
}

} // namespace LCompilers
