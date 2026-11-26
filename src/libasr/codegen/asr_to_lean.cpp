// Lean 4 backend. Every Fortran procedure becomes one Lean template over
// `K : Kinds` (`lean/runtime/LFortran/Arith.lean`): `integer(k)` is `K.Ik`,
// `real(k)` is `K.Rk`. A procedure requires the weakest class it needs:
// `FKinds` (arithmetic; what a proof instantiates), `FKindsMath` (math
// intrinsics) or `FKindsIO` (`print`). A program's `main` is one line that
// instantiates its template at `LFortran.machine`. `--show-lean` and
// `--backend=lean` emit the same code; `--backend=lean` requires a program.
//
// Arrays are `Arr` values and procedures are pure functions with value
// semantics: a subroutine returns its `intent(out)` and `intent(inout)`
// arguments, and `call s(a, b)` assigns them back (copy-in/copy-out, which
// is faithful because Fortran forbids aliasing a modified dummy argument).
// An `intent(out)` scalar is not passed in.
//
// Constructs not translated yet are rejected with an error rather than
// translated approximately.

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <functional>
#include <limits>
#include <map>
#include <set>
#include <string>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/codegen/asr_to_lean.h>
#include <libasr/pass/intrinsic_array_function_registry.h>
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
        if (c == '\\' || c == '"') { o += '\\'; o += c; }
        else if (c == '\n') o += "\\n";
        else if (c == '\r') o += "\\r";
        else if (c == '\t') o += "\\t";
        else o += c;
    }
    return o + "\"";
}

// Fortran names that Lean 4 parses as keywords. They are emitted as `«name»`,
// which is the identifier `name`, so they cannot collide with another name.
const std::set<std::string> reserved = {
    "abbrev", "at", "attribute", "axiom", "break", "builtin_initialize", "by",
    "calc", "catch", "class", "coinductive", "continue", "dbg_trace",
    "decreasing_by", "declare_syntax_cat", "def", "deriving", "do", "elab",
    "elab_rules", "else", "end", "example", "exists", "export", "extends",
    "false", "finally", "for", "forall", "from", "fun", "have", "hiding",
    "if", "import", "in", "include", "inductive", "infix", "infixl",
    "infixr", "initialize", "instance", "lemma", "let", "let_fun", "local",
    "macro", "macro_rules", "match", "meta", "mut", "mutual", "namespace",
    "noncomputable", "nofun", "nomatch", "nonrec", "notation", "omit",
    "opaque", "open", "partial", "postfix", "prefix", "private", "protected",
    "public", "rec", "renaming", "repeat", "return", "scoped", "section", "set_option", "show", "sorry", "structure",
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

// The `do` variables that are used outside the loops they index (before or
// after a loop, or as an ordinary variable). Fortran gives them a value
// after the loop; the others are emitted as the bound name of `for`.
class LoopVarUses : public ASR::BaseWalkVisitor<LoopVarUses> {
public:
    std::vector<ASR::Variable_t*> index;
    std::set<ASR::Variable_t*> outside;

    void visit_Var(const ASR::Var_t &x) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_v);
        if (!ASR::is_a<ASR::Variable_t>(*s)) return;
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
        if (std::find(index.begin(), index.end(), v) == index.end()) outside.insert(v);
    }

    void visit_DoLoop(const ASR::DoLoop_t &x) {
        // The bounds are evaluated before the loop.
        if (x.m_head.m_start) visit_expr(*x.m_head.m_start);
        if (x.m_head.m_end) visit_expr(*x.m_head.m_end);
        if (x.m_head.m_increment) visit_expr(*x.m_head.m_increment);
        ASR::Variable_t *v = x.m_head.m_v && ASR::is_a<ASR::Var_t>(*x.m_head.m_v)
            ? ASRUtils::EXPR2VAR(x.m_head.m_v) : nullptr;
        if (v) index.push_back(v);
        for (size_t i = 0; i < x.n_body; i++) visit_stmt(*x.m_body[i]);
        for (size_t i = 0; i < x.n_orelse; i++) visit_stmt(*x.m_orelse[i]);
        if (v) index.pop_back();
    }
};

// Pre-pass collecting direct IO usage, callees, and modified variables of a
// procedure so `io_procs` and effective `Unspecified` intents are known before
// any body is lowered.
class ProcEffects : public ASR::BaseWalkVisitor<ProcEffects> {
public:
    bool has_io = false;
    std::set<ASR::Function_t*> callees;
    std::set<ASR::Variable_t*> modified;
    struct SubArg {
        ASR::Function_t *callee;
        size_t idx;
        ASR::Variable_t *var;
    };
    std::vector<SubArg> sub_args;

    static ASR::Variable_t *root_var(ASR::expr_t *e) {
        while (e) {
            if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
                e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
            } else if (ASR::is_a<ASR::ArrayItem_t>(*e)) {
                e = ASR::down_cast<ASR::ArrayItem_t>(e)->m_v;
            } else if (ASR::is_a<ASR::ArraySection_t>(*e)) {
                e = ASR::down_cast<ASR::ArraySection_t>(e)->m_v;
            } else if (ASR::is_a<ASR::Var_t>(*e)) {
                ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(e)->m_v);
                return ASR::is_a<ASR::Variable_t>(*s) ? ASR::down_cast<ASR::Variable_t>(s) : nullptr;
            } else {
                return nullptr;
            }
        }
        return nullptr;
    }

    void visit_Print(const ASR::Print_t &x) {
        has_io = true;
        ASR::BaseWalkVisitor<ProcEffects>::visit_Print(x);
    }

    void visit_Stop(const ASR::Stop_t &x) {
        has_io = true;
        ASR::BaseWalkVisitor<ProcEffects>::visit_Stop(x);
    }

    void visit_ErrorStop(const ASR::ErrorStop_t &x) {
        has_io = true;
        ASR::BaseWalkVisitor<ProcEffects>::visit_ErrorStop(x);
    }

    void visit_Assignment(const ASR::Assignment_t &x) {
        if (ASR::Variable_t *v = root_var(x.m_target)) modified.insert(v);
        ASR::BaseWalkVisitor<ProcEffects>::visit_Assignment(x);
    }

    void visit_DoLoop(const ASR::DoLoop_t &x) {
        if (ASR::Variable_t *v = root_var(x.m_head.m_v)) modified.insert(v);
        ASR::BaseWalkVisitor<ProcEffects>::visit_DoLoop(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_name);
        if (ASR::is_a<ASR::Function_t>(*s)) callees.insert(ASR::down_cast<ASR::Function_t>(s));
        ASR::BaseWalkVisitor<ProcEffects>::visit_FunctionCall(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_name);
        if (ASR::is_a<ASR::Function_t>(*s)) {
            ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(s);
            callees.insert(fn);
            for (size_t i = 0; i < x.n_args && i < fn->n_args; i++) {
                if (ASR::Variable_t *v = root_var(x.m_args[i].m_value)) {
                    sub_args.push_back({fn, i, v});
                }
            }
        }
        ASR::BaseWalkVisitor<ProcEffects>::visit_SubroutineCall(x);
    }
};

struct Proc {
    ASR::Function_t *fn = nullptr;
    std::string sig;        // arguments and result type
    std::string body;
    std::string ret;        // the value returned at the end
    int level = 0;
    bool uses_while = false;
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
    std::map<ASR::Function_t*, std::string> proc_ns;
    std::set<ASR::Function_t*> io_procs;
    std::map<ASR::Function_t*, std::vector<ASR::intentType>> eff_intents;
    std::vector<ASR::Function_t*> order;
    Proc *cur = nullptr;
    bool in_program = false;
    int level = 0;
    int ind = 0;
    ASR::Variable_t *result = nullptr;
    std::set<ASR::Variable_t*> loop_vars;
    std::vector<std::string> exit_flags;
    int loop_id = 0;
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
        if (ASR::is_a<ASR::Complex_t>(*t)) {
            int k = ASR::down_cast<ASR::Complex_t>(t)->m_kind;
            if (k == 4 || k == 8) return "FCplx K.R" + std::to_string(k);
            unsupported("complex kind " + std::to_string(k) + " is", loc);
        }
        if (ASR::is_a<ASR::Logical_t>(*t)) return "Bool";
        if (ASR::is_a<ASR::String_t>(*t)) {
            int k = ASR::down_cast<ASR::String_t>(t)->m_kind;
            if (k == 1) return "String";
            unsupported("character kind " + std::to_string(k) + " is", loc);
        }
        unsupported("this type is", loc);
    }

    std::string zero(ASR::ttype_t *t, const Location &loc) {
        t = scalar(t, loc);
        if (ASR::is_a<ASR::Integer_t>(*t)) return "FInt.ofInt 0";
        if (ASR::is_a<ASR::Real_t>(*t)) return "FReal.zero";
        if (ASR::is_a<ASR::Complex_t>(*t)) return "FCplx.zero";
        if (ASR::is_a<ASR::Logical_t>(*t)) return "false";
        if (ASR::is_a<ASR::String_t>(*t)) {
            auto &s = *ASR::down_cast<ASR::String_t>(t);
            if (s.m_len) return "FStr.fix " + extent(s.m_len) + " \"\"";
            return "\"\"";
        }
        unsupported("this type is", loc);
    }

    bool is_array(ASR::ttype_t *t) {
        return t && ASR::is_a<ASR::Array_t>(*t);
    }

    // The type of a variable: a scalar type, or `Arr` of one.
    std::string vtype(ASR::ttype_t *t, const Location &loc) {
        if (is_array(t)) {
            std::string et = type(ASR::down_cast<ASR::Array_t>(t)->m_type, loc);
            if (et.find(' ') != std::string::npos) et = "(" + et + ")";
            return "Arr " + et;
        }
        return type(t, loc);
    }

    // An integer expression as a Lean `Int`.
    Val int_expr(ASR::expr_t *e) {
        if (ASR::expr_t *v = ASRUtils::expr_value(e)) e = v;
        if (ASR::is_a<ASR::IntegerConstant_t>(*e)) {
            return {int_lit(ASR::down_cast<ASR::IntegerConstant_t>(e)->m_n), pAtom};
        }
        if (!is_int(e)) unsupported("this index is", e->base.loc);
        return app("FInt.toInt", {operand(e)});
    }

    // An extent as a Lean `Nat`.
    std::string extent(ASR::expr_t *e) {
        ASR::expr_t *v = ASRUtils::expr_value(e);
        if (v && ASR::is_a<ASR::IntegerConstant_t>(*v)) {
            int64_t n = ASR::down_cast<ASR::IntegerConstant_t>(v)->m_n;
            return std::to_string(n < 0 ? 0 : n);
        }
        return atom(int_expr(e)) + ".toNat";
    }

    std::string int_array(const std::vector<std::string> &xs) {
        std::string s = "#[";
        for (size_t i = 0; i < xs.size(); i++) s += (i ? ", " : "") + xs[i];
        return s + "]";
    }

    // The declared lower bounds and, for an explicit shape, extents.
    void dims(ASR::ttype_t *t, std::vector<std::string> &lbs,
            std::vector<std::string> &exts, bool &assumed, const Location &loc) {
        ASR::Array_t *a = ASR::down_cast<ASR::Array_t>(t);
        if (a->m_physical_type == ASR::array_physical_typeType::UnboundedPointerArray
                || a->m_physical_type == ASR::array_physical_typeType::AssumedRankArray) {
            unsupported("this array shape is", loc);
        }
        size_t n_len = 0;
        for (size_t i = 0; i < a->n_dims; i++) {
            ASR::dimension_t &d = a->m_dims[i];
            lbs.push_back(d.m_start ? int_expr(d.m_start).src : "1");
            if (d.m_length) {
                n_len++;
                exts.push_back(extent(d.m_length));
            }
        }
        if (n_len != 0 && n_len != a->n_dims) unsupported("this array shape is", loc);
        assumed = n_len == 0;
    }

    // An array argument or operand: a whole array variable or array literal.
    ASR::expr_t *array_var(ASR::expr_t *e) {
        while (ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
            e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
        }
        if (!ASR::is_a<ASR::Var_t>(*e) && !ASR::is_a<ASR::ArrayConstant_t>(*e)
                && !ASR::is_a<ASR::ArrayConstructor_t>(*e)) {
            unsupported("this array expression is", e->base.loc);
        }
        return e;
    }

    Val array_constant(const ASR::ArrayConstant_t &x, const Location &loc) {
        if (x.m_storage_format != ASR::arraystorageType::ColMajor) {
            unsupported("this array expression is", loc);
        }
        if (!is_array(x.m_type)) unsupported("this array expression is", loc);
        ASR::Array_t *at = ASR::down_cast<ASR::Array_t>(x.m_type);
        ASR::ttype_t *et = scalar(at->m_type, loc);
        type(et, loc);
        std::vector<std::string> lbs, exts;
        bool assumed;
        dims(x.m_type, lbs, exts, assumed, loc);
        if (assumed) unsupported("this array expression is", loc);
        int64_t n = ASRUtils::get_fixed_size_of_array(x.m_type);
        if (n < 0 || n > 4096 || (n > 0 && !x.m_data)) {
            unsupported("this array expression is", loc);
        }
        std::string elems = "#[";
        for (int64_t i = 0; i < n; i++) {
            if (i) elems += ", ";
            if (ASR::is_a<ASR::Integer_t>(*et)) {
                int k = ASR::down_cast<ASR::Integer_t>(et)->m_kind;
                int64_t v = 0;
                if (k == 1) v = static_cast<int8_t*>(x.m_data)[i];
                else if (k == 2) v = static_cast<int16_t*>(x.m_data)[i];
                else if (k == 4) v = static_cast<int32_t*>(x.m_data)[i];
                else v = static_cast<int64_t*>(x.m_data)[i];
                elems += "FInt.ofInt " + int_lit(v);
            } else if (ASR::is_a<ASR::Real_t>(*et)) {
                int k = ASR::down_cast<ASR::Real_t>(et)->m_kind;
                double v = k == 4 ? static_cast<double>(static_cast<float*>(x.m_data)[i])
                    : static_cast<double*>(x.m_data)[i];
                elems += real_lit(v, k, loc);
            } else if (ASR::is_a<ASR::Complex_t>(*et)) {
                int k = ASR::down_cast<ASR::Complex_t>(et)->m_kind;
                double re = k == 4 ? static_cast<double>(static_cast<float*>(x.m_data)[2 * i])
                    : static_cast<double*>(x.m_data)[2 * i];
                double im = k == 4 ? static_cast<double>(static_cast<float*>(x.m_data)[2 * i + 1])
                    : static_cast<double*>(x.m_data)[2 * i + 1];
                elems += "FCplx.mk (" + real_lit(re, k, loc) + ") (" + real_lit(im, k, loc) + ")";
            } else if (ASR::is_a<ASR::Logical_t>(*et)) {
                bool v = static_cast<const bool*>(x.m_data)[i];
                elems += v ? "true" : "false";
            } else {
                unsupported("this array expression is", loc);
            }
        }
        elems += "]";
        return {"({ lbs := " + int_array(lbs) + ", exts := " + int_array(exts)
            + ", data := " + elems + " } : " + vtype(x.m_type, loc) + ")", pAtom};
    }

    Val array_constructor(const ASR::ArrayConstructor_t &x, const Location &loc) {
        if (x.m_value && ASR::is_a<ASR::ArrayConstant_t>(*x.m_value)) {
            return array_constant(*ASR::down_cast<ASR::ArrayConstant_t>(x.m_value), loc);
        }
        if (x.m_storage_format != ASR::arraystorageType::ColMajor || !is_array(x.m_type)) {
            unsupported("this array expression is", loc);
        }
        ASR::Array_t *at = ASR::down_cast<ASR::Array_t>(x.m_type);
        if (at->n_dims != 1) unsupported("this array expression is", loc);
        std::string elems = "#[";
        for (size_t i = 0; i < x.n_args; i++) {
            if (is_array(ASRUtils::expr_type(x.m_args[i]))
                    || ASR::is_a<ASR::ImpliedDoLoop_t>(*x.m_args[i])) {
                unsupported("this array expression is", loc);
            }
            if (i) elems += ", ";
            elems += fix_string(at->m_type, "", lower(x.m_args[i]), loc);
        }
        elems += "]";
        return {"({ lbs := #[1], exts := #[" + std::to_string(x.n_args)
            + "], data := " + elems + " } : " + vtype(x.m_type, loc) + ")", pAtom};
    }

    bool is_int(ASR::expr_t *e) {
        return ASR::is_a<ASR::Integer_t>(*ASRUtils::expr_type(e));
    }

    bool is_real(ASR::expr_t *e) {
        return ASR::is_a<ASR::Real_t>(*ASRUtils::expr_type(e));
    }

    bool is_complex(ASR::expr_t *e) {
        return ASR::is_a<ASR::Complex_t>(*ASRUtils::expr_type(e));
    }

    bool is_logical(ASR::expr_t *e) {
        return ASR::is_a<ASR::Logical_t>(*ASRUtils::expr_type(e));
    }

    bool is_string(ASR::expr_t *e) {
        return ASR::is_a<ASR::String_t>(
            *ASRUtils::type_get_past_allocatable(ASRUtils::expr_type(e)));
    }

    bool is_assumed_len_string(ASR::ttype_t *t) {
        return t && ASR::is_a<ASR::String_t>(*t)
            && !ASR::down_cast<ASR::String_t>(t)->m_len;
    }

    std::string fix_string(ASR::ttype_t *t, const std::string &var_name, const Val &rhs,
            const Location &loc) {
        if (!ASR::is_a<ASR::String_t>(*t)) return rhs.src;
        auto &st = *ASR::down_cast<ASR::String_t>(t);
        if (st.m_len) return "FStr.fix " + extent(st.m_len) + " " + atom(rhs);
        if (st.m_len_kind == ASR::string_length_kindType::AssumedLength) {
            if (!var_name.empty()) return "FStr.fix " + var_name + ".length " + atom(rhs);
            unsupported("an assumed-length character array element assignment is", loc);
        }
        return rhs.src;
    }

    ASR::ttype_t *elem_type(ASR::ttype_t *t) {
        return is_array(t) ? ASR::down_cast<ASR::Array_t>(t)->m_type : t;
    }

    // Lift a binary operation on scalar expressions to arrays (handling
    // ArrayBroadcast on either side via Arr.map, or two arrays via Arr.zipWith).
    Val lift_binop(ASR::expr_t *l, ASR::expr_t *r,
            const std::function<std::string(const std::string&, const std::string&)> &fn) {
        bool lb = ASR::is_a<ASR::ArrayBroadcast_t>(*l);
        bool rb = ASR::is_a<ASR::ArrayBroadcast_t>(*r);
        if (lb && !rb) {
            ASR::expr_t *ls = ASR::down_cast<ASR::ArrayBroadcast_t>(l)->m_array;
            if (is_array(ASRUtils::expr_type(ls))) unsupported("this array expression is", l->base.loc);
            std::string sv = atom(operand(ls));
            return app("Arr.map", {{"((fun s' y' => " + fn("s'", "y'") + ") " + sv + ")", pAtom},
                lower(r)});
        }
        if (rb && !lb) {
            ASR::expr_t *rs = ASR::down_cast<ASR::ArrayBroadcast_t>(r)->m_array;
            if (is_array(ASRUtils::expr_type(rs))) unsupported("this array expression is", r->base.loc);
            std::string sv = atom(operand(rs));
            return app("Arr.map", {{"((fun s' x' => " + fn("x'", "s'") + ") " + sv + ")", pAtom},
                lower(l)});
        }
        if (is_array(ASRUtils::expr_type(l)) && is_array(ASRUtils::expr_type(r)) && !lb && !rb) {
            return app("Arr.zipWith", {{"(fun x' y' => " + fn("x'", "y'") + ")", pAtom},
                lower(l), lower(r)});
        }
        unsupported("this array expression is", l->base.loc);
    }

    // Expressions ----------------------------------------------------------

    // A real constant: `FReal.ofInt n` for an integer below `2^53`, else
    // `FReal.lit m e` with the exact value `m · 2^e`, `m` odd.
    std::string real_lit(double v, int kind, const Location &loc) {
        if (kind == 4) v = static_cast<double>(static_cast<float>(v));
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

    // Whether the emitted expression determines its Lean type: it contains a
    // variable, an array element, a call, or a conversion with a stated kind.
    // `FReal.ofInt 1` or `FReal.ofInt (FInt.toInt i)` alone does not.
    bool determined(ASR::expr_t *e) {
        if (ASRUtils::expr_value(e)) return false;
        switch (e->type) {
            case ASR::exprType::Var:
            case ASR::exprType::ArrayItem:
            case ASR::exprType::ArraySize:
            case ASR::exprType::ArrayBound:
            case ASR::exprType::IntrinsicArrayFunction:
            case ASR::exprType::FunctionCall:
            case ASR::exprType::IntegerCompare:
            case ASR::exprType::RealCompare:
            case ASR::exprType::ComplexCompare:
            case ASR::exprType::StringCompare:
            case ASR::exprType::StringConstant:
            case ASR::exprType::StringConcat:
            case ASR::exprType::StringRepeat:
            case ASR::exprType::StringLen:
            case ASR::exprType::StringItem:
            case ASR::exprType::StringSection:
            case ASR::exprType::StringOrd:
            case ASR::exprType::StringChr:
            case ASR::exprType::StringPhysicalCast:
            case ASR::exprType::LogicalBinOp:
            case ASR::exprType::LogicalNot:
                return true;
            case ASR::exprType::ComplexConstructor: {
                auto &x = *ASR::down_cast<ASR::ComplexConstructor_t>(e);
                return determined(x.m_re) || determined(x.m_im);
            }
            case ASR::exprType::ComplexBinOp: {
                auto &x = *ASR::down_cast<ASR::ComplexBinOp_t>(e);
                if (x.m_op == ASR::binopType::Pow && is_int(x.m_right)) {
                    return determined(x.m_left);
                }
                return determined(x.m_left) || determined(x.m_right);
            }
            case ASR::exprType::ComplexUnaryMinus:
                return determined(ASR::down_cast<ASR::ComplexUnaryMinus_t>(e)->m_arg);
            case ASR::exprType::ComplexRe:
                return determined(ASR::down_cast<ASR::ComplexRe_t>(e)->m_arg);
            case ASR::exprType::ComplexIm:
                return determined(ASR::down_cast<ASR::ComplexIm_t>(e)->m_arg);
            case ASR::exprType::IntegerBinOp: {
                auto &x = *ASR::down_cast<ASR::IntegerBinOp_t>(e);
                return determined(x.m_left) || determined(x.m_right);
            }
            case ASR::exprType::RealBinOp: {
                auto &x = *ASR::down_cast<ASR::RealBinOp_t>(e);
                if (x.m_op == ASR::binopType::Pow && is_int(x.m_right)) {
                    return determined(x.m_left);
                }
                return determined(x.m_left) || determined(x.m_right);
            }
            case ASR::exprType::IntegerUnaryMinus:
                return determined(ASR::down_cast<ASR::IntegerUnaryMinus_t>(e)->m_arg);
            case ASR::exprType::RealUnaryMinus:
                return determined(ASR::down_cast<ASR::RealUnaryMinus_t>(e)->m_arg);
            case ASR::exprType::Cast: {
                auto &c = *ASR::down_cast<ASR::Cast_t>(e);
                if (c.m_kind == ASR::cast_kindType::IntegerToReal
                        || c.m_kind == ASR::cast_kindType::IntegerToComplex) {
                    return false;
                }
                if (c.m_kind == ASR::cast_kindType::RealToReal
                        && ASR::down_cast<ASR::Real_t>(c.m_type)->m_kind
                            == ASR::down_cast<ASR::Real_t>(ASRUtils::expr_type(c.m_arg))->m_kind) {
                    return determined(c.m_arg);
                }
                if (c.m_kind == ASR::cast_kindType::RealToComplex
                        && ASR::down_cast<ASR::Complex_t>(c.m_type)->m_kind
                            == ASR::down_cast<ASR::Real_t>(ASRUtils::expr_type(c.m_arg))->m_kind) {
                    return determined(c.m_arg);
                }
                if (c.m_kind == ASR::cast_kindType::ComplexToReal
                        && ASR::down_cast<ASR::Real_t>(c.m_type)->m_kind
                            == ASR::down_cast<ASR::Complex_t>(ASRUtils::expr_type(c.m_arg))->m_kind) {
                    return determined(c.m_arg);
                }
                if (c.m_kind == ASR::cast_kindType::ComplexToComplex
                        && ASR::down_cast<ASR::Complex_t>(c.m_type)->m_kind
                            == ASR::down_cast<ASR::Complex_t>(ASRUtils::expr_type(c.m_arg))->m_kind) {
                    return determined(c.m_arg);
                }
                return true;
            }
            case ASR::exprType::IntrinsicElementalFunction: {
                auto &x = *ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e);
                std::string f = ASRUtils::get_intrinsic_name(x.m_intrinsic_id);
                std::transform(f.begin(), f.end(), f.begin(), ::tolower);
                if (f == "int" || f == "real" || f == "floor" || f == "ceiling" || f == "nint"
                        || f == "stringlentrim" || f == "substrindex" || f == "stringtrim"
                        || f == "stringconcat" || f == "repeat" || f == "adjustl"
                        || f == "adjustr" || f == "lge" || f == "lgt" || f == "lle" || f == "llt") {
                    return true;
                }
                if (f == "merge") {
                    return (x.n_args > 0 && determined(x.m_args[0]))
                        || (x.n_args > 1 && determined(x.m_args[1]));
                }
                for (size_t i = 0; i < x.n_args; i++) {
                    if (determined(x.m_args[i])) return true;
                }
                return false;
            }
            default:
                return false;
        }
    }

    // An operand whose type the context does not fix (`FReal.signbit`,
    // comparisons, conversions, ...) is ascribed its kind unless it
    // determines it.
    Val operand(ASR::expr_t *e) {
        Val v = lower(e);
        if (!determined(e)) {
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
        if (ASR::is_a<ASR::Function_t>(*sym)) {
            auto it = proc_ns.find(ASR::down_cast<ASR::Function_t>(sym));
            if (it != proc_ns.end() && !it->second.empty()) {
                std::string mn = it->second.find('\'') != std::string::npos
                    ? it->second : name(it->second, sym->base.loc);
                if (cur_scope() && cur_scope()->get_symbol(it->second)) {
                    return "_root_." + mn + "." + n;
                }
                return mn + "." + n;
            }
        }
        if (ASR::Module_t *m = ASRUtils::get_sym_module(sym)) {
            std::string mn = name(m->m_name, m->base.base.loc);
            if (cur_scope() && cur_scope()->get_symbol(m->m_name)) {
                return "_root_." + mn + "." + n;
            }
            return mn + "." + n;
        }
        if (cur_scope() && cur_scope()->get_symbol(ASRUtils::symbol_name(sym))) {
            return "_root_." + n;
        }
        return n;
    }

    ASR::Function_t *callee(ASR::symbol_t *sym, const Location &loc) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(sym);
        if (!ASR::is_a<ASR::Function_t>(*s)) unsupported("this call is", loc);
        return ASR::down_cast<ASR::Function_t>(s);
    }

    static ASR::Variable_t *dummy(ASR::Function_t *fn, size_t i) {
        if (!fn->m_args[i] || !ASR::is_a<ASR::Var_t>(*fn->m_args[i])) return nullptr;
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(fn->m_args[i])->m_v);
        return ASR::is_a<ASR::Variable_t>(*s) ? ASR::down_cast<ASR::Variable_t>(s) : nullptr;
    }

    ASR::intentType arg_intent(ASR::Function_t *fn, size_t i) const {
        auto it = eff_intents.find(fn);
        if (it != eff_intents.end() && i < it->second.size()) return it->second[i];
        ASR::Variable_t *d = dummy(fn, i);
        return d ? d->m_intent : ASR::intentType::Unspecified;
    }

    // The application of a procedure to the arguments it takes: every
    // argument except an `intent(out)` scalar.
    Val call(ASR::symbol_t *sym, ASR::call_arg_t *args, size_t n,
            const Location &loc) {
        ASR::Function_t *fn = callee(sym, loc);
        cur->callees.insert(fn);
        if (n != fn->n_args) unsupported("this call is", loc);
        std::string src = call_name((ASR::symbol_t*) fn) + " (K := K)";
        for (size_t i = 0; i < n; i++) {
            if (!args[i].m_value) unsupported("an omitted argument is", loc);
            ASR::Variable_t *d = dummy(fn, i);
            if (!d) unsupported("a procedure argument is", args[i].m_value->base.loc);
            ASR::intentType in = arg_intent(fn, i);
            if (in == ASR::intentType::Out && !is_array(d->m_type)
                    && !is_assumed_len_string(d->m_type)) continue;
            ASR::expr_t *a = args[i].m_value;
            if (is_array(d->m_type) != is_array(ASRUtils::expr_type(a))) {
                unsupported("this argument is", a->base.loc);
            }
            if (is_array(ASRUtils::expr_type(a)) && in != ASR::intentType::In) {
                a = array_var(a);
            }
            src += " " + atom(lower(a));
        }
        return {src, pApp};
    }

    Val intrinsic(const ASR::IntrinsicElementalFunction_t &x, const Location &loc) {
        std::string f = ASRUtils::get_intrinsic_name(x.m_intrinsic_id);
        std::transform(f.begin(), f.end(), f.begin(), ::tolower);
        if (x.n_args == 1 && f == "abs") {
            if (is_real(x.m_args[0])) return app("FReal.abs", {operand(x.m_args[0])});
            if (is_complex(x.m_args[0])) {
                level = std::max(level, 1);
                return app("FCplx.abs", {operand(x.m_args[0])});
            }
            if (is_int(x.m_args[0])) {
                Val a = operand(x.m_args[0]);
                return {"(FInt.ofInt " + atom({"Int.natAbs " + atom(
                    app("FInt.toInt", {a})), pApp}) + " : " + type(x.m_type, loc) + ")", pAtom};
            }
        }
        if (x.n_args == 1 && (f == "conjg" || f == "dconjg") && is_complex(x.m_args[0])) {
            return app("FCplx.conj", {operand(x.m_args[0])});
        }
        if (x.n_args == 1 && f == "aimag" && is_complex(x.m_args[0])) {
            return {atom(operand(x.m_args[0])) + ".im", pAtom};
        }
        // `int(x)`: truncation toward zero, in the result's integer kind.
        if (x.n_args >= 1 && f == "int" && is_real(x.m_args[0])) {
            return {"(FInt.ofInt " + atom(app("FReal.toInt", {operand(x.m_args[0])}))
                + " : " + type(x.m_type, loc) + ")", pAtom};
        }
        if (x.n_args >= 1 && f == "real") {
            std::string dst_ty = type(x.m_type, loc);
            if (is_int(x.m_args[0])) {
                if (ASR::is_a<ASR::IntegerConstant_t>(*x.m_args[0])) {
                    return {"(FReal.ofInt " + int_lit(ASR::down_cast<
                        ASR::IntegerConstant_t>(x.m_args[0])->m_n) + " : " + dst_ty + ")", pAtom};
                }
                return {"(" + app("FReal.ofInt", {app("FInt.toInt", {operand(x.m_args[0])})}).src
                    + " : " + dst_ty + ")", pAtom};
            }
            if (is_real(x.m_args[0])) {
                type(ASRUtils::expr_type(x.m_args[0]), x.m_args[0]->base.loc);
                int dst_k = ASR::down_cast<ASR::Real_t>(x.m_type)->m_kind;
                int src_k = ASR::down_cast<ASR::Real_t>(ASRUtils::expr_type(x.m_args[0]))->m_kind;
                if (src_k == dst_k) return {"(" + lower(x.m_args[0]).src + " : " + dst_ty + ")", pAtom};
                return app(dst_k == 8 ? "FKinds.r4to8" : "FKinds.r8to4", {operand(x.m_args[0])});
            }
            if (is_complex(x.m_args[0])) {
                type(ASRUtils::expr_type(x.m_args[0]), x.m_args[0]->base.loc);
                int dst_k = ASR::down_cast<ASR::Real_t>(x.m_type)->m_kind;
                int src_k = ASR::down_cast<ASR::Complex_t>(ASRUtils::expr_type(x.m_args[0]))->m_kind;
                std::string re = atom(operand(x.m_args[0])) + ".re";
                if (src_k == dst_k) return {"(" + re + " : " + dst_ty + ")", pAtom};
                return app(dst_k == 8 ? "FKinds.r4to8" : "FKinds.r8to4", {{re, pAtom}});
            }
        }
        if (x.n_args >= 2 && (f == "max" || f == "min")) {
            bool all_int = is_int(x.m_args[0]);
            bool all_real = is_real(x.m_args[0]);
            for (size_t i = 1; i < x.n_args; i++) {
                all_int = all_int && is_int(x.m_args[i]);
                all_real = all_real && is_real(x.m_args[i]);
            }
            if (all_int || all_real) {
                std::string fn = (all_int ? "FInt." : "FReal.") + std::string(f == "max" ? "fmax" : "fmin");
                Val acc = lower(x.m_args[0]);
                for (size_t i = 1; i < x.n_args; i++) {
                    acc = app(fn, {acc, lower(x.m_args[i])});
                }
                return acc;
            }
        }
        if (x.n_args == 2 && (f == "modulo" || f == "mod") && is_int(x.m_args[0])
                && is_int(x.m_args[1])) {
            int rk = ASR::down_cast<ASR::Integer_t>(x.m_type)->m_kind;
            std::string rty = type(x.m_type, loc);
            auto arg_as = [&](ASR::expr_t *e) -> Val {
                int ak = ASR::down_cast<ASR::Integer_t>(ASRUtils::expr_type(e))->m_kind;
                if (ak == rk) return lower(e);
                return {"(FInt.ofInt " + atom(app("FInt.toInt", {operand(e)}))
                    + " : " + rty + ")", pAtom};
            };
            return app(f == "modulo" ? "FInt.modulo" : "FInt.mod",
                {arg_as(x.m_args[0]), arg_as(x.m_args[1])});
        }
        // `sign(a, b)`: `|a|` with the sign bit of `b`, so `b = -0` gives
        // `-|a|`, as on every processor with signed zeros.
        if (x.n_args == 2 && f == "sign" && is_real(x.m_args[0])) {
            Val a = app("FReal.abs", {operand(x.m_args[0])});
            Val b = operand(x.m_args[1]);
            if (a.src.find("←") != std::string::npos || b.src.find("←") != std::string::npos) {
                return {"((fun a' b' => if FReal.signbit b' then FReal.neg a' else a') "
                    + atom(a) + " " + atom(b) + ")", pAtom};
            }
            return {"(if " + app("FReal.signbit", {b}).src
                + " then " + app("FReal.neg", {a}).src + " else " + a.src + ")", pAtom};
        }
        if (x.n_args >= 1 && (f == "floor" || f == "ceiling" || f == "nint")
                && is_real(x.m_args[0])) {
            std::string h = f == "floor" ? "FReal.floorInt"
                : f == "ceiling" ? "FReal.ceilInt" : "FReal.nintInt";
            return {"(FInt.ofInt " + atom(app(h, {operand(x.m_args[0])}))
                + " : " + type(x.m_type, loc) + ")", pAtom};
        }
        if (x.n_args == 3 && f == "merge" && is_logical(x.m_args[2])
                && !is_array(x.m_type)) {
            Val t = lower(x.m_args[0]), fv = lower(x.m_args[1]), m = lower(x.m_args[2]);
            if (t.src.find("←") != std::string::npos || fv.src.find("←") != std::string::npos
                    || m.src.find("←") != std::string::npos) {
                return {"((fun t' f' m' => if m' then t' else f') "
                    + atom(t) + " " + atom(fv) + " " + atom(m) + ")", pAtom};
            }
            return {"(if " + m.src + " then " + t.src + " else " + fv.src + ")", pAtom};
        }
        static const std::set<std::string> math = {
            "sqrt", "exp", "log", "log10", "sin", "cos", "tan",
            "asin", "acos", "atan", "sinh", "cosh", "tanh"};
        if (x.n_args == 1 && math.count(f) && is_real(x.m_args[0])) {
            level = std::max(level, 1);
            return app("FRealMath." + f, {operand(x.m_args[0])});
        }
        if (x.n_args == 1 && f == "stringtrim" && is_string(x.m_args[0])) {
            return app("FStr.trim", {lower(x.m_args[0])});
        }
        if (x.n_args == 1 && f == "stringlentrim" && is_string(x.m_args[0])) {
            return {"(FInt.ofInt " + atom(app("FStr.lenTrim", {lower(x.m_args[0])}))
                + " : " + type(x.m_type, loc) + ")", pAtom};
        }
        if (x.n_args == 1 && (f == "adjustl" || f == "adjustr") && is_string(x.m_args[0])) {
            return app("FStr." + f, {lower(x.m_args[0])});
        }
        if (x.n_args == 2 && f == "substrindex" && is_string(x.m_args[0]) && is_string(x.m_args[1])) {
            return {"(FInt.ofInt " + atom(app("FStr.index",
                {lower(x.m_args[0]), lower(x.m_args[1])}))
                + " : " + type(x.m_type, loc) + ")", pAtom};
        }
        if (x.n_args == 2 && f == "stringconcat" && is_string(x.m_args[0]) && is_string(x.m_args[1])) {
            return {"(" + atom(lower(x.m_args[0])) + " ++ " + atom(lower(x.m_args[1])) + ")", pAtom};
        }
        if (x.n_args == 2 && f == "repeat" && is_string(x.m_args[0]) && is_int(x.m_args[1])) {
            return app("FStr.rep", {lower(x.m_args[0]), int_expr(x.m_args[1])});
        }
        if (x.n_args == 2 && (f == "lge" || f == "lgt" || f == "lle" || f == "llt")
                && is_string(x.m_args[0]) && is_string(x.m_args[1])) {
            Val a = lower(x.m_args[0]), b = lower(x.m_args[1]);
            if (f == "llt") return app("FStr.lt", {a, b});
            if (f == "lle") return app("FStr.le", {a, b});
            if (f == "lgt") return app("FStr.lt", {b, a});
            return app("FStr.le", {b, a});
        }
        if (x.n_args == 2 && f == "atan2" && is_real(x.m_args[0]) && is_real(x.m_args[1])) {
            level = std::max(level, 1);
            return app("FRealMath.atan2", {operand(x.m_args[0]), lower(x.m_args[1])});
        }
        if (x.n_args == 1 && is_array(ASRUtils::expr_type(x.m_args[0]))) {
            ASR::ttype_t *et = elem_type(ASRUtils::expr_type(x.m_args[0]));
            std::string body;
            if (f == "abs") {
                if (ASR::is_a<ASR::Real_t>(*et)) body = "FReal.abs x'";
                else if (ASR::is_a<ASR::Integer_t>(*et)) {
                    body = "(FInt.ofInt (Int.natAbs (FInt.toInt x')) : " + type(et, loc) + ")";
                }
            } else if (math.count(f) && ASR::is_a<ASR::Real_t>(*et)) {
                level = std::max(level, 1);
                body = "FRealMath." + f + " x'";
            } else if ((f == "int" || f == "floor" || f == "ceiling" || f == "nint")
                    && ASR::is_a<ASR::Real_t>(*et)) {
                std::string h = f == "int" ? "FReal.toInt" : f == "floor" ? "FReal.floorInt"
                    : f == "ceiling" ? "FReal.ceilInt" : "FReal.nintInt";
                body = "(FInt.ofInt (" + h + " x') : " + type(elem_type(x.m_type), loc) + ")";
            }
            if (!body.empty()) {
                return app("Arr.map", {{"(fun x' => " + body + ")", pAtom}, lower(x.m_args[0])});
            }
        }
        if (x.m_value && x.m_value != (ASR::expr_t*) &x) return lower(x.m_value);
        unsupported("the intrinsic '" + f + "' is", loc);
    }

    Val cast(const ASR::Cast_t &x, const Location &loc) {
        if (x.m_value && x.m_value != (ASR::expr_t*) &x) return lower(x.m_value);
        if (is_array(ASRUtils::expr_type(x.m_arg))) {
            ASR::ttype_t *dst_et = elem_type(x.m_type);
            ASR::ttype_t *src_et = elem_type(ASRUtils::expr_type(x.m_arg));
            std::string dty = type(dst_et, loc);
            type(src_et, x.m_arg->base.loc);
            std::string body;
            switch (x.m_kind) {
                case ASR::cast_kindType::IntegerToReal:
                    body = "(FReal.ofInt (FInt.toInt x') : " + dty + ")";
                    break;
                case ASR::cast_kindType::IntegerToInteger:
                    body = "(FInt.ofInt (FInt.toInt x') : " + dty + ")";
                    break;
                case ASR::cast_kindType::RealToInteger:
                    body = "(FInt.ofInt (FReal.toInt x') : " + dty + ")";
                    break;
                case ASR::cast_kindType::RealToReal: {
                    int dk = ASR::down_cast<ASR::Real_t>(dst_et)->m_kind;
                    int sk = ASR::down_cast<ASR::Real_t>(src_et)->m_kind;
                    if (sk == dk) return lower(x.m_arg);
                    body = dk == 8 ? "FKinds.r4to8 x'" : "FKinds.r8to4 x'";
                    break;
                }
                default:
                    unsupported("this conversion is", loc);
            }
            return app("Arr.map", {{"(fun x' => " + body + ")", pAtom}, lower(x.m_arg)});
        }
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
                type(x.m_type, loc);
                type(ASRUtils::expr_type(x.m_arg), x.m_arg->base.loc);
                int dst_k = ASR::down_cast<ASR::Real_t>(x.m_type)->m_kind;
                int src_k = ASR::down_cast<ASR::Real_t>(ASRUtils::expr_type(x.m_arg))->m_kind;
                if (src_k == dst_k) return lower(x.m_arg);
                return app(dst_k == 8 ? "FKinds.r4to8" : "FKinds.r8to4",
                    {operand(x.m_arg)});
            }
            case ASR::cast_kindType::RealToComplex: {
                type(x.m_type, loc);
                type(ASRUtils::expr_type(x.m_arg), x.m_arg->base.loc);
                int dst_k = ASR::down_cast<ASR::Complex_t>(x.m_type)->m_kind;
                int src_k = ASR::down_cast<ASR::Real_t>(ASRUtils::expr_type(x.m_arg))->m_kind;
                Val r = src_k == dst_k ? lower(x.m_arg)
                    : app(dst_k == 8 ? "FKinds.r4to8" : "FKinds.r8to4", {operand(x.m_arg)});
                return app("FCplx.ofReal", {r});
            }
            case ASR::cast_kindType::IntegerToComplex: {
                type(x.m_type, loc);
                Val r = ASR::is_a<ASR::IntegerConstant_t>(*x.m_arg)
                    ? Val{"FReal.ofInt " + int_lit(
                        ASR::down_cast<ASR::IntegerConstant_t>(x.m_arg)->m_n), pApp}
                    : app("FReal.ofInt", {app("FInt.toInt", {operand(x.m_arg)})});
                return app("FCplx.ofReal", {r});
            }
            case ASR::cast_kindType::ComplexToReal: {
                type(x.m_type, loc);
                type(ASRUtils::expr_type(x.m_arg), x.m_arg->base.loc);
                int dst_k = ASR::down_cast<ASR::Real_t>(x.m_type)->m_kind;
                int src_k = ASR::down_cast<ASR::Complex_t>(ASRUtils::expr_type(x.m_arg))->m_kind;
                std::string re = atom(operand(x.m_arg)) + ".re";
                if (src_k == dst_k) return {re, pAtom};
                return app(dst_k == 8 ? "FKinds.r4to8" : "FKinds.r8to4", {{re, pAtom}});
            }
            case ASR::cast_kindType::ComplexToInteger:
                type(ASRUtils::expr_type(x.m_arg), x.m_arg->base.loc);
                return {"(FInt.ofInt " + atom(app("FReal.toInt",
                    {{atom(operand(x.m_arg)) + ".re", pAtom}}))
                    + " : " + type(x.m_type, loc) + ")", pAtom};
            case ASR::cast_kindType::ComplexToComplex: {
                type(x.m_type, loc);
                type(ASRUtils::expr_type(x.m_arg), x.m_arg->base.loc);
                int dst_k = ASR::down_cast<ASR::Complex_t>(x.m_type)->m_kind;
                int src_k = ASR::down_cast<ASR::Complex_t>(ASRUtils::expr_type(x.m_arg))->m_kind;
                if (src_k == dst_k) return lower(x.m_arg);
                std::string fn = dst_k == 8 ? "FKinds.r4to8" : "FKinds.r8to4";
                return {"((fun z' => FCplx.mk (" + fn + " z'.re) (" + fn + " z'.im)) "
                    + atom(operand(x.m_arg)) + ")", pAtom};
            }
            case ASR::cast_kindType::StringToString:
                return lower(x.m_arg);
            default:
                unsupported("this conversion is", loc);
        }
    }

    // The subscripts of an array element, as `#[i₁, …]`.
    std::string index(const ASR::ArrayItem_t &x) {
        std::vector<std::string> idx;
        for (size_t i = 0; i < x.n_args; i++) {
            const ASR::array_index_t &a = x.m_args[i];
            if (a.m_left || a.m_step || !a.m_right) {
                unsupported("an array section is", x.base.base.loc);
            }
            idx.push_back(int_expr(a.m_right).src);
        }
        return int_array(idx);
    }

    Val intrinsic_array(const ASR::IntrinsicArrayFunction_t &x, const Location &loc) {
        if (x.m_value && x.m_value != (ASR::expr_t*) &x) return lower(x.m_value);
        using F = ASRUtils::IntrinsicArrayFunctions;
        F id = static_cast<F>(x.m_arr_intrinsic_id);
        if (id == F::Sum && x.n_args == 1 && is_array(ASRUtils::expr_type(x.m_args[0]))) {
            ASR::ttype_t *et = elem_type(ASRUtils::expr_type(x.m_args[0]));
            std::string a = atom(lower(x.m_args[0]));
            if (ASR::is_a<ASR::Integer_t>(*et)) {
                return {"(" + a + ".data.foldl FInt.add (FInt.ofInt 0) : "
                    + type(x.m_type, loc) + ")", pAtom};
            }
            if (ASR::is_a<ASR::Real_t>(*et)) {
                return {"(" + a + ".data.foldl FReal.add (FReal.ofInt 0) : "
                    + type(x.m_type, loc) + ")", pAtom};
            }
            if (ASR::is_a<ASR::Complex_t>(*et)) {
                return {"(" + a + ".data.foldl FCplx.add FCplx.zero : "
                    + type(x.m_type, loc) + ")", pAtom};
            }
        }
        if (id == F::Product && x.n_args == 1 && is_array(ASRUtils::expr_type(x.m_args[0]))) {
            ASR::ttype_t *et = elem_type(ASRUtils::expr_type(x.m_args[0]));
            std::string a = atom(lower(x.m_args[0]));
            if (ASR::is_a<ASR::Integer_t>(*et)) {
                return {"(" + a + ".data.foldl FInt.mul (FInt.ofInt 1) : "
                    + type(x.m_type, loc) + ")", pAtom};
            }
            if (ASR::is_a<ASR::Real_t>(*et)) {
                return {"(" + a + ".data.foldl FReal.mul (FReal.ofInt 1) : "
                    + type(x.m_type, loc) + ")", pAtom};
            }
            if (ASR::is_a<ASR::Complex_t>(*et)) {
                return {"(" + a + ".data.foldl FCplx.mul (FCplx.ofReal (FReal.ofInt 1)) : "
                    + type(x.m_type, loc) + ")", pAtom};
            }
        }
        if ((id == F::Any || id == F::All) && x.n_args == 1
                && is_array(ASRUtils::expr_type(x.m_args[0]))
                && ASR::is_a<ASR::Logical_t>(*elem_type(ASRUtils::expr_type(x.m_args[0])))) {
            std::string a = atom(lower(x.m_args[0]));
            return {a + ".data." + (id == F::Any ? "any" : "all") + " (fun b' => b')", pApp};
        }
        if (id == F::Count && x.n_args == 1
                && is_array(ASRUtils::expr_type(x.m_args[0]))
                && ASR::is_a<ASR::Logical_t>(*elem_type(ASRUtils::expr_type(x.m_args[0])))) {
            std::string a = atom(lower(x.m_args[0]));
            return {"(FInt.ofInt (Int.ofNat (" + a
                + ".data.foldl (fun n' b' => if b' then n' + 1 else n') 0)) : "
                + type(x.m_type, loc) + ")", pAtom};
        }
        if (id == F::DotProduct && x.n_args == 2
                && is_array(ASRUtils::expr_type(x.m_args[0]))
                && is_array(ASRUtils::expr_type(x.m_args[1]))) {
            ASR::ttype_t *et0 = elem_type(ASRUtils::expr_type(x.m_args[0]));
            ASR::ttype_t *et1 = elem_type(ASRUtils::expr_type(x.m_args[1]));
            std::string a = atom(lower(x.m_args[0]));
            std::string b = atom(lower(x.m_args[1]));
            if (ASR::is_a<ASR::Integer_t>(*et0) && ASR::is_a<ASR::Integer_t>(*et1)
                    && ASR::down_cast<ASR::Integer_t>(et0)->m_kind
                        == ASR::down_cast<ASR::Integer_t>(et1)->m_kind) {
                return {"((Array.zipWith FInt.mul " + a + ".data " + b
                    + ".data).foldl FInt.add (FInt.ofInt 0) : " + type(x.m_type, loc) + ")", pAtom};
            }
            if (ASR::is_a<ASR::Real_t>(*et0) && ASR::is_a<ASR::Real_t>(*et1)
                    && ASR::down_cast<ASR::Real_t>(et0)->m_kind
                        == ASR::down_cast<ASR::Real_t>(et1)->m_kind) {
                return {"((Array.zipWith FReal.mul " + a + ".data " + b
                    + ".data).foldl FReal.add (FReal.ofInt 0) : " + type(x.m_type, loc) + ")", pAtom};
            }
            if (ASR::is_a<ASR::Logical_t>(*et0) && ASR::is_a<ASR::Logical_t>(*et1)) {
                return {"((Array.zipWith (fun x' y' => x' && y') " + a + ".data " + b
                    + ".data).any (fun b' => b'))", pAtom};
            }
        }
        if ((id == F::MaxVal || id == F::MinVal) && x.n_args == 1
                && is_array(ASRUtils::expr_type(x.m_args[0]))) {
            ASR::ttype_t *et = elem_type(ASRUtils::expr_type(x.m_args[0]));
            std::string a = atom(lower(x.m_args[0]));
            std::string ty = type(x.m_type, loc);
            std::string empty_val;
            std::string fn;
            if (ASR::is_a<ASR::Integer_t>(*et)) {
                int k = ASR::down_cast<ASR::Integer_t>(et)->m_kind;
                int64_t hi = k == 1 ? INT8_MAX : k == 2 ? INT16_MAX : k == 4 ? INT32_MAX : INT64_MAX;
                int64_t lo = k == 1 ? INT8_MIN : k == 2 ? INT16_MIN : k == 4 ? INT32_MIN : INT64_MIN;
                empty_val = "FInt.ofInt " + int_lit(id == F::MaxVal ? lo : hi);
                fn = id == F::MaxVal ? "FInt.fmax" : "FInt.fmin";
            } else if (ASR::is_a<ASR::Real_t>(*et)) {
                int k = ASR::down_cast<ASR::Real_t>(et)->m_kind;
                double h = k == 4 ? static_cast<double>(std::numeric_limits<float>::max())
                    : std::numeric_limits<double>::max();
                std::string hv = real_lit(h, k, loc);
                empty_val = id == F::MaxVal ? "FReal.neg (" + hv + ")" : hv;
                fn = id == F::MaxVal ? "FReal.fmax" : "FReal.fmin";
            }
            if (!fn.empty()) {
                return {"((let d' := " + a + ".data; if d'.isEmpty then " + empty_val
                    + " else d'.foldl " + fn + " d'[0]!) : " + ty + ")", pAtom};
            }
        }
        unsupported("this array intrinsic is", loc);
    }

    Val array_section(const ASR::ArraySection_t &x, const Location &loc) {
        std::string a = atom(lower(x.m_v));
        std::vector<std::string> specs;
        for (size_t i = 0; i < x.n_args; i++) {
            const ASR::array_index_t &idx = x.m_args[i];
            std::string dim = std::to_string(i + 1);
            if (!idx.m_left && !idx.m_step && idx.m_right) {
                specs.push_back("Sum.inl " + atom(int_expr(idx.m_right)));
            } else {
                std::string lo = idx.m_left ? int_expr(idx.m_left).src
                    : "Arr.lbound " + a + " " + dim;
                std::string hi = idx.m_right ? int_expr(idx.m_right).src
                    : "Arr.ubound " + a + " " + dim;
                std::string st = idx.m_step ? int_expr(idx.m_step).src : "1";
                specs.push_back("Sum.inr (" + lo + ", " + hi + ", " + st + ")");
            }
        }
        return app("Arr.slice", {{a, pAtom}, {int_array(specs), pAtom}});
    }

    Val lower(ASR::expr_t *e) {
        const Location &loc = e->base.loc;
        switch (e->type) {
            case ASR::exprType::ArrayPhysicalCast:
                return lower(ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg);
            case ASR::exprType::ArrayConstant:
                return array_constant(*ASR::down_cast<ASR::ArrayConstant_t>(e), loc);
            case ASR::exprType::ArrayConstructor:
                return array_constructor(*ASR::down_cast<ASR::ArrayConstructor_t>(e), loc);
            case ASR::exprType::ArraySection:
                return array_section(*ASR::down_cast<ASR::ArraySection_t>(e), loc);
            case ASR::exprType::IntrinsicArrayFunction:
                return intrinsic_array(*ASR::down_cast<ASR::IntrinsicArrayFunction_t>(e), loc);
            case ASR::exprType::ArrayItem: {
                auto &x = *ASR::down_cast<ASR::ArrayItem_t>(e);
                return app("Arr.get", {lower(x.m_v), {index(x), pAtom}});
            }
            case ASR::exprType::ArraySize: {
                auto &x = *ASR::down_cast<ASR::ArraySize_t>(e);
                std::string a = atom(lower(x.m_v));
                std::string sz = x.m_dim ? "Arr.sizeDim " + a + " " + atom(int_expr(x.m_dim))
                    : "Arr.size " + a;
                return {"(FInt.ofInt (" + sz + ") : " + type(x.m_type, loc) + ")", pAtom};
            }
            case ASR::exprType::ArrayBound: {
                auto &x = *ASR::down_cast<ASR::ArrayBound_t>(e);
                if (!x.m_dim) unsupported("this array bound is", loc);
                std::string f = x.m_bound == ASR::arrayboundType::LBound
                    ? "Arr.lbound " : "Arr.ubound ";
                return {"(FInt.ofInt (" + f + atom(lower(x.m_v)) + " "
                    + atom(int_expr(x.m_dim)) + ") : " + type(x.m_type, loc) + ")", pAtom};
            }
            case ASR::exprType::Var: {
                ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(e)->m_v);
                if (!ASR::is_a<ASR::Variable_t>(*s)) unsupported("this name is", loc);
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
                if (v->m_storage == ASR::storage_typeType::Parameter) {
                    if (!v->m_value) unsupported("this parameter is", loc);
                    if (is_array(v->m_type)) {
                        std::vector<std::string> lbs, exts;
                        bool assumed;
                        dims(v->m_type, lbs, exts, assumed, loc);
                        if (assumed) {
                            return app("Arr.rebase", {lower(v->m_value), {int_array(lbs), pAtom}});
                        }
                        return app("Arr.reshape", {lower(v->m_value), {int_array(lbs), pAtom},
                            {int_array(exts), pAtom}});
                    }
                    return lower(v->m_value);
                }
                if (v->m_parent_symtab != cur_scope()) {
                    unsupported("a variable from another scope is", loc);
                }
                return {name(v->m_name, v->base.base.loc), pAtom};
            }
            case ASR::exprType::IntegerConstant: {
                auto &x = *ASR::down_cast<ASR::IntegerConstant_t>(e);
                type(x.m_type, loc);
                return {"FInt.ofInt " + int_lit(x.m_n), pApp};
            }
            case ASR::exprType::RealConstant: {
                auto &x = *ASR::down_cast<ASR::RealConstant_t>(e);
                type(x.m_type, loc);
                return {real_lit(x.m_r, ASR::down_cast<ASR::Real_t>(x.m_type)->m_kind,
                    loc), pApp};
            }
            case ASR::exprType::ComplexConstant: {
                auto &x = *ASR::down_cast<ASR::ComplexConstant_t>(e);
                type(x.m_type, loc);
                int k = ASR::down_cast<ASR::Complex_t>(x.m_type)->m_kind;
                return app("FCplx.mk", {{real_lit(x.m_re, k, loc), pApp},
                    {real_lit(x.m_im, k, loc), pApp}});
            }
            case ASR::exprType::ComplexConstructor: {
                auto &x = *ASR::down_cast<ASR::ComplexConstructor_t>(e);
                type(x.m_type, loc);
                int k = ASR::down_cast<ASR::Complex_t>(x.m_type)->m_kind;
                auto part = [&](ASR::expr_t *p) -> Val {
                    if (is_int(p)) {
                        if (ASR::is_a<ASR::IntegerConstant_t>(*p)) {
                            return {"FReal.ofInt " + int_lit(
                                ASR::down_cast<ASR::IntegerConstant_t>(p)->m_n), pApp};
                        }
                        return app("FReal.ofInt", {app("FInt.toInt", {operand(p)})});
                    }
                    if (is_real(p)) {
                        type(ASRUtils::expr_type(p), p->base.loc);
                        int pk = ASR::down_cast<ASR::Real_t>(ASRUtils::expr_type(p))->m_kind;
                        if (pk == k) return lower(p);
                        return app(k == 8 ? "FKinds.r4to8" : "FKinds.r8to4", {operand(p)});
                    }
                    unsupported("this complex constructor is", p->base.loc);
                };
                return app("FCplx.mk", {part(x.m_re), part(x.m_im)});
            }
            case ASR::exprType::ComplexRe: {
                auto &x = *ASR::down_cast<ASR::ComplexRe_t>(e);
                if (is_array(x.m_type)) {
                    return app("Arr.map", {{"(fun z' => z'.re)", pAtom}, lower(x.m_arg)});
                }
                return {atom(operand(x.m_arg)) + ".re", pAtom};
            }
            case ASR::exprType::ComplexIm: {
                auto &x = *ASR::down_cast<ASR::ComplexIm_t>(e);
                if (is_array(x.m_type)) {
                    return app("Arr.map", {{"(fun z' => z'.im)", pAtom}, lower(x.m_arg)});
                }
                return {atom(operand(x.m_arg)) + ".im", pAtom};
            }
            case ASR::exprType::ComplexUnaryMinus: {
                auto &x = *ASR::down_cast<ASR::ComplexUnaryMinus_t>(e);
                if (is_array(x.m_type)) {
                    return app("Arr.map", {{"FCplx.neg", pAtom}, lower(x.m_arg)});
                }
                if (ASR::expr_t *v = ASRUtils::expr_value(e); v && v != e) return lower(v);
                return app("FCplx.neg", {lower(x.m_arg)});
            }
            case ASR::exprType::ComplexBinOp: {
                auto &x = *ASR::down_cast<ASR::ComplexBinOp_t>(e);
                const char *f = nullptr;
                switch (x.m_op) {
                    case ASR::binopType::Add: f = "FCplx.add"; break;
                    case ASR::binopType::Sub: f = "FCplx.sub"; break;
                    case ASR::binopType::Mul: f = "FCplx.mul"; break;
                    case ASR::binopType::Div: f = "FCplx.div"; break;
                    case ASR::binopType::Pow:
                        if (!is_array(x.m_type) && is_int(x.m_right)) {
                            return app("FCplx.powInt", {lower(x.m_left), int_expr(x.m_right)});
                        }
                        unsupported("this complex operator is", loc);
                    default: unsupported("this complex operator is", loc);
                }
                if (is_array(x.m_type)) {
                    return lift_binop(x.m_left, x.m_right,
                        [&](const std::string &a, const std::string &b) {
                            return std::string(f) + " " + a + " " + b;
                        });
                }
                return app(f, {lower(x.m_left), lower(x.m_right)});
            }
            case ASR::exprType::ComplexCompare: {
                auto &x = *ASR::down_cast<ASR::ComplexCompare_t>(e);
                if (x.m_op != ASR::cmpopType::Eq && x.m_op != ASR::cmpopType::NotEq) {
                    unsupported("this complex comparison is", loc);
                }
                if (is_array(x.m_type)) {
                    return lift_binop(x.m_left, x.m_right,
                        [&](const std::string &a, const std::string &b) {
                            return x.m_op == ASR::cmpopType::Eq
                                ? "FCplx.beq " + a + " " + b
                                : "!(FCplx.beq " + a + " " + b + ")";
                        });
                }
                Val eq = app("FCplx.beq", {operand(x.m_left), lower(x.m_right)});
                return x.m_op == ASR::cmpopType::Eq ? eq : Val{"!" + atom(eq), pApp};
            }
            case ASR::exprType::LogicalConstant:
                return {ASR::down_cast<ASR::LogicalConstant_t>(e)->m_value
                    ? "true" : "false", pAtom};
            case ASR::exprType::StringConstant: {
                auto &x = *ASR::down_cast<ASR::StringConstant_t>(e);
                type(x.m_type, loc);
                return {string_lit(x.m_s), pAtom};
            }
            case ASR::exprType::StringPhysicalCast:
                return lower(ASR::down_cast<ASR::StringPhysicalCast_t>(e)->m_arg);
            case ASR::exprType::StringConcat: {
                auto &x = *ASR::down_cast<ASR::StringConcat_t>(e);
                if (is_array(x.m_type)) unsupported("this array expression is", loc);
                return {"(" + atom(lower(x.m_left)) + " ++ " + atom(lower(x.m_right)) + ")", pAtom};
            }
            case ASR::exprType::StringRepeat: {
                auto &x = *ASR::down_cast<ASR::StringRepeat_t>(e);
                return app("FStr.rep", {lower(x.m_left), int_expr(x.m_right)});
            }
            case ASR::exprType::StringLen: {
                auto &x = *ASR::down_cast<ASR::StringLen_t>(e);
                ASR::ttype_t *at = ASRUtils::expr_type(x.m_arg);
                if (is_array(at)) {
                    ASR::ttype_t *et = elem_type(at);
                    if (!ASR::is_a<ASR::String_t>(*et)) unsupported("this string length is", loc);
                    ASR::String_t *st = ASR::down_cast<ASR::String_t>(et);
                    if (st->m_len) {
                        return {"(FInt.ofInt (Int.ofNat " + extent(st->m_len) + ") : "
                            + type(x.m_type, loc) + ")", pAtom};
                    }
                    unsupported("assumed-length character array len is", loc);
                }
                return {"(FInt.ofInt " + atom(app("FStr.len", {lower(x.m_arg)}))
                    + " : " + type(x.m_type, loc) + ")", pAtom};
            }
            case ASR::exprType::StringItem: {
                auto &x = *ASR::down_cast<ASR::StringItem_t>(e);
                return app("FStr.item", {lower(x.m_arg), int_expr(x.m_idx)});
            }
            case ASR::exprType::StringSection: {
                auto &x = *ASR::down_cast<ASR::StringSection_t>(e);
                bool unit_step = !x.m_step
                    || (ASR::is_a<ASR::IntegerConstant_t>(*x.m_step)
                        && ASR::down_cast<ASR::IntegerConstant_t>(x.m_step)->m_n == 1);
                if (!unit_step) unsupported("a string section step is", loc);
                std::string s = atom(lower(x.m_arg));
                std::string lo = x.m_start ? atom(int_expr(x.m_start)) : "1";
                if (!x.m_end) {
                    return {"((fun s' lo' => FStr.slice s' lo' (FStr.len s')) " + s + " " + lo + ")", pAtom};
                }
                return app("FStr.slice", {{s, pAtom}, {lo, pAtom}, {atom(int_expr(x.m_end)), pAtom}});
            }
            case ASR::exprType::StringCompare: {
                auto &x = *ASR::down_cast<ASR::StringCompare_t>(e);
                if (is_array(x.m_type)) unsupported("this array expression is", loc);
                Val a = lower(x.m_left), b = lower(x.m_right);
                switch (x.m_op) {
                    case ASR::cmpopType::Eq: return app("FStr.beq", {a, b});
                    case ASR::cmpopType::NotEq: return {"!" + atom(app("FStr.beq", {a, b})), pApp};
                    case ASR::cmpopType::Lt: return app("FStr.lt", {a, b});
                    case ASR::cmpopType::LtE: return app("FStr.le", {a, b});
                    case ASR::cmpopType::Gt: return app("FStr.lt", {b, a});
                    case ASR::cmpopType::GtE: return app("FStr.le", {b, a});
                }
                return app("FStr.beq", {a, b});
            }
            case ASR::exprType::StringOrd: {
                auto &x = *ASR::down_cast<ASR::StringOrd_t>(e);
                return {"(FInt.ofInt " + atom(app("FStr.ord", {lower(x.m_arg)}))
                    + " : " + type(x.m_type, loc) + ")", pAtom};
            }
            case ASR::exprType::StringChr: {
                auto &x = *ASR::down_cast<ASR::StringChr_t>(e);
                return app("FStr.chr", {int_expr(x.m_arg)});
            }
            case ASR::exprType::IntegerBinOp: {
                auto &x = *ASR::down_cast<ASR::IntegerBinOp_t>(e);
                const char *f = nullptr;
                switch (x.m_op) {
                    case ASR::binopType::Add: f = "FInt.add"; break;
                    case ASR::binopType::Sub: f = "FInt.sub"; break;
                    case ASR::binopType::Mul: f = "FInt.mul"; break;
                    case ASR::binopType::Div: f = "FInt.div"; break;
                    case ASR::binopType::Pow: f = "FInt.pow"; break;
                    default: unsupported("this integer operator is", loc);
                }
                if (is_array(x.m_type)) {
                    return lift_binop(x.m_left, x.m_right,
                        [&](const std::string &a, const std::string &b) {
                            return std::string(f) + " " + a + " " + b;
                        });
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
                    case ASR::binopType::Pow:
                        if (is_array(x.m_type)) {
                            if (ASR::is_a<ASR::Integer_t>(*elem_type(ASRUtils::expr_type(x.m_right)))) {
                                return lift_binop(x.m_left, x.m_right,
                                    [](const std::string &a, const std::string &b) {
                                        return "FReal.powInt " + a + " (FInt.toInt " + b + ")";
                                    });
                            }
                            level = std::max(level, 1);
                            return lift_binop(x.m_left, x.m_right,
                                [](const std::string &a, const std::string &b) {
                                    return "FRealMath.pow " + a + " " + b;
                                });
                        }
                        if (is_int(x.m_right)) {
                            return app("FReal.powInt", {lower(x.m_left), int_expr(x.m_right)});
                        }
                        level = std::max(level, 1);
                        f = "FRealMath.pow";
                        break;
                    default: unsupported("this real operator is", loc);
                }
                if (is_array(x.m_type)) {
                    return lift_binop(x.m_left, x.m_right,
                        [&](const std::string &a, const std::string &b) {
                            return std::string(f) + " " + a + " " + b;
                        });
                }
                return app(f, {lower(x.m_left), lower(x.m_right)});
            }
            case ASR::exprType::IntegerUnaryMinus: {
                auto &x = *ASR::down_cast<ASR::IntegerUnaryMinus_t>(e);
                if (is_array(x.m_type)) {
                    return app("Arr.map", {{"(fun x' => FInt.sub (FInt.ofInt 0) x')", pAtom},
                        lower(x.m_arg)});
                }
                if (ASR::expr_t *v = ASRUtils::expr_value(e)) return lower(v);
                return app("FInt.sub", {{"FInt.ofInt 0", pApp}, lower(x.m_arg)});
            }
            case ASR::exprType::RealUnaryMinus: {
                auto &x = *ASR::down_cast<ASR::RealUnaryMinus_t>(e);
                if (is_array(x.m_type)) {
                    return app("Arr.map", {{"FReal.neg", pAtom}, lower(x.m_arg)});
                }
                if (ASR::expr_t *v = ASRUtils::expr_value(e)) return lower(v);
                return app("FReal.neg", {lower(x.m_arg)});
            }
            case ASR::exprType::IntegerCompare: {
                auto &x = *ASR::down_cast<ASR::IntegerCompare_t>(e);
                if (is_array(x.m_type)) {
                    return lift_binop(x.m_left, x.m_right,
                        [&](const std::string &a, const std::string &b) {
                            switch (x.m_op) {
                                case ASR::cmpopType::Eq: return "FInt.beq " + a + " " + b;
                                case ASR::cmpopType::NotEq: return "!(FInt.beq " + a + " " + b + ")";
                                case ASR::cmpopType::Lt: return "FInt.lt " + a + " " + b;
                                case ASR::cmpopType::LtE: return "FInt.le " + a + " " + b;
                                case ASR::cmpopType::Gt: return "FInt.lt " + b + " " + a;
                                case ASR::cmpopType::GtE: return "FInt.le " + b + " " + a;
                            }
                            return "FInt.beq " + a + " " + b;
                        });
                }
                return compare(x.m_left, x.m_op, x.m_right);
            }
            case ASR::exprType::RealCompare: {
                auto &x = *ASR::down_cast<ASR::RealCompare_t>(e);
                if (is_array(x.m_type)) {
                    return lift_binop(x.m_left, x.m_right,
                        [&](const std::string &a, const std::string &b) {
                            switch (x.m_op) {
                                case ASR::cmpopType::Eq: return "FReal.beq " + a + " " + b;
                                case ASR::cmpopType::NotEq: return "!(FReal.beq " + a + " " + b + ")";
                                case ASR::cmpopType::Lt: return "FReal.lt " + a + " " + b;
                                case ASR::cmpopType::LtE: return "FReal.le " + a + " " + b;
                                case ASR::cmpopType::Gt: return "FReal.lt " + b + " " + a;
                                case ASR::cmpopType::GtE: return "FReal.le " + b + " " + a;
                            }
                            return "FReal.beq " + a + " " + b;
                        });
                }
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
                if (is_array(x.m_type)) {
                    return lift_binop(x.m_left, x.m_right,
                        [&](const std::string &a, const std::string &b) {
                            return "(" + a + " " + op + " " + b + ")";
                        });
                }
                return {atom(lower(x.m_left)) + " " + op + " "
                    + atom(lower(x.m_right)), p};
            }
            case ASR::exprType::LogicalNot: {
                auto &x = *ASR::down_cast<ASR::LogicalNot_t>(e);
                if (is_array(x.m_type)) {
                    return app("Arr.map", {{"(fun b' => !b')", pAtom}, lower(x.m_arg)});
                }
                return {"!" + atom(lower(x.m_arg)), pApp};
            }
            case ASR::exprType::Cast:
                return cast(*ASR::down_cast<ASR::Cast_t>(e), loc);
            case ASR::exprType::FunctionCall: {
                auto &x = *ASR::down_cast<ASR::FunctionCall_t>(e);
                if (x.m_dt) unsupported("a type-bound call is", loc);
                ASR::Function_t *fn = callee(x.m_name, loc);
                Val c = call(x.m_name, x.m_args, x.n_args, loc);
                if (io_procs.count(fn)) return {"(← " + c.src + ")", pAtom};
                return c;
            }
            case ASR::exprType::IntrinsicElementalFunction:
                return intrinsic(*ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e),
                    loc);
            default:
                if (ASR::expr_t *v = ASRUtils::expr_value(e); v && v != e) {
                    return lower(v);
                }
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
        if (ASR::is_a<ASR::Var_t>(*e)) {
            ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(e)->m_v);
            if (ASR::is_a<ASR::Variable_t>(*s)) {
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
                if (v->m_storage == ASR::storage_typeType::Parameter && v->m_value
                        && ASR::is_a<ASR::StringConstant_t>(*v->m_value)) {
                    return string_lit(ASR::down_cast<ASR::StringConstant_t>(v->m_value)->m_s);
                }
            }
        }
        if (ASR::is_a<ASR::IntegerConstant_t>(*e)) {
            return string_lit(std::to_string(
                ASR::down_cast<ASR::IntegerConstant_t>(e)->m_n));
        }
        if (is_array(ASRUtils::expr_type(e))) {
            ASR::ttype_t *et = scalar(elem_type(ASRUtils::expr_type(e)), loc);
            std::string ety = type(et, loc);
            std::string fmt;
            if (ASR::is_a<ASR::Integer_t>(*et)) fmt = "toString (FInt.toInt (x' : " + ety + "))";
            else if (ASR::is_a<ASR::Real_t>(*et)) {
                fmt = "FKindsIO.fmtR" + std::to_string(ASR::down_cast<ASR::Real_t>(et)->m_kind)
                    + " (x' : " + ety + ")";
            } else if (ASR::is_a<ASR::Complex_t>(*et)) {
                std::string fn = "FKindsIO.fmtR" + std::to_string(ASR::down_cast<ASR::Complex_t>(et)->m_kind);
                fmt = "\"(\" ++ " + fn + " (x' : " + ety + ").re ++ \",\" ++ " + fn + " x'.im ++ \")\"";
            } else if (ASR::is_a<ASR::Logical_t>(*et)) fmt = "(if x' then \"T\" else \"F\")";
            else if (ASR::is_a<ASR::String_t>(*et)) fmt = "x'";
            else unsupported("printing this value is", loc);
            return "(String.intercalate \" \" (" + atom(lower(e))
                + ".data.toList.map (fun x' => " + fmt + ")))";
        }
        if (is_string(e)) {
            return lower(e).src;
        }
        ASR::ttype_t *t = scalar(ASRUtils::expr_type(e), loc);
        // The kind is stated: a value such as `FInt.ofInt 10` does not fix it.
        std::string v = "(" + lower(e).src + " : " + type(t, loc) + ")";
        if (ASR::is_a<ASR::Integer_t>(*t)) {
            return "toString (FInt.toInt " + v + ")";
        }
        if (ASR::is_a<ASR::Real_t>(*t)) {
            int k = ASR::down_cast<ASR::Real_t>(t)->m_kind;
            return "FKindsIO.fmtR" + std::to_string(k) + " " + v;
        }
        if (ASR::is_a<ASR::Complex_t>(*t)) {
            int k = ASR::down_cast<ASR::Complex_t>(t)->m_kind;
            std::string fn = "FKindsIO.fmtR" + std::to_string(k);
            return "((fun z' => \"(\" ++ " + fn + " z'.re ++ \",\" ++ " + fn
                + " z'.im ++ \")\") " + v + ")";
        }
        if (ASR::is_a<ASR::Logical_t>(*t)) {
            return "(if " + lower(e).src + " then \"T\" else \"F\")";
        }
        if (ASR::is_a<ASR::String_t>(*t)) {
            return lower(e).src;
        }
        unsupported("printing this value is", loc);
    }

    void halt(ASR::expr_t *code, bool error, const Location &loc) {
        if (!in_program && !(cur && io_procs.count(cur->fn))) {
            unsupported("stop in a procedure is", loc);
        }
        if (!code) {
            line(std::string("LFortran.halt ") + (error ? "1" : "0"));
            return;
        }
        if (ASR::is_a<ASR::IntegerConstant_t>(*code)) {
            line("LFortran.halt (LFortran.stopCode " + int_lit(
                ASR::down_cast<ASR::IntegerConstant_t>(code)->m_n) + ")");
            return;
        }
        if (is_int(code)) {
            line("LFortran.halt (LFortran.stopCode " + atom(app("FInt.toInt", {operand(code)})) + ")");
            return;
        }
        unsupported("this stop code is", loc);
    }

    static bool body_has_exit(ASR::stmt_t **body, size_t n) {
        for (size_t i = 0; i < n; i++) {
            ASR::stmt_t *s = body[i];
            if (ASR::is_a<ASR::Exit_t>(*s)) return true;
            if (ASR::is_a<ASR::If_t>(*s)) {
                auto &d = *ASR::down_cast<ASR::If_t>(s);
                if (body_has_exit(d.m_body, d.n_body) || body_has_exit(d.m_orelse, d.n_orelse)) {
                    return true;
                }
            } else if (ASR::is_a<ASR::Select_t>(*s)) {
                auto &d = *ASR::down_cast<ASR::Select_t>(s);
                for (size_t j = 0; j < d.n_body; j++) {
                    ASR::case_stmt_t *c = d.m_body[j];
                    if (ASR::is_a<ASR::CaseStmt_t>(*c)) {
                        auto &cs = *ASR::down_cast<ASR::CaseStmt_t>(c);
                        if (body_has_exit(cs.m_body, cs.n_body)) return true;
                    } else if (ASR::is_a<ASR::CaseStmt_Range_t>(*c)) {
                        auto &cs = *ASR::down_cast<ASR::CaseStmt_Range_t>(c);
                        if (body_has_exit(cs.m_body, cs.n_body)) return true;
                    }
                }
                if (body_has_exit(d.m_default, d.n_default)) return true;
            }
        }
        return false;
    }

    void stmts(ASR::stmt_t **body, size_t n) {
        for (size_t i = 0; i < n; i++) stmt(body[i]);
    }

    void stmt(ASR::stmt_t *s) {
        const Location &loc = s->base.loc;
        switch (s->type) {
            case ASR::stmtType::Assignment: {
                auto &x = *ASR::down_cast<ASR::Assignment_t>(s);
                if (x.m_overloaded) unsupported("this assignment is", loc);
                if (ASR::is_a<ASR::ArrayItem_t>(*x.m_target)) {
                    auto &t = *ASR::down_cast<ASR::ArrayItem_t>(x.m_target);
                    std::string a = lower(array_var(t.m_v)).src;
                    std::string rhs = fix_string(ASRUtils::expr_type(x.m_target), "",
                        lower(x.m_value), loc);
                    line(a + " := " + app("Arr.set", {{a, pAtom}, {index(t), pAtom},
                        {rhs, pApp}}).src);
                    break;
                }
                if (!ASR::is_a<ASR::Var_t>(*x.m_target)) {
                    unsupported("this assignment is", loc);
                }
                std::string a = lower(x.m_target).src;
                if (is_array(ASRUtils::expr_type(x.m_target))) {
                    // `a = b` keeps the bounds of `a`; `a = v` sets every element.
                    ASR::ttype_t *et = elem_type(ASRUtils::expr_type(x.m_target));
                    ASR::expr_t *v = x.m_value;
                    if (ASR::is_a<ASR::ArrayBroadcast_t>(*v)) {
                        v = ASR::down_cast<ASR::ArrayBroadcast_t>(v)->m_array;
                    }
                    if (is_array(ASRUtils::expr_type(v))) {
                        std::string src = atom(lower(v));
                        if (ASR::is_a<ASR::String_t>(*et)) {
                            auto &st = *ASR::down_cast<ASR::String_t>(et);
                            if (!st.m_len) unsupported("an assumed-length character array assignment is", loc);
                            src = "(Arr.map (FStr.fix " + extent(st.m_len) + ") " + src + ")";
                        }
                        line(a + " := Arr.copyBack " + a + " " + src);
                    } else {
                        std::string rhs = fix_string(et, "", lower(v), loc);
                        line(a + " := Arr.fill " + a + " " + atom({rhs, pApp}));
                    }
                    break;
                }
                line(a + " := " + fix_string(ASRUtils::expr_type(x.m_target), a, lower(x.m_value), loc));
                break;
            }
            case ASR::stmtType::SubroutineCall: {
                auto &x = *ASR::down_cast<ASR::SubroutineCall_t>(s);
                if (x.m_dt) unsupported("a type-bound call is", loc);
                ASR::Function_t *fn = callee(x.m_name, loc);
                if (fn->m_return_var) unsupported("this call is", loc);
                Val c = call(x.m_name, x.m_args, x.n_args, loc);
                // The `intent(out)`/`intent(inout)` actual arguments, in order.
                std::vector<ASR::expr_t*> outs;
                for (size_t i = 0; i < x.n_args; i++) {
                    if (arg_intent(fn, i) == ASR::intentType::In) continue;
                    ASR::expr_t *a = x.m_args[i].m_value;
                    if (is_array(ASRUtils::expr_type(a))) a = array_var(a);
                    if (!ASR::is_a<ASR::Var_t>(*a)) {
                        unsupported("this intent(out) or intent(inout) argument is",
                            a->base.loc);
                    }
                    outs.push_back(a);
                }
                auto assign = [&](ASR::expr_t *a, const std::string &v) {
                    std::string n = lower(a).src;
                    if (is_array(ASRUtils::expr_type(a))) {
                        line(n + " := Arr.copyBack " + n + " " + v);
                    } else {
                        line(n + " := " + fix_string(ASRUtils::expr_type(a), n, {v, pAtom}, loc));
                    }
                };
                bool cal_io = io_procs.count(fn) > 0;
                if (outs.empty()) {
                    line(cal_io ? c.src : "let _ := " + c.src);
                } else if (outs.size() == 1) {
                    if (cal_io) {
                        line("let r'1 ← " + c.src);
                        assign(outs[0], "r'1");
                    } else {
                        assign(outs[0], atom(c));
                    }
                } else {
                    // Temporaries named `r'i`: `'` cannot occur in Fortran names.
                    std::string pat;
                    for (size_t i = 0; i < outs.size(); i++) {
                        pat += (i ? ", " : "") + std::string("r'") + std::to_string(i + 1);
                    }
                    line("let (" + pat + ") " + (cal_io ? "← " : ":= ") + c.src);
                    for (size_t i = 0; i < outs.size(); i++) {
                        assign(outs[i], "r'" + std::to_string(i + 1));
                    }
                }
                break;
            }
            case ASR::stmtType::If: {
                auto &x = *ASR::down_cast<ASR::If_t>(s);
                line("if " + lower(x.m_test).src + " then");
                ind++;
                if (x.n_body == 0) line("Pure.pure ()");
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
                if (!h.m_v || !ASR::is_a<ASR::Var_t>(*h.m_v) || !h.m_start
                        || !h.m_end || x.m_name) {
                    unsupported("this do loop is", loc);
                }
                // The index is not declared, so its kind is fixed on `lo`.
                std::string ty = type(ASRUtils::expr_type(h.m_v), loc);
                std::string lo = lower(h.m_start).src;
                std::string asc = " : " + ty + ")";
                if (!(lo.size() > asc.size() && lo[0] == '('
                        && lo.compare(lo.size() - asc.size(), asc.size(), asc) == 0)) {
                    lo = "(" + lo + asc;
                }
                std::string v = lower(h.m_v).src;
                if (loop_vars.count(ASRUtils::EXPR2VAR(h.m_v))) {
                    if (unit) {
                        line("for " + v + " in FInt.range " + lo + " "
                            + atom(lower(h.m_end)) + " do");
                    } else {
                        line("for " + v + " in FInt.rangeStep " + lo + " "
                            + atom(lower(h.m_end)) + " " + atom(lower(h.m_increment)) + " do");
                    }
                    ind++;
                    exit_flags.push_back("");
                    if (x.n_body == 0) line("Pure.pure ()");
                    stmts(x.m_body, x.n_body);
                    exit_flags.pop_back();
                    ind--;
                    break;
                }
                // The `do` variable is an ordinary variable: it takes each
                // index, and after the loop the first one not executed,
                // `lo + max(0, (hi - lo + step) / step) * step` unless `exit`
                // left the loop early. The bounds are evaluated once.
                std::string id = std::to_string(++loop_id);
                std::string lv = "lo'" + id, hv = "hi'" + id, iv = "i'" + id;
                bool has_ex = body_has_exit(x.m_body, x.n_body);
                std::string brk = has_ex ? "brk'" + id : "";
                line("let " + lv + " : " + ty + " := " + lower(h.m_start).src);
                line("let " + hv + " : " + ty + " := " + lower(h.m_end).src);
                std::string sv;
                if (!unit) {
                    sv = "step'" + id;
                    line("let " + sv + " : " + ty + " := " + lower(h.m_increment).src);
                }
                if (has_ex) line("let mut " + brk + " : Bool := false");
                if (unit) {
                    line("for " + iv + " in FInt.range " + lv + " " + hv + " do");
                } else {
                    line("for " + iv + " in FInt.rangeStep " + lv + " " + hv + " " + sv + " do");
                }
                ind++;
                exit_flags.push_back(brk);
                line(v + " := " + iv);
                stmts(x.m_body, x.n_body);
                exit_flags.pop_back();
                ind--;
                std::string post = unit
                    ? v + " := FInt.ofInt (FInt.toInt " + lv + " + Max.max 0 (FInt.toInt " + hv
                        + " + 1 - FInt.toInt " + lv + "))"
                    : v + " := FInt.ofInt (FInt.toInt " + lv + " + (if FInt.toInt " + sv
                        + " == 0 then 0 else Max.max 0 ((FInt.toInt " + hv + " - FInt.toInt "
                        + lv + " + FInt.toInt " + sv + ").tdiv (FInt.toInt " + sv
                        + ")) * FInt.toInt " + sv + "))";
                if (has_ex) {
                    line("if !" + brk + " then");
                    ind++;
                    line(post);
                    ind--;
                } else {
                    line(post);
                }
                break;
            }
            case ASR::stmtType::WhileLoop: {
                auto &x = *ASR::down_cast<ASR::WhileLoop_t>(s);
                if (x.m_name || x.n_orelse > 0) unsupported("this while loop is", loc);
                if (cur) cur->uses_while = true;
                line("while " + lower(x.m_test).src + " do");
                ind++;
                exit_flags.push_back("");
                if (x.n_body == 0) line("Pure.pure ()");
                stmts(x.m_body, x.n_body);
                exit_flags.pop_back();
                ind--;
                break;
            }
            case ASR::stmtType::Exit: {
                auto &x = *ASR::down_cast<ASR::Exit_t>(s);
                if (x.m_stmt_name || exit_flags.empty()) unsupported("this exit is", loc);
                if (!exit_flags.back().empty()) line(exit_flags.back() + " := true");
                line("break");
                break;
            }
            case ASR::stmtType::Cycle: {
                auto &x = *ASR::down_cast<ASR::Cycle_t>(s);
                if (x.m_stmt_name || exit_flags.empty()) unsupported("this cycle is", loc);
                line("continue");
                break;
            }
            case ASR::stmtType::Select: {
                auto &x = *ASR::down_cast<ASR::Select_t>(s);
                if (x.m_name || x.m_enable_fall_through || !is_int(x.m_test)) {
                    unsupported("this select case is", loc);
                }
                std::string id = std::to_string(++loop_id);
                std::string sv = "sel'" + id;
                line("let " + sv + " : Int := " + int_expr(x.m_test).src);
                bool first = true;
                for (size_t i = 0; i < x.n_body; i++) {
                    ASR::case_stmt_t *c = x.m_body[i];
                    std::string cond;
                    ASR::stmt_t **cbody = nullptr;
                    size_t n_cbody = 0;
                    if (ASR::is_a<ASR::CaseStmt_t>(*c)) {
                        auto &cs = *ASR::down_cast<ASR::CaseStmt_t>(c);
                        if (cs.m_fall_through || cs.n_test == 0) {
                            unsupported("this select case is", loc);
                        }
                        for (size_t j = 0; j < cs.n_test; j++) {
                            if (j) cond += " || ";
                            cond += "(" + sv + " == " + atom(int_expr(cs.m_test[j])) + ")";
                        }
                        cbody = cs.m_body;
                        n_cbody = cs.n_body;
                    } else if (ASR::is_a<ASR::CaseStmt_Range_t>(*c)) {
                        auto &cs = *ASR::down_cast<ASR::CaseStmt_Range_t>(c);
                        if (!cs.m_start && !cs.m_end) unsupported("this select case is", loc);
                        if (cs.m_start && cs.m_end) {
                            cond = "decide (" + atom(int_expr(cs.m_start)) + " ≤ " + sv
                                + ") && decide (" + sv + " ≤ " + atom(int_expr(cs.m_end)) + ")";
                        } else if (cs.m_start) {
                            cond = "decide (" + atom(int_expr(cs.m_start)) + " ≤ " + sv + ")";
                        } else {
                            cond = "decide (" + sv + " ≤ " + atom(int_expr(cs.m_end)) + ")";
                        }
                        cbody = cs.m_body;
                        n_cbody = cs.n_body;
                    } else {
                        unsupported("this select case is", loc);
                    }
                    line((first ? "if " : "else if ") + cond + " then");
                    first = false;
                    ind++;
                    if (n_cbody == 0) line("Pure.pure ()");
                    stmts(cbody, n_cbody);
                    ind--;
                }
                if (x.n_default > 0) {
                    if (first) {
                        stmts(x.m_default, x.n_default);
                    } else {
                        line("else");
                        ind++;
                        stmts(x.m_default, x.n_default);
                        ind--;
                    }
                }
                break;
            }
            case ASR::stmtType::Return:
                line(in_program ? std::string("return") : "return " + cur->ret);
                break;
            case ASR::stmtType::Print: {
                auto &x = *ASR::down_cast<ASR::Print_t>(s);
                if (!in_program && !(cur && io_procs.count(cur->fn))) {
                    unsupported("print in a procedure is", loc);
                }
                level = std::max(level, 2);
                if (!x.m_text) {
                    line("IO.println \"\"");
                    break;
                }
                if (ASR::is_a<ASR::StringConstant_t>(*x.m_text)) {
                    line("IO.println " + string_lit(
                        ASR::down_cast<ASR::StringConstant_t>(x.m_text)->m_s));
                    break;
                }
                if (ASR::is_a<ASR::StringFormat_t>(*x.m_text)) {
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
                if (is_string(x.m_text)) {
                    line("IO.println " + atom(lower(x.m_text)));
                    break;
                }
                unsupported("this print is", loc);
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

    // The `do` variables emitted as the bound name of `for`: those used only
    // inside the loops they index.
    void collect_simple_loop_vars(ASR::stmt_t **body, size_t n) {
        collect_loop_vars(body, n);
        LoopVarUses u;
        for (size_t i = 0; i < n; i++) u.visit_stmt(*body[i]);
        for (ASR::Variable_t *v : u.outside) loop_vars.erase(v);
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
            } else if (ASR::is_a<ASR::WhileLoop_t>(*s)) {
                auto &d = *ASR::down_cast<ASR::WhileLoop_t>(s);
                collect_loop_vars(d.m_body, d.n_body);
            } else if (ASR::is_a<ASR::Select_t>(*s)) {
                auto &d = *ASR::down_cast<ASR::Select_t>(s);
                for (size_t j = 0; j < d.n_body; j++) {
                    ASR::case_stmt_t *c = d.m_body[j];
                    if (ASR::is_a<ASR::CaseStmt_t>(*c)) {
                        auto &cs = *ASR::down_cast<ASR::CaseStmt_t>(c);
                        collect_loop_vars(cs.m_body, cs.n_body);
                    } else if (ASR::is_a<ASR::CaseStmt_Range_t>(*c)) {
                        auto &cs = *ASR::down_cast<ASR::CaseStmt_Range_t>(c);
                        collect_loop_vars(cs.m_body, cs.n_body);
                    }
                }
                collect_loop_vars(d.m_default, d.n_default);
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
            if (!in_program && v->m_storage == ASR::storage_typeType::Save) {
                unsupported("a saved local variable is", vloc);
            }
            if (v->m_symbolic_value || v->m_value) {
                if (!in_program) {
                    unsupported("an initialized local variable is", vloc);
                }
                ASR::expr_t *init = v->m_symbolic_value ? v->m_symbolic_value : v->m_value;
                if (is_array(v->m_type)) {
                    std::vector<std::string> lbs, exts;
                    bool assumed;
                    dims(v->m_type, lbs, exts, assumed, vloc);
                    if (assumed) unsupported("this local array is", vloc);
                    std::string src = atom(lower(array_var(init)));
                    ASR::ttype_t *et = elem_type(v->m_type);
                    if (ASR::is_a<ASR::String_t>(*et)) {
                        auto &st = *ASR::down_cast<ASR::String_t>(et);
                        if (!st.m_len) unsupported("this local array is", vloc);
                        src = "(Arr.map (FStr.fix " + extent(st.m_len) + ") " + src + ")";
                    }
                    line("let mut " + name(v->m_name, vloc) + " : " + vtype(v->m_type, vloc)
                        + " := Arr.reshape " + src + " "
                        + int_array(lbs) + " " + int_array(exts));
                    continue;
                }
                line("let mut " + name(v->m_name, vloc) + " : " + type(v->m_type, vloc)
                    + " := " + fix_string(v->m_type, "", lower(init), vloc));
                continue;
            }
            if (is_array(v->m_type)) {
                std::vector<std::string> lbs, exts;
                bool assumed;
                dims(v->m_type, lbs, exts, assumed, vloc);
                if (assumed) unsupported("this local array is", vloc);
                line("let mut " + name(v->m_name, vloc) + " : " + vtype(v->m_type, vloc)
                    + " := Arr.make " + int_array(lbs) + " " + int_array(exts) + " "
                    + atom({zero(ASR::down_cast<ASR::Array_t>(v->m_type)->m_type, vloc),
                        pApp}));
                continue;
            }
            line("let mut " + name(v->m_name, vloc) + " : " + type(v->m_type, vloc)
                + " := " + zero(v->m_type, vloc));
        }
    }

    void check_symbols(SymbolTable *st, bool allow_functions = false) {
        for (ASR::symbol_t *s : symbols_in_order(st)) {
            if (ASR::is_a<ASR::Variable_t>(*s) || ASR::is_a<ASR::ExternalSymbol_t>(*s)
                    || (allow_functions && ASR::is_a<ASR::Function_t>(*s))) {
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
        bool is_io = io_procs.count(fn) > 0;
        level = is_io ? 2 : 0;
        loop_vars.clear();
        scope = fn->m_symtab;
        in_program = false;
        check_symbols(fn->m_symtab);
        // An interface body declares a procedure implemented elsewhere (a
        // separate module procedure, an external procedure).
        if (ASRUtils::get_FunctionType(fn)->m_deftype != ASR::deftypeType::Implementation) {
            unsupported("a procedure without its implementation in this module is", loc);
        }
        result = fn->m_return_var ? ASRUtils::EXPR2VAR(fn->m_return_var) : nullptr;
        std::set<ASR::Variable_t*> dummies;
        std::vector<std::pair<uint32_t, std::string>> entry;
        std::vector<std::string> outs, out_types;
        for (size_t i = 0; i < fn->n_args; i++) {
            ASR::Variable_t *a = dummy(fn, i);
            if (!a) unsupported("a procedure argument is", fn->m_args[i]->base.loc);
            const Location &aloc = a->base.base.loc;
            std::string an = name(a->m_name, aloc);
            ASR::intentType in = arg_intent(fn, i);
            if (in != ASR::intentType::In && in != ASR::intentType::Out
                    && in != ASR::intentType::InOut) {
                unsupported("an argument without intent(in), intent(out) or "
                    "intent(inout) is", aloc);
            }
            if (result && in != ASR::intentType::In) {
                unsupported("a function argument that is not intent(in) is", aloc);
            }
            dummies.insert(a);
            std::string let = in == ASR::intentType::In ? "let " : "let mut ";
            if (is_array(a->m_type)) {
                // The dummy takes its declared bounds.
                std::vector<std::string> lbs, exts;
                bool assumed;
                dims(a->m_type, lbs, exts, assumed, aloc);
                p.sig += " (" + an + " : " + vtype(a->m_type, aloc) + ")";
                entry.push_back({aloc.first, let + an + " := " + (assumed
                    ? "Arr.rebase " + an + " " + int_array(lbs)
                    : "Arr.reshape " + an + " " + int_array(lbs) + " " + int_array(exts))});
            } else if (in == ASR::intentType::Out) {
                if (is_assumed_len_string(a->m_type)) {
                    p.sig += " (" + an + " : String)";
                    entry.push_back({aloc.first, "let mut " + an + " := FStr.fix " + an + ".length \"\""});
                } else {
                    entry.push_back({aloc.first, "let mut " + an + " : " + type(a->m_type, aloc) + " := "
                        + zero(a->m_type, aloc)});
                }
            } else {
                p.sig += " (" + an + " : " + type(a->m_type, aloc) + ")";
                if (ASR::is_a<ASR::String_t>(*a->m_type) && ASR::down_cast<ASR::String_t>(a->m_type)->m_len) {
                    entry.push_back({aloc.first, let + an + " := FStr.fix "
                        + extent(ASR::down_cast<ASR::String_t>(a->m_type)->m_len) + " " + an});
                } else if (in == ASR::intentType::InOut) {
                    entry.push_back({aloc.first, "let mut " + an + " := " + an});
                }
            }
            if (in != ASR::intentType::In) {
                outs.push_back(an);
                out_types.push_back(vtype(a->m_type, aloc));
            }
        }
        std::stable_sort(entry.begin(), entry.end(),
            [](const auto &a, const auto &b) { return a.first < b.first; });
        if (result) {
            p.ret = name(result->m_name, result->base.base.loc);
            std::string rty = vtype(result->m_type, result->base.base.loc);
            p.sig += is_io ? " : IO (" + rty + ")" : " : " + rty;
        } else if (outs.empty()) {
            p.ret = "()";
            p.sig += is_io ? " : IO Unit" : " : Unit";
        } else {
            std::string ret, ty;
            for (size_t i = 0; i < outs.size(); i++) {
                ret += (i ? ", " : "") + outs[i];
                ty += (i ? " × " : "") + out_types[i];
            }
            p.ret = outs.size() == 1 ? ret : "(" + ret + ")";
            p.sig += is_io ? " : IO (" + ty + ")" : " : " + ty;
        }
        collect_simple_loop_vars(fn->m_body, fn->n_body);
        for (ASR::Variable_t *d : dummies) loop_vars.erase(d);
        if (result) loop_vars.erase(result);
        buf = &p.body;
        ind = 1;
        for (const auto &l : entry) line(l.second);
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
        std::string kw = (p.callees.count(p.fn) || p.uses_while) ? "partial def " : "def ";
        std::string rhs = io_procs.count(p.fn) ? " := do\n" : " := Id.run do\n";
        return kw + short_name + " {K : Kinds} [" + level_class(p.level) + " K]"
            + p.sig + rhs + p.body + "\n";
    }

    void analyze_procs(const std::vector<ASR::Function_t*> &all_fns) {
        std::map<ASR::Function_t*, ProcEffects> effs;
        for (ASR::Function_t *fn : all_fns) {
            ProcEffects &pe = effs[fn];
            for (ASR::symbol_t *s : symbols_in_order(fn->m_symtab)) {
                if (ASR::is_a<ASR::Variable_t>(*s)) {
                    pe.visit_ttype(*ASR::down_cast<ASR::Variable_t>(s)->m_type);
                }
            }
            for (size_t i = 0; i < fn->n_body; i++) pe.visit_stmt(*fn->m_body[i]);
            if (pe.has_io) io_procs.insert(fn);
            std::vector<ASR::intentType> &ins = eff_intents[fn];
            ins.resize(fn->n_args);
            for (size_t i = 0; i < fn->n_args; i++) {
                ASR::Variable_t *d = dummy(fn, i);
                if (!d) {
                    ins[i] = ASR::intentType::Unspecified;
                } else if (d->m_intent == ASR::intentType::Unspecified) {
                    ins[i] = pe.modified.count(d)
                        ? ASR::intentType::InOut : ASR::intentType::In;
                } else {
                    ins[i] = d->m_intent;
                }
            }
        }
        bool changed = true;
        while (changed) {
            changed = false;
            for (ASR::Function_t *fn : all_fns) {
                ProcEffects &pe = effs[fn];
                if (!io_procs.count(fn)) {
                    for (ASR::Function_t *c : pe.callees) {
                        if (io_procs.count(c)) {
                            io_procs.insert(fn);
                            changed = true;
                            break;
                        }
                    }
                }
                std::vector<ASR::intentType> &ins = eff_intents[fn];
                for (const auto &sa : pe.sub_args) {
                    auto it = eff_intents.find(sa.callee);
                    ASR::Variable_t *cd = dummy(sa.callee, sa.idx);
                    ASR::intentType cin = (it != eff_intents.end() && sa.idx < it->second.size())
                        ? it->second[sa.idx]
                        : (cd ? cd->m_intent : ASR::intentType::Unspecified);
                    if (cin == ASR::intentType::In) continue;
                    for (size_t i = 0; i < fn->n_args; i++) {
                        ASR::Variable_t *d = dummy(fn, i);
                        if (d && d == sa.var && d->m_intent == ASR::intentType::Unspecified
                                && ins[i] == ASR::intentType::In) {
                            ins[i] = ASR::intentType::InOut;
                            changed = true;
                        }
                    }
                }
            }
        }
    }

public:
    void translate(ASR::TranslationUnit_t &tu) {
        std::vector<ASR::Module_t*> mods;
        std::vector<ASR::Function_t*> top_fns;
        ASR::Program_t *prog = nullptr;
        for (ASR::symbol_t *s : symbols_in_order(tu.m_symtab)) {
            if (ASR::is_a<ASR::Module_t>(*s)) {
                ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(s);
                if (m->m_intrinsic) continue;
                mods.push_back(m);
            } else if (ASR::is_a<ASR::Function_t>(*s)) {
                top_fns.push_back(ASR::down_cast<ASR::Function_t>(s));
            } else if (ASR::is_a<ASR::Program_t>(*s)) {
                prog = ASR::down_cast<ASR::Program_t>(s);
            } else {
                unsupported("this top-level entity is", s->base.loc);
            }
        }
        if (require_program && !prog) {
            unsupported("a file with no program is", tu.base.base.loc);
        }
        std::vector<ASR::Function_t*> all_fns;
        for (ASR::Module_t *m : mods) {
            name(m->m_name, m->base.base.loc);
            for (ASR::symbol_t *s : symbols_in_order(m->m_symtab)) {
                if (ASR::is_a<ASR::Function_t>(*s)) {
                    ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(s);
                    proc_ns[fn] = m->m_name;
                    all_fns.push_back(fn);
                } else if (ASR::is_a<ASR::Variable_t>(*s)) {
                    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
                    if (v->m_storage != ASR::storage_typeType::Parameter) {
                        unsupported("a module variable is", s->base.loc);
                    }
                } else if (!ASR::is_a<ASR::ExternalSymbol_t>(*s)) {
                    unsupported("this module entity is", s->base.loc);
                }
            }
        }
        std::set<std::string> top_names;
        for (ASR::Function_t *fn : top_fns) {
            if (prog && std::string(fn->m_name) == "main") {
                unsupported("a top-level procedure named 'main' is", fn->base.base.loc);
            }
            proc_ns[fn] = "";
            top_names.insert(fn->m_name);
            all_fns.push_back(fn);
        }
        if (prog) {
            check_symbols(prog->m_symtab, /*allow_functions=*/true);
            name(prog->m_name, prog->base.base.loc);
            std::string prog_ns = std::string(prog->m_name) + "'procs";
            for (ASR::symbol_t *s : symbols_in_order(prog->m_symtab)) {
                if (ASR::is_a<ASR::Function_t>(*s)) {
                    ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(s);
                    proc_ns[fn] = prog_ns;
                    all_fns.push_back(fn);
                }
            }
        }
        analyze_procs(all_fns);
        for (ASR::Function_t *fn : all_fns) function(fn);
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
            collect_simple_loop_vars(prog->m_body, prog->n_body);
            buf = &prog_body;
            ind = 1;
            locals(prog->m_symtab, {});
            stmts(prog->m_body, prog->n_body);
            if (prog_body.empty()) line("Pure.pure ()");
            cur = nullptr;
        }
        propagate_levels();
        out = "-- Generated by LFortran (`lfortran "
            + std::string(executable ? "--backend=lean " : "--show-lean ") + filename
            + "`). Do not edit.\nimport LFortran.Runtime\n\nopen LFortran\n\n";
        std::string cur_ns;
        for (ASR::Function_t *fn : callees_first(all_fns)) {
            std::string raw_ns = proc_ns[fn];
            std::string fns = raw_ns.empty() ? ""
                : (raw_ns.find('\'') != std::string::npos ? raw_ns
                    : name(raw_ns, fn->base.base.loc));
            if (fns != cur_ns) {
                if (!cur_ns.empty()) out += "end " + cur_ns + "\n\n";
                if (!fns.empty()) out += "namespace " + fns + "\n\n";
                cur_ns = fns;
            }
            out += def(procs[fn], name(fn->m_name, fn->base.base.loc));
        }
        if (!cur_ns.empty()) out += "end " + cur_ns + "\n\n";
        if (prog) {
            for (ASR::Function_t *c : prog_proc.callees) {
                auto it = procs.find(c);
                if (it == procs.end()) {
                    unsupported("a call to a procedure outside this file is",
                        c->base.base.loc);
                }
                prog_level = std::max(prog_level, it->second.level);
            }
            std::string raw_pn = std::string(prog->m_name) + "_main";
            std::string pn = top_names.count(raw_pn)
                ? std::string(prog->m_name) + "'main"
                : name(raw_pn, prog->base.base.loc);
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
