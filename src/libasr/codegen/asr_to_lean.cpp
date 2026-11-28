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
    static const char hex[] = "0123456789abcdef";
    std::string o = "\"";
    for (unsigned char c : s) {
        if (c == '\\' || c == '"') { o += '\\'; o += static_cast<char>(c); }
        else if (c == '\n') o += "\\n";
        else if (c == '\r') o += "\\r";
        else if (c == '\t') o += "\\t";
        else if (c < 0x20 || c >= 0x7f) {
            o += "\\x";
            o += hex[c >> 4];
            o += hex[c & 0xf];
        } else o += static_cast<char>(c);
    }
    return o + "\"";
}

std::string string_const_str(const ASR::StringConstant_t &x) {
    if (x.m_type && ASR::is_a<ASR::String_t>(*x.m_type)) {
        ASR::expr_t *le = ASR::down_cast<ASR::String_t>(x.m_type)->m_len;
        if (le) {
            ASR::expr_t *v = ASRUtils::expr_value(le);
            if (v && ASR::is_a<ASR::IntegerConstant_t>(*v)) {
                int64_t n = ASR::down_cast<ASR::IntegerConstant_t>(v)->m_n;
                if (n >= 0) return std::string(x.m_s, static_cast<size_t>(n));
            }
        }
    }
    return std::string(x.m_s);
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

    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        std::vector<ASR::Variable_t*> vs;
        for (size_t i = 0; i < x.n_head; i++) {
            const ASR::do_loop_head_t &h = x.m_head[i];
            if (h.m_start) visit_expr(*h.m_start);
            if (h.m_end) visit_expr(*h.m_end);
            if (h.m_increment) visit_expr(*h.m_increment);
        }
        for (size_t i = 0; i < x.n_head; i++) {
            const ASR::do_loop_head_t &h = x.m_head[i];
            ASR::Variable_t *v = h.m_v && ASR::is_a<ASR::Var_t>(*h.m_v)
                ? ASRUtils::EXPR2VAR(h.m_v) : nullptr;
            if (v) { index.push_back(v); vs.push_back(v); }
        }
        for (size_t i = 0; i < x.n_local; i++) {
            if (x.m_local[i] && ASR::is_a<ASR::Var_t>(*x.m_local[i])) {
                outside.insert(ASRUtils::EXPR2VAR(x.m_local[i]));
            }
        }
        for (size_t i = 0; i < x.n_body; i++) visit_stmt(*x.m_body[i]);
        for (size_t i = 0; i < vs.size(); i++) index.pop_back();
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_m);
        if (ASR::is_a<ASR::Block_t>(*s)) {
            ASR::Block_t *b = ASR::down_cast<ASR::Block_t>(s);
            for (size_t i = 0; i < b->n_body; i++) visit_stmt(*b->m_body[i]);
        }
    }
};

// Pre-pass collecting direct IO usage, global-variable usage, callees, and
// modified variables of a procedure so `io_procs`, `global_procs`, and
// effective `Unspecified` intents are known before any body is lowered.
class ProcEffects : public ASR::BaseWalkVisitor<ProcEffects> {
public:
    const std::map<ASR::Variable_t*, std::string> &global_fields;
    bool has_io = false;
    bool uses_globals = false;
    std::set<ASR::Function_t*> callees;
    std::set<ASR::Variable_t*> modified;
    struct SubArg {
        ASR::Function_t *callee;
        size_t idx;
        ASR::Variable_t *var;
    };
    std::vector<SubArg> sub_args;

    explicit ProcEffects(const std::map<ASR::Variable_t*, std::string> &gf)
        : global_fields(gf) {}

    void visit_Var(const ASR::Var_t &x) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_v);
        if (ASR::is_a<ASR::Variable_t>(*s)
                && global_fields.count(ASR::down_cast<ASR::Variable_t>(s))) {
            uses_globals = true;
        }
    }

    static ASR::Variable_t *root_var(ASR::expr_t *e) {
        while (e) {
            if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
                e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
            } else if (ASR::is_a<ASR::StringPhysicalCast_t>(*e)) {
                e = ASR::down_cast<ASR::StringPhysicalCast_t>(e)->m_arg;
            } else if (ASR::is_a<ASR::ArrayItem_t>(*e)) {
                e = ASR::down_cast<ASR::ArrayItem_t>(e)->m_v;
            } else if (ASR::is_a<ASR::ArraySection_t>(*e)) {
                e = ASR::down_cast<ASR::ArraySection_t>(e)->m_v;
            } else if (ASR::is_a<ASR::StringSection_t>(*e)) {
                e = ASR::down_cast<ASR::StringSection_t>(e)->m_arg;
            } else if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
                e = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v;
            } else if (ASR::is_a<ASR::ComplexRe_t>(*e)) {
                e = ASR::down_cast<ASR::ComplexRe_t>(e)->m_arg;
            } else if (ASR::is_a<ASR::ComplexIm_t>(*e)) {
                e = ASR::down_cast<ASR::ComplexIm_t>(e)->m_arg;
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

    void visit_FileWrite(const ASR::FileWrite_t &x) {
        if (x.m_unit && ASR::is_a<ASR::String_t>(
                *ASRUtils::type_get_past_allocatable(ASRUtils::expr_type(x.m_unit)))) {
            if (ASR::Variable_t *v = root_var(x.m_unit)) modified.insert(v);
        } else {
            has_io = true;
        }
        if (x.m_iostat) {
            if (ASR::Variable_t *v = root_var(x.m_iostat)) modified.insert(v);
        }
        ASR::BaseWalkVisitor<ProcEffects>::visit_FileWrite(x);
    }

    void visit_FileRead(const ASR::FileRead_t &x) {
        if (x.m_unit && ASR::is_a<ASR::String_t>(
                *ASRUtils::type_get_past_allocatable(ASRUtils::expr_type(x.m_unit)))) {
            for (size_t i = 0; i < x.n_values; i++) {
                if (ASR::Variable_t *v = root_var(x.m_values[i])) modified.insert(v);
            }
        } else {
            has_io = true;
        }
        if (x.m_iostat) {
            if (ASR::Variable_t *v = root_var(x.m_iostat)) modified.insert(v);
        }
        ASR::BaseWalkVisitor<ProcEffects>::visit_FileRead(x);
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

    void visit_Allocate(const ASR::Allocate_t &x) {
        for (size_t i = 0; i < x.n_args; i++) {
            if (ASR::Variable_t *v = root_var(x.m_args[i].m_a)) modified.insert(v);
        }
        if (x.m_stat) {
            if (ASR::Variable_t *v = root_var(x.m_stat)) modified.insert(v);
        }
        ASR::BaseWalkVisitor<ProcEffects>::visit_Allocate(x);
    }

    void visit_ReAlloc(const ASR::ReAlloc_t &x) {
        for (size_t i = 0; i < x.n_args; i++) {
            if (ASR::Variable_t *v = root_var(x.m_args[i].m_a)) modified.insert(v);
        }
        ASR::BaseWalkVisitor<ProcEffects>::visit_ReAlloc(x);
    }

    void visit_ExplicitDeallocate(const ASR::ExplicitDeallocate_t &x) {
        for (size_t i = 0; i < x.n_vars; i++) {
            if (ASR::Variable_t *v = root_var(x.m_vars[i])) modified.insert(v);
        }
        if (x.m_stat) {
            if (ASR::Variable_t *v = root_var(x.m_stat)) modified.insert(v);
        }
        ASR::BaseWalkVisitor<ProcEffects>::visit_ExplicitDeallocate(x);
    }

    void visit_DoLoop(const ASR::DoLoop_t &x) {
        if (ASR::Variable_t *v = root_var(x.m_head.m_v)) modified.insert(v);
        ASR::BaseWalkVisitor<ProcEffects>::visit_DoLoop(x);
    }

    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        for (size_t i = 0; i < x.n_head; i++) {
            if (ASR::Variable_t *v = root_var(x.m_head[i].m_v)) modified.insert(v);
        }
        ASR::BaseWalkVisitor<ProcEffects>::visit_DoConcurrentLoop(x);
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_m);
        if (ASR::is_a<ASR::Block_t>(*s)) {
            ASR::Block_t *b = ASR::down_cast<ASR::Block_t>(s);
            for (auto &item : b->m_symtab->get_scope()) {
                if (ASR::is_a<ASR::Variable_t>(*item.second)) {
                    visit_ttype(*ASR::down_cast<ASR::Variable_t>(item.second)->m_type);
                }
            }
            for (size_t i = 0; i < b->n_body; i++) visit_stmt(*b->m_body[i]);
        }
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
    std::map<ASR::Struct_t*, std::string> struct_ns;
    std::map<ASR::Variable_t*, std::string> global_fields;
    std::set<ASR::Function_t*> io_procs;
    std::set<ASR::Function_t*> global_procs;
    std::map<ASR::Function_t*, std::vector<ASR::intentType>> eff_intents;
    std::vector<ASR::Function_t*> order;
    Proc *cur = nullptr;
    bool in_program = false;
    bool prog_uses_globals = false;
    int level = 0;
    int ind = 0;
    ASR::Variable_t *result = nullptr;
    std::set<ASR::Variable_t*> loop_vars;
    struct LoopFrame {
        std::string name;
        std::string brk;
        std::string named_brk;
        std::string named_cyc;
    };
    std::vector<LoopFrame> loop_stack;
    std::map<const ASR::ArrayItem_t*, std::string> pre_idx;
    std::map<ASR::Variable_t*, std::string> alloc_str_len;
    std::map<ASR::Variable_t*, std::string> var_alias;
    std::map<ASR::Variable_t*, std::string> implied_vars;
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

    std::string var_name(ASR::Variable_t *v) {
        auto it = var_alias.find(v);
        if (it != var_alias.end()) return it->second;
        return name(v->m_name, v->base.base.loc);
    }

    // Derived types --------------------------------------------------------

    ASR::Struct_t *resolve_struct(ASR::symbol_t *sym, const Location &loc) {
        if (!sym) unsupported("this derived type is", loc);
        sym = ASRUtils::symbol_get_past_external(sym);
        if (!ASR::is_a<ASR::Struct_t>(*sym)) unsupported("this derived type is", loc);
        ASR::Struct_t *st = ASR::down_cast<ASR::Struct_t>(sym);
        if (!struct_ns.count(st)) unsupported("this derived type is", loc);
        return st;
    }

    ASR::Variable_t *struct_member(ASR::Struct_t *st, size_t i, const Location &loc) {
        ASR::symbol_t *ms = st->m_symtab->get_symbol(st->m_members[i]);
        if (!ms || !ASR::is_a<ASR::Variable_t>(*ms)) {
            unsupported("this derived type member is", loc);
        }
        return ASR::down_cast<ASR::Variable_t>(ms);
    }

    std::vector<ASR::Variable_t*> all_struct_members(ASR::Struct_t *st, const Location &loc) {
        std::vector<ASR::Variable_t*> ms;
        if (st->m_parent) {
            ms = all_struct_members(resolve_struct(st->m_parent, loc), loc);
        }
        for (size_t i = 0; i < st->n_members; i++) {
            ms.push_back(struct_member(st, i, loc));
        }
        return ms;
    }

    std::string struct_parent_proj(ASR::Struct_t *parent, const Location &loc) {
        name(parent->m_name, loc);
        return "to" + std::string(parent->m_name);
    }

    std::string struct_qname(ASR::Struct_t *st, const Location &loc) {
        std::string sn = name(st->m_name, loc);
        auto it = struct_ns.find(st);
        if (it == struct_ns.end() || it->second.empty()) return "_root_." + sn;
        return "_root_." + it->second + "." + sn;
    }

    std::string struct_init_name(ASR::Struct_t *st, const Location &loc) {
        name(st->m_name, loc);
        return std::string(st->m_name) + "'init";
    }

    std::string struct_init_qname(ASR::Struct_t *st, const Location &loc) {
        std::string in = struct_init_name(st, loc);
        auto it = struct_ns.find(st);
        if (it == struct_ns.end() || it->second.empty()) return "_root_." + in;
        return "_root_." + it->second + "." + in;
    }

    ASR::symbol_t *expr_type_decl(ASR::expr_t *e) {
        while (e) {
            switch (e->type) {
                case ASR::exprType::Var: {
                    ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::Var_t>(e)->m_v);
                    if (ASR::is_a<ASR::Variable_t>(*s)) {
                        return ASR::down_cast<ASR::Variable_t>(s)->m_type_declaration;
                    }
                    return nullptr;
                }
                case ASR::exprType::StructConstructor:
                    return ASR::down_cast<ASR::StructConstructor_t>(e)->m_dt_sym;
                case ASR::exprType::StructConstant:
                    return ASR::down_cast<ASR::StructConstant_t>(e)->m_dt_sym;
                case ASR::exprType::StructInstanceMember: {
                    ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_m);
                    if (ASR::is_a<ASR::Variable_t>(*s)) {
                        return ASR::down_cast<ASR::Variable_t>(s)->m_type_declaration;
                    }
                    if (ASR::is_a<ASR::Struct_t>(*s)) {
                        return s;
                    }
                    return nullptr;
                }
                case ASR::exprType::ArrayItem:
                    e = ASR::down_cast<ASR::ArrayItem_t>(e)->m_v;
                    break;
                case ASR::exprType::ArraySection:
                    e = ASR::down_cast<ASR::ArraySection_t>(e)->m_v;
                    break;
                case ASR::exprType::ArrayPhysicalCast:
                    e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
                    break;
                case ASR::exprType::ArrayBroadcast:
                    e = ASR::down_cast<ASR::ArrayBroadcast_t>(e)->m_array;
                    break;
                case ASR::exprType::ArrayReshape:
                    e = ASR::down_cast<ASR::ArrayReshape_t>(e)->m_array;
                    break;
                case ASR::exprType::ArrayTranspose:
                    e = ASR::down_cast<ASR::ArrayTranspose_t>(e)->m_matrix;
                    break;
                case ASR::exprType::ArrayPack:
                    e = ASR::down_cast<ASR::ArrayPack_t>(e)->m_array;
                    break;
                case ASR::exprType::IntrinsicArrayFunction: {
                    auto &x = *ASR::down_cast<ASR::IntrinsicArrayFunction_t>(e);
                    auto aid = static_cast<ASRUtils::IntrinsicArrayFunctions>(x.m_arr_intrinsic_id);
                    if ((aid == ASRUtils::IntrinsicArrayFunctions::Pack
                            || aid == ASRUtils::IntrinsicArrayFunctions::Spread) && x.n_args > 0) {
                        e = x.m_args[0];
                        break;
                    }
                    return nullptr;
                }
                case ASR::exprType::ImpliedDoLoop: {
                    auto &x = *ASR::down_cast<ASR::ImpliedDoLoop_t>(e);
                    if (x.n_values > 0) { e = x.m_values[0]; break; }
                    return nullptr;
                }
                case ASR::exprType::FunctionCall: {
                    ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::FunctionCall_t>(e)->m_name);
                    if (ASR::is_a<ASR::Function_t>(*s)) {
                        ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(s);
                        if (fn->m_return_var && ASR::is_a<ASR::Var_t>(*fn->m_return_var)) {
                            return ASRUtils::EXPR2VAR(fn->m_return_var)->m_type_declaration;
                        }
                    }
                    return nullptr;
                }
                case ASR::exprType::ArrayConstructor: {
                    auto &x = *ASR::down_cast<ASR::ArrayConstructor_t>(e);
                    if (x.n_args > 0) { e = x.m_args[0]; break; }
                    return nullptr;
                }
                case ASR::exprType::ArrayConstant: {
                    auto &x = *ASR::down_cast<ASR::ArrayConstant_t>(e);
                    if (x.m_data && is_array(x.m_type)
                            && ASR::is_a<ASR::StructType_t>(*elem_type(x.m_type))
                            && ASRUtils::get_fixed_size_of_array(x.m_type) > 0) {
                        e = static_cast<ASR::expr_t**>(x.m_data)[0];
                        break;
                    }
                    return nullptr;
                }
                case ASR::exprType::IntrinsicElementalFunction: {
                    auto &x = *ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e);
                    if (x.n_args > 0) { e = x.m_args[0]; break; }
                    return nullptr;
                }
                default:
                    return nullptr;
            }
        }
        return nullptr;
    }

    // Types ----------------------------------------------------------------

    bool is_allocatable(ASR::ttype_t *t) const {
        return t && ASR::is_a<ASR::Allocatable_t>(*t);
    }

    bool is_optional_dummy(const ASR::Variable_t *v) const {
        return v && v->m_presence == ASR::presenceType::Optional
            && !is_allocatable(v->m_type);
    }

    ASR::ttype_t *unalloc(ASR::ttype_t *t) const {
        while (t && ASR::is_a<ASR::Allocatable_t>(*t)) {
            t = ASR::down_cast<ASR::Allocatable_t>(t)->m_type;
        }
        return t;
    }

    ASR::ttype_t *unalloc_type(ASR::expr_t *e) const {
        return unalloc(ASRUtils::expr_type(e));
    }

    static ASR::expr_t *unwrap_physical_cast(ASR::expr_t *e) {
        while (e) {
            if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
                e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
            } else if (ASR::is_a<ASR::StringPhysicalCast_t>(*e)) {
                e = ASR::down_cast<ASR::StringPhysicalCast_t>(e)->m_arg;
            } else {
                break;
            }
        }
        return e;
    }

    bool is_option_expr(ASR::expr_t *e) const {
        e = unwrap_physical_cast(e);
        if (!e) return false;
        if (ASR::is_a<ASR::Var_t>(*e)) {
            ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(e)->m_v);
            if (ASR::is_a<ASR::Variable_t>(*s)) {
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
                return v->m_storage != ASR::storage_typeType::Parameter
                    && (is_allocatable(v->m_type) || is_optional_dummy(v));
            }
            return false;
        }
        if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
            auto &m = *ASR::down_cast<ASR::StructInstanceMember_t>(e);
            if (is_array(ASRUtils::expr_type(m.m_v))) return false;
            ASR::symbol_t *s = ASRUtils::symbol_get_past_external(m.m_m);
            if (ASR::is_a<ASR::Variable_t>(*s)) {
                return is_allocatable(ASR::down_cast<ASR::Variable_t>(s)->m_type);
            }
            return false;
        }
        if (ASR::is_a<ASR::FunctionCall_t>(*e)) {
            ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::FunctionCall_t>(e)->m_name);
            if (ASR::is_a<ASR::Function_t>(*s)) {
                ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(s);
                if (fn->m_return_var && ASR::is_a<ASR::Var_t>(*fn->m_return_var)) {
                    return is_allocatable(ASRUtils::EXPR2VAR(fn->m_return_var)->m_type);
                }
            }
            return false;
        }
        return false;
    }

    ASR::ttype_t *scalar(ASR::ttype_t *t, const Location &loc) {
        t = unalloc(t);
        if (!t || ASR::is_a<ASR::Array_t>(*t) || ASR::is_a<ASR::Pointer_t>(*t)) {
            unsupported("this type is", loc);
        }
        return t;
    }

    std::string type(ASR::ttype_t *t, const Location &loc, ASR::symbol_t *type_decl = nullptr) {
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
        if (ASR::is_a<ASR::StructType_t>(*t)) {
            auto &st = *ASR::down_cast<ASR::StructType_t>(t);
            if (st.m_is_unlimited_polymorphic || ASRUtils::is_class_type(t)) {
                unsupported("a polymorphic derived type is", loc);
            }
            return struct_qname(resolve_struct(type_decl, loc), loc) + " K";
        }
        if (ASR::is_a<ASR::EnumType_t>(*t)) {
            ASR::symbol_t *es = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::EnumType_t>(t)->m_enum_type);
            if (ASR::is_a<ASR::Enum_t>(*es)) {
                return type(ASR::down_cast<ASR::Enum_t>(es)->m_type, loc);
            }
        }
        unsupported("this type is", loc);
    }

    std::string zero(ASR::ttype_t *t, const Location &loc, ASR::symbol_t *type_decl = nullptr) {
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
        if (ASR::is_a<ASR::StructType_t>(*t)) {
            auto &st = *ASR::down_cast<ASR::StructType_t>(t);
            if (st.m_is_unlimited_polymorphic || ASRUtils::is_class_type(t)) {
                unsupported("a polymorphic derived type is", loc);
            }
            return struct_init_qname(resolve_struct(type_decl, loc), loc) + " (K := K)";
        }
        if (ASR::is_a<ASR::EnumType_t>(*t)) {
            ASR::symbol_t *es = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::EnumType_t>(t)->m_enum_type);
            if (ASR::is_a<ASR::Enum_t>(*es)) {
                return zero(ASR::down_cast<ASR::Enum_t>(es)->m_type, loc);
            }
        }
        unsupported("this type is", loc);
    }

    bool is_array(ASR::ttype_t *t) const {
        t = unalloc(t);
        return t && ASR::is_a<ASR::Array_t>(*t);
    }

    // The type of a variable: a scalar type, an `Arr`, or `Option` of one.
    std::string vtype(ASR::ttype_t *t, const Location &loc, ASR::symbol_t *type_decl = nullptr) {
        if (is_allocatable(t)) {
            std::string inner = vtype(unalloc(t), loc, type_decl);
            if (inner.find(' ') != std::string::npos) inner = "(" + inner + ")";
            return "Option " + inner;
        }
        if (is_array(t)) {
            std::string et = type(ASR::down_cast<ASR::Array_t>(t)->m_type, loc, type_decl);
            if (et.find(' ') != std::string::npos) et = "(" + et + ")";
            return "Arr " + et;
        }
        return type(t, loc, type_decl);
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
        ASR::Array_t *a = ASR::down_cast<ASR::Array_t>(unalloc(t));
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
                && !ASR::is_a<ASR::ArrayConstructor_t>(*e)
                && !ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
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
        int64_t n = ASRUtils::get_fixed_size_of_array(x.m_type);
        if (n < 0 || n > 4096 || (n > 0 && !x.m_data)) {
            unsupported("this array expression is", loc);
        }
        ASR::symbol_t *td = nullptr;
        if (ASR::is_a<ASR::StructType_t>(*et)) {
            if (n == 0) unsupported("this array expression is", loc);
            td = expr_type_decl(static_cast<ASR::expr_t**>(x.m_data)[0]);
        }
        type(et, loc, td);
        std::vector<std::string> lbs, exts;
        bool assumed;
        dims(x.m_type, lbs, exts, assumed, loc);
        if (assumed) unsupported("this array expression is", loc);
        int64_t str_len = -1;
        if (ASR::is_a<ASR::String_t>(*et)) {
            ASR::expr_t *le = ASR::down_cast<ASR::String_t>(et)->m_len;
            ASR::expr_t *lv = le ? ASRUtils::expr_value(le) : nullptr;
            if (!lv || !ASR::is_a<ASR::IntegerConstant_t>(*lv)) {
                unsupported("this array expression is", loc);
            }
            str_len = ASR::down_cast<ASR::IntegerConstant_t>(lv)->m_n;
            if (str_len < 0) unsupported("this array expression is", loc);
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
            } else if (ASR::is_a<ASR::String_t>(*et)) {
                const char *dc = static_cast<const char*>(x.m_data);
                elems += string_lit(std::string(dc + i * str_len, static_cast<size_t>(str_len)));
            } else if (ASR::is_a<ASR::StructType_t>(*et)) {
                ASR::expr_t *el = static_cast<ASR::expr_t**>(x.m_data)[i];
                if (!el) unsupported("this array expression is", loc);
                elems += lower(el).src;
            } else {
                unsupported("this array expression is", loc);
            }
        }
        elems += "]";
        return {"({ lbs := " + int_array(lbs) + ", exts := " + int_array(exts)
            + ", data := " + elems + " } : " + vtype(x.m_type, loc, td) + ")", pAtom};
    }

    std::string implied_do_array(const ASR::ImpliedDoLoop_t &x, ASR::ttype_t *elem_ty,
            const Location &loc) {
        if (!x.m_var || !ASR::is_a<ASR::Var_t>(*x.m_var) || !x.m_start || !x.m_end) {
            unsupported("this implied do loop is", loc);
        }
        ASR::Variable_t *iv = ASRUtils::EXPR2VAR(x.m_var);
        ASR::ttype_t *ivt = unalloc(ASRUtils::expr_type(x.m_var));
        std::string ity = type(ivt, loc);
        int ik = ASRUtils::extract_kind_from_ttype_t(ivt);
        auto bound_in_kind = [&](ASR::expr_t *b) -> std::string {
            ASR::ttype_t *bt = unalloc(ASRUtils::expr_type(b));
            if (ASRUtils::extract_kind_from_ttype_t(bt) != ik) {
                return "(FInt.ofInt " + atom(int_expr(b)) + " : " + ity + ")";
            }
            return atom(lower(b));
        };
        std::string lo = "(" + bound_in_kind(x.m_start) + " : " + ity + ")";
        std::string hi = bound_in_kind(x.m_end);
        bool unit = !x.m_increment
            || (ASR::is_a<ASR::IntegerConstant_t>(*x.m_increment)
                && ASR::down_cast<ASR::IntegerConstant_t>(x.m_increment)->m_n == 1);
        std::string range = unit
            ? "(FInt.range " + lo + " " + hi + ")"
            : "(FInt.rangeStep " + lo + " " + hi + " " + bound_in_kind(x.m_increment) + ")";
        std::string vn = "id'" + std::to_string(++loop_id);
        std::string acc = "acc'" + std::to_string(++loop_id);
        auto prev = implied_vars.find(iv);
        bool had_prev = prev != implied_vars.end();
        std::string prev_name = had_prev ? prev->second : "";
        implied_vars[iv] = vn;
        std::string body = acc;
        for (size_t i = 0; i < x.n_values; i++) {
            std::string chunk = array_ctor_chunk(x.m_values[i], elem_ty, loc);
            if (chunk.find("←") != std::string::npos) {
                if (had_prev) implied_vars[iv] = prev_name;
                else implied_vars.erase(iv);
                unsupported("a monadic expression in an implied do loop is", loc);
            }
            body += " ++ " + chunk;
        }
        if (had_prev) implied_vars[iv] = prev_name;
        else implied_vars.erase(iv);
        return "(" + range + ".foldl (fun " + acc + " " + vn + " => " + body + ") #[])";
    }

    std::string array_ctor_chunk(ASR::expr_t *arg, ASR::ttype_t *elem_ty, const Location &loc) {
        if (ASR::is_a<ASR::ImpliedDoLoop_t>(*arg)) {
            return implied_do_array(*ASR::down_cast<ASR::ImpliedDoLoop_t>(arg), elem_ty, loc);
        }
        if (is_array(ASRUtils::expr_type(arg))) {
            std::string s = atom(lower(arg)) + ".data";
            if (ASR::is_a<ASR::String_t>(*elem_ty) && ASR::down_cast<ASR::String_t>(elem_ty)->m_len) {
                s = "(" + s + ".map (FStr.fix "
                    + extent(ASR::down_cast<ASR::String_t>(elem_ty)->m_len) + "))";
            }
            return s;
        }
        return "#[" + fix_string(elem_ty, "", lower(arg), loc) + "]";
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
        bool has_complex = false;
        for (size_t i = 0; i < x.n_args; i++) {
            if (is_array(ASRUtils::expr_type(x.m_args[i]))
                    || ASR::is_a<ASR::ImpliedDoLoop_t>(*x.m_args[i])) {
                has_complex = true;
                break;
            }
        }
        ASR::symbol_t *td = x.n_args > 0 ? expr_type_decl(x.m_args[0]) : nullptr;
        if (!has_complex) {
            std::string elems = "#[";
            for (size_t i = 0; i < x.n_args; i++) {
                if (i) elems += ", ";
                elems += fix_string(at->m_type, "", lower(x.m_args[i]), loc);
            }
            elems += "]";
            return {"({ lbs := #[1], exts := #[" + std::to_string(x.n_args)
                + "], data := " + elems + " } : " + vtype(x.m_type, loc, td) + ")", pAtom};
        }
        std::string data = "#[]";
        for (size_t i = 0; i < x.n_args; i++) {
            std::string chunk = array_ctor_chunk(x.m_args[i], at->m_type, loc);
            data = (i == 0) ? chunk : "(" + data + " ++ " + chunk + ")";
        }
        std::string dv = "d'" + std::to_string(++loop_id);
        return {"((fun (" + dv + " : Array (" + type(at->m_type, loc, td)
            + ")) => Arr.mk #[1] #[" + dv + ".size] " + dv + ") " + data + ")", pAtom};
    }

    bool is_int(ASR::expr_t *e) {
        return ASR::is_a<ASR::Integer_t>(*unalloc(ASRUtils::expr_type(e)));
    }

    bool is_real(ASR::expr_t *e) {
        return ASR::is_a<ASR::Real_t>(*unalloc(ASRUtils::expr_type(e)));
    }

    bool is_complex(ASR::expr_t *e) {
        return ASR::is_a<ASR::Complex_t>(*unalloc(ASRUtils::expr_type(e)));
    }

    bool is_logical(ASR::expr_t *e) {
        return ASR::is_a<ASR::Logical_t>(*unalloc(ASRUtils::expr_type(e)));
    }

    bool is_string(ASR::expr_t *e) {
        return ASR::is_a<ASR::String_t>(*unalloc(ASRUtils::expr_type(e)));
    }

    bool is_assumed_len_string(ASR::ttype_t *t) {
        t = unalloc(t);
        return t && ASR::is_a<ASR::String_t>(*t)
            && !ASR::down_cast<ASR::String_t>(t)->m_len;
    }

    std::string fix_string(ASR::ttype_t *t, const std::string &var_name, const Val &rhs,
            const Location &loc) {
        t = unalloc(t);
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
        t = unalloc(t);
        return ASR::is_a<ASR::Array_t>(*t) ? ASR::down_cast<ASR::Array_t>(t)->m_type : t;
    }

    // Lift a binary operation on scalar expressions to arrays (handling
    // scalar or ArrayBroadcast on either side via Arr.map, or two arrays via Arr.zipWith).
    Val lift_binop(ASR::expr_t *l, ASR::expr_t *r,
            const std::function<std::string(const std::string&, const std::string&)> &fn) {
        l = unwrap_physical_cast(l);
        r = unwrap_physical_cast(r);
        bool lb = ASR::is_a<ASR::ArrayBroadcast_t>(*l);
        bool rb = ASR::is_a<ASR::ArrayBroadcast_t>(*r);
        bool ls_scalar = lb || !is_array(ASRUtils::expr_type(l));
        bool rs_scalar = rb || !is_array(ASRUtils::expr_type(r));
        if (ls_scalar && !rs_scalar) {
            ASR::expr_t *ls = lb ? ASR::down_cast<ASR::ArrayBroadcast_t>(l)->m_array : l;
            if (is_array(ASRUtils::expr_type(ls))) unsupported("this array expression is", l->base.loc);
            std::string sv = atom(operand(ls));
            return app("Arr.map", {{"((fun s' y' => " + fn("s'", "y'") + ") " + sv + ")", pAtom},
                lower(r)});
        }
        if (rs_scalar && !ls_scalar) {
            ASR::expr_t *rs = rb ? ASR::down_cast<ASR::ArrayBroadcast_t>(r)->m_array : r;
            if (is_array(ASRUtils::expr_type(rs))) unsupported("this array expression is", r->base.loc);
            std::string sv = atom(operand(rs));
            return app("Arr.map", {{"((fun s' x' => " + fn("x'", "s'") + ") " + sv + ")", pAtom},
                lower(l)});
        }
        if (!ls_scalar && !rs_scalar) {
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
            case ASR::exprType::ArrayRank:
            case ASR::exprType::IntrinsicArrayFunction:
            case ASR::exprType::IntrinsicImpureFunction:
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
            case ASR::exprType::StructInstanceMember:
            case ASR::exprType::StructConstructor:
            case ASR::exprType::StructConstant:
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
                            == ASR::down_cast<ASR::Real_t>(unalloc_type(c.m_arg))->m_kind) {
                    return determined(c.m_arg);
                }
                if (c.m_kind == ASR::cast_kindType::RealToComplex
                        && ASR::down_cast<ASR::Complex_t>(c.m_type)->m_kind
                            == ASR::down_cast<ASR::Real_t>(unalloc_type(c.m_arg))->m_kind) {
                    return determined(c.m_arg);
                }
                if (c.m_kind == ASR::cast_kindType::ComplexToReal
                        && ASR::down_cast<ASR::Real_t>(c.m_type)->m_kind
                            == ASR::down_cast<ASR::Complex_t>(unalloc_type(c.m_arg))->m_kind) {
                    return determined(c.m_arg);
                }
                if (c.m_kind == ASR::cast_kindType::ComplexToComplex
                        && ASR::down_cast<ASR::Complex_t>(c.m_type)->m_kind
                            == ASR::down_cast<ASR::Complex_t>(unalloc_type(c.m_arg))->m_kind) {
                    return determined(c.m_arg);
                }
                return true;
            }
            case ASR::exprType::IntrinsicElementalFunction: {
                auto &x = *ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e);
                std::string f = ASRUtils::get_intrinsic_name(x.m_intrinsic_id);
                std::transform(f.begin(), f.end(), f.begin(), ::tolower);
                if (f == "int" || f == "real" || f == "dble" || f == "cmplx"
                        || f == "floor" || f == "ceiling" || f == "nint"
                        || f == "present" || f == "stringlentrim" || f == "substrindex" || f == "stringtrim"
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
                + type(ASRUtils::expr_type(e), e->base.loc, expr_type_decl(e)) + ")", pAtom};
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

    std::string escape_ns(const std::string &raw, const Location &loc) {
        if (raw.empty()) return "";
        std::string res;
        size_t pos = 0;
        while (pos <= raw.size()) {
            size_t dot = raw.find('.', pos);
            std::string seg = dot == std::string::npos ? raw.substr(pos) : raw.substr(pos, dot - pos);
            if (!res.empty()) res += ".";
            res += (seg.find('\'') != std::string::npos) ? seg : name(seg, loc);
            if (dot == std::string::npos) break;
            pos = dot + 1;
        }
        return res;
    }

    std::string call_name(ASR::symbol_t *sym) {
        sym = ASRUtils::symbol_get_past_external(sym);
        std::string n = name(ASRUtils::symbol_name(sym), sym->base.loc);
        if (ASR::is_a<ASR::Function_t>(*sym)) {
            auto it = proc_ns.find(ASR::down_cast<ASR::Function_t>(sym));
            if (it != proc_ns.end() && !it->second.empty()) {
                std::string mn = escape_ns(it->second, sym->base.loc);
                size_t dot = it->second.find('.');
                std::string head = dot == std::string::npos ? it->second : it->second.substr(0, dot);
                if (name_in_scope(head)) {
                    return "_root_." + mn + "." + n;
                }
                return mn + "." + n;
            }
        }
        if (ASR::Module_t *m = ASRUtils::get_sym_module(sym)) {
            std::string mn = name(m->m_name, m->base.base.loc);
            if (name_in_scope(m->m_name)) {
                return "_root_." + mn + "." + n;
            }
            return mn + "." + n;
        }
        if (name_in_scope(ASRUtils::symbol_name(sym))) {
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

    bool cur_has_io() const {
        return in_program || (cur && io_procs.count(cur->fn) > 0);
    }

    bool cur_has_globals() const {
        return in_program ? prog_uses_globals : (cur && global_procs.count(cur->fn) > 0);
    }

    std::pair<bool, std::string> monadic_call(ASR::Function_t *fn, const Val &c) const {
        bool cal_io = io_procs.count(fn) > 0;
        bool cal_gl = global_procs.count(fn) > 0;
        if (!cal_io && !cal_gl) return {false, c.src};
        if (cal_gl && !cal_io && cur_has_io()) {
            return {true, "modifyGet (" + c.src + ").run"};
        }
        if (cal_io && !cal_gl && cur_has_globals()) {
            return {true, "liftM (" + c.src + ")"};
        }
        return {true, c.src};
    }

    ASR::Variable_t *global_var(ASR::expr_t *e) const {
        if (!e || !ASR::is_a<ASR::Var_t>(*e)) return nullptr;
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(e)->m_v);
        if (!ASR::is_a<ASR::Variable_t>(*s)) return nullptr;
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
        return (!var_alias.count(v) && global_fields.count(v)) ? v : nullptr;
    }

    // The application of a procedure to the arguments it takes: every
    // argument except an `intent(out)` scalar.
    Val call(ASR::symbol_t *sym, ASR::call_arg_t *args, size_t n,
            const Location &loc) {
        if (!cur) unsupported("a function call in a static initializer is", loc);
        ASR::Function_t *fn = callee(sym, loc);
        cur->callees.insert(fn);
        if (n != fn->n_args) unsupported("this call is", loc);
        std::string src = call_name((ASR::symbol_t*) fn) + " (K := K)";
        for (size_t i = 0; i < n; i++) {
            ASR::Variable_t *d = dummy(fn, i);
            if (!d) unsupported("a procedure argument is", loc);
            ASR::intentType in = arg_intent(fn, i);
            if (is_optional_dummy(d)) {
                if (!args[i].m_value) {
                    src += " none";
                    continue;
                }
                ASR::expr_t *a = args[i].m_value;
                if (is_array(d->m_type) != is_array(ASRUtils::expr_type(a))) {
                    unsupported("this argument is", a->base.loc);
                }
                ASR::expr_t *au = unwrap_physical_cast(a);
                if (is_array(d->m_type) && ASR::is_a<ASR::ArraySection_t>(*au)) {
                    ASR::Array_t *da = ASR::down_cast<ASR::Array_t>(unalloc(d->m_type));
                    ASR::Array_t *aa = ASR::down_cast<ASR::Array_t>(unalloc_type(au));
                    if (da->n_dims != aa->n_dims) {
                        unsupported("legacy sequence association of an array section is", a->base.loc);
                    }
                }
                if (is_option_expr(a)) {
                    src += " " + atom(lower_raw(a));
                } else {
                    src += " (some " + atom(lower(a)) + ")";
                }
                continue;
            }
            if (!args[i].m_value) unsupported("an omitted argument is", loc);
            ASR::expr_t *a = args[i].m_value;
            if (is_array(d->m_type) != is_array(ASRUtils::expr_type(a))) {
                unsupported("this argument is", a->base.loc);
            }
            if (in == ASR::intentType::Out && (is_allocatable(d->m_type)
                    || (!is_array(d->m_type) && !is_assumed_len_string(d->m_type)))) continue;
            ASR::expr_t *au = unwrap_physical_cast(a);
            if (is_array(d->m_type) && ASR::is_a<ASR::ArraySection_t>(*au)) {
                ASR::Array_t *da = ASR::down_cast<ASR::Array_t>(unalloc(d->m_type));
                ASR::Array_t *aa = ASR::down_cast<ASR::Array_t>(unalloc_type(au));
                if (da->n_dims != aa->n_dims) {
                    unsupported("legacy sequence association of an array section is", a->base.loc);
                }
            }
            if (is_allocatable(d->m_type)) {
                if (!is_option_expr(a)) {
                    unsupported("passing a non-allocatable to an allocatable dummy is", a->base.loc);
                }
                src += " " + atom(lower_raw(a));
                continue;
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
        if (x.n_args == 1 && f == "present") {
            ASR::expr_t *a = unwrap_physical_cast(x.m_args[0]);
            if (a && ASR::is_a<ASR::Var_t>(*a)) {
                ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(a)->m_v);
                if (ASR::is_a<ASR::Variable_t>(*s)
                        && is_optional_dummy(ASR::down_cast<ASR::Variable_t>(s))) {
                    return {atom(lower_raw(a)) + ".isSome", pAtom};
                }
            }
            unsupported("the intrinsic 'present' is", loc);
        }
        // `int(x)`: truncation toward zero, in the result's integer kind.
        if (x.n_args >= 1 && f == "int") {
            if (is_real(x.m_args[0])) {
                return {"(FInt.ofInt " + atom(app("FReal.toInt", {operand(x.m_args[0])}))
                    + " : " + type(x.m_type, loc) + ")", pAtom};
            }
            if (is_int(x.m_args[0])) {
                if (ASR::is_a<ASR::IntegerConstant_t>(*x.m_args[0])) {
                    auto &ic = *ASR::down_cast<ASR::IntegerConstant_t>(x.m_args[0]);
                    if (ic.m_intboz_type != ASR::integerbozType::Decimal) {
                        return {"(FInt.ofInt " + int_lit(ic.m_n) + " : "
                            + type(x.m_type, loc) + ")", pAtom};
                    }
                }
                return {"(FInt.ofInt " + atom(app("FInt.toInt", {operand(x.m_args[0])}))
                    + " : " + type(x.m_type, loc) + ")", pAtom};
            }
            if (is_complex(x.m_args[0])) {
                return {"(FInt.ofInt " + atom(app("FReal.toInt",
                    {{atom(operand(x.m_args[0])) + ".re", pAtom}}))
                    + " : " + type(x.m_type, loc) + ")", pAtom};
            }
        }
        if (x.n_args >= 1 && (f == "real" || f == "dble")) {
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
                int src_k = ASR::down_cast<ASR::Real_t>(unalloc_type(x.m_args[0]))->m_kind;
                if (src_k == dst_k) return {"(" + lower(x.m_args[0]).src + " : " + dst_ty + ")", pAtom};
                return app(dst_k == 8 ? "FKinds.r4to8" : "FKinds.r8to4", {operand(x.m_args[0])});
            }
            if (is_complex(x.m_args[0])) {
                type(ASRUtils::expr_type(x.m_args[0]), x.m_args[0]->base.loc);
                int dst_k = ASR::down_cast<ASR::Real_t>(x.m_type)->m_kind;
                int src_k = ASR::down_cast<ASR::Complex_t>(unalloc_type(x.m_args[0]))->m_kind;
                std::string re = atom(operand(x.m_args[0])) + ".re";
                if (src_k == dst_k) return {"(" + re + " : " + dst_ty + ")", pAtom};
                return app(dst_k == 8 ? "FKinds.r4to8" : "FKinds.r8to4", {{re, pAtom}});
            }
        }
        if (x.n_args >= 1 && f == "cmplx" && !is_array(x.m_type)) {
            std::string dst_ty = type(x.m_type, loc);
            int dst_k = ASR::down_cast<ASR::Complex_t>(x.m_type)->m_kind;
            auto to_real_part = [&](ASR::expr_t *p) -> Val {
                if (is_int(p)) {
                    if (ASR::is_a<ASR::IntegerConstant_t>(*p)) {
                        auto &ic = *ASR::down_cast<ASR::IntegerConstant_t>(p);
                        if (ic.m_intboz_type != ASR::integerbozType::Decimal) {
                            unsupported("a BOZ argument to cmplx is", p->base.loc);
                        }
                        return {"FReal.ofInt " + int_lit(ic.m_n), pApp};
                    }
                    return app("FReal.ofInt", {app("FInt.toInt", {operand(p)})});
                }
                if (is_real(p)) {
                    type(ASRUtils::expr_type(p), p->base.loc);
                    int pk = ASR::down_cast<ASR::Real_t>(unalloc_type(p))->m_kind;
                    if (pk == dst_k) return lower(p);
                    return app(dst_k == 8 ? "FKinds.r4to8" : "FKinds.r8to4", {operand(p)});
                }
                unsupported("this cmplx argument is", p->base.loc);
            };
            if (is_complex(x.m_args[0])) {
                type(ASRUtils::expr_type(x.m_args[0]), x.m_args[0]->base.loc);
                int src_k = ASR::down_cast<ASR::Complex_t>(unalloc_type(x.m_args[0]))->m_kind;
                if (src_k == dst_k) return {"(" + lower(x.m_args[0]).src + " : " + dst_ty + ")", pAtom};
                std::string fn = dst_k == 8 ? "FKinds.r4to8" : "FKinds.r8to4";
                return {"((fun z' => FCplx.mk (" + fn + " z'.re) (" + fn + " z'.im)) "
                    + atom(operand(x.m_args[0])) + ")", pAtom};
            }
            Val re = to_real_part(x.m_args[0]);
            Val im = (x.n_args >= 2 && x.m_args[1]) ? to_real_part(x.m_args[1]) : Val{"FReal.zero", pAtom};
            return {"(" + app("FCplx.mk", {re, im}).src + " : " + dst_ty + ")", pAtom};
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
                int ak = ASR::down_cast<ASR::Integer_t>(unalloc_type(e))->m_kind;
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
        if (x.n_args == 3 && f == "merge" && is_array(x.m_type)) {
            auto sum_arg = [&](ASR::expr_t *arg) -> std::string {
                arg = unwrap_physical_cast(arg);
                bool b = ASR::is_a<ASR::ArrayBroadcast_t>(*arg);
                if (b || !is_array(ASRUtils::expr_type(arg))) {
                    ASR::expr_t *s = b ? ASR::down_cast<ASR::ArrayBroadcast_t>(arg)->m_array : arg;
                    if (is_array(ASRUtils::expr_type(s))) unsupported("the intrinsic 'merge' is", loc);
                    return "(Sum.inl " + atom(operand(s)) + ")";
                }
                return "(Sum.inr " + atom(lower(arg)) + ")";
            };
            ASR::symbol_t *td = expr_type_decl(x.m_args[0]);
            if (!td) td = expr_type_decl(x.m_args[1]);
            return {"(" + app("Arr.merge", {{sum_arg(x.m_args[0]), pAtom},
                {sum_arg(x.m_args[1]), pAtom}, {sum_arg(x.m_args[2]), pAtom}}).src
                + " : " + vtype(unalloc(x.m_type), loc, td) + ")", pAtom};
        }
        if (x.n_args == 1 && f == "aimag" && is_complex(x.m_args[0])) {
            type(ASRUtils::expr_type(x.m_args[0]), x.m_args[0]->base.loc);
            return {atom(operand(x.m_args[0])) + ".im", pAtom};
        }
        if ((x.n_args == 1 || x.n_args == 2) && (f == "iachar" || f == "ichar")
                && is_string(x.m_args[0])) {
            return {"(FInt.ofInt " + atom(app("FStr.ord", {lower(x.m_args[0])}))
                + " : " + type(x.m_type, loc) + ")", pAtom};
        }
        if ((x.n_args == 1 || x.n_args == 2) && (f == "achar" || f == "char")
                && is_int(x.m_args[0])) {
            type(x.m_type, loc);
            return app("FStr.chr", {int_expr(x.m_args[0])});
        }
        if (!is_array(x.m_type)) {
            if (x.n_args == 1 && f == "not" && is_int(x.m_args[0])) {
                std::string w = std::to_string(8 * ASR::down_cast<ASR::Integer_t>(unalloc_type(x.m_args[0]))->m_kind);
                return app("FInt.inot", {{w, pAtom}, operand(x.m_args[0])});
            }
            if (x.n_args == 2 && (f == "iand" || f == "ior" || f == "ieor")
                    && is_int(x.m_args[0]) && is_int(x.m_args[1])) {
                int k0 = ASR::down_cast<ASR::Integer_t>(unalloc_type(x.m_args[0]))->m_kind;
                int k1 = ASR::down_cast<ASR::Integer_t>(unalloc_type(x.m_args[1]))->m_kind;
                if (k0 == k1) {
                    std::string w = std::to_string(8 * k0);
                    return app("FInt." + f, {{w, pAtom}, operand(x.m_args[0]), operand(x.m_args[1])});
                }
            }
            if (x.n_args == 2 && (f == "ishft" || f == "shiftl" || f == "shiftr")
                    && is_int(x.m_args[0]) && is_int(x.m_args[1])) {
                std::string w = std::to_string(8 * ASR::down_cast<ASR::Integer_t>(unalloc_type(x.m_args[0]))->m_kind);
                Val sh = int_expr(x.m_args[1]);
                if (f == "shiftr") sh = {"(-" + atom(sh) + ")", pAtom};
                return app("FInt.ishft", {{w, pAtom}, operand(x.m_args[0]), sh});
            }
            if (x.n_args == 2 && (f == "btest" || f == "ibset" || f == "ibclr")
                    && is_int(x.m_args[0]) && is_int(x.m_args[1])) {
                std::string w = std::to_string(8 * ASR::down_cast<ASR::Integer_t>(unalloc_type(x.m_args[0]))->m_kind);
                return app("FInt." + f, {{w, pAtom}, operand(x.m_args[0]), int_expr(x.m_args[1])});
            }
            if (x.n_args == 3 && f == "ibits"
                    && is_int(x.m_args[0]) && is_int(x.m_args[1]) && is_int(x.m_args[2])) {
                std::string w = std::to_string(8 * ASR::down_cast<ASR::Integer_t>(unalloc_type(x.m_args[0]))->m_kind);
                return app("FInt.ibits", {{w, pAtom}, operand(x.m_args[0]),
                    int_expr(x.m_args[1]), int_expr(x.m_args[2])});
            }
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
        if ((x.n_args == 1 || (x.n_args == 2 && (f == "iachar" || f == "ichar"
                || f == "achar" || f == "char" || f == "int" || f == "floor"
                || f == "ceiling" || f == "nint")))
                && is_array(x.m_type) && is_array(ASRUtils::expr_type(x.m_args[0]))) {
            ASR::ttype_t *et = elem_type(ASRUtils::expr_type(x.m_args[0]));
            std::string body;
            if (f == "abs") {
                if (ASR::is_a<ASR::Real_t>(*et)) body = "FReal.abs x'";
                else if (ASR::is_a<ASR::Integer_t>(*et)) {
                    body = "(FInt.ofInt (Int.natAbs (FInt.toInt x')) : " + type(et, loc) + ")";
                } else if (ASR::is_a<ASR::Complex_t>(*et)) {
                    level = std::max(level, 1);
                    body = "FCplx.abs x'";
                }
            } else if (f == "aimag" && ASR::is_a<ASR::Complex_t>(*et)) {
                body = "x'.im";
            } else if (f == "not" && ASR::is_a<ASR::Integer_t>(*et)) {
                std::string w = std::to_string(8 * ASR::down_cast<ASR::Integer_t>(et)->m_kind);
                body = "FInt.inot " + w + " x'";
            } else if (math.count(f) && ASR::is_a<ASR::Real_t>(*et)) {
                level = std::max(level, 1);
                body = "FRealMath." + f + " x'";
            } else if ((f == "int" || f == "floor" || f == "ceiling" || f == "nint")
                    && ASR::is_a<ASR::Real_t>(*et)) {
                std::string h = f == "int" ? "FReal.toInt" : f == "floor" ? "FReal.floorInt"
                    : f == "ceiling" ? "FReal.ceilInt" : "FReal.nintInt";
                body = "(FInt.ofInt (" + h + " x') : " + type(elem_type(x.m_type), loc) + ")";
            } else if (f == "int" && ASR::is_a<ASR::Integer_t>(*et)) {
                body = "(FInt.ofInt (FInt.toInt x') : " + type(elem_type(x.m_type), loc) + ")";
            } else if ((f == "iachar" || f == "ichar") && ASR::is_a<ASR::String_t>(*et)) {
                body = "(FInt.ofInt (FStr.ord x') : " + type(elem_type(x.m_type), loc) + ")";
            } else if ((f == "achar" || f == "char") && ASR::is_a<ASR::Integer_t>(*et)) {
                type(elem_type(x.m_type), loc);
                body = "FStr.chr (FInt.toInt x')";
            } else if (f == "stringtrim" && ASR::is_a<ASR::String_t>(*et)) {
                body = "FStr.trim x'";
            } else if (f == "stringlentrim" && ASR::is_a<ASR::String_t>(*et)) {
                body = "(FInt.ofInt (FStr.lenTrim x') : " + type(elem_type(x.m_type), loc) + ")";
            } else if ((f == "adjustl" || f == "adjustr") && ASR::is_a<ASR::String_t>(*et)) {
                body = "FStr." + f + " x'";
            }
            if (!body.empty()) {
                return app("Arr.map", {{"(fun x' => " + body + ")", pAtom}, lower(x.m_args[0])});
            }
        }
        if (x.n_args == 2 && is_array(x.m_type)) {
            ASR::ttype_t *e0 = elem_type(ASRUtils::expr_type(x.m_args[0]));
            ASR::ttype_t *e1 = elem_type(ASRUtils::expr_type(x.m_args[1]));
            if ((f == "max" || f == "min") && e0->type == e1->type) {
                if (ASR::is_a<ASR::Integer_t>(*e0)
                        && ASR::down_cast<ASR::Integer_t>(e0)->m_kind
                            == ASR::down_cast<ASR::Integer_t>(e1)->m_kind) {
                    std::string fn = f == "max" ? "FInt.fmax" : "FInt.fmin";
                    return lift_binop(x.m_args[0], x.m_args[1],
                        [&](const std::string &a, const std::string &b) {
                            return fn + " " + a + " " + b;
                        });
                }
                if (ASR::is_a<ASR::Real_t>(*e0)
                        && ASR::down_cast<ASR::Real_t>(e0)->m_kind
                            == ASR::down_cast<ASR::Real_t>(e1)->m_kind) {
                    std::string fn = f == "max" ? "FReal.fmax" : "FReal.fmin";
                    return lift_binop(x.m_args[0], x.m_args[1],
                        [&](const std::string &a, const std::string &b) {
                            return fn + " " + a + " " + b;
                        });
                }
            } else if ((f == "mod" || f == "modulo" || f == "iand" || f == "ior" || f == "ieor")
                    && ASR::is_a<ASR::Integer_t>(*e0) && ASR::is_a<ASR::Integer_t>(*e1)
                    && ASR::down_cast<ASR::Integer_t>(e0)->m_kind
                        == ASR::down_cast<ASR::Integer_t>(e1)->m_kind) {
                int k = ASR::down_cast<ASR::Integer_t>(e0)->m_kind;
                std::string w = std::to_string(8 * k);
                bool bit = (f == "iand" || f == "ior" || f == "ieor");
                std::string fn = "FInt." + f + (bit ? " " + w : "");
                return lift_binop(x.m_args[0], x.m_args[1],
                    [&](const std::string &a, const std::string &b) {
                        return fn + " " + a + " " + b;
                    });
            } else if ((f == "ishft" || f == "shiftl" || f == "shiftr"
                    || f == "btest" || f == "ibset" || f == "ibclr")
                    && ASR::is_a<ASR::Integer_t>(*e0) && ASR::is_a<ASR::Integer_t>(*e1)) {
                std::string w = std::to_string(8 * ASR::down_cast<ASR::Integer_t>(e0)->m_kind);
                std::string fn = (f == "shiftl" || f == "shiftr") ? "FInt.ishft" : "FInt." + f;
                bool neg = (f == "shiftr");
                return lift_binop(x.m_args[0], x.m_args[1],
                    [&](const std::string &a, const std::string &b) {
                        std::string sh = neg ? "(-(FInt.toInt " + b + "))" : "(FInt.toInt " + b + ")";
                        return fn + " " + w + " " + a + " " + sh;
                    });
            } else if (f == "atan2" && ASR::is_a<ASR::Real_t>(*e0) && ASR::is_a<ASR::Real_t>(*e1)
                    && ASR::down_cast<ASR::Real_t>(e0)->m_kind
                        == ASR::down_cast<ASR::Real_t>(e1)->m_kind) {
                level = std::max(level, 1);
                return lift_binop(x.m_args[0], x.m_args[1],
                    [](const std::string &a, const std::string &b) {
                        return "FRealMath.atan2 " + a + " " + b;
                    });
            } else if (f == "stringconcat" && ASR::is_a<ASR::String_t>(*e0)
                    && ASR::is_a<ASR::String_t>(*e1)) {
                return lift_binop(x.m_args[0], x.m_args[1],
                    [](const std::string &a, const std::string &b) {
                        return "(" + a + " ++ " + b + ")";
                    });
            } else if (f == "substrindex" && ASR::is_a<ASR::String_t>(*e0)
                    && ASR::is_a<ASR::String_t>(*e1)) {
                std::string rty = type(elem_type(x.m_type), loc);
                return lift_binop(x.m_args[0], x.m_args[1],
                    [&](const std::string &a, const std::string &b) {
                        return "(FInt.ofInt (FStr.index " + a + " " + b + ") : " + rty + ")";
                    });
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
                int src_k = ASR::down_cast<ASR::Real_t>(unalloc_type(x.m_arg))->m_kind;
                if (src_k == dst_k) return lower(x.m_arg);
                return app(dst_k == 8 ? "FKinds.r4to8" : "FKinds.r8to4",
                    {operand(x.m_arg)});
            }
            case ASR::cast_kindType::RealToComplex: {
                type(x.m_type, loc);
                type(ASRUtils::expr_type(x.m_arg), x.m_arg->base.loc);
                int dst_k = ASR::down_cast<ASR::Complex_t>(x.m_type)->m_kind;
                int src_k = ASR::down_cast<ASR::Real_t>(unalloc_type(x.m_arg))->m_kind;
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
                int src_k = ASR::down_cast<ASR::Complex_t>(unalloc_type(x.m_arg))->m_kind;
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
                int src_k = ASR::down_cast<ASR::Complex_t>(unalloc_type(x.m_arg))->m_kind;
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
        auto it = pre_idx.find(&x);
        if (it != pre_idx.end()) return it->second;
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
        auto is_mask_arg = [&](ASR::expr_t *m) -> bool {
            return m && ASR::is_a<ASR::Logical_t>(*elem_type(ASRUtils::expr_type(m)));
        };
        bool scalar_red = !is_array(x.m_type) && ((x.n_args == 1)
            || (x.n_args == 2 && (!x.m_args[1] || is_mask_arg(x.m_args[1]) || is_int(x.m_args[1])))
            || (x.n_args == 3 && (!x.m_args[1] || is_int(x.m_args[1]))
                && (!x.m_args[2] || is_mask_arg(x.m_args[2]))));
        size_t mask_idx = (x.n_args == 2 && is_mask_arg(x.m_args[1])) ? 1
            : ((x.n_args == 3 && is_mask_arg(x.m_args[2])) ? 2 : x.n_args);
        ASR::expr_t *scalar_dim = (scalar_red && x.n_args >= 2 && x.m_args[1] && is_int(x.m_args[1]))
            ? x.m_args[1] : nullptr;
        auto wrap_scalar_dim = [&](Val res) -> Val {
            if (!scalar_dim || ASR::is_a<ASR::IntegerConstant_t>(*scalar_dim)) return res;
            return {"((fun r' (_ : Int) => r') " + atom(res) + " "
                + atom(int_expr(scalar_dim)) + ")", pAtom};
        };
        auto red_arr = [&]() -> std::string {
            if (mask_idx < x.n_args && x.m_args[mask_idx]) {
                return atom(lower_pack(x.m_args[0], x.m_args[mask_idx], loc));
            }
            return atom(lower(x.m_args[0]));
        };
        if (id == F::Sum && scalar_red
                && is_array(ASRUtils::expr_type(x.m_args[0]))) {
            ASR::ttype_t *et = elem_type(ASRUtils::expr_type(x.m_args[0]));
            std::string a = red_arr();
            if (ASR::is_a<ASR::Integer_t>(*et)) {
                return wrap_scalar_dim({"(" + a + ".data.foldl FInt.add (FInt.ofInt 0) : "
                    + type(x.m_type, loc) + ")", pAtom});
            }
            if (ASR::is_a<ASR::Real_t>(*et)) {
                return wrap_scalar_dim({"(" + a + ".data.foldl FReal.add (FReal.ofInt 0) : "
                    + type(x.m_type, loc) + ")", pAtom});
            }
            if (ASR::is_a<ASR::Complex_t>(*et)) {
                return wrap_scalar_dim({"(" + a + ".data.foldl FCplx.add FCplx.zero : "
                    + type(x.m_type, loc) + ")", pAtom});
            }
        }
        if (id == F::Product && scalar_red
                && is_array(ASRUtils::expr_type(x.m_args[0]))) {
            ASR::ttype_t *et = elem_type(ASRUtils::expr_type(x.m_args[0]));
            std::string a = red_arr();
            if (ASR::is_a<ASR::Integer_t>(*et)) {
                return wrap_scalar_dim({"(" + a + ".data.foldl FInt.mul (FInt.ofInt 1) : "
                    + type(x.m_type, loc) + ")", pAtom});
            }
            if (ASR::is_a<ASR::Real_t>(*et)) {
                return wrap_scalar_dim({"(" + a + ".data.foldl FReal.mul (FReal.ofInt 1) : "
                    + type(x.m_type, loc) + ")", pAtom});
            }
            if (ASR::is_a<ASR::Complex_t>(*et)) {
                return wrap_scalar_dim({"(" + a + ".data.foldl FCplx.mul (FCplx.ofReal (FReal.ofInt 1)) : "
                    + type(x.m_type, loc) + ")", pAtom});
            }
        }
        if ((id == F::Any || id == F::All) && !is_array(x.m_type)
                && (x.n_args == 1 || (x.n_args == 2 && (!x.m_args[1] || is_int(x.m_args[1]))))
                && is_array(ASRUtils::expr_type(x.m_args[0]))
                && ASR::is_a<ASR::Logical_t>(*elem_type(ASRUtils::expr_type(x.m_args[0])))) {
            std::string a = atom(lower(x.m_args[0]));
            return wrap_scalar_dim({a + ".data." + (id == F::Any ? "any" : "all")
                + " (fun b' => b')", pApp});
        }
        if (id == F::Count && !is_array(x.m_type)
                && (x.n_args == 1 || (x.n_args == 2 && (!x.m_args[1] || is_int(x.m_args[1]))))
                && is_array(ASRUtils::expr_type(x.m_args[0]))
                && ASR::is_a<ASR::Logical_t>(*elem_type(ASRUtils::expr_type(x.m_args[0])))) {
            std::string a = atom(lower(x.m_args[0]));
            return wrap_scalar_dim({"(FInt.ofInt (Int.ofNat (" + a
                + ".data.foldl (fun n' b' => if b' then n' + 1 else n') 0)) : "
                + type(x.m_type, loc) + ")", pAtom});
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
            if (ASR::is_a<ASR::Complex_t>(*et0) && ASR::is_a<ASR::Complex_t>(*et1)
                    && ASR::down_cast<ASR::Complex_t>(et0)->m_kind
                        == ASR::down_cast<ASR::Complex_t>(et1)->m_kind) {
                return {"((Array.zipWith (fun x' y' => FCplx.mul (FCplx.conj x') y') "
                    + a + ".data " + b + ".data).foldl FCplx.add FCplx.zero : "
                    + type(x.m_type, loc) + ")", pAtom};
            }
        }
        if ((id == F::Iany || id == F::Iall || id == F::Iparity) && scalar_red
                && is_array(ASRUtils::expr_type(x.m_args[0]))) {
            ASR::ttype_t *et = elem_type(ASRUtils::expr_type(x.m_args[0]));
            if (ASR::is_a<ASR::Integer_t>(*et)) {
                int k = ASR::down_cast<ASR::Integer_t>(et)->m_kind;
                std::string w = std::to_string(8 * k);
                std::string a = red_arr();
                std::string init = id == F::Iall ? "(FInt.ofInt (-1))" : "(FInt.ofInt 0)";
                std::string op = id == F::Iany ? "FInt.ior " + w
                    : id == F::Iall ? "FInt.iand " + w : "FInt.ieor " + w;
                return wrap_scalar_dim({"(" + a + ".data.foldl (" + op + ") " + init + " : "
                    + type(x.m_type, loc) + ")", pAtom});
            }
        }
        if ((id == F::MaxVal || id == F::MinVal) && scalar_red && !is_array(x.m_type)
                && is_array(ASRUtils::expr_type(x.m_args[0]))) {
            ASR::ttype_t *et = elem_type(ASRUtils::expr_type(x.m_args[0]));
            std::string a = red_arr();
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
                return wrap_scalar_dim({"((let d' := " + a + ".data; if d'.isEmpty then "
                    + empty_val + " else d'.foldl " + fn + " d'[0]!) : " + ty + ")", pAtom});
            }
        }
        // `dim=` reductions (`sum`, `product`, `any`, `all`, `count`, `maxval`, `minval`,
        // `iany`, `iall`, `iparity`) returning an array of rank - 1.
        if ((id == F::Sum || id == F::Product || id == F::Any || id == F::All
                || id == F::Count || id == F::MaxVal || id == F::MinVal
                || id == F::Iany || id == F::Iall || id == F::Iparity)
                && is_array(x.m_type) && x.n_args >= 2 && x.m_args[1]
                && is_int(x.m_args[1]) && is_array(ASRUtils::expr_type(x.m_args[0]))) {
            ASR::expr_t *mask = (x.n_args >= 3 && x.m_args[2] && is_mask_arg(x.m_args[2]))
                ? x.m_args[2] : nullptr;
            if (x.n_args <= 3 && (!mask || is_array(ASRUtils::expr_type(mask)))) {
                ASR::ttype_t *et = elem_type(ASRUtils::expr_type(x.m_args[0]));
                ASR::ttype_t *rt = elem_type(x.m_type);
                std::string rty = type(rt, loc);
                std::string init, op;
                if (id == F::Sum || id == F::Product) {
                    if (ASR::is_a<ASR::Integer_t>(*et)) {
                        init = "(" + std::string(id == F::Sum ? "FInt.ofInt 0" : "FInt.ofInt 1") + " : " + rty + ")";
                        op = id == F::Sum ? "FInt.add" : "FInt.mul";
                    } else if (ASR::is_a<ASR::Real_t>(*et)) {
                        init = "(" + std::string(id == F::Sum ? "FReal.zero" : "FReal.ofInt 1") + " : " + rty + ")";
                        op = id == F::Sum ? "FReal.add" : "FReal.mul";
                    } else if (ASR::is_a<ASR::Complex_t>(*et)) {
                        init = "(" + std::string(id == F::Sum ? "FCplx.zero" : "FCplx.ofReal (FReal.ofInt 1)") + " : " + rty + ")";
                        op = id == F::Sum ? "FCplx.add" : "FCplx.mul";
                    }
                } else if ((id == F::Iany || id == F::Iall || id == F::Iparity)
                        && ASR::is_a<ASR::Integer_t>(*et)) {
                    std::string w = std::to_string(8 * ASR::down_cast<ASR::Integer_t>(et)->m_kind);
                    init = "(" + std::string(id == F::Iall ? "FInt.ofInt (-1)" : "FInt.ofInt 0") + " : " + rty + ")";
                    op = "(" + std::string(id == F::Iany ? "FInt.ior "
                        : id == F::Iall ? "FInt.iand " : "FInt.ieor ") + w + ")";
                } else if (id == F::Any && ASR::is_a<ASR::Logical_t>(*et)) {
                    init = "false";
                    op = "(fun acc' x' => acc' || x')";
                } else if (id == F::All && ASR::is_a<ASR::Logical_t>(*et)) {
                    init = "true";
                    op = "(fun acc' x' => acc' && x')";
                } else if (id == F::Count && ASR::is_a<ASR::Logical_t>(*et)) {
                    init = "(FInt.ofInt 0 : " + rty + ")";
                    op = "(fun acc' b' => if b' then FInt.add acc' (FInt.ofInt 1) else acc')";
                } else if (id == F::MaxVal || id == F::MinVal) {
                    if (ASR::is_a<ASR::Integer_t>(*et)) {
                        int k = ASR::down_cast<ASR::Integer_t>(et)->m_kind;
                        int64_t hi = k == 1 ? INT8_MAX : k == 2 ? INT16_MAX : k == 4 ? INT32_MAX : INT64_MAX;
                        int64_t lo = k == 1 ? INT8_MIN : k == 2 ? INT16_MIN : k == 4 ? INT32_MIN : INT64_MIN;
                        init = "(FInt.ofInt " + int_lit(id == F::MaxVal ? lo : hi) + " : " + rty + ")";
                        op = id == F::MaxVal ? "FInt.fmax" : "FInt.fmin";
                    } else if (ASR::is_a<ASR::Real_t>(*et)) {
                        int k = ASR::down_cast<ASR::Real_t>(et)->m_kind;
                        double h = k == 4 ? static_cast<double>(std::numeric_limits<float>::max())
                            : std::numeric_limits<double>::max();
                        std::string hv = real_lit(h, k, loc);
                        init = "(" + (id == F::MaxVal ? "FReal.neg (" + hv + ")" : hv) + " : " + rty + ")";
                        op = id == F::MaxVal ? "FReal.fmax" : "FReal.fmin";
                    }
                }
                if (!op.empty()) {
                    std::string fn_name = (id == F::MaxVal || id == F::MinVal)
                        ? "Arr.reduceDim1" : "Arr.reduceDim";
                    if (mask) {
                        return app(fn_name, {{init, pAtom}, {op, pAtom},
                            lower(x.m_args[0]), int_expr(x.m_args[1]),
                            {"(some " + atom(lower(mask)) + ")", pAtom}});
                    }
                    return app(fn_name, {{init, pAtom}, {op, pAtom},
                        lower(x.m_args[0]), int_expr(x.m_args[1])});
                }
            }
        }
        if (id == F::MatMul && x.n_args == 2) {
            ASR::ttype_t *et = scalar(elem_type(x.m_type), loc);
            ASR::ttype_t *e0 = elem_type(ASRUtils::expr_type(x.m_args[0]));
            ASR::ttype_t *e1 = elem_type(ASRUtils::expr_type(x.m_args[1]));
            bool same = e0 && e1 && e0->type == et->type && e1->type == et->type
                && ASRUtils::extract_kind_from_ttype_t(e0) == ASRUtils::extract_kind_from_ttype_t(et)
                && ASRUtils::extract_kind_from_ttype_t(e1) == ASRUtils::extract_kind_from_ttype_t(et);
            if (!same) unsupported("mixed-type matmul is", loc);
            std::string ty = type(et, loc);
            std::string z, add, mul;
            if (ASR::is_a<ASR::Integer_t>(*et)) {
                z = "(FInt.ofInt 0 : " + ty + ")";
                add = "FInt.add";
                mul = "FInt.mul";
            } else if (ASR::is_a<ASR::Real_t>(*et)) {
                z = "(FReal.zero : " + ty + ")";
                add = "FReal.add";
                mul = "FReal.mul";
            } else if (ASR::is_a<ASR::Complex_t>(*et)) {
                z = "(FCplx.zero : " + ty + ")";
                add = "FCplx.add";
                mul = "FCplx.mul";
            } else if (ASR::is_a<ASR::Logical_t>(*et)) {
                z = "false";
                add = "(fun x' y' => x' || y')";
                mul = "(fun x' y' => x' && y')";
            }
            if (!z.empty()) {
                return app("Arr.matmul", {{z, pAtom}, {add, pAtom}, {mul, pAtom},
                    lower(x.m_args[0]), lower(x.m_args[1])});
            }
        }
        if (id == F::Shape && (x.n_args == 1 || x.n_args == 2) && is_array(x.m_type)) {
            std::string rty = type(elem_type(x.m_type), loc);
            if (!is_array(ASRUtils::expr_type(x.m_args[0]))) {
                return {"(Arr.mk #[1] #[0] (#[] : Array " + rty + "))", pAtom};
            }
            return {"((fun (a' : Arr _) => Arr.mk #[1] #[a'.exts.size] "
                "(a'.exts.map (fun e' => (FInt.ofInt (Int.ofNat e') : " + rty + ")))) "
                + atom(lower(x.m_args[0])) + ")", pAtom};
        }
        if (id == F::Pack && (x.n_args == 2 || x.n_args == 3)) {
            return lower_pack(x.m_args[0], x.m_args[1], loc,
                x.n_args == 3 ? x.m_args[2] : nullptr);
        }
        if (id == F::Unpack && x.n_args == 3
                && is_array(ASRUtils::expr_type(x.m_args[0]))
                && is_array(ASRUtils::expr_type(x.m_args[1]))) {
            ASR::expr_t *fld = unwrap_physical_cast(x.m_args[2]);
            bool fld_bcast = ASR::is_a<ASR::ArrayBroadcast_t>(*fld);
            bool fld_scalar = fld_bcast || !is_array(ASRUtils::expr_type(fld));
            std::string fld_src;
            if (fld_scalar) {
                ASR::expr_t *fs = fld_bcast ? ASR::down_cast<ASR::ArrayBroadcast_t>(fld)->m_array : fld;
                if (is_array(ASRUtils::expr_type(fs))) unsupported("this unpack is", loc);
                fld_src = "(Sum.inl " + atom(operand(fs)) + ")";
            } else {
                fld_src = "(Sum.inr " + atom(lower(fld)) + ")";
            }
            return app("Arr.unpack", {lower(x.m_args[0]), lower(x.m_args[1]), {fld_src, pAtom}});
        }
        if ((id == F::Cshift && (x.n_args == 2 || x.n_args == 3))
                || (id == F::Eoshift && (x.n_args == 3 || x.n_args == 4))) {
            if (is_array(ASRUtils::expr_type(x.m_args[0]))) {
                ASR::expr_t *sh = unwrap_physical_cast(x.m_args[1]);
                bool sh_bcast = ASR::is_a<ASR::ArrayBroadcast_t>(*sh);
                bool sh_scalar = sh_bcast || !is_array(ASRUtils::expr_type(sh));
                std::string sh_src;
                if (sh_scalar) {
                    ASR::expr_t *ss = sh_bcast ? ASR::down_cast<ASR::ArrayBroadcast_t>(sh)->m_array : sh;
                    if (is_array(ASRUtils::expr_type(ss))) unsupported("this shift is", loc);
                    sh_src = "(Sum.inl " + atom(int_expr(ss)) + ")";
                } else {
                    sh_src = "(Sum.inr (Arr.map FInt.toInt " + atom(lower(sh)) + "))";
                }
                if (id == F::Cshift) {
                    Val dim_v = (x.n_args == 3 && x.m_args[2]) ? int_expr(x.m_args[2]) : Val{"1", pAtom};
                    return app("Arr.cshift", {lower(x.m_args[0]), {sh_src, pAtom}, dim_v});
                }
                ASR::ttype_t *et = elem_type(ASRUtils::expr_type(x.m_args[0]));
                if (ASR::is_a<ASR::String_t>(*et) && !ASR::down_cast<ASR::String_t>(et)->m_len) {
                    unsupported("this eoshift is", loc);
                }
                std::string bnd_src;
                if (!x.m_args[2]) {
                    bnd_src = "(Sum.inl " + atom({zero(et, loc, expr_type_decl(x.m_args[0])), pApp}) + ")";
                } else {
                    ASR::expr_t *bn = unwrap_physical_cast(x.m_args[2]);
                    bool bn_bcast = ASR::is_a<ASR::ArrayBroadcast_t>(*bn);
                    bool bn_scalar = bn_bcast || !is_array(ASRUtils::expr_type(bn));
                    if (bn_scalar) {
                        ASR::expr_t *bs = bn_bcast ? ASR::down_cast<ASR::ArrayBroadcast_t>(bn)->m_array : bn;
                        if (is_array(ASRUtils::expr_type(bs))) unsupported("this eoshift is", loc);
                        std::string bval = fix_string(et, "", operand(bs), loc);
                        bnd_src = "(Sum.inl " + atom({bval, pApp}) + ")";
                    } else {
                        std::string bval = atom(lower(bn));
                        if (ASR::is_a<ASR::String_t>(*et)) {
                            bval = "(Arr.map (FStr.fix "
                                + extent(ASR::down_cast<ASR::String_t>(et)->m_len)
                                + ") " + bval + ")";
                        }
                        bnd_src = "(Sum.inr " + bval + ")";
                    }
                }
                Val dim_v = (x.n_args == 4 && x.m_args[3]) ? int_expr(x.m_args[3]) : Val{"1", pAtom};
                return app("Arr.eoshift", {lower(x.m_args[0]), {sh_src, pAtom}, {bnd_src, pAtom}, dim_v});
            }
        }
        if (id == F::Spread && x.n_args == 3) {
            if (x.m_overload_id == -1 || !is_array(ASRUtils::expr_type(x.m_args[0]))) {
                std::string nc = extent(x.m_args[2]);
                std::string elem = is_array(ASRUtils::expr_type(x.m_args[0]))
                    ? "(" + atom(lower(x.m_args[0])) + ".data[0]!)"
                    : atom(operand(x.m_args[0]));
                return {"(Arr.make #[1] #[" + nc + "] " + elem + ")", pAtom};
            }
            return app("Arr.spread", {lower(x.m_args[0]),
                int_expr(x.m_args[1]), int_expr(x.m_args[2])});
        }
        if ((id == F::MaxLoc || id == F::MinLoc) && x.n_args == 5
                && x.m_overload_id >= 0 && x.m_overload_id <= 3
                && is_array(ASRUtils::expr_type(x.m_args[0]))) {
            ASR::ttype_t *et = elem_type(ASRUtils::expr_type(x.m_args[0]));
            type(et, loc);
            std::string lt_cmp, le_cmp;
            if (ASR::is_a<ASR::Integer_t>(*et)) {
                lt_cmp = id == F::MaxLoc
                    ? "(fun cur' cand' => FInt.lt cur' cand')"
                    : "(fun cur' cand' => FInt.lt cand' cur')";
                le_cmp = id == F::MaxLoc
                    ? "(fun cur' cand' => FInt.le cur' cand')"
                    : "(fun cur' cand' => FInt.le cand' cur')";
            } else if (ASR::is_a<ASR::Real_t>(*et)) {
                lt_cmp = id == F::MaxLoc
                    ? "(fun cur' cand' => FReal.lt cur' cand')"
                    : "(fun cur' cand' => FReal.lt cand' cur')";
                le_cmp = id == F::MaxLoc
                    ? "(fun cur' cand' => FReal.le cur' cand')"
                    : "(fun cur' cand' => FReal.le cand' cur')";
            }
            if (!lt_cmp.empty()) {
                std::string better = lt_cmp;
                if (x.m_args[4]) {
                    ASR::expr_t *bv = ASRUtils::expr_value(x.m_args[4]);
                    if (bv && ASR::is_a<ASR::LogicalConstant_t>(*bv)) {
                        if (ASR::down_cast<ASR::LogicalConstant_t>(bv)->m_value) better = le_cmp;
                    } else {
                        better = "(if " + lower(x.m_args[4]).src + " then " + le_cmp
                            + " else " + lt_cmp + ")";
                    }
                }
                std::string rty = type(elem_type(x.m_type), loc);
                std::string arr_src = atom(lower(x.m_args[0]));
                bool has_dim = (x.m_overload_id == 2 || x.m_overload_id == 3);
                bool has_mask = (x.m_overload_id == 1 || x.m_overload_id == 3) && x.m_args[2];
                if (!has_dim || !is_array(x.m_type)) {
                    // Whole-array maxloc/minloc or rank-1 maxloc/minloc with dim=1.
                    std::string loc_expr;
                    if (has_mask) {
                        if (is_array(ASRUtils::expr_type(x.m_args[2]))) {
                            loc_expr = "(Arr.extremaLoc " + better + " " + arr_src
                                + " (some " + atom(lower(x.m_args[2])) + "))";
                        } else {
                            loc_expr = "((fun (a' : Arr _) (m' : Bool) (cmp' : _ → _ → Bool) => "
                                "if m' then Arr.extremaLoc cmp' a' none "
                                "else Array.replicate a'.exts.size (0 : Int)) "
                                + arr_src + " " + atom(lower(x.m_args[2])) + " " + better + ")";
                        }
                    } else {
                        loc_expr = "(Arr.extremaLoc " + better + " " + arr_src + " none)";
                    }
                    if (!is_array(x.m_type)) {
                        return {"(FInt.ofInt (" + loc_expr + ".getD 0 0) : " + rty + ")", pAtom};
                    }
                    return {"((fun c' => Arr.mk #[1] #[c'.size] "
                        "(c'.map (fun x' => (FInt.ofInt x' : " + rty + ")))) "
                        + loc_expr + ")", pAtom};
                }
                // Rank >= 2 array maxloc/minloc with `dim=` returning rank - 1 array.
                std::string dim_src = atom(int_expr(x.m_args[1]));
                std::string res_expr;
                if (has_mask) {
                    if (is_array(ASRUtils::expr_type(x.m_args[2]))) {
                        res_expr = "(Arr.extremaLocDim " + better + " " + arr_src + " "
                            + dim_src + " (some " + atom(lower(x.m_args[2])) + "))";
                    } else {
                        res_expr = "((fun (a' : Arr _) (d' : Int) (m' : Bool) (cmp' : _ → _ → Bool) => "
                            "if m' then Arr.extremaLocDim cmp' a' d' none "
                            "else Arr.extremaLocDim cmp' a' d' (some { a' with data := Array.replicate a'.data.size false })) "
                            + arr_src + " " + dim_src + " " + atom(lower(x.m_args[2])) + " " + better + ")";
                    }
                } else {
                    res_expr = "(Arr.extremaLocDim " + better + " " + arr_src + " "
                        + dim_src + " none)";
                }
                return {"(Arr.map (fun x' => (FInt.ofInt x' : " + rty + ")) "
                    + res_expr + ")", pAtom};
            }
        }
        unsupported("this array intrinsic is", loc);
    }

    Val lower_pack(ASR::expr_t *arr, ASR::expr_t *mask, const Location &loc,
            ASR::expr_t *vec = nullptr) {
        if (!is_array(ASRUtils::expr_type(arr))) unsupported("this pack is", loc);
        if (vec && !is_array(ASRUtils::expr_type(vec))) unsupported("this pack is", loc);
        if (is_array(ASRUtils::expr_type(mask))) {
            if (vec) {
                return {"((fun (a' : Arr _) (m' : Arr Bool) (v' : Arr _) => "
                    "let p' := (Array.range a'.data.size).foldl "
                    "(fun acc' i' => if m'.data.getD i' false then acc'.push a'.data[i']! else acc') #[]; "
                    "let out' := if p'.size < v'.data.size then p' ++ v'.data.extract p'.size v'.data.size else p'; "
                    "Arr.mk #[1] #[out'.size] out') "
                    + atom(lower(arr)) + " " + atom(lower(mask)) + " " + atom(lower(vec)) + ")", pAtom};
            }
            return {"((fun (a' : Arr _) (m' : Arr Bool) => "
                "let out' := (Array.range a'.data.size).foldl "
                "(fun acc' i' => if m'.data.getD i' false then acc'.push a'.data[i']! else acc') #[]; "
                "Arr.mk #[1] #[out'.size] out') "
                + atom(lower(arr)) + " " + atom(lower(mask)) + ")", pAtom};
        }
        if (vec) {
            return {"((fun (a' : Arr _) (m' : Bool) (v' : Arr _) => "
                "let p' := if m' then a'.data else #[]; "
                "let out' := if p'.size < v'.data.size then p' ++ v'.data.extract p'.size v'.data.size else p'; "
                "Arr.mk #[1] #[out'.size] out') "
                + atom(lower(arr)) + " " + atom(lower(mask)) + " " + atom(lower(vec)) + ")", pAtom};
        }
        return {"((fun (a' : Arr _) (m' : Bool) => "
            "if m' then Arr.mk #[1] #[a'.data.size] a'.data else Arr.mk #[1] #[0] #[]) "
            + atom(lower(arr)) + " " + atom(lower(mask)) + ")", pAtom};
    }

    Val array_section_args(ASR::expr_t *v, ASR::array_index_t *args, size_t n_args) {
        std::vector<std::string> specs;
        for (size_t i = 0; i < n_args; i++) {
            const ASR::array_index_t &idx = args[i];
            if (!idx.m_left && !idx.m_step && idx.m_right) {
                if (is_array(ASRUtils::expr_type(idx.m_right))) {
                    specs.push_back("Arr.DimSpec.vec (" + atom(lower(idx.m_right))
                        + ".data.map FInt.toInt)");
                } else {
                    specs.push_back("Arr.DimSpec.idx " + atom(int_expr(idx.m_right)));
                }
            } else {
                std::string lo = idx.m_left ? "(some " + atom(int_expr(idx.m_left)) + ")" : "none";
                std::string hi = idx.m_right ? "(some " + atom(int_expr(idx.m_right)) + ")" : "none";
                std::string st = idx.m_step ? atom(int_expr(idx.m_step)) : "1";
                specs.push_back("Arr.DimSpec.slice " + lo + " " + hi + " " + st);
            }
        }
        return app("Arr.slice", {lower(v), {int_array(specs), pAtom}});
    }

    Val array_section(const ASR::ArraySection_t &x) {
        return array_section_args(x.m_v, x.m_args, x.n_args);
    }

    Val lower(ASR::expr_t *e) {
        Val v = lower_raw(e);
        if (is_option_expr(e)) return {atom(v) + ".get!", pAtom};
        return v;
    }

    Val lower_raw(ASR::expr_t *e) {
        const Location &loc = e->base.loc;
        switch (e->type) {
            case ASR::exprType::ArrayPhysicalCast:
                return lower_raw(ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg);
            case ASR::exprType::ArrayConstant:
                return array_constant(*ASR::down_cast<ASR::ArrayConstant_t>(e), loc);
            case ASR::exprType::ArrayConstructor:
                return array_constructor(*ASR::down_cast<ASR::ArrayConstructor_t>(e), loc);
            case ASR::exprType::ArraySection:
                return array_section(*ASR::down_cast<ASR::ArraySection_t>(e));
            case ASR::exprType::ArrayRank: {
                auto &x = *ASR::down_cast<ASR::ArrayRank_t>(e);
                if (x.m_value && x.m_value != e) return lower_raw(x.m_value);
                std::string r = is_array(ASRUtils::expr_type(x.m_v))
                    ? "(Int.ofNat (" + atom(lower(x.m_v)) + ".exts.size))" : "0";
                return {"(FInt.ofInt " + r + " : " + type(x.m_type, loc) + ")", pAtom};
            }
            case ASR::exprType::ArrayReshape: {
                auto &x = *ASR::down_cast<ASR::ArrayReshape_t>(e);
                if (x.m_value && x.m_value != e) return lower_raw(x.m_value);
                if (!x.m_pad && !x.m_order) {
                    return {"((fun (a' : Arr _) (sh' : Arr _) => "
                        "let exts' := sh'.data.map (fun x' => (FInt.toInt x').toNat); "
                        "let n' := exts'.foldl (fun p' e' => p' * e') 1; "
                        "Arr.mk (Array.replicate exts'.size (1 : Int)) exts' (a'.data.extract 0 n')) "
                        + atom(lower(x.m_array)) + " " + atom(lower(x.m_shape)) + ")", pAtom};
                }
                std::string pad_src = x.m_pad ? "(some " + atom(lower(x.m_pad)) + ")" : "none";
                std::string ord_src = x.m_order
                    ? "(some (" + atom(lower(x.m_order)) + ".data.map (fun x' => (FInt.toInt x').toNat)))"
                    : "none";
                return {"(Arr.reshapeFull " + atom(lower(x.m_array))
                    + " (" + atom(lower(x.m_shape)) + ".data.map (fun x' => (FInt.toInt x').toNat)) "
                    + pad_src + " " + ord_src + ")", pAtom};
            }
            case ASR::exprType::ArrayTranspose: {
                auto &x = *ASR::down_cast<ASR::ArrayTranspose_t>(e);
                if (x.m_value && x.m_value != e) return lower_raw(x.m_value);
                return {"((fun (a' : Arr _) => "
                    "let m' := a'.exts.getD 0 0; "
                    "let n' := a'.exts.getD 1 0; "
                    "Arr.mk #[1, 1] #[n', m'] (Array.ofFn (n := m' * n') (fun k' => "
                    "a'.data[k'.val / n' + m' * (k'.val % n')]!))) "
                    + atom(lower(x.m_matrix)) + ")", pAtom};
            }
            case ASR::exprType::ArrayPack: {
                auto &x = *ASR::down_cast<ASR::ArrayPack_t>(e);
                if (x.m_value && x.m_value != e) return lower_raw(x.m_value);
                if (x.m_vector) unsupported("pack with vector is", loc);
                return lower_pack(x.m_array, x.m_mask, loc);
            }
            case ASR::exprType::IntrinsicArrayFunction:
                return intrinsic_array(*ASR::down_cast<ASR::IntrinsicArrayFunction_t>(e), loc);
            case ASR::exprType::ArrayItem: {
                auto &x = *ASR::down_cast<ASR::ArrayItem_t>(e);
                if (is_array(x.m_type)) {
                    return array_section_args(x.m_v, x.m_args, x.n_args);
                }
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
                std::string f = x.m_bound == ASR::arrayboundType::LBound
                    ? "Arr.lbound " : "Arr.ubound ";
                if (!x.m_dim) {
                    if (!is_array(x.m_type)) unsupported("this array bound is", loc);
                    std::string rty = type(elem_type(x.m_type), loc);
                    return {"((fun (a' : Arr _) => Arr.mk #[1] #[a'.exts.size] "
                        "((Array.range a'.exts.size).map (fun k' => "
                        "(FInt.ofInt (" + f + "a' (Int.ofNat (k' + 1))) : " + rty + ")))) "
                        + atom(lower(x.m_v)) + ")", pAtom};
                }
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
                auto iit = implied_vars.find(v);
                if (iit != implied_vars.end()) {
                    return {iit->second, pAtom};
                }
                auto vit = var_alias.find(v);
                if (vit != var_alias.end()) {
                    return {vit->second, pAtom};
                }
                if (global_fields.count(v)) {
                    if (!cur) unsupported("a non-constant global initializer is", loc);
                    return {"(← get)." + global_fields[v], pAtom};
                }
                if (!cur) unsupported("a non-constant static initializer is", loc);
                if (!in_scope_stack(v->m_parent_symtab)) {
                    unsupported("a variable from another scope is", loc);
                }
                return {var_name(v), pAtom};
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
                        int pk = ASR::down_cast<ASR::Real_t>(unalloc_type(p))->m_kind;
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
                return {string_lit(string_const_str(x)), pAtom};
            }
            case ASR::exprType::StringPhysicalCast:
                return lower_raw(ASR::down_cast<ASR::StringPhysicalCast_t>(e)->m_arg);
            case ASR::exprType::StringFormat:
                return {lower_string_format(*ASR::down_cast<ASR::StringFormat_t>(e), "\" \"", loc), pAtom};
            case ASR::exprType::StringConcat: {
                auto &x = *ASR::down_cast<ASR::StringConcat_t>(e);
                if (x.m_value && x.m_value != e) return lower(x.m_value);
                if (is_array(x.m_type)) {
                    return lift_binop(x.m_left, x.m_right,
                        [](const std::string &a, const std::string &b) {
                            return "(" + a + " ++ " + b + ")";
                        });
                }
                return {"(" + atom(lower(x.m_left)) + " ++ " + atom(lower(x.m_right)) + ")", pAtom};
            }
            case ASR::exprType::StringRepeat: {
                auto &x = *ASR::down_cast<ASR::StringRepeat_t>(e);
                if (x.m_value && x.m_value != e) return lower(x.m_value);
                return app("FStr.rep", {lower(x.m_left), int_expr(x.m_right)});
            }
            case ASR::exprType::StringLen: {
                auto &x = *ASR::down_cast<ASR::StringLen_t>(e);
                if (x.m_value && x.m_value != e) return lower(x.m_value);
                ASR::ttype_t *at = unalloc_type(x.m_arg);
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
                if (ASR::is_a<ASR::String_t>(*at)) {
                    ASR::String_t *st = ASR::down_cast<ASR::String_t>(at);
                    ASR::expr_t *au = unwrap_physical_cast(x.m_arg);
                    if (st->m_len && (is_option_expr(x.m_arg)
                            || (au && (ASR::is_a<ASR::Var_t>(*au)
                                || ASR::is_a<ASR::StructInstanceMember_t>(*au))))) {
                        std::string len_nat = (au && ASR::is_a<ASR::Var_t>(*au)
                                && alloc_str_len.count(ASRUtils::EXPR2VAR(au)))
                            ? alloc_str_len[ASRUtils::EXPR2VAR(au)] : extent(st->m_len);
                        return {"(FInt.ofInt (Int.ofNat " + len_nat + ") : "
                            + type(x.m_type, loc) + ")", pAtom};
                    }
                }
                return {"(FInt.ofInt " + atom(app("FStr.len", {lower(x.m_arg)}))
                    + " : " + type(x.m_type, loc) + ")", pAtom};
            }
            case ASR::exprType::StringItem: {
                auto &x = *ASR::down_cast<ASR::StringItem_t>(e);
                if (x.m_value && x.m_value != e) return lower(x.m_value);
                return app("FStr.item", {lower(x.m_arg), int_expr(x.m_idx)});
            }
            case ASR::exprType::StringSection: {
                auto &x = *ASR::down_cast<ASR::StringSection_t>(e);
                if (x.m_value && x.m_value != e) return lower(x.m_value);
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
                if (x.m_value && x.m_value != e) return lower(x.m_value);
                if (is_array(x.m_type)) {
                    return lift_binop(x.m_left, x.m_right,
                        [&](const std::string &a, const std::string &b) -> std::string {
                            switch (x.m_op) {
                                case ASR::cmpopType::Eq: return "FStr.beq " + a + " " + b;
                                case ASR::cmpopType::NotEq: return "!(FStr.beq " + a + " " + b + ")";
                                case ASR::cmpopType::Lt: return "FStr.lt " + a + " " + b;
                                case ASR::cmpopType::LtE: return "FStr.le " + a + " " + b;
                                case ASR::cmpopType::Gt: return "FStr.lt " + b + " " + a;
                                case ASR::cmpopType::GtE: return "FStr.le " + b + " " + a;
                            }
                            return "FStr.beq " + a + " " + b;
                        });
                }
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
            case ASR::exprType::IfExp: {
                auto &x = *ASR::down_cast<ASR::IfExp_t>(e);
                if (is_array(ASRUtils::expr_type(x.m_test))) {
                    unsupported("this expression is", loc);
                }
                ASR::symbol_t *td = expr_type_decl(x.m_body);
                if (!td) td = expr_type_decl(x.m_orelse);
                Val cond = lower(x.m_test);
                Val body = lower(x.m_body);
                Val orelse = lower(x.m_orelse);
                std::string ty = vtype(unalloc(x.m_type), loc, td);
                if (body.src.find("←") != std::string::npos
                        || orelse.src.find("←") != std::string::npos) {
                    return {"(← (do if " + cond.src + " then return (" + body.src + " : " + ty
                        + ") else return (" + orelse.src + " : " + ty + ")))", pAtom};
                }
                return {"((if " + cond.src + " then " + body.src
                    + " else " + orelse.src + ") : " + ty + ")", pAtom};
            }
            case ASR::exprType::StringOrd: {
                auto &x = *ASR::down_cast<ASR::StringOrd_t>(e);
                if (x.m_value && x.m_value != e) return lower(x.m_value);
                return {"(FInt.ofInt " + atom(app("FStr.ord", {lower(x.m_arg)}))
                    + " : " + type(x.m_type, loc) + ")", pAtom};
            }
            case ASR::exprType::StringChr: {
                auto &x = *ASR::down_cast<ASR::StringChr_t>(e);
                if (x.m_value && x.m_value != e) return lower(x.m_value);
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
                            ASR::expr_t *ru = unwrap_physical_cast(x.m_right);
                            ASR::expr_t *rb = ASR::is_a<ASR::ArrayBroadcast_t>(*ru)
                                ? ASR::down_cast<ASR::ArrayBroadcast_t>(ru)->m_array : ru;
                            if (ASR::is_a<ASR::Integer_t>(*elem_type(ASRUtils::expr_type(rb)))) {
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
                auto [is_m, expr] = monadic_call(fn, c);
                if (is_m) return {"(← " + expr + ")", pAtom};
                return c;
            }
            case ASR::exprType::IntrinsicElementalFunction:
                return intrinsic(*ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e),
                    loc);
            case ASR::exprType::IntrinsicImpureFunction: {
                auto &x = *ASR::down_cast<ASR::IntrinsicImpureFunction_t>(e);
                using F = ASRUtils::IntrinsicImpureFunctions;
                F id = static_cast<F>(x.m_impure_intrinsic_id);
                if (id == F::Allocated && x.n_args == 1 && is_option_expr(x.m_args[0])) {
                    return {atom(lower_raw(x.m_args[0])) + ".isSome", pAtom};
                }
                unsupported("this impure intrinsic is", loc);
            }
            case ASR::exprType::StructConstructor: {
                auto &x = *ASR::down_cast<ASR::StructConstructor_t>(e);
                return struct_ctor(x.m_dt_sym, x.m_args, x.n_args, loc);
            }
            case ASR::exprType::StructConstant: {
                auto &x = *ASR::down_cast<ASR::StructConstant_t>(e);
                return struct_ctor(x.m_dt_sym, x.m_args, x.n_args, loc);
            }
            case ASR::exprType::StructInstanceMember: {
                auto &x = *ASR::down_cast<ASR::StructInstanceMember_t>(e);
                if (x.m_value && x.m_value != e) return lower(x.m_value);
                ASR::symbol_t *ms = ASRUtils::symbol_get_past_external(x.m_m);
                std::string fn;
                bool member_is_arr = false, member_is_alloc = false;
                if (ASR::is_a<ASR::Variable_t>(*ms)) {
                    ASR::Variable_t *mv = ASR::down_cast<ASR::Variable_t>(ms);
                    fn = name(mv->m_name, loc);
                    member_is_arr = is_array(mv->m_type);
                    member_is_alloc = is_allocatable(mv->m_type);
                } else if (ASR::is_a<ASR::Struct_t>(*ms)) {
                    fn = struct_parent_proj(ASR::down_cast<ASR::Struct_t>(ms), loc);
                } else {
                    unsupported("this derived type member is", loc);
                }
                if (is_array(ASRUtils::expr_type(x.m_v))) {
                    if (member_is_arr || member_is_alloc) {
                        unsupported("this array of derived types member is", loc);
                    }
                    return app("Arr.map", {{"(fun s' => s'." + fn + ")", pAtom}, lower(x.m_v)});
                }
                return {atom(lower(x.m_v)) + "." + fn, pAtom};
            }
            default:
                if (ASR::expr_t *v = ASRUtils::expr_value(e); v && v != e) {
                    return lower(v);
                }
                unsupported("this expression is", loc);
        }
    }

    std::string struct_member_init(ASR::Variable_t *mv, ASR::expr_t *val, const Location &loc) {
        if (is_allocatable(mv->m_type)) {
            if (!val) return "none";
            ASR::ttype_t *it = unalloc(mv->m_type);
            if (is_option_expr(val)) {
                std::string raw = atom(lower_raw(val));
                if (is_array(it)) {
                    ASR::ttype_t *et = elem_type(it);
                    if (ASR::is_a<ASR::String_t>(*et) && ASR::down_cast<ASR::String_t>(et)->m_len) {
                        std::string ln = extent(ASR::down_cast<ASR::String_t>(et)->m_len);
                        return "(" + raw + ").map (fun a' => { a' with data := a'.data.map (FStr.fix "
                            + ln + ") })";
                    }
                } else if (ASR::is_a<ASR::String_t>(*it) && ASR::down_cast<ASR::String_t>(it)->m_len) {
                    return "(" + raw + ").map (FStr.fix "
                        + extent(ASR::down_cast<ASR::String_t>(it)->m_len) + ")";
                }
                return raw;
            }
            if (is_array(it)) {
                std::string src = atom(lower(val));
                ASR::ttype_t *et = elem_type(it);
                if (ASR::is_a<ASR::String_t>(*et) && ASR::down_cast<ASR::String_t>(et)->m_len) {
                    src = "(Arr.map (FStr.fix "
                        + extent(ASR::down_cast<ASR::String_t>(et)->m_len) + ") " + src + ")";
                }
                return "some " + src;
            }
            return "some " + atom({fix_string(it, "", lower(val), loc), pApp});
        }
        if (val) {
            if (is_array(mv->m_type)) {
                std::vector<std::string> lbs, exts;
                bool assumed;
                dims(mv->m_type, lbs, exts, assumed, loc);
                if (assumed) unsupported("this derived type array member is", loc);
                ASR::ttype_t *et = elem_type(mv->m_type);
                ASR::expr_t *v = val;
                if (ASR::is_a<ASR::ArrayBroadcast_t>(*v)) {
                    v = ASR::down_cast<ASR::ArrayBroadcast_t>(v)->m_array;
                }
                if (is_array(ASRUtils::expr_type(v))) {
                    std::string src = atom(lower(v));
                    if (ASR::is_a<ASR::String_t>(*et)) {
                        auto &st = *ASR::down_cast<ASR::String_t>(et);
                        if (!st.m_len) unsupported("this derived type array member is", loc);
                        src = "(Arr.map (FStr.fix " + extent(st.m_len) + ") " + src + ")";
                    }
                    return "Arr.reshape " + src + " " + int_array(lbs) + " " + int_array(exts);
                }
                std::string rhs = fix_string(et, "", lower(v), loc);
                return "Arr.make " + int_array(lbs) + " " + int_array(exts) + " " + atom({rhs, pApp});
            }
            return fix_string(mv->m_type, "", lower(val), loc);
        }
        if (is_array(mv->m_type)) {
            std::vector<std::string> lbs, exts;
            bool assumed;
            dims(mv->m_type, lbs, exts, assumed, loc);
            if (assumed) unsupported("this derived type array member is", loc);
            return "Arr.make " + int_array(lbs) + " " + int_array(exts) + " "
                + atom({zero(ASR::down_cast<ASR::Array_t>(mv->m_type)->m_type, loc,
                    mv->m_type_declaration), pApp});
        }
        return zero(mv->m_type, loc, mv->m_type_declaration);
    }

    std::string struct_member_default(ASR::Variable_t *mv) {
        const Location &loc = mv->base.base.loc;
        ASR::expr_t *init = mv->m_value ? mv->m_value : mv->m_symbolic_value;
        return struct_member_init(mv, init, loc);
    }

    Val struct_ctor(ASR::symbol_t *dt_sym, ASR::call_arg_t *args, size_t n_args,
            const Location &loc) {
        ASR::Struct_t *st = resolve_struct(dt_sym, loc);
        // Handle `child(parent_val, own_fields...)` when first arg has parent StructType.
        if (st->m_parent && n_args == 1 + st->n_members && args[0].m_value
                && ASR::is_a<ASR::StructType_t>(*unalloc_type(args[0].m_value))) {
            ASR::Struct_t *pst = resolve_struct(st->m_parent, loc);
            ASR::symbol_t *arg0_td = expr_type_decl(args[0].m_value);
            ASR::symbol_t *arg0_sym = arg0_td ? ASRUtils::symbol_get_past_external(arg0_td) : nullptr;
            if (arg0_sym && ASR::is_a<ASR::Struct_t>(*arg0_sym)
                    && ASR::down_cast<ASR::Struct_t>(arg0_sym) == pst) {
                std::string fields = struct_parent_proj(pst, loc) + " := " + lower(args[0].m_value).src;
                for (size_t i = 0; i < st->n_members; i++) {
                    ASR::Variable_t *mv = struct_member(st, i, loc);
                    ASR::expr_t *arg = args[i + 1].m_value;
                    std::string val = arg ? struct_member_init(mv, arg, loc) : struct_member_default(mv);
                    fields += ", " + name(mv->m_name, mv->base.base.loc) + " := " + val;
                }
                return {"({ " + fields + " } : " + struct_qname(st, loc) + " K)", pAtom};
            }
        }
        std::vector<ASR::Variable_t*> ms = all_struct_members(st, loc);
        if (n_args > ms.size()) unsupported("this derived type constructor is", loc);
        std::string fields;
        for (size_t i = 0; i < ms.size(); i++) {
            ASR::Variable_t *mv = ms[i];
            if (i) fields += ", ";
            ASR::expr_t *arg = (i < n_args) ? args[i].m_value : nullptr;
            std::string val = arg ? struct_member_init(mv, arg, loc) : struct_member_default(mv);
            fields += name(mv->m_name, mv->base.base.loc) + " := " + val;
        }
        std::string body = fields.empty() ? "{}" : "{ " + fields + " }";
        return {"(" + body + " : " + struct_qname(st, loc) + " K)", pAtom};
    }

    // Statements -----------------------------------------------------------

    SymbolTable *scope = nullptr;
    std::vector<SymbolTable*> scope_stack;
    SymbolTable *cur_scope() const { return scope; }
    bool in_scope_stack(SymbolTable *st) const {
        if (st == scope) return true;
        return std::find(scope_stack.begin(), scope_stack.end(), st) != scope_stack.end();
    }
    bool name_in_scope(const std::string &n) const {
        if (scope && scope->get_symbol(n)) return true;
        for (SymbolTable *st : scope_stack) {
            if (st && st->get_symbol(n)) return true;
        }
        return false;
    }

    std::string shown_val(const std::string &val_src, ASR::ttype_t *t,
            ASR::symbol_t *type_decl, const Location &loc, int depth = 0) {
        if (is_allocatable(t)) {
            return shown_val("(" + val_src + ").get!", unalloc(t), type_decl, loc, depth);
        }
        if (is_array(t)) {
            ASR::ttype_t *et = scalar(elem_type(t), loc);
            std::string xv = "x'" + std::to_string(depth);
            std::string fmt = shown_val(xv, et, type_decl, loc, depth + 1);
            return "(String.intercalate \" \" (" + val_src + ".data.toList.map (fun "
                + xv + " => " + fmt + ")))";
        }
        t = scalar(t, loc);
        if (ASR::is_a<ASR::Integer_t>(*t)) {
            return "toString (FInt.toInt (" + val_src + " : " + type(t, loc) + "))";
        }
        if (ASR::is_a<ASR::Real_t>(*t)) {
            level = std::max(level, 2);
            int k = ASR::down_cast<ASR::Real_t>(t)->m_kind;
            return "FKindsIO.fmtR" + std::to_string(k) + " (" + val_src + " : " + type(t, loc) + ")";
        }
        if (ASR::is_a<ASR::Complex_t>(*t)) {
            level = std::max(level, 2);
            int k = ASR::down_cast<ASR::Complex_t>(t)->m_kind;
            std::string fn = "FKindsIO.fmtR" + std::to_string(k);
            std::string zv = "z'" + std::to_string(depth);
            return "((fun " + zv + " => \"(\" ++ " + fn + " " + zv + ".re ++ \",\" ++ " + fn
                + " " + zv + ".im ++ \")\") (" + val_src + " : " + type(t, loc) + "))";
        }
        if (ASR::is_a<ASR::Logical_t>(*t)) {
            return "(if " + val_src + " then \"T\" else \"F\")";
        }
        if (ASR::is_a<ASR::String_t>(*t)) {
            return val_src;
        }
        if (ASR::is_a<ASR::StructType_t>(*t)) {
            ASR::Struct_t *st = resolve_struct(type_decl, loc);
            std::vector<ASR::Variable_t*> ms = all_struct_members(st, loc);
            if (ms.empty()) return "((fun _ => \"\") " + atom({val_src, pApp}) + ")";
            std::string sv = "s'" + std::to_string(depth);
            std::string parts;
            for (size_t i = 0; i < ms.size(); i++) {
                ASR::Variable_t *mv = ms[i];
                if (i) parts += ", ";
                parts += shown_val(sv + "." + name(mv->m_name, mv->base.base.loc),
                    mv->m_type, mv->m_type_declaration, loc, depth + 1);
            }
            return "((fun " + sv + " => String.intercalate \" \" [" + parts + "]) "
                + atom({val_src, pApp}) + ")";
        }
        unsupported("printing this value is", loc);
    }

    std::string shown(ASR::expr_t *e) {
        const Location &loc = e->base.loc;
        if (ASR::is_a<ASR::StringConstant_t>(*e)) {
            return string_lit(string_const_str(*ASR::down_cast<ASR::StringConstant_t>(e)));
        }
        if (ASR::is_a<ASR::Var_t>(*e)) {
            ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(e)->m_v);
            if (ASR::is_a<ASR::Variable_t>(*s)) {
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
                if (v->m_storage == ASR::storage_typeType::Parameter && v->m_value
                        && ASR::is_a<ASR::StringConstant_t>(*v->m_value)) {
                    return string_lit(string_const_str(
                        *ASR::down_cast<ASR::StringConstant_t>(v->m_value)));
                }
            }
        }
        if (ASR::is_a<ASR::IntegerConstant_t>(*e)) {
            return string_lit(std::to_string(
                ASR::down_cast<ASR::IntegerConstant_t>(e)->m_n));
        }
        if (is_array(ASRUtils::expr_type(e))) {
            return shown_val(atom(lower(e)), unalloc(ASRUtils::expr_type(e)), expr_type_decl(e), loc);
        }
        if (is_string(e)) {
            return lower(e).src;
        }
        ASR::ttype_t *t = scalar(ASRUtils::expr_type(e), loc);
        if (ASR::is_a<ASR::StructType_t>(*t)) {
            return shown_val(atom(lower(e)), t, expr_type_decl(e), loc);
        }
        // The kind is stated: a value such as `FInt.ofInt 10` does not fix it.
        std::string v = "(" + lower(e).src + " : " + type(t, loc) + ")";
        if (ASR::is_a<ASR::Integer_t>(*t)) {
            return "toString (FInt.toInt " + v + ")";
        }
        if (ASR::is_a<ASR::Real_t>(*t)) {
            level = std::max(level, 2);
            int k = ASR::down_cast<ASR::Real_t>(t)->m_kind;
            return "FKindsIO.fmtR" + std::to_string(k) + " " + v;
        }
        if (ASR::is_a<ASR::Complex_t>(*t)) {
            level = std::max(level, 2);
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

    std::string fval_val(const std::string &val_src, ASR::ttype_t *t,
            ASR::symbol_t *type_decl, const Location &loc, int depth = 0) {
        if (is_allocatable(t)) {
            return fval_val("(" + val_src + ").get!", unalloc(t), type_decl, loc, depth);
        }
        if (is_array(t)) {
            ASR::ttype_t *et = scalar(elem_type(t), loc);
            std::string xv = "fx'" + std::to_string(depth);
            std::string av = "fa'" + std::to_string(depth);
            std::string elem = fval_val(xv, et, type_decl, loc, depth + 1);
            return "(" + val_src + ".data.foldl (fun " + av + " " + xv + " => "
                + av + " ++ " + elem + ") #[])";
        }
        t = scalar(t, loc);
        if (ASR::is_a<ASR::Integer_t>(*t)) {
            int k = ASR::down_cast<ASR::Integer_t>(t)->m_kind;
            return "#[FFmtVal.int (FInt.toInt (" + val_src + " : " + type(t, loc) + ")) "
                + std::to_string(8 * k) + "]";
        }
        if (ASR::is_a<ASR::Real_t>(*t)) {
            level = std::max(level, 2);
            int k = ASR::down_cast<ASR::Real_t>(t)->m_kind;
            return "#[FFmtVal.real (FKindsIO.toFloatR" + std::to_string(k)
                + " (" + val_src + " : " + type(t, loc) + "))]";
        }
        if (ASR::is_a<ASR::Complex_t>(*t)) {
            level = std::max(level, 2);
            int k = ASR::down_cast<ASR::Complex_t>(t)->m_kind;
            std::string fn = "FKindsIO.toFloatR" + std::to_string(k);
            std::string zv = "fz'" + std::to_string(depth);
            return "((fun " + zv + " => #[FFmtVal.real (" + fn + " " + zv
                + ".re), FFmtVal.real (" + fn + " " + zv + ".im)]) ("
                + val_src + " : " + type(t, loc) + "))";
        }
        if (ASR::is_a<ASR::Logical_t>(*t)) {
            return "#[FFmtVal.bool " + val_src + "]";
        }
        if (ASR::is_a<ASR::String_t>(*t)) {
            return "#[FFmtVal.str " + val_src + "]";
        }
        if (ASR::is_a<ASR::StructType_t>(*t)) {
            ASR::Struct_t *st = resolve_struct(type_decl, loc);
            std::vector<ASR::Variable_t*> ms = all_struct_members(st, loc);
            if (ms.empty()) return "((fun _ => (#[] : Array FFmtVal)) " + atom({val_src, pApp}) + ")";
            std::string sv = "fs'" + std::to_string(depth);
            std::string parts;
            for (size_t i = 0; i < ms.size(); i++) {
                ASR::Variable_t *mv = ms[i];
                if (i) parts += " ++ ";
                parts += fval_val(sv + "." + name(mv->m_name, mv->base.base.loc),
                    mv->m_type, mv->m_type_declaration, loc, depth + 1);
            }
            return "((fun " + sv + " => " + parts + ") " + atom({val_src, pApp}) + ")";
        }
        unsupported("formatting this value is", loc);
    }

    std::string fval_expr(ASR::expr_t *e) {
        const Location &loc = e->base.loc;
        if (ASR::is_a<ASR::StringConstant_t>(*e)) {
            return "#[FFmtVal.str " + string_lit(string_const_str(
                *ASR::down_cast<ASR::StringConstant_t>(e))) + "]";
        }
        return fval_val(atom(lower(e)), unalloc(ASRUtils::expr_type(e)), expr_type_decl(e), loc);
    }

    std::string lower_string_format(const ASR::StringFormat_t &f,
            const std::string &sep, const Location &loc) {
        if (!f.m_fmt) {
            std::string items;
            for (size_t i = 0; i < f.n_args; i++) {
                if (i) items += ", ";
                items += shown(f.m_args[i]);
            }
            return "(String.intercalate " + sep + " [" + items + "])";
        }
        if (!is_string(f.m_fmt)) unsupported("formatted output is", loc);
        std::string fmt_src = atom(lower(f.m_fmt));
        std::string vals = "(#[] : Array FFmtVal)";
        for (size_t i = 0; i < f.n_args; i++) {
            std::string elem = fval_expr(f.m_args[i]);
            vals = (i == 0) ? elem : "(" + vals + " ++ " + elem + ")";
        }
        return "(FFmt.format " + fmt_src + " " + vals + ")";
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
        if (is_string(code)) {
            level = std::max(level, 2);
            line("IO.println " + atom(lower(code)));
            line(std::string("LFortran.halt ") + (error ? "1" : "0"));
            return;
        }
        unsupported("this stop code is", loc);
    }

    static bool body_has_exit(ASR::stmt_t **body, size_t n, bool nested = false) {
        for (size_t i = 0; i < n; i++) {
            ASR::stmt_t *s = body[i];
            if (ASR::is_a<ASR::Exit_t>(*s)) {
                auto &ex = *ASR::down_cast<ASR::Exit_t>(s);
                if (!nested || ex.m_stmt_name) return true;
            } else if (ASR::is_a<ASR::Cycle_t>(*s)) {
                auto &cy = *ASR::down_cast<ASR::Cycle_t>(s);
                if (cy.m_stmt_name) return true;
            } else if (ASR::is_a<ASR::DoLoop_t>(*s)) {
                auto &d = *ASR::down_cast<ASR::DoLoop_t>(s);
                if (body_has_exit(d.m_body, d.n_body, true)) return true;
            } else if (ASR::is_a<ASR::WhileLoop_t>(*s)) {
                auto &d = *ASR::down_cast<ASR::WhileLoop_t>(s);
                if (body_has_exit(d.m_body, d.n_body, true)) return true;
            } else if (ASR::is_a<ASR::DoConcurrentLoop_t>(*s)) {
                auto &d = *ASR::down_cast<ASR::DoConcurrentLoop_t>(s);
                if (body_has_exit(d.m_body, d.n_body, true)) return true;
            } else if (ASR::is_a<ASR::If_t>(*s)) {
                auto &d = *ASR::down_cast<ASR::If_t>(s);
                if (body_has_exit(d.m_body, d.n_body, nested)
                        || body_has_exit(d.m_orelse, d.n_orelse, nested)) {
                    return true;
                }
            } else if (ASR::is_a<ASR::Select_t>(*s)) {
                auto &d = *ASR::down_cast<ASR::Select_t>(s);
                for (size_t j = 0; j < d.n_body; j++) {
                    ASR::case_stmt_t *c = d.m_body[j];
                    if (ASR::is_a<ASR::CaseStmt_t>(*c)) {
                        auto &cs = *ASR::down_cast<ASR::CaseStmt_t>(c);
                        if (body_has_exit(cs.m_body, cs.n_body, nested)) return true;
                    } else if (ASR::is_a<ASR::CaseStmt_Range_t>(*c)) {
                        auto &cs = *ASR::down_cast<ASR::CaseStmt_Range_t>(c);
                        if (body_has_exit(cs.m_body, cs.n_body, nested)) return true;
                    }
                }
                if (body_has_exit(d.m_default, d.n_default, nested)) return true;
            } else if (ASR::is_a<ASR::BlockCall_t>(*s)) {
                ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                if (ASR::is_a<ASR::Block_t>(*sym)) {
                    ASR::Block_t *b = ASR::down_cast<ASR::Block_t>(sym);
                    if (body_has_exit(b->m_body, b->n_body, nested)) return true;
                }
            }
        }
        return false;
    }

    void propagate_named_exits() {
        if (loop_stack.empty()) return;
        for (size_t j = 0; j < loop_stack.size(); j++) {
            const LoopFrame &f = loop_stack[j];
            if (f.named_brk.empty() && f.named_cyc.empty()) continue;
            if (j + 1 == loop_stack.size()) {
                if (!f.named_brk.empty()) {
                    line("if " + f.named_brk + " then break");
                }
                if (!f.named_cyc.empty()) {
                    line("if " + f.named_cyc + " then");
                    ind++;
                    line(f.named_cyc + " := false");
                    line("continue");
                    ind--;
                }
            } else {
                line("if " + f.named_brk + " || " + f.named_cyc + " then break");
            }
        }
    }

    void emit_where_body(ASR::stmt_t **body, size_t n, const std::string &mask_var,
            const Location &loc) {
        for (size_t i = 0; i < n; i++) {
            ASR::stmt_t *s = body[i];
            if (ASR::is_a<ASR::Where_t>(*s)) {
                auto &w = *ASR::down_cast<ASR::Where_t>(s);
                std::string cv = "wc'" + std::to_string(++loop_id);
                line("let " + cv + " : Arr Bool := " + lower(w.m_test).src);
                std::string mv = "wm'" + std::to_string(++loop_id);
                line("let " + mv + " : Arr Bool := Arr.zipWith (fun a' b' => a' && b') "
                    + mask_var + " " + cv);
                emit_where_body(w.m_body, w.n_body, mv, w.base.base.loc);
                if (w.n_orelse > 0) {
                    std::string ev = "wm'" + std::to_string(++loop_id);
                    line("let " + ev + " : Arr Bool := Arr.zipWith (fun a' b' => a' && !b') "
                        + mask_var + " " + cv);
                    emit_where_body(w.m_orelse, w.n_orelse, ev, w.base.base.loc);
                }
            } else if (ASR::is_a<ASR::Assignment_t>(*s)) {
                auto &x = *ASR::down_cast<ASR::Assignment_t>(s);
                const Location &aloc = x.base.base.loc;
                if (x.m_overloaded || !is_array(ASRUtils::expr_type(x.m_target))) {
                    unsupported("this where statement is", aloc);
                }
                ASR::ttype_t *et = elem_type(ASRUtils::expr_type(x.m_target));
                if (ASR::is_a<ASR::String_t>(*et) && !ASR::down_cast<ASR::String_t>(et)->m_len) {
                    unsupported("this where statement is", aloc);
                }
                ASR::expr_t *v = unwrap_physical_cast(x.m_value);
                if (ASR::is_a<ASR::ArrayBroadcast_t>(*v)) {
                    v = ASR::down_cast<ASR::ArrayBroadcast_t>(v)->m_array;
                }
                bool rhs_is_arr = is_array(ASRUtils::expr_type(v));
                std::string rv = "rhs'" + std::to_string(++loop_id);
                if (rhs_is_arr) {
                    std::string src = atom(lower(v));
                    if (ASR::is_a<ASR::String_t>(*et)) {
                        src = "(Arr.map (FStr.fix "
                            + extent(ASR::down_cast<ASR::String_t>(et)->m_len) + ") " + src + ")";
                    }
                    line("let " + rv + " := " + src);
                } else {
                    std::string rhs = fix_string(et, "", lower(v), aloc);
                    line("let " + rv + " := " + rhs);
                }
                std::string t_arg = rhs_is_arr ? "(Sum.inr " + rv + ")" : "(Sum.inl " + rv + ")";
                if (ASR::is_a<ASR::ArraySection_t>(*x.m_target)
                        || ASR::is_a<ASR::ArrayItem_t>(*x.m_target)) {
                    ASR::expr_t *sec_v = nullptr;
                    ASR::array_index_t *sec_args = nullptr;
                    size_t sec_n_args = 0;
                    if (ASR::is_a<ASR::ArraySection_t>(*x.m_target)) {
                        auto &sec = *ASR::down_cast<ASR::ArraySection_t>(x.m_target);
                        sec_v = sec.m_v; sec_args = sec.m_args; sec_n_args = sec.n_args;
                    } else {
                        auto &sec = *ASR::down_cast<ASR::ArrayItem_t>(x.m_target);
                        sec_v = sec.m_v; sec_args = sec.m_args; sec_n_args = sec.n_args;
                    }
                    if (!is_lvalue(sec_v)) unsupported("this where statement is", aloc);
                    std::vector<std::string> spec_parts;
                    for (size_t k = 0; k < sec_n_args; k++) {
                        const ASR::array_index_t &idx = sec_args[k];
                        if (!idx.m_left && !idx.m_step && idx.m_right) {
                            if (is_array(ASRUtils::expr_type(idx.m_right))) {
                                std::string vv = "sv'" + std::to_string(++loop_id);
                                line("let " + vv + " : Array Int := (" + atom(lower(idx.m_right))
                                    + ".data.map FInt.toInt)");
                                spec_parts.push_back("Arr.DimSpec.vec " + vv);
                            } else {
                                std::string iv = "si'" + std::to_string(++loop_id);
                                line("let " + iv + " : Int := " + int_expr(idx.m_right).src);
                                spec_parts.push_back("Arr.DimSpec.idx " + iv);
                            }
                        } else {
                            std::string lo = "none", hi = "none", st = "1";
                            if (idx.m_left) {
                                std::string lv = "sl'" + std::to_string(++loop_id);
                                line("let " + lv + " : Int := " + int_expr(idx.m_left).src);
                                lo = "(some " + lv + ")";
                            }
                            if (idx.m_right) {
                                std::string hv = "sh'" + std::to_string(++loop_id);
                                line("let " + hv + " : Int := " + int_expr(idx.m_right).src);
                                hi = "(some " + hv + ")";
                            }
                            if (idx.m_step) {
                                std::string sv = "ss'" + std::to_string(++loop_id);
                                line("let " + sv + " : Int := " + int_expr(idx.m_step).src);
                                st = sv;
                            }
                            spec_parts.push_back("Arr.DimSpec.slice " + lo + " " + hi + " " + st);
                        }
                    }
                    std::string specs_src = int_array(spec_parts);
                    update_lvalue(sec_v, [&](const std::string &cur_a) {
                        return "Arr.setSlice " + cur_a + " " + specs_src
                            + " (Arr.merge " + t_arg + " (Sum.inr (Arr.slice " + cur_a
                            + " " + specs_src + ")) (Sum.inr " + mask_var + "))";
                    }, aloc);
                } else {
                    if (!is_lvalue(x.m_target)) unsupported("this where statement is", aloc);
                    update_lvalue(x.m_target, [&](const std::string &cur_a) {
                        return "Arr.copyBack " + cur_a + " (Arr.merge " + t_arg
                            + " (Sum.inr " + cur_a + ") (Sum.inr " + mask_var + "))";
                    }, aloc);
                }
            } else {
                unsupported("this where statement is", loc);
            }
        }
    }

    bool is_lvalue(ASR::expr_t *e) {
        e = unwrap_physical_cast(e);
        if (!e) return false;
        if (ASR::is_a<ASR::Var_t>(*e)) return true;
        if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
            auto &m = *ASR::down_cast<ASR::StructInstanceMember_t>(e);
            return !is_array(ASRUtils::expr_type(m.m_v)) && is_lvalue(m.m_v);
        }
        if (ASR::is_a<ASR::ComplexRe_t>(*e)) {
            auto &c = *ASR::down_cast<ASR::ComplexRe_t>(e);
            return !is_array(ASRUtils::expr_type(c.m_arg)) && is_lvalue(c.m_arg);
        }
        if (ASR::is_a<ASR::ComplexIm_t>(*e)) {
            auto &c = *ASR::down_cast<ASR::ComplexIm_t>(e);
            return !is_array(ASRUtils::expr_type(c.m_arg)) && is_lvalue(c.m_arg);
        }
        if (ASR::is_a<ASR::ArrayItem_t>(*e)) {
            return is_lvalue(ASR::down_cast<ASR::ArrayItem_t>(e)->m_v);
        }
        return false;
    }

    void update_lvalue(ASR::expr_t *lhs,
            const std::function<std::string(const std::string&)> &rhs_fn,
            const Location &loc, bool raw = false) {
        lhs = unwrap_physical_cast(lhs);
        if (!raw && is_option_expr(lhs)) {
            std::string ov = "o'" + std::to_string(++loop_id);
            update_lvalue(lhs, [&](const std::string &cur_opt) {
                return "(" + cur_opt + ").map (fun " + ov + " => " + rhs_fn(ov) + ")";
            }, loc, /*raw=*/true);
            return;
        }
        if (ASR::is_a<ASR::Var_t>(*lhs)) {
            if (ASR::Variable_t *gv = global_var(lhs)) {
                std::string gf = global_fields[gv];
                std::string tmp = "v'" + std::to_string(++loop_id);
                line("let " + tmp + " := " + rhs_fn("(← get)." + gf));
                line("modify fun g' => { g' with " + gf + " := " + tmp + " }");
                return;
            }
            std::string a = lower_raw(lhs).src;
            line(a + " := " + rhs_fn(a));
            return;
        }
        if (ASR::is_a<ASR::StructInstanceMember_t>(*lhs)) {
            auto &m = *ASR::down_cast<ASR::StructInstanceMember_t>(lhs);
            if (is_array(ASRUtils::expr_type(m.m_v))) {
                unsupported("this assignment is", loc);
            }
            ASR::symbol_t *ms = ASRUtils::symbol_get_past_external(m.m_m);
            std::string fn;
            if (ASR::is_a<ASR::Variable_t>(*ms)) {
                fn = name(ASR::down_cast<ASR::Variable_t>(ms)->m_name, loc);
            } else if (ASR::is_a<ASR::Struct_t>(*ms)) {
                fn = struct_parent_proj(ASR::down_cast<ASR::Struct_t>(ms), loc);
            } else {
                unsupported("this assignment is", loc);
            }
            update_lvalue(m.m_v, [&](const std::string &cur_s) {
                return "{ " + cur_s + " with " + fn + " := " + rhs_fn(cur_s + "." + fn) + " }";
            }, loc, /*raw=*/false);
            return;
        }
        if (ASR::is_a<ASR::ComplexRe_t>(*lhs)) {
            auto &c = *ASR::down_cast<ASR::ComplexRe_t>(lhs);
            if (is_array(ASRUtils::expr_type(c.m_arg))) unsupported("this assignment is", loc);
            update_lvalue(c.m_arg, [&](const std::string &cur_z) {
                return "{ " + cur_z + " with re := " + rhs_fn(cur_z + ".re") + " }";
            }, loc, /*raw=*/false);
            return;
        }
        if (ASR::is_a<ASR::ComplexIm_t>(*lhs)) {
            auto &c = *ASR::down_cast<ASR::ComplexIm_t>(lhs);
            if (is_array(ASRUtils::expr_type(c.m_arg))) unsupported("this assignment is", loc);
            update_lvalue(c.m_arg, [&](const std::string &cur_z) {
                return "{ " + cur_z + " with im := " + rhs_fn(cur_z + ".im") + " }";
            }, loc, /*raw=*/false);
            return;
        }
        if (ASR::is_a<ASR::ArrayItem_t>(*lhs)) {
            auto &t = *ASR::down_cast<ASR::ArrayItem_t>(lhs);
            auto it = pre_idx.find(&t);
            std::string iv;
            if (it != pre_idx.end()) {
                iv = it->second;
            } else {
                iv = "idx'" + std::to_string(++loop_id);
                line("let " + iv + " := " + index(t));
            }
            update_lvalue(t.m_v, [&](const std::string &cur_a) {
                std::string elem = "(Arr.get " + cur_a + " " + iv + ")";
                return app("Arr.set", {{cur_a, pAtom}, {iv, pAtom},
                    {rhs_fn(elem), pApp}}).src;
            }, loc, /*raw=*/false);
            return;
        }
        unsupported("this assignment is", loc);
    }

    void alloc_one(const ASR::alloc_arg_t &aa, const std::string &src_var,
            bool src_is_arr, const Location &loc) {
        if (aa.n_codims > 0 || aa.m_sym_subclass
                || !is_option_expr(aa.m_a) || !is_lvalue(aa.m_a)) {
            unsupported("this allocate is", loc);
        }
        ASR::ttype_t *it = unalloc(ASRUtils::expr_type(aa.m_a));
        if (aa.m_type) {
            ASR::ttype_t *at = elem_type(aa.m_type);
            ASR::ttype_t *et = elem_type(it);
            if (!at || !et || at->type != et->type
                    || ASR::is_a<ASR::StructType_t>(*at)
                    || ASRUtils::extract_kind_from_ttype_t(at)
                        != ASRUtils::extract_kind_from_ttype_t(et)) {
                unsupported("this allocate is", loc);
            }
        }
        std::string val;
        if (is_array(it)) {
            if (aa.n_dims == 0) {
                if (src_var.empty() || !src_is_arr) unsupported("this allocate is", loc);
                std::string s = src_var;
                ASR::ttype_t *et = elem_type(it);
                if (ASR::is_a<ASR::String_t>(*et)) {
                    ASR::expr_t *le = aa.m_len_expr ? aa.m_len_expr
                        : ASR::down_cast<ASR::String_t>(et)->m_len;
                    if (le) {
                        s = "{ " + s + " with data := " + s + ".data.map (FStr.fix "
                            + extent(le) + ") }";
                    }
                }
                val = "some " + s;
            } else {
                std::vector<std::string> lbs, exts;
                for (size_t j = 0; j < aa.n_dims; j++) {
                    lbs.push_back(aa.m_dims[j].m_start ? int_expr(aa.m_dims[j].m_start).src : "1");
                    if (!aa.m_dims[j].m_length) unsupported("this allocate is", loc);
                    exts.push_back(extent(aa.m_dims[j].m_length));
                }
                ASR::ttype_t *et = elem_type(it);
                if (!src_var.empty()) {
                    if (src_is_arr) {
                        std::string s = src_var;
                        if (ASR::is_a<ASR::String_t>(*et)) {
                            ASR::expr_t *le = aa.m_len_expr ? aa.m_len_expr
                                : ASR::down_cast<ASR::String_t>(et)->m_len;
                            if (le) s = "(Arr.map (FStr.fix " + extent(le) + ") " + s + ")";
                        }
                        val = "some (Arr.reshape " + s + " " + int_array(lbs) + " "
                            + int_array(exts) + ")";
                    } else {
                        std::string elem = src_var;
                        if (ASR::is_a<ASR::String_t>(*et)) {
                            ASR::expr_t *le = aa.m_len_expr ? aa.m_len_expr
                                : ASR::down_cast<ASR::String_t>(et)->m_len;
                            if (le) elem = "(FStr.fix " + extent(le) + " " + elem + ")";
                        }
                        val = "some (Arr.make " + int_array(lbs) + " " + int_array(exts)
                            + " " + elem + ")";
                    }
                } else {
                    std::string z = (ASR::is_a<ASR::String_t>(*et) && aa.m_len_expr)
                        ? "FStr.fix " + extent(aa.m_len_expr) + " \"\""
                        : zero(et, loc, expr_type_decl(aa.m_a));
                    val = "some (Arr.make " + int_array(lbs) + " " + int_array(exts)
                        + " " + atom({z, pApp}) + ")";
                }
            }
        } else {
            if (!src_var.empty()) {
                std::string elem = src_var;
                if (ASR::is_a<ASR::String_t>(*it)) {
                    ASR::expr_t *le = aa.m_len_expr ? aa.m_len_expr
                        : ASR::down_cast<ASR::String_t>(it)->m_len;
                    if (le) elem = "(FStr.fix " + extent(le) + " " + elem + ")";
                }
                val = "some " + elem;
            } else {
                std::string z;
                ASR::Variable_t *av = ASR::is_a<ASR::Var_t>(*aa.m_a)
                    ? ASRUtils::EXPR2VAR(aa.m_a) : nullptr;
                if (ASR::is_a<ASR::String_t>(*it) && aa.m_len_expr) {
                    z = "FStr.fix " + extent(aa.m_len_expr) + " \"\"";
                } else if (av && alloc_str_len.count(av)) {
                    z = "FStr.fix " + alloc_str_len[av] + " \"\"";
                } else {
                    z = zero(it, loc, expr_type_decl(aa.m_a));
                }
                val = "some " + atom({z, pApp});
            }
        }
        if (val.find("←") != std::string::npos) {
            std::string rv = "rhs'" + std::to_string(++loop_id);
            line("let " + rv + " := " + val);
            val = rv;
        }
        update_lvalue(aa.m_a, [&](const std::string &) { return val; }, loc, /*raw=*/true);
    }

    void hoist_lvalue_indices(ASR::expr_t *e) {
        e = unwrap_physical_cast(e);
        if (!e) return;
        if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
            hoist_lvalue_indices(ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v);
        } else if (ASR::is_a<ASR::ComplexRe_t>(*e)) {
            hoist_lvalue_indices(ASR::down_cast<ASR::ComplexRe_t>(e)->m_arg);
        } else if (ASR::is_a<ASR::ComplexIm_t>(*e)) {
            hoist_lvalue_indices(ASR::down_cast<ASR::ComplexIm_t>(e)->m_arg);
        } else if (ASR::is_a<ASR::ArrayItem_t>(*e)) {
            auto &t = *ASR::down_cast<ASR::ArrayItem_t>(e);
            hoist_lvalue_indices(t.m_v);
            if (!pre_idx.count(&t)) {
                std::string iv = "idx'" + std::to_string(++loop_id);
                line("let " + iv + " := " + index(t));
                pre_idx[&t] = iv;
            }
        }
    }

    void stmts(ASR::stmt_t **body, size_t n) {
        for (size_t i = 0; i < n; i++) stmt(body[i]);
    }

    void stmt(ASR::stmt_t *s) {
        const Location &loc = s->base.loc;
        switch (s->type) {
            case ASR::stmtType::Assignment: {
                auto &x = *ASR::down_cast<ASR::Assignment_t>(s);
                if (x.m_overloaded) {
                    stmt(x.m_overloaded);
                    break;
                }
                if (ASR::is_a<ASR::ArrayItem_t>(*x.m_target)
                        && !is_array(ASRUtils::expr_type(x.m_target))) {
                    auto &t = *ASR::down_cast<ASR::ArrayItem_t>(x.m_target);
                    ASR::expr_t *base = t.m_v;
                    while (ASR::is_a<ASR::ArrayPhysicalCast_t>(*base)) {
                        base = ASR::down_cast<ASR::ArrayPhysicalCast_t>(base)->m_arg;
                    }
                    if (ASR::is_a<ASR::Var_t>(*base) && !global_var(base) && !is_option_expr(base)) {
                        std::string a = lower(base).src;
                        std::string rhs = fix_string(ASRUtils::expr_type(x.m_target), "",
                            lower(x.m_value), loc);
                        line(a + " := " + app("Arr.set", {{a, pAtom}, {index(t), pAtom},
                            {rhs, pApp}}).src);
                        break;
                    }
                }
                if (ASR::is_a<ASR::ArraySection_t>(*x.m_target)
                        || (ASR::is_a<ASR::ArrayItem_t>(*x.m_target)
                            && is_array(ASRUtils::expr_type(x.m_target)))) {
                    ASR::expr_t *sec_v = nullptr;
                    ASR::array_index_t *sec_args = nullptr;
                    size_t sec_n_args = 0;
                    if (ASR::is_a<ASR::ArraySection_t>(*x.m_target)) {
                        auto &sec = *ASR::down_cast<ASR::ArraySection_t>(x.m_target);
                        sec_v = sec.m_v; sec_args = sec.m_args; sec_n_args = sec.n_args;
                    } else {
                        auto &sec = *ASR::down_cast<ASR::ArrayItem_t>(x.m_target);
                        sec_v = sec.m_v; sec_args = sec.m_args; sec_n_args = sec.n_args;
                    }
                    if (!is_lvalue(sec_v)) unsupported("this assignment is", loc);
                    // Hoist all explicit section bounds/steps/vectors before the update so
                    // side effects execute once and no `←` enters `update_lvalue`.
                    struct SpecH {
                        int kind = 0; // 0 = slice, 1 = idx, 2 = vec
                        std::string idx, lo, hi, st;
                    };
                    std::vector<SpecH> specs(sec_n_args);
                    for (size_t i = 0; i < sec_n_args; i++) {
                        const ASR::array_index_t &idx = sec_args[i];
                        if (!idx.m_left && !idx.m_step && idx.m_right) {
                            if (is_array(ASRUtils::expr_type(idx.m_right))) {
                                specs[i].kind = 2;
                                std::string vv = "sv'" + std::to_string(++loop_id);
                                line("let " + vv + " : Array Int := (" + atom(lower(idx.m_right))
                                    + ".data.map FInt.toInt)");
                                specs[i].idx = vv;
                            } else {
                                specs[i].kind = 1;
                                std::string iv = "si'" + std::to_string(++loop_id);
                                line("let " + iv + " : Int := " + int_expr(idx.m_right).src);
                                specs[i].idx = iv;
                            }
                        } else {
                            if (idx.m_left) {
                                std::string lv = "sl'" + std::to_string(++loop_id);
                                line("let " + lv + " : Int := " + int_expr(idx.m_left).src);
                                specs[i].lo = lv;
                            }
                            if (idx.m_right) {
                                std::string hv = "sh'" + std::to_string(++loop_id);
                                line("let " + hv + " : Int := " + int_expr(idx.m_right).src);
                                specs[i].hi = hv;
                            }
                            if (idx.m_step) {
                                std::string sv = "ss'" + std::to_string(++loop_id);
                                line("let " + sv + " : Int := " + int_expr(idx.m_step).src);
                                specs[i].st = sv;
                            } else {
                                specs[i].st = "1";
                            }
                        }
                    }
                    std::vector<std::string> spec_parts;
                    for (size_t i = 0; i < specs.size(); i++) {
                        if (specs[i].kind == 1) {
                            spec_parts.push_back("Arr.DimSpec.idx " + specs[i].idx);
                        } else if (specs[i].kind == 2) {
                            spec_parts.push_back("Arr.DimSpec.vec " + specs[i].idx);
                        } else {
                            std::string lo = specs[i].lo.empty() ? "none" : "(some " + specs[i].lo + ")";
                            std::string hi = specs[i].hi.empty() ? "none" : "(some " + specs[i].hi + ")";
                            spec_parts.push_back("Arr.DimSpec.slice " + lo + " " + hi + " " + specs[i].st);
                        }
                    }
                    std::string specs_src = int_array(spec_parts);
                    ASR::ttype_t *et = elem_type(ASRUtils::expr_type(x.m_target));
                    ASR::expr_t *v = unwrap_physical_cast(x.m_value);
                    if (ASR::is_a<ASR::ArrayBroadcast_t>(*v)) {
                        v = ASR::down_cast<ASR::ArrayBroadcast_t>(v)->m_array;
                    }
                    if (is_array(ASRUtils::expr_type(v))) {
                        std::string src = atom(lower(v));
                        if (ASR::is_a<ASR::String_t>(*et)) {
                            auto &st = *ASR::down_cast<ASR::String_t>(et);
                            if (!st.m_len) {
                                unsupported("an assumed- or deferred-length character array section assignment is", loc);
                            }
                            src = "(Arr.map (FStr.fix " + extent(st.m_len) + ") " + src + ")";
                        }
                        std::string rv = "rhs'" + std::to_string(++loop_id);
                        line("let " + rv + " := " + src);
                        update_lvalue(sec_v, [&](const std::string &cur_a) {
                            return "Arr.setSlice " + cur_a + " " + specs_src + " " + rv;
                        }, loc);
                    } else {
                        if (ASR::is_a<ASR::String_t>(*et) && !ASR::down_cast<ASR::String_t>(et)->m_len) {
                            unsupported("an assumed- or deferred-length character array section assignment is", loc);
                        }
                        std::string rhs = fix_string(et, "", lower(v), loc);
                        std::string rv = "rhs'" + std::to_string(++loop_id);
                        line("let " + rv + " := " + rhs);
                        update_lvalue(sec_v, [&](const std::string &cur_a) {
                            return "Arr.fillSlice " + cur_a + " " + specs_src + " " + rv;
                        }, loc);
                    }
                    break;
                }
                if (ASR::is_a<ASR::StringSection_t>(*x.m_target)) {
                    auto &sec = *ASR::down_cast<ASR::StringSection_t>(x.m_target);
                    bool unit_step = !sec.m_step
                        || (ASR::is_a<ASR::IntegerConstant_t>(*sec.m_step)
                            && ASR::down_cast<ASR::IntegerConstant_t>(sec.m_step)->m_n == 1);
                    if (!unit_step || !is_lvalue(sec.m_arg)
                            || is_array(ASRUtils::expr_type(sec.m_arg))) {
                        unsupported("this string section assignment is", loc);
                    }
                    std::string lo = "1";
                    if (sec.m_start) {
                        lo = "sl'" + std::to_string(++loop_id);
                        line("let " + lo + " : Int := " + int_expr(sec.m_start).src);
                    }
                    std::string hi;
                    if (sec.m_end) {
                        hi = "sh'" + std::to_string(++loop_id);
                        line("let " + hi + " : Int := " + int_expr(sec.m_end).src);
                    }
                    std::string rv = "rhs'" + std::to_string(++loop_id);
                    line("let " + rv + " := " + lower(x.m_value).src);
                    update_lvalue(sec.m_arg, [&](const std::string &cur_s) {
                        std::string hv = hi.empty() ? "(FStr.len " + cur_s + ")" : hi;
                        return "FStr.setSlice " + cur_s + " " + lo + " " + hv + " " + rv;
                    }, loc);
                    break;
                }
                if (!is_lvalue(x.m_target)) {
                    unsupported("this assignment is", loc);
                }
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
                            if (st.m_len) {
                                src = "(Arr.map (FStr.fix " + extent(st.m_len) + ") " + src + ")";
                            } else if (st.m_len_kind != ASR::string_length_kindType::DeferredLength) {
                                unsupported("an assumed-length character array assignment is", loc);
                            }
                        }
                        bool target_alloc = is_allocatable(ASRUtils::expr_type(x.m_target));
                        if (src.find("←") != std::string::npos || target_alloc) {
                            std::string rv = "rhs'" + std::to_string(++loop_id);
                            line("let " + rv + " := " + src);
                            src = rv;
                        }
                        if (target_alloc) {
                            ASR::expr_t *vu = unwrap_physical_cast(v);
                            bool keep_lbs = ASR::is_a<ASR::Var_t>(*vu)
                                || ASR::is_a<ASR::StructInstanceMember_t>(*vu);
                            std::string fresh = keep_lbs ? src
                                : "{ " + src + " with lbs := Array.replicate " + src + ".exts.size (1 : Int) }";
                            update_lvalue(x.m_target, [&](const std::string &cur_opt) {
                                return "(match " + cur_opt + " with | some c' => if c'.exts == "
                                    + src + ".exts then some (Arr.copyBack c' " + src
                                    + ") else some " + fresh + " | none => some " + fresh + ")";
                            }, loc, /*raw=*/true);
                        } else {
                            update_lvalue(x.m_target, [&](const std::string &cur_a) {
                                return "Arr.copyBack " + cur_a + " " + src;
                            }, loc);
                        }
                    } else {
                        std::string rhs = fix_string(et, "", lower(v), loc);
                        if (rhs.find("←") != std::string::npos) {
                            std::string rv = "rhs'" + std::to_string(++loop_id);
                            line("let " + rv + " := " + rhs);
                            rhs = rv;
                        }
                        update_lvalue(x.m_target, [&](const std::string &cur_a) {
                            return "Arr.fill " + cur_a + " " + atom({rhs, pApp});
                        }, loc);
                    }
                    break;
                }
                if (is_allocatable(ASRUtils::expr_type(x.m_target))) {
                    ASR::Variable_t *tv = ASR::is_a<ASR::Var_t>(*x.m_target)
                        ? ASRUtils::EXPR2VAR(x.m_target) : nullptr;
                    auto len_for_target = [&]() -> std::string {
                        if (tv && alloc_str_len.count(tv)) return alloc_str_len[tv];
                        ASR::ttype_t *it = unalloc(ASRUtils::expr_type(x.m_target));
                        if (ASR::is_a<ASR::String_t>(*it) && ASR::down_cast<ASR::String_t>(it)->m_len) {
                            return extent(ASR::down_cast<ASR::String_t>(it)->m_len);
                        }
                        return "";
                    };
                    if (is_option_expr(x.m_value)) {
                        Val rhs = lower_raw(x.m_value);
                        std::string val = rhs.src;
                        std::string slen = len_for_target();
                        if (!slen.empty()) {
                            val = "(" + val + ").map (FStr.fix " + slen + ")";
                        }
                        if (val.find("←") != std::string::npos) {
                            std::string rv = "rhs'" + std::to_string(++loop_id);
                            line("let " + rv + " := " + val);
                            val = rv;
                        }
                        update_lvalue(x.m_target, [&](const std::string &) {
                            return val;
                        }, loc, /*raw=*/true);
                        break;
                    }
                    Val rhs = lower(x.m_value);
                    std::string slen = len_for_target();
                    std::string val = !slen.empty()
                        ? "FStr.fix " + slen + " " + atom(rhs)
                        : fix_string(ASRUtils::expr_type(x.m_target), "", rhs, loc);
                    if (val.find("←") != std::string::npos) {
                        std::string rv = "rhs'" + std::to_string(++loop_id);
                        line("let " + rv + " := " + val);
                        val = rv;
                    }
                    update_lvalue(x.m_target, [&](const std::string &) {
                        return "some " + atom({val, pApp});
                    }, loc, /*raw=*/true);
                    break;
                }
                Val rhs = lower(x.m_value);
                if (rhs.src.find("←") != std::string::npos) {
                    std::string rv = "rhs'" + std::to_string(++loop_id);
                    line("let " + rv + " := " + rhs.src);
                    rhs = {rv, pAtom};
                }
                update_lvalue(x.m_target, [&](const std::string &cur_v) {
                    return fix_string(ASRUtils::expr_type(x.m_target), cur_v, rhs, loc);
                }, loc);
                break;
            }
            case ASR::stmtType::SubroutineCall: {
                auto &x = *ASR::down_cast<ASR::SubroutineCall_t>(s);
                if (x.m_dt) unsupported("a type-bound call is", loc);
                ASR::Function_t *fn = callee(x.m_name, loc);
                if (fn->m_return_var || x.n_args != fn->n_args) unsupported("this call is", loc);
                // The `intent(out)`/`intent(inout)` actual arguments, in order.
                // Hoist any array-element subscripts before the call so earlier
                // copy-backs cannot alter the index and subscripts are evaluated once.
                struct OutArg {
                    ASR::expr_t *actual;
                    bool d_alloc;
                    bool d_opt;
                };
                std::vector<OutArg> outs;
                for (size_t i = 0; i < x.n_args; i++) {
                    if (arg_intent(fn, i) == ASR::intentType::In) continue;
                    ASR::Variable_t *d = dummy(fn, i);
                    bool d_alloc = d && is_allocatable(d->m_type);
                    bool d_opt = d && is_optional_dummy(d);
                    ASR::expr_t *a = x.m_args[i].m_value;
                    if (!a) {
                        if (d_opt) {
                            outs.push_back({nullptr, false, true});
                            continue;
                        }
                        unsupported("an omitted argument is", loc);
                    }
                    a = unwrap_physical_cast(a);
                    if (!is_lvalue(a)) {
                        unsupported("this intent(out) or intent(inout) argument is",
                            a->base.loc);
                    }
                    if (d_alloc && !is_option_expr(a)) {
                        unsupported("passing a non-allocatable to an allocatable dummy is",
                            a->base.loc);
                    }
                    hoist_lvalue_indices(a);
                    outs.push_back({a, d_alloc, d_opt});
                }
                Val c = call(x.m_name, x.m_args, x.n_args, loc);
                auto assign = [&](const OutArg &o, const std::string &v) {
                    ASR::expr_t *a = o.actual;
                    if (!a) return;
                    if (o.d_alloc) {
                        update_lvalue(a, [&](const std::string &) { return v; }, loc, /*raw=*/true);
                    } else if (o.d_opt) {
                        if (is_option_expr(a)) {
                            if (is_array(ASRUtils::expr_type(a))) {
                                update_lvalue(a, [&](const std::string &cur_opt) {
                                    return "(match " + cur_opt + ", " + v
                                        + " with | some c', some s' => some (Arr.copyBack c' s') | _, _ => "
                                        + cur_opt + ")";
                                }, loc, /*raw=*/true);
                            } else {
                                update_lvalue(a, [&](const std::string &cur_opt) {
                                    return "(match " + cur_opt + ", " + v
                                        + " with | some c', some s' => some ("
                                        + fix_string(ASRUtils::expr_type(a), "c'", {"s'", pAtom}, loc)
                                        + ") | _, _ => " + cur_opt + ")";
                                }, loc, /*raw=*/true);
                            }
                        } else if (is_array(ASRUtils::expr_type(a))) {
                            update_lvalue(a, [&](const std::string &cur_a) {
                                return "Arr.copyBack " + cur_a + " (" + v + ").get!";
                            }, loc);
                        } else {
                            update_lvalue(a, [&](const std::string &cur_v) {
                                return fix_string(ASRUtils::expr_type(a), cur_v,
                                    {"(" + v + ").get!", pAtom}, loc);
                            }, loc);
                        }
                    } else if (is_array(ASRUtils::expr_type(a))) {
                        update_lvalue(a, [&](const std::string &cur_a) {
                            return "Arr.copyBack " + cur_a + " " + v;
                        }, loc);
                    } else {
                        update_lvalue(a, [&](const std::string &cur_v) {
                            return fix_string(ASRUtils::expr_type(a), cur_v, {v, pAtom}, loc);
                        }, loc);
                    }
                };
                auto [cal_m, cal_expr] = monadic_call(fn, c);
                if (outs.empty()) {
                    line(cal_m ? cal_expr : "let _ := " + cal_expr);
                } else if (outs.size() == 1) {
                    if (!outs[0].actual) {
                        line(cal_m ? "let _ ← " + cal_expr : "let _ := " + cal_expr);
                    } else if (cal_m) {
                        line("let r'1 ← " + cal_expr);
                        assign(outs[0], "r'1");
                    } else if (c.src.find("←") != std::string::npos) {
                        line("let r'1 := " + c.src);
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
                    line("let (" + pat + ") " + (cal_m ? "← " : ":= ") + cal_expr);
                    for (size_t i = 0; i < outs.size(); i++) {
                        assign(outs[i], "r'" + std::to_string(i + 1));
                    }
                }
                pre_idx.clear();
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
                if (!h.m_v || !ASR::is_a<ASR::Var_t>(*h.m_v) || !h.m_start || !h.m_end) {
                    unsupported("this do loop is", loc);
                }
                std::string lname = x.m_name ? std::string(x.m_name) : "";
                std::string nb, nc;
                if (!lname.empty()) {
                    std::string nid = std::to_string(++loop_id);
                    nb = "xbrk'" + nid;
                    nc = "xcyc'" + nid;
                    line("let mut " + nb + " : Bool := false");
                    line("let mut " + nc + " : Bool := false");
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
                    loop_stack.push_back({lname, "", nb, nc});
                    if (x.n_body == 0) line("Pure.pure ()");
                    stmts(x.m_body, x.n_body);
                    loop_stack.pop_back();
                    ind--;
                    propagate_named_exits();
                    break;
                }
                // The `do` variable is an ordinary variable: it takes each
                // index, and after the loop the first one not executed,
                // `lo + max(0, (hi - lo + step) / step) * step` unless `exit`
                // left the loop early. The bounds are evaluated once.
                std::string id = std::to_string(++loop_id);
                std::string lv = "lo'" + id, hv = "hi'" + id, iv = "i'" + id;
                bool has_ex = body_has_exit(x.m_body, x.n_body) || !lname.empty();
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
                loop_stack.push_back({lname, brk, nb, nc});
                update_lvalue(h.m_v, [&](const std::string &) { return iv; }, loc);
                stmts(x.m_body, x.n_body);
                loop_stack.pop_back();
                ind--;
                std::string post_val = unit
                    ? "FInt.ofInt (FInt.toInt " + lv + " + Max.max 0 (FInt.toInt " + hv
                        + " + 1 - FInt.toInt " + lv + "))"
                    : "FInt.ofInt (FInt.toInt " + lv + " + (if FInt.toInt " + sv
                        + " == 0 then 0 else Max.max 0 ((FInt.toInt " + hv + " - FInt.toInt "
                        + lv + " + FInt.toInt " + sv + ").tdiv (FInt.toInt " + sv
                        + ")) * FInt.toInt " + sv + "))";
                if (has_ex) {
                    line("if !" + brk + " then");
                    ind++;
                    update_lvalue(h.m_v, [&](const std::string &) { return post_val; }, loc);
                    ind--;
                } else {
                    update_lvalue(h.m_v, [&](const std::string &) { return post_val; }, loc);
                }
                propagate_named_exits();
                break;
            }
            case ASR::stmtType::DoConcurrentLoop: {
                auto &x = *ASR::down_cast<ASR::DoConcurrentLoop_t>(s);
                if (x.n_head == 0) {
                    unsupported("this do concurrent loop is", loc);
                }
                std::vector<std::pair<ASR::Variable_t*, ASR::expr_t*>> local_vars;
                std::set<ASR::Variable_t*> seen_local;
                for (size_t i = 0; i < x.n_local; i++) {
                    if (!x.m_local[i] || !ASR::is_a<ASR::Var_t>(*x.m_local[i])) {
                        unsupported("this do concurrent loop is", loc);
                    }
                    ASR::Variable_t *lv = ASRUtils::EXPR2VAR(x.m_local[i]);
                    if (is_optional_dummy(lv)) {
                        unsupported("this do concurrent loop is", loc);
                    }
                    if (seen_local.insert(lv).second) {
                        local_vars.push_back({lv, x.m_local[i]});
                    }
                }
                struct DCBound {
                    std::string lv, hv, sv, iv;
                    bool unit;
                    bool simple;
                };
                std::vector<DCBound> bounds(x.n_head);
                for (size_t i = 0; i < x.n_head; i++) {
                    const ASR::do_loop_head_t &h = x.m_head[i];
                    if (!h.m_v || !ASR::is_a<ASR::Var_t>(*h.m_v) || !h.m_start || !h.m_end) {
                        unsupported("this do concurrent loop is", loc);
                    }
                    bool unit = !h.m_increment
                        || (ASR::is_a<ASR::IntegerConstant_t>(*h.m_increment)
                            && ASR::down_cast<ASR::IntegerConstant_t>(
                                h.m_increment)->m_n == 1);
                    std::string ty = type(ASRUtils::expr_type(h.m_v), loc);
                    std::string id = std::to_string(++loop_id);
                    bounds[i] = {"lo'" + id, "hi'" + id, unit ? "" : "step'" + id, "i'" + id,
                        unit, loop_vars.count(ASRUtils::EXPR2VAR(h.m_v)) > 0};
                    line("let " + bounds[i].lv + " : " + ty + " := " + lower(h.m_start).src);
                    line("let " + bounds[i].hv + " : " + ty + " := " + lower(h.m_end).src);
                    if (!unit) {
                        line("let " + bounds[i].sv + " : " + ty + " := " + lower(h.m_increment).src);
                    }
                }
                std::function<void(size_t)> emit_nest = [&](size_t idx) {
                    if (idx == x.n_head) {
                        std::string dcid = std::to_string(++loop_id);
                        std::vector<std::pair<ASR::Variable_t*, std::string>> new_aliases;
                        for (auto &[lv, le] : local_vars) {
                            const Location &vloc = lv->base.base.loc;
                            name(lv->m_name, vloc);
                            std::string alias = std::string(lv->m_name) + "'dc" + dcid;
                            std::string linit;
                            if (is_allocatable(lv->m_type)) {
                                linit = "none";
                            } else if (is_array(lv->m_type)) {
                                ASR::ttype_t *et = ASR::down_cast<ASR::Array_t>(lv->m_type)->m_type;
                                if (ASR::is_a<ASR::String_t>(*et)
                                        && !ASR::down_cast<ASR::String_t>(et)->m_len) {
                                    unsupported("this do concurrent loop is", vloc);
                                }
                                std::vector<std::string> lbs, exts;
                                bool assumed;
                                dims(lv->m_type, lbs, exts, assumed, vloc);
                                std::string z = atom({zero(et, vloc, lv->m_type_declaration), pApp});
                                if (assumed) {
                                    std::string outer = atom(lower(le));
                                    linit = "Arr.make " + outer + ".lbs " + outer + ".exts " + z;
                                } else {
                                    linit = "Arr.make " + int_array(lbs) + " "
                                        + int_array(exts) + " " + z;
                                }
                            } else if (ASR::is_a<ASR::String_t>(*lv->m_type)
                                    && !ASR::down_cast<ASR::String_t>(lv->m_type)->m_len) {
                                linit = "FStr.fix " + atom(lower(le)) + ".length \"\"";
                            } else {
                                linit = zero(lv->m_type, vloc, lv->m_type_declaration);
                            }
                            line("let mut " + alias + " : "
                                + vtype(lv->m_type, vloc, lv->m_type_declaration)
                                + " := " + linit);
                            new_aliases.push_back({lv, alias});
                        }
                        std::vector<std::pair<ASR::Variable_t*, std::pair<bool, std::string>>> saved;
                        for (auto &[lv, alias] : new_aliases) {
                            auto it = var_alias.find(lv);
                            if (it != var_alias.end()) {
                                saved.push_back({lv, {true, it->second}});
                            } else {
                                saved.push_back({lv, {false, ""}});
                            }
                            var_alias[lv] = alias;
                        }
                        if (x.n_body == 0) line("Pure.pure ()");
                        stmts(x.m_body, x.n_body);
                        for (auto it = saved.rbegin(); it != saved.rend(); ++it) {
                            if (it->second.first) var_alias[it->first] = it->second.second;
                            else var_alias.erase(it->first);
                        }
                        return;
                    }
                    const ASR::do_loop_head_t &h = x.m_head[idx];
                    const DCBound &b = bounds[idx];
                    std::string iter = b.simple ? lower(h.m_v).src : b.iv;
                    if (b.unit) {
                        line("for " + iter + " in FInt.range " + b.lv + " " + b.hv + " do");
                    } else {
                        line("for " + iter + " in FInt.rangeStep " + b.lv + " " + b.hv + " " + b.sv + " do");
                    }
                    ind++;
                    loop_stack.push_back({"", "", "", ""});
                    if (!b.simple) {
                        update_lvalue(h.m_v, [&](const std::string &) { return b.iv; }, loc);
                    }
                    emit_nest(idx + 1);
                    loop_stack.pop_back();
                    ind--;
                    propagate_named_exits();
                };
                emit_nest(0);
                break;
            }
            case ASR::stmtType::WhileLoop: {
                auto &x = *ASR::down_cast<ASR::WhileLoop_t>(s);
                if (x.n_orelse > 0) unsupported("this while loop is", loc);
                if (cur) cur->uses_while = true;
                std::string lname = x.m_name ? std::string(x.m_name) : "";
                std::string nb, nc;
                if (!lname.empty()) {
                    std::string nid = std::to_string(++loop_id);
                    nb = "xbrk'" + nid;
                    nc = "xcyc'" + nid;
                    line("let mut " + nb + " : Bool := false");
                    line("let mut " + nc + " : Bool := false");
                }
                line("while " + lower(x.m_test).src + " do");
                ind++;
                loop_stack.push_back({lname, "", nb, nc});
                if (x.n_body == 0) line("Pure.pure ()");
                stmts(x.m_body, x.n_body);
                loop_stack.pop_back();
                ind--;
                propagate_named_exits();
                break;
            }
            case ASR::stmtType::Exit: {
                auto &x = *ASR::down_cast<ASR::Exit_t>(s);
                if (loop_stack.empty()) unsupported("this exit is", loc);
                if (!x.m_stmt_name) {
                    if (!loop_stack.back().brk.empty()) line(loop_stack.back().brk + " := true");
                    line("break");
                    break;
                }
                std::string tname = x.m_stmt_name;
                size_t k = loop_stack.size();
                for (size_t j = loop_stack.size(); j > 0; j--) {
                    if (loop_stack[j - 1].name == tname) { k = j - 1; break; }
                }
                if (k == loop_stack.size()) unsupported("this exit is", loc);
                for (size_t j = k; j < loop_stack.size(); j++) {
                    if (!loop_stack[j].brk.empty()) line(loop_stack[j].brk + " := true");
                }
                if (k + 1 < loop_stack.size()) {
                    line(loop_stack[k].named_brk + " := true");
                }
                line("break");
                break;
            }
            case ASR::stmtType::Cycle: {
                auto &x = *ASR::down_cast<ASR::Cycle_t>(s);
                if (loop_stack.empty()) unsupported("this cycle is", loc);
                if (!x.m_stmt_name) {
                    line("continue");
                    break;
                }
                std::string tname = x.m_stmt_name;
                size_t k = loop_stack.size();
                for (size_t j = loop_stack.size(); j > 0; j--) {
                    if (loop_stack[j - 1].name == tname) { k = j - 1; break; }
                }
                if (k == loop_stack.size()) unsupported("this cycle is", loc);
                if (k + 1 == loop_stack.size()) {
                    line("continue");
                } else {
                    for (size_t j = k + 1; j < loop_stack.size(); j++) {
                        if (!loop_stack[j].brk.empty()) line(loop_stack[j].brk + " := true");
                    }
                    line(loop_stack[k].named_cyc + " := true");
                    line("break");
                }
                break;
            }
            case ASR::stmtType::Where: {
                auto &x = *ASR::down_cast<ASR::Where_t>(s);
                std::string mv = "wm'" + std::to_string(++loop_id);
                line("let " + mv + " : Arr Bool := " + lower(x.m_test).src);
                emit_where_body(x.m_body, x.n_body, mv, loc);
                if (x.n_orelse > 0) {
                    std::string ev = "wm'" + std::to_string(++loop_id);
                    line("let " + ev + " : Arr Bool := Arr.map (fun b' => !b') " + mv);
                    emit_where_body(x.m_orelse, x.n_orelse, ev, loc);
                }
                break;
            }
            case ASR::stmtType::ForAllSingle: {
                std::vector<ASR::do_loop_head_t*> heads;
                ASR::stmt_t *inner = s;
                while (ASR::is_a<ASR::ForAllSingle_t>(*inner)) {
                    auto &fa = *ASR::down_cast<ASR::ForAllSingle_t>(inner);
                    heads.push_back(&fa.m_head);
                    inner = fa.m_assign_stmt;
                }
                ASR::expr_t *mask_test = nullptr;
                ASR::Assignment_t *asgn = nullptr;
                if (ASR::is_a<ASR::Assignment_t>(*inner)) {
                    asgn = ASR::down_cast<ASR::Assignment_t>(inner);
                } else if (ASR::is_a<ASR::If_t>(*inner)) {
                    auto &if_s = *ASR::down_cast<ASR::If_t>(inner);
                    if (if_s.n_body == 1 && if_s.n_orelse == 0
                            && ASR::is_a<ASR::Assignment_t>(*if_s.m_body[0])) {
                        mask_test = if_s.m_test;
                        asgn = ASR::down_cast<ASR::Assignment_t>(if_s.m_body[0]);
                    }
                }
                if (!asgn || asgn->m_overloaded || !is_lvalue(asgn->m_target)
                        || is_array(ASRUtils::expr_type(asgn->m_target))
                        || is_allocatable(ASRUtils::expr_type(asgn->m_target))) {
                    unsupported("this forall statement is", loc);
                }
                ASR::Variable_t *tv = ProcEffects::root_var(asgn->m_target);
                if (!tv) unsupported("this forall statement is", loc);
                // Snapshot target root variable before the loop for exact forall semantics.
                std::string snap = "fa'" + std::to_string(++loop_id);
                if (!var_alias.count(tv) && global_fields.count(tv)) {
                    line("let " + snap + " := (← get)." + global_fields[tv]);
                } else {
                    line("let " + snap + " := " + var_name(tv));
                }
                for (size_t i = 0; i < heads.size(); i++) {
                    const ASR::do_loop_head_t &h = *heads[i];
                    if (!h.m_v || !ASR::is_a<ASR::Var_t>(*h.m_v) || !h.m_start || !h.m_end) {
                        unsupported("this forall statement is", loc);
                    }
                }
                auto tv_it = var_alias.find(tv);
                bool had_tv = tv_it != var_alias.end();
                std::string prev_tv = had_tv ? tv_it->second : "";
                auto set_tv_snap = [&]() { var_alias[tv] = snap; };
                auto restore_tv = [&]() {
                    if (had_tv) var_alias[tv] = prev_tv;
                    else var_alias.erase(tv);
                };
                set_tv_snap();
                std::vector<std::pair<ASR::Variable_t*, std::pair<bool, std::string>>> saved_idx;
                std::function<void(size_t)> emit_fa = [&](size_t idx) {
                    if (idx == heads.size()) {
                        if (mask_test) {
                            line("if " + lower(mask_test).src + " then");
                            ind++;
                        }
                        hoist_lvalue_indices(asgn->m_target);
                        std::string rv = "frhs'" + std::to_string(++loop_id);
                        std::string rhs = fix_string(ASRUtils::expr_type(asgn->m_target), "",
                            lower(asgn->m_value), loc);
                        line("let " + rv + " := " + rhs);
                        restore_tv();
                        update_lvalue(asgn->m_target, [&](const std::string &) { return rv; }, loc);
                        set_tv_snap();
                        pre_idx.clear();
                        if (mask_test) ind--;
                        return;
                    }
                    const ASR::do_loop_head_t &h = *heads[idx];
                    bool unit = !h.m_increment
                        || (ASR::is_a<ASR::IntegerConstant_t>(*h.m_increment)
                            && ASR::down_cast<ASR::IntegerConstant_t>(h.m_increment)->m_n == 1);
                    std::string ty = type(ASRUtils::expr_type(h.m_v), loc);
                    std::string bid = std::to_string(++loop_id);
                    std::string lv = "flo'" + bid, hv = "fhi'" + bid;
                    std::string sv = unit ? "" : "fst'" + bid;
                    std::string alias = "fi'" + bid;
                    ASR::Variable_t *iv = ASRUtils::EXPR2VAR(h.m_v);
                    line("let " + lv + " : " + ty + " := " + lower(h.m_start).src);
                    line("let " + hv + " : " + ty + " := " + lower(h.m_end).src);
                    if (!unit) {
                        line("let " + sv + " : " + ty + " := " + lower(h.m_increment).src);
                    }
                    if (unit) {
                        line("for " + alias + " in FInt.range " + lv + " " + hv + " do");
                    } else {
                        line("for " + alias + " in FInt.rangeStep " + lv + " " + hv
                            + " " + sv + " do");
                    }
                    ind++;
                    auto it = var_alias.find(iv);
                    saved_idx.push_back({iv, it != var_alias.end()
                        ? std::make_pair(true, it->second) : std::make_pair(false, std::string())});
                    var_alias[iv] = alias;
                    emit_fa(idx + 1);
                    auto last = saved_idx.back();
                    saved_idx.pop_back();
                    if (last.second.first) var_alias[last.first] = last.second.second;
                    else var_alias.erase(last.first);
                    ind--;
                };
                emit_fa(0);
                restore_tv();
                break;
            }
            case ASR::stmtType::Select: {
                auto &x = *ASR::down_cast<ASR::Select_t>(s);
                bool sel_int = is_int(x.m_test);
                bool sel_str = is_string(x.m_test);
                bool sel_log = is_logical(x.m_test);
                if (x.m_enable_fall_through || !(sel_int || sel_str || sel_log)) {
                    unsupported("this select case is", loc);
                }
                std::string id = std::to_string(++loop_id);
                std::string sv = "sel'" + id;
                if (sel_int) {
                    line("let " + sv + " : Int := " + int_expr(x.m_test).src);
                } else if (sel_str) {
                    line("let " + sv + " : String := " + lower(x.m_test).src);
                } else {
                    line("let " + sv + " : Bool := " + lower(x.m_test).src);
                }
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
                            if (sel_int) {
                                cond += "(" + sv + " == " + atom(int_expr(cs.m_test[j])) + ")";
                            } else if (sel_str) {
                                cond += "FStr.beq " + sv + " " + atom(lower(cs.m_test[j]));
                            } else {
                                cond += "(" + sv + " == " + atom(lower(cs.m_test[j])) + ")";
                            }
                        }
                        cbody = cs.m_body;
                        n_cbody = cs.n_body;
                    } else if (ASR::is_a<ASR::CaseStmt_Range_t>(*c)) {
                        auto &cs = *ASR::down_cast<ASR::CaseStmt_Range_t>(c);
                        if (sel_log || (!cs.m_start && !cs.m_end)) {
                            unsupported("this select case is", loc);
                        }
                        if (sel_int) {
                            if (cs.m_start && cs.m_end) {
                                cond = "decide (" + atom(int_expr(cs.m_start)) + " ≤ " + sv
                                    + ") && decide (" + sv + " ≤ " + atom(int_expr(cs.m_end)) + ")";
                            } else if (cs.m_start) {
                                cond = "decide (" + atom(int_expr(cs.m_start)) + " ≤ " + sv + ")";
                            } else {
                                cond = "decide (" + sv + " ≤ " + atom(int_expr(cs.m_end)) + ")";
                            }
                        } else {
                            if (cs.m_start && cs.m_end) {
                                cond = "FStr.le " + atom(lower(cs.m_start)) + " " + sv
                                    + " && FStr.le " + sv + " " + atom(lower(cs.m_end));
                            } else if (cs.m_start) {
                                cond = "FStr.le " + atom(lower(cs.m_start)) + " " + sv;
                            } else {
                                cond = "FStr.le " + sv + " " + atom(lower(cs.m_end));
                            }
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
                    line("IO.println " + lower_string_format(f, "\" \"", loc));
                    break;
                }
                if (is_string(x.m_text)) {
                    line("IO.println " + atom(lower(x.m_text)));
                    break;
                }
                unsupported("this print is", loc);
            }
            case ASR::stmtType::FileWrite: {
                auto &x = *ASR::down_cast<ASR::FileWrite_t>(s);
                if (x.m_overloaded || x.m_nml || x.m_rec || x.m_pos || x.m_id
                        || x.m_asynchronous || x.m_decimal || x.m_delim || !x.m_is_formatted) {
                    unsupported("this file write is", loc);
                }
                if (x.m_iomsg && (!is_lvalue(x.m_iomsg) || !is_string(x.m_iomsg))) {
                    unsupported("this file write is", loc);
                }
                bool is_str_unit = x.m_unit && is_string(x.m_unit) && is_lvalue(x.m_unit);
                bool is_stdout = !x.m_unit;
                if (!is_stdout && !is_str_unit && x.m_unit && is_int(x.m_unit)) {
                    ASR::expr_t *uv = ASRUtils::expr_value(x.m_unit);
                    if (uv && ASR::is_a<ASR::IntegerConstant_t>(*uv)
                            && ASR::down_cast<ASR::IntegerConstant_t>(uv)->m_n == 6) {
                        is_stdout = true;
                    }
                }
                if (!is_stdout && !is_str_unit) unsupported("this file write is", loc);
                if (is_stdout && !in_program && !(cur && io_procs.count(cur->fn))) {
                    unsupported("write in a procedure is", loc);
                }
                if (x.m_iostat && (!is_lvalue(x.m_iostat) || !is_int(x.m_iostat))) {
                    unsupported("this file write is", loc);
                }
                if (is_stdout) level = std::max(level, 2);
                std::string sep = x.m_separator ? atom(lower(x.m_separator)) : "\" \"";
                std::string text;
                if (x.n_values == 0) {
                    text = "\"\"";
                } else if (x.n_values == 1 && ASR::is_a<ASR::StringFormat_t>(*x.m_values[0])) {
                    text = lower_string_format(
                        *ASR::down_cast<ASR::StringFormat_t>(x.m_values[0]), sep, loc);
                } else if (x.n_values == 1 && ASR::is_a<ASR::StringConstant_t>(*x.m_values[0])) {
                    text = string_lit(string_const_str(
                        *ASR::down_cast<ASR::StringConstant_t>(x.m_values[0])));
                } else if (x.n_values == 1 && is_string(x.m_values[0])) {
                    text = atom(lower(x.m_values[0]));
                } else {
                    std::string items;
                    for (size_t i = 0; i < x.n_values; i++) {
                        if (i) items += ", ";
                        if (ASR::is_a<ASR::StringFormat_t>(*x.m_values[i])) {
                            items += lower_string_format(
                                *ASR::down_cast<ASR::StringFormat_t>(x.m_values[i]), sep, loc);
                        } else {
                            items += shown(x.m_values[i]);
                        }
                    }
                    text = "(String.intercalate " + sep + " [" + items + "])";
                }
                if (is_stdout) {
                    if (x.m_end) {
                        line("IO.print (" + text + " ++ " + atom(lower(x.m_end)) + ")");
                    } else {
                        line("IO.println " + text);
                    }
                } else {
                    std::string rv = "wr'" + std::to_string(++loop_id);
                    line("let " + rv + " : String := " + text);
                    ASR::ttype_t *it = unalloc(ASRUtils::expr_type(x.m_unit));
                    ASR::expr_t *le = ASR::down_cast<ASR::String_t>(it)->m_len;
                    update_lvalue(x.m_unit, [&](const std::string &cur_s) {
                        if (le) return "FStr.fix " + extent(le) + " " + rv;
                        return "FStr.fix " + cur_s + ".length " + rv;
                    }, loc);
                }
                if (x.m_iostat) {
                    std::string ity = type(unalloc(ASRUtils::expr_type(x.m_iostat)), loc);
                    update_lvalue(x.m_iostat, [&](const std::string &) {
                        return "(FInt.ofInt 0 : " + ity + ")";
                    }, loc);
                }
                break;
            }
            case ASR::stmtType::FileRead: {
                auto &x = *ASR::down_cast<ASR::FileRead_t>(s);
                if (!x.m_unit || !is_string(x.m_unit) || x.m_fmt || !x.m_is_formatted
                        || x.m_overloaded || x.m_nml || x.m_rec || x.m_pos || x.m_id
                        || x.m_advance || x.m_size || x.m_pad || x.m_decimal) {
                    unsupported("this file read is", loc);
                }
                if (x.m_iomsg && (!is_lvalue(x.m_iomsg) || !is_string(x.m_iomsg))) {
                    unsupported("this file read is", loc);
                }
                if (x.m_iostat && (!is_lvalue(x.m_iostat) || !is_int(x.m_iostat))) {
                    unsupported("this file read is", loc);
                }
                std::string rid = std::to_string(++loop_id);
                std::string toks = "rtok'" + rid;
                std::string pos = "rpos'" + rid;
                std::string stv = "rstat'" + rid;
                line("let " + toks + " : Array String := FRead.tokenize " + atom(lower(x.m_unit)));
                line("let mut " + pos + " : Nat := 0");
                line("let mut " + stv + " : Int := 0");
                auto parse_scalar = [&](ASR::ttype_t *et, const std::string &tok_var,
                        const Location &vloc) -> std::string {
                    et = scalar(et, vloc);
                    if (ASR::is_a<ASR::Integer_t>(*et)) {
                        return "(FRead.parseInt " + tok_var + ").map (fun n' => (FInt.ofInt n' : "
                            + type(et, vloc) + "))";
                    }
                    if (ASR::is_a<ASR::Real_t>(*et)) {
                        level = std::max(level, 2);
                        int k = ASR::down_cast<ASR::Real_t>(et)->m_kind;
                        return "(FRead.parseFloat " + tok_var + ").map FKindsIO.ofFloatR"
                            + std::to_string(k);
                    }
                    if (ASR::is_a<ASR::Complex_t>(*et)) {
                        level = std::max(level, 2);
                        int k = ASR::down_cast<ASR::Complex_t>(et)->m_kind;
                        std::string fn = "FKindsIO.ofFloatR" + std::to_string(k);
                        return "(FRead.parseComplex " + tok_var
                            + ").map (fun p' => FCplx.mk (" + fn + " p'.1) (" + fn + " p'.2))";
                    }
                    if (ASR::is_a<ASR::Logical_t>(*et)) {
                        return "(FRead.parseBool " + tok_var + ")";
                    }
                    if (ASR::is_a<ASR::String_t>(*et)) {
                        ASR::expr_t *le = ASR::down_cast<ASR::String_t>(et)->m_len;
                        if (le) return "(some (FStr.fix " + extent(le) + " " + tok_var + "))";
                        return "(some " + tok_var + ")";
                    }
                    unsupported("reading this type is", vloc);
                };
                for (size_t i = 0; i < x.n_values; i++) {
                    ASR::expr_t *val = unwrap_physical_cast(x.m_values[i]);
                    const Location &vloc = val->base.loc;
                    if (!is_lvalue(val)) unsupported("this file read is", vloc);
                    ASR::ttype_t *vt = unalloc(ASRUtils::expr_type(val));
                    if (is_array(vt)) {
                        ASR::ttype_t *et = elem_type(vt);
                        if (ASR::is_a<ASR::String_t>(*et)
                                && !ASR::down_cast<ASR::String_t>(et)->m_len) {
                            unsupported("reading this type is", vloc);
                        }
                        std::string aid = std::to_string(++loop_id);
                        std::string av = "rarr'" + aid;
                        std::string dv = "rdat'" + aid;
                        std::string iv = "ri'" + aid;
                        line("let " + av + " := " + lower(val).src);
                        line("let mut " + dv + " := " + av + ".data");
                        line("for " + iv + " in [:" + dv + ".size] do");
                        ind++;
                        line("if " + stv + " == 0 then");
                        ind++;
                        line("if " + pos + " >= " + toks + ".size then");
                        ind++;
                        line(stv + " := -1");
                        ind--;
                        line("else");
                        ind++;
                        std::string tv = "rt'" + std::to_string(++loop_id);
                        line("let " + tv + " := " + toks + "[" + pos + "]!");
                        line(pos + " := " + pos + " + 1");
                        line("match " + parse_scalar(et, tv, vloc) + " with");
                        line("| some v' => " + dv + " := " + dv + ".set! " + iv + " v'");
                        line("| none => " + stv + " := 1");
                        ind--;
                        ind--;
                        ind--;
                        update_lvalue(val, [&](const std::string &cur_a) {
                            return "{ " + cur_a + " with data := " + dv + " }";
                        }, vloc);
                    } else {
                        line("if " + stv + " == 0 then");
                        ind++;
                        line("if " + pos + " >= " + toks + ".size then");
                        ind++;
                        line(stv + " := -1");
                        ind--;
                        line("else");
                        ind++;
                        std::string tv = "rt'" + std::to_string(++loop_id);
                        line("let " + tv + " := " + toks + "[" + pos + "]!");
                        line(pos + " := " + pos + " + 1");
                        if (ASR::is_a<ASR::String_t>(*vt)) {
                            ASR::expr_t *le = ASR::down_cast<ASR::String_t>(vt)->m_len;
                            update_lvalue(val, [&](const std::string &cur_s) {
                                if (le) return "FStr.fix " + extent(le) + " " + tv;
                                return "FStr.fix " + cur_s + ".length " + tv;
                            }, vloc);
                        } else {
                            line("match " + parse_scalar(vt, tv, vloc) + " with");
                            line("| some v' =>");
                            ind++;
                            update_lvalue(val, [&](const std::string &) { return "v'"; }, vloc);
                            ind--;
                            line("| none => " + stv + " := 1");
                        }
                        ind--;
                        ind--;
                    }
                }
                if (x.m_iostat) {
                    std::string ity = type(unalloc(ASRUtils::expr_type(x.m_iostat)), loc);
                    update_lvalue(x.m_iostat, [&](const std::string &) {
                        return "(FInt.ofInt " + stv + " : " + ity + ")";
                    }, loc);
                }
                break;
            }
            case ASR::stmtType::Allocate: {
                auto &x = *ASR::down_cast<ASR::Allocate_t>(s);
                if (x.m_errmsg) unsupported("allocate with stat/errmsg is", loc);
                std::string stat_ty;
                bool guarded = false;
                if (x.m_stat) {
                    if (!is_lvalue(x.m_stat) || !is_int(x.m_stat)) {
                        unsupported("allocate with stat/errmsg is", loc);
                    }
                    stat_ty = type(unalloc(ASRUtils::expr_type(x.m_stat)), loc);
                    std::string already;
                    for (size_t i = 0; i < x.n_args; i++) {
                        if (!is_option_expr(x.m_args[i].m_a) || !is_lvalue(x.m_args[i].m_a)) {
                            unsupported("this allocate is", loc);
                        }
                        if (i) already += " || ";
                        already += "(" + atom(lower_raw(x.m_args[i].m_a)) + ".isSome)";
                    }
                    if (!already.empty()) {
                        guarded = true;
                        line("if " + already + " then");
                        ind++;
                        update_lvalue(x.m_stat, [&](const std::string &) {
                            return "(FInt.ofInt 1 : " + stat_ty + ")";
                        }, loc);
                        ind--;
                        line("else");
                        ind++;
                    }
                }
                std::string src_var;
                bool src_is_arr = false;
                if (x.m_source) {
                    src_is_arr = is_array(ASRUtils::expr_type(x.m_source));
                    src_var = "src'" + std::to_string(++loop_id);
                    line("let " + src_var + " := " + lower(x.m_source).src);
                }
                for (size_t i = 0; i < x.n_args; i++) {
                    alloc_one(x.m_args[i], src_var, src_is_arr, loc);
                }
                if (x.m_stat) {
                    update_lvalue(x.m_stat, [&](const std::string &) {
                        return "(FInt.ofInt 0 : " + stat_ty + ")";
                    }, loc);
                    if (guarded) ind--;
                }
                break;
            }
            case ASR::stmtType::ReAlloc: {
                auto &x = *ASR::down_cast<ASR::ReAlloc_t>(s);
                if (x.n_args != 1) unsupported("this realloc is", loc);
                const ASR::alloc_arg_t &aa = x.m_args[0];
                if (!is_option_expr(aa.m_a) || !is_lvalue(aa.m_a)) {
                    unsupported("this realloc is", loc);
                }
                line("if !(" + atom(lower_raw(aa.m_a)) + ".isSome) then");
                ind++;
                alloc_one(aa, "", false, loc);
                ind--;
                line("else if " + lower(x.m_shape_differs).src + " then");
                ind++;
                alloc_one(aa, "", false, loc);
                ind--;
                break;
            }
            case ASR::stmtType::ExplicitDeallocate: {
                auto &x = *ASR::down_cast<ASR::ExplicitDeallocate_t>(s);
                if (x.m_errmsg) unsupported("deallocate with stat/errmsg is", loc);
                std::string stat_ty;
                bool guarded = false;
                if (x.m_stat) {
                    if (!is_lvalue(x.m_stat) || !is_int(x.m_stat)) {
                        unsupported("deallocate with stat/errmsg is", loc);
                    }
                    stat_ty = type(unalloc(ASRUtils::expr_type(x.m_stat)), loc);
                    std::string not_alloc;
                    for (size_t i = 0; i < x.n_vars; i++) {
                        if (!is_option_expr(x.m_vars[i]) || !is_lvalue(x.m_vars[i])) {
                            unsupported("this deallocate is", loc);
                        }
                        if (i) not_alloc += " || ";
                        not_alloc += "!(" + atom(lower_raw(x.m_vars[i])) + ".isSome)";
                    }
                    if (!not_alloc.empty()) {
                        guarded = true;
                        line("if " + not_alloc + " then");
                        ind++;
                        update_lvalue(x.m_stat, [&](const std::string &) {
                            return "(FInt.ofInt 1 : " + stat_ty + ")";
                        }, loc);
                        ind--;
                        line("else");
                        ind++;
                    }
                }
                for (size_t i = 0; i < x.n_vars; i++) {
                    if (!is_option_expr(x.m_vars[i]) || !is_lvalue(x.m_vars[i])) {
                        unsupported("this deallocate is", loc);
                    }
                    update_lvalue(x.m_vars[i], [&](const std::string &) { return "none"; },
                        loc, /*raw=*/true);
                }
                if (x.m_stat) {
                    update_lvalue(x.m_stat, [&](const std::string &) {
                        return "(FInt.ofInt 0 : " + stat_ty + ")";
                    }, loc);
                    if (guarded) ind--;
                }
                break;
            }
            case ASR::stmtType::ImplicitDeallocate: {
                auto &x = *ASR::down_cast<ASR::ImplicitDeallocate_t>(s);
                for (size_t i = 0; i < x.n_vars; i++) {
                    if (is_option_expr(x.m_vars[i]) && is_lvalue(x.m_vars[i])) {
                        update_lvalue(x.m_vars[i], [&](const std::string &) { return "none"; },
                            loc, /*raw=*/true);
                    }
                }
                break;
            }
            case ASR::stmtType::BlockCall: {
                auto &x = *ASR::down_cast<ASR::BlockCall_t>(s);
                ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(x.m_m);
                if (!ASR::is_a<ASR::Block_t>(*sym)) unsupported("this block is", loc);
                ASR::Block_t *b = ASR::down_cast<ASR::Block_t>(sym);
                check_symbols(b->m_symtab);
                line("if true then");
                ind++;
                SymbolTable *prev_scope = scope;
                scope_stack.push_back(scope);
                scope = b->m_symtab;
                std::string bid = std::to_string(++loop_id);
                std::vector<ASR::Variable_t*> aliased;
                for (ASR::symbol_t *bs : symbols_in_order(b->m_symtab)) {
                    if (ASR::is_a<ASR::Variable_t>(*bs)) {
                        ASR::Variable_t *bv = ASR::down_cast<ASR::Variable_t>(bs);
                        if (bv->m_storage != ASR::storage_typeType::Parameter) {
                            name(bv->m_name, bv->base.base.loc);
                            var_alias[bv] = std::string(bv->m_name) + "'b" + bid;
                            aliased.push_back(bv);
                        }
                    }
                }
                size_t before = buf->size();
                locals(b->m_symtab, {});
                stmts(b->m_body, b->n_body);
                if (buf->size() == before) line("Pure.pure ()");
                for (ASR::Variable_t *bv : aliased) var_alias.erase(bv);
                scope = prev_scope;
                scope_stack.pop_back();
                ind--;
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

    // The `do` variables emitted as the bound name of `for`: those used only
    // inside the loops they index.
    void collect_simple_loop_vars(ASR::stmt_t **body, size_t n) {
        collect_loop_vars(body, n);
        LoopVarUses u;
        for (size_t i = 0; i < n; i++) u.visit_stmt(*body[i]);
        for (ASR::Variable_t *v : u.outside) loop_vars.erase(v);
        for (const auto &kv : global_fields) loop_vars.erase(kv.first);
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
            } else if (ASR::is_a<ASR::DoConcurrentLoop_t>(*s)) {
                auto &d = *ASR::down_cast<ASR::DoConcurrentLoop_t>(s);
                for (size_t j = 0; j < d.n_head; j++) {
                    if (d.m_head[j].m_v && ASR::is_a<ASR::Var_t>(*d.m_head[j].m_v)) {
                        loop_vars.insert(ASRUtils::EXPR2VAR(d.m_head[j].m_v));
                    }
                }
                collect_loop_vars(d.m_body, d.n_body);
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
            } else if (ASR::is_a<ASR::BlockCall_t>(*s)) {
                ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                if (ASR::is_a<ASR::Block_t>(*sym)) {
                    ASR::Block_t *b = ASR::down_cast<ASR::Block_t>(sym);
                    collect_loop_vars(b->m_body, b->n_body);
                }
            }
        }
    }

    // Emit one local variable declaration into `*buf`.
    void emit_local(ASR::Variable_t *v) {
        const Location &vloc = v->base.base.loc;
        if ((!in_program || !scope_stack.empty()) && v->m_storage == ASR::storage_typeType::Save) {
            unsupported("a saved local variable is", vloc);
        }
        std::string vn = var_name(v);
        if (is_allocatable(v->m_type)) {
            if (v->m_symbolic_value || v->m_value) {
                unsupported("an initialized allocatable variable is", vloc);
            }
            ASR::ttype_t *it = unalloc(v->m_type);
            if (ASR::is_a<ASR::String_t>(*it)) {
                ASR::expr_t *le = ASR::down_cast<ASR::String_t>(it)->m_len;
                if (le && !ASRUtils::expr_value(le)) {
                    std::string lv = "len'" + std::to_string(++loop_id);
                    line("let " + lv + " : Nat := " + extent(le));
                    alloc_str_len[v] = lv;
                }
            }
            line("let mut " + vn + " : "
                + vtype(v->m_type, vloc, v->m_type_declaration) + " := none");
            return;
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
                ASR::ttype_t *et = elem_type(v->m_type);
                ASR::expr_t *iv = unwrap_physical_cast(init);
                if (ASR::is_a<ASR::ArrayBroadcast_t>(*iv)) {
                    iv = ASR::down_cast<ASR::ArrayBroadcast_t>(iv)->m_array;
                }
                if (is_array(ASRUtils::expr_type(iv))) {
                    std::string src = atom(lower(iv));
                    if (ASR::is_a<ASR::String_t>(*et)) {
                        auto &st = *ASR::down_cast<ASR::String_t>(et);
                        if (!st.m_len) unsupported("this local array is", vloc);
                        src = "(Arr.map (FStr.fix " + extent(st.m_len) + ") " + src + ")";
                    }
                    line("let mut " + vn + " : "
                        + vtype(v->m_type, vloc, v->m_type_declaration)
                        + " := Arr.reshape " + src + " "
                        + int_array(lbs) + " " + int_array(exts));
                } else {
                    if (ASR::is_a<ASR::String_t>(*et) && !ASR::down_cast<ASR::String_t>(et)->m_len) {
                        unsupported("this local array is", vloc);
                    }
                    std::string elem = fix_string(et, "", lower(iv), vloc);
                    line("let mut " + vn + " : "
                        + vtype(v->m_type, vloc, v->m_type_declaration)
                        + " := Arr.make " + int_array(lbs) + " " + int_array(exts) + " "
                        + atom({elem, pApp}));
                }
                return;
            }
            line("let mut " + vn + " : "
                + type(v->m_type, vloc, v->m_type_declaration)
                + " := " + fix_string(v->m_type, "", lower(init), vloc));
            return;
        }
        if (is_array(v->m_type)) {
            std::vector<std::string> lbs, exts;
            bool assumed;
            dims(v->m_type, lbs, exts, assumed, vloc);
            if (assumed) unsupported("this local array is", vloc);
            line("let mut " + vn + " : "
                + vtype(v->m_type, vloc, v->m_type_declaration)
                + " := Arr.make " + int_array(lbs) + " " + int_array(exts) + " "
                + atom({zero(ASR::down_cast<ASR::Array_t>(v->m_type)->m_type, vloc,
                    v->m_type_declaration), pApp}));
            return;
        }
        line("let mut " + vn + " : "
            + type(v->m_type, vloc, v->m_type_declaration)
            + " := " + zero(v->m_type, vloc, v->m_type_declaration));
    }

    // Locals of a procedure or program body: every variable that is not a
    // dummy, a parameter, a global/saved variable or a loop index.
    void locals(SymbolTable *st, const std::set<ASR::Variable_t*> &dummies) {
        for (ASR::symbol_t *s : symbols_in_order(st)) {
            if (!ASR::is_a<ASR::Variable_t>(*s)) continue;
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
            if (dummies.count(v) || loop_vars.count(v) || global_fields.count(v)) continue;
            if (v->m_storage == ASR::storage_typeType::Parameter) continue;
            emit_local(v);
        }
    }

    void check_symbols(SymbolTable *st, bool allow_functions = false) {
        for (ASR::symbol_t *s : symbols_in_order(st)) {
            if (ASR::is_a<ASR::Variable_t>(*s) || ASR::is_a<ASR::ExternalSymbol_t>(*s)
                    || ASR::is_a<ASR::Struct_t>(*s)
                    || ASR::is_a<ASR::Enum_t>(*s)
                    || ASR::is_a<ASR::GenericProcedure_t>(*s)
                    || ASR::is_a<ASR::CustomOperator_t>(*s)
                    || ASR::is_a<ASR::Block_t>(*s)
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
        bool is_gl = global_procs.count(fn) > 0;
        level = is_io ? 2 : 0;
        loop_vars.clear();
        alloc_str_len.clear();
        scope = fn->m_symtab;
        in_program = false;
        check_symbols(fn->m_symtab, /*allow_functions=*/true);
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
            std::string let = (in == ASR::intentType::In && !a->m_value_attr) ? "let " : "let mut ";
            if (a->m_presence == ASR::presenceType::Optional && is_allocatable(a->m_type)) {
                unsupported("an optional allocatable dummy argument is", aloc);
            }
            if (is_optional_dummy(a)) {
                std::string inner = vtype(a->m_type, aloc, a->m_type_declaration);
                std::string oty = "Option " + (inner.find(' ') != std::string::npos
                    ? "(" + inner + ")" : inner);
                p.sig += " (" + an + " : " + oty + ")";
                if (is_array(a->m_type)) {
                    std::vector<std::string> lbs, exts;
                    bool assumed;
                    dims(a->m_type, lbs, exts, assumed, aloc);
                    std::string init_arr = assumed
                        ? "Arr.rebase a' " + int_array(lbs)
                        : "Arr.reshape a' " + int_array(lbs) + " " + int_array(exts);
                    ASR::ttype_t *et = ASR::down_cast<ASR::Array_t>(a->m_type)->m_type;
                    if (in == ASR::intentType::Out && ASR::is_a<ASR::StructType_t>(*et)) {
                        init_arr = "Arr.fill (" + init_arr + ") ("
                            + zero(et, aloc, a->m_type_declaration) + ")";
                    }
                    entry.push_back({aloc.first, let + an + " := " + an + ".map (fun a' => "
                        + init_arr + ")"});
                } else if (in == ASR::intentType::Out) {
                    if (is_assumed_len_string(a->m_type)) {
                        entry.push_back({aloc.first, "let mut " + an + " := " + an
                            + ".map (fun s' => FStr.fix s'.length \"\")"});
                    } else {
                        entry.push_back({aloc.first, "let mut " + an + " := " + an
                            + ".map (fun _ => " + zero(a->m_type, aloc, a->m_type_declaration) + ")"});
                    }
                } else if (ASR::is_a<ASR::String_t>(*a->m_type)
                        && ASR::down_cast<ASR::String_t>(a->m_type)->m_len) {
                    entry.push_back({aloc.first, let + an + " := " + an
                        + ".map (fun s' => FStr.fix "
                        + extent(ASR::down_cast<ASR::String_t>(a->m_type)->m_len) + " s')"});
                } else if (in == ASR::intentType::InOut || a->m_value_attr) {
                    entry.push_back({aloc.first, "let mut " + an + " := " + an});
                }
                if (in != ASR::intentType::In) {
                    outs.push_back(an);
                    out_types.push_back(oty);
                }
                continue;
            }
            if (is_allocatable(a->m_type)) {
                ASR::ttype_t *et = elem_type(a->m_type);
                if (ASR::is_a<ASR::String_t>(*et) && !ASR::down_cast<ASR::String_t>(et)->m_len
                        && ASR::down_cast<ASR::String_t>(et)->m_len_kind
                            == ASR::string_length_kindType::AssumedLength) {
                    unsupported("an assumed-length allocatable character dummy is", aloc);
                }
                if (in == ASR::intentType::Out) {
                    entry.push_back({aloc.first, "let mut " + an + " : "
                        + vtype(a->m_type, aloc, a->m_type_declaration) + " := none"});
                } else {
                    p.sig += " (" + an + " : " + vtype(a->m_type, aloc, a->m_type_declaration) + ")";
                    if (in == ASR::intentType::InOut || a->m_value_attr) {
                        entry.push_back({aloc.first, "let mut " + an + " := " + an});
                    }
                }
            } else if (is_array(a->m_type)) {
                // The dummy takes its declared bounds.
                std::vector<std::string> lbs, exts;
                bool assumed;
                dims(a->m_type, lbs, exts, assumed, aloc);
                p.sig += " (" + an + " : " + vtype(a->m_type, aloc, a->m_type_declaration) + ")";
                std::string init_arr = assumed
                    ? "Arr.rebase " + an + " " + int_array(lbs)
                    : "Arr.reshape " + an + " " + int_array(lbs) + " " + int_array(exts);
                ASR::ttype_t *et = ASR::down_cast<ASR::Array_t>(a->m_type)->m_type;
                if (in == ASR::intentType::Out && ASR::is_a<ASR::StructType_t>(*et)) {
                    init_arr = "Arr.fill (" + init_arr + ") ("
                        + zero(et, aloc, a->m_type_declaration) + ")";
                }
                entry.push_back({aloc.first, let + an + " := " + init_arr});
            } else if (in == ASR::intentType::Out) {
                if (is_assumed_len_string(a->m_type)) {
                    p.sig += " (" + an + " : String)";
                    entry.push_back({aloc.first, "let mut " + an + " := FStr.fix " + an + ".length \"\""});
                } else {
                    entry.push_back({aloc.first, "let mut " + an + " : "
                        + type(a->m_type, aloc, a->m_type_declaration) + " := "
                        + zero(a->m_type, aloc, a->m_type_declaration)});
                }
            } else {
                p.sig += " (" + an + " : " + type(a->m_type, aloc, a->m_type_declaration) + ")";
                if (ASR::is_a<ASR::String_t>(*a->m_type) && ASR::down_cast<ASR::String_t>(a->m_type)->m_len) {
                    entry.push_back({aloc.first, let + an + " := FStr.fix "
                        + extent(ASR::down_cast<ASR::String_t>(a->m_type)->m_len) + " " + an});
                } else if (in == ASR::intentType::InOut || a->m_value_attr) {
                    entry.push_back({aloc.first, "let mut " + an + " := " + an});
                }
            }
            if (in != ASR::intentType::In) {
                outs.push_back(an);
                out_types.push_back(vtype(a->m_type, aloc, a->m_type_declaration));
            }
        }
        std::stable_sort(entry.begin(), entry.end(),
            [](const auto &a, const auto &b) { return a.first < b.first; });
        auto ret_sig = [&](const std::string &rty, bool is_unit) -> std::string {
            if (is_gl && is_io) {
                return is_unit ? " : StateT (Globals' K) IO Unit"
                    : " : StateT (Globals' K) IO (" + rty + ")";
            }
            if (is_gl) {
                return is_unit ? " : StateM (Globals' K) Unit"
                    : " : StateM (Globals' K) (" + rty + ")";
            }
            if (is_io) {
                return is_unit ? " : IO Unit" : " : IO (" + rty + ")";
            }
            return " : " + rty;
        };
        if (result) {
            p.ret = name(result->m_name, result->base.base.loc);
            std::string rty = vtype(result->m_type, result->base.base.loc,
                result->m_type_declaration);
            p.sig += ret_sig(rty, false);
        } else if (outs.empty()) {
            p.ret = "()";
            p.sig += ret_sig("Unit", true);
        } else {
            std::string ret, ty;
            for (size_t i = 0; i < outs.size(); i++) {
                ret += (i ? ", " : "") + outs[i];
                ty += (i ? " × " : "") + out_types[i];
            }
            p.ret = outs.size() == 1 ? ret : "(" + ret + ")";
            p.sig += ret_sig(ty, false);
        }
        collect_simple_loop_vars(fn->m_body, fn->n_body);
        for (ASR::Variable_t *d : dummies) loop_vars.erase(d);
        if (result) loop_vars.erase(result);
        buf = &p.body;
        ind = 1;
        // Emit dummy `entry` rebinds and local variable declarations in source
        // declaration order so a dummy's dimension can reference an earlier local.
        std::vector<ASR::Variable_t*> local_vars;
        for (ASR::symbol_t *s : symbols_in_order(fn->m_symtab)) {
            if (!ASR::is_a<ASR::Variable_t>(*s)) continue;
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
            if (dummies.count(v) || loop_vars.count(v) || global_fields.count(v)) continue;
            if (v->m_storage == ASR::storage_typeType::Parameter) continue;
            local_vars.push_back(v);
        }
        size_t ei = 0, li = 0;
        while (ei < entry.size() || li < local_vars.size()) {
            if (ei < entry.size() && (li == local_vars.size()
                    || local_vars[li] == result
                    || entry[ei].first <= local_vars[li]->base.base.loc.first)) {
                line(entry[ei++].second);
            } else {
                emit_local(local_vars[li++]);
            }
        }
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
        std::string rhs = (io_procs.count(p.fn) || global_procs.count(p.fn))
            ? " := do\n" : " := Id.run do\n";
        return kw + short_name + " {K : Kinds} [" + level_class(p.level) + " K]"
            + p.sig + rhs + p.body + "\n";
    }

    void analyze_procs(const std::vector<ASR::Function_t*> &all_fns, ASR::Program_t *prog) {
        std::map<ASR::Function_t*, ProcEffects> effs;
        for (ASR::Function_t *fn : all_fns) {
            ProcEffects &pe = effs.emplace(fn, ProcEffects(global_fields)).first->second;
            for (ASR::symbol_t *s : symbols_in_order(fn->m_symtab)) {
                if (ASR::is_a<ASR::Variable_t>(*s)) {
                    pe.visit_ttype(*ASR::down_cast<ASR::Variable_t>(s)->m_type);
                }
            }
            for (size_t i = 0; i < fn->n_body; i++) pe.visit_stmt(*fn->m_body[i]);
            if (pe.has_io) io_procs.insert(fn);
            if (pe.uses_globals) global_procs.insert(fn);
            std::vector<ASR::intentType> &ins = eff_intents[fn];
            ins.resize(fn->n_args);
            for (size_t i = 0; i < fn->n_args; i++) {
                ASR::Variable_t *d = dummy(fn, i);
                if (!d) {
                    ins[i] = ASR::intentType::Unspecified;
                } else if (d->m_value_attr) {
                    ins[i] = ASR::intentType::In;
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
                ProcEffects &pe = effs.at(fn);
                if (!io_procs.count(fn)) {
                    for (ASR::Function_t *c : pe.callees) {
                        if (io_procs.count(c)) {
                            io_procs.insert(fn);
                            changed = true;
                            break;
                        }
                    }
                }
                if (!global_procs.count(fn)) {
                    for (ASR::Function_t *c : pe.callees) {
                        if (global_procs.count(c)) {
                            global_procs.insert(fn);
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
                        if (d && d == sa.var && !d->m_value_attr
                                && d->m_intent == ASR::intentType::Unspecified
                                && ins[i] == ASR::intentType::In) {
                            ins[i] = ASR::intentType::InOut;
                            changed = true;
                        }
                    }
                }
            }
        }
        if (prog && !global_fields.empty()) {
            ProcEffects pe(global_fields);
            for (ASR::symbol_t *s : symbols_in_order(prog->m_symtab)) {
                if (ASR::is_a<ASR::Variable_t>(*s)) {
                    pe.visit_ttype(*ASR::down_cast<ASR::Variable_t>(s)->m_type);
                }
            }
            for (size_t i = 0; i < prog->n_body; i++) pe.visit_stmt(*prog->m_body[i]);
            prog_uses_globals = pe.uses_globals;
            for (ASR::Function_t *c : pe.callees) {
                if (global_procs.count(c)) {
                    prog_uses_globals = true;
                    break;
                }
            }
        }
    }

    std::vector<ASR::Struct_t*> structs_in_order(const std::vector<ASR::Struct_t*> &sts) {
        std::vector<ASR::Struct_t*> sorted;
        std::map<ASR::Struct_t*, int> state;
        std::function<void(ASR::Struct_t*)> visit = [&](ASR::Struct_t *st) {
            if (state[st] == 2) return;
            if (state[st] == 1) {
                unsupported("a recursive derived type is", st->base.base.loc);
            }
            state[st] = 1;
            if (st->m_parent) {
                ASR::symbol_t *ps = ASRUtils::symbol_get_past_external(st->m_parent);
                if (ASR::is_a<ASR::Struct_t>(*ps)) {
                    ASR::Struct_t *pdep = ASR::down_cast<ASR::Struct_t>(ps);
                    if (struct_ns.count(pdep)) visit(pdep);
                }
            }
            for (size_t i = 0; i < st->n_members; i++) {
                ASR::Variable_t *mv = struct_member(st, i, st->base.base.loc);
                ASR::ttype_t *et = elem_type(mv->m_type);
                if (ASR::is_a<ASR::StructType_t>(*et) && mv->m_type_declaration) {
                    ASR::symbol_t *ds = ASRUtils::symbol_get_past_external(mv->m_type_declaration);
                    if (ASR::is_a<ASR::Struct_t>(*ds)) {
                        ASR::Struct_t *dep = ASR::down_cast<ASR::Struct_t>(ds);
                        if (struct_ns.count(dep)) visit(dep);
                    }
                }
            }
            state[st] = 2;
            sorted.push_back(st);
        };
        for (ASR::Struct_t *st : sts) visit(st);
        return sorted;
    }

    std::string emit_struct(ASR::Struct_t *st) {
        const Location &loc = st->base.base.loc;
        if (st->n_member_functions > 0) unsupported("type-bound procedures are", loc);
        if (st->n_kind_params > 0) unsupported("parameterized derived types are", loc);
        std::string sn = name(st->m_name, loc);
        std::string ext_clause;
        std::string init_fields;
        if (st->m_parent) {
            ASR::Struct_t *pst = resolve_struct(st->m_parent, loc);
            ext_clause = " extends " + struct_qname(pst, loc) + " K";
            init_fields = struct_parent_proj(pst, loc) + " := "
                + struct_init_qname(pst, loc) + " (K := K)";
        }
        std::string s = "structure " + sn + " (K : Kinds)" + ext_clause + " where\n";
        SymbolTable *saved_scope = scope;
        int saved_level = level;
        scope = st->m_symtab;
        level = 0;
        for (size_t i = 0; i < st->n_members; i++) {
            ASR::Variable_t *mv = struct_member(st, i, loc);
            const Location &mloc = mv->base.base.loc;
            std::string mn = name(mv->m_name, mloc);
            s += "  " + mn + " : " + vtype(mv->m_type, mloc, mv->m_type_declaration) + "\n";
            if (!init_fields.empty()) init_fields += ", ";
            init_fields += mn + " := " + struct_member_default(mv);
        }
        int init_level = level;
        scope = saved_scope;
        level = saved_level;
        if (init_level > 0) {
            unsupported("a derived type default initializer using math or IO is", loc);
        }
        std::string init_body = init_fields.empty() ? "{}" : "{ " + init_fields + " }";
        std::string in = struct_init_name(st, loc);
        s += "\ndef " + in + " {K : Kinds} [FKinds K] : " + sn + " K :=\n  "
            + init_body + "\n\n";
        s += "instance {K : Kinds} [FKinds K] : Inhabited (" + sn + " K) := ⟨"
            + in + " (K := K)⟩\n\n";
        return s;
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
            } else if (ASR::is_a<ASR::ExternalSymbol_t>(*s) || ASR::is_a<ASR::Enum_t>(*s)) {
                continue;
            } else {
                unsupported("this top-level entity is", s->base.loc);
            }
        }
        if (require_program && !prog) {
            unsupported("a file with no program is", tu.base.base.loc);
        }
        std::vector<ASR::Function_t*> all_fns;
        std::vector<ASR::Struct_t*> all_structs;
        std::vector<ASR::Variable_t*> all_globals;
        for (ASR::Module_t *m : mods) {
            std::string mns = name(m->m_name, m->base.base.loc);
            for (ASR::symbol_t *s : symbols_in_order(m->m_symtab)) {
                if (ASR::is_a<ASR::Function_t>(*s)) {
                    ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(s);
                    proc_ns[fn] = m->m_name;
                    all_fns.push_back(fn);
                } else if (ASR::is_a<ASR::Struct_t>(*s)) {
                    ASR::Struct_t *st = ASR::down_cast<ASR::Struct_t>(s);
                    struct_ns[st] = mns;
                    all_structs.push_back(st);
                } else if (ASR::is_a<ASR::Variable_t>(*s)) {
                    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
                    if (v->m_storage != ASR::storage_typeType::Parameter) {
                        name(v->m_name, v->base.base.loc);
                        global_fields[v] = std::string(m->m_name) + "'" + std::string(v->m_name);
                        all_globals.push_back(v);
                    }
                } else if (!ASR::is_a<ASR::ExternalSymbol_t>(*s)
                        && !ASR::is_a<ASR::Enum_t>(*s)
                        && !ASR::is_a<ASR::GenericProcedure_t>(*s)
                        && !ASR::is_a<ASR::CustomOperator_t>(*s)) {
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
                } else if (ASR::is_a<ASR::Struct_t>(*s)) {
                    ASR::Struct_t *st = ASR::down_cast<ASR::Struct_t>(s);
                    struct_ns[st] = prog_ns;
                    all_structs.push_back(st);
                }
            }
        }
        for (size_t fi = 0; fi < all_fns.size(); fi++) {
            ASR::Function_t *fn = all_fns[fi];
            std::string raw_ns = proc_ns[fn];
            std::string fns = escape_ns(raw_ns, fn->base.base.loc);
            std::string lns = (fns.empty() ? "" : fns + ".")
                + std::string(fn->m_name) + "'types";
            std::string raw_sub_ns = (raw_ns.empty() ? "" : raw_ns + ".")
                + std::string(fn->m_name) + "'procs";
            std::string fn_prefix = (raw_ns.empty() ? std::string("top") : raw_ns)
                + "'" + std::string(fn->m_name) + "'save'";
            std::replace(fn_prefix.begin(), fn_prefix.end(), '.', '\'');
            for (ASR::symbol_t *s : symbols_in_order(fn->m_symtab)) {
                if (ASR::is_a<ASR::Struct_t>(*s)) {
                    ASR::Struct_t *st = ASR::down_cast<ASR::Struct_t>(s);
                    struct_ns[st] = lns;
                    all_structs.push_back(st);
                } else if (ASR::is_a<ASR::Function_t>(*s)) {
                    ASR::Function_t *sub = ASR::down_cast<ASR::Function_t>(s);
                    proc_ns[sub] = raw_sub_ns;
                    all_fns.push_back(sub);
                } else if (ASR::is_a<ASR::Variable_t>(*s)) {
                    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
                    if (v->m_storage == ASR::storage_typeType::Save
                            && v->m_intent == ASR::intentType::Local) {
                        name(v->m_name, v->base.base.loc);
                        global_fields[v] = fn_prefix + std::string(v->m_name);
                        all_globals.push_back(v);
                    }
                }
            }
        }
        std::vector<ASR::Struct_t*> sorted_structs = structs_in_order(all_structs);
        std::map<ASR::Struct_t*, std::string> struct_defs;
        for (ASR::Struct_t *st : sorted_structs) {
            struct_defs[st] = emit_struct(st);
        }
        std::string globals_def;
        if (!all_globals.empty()) {
            globals_def = "structure Globals' (K : Kinds) where\n";
            std::string init_fields;
            SymbolTable *saved_scope = scope;
            int saved_level = level;
            level = 0;
            for (size_t i = 0; i < all_globals.size(); i++) {
                ASR::Variable_t *v = all_globals[i];
                const Location &vloc = v->base.base.loc;
                scope = v->m_parent_symtab;
                std::string gf = global_fields[v];
                globals_def += "  " + gf + " : " + vtype(v->m_type, vloc, v->m_type_declaration) + "\n";
                if (i) init_fields += ", ";
                init_fields += gf + " := " + struct_member_default(v);
                if (level > 0) {
                    scope = saved_scope;
                    level = saved_level;
                    unsupported("a module variable initializer using math or IO is", vloc);
                }
            }
            scope = saved_scope;
            level = saved_level;
            globals_def += "\ndef Globals''init {K : Kinds} [FKinds K] : Globals' K :=\n  { "
                + init_fields + " }\n\n";
            globals_def += "instance {K : Kinds} [FKinds K] : Inhabited (Globals' K) := ⟨Globals''init (K := K)⟩\n\n";
        }
        analyze_procs(all_fns, prog);
        for (ASR::Function_t *fn : all_fns) function(fn);
        std::string prog_body;
        int prog_level = 2;
        Proc prog_proc;
        if (prog) {
            cur = &prog_proc;
            level = 2;
            loop_vars.clear();
            alloc_str_len.clear();
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
        for (ASR::Struct_t *st : sorted_structs) {
            std::string sns = struct_ns[st];
            if (sns != cur_ns) {
                if (!cur_ns.empty()) out += "end " + cur_ns + "\n\n";
                if (!sns.empty()) out += "namespace " + sns + "\n\n";
                cur_ns = sns;
            }
            out += struct_defs[st];
        }
        if (!cur_ns.empty()) {
            out += "end " + cur_ns + "\n\n";
            cur_ns.clear();
        }
        if (!globals_def.empty()) out += globals_def;
        for (ASR::Function_t *fn : callees_first(all_fns)) {
            std::string fns = escape_ns(proc_ns[fn], fn->base.base.loc);
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
            if (prog_uses_globals) {
                out += "def " + pn + " {K : Kinds} [" + level_class(prog_level)
                    + " K] : StateT (Globals' K) IO Unit := do\n" + prog_body + "\n";
                out += "def main : IO UInt32 := LFortran.run ((" + pn
                    + " (K := LFortran.machine)).run' (Globals''init (K := LFortran.machine)))\n";
            } else {
                out += "def " + pn + " {K : Kinds} [" + level_class(prog_level)
                    + " K] : IO Unit := do\n" + prog_body + "\n";
                out += "def main : IO UInt32 := LFortran.run (" + pn
                    + " (K := LFortran.machine))\n";
            }
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
