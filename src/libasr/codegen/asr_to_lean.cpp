#include <algorithm>
#include <cmath>
#include <set>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/codegen/asr_to_lean.h>

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

// Lean precedences of the emitted expressions
constexpr int prec_atom = 1024;
constexpr int prec_app = 1023;
constexpr int prec_mul = 70;
constexpr int prec_add = 65;
constexpr int prec_cmp = 50;

const std::set<std::string> reserved_names = {"fp", "intRange", "Id", "at",
    "by", "def", "do", "else", "end", "for", "from", "fun", "have", "if", "in",
    "let", "match", "mut", "open", "return", "show", "then", "where", "with"};

std::vector<ASR::symbol_t*> symbols_in_source_order(SymbolTable *symtab) {
    std::vector<ASR::symbol_t*> syms;
    for (auto &item : symtab->get_scope()) syms.push_back(item.second);
    std::stable_sort(syms.begin(), syms.end(),
        [](ASR::symbol_t *a, ASR::symbol_t *b) {
            return a->base.loc.first < b->base.loc.first;
        });
    return syms;
}

// The exact value of a double: an integer, or `(m / 2^e : ℝ)`
std::string real_literal(double v, const Location &loc) {
    if (!std::isfinite(v)) unsupported("non-finite real constant", loc);
    if (v == std::trunc(v) && std::fabs(v) < 0x1p62) {
        int64_t i = (int64_t) v;
        return i < 0 ? "(" + std::to_string(i) + ")" : std::to_string(i);
    }
    int e;
    double f = std::frexp(v, &e);
    int64_t m = (int64_t) std::ldexp(f, 53);
    e -= 53;
    while (m % 2 == 0) {
        m /= 2;
        e++;
    }
    std::string ms = std::to_string(m);
    if (e < 0) return "(" + ms + " / 2^" + std::to_string(-e) + " : ℝ)";
    return "(" + ms + " * 2^" + std::to_string(e) + " : ℝ)";
}

class ASRToLeanVisitor : public ASR::BaseVisitor<ASRToLeanVisitor>
{
public:
    std::string out;
    // Result of the last visited expression and its Lean precedence
    std::string src;
    int prec = prec_atom;

    std::string filename;
    int indent_level = 0;
    SymbolTable *scope = nullptr;
    std::string return_var;
    std::set<std::string> loop_vars, active_loop_vars;

    std::string indent() {
        return std::string(2 * indent_level, ' ');
    }

    std::string lean_type(ASR::ttype_t *t, const Location &loc) {
        if (ASR::is_a<ASR::Integer_t>(*t)
                && ASR::down_cast<ASR::Integer_t>(t)->m_kind == 4) {
            return "ℤ";
        }
        if (ASR::is_a<ASR::Real_t>(*t)
                && ASR::down_cast<ASR::Real_t>(t)->m_kind == 8) {
            return "ℝ";
        }
        unsupported("type '" + ASRUtils::type_to_str_fortran_expr(t, nullptr)
            + "' is", loc);
    }

    std::string lean_name(const std::string &name, const Location &loc) {
        if (reserved_names.count(name)) {
            unsupported("the name '" + name + "' is", loc);
        }
        return name;
    }

    std::string expr(const ASR::expr_t &x) {
        switch (x.type) {
            case ASR::exprType::Var:
            case ASR::exprType::IntegerConstant:
            case ASR::exprType::RealConstant:
            case ASR::exprType::IntegerBinOp:
            case ASR::exprType::IntegerCompare:
            case ASR::exprType::RealBinOp:
            case ASR::exprType::Cast:
                visit_expr(x);
                return src;
            default:
                unsupported("this expression is", x.base.loc);
        }
    }

    // An operand of a function application
    std::string arg(const ASR::expr_t &x) {
        std::string r = expr(x);
        return prec < prec_atom ? "(" + r + ")" : r;
    }

    void stmts(ASR::stmt_t **body, size_t n) {
        indent_level++;
        if (n == 0) out += indent() + "pure ()\n";
        for (size_t i = 0; i < n; i++) {
            switch (body[i]->type) {
                case ASR::stmtType::Assignment:
                case ASR::stmtType::If:
                case ASR::stmtType::DoLoop:
                case ASR::stmtType::Return:
                    visit_stmt(*body[i]);
                    break;
                default:
                    unsupported("this statement is", body[i]->base.loc);
            }
        }
        indent_level--;
    }

    void collect_loop_vars(ASR::stmt_t **body, size_t n) {
        for (size_t i = 0; i < n; i++) {
            if (ASR::is_a<ASR::DoLoop_t>(*body[i])) {
                ASR::DoLoop_t *d = ASR::down_cast<ASR::DoLoop_t>(body[i]);
                if (d->m_head.m_v && ASR::is_a<ASR::Var_t>(*d->m_head.m_v)) {
                    loop_vars.insert(ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(d->m_head.m_v)->m_v));
                }
                collect_loop_vars(d->m_body, d->n_body);
            } else if (ASR::is_a<ASR::If_t>(*body[i])) {
                ASR::If_t *s = ASR::down_cast<ASR::If_t>(body[i]);
                collect_loop_vars(s->m_body, s->n_body);
                collect_loop_vars(s->m_orelse, s->n_orelse);
            }
        }
    }

    void visit_TranslationUnit(const ASR::TranslationUnit_t &x) {
        out = "-- Generated by LFortran (`lfortran --show-lean " + filename
            + "`). Do not edit.\nimport LFortran.Prelude\n\nopen LFortran\n\n";
        for (ASR::symbol_t *sym : symbols_in_source_order(x.m_symtab)) {
            if (!ASR::is_a<ASR::Module_t>(*sym)) {
                unsupported("a top-level entity other than a module is",
                    sym->base.loc);
            }
            visit_symbol(*sym);
        }
    }

    void visit_Module(const ASR::Module_t &x) {
        if (x.n_dependencies > 0) {
            unsupported("a module with dependencies is", x.base.base.loc);
        }
        std::string name = lean_name(x.m_name, x.base.base.loc);
        out += "namespace " + name + "\n\n";
        for (ASR::symbol_t *sym : symbols_in_source_order(x.m_symtab)) {
            if (ASR::is_a<ASR::Function_t>(*sym)) {
                visit_symbol(*sym);
                out += "\n";
                continue;
            }
            // Integer parameters such as kind constants need no translation
            ASR::Variable_t *v = ASR::is_a<ASR::Variable_t>(*sym)
                ? ASR::down_cast<ASR::Variable_t>(sym) : nullptr;
            if (!v || v->m_storage != ASR::storage_typeType::Parameter
                    || !ASR::is_a<ASR::Integer_t>(*v->m_type)) {
                unsupported("this module entity is", sym->base.loc);
            }
        }
        out += "end " + name + "\n";
    }

    void visit_Function(const ASR::Function_t &x) {
        const Location &loc = x.base.base.loc;
        ASR::FunctionType_t *ft = ASRUtils::get_FunctionType(x);
        if (!x.m_return_var) unsupported("a subroutine is", loc);
        if (ft->m_abi != ASR::abiType::Source
                || ft->m_deftype != ASR::deftypeType::Implementation
                || ft->m_elemental) {
            unsupported("this kind of procedure is", loc);
        }
        scope = x.m_symtab;
        loop_vars.clear();
        collect_loop_vars(x.m_body, x.n_body);

        std::string header = "noncomputable def "
            + lean_name(x.m_name, loc) + " (fp : FPModel)";
        std::set<std::string> args;
        for (size_t i = 0; i < x.n_args; i++) {
            ASR::Variable_t *v = ASRUtils::EXPR2VAR(x.m_args[i]);
            if (v->m_intent != ASR::intentType::In) {
                unsupported("an argument without intent(in) is",
                    v->base.base.loc);
            }
            args.insert(v->m_name);
            header += " (" + lean_name(v->m_name, v->base.base.loc) + " : "
                + lean_type(v->m_type, v->base.base.loc) + ")";
        }
        ASR::Variable_t *ret = ASRUtils::EXPR2VAR(x.m_return_var);
        return_var = ret->m_name;
        out += header + " : " + lean_type(ret->m_type, ret->base.base.loc)
            + " := Id.run do\n";

        // The result variable first, then the locals in source order
        std::vector<ASR::Variable_t*> decls = {ret};
        for (ASR::symbol_t *sym : symbols_in_source_order(x.m_symtab)) {
            if (!ASR::is_a<ASR::Variable_t>(*sym)) {
                unsupported("this entity in a function is", sym->base.loc);
            }
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
            if (args.count(v->m_name) || v == ret
                    || loop_vars.count(v->m_name)) {
                continue;
            }
            if (v->m_intent != ASR::intentType::Local
                    || v->m_storage != ASR::storage_typeType::Default
                    || v->m_symbolic_value) {
                unsupported("this variable declaration is", v->base.base.loc);
            }
            decls.push_back(v);
        }
        indent_level++;
        for (ASR::Variable_t *v : decls) {
            out += indent() + "let mut " + lean_name(v->m_name, v->base.base.loc)
                + " : " + lean_type(v->m_type, v->base.base.loc) + " := 0\n";
        }
        indent_level--;
        stmts(x.m_body, x.n_body);
        if (x.n_body == 0 || !ASR::is_a<ASR::Return_t>(*x.m_body[x.n_body-1])) {
            out += "  return " + return_var + "\n";
        }
        scope = nullptr;
    }

    void visit_Assignment(const ASR::Assignment_t &x) {
        if (x.m_overloaded || !ASR::is_a<ASR::Var_t>(*x.m_target)) {
            unsupported("this assignment is", x.base.base.loc);
        }
        ASR::Variable_t *v = ASRUtils::EXPR2VAR(x.m_target);
        if (v->m_intent == ASR::intentType::In || loop_vars.count(v->m_name)) {
            unsupported("assignment to this variable is", x.base.base.loc);
        }
        std::string target = expr(*x.m_target);
        out += indent() + target + " := " + expr(*x.m_value) + "\n";
    }

    void visit_If(const ASR::If_t &x) {
        out += indent() + "if " + expr(*x.m_test) + " then\n";
        stmts(x.m_body, x.n_body);
        if (x.n_orelse > 0) {
            out += indent() + "else\n";
            stmts(x.m_orelse, x.n_orelse);
        }
    }

    void visit_DoLoop(const ASR::DoLoop_t &x) {
        const ASR::do_loop_head_t &h = x.m_head;
        bool unit_step = !h.m_increment
            || (ASR::is_a<ASR::IntegerConstant_t>(*h.m_increment)
                && ASR::down_cast<ASR::IntegerConstant_t>(
                    h.m_increment)->m_n == 1);
        if (!h.m_v || !ASR::is_a<ASR::Var_t>(*h.m_v) || !h.m_start
                || !h.m_end || !unit_step || x.n_orelse > 0) {
            unsupported("this do loop is", x.base.base.loc);
        }
        std::string var = ASRUtils::EXPR2VAR(h.m_v)->m_name;
        lean_type(ASRUtils::expr_type(h.m_v), x.base.base.loc);
        if (active_loop_vars.count(var)) {
            unsupported("reusing an active loop variable is", x.base.base.loc);
        }
        std::string lo = arg(*h.m_start);
        std::string hi = arg(*h.m_end);
        out += indent() + "for " + var + " in intRange " + lo + " " + hi
            + " do\n";
        active_loop_vars.insert(var);
        stmts(x.m_body, x.n_body);
        active_loop_vars.erase(var);
    }

    void visit_Return(const ASR::Return_t &/*x*/) {
        out += indent() + "return " + return_var + "\n";
    }

    void visit_Var(const ASR::Var_t &x) {
        ASR::symbol_t *sym = x.m_v;
        if (!ASR::is_a<ASR::Variable_t>(*sym)
                || ASRUtils::symbol_parent_symtab(sym) != scope) {
            unsupported("a reference to a non-local entity is", x.base.base.loc);
        }
        std::string name = ASRUtils::symbol_name(sym);
        if (loop_vars.count(name) && !active_loop_vars.count(name)) {
            unsupported("using a do-loop variable outside its loop is",
                x.base.base.loc);
        }
        src = name;
        prec = prec_atom;
    }

    void visit_IntegerConstant(const ASR::IntegerConstant_t &x) {
        lean_type(x.m_type, x.base.base.loc);
        src = std::to_string(x.m_n);
        if (x.m_n < 0) src = "(" + src + ")";
        prec = prec_atom;
    }

    void visit_RealConstant(const ASR::RealConstant_t &x) {
        lean_type(x.m_type, x.base.base.loc);
        // A literal is the rounding of its exact value to floating point
        src = "fp.round " + real_literal(x.m_r, x.base.base.loc);
        prec = prec_app;
    }

    void visit_IntegerBinOp(const ASR::IntegerBinOp_t &x) {
        lean_type(x.m_type, x.base.base.loc);
        std::string op;
        int p;
        switch (x.m_op) {
            case ASR::binopType::Add: op = " + "; p = prec_add; break;
            case ASR::binopType::Sub: op = " - "; p = prec_add; break;
            case ASR::binopType::Mul: op = " * "; p = prec_mul; break;
            default: unsupported("this integer operator is", x.base.base.loc);
        }
        std::string left = expr(*x.m_left);
        if (prec < p) left = "(" + left + ")";
        std::string right = expr(*x.m_right);
        if (prec <= p) right = "(" + right + ")";
        src = left + op + right;
        prec = p;
    }

    void visit_IntegerCompare(const ASR::IntegerCompare_t &x) {
        std::string op;
        switch (x.m_op) {
            case ASR::cmpopType::Eq: op = " = "; break;
            case ASR::cmpopType::NotEq: op = " ≠ "; break;
            case ASR::cmpopType::Lt: op = " < "; break;
            case ASR::cmpopType::LtE: op = " ≤ "; break;
            case ASR::cmpopType::Gt: op = " > "; break;
            case ASR::cmpopType::GtE: op = " ≥ "; break;
        }
        std::string left = expr(*x.m_left);
        if (prec <= prec_cmp) left = "(" + left + ")";
        std::string right = expr(*x.m_right);
        if (prec <= prec_cmp) right = "(" + right + ")";
        src = left + op + right;
        prec = prec_cmp;
    }

    void visit_RealBinOp(const ASR::RealBinOp_t &x) {
        lean_type(x.m_type, x.base.base.loc);
        std::string fn;
        switch (x.m_op) {
            case ASR::binopType::Add: fn = "fp.add"; break;
            case ASR::binopType::Sub: fn = "fp.sub"; break;
            case ASR::binopType::Mul: fn = "fp.mul"; break;
            case ASR::binopType::Div: fn = "fp.div"; break;
            default: unsupported("this real operator is", x.base.base.loc);
        }
        std::string left = arg(*x.m_left);
        std::string right = arg(*x.m_right);
        src = fn + " " + left + " " + right;
        prec = prec_app;
    }

    void visit_Cast(const ASR::Cast_t &x) {
        if (x.m_kind != ASR::cast_kindType::IntegerToReal) {
            unsupported("this type conversion is", x.base.base.loc);
        }
        lean_type(x.m_type, x.base.base.loc);
        lean_type(ASRUtils::expr_type(x.m_arg), x.base.base.loc);
        src = "fp.ofInt " + arg(*x.m_arg);
        prec = prec_app;
    }
};

} // namespace

Result<std::string> asr_to_lean(ASR::TranslationUnit_t &asr,
    diag::Diagnostics &diagnostics, const std::string &filename)
{
    ASRToLeanVisitor v;
    v.filename = filename;
    try {
        v.visit_asr((ASR::asr_t &) asr);
    } catch (const LeanError &e) {
        diagnostics.diagnostics.push_back(e.d);
        return Error();
    }
    return v.out;
}

} // namespace LCompilers
