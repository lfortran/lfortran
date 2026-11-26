/*
The LL parser of free-form Fortran (a prototype; the Bison parser is the one
that is used by default).

The grammar is in ll_parser.ll. llgen.py generates from it the member
functions that parse its rules (ll_parser_rules.h), which are included in the
class `LL` below. This file has the rest of the parser:

* the tokens: the whole file is tokenized first, with the same re2c tokenizer
  as the Bison parser, into arrays. A keyword is just a kind of name:
  `is_name()` is a range check, a keyword test is an integer comparison;
* the predicates that the grammar uses (`%predicate`);
* the few rules that are written by hand (`%extern`);
* the functions that the actions of the grammar call, where a macro of
  semantics.h alone is not enough.

The AST is built by the same macros and functions as the Bison parser
(semantics.h), so both parsers return the same AST, including locations.

The parser reports no syntax errors: at anything it cannot parse it throws
`LLUnsupported`, and the caller parses the file again with the Bison parser.
It parses all the free-form files in integration_tests/ and tests/ that the
Bison parser accepts. It does not parse fixed form or interactive input.
*/

#include <lfortran/parser/parser.h>
#include <lfortran/parser/tokenizer.h>
#include <lfortran/parser/parser.tab.hh>
#include <lfortran/parser/ll_parser.h>
#include <lfortran/ast.h>
#include <lfortran/ast_kind.h>
#include <libasr/string_utils.h>
#include <lfortran/parser/parser_exception.h>
#include <cstring>

namespace {

// semantics.h is written to be included once, in the Bison parser. Including
// it here in an unnamed namespace gives this file its own copy of everything.
#if defined(__GNUC__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
#endif
#include <lfortran/parser/semantics.h>
#if defined(__GNUC__)
#pragma GCC diagnostic pop
#endif

using LCompilers::LFortran::YYSTYPE;
using LCompilers::LFortran::LLUnsupported;

// The header of FORALL and DO CONCURRENT: the controls and the mask (or null)
struct ConcurrentHeader {
    Vec<ast_t*> controls;
    ast_t *mask;
};

// The stop code and the QUIET specifier of STOP and ERROR STOP (or null)
struct StopCode {
    ast_t *code;
    ast_t *quiet;
};

// RESULT and BIND of a function (or null)
struct FunctionClauses {
    ast_t *result;
    ast_t *bind;
};

struct LL {
    Allocator &m_a;
    LCompilers::diag::Diagnostics &diag;
    bool fixed_form = false;
    Vec<ast_t*> result;
    // The semantics.h macros refer to the parser as `p`
    LL &p;

    // Tokens: kind, semantic value and location; `i` is the current token
    const int *K;
    const YYSTYPE *V;
    const Location *Lc;
    size_t i = 0;

    LL(Allocator &al, LCompilers::diag::Diagnostics &diag, const int *K,
        const YYSTYPE *V, const Location *Lc)
        : m_a{al}, diag{diag}, p{*this}, K{K}, V{V}, Lc{Lc} {
        result.reserve(al, 32);
    }

    // Gives up; `line` is the line of the grammar (or of this file) that did
    [[noreturn]] void unsup(int line) {
        throw LLUnsupported{line, K[i], Lc[i].first};
    }

    static bool is_name(int t) {
        return t == TK_NAME || (t >= KW_ABSTRACT && t <= KW_WRITE);
    }
    static bool is_sep(int t) {
        return t == TK_NEWLINE || t == TK_COMMENT || t == TK_EOLCOMMENT
            || t == TK_SEMICOLON;
    }
    void expect_(int t, int line) { if (K[i] != t) unsup(line); i++; }
    bool accept(int t) { if (K[i] == t) { i++; return true; } return false; }
    // The location of the tokens from `s` to the last one consumed
    Location span(size_t s) const {
        Location l;
        l.first = Lc[s].first;
        l.last = Lc[i-1].last;
        return l;
    }

    // The rules of the grammar
    #include <lfortran/parser/ll_parser_rules.h>

    // ------------------------------------------------------------------
    // Predicates (%predicate in the grammar)

    // Is the statement that starts at the current token (a name) an
    // assignment? It is if a designator is followed by `=` or `=>`:
    //     name [ (...) ]... [ % name [ (...) ]... ]... =
    bool is_assignment() const {
        if (!is_name(K[i])) return false;
        size_t j = i + 1;
        for (;;) {
            int t = K[j];
            if (t == TK_LPAREN || t == TK_LBRACKET) {
                int depth = 1;
                j++;
                while (depth > 0) {
                    int u = K[j];
                    if (u == TK_LPAREN || u == TK_LBRACKET) {
                        depth++;
                    } else if (u == TK_RPAREN || u == TK_RBRACKET
                            || u == TK_RBRACKET_OLD) {
                        depth--;
                    } else if (u == TK_NEWLINE || u == END_OF_FILE) {
                        return false;
                    }
                    j++;
                }
            } else if (t == TK_PERCENT) {
                if (!is_name(K[j+1])) return false;
                j += 2;
            } else {
                return t == TK_EQUAL || t == TK_ARROW
                    || t == TK_COLON_EQUAL;
            }
        }
    }

    static bool is_intrinsic_type_kw(int t) {
        return t == KW_INTEGER || t == KW_REAL || t == KW_COMPLEX
            || t == KW_LOGICAL || t == KW_CHARACTER || t == KW_BYTE
            || t == KW_DOUBLE_PRECISION || t == KW_DOUBLE_COMPLEX;
    }

    // Does an intrinsic_type_spec start at the current token?
    bool is_intrinsic_type_start() const {
        int t = K[i];
        if (is_intrinsic_type_kw(t)) return true;
        if (t == KW_DOUBLE) {
            return K[i+1] == KW_PRECISION || K[i+1] == KW_COMPLEX;
        }
        if (t == KW_LIST || t == KW_SET || t == KW_DICT || t == KW_TUPLE) {
            return K[i+1] == TK_LPAREN;
        }
        return false;
    }

    // Does a var_type start at the current token?
    bool is_var_type_start() const {
        if (is_intrinsic_type_start()) return true;
        int t = K[i];
        return (t == KW_TYPE || t == KW_CLASS || t == KW_PROCEDURE)
            && K[i+1] == TK_LPAREN;
    }

    // Does `::` come before the bracket that closes the array constructor
    // which starts just before the current token?
    bool array_constructor_has_type() const {
        int depth = 0;
        for (size_t j = i; ; j++) {
            int u = K[j];
            if (u == TK_LPAREN || u == TK_LBRACKET) {
                depth++;
            } else if (u == TK_RPAREN) {
                depth--;
            } else if (u == TK_RBRACKET || u == TK_RBRACKET_OLD) {
                if (depth == 0) return false;
                depth--;
            } else if (u == TK_DBL_COLON && depth == 0) {
                return true;
            } else if (u == TK_NEWLINE || u == END_OF_FILE) {
                return false;
            }
        }
    }

    // Is there a `::` before the next `,` or `)` that is not nested?
    bool has_dbl_colon() const {
        int depth = 0;
        for (size_t j = i; ; j++) {
            int u = K[j];
            if (u == TK_LPAREN || u == TK_LBRACKET) {
                depth++;
            } else if (u == TK_RPAREN || u == TK_RBRACKET) {
                if (depth == 0) return false;
                depth--;
            } else if (u == TK_COMMA && depth == 0) {
                return false;
            } else if (u == TK_DBL_COLON && depth == 0) {
                return true;
            } else if (u == TK_NEWLINE || u == END_OF_FILE) {
                return false;
            }
        }
    }

    // ------------------------------------------------------------------
    // Rules written by hand (%extern in the grammar)

    ast_t* id() {
        if (!is_name(K[i])) unsup(__LINE__);
        Location l = Lc[i];
        ast_t *r = SYMBOL(V[i].string, l);
        i++;
        return r;
    }

    ast_t* sep_one() {
        Location l = Lc[i];
        const YYSTYPE &v = V[i];
        switch (K[i++]) {
            case TK_NEWLINE: return nullptr;
            case TK_COMMENT: return COMMENT(v.string, l);
            case TK_EOLCOMMENT: return EOLCOMMENT(v.string, l);
            default: return SEMICOLON(l);
        }
    }

    Vec<ast_t*> sep() {
        if (!is_sep(K[i])) unsup(__LINE__);
        size_t s = i;
        Vec<ast_t*> v = sep_new(m_a, sep_one());
        while (is_sep(K[i])) {
            Location first = span(s);
            Location l = Lc[i];
            v = sep_add(m_a, v, first, sep_one(), l);
        }
        return v;
    }

    ast_t* expr() { return binary(1); }

    // "(" expr ")", a complex constant, an implied do loop or a conditional
    // expression
    ast_t* parenthesized() {
        size_t s = i;
        i++;
        ast_t *x = expr();
        Location l;
        if (accept(TK_RPAREN)) {
            l = span(s);
            return PAREN(x, l);
        }
        if (accept(TK_QUESTION)) {
            ast_t *a = cond_consequent();
            expect_(TK_COLON, __LINE__);
            ast_t *b = cond_expr_tail();
            expect_(TK_RPAREN, __LINE__);
            l = span(s);
            return COND_EXPR(x, a, b, l);
        }
        expect_(TK_COMMA, __LINE__);
        // The expressions after the first one, up to the loop variable
        ast_t *y = nullptr;
        Vec<ast_t*> rest;
        rest.from_pointer_n(nullptr, 0);
        size_t n = 1;
        for (;;) {
            if (is_name(K[i]) && K[i+1] == TK_EQUAL) {
                ast_t *var = id();
                i++;
                ast_t *lo = expr();
                expect_(TK_COMMA, __LINE__);
                ast_t *hi = expr();
                ast_t *incr = nullptr;
                if (accept(TK_COMMA)) incr = expr();
                expect_(TK_RPAREN, __LINE__);
                l = span(s);
                if (n == 1) {
                    if (incr) return IMPLIED_DO_LOOP4(x, var, lo, hi, incr, l);
                    return IMPLIED_DO_LOOP1(x, var, lo, hi, l);
                } else if (n == 2) {
                    if (incr) return IMPLIED_DO_LOOP5(x, y, var, lo, hi, incr, l);
                    return IMPLIED_DO_LOOP2(x, y, var, lo, hi, l);
                }
                if (incr) return IMPLIED_DO_LOOP6(x, y, rest, var, lo, hi, incr, l);
                return IMPLIED_DO_LOOP3(x, y, rest, var, lo, hi, l);
            }
            ast_t *e = expr();
            n++;
            if (n == 2) {
                y = e;
            } else {
                if (n == 3) LIST_NEW(rest);
                LIST_ADD(rest, e);
            }
            if (accept(TK_RPAREN)) {
                if (n != 2) unsup(__LINE__);
                l = span(s);
                return COMPLEX(x, y, l);
            }
            expect_(TK_COMMA, __LINE__);
        }
    }

    ast_t* cond_consequent() {
        if (K[i] == TK_NIL) {
            Location l = Lc[i];
            i++;
            return NIL(l);
        }
        return expr();
    }

    ast_t* cond_expr_tail() {
        size_t s = i;
        ast_t *x = cond_consequent();
        if (accept(TK_QUESTION)) {
            ast_t *a = cond_consequent();
            expect_(TK_COLON, __LINE__);
            ast_t *b = cond_expr_tail();
            Location l = span(s);
            return COND_EXPR(x, a, b, l);
        }
        return x;
    }

    // designator: [ member % ]... id [ (args) [ (args) ] ] [ "[" coargs "]" ]
    // `call_stmt` parses the same for a CALL statement.
    ast_t* designator(bool call_stmt = false, size_t s0 = 0) {
        size_t s = call_stmt ? s0 : i;
        Vec<struct_member_t> members;
        bool has_members = false;
        for (;;) {
            ast_t *name = id();
            Vec<FnArg> args;
            bool has_args = false;
            if (K[i] == TK_LPAREN) {
                i++;
                args = fnarray_arg_list_opt();
                expect_(TK_RPAREN, __LINE__);
                has_args = true;
            }
            if (K[i] == TK_PERCENT) {
                i++;
                if (!has_members) {
                    LIST_NEW(members);
                    has_members = true;
                }
                struct_member_t *m;
                if (has_args) {
                    STRUCT_MEMBER2(m, name, args);
                } else {
                    STRUCT_MEMBER1(m, name);
                }
                PLIST_ADD(members, m);
                continue;
            }
            if (K[i] == TK_LBRACE) {
                if (has_args || has_members) unsup(__LINE__);
                Vec<ast_t*> temp_args = instantiate_symbols();
                expect_(TK_LPAREN, __LINE__);
                args = fnarray_arg_list_opt();
                expect_(TK_RPAREN, __LINE__);
                Location l = span(s);
                if (call_stmt) return SUBROUTINE_CALL4(name, temp_args, args, l);
                return FUNCCALLORARRAY5(name, args, temp_args, l);
            }
            if (call_stmt) {
                Location l = span(s);
                if (has_members) {
                    if (has_args) return SUBROUTINE_CALL1(members, name, args, l);
                    return SUBROUTINE_CALL3(members, name, l);
                }
                if (has_args) return SUBROUTINE_CALL(name, args, l);
                return SUBROUTINE_CALL2(name, l);
            }
            if (K[i] == TK_LBRACKET) {
                i++;
                Vec<CoarrayArg> co = coarray_arg_list();
                expect_(TK_RBRACKET, __LINE__);
                Location l = span(s);
                if (has_members) {
                    if (has_args) return COARRAY4(members, name, args, co, l);
                    return COARRAY3(members, name, co, l);
                }
                if (has_args) return COARRAY2(name, args, co, l);
                return COARRAY1(name, co, l);
            }
            if (has_args && K[i] == TK_LPAREN) {
                i++;
                Vec<FnArg> args2 = fnarray_arg_list_opt();
                expect_(TK_RPAREN, __LINE__);
                Location l = span(s);
                if (has_members) {
                    return FUNCCALLORARRAY4(members, name, args, args2, l);
                }
                return FUNCCALLORARRAY3(name, args, args2, l);
            }
            Location l = span(s);
            if (has_args) {
                if (has_members) return FUNCCALLORARRAY2(members, name, args, l);
                return FUNCCALLORARRAY(name, args, l);
            }
            if (has_members) {
                ast_t *out;
                NAME1(out, name, members, l);
                return out;
            }
            return name;
        }
    }

    ast_t* call_statement() {
        size_t s = i;
        i++;
        return designator(true, s);
    }

    // statement: [ label ] statement1 sep
    // Returns nullptr (and consumes nothing) if no statement starts here.
    ast_t* statement() {
        size_t s = i;
        bool has_label = (K[i] == TK_LABEL);
        if (has_label) i++;
        ast_t *st = statement1();
        if (st == nullptr) {
            i = s;
            return nullptr;
        }
        Vec<ast_t*> sp = sep();
        Location l = span(s);
        if (has_label) LABEL(st, V[s].n);
        TRIVIA_(st, TRIVIA_AFTER(sp, l));
        return st;
    }

    ast_t* assignment_statement() {
        size_t s = i;
        ast_t *x = designator();
        int op = K[i];
        i++;
        ast_t *y = expr();
        Location l = span(s);
        if (op == TK_ARROW) return ASSOCIATE(x, y, l);
        if (op == TK_COLON_EQUAL) return INFER_ASSIGNMENT(x, y, l);
        return ASSIGNMENT(x, y, l);
    }

    Vec<ast_t*> data_sets() {
        Vec<ast_t*> sets;
        LIST_NEW(sets);
        for (;;) {
            LIST_ADD(sets, data_set());
            if (accept(TK_COMMA)) continue;
            if (is_sep(K[i])) break;
        }
        return sets;
    }

    ast_t* data_object() {
        size_t s = i;
        if (K[i] != TK_LPAREN) return designator();
        i++;
        Vec<ast_t*> objects;
        LIST_NEW(objects);
        for (;;) {
            LIST_ADD(objects, data_object());
            expect_(TK_COMMA, __LINE__);
            ast_t *type = nullptr;
            if (K[i] == KW_INTEGER && K[i+1] != TK_EQUAL) {
                type = var_type();
                expect_(TK_DBL_COLON, __LINE__);
            } else if (!(is_name(K[i]) && K[i+1] == TK_EQUAL)) {
                continue;
            }
            ast_t *var = id();
            expect_(TK_EQUAL, __LINE__);
            ast_t *lo = expr();
            expect_(TK_COMMA, __LINE__);
            ast_t *hi = expr();
            ast_t *incr = nullptr;
            if (accept(TK_COMMA)) incr = expr();
            expect_(TK_RPAREN, __LINE__);
            Location l = span(s);
            if (incr) return DATA_IMPLIED_DO2(objects, type, var, lo, hi, incr, l);
            return DATA_IMPLIED_DO1(objects, type, var, lo, hi, l);
        }
    }

    Vec<ast_t*> data_stmt_value_list() {
        Vec<ast_t*> v;
        LIST_NEW(v);
        do {
            ast_t *x = data_stmt_constant();
            if (accept(TK_STAR)) {
                ast_t *y = data_stmt_constant();
                REPEAT_LIST_ADD(v, x, y);
            } else {
                LIST_ADD(v, x);
            }
        } while (accept(TK_COMMA));
        return v;
    }

    // concurrent_control_list [ "," mask ] ")"
    Vec<ast_t*> concurrent_controls(ast_t *&mask) {
        Vec<ast_t*> v;
        LIST_NEW(v);
        mask = nullptr;
        do {
            size_t s = i;
            ast_t *type = nullptr;
            if (!(is_name(K[i]) && K[i+1] == TK_EQUAL)) {
                if (!has_dbl_colon()) {
                    mask = expr();
                    break;
                }
                type = var_type();
                expect_(TK_DBL_COLON, __LINE__);
            }
            ast_t *var = id();
            expect_(TK_EQUAL, __LINE__);
            ast_t *a = expr();
            expect_(TK_COLON, __LINE__);
            ast_t *b = expr();
            Location l;
            if (accept(TK_COLON)) {
                ast_t *c = expr();
                l = span(s);
                LIST_ADD(v, CONCURRENT_CONTROL2(type, var, a, b, c, l));
            } else {
                l = span(s);
                LIST_ADD(v, CONCURRENT_CONTROL1(type, var, a, b, l));
            }
        } while (accept(TK_COMMA));
        expect_(TK_RPAREN, __LINE__);
        return v;
    }

    ConcurrentHeader concurrent_header() {
        expect_(TK_LPAREN, __LINE__);
        ConcurrentHeader h;
        h.controls = concurrent_controls(h.mask);
        return h;
    }

    Vec<ast_t*> event_wait_specs() {
        expect_(TK_COMMA, __LINE__);
        Vec<ast_t*> specs;
        LIST_NEW(specs);
        if (K[i] != TK_RPAREN) {
            size_t s = i;
            ast_t *n = id();
            expect_(TK_EQUAL, __LINE__);
            ast_t *v = expr();
            Location l = span(s);
            LIST_ADD(specs, EVENT_WAIT_KW_ARG(n, v, l));
            if (K[i] == TK_COMMA) {
                Vec<ast_t*> stats = sync_stat_list();
                for (size_t j = 0; j < stats.size(); j++) {
                    LIST_ADD(specs, stats[j]);
                }
            }
        }
        return specs;
    }

    Vec<ast_t*> decl_statements() {
        Vec<ast_t*> v;
        LIST_NEW(v);
        for (;;) {
            if (K[i] == KW_INSTANTIATE && !is_assignment()) {
                v = LIST_EXTEND(m_a, v, instantiate());
                continue;
            }
            if (K[i] == KW_DEFERRED && K[i+1] == KW_TYPE) {
                v = LIST_EXTEND(m_a, v, deferred_type_decl());
                continue;
            }
            ast_t *s = decl_statement();
            if (s == nullptr) return v;
            LIST_ADD(v, s);
        }
    }

    // "{" instantiate_symbol_list_opt "}"
    Vec<ast_t*> instantiate_symbols() {
        expect_(TK_LBRACE, __LINE__);
        Vec<ast_t*> v;
        LIST_NEW(v);
        if (K[i] != TK_RBRACE) {
            do {
                size_t s = i;
                if (is_name(K[i]) && K[i+1] == TK_EQUAL) {
                    ast_t *n = id();
                    i++;
                    ast_t *sym = instantiate_symbol();
                    Location l = span(s);
                    LIST_ADD(v, ATTR_KEYWORD(n, sym, l));
                } else {
                    LIST_ADD(v, instantiate_symbol());
                }
            } while (accept(TK_COMMA));
        }
        expect_(TK_RBRACE, __LINE__);
        return v;
    }

    ast_t* instantiate_symbol() {
        size_t s = i;
        Location l;
        if (K[i] == KW_OPERATOR && K[i+1] == TK_LPAREN) return decl_op();
        if (is_var_type_start()) {
            // A type wins over an expression if both are possible
            try {
                ast_t *vt = var_type();
                if (K[i] == TK_COMMA || K[i] == TK_RBRACE) return vt;
            } catch (const LLUnsupported &) {
            }
            i = s;
        }
        ast_t *e = expr();
        l = span(s);
        return instantiate_arg_expr(m_a, e, l);
    }

    Vec<ast_t*> instantiate() {
        size_t s = i;
        i++;
        Vec<ast_t*> v;
        LIST_NEW(v);
        Location l;
        if (accept(TK_DBL_COLON) && is_name(K[i]) && K[i+1] == TK_ARROW) {
            ast_t *local = id();
            i++;
            ast_t *n = id();
            Vec<ast_t*> args = instantiate_symbols();
            sep();
            l = span(s);
            LIST_ADD(v, INSTANTIATE_SUBP(n, local, args, l));
            return v;
        }
        ast_t *n = id();
        Vec<ast_t*> args = instantiate_symbols();
        if (accept(TK_COMMA)) {
            if (K[i] == KW_ONLY && K[i+1] == TK_COLON) {
                i += 2;
                Vec<ast_t*> syms = use_symbol_list();
                sep();
                l = span(s);
                LIST_ADD(v, INSTANTIATE2(n, args, syms, l));
                return v;
            }
            size_t s2 = i;
            Vec<ast_t*> syms = use_symbol_list();
            Location l2 = span(s2);
            sep();
            l = span(s);
            LIST_ADD(v, INSTANTIATE2(n, args, syms, l2));
            LIST_ADD(v, INSTANTIATE1(n, args, l));
            return v;
        }
        sep();
        l = span(s);
        LIST_ADD(v, INSTANTIATE1(n, args, l));
        return v;
    }

    Vec<ast_t*> deferred_type_decl() {
        size_t s = i;
        i += 2;
        Vec<ast_t*> attrs;
        bool has_attrs = false;
        if (K[i] == TK_COMMA) {
            LIST_NEW(attrs);
            has_attrs = true;
            while (accept(TK_COMMA)) {
                Location l2 = Lc[i];
                if (accept(KW_ABSTRACT)) {
                    LIST_ADD(attrs, SIMPLE_ATTR(Abstract, l2));
                } else {
                    expect_(KW_EXTENSIBLE, __LINE__);
                    LIST_ADD(attrs, SIMPLE_ATTR(Extensible, l2));
                }
            }
        }
        expect_(TK_DBL_COLON, __LINE__);
        Vec<ast_t*> ids = id_list();
        Vec<ast_t*> sp = sep();
        Location l = span(s);
        if (has_attrs) {
            return DEFERRED_TYPES(m_a, attrs.p, attrs.size(), ids,
                TRIVIA_AFTER(sp, l), l);
        }
        return DEFERRED_TYPES(m_a, nullptr, 0, ids, TRIVIA_AFTER(sp, l), l);
    }

    // deferred_const_decl and deferred_proc_decl
    ast_t* deferred_decl() {
        size_t s = i;
        i++;
        Location l;
        if (K[i] == KW_PROCEDURE && K[i+1] == TK_LPAREN
                && is_name(K[i+2]) && K[i+3] == TK_RPAREN
                && (K[i+4] == TK_DBL_COLON || is_name(K[i+4]))) {
            i += 2;
            ast_t *iface = id();
            i++;
            accept(TK_DBL_COLON);
            Vec<ast_t*> ids = id_list();
            Location l2 = span(s);
            Vec<ast_t*> sp = sep();
            l = span(s);
            return DEFERRED_PROCEDURE(iface, ids, TRIVIA_AFTER(sp, l), l2);
        }
        ast_t *vt = var_type();
        if (K[i] == TK_COMMA) {
            Vec<ast_t*> attrs;
            LIST_NEW(attrs);
            while (accept(TK_COMMA)) {
                size_t s2 = i;
                if (K[i] == KW_RANK && K[i+1] == TK_LPAREN) {
                    i += 2;
                    Vec<ast_t*> e = expr_list();
                    expect_(TK_RPAREN, __LINE__);
                    Location l2 = span(s2);
                    LIST_ADD(attrs, ATTR_RANK(e, l2));
                } else {
                    ast_t *m = var_modifier();
                    if (m == nullptr) unsup(__LINE__);
                    LIST_ADD(attrs, m);
                }
            }
            expect_(TK_DBL_COLON, __LINE__);
            Vec<var_sym_t> syms = var_sym_decl_list();
            l = span(s);
            Vec<ast_t*> sp = sep();
            return DEFERRED_CONST_DECL(m_a, vt, attrs, syms,
                TRIVIA_AFTER(sp, l), l);
        }
        expect_(TK_DBL_COLON, __LINE__);
        Vec<var_sym_t> syms = var_sym_decl_list();
        l = span(s);
        Vec<ast_t*> sp = sep();
        return DEFERRED_CONST_DECL_NOATTR(m_a, vt, syms, TRIVIA_AFTER(sp, l), l);
    }

    // A declaration with a label (not one that starts with an attribute),
    // or else a statement with a label
    ast_t* labeled_decl_or_statement() {
        size_t s = i;
        i++;
        ast_t *r = typed_decl(s);
        if (r != nullptr) return r;
        i = s;
        return statement();
    }

    ast_t* var_decl_or_statement() {
        size_t s = i;
        ast_t *r = var_decl();
        if (r != nullptr) return r;
        i = s;
        return statement();
    }

    var_sym_t* slash_name() {
        size_t s = i;
        expect_(TK_SLASH, __LINE__);
        ast_t *n = id();
        expect_(TK_SLASH, __LINE__);
        Location l = span(s);
        return VAR_SYM_NAME(n, None, l);
    }

    var_sym_t* plain_name() {
        size_t s = i;
        ast_t *n = id();
        Location l = span(s);
        return VAR_SYM_NAME(n, None, l);
    }

    Vec<namelist_group_t> namelist_group_list() {
        Vec<namelist_group_t> out;
        {
            var_sym_t *name = slash_name();
            var_sym_t *object = plain_name();
            NAMELIST_GROUP_1(out, name, object);
        }
        while (!is_sep(K[i])) {
            accept(TK_COMMA);
            Vec<namelist_group_t> groups = out;
            if (K[i] == TK_SLASH) {
                var_sym_t *name = slash_name();
                var_sym_t *object = plain_name();
                NAMELIST_GROUP_3(out, groups, name, object);
            } else {
                var_sym_t *object = plain_name();
                NAMELIST_GROUP_2(out, groups, object);
            }
        }
        return out;
    }

    var_sym_t* common_block_object() {
        size_t s = i;
        ast_t *n = id();
        Location l;
        if (accept(TK_LPAREN)) {
            Vec<dimension_t> d = array_comp_decl_list();
            expect_(TK_RPAREN, __LINE__);
            l = span(s);
            return VAR_SYM_DIM(n, d.p, d.n, None, l);
        }
        l = span(s);
        return VAR_SYM_NAME(n, None, l);
    }

    // common_block_start, if there is one here
    var_sym_t* common_block_start() {
        size_t s = i;
        Location l;
        if (accept(TK_CONCAT)) {
            l = span(s);
            return VAR_SYM_EMPTY(l);
        }
        if (!accept(TK_SLASH)) return nullptr;
        if (accept(TK_SLASH)) {
            l = span(s);
            return VAR_SYM_EMPTY(l);
        }
        ast_t *n = id();
        expect_(TK_SLASH, __LINE__);
        l = span(s);
        return VAR_SYM_NAME(n, None, l);
    }

    Vec<common_block_t> common_block_list() {
        size_t s = i;
        Vec<common_block_t> out;
        Location l;
        {
            var_sym_t *start = common_block_start();
            var_sym_t *object = common_block_object();
            l = span(s);
            if (start) {
                COMMON_BLOCK_2(out, start, object, l);
            } else {
                COMMON_BLOCK_1(out, object, l);
            }
        }
        while (!is_sep(K[i])) {
            accept(TK_COMMA);
            Vec<common_block_t> prev = out;
            var_sym_t *start = common_block_start();
            var_sym_t *object = common_block_object();
            l = span(s);
            if (start) {
                COMMON_BLOCK_5(out, prev, start, object, l);
            } else {
                COMMON_BLOCK_3(out, prev, object, l);
            }
        }
        return out;
    }

    // contains_block
    Vec<ast_t*> contains_block() {
        expect_(KW_CONTAINS, __LINE__);
        sep();
        Vec<ast_t*> v;
        LIST_NEW(v);
        for (;;) {
            ast_t *u = sub_or_func();
            if (u == nullptr) return v;
            LIST_ADD(v, u);
        }
    }

    // subroutine | function | procedure, with its prefixes; null (and
    // nothing consumed) if none starts here
    ast_t* sub_or_func() {
        size_t s = i;
        Vec<ast_t*> mods = fn_mods();
        if (K[i] == KW_TEMPLATE) {
            i++;
            Vec<ast_t*> mods2 = fn_mods();
            if (mods.size() == 0) {
                mods = mods2;
            } else if (mods2.size() > 0) {
                mods = concat_prefix(m_a, mods, mods2);
            }
            return templated_unit(s, mods);
        }
        ast_t *u;
        if (K[i] == KW_PROCEDURE && mods.size() > 0) {
            u = module_procedure(s, mods);
        } else {
            u = subprogram(s, mods);
        }
        if (u == nullptr) i = s;
        return u;
    }

    // A script_unit that is not a program unit: a declaration, a statement
    // or an expression at the top level of a file
    ast_t* top_level_item() {
        size_t s = i;
        Location l;
        if (K[i] == KW_END_PROGRAM && is_sep(K[i+1])) {
            i++;
            sep();
            l = span(s);
            return SYMBOL(V[s].string, l);
        }
        if (K[i] == TK_LABEL && (K[i+1] == KW_END || K[i+1] == KW_END_PROGRAM
                || K[i+1] == KW_ENDPROGRAM) && is_sep(K[i+2])) {
            i += 2;
            sep();
            l = span(s);
            return LABELED_END(V[s].n, V[s+1].string, l);
        }
        // An expression has a higher precedence here than a statement, so
        // a keyword alone on its line is a name (which
        // fix_program_without_program_line() turns into the statement)
        switch (K[i]) {
            case KW_STOP: case KW_RETURN: case KW_EXIT: case KW_CYCLE:
            case KW_CONTINUE: case KW_PAUSE: {
                if (is_sep(K[i+1])) {
                    ast_t *e = id();
                    sep();
                    return e;
                }
                break;
            }
            default: break;
        }
        switch (K[i]) {
            case KW_ALLOCATE: case KW_DEALLOCATE: case KW_NULLIFY:
            case KW_OPEN: case KW_CLOSE: case KW_REWIND: case KW_BACKSPACE:
            case KW_END_FILE: case KW_ENDFILE: case KW_FLUSH: case KW_INQUIRE:
            case KW_READ: case KW_WRITE: {
                // These statements keep no trivia at the top level
                if (is_assignment()) break;
                ast_t *st = statement1();
                if (st != nullptr && is_sep(K[i])) {
                    sep();
                    return st;
                }
                i = s;
                break;
            }
            default: break;
        }
        ast_t *r = decl_statement();
        if (r != nullptr) return r;
        ast_t *e = expr();
        sep();
        return e;
    }

    ast_t* subprogram_or_item() {
        ast_t *u = sub_or_func();
        if (u == nullptr) u = top_level_item();
        return u;
    }

    // The whole file
    void units() {
        if (is_sep(K[i])) sep();
        while (K[i] != END_OF_FILE) {
            RESULT(unit());
        }
    }

    // ------------------------------------------------------------------
    // Functions that the actions of the grammar call

    Vec<ast_t*> empty_list() {
        Vec<ast_t*> v;
        LIST_NEW(v);
        return v;
    }

    Vec<FnArg> empty_fnargs() {
        Vec<FnArg> v;
        LIST_NEW(v);
        return v;
    }

    Vec<ast_t*> one_item(ast_t *x) {
        Vec<ast_t*> v;
        LIST_NEW(v);
        LIST_ADD(v, x);
        return v;
    }

    // `x`, which has to be there
    ast_t* required(ast_t *x) {
        if (x == nullptr) unsup(__LINE__);
        return x;
    }

    ast_t* stmt_name(ast_t *first_name, ast_t *last_name, ast_t *st) {
        ast_t *r;
        r = STMT_NAME(first_name, last_name, st);
        return r;
    }

    void warn_endif(Location l) {
        WARN_ENDIF(l);
    }

    int64_t warn_enddo(Location l) {
        WARN_ENDDO(l);
        return 0;
    }

    // `type` (the old form `integer*4` of the type with the keyword `kw`),
    // after a warning at `l`
    ast_t* warn_star(int kw, const IntSuffix &v, Location l, ast_t *type) {
        switch (kw) {
            case KW_INTEGER: WARN_INTEGERSTAR(v, l); break;
            case KW_REAL: WARN_REALSTAR(v, l); break;
            case KW_COMPLEX: WARN_COMPLEXSTAR(v, l); break;
            case KW_LOGICAL: WARN_LOGICALSTAR(v, l); break;
            default: WARN_CHARACTERSTAR(v, l); break;
        }
        return type;
    }

    ast_t* warn_star_expr(Location l, ast_t *type) {
        WARN_CHARACTERSTAR_EXPR(l);
        return type;
    }

    ast_t* attr_type(decl_typeType type, Location &l) {
        return make_AttrType_t(m_a, l, type, nullptr, 0, nullptr, nullptr,
            None);
    }

    ast_t* attr_type(decl_typeType type, Vec<kind_item_t> &kind, Location &l) {
        return make_AttrType_t(m_a, l, type, kind.p, kind.size(), nullptr,
            nullptr, None);
    }

    ast_t* byte_type(Location &l) {
        ast_t *r = ATTR_TYPE_EXPR(Integer, INT1(l), l);
        WARN_BYTE(l);
        return r;
    }

    ast_t* stop_statement(const StopCode &c, Location &l) {
        if (c.code && c.quiet) return STOP3(c.code, c.quiet, l);
        if (c.quiet) return STOP2(c.quiet, l);
        if (c.code) return STOP1(c.code, l);
        return STOP(l);
    }

    ast_t* error_stop_statement(const StopCode &c, Location &l) {
        if (c.code && c.quiet) return ERROR_STOP3(c.code, c.quiet, l);
        if (c.quiet) return ERROR_STOP2(c.quiet, l);
        if (c.code) return ERROR_STOP1(c.code, l);
        return ERROR_STOP(l);
    }

    // An argument `name = value` or `name = *` (a null value) of an I/O
    // statement, and one without a name
    ArgStarKw* write_arg_keyword(ast_t *name, ast_t *value) {
        ArgStarKw *a;
        WRITE_ARG2(a, name, value);
        return a;
    }

    ArgStarKw* write_arg_value(ast_t *value) {
        ArgStarKw *a;
        WRITE_ARG1(a, value);
        return a;
    }

    // A counted DO loop; `c` is the step, or null
    ast_t* do_loop(ast_t *var, ast_t *a, ast_t *b, ast_t *c, ast_t *trivia,
            Vec<ast_t*> &body, int64_t end_label, Location &l) {
        if (c) return DO3(var, a, b, c, trivia, body, end_label, l);
        return DO2(var, a, b, trivia, body, end_label, l);
    }

    ast_t* do_loop_label(int64_t label, ast_t *var, ast_t *a, ast_t *b,
            ast_t *c, ast_t *trivia, Vec<ast_t*> &body, int64_t end_label,
            Location &l) {
        // The label of the END DO is not kept for a loop that has a label
        (void)end_label;
        ast_t *r;
        if (c) {
            r = DO3_LABEL(label, var, a, b, c, trivia, body, end_label, l);
        } else {
            r = DO2_LABEL(label, var, a, b, trivia, body, end_label, l);
        }
        return r;
    }

    ast_t* do_concurrent(ConcurrentHeader &h, Vec<ast_t*> &loc, ast_t *trivia,
            Vec<ast_t*> &body, Location &l) {
        if (h.mask) {
            return DO_CONCURRENT2(h.controls, h.mask, loc, trivia, body, l);
        }
        return DO_CONCURRENT1(h.controls, loc, trivia, body, l);
    }

    ast_t* forall_construct(ConcurrentHeader &h, Vec<ast_t*> &loc,
            ast_t *trivia, Vec<ast_t*> &body, Location &l) {
        if (h.mask) return FORALL2(h.controls, h.mask, loc, trivia, body, l);
        return FORALL1(h.controls, loc, trivia, body, l);
    }

    ast_t* forall_single(ConcurrentHeader &h, ast_t *st, Location &l) {
        if (h.mask) return FORALLSINGLE2(h.controls, h.mask, st, l);
        return FORALLSINGLE1(h.controls, st, l);
    }

    // The USE statement `u` with the trivia after it
    ast_t* use_trivia(ast_t *u, ast_t *trivia) {
        TRIVIA2_(u, trivia);
        return u;
    }

    ast_t* interface_module_proc(Vec<ast_t*> &mods, Vec<ast_t*> &ids,
            ast_t *trivia, Location &l) {
        if (mods.size() > 0) {
            return INTERFACE_MODULE_PROC1(mods, ids, trivia, l);
        }
        return INTERFACE_MODULE_PROC(ids, trivia, l);
    }

    // The interface body `u` of a subroutine or a function
    ast_t* interface_proc(ast_t *u) {
        if (u == nullptr) unsup(__LINE__);
        return INTERFACE_PROC(u, u->loc);
    }

    // A subroutine and a function, with their prefixes `mods` if any
    ast_t* subroutine(Vec<ast_t*> &mods, ast_t *name, Vec<ast_t*> &args,
            ast_t *b, ast_t *trivia, Vec<ast_t*> &decls, ContainsEnd *ce,
            Location &l) {
        if (mods.size() > 0) {
            return SUBROUTINE1(mods, name, args, b, trivia, decls, ce, l);
        }
        return SUBROUTINE(name, args, b, trivia, decls, ce, l);
    }

    ast_t* function(Vec<ast_t*> &mods, ast_t *name, Vec<ast_t*> &args,
            const FunctionClauses &c, ast_t *trivia, Vec<ast_t*> &decls,
            ContainsEnd *ce, Location &l) {
        if (mods.size() > 0) {
            return FUNCTION(mods, name, args, c.result, c.bind, trivia, decls,
                ce, l);
        }
        return FUNCTION0(name, args, c.result, c.bind, trivia, decls, ce, l);
    }
};

} // namespace

namespace LCompilers::LFortran {

// Defined in parser.cpp
bool is_program_needed(AST::TranslationUnit_t &ast);
void fix_program_without_program_line(Allocator &al,
    AST::TranslationUnit_t &ast, diag::Diagnostics &diagnostics);

void ll_tokenize(Allocator &al, const std::string &input,
        diag::Diagnostics &diagnostics, LLTokens &t, bool openmp,
        uint32_t loc_offset)
{
    Tokenizer tok;
    tok.openmp_enabled = openmp;
    tok.loc_offset = loc_offset;
    // The tokenizer needs the input to end with a newline
    const std::string *inp = &input;
    if (input.empty() || input.back() != '\n') {
        t.input = input;
        t.input.append("\n");
        inp = &t.input;
    }
    tok.set_string(*inp);
    // A token takes at least one character; the arrays end with several
    // END_OF_FILE tokens, so that the parser can look ahead without checks
    size_t max = inp->size() + 8;
    if (t.kind.size() < max) {
        t.kind.resize(max);
        t.value.resize(max);
        t.loc.resize(max);
    }
    int *K = t.kind.data();
    YYSTYPE *V = t.value.data();
    Location *L = t.loc.data();
    size_t n = 0;
    for (;;) {
        int k = tok.lex(al, V[n], L[n], diagnostics, false);
        K[n] = k;
        n++;
        if (k == END_OF_FILE) break;
    }
    for (size_t j = 0; j < 4; j++) {
        K[n+j] = END_OF_FILE;
        L[n+j] = L[n-1];
    }
    t.n = n;
}

AST::TranslationUnit_t* ll_parse(Allocator &al, const std::string &input,
        diag::Diagnostics &diagnostics, LLTokens &tokens, bool openmp,
        uint32_t loc_offset)
{
    ll_tokenize(al, input, diagnostics, tokens, openmp, loc_offset);
    LL ll(al, diagnostics, tokens.kind.data(), tokens.value.data(),
        tokens.loc.data());
    ll.units();
    Location l;
    if (ll.result.size() == 0) {
        l.first = 0;
        l.last = 0;
    } else {
        l.first = ll.result[0]->loc.first;
        l.last = ll.result[ll.result.size()-1]->loc.last;
    }
    AST::TranslationUnit_t *ast = (AST::TranslationUnit_t*)
        AST::make_TranslationUnit_t(al, l, ll.result.p, ll.result.size());
    if (is_program_needed(*ast)) {
        fix_program_without_program_line(al, *ast, diagnostics);
    }
    return ast;
}

AST::TranslationUnit_t* ll_parse_or_null(Allocator &al,
        const std::string &input, diag::Diagnostics &diagnostics, bool openmp,
        uint32_t loc_offset)
{
    // The diagnostics are kept apart until the parse succeeds, because the
    // caller parses the input again if it does not
    diag::Diagnostics d;
    LLTokens tokens;
    AST::TranslationUnit_t *ast;
    try {
        ast = ll_parse(al, input, d, tokens, openmp, loc_offset);
    } catch (const LLUnsupported &) {
        return nullptr;
    } catch (const parser_local::TokenizerAbort &) {
        return nullptr;
    } catch (const parser_local::ParserAbort &) {
        return nullptr;
    } catch (const LCompilersException &) {
        return nullptr;
    }
    if (d.has_error()) return nullptr;
    for (auto &x : d.diagnostics) {
        diagnostics.diagnostics.push_back(x);
    }
    return ast;
}

} // namespace LCompilers::LFortran
