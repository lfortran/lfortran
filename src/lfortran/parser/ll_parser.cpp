/*
Prototype of an LL parser for free-form Fortran.

It is a recursive descent parser, written by hand in the style that a
generator would emit from an LL grammar with predicates:

* The whole file is tokenized first, with the same re2c tokenizer as the Bison
  parser, into arrays. A keyword is just a kind of name: `is_name()` is a range
  check, a keyword test is an integer comparison.
* Each grammar rule is one function. A choice is a `switch` on the token kind
  or a test of the next one or two tokens.
* A few predicates look further ahead. The main one is `is_assignment()`,
  which decides at the start of a statement whether it is an assignment (so
  the first name is a variable, even if it spells a keyword) from the shape of
  the tokens before `=`. The others scan a parenthesized or bracketed list for
  `::`.
* The AST is built by the same macros and functions as the Bison parser
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

    // ------------------------------------------------------------------
    // Predicate

    // Is the statement that starts at the current token (a name) an
    // assignment? It is if a designator is followed by `=` or `=>`:
    //     name [ (...) ]... [ % name [ (...) ]... ]... =
    bool is_assignment() const {
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

    // ------------------------------------------------------------------
    // Names and separators

    ast_t* id() {
        if (!is_name(K[i])) unsup(__LINE__);
        Location l = Lc[i];
        ast_t *r = SYMBOL(V[i].string, l);
        i++;
        return r;
    }

    void id_opt() { if (is_name(K[i])) id(); }

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

    // ------------------------------------------------------------------
    // Expressions: precedence climbing

    static int binary_precedence(int t) {
        switch (t) {
            case TK_DEF_OP: return 1;
            case TK_EQV: case TK_NEQV: return 2;
            case TK_OR: case TK_XOR: return 3;
            case TK_AND: return 4;
            case TK_EQ: case TK_NE: case TK_LT: case TK_LE: case TK_GT:
            case TK_GE: return 6;
            case TK_CONCAT: return 7;
            case TK_PLUS: case TK_MINUS: return 8;
            case TK_STAR: case TK_SLASH: return 9;
            case TK_POW: return 11;
            default: return 0;
        }
    }

    ast_t* expr() { return binary(1); }

    ast_t* binary(int min_prec) {
        size_t s = i;
        ast_t *x = prefix();
        for (;;) {
            int t = K[i];
            int prec = binary_precedence(t);
            if (prec == 0 || prec < min_prec) return x;
            size_t op = i;
            i++;
            // `**` is right associative, the others are left associative
            ast_t *y = binary(prec == 11 ? 11 : prec + 1);
            Location l = span(s);
            switch (t) {
                case TK_PLUS: x = ADD(x, y, l); break;
                case TK_MINUS: x = SUB(x, y, l); break;
                case TK_STAR: x = MUL(x, y, l); break;
                case TK_SLASH: x = DIV(x, y, l); break;
                case TK_POW: x = POW(x, y, l); break;
                case TK_CONCAT: x = STRCONCAT(x, y, l); break;
                case TK_EQ: x = EQ(x, y, l); break;
                case TK_NE: x = NE(x, y, l); break;
                case TK_LT: x = LT(x, y, l); break;
                case TK_LE: x = LE(x, y, l); break;
                case TK_GT: x = GT(x, y, l); break;
                case TK_GE: x = GE(x, y, l); break;
                case TK_AND: x = AND(x, y, l); break;
                case TK_OR: x = OR(x, y, l); break;
                case TK_XOR: x = XOR(x, y, l); break;
                case TK_EQV: x = EQV(x, y, l); break;
                case TK_NEQV: x = NEQV(x, y, l); break;
                default: x = DEFOP(x, V[op].string, y, l); break;
            }
        }
    }

    ast_t* prefix() {
        size_t s = i;
        switch (K[i]) {
            case TK_MINUS: {
                i++;
                ast_t *x = binary(11);
                Location l = span(s);
                return UNARY_MINUS(x, l);
            }
            case TK_PLUS: {
                i++;
                ast_t *x = binary(11);
                Location l = span(s);
                return UNARY_PLUS(x, l);
            }
            case TK_NOT: {
                i++;
                ast_t *x = binary(6);
                Location l = span(s);
                return NOT(x, l);
            }
            case TK_DEF_OP: {
                i++;
                ast_t *x = primary();
                Location l = span(s);
                return UNARY_DEFOP(V[s].string, x, l);
            }
            default: return primary();
        }
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

    ast_t* primary() {
        size_t s = i;
        int t = K[i];
        if (is_name(t)) return designator();
        Location l = Lc[s];
        switch (t) {
            case TK_INTEGER: i++; return INTEGER(V[s].int_suffix, l);
            case TK_REAL: i++; return REAL(V[s].string, l);
            case TK_STRING: {
                i++;
                if (accept(TK_LPAREN)) {
                    Vec<FnArg> args = fnarray_arg_list_opt();
                    expect_(TK_RPAREN, __LINE__);
                    l = span(s);
                    return SUBSTRING(V[s].str_prefix, args, l);
                }
                return STRING(V[s].str_prefix, l);
            }
            case TK_BOZ_CONSTANT: i++; return BOZ(V[s].string, l);
            case TK_TRUE: i++; return TRUE(V[s].string, l);
            case TK_FALSE: i++; return FALSE(V[s].string, l);
            case TK_LPAREN: return parenthesized();
            case TK_LBRACKET: {
                i++;
                Vec<ast_t*> v;
                if (array_constructor_has_type()) {
                    if (is_var_type_start()) {
                        ast_t *vt = var_type();
                        expect_(TK_DBL_COLON, __LINE__);
                        v = expr_list_opt_bracket();
                        l = span(s);
                        return ARRAY_IN2(vt, v, l);
                    }
                    ast_t *n = id();
                    expect_(TK_DBL_COLON, __LINE__);
                    v = expr_list_opt_bracket();
                    l = span(s);
                    return ARRAY_IN3(n, v, l);
                }
                v = expr_list_opt_bracket();
                l = span(s);
                return ARRAY_IN1(v, l);
            }
            default: unsup(__LINE__);
        }
    }

    // expr_list_opt rbracket
    Vec<ast_t*> expr_list_opt_bracket() {
        Vec<ast_t*> v;
        if (K[i] == TK_RBRACKET || K[i] == TK_RBRACKET_OLD) {
            LIST_NEW(v);
        } else {
            v = expr_list();
        }
        if (!accept(TK_RBRACKET)) expect_(TK_RBRACKET_OLD, __LINE__);
        return v;
    }

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

    Vec<ast_t*> expr_list() {
        Vec<ast_t*> v;
        LIST_NEW(v);
        do {
            LIST_ADD(v, expr());
        } while (accept(TK_COMMA));
        return v;
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

    Vec<FnArg> fnarray_arg_list_opt() {
        Vec<FnArg> v;
        LIST_NEW(v);
        if (K[i] == TK_RPAREN) return v;
        do {
            FnArg *a = fnarray_arg();
            PLIST_ADD(v, a);
        } while (accept(TK_COMMA));
        return v;
    }

    bool at_arg_end() const {
        return K[i] == TK_COMMA || K[i] == TK_RPAREN || K[i] == TK_RBRACKET;
    }

    FnArg* fnarray_arg() {
        size_t s = i;
        Location l;
        if (is_name(K[i]) && K[i+1] == TK_EQUAL) {
            ast_t *n = id();
            i++;
            ast_t *e = expr();
            l = span(s);
            return ARRAY_COMP_DECL1k(n, e, l);
        }
        if (K[i] == TK_STAR) {
            i++;
            if (K[i] != TK_INTEGER) unsup(__LINE__);
            const YYSTYPE &v = V[i];
            i++;
            l = span(s);
            return ARRAY_COMP_DECL_label(v.int_suffix, l);
        }
        if (accept(TK_DBL_COLON)) {
            ast_t *c = expr();
            l = span(s);
            return ARRAY_COMP_DECL_00c(c, l);
        }
        if (accept(TK_COLON)) {
            if (accept(TK_COLON)) {
                ast_t *c = expr();
                l = span(s);
                return ARRAY_COMP_DECL_00c(c, l);
            }
            if (at_arg_end()) {
                l = span(s);
                return ARRAY_COMP_DECL_001(l);
            }
            ast_t *b = expr();
            if (accept(TK_COLON)) {
                ast_t *c = expr();
                l = span(s);
                return ARRAY_COMP_DECL_0bc(b, c, l);
            }
            l = span(s);
            return ARRAY_COMP_DECL_0b1(b, l);
        }
        ast_t *a = expr();
        if (accept(TK_DBL_COLON)) {
            ast_t *c = expr();
            l = span(s);
            return ARRAY_COMP_DECL_a0c(a, c, l);
        }
        if (accept(TK_COLON)) {
            if (at_arg_end()) {
                l = span(s);
                return ARRAY_COMP_DECL_a01(a, l);
            }
            if (accept(TK_COLON)) {
                ast_t *c = expr();
                l = span(s);
                return ARRAY_COMP_DECL_a0c(a, c, l);
            }
            ast_t *b = expr();
            if (accept(TK_COLON)) {
                ast_t *c = expr();
                l = span(s);
                return ARRAY_COMP_DECL_abc(a, b, c, l);
            }
            l = span(s);
            return ARRAY_COMP_DECL_ab1(a, b, l);
        }
        l = span(s);
        return ARRAY_COMP_DECL_0i0(a, l);
    }

    Vec<CoarrayArg> coarray_arg_list() {
        Vec<CoarrayArg> v;
        LIST_NEW(v);
        do {
            CoarrayArg *a = coarray_arg();
            PLIST_ADD(v, a);
        } while (accept(TK_COMMA));
        return v;
    }

    CoarrayArg* coarray_arg() {
        size_t s = i;
        Location l;
        if (is_name(K[i]) && K[i+1] == TK_EQUAL) {
            ast_t *n = id();
            i++;
            ast_t *e = expr();
            l = span(s);
            return COARRAY_COMP_DECL1k(n, e, l);
        }
        if (accept(TK_STAR)) {
            l = span(s);
            return COARRAY_COMP_DECL_star(l);
        }
        if (accept(TK_COLON)) {
            if (at_arg_end()) {
                l = span(s);
                return COARRAY_COMP_DECL_001(l);
            }
            ast_t *b = expr();
            l = span(s);
            return COARRAY_COMP_DECL_0b1(b, l);
        }
        ast_t *a = expr();
        if (accept(TK_COLON)) {
            if (at_arg_end()) {
                l = span(s);
                return COARRAY_COMP_DECL_a01(a, l);
            }
            if (accept(TK_STAR)) {
                l = span(s);
                return COARRAY_COMP_DECL_astar(a, l);
            }
            ast_t *b = expr();
            l = span(s);
            return COARRAY_COMP_DECL_ab1(a, b, l);
        }
        l = span(s);
        return COARRAY_COMP_DECL_0i0(a, l);
    }

    // ------------------------------------------------------------------
    // Statements

    Vec<ast_t*> statements() {
        Vec<ast_t*> v;
        LIST_NEW(v);
        for (;;) {
            ast_t *s = statement();
            if (s == nullptr) return v;
            LIST_ADD(v, s);
        }
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

    ast_t* stmt_name(ast_t *first_name, ast_t *last_name, ast_t *st) {
        ast_t *r;
        r = STMT_NAME(first_name, last_name, st);
        return r;
    }

    ast_t* assignment_statement() {
        size_t s = i;
        if (!is_name(K[i]) || !is_assignment()) unsup(__LINE__);
        ast_t *x = designator();
        int op = K[i];
        i++;
        ast_t *y = expr();
        Location l = span(s);
        if (op == TK_ARROW) return ASSOCIATE(x, y, l);
        if (op == TK_COLON_EQUAL) return INFER_ASSIGNMENT(x, y, l);
        return ASSIGNMENT(x, y, l);
    }

    // "(" write_arg_list ")"
    Vec<ArgStarKw> write_args() {
        expect_(TK_LPAREN, __LINE__);
        Vec<ArgStarKw> v;
        LIST_NEW(v);
        do {
            ArgStarKw *a;
            if (is_name(K[i]) && K[i+1] == TK_EQUAL) {
                ast_t *n = id();
                i++;
                ast_t *e = accept(TK_STAR) ? nullptr : expr();
                WRITE_ARG2(a, n, e);
            } else {
                ast_t *e = accept(TK_STAR) ? nullptr : expr();
                WRITE_ARG1(a, e);
            }
            PLIST_ADD(v, a);
        } while (accept(TK_COMMA));
        expect_(TK_RPAREN, __LINE__);
        return v;
    }

    // [","] sync_stat ["," sync_stat]...
    Vec<ast_t*> sync_stat_list() {
        Vec<ast_t*> v;
        LIST_NEW(v);
        accept(TK_COMMA);
        do {
            size_t s = i;
            int t = K[i];
            i++;
            expect_(TK_EQUAL, __LINE__);
            Location l;
            if (t == KW_STAT) {
                ast_t *n = id();
                l = span(s);
                LIST_ADD(v, STAT(n, l));
            } else if (t == KW_ERRMSG) {
                ast_t *n = id();
                l = span(s);
                LIST_ADD(v, ERRMSG(n, l));
            } else if (t == KW_NEW_INDEX) {
                ast_t *e = expr();
                l = span(s);
                LIST_ADD(v, NEW_INDEX(e, l));
            } else {
                unsup(__LINE__);
            }
        } while (accept(TK_COMMA));
        return v;
    }

    // Returns nullptr (the position is then undefined) if no statement
    // starts at the current token
    ast_t* statement1() {
        size_t s = i;
        int t = K[i];
        if (is_name(t)) {
            if (is_assignment()) return assignment_statement();
            if (K[i+1] == TK_COLON) {
                // A named construct: name: construct name
                ast_t *n1 = id();
                i++;
                ast_t *st = statement1();
                if (st == nullptr) unsup(__LINE__);
                ast_t *n2 = id();
                return stmt_name(n1, n2, st);
            }
        }
        Location l;
        switch (t) {
            case KW_IF: return if_statement();
            case KW_DO: case KW_DOWHILE: return do_statement();
            case KW_PRINT: {
                i++;
                ast_t *fmt = nullptr;
                if (!accept(TK_STAR)) fmt = expr();
                if (accept(TK_COMMA) && !is_sep(K[i])) {
                    Vec<ast_t*> args = expr_list();
                    l = span(s);
                    return PRINT(fmt, args, l);
                }
                l = span(s);
                return PRINT0(fmt, l);
            }
            case KW_CALL: {
                i++;
                return designator(true, s);
            }
            case KW_EXIT: {
                i++;
                if (is_name(K[i])) {
                    ast_t *n = id();
                    l = span(s);
                    return EXIT2(n, l);
                }
                l = span(s);
                return EXIT(l);
            }
            case KW_CYCLE: {
                i++;
                if (is_name(K[i])) {
                    ast_t *n = id();
                    l = span(s);
                    return CYCLE2(n, l);
                }
                l = span(s);
                return CYCLE(l);
            }
            case KW_CONTINUE: {
                i++;
                l = span(s);
                return CONTINUE(l);
            }
            case KW_RETURN: {
                i++;
                if (!is_sep(K[i])) {
                    ast_t *e = expr();
                    l = span(s);
                    return RETURN1(e, l);
                }
                l = span(s);
                return RETURN(l);
            }
            case KW_STOP: {
                i++;
                return stop_rest(s, false);
            }
            case KW_ERROR: {
                if (K[i+1] != KW_STOP) return nullptr;
                i += 2;
                return stop_rest(s, true);
            }
            case KW_PAUSE: {
                i++;
                if (!is_sep(K[i])) {
                    ast_t *e = expr();
                    l = span(s);
                    return make_Pause_t(m_a, l, 0, EXPR(e), nullptr);
                }
                l = span(s);
                return make_Pause_t(m_a, l, 0, nullptr, nullptr);
            }
            case KW_ALLOCATE: {
                i++;
                expect_(TK_LPAREN, __LINE__);
                Vec<FnArg> args = fnarray_arg_list_opt();
                expect_(TK_RPAREN, __LINE__);
                l = span(s);
                return ALLOCATE_STMT(args, l);
            }
            case KW_DEALLOCATE: {
                i++;
                expect_(TK_LPAREN, __LINE__);
                Vec<FnArg> args = fnarray_arg_list_opt();
                expect_(TK_RPAREN, __LINE__);
                l = span(s);
                return DEALLOCATE_STMT(args, l);
            }
            case KW_NULLIFY: {
                i++;
                Vec<ArgStarKw> a = write_args();
                l = span(s);
                return NULLIFY(a, l);
            }
            case KW_OPEN: {
                i++;
                Vec<ArgStarKw> a = write_args();
                l = span(s);
                return OPEN(a, l);
            }
            case KW_CLOSE: {
                i++;
                Vec<ArgStarKw> a = write_args();
                l = span(s);
                return CLOSE(a, l);
            }
            case KW_WRITE: {
                i++;
                Vec<ArgStarKw> a = write_args();
                if (is_sep(K[i])) {
                    l = span(s);
                    return WRITE0(a, l);
                }
                accept(TK_COMMA);
                Vec<ast_t*> args = expr_list();
                l = span(s);
                return WRITE(a, args, l);
            }
            case KW_READ: {
                i++;
                if (K[i] == TK_LPAREN) {
                    Vec<ArgStarKw> a = write_args();
                    if (is_sep(K[i])) {
                        l = span(s);
                        return READ0(a, l);
                    }
                    accept(TK_COMMA);
                    Vec<ast_t*> args = expr_list();
                    l = span(s);
                    return READ(a, args, l);
                }
                if (K[i] == TK_INTEGER) {
                    const YYSTYPE &v = V[i];
                    i++;
                    if (accept(TK_COMMA)) {
                        Vec<ast_t*> args = expr_list();
                        l = span(s);
                        return READ2(v.int_suffix, args, l);
                    }
                    l = span(s);
                    return READ4(v.int_suffix, l);
                }
                if (K[i] == TK_STAR) {
                    i++;
                    if (accept(TK_COMMA)) {
                        Vec<ast_t*> args;
                        if (is_sep(K[i])) {
                            LIST_NEW(args);
                        } else {
                            args = expr_list();
                        }
                        l = span(s);
                        return READ3(args, l);
                    }
                    l = span(s);
                    return READ6(l);
                }
                if (K[i] == TK_STRING) {
                    const YYSTYPE &v = V[i];
                    i++;
                    expect_(TK_COMMA, __LINE__);
                    Vec<ast_t*> args = expr_list();
                    l = span(s);
                    return READ5(v.str_prefix, args, l);
                }
                return nullptr;
            }
            case KW_INQUIRE: {
                i++;
                Vec<ArgStarKw> a = write_args();
                if (is_sep(K[i])) {
                    l = span(s);
                    return INQUIRE0(a, l);
                }
                Vec<ast_t*> args = expr_list();
                l = span(s);
                return INQUIRE(a, args, l);
            }
            case KW_REWIND: case KW_BACKSPACE: case KW_FLUSH:
            case KW_END_FILE: case KW_ENDFILE: {
                i++;
                if (K[i] == TK_LPAREN) {
                    Vec<ArgStarKw> a = write_args();
                    l = span(s);
                    if (t == KW_REWIND) return REWIND(a, l);
                    if (t == KW_BACKSPACE) return BACKSPACE(a, l);
                    if (t == KW_FLUSH) return FLUSH(a, l);
                    return ENDFILE(a, l);
                }
                ast_t *arg;
                if (K[i] == TK_INTEGER) {
                    const YYSTYPE &v = V[i];
                    i++;
                    l = span(s);
                    arg = INTEGER(v.int_suffix, l);
                } else {
                    ast_t *n = id();
                    if (accept(TK_LPAREN)) {
                        Vec<FnArg> args = fnarray_arg_list_opt();
                        expect_(TK_RPAREN, __LINE__);
                        l = span(s);
                        arg = FUNCCALLORARRAY(n, args, l);
                    } else {
                        l = span(s);
                        arg = n;
                    }
                }
                if (t == KW_REWIND) return REWIND2(arg, l);
                if (t == KW_BACKSPACE) return BACKSPACE2(arg, l);
                if (t == KW_FLUSH) return FLUSH2(arg, l);
                return ENDFILE2(arg, l);
            }
            case KW_INCLUDE: {
                i++;
                if (K[i] != TK_STRING) return nullptr;
                const YYSTYPE &v = V[i];
                i++;
                l = span(s);
                return INCLUDE(v.str_prefix, l);
            }
            case KW_ASSIGN: {
                if (K[i+1] != TK_INTEGER) return nullptr;
                const YYSTYPE &v = V[i+1];
                i += 2;
                expect_(KW_TO, __LINE__);
                ast_t *n = id();
                l = span(s);
                return ASSIGN(INTEGER3(v.int_suffix), n, l);
            }
            case KW_GO: {
                if (K[i+1] != KW_TO) return nullptr;
                i += 2;
                return goto_rest(s);
            }
            case KW_GOTO: {
                i++;
                return goto_rest(s);
            }
            case KW_DATA: {
                i++;
                Vec<ast_t*> sets;
                LIST_NEW(sets);
                for (;;) {
                    LIST_ADD(sets, data_set());
                    if (accept(TK_COMMA)) continue;
                    if (is_sep(K[i])) break;
                }
                l = span(s);
                return DATASTMT(sets, l);
            }
            case TK_FORMAT: {
                i++;
                l = span(s);
                return FORMAT(V[s].string, l);
            }
            case TK_OMP: {
                i++;
                l = span(s);
                return OMP_PRAGMA2(m_a, l, V[s].string.c_str(m_a));
            }
            case TK_OMP_END: {
                i++;
                l = span(s);
                return OMP_PRAGMA(V[s].string, l);
            }
            case KW_ENTRY: {
                i++;
                ast_t *n = id();
                Vec<ast_t*> args = sub_args();
                ast_t *b = nullptr, *r = nullptr;
                bool bind_first = (K[i] == KW_BIND);
                bool any = false;
                for (;;) {
                    if (K[i] == KW_BIND && b == nullptr) {
                        b = bind();
                        any = true;
                    } else if (K[i] == KW_RESULT && r == nullptr) {
                        r = result_clause();
                        any = true;
                    } else {
                        break;
                    }
                }
                l = span(s);
                if (!any) return ENTRY1(n, args, l);
                if (bind_first) return ENTRY2(n, args, b, r, l);
                return ENTRY3(n, args, r, b, l);
            }
            case KW_WHERE: return where_statement();
            case KW_FORALL: return forall_statement();
            case KW_SELECT: {
                if (K[i+1] == KW_CASE) {
                    i += 2;
                    return select_case(s);
                }
                if (K[i+1] == KW_TYPE) {
                    i += 2;
                    return select_type(s);
                }
                if (K[i+1] == KW_RANK) {
                    i += 2;
                    return select_rank(s);
                }
                return nullptr;
            }
            case KW_SELECT_CASE: i++; return select_case(s);
            case KW_SELECT_TYPE: i++; return select_type(s);
            case KW_SELECT_RANK: i++; return select_rank(s);
            case KW_ASSOCIATE: {
                if (K[i+1] != TK_LPAREN) return nullptr;
                i += 2;
                Vec<var_sym_t> syms = var_sym_decl_list();
                expect_(TK_RPAREN, __LINE__);
                Vec<ast_t*> sp = sep();
                Vec<ast_t*> body = statements();
                if (!accept(KW_END_ASSOCIATE)) expect_(KW_ENDASSOCIATE, __LINE__);
                l = span(s);
                return ASSOCIATE_BLOCK(syms, TRIVIA_AFTER(sp, l), body, l);
            }
            case KW_BLOCK: {
                if (!is_sep(K[i+1])) return nullptr;
                i++;
                Vec<ast_t*> sp = sep();
                Vec<ast_t*> decls = decl_statements();
                if (!accept(KW_END_BLOCK)) expect_(KW_ENDBLOCK, __LINE__);
                l = span(s);
                return BLOCK(TRIVIA_AFTER(sp, l), decls, l);
            }
            case KW_CRITICAL: {
                i++;
                if (accept(TK_LPAREN)) {
                    if (accept(TK_RPAREN)) {
                        Vec<ast_t*> sp = sep();
                        Vec<ast_t*> body = statements();
                        if (!accept(KW_END_CRITICAL)) expect_(KW_ENDCRITICAL, __LINE__);
                        l = span(s);
                        return CRITICAL1(TRIVIA_AFTER(sp, l), body, l);
                    }
                    Vec<ast_t*> stats = sync_stat_list();
                    expect_(TK_RPAREN, __LINE__);
                    Vec<ast_t*> sp = sep();
                    Vec<ast_t*> body = statements();
                    if (!accept(KW_END_CRITICAL)) expect_(KW_ENDCRITICAL, __LINE__);
                    l = span(s);
                    return CRITICAL2(stats, TRIVIA_AFTER(sp, l), body, l);
                }
                if (!is_sep(K[i])) return nullptr;
                Vec<ast_t*> sp = sep();
                Vec<ast_t*> body = statements();
                if (!accept(KW_END_CRITICAL)) expect_(KW_ENDCRITICAL, __LINE__);
                l = span(s);
                return CRITICAL(TRIVIA_AFTER(sp, l), body, l);
            }
            case KW_SYNC: case KW_SYNC_ALL: case KW_SYNC_IMAGES:
            case KW_SYNC_MEMORY: case KW_SYNC_TEAM:
                return sync_statement();
            case KW_FORM: case KW_FORM_TEAM: {
                i++;
                if (t == KW_FORM && !accept(KW_TEAM)) return nullptr;
                if (!accept(TK_LPAREN)) return nullptr;
                ast_t *e = expr();
                expect_(TK_COMMA, __LINE__);
                ast_t *n = id();
                if (accept(TK_RPAREN)) {
                    l = span(s);
                    return FORMTEAM1(e, n, l);
                }
                Vec<ast_t*> stats = sync_stat_list();
                expect_(TK_RPAREN, __LINE__);
                l = span(s);
                return FORMTEAM2(e, n, stats, l);
            }
            case KW_CHANGE: case KW_CHANGE_TEAM: {
                i++;
                if (t == KW_CHANGE && !accept(KW_TEAM)) return nullptr;
                if (!accept(TK_LPAREN)) return nullptr;
                ast_t *e = expr();
                Vec<ast_t*> assoc;
                LIST_NEW(assoc);
                while (K[i] == TK_COMMA && is_name(K[i+1])
                        && K[i+2] == TK_LBRACKET) {
                    i++;
                    size_t s2 = i;
                    ast_t *n = id();
                    i++;
                    Vec<CoarrayArg> co = coarray_arg_list();
                    expect_(TK_RBRACKET, __LINE__);
                    expect_(TK_ARROW, __LINE__);
                    ast_t *e2 = expr();
                    Location l2 = span(s2);
                    LIST_ADD(assoc, COARRAY_ASSOC(n, co, e2, l2));
                }
                Vec<ast_t*> stats;
                bool has_stats = false;
                if (K[i] != TK_RPAREN) {
                    stats = sync_stat_list();
                    has_stats = true;
                }
                expect_(TK_RPAREN, __LINE__);
                Vec<ast_t*> sp = sep();
                Vec<ast_t*> body = statements();
                if (!accept(KW_END_TEAM)) expect_(KW_ENDTEAM, __LINE__);
                if (accept(TK_LPAREN)) {
                    Vec<ast_t*> stats2 = sync_stat_list();
                    expect_(TK_RPAREN, __LINE__);
                    l = span(s);
                    if (has_stats) {
                        return CHANGETEAM4(e, assoc, stats, TRIVIA_AFTER(sp, l),
                            body, stats2, l);
                    }
                    return CHANGETEAM2(e, assoc, TRIVIA_AFTER(sp, l), body,
                        stats2, l);
                }
                l = span(s);
                if (has_stats) {
                    return CHANGETEAM3(e, assoc, stats, TRIVIA_AFTER(sp, l),
                        body, l);
                }
                return CHANGETEAM1(e, assoc, TRIVIA_AFTER(sp, l), body, l);
            }
            case KW_EVENT: {
                int u = K[i+1];
                if ((u != KW_POST && u != KW_WAIT) || K[i+2] != TK_LPAREN) {
                    return nullptr;
                }
                i += 3;
                ast_t *e = expr();
                if (accept(TK_RPAREN)) {
                    l = span(s);
                    if (u == KW_POST) return EVENT_POST(e, l);
                    return EVENT_WAIT(e, l);
                }
                expect_(TK_COMMA, __LINE__);
                Vec<ast_t*> specs;
                if (u == KW_POST) {
                    specs = sync_stat_list();
                    expect_(TK_RPAREN, __LINE__);
                    l = span(s);
                    return EVENT_POST1(e, specs, l);
                }
                LIST_NEW(specs);
                if (K[i] != TK_RPAREN) {
                    size_t s2 = i;
                    ast_t *n = id();
                    expect_(TK_EQUAL, __LINE__);
                    ast_t *v = expr();
                    Location l2 = span(s2);
                    LIST_ADD(specs, EVENT_WAIT_KW_ARG(n, v, l2));
                    if (K[i] == TK_COMMA) {
                        Vec<ast_t*> stats = sync_stat_list();
                        for (size_t j = 0; j < stats.size(); j++) {
                            LIST_ADD(specs, stats[j]);
                        }
                    }
                }
                expect_(TK_RPAREN, __LINE__);
                l = span(s);
                return EVENT_WAIT1(e, specs, l);
            }
            default: return nullptr;
        }
    }

    ast_t* sync_statement() {
        size_t s = i;
        int t = K[i];
        i++;
        if (t == KW_SYNC) {
            t = K[i];
            i++;
            if (t == KW_ALL) {
                t = KW_SYNC_ALL;
            } else if (t == KW_IMAGES) {
                t = KW_SYNC_IMAGES;
            } else if (t == KW_MEMORY) {
                t = KW_SYNC_MEMORY;
            } else if (t == KW_TEAM) {
                t = KW_SYNC_TEAM;
            } else {
                return nullptr;
            }
        }
        Location l;
        if (t == KW_SYNC_ALL || t == KW_SYNC_MEMORY) {
            if (!accept(TK_LPAREN)) {
                l = span(s);
                if (t == KW_SYNC_ALL) return SYNC_ALL(l);
                return SYNC_MEMORY(l);
            }
            if (accept(TK_RPAREN)) {
                l = span(s);
                if (t == KW_SYNC_ALL) return SYNC_ALL1(l);
                return SYNC_MEMORY1(l);
            }
            Vec<ast_t*> stats = sync_stat_list();
            expect_(TK_RPAREN, __LINE__);
            l = span(s);
            if (t == KW_SYNC_ALL) return SYNC_ALL2(stats, l);
            return SYNC_MEMORY2(stats, l);
        }
        expect_(TK_LPAREN, __LINE__);
        if (t == KW_SYNC_IMAGES) {
            if (accept(TK_STAR)) {
                if (accept(TK_RPAREN)) {
                    l = span(s);
                    return SYNC_IMAGE1(Asterisk, l);
                }
                Vec<ast_t*> stats = sync_stat_list();
                expect_(TK_RPAREN, __LINE__);
                l = span(s);
                return SYNC_IMAGE3(Asterisk, stats, l);
            }
            ast_t *e = expr();
            if (accept(TK_RPAREN)) {
                l = span(s);
                return SYNC_IMAGE2(e, l);
            }
            Vec<ast_t*> stats = sync_stat_list();
            expect_(TK_RPAREN, __LINE__);
            l = span(s);
            return SYNC_IMAGE4(e, stats, l);
        }
        ast_t *e = expr();
        if (accept(TK_RPAREN)) {
            l = span(s);
            return SYNCTEAM1(e, l);
        }
        Vec<ast_t*> stats = sync_stat_list();
        expect_(TK_RPAREN, __LINE__);
        l = span(s);
        return SYNCTEAM2(e, stats, l);
    }

    // The rest of STOP and ERROR STOP, which start at token `s`
    ast_t* stop_rest(size_t s, bool error) {
        ast_t *code = nullptr, *quiet = nullptr;
        if (!is_sep(K[i]) && K[i] != TK_COMMA) code = expr();
        if (accept(TK_COMMA)) {
            expect_(KW_QUIET, __LINE__);
            expect_(TK_EQUAL, __LINE__);
            quiet = expr();
        }
        Location l = span(s);
        if (error) {
            if (code && quiet) return ERROR_STOP3(code, quiet, l);
            if (quiet) return ERROR_STOP2(quiet, l);
            if (code) return ERROR_STOP1(code, l);
            return ERROR_STOP(l);
        }
        if (code && quiet) return STOP3(code, quiet, l);
        if (quiet) return STOP2(quiet, l);
        if (code) return STOP1(code, l);
        return STOP(l);
    }

    ast_t* goto_rest(size_t s) {
        Location l;
        if (K[i] == TK_INTEGER) {
            const YYSTYPE &v = V[i];
            i++;
            l = span(s);
            return GOTO(v.int_suffix, l);
        }
        if (accept(TK_LPAREN)) {
            Vec<ast_t*> labels = expr_list();
            expect_(TK_RPAREN, __LINE__);
            accept(TK_COMMA);
            ast_t *e = expr();
            l = span(s);
            return GOTO1(labels, e, l);
        }
        ast_t *n = id();
        if (is_sep(K[i])) {
            l = span(s);
            return GOTO2(n, l);
        }
        accept(TK_COMMA);
        expect_(TK_LPAREN, __LINE__);
        Vec<ast_t*> labels = expr_list();
        expect_(TK_RPAREN, __LINE__);
        l = span(s);
        return GOTO3(n, labels, l);
    }

    // DATA statement

    ast_t* data_set() {
        size_t s = i;
        Vec<ast_t*> objects;
        LIST_NEW(objects);
        do {
            LIST_ADD(objects, data_object());
        } while (accept(TK_COMMA));
        expect_(TK_SLASH, __LINE__);
        Vec<ast_t*> values = data_stmt_value_list();
        expect_(TK_SLASH, __LINE__);
        Location l = span(s);
        return DATA(objects, values, l);
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

    ast_t* signed_numeric_constant() {
        size_t s = i;
        Location l;
        switch (K[i]) {
            case TK_INTEGER: i++; l = span(s); return INTEGER(V[s].int_suffix, l);
            case TK_REAL: i++; l = span(s); return REAL(V[s].string, l);
            case TK_MINUS: {
                i++;
                ast_t *x = signed_numeric_constant();
                l = span(s);
                return UNARY_MINUS(x, l);
            }
            case TK_PLUS: {
                i++;
                ast_t *x = signed_numeric_constant();
                l = span(s);
                return UNARY_PLUS(x, l);
            }
            default: unsup(__LINE__);
        }
    }

    ast_t* data_stmt_constant() {
        size_t s = i;
        Location l = Lc[s];
        if (is_name(K[i])) return designator();
        switch (K[i]) {
            case TK_STRING: i++; return STRING(V[s].str_prefix, l);
            case TK_BOZ_CONSTANT: i++; return BOZ(V[s].string, l);
            case TK_TRUE: i++; return TRUE(V[s].string, l);
            case TK_FALSE: i++; return FALSE(V[s].string, l);
            case TK_LPAREN: {
                i++;
                ast_t *x = signed_numeric_constant();
                expect_(TK_COMMA, __LINE__);
                ast_t *y = signed_numeric_constant();
                expect_(TK_RPAREN, __LINE__);
                l = span(s);
                return COMPLEX(x, y, l);
            }
            default: return signed_numeric_constant();
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

    void endif() {
        if (K[i] == KW_END_IF) {
            i++;
        } else if (K[i] == KW_ENDIF) {
            Location l = Lc[i];
            i++;
            WARN_ENDIF(l);
        } else {
            unsup(__LINE__);
        }
    }

    int64_t enddo() {
        int64_t label = 0;
        if (K[i] == TK_LABEL) {
            label = V[i].n;
            i++;
            if (!accept(KW_END_DO)) expect_(KW_ENDDO, __LINE__);
            return label;
        }
        if (K[i] == KW_END_DO) {
            i++;
        } else if (K[i] == KW_ENDDO) {
            Location l = Lc[i];
            i++;
            WARN_ENDDO(l);
        } else {
            unsup(__LINE__);
        }
        return label;
    }

    // if_statement and if_statement_single
    ast_t* if_statement() {
        size_t s = i;
        if (K[i+1] != TK_LPAREN) return nullptr;
        i += 2;
        ast_t *cond = expr();
        expect_(TK_RPAREN, __LINE__);
        if (K[i] == KW_THEN && (is_sep(K[i+1])
                || (is_name(K[i+1]) && is_sep(K[i+2])))) {
            return if_block(s, cond);
        }
        Location l;
        if (K[i] == TK_INTEGER) {
            const YYSTYPE &a = V[i];
            i++;
            expect_(TK_COMMA, __LINE__);
            const YYSTYPE &b = V[i];
            expect_(TK_INTEGER, __LINE__);
            expect_(TK_COMMA, __LINE__);
            const YYSTYPE &c = V[i];
            expect_(TK_INTEGER, __LINE__);
            l = span(s);
            return IFARITHMETIC(cond, INTEGER3(a.int_suffix),
                INTEGER3(b.int_suffix), INTEGER3(c.int_suffix), l);
        }
        ast_t *body = statement1();
        if (body == nullptr) unsup(__LINE__);
        l = span(s);
        return IFSINGLE(cond, body, l);
    }

    // The rest of an IF construct that starts at token `s`, from `then` on
    ast_t* if_block(size_t s, ast_t *cond) {
        expect_(KW_THEN, __LINE__);
        id_opt();
        Vec<ast_t*> sp = sep();
        Vec<ast_t*> body = statements();
        Location l;
        if (K[i] == KW_ELSEIF || (K[i] == KW_ELSE
                && K[i+1] == KW_IF)) {
            if (K[i] == KW_ELSE) i++;
            size_t s2 = i;
            i++;
            expect_(TK_LPAREN, __LINE__);
            ast_t *cond2 = expr();
            expect_(TK_RPAREN, __LINE__);
            ast_t *block = if_block(s2, cond2);
            l = span(s);
            return IF3(cond, TRIVIA_AFTER(sp, l), body, block, l);
        }
        if (accept(KW_ELSE)) {
            id_opt();
            Vec<ast_t*> sp2 = sep();
            Vec<ast_t*> orelse = statements();
            endif();
            l = span(s);
            return IF2(cond, TRIVIA(sp, sp2, l), body, orelse, l);
        }
        endif();
        l = span(s);
        return IF1(cond, TRIVIA_AFTER(sp, l), body, l);
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

    Vec<ast_t*> id_list() {
        Vec<ast_t*> v;
        LIST_NEW(v);
        do {
            LIST_ADD(v, id());
        } while (accept(TK_COMMA));
        return v;
    }

    Vec<ast_t*> concurrent_localities() {
        Vec<ast_t*> v;
        LIST_NEW(v);
        for (;;) {
            size_t s = i;
            int t = K[i];
            if (K[i+1] != TK_LPAREN || (t != KW_LOCAL && t != KW_LOCAL_INIT
                    && t != KW_SHARED && t != KW_DEFAULT && t != KW_REDUCE)) {
                return v;
            }
            i += 2;
            Location l;
            if (t == KW_DEFAULT) {
                expect_(KW_NONE, __LINE__);
                expect_(TK_RPAREN, __LINE__);
                l = span(s);
                LIST_ADD(v, CONCURRENT_DEFAULT(l));
            } else if (t == KW_REDUCE) {
                reduce_opType op;
                Location lo = Lc[i];
                switch (K[i]) {
                    case TK_PLUS: i++; op = REDUCE_OP_TYPE_ADD(lo); break;
                    case TK_STAR: i++; op = REDUCE_OP_TYPE_MUL(lo); break;
                    case TK_AND: i++; op = reduce_opType::ReduceAND; break;
                    case TK_OR: i++; op = reduce_opType::ReduceOR; break;
                    case TK_EQV: i++; op = reduce_opType::ReduceEQV; break;
                    case TK_NEQV: i++; op = reduce_opType::ReduceNEQV; break;
                    default: {
                        ast_t *n = id();
                        op = REDUCE_OP_TYPE_ID(n, lo);
                    }
                }
                expect_(TK_COLON, __LINE__);
                Vec<ast_t*> ids = id_list();
                expect_(TK_RPAREN, __LINE__);
                l = span(s);
                LIST_ADD(v, CONCURRENT_REDUCE(op, ids, l));
            } else {
                Vec<ast_t*> ids = id_list();
                expect_(TK_RPAREN, __LINE__);
                l = span(s);
                if (t == KW_LOCAL) {
                    LIST_ADD(v, CONCURRENT_LOCAL(ids, l));
                } else if (t == KW_LOCAL_INIT) {
                    LIST_ADD(v, CONCURRENT_LOCAL_INIT(ids, l));
                } else {
                    LIST_ADD(v, CONCURRENT_SHARED(ids, l));
                }
            }
        }
    }

    // do_statement and while_statement
    ast_t* do_statement() {
        size_t s = i;
        Location l;
        if (K[i] == KW_DOWHILE) {
            if (K[i+1] != TK_LPAREN) return nullptr;
            i += 2;
            ast_t *cond = expr();
            expect_(TK_RPAREN, __LINE__);
            Vec<ast_t*> sp = sep();
            Vec<ast_t*> body = statements();
            enddo();
            l = span(s);
            return WHILE(cond, TRIVIA_AFTER(sp, l), body, l);
        }
        i++;
        if (is_sep(K[i])) {
            Vec<ast_t*> sp = sep();
            Vec<ast_t*> body = statements();
            int64_t end_label = enddo();
            l = span(s);
            return DO1(TRIVIA_AFTER(sp, l), body, end_label, l);
        }
        bool has_label = false;
        int64_t label = 0;
        if (K[i] == TK_INTEGER) {
            label = INTEGER3(V[i].int_suffix);
            has_label = true;
            i++;
        }
        accept(TK_COMMA);
        if (!has_label && K[i] == KW_WHILE && K[i+1] == TK_LPAREN) {
            i += 2;
            ast_t *cond = expr();
            expect_(TK_RPAREN, __LINE__);
            Vec<ast_t*> sp = sep();
            Vec<ast_t*> body = statements();
            enddo();
            l = span(s);
            return WHILE(cond, TRIVIA_AFTER(sp, l), body, l);
        }
        if (!has_label && K[i] == KW_CONCURRENT && K[i+1] == TK_LPAREN) {
            i += 2;
            ast_t *mask;
            Vec<ast_t*> controls = concurrent_controls(mask);
            Vec<ast_t*> loc = concurrent_localities();
            Vec<ast_t*> sp = sep();
            Vec<ast_t*> body = statements();
            enddo();
            l = span(s);
            if (mask) {
                return DO_CONCURRENT2(controls, mask, loc, TRIVIA_AFTER(sp, l),
                    body, l);
            }
            return DO_CONCURRENT1(controls, loc, TRIVIA_AFTER(sp, l), body, l);
        }
        if (!is_name(K[i]) || K[i+1] != TK_EQUAL) return nullptr;
        ast_t *var = id();
        i++;
        ast_t *a = expr();
        expect_(TK_COMMA, __LINE__);
        ast_t *b = expr();
        ast_t *c = nullptr;
        if (accept(TK_COMMA)) c = expr();
        Vec<ast_t*> sp = sep();
        Vec<ast_t*> body = statements();
        int64_t end_label = enddo();
        l = span(s);
        ast_t *r;
        if (has_label) {
            if (c) {
                r = DO3_LABEL(label, var, a, b, c, TRIVIA_AFTER(sp, l), body,
                    end_label, l);
            } else {
                r = DO2_LABEL(label, var, a, b, TRIVIA_AFTER(sp, l), body,
                    end_label, l);
            }
            return r;
        }
        if (c) return DO3(var, a, b, c, TRIVIA_AFTER(sp, l), body, end_label, l);
        return DO2(var, a, b, TRIVIA_AFTER(sp, l), body, end_label, l);
    }

    void endwhere() {
        if (!accept(KW_END_WHERE)) expect_(KW_ENDWHERE, __LINE__);
    }

    // where_statement and where_statement_single
    ast_t* where_statement() {
        size_t s = i;
        if (K[i+1] != TK_LPAREN) return nullptr;
        i += 2;
        ast_t *cond = expr();
        expect_(TK_RPAREN, __LINE__);
        if (!is_sep(K[i])) {
            ast_t *body = assignment_statement();
            Location l = span(s);
            return WHERESINGLE(cond, body, l);
        }
        return where_block(s, cond);
    }

    // The rest of a WHERE construct (or of an ELSEWHERE block) that starts
    // at token `s`, after the mask
    ast_t* where_block(size_t s, ast_t *cond) {
        Vec<ast_t*> sp = sep();
        Vec<ast_t*> body = statements();
        Location l;
        size_t s2 = i;
        bool elsewhere = false;
        if (accept(KW_ELSEWHERE)) {
            elsewhere = true;
        } else if (K[i] == KW_ELSE && K[i+1] == KW_WHERE) {
            i += 2;
            elsewhere = true;
        }
        if (elsewhere) {
            if (accept(TK_LPAREN)) {
                ast_t *cond2 = expr();
                expect_(TK_RPAREN, __LINE__);
                ast_t *block = where_block(s2, cond2);
                l = span(s);
                return WHERE3(cond, TRIVIA_AFTER(sp, l), body, block, l);
            }
            Vec<ast_t*> sp2 = sep();
            Vec<ast_t*> orelse = statements();
            endwhere();
            l = span(s);
            return WHERE2(cond, TRIVIA(sp, sp2, l), body, orelse, l);
        }
        endwhere();
        l = span(s);
        return WHERE1(cond, TRIVIA_AFTER(sp, l), body, l);
    }

    // forall_statement and forall_statement_single
    ast_t* forall_statement() {
        size_t s = i;
        if (K[i+1] != TK_LPAREN) return nullptr;
        i += 2;
        ast_t *mask;
        Vec<ast_t*> controls = concurrent_controls(mask);
        Location l;
        Vec<ast_t*> loc = concurrent_localities();
        if (!is_sep(K[i])) {
            ast_t *body = assignment_statement();
            l = span(s);
            if (mask) return FORALLSINGLE2(controls, mask, body, l);
            return FORALLSINGLE1(controls, body, l);
        }
        Vec<ast_t*> sp = sep();
        Vec<ast_t*> body = statements();
        if (!accept(KW_END_FORALL)) expect_(KW_ENDFORALL, __LINE__);
        l = span(s);
        if (mask) return FORALL2(controls, mask, loc, TRIVIA_AFTER(sp, l), body, l);
        return FORALL1(controls, loc, TRIVIA_AFTER(sp, l), body, l);
    }

    void end_select() {
        if (!accept(KW_END_SELECT)) expect_(KW_ENDSELECT, __LINE__);
    }

    // The rest of SELECT CASE that starts at token `s`
    ast_t* select_case(size_t s) {
        expect_(TK_LPAREN, __LINE__);
        ast_t *e = expr();
        expect_(TK_RPAREN, __LINE__);
        Vec<ast_t*> sp = sep();
        Vec<ast_t*> cases;
        LIST_NEW(cases);
        Location l;
        while (K[i] == KW_CASE) {
            size_t s2 = i;
            i++;
            if (accept(KW_DEFAULT)) {
                id_opt();
                Vec<ast_t*> sp2 = sep();
                Vec<ast_t*> body = statements();
                l = span(s2);
                LIST_ADD(cases, CASE_STMT_DEFAULT(TRIVIA_AFTER(sp2, l), body, l));
                continue;
            }
            expect_(TK_LPAREN, __LINE__);
            Vec<ast_t*> conds;
            LIST_NEW(conds);
            do {
                size_t s3 = i;
                if (accept(TK_COLON)) {
                    ast_t *b = expr();
                    l = span(s3);
                    LIST_ADD(conds, CASE_RANGE2(b, l));
                    continue;
                }
                ast_t *a = expr();
                if (accept(TK_COLON)) {
                    if (at_arg_end()) {
                        l = span(s3);
                        LIST_ADD(conds, CASE_RANGE1(a, l));
                    } else {
                        ast_t *b = expr();
                        l = span(s3);
                        LIST_ADD(conds, CASE_RANGE3(a, b, l));
                    }
                } else {
                    l = span(s3);
                    LIST_ADD(conds, CASE_EXPR(a, l));
                }
            } while (accept(TK_COMMA));
            expect_(TK_RPAREN, __LINE__);
            id_opt();
            Vec<ast_t*> sp2 = sep();
            Vec<ast_t*> body = statements();
            l = span(s2);
            LIST_ADD(cases, CASE_STMT(conds, TRIVIA_AFTER(sp2, l), body, l));
        }
        end_select();
        l = span(s);
        return SELECT(e, TRIVIA_AFTER(sp, l), cases, l);
    }

    ast_t* select_type(size_t s) {
        expect_(TK_LPAREN, __LINE__);
        ast_t *assoc = nullptr;
        if (is_name(K[i]) && K[i+1] == TK_ARROW) {
            assoc = id();
            i++;
        }
        ast_t *e = expr();
        expect_(TK_RPAREN, __LINE__);
        Vec<ast_t*> sp = sep();
        Vec<ast_t*> body;
        LIST_NEW(body);
        Location l;
        for (;;) {
            size_t s2 = i;
            if (K[i] == KW_TYPE && K[i+1] == KW_IS) {
                i += 2;
                expect_(TK_LPAREN, __LINE__);
                if (K[i] == TK_NAME && K[i+1] == TK_RPAREN) {
                    const YYSTYPE &v = V[i];
                    i += 2;
                    Vec<ast_t*> sp2 = sep();
                    Vec<ast_t*> stmts = statements();
                    l = span(s2);
                    LIST_ADD(body, TYPE_STMTNAME(v.string,
                        TRIVIA_AFTER(sp2, l), stmts, l));
                } else if (K[i] == TK_NAME && K[i+1] == TK_LPAREN) {
                    size_t ni = i;
                    i += 2;
                    Vec<kind_item_t> kind = kind_arg_list();
                    expect_(TK_RPAREN, __LINE__);
                    expect_(TK_RPAREN, __LINE__);
                    Vec<ast_t*> sp2 = sep();
                    Vec<ast_t*> stmts = statements();
                    l = span(s2);
                    Location nl = Lc[ni];
                    LIST_ADD(body, TYPE_STMTVAR(ATTR_TYPE_NAME_KIND(Type,
                        SYMBOL(V[ni].string, nl), kind, l),
                        TRIVIA_AFTER(sp2, l), stmts, l));
                } else {
                    ast_t *vt = var_type();
                    expect_(TK_RPAREN, __LINE__);
                    Vec<ast_t*> sp2 = sep();
                    Vec<ast_t*> stmts = statements();
                    l = span(s2);
                    LIST_ADD(body, TYPE_STMTVAR(vt, TRIVIA_AFTER(sp2, l),
                        stmts, l));
                }
            } else if (K[i] == KW_CLASS && K[i+1] == KW_IS) {
                i += 2;
                expect_(TK_LPAREN, __LINE__);
                ast_t *n = id();
                expect_(TK_RPAREN, __LINE__);
                Vec<ast_t*> sp2 = sep();
                Vec<ast_t*> stmts = statements();
                l = span(s2);
                LIST_ADD(body, CLASS_STMT(n, TRIVIA_AFTER(sp2, l), stmts, l));
            } else if (K[i] == KW_CLASS && K[i+1] == KW_DEFAULT) {
                i += 2;
                Vec<ast_t*> sp2 = sep();
                Vec<ast_t*> stmts = statements();
                l = span(s2);
                LIST_ADD(body, CLASS_DEFAULT(TRIVIA_AFTER(sp2, l), stmts, l));
            } else {
                break;
            }
        }
        end_select();
        l = span(s);
        if (assoc) return SELECT_TYPE2(assoc, e, TRIVIA_AFTER(sp, l), body, l);
        return SELECT_TYPE1(e, TRIVIA_AFTER(sp, l), body, l);
    }

    ast_t* select_rank(size_t s) {
        expect_(TK_LPAREN, __LINE__);
        ast_t *assoc = nullptr;
        if (is_name(K[i]) && K[i+1] == TK_ARROW) {
            assoc = id();
            i++;
        }
        ast_t *e = expr();
        expect_(TK_RPAREN, __LINE__);
        Vec<ast_t*> sp = sep();
        Vec<ast_t*> body;
        LIST_NEW(body);
        Location l;
        while (K[i] == KW_RANK) {
            size_t s2 = i;
            i++;
            if (accept(KW_DEFAULT)) {
                id_opt();
                Vec<ast_t*> sp2 = sep();
                Vec<ast_t*> stmts = statements();
                l = span(s2);
                LIST_ADD(body, RANK_DEFAULT(TRIVIA_AFTER(sp2, l), stmts, l));
                continue;
            }
            expect_(TK_LPAREN, __LINE__);
            if (K[i] == TK_STAR && K[i+1] == TK_RPAREN) {
                i += 2;
                id_opt();
                Vec<ast_t*> sp2 = sep();
                Vec<ast_t*> stmts = statements();
                l = span(s2);
                LIST_ADD(body, RANK_STAR(TRIVIA_AFTER(sp2, l), stmts, l));
                continue;
            }
            ast_t *r = expr();
            expect_(TK_RPAREN, __LINE__);
            id_opt();
            Vec<ast_t*> sp2 = sep();
            Vec<ast_t*> stmts = statements();
            l = span(s2);
            LIST_ADD(body, RANK_EXPR(r, TRIVIA_AFTER(sp2, l), stmts, l));
        }
        end_select();
        l = span(s);
        if (assoc) return SELECT_RANK2(assoc, e, TRIVIA_AFTER(sp, l), body, l);
        return SELECT_RANK1(e, TRIVIA_AFTER(sp, l), body, l);
    }

    // ------------------------------------------------------------------
    // Declarations

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
        intrinsicopType op;
        size_t defop;
        ast_t *name;
        Location l;
        Generic g = generic_spec(op, defop, name, false);
        if (g == Generic::Op) {
            l = span(s);
            return DECL_OP(op, l);
        } else if (g == Generic::Defop) {
            l = span(s);
            return DECL_DEFOP(V[defop].string, l);
        } else if (g != Generic::None) {
            unsup(__LINE__);
        }
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

    ast_t* template_decl() {
        size_t s = i;
        i++;
        ast_t *name = id();
        expect_(TK_LBRACE, __LINE__);
        Vec<ast_t*> params;
        if (K[i] == TK_RBRACE) {
            LIST_NEW(params);
        } else {
            params = id_list();
        }
        expect_(TK_RBRACE, __LINE__);
        sep();
        Vec<ast_t*> decls = decl_statements();
        Vec<ast_t*> contains = contains_block_opt();
        expect_(KW_END, __LINE__);
        expect_(KW_TEMPLATE, __LINE__);
        ast_t *end_name = nullptr;
        if (is_name(K[i])) end_name = id();
        sep();
        Location l = span(s);
        return TEMPLATE(name, params, decls, contains, end_name, l);
    }

    ast_t* requirement_decl() {
        size_t s = i;
        i++;
        ast_t *name = id();
        expect_(TK_LBRACE, __LINE__);
        Vec<ast_t*> params;
        if (K[i] == TK_RBRACE) {
            LIST_NEW(params);
        } else {
            params = id_list();
        }
        expect_(TK_RBRACE, __LINE__);
        sep();
        Vec<ast_t*> decls = decl_statements();
        expect_(KW_END, __LINE__);
        expect_(KW_REQUIREMENT, __LINE__);
        ast_t *end_name = nullptr;
        if (is_name(K[i])) end_name = id();
        sep();
        Location l = span(s);
        return REQUIREMENT(name, params, decls, end_name, l);
    }

    ast_t* require_decl() {
        size_t s = i;
        i++;
        accept(TK_DBL_COLON);
        size_t s2 = i;
        ast_t *name = id();
        Vec<ast_t*> args = instantiate_symbols();
        Location l = span(s2);
        ast_t *req = UNIT_REQUIRE(name, args, l);
        sep();
        l = span(s);
        return REQUIRE(req, l);
    }

    ast_t* union_type_decl() {
        size_t s = i;
        i++;
        Vec<ast_t*> mods = var_modifiers();
        ast_t *name = id();
        Vec<ast_t*> sp = sep();
        Vec<ast_t*> decls = var_decl_star();
        expect_(KW_END_UNION_TYPE, __LINE__);
        id_opt();
        Vec<ast_t*> sp2 = sep();
        Location l = span(s);
        return UNION_TYPE(mods, name, TRIVIA(sp, sp2, l), decls, l);
    }

    // Returns nullptr (and consumes nothing) if no declaration or statement
    // starts here
    ast_t* decl_statement() {
        size_t s = i;
        size_t j = (K[i] == TK_LABEL) ? i + 1 : i;
        int t = K[j];
        if (is_name(t)) {
            size_t save = i;
            i = j;
            bool assignment = is_assignment();
            i = save;
            if (assignment) return statement();
        }
        ast_t *r = nullptr;
        switch (t) {
            case KW_IMPLICIT: r = implicit_statement(); break;
            case KW_USE: r = use_statement(); break;
            case KW_IMPORT: r = import_statement(); break;
            case KW_INTERFACE: r = interface_decl(); break;
            case KW_ABSTRACT: {
                if (K[j+1] == KW_INTERFACE) {
                    r = interface_decl();
                } else {
                    r = var_decl();
                }
                break;
            }
            case KW_DEFERRED: {
                if (K[j+1] == KW_INTERFACE) {
                    r = interface_decl();
                } else {
                    r = deferred_decl();
                }
                break;
            }
            case KW_TEMPLATE: {
                if (!is_name(K[j+1]) || K[j+2] != TK_LBRACE) return nullptr;
                r = template_decl();
                break;
            }
            case KW_REQUIREMENT: r = requirement_decl(); break;
            case KW_REQUIRE: r = require_decl(); break;
            case KW_UNION_TYPE: r = union_type_decl(); break;
            case KW_TYPE: {
                if (K[j+1] == KW_IS) return nullptr;
                if (K[j+1] == TK_LPAREN) {
                    r = var_decl();
                } else {
                    r = derived_type_decl();
                }
                break;
            }
            case KW_CLASS: {
                if (K[j+1] != TK_LPAREN) return nullptr;
                r = var_decl();
                break;
            }
            case KW_ENUM: r = enum_decl(); break;
            case KW_PROCEDURE: {
                if (K[j+1] != TK_LPAREN) return nullptr;
                r = var_decl();
                break;
            }
            default: {
                r = var_decl();
                if (r == nullptr) {
                    i = s;
                    return statement();
                }
            }
        }
        if (r == nullptr) i = s;
        return r;
    }

    ast_t* implicit_statement() {
        size_t s = i;
        i++;
        Location l;
        if (accept(KW_NONE)) {
            if (accept(TK_LPAREN)) {
                Vec<ast_t*> specs;
                LIST_NEW(specs);
                if (K[i] != TK_RPAREN) {
                    do {
                        Location l2 = Lc[i];
                        if (accept(KW_EXTERNAL)) {
                            LIST_ADD(specs, IMPLICIT_NONE_EXTERNAL(l2));
                        } else {
                            expect_(KW_TYPE, __LINE__);
                            LIST_ADD(specs, IMPLICIT_NONE_TYPE(l2));
                        }
                    } while (accept(TK_COMMA));
                }
                expect_(TK_RPAREN, __LINE__);
                Vec<ast_t*> sp = sep();
                l = span(s);
                return IMPLICIT_NONE2(specs, TRIVIA_AFTER(sp, l), l);
            }
            Vec<ast_t*> sp = sep();
            l = span(s);
            return IMPLICIT_NONE(TRIVIA_AFTER(sp, l), l);
        }
        Vec<ast_t*> specs;
        LIST_NEW(specs);
        do {
            LIST_ADD(specs, implicit_spec());
        } while (accept(TK_COMMA));
        Vec<ast_t*> sp = sep();
        l = span(s);
        return IMPLICIT(specs, TRIVIA_AFTER(sp, l), l);
    }

    // "(" kind_arg_list ")"
    Vec<kind_item_t> paren_kind_args() {
        expect_(TK_LPAREN, __LINE__);
        Vec<kind_item_t> v = kind_arg_list();
        expect_(TK_RPAREN, __LINE__);
        return v;
    }

    ast_t* implicit_spec() {
        size_t s = i;
        int t = K[i];
        i++;
        Location l;
        decl_typeType type;
        switch (t) {
            case KW_INTEGER: type = decl_typeType::TypeInteger; break;
            case KW_CHARACTER: type = decl_typeType::TypeCharacter; break;
            case KW_REAL: type = decl_typeType::TypeReal; break;
            case KW_LOGICAL: type = decl_typeType::TypeLogical; break;
            case KW_COMPLEX: type = decl_typeType::TypeComplex; break;
            case KW_DOUBLE: {
                if (accept(KW_PRECISION)) {
                    type = decl_typeType::TypeDoublePrecision;
                } else {
                    expect_(KW_COMPLEX, __LINE__);
                    type = decl_typeType::TypeDoubleComplex;
                }
                Vec<kind_item_t> letters = paren_kind_args();
                l = span(s);
                return IMPLICIT_SPEC(make_AttrType_t(m_a, l, type, nullptr, 0,
                    nullptr, nullptr, None), letters, l);
            }
            case KW_DOUBLE_PRECISION: case KW_DOUBLE_COMPLEX: {
                type = (t == KW_DOUBLE_PRECISION)
                    ? decl_typeType::TypeDoublePrecision
                    : decl_typeType::TypeDoubleComplex;
                Vec<kind_item_t> letters = paren_kind_args();
                l = span(s);
                return IMPLICIT_SPEC(make_AttrType_t(m_a, l, type, nullptr, 0,
                    nullptr, nullptr, None), letters, l);
            }
            case KW_TYPE: case KW_CLASS: {
                expect_(TK_LPAREN, __LINE__);
                if (t == KW_TYPE && is_intrinsic_type_start()) {
                    ast_t *it = intrinsic_type_spec();
                    expect_(TK_RPAREN, __LINE__);
                    Vec<kind_item_t> letters = paren_kind_args();
                    l = span(s);
                    return IMPLICIT_SPEC(ATTR_TYPE_ATTR(Type, it, l), letters, l);
                }
                ast_t *n = id();
                expect_(TK_RPAREN, __LINE__);
                Vec<kind_item_t> letters = paren_kind_args();
                l = span(s);
                if (t == KW_TYPE) {
                    return IMPLICIT_SPEC(ATTR_TYPE_NAME(Type, n, l), letters, l);
                }
                return IMPLICIT_SPEC(ATTR_TYPE_NAME(Class, n, l), letters, l);
            }
            default: unsup(__LINE__);
        }
        if (accept(TK_STAR)) {
            Location l2 = Lc[i-1];
            if (K[i] == TK_INTEGER) {
                const YYSTYPE &v = V[i];
                i++;
                Vec<kind_item_t> letters = paren_kind_args();
                l = span(s);
                ast_t *r;
                switch (t) {
                    case KW_INTEGER: r = IMPLICIT_SPEC(ATTR_TYPE_INT(Integer, v.int_suffix, l), letters, l); WARN_INTEGERSTAR(v.int_suffix, l2); break;
                    case KW_CHARACTER: r = IMPLICIT_SPEC(ATTR_TYPE_INT(Character, v.int_suffix, l), letters, l); WARN_CHARACTERSTAR(v.int_suffix, l2); break;
                    case KW_REAL: r = IMPLICIT_SPEC(ATTR_TYPE_INT(Real, v.int_suffix, l), letters, l); WARN_REALSTAR(v.int_suffix, l2); break;
                    case KW_LOGICAL: r = IMPLICIT_SPEC(ATTR_TYPE_INT(Logical, v.int_suffix, l), letters, l); WARN_LOGICALSTAR(v.int_suffix, l2); break;
                    default: r = IMPLICIT_SPEC(ATTR_TYPE_INT(Complex, DIV2(v.int_suffix), l), letters, l); WARN_COMPLEXSTAR(v.int_suffix, l2); break;
                }
                return r;
            }
            expect_(TK_LPAREN, __LINE__);
            ast_t *e = expr();
            expect_(TK_RPAREN, __LINE__);
            Vec<kind_item_t> letters = paren_kind_args();
            l = span(s);
            ast_t *r = IMPLICIT_SPEC(ATTR_TYPE_EXPR(Character, e, l), letters, l);
            WARN_CHARACTERSTAR_EXPR(l2);
            return r;
        }
        Vec<kind_item_t> first = paren_kind_args();
        if (K[i] == TK_LPAREN) {
            Vec<kind_item_t> letters = paren_kind_args();
            l = span(s);
            return IMPLICIT_SPEC(make_AttrType_t(m_a, l, type, first.p,
                first.size(), nullptr, nullptr, None), letters, l);
        }
        l = span(s);
        return IMPLICIT_SPEC(make_AttrType_t(m_a, l, type, nullptr, 0, nullptr,
            nullptr, None), first, l);
    }

    ast_t* import_statement() {
        size_t s = i;
        i++;
        Location l;
        if (is_sep(K[i])) {
            Vec<ast_t*> sp = sep();
            l = span(s);
            return IMPORT0(Default, TRIVIA_AFTER(sp, l), l);
        }
        if (accept(TK_COMMA)) {
            if (accept(KW_ONLY)) {
                expect_(TK_COLON, __LINE__);
                Vec<ast_t*> ids = id_list();
                Vec<ast_t*> sp = sep();
                l = span(s);
                return IMPORT1(ids, Only, TRIVIA_AFTER(sp, l), l);
            }
            if (accept(KW_NONE)) {
                Vec<ast_t*> sp = sep();
                l = span(s);
                return IMPORT0(None, TRIVIA_AFTER(sp, l), l);
            }
            expect_(KW_ALL, __LINE__);
            Vec<ast_t*> sp = sep();
            l = span(s);
            return IMPORT0(All, TRIVIA_AFTER(sp, l), l);
        }
        accept(TK_DBL_COLON);
        Vec<ast_t*> ids = id_list();
        Vec<ast_t*> sp = sep();
        l = span(s);
        return IMPORT1(ids, Default, TRIVIA_AFTER(sp, l), l);
    }

    // Is the current token one of the operators of operator_type?
    bool operator_type(intrinsicopType &op) {
        // OPERATOR() takes a location like the other macros, and ignores it
        Location l = Lc[i];
        (void)l;
        switch (K[i]) {
            case TK_PLUS: op = OPERATOR(PLUS, l); break;
            case TK_MINUS: op = OPERATOR(MINUS, l); break;
            case TK_STAR: op = OPERATOR(STAR, l); break;
            case TK_SLASH: op = OPERATOR(DIV, l); break;
            case TK_POW: op = OPERATOR(POW, l); break;
            case TK_EQ: op = OPERATOR(EQ, l); break;
            case TK_NE: op = OPERATOR(NOTEQ, l); break;
            case TK_GT: op = OPERATOR(GT, l); break;
            case TK_GE: op = OPERATOR(GTE, l); break;
            case TK_LT: op = OPERATOR(LT, l); break;
            case TK_LE: op = OPERATOR(LTE, l); break;
            case TK_CONCAT: op = OPERATOR(CONCAT, l); break;
            case TK_NOT: op = OPERATOR(NOT, l); break;
            case TK_AND: op = OPERATOR(AND, l); break;
            case TK_OR: op = OPERATOR(OR, l); break;
            case TK_XOR: op = OPERATOR(XOR, l); break;
            case TK_EQV: op = OPERATOR(EQV, l); break;
            case TK_NEQV: op = OPERATOR(NEQV, l); break;
            default: return false;
        }
        i++;
        return true;
    }

    // What follows `operator` (or `assignment`, `read`, `write`) in
    // parentheses, in an interface, a USE statement and a declaration
    enum class Generic { None, Op, Defop, Assignment, Write, Read };

    // Parses `operator(op)`, `operator(.op.)`, `assignment(=)`, `write(id)`
    // and `read(id)` if one of them starts here
    Generic generic_spec(intrinsicopType &op, size_t &defop, ast_t *&name,
            bool io = true) {
        int t = K[i];
        if (K[i+1] != TK_LPAREN) return Generic::None;
        if (t == KW_OPERATOR) {
            i += 2;
            if (accept(TK_RBRACKET_OLD)) {
                op = intrinsicopType::DIV;
                return Generic::Op;
            }
            if (K[i] == TK_DEF_OP) {
                defop = i;
                i++;
                expect_(TK_RPAREN, __LINE__);
                return Generic::Defop;
            }
            if (!operator_type(op)) unsup(__LINE__);
            expect_(TK_RPAREN, __LINE__);
            return Generic::Op;
        }
        if (t == KW_ASSIGNMENT) {
            if (K[i+2] != TK_EQUAL) return Generic::None;
            i += 3;
            expect_(TK_RPAREN, __LINE__);
            return Generic::Assignment;
        }
        if (io && (t == KW_WRITE || t == KW_READ)) {
            if (!is_name(K[i+2]) || K[i+3] != TK_RPAREN) return Generic::None;
            i += 2;
            name = id();
            i++;
            return (t == KW_WRITE) ? Generic::Write : Generic::Read;
        }
        return Generic::None;
    }

    ast_t* use_symbol() {
        size_t s = i;
        intrinsicopType op;
        size_t defop;
        ast_t *name;
        Location l;
        switch (generic_spec(op, defop, name)) {
            case Generic::Op: l = span(s); return INTRINSIC_OPERATOR(op, l);
            case Generic::Defop: {
                if (accept(TK_ARROW)) {
                    expect_(KW_OPERATOR, __LINE__);
                    expect_(TK_LPAREN, __LINE__);
                    size_t defop2 = i;
                    expect_(TK_DEF_OP, __LINE__);
                    expect_(TK_RPAREN, __LINE__);
                    l = span(s);
                    return RENAME_OPERATOR(V[defop].string, V[defop2].string, l);
                }
                l = span(s);
                return DEFINED_OPERATOR(V[defop].string, l);
            }
            case Generic::Assignment: l = span(s); return USE_ASSIGNMENT(l);
            case Generic::Write: l = span(s); return USE_WRITE(name, l);
            case Generic::Read: l = span(s); return USE_READ(name, l);
            default: break;
        }
        ast_t *a = id();
        if (accept(TK_ARROW)) {
            ast_t *b = id();
            l = span(s);
            return USE_SYMBOL2(a, b, l);
        }
        l = span(s);
        return USE_SYMBOL1(a, l);
    }

    Vec<ast_t*> use_symbol_list() {
        Vec<ast_t*> v;
        LIST_NEW(v);
        do {
            LIST_ADD(v, use_symbol());
        } while (accept(TK_COMMA));
        return v;
    }

    ast_t* use_statement() {
        size_t s = i;
        i++;
        Vec<ast_t*> mods;
        LIST_NEW(mods);
        Location l;
        if (K[i] == TK_COMMA) {
            while (accept(TK_COMMA)) {
                l = Lc[i];
                if (accept(KW_INTRINSIC)) {
                    LIST_ADD(mods, SIMPLE_ATTR(Intrinsic, l));
                } else {
                    expect_(KW_NON_INTRINSIC, __LINE__);
                    LIST_ADD(mods, SIMPLE_ATTR(Non_Intrinsic, l));
                }
            }
            expect_(TK_DBL_COLON, __LINE__);
        } else {
            accept(TK_DBL_COLON);
        }
        ast_t *mod = id();
        ast_t *u;
        if (accept(TK_COMMA)) {
            if (K[i] == KW_ONLY && K[i+1] == TK_COLON) {
                i += 2;
                if (is_sep(K[i])) {
                    l = span(s);
                    u = USE3(mods, mod, nullptr, l);
                } else {
                    Vec<ast_t*> syms = use_symbol_list();
                    l = span(s);
                    u = USE2(mods, mod, syms, nullptr, l);
                }
            } else {
                Vec<ast_t*> syms = use_symbol_list();
                l = span(s);
                u = USE4(mods, mod, syms, nullptr, l);
            }
        } else {
            l = span(s);
            u = USE1(mods, mod, nullptr, l);
        }
        Vec<ast_t*> sp = sep();
        l = span(s);
        TRIVIA2_(u, TRIVIA_AFTER(sp, l));
        return u;
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

    Vec<var_sym_t> var_sym_decl_list() {
        Vec<var_sym_t> syms;
        LIST_NEW(syms);
        do {
            var_sym_t *v = var_sym_decl();
            PLIST_ADD(syms, v);
        } while (accept(TK_COMMA));
        return syms;
    }

    // var_decl; returns nullptr if no declaration starts here
    ast_t* var_decl() {
        size_t s = i;
        if (K[i] == TK_LABEL) i++;
        int t = K[i];
        Location l;
        if (t == KW_PARAMETER && K[i+1] == TK_LPAREN) {
            i += 2;
            Vec<var_sym_t> v;
            LIST_NEW(v);
            do {
                size_t s2 = i;
                ast_t *n = id();
                expect_(TK_EQUAL, __LINE__);
                ast_t *e = expr();
                l = span(s2);
                var_sym_t *vs = VAR_SYM_DIM_INIT(n, nullptr, 0, e, Equal, l);
                PLIST_ADD(v, vs);
            } while (accept(TK_COMMA));
            expect_(TK_RPAREN, __LINE__);
            l = span(s);
            Vec<ast_t*> sp = sep();
            return VAR_DECL_PARAMETER(v, TRIVIA_AFTER(sp, l), l);
        }
        if (t == KW_NAMELIST) {
            i++;
            Vec<namelist_group_t> groups = namelist_group_list();
            l = span(s);
            Vec<ast_t*> sp = sep();
            return VAR_DECL_NAMELIST(groups, TRIVIA_AFTER(sp, l), l);
        }
        if (t == KW_COMMON) {
            i++;
            Vec<common_block_t> blocks = common_block_list();
            l = span(s);
            Vec<ast_t*> sp = sep();
            return VAR_DECL_COMMON(blocks, TRIVIA_AFTER(sp, l), l);
        }
        if (t == KW_EQUIVALENCE) {
            i++;
            Vec<equi_t> sets;
            LIST_NEW(sets);
            do {
                size_t s2 = i;
                expect_(TK_LPAREN, __LINE__);
                Vec<ast_t*> e = expr_list();
                expect_(TK_RPAREN, __LINE__);
                l = span(s2);
                equi_t *q = EQUIVALENCE_SET(e, l);
                PLIST_ADD(sets, q);
            } while (accept(TK_COMMA));
            l = span(s);
            Vec<ast_t*> sp = sep();
            return VAR_DECL_EQUIVALENCE(sets, TRIVIA_AFTER(sp, l), l);
        }
        if (t == TK_PRAGMA_DECL) {
            const YYSTYPE &v = V[i];
            i++;
            l = span(s);
            Vec<ast_t*> sp = sep();
            return VAR_DECL_PRAGMA(v.string, TRIVIA_AFTER(sp, l), l);
        }
        if (is_var_type_start()) {
            ast_t *vt = var_type();
            Vec<ast_t*> mods;
            bool has_mods = false;
            bool dbl_colon = false;
            if (K[i] == TK_COMMA) {
                LIST_NEW(mods);
                has_mods = true;
                while (accept(TK_COMMA)) {
                    ast_t *m = var_modifier();
                    if (m == nullptr) unsup(__LINE__);
                    LIST_ADD(mods, m);
                }
                expect_(TK_DBL_COLON, __LINE__);
            } else {
                dbl_colon = accept(TK_DBL_COLON);
            }
            Vec<var_sym_t> syms = var_sym_decl_list();
            l = span(s);
            Vec<ast_t*> sp = sep();
            if (has_mods) return VAR_DECL1a(vt, mods, syms, TRIVIA_AFTER(sp, l), l);
            if (dbl_colon) return VAR_DECL1b(vt, syms, TRIVIA_AFTER(sp, l), l);
            return VAR_DECL1c(vt, syms, TRIVIA_AFTER(sp, l), l);
        }
        if (s != i) return nullptr;
        ast_t *m = var_modifier();
        if (m == nullptr) return nullptr;
        if (is_sep(K[i])) {
            l = span(s);
            Vec<ast_t*> sp = sep();
            return VAR_DECL2(m, TRIVIA_AFTER(sp, l), l);
        }
        accept(TK_DBL_COLON);
        Vec<var_sym_t> syms = var_sym_decl_list();
        l = span(s);
        Vec<ast_t*> sp = sep();
        return VAR_DECL3(m, syms, TRIVIA_AFTER(sp, l), l);
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

    // intrinsic_type_spec
    ast_t* intrinsic_type_spec() {
        size_t s = i;
        int t = K[i];
        i++;
        Location l;
        decl_typeType type;
        switch (t) {
            case KW_INTEGER: type = decl_typeType::TypeInteger; break;
            case KW_REAL: type = decl_typeType::TypeReal; break;
            case KW_COMPLEX: type = decl_typeType::TypeComplex; break;
            case KW_LOGICAL: type = decl_typeType::TypeLogical; break;
            case KW_CHARACTER: type = decl_typeType::TypeCharacter; break;
            case KW_BYTE: {
                l = span(s);
                ast_t *r = ATTR_TYPE_EXPR(Integer, INT1(l), l);
                WARN_BYTE(l);
                return r;
            }
            case KW_DOUBLE: {
                if (accept(KW_PRECISION)) {
                    l = span(s);
                    return ATTR_TYPE(DoublePrecision, l);
                }
                expect_(KW_COMPLEX, __LINE__);
                l = span(s);
                return ATTR_TYPE(DoubleComplex, l);
            }
            case KW_DOUBLE_PRECISION: l = span(s); return ATTR_TYPE(DoublePrecision, l);
            case KW_DOUBLE_COMPLEX: l = span(s); return ATTR_TYPE(DoubleComplex, l);
            case KW_LIST: case KW_SET: {
                expect_(TK_LPAREN, __LINE__);
                ast_t *it = intrinsic_type_spec();
                expect_(TK_RPAREN, __LINE__);
                l = span(s);
                if (t == KW_LIST) return ATTR_TYPE_ATTR(List, it, l);
                return ATTR_TYPE_ATTR(Set, it, l);
            }
            case KW_DICT: case KW_TUPLE: {
                expect_(TK_LPAREN, __LINE__);
                Vec<ast_t*> v;
                LIST_NEW(v);
                do {
                    LIST_ADD(v, intrinsic_type_spec());
                } while (accept(TK_COMMA));
                expect_(TK_RPAREN, __LINE__);
                l = span(s);
                if (t == KW_DICT) return ATTR_TYPE_LIST(Dict, v, l);
                return ATTR_TYPE_LIST(Tuple, v, l);
            }
            default: unsup(__LINE__);
        }
        if (K[i] == TK_LPAREN) {
            Vec<kind_item_t> kind = paren_kind_args();
            l = span(s);
            return make_AttrType_t(m_a, l, type, kind.p, kind.size(), nullptr,
                nullptr, None);
        }
        if (K[i] == TK_STAR) {
            if (K[i+1] == TK_INTEGER) {
                const YYSTYPE &v = V[i+1];
                i += 2;
                l = span(s);
                ast_t *r;
                switch (t) {
                    case KW_INTEGER: r = ATTR_TYPE_INT(Integer, v.int_suffix, l); WARN_INTEGERSTAR(v.int_suffix, l); break;
                    case KW_REAL: r = ATTR_TYPE_INT(Real, v.int_suffix, l); WARN_REALSTAR(v.int_suffix, l); break;
                    case KW_COMPLEX: r = ATTR_TYPE_INT(Complex, DIV2(v.int_suffix), l); WARN_COMPLEXSTAR(v.int_suffix, l); break;
                    case KW_LOGICAL: r = ATTR_TYPE_INT(Logical, v.int_suffix, l); WARN_LOGICALSTAR(v.int_suffix, l); break;
                    default: r = ATTR_TYPE_INT(Character, v.int_suffix, l); WARN_CHARACTERSTAR(v.int_suffix, l); break;
                }
                return r;
            }
            if (t == KW_CHARACTER && K[i+1] == TK_LPAREN) {
                i += 2;
                if (K[i] == TK_STAR && K[i+1] == TK_RPAREN) {
                    i += 2;
                    l = span(s);
                    return ATTR_TYPE_STAR(Character, DoubleAsterisk, l);
                }
                ast_t *e = expr();
                expect_(TK_RPAREN, __LINE__);
                l = span(s);
                ast_t *r = ATTR_TYPE_EXPR(Character, e, l);
                WARN_CHARACTERSTAR_EXPR(l);
                return r;
            }
        }
        l = span(s);
        return make_AttrType_t(m_a, l, type, nullptr, 0, nullptr, nullptr, None);
    }

    ast_t* var_type() {
        size_t s = i;
        if (is_intrinsic_type_start()) return intrinsic_type_spec();
        int t = K[i];
        i++;
        expect_(TK_LPAREN, __LINE__);
        Location l;
        if (t == KW_PROCEDURE) {
            if (accept(TK_RPAREN)) {
                l = span(s);
                return ATTR_TYPE(Procedure, l);
            }
            if (is_intrinsic_type_start() && K[i+1] != TK_RPAREN) {
                // The location of the inner type is the one of the whole
                // var_type, as in the Bison grammar
                int u = K[i];
                i++;
                decl_typeType type;
                bool kind = true;
                switch (u) {
                    case KW_INTEGER: type = decl_typeType::TypeInteger; break;
                    case KW_REAL: type = decl_typeType::TypeReal; break;
                    case KW_COMPLEX: type = decl_typeType::TypeComplex; break;
                    case KW_LOGICAL: type = decl_typeType::TypeLogical; break;
                    case KW_CHARACTER: type = decl_typeType::TypeCharacter; break;
                    case KW_DOUBLE: {
                        expect_(KW_PRECISION, __LINE__);
                        type = decl_typeType::TypeDoublePrecision;
                        kind = false;
                        break;
                    }
                    default: unsup(__LINE__);
                }
                Vec<kind_item_t> k;
                if (kind) k = paren_kind_args();
                expect_(TK_RPAREN, __LINE__);
                l = span(s);
                ast_t *inner;
                if (kind) {
                    inner = make_AttrType_t(m_a, l, type, k.p, k.size(),
                        nullptr, nullptr, None);
                } else {
                    inner = make_AttrType_t(m_a, l, type, nullptr, 0, nullptr,
                        nullptr, None);
                }
                return ATTR_TYPE_ATTR(Procedure, inner, l);
            }
            ast_t *n = id();
            expect_(TK_RPAREN, __LINE__);
            l = span(s);
            return ATTR_TYPE_NAME(Procedure, n, l);
        }
        if (accept(TK_STAR)) {
            expect_(TK_RPAREN, __LINE__);
            l = span(s);
            if (t == KW_TYPE) return ATTR_TYPE_STAR(Type, Asterisk, l);
            return ATTR_TYPE_STAR(Class, Asterisk, l);
        }
        if (t == KW_TYPE && is_intrinsic_type_start()) {
            ast_t *it = intrinsic_type_spec();
            expect_(TK_RPAREN, __LINE__);
            l = span(s);
            return ATTR_TYPE_ATTR(Type, it, l);
        }
        ast_t *n = id();
        if (K[i] == TK_LPAREN) {
            Vec<kind_item_t> kind = paren_kind_args();
            expect_(TK_RPAREN, __LINE__);
            l = span(s);
            if (t == KW_TYPE) return ATTR_TYPE_NAME_KIND(Type, n, kind, l);
            return ATTR_TYPE_NAME_KIND(Class, n, kind, l);
        }
        expect_(TK_RPAREN, __LINE__);
        l = span(s);
        if (t == KW_TYPE) return ATTR_TYPE_NAME(Type, n, l);
        return ATTR_TYPE_NAME(Class, n, l);
    }

    Vec<kind_item_t> kind_arg_list() {
        Vec<kind_item_t> kind;
        LIST_NEW(kind);
        do {
            kind_item_t *k = kind_arg();
            LIST_ADD(kind, *k);
        } while (accept(TK_COMMA));
        return kind;
    }

    kind_item_t* kind_arg() {
        size_t s = i;
        Location l;
        if (is_name(K[i]) && K[i+1] == TK_EQUAL) {
            ast_t *n = id();
            i++;
            if (accept(TK_STAR)) {
                l = span(s);
                return KIND_ARG2S(n, l);
            }
            if (accept(TK_COLON)) {
                l = span(s);
                return KIND_ARG2C(n, l);
            }
            ast_t *e = expr();
            l = span(s);
            return KIND_ARG2(n, e, l);
        }
        if (accept(TK_STAR)) {
            l = span(s);
            return KIND_ARG1S(l);
        }
        if (accept(TK_COLON)) {
            l = span(s);
            return KIND_ARG1C(l);
        }
        ast_t *e = expr();
        l = span(s);
        return KIND_ARG1(e, l);
    }

    ast_t* bind() {
        size_t s = i;
        i++;
        Vec<ArgStarKw> a = write_args();
        Location l = span(s);
        return BIND2(a, l);
    }

    ast_t* result_clause() {
        i++;
        expect_(TK_LPAREN, __LINE__);
        ast_t *r = id();
        expect_(TK_RPAREN, __LINE__);
        return r;
    }

    // var_modifier; returns nullptr (and consumes nothing) if there is none
    ast_t* var_modifier() {
        size_t s = i;
        int t = K[i];
        if (t == KW_BIND) {
            if (K[i+1] != TK_LPAREN) return nullptr;
            ast_t *b = bind();
            Location l = span(s);
            return BIND(b, l);
        }
        i++;
        Location l = span(s);
        switch (t) {
            case KW_PARAMETER: return SIMPLE_ATTR(Parameter, l);
            case KW_ALLOCATABLE: return SIMPLE_ATTR(Allocatable, l);
            case KW_ASYNCHRONOUS: return SIMPLE_ATTR(Asynchronous, l);
            case KW_POINTER: return SIMPLE_ATTR(Pointer, l);
            case KW_TARGET: return SIMPLE_ATTR(Target, l);
            case KW_OPTIONAL: return SIMPLE_ATTR(Optional, l);
            case KW_PROTECTED: return SIMPLE_ATTR(Protected, l);
            case KW_SAVE: return SIMPLE_ATTR(Save, l);
            case KW_SEQUENCE: return SIMPLE_ATTR(Sequence, l);
            case KW_CONTIGUOUS: return SIMPLE_ATTR(Contiguous, l);
            case KW_NOPASS: return SIMPLE_ATTR(NoPass, l);
            case KW_PRIVATE: return SIMPLE_ATTR(Private, l);
            case KW_PUBLIC: return SIMPLE_ATTR(Public, l);
            case KW_ABSTRACT: return SIMPLE_ATTR(Abstract, l);
            case KW_ENUMERATOR: return SIMPLE_ATTR(Enumerator, l);
            case KW_EXTERNAL: return SIMPLE_ATTR(External, l);
            case KW_INTRINSIC: return SIMPLE_ATTR(Intrinsic, l);
            case KW_VALUE: return SIMPLE_ATTR(Value, l);
            case KW_VOLATILE: return SIMPLE_ATTR(Volatile, l);
            case KW_KIND: return SIMPLE_ATTR(Kind, l);
            case KW_LEN: return SIMPLE_ATTR(Len, l);
            case KW_PASS: {
                if (accept(TK_LPAREN)) {
                    ast_t *n = id();
                    expect_(TK_RPAREN, __LINE__);
                    l = span(s);
                    return PASS(n, l);
                }
                return PASS(nullptr, l);
            }
            case KW_EXTENDS: {
                expect_(TK_LPAREN, __LINE__);
                ast_t *n = id();
                expect_(TK_RPAREN, __LINE__);
                l = span(s);
                return EXTENDS(n, l);
            }
            case KW_DIMENSION: {
                if (accept(TK_LPAREN)) {
                    Vec<dimension_t> d = array_comp_decl_list();
                    expect_(TK_RPAREN, __LINE__);
                    l = span(s);
                    return DIMENSION(d, l);
                }
                return DIMENSION0(l);
            }
            case KW_CODIMENSION: {
                expect_(TK_LBRACKET, __LINE__);
                Vec<codimension_t> d = coarray_comp_decl_list();
                expect_(TK_RBRACKET, __LINE__);
                l = span(s);
                return CODIMENSION(d, l);
            }
            case KW_INTENT: {
                expect_(TK_LPAREN, __LINE__);
                int u = K[i++];
                expect_(TK_RPAREN, __LINE__);
                l = span(s);
                if (u == KW_IN) return INTENT(In, l);
                if (u == KW_OUT) return INTENT(Out, l);
                if (u == KW_INOUT || u == KW_IN_OUT) return INTENT(InOut, l);
                unsup(__LINE__);
            }
            default: {
                i = s;
                return nullptr;
            }
        }
    }

    Vec<dimension_t> array_comp_decl_list() {
        Vec<dimension_t> v;
        LIST_NEW(v);
        do {
            dimension_t *d = array_comp_decl();
            PLIST_ADD(v, d);
        } while (accept(TK_COMMA));
        return v;
    }

    dimension_t* array_comp_decl() {
        size_t s = i;
        Location l;
        if (accept(TK_COLON)) {
            if (at_arg_end()) {
                l = span(s);
                return ARRAY_COMP_DECL5d(l);
            }
            ast_t *b = expr();
            l = span(s);
            return ARRAY_COMP_DECL4d(b, l);
        }
        if (accept(TK_STAR)) {
            l = span(s);
            return ARRAY_COMP_DECL6d(l);
        }
        if (accept(TK_DBL_DOT)) {
            l = span(s);
            return ARRAY_COMP_DECL8d(l);
        }
        ast_t *a = expr();
        if (accept(TK_COLON)) {
            if (at_arg_end()) {
                l = span(s);
                return ARRAY_COMP_DECL3d(a, l);
            }
            if (accept(TK_STAR)) {
                l = span(s);
                return ARRAY_COMP_DECL7d(a, l);
            }
            ast_t *b = expr();
            l = span(s);
            return ARRAY_COMP_DECL2d(a, b, l);
        }
        l = span(s);
        return ARRAY_COMP_DECL1d(a, l);
    }

    Vec<codimension_t> coarray_comp_decl_list() {
        Vec<codimension_t> v;
        LIST_NEW(v);
        do {
            size_t s = i;
            Location l;
            codimension_t *d;
            if (accept(TK_COLON)) {
                if (at_arg_end()) {
                    l = span(s);
                    d = COARRAY_COMP_DECL5d(l);
                } else {
                    ast_t *b = expr();
                    l = span(s);
                    d = COARRAY_COMP_DECL4d(b, l);
                }
            } else if (accept(TK_STAR)) {
                l = span(s);
                d = COARRAY_COMP_DECL6d(l);
            } else {
                ast_t *a = expr();
                if (accept(TK_COLON)) {
                    if (at_arg_end()) {
                        l = span(s);
                        d = COARRAY_COMP_DECL3d(a, l);
                    } else if (accept(TK_STAR)) {
                        l = span(s);
                        d = COARRAY_COMP_DECL7d(a, l);
                    } else {
                        ast_t *b = expr();
                        l = span(s);
                        d = COARRAY_COMP_DECL2d(a, b, l);
                    }
                } else {
                    l = span(s);
                    d = COARRAY_COMP_DECL1d(a, l);
                }
            }
            PLIST_ADD(v, d);
        } while (accept(TK_COMMA));
        return v;
    }

    // "*" "(" "*" ")" at the current token?
    bool star_paren_star() const {
        return K[i] == TK_STAR && K[i+1] == TK_LPAREN && K[i+2] == TK_STAR
            && K[i+3] == TK_RPAREN;
    }

    var_sym_t* var_sym_decl() {
        size_t s = i;
        Location l;
        if (K[i] == TK_SLASH) {
            i++;
            ast_t *n = id();
            expect_(TK_SLASH, __LINE__);
            l = span(s);
            return VAR_SYM_NAME(n, Slash, l);
        }
        {
            intrinsicopType op;
            size_t defop;
            ast_t *name;
            Generic g = generic_spec(op, defop, name, false);
            if (g == Generic::Op) {
                l = span(s);
                return VAR_SYM_SPEC(DECL_OP(op, l), None, l);
            } else if (g == Generic::Defop) {
                l = span(s);
                return VAR_SYM_SPEC(DECL_DEFOP(V[defop].string, l), None, l);
            } else if (g == Generic::Assignment) {
                l = span(s);
                return VAR_SYM_SPEC(DECL_ASSIGNMENT(l), None, l);
            } else if (g != Generic::None) {
                unsup(__LINE__);
            }
        }
        ast_t *n = id();
        dimension_t *dims = nullptr;
        size_t n_dims = 0;
        bool has_dims = false;
        if (accept(TK_LPAREN)) {
            Vec<dimension_t> d = array_comp_decl_list();
            expect_(TK_RPAREN, __LINE__);
            dims = d.p;
            n_dims = d.n;
            has_dims = true;
        }
        if (accept(TK_LBRACKET)) {
            Vec<codimension_t> c = coarray_comp_decl_list();
            expect_(TK_RBRACKET, __LINE__);
            if (accept(TK_EQUAL)) {
                ast_t *e = expr();
                l = span(s);
                if (has_dims) {
                    return VAR_SYM_DIM_CODIM_INIT(n, dims, n_dims, c.p, c.n, e,
                        Equal, l);
                }
                return VAR_SYM_CODIM_INIT(n, c.p, c.n, e, Equal, l);
            }
            l = span(s);
            if (has_dims) {
                return VAR_SYM_DIM_CODIM(n, dims, n_dims, c.p, c.n, None, l);
            }
            return VAR_SYM_CODIM(n, c.p, c.n, None, l);
        }
        if (star_paren_star()) {
            i += 4;
            if (accept(TK_EQUAL)) {
                ast_t *e = expr();
                l = span(s);
                return VAR_SYM_DIM_INIT(n, dims, n_dims, e, DoubleAsterisk, l);
            }
            l = span(s);
            if (has_dims) return VAR_SYM_DIM(n, dims, n_dims, DoubleAsterisk, l);
            return VAR_SYM_NAME(n, DoubleAsterisk, l);
        }
        if (accept(TK_STAR)) {
            ast_t *len = expr();
            if (accept(TK_EQUAL)) {
                ast_t *e = expr();
                l = span(s);
                return VAR_SYM_DIM_LEN_INIT(n, dims, n_dims, len, e, Equal, l);
            }
            l = span(s);
            return VAR_SYM_DIM_LEN(n, dims, n_dims, len, Asterisk, l);
        }
        if (accept(TK_EQUAL)) {
            ast_t *e = expr();
            l = span(s);
            return VAR_SYM_DIM_INIT(n, dims, n_dims, e, Equal, l);
        }
        if (accept(TK_ARROW)) {
            ast_t *e = expr();
            l = span(s);
            return VAR_SYM_DIM_INIT(n, dims, n_dims, e, Arrow, l);
        }
        if (accept(TK_SLASH)) {
            Vec<ast_t*> values;
            LIST_NEW(values);
            do {
                LIST_ADD(values, data_stmt_constant());
            } while (accept(TK_COMMA));
            expect_(TK_SLASH, __LINE__);
            l = span(s);
            return VAR_SYM_DIM_INIT(n, dims, n_dims, SLASH_INIT_EXPR(values, l),
                SlashInit, l);
        }
        l = span(s);
        if (has_dims) return VAR_SYM_DIM(n, dims, n_dims, None, l);
        return VAR_SYM_NAME(n, None, l);
    }

    // var_modifiers: [ [ "," var_modifier ]... "::" ]
    Vec<ast_t*> var_modifiers() {
        Vec<ast_t*> v;
        LIST_NEW(v);
        if (K[i] == TK_COMMA) {
            while (accept(TK_COMMA)) {
                ast_t *m = var_modifier();
                if (m == nullptr) unsup(__LINE__);
                LIST_ADD(v, m);
            }
            expect_(TK_DBL_COLON, __LINE__);
        } else {
            accept(TK_DBL_COLON);
        }
        return v;
    }

    Vec<ast_t*> var_decl_star() {
        Vec<ast_t*> v;
        LIST_NEW(v);
        for (;;) {
            size_t s = i;
            if (K[i] == KW_CONTAINS || K[i] == KW_END_TYPE
                    || K[i] == KW_ENDTYPE || K[i] == KW_END_ENUM
                    || K[i] == KW_ENDENUM || K[i] == KW_END_UNION_TYPE) return v;
            ast_t *d = var_decl();
            if (d == nullptr) {
                i = s;
                return v;
            }
            LIST_ADD(v, d);
        }
    }

    ast_t* derived_type_decl() {
        size_t s = i;
        i++;
        Vec<ast_t*> mods = var_modifiers();
        ast_t *name = id();
        Vec<ast_t*> params;
        bool has_params = false;
        if (accept(TK_LPAREN)) {
            params = id_list();
            expect_(TK_RPAREN, __LINE__);
            has_params = true;
        }
        Vec<ast_t*> sp = sep();
        Vec<ast_t*> decls = var_decl_star();
        Vec<ast_t*> contains;
        LIST_NEW(contains);
        if (accept(KW_CONTAINS)) {
            sep();
            for (;;) {
                ast_t *pd = procedure_decl();
                if (pd == nullptr) break;
                LIST_ADD(contains, pd);
            }
        }
        if (!accept(KW_END_TYPE)) expect_(KW_ENDTYPE, __LINE__);
        id_opt();
        Vec<ast_t*> sp2 = sep();
        Location l = span(s);
        if (has_params) {
            return DERIVED_TYPE1(mods, name, params, TRIVIA(sp, sp2, l), decls,
                contains, l);
        }
        return DERIVED_TYPE(mods, name, TRIVIA(sp, sp2, l), decls, contains, l);
    }

    // proc_modifiers
    Vec<ast_t*> proc_modifiers() {
        Vec<ast_t*> v;
        LIST_NEW(v);
        if (K[i] != TK_COMMA) {
            accept(TK_DBL_COLON);
            return v;
        }
        while (accept(TK_COMMA)) {
            size_t s = i;
            int t = K[i];
            i++;
            Location l = span(s);
            switch (t) {
                case KW_PRIVATE: LIST_ADD(v, SIMPLE_ATTR(Private, l)); break;
                case KW_PUBLIC: LIST_ADD(v, SIMPLE_ATTR(Public, l)); break;
                case KW_NOPASS: LIST_ADD(v, SIMPLE_ATTR(NoPass, l)); break;
                case KW_DEFERRED: LIST_ADD(v, SIMPLE_ATTR(Deferred, l)); break;
                case KW_NON_OVERRIDABLE: LIST_ADD(v, SIMPLE_ATTR(NonDeferred, l)); break;
                case KW_PASS: {
                    if (accept(TK_LPAREN)) {
                        ast_t *n = id();
                        expect_(TK_RPAREN, __LINE__);
                        l = span(s);
                        LIST_ADD(v, PASS(n, l));
                    } else {
                        LIST_ADD(v, PASS(nullptr, l));
                    }
                    break;
                }
                default: unsup(__LINE__);
            }
        }
        expect_(TK_DBL_COLON, __LINE__);
        return v;
    }

    // procedure_decl; returns nullptr if there is none here
    ast_t* procedure_decl() {
        size_t s = i;
        Location l;
        switch (K[i]) {
            case KW_PROCEDURE: {
                i++;
                if (accept(TK_LPAREN)) {
                    ast_t *iface = id();
                    expect_(TK_RPAREN, __LINE__);
                    Vec<ast_t*> mods = proc_modifiers();
                    Vec<ast_t*> syms = use_symbol_list();
                    Vec<ast_t*> sp = sep();
                    l = span(s);
                    return DERIVED_TYPE_PROC1(iface, mods, syms,
                        TRIVIA_AFTER(sp, l), l);
                }
                Vec<ast_t*> mods = proc_modifiers();
                Vec<ast_t*> syms = use_symbol_list();
                Vec<ast_t*> sp = sep();
                l = span(s);
                return DERIVED_TYPE_PROC(mods, syms, TRIVIA_AFTER(sp, l), l);
            }
            case KW_GENERIC: {
                i++;
                Vec<ast_t*> access;
                LIST_NEW(access);
                if (accept(TK_COMMA)) {
                    size_t s2 = i - 1;
                    int t = K[i];
                    i++;
                    Location l2 = span(s2);
                    if (t == KW_PRIVATE) {
                        LIST_ADD(access, SIMPLE_ATTR(Private, l2));
                    } else if (t == KW_PUBLIC) {
                        LIST_ADD(access, SIMPLE_ATTR(Public, l2));
                    } else {
                        unsup(__LINE__);
                    }
                }
                expect_(TK_DBL_COLON, __LINE__);
                intrinsicopType op;
                size_t defop;
                ast_t *name;
                Generic g = generic_spec(op, defop, name);
                ast_t *gname = nullptr;
                Location lop = span(s);
                if (g == Generic::None) gname = id();
                expect_(TK_ARROW, __LINE__);
                Vec<ast_t*> ids = id_list();
                Vec<ast_t*> sp = sep();
                l = span(s);
                (void)lop;
                switch (g) {
                    case Generic::Op: return GENERIC_OPERATOR(access, op, ids, TRIVIA_AFTER(sp, l), l);
                    case Generic::Defop: return GENERIC_DEFOP(access, V[defop].string, ids, TRIVIA_AFTER(sp, l), l);
                    case Generic::Assignment: return GENERIC_ASSIGNMENT(access, ids, TRIVIA_AFTER(sp, l), l);
                    case Generic::Write: return GENERIC_WRITE(access, name, ids, TRIVIA_AFTER(sp, l), l);
                    case Generic::Read: return GENERIC_READ(access, name, ids, TRIVIA_AFTER(sp, l), l);
                    default: return GENERIC_NAME(access, gname, ids, TRIVIA_AFTER(sp, l), l);
                }
            }
            case KW_FINAL: {
                i++;
                expect_(TK_DBL_COLON, __LINE__);
                ast_t *n = id();
                Vec<ast_t*> sp = sep();
                l = span(s);
                return FINAL_NAME(n, TRIVIA_AFTER(sp, l), l);
            }
            case KW_PRIVATE: {
                i++;
                Vec<ast_t*> sp = sep();
                l = span(s);
                return PRIVATE(Private, TRIVIA_AFTER(sp, l), l);
            }
            default: return nullptr;
        }
    }

    ast_t* enum_decl() {
        size_t s = i;
        i++;
        Vec<ast_t*> mods;
        LIST_NEW(mods);
        while (accept(TK_COMMA)) {
            ast_t *m = var_modifier();
            if (m == nullptr) unsup(__LINE__);
            LIST_ADD(mods, m);
        }
        Vec<ast_t*> sp = sep();
        Vec<ast_t*> decls = var_decl_star();
        if (!accept(KW_END_ENUM)) expect_(KW_ENDENUM, __LINE__);
        Vec<ast_t*> sp2 = sep();
        Location l = span(s);
        return ENUM(mods, TRIVIA(sp, sp2, l), decls, l);
    }

    ast_t* interface_decl() {
        size_t s = i;
        Location l;
        ast_t *header;
        if (accept(KW_ABSTRACT)) {
            i++;
            l = span(s);
            header = ABSTRACT_INTERFACE_HEADER(l);
        } else if (accept(KW_DEFERRED)) {
            i++;
            l = span(s);
            header = DEFERRED_INTERFACE_HEADER(l);
        } else {
            i++;
            intrinsicopType op;
            size_t defop;
            ast_t *name;
            Generic g = generic_spec(op, defop, name);
            if (g == Generic::None && is_name(K[i])) {
                ast_t *n = id();
                l = span(s);
                header = INTERFACE_HEADER_NAME(n, l);
            } else {
                l = span(s);
                switch (g) {
                    case Generic::Op: header = INTERFACE_HEADER_OPERATOR(op, l); break;
                    case Generic::Defop: header = INTERFACE_HEADER_DEFOP(V[defop].string, l); break;
                    case Generic::Assignment: header = INTERFACE_HEADER_ASSIGNMENT(l); break;
                    case Generic::Write: header = INTERFACE_HEADER_WRITE(name, l); break;
                    case Generic::Read: header = INTERFACE_HEADER_READ(name, l); break;
                    default: header = INTERFACE_HEADER(l); break;
                }
            }
        }
        Vec<ast_t*> sp = sep();
        Vec<ast_t*> items;
        LIST_NEW(items);
        for (;;) {
            ast_t *item = interface_item();
            if (item == nullptr) break;
            LIST_ADD(items, item);
        }
        if (!accept(KW_END_INTERFACE)) expect_(KW_ENDINTERFACE, __LINE__);
        {
            intrinsicopType op;
            size_t defop;
            ast_t *name;
            if (generic_spec(op, defop, name) == Generic::None) id_opt();
        }
        Vec<ast_t*> sp2 = sep();
        l = span(s);
        return INTERFACE(header, TRIVIA(sp, sp2, l), items, l);
    }

    // interface_item; returns nullptr at the end of the interface
    ast_t* interface_item() {
        size_t s = i;
        if (K[i] == KW_END_INTERFACE || K[i] == KW_ENDINTERFACE) return nullptr;
        Vec<ast_t*> mods = fn_mods();
        Location l;
        if (K[i] == KW_PROCEDURE) {
            i++;
            accept(TK_DBL_COLON);
            Vec<ast_t*> ids = id_list();
            Vec<ast_t*> sp = sep();
            l = span(s);
            if (mods.size() > 0) {
                return INTERFACE_MODULE_PROC1(mods, ids, TRIVIA_AFTER(sp, l), l);
            }
            return INTERFACE_MODULE_PROC(ids, TRIVIA_AFTER(sp, l), l);
        }
        ast_t *u = sub_or_func_rest(s, mods, false);
        if (u == nullptr) unsup(__LINE__);
        return INTERFACE_PROC(u, u->loc);
    }

    // ------------------------------------------------------------------
    // Program units

    // fn_mod_plus, possibly empty
    Vec<ast_t*> fn_mods() {
        Vec<ast_t*> v;
        LIST_NEW(v);
        for (;;) {
            Location l = Lc[i];
            switch (K[i]) {
                case KW_ELEMENTAL: i++; LIST_ADD(v, SIMPLE_ATTR(Elemental, l)); break;
                case KW_IMPURE: i++; LIST_ADD(v, SIMPLE_ATTR(Impure, l)); break;
                case KW_MODULE: i++; LIST_ADD(v, SIMPLE_ATTR(Module, l)); break;
                case KW_PURE: i++; LIST_ADD(v, SIMPLE_ATTR(Pure, l)); break;
                case KW_RECURSIVE: i++; LIST_ADD(v, SIMPLE_ATTR(Recursive, l)); break;
                case KW_NON_RECURSIVE: i++; LIST_ADD(v, SIMPLE_ATTR(NonRecursive, l)); break;
                default: {
                    if (!is_var_type_start()) return v;
                    LIST_ADD(v, var_type());
                }
            }
        }
    }

    // sub_args
    Vec<ast_t*> sub_args() {
        Vec<ast_t*> args;
        LIST_NEW(args);
        if (accept(TK_LPAREN)) {
            if (K[i] != TK_RPAREN) {
                do {
                    if (accept(TK_STAR)) {
                        LIST_ADD(args, nullptr);
                    } else {
                        LIST_ADD(args, id());
                    }
                } while (accept(TK_COMMA));
            }
            expect_(TK_RPAREN, __LINE__);
        }
        return args;
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

    // [ contains_block ] end, where `end` is `end` or one of the two given
    // tokens, optionally with a label
    ContainsEnd* contains_end(int end1, int end2) {
        Vec<ast_t*> contains;
        bool has_contains = false;
        if (K[i] == KW_CONTAINS) {
            contains = contains_block();
            has_contains = true;
        }
        EndStmt end = end_stmt(end1, end2);
        if (has_contains) return CONTAINS_END2(m_a, contains, end);
        return CONTAINS_END1(m_a, end);
    }

    EndStmt end_stmt(int end1, int end2) {
        size_t s = i;
        int64_t label = 0;
        if (K[i] == TK_LABEL) {
            label = V[i].n;
            i++;
        }
        ast_t *name = nullptr;
        if (accept(KW_END)) {
        } else if (accept(end1) || accept(end2)) {
            if (is_name(K[i])) name = id();
        } else {
            unsup(__LINE__);
        }
        Location l = span(s);
        return END_STMT(name, label, l);
    }

    ast_t* program() {
        size_t s = i;
        i++;
        ast_t *name = id();
        Vec<ast_t*> sp = sep();
        Vec<ast_t*> decls = decl_statements();
        ContainsEnd *ce = contains_end(KW_END_PROGRAM, KW_ENDPROGRAM);
        Location l = span(s);
        Vec<ast_t*> sp2 = sep();
        return PROGRAM(name, TRIVIA(sp, sp2, l), decls, ce, l);
    }

    // subroutine | function | procedure; returns nullptr if none starts here
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
        ast_t *u = sub_or_func_rest(s, mods, true);
        if (u == nullptr) i = s;
        return u;
    }

    ast_t* templated_unit(size_t s, Vec<ast_t*> &mods) {
        if (accept(KW_SUBROUTINE)) {
            ast_t *name = id();
            expect_(TK_LBRACE, __LINE__);
            Vec<ast_t*> temp_args = id_list();
            expect_(TK_RBRACE, __LINE__);
            Vec<ast_t*> args = sub_args();
            ast_t *b = nullptr;
            if (K[i] == KW_BIND) b = bind();
            Vec<ast_t*> sp = sep();
            Vec<ast_t*> decls = decl_statements();
            EndStmt end = end_stmt(KW_END_SUBROUTINE, KW_ENDSUBROUTINE);
            Location l = span(s);
            Vec<ast_t*> sp2 = sep();
            return TEMPLATED_SUBROUTINE1(mods, name, temp_args, args, b,
                TRIVIA(sp, sp2, l), decls, end, l);
        }
        expect_(KW_FUNCTION, __LINE__);
        ast_t *name = id();
        expect_(TK_LBRACE, __LINE__);
        Vec<ast_t*> temp_args = id_list();
        expect_(TK_RBRACE, __LINE__);
        expect_(TK_LPAREN, __LINE__);
        Vec<ast_t*> args;
        if (K[i] == TK_RPAREN) {
            LIST_NEW(args);
        } else {
            args = id_list();
        }
        expect_(TK_RPAREN, __LINE__);
        ast_t *res = nullptr, *b = nullptr;
        if (K[i] == KW_RESULT) res = result_clause();
        if (K[i] == KW_BIND) b = bind();
        Vec<ast_t*> sp = sep();
        Vec<ast_t*> decls = decl_statements();
        EndStmt end = end_stmt(KW_END_FUNCTION, KW_ENDFUNCTION);
        Location l = span(s);
        Vec<ast_t*> sp2 = sep();
        return TEMPLATED_FUNCTION(mods, name, temp_args, args, res, b,
            TRIVIA(sp, sp2, l), decls, end, l);
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

    // The unit that starts at token `s` with the prefixes `mods`
    ast_t* sub_or_func_rest(size_t s, Vec<ast_t*> &mods, bool allow_procedure) {
        bool has_mods = mods.size() > 0;
        if (K[i] == KW_SUBROUTINE) {
            i++;
            ast_t *name = id();
            Vec<ast_t*> args = sub_args();
            ast_t *b = nullptr;
            if (K[i] == KW_BIND) b = bind();
            Vec<ast_t*> sp = sep();
            Vec<ast_t*> decls = decl_statements();
            ContainsEnd *ce = contains_end(KW_END_SUBROUTINE, KW_ENDSUBROUTINE);
            Location l = span(s);
            Vec<ast_t*> sp2 = sep();
            if (has_mods) {
                return SUBROUTINE1(mods, name, args, b, TRIVIA(sp, sp2, l),
                    decls, ce, l);
            }
            return SUBROUTINE(name, args, b, TRIVIA(sp, sp2, l), decls, ce, l);
        }
        if (K[i] == KW_FUNCTION) {
            i++;
            ast_t *name = id();
            expect_(TK_LPAREN, __LINE__);
            Vec<ast_t*> args;
            if (K[i] == TK_RPAREN) {
                LIST_NEW(args);
            } else {
                args = id_list();
            }
            expect_(TK_RPAREN, __LINE__);
            ast_t *res = nullptr, *b = nullptr;
            for (;;) {
                if (K[i] == KW_RESULT && res == nullptr) {
                    res = result_clause();
                } else if (K[i] == KW_BIND && b == nullptr) {
                    b = bind();
                } else {
                    break;
                }
            }
            Vec<ast_t*> sp = sep();
            Vec<ast_t*> decls = decl_statements();
            ContainsEnd *ce = contains_end(KW_END_FUNCTION, KW_ENDFUNCTION);
            Location l = span(s);
            Vec<ast_t*> sp2 = sep();
            if (has_mods) {
                return FUNCTION(mods, name, args, res, b, TRIVIA(sp, sp2, l),
                    decls, ce, l);
            }
            return FUNCTION0(name, args, res, b, TRIVIA(sp, sp2, l), decls,
                ce, l);
        }
        if (K[i] == KW_PROCEDURE && has_mods && allow_procedure) {
            i++;
            ast_t *name = id();
            Vec<ast_t*> args = sub_args();
            Vec<ast_t*> sp = sep();
            Vec<ast_t*> decls = decl_statements();
            ContainsEnd *ce = contains_end(KW_END_PROCEDURE, KW_ENDPROCEDURE);
            // The location of this unit includes the separator after it
            Vec<ast_t*> sp2 = sep();
            Location l = span(s);
            return PROCEDURE(mods, name, args, TRIVIA(sp, sp2, l), decls, ce, l);
        }
        return nullptr;
    }

    Vec<ast_t*> contains_block_opt() {
        if (K[i] == KW_CONTAINS) return contains_block();
        Vec<ast_t*> v;
        LIST_NEW(v);
        return v;
    }

    ast_t* module() {
        size_t s = i;
        i++;
        ast_t *name = id();
        Vec<ast_t*> sp = sep();
        Vec<ast_t*> decls = decl_statements();
        Vec<ast_t*> contains = contains_block_opt();
        ast_t *end_name = nullptr;
        if (accept(KW_END)) {
        } else if (accept(KW_END_MODULE) || accept(KW_ENDMODULE)) {
            if (is_name(K[i])) end_name = id();
        } else {
            unsup(__LINE__);
        }
        Location l = span(s);
        Vec<ast_t*> sp2 = sep();
        return MODULE(name, TRIVIA(sp, sp2, l), decls, contains, end_name, l);
    }

    ast_t* submodule() {
        size_t s = i;
        i++;
        expect_(TK_LPAREN, __LINE__);
        ast_t *id1 = id();
        ast_t *id2 = nullptr;
        if (accept(TK_COLON)) id2 = id();
        expect_(TK_RPAREN, __LINE__);
        ast_t *name = id();
        Vec<ast_t*> sp = sep();
        Vec<ast_t*> decls = decl_statements();
        Vec<ast_t*> contains = contains_block_opt();
        ast_t *end_name = nullptr;
        if (accept(KW_END)) {
        } else if (accept(KW_END_SUBMODULE) || accept(KW_ENDSUBMODULE)) {
            if (is_name(K[i])) end_name = id();
        } else {
            unsup(__LINE__);
        }
        Location l = span(s);
        Vec<ast_t*> sp2 = sep();
        if (id2) {
            return SUBMODULE1(id1, id2, name, TRIVIA(sp, sp2, l), decls,
                contains, end_name, l);
        }
        return SUBMODULE(id1, name, TRIVIA(sp, sp2, l), decls, contains,
            end_name, l);
    }

    ast_t* block_data() {
        size_t s = i;
        i += 2;
        ast_t *name = nullptr;
        if (is_name(K[i])) name = id();
        Vec<ast_t*> sp = sep();
        Vec<ast_t*> decls = decl_statements();
        if (accept(KW_END)) {
        } else if (accept(KW_END_BLOCK_DATA) || accept(KW_ENDBLOCKDATA)) {
            id_opt();
        } else {
            unsup(__LINE__);
        }
        Vec<ast_t*> sp2 = sep();
        Location l = span(s);
        if (name) return BLOCKDATA1(name, TRIVIA(sp, sp2, l), decls, l);
        return BLOCKDATA(TRIVIA(sp, sp2, l), decls, l);
    }

    void units() {
        if (is_sep(K[i])) sep();
        while (K[i] != END_OF_FILE) {
            ast_t *u;
            switch (K[i]) {
                case KW_PROGRAM: u = program(); break;
                case KW_SUBMODULE: u = submodule(); break;
                case KW_MODULE: {
                    if (is_name(K[i+1]) && is_sep(K[i+2])) {
                        u = module();
                    } else {
                        u = sub_or_func();
                        if (u == nullptr) u = top_level_item();
                    }
                    break;
                }
                case KW_BLOCK: {
                    if (K[i+1] == KW_DATA) {
                        u = block_data();
                    } else {
                        u = top_level_item();
                    }
                    break;
                }
                default: {
                    u = sub_or_func();
                    if (u == nullptr) u = top_level_item();
                }
            }
            RESULT(u);
        }
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
