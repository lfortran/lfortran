// The LL grammar of free-form Fortran, with predicates and actions.
//
// llgen.py generates ll_parser.cpp from this file; see llgen.py for the
// format. The parser:
//
// * tokenizes the whole file first, with the same re2c tokenizer as the Bison
//   parser, into arrays; a keyword is just a kind of name (`is_name()`);
// * builds the AST with the same macros and functions as the Bison parser
//   (semantics.h), so both parsers return the same AST, with locations;
// * reports no syntax errors: at anything it cannot parse it throws
//   `LLUnsupported`, and the caller parses the file again with the Bison
//   parser.
//
// It parses all the free-form files in integration_tests/ and tests/ that
// the Bison parser accepts, not fixed form or interactive input.
//
// Conventions in the actions: `K[i]` is the kind of the current token, `V[j]`
// and `Lc[j]` are the value and the location of token `j`, and a rule that
// takes `s` continues a construct that starts at token `s`.

%code {{
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

    // Gives up; `line` is the line of the grammar that did
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
    // Predicates

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

    // "*" "(" "*" ")" at the current token?
    bool star_paren_star() const {
        return K[i] == TK_STAR && K[i+1] == TK_LPAREN && K[i+2] == TK_STAR
            && K[i+3] == TK_RPAREN;
    }

    bool at_arg_end() const {
        return K[i] == TK_COMMA || K[i] == TK_RPAREN || K[i] == TK_RBRACKET;
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

}}

%first id: NAME
%first assignment_statement: NAME
%first designator: NAME
%first if_statement: KW_IF
%first do_statement: KW_DO KW_DOWHILE
%first where_statement: KW_WHERE
%first parenthesized: TK_LPAREN
%first deferred_decl: KW_DEFERRED

// ----------------------------------------------------------------------------
// Expressions

%code {{
    ast_t* expr() { return binary(1); }

}}

%operators binary prefix
    left 1 TK_DEF_OP { x = DEFOP(x, V[op].string, y, l); }
    left 2 TK_EQV { x = EQV(x, y, l); }
    left 2 TK_NEQV { x = NEQV(x, y, l); }
    left 3 TK_OR { x = OR(x, y, l); }
    left 3 TK_XOR { x = XOR(x, y, l); }
    left 4 TK_AND { x = AND(x, y, l); }
    left 6 TK_EQ { x = EQ(x, y, l); }
    left 6 TK_NE { x = NE(x, y, l); }
    left 6 TK_LT { x = LT(x, y, l); }
    left 6 TK_LE { x = LE(x, y, l); }
    left 6 TK_GT { x = GT(x, y, l); }
    left 6 TK_GE { x = GE(x, y, l); }
    left 7 TK_CONCAT { x = STRCONCAT(x, y, l); }
    left 8 TK_PLUS { x = ADD(x, y, l); }
    left 8 TK_MINUS { x = SUB(x, y, l); }
    left 9 TK_STAR { x = MUL(x, y, l); }
    left 9 TK_SLASH { x = DIV(x, y, l); }
    right 11 TK_POW { x = POW(x, y, l); }
%end

// The operand of a unary minus or plus is everything that binds tighter than
// they do (just `**`); the operand of `.not.` is a comparison or tighter
rule prefix() -> ast_t*
    : "-" x=binary(11) { return UNARY_MINUS(x, @$); }
    | "+" x=binary(11) { return UNARY_PLUS(x, @$); }
    | TK_NOT x=binary(6) { return NOT(x, @$); }
    | op=TK_DEF_OP x=primary() { return UNARY_DEFOP(V[op].string, x, @$); }
    | x=primary() { return x; }
    ;

rule primary() -> ast_t*
    : x=designator() { return x; }
    | v=TK_INTEGER { return INTEGER(V[v].int_suffix, @$); }
    | v=TK_REAL { return REAL(V[v].string, @$); }
    | v=TK_STRING
        ( "(" args=fnarray_arg_list_opt() ")"
            { return SUBSTRING(V[v].str_prefix, args, @$); } )?
        { return STRING(V[v].str_prefix, @$); }
    | v=TK_BOZ_CONSTANT { return BOZ(V[v].string, @$); }
    | v=TK_TRUE { return TRUE(V[v].string, @$); }
    | v=TK_FALSE { return FALSE(V[v].string, @$); }
    | x=parenthesized() { return x; }
    | "["
        ( &{ array_constructor_has_type() }
            ( &{ is_var_type_start() } vt=var_type() "::"
                v=expr_list_opt_bracket() { return ARRAY_IN2(vt, v, @$); }
            | n=id() "::" v=expr_list_opt_bracket()
                { return ARRAY_IN3(n, v, @$); }
            )
        )?
        v=expr_list_opt_bracket() { return ARRAY_IN1(v, @$); }
    ;

%code {{
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

    Vec<CoarrayArg> coarray_arg_list() {
        Vec<CoarrayArg> v;
        LIST_NEW(v);
        do {
            CoarrayArg *a = coarray_arg();
            PLIST_ADD(v, a);
        } while (accept(TK_COMMA));
        return v;
    }

}}

// An argument of a function call or an array reference: an expression, a
// section `a:b:c` with any part left out, `name = value` or an alternate
// return `*label`
rule fnarray_arg() -> FnArg*
    : &{ K[i+1] == TK_EQUAL } n=id() "=" e=expr()
        { return ARRAY_COMP_DECL1k(n, e, @$); }
    | "*" v=TK_INTEGER { return ARRAY_COMP_DECL_label(V[v].int_suffix, @$); }
    | "::" c=expr() { return ARRAY_COMP_DECL_00c(c, @$); }
    | ":"
        ( ":" c=expr() { return ARRAY_COMP_DECL_00c(c, @$); } )?
        ( &{ at_arg_end() } { return ARRAY_COMP_DECL_001(@$); } )?
        b=expr()
        ( ":" c=expr() { return ARRAY_COMP_DECL_0bc(b, c, @$); } )?
        { return ARRAY_COMP_DECL_0b1(b, @$); }
    | a=expr()
        ( "::" c=expr() { return ARRAY_COMP_DECL_a0c(a, c, @$); } )?
        ( ":"
            ( &{ at_arg_end() } { return ARRAY_COMP_DECL_a01(a, @$); } )?
            ( ":" c=expr() { return ARRAY_COMP_DECL_a0c(a, c, @$); } )?
            b=expr()
            ( ":" c=expr() { return ARRAY_COMP_DECL_abc(a, b, c, @$); } )?
            { return ARRAY_COMP_DECL_ab1(a, b, @$); }
        )?
        { return ARRAY_COMP_DECL_0i0(a, @$); }
    ;

rule coarray_arg() -> CoarrayArg*
    : &{ K[i+1] == TK_EQUAL } n=id() "=" e=expr()
        { return COARRAY_COMP_DECL1k(n, e, @$); }
    | "*" { return COARRAY_COMP_DECL_star(@$); }
    | ":"
        ( &{ at_arg_end() } { return COARRAY_COMP_DECL_001(@$); } )?
        b=expr() { return COARRAY_COMP_DECL_0b1(b, @$); }
    | a=expr()
        ( ":"
            ( &{ at_arg_end() } { return COARRAY_COMP_DECL_a01(a, @$); } )?
            ( "*" { return COARRAY_COMP_DECL_astar(a, @$); } )?
            b=expr() { return COARRAY_COMP_DECL_ab1(a, b, @$); }
        )?
        { return COARRAY_COMP_DECL_0i0(a, @$); }
    ;

// ----------------------------------------------------------------------------
// Statements

%code {{
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
        ast_t *x = designator();
        int op = K[i];
        i++;
        ast_t *y = expr();
        Location l = span(s);
        if (op == TK_ARROW) return ASSOCIATE(x, y, l);
        if (op == TK_COLON_EQUAL) return INFER_ASSIGNMENT(x, y, l);
        return ASSIGNMENT(x, y, l);
    }

}}

// "(" write_arg_list ")"
rule write_args() -> Vec<ArgStarKw>
    : "(" { Vec<ArgStarKw> v; LIST_NEW(v); }
        a=write_arg() { PLIST_ADD(v, a); }
        ( "," a<-write_arg() { PLIST_ADD(v, a); } )*
        ")" { return v; }
    ;

rule write_arg() -> ArgStarKw*
    : { ArgStarKw *a; ast_t *e = nullptr; }
        ( &{ is_name(K[i]) && K[i+1] == TK_EQUAL } n=id() "="
            ( "*" | e<-expr() ) { WRITE_ARG2(a, n, e); return a; }
        )?
        ( "*" | e<-expr() ) { WRITE_ARG1(a, e); return a; }
    ;

// [","] sync_stat ["," sync_stat]...
rule sync_stat_list() -> Vec<ast_t*>
    : { Vec<ast_t*> v; LIST_NEW(v); } ","?
        x=sync_stat() { LIST_ADD(v, x); }
        ( "," x<-sync_stat() { LIST_ADD(v, x); } )*
        { return v; }
    ;

rule sync_stat() -> ast_t*
    : KW_STAT "=" n=id() { return STAT(n, @$); }
    | KW_ERRMSG "=" n=id() { return ERRMSG(n, @$); }
    | KW_NEW_INDEX "=" e=expr() { return NEW_INDEX(e, @$); }
    ;

// A statement without its label and the separator after it. A statement that
// starts with a name is an assignment if is_assignment() says so, whatever
// the name is; only then is the name looked at as a keyword.
rule statement1() -> ast_t* try
    : &{ is_assignment() } x=assignment_statement() { return x; }
    | &{ K[i+1] == TK_COLON } n1=id() ":" st=statement1()
        { if (st == nullptr) unsup(__LINE__); }
        n2=id() { return stmt_name(n1, n2, st); }
    | &{ K[i+1] == TK_LPAREN } x=if_statement() { return x; }
    | x=do_statement() { return x; }
    | &{ K[i+1] == TK_LPAREN } x=where_statement() { return x; }
    | &{ K[i+1] == TK_LPAREN } x=forall_statement() { return x; }
    | KW_PRINT { ast_t *fmt = nullptr; } ( "*" | fmt<-expr() )
        ( &{ !is_sep(K[i+1]) } "," args=expr_list()
            { return PRINT(fmt, args, @$); } )?
        ","? { return PRINT0(fmt, @$); }
    | KW_CALL x=designator(true, _s) { return x; }
    | KW_EXIT ( n=id() { return EXIT2(n, @$); } )? { return EXIT(@$); }
    | KW_CYCLE ( n=id() { return CYCLE2(n, @$); } )? { return CYCLE(@$); }
    | KW_CONTINUE { return CONTINUE(@$); }
    | KW_RETURN
        ( &{ !is_sep(K[i]) } e=expr() { return RETURN1(e, @$); } )?
        { return RETURN(@$); }
    | KW_STOP x=stop_rest(_s, false) { return x; }
    | &{ K[i+1] == KW_STOP } KW_ERROR KW_STOP x=stop_rest(_s, true)
        { return x; }
    | KW_PAUSE
        ( &{ !is_sep(K[i]) } e=expr()
            { return make_Pause_t(m_a, @$, 0, EXPR(e), nullptr); } )?
        { return make_Pause_t(m_a, @$, 0, nullptr, nullptr); }
    | KW_ALLOCATE "(" args=fnarray_arg_list_opt() ")"
        { return ALLOCATE_STMT(args, @$); }
    | KW_DEALLOCATE "(" args=fnarray_arg_list_opt() ")"
        { return DEALLOCATE_STMT(args, @$); }
    | KW_NULLIFY a=write_args() { return NULLIFY(a, @$); }
    | KW_OPEN a=write_args() { return OPEN(a, @$); }
    | KW_CLOSE a=write_args() { return CLOSE(a, @$); }
    | KW_WRITE a=write_args()
        ( &{ is_sep(K[i]) } { return WRITE0(a, @$); } )?
        ","? args=expr_list() { return WRITE(a, args, @$); }
    | &{ K[i+1] == TK_LPAREN } KW_READ a=write_args()
        ( &{ is_sep(K[i]) } { return READ0(a, @$); } )?
        ","? args=expr_list() { return READ(a, args, @$); }
    | &{ K[i+1] == TK_INTEGER } KW_READ v=TK_INTEGER
        ( "," args=expr_list() { return READ2(V[v].int_suffix, args, @$); } )?
        { return READ4(V[v].int_suffix, @$); }
    | &{ K[i+1] == TK_STAR } KW_READ "*"
        ( "," { Vec<ast_t*> args; }
            ( &{ is_sep(K[i]) } { LIST_NEW(args); } | args<-expr_list() )
            { return READ3(args, @$); } )?
        { return READ6(@$); }
    | &{ K[i+1] == TK_STRING } KW_READ v=TK_STRING "," args=expr_list()
        { return READ5(V[v].str_prefix, args, @$); }
    | KW_INQUIRE a=write_args()
        ( &{ is_sep(K[i]) } { return INQUIRE0(a, @$); } )?
        args=expr_list() { return INQUIRE(a, args, @$); }
    | KW_REWIND
        ( a=write_args() { return REWIND(a, @$); } )?
        arg=unit_number(_s) { return REWIND2(arg, @$); }
    | KW_BACKSPACE
        ( a=write_args() { return BACKSPACE(a, @$); } )?
        arg=unit_number(_s) { return BACKSPACE2(arg, @$); }
    | KW_FLUSH
        ( a=write_args() { return FLUSH(a, @$); } )?
        arg=unit_number(_s) { return FLUSH2(arg, @$); }
    | ( KW_END_FILE | KW_ENDFILE )
        ( a=write_args() { return ENDFILE(a, @$); } )?
        arg=unit_number(_s) { return ENDFILE2(arg, @$); }
    | &{ K[i+1] == TK_STRING } KW_INCLUDE v=TK_STRING
        { return INCLUDE(V[v].str_prefix, @$); }
    | &{ K[i+1] == TK_INTEGER } KW_ASSIGN v=TK_INTEGER KW_TO n=id()
        { return ASSIGN(INTEGER3(V[v].int_suffix), n, @$); }
    | &{ K[i+1] == KW_TO } KW_GO KW_TO x=goto_rest(_s) { return x; }
    | KW_GOTO x=goto_rest(_s) { return x; }
    | KW_DATA { Vec<ast_t*> sets; LIST_NEW(sets); }
        d=data_set() { LIST_ADD(sets, d); }
        ( &{ !is_sep(K[i]) } ","? d<-data_set() { LIST_ADD(sets, d); } )*
        { return DATASTMT(sets, @$); }
    | v=TK_FORMAT { return FORMAT(V[v].string, @$); }
    | v=TK_OMP { return OMP_PRAGMA2(m_a, @$, V[v].string.c_str(m_a)); }
    | v=TK_OMP_END { return OMP_PRAGMA(V[v].string, @$); }
    | KW_ENTRY n=id() args=sub_args()
        { ast_t *b = nullptr, *r = nullptr; bool bind_first = (K[i] == KW_BIND); }
        ( &{ b == nullptr } b<-bind() | &{ r == nullptr } r<-result_clause() )*
        {
            l = span(_s);
            if (b == nullptr && r == nullptr) return ENTRY1(n, args, l);
            if (bind_first) return ENTRY2(n, args, b, r, l);
            return ENTRY3(n, args, r, b, l);
        }
    | &{ K[i+1] == KW_CASE } KW_SELECT KW_CASE x=select_case(_s) { return x; }
    | &{ K[i+1] == KW_TYPE } KW_SELECT KW_TYPE x=select_type(_s) { return x; }
    | &{ K[i+1] == KW_RANK } KW_SELECT KW_RANK x=select_rank(_s) { return x; }
    | KW_SELECT_CASE x=select_case(_s) { return x; }
    | KW_SELECT_TYPE x=select_type(_s) { return x; }
    | KW_SELECT_RANK x=select_rank(_s) { return x; }
    | &{ K[i+1] == TK_LPAREN } KW_ASSOCIATE "(" syms=var_sym_decl_list() ")"
        sp=sep() body=statements() ( KW_END_ASSOCIATE | KW_ENDASSOCIATE )
        { return ASSOCIATE_BLOCK(syms, TRIVIA_AFTER(sp, l), body, @$); }
    | &{ is_sep(K[i+1]) } KW_BLOCK sp=sep() decls=decl_statements()
        ( KW_END_BLOCK | KW_ENDBLOCK )
        { return BLOCK(TRIVIA_AFTER(sp, l), decls, @$); }
    | &{ K[i+1] == TK_LPAREN && K[i+2] == TK_RPAREN } KW_CRITICAL "(" ")"
        sp=sep() body=statements() end_critical()
        { return CRITICAL1(TRIVIA_AFTER(sp, l), body, @$); }
    | &{ K[i+1] == TK_LPAREN } KW_CRITICAL "(" stats=sync_stat_list() ")"
        sp=sep() body=statements() end_critical()
        { return CRITICAL2(stats, TRIVIA_AFTER(sp, l), body, @$); }
    | &{ is_sep(K[i+1]) } KW_CRITICAL sp=sep() body=statements()
        end_critical() { return CRITICAL(TRIVIA_AFTER(sp, l), body, @$); }
    | x=sync_statement() { return x; }
    | &{ K[i+1] == KW_TEAM && K[i+2] == TK_LPAREN } KW_FORM KW_TEAM
        x=form_team_rest(_s) { return x; }
    | &{ K[i+1] == TK_LPAREN } KW_FORM_TEAM x=form_team_rest(_s) { return x; }
    | &{ K[i+1] == KW_TEAM && K[i+2] == TK_LPAREN } KW_CHANGE KW_TEAM
        x=change_team_rest(_s) { return x; }
    | &{ K[i+1] == TK_LPAREN } KW_CHANGE_TEAM x=change_team_rest(_s)
        { return x; }
    | &{ K[i+1] == KW_POST && K[i+2] == TK_LPAREN } KW_EVENT KW_POST "("
        e=expr()
        ( ")" { return EVENT_POST(e, @$); } )?
        "," specs=sync_stat_list() ")" { return EVENT_POST1(e, specs, @$); }
    | &{ K[i+1] == KW_WAIT && K[i+2] == TK_LPAREN } KW_EVENT KW_WAIT "("
        e=expr()
        ( ")" { return EVENT_WAIT(e, @$); } )?
        "," { Vec<ast_t*> specs; LIST_NEW(specs); }
        ( n=id() "=" v=expr()
            { Location l2 = span(_s); l2.first = n->loc.first;
              LIST_ADD(specs, EVENT_WAIT_KW_ARG(n, v, l2)); }
            ( &{ K[i] == TK_COMMA } stats=sync_stat_list()
                { for (size_t j = 0; j < stats.size(); j++) LIST_ADD(specs, stats[j]); }
            )?
        )?
        ")" { return EVENT_WAIT1(e, specs, @$); }
    ;

rule end_critical() -> void
    : KW_END_CRITICAL
    | KW_ENDCRITICAL
    ;

// The unit of REWIND, BACKSPACE, FLUSH and ENDFILE without parentheses; the
// location of an integer or a function reference is the whole statement `s`
rule unit_number(size_t s) -> ast_t*
    : v=TK_INTEGER { l = span(s); return INTEGER(V[v].int_suffix, l); }
    | n=id()
        ( "(" args=fnarray_arg_list_opt() ")"
            { l = span(s); return FUNCCALLORARRAY(n, args, l); } )?
        { return n; }
    ;

%code {{
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

}}

rule sync_statement() -> ast_t* try
    : &{ K[i+1] == KW_ALL } KW_SYNC KW_ALL x=sync_all_rest(_s, true) { return x; }
    | &{ K[i+1] == KW_MEMORY } KW_SYNC KW_MEMORY x=sync_all_rest(_s, false)
        { return x; }
    | &{ K[i+1] == KW_IMAGES } KW_SYNC KW_IMAGES x=sync_images_rest(_s)
        { return x; }
    | &{ K[i+1] == KW_TEAM } KW_SYNC KW_TEAM x=sync_team_rest(_s) { return x; }
    | KW_SYNC_ALL x=sync_all_rest(_s, true) { return x; }
    | KW_SYNC_MEMORY x=sync_all_rest(_s, false) { return x; }
    | KW_SYNC_IMAGES x=sync_images_rest(_s) { return x; }
    | KW_SYNC_TEAM x=sync_team_rest(_s) { return x; }
    ;

// The rest of SYNC ALL (`all`) or SYNC MEMORY that starts at token `s`
rule sync_all_rest(size_t s, bool all) -> ast_t*
    : "("
        ( ")" { l = span(s); if (all) return SYNC_ALL1(l); return SYNC_MEMORY1(l); } )?
        stats=sync_stat_list() ")"
        { l = span(s); if (all) return SYNC_ALL2(stats, l); return SYNC_MEMORY2(stats, l); }
    | { l = span(s); if (all) return SYNC_ALL(l); return SYNC_MEMORY(l); }
    ;

rule sync_images_rest(size_t s) -> ast_t*
    : "("
        ( "*"
            ( ")" { l = span(s); return SYNC_IMAGE1(Asterisk, l); } )?
            stats=sync_stat_list() ")"
            { l = span(s); return SYNC_IMAGE3(Asterisk, stats, l); }
        )?
        e=expr()
        ( ")" { l = span(s); return SYNC_IMAGE2(e, l); } )?
        stats=sync_stat_list() ")" { l = span(s); return SYNC_IMAGE4(e, stats, l); }
    ;

rule sync_team_rest(size_t s) -> ast_t*
    : "(" e=expr()
        ( ")" { l = span(s); return SYNCTEAM1(e, l); } )?
        stats=sync_stat_list() ")" { l = span(s); return SYNCTEAM2(e, stats, l); }
    ;

// The rest of a GO TO statement that starts at token `s`
rule goto_rest(size_t s) -> ast_t*
    : v=TK_INTEGER { l = span(s); return GOTO(V[v].int_suffix, l); }
    | "(" labels=expr_list() ")" ","? e=expr()
        { l = span(s); return GOTO1(labels, e, l); }
    | n=id()
        ( &{ is_sep(K[i]) } { l = span(s); return GOTO2(n, l); } )?
        ","? "(" labels=expr_list() ")" { l = span(s); return GOTO3(n, labels, l); }
    ;

rule form_team_rest(size_t s) -> ast_t*
    : "(" e=expr() "," n=id()
        ( ")" { l = span(s); return FORMTEAM1(e, n, l); } )?
        stats=sync_stat_list() ")" { l = span(s); return FORMTEAM2(e, n, stats, l); }
    ;

rule change_team_rest(size_t s) -> ast_t*
    : "(" e=expr() { Vec<ast_t*> assoc; LIST_NEW(assoc); }
        ( &{ is_name(K[i+1]) && K[i+2] == TK_LBRACKET } ","
            { size_t s2 = i; } n=id() "[" co=coarray_arg_list() "]" "=>" e2=expr()
            { Location l2 = span(s2); LIST_ADD(assoc, COARRAY_ASSOC(n, co, e2, l2)); }
        )*
        { Vec<ast_t*> stats; stats.from_pointer_n(nullptr, 0);
          bool has_stats = (K[i] != TK_RPAREN); }
        ( &{ has_stats } stats<-sync_stat_list() )?
        ")" sp=sep() body=statements() ( KW_END_TEAM | KW_ENDTEAM )
        ( "(" stats2=sync_stat_list() ")"
            {
                l = span(s);
                if (has_stats) {
                    return CHANGETEAM4(e, assoc, stats, TRIVIA_AFTER(sp, l),
                        body, stats2, l);
                }
                return CHANGETEAM2(e, assoc, TRIVIA_AFTER(sp, l), body, stats2, l);
            }
        )?
        {
            l = span(s);
            if (has_stats) {
                return CHANGETEAM3(e, assoc, stats, TRIVIA_AFTER(sp, l), body, l);
            }
            return CHANGETEAM1(e, assoc, TRIVIA_AFTER(sp, l), body, l);
        }
    ;

// DATA statement

rule data_set() -> ast_t*
    : { Vec<ast_t*> objects; LIST_NEW(objects); }
        o=data_object() { LIST_ADD(objects, o); }
        ( "," o<-data_object() { LIST_ADD(objects, o); } )*
        "/" values=data_stmt_value_list() "/"
        { return DATA(objects, values, @$); }
    ;

%code {{
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

}}

rule signed_numeric_constant() -> ast_t*
    : v=TK_INTEGER { return INTEGER(V[v].int_suffix, @$); }
    | v=TK_REAL { return REAL(V[v].string, @$); }
    | "-" x=signed_numeric_constant() { return UNARY_MINUS(x, @$); }
    | "+" x=signed_numeric_constant() { return UNARY_PLUS(x, @$); }
    ;

rule data_stmt_constant() -> ast_t*
    : x=designator() { return x; }
    | v=TK_STRING { return STRING(V[v].str_prefix, @$); }
    | v=TK_BOZ_CONSTANT { return BOZ(V[v].string, @$); }
    | v=TK_TRUE { return TRUE(V[v].string, @$); }
    | v=TK_FALSE { return FALSE(V[v].string, @$); }
    | "(" x=signed_numeric_constant() "," y=signed_numeric_constant() ")"
        { return COMPLEX(x, y, @$); }
    | x=signed_numeric_constant() { return x; }
    ;

// data_stmt_value_list: constants, each optionally repeated: `n*constant`
rule data_stmt_value_list() -> Vec<ast_t*>
    : { Vec<ast_t*> v; LIST_NEW(v); }
        data_stmt_value(v) ( "," data_stmt_value(v) )* { return v; }
    ;

rule data_stmt_value(Vec<ast_t*> &v) -> void
    : x=data_stmt_constant()
        ( "*" y=data_stmt_constant() { REPEAT_LIST_ADD(v, x, y); return; } )?
        { LIST_ADD(v, x); }
    ;

// IF

rule endif() -> void
    : KW_END_IF
    | v=KW_ENDIF { Location l2 = Lc[v]; WARN_ENDIF(l2); }
    ;

// The value is the label of a labeled END DO, or 0
rule enddo() -> int64_t
    : v=TK_LABEL ( KW_END_DO | KW_ENDDO ) { return V[v].n; }
    | KW_END_DO { return 0; }
    | v=KW_ENDDO { Location l2 = Lc[v]; WARN_ENDDO(l2); return 0; }
    ;

// An IF construct, an IF statement or an arithmetic IF. `then` starts a
// construct only at the end of the line (or before a construct name):
// `if (c) then = 1` is an IF statement.
rule if_statement() -> ast_t*
    : KW_IF "(" cond=expr() ")"
        ( &{ is_sep(K[i+1]) || (is_name(K[i+1]) && is_sep(K[i+2])) }
            x=if_block(_s, cond) { return x; } )?
        ( a=TK_INTEGER "," b=TK_INTEGER "," c=TK_INTEGER
            { return IFARITHMETIC(cond, INTEGER3(V[a].int_suffix),
                INTEGER3(V[b].int_suffix), INTEGER3(V[c].int_suffix), @$); } )?
        body=statement1()
        { if (body == nullptr) unsup(__LINE__); return IFSINGLE(cond, body, @$); }
    ;

// The rest of an IF construct that starts at token `s`, from `then` on
rule if_block(size_t s, ast_t *cond) -> ast_t*
    : KW_THEN id_opt() sp=sep() body=statements()
        ( s2=KW_ELSEIF "(" cond2=expr() ")" block=if_block(s2, cond2)
            { l = span(s); return IF3(cond, TRIVIA_AFTER(sp, l), body, block, l); }
        | &{ K[i+1] == KW_IF } KW_ELSE s2=KW_IF "(" cond2=expr() ")"
            block=if_block(s2, cond2)
            { l = span(s); return IF3(cond, TRIVIA_AFTER(sp, l), body, block, l); }
        | KW_ELSE id_opt() sp2=sep() orelse=statements() endif()
            { l = span(s); return IF2(cond, TRIVIA(sp, sp2, l), body, orelse, l); }
        )?
        endif()
        { l = span(s); return IF1(cond, TRIVIA_AFTER(sp, l), body, l); }
    ;

// DO

%code {{
    // Is the token after `do` and an optional comma `t`, followed by "("?
    bool do_keyword(int t) const {
        size_t j = i + 1;
        if (K[j] == TK_COMMA) j++;
        return K[j] == t && K[j+1] == TK_LPAREN;
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

}}

rule concurrent_localities() -> Vec<ast_t*>
    : { Vec<ast_t*> v; LIST_NEW(v); }
        ( &{ K[i+1] == TK_LPAREN } x=concurrent_locality() { LIST_ADD(v, x); } )*
        { return v; }
    ;

rule concurrent_locality() -> ast_t*
    : KW_LOCAL "(" ids=id_list() ")" { return CONCURRENT_LOCAL(ids, @$); }
    | KW_LOCAL_INIT "(" ids=id_list() ")"
        { return CONCURRENT_LOCAL_INIT(ids, @$); }
    | KW_SHARED "(" ids=id_list() ")" { return CONCURRENT_SHARED(ids, @$); }
    | KW_DEFAULT "(" KW_NONE ")" { return CONCURRENT_DEFAULT(@$); }
    | KW_REDUCE "(" op=reduce_op() ":" ids=id_list() ")"
        { return CONCURRENT_REDUCE(op, ids, @$); }
    ;

rule reduce_op() -> reduce_opType
    : "+" { return REDUCE_OP_TYPE_ADD(@$); }
    | "*" { return REDUCE_OP_TYPE_MUL(@$); }
    | TK_AND { return reduce_opType::ReduceAND; }
    | TK_OR { return reduce_opType::ReduceOR; }
    | TK_EQV { return reduce_opType::ReduceEQV; }
    | TK_NEQV { return reduce_opType::ReduceNEQV; }
    | n=id() { return REDUCE_OP_TYPE_ID(n, @$); }
    ;

// do_statement and while_statement. The value is null if what follows `do`
// is not a loop header.
rule do_statement() -> ast_t* try
    : &{ K[i+1] == TK_LPAREN } KW_DOWHILE "(" cond=expr() ")"
        x=while_rest(_s, cond) { return x; }
    | &{ is_sep(K[i+1]) } KW_DO sp=sep() body=statements() end_label=enddo()
        { return DO1(TRIVIA_AFTER(sp, l), body, end_label, @$); }
    | &{ do_keyword(KW_WHILE) } KW_DO ","? KW_WHILE "(" cond=expr() ")"
        x=while_rest(_s, cond) { return x; }
    | &{ do_keyword(KW_CONCURRENT) } KW_DO ","? KW_CONCURRENT "("
        { ast_t *mask; } controls=concurrent_controls(mask)
        loc=concurrent_localities() sp=sep() body=statements() enddo()
        {
            l = span(_s);
            if (mask) {
                return DO_CONCURRENT2(controls, mask, loc, TRIVIA_AFTER(sp, l),
                    body, l);
            }
            return DO_CONCURRENT1(controls, loc, TRIVIA_AFTER(sp, l), body, l);
        }
    | &{ K[i+1] == TK_INTEGER } KW_DO v=TK_INTEGER ","?
        x=do_loop_rest(_s, true, INTEGER3(V[v].int_suffix)) { return x; }
    | KW_DO ","? x=do_loop_rest(_s, false, 0) { return x; }
    ;

rule while_rest(size_t s, ast_t *cond) -> ast_t*
    : sp=sep() body=statements() enddo()
        { l = span(s); return WHILE(cond, TRIVIA_AFTER(sp, l), body, l); }
    ;

// The rest of a counted loop that starts at token `s`: i = a, b [, c]
rule do_loop_rest(size_t s, bool has_label, int64_t label) -> ast_t* try
    : &{ K[i+1] == TK_EQUAL } var=id() "=" a=expr() "," b=expr()
        { ast_t *c = nullptr; } ( "," c<-expr() )?
        sp=sep() body=statements() end_label=enddo()
        {
            l = span(s);
            if (has_label) {
                ast_t *r;
                if (c) {
                    r = DO3_LABEL(label, var, a, b, c, TRIVIA_AFTER(sp, l),
                        body, end_label, l);
                } else {
                    r = DO2_LABEL(label, var, a, b, TRIVIA_AFTER(sp, l), body,
                        end_label, l);
                }
                return r;
            }
            if (c) return DO3(var, a, b, c, TRIVIA_AFTER(sp, l), body, end_label, l);
            return DO2(var, a, b, TRIVIA_AFTER(sp, l), body, end_label, l);
        }
    ;

// WHERE

rule endwhere() -> void
    : KW_END_WHERE
    | KW_ENDWHERE
    ;

rule where_statement() -> ast_t*
    : KW_WHERE "(" cond=expr() ")"
        ( &{ !is_sep(K[i]) }
            { if (!is_name(K[i]) || !is_assignment()) unsup(__LINE__); }
            body=assignment_statement() { return WHERESINGLE(cond, body, @$); } )?
        x=where_block(_s, cond) { return x; }
    ;

// The rest of a WHERE construct (or of an ELSEWHERE block) that starts at
// token `s`, after the mask
rule where_block(size_t s, ast_t *cond) -> ast_t*
    : sp=sep() body=statements()
        ( s2=KW_ELSEWHERE x=elsewhere_rest(s, s2, cond, sp, body) { return x; }
        | &{ K[i+1] == KW_WHERE } s2=KW_ELSE KW_WHERE
            x=elsewhere_rest(s, s2, cond, sp, body) { return x; }
        )?
        endwhere()
        { l = span(s); return WHERE1(cond, TRIVIA_AFTER(sp, l), body, l); }
    ;

rule elsewhere_rest(size_t s, size_t s2, ast_t *cond, Vec<ast_t*> &sp, Vec<ast_t*> &body) -> ast_t*
    : "(" cond2=expr() ")" block=where_block(s2, cond2)
        { l = span(s); return WHERE3(cond, TRIVIA_AFTER(sp, l), body, block, l); }
    | sp2=sep() orelse=statements() endwhere()
        { l = span(s); return WHERE2(cond, TRIVIA(sp, sp2, l), body, orelse, l); }
    ;

// FORALL

rule forall_statement() -> ast_t*
    : KW_FORALL "(" { ast_t *mask; } controls=concurrent_controls(mask)
        loc=concurrent_localities()
        ( &{ !is_sep(K[i]) }
            { if (!is_name(K[i]) || !is_assignment()) unsup(__LINE__); }
            body=assignment_statement()
            {
                l = span(_s);
                if (mask) return FORALLSINGLE2(controls, mask, body, l);
                return FORALLSINGLE1(controls, body, l);
            }
        )?
        sp=sep() body=statements() ( KW_END_FORALL | KW_ENDFORALL )
        {
            l = span(_s);
            if (mask) {
                return FORALL2(controls, mask, loc, TRIVIA_AFTER(sp, l), body, l);
            }
            return FORALL1(controls, loc, TRIVIA_AFTER(sp, l), body, l);
        }
    ;

// SELECT CASE, SELECT TYPE, SELECT RANK: the rest of the construct that
// starts at token `s`

rule end_select() -> void
    : KW_END_SELECT
    | KW_ENDSELECT
    ;

rule select_case(size_t s) -> ast_t*
    : "(" e=expr() ")" sp=sep() { Vec<ast_t*> cases; LIST_NEW(cases); }
        ( c=case_statement() { LIST_ADD(cases, c); } )*
        end_select()
        { l = span(s); return SELECT(e, TRIVIA_AFTER(sp, l), cases, l); }
    ;

rule case_statement() -> ast_t*
    : &{ K[i+1] == KW_DEFAULT } KW_CASE KW_DEFAULT id_opt() sp=sep()
        body=statements()
        { return CASE_STMT_DEFAULT(TRIVIA_AFTER(sp, l), body, @$); }
    | KW_CASE "(" { Vec<ast_t*> conds; LIST_NEW(conds); }
        c=case_condition() { LIST_ADD(conds, c); }
        ( "," c<-case_condition() { LIST_ADD(conds, c); } )*
        ")" id_opt() sp=sep() body=statements()
        { return CASE_STMT(conds, TRIVIA_AFTER(sp, l), body, @$); }
    ;

rule case_condition() -> ast_t*
    : ":" b=expr() { return CASE_RANGE2(b, @$); }
    | a=expr()
        ( ":"
            ( &{ at_arg_end() } { return CASE_RANGE1(a, @$); } )?
            b=expr() { return CASE_RANGE3(a, b, @$); }
        )?
        { return CASE_EXPR(a, @$); }
    ;

rule select_type(size_t s) -> ast_t*
    : "(" { ast_t *assoc = nullptr; }
        ( &{ is_name(K[i]) && K[i+1] == TK_ARROW } assoc<-id() "=>" )?
        e=expr() ")" sp=sep() { Vec<ast_t*> body; LIST_NEW(body); }
        ( &{ K[i+1] == KW_IS || (K[i] == KW_CLASS && K[i+1] == KW_DEFAULT) }
            g=type_guard() { LIST_ADD(body, g); } )*
        end_select()
        {
            l = span(s);
            if (assoc) return SELECT_TYPE2(assoc, e, TRIVIA_AFTER(sp, l), body, l);
            return SELECT_TYPE1(e, TRIVIA_AFTER(sp, l), body, l);
        }
    ;

// `type is (name)` takes a plain name only; a keyword there is an intrinsic
// type, parsed by var_type()
rule type_guard() -> ast_t*
    : &{ K[i+3] == TK_NAME && K[i+4] == TK_RPAREN } KW_TYPE KW_IS "("
        v=TK_NAME ")" sp=sep() stmts=statements()
        { return TYPE_STMTNAME(V[v].string, TRIVIA_AFTER(sp, l), stmts, @$); }
    | &{ K[i+3] == TK_NAME && K[i+4] == TK_LPAREN } KW_TYPE KW_IS "("
        v=TK_NAME kind=paren_kind_args() ")" sp=sep() stmts=statements()
        {
            l = span(_s);
            Location nl = Lc[v];
            return TYPE_STMTVAR(ATTR_TYPE_NAME_KIND(Type,
                SYMBOL(V[v].string, nl), kind, l), TRIVIA_AFTER(sp, l), stmts, l);
        }
    | KW_TYPE KW_IS "(" vt=var_type() ")" sp=sep() stmts=statements()
        { return TYPE_STMTVAR(vt, TRIVIA_AFTER(sp, l), stmts, @$); }
    | &{ K[i+1] == KW_IS } KW_CLASS KW_IS "(" n=id() ")" sp=sep()
        stmts=statements()
        { return CLASS_STMT(n, TRIVIA_AFTER(sp, l), stmts, @$); }
    | KW_CLASS KW_DEFAULT sp=sep() stmts=statements()
        { return CLASS_DEFAULT(TRIVIA_AFTER(sp, l), stmts, @$); }
    ;

rule select_rank(size_t s) -> ast_t*
    : "(" { ast_t *assoc = nullptr; }
        ( &{ is_name(K[i]) && K[i+1] == TK_ARROW } assoc<-id() "=>" )?
        e=expr() ")" sp=sep() { Vec<ast_t*> body; LIST_NEW(body); }
        ( r=rank_case() { LIST_ADD(body, r); } )*
        end_select()
        {
            l = span(s);
            if (assoc) return SELECT_RANK2(assoc, e, TRIVIA_AFTER(sp, l), body, l);
            return SELECT_RANK1(e, TRIVIA_AFTER(sp, l), body, l);
        }
    ;

rule rank_case() -> ast_t*
    : &{ K[i+1] == KW_DEFAULT } KW_RANK KW_DEFAULT id_opt() sp=sep()
        stmts=statements()
        { return RANK_DEFAULT(TRIVIA_AFTER(sp, l), stmts, @$); }
    | &{ K[i+2] == TK_STAR && K[i+3] == TK_RPAREN } KW_RANK "(" "*" ")"
        id_opt() sp=sep() stmts=statements()
        { return RANK_STAR(TRIVIA_AFTER(sp, l), stmts, @$); }
    | KW_RANK "(" r=expr() ")" id_opt() sp=sep() stmts=statements()
        { return RANK_EXPR(r, TRIVIA_AFTER(sp, l), stmts, @$); }
    ;

// ----------------------------------------------------------------------------
// Declarations

%code {{
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

}}

rule template_decl() -> ast_t*
    : KW_TEMPLATE name=id() "{" params=id_list_opt(TK_RBRACE) "}" sep()
        decls=decl_statements() contains=contains_block_opt()
        KW_END KW_TEMPLATE { ast_t *end_name = nullptr; }
        ( end_name<-id() )? sep()
        { return TEMPLATE(name, params, decls, contains, end_name, @$); }
    ;

rule requirement_decl() -> ast_t*
    : KW_REQUIREMENT name=id() "{" params=id_list_opt(TK_RBRACE) "}" sep()
        decls=decl_statements()
        KW_END KW_REQUIREMENT { ast_t *end_name = nullptr; }
        ( end_name<-id() )? sep()
        { return REQUIREMENT(name, params, decls, end_name, @$); }
    ;

rule require_decl() -> ast_t*
    : KW_REQUIRE "::"? { size_t s2 = i; } name=id() args=instantiate_symbols()
        { Location l2 = span(s2); ast_t *req = UNIT_REQUIRE(name, args, l2); }
        sep() { return REQUIRE(req, @$); }
    ;

rule union_type_decl() -> ast_t*
    : KW_UNION_TYPE mods=var_modifiers() name=id() sp=sep()
        decls=var_decl_star() KW_END_UNION_TYPE id_opt() sp2=sep()
        { return UNION_TYPE(mods, name, TRIVIA(sp, sp2, l), decls, @$); }
    ;

%code {{
    // An id_list, or an empty list before the token `end`
    Vec<ast_t*> id_list_opt(int end) {
        if (K[i] != end) return id_list();
        Vec<ast_t*> v;
        LIST_NEW(v);
        return v;
    }



}}

%code {{
    ast_t* var_decl_or_statement() {
        size_t s = i;
        ast_t *r = var_decl();
        if (r != nullptr) return r;
        i = s;
        return statement();
    }

}}

// One declaration or statement of the specification and execution parts,
// which the grammar (like the Bison one) does not keep apart. The value is
// null if none starts here. Only a variable declaration and a statement can
// have a label.
rule decl_statement() -> ast_t* try
    : &{ K[i] == TK_LABEL } x=var_decl_or_statement() { return x; }
    | &{ is_name(K[i]) && is_assignment() } x=statement() { return x; }
    | x=implicit_statement() { return x; }
    | x=use_statement() { return x; }
    | x=import_statement() { return x; }
    | &{ K[i] == KW_INTERFACE || K[i+1] == KW_INTERFACE } x=interface_decl()
        { return x; }
    | x=deferred_decl() { return x; }
    | &{ is_name(K[i+1]) && K[i+2] == TK_LBRACE } x=template_decl() { return x; }
    | x=requirement_decl() { return x; }
    | x=require_decl() { return x; }
    | x=union_type_decl() { return x; }
    | &{ K[i+1] != KW_IS && K[i+1] != TK_LPAREN } x=derived_type_decl()
        { return x; }
    | x=enum_decl() { return x; }
    | x=var_decl_or_statement() { return x; }
    ;

rule implicit_statement() -> ast_t*
    : KW_IMPLICIT
        ( KW_NONE
            ( "(" { Vec<ast_t*> specs; LIST_NEW(specs); }
                ( x=implicit_none_spec() { LIST_ADD(specs, x); }
                    ( "," x<-implicit_none_spec() { LIST_ADD(specs, x); } )* )?
                ")" sp=sep()
                { return IMPLICIT_NONE2(specs, TRIVIA_AFTER(sp, l), @$); } )?
            sp=sep() { return IMPLICIT_NONE(TRIVIA_AFTER(sp, l), @$); }
        )?
        { Vec<ast_t*> specs; LIST_NEW(specs); }
        x=implicit_spec() { LIST_ADD(specs, x); }
        ( "," x<-implicit_spec() { LIST_ADD(specs, x); } )*
        sp=sep() { return IMPLICIT(specs, TRIVIA_AFTER(sp, l), @$); }
    ;

rule implicit_none_spec() -> ast_t*
    : KW_EXTERNAL { return IMPLICIT_NONE_EXTERNAL(@$); }
    | KW_TYPE { return IMPLICIT_NONE_TYPE(@$); }
    ;

// "(" kind_arg_list ")"
rule paren_kind_args() -> Vec<kind_item_t>
    : "(" v=kind_arg_list() ")" { return v; }
    ;

// A type and the letters it applies to: `integer (a-h)`. With two
// parenthesized lists the first one is the kind: `integer(8) (a-h)`. The
// warning for the old `*n` form is at the `*`.
rule implicit_spec() -> ast_t*
    : &{ K[i+1] == TK_STAR && K[i+2] == TK_INTEGER } KW_INTEGER "*" v=TK_INTEGER
        letters=paren_kind_args()
        {
            ast_t *r = IMPLICIT_SPEC(ATTR_TYPE_INT(Integer, V[v].int_suffix, l), letters, @$);
            Location l2 = Lc[v-1];
            WARN_INTEGERSTAR(V[v].int_suffix, l2);
            return r;
        }
    | &{ K[i+1] == TK_STAR && K[i+2] == TK_INTEGER } KW_CHARACTER "*"
        v=TK_INTEGER letters=paren_kind_args()
        {
            ast_t *r = IMPLICIT_SPEC(ATTR_TYPE_INT(Character, V[v].int_suffix, l), letters, @$);
            Location l2 = Lc[v-1];
            WARN_CHARACTERSTAR(V[v].int_suffix, l2);
            return r;
        }
    | &{ K[i+1] == TK_STAR && K[i+2] == TK_INTEGER } KW_REAL "*" v=TK_INTEGER
        letters=paren_kind_args()
        {
            ast_t *r = IMPLICIT_SPEC(ATTR_TYPE_INT(Real, V[v].int_suffix, l), letters, @$);
            Location l2 = Lc[v-1];
            WARN_REALSTAR(V[v].int_suffix, l2);
            return r;
        }
    | &{ K[i+1] == TK_STAR && K[i+2] == TK_INTEGER } KW_LOGICAL "*" v=TK_INTEGER
        letters=paren_kind_args()
        {
            ast_t *r = IMPLICIT_SPEC(ATTR_TYPE_INT(Logical, V[v].int_suffix, l), letters, @$);
            Location l2 = Lc[v-1];
            WARN_LOGICALSTAR(V[v].int_suffix, l2);
            return r;
        }
    | &{ K[i+1] == TK_STAR && K[i+2] == TK_INTEGER } KW_COMPLEX "*" v=TK_INTEGER
        letters=paren_kind_args()
        {
            ast_t *r = IMPLICIT_SPEC(ATTR_TYPE_INT(Complex, DIV2(V[v].int_suffix), l), letters, @$);
            Location l2 = Lc[v-1];
            WARN_COMPLEXSTAR(V[v].int_suffix, l2);
            return r;
        }
    | &{ K[i+1] == TK_STAR } KW_CHARACTER s2=TK_STAR "(" e=expr() ")"
        letters=paren_kind_args()
        {
            ast_t *r = IMPLICIT_SPEC(ATTR_TYPE_EXPR(Character, e, l), letters, @$);
            Location l2 = Lc[s2];
            WARN_CHARACTERSTAR_EXPR(l2);
            return r;
        }
    | KW_INTEGER x=implicit_spec_kind(_s, decl_typeType::TypeInteger) { return x; }
    | KW_CHARACTER x=implicit_spec_kind(_s, decl_typeType::TypeCharacter)
        { return x; }
    | KW_REAL x=implicit_spec_kind(_s, decl_typeType::TypeReal) { return x; }
    | KW_LOGICAL x=implicit_spec_kind(_s, decl_typeType::TypeLogical) { return x; }
    | KW_COMPLEX x=implicit_spec_kind(_s, decl_typeType::TypeComplex) { return x; }
    | KW_DOUBLE
        ( KW_PRECISION letters=paren_kind_args()
            { return IMPLICIT_SPEC(ATTR_TYPE(DoublePrecision, l), letters, @$); }
        | KW_COMPLEX letters=paren_kind_args()
            { return IMPLICIT_SPEC(ATTR_TYPE(DoubleComplex, l), letters, @$); }
        )
    | KW_DOUBLE_PRECISION letters=paren_kind_args()
        { return IMPLICIT_SPEC(ATTR_TYPE(DoublePrecision, l), letters, @$); }
    | KW_DOUBLE_COMPLEX letters=paren_kind_args()
        { return IMPLICIT_SPEC(ATTR_TYPE(DoubleComplex, l), letters, @$); }
    | KW_TYPE "("
        ( &{ is_intrinsic_type_start() } it=intrinsic_type_spec() ")"
            letters=paren_kind_args()
            { return IMPLICIT_SPEC(ATTR_TYPE_ATTR(Type, it, l), letters, @$); } )?
        n=id() ")" letters=paren_kind_args()
        { return IMPLICIT_SPEC(ATTR_TYPE_NAME(Type, n, l), letters, @$); }
    | KW_CLASS "(" n=id() ")" letters=paren_kind_args()
        { return IMPLICIT_SPEC(ATTR_TYPE_NAME(Class, n, l), letters, @$); }
    ;

rule implicit_spec_kind(size_t s, decl_typeType type) -> ast_t*
    : first=paren_kind_args()
        ( letters=paren_kind_args()
            {
                l = span(s);
                return IMPLICIT_SPEC(make_AttrType_t(m_a, l, type, first.p,
                    first.size(), nullptr, nullptr, None), letters, l);
            }
        )?
        {
            l = span(s);
            return IMPLICIT_SPEC(make_AttrType_t(m_a, l, type, nullptr, 0,
                nullptr, nullptr, None), first, l);
        }
    ;

rule import_statement() -> ast_t*
    : KW_IMPORT
        ( &{ is_sep(K[i]) } sp=sep()
            { return IMPORT0(Default, TRIVIA_AFTER(sp, l), @$); } )?
        ( ","
            ( KW_ONLY ":" ids=id_list() sp=sep()
                { return IMPORT1(ids, Only, TRIVIA_AFTER(sp, l), @$); }
            | KW_NONE sp=sep() { return IMPORT0(None, TRIVIA_AFTER(sp, l), @$); }
            | KW_ALL sp=sep() { return IMPORT0(All, TRIVIA_AFTER(sp, l), @$); }
            )
        )?
        "::"? ids=id_list() sp=sep()
        { return IMPORT1(ids, Default, TRIVIA_AFTER(sp, l), @$); }
    ;

// operator_type: the value is false (and nothing is consumed) if the current
// token is not an operator
rule operator_type(intrinsicopType &op) -> bool try
    : "+" { op = intrinsicopType::PLUS; return true; }
    | "-" { op = intrinsicopType::MINUS; return true; }
    | "*" { op = intrinsicopType::STAR; return true; }
    | "/" { op = intrinsicopType::DIV; return true; }
    | TK_POW { op = intrinsicopType::POW; return true; }
    | TK_EQ { op = intrinsicopType::EQ; return true; }
    | TK_NE { op = intrinsicopType::NOTEQ; return true; }
    | TK_GT { op = intrinsicopType::GT; return true; }
    | TK_GE { op = intrinsicopType::GTE; return true; }
    | TK_LT { op = intrinsicopType::LT; return true; }
    | TK_LE { op = intrinsicopType::LTE; return true; }
    | "//" { op = intrinsicopType::CONCAT; return true; }
    | TK_NOT { op = intrinsicopType::NOT; return true; }
    | TK_AND { op = intrinsicopType::AND; return true; }
    | TK_OR { op = intrinsicopType::OR; return true; }
    | TK_XOR { op = intrinsicopType::XOR; return true; }
    | TK_EQV { op = intrinsicopType::EQV; return true; }
    | TK_NEQV { op = intrinsicopType::NEQV; return true; }
    ;

%code {{
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

    Vec<var_sym_t> var_sym_decl_list() {
        Vec<var_sym_t> syms;
        LIST_NEW(syms);
        do {
            var_sym_t *v = var_sym_decl();
            PLIST_ADD(syms, v);
        } while (accept(TK_COMMA));
        return syms;
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

    Vec<kind_item_t> kind_arg_list() {
        Vec<kind_item_t> kind;
        LIST_NEW(kind);
        do {
            kind_item_t *k = kind_arg();
            LIST_ADD(kind, *k);
        } while (accept(TK_COMMA));
        return kind;
    }

}}

rule use_statement() -> ast_t*
    : KW_USE { Vec<ast_t*> mods; LIST_NEW(mods); }
        ( &{ K[i] == TK_COMMA }
            ( "," m=use_modifier() { LIST_ADD(mods, m); } )* "::"
        | "::"?
        )
        mod=id() { ast_t *u; }
        ( ","
            ( &{ K[i+1] == TK_COLON } KW_ONLY ":"
                ( &{ is_sep(K[i]) } { l = span(_s); u = USE3(mods, mod, nullptr, l); }
                | syms=use_symbol_list()
                    { l = span(_s); u = USE2(mods, mod, syms, nullptr, l); }
                )
            | syms=use_symbol_list()
                { l = span(_s); u = USE4(mods, mod, syms, nullptr, l); }
            )
        | { l = span(_s); u = USE1(mods, mod, nullptr, l); }
        )
        sp=sep() { l = span(_s); TRIVIA2_(u, TRIVIA_AFTER(sp, l)); return u; }
    ;

rule use_modifier() -> ast_t*
    : KW_INTRINSIC { return SIMPLE_ATTR(Intrinsic, @$); }
    | KW_NON_INTRINSIC { return SIMPLE_ATTR(Non_Intrinsic, @$); }
    ;

// var_decl: a declaration, optionally with a label. The value is null if
// none starts here.
rule var_decl() -> ast_t* try
    : TK_LABEL x=var_decl_body(_s) { return x; }
    | x=var_decl_body(_s) { return x; }
    ;

// The location of a declaration `s` does not include the separator after it
rule var_decl_body(size_t s) -> ast_t* try
    : &{ K[i+1] == TK_LPAREN } KW_PARAMETER "(" { Vec<var_sym_t> v; LIST_NEW(v); }
        c=named_constant_def() { PLIST_ADD(v, c); }
        ( "," c<-named_constant_def() { PLIST_ADD(v, c); } )* ")"
        { l = span(s); } sp=sep()
        { return VAR_DECL_PARAMETER(v, TRIVIA_AFTER(sp, l), l); }
    | KW_NAMELIST groups=namelist_group_list() { l = span(s); } sp=sep()
        { return VAR_DECL_NAMELIST(groups, TRIVIA_AFTER(sp, l), l); }
    | KW_COMMON blocks=common_block_list() { l = span(s); } sp=sep()
        { return VAR_DECL_COMMON(blocks, TRIVIA_AFTER(sp, l), l); }
    | KW_EQUIVALENCE { Vec<equi_t> sets; LIST_NEW(sets); }
        q=equivalence_set() { PLIST_ADD(sets, q); }
        ( "," q<-equivalence_set() { PLIST_ADD(sets, q); } )*
        { l = span(s); } sp=sep()
        { return VAR_DECL_EQUIVALENCE(sets, TRIVIA_AFTER(sp, l), l); }
    | v=TK_PRAGMA_DECL { l = span(s); } sp=sep()
        { return VAR_DECL_PRAGMA(V[v].string, TRIVIA_AFTER(sp, l), l); }
    | &{ is_var_type_start() } vt=var_type()
        { Vec<ast_t*> mods; mods.from_pointer_n(nullptr, 0);
          bool has_mods = false, dbl_colon = false; }
        ( &{ K[i] == TK_COMMA } { LIST_NEW(mods); has_mods = true; }
            ( "," m=var_modifier()
                { if (m == nullptr) unsup(__LINE__); LIST_ADD(mods, m); } )*
            "::"
        | dbl_colon<-"::"?
        )
        syms=var_sym_decl_list() { l = span(s); } sp=sep()
        {
            if (has_mods) return VAR_DECL1a(vt, mods, syms, TRIVIA_AFTER(sp, l), l);
            if (dbl_colon) return VAR_DECL1b(vt, syms, TRIVIA_AFTER(sp, l), l);
            return VAR_DECL1c(vt, syms, TRIVIA_AFTER(sp, l), l);
        }
    | &{ s == i } m=var_modifier() { if (m == nullptr) return nullptr; }
        ( &{ is_sep(K[i]) } { l = span(s); } sp=sep()
            { return VAR_DECL2(m, TRIVIA_AFTER(sp, l), l); } )?
        "::"? syms=var_sym_decl_list() { l = span(s); } sp=sep()
        { return VAR_DECL3(m, syms, TRIVIA_AFTER(sp, l), l); }
    ;

rule named_constant_def() -> var_sym_t*
    : n=id() "=" e=expr() { return VAR_SYM_DIM_INIT(n, nullptr, 0, e, Equal, @$); }
    ;

rule equivalence_set() -> equi_t*
    : "(" e=expr_list() ")" { return EQUIVALENCE_SET(e, @$); }
    ;

// intrinsic_type_spec. `real*8` is the old form of `real(8)`; a `*` that is
// not followed by a number is not part of the type.
rule intrinsic_type_spec() -> ast_t*
    : &{ K[i+1] == TK_STAR && K[i+2] == TK_INTEGER } KW_INTEGER "*" v=TK_INTEGER
        { ast_t *r = ATTR_TYPE_INT(Integer, V[v].int_suffix, @$);
          WARN_INTEGERSTAR(V[v].int_suffix, l); return r; }
    | &{ K[i+1] == TK_STAR && K[i+2] == TK_INTEGER } KW_REAL "*" v=TK_INTEGER
        { ast_t *r = ATTR_TYPE_INT(Real, V[v].int_suffix, @$);
          WARN_REALSTAR(V[v].int_suffix, l); return r; }
    | &{ K[i+1] == TK_STAR && K[i+2] == TK_INTEGER } KW_COMPLEX "*" v=TK_INTEGER
        { ast_t *r = ATTR_TYPE_INT(Complex, DIV2(V[v].int_suffix), @$);
          WARN_COMPLEXSTAR(V[v].int_suffix, l); return r; }
    | &{ K[i+1] == TK_STAR && K[i+2] == TK_INTEGER } KW_LOGICAL "*" v=TK_INTEGER
        { ast_t *r = ATTR_TYPE_INT(Logical, V[v].int_suffix, @$);
          WARN_LOGICALSTAR(V[v].int_suffix, l); return r; }
    | &{ K[i+1] == TK_STAR && K[i+2] == TK_INTEGER } KW_CHARACTER "*"
        v=TK_INTEGER
        { ast_t *r = ATTR_TYPE_INT(Character, V[v].int_suffix, @$);
          WARN_CHARACTERSTAR(V[v].int_suffix, l); return r; }
    | &{ K[i+1] == TK_STAR && K[i+2] == TK_LPAREN } KW_CHARACTER "*" "("
        ( &{ K[i+1] == TK_RPAREN } "*" ")"
            { return ATTR_TYPE_STAR(Character, DoubleAsterisk, @$); } )?
        e=expr() ")"
        { ast_t *r = ATTR_TYPE_EXPR(Character, e, @$);
          WARN_CHARACTERSTAR_EXPR(l); return r; }
    | KW_INTEGER x=type_with_kind(_s, decl_typeType::TypeInteger) { return x; }
    | KW_REAL x=type_with_kind(_s, decl_typeType::TypeReal) { return x; }
    | KW_COMPLEX x=type_with_kind(_s, decl_typeType::TypeComplex) { return x; }
    | KW_LOGICAL x=type_with_kind(_s, decl_typeType::TypeLogical) { return x; }
    | KW_CHARACTER x=type_with_kind(_s, decl_typeType::TypeCharacter) { return x; }
    | KW_BYTE
        { l = span(_s); ast_t *r = ATTR_TYPE_EXPR(Integer, INT1(l), l);
          WARN_BYTE(l); return r; }
    | KW_DOUBLE
        ( KW_PRECISION { return ATTR_TYPE(DoublePrecision, @$); }
        | KW_COMPLEX { return ATTR_TYPE(DoubleComplex, @$); }
        )
    | KW_DOUBLE_PRECISION { return ATTR_TYPE(DoublePrecision, @$); }
    | KW_DOUBLE_COMPLEX { return ATTR_TYPE(DoubleComplex, @$); }
    | KW_LIST "(" it=intrinsic_type_spec() ")" { return ATTR_TYPE_ATTR(List, it, @$); }
    | KW_SET "(" it=intrinsic_type_spec() ")" { return ATTR_TYPE_ATTR(Set, it, @$); }
    | KW_DICT "(" v=intrinsic_type_spec_list() ")"
        { return ATTR_TYPE_LIST(Dict, v, @$); }
    | KW_TUPLE "(" v=intrinsic_type_spec_list() ")"
        { return ATTR_TYPE_LIST(Tuple, v, @$); }
    ;

rule intrinsic_type_spec_list() -> Vec<ast_t*>
    : { Vec<ast_t*> v; LIST_NEW(v); }
        x=intrinsic_type_spec() { LIST_ADD(v, x); }
        ( "," x<-intrinsic_type_spec() { LIST_ADD(v, x); } )*
        { return v; }
    ;

// The type that starts at token `s`, with its kind if there is one
rule type_with_kind(size_t s, decl_typeType type) -> ast_t*
    : kind=paren_kind_args()
        {
            l = span(s);
            return make_AttrType_t(m_a, l, type, kind.p, kind.size(), nullptr,
                nullptr, None);
        }
    | { l = span(s); return make_AttrType_t(m_a, l, type, nullptr, 0, nullptr,
            nullptr, None); }
    ;

rule var_type() -> ast_t*
    : &{ is_intrinsic_type_start() } x=intrinsic_type_spec() { return x; }
    | KW_PROCEDURE "("
        ( ")" { return ATTR_TYPE(Procedure, @$); } )?
        ( &{ is_intrinsic_type_start() && K[i+1] != TK_RPAREN }
            x=procedure_intrinsic_type(_s) { return x; } )?
        n=id() ")" { return ATTR_TYPE_NAME(Procedure, n, @$); }
    | KW_TYPE "(" x=type_or_class(_s, true) { return x; }
    | KW_CLASS "(" x=type_or_class(_s, false) { return x; }
    ;

// The rest of `procedure(` with an intrinsic type, such as
// `procedure(real(8))`. The location of the inner type is the one of the
// whole var_type, as in the Bison grammar.
rule procedure_intrinsic_type(size_t s) -> ast_t*
    : { decl_typeType type; Vec<kind_item_t> k; k.from_pointer_n(nullptr, 0); }
        ( KW_INTEGER { type = decl_typeType::TypeInteger; } k<-paren_kind_args()
        | KW_REAL { type = decl_typeType::TypeReal; } k<-paren_kind_args()
        | KW_COMPLEX { type = decl_typeType::TypeComplex; } k<-paren_kind_args()
        | KW_LOGICAL { type = decl_typeType::TypeLogical; } k<-paren_kind_args()
        | KW_CHARACTER { type = decl_typeType::TypeCharacter; } k<-paren_kind_args()
        | KW_DOUBLE KW_PRECISION { type = decl_typeType::TypeDoublePrecision; }
        )
        ")"
        {
            l = span(s);
            ast_t *inner = make_AttrType_t(m_a, l, type, k.p, k.size(), nullptr,
                nullptr, None);
            return ATTR_TYPE_ATTR(Procedure, inner, l);
        }
    ;

// The rest of `type(` (`is_type`) or `class(`. `type(integer)` is the
// intrinsic type, not a type named `integer`.
rule type_or_class(size_t s, bool is_type) -> ast_t*
    : "*" ")"
        {
            l = span(s);
            if (is_type) return ATTR_TYPE_STAR(Type, Asterisk, l);
            return ATTR_TYPE_STAR(Class, Asterisk, l);
        }
    | &{ is_type && is_intrinsic_type_start() } it=intrinsic_type_spec() ")"
        { l = span(s); return ATTR_TYPE_ATTR(Type, it, l); }
    | n=id()
        ( kind=paren_kind_args() ")"
            {
                l = span(s);
                if (is_type) return ATTR_TYPE_NAME_KIND(Type, n, kind, l);
                return ATTR_TYPE_NAME_KIND(Class, n, kind, l);
            }
        )?
        ")"
        {
            l = span(s);
            if (is_type) return ATTR_TYPE_NAME(Type, n, l);
            return ATTR_TYPE_NAME(Class, n, l);
        }
    ;

rule kind_arg() -> kind_item_t*
    : &{ K[i+1] == TK_EQUAL } n=id() "="
        ( "*" { return KIND_ARG2S(n, @$); }
        | ":" { return KIND_ARG2C(n, @$); }
        | e=expr() { return KIND_ARG2(n, e, @$); }
        )
    | "*" { return KIND_ARG1S(@$); }
    | ":" { return KIND_ARG1C(@$); }
    | e=expr() { return KIND_ARG1(e, @$); }
    ;

rule bind() -> ast_t*
    : KW_BIND a=write_args() { return BIND2(a, @$); }
    ;

rule result_clause() -> ast_t*
    : KW_RESULT "(" r=id() ")" { return r; }
    ;

// var_modifier: an attribute of a declaration. The value is null (and
// nothing is consumed) if there is none here.
rule var_modifier() -> ast_t* try
    : &{ K[i+1] == TK_LPAREN } b=bind() { return BIND(b, @$); }
    | KW_PARAMETER { return SIMPLE_ATTR(Parameter, @$); }
    | KW_ALLOCATABLE { return SIMPLE_ATTR(Allocatable, @$); }
    | KW_ASYNCHRONOUS { return SIMPLE_ATTR(Asynchronous, @$); }
    | KW_POINTER { return SIMPLE_ATTR(Pointer, @$); }
    | KW_TARGET { return SIMPLE_ATTR(Target, @$); }
    | KW_OPTIONAL { return SIMPLE_ATTR(Optional, @$); }
    | KW_PROTECTED { return SIMPLE_ATTR(Protected, @$); }
    | KW_SAVE { return SIMPLE_ATTR(Save, @$); }
    | KW_SEQUENCE { return SIMPLE_ATTR(Sequence, @$); }
    | KW_CONTIGUOUS { return SIMPLE_ATTR(Contiguous, @$); }
    | KW_NOPASS { return SIMPLE_ATTR(NoPass, @$); }
    | KW_PRIVATE { return SIMPLE_ATTR(Private, @$); }
    | KW_PUBLIC { return SIMPLE_ATTR(Public, @$); }
    | KW_ABSTRACT { return SIMPLE_ATTR(Abstract, @$); }
    | KW_ENUMERATOR { return SIMPLE_ATTR(Enumerator, @$); }
    | KW_EXTERNAL { return SIMPLE_ATTR(External, @$); }
    | KW_INTRINSIC { return SIMPLE_ATTR(Intrinsic, @$); }
    | KW_VALUE { return SIMPLE_ATTR(Value, @$); }
    | KW_VOLATILE { return SIMPLE_ATTR(Volatile, @$); }
    | KW_KIND { return SIMPLE_ATTR(Kind, @$); }
    | KW_LEN { return SIMPLE_ATTR(Len, @$); }
    | KW_PASS ( "(" n=id() ")" { return PASS(n, @$); } )?
        { return PASS(nullptr, @$); }
    | KW_EXTENDS "(" n=id() ")" { return EXTENDS(n, @$); }
    | KW_DIMENSION
        ( "(" d=array_comp_decl_list() ")" { return DIMENSION(d, @$); } )?
        { return DIMENSION0(@$); }
    | KW_CODIMENSION "[" d=coarray_comp_decl_list() "]"
        { return CODIMENSION(d, @$); }
    | KW_INTENT "("
        ( KW_IN ")" { return INTENT(In, @$); }
        | KW_OUT ")" { return INTENT(Out, @$); }
        | ( KW_INOUT | KW_IN_OUT ) ")" { return INTENT(InOut, @$); }
        )
    ;

rule array_comp_decl_list() -> Vec<dimension_t>
    : { Vec<dimension_t> v; LIST_NEW(v); }
        d=array_comp_decl() { PLIST_ADD(v, d); }
        ( "," d<-array_comp_decl() { PLIST_ADD(v, d); } )*
        { return v; }
    ;

rule array_comp_decl() -> dimension_t*
    : ":"
        ( &{ at_arg_end() } { return ARRAY_COMP_DECL5d(@$); } )?
        b=expr() { return ARRAY_COMP_DECL4d(b, @$); }
    | "*" { return ARRAY_COMP_DECL6d(@$); }
    | ".." { return ARRAY_COMP_DECL8d(@$); }
    | a=expr()
        ( ":"
            ( &{ at_arg_end() } { return ARRAY_COMP_DECL3d(a, @$); } )?
            ( "*" { return ARRAY_COMP_DECL7d(a, @$); } )?
            b=expr() { return ARRAY_COMP_DECL2d(a, b, @$); }
        )?
        { return ARRAY_COMP_DECL1d(a, @$); }
    ;

rule coarray_comp_decl_list() -> Vec<codimension_t>
    : { Vec<codimension_t> v; LIST_NEW(v); }
        d=coarray_comp_decl() { PLIST_ADD(v, d); }
        ( "," d<-coarray_comp_decl() { PLIST_ADD(v, d); } )*
        { return v; }
    ;

rule coarray_comp_decl() -> codimension_t*
    : ":"
        ( &{ at_arg_end() } { return COARRAY_COMP_DECL5d(@$); } )?
        b=expr() { return COARRAY_COMP_DECL4d(b, @$); }
    | "*" { return COARRAY_COMP_DECL6d(@$); }
    | a=expr()
        ( ":"
            ( &{ at_arg_end() } { return COARRAY_COMP_DECL3d(a, @$); } )?
            ( "*" { return COARRAY_COMP_DECL7d(a, @$); } )?
            b=expr() { return COARRAY_COMP_DECL2d(a, b, @$); }
        )?
        { return COARRAY_COMP_DECL1d(a, @$); }
    ;

%code {{
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

}}

%code {{
    // decl_spec: `operator(+)`, `operator(.op.)` or `assignment(=)` in a
    // declaration such as `public :: operator(+)`; null if there is none
    var_sym_t* decl_spec() {
        size_t s = i;
        intrinsicopType op;
        size_t defop;
        ast_t *name;
        Generic g = generic_spec(op, defop, name, false);
        Location l = span(s);
        if (g == Generic::Op) return VAR_SYM_SPEC(DECL_OP(op, l), None, l);
        if (g == Generic::Defop) {
            return VAR_SYM_SPEC(DECL_DEFOP(V[defop].string, l), None, l);
        }
        if (g == Generic::Assignment) {
            return VAR_SYM_SPEC(DECL_ASSIGNMENT(l), None, l);
        }
        return nullptr;
    }

}}

// One entity of a declaration: a name with its array bounds, codimensions,
// character length and initialization
rule var_sym_decl() -> var_sym_t*
    : "/" n=id() "/" { return VAR_SYM_NAME(n, Slash, @$); }
    | { var_sym_t *spec = decl_spec(); if (spec) return spec; }
        n=id() { dimension_t *dims = nullptr; size_t n_dims = 0; bool has_dims = false; }
        ( "(" d=array_comp_decl_list() ")"
            { dims = d.p; n_dims = d.n; has_dims = true; } )?
        ( "[" c=coarray_comp_decl_list() "]"
            ( "=" e=expr()
                {
                    l = span(_s);
                    if (has_dims) {
                        return VAR_SYM_DIM_CODIM_INIT(n, dims, n_dims, c.p, c.n,
                            e, Equal, l);
                    }
                    return VAR_SYM_CODIM_INIT(n, c.p, c.n, e, Equal, l);
                }
            )?
            {
                l = span(_s);
                if (has_dims) {
                    return VAR_SYM_DIM_CODIM(n, dims, n_dims, c.p, c.n, None, l);
                }
                return VAR_SYM_CODIM(n, c.p, c.n, None, l);
            }
        )?
        ( &{ star_paren_star() } "*" "(" "*" ")"
            ( "=" e=expr()
                { return VAR_SYM_DIM_INIT(n, dims, n_dims, e, DoubleAsterisk, @$); } )?
            {
                l = span(_s);
                if (has_dims) return VAR_SYM_DIM(n, dims, n_dims, DoubleAsterisk, l);
                return VAR_SYM_NAME(n, DoubleAsterisk, l);
            }
        )?
        ( "*" len=expr()
            ( "=" e=expr()
                { return VAR_SYM_DIM_LEN_INIT(n, dims, n_dims, len, e, Equal, @$); } )?
            { return VAR_SYM_DIM_LEN(n, dims, n_dims, len, Asterisk, @$); }
        )?
        ( "=" e=expr() { return VAR_SYM_DIM_INIT(n, dims, n_dims, e, Equal, @$); } )?
        ( "=>" e=expr() { return VAR_SYM_DIM_INIT(n, dims, n_dims, e, Arrow, @$); } )?
        ( "/" { Vec<ast_t*> values; LIST_NEW(values); }
            x=data_stmt_constant() { LIST_ADD(values, x); }
            ( "," x<-data_stmt_constant() { LIST_ADD(values, x); } )* "/"
            {
                l = span(_s);
                return VAR_SYM_DIM_INIT(n, dims, n_dims,
                    SLASH_INIT_EXPR(values, l), SlashInit, l);
            }
        )?
        {
            l = span(_s);
            if (has_dims) return VAR_SYM_DIM(n, dims, n_dims, None, l);
            return VAR_SYM_NAME(n, None, l);
        }
    ;

rule derived_type_decl() -> ast_t*
    : KW_TYPE mods=var_modifiers() name=id()
        { Vec<ast_t*> params; bool has_params = false; }
        ( "(" params<-id_list() ")" { has_params = true; } )?
        sp=sep() decls=var_decl_star()
        { Vec<ast_t*> contains; LIST_NEW(contains); }
        ( KW_CONTAINS sep()
            ( pd=procedure_decl() { LIST_ADD(contains, pd); } )* )?
        ( KW_END_TYPE | KW_ENDTYPE ) id_opt() sp2=sep()
        {
            l = span(_s);
            if (has_params) {
                return DERIVED_TYPE1(mods, name, params, TRIVIA(sp, sp2, l),
                    decls, contains, l);
            }
            return DERIVED_TYPE(mods, name, TRIVIA(sp, sp2, l), decls, contains, l);
        }
    ;

// proc_modifiers: [ [ "," proc_modifier ]... "::" ]
rule proc_modifiers() -> Vec<ast_t*>
    : { Vec<ast_t*> v; LIST_NEW(v); }
        ( &{ K[i] != TK_COMMA } "::"? { return v; } )?
        ( "," m=proc_modifier() { LIST_ADD(v, m); } )*
        "::" { return v; }
    ;

rule proc_modifier() -> ast_t*
    : KW_PRIVATE { return SIMPLE_ATTR(Private, @$); }
    | KW_PUBLIC { return SIMPLE_ATTR(Public, @$); }
    | KW_NOPASS { return SIMPLE_ATTR(NoPass, @$); }
    | KW_DEFERRED { return SIMPLE_ATTR(Deferred, @$); }
    | KW_NON_OVERRIDABLE { return SIMPLE_ATTR(NonDeferred, @$); }
    | KW_PASS ( "(" n=id() ")" { return PASS(n, @$); } )?
        { return PASS(nullptr, @$); }
    ;

// A type-bound procedure declaration
rule procedure_decl() -> ast_t* try
    : &{ K[i+1] == TK_LPAREN } KW_PROCEDURE "(" iface=id() ")"
        mods=proc_modifiers() syms=use_symbol_list() sp=sep()
        { return DERIVED_TYPE_PROC1(iface, mods, syms, TRIVIA_AFTER(sp, l), @$); }
    | KW_PROCEDURE mods=proc_modifiers() syms=use_symbol_list() sp=sep()
        { return DERIVED_TYPE_PROC(mods, syms, TRIVIA_AFTER(sp, l), @$); }
    | KW_GENERIC { Vec<ast_t*> access; LIST_NEW(access); }
        ( s2=TK_COMMA
            ( KW_PRIVATE { Location l2 = span(s2); LIST_ADD(access, SIMPLE_ATTR(Private, l2)); }
            | KW_PUBLIC { Location l2 = span(s2); LIST_ADD(access, SIMPLE_ATTR(Public, l2)); }
            )
        )?
        "::"
        {
            intrinsicopType op;
            size_t defop;
            ast_t *name;
            Generic g = generic_spec(op, defop, name);
            ast_t *gname = nullptr;
        }
        ( &{ g == Generic::None } gname<-id() )?
        "=>" ids=id_list() sp=sep()
        {
            l = span(_s);
            switch (g) {
                case Generic::Op: return GENERIC_OPERATOR(access, op, ids, TRIVIA_AFTER(sp, l), l);
                case Generic::Defop: return GENERIC_DEFOP(access, V[defop].string, ids, TRIVIA_AFTER(sp, l), l);
                case Generic::Assignment: return GENERIC_ASSIGNMENT(access, ids, TRIVIA_AFTER(sp, l), l);
                case Generic::Write: return GENERIC_WRITE(access, name, ids, TRIVIA_AFTER(sp, l), l);
                case Generic::Read: return GENERIC_READ(access, name, ids, TRIVIA_AFTER(sp, l), l);
                default: return GENERIC_NAME(access, gname, ids, TRIVIA_AFTER(sp, l), l);
            }
        }
    | KW_FINAL "::" n=id() sp=sep()
        { return FINAL_NAME(n, TRIVIA_AFTER(sp, l), @$); }
    | KW_PRIVATE sp=sep() { return PRIVATE(Private, TRIVIA_AFTER(sp, l), @$); }
    ;

rule enum_decl() -> ast_t*
    : KW_ENUM { Vec<ast_t*> mods; LIST_NEW(mods); }
        ( "," m=var_modifier()
            { if (m == nullptr) unsup(__LINE__); LIST_ADD(mods, m); } )*
        sp=sep() decls=var_decl_star() ( KW_END_ENUM | KW_ENDENUM ) sp2=sep()
        { return ENUM(mods, TRIVIA(sp, sp2, l), decls, @$); }
    ;

rule interface_decl() -> ast_t*
    : header=interface_header() sp=sep() { Vec<ast_t*> items; LIST_NEW(items); }
        ( &{ K[i] != KW_END_INTERFACE && K[i] != KW_ENDINTERFACE }
            it=interface_item() { LIST_ADD(items, it); } )*
        ( KW_END_INTERFACE | KW_ENDINTERFACE )
        {
            // What follows `end interface` is not checked against the header
            intrinsicopType op;
            size_t defop;
            ast_t *name;
            if (generic_spec(op, defop, name) == Generic::None) id_opt();
        }
        sp2=sep() { return INTERFACE(header, TRIVIA(sp, sp2, l), items, @$); }
    ;

rule interface_header() -> ast_t*
    : KW_ABSTRACT KW_INTERFACE { return ABSTRACT_INTERFACE_HEADER(@$); }
    | KW_DEFERRED KW_INTERFACE { return DEFERRED_INTERFACE_HEADER(@$); }
    | KW_INTERFACE
        {
            intrinsicopType op;
            size_t defop;
            ast_t *name;
            Generic g = generic_spec(op, defop, name);
        }
        ( &{ g == Generic::None } n=id() { return INTERFACE_HEADER_NAME(n, @$); } )?
        {
            l = span(_s);
            switch (g) {
                case Generic::Op: return INTERFACE_HEADER_OPERATOR(op, l);
                case Generic::Defop: return INTERFACE_HEADER_DEFOP(V[defop].string, l);
                case Generic::Assignment: return INTERFACE_HEADER_ASSIGNMENT(l);
                case Generic::Write: return INTERFACE_HEADER_WRITE(name, l);
                case Generic::Read: return INTERFACE_HEADER_READ(name, l);
                default: return INTERFACE_HEADER(l);
            }
        }
    ;

// `[module] procedure a, b` or the interface of a subroutine or a function
rule interface_item() -> ast_t*
    : mods=fn_mods()
        ( KW_PROCEDURE "::"? ids=id_list() sp=sep()
            {
                l = span(_s);
                if (mods.size() > 0) {
                    return INTERFACE_MODULE_PROC1(mods, ids, TRIVIA_AFTER(sp, l), l);
                }
                return INTERFACE_MODULE_PROC(ids, TRIVIA_AFTER(sp, l), l);
            }
        )?
        u=sub_or_func_rest(_s, mods, false)
        { if (u == nullptr) unsup(__LINE__); return INTERFACE_PROC(u, u->loc); }
    ;

// ----------------------------------------------------------------------------
// Program units

%code {{
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

}}

%code {{
    // Does a prefix of a subroutine or a function start here?
    bool fn_mod_start() const {
        switch (K[i]) {
            case KW_ELEMENTAL: case KW_IMPURE: case KW_MODULE: case KW_PURE:
            case KW_RECURSIVE: case KW_NON_RECURSIVE: return true;
            default: return is_var_type_start();
        }
    }

}}

// fn_mod_plus, possibly empty
rule fn_mods() -> Vec<ast_t*>
    : { Vec<ast_t*> v; LIST_NEW(v); }
        ( &{ fn_mod_start() } m=fn_mod() { LIST_ADD(v, m); } )*
        { return v; }
    ;

rule fn_mod() -> ast_t*
    : KW_ELEMENTAL { return SIMPLE_ATTR(Elemental, @$); }
    | KW_IMPURE { return SIMPLE_ATTR(Impure, @$); }
    | KW_MODULE { return SIMPLE_ATTR(Module, @$); }
    | KW_PURE { return SIMPLE_ATTR(Pure, @$); }
    | KW_RECURSIVE { return SIMPLE_ATTR(Recursive, @$); }
    | KW_NON_RECURSIVE { return SIMPLE_ATTR(NonRecursive, @$); }
    | vt=var_type() { return vt; }
    ;

// sub_args: the dummy arguments of a subroutine; `*` is an alternate return
rule sub_args() -> Vec<ast_t*>
    : { Vec<ast_t*> args; LIST_NEW(args); }
        ( "("
            ( &{ K[i] != TK_RPAREN } a=id_or_star() { LIST_ADD(args, a); }
                ( "," a<-id_or_star() { LIST_ADD(args, a); } )* )?
            ")" )?
        { return args; }
    ;

rule id_or_star() -> ast_t*
    : "*" { return nullptr; }
    | n=id() { return n; }
    ;

// The subroutine, function or (after a prefix) module procedure that starts
// at token `s` with the prefixes `mods`; null if none starts here
rule sub_or_func_rest(size_t s, Vec<ast_t*> &mods, bool allow_procedure) -> ast_t* try
    : KW_SUBROUTINE name=id() args=sub_args() { ast_t *b = nullptr; }
        ( b<-bind() )? sp=sep() decls=decl_statements()
        ce=contains_end(KW_END_SUBROUTINE, KW_ENDSUBROUTINE) { l = span(s); }
        sp2=sep()
        {
            if (mods.size() > 0) {
                return SUBROUTINE1(mods, name, args, b, TRIVIA(sp, sp2, l),
                    decls, ce, l);
            }
            return SUBROUTINE(name, args, b, TRIVIA(sp, sp2, l), decls, ce, l);
        }
    | KW_FUNCTION name=id() "(" args=id_list_opt(TK_RPAREN) ")"
        { ast_t *res = nullptr, *b = nullptr; }
        ( &{ res == nullptr } res<-result_clause() | &{ b == nullptr } b<-bind() )*
        sp=sep() decls=decl_statements()
        ce=contains_end(KW_END_FUNCTION, KW_ENDFUNCTION) { l = span(s); }
        sp2=sep()
        {
            if (mods.size() > 0) {
                return FUNCTION(mods, name, args, res, b, TRIVIA(sp, sp2, l),
                    decls, ce, l);
            }
            return FUNCTION0(name, args, res, b, TRIVIA(sp, sp2, l), decls, ce, l);
        }
    | &{ mods.size() > 0 && allow_procedure } KW_PROCEDURE name=id()
        args=sub_args() sp=sep() decls=decl_statements()
        ce=contains_end(KW_END_PROCEDURE, KW_ENDPROCEDURE) sp2=sep()
        // The location of this unit includes the separator after it
        { l = span(s); return PROCEDURE(mods, name, args, TRIVIA(sp, sp2, l),
            decls, ce, l); }
    ;

// A templated subroutine or function, after `template` and its prefixes
rule templated_unit(size_t s, Vec<ast_t*> &mods) -> ast_t*
    : KW_SUBROUTINE name=id() "{" temp_args=id_list() "}" args=sub_args()
        { ast_t *b = nullptr; } ( b<-bind() )? sp=sep() decls=decl_statements()
        end=end_stmt(KW_END_SUBROUTINE, KW_ENDSUBROUTINE) { l = span(s); }
        sp2=sep()
        { return TEMPLATED_SUBROUTINE1(mods, name, temp_args, args, b,
            TRIVIA(sp, sp2, l), decls, end, l); }
    | KW_FUNCTION name=id() "{" temp_args=id_list() "}" "("
        args=id_list_opt(TK_RPAREN) ")" { ast_t *res = nullptr, *b = nullptr; }
        ( res<-result_clause() )? ( b<-bind() )? sp=sep()
        decls=decl_statements()
        end=end_stmt(KW_END_FUNCTION, KW_ENDFUNCTION) { l = span(s); } sp2=sep()
        { return TEMPLATED_FUNCTION(mods, name, temp_args, args, res, b,
            TRIVIA(sp, sp2, l), decls, end, l); }
    ;

// The location of a unit ends at its END statement; the separator after it
// only gives the trivia
rule program() -> ast_t*
    : KW_PROGRAM name=id() sp=sep() decls=decl_statements()
        ce=contains_end(KW_END_PROGRAM, KW_ENDPROGRAM) { l = span(_s); }
        sp2=sep() { return PROGRAM(name, TRIVIA(sp, sp2, l), decls, ce, l); }
    ;

%code {{
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

    Vec<ast_t*> contains_block_opt() {
        if (K[i] == KW_CONTAINS) return contains_block();
        Vec<ast_t*> v;
        LIST_NEW(v);
        return v;
    }

}}

// end_module and end_submodule: the value is the name after it, if any
rule end_unit(int end1, int end2) -> ast_t*
    : KW_END { return nullptr; }
    | &{ K[i] == end1 || K[i] == end2 } { i++; }
        ( n=id() { return n; } )? { return nullptr; }
    ;

rule module() -> ast_t*
    : KW_MODULE name=id() sp=sep() decls=decl_statements()
        contains=contains_block_opt()
        end_name=end_unit(KW_END_MODULE, KW_ENDMODULE) { l = span(_s); }
        sp2=sep()
        { return MODULE(name, TRIVIA(sp, sp2, l), decls, contains, end_name, l); }
    ;

rule submodule() -> ast_t*
    : KW_SUBMODULE "(" id1=id() { ast_t *id2 = nullptr; } ( ":" id2<-id() )? ")"
        name=id() sp=sep() decls=decl_statements()
        contains=contains_block_opt()
        end_name=end_unit(KW_END_SUBMODULE, KW_ENDSUBMODULE) { l = span(_s); }
        sp2=sep()
        {
            if (id2) {
                return SUBMODULE1(id1, id2, name, TRIVIA(sp, sp2, l), decls,
                    contains, end_name, l);
            }
            return SUBMODULE(id1, name, TRIVIA(sp, sp2, l), decls, contains,
                end_name, l);
        }
    ;

rule block_data() -> ast_t*
    : KW_BLOCK KW_DATA { ast_t *name = nullptr; } ( name<-id() )? sp=sep()
        decls=decl_statements() end_unit(KW_END_BLOCK_DATA, KW_ENDBLOCKDATA)
        sp2=sep()
        {
            l = span(_s);
            if (name) return BLOCKDATA1(name, TRIVIA(sp, sp2, l), decls, l);
            return BLOCKDATA(TRIVIA(sp, sp2, l), decls, l);
        }
    ;

// The whole file
rule units() -> void
    : ( &{ is_sep(K[i]) } sep() )?
        ( &{ K[i] != END_OF_FILE } u=unit() { RESULT(u); } )*
    ;

rule unit() -> ast_t*
    : x=program() { return x; }
    | x=submodule() { return x; }
    | &{ is_name(K[i+1]) && is_sep(K[i+2]) } x=module() { return x; }
    | &{ K[i+1] == KW_DATA } x=block_data() { return x; }
    | x=sub_or_func() { if (x == nullptr) x = top_level_item(); return x; }
    ;

%code {{
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
}}
