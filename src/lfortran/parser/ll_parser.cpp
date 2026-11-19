/*
The LL parser of Fortran. Free form is tokenized by the re2c tokenizer
(tokenizer.re), fixed form by the fixed-form tokenizer
(fixedform_tokenizer.cpp); the parser is the same for both.

The grammar is in ll_parser.ll. llgen.py generates from it the member
functions that parse its rules (ll_parser_rules.h), which are included in the
class `LL` below. This file has the rest of the parser:

* the tokens: the whole file is tokenized first, into arrays. A keyword is
  just a kind of name:
  `is_name()` is a range check, a keyword test is an integer comparison;
* the predicates that the grammar uses (`%predicate`);
* the few rules that are written by hand (`%extern`);
* the functions that the actions of the grammar call, where a macro of
  semantics.h alone is not enough.

The AST is built by the macros and functions of semantics.h.

Syntax errors: a rule that cannot go on throws `LLUnsupported` at the token
that it did not expect, and the error is reported there. With
--continue-compilation the parser then skips to the end of the line and goes
on with the declarations and statements of the program unit it is in.
*/

#include <lfortran/parser/parser.h>
#include <lfortran/parser/tokenizer.h>
#include <lfortran/parser/fixedform_tokenizer.h>
#include <lfortran/parser/tokens.h>
#include <lfortran/parser/ll_parser.h>
#include <lfortran/ast.h>
#include <lfortran/ast_kind.h>
#include <libasr/string_utils.h>
#include <lfortran/parser/parser_exception.h>
#include <cstring>

namespace {

// semantics.h is written to be included once. Including it here in an
// unnamed namespace gives this file its own copy of everything.
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

// Thrown when the parser cannot go on after an error that it has reported
struct LLStop {};

// The header of FORALL and DO CONCURRENT: the controls and the mask (or null)
struct ConcurrentHeader {
    Vec<ast_t*> controls;
    ast_t *mask;
};

// An item of the list of a NAMELIST or COMMON statement: an object and the
// name of the group or block that starts with it (or null)
struct ListItem {
    var_sym_t *group;
    var_sym_t *object;
    explicit operator bool() const { return object != nullptr; }
};

// `i = a, b [, c]` of an implied do loop; `step` is null if there is none
struct DoControl {
    ast_t *var, *start, *end, *step;
};

// The stop code and the QUIET specifier of STOP and ERROR STOP (or null)
struct StopCode {
    ast_t *code;
    ast_t *quiet;
};

// A name with its arguments, a part of a designator
struct DesignatorPart {
    ast_t *name;
    Vec<FnArg> args;
    bool has_args;
};

// RESULT and BIND of a function (or null)
struct FunctionClauses {
    ast_t *result;
    ast_t *bind;
};

struct LL {
    Allocator &m_a;
    LCompilers::diag::Diagnostics &diag;
    // Fixed form: the semantics.h macros give no style suggestions then
    bool fixed_form = false;
    Vec<ast_t*> result;
    // The semantics.h macros refer to the parser as `p`
    LL &p;

    // Tokens: kind, semantic value and location; `i` is the current token
    const int *K;
    const YYSTYPE *V;
    const Location *Lc;
    size_t i = 0;

    // The source, to show a token in an error message; the locations of the
    // tokens are offsets into it plus `loc_offset`
    const std::string &src;
    uint32_t loc_offset;
    // --continue-compilation: go on after a syntax error
    bool continue_compilation;
    // After an error, further errors are not reported before this token
    size_t quiet_until = 0;
    // The errors of the tokenizer, which has run before the parser; they
    // are reported in the order of the source, with those of the parser
    std::vector<LCompilers::diag::Diagnostic> *tokenizer_errors = nullptr;
    size_t n_tokenizer_errors = 0;
    // The token at which a statement at the top level of the file starts
    size_t top_statement = SIZE_MAX;
    // Interactive mode: declarations, statements and expressions outside of
    // a program unit are kept as they are, not taken as a main program
    bool script = false;
    // A main program without the PROGRAM statement was parsed
    bool implicit_main = false;
    // The token after the last statement, before its separators
    size_t stmt_end = 0;
    // The token after the statement that ended a labeled DO loop (with the
    // separators after it), and the label; a loop around it with the same
    // label ends there too
    size_t do_end = SIZE_MAX;
    int64_t do_end_label = 0;

    LL(Allocator &al, LCompilers::diag::Diagnostics &diag, const int *K,
        const YYSTYPE *V, const Location *Lc, const std::string &src,
        uint32_t loc_offset, bool continue_compilation)
        : m_a{al}, diag{diag}, p{*this}, K{K}, V{V}, Lc{Lc}, src{src},
        loc_offset{loc_offset}, continue_compilation{continue_compilation} {
        result.reserve(al, 32);
    }

    // Reports the errors of the tokenizer up to the current token
    void flush_tokenizer_errors(bool all = false) {
        if (tokenizer_errors == nullptr) return;
        while (n_tokenizer_errors < tokenizer_errors->size()) {
            LCompilers::diag::Diagnostic &d
                = (*tokenizer_errors)[n_tokenizer_errors];
            if (!all && !d.labels.empty() && !d.labels[0].spans.empty()
                    && d.labels[0].spans[0].loc.first > Lc[i].last) {
                break;
            }
            diag.diagnostics.push_back(d);
            n_tokenizer_errors++;
        }
    }

    // Reports the current token as a syntax error
    void syntax_error() {
        flush_tokenizer_errors();
        if (i < quiet_until) return;
        int token = K[i];
        std::string message;
        if (token == END_OF_FILE) {
            message = "End of file is unexpected here";
        } else if (token == TK_NEWLINE) {
            message = "Newline is unexpected here";
        } else {
            std::string type = LCompilers::LFortran::token2text(token);
            size_t first = Lc[i].first - loc_offset;
            size_t last = Lc[i].last - loc_offset;
            std::string text;
            if (first <= last && last < src.size()) {
                text = src.substr(first, last - first + 1);
            }
            if (text == type || text.size() == 0) {
                message = "Token '" + type + "' is unexpected here";
            } else {
                message = "Token '" + text + "' (of type '" + type
                    + "') is unexpected here";
            }
        }
        diag.add(LCompilers::diag::Diagnostic(message,
            LCompilers::diag::Level::Error, LCompilers::diag::Stage::Parser,
            {LCompilers::diag::Label("", {Lc[i]})}));
    }

    // After a syntax error: skips to the end of the statement and over one
    // separator. Errors in the next two tokens are not reported.
    void recover() {
        while (!is_sep(K[i])) {
            if (K[i] == END_OF_FILE) throw LLStop();
            i++;
        }
        i++;
        quiet_until = i + 2;
    }

    // Can the current token follow the declarations and statements of a
    // program unit or a block?
    bool is_decl_follow() const {
        size_t j = (K[i] == TK_LABEL) ? i + 1 : i;
        switch (K[j]) {
            case KW_END: case KW_CONTAINS: case END_OF_FILE:
            case KW_END_PROGRAM: case KW_ENDPROGRAM:
            case KW_END_MODULE: case KW_ENDMODULE:
            case KW_END_SUBMODULE: case KW_ENDSUBMODULE:
            case KW_END_BLOCK: case KW_ENDBLOCK:
            case KW_END_BLOCK_DATA: case KW_ENDBLOCKDATA:
            case KW_END_SUBROUTINE: case KW_ENDSUBROUTINE:
            case KW_END_FUNCTION: case KW_ENDFUNCTION:
            case KW_END_PROCEDURE: case KW_ENDPROCEDURE:
                return true;
            default: return false;
        }
    }

    // Gives up; `line` is the line of the grammar (or of this file) that did
    [[noreturn]] void unsup(int line) {
        throw LLUnsupported{line, K[i], Lc[i].first};
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

    // statement: [ label ] statement1 sep
    // Returns nullptr (and consumes nothing) if no statement starts here.
    ast_t* statement() {
        size_t s = i;
        bool has_label = (K[i] == TK_LABEL);
        if (has_label) i++;
        ast_t *st = statement1();
        if (st == nullptr) {
            // An identifier (not a keyword) can only start an assignment
            // here; at the top level of a file it can be an expression
            if (K[i] == TK_NAME && s != top_statement) statement_error();
            i = s;
            return nullptr;
        }
        if (has_label) LABEL(st, V[s].n);
        // A labeled DO loop ends with its last statement, which has the
        // separators
        if (do_end == i) return st;
        stmt_end = i;
        Vec<ast_t*> sp = sep();
        Location l = span(s);
        TRIVIA_(st, TRIVIA_AFTER(sp, l));
        return st;
    }

    // A statement; it is an error if none starts here
    ast_t* required_statement() {
        ast_t *st = statement();
        if (st == nullptr) {
            if (K[i] == TK_LABEL) i++;
            unsup(__LINE__);
        }
        return st;
    }

    // Is the token `j` the END DO statement (and not, say, `enddo = 5`)?
    bool is_enddo(size_t j) const {
        if (K[j] != KW_END_DO && K[j] != KW_ENDDO) return false;
        if (is_name(K[j+1])) j++;
        return is_sep(K[j+1]);
    }

    // The loop ends with the statement that has its label, together with
    // the loops around it that have the same label:
    //
    //     do 10 i = 1, n
    //     do 10 j = 1, m
    //         ...
    //  10 continue
    //
    // which the tokens cannot say, so it is found here and not in the
    // grammar.
    Vec<ast_t*> labeled_do_body(size_t lab) {
        int64_t label = INTEGER3(V[lab].int_suffix);
        Vec<ast_t*> v;
        LIST_NEW(v);
        for (;;) {
            bool labeled = (K[i] == TK_LABEL);
            if (labeled && V[i].n == label) {
                if (is_enddo(i + 1)) {
                    // `10 end do` is `10 continue`, which is not kept
                    i += 2;
                    if (is_name(K[i])) i++;
                    stmt_end = i;
                    sep();
                } else {
                    LIST_ADD(v, required_statement());
                }
                break;
            }
            if (is_enddo(labeled ? i + 1 : i)) {
                // An END DO that is not the statement with the label
                enddo();
                return v;
            }
            LIST_ADD(v, required_statement());
            // The statement was a loop that ended with this label
            if (do_end == i && do_end_label == label) break;
        }
        do_end = i;
        do_end_label = label;
        return v;
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

    Vec<ast_t*> decl_statements() {
        Vec<ast_t*> v;
        LIST_NEW(v);
        decl_statements_loop(v);
        return v;
    }

    void decl_statements_loop(Vec<ast_t*> &v) {
        for (;;) {
            flush_tokenizer_errors();
            try {
                if (K[i] == KW_INSTANTIATE && !is_assignment()) {
                    v = LIST_EXTEND(m_a, v, instantiate());
                    continue;
                }
                if (K[i] == KW_DEFERRED && K[i+1] == KW_TYPE
                        && K[i+2] != TK_LPAREN) {
                    v = LIST_EXTEND(m_a, v, deferred_type_decl());
                    continue;
                }
                if (K[i] == TK_PRAGMA_DECL) {
                    // A pragma that is not valid is reported, and is a null
                    // declaration
                    LIST_ADD(v, pragma_decl());
                    continue;
                }
                ast_t *s = decl_statement();
                if (s == nullptr) {
                    if (!continue_compilation || is_decl_follow()) return;
                    statement_error();
                }
                LIST_ADD(v, s);
            } catch (const LLUnsupported &) {
                // With --continue-compilation the declarations and
                // statements go on after the line of a syntax error
                if (!continue_compilation) throw;
                syntax_error();
                recover();
            }
        }
    }

    // Error recovery (`resume:` in the grammar): the rest of a program unit
    // after its declarations and statements `v` did not parse. With
    // --continue-compilation, reports the error and goes on with more
    // declarations and statements after the line of the error.
    bool more_decl_statements(Vec<ast_t*> &v) {
        if (!continue_compilation) return false;
        syntax_error();
        recover();
        decl_statements_loop(v);
        return true;
    }

    // No statement starts at the current token. A name can start an
    // assignment, whose left-hand side is an expression in the grammar, so
    // the token to report is the one after that expression.
    [[noreturn]] void statement_error() {
        if (K[i] == TK_LABEL) i++;
        if (is_name(K[i])) expr();
        unsup(__LINE__);
    }

    // An argument of an instantiation: a type if it can be one, else an
    // expression
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
        // a keyword alone on its line is a name
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
        // A statement of one line that is also an expression is the
        // expression: `stop (1)`, `print (x)`
        switch (K[i]) {
            case KW_STOP: case KW_RETURN: case KW_PAUSE: case KW_PRINT: {
                if (K[i+1] != TK_LPAREN) break;
                try {
                    ast_t *e = expr();
                    if (is_sep(K[i])) {
                        sep();
                        return e;
                    }
                } catch (const LLUnsupported &) {
                }
                i = s;
                break;
            }
            default: break;
        }
        // A declaration or a statement, or else an expression. If neither
        // parses, the error is the one that is further in the source.
        size_t failed_at = s;
        top_statement = s;
        try {
            ast_t *r = decl_statement();
            if (r != nullptr) return r;
        } catch (const LLUnsupported &) {
            failed_at = i;
        }
        i = s;
        top_statement = SIZE_MAX;
        try {
            ast_t *e = expr();
            sep();
            return e;
        } catch (const LLUnsupported &) {
            if (i < failed_at) i = failed_at;
            throw;
        }
    }

    ast_t* subprogram_or_main() {
        ast_t *u = sub_or_func();
        if (u != nullptr) return u;
        if (script) return top_level_item();
        // A file has one main program
        if (implicit_main) unsup(__LINE__);
        implicit_main = true;
        return main_program();
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

    Vec<ast_t*> two_items(ast_t *x, ast_t *y) {
        Vec<ast_t*> v;
        LIST_NEW(v);
        LIST_ADD(v, x);
        LIST_ADD(v, y);
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
            ast_t *c, ast_t *trivia, Vec<ast_t*> &body, Location l) {
        // The loop ends with its last statement, not with the separators
        // after it
        if (do_end == i) l.last = Lc[stmt_end-1].last;
        ast_t *r;
        if (c) {
            r = DO3_LABEL(label, var, a, b, c, trivia, body, 0, l);
        } else {
            r = DO2_LABEL(label, var, a, b, trivia, body, 0, l);
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

    // The statements `body` of a block, with a CONTINUE for the label of the
    // END statement `e` that ends the block
    Vec<ast_t*> with_end_label(Vec<ast_t*> &body, const EndStmt &e) {
        append_labeled_end(m_a, body, e.label, e.loc);
        return body;
    }

    // The members `a(i)%b%` of a designator: all its parts but the last one
    Vec<struct_member_t> members(Vec<DesignatorPart> &parts) {
        Vec<struct_member_t> v;
        if (parts.size() == 1) {
            v.from_pointer_n(nullptr, 0);
            return v;
        }
        LIST_NEW(v);
        for (size_t j = 0; j + 1 < parts.size(); j++) {
            struct_member_t *m;
            if (parts.p[j].has_args) {
                STRUCT_MEMBER2(m, parts.p[j].name, parts.p[j].args);
            } else {
                STRUCT_MEMBER1(m, parts.p[j].name);
            }
            PLIST_ADD(v, m);
        }
        return v;
    }

    ast_t* designator_ref(Vec<DesignatorPart> &parts, Location &l) {
        DesignatorPart &last = parts.p[parts.size()-1];
        Vec<struct_member_t> mem = members(parts);
        if (last.has_args) {
            return FUNCCALLORARRAY0(m_a, last.name, mem, last.args, empty1(),
                empty_vecast(), l);
        }
        if (parts.size() == 1) return last.name;
        ast_t *r;
        NAME1(r, last.name, mem, l);
        return r;
    }

    // `x(1:2)(3:4)`: a substring of an array element
    ast_t* substring_ref(Vec<DesignatorPart> &parts, Vec<FnArg> &args,
            Location &l) {
        DesignatorPart &last = parts.p[parts.size()-1];
        if (!last.has_args) unsup(__LINE__);
        return FUNCCALLORARRAY0(m_a, last.name, members(parts), last.args,
            args, empty_vecast(), l);
    }

    ast_t* coarray_ref(Vec<DesignatorPart> &parts, Vec<CoarrayArg> &co,
            Location &l) {
        DesignatorPart &last = parts.p[parts.size()-1];
        return COARRAY(m_a, last.name, members(parts), last.args, co, l);
    }

    ast_t* call_ref(Vec<DesignatorPart> &parts, Location &l) {
        DesignatorPart &last = parts.p[parts.size()-1];
        Vec<struct_member_t> mem = members(parts);
        if (last.has_args) {
            return SUBROUTINE_CALL0(m_a, mem.p, mem.n, last.name, last.args,
                empty_vecast(), l);
        }
        return make_SubroutineCall_t(m_a, l, 0, name2char(last.name), mem.p,
            mem.n, nullptr, 0, nullptr, 0, nullptr, 0, nullptr);
    }

    // An implied do loop with the values `x`, `y` (or null) and `ys`
    ast_t* implied_do(ast_t *x, ast_t *y, Vec<ast_t*> ys, DoControl &d,
            Location &l) {
        Vec<ast_t*> v;
        v.reserve(m_a, 2 + ys.size());
        v.push_back(m_a, x);
        if (y) v.push_back(m_a, y);
        for (size_t j = 0; j < ys.size(); j++) v.push_back(m_a, ys[j]);
        return implied_do_loop(m_a, l, v, d.var, d.start, d.end, d.step);
    }

    // An implied do loop of DATA objects: `o`, then `os`
    ast_t* data_do_loop(ast_t *o, Vec<ast_t*> &os, ast_t *type, ast_t *var,
            ast_t *a, ast_t *b, ast_t *c, Location &l) {
        Vec<ast_t*> objects;
        LIST_NEW(objects);
        LIST_ADD(objects, o);
        for (size_t j = 0; j < os.size(); j++) LIST_ADD(objects, os[j]);
        if (c) return DATA_IMPLIED_DO2(objects, type, var, a, b, c, l);
        return DATA_IMPLIED_DO1(objects, type, var, a, b, l);
    }

    // The controls and the mask (only after the last control)
    ConcurrentHeader concurrent_header_of(Vec<ast_t*> &items) {
        ConcurrentHeader h;
        h.mask = nullptr;
        LIST_NEW(h.controls);
        for (size_t j = 0; j < items.size(); j++) {
            ast_t *x = items[j];
            if (x->type == astType::concurrent_control) {
                if (h.mask) unsup(__LINE__);
                LIST_ADD(h.controls, x);
            } else {
                if (h.mask) unsup(__LINE__);
                h.mask = x;
            }
        }
        return h;
    }

    Vec<ast_t*> event_wait_specs(ast_t *k, Vec<ast_t*> &stats) {
        Vec<ast_t*> specs;
        LIST_NEW(specs);
        if (k) LIST_ADD(specs, k);
        for (size_t j = 0; j < stats.size(); j++) LIST_ADD(specs, stats[j]);
        return specs;
    }

    // The objects after the name of a group are in the group
    Vec<namelist_group_t> namelist_groups(var_sym_t *g, var_sym_t *o,
            Vec<ListItem> &items) {
        Vec<namelist_group_t> out;
        {
            NAMELIST_GROUP_1(out, g, o);
        }
        for (size_t j = 0; j < items.size(); j++) {
            Vec<namelist_group_t> groups = out;
            if (items.p[j].group) {
                NAMELIST_GROUP_3(out, groups, items.p[j].group,
                    items.p[j].object);
            } else {
                NAMELIST_GROUP_2(out, groups, items.p[j].object);
            }
        }
        return out;
    }

    // The objects after the name of a block are in the block. The location
    // of a block is from the start of the list to its first object.
    Vec<common_block_t> common_blocks(var_sym_t *g, var_sym_t *o,
            Vec<ListItem> &items, Location &all) {
        Vec<common_block_t> out;
        Location l = all;
        l.last = o->loc.last;
        if (g) {
            COMMON_BLOCK_2(out, g, o, l);
        } else {
            COMMON_BLOCK_1(out, o, l);
        }
        for (size_t j = 0; j < items.size(); j++) {
            Vec<common_block_t> prev = out;
            var_sym_t *start = items.p[j].group;
            var_sym_t *object = items.p[j].object;
            l.last = object->loc.last;
            if (start) {
                COMMON_BLOCK_5(out, prev, start, object, l);
            } else {
                COMMON_BLOCK_3(out, prev, object, l);
            }
        }
        return out;
    }

    // The name of a main program without the PROGRAM statement
    static constexpr const char *implicit_program_name
        = "implicit_program_lfortran";

    ast_t* implicit_program(Vec<ast_t*> &decls, ast_t *trivia,
            ContainsEnd *ce, Location &l) {
        return PROGRAM2(m_a, l, LCompilers::s2c(m_a, implicit_program_name),
            trivia_cast(trivia), decls, CONTAINS(CE_CONTAINS(ce)),
            CE_CONTAINS(ce).size(), nullptr, END_NAME_LOC(CE_END(ce)),
            END_LABEL(CE_END(ce)), END_LOC(CE_END(ce)), diag);
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

void ll_tokenize(Allocator &al, const std::string &input,
        diag::Diagnostics &diagnostics, LLTokens &t, bool openmp,
        uint32_t loc_offset, bool continue_compilation)
{
    Tokenizer tok;
    tok.openmp_enabled = openmp;
    tok.loc_offset = loc_offset;
    // The tokenizer needs the input to end with a newline
    if (input.empty() || input.back() != '\n') {
        t.input = input;
        t.input.append("\n");
        t.source = &t.input;
    } else {
        t.source = &input;
    }
    tok.set_string(*t.source);
    // The arrays end with several END_OF_FILE tokens, so that the parser can
    // look ahead without checks. They grow as needed: a token takes several
    // characters on average, and the arrays are not worth clearing for the
    // one token per character that is possible.
    const size_t margin = 8;
    size_t capacity = t.kind.size();
    if (capacity < t.source->size() / 4 + margin) {
        capacity = t.source->size() / 4 + margin;
        t.kind.resize(capacity);
        t.value.resize(capacity);
        t.loc.resize(capacity);
    }
    int *K = t.kind.data();
    YYSTYPE *V = t.value.data();
    Location *L = t.loc.data();
    size_t n = 0;
    for (;;) {
        if (n + margin > capacity) {
            capacity *= 2;
            t.kind.resize(capacity);
            t.value.resize(capacity);
            t.loc.resize(capacity);
            K = t.kind.data();
            V = t.value.data();
            L = t.loc.data();
        }
        int k = tok.lex(al, V[n], L[n], diagnostics, continue_compilation);
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

Result<AST::TranslationUnit_t*> ll_parse(Allocator &al,
        const std::string &input, diag::Diagnostics &diagnostics,
        LLTokens &tokens, const CompilerOptions &co, uint32_t loc_offset)
{
    // The whole file is tokenized first; the errors of the tokenizer are
    // added to the diagnostics as the parser gets to them
    diag::Diagnostics tokenizer_diagnostics;
    if (co.fixed_form) {
        // The fixed-form tokenizer finds the tokens in the source without
        // blanks, and reports its errors before the parser starts
        tokens.input = input;
        if (input.empty() || input.back() != '\n') tokens.input.append("\n");
        tokens.source = &tokens.input;
        FixedFormTokenizer t;
        t.set_string(tokens.input);
        if (!t.tokenize_input(diagnostics, al, co.continue_compilation)) {
            return Error();
        }
        tokens.kind = std::move(t.tokens);
        tokens.value = std::move(t.stypes);
        tokens.loc = std::move(t.locations);
        if (tokens.kind.empty() || tokens.kind.back() != END_OF_FILE) {
            tokens.kind.push_back(END_OF_FILE);
            tokens.value.push_back(YYSTYPE());
            tokens.loc.push_back(tokens.loc.empty() ? Location()
                : tokens.loc.back());
        }
        tokens.n = tokens.kind.size();
        // The parser looks ahead without checks
        for (size_t j = 0; j < 4; j++) {
            tokens.kind.push_back(END_OF_FILE);
            tokens.value.push_back(YYSTYPE());
            tokens.loc.push_back(tokens.loc.back());
        }
    } else {
        try {
            ll_tokenize(al, input, tokenizer_diagnostics, tokens, co.openmp,
                loc_offset, co.continue_compilation);
        } catch (const parser_local::TokenizerAbort &) {
            for (auto &d : tokenizer_diagnostics.diagnostics) {
                diagnostics.diagnostics.push_back(d);
            }
            return Error();
        }
    }
    LL ll(al, diagnostics, tokens.kind.data(), tokens.value.data(),
        tokens.loc.data(), *tokens.source, loc_offset,
        co.continue_compilation);
    ll.fixed_form = co.fixed_form;
    ll.script = !co.fixed_form && (co.interactive || co.infer_mode);
    ll.tokenizer_errors = &tokenizer_diagnostics.diagnostics;
    try {
        ll.units();
        ll.flush_tokenizer_errors(true);
    } catch (const LLUnsupported &) {
        ll.syntax_error();
        ll.flush_tokenizer_errors(true);
    } catch (const LLStop &) {
        ll.flush_tokenizer_errors(true);
    } catch (const parser_local::ParserAbort &) {
        ll.flush_tokenizer_errors(true);
        return Error();
    }
    if (diagnostics.has_error() && !co.continue_compilation) {
        return Error();
    }
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
    return ast;
}

} // namespace LCompilers::LFortran
