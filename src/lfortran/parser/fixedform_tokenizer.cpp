#include <limits>
#include <utility>

#include <lfortran/parser/parser_exception.h>
#include <lfortran/parser/fixedform_tokenizer.h>
#include <lfortran/parser/parser.tab.hh>
#include <lfortran/parser/tokenizer.h>
#include <libasr/bigint.h>

#include <lfortran/pickle.h>


int position = 0;
#define TOK(pos, arg) std::cout << std::string(pos, ' ') << "TOKEN: " << arg << std::endl;

namespace LFortran
{

std::map<std::string, yytokentype> identifiers_map = {
    {"end of file", END_OF_FILE},
    {"newline", TK_NEWLINE},
    {"\n", TK_NEWLINE},
    {"name", TK_NAME},
    {"def_op", TK_DEF_OP},
    {"tk_integer", TK_INTEGER},
    {"tk_label", TK_LABEL},
    {"tk_real", TK_REAL},
    {"boz_constant", TK_BOZ_CONSTANT},
    {"plus", TK_PLUS},
    {"minus", TK_MINUS},
    {"star", TK_STAR},
    {"slash", TK_SLASH},
    {"colon", TK_COLON},
    {"semicolon", TK_SEMICOLON},
    {"comma", TK_COMMA},
    {"equal", TK_EQUAL},
    {"lparen", TK_LPAREN},
    {"rparen", TK_RPAREN},
    {"lbracket", TK_LBRACKET},
    {"rbracket", TK_RBRACKET},
    {"rbracket_old", TK_RBRACKET_OLD},
    {"percent", TK_PERCENT},
    {"vbar", TK_VBAR},
    {"string", TK_STRING},
    {"comment", TK_COMMENT},
    {"eolcomment", TK_EOLCOMMENT},
    {"..", TK_DBL_DOT},
    {"::", TK_DBL_COLON},
    {"pow", TK_POW},
    {"concat", TK_CONCAT},
    {"arrow", TK_ARROW},
    {"eq", TK_EQ},
    {"ne", TK_NE},
    {"lt", TK_LT},
    {"le", TK_LE},
    {"gt", TK_GT},
    {"ge", TK_GE},
    {"not", TK_NOT},
    {"and", TK_AND},
    {"or", TK_OR},
    {"xor", TK_XOR},
    {"eqv", TK_EQV},
    {"neqv", TK_NEQV},
    {"true", TK_TRUE},
    {"false", TK_FALSE},
    {"format", TK_FORMAT},
    {"abstract", KW_ABSTRACT},
    {"all", KW_ALL},
    {"allocatable", KW_ALLOCATABLE},
    {"allocate", KW_ALLOCATE},
    {"assign", KW_ASSIGN},
    {"assignment", KW_ASSIGNMENT},
    {"associate", KW_ASSOCIATE},
    {"asynchronous", KW_ASYNCHRONOUS},
    {"backspace", KW_BACKSPACE},
    {"bind", KW_BIND},
    {"block", KW_BLOCK},
    {"call", KW_CALL},
    {"case", KW_CASE},
    {"change", KW_CHANGE},
    {"change_team", KW_CHANGE_TEAM},
    {"character", KW_CHARACTER},
    {"class", KW_CLASS},
    {"close", KW_CLOSE},
    {"codimension", KW_CODIMENSION},
    {"common", KW_COMMON},
    {"complex", KW_COMPLEX},
    {"concurrent", KW_CONCURRENT},
    {"contains", KW_CONTAINS},
    {"contiguous", KW_CONTIGUOUS},
    {"continue", KW_CONTINUE},
    {"critical", KW_CRITICAL},
    {"cycle", KW_CYCLE},
    {"data", KW_DATA},
    {"deallocate", KW_DEALLOCATE},
    {"default", KW_DEFAULT},
    {"deferred", KW_DEFERRED},
    {"dimension", KW_DIMENSION},
    {"do", KW_DO},
    {"dowhile", KW_DOWHILE},
    {"double", KW_DOUBLE},
    {"doubleprecision", KW_DOUBLE_PRECISION},
    {"doublecomplex", KW_DOUBLE_COMPLEX},
    {"elemental", KW_ELEMENTAL},
    {"else", KW_ELSE},
    {"elseif", KW_ELSEIF},
    {"elsewhere", KW_ELSEWHERE},
    {"end", KW_END},
    {"end_program", KW_END_PROGRAM},
    {"endprogram", KW_ENDPROGRAM},
    {"end_module", KW_END_MODULE},
    {"endmodule", KW_ENDMODULE},
    {"end_submodule", KW_END_SUBMODULE},
    {"endsubmodule", KW_ENDSUBMODULE},
    {"end_block", KW_END_BLOCK},
    {"endblock", KW_ENDBLOCK},
    {"end_block_data", KW_END_BLOCK_DATA},
    {"endblockdata", KW_ENDBLOCKDATA},
    {"end_subroutine", KW_END_SUBROUTINE},
    {"endsubroutine", KW_ENDSUBROUTINE},
    {"end_function", KW_END_FUNCTION},
    {"endfunction", KW_ENDFUNCTION},
    {"end_procedure", KW_END_PROCEDURE},
    {"endprocedure", KW_ENDPROCEDURE},
    {"end_enum", KW_END_ENUM},
    {"endenum", KW_ENDENUM},
    {"end_select", KW_END_SELECT},
    {"endselect", KW_ENDSELECT},
    {"end_if", KW_END_IF},
    {"endif", KW_ENDIF},
    {"end_interface", KW_END_INTERFACE},
    {"endinterface", KW_ENDINTERFACE},
    {"end_type", KW_END_TYPE},
    {"endtype", KW_ENDTYPE},
    {"end_associate", KW_END_ASSOCIATE},
    {"endassociate", KW_ENDASSOCIATE},
    {"end_forall", KW_END_FORALL},
    {"endforall", KW_ENDFORALL},
    {"end_do", KW_END_DO},
    {"enddo", KW_ENDDO},
    {"end_where", KW_END_WHERE},
    {"endwhere", KW_ENDWHERE},
    {"end_critical", KW_END_CRITICAL},
    {"endcritical", KW_ENDCRITICAL},
    {"end_file", KW_END_FILE},
    {"endfile", KW_ENDFILE},
    {"end_team", KW_END_TEAM},
    {"endteam", KW_ENDTEAM},
    {"entry", KW_ENTRY},
    {"enum", KW_ENUM},
    {"enumerator", KW_ENUMERATOR},
    {"equivalence", KW_EQUIVALENCE},
    {"errmsg", KW_ERRMSG},
    {"error", KW_ERROR},
    {"event", KW_EVENT},
    {"exit", KW_EXIT},
    {"extends", KW_EXTENDS},
    {"external", KW_EXTERNAL},
    {"file", KW_FILE},
    {"final", KW_FINAL},
    {"flush", KW_FLUSH},
    {"forall", KW_FORALL},
    {"formatted", KW_FORMATTED},
    {"form", KW_FORM},
    {"form_team", KW_FORM_TEAM},
    {"function", KW_FUNCTION},
    {"generic", KW_GENERIC},
    {"go", KW_GO},
    {"goto", KW_GOTO},
    {"if", KW_IF},
    {"images", KW_IMAGES},
    {"implicit", KW_IMPLICIT},
    {"import", KW_IMPORT},
    {"impure", KW_IMPURE},
    {"in", KW_IN},
    {"include", KW_INCLUDE},
    {"inout", KW_INOUT},
    {"in_out", KW_IN_OUT},
    {"inquire", KW_INQUIRE},
    {"integer", KW_INTEGER},
    {"intent", KW_INTENT},
    {"interface", KW_INTERFACE},
    {"intrinsic", KW_INTRINSIC},
    {"is", KW_IS},
    {"kind", KW_KIND},
    {"len", KW_LEN},
    {"local", KW_LOCAL},
    {"local_init", KW_LOCAL_INIT},
    {"logical", KW_LOGICAL},
    {"memory", KW_MEMORY},
    {"module", KW_MODULE},
    {"mold", KW_MOLD},
    {"name", KW_NAME},
    {"namelist", KW_NAMELIST},
    {"new_index", KW_NEW_INDEX},
    {"nopass", KW_NOPASS},
    {"non_intrinsic", KW_NON_INTRINSIC},
    {"non_overridable", KW_NON_OVERRIDABLE},
    {"non_recursive", KW_NON_RECURSIVE},
    {"none", KW_NONE},
    {"nullify", KW_NULLIFY},
    {"only", KW_ONLY},
    {"open", KW_OPEN},
    {"operator", KW_OPERATOR},
    {"optional", KW_OPTIONAL},
    {"out", KW_OUT},
    {"parameter", KW_PARAMETER},
    {"pass", KW_PASS},
    {"pointer", KW_POINTER},
    {"post", KW_POST},
    {"precision", KW_PRECISION},
    {"print", KW_PRINT},
    {"private", KW_PRIVATE},
    {"procedure", KW_PROCEDURE},
    {"program", KW_PROGRAM},
    {"protected", KW_PROTECTED},
    {"public", KW_PUBLIC},
    {"pure", KW_PURE},
    {"quiet", KW_QUIET},
    {"rank", KW_RANK},
    {"read", KW_READ},
    {"real", KW_REAL},
    {"recursive", KW_RECURSIVE},
    {"reduce", KW_REDUCE},
    {"result", KW_RESULT},
    {"return", KW_RETURN},
    {"rewind", KW_REWIND},
    {"save", KW_SAVE},
    {"select", KW_SELECT},
    {"select_case", KW_SELECT_CASE},
    {"select_rank", KW_SELECT_RANK},
    {"select_type", KW_SELECT_TYPE},
    {"sequence", KW_SEQUENCE},
    {"shared", KW_SHARED},
    {"source", KW_SOURCE},
    {"stat", KW_STAT},
    {"stop", KW_STOP},
    {"submodule", KW_SUBMODULE},
    {"subroutine", KW_SUBROUTINE},
    {"sync", KW_SYNC},
    {"sync_all", KW_SYNC_ALL},
    {"sync_images", KW_SYNC_IMAGES},
    {"sync_memory", KW_SYNC_MEMORY},
    {"sync_team", KW_SYNC_TEAM},
    {"target", KW_TARGET},
    {"team", KW_TEAM},
    {"team_number", KW_TEAM_NUMBER},
    {"then", KW_THEN},
    {"to", KW_TO},
    {"type", KW_TYPE},
    {"unformatted", KW_UNFORMATTED},
    {"use", KW_USE},
    {"value", KW_VALUE},
    {"volatile", KW_VOLATILE},
    {"wait", KW_WAIT},
    {"where", KW_WHERE},
    {"while", KW_WHILE},
    {"write", KW_WRITE},
    {"uminus", UMINUS}
};

std::vector<std::string> declarators{
            "integer",
            "real",
            "complex",
            "doubleprecision",
            "doublecomplex",
            "external",
            "dimension",
            "character",
            "logical",
            "bytes",
            "data"
        };

std::vector<std::string> lines{};

std::vector<std::string> io_names{"open", "read", "write", "format", "close", "print"};

void FixedFormTokenizer::set_string(const std::string &str)
{
    // The input string must be NULL terminated, otherwise the tokenizertostr will
    // not detect the end of string. After C++11, the std::string is guaranteed
    // to end with \0, but we check this here just in case.
    LFORTRAN_ASSERT(str[str.size()] == '\0');
    cur = (unsigned char *)(&str[0]);
    string_start = cur;
    cur_line = cur;
    line_num = 1;
}

#define KW(x) RET(KW_##x);
#define RET(x) token_loc(loc); last_token=yytokentype::x; return yytokentype::x;
#define WARN_REL(x) add_rel_warning(diagnostics, yytokentype::TK_##x);

struct FixedFormRecursiveDescent {
    Tokenizer t;
    diag::Diagnostics &diag;
    Allocator &m_a;
    unsigned char *string_start;
    std::vector<YYSTYPE> stypes;
    std::vector<int> tokens;
    std::vector<Location> locations;
    // we need to keep state when descending a loop nesting
    bool abort_loop = false;
    int64_t do_levels = 0;
    std::vector<int64_t> do_labels;

    FixedFormRecursiveDescent(diag::Diagnostics &diag,
        Allocator &m_a) : diag{diag}, m_a{m_a} {
            t.fixed_form = true;
        };

    // Auxiliary functions:

    void error(unsigned char *cur, const std::string &text) {
        uint32_t loc_first = cur-string_start;
        Location loc;
        loc.first = loc_first;
        loc.last = loc_first;
        // auto next=cur; next_line(next);
        //std::cout << "error line " << tostr(cur,next-1) << std::endl;
        throw LFortran::parser_local::TokenizerError(text, loc);
    }

    // Are the next characters in the `cur` stream equal to `str`?
    bool next_is(unsigned char *cur, const std::string &str) {
        unsigned char *tok = cur;
        unsigned char *cur2 = cur;
        while ((size_t)(cur2-tok) < str.size()) {
            if (*cur2 == '\0') {
                return false;
            }
            cur2++;
        }
        std::string next_str = std::string((char *)tok, cur2 - tok);
        return next_str == str;
    }

    bool next_is_eol(unsigned char *cur) {
        if (*cur == '\n') {
            return true;
        } else if (*cur == '\r' && *(cur+1) == '\n') {
            return true;
        }
        return false;
    }

    bool is_integer(const std::string &s) const {
        return !s.empty() && std::all_of(s.begin(), s.end(), [](char c) {
            // TODO: I think `c` can never be ' ', since we remove all
            // space
            return ::isdigit(c) || c == ' ';
        });
    }

    int64_t eat_label(unsigned char *&cur) {
        // consume label if it is available
        // for line beginnings
        const int reserved_cols = 6;
        std::string label;
        label.assign((char*)cur, reserved_cols);
        if (is_integer(label)) {
            lines.push_back(label);
            YYSTYPE y;
            std::string::iterator end = std::remove(label.begin(), label.end(), ' ');
            label.erase(end, label.end());
            unsigned char *t = (unsigned char*)&label[0];
            unsigned char *e = (unsigned char*)&label[label.size()];
            lex_int_large(m_a, t,e,
                    y.int_suffix.int_n,
                    y.int_suffix.int_kind);
            tokens.push_back(yytokentype::TK_LABEL);
            stypes.push_back(y);
            Location loc;
            loc.first = cur - string_start;
            loc.last = cur - string_start + label.size();
            locations.push_back(loc);
            cur+=reserved_cols;
            return y.int_suffix.int_n.n;
        }
        return -1;
    }

    void undo_label(unsigned char *&cur) {
        cur -= 6;
        tokens.pop_back();
        stypes.pop_back();
        locations.pop_back();
    }

    int64_t eat_label_inline(unsigned char *&cur) {
        // consume label if it is available
        auto start = cur;
        auto cur2 = cur;
        std::string label;
        unsigned long long count = 0;
        if (::isdigit(*cur2)) {
            while(*cur2 == ' ' || ::isdigit(*(cur2++))) count++;
            cur2--; // we advance one too much
        }
        label.assign((char*)start, count);
        if (is_integer(label) && count > 0) {
            YYSTYPE yy;
            lex_int_large(m_a, cur, cur2,
                    yy.int_suffix.int_n,
                    yy.int_suffix.int_kind);
            tokens.push_back(yytokentype::TK_INTEGER);
            stypes.push_back(yy);
            Location loc;
            loc.first = cur - string_start;
            loc.last = cur - string_start + label.size();
            locations.push_back(loc);
            cur+=count+1; // TODO revisit
            return yy.int_suffix.int_n.n;
        }
        return -1;
    }

    void next_line(unsigned char *&cur) {
        while (*cur != '\n' && *cur != '\0') {
            cur++;
        }
        if (*cur == '\n') cur++;
    }

    // Push the TK_NEWLINE, YYSTYPE and Location of the newline at `cur`.
    // (Does not modify `cur`.)
    void push_TK_NEWLINE(unsigned char *cur) {
        push_token_no_advance(cur, "\n", TK_NEWLINE);
    }

    // Push the token_type, YYSTYPE and Location of the token_str at `cur`.
    // (Does not modify `cur`.)
    void push_token_no_advance(unsigned char *cur, const std::string &token_str,
            yytokentype token_type) {
        YYSTYPE yy;
        yy.string.from_str(m_a, token_str);
        stypes.push_back(yy);
        tokens.push_back(token_type);
        Location loc;
        loc.first = cur - string_start;
        loc.last = cur - string_start + token_str.size();
        locations.push_back(loc);
    }

    // Same as push_token_no_advance(), but update `cur` and `t.cur`
    // TODO: just use `token_type`, and advance `cur` accordingly and check
    // that the string actually matches
    void push_token2(unsigned char *&cur, const std::string &token_str,
            yytokentype token_type) {
        push_token_no_advance(cur, token_str, token_type);
        cur += token_str.size();
        t.cur = cur;
    }

    // Same as push_token2(), but token_type is automatically determined
    void push_token3(unsigned char *&cur, const std::string &token_str) {
        push_token2(cur, token_str, identifiers_map[token_str]);
    }

    bool contains(unsigned char *start, unsigned char *end, char ch) {
        unsigned char *cur = start;
        while (*cur != '\0' && cur < end) {
            if (*cur == ch) {
                return true;
            }
            cur++;
        }
        return false;
    }

    std::string tostr(unsigned char *start, unsigned char *end) {
        return std::string((char *)start, end-start);
    }

    // Return the current token's location
    void token_loc(unsigned char *tok, unsigned char *cur, Location &loc) const
    {
        loc.first = tok-string_start;
        loc.last = cur-string_start-1;
    }

    /*
    ------------------------------------------------------------------------
    The is_*() functions return true/false if the given character is
    of a given type (digit, char)
    */

    bool is_digit(unsigned char ch) {
        return (ch >= '0' && ch <= '9');
    }

    bool is_char(unsigned char ch) {
        return (ch >= 'a' && ch <= 'z');
    }

    bool is_arit_op(unsigned char ch) {
        return (ch == '+' || ch == '-' || ch == '*' || ch == '/');
    }

    // Two character relational operator
    bool is_rel2_op(unsigned char *cur) {
        if (*cur != '\0' && *(cur+1) != '\0') {
            std::string op((char*)cur,2);
            if (op == "<=" || op == ">=" || op == "==" || op == "/=") {
                return true;
            }
        }
        return false;
    }

    /*
    ------------------------------------------------------------------------
    The try_*() functions return true/false if the pointer `cur` points to
    a given type (integer, name, given keyword, etc.). If true, it also
    advances the pointer. If false, the pointer is untouched.

    The try_*() functions are probably the same as the lex_* functions.
    I think lex_* functions that return a bool should be renamed to try_* to
    make it obvious that they might fail. Lex would then always succeed or
    return an error message.

    The main difference with these and the rest of this file is that these
    functions parse the code, but only communicate true/false (success/failure)
    and move the cursor `cur`, but do not emit any tokens or other information.

    They are used to probe the code to figure out what Fortran statement we
    are dealing with. For example try_expr() will return true if it is
    an expression and move the cursor, or false. If we want to return the
    tokens, we later need to then take the span and turn it into tokens.
    */

    // cur points to an integer: digit*
    bool try_integer(unsigned char *&cur) {
        unsigned char *old = cur;
        while (is_digit(*cur)) cur++;
        if (cur > old) {
            return true;
        } else {
            return false;
        }
    }

    // cur points to an name: char|_ (char|digit|_)*
    bool try_name(unsigned char *&cur) {
        unsigned char *old = cur;
        if (is_char(*cur) || (*cur == '_')) {
            cur++;
            while (is_char(*cur) || is_digit(*cur) || (*cur == '_')) cur++;
        }
        if (cur > old) {
            return true;
        } else {
            return false;
        }
    }

    // cur points exactly to "str"
    bool try_next(unsigned char *&cur, const std::string &str) {
        if (next_is(cur, str)) {
            cur += str.size();
            return true;
        }
        return false;
    }

    // cur points to an name: char (char|digit)*
    bool try_end_of_stmt(unsigned char *&cur) {
        if (try_next(cur, "\n")) return true;
        if (try_next(cur, ";")) return true;
        return false;
    }

    // ------------------------------------------------------------------
    // The following try_*() and lex_*() functions behave exactly like the above
    // ones, but they now parse Fortran syntax

    // Parses ' and " strings
    bool lex_string(unsigned char *&cur) {
        unsigned char *delim = cur;
        LFORTRAN_ASSERT(*delim == '"' || *delim == '\'')
        cur++;
        while (true) {
            if (*cur == '\0') {
                // String is not terminated
                cur = delim;
                return false;
            }
            if (*cur == *delim) {
                cur++;
                if (*cur == '\0') {
                    // String is ended by delim
                    return true;
                }
                if (*cur == *delim) {
                    // Either '' or "", we continue
                    cur++;
                    continue;
                }
                // String is ended by delim
                return true;
            }
            cur++;
        }
    }

    // cur points to a Fortran expression
    bool try_expr(unsigned char *&cur, bool nested) {
        // We do not try to parse all the details, we simply accept all
        // characters that can appear in an expression and we correctly
        // match parentheses and parse strings.
        unsigned char *old = cur;
        while (true) {
            if (is_rel2_op(cur)) {
                cur += 2;
                continue;
            }
            if (   is_digit(*cur)
                || is_char(*cur)
                || is_arit_op(*cur)
                || (*cur == '%')
                || (*cur == '[')
                || (*cur == ']')
                || (*cur == '<')
                || (*cur == '>')
                || (*cur == '.')
                || (*cur == '_')
                    ) {
                cur++;
                continue;
            }
            if (nested) {
                // Nested expressions can also contain [,:=], such as
                // in "f(x, y=3)/4" or "A(:,:)"
                if ((*cur == ',') || (*cur == '=') || (*cur == ':')) {
                    cur++;
                    continue;
                }
            }
            if (*cur == '(') {
                cur++;
                if (*cur == ')') {
                    cur++;
                    continue;
                }
                if (!try_expr(cur, true)) {
                    cur = old;
                    return false;
                }
                if (*cur == ')') {
                    cur++;
                    continue;
                } else {
                    // Unmatched parenthesis
                    cur = old;
                    return false;
                }
            }
            if (*cur == '"' || *cur == '\'') {
                lex_string(cur);
                continue;
            }
            break;
        }
        if (cur > old) {
            return true;
        } else {
            return false;
        }
    }


    // Recursive descent parser with backtracking
    //
    // If a function returns void, then it will always parse the given grammar
    // rule (and `cur` progressed), or give a compiler error (via the `error`
    // function).  If a function returns bool, then it does not raise any
    // errors, and returns true (parsed, `cur` progressed) or false (not-parsed,
    // `cur` unchanged).


    /*
     * Then the rest of the line is tokenized, including the new line.
     */
    void tokenize_line(unsigned char *&cur) {
        t.cur = cur;
        next_line(cur);
        tokenize_until(cur);
        LFORTRAN_ASSERT(*(t.cur-1) == '\n')
    }

    /*
     * The span [t.cur, end) is tokenized. The `t.cur` is inclusive, `end`
     * is exclusive (points to the next character).
     * The last token *must* end at the position end-1. If it does not,
     * we raise an exception.
     * The tokenizer ideally should be called consecutively, as it maintains
     * an internal state as well for do loops and other things. If you
     * need to adjust the starting point, you must make sure the internal
     * state is not made inconsistent.
     */
    void tokenize_until(unsigned char *end) {
        LFORTRAN_ASSERT(t.cur < end)
        // TODO: Is this needed?
        std::string line{tostr(t.cur, end)};
        lines.push_back(line);
        Location loc;
        ptrdiff_t len;
        while (t.cur < end) {
            YYSTYPE y2;
            auto token = t.lex(m_a, y2, loc, diag);
            len = t.cur - t.tok;
            tokens.push_back(token);
            if (token == yytokentype::TK_INTEGER) {
                lex_int_large(m_a, t.tok, t.cur,
                    y2.int_suffix.int_n,
                    y2.int_suffix.int_kind);
            } else if (token == yytokentype::TK_STRING) {
                std::string tk{tostr(t.tok+1, t.tok + len-1)};
                y2.string.from_str(m_a, tk);
            } else {
                std::string tk{tostr(t.tok, t.tok + len)};
                y2.string.from_str(m_a, tk);
            }
            stypes.push_back(y2);
            locations.push_back(loc);
        }
        // Check that the last token ends exactly on `end`:
        LFORTRAN_ASSERT(t.cur == end)
    }

    // returns TRUE iff multiline-if
    bool lex_if_statement(unsigned char *&cur) {
        YYSTYPE y1;
        std::string l("if");
        y1.string.from_str(m_a, l);
        stypes.push_back(y1);
        tokens.push_back(yytokentype::KW_IF);

        Location loc;
        loc.first = cur - string_start;
        loc.last = cur - string_start + l.size();
        locations.push_back(loc);

        unsigned char *start = cur + l.size();
        t.cur = start;

        LFORTRAN_ASSERT(*t.cur == '(')
        tokenize_until(t.cur+1);
        unsigned char *end = t.cur;
        bool multiline = false;
        if (try_expr(end, false)) {
            if (*end == ')') {
                end++;
                if (next_is(end, "then")) {
                    if (next_is_eol(end+4) || *(end+4) == '!') {
                        multiline = true;
                    }
                }
            } else {
                loc.first = end - string_start;
                loc.last = end - string_start;
                throw parser_local::TokenizerError("Expected `)` here to end the condition expression of the if statement ", loc);
            }
        } else {
            throw parser_local::TokenizerError("Expected expression after `if`", loc);
        }
        tokenize_until(end);
        cur = end;
        next_line(cur);
        if (multiline) {
            // Take care of "then\n"
            tokenize_until(cur);
        } else {
            // Check for arithmetic if
            if (is_digit(*end)) {
                // Arithmetic if
                tokenize_until(cur);
            } else {
                // Regular statement
                // Tokenize the rest of the single line if statement
                lex_body_statement(end);
            }
        }
        return multiline;
    }

    bool lex_declaration(unsigned char *&cur) {
        unsigned char *start = cur;
        next_line(cur);
        if (lex_declarator(start)) {
            tokenize_line(start);
            return true;
        }
        cur = start;
        return false;
    }

    bool lex_declarator(unsigned char *&cur) {
        for(const auto& declarator : declarators) {
            if(next_is(cur, declarator)) {
                push_token3(cur, declarator);
                return true;
            }
        }
        return false;
    }

    bool lex_io(unsigned char *&cur) {
        for(const auto &io_str: io_names) {
            if (next_is(cur, io_str)) {
                if (io_str == "format") {
                    cur += io_str.size();
                    unsigned char *start;
                    Location loc;
                    lex_format(cur, loc, start);
                    locations.push_back(loc);
                    YYSTYPE yylval;
                    yylval.string.p = (char*) start;
                    yylval.string.n = cur-start-1;
                    stypes.push_back(yylval);
                    tokens.push_back(TK_FORMAT);
                    next_line(cur);
                    push_TK_NEWLINE(cur-1);
                } else {
                    push_token3(cur, io_str);
                    tokenize_line(cur);
                }
                return true;
            }
        }
        return false;
    }

    bool has_terminal(unsigned char *&cur) {
        std::vector<std::string> terminals {
            "end"
        };
        for (const auto &terminal : terminals) {
            if (next_is(cur, terminal))
                return true;
        }
        return false;
    }

    void lex_implicit(unsigned char *&cur) {
        LFORTRAN_ASSERT(next_is(cur, "implicit"))
        push_token3(cur, "implicit");
        if (next_is(cur, "doubleprecision(")) {
            push_token3(cur, "double");
        }
        tokenize_line(cur);
    }

    bool is_function_call(unsigned char *cur) {
        if (try_next(cur, "call")) {
            if (try_name(cur)) {
                // Skip optional parentheses (skips strings, nested
                // parentheses etc.)
                if (*cur == '(') {
                    cur++;
                    if (*cur == ')') {
                        cur++;
                    } else {
                        // By setting `true` we ensure all arguments are parsed
                        // (separated by commas)
                        if (!try_expr(cur, true)) {
                            // If the expression failed to parse, then it
                            // is not a properly formed function call
                            return false;
                        };
                        if (*cur != ')') {
                            // Missing right parenthesis
                            return false;
                        }
                        cur++;
                    }
                }
                // If we are at the end of the statement, then this must
                // be a function call. Otherwise it's something else,
                // such as assignment (=, or =>).
                if (*cur == '\n' || *cur == ';') {
                    return true;
                }
            }
        }
        return false;
    }

    bool lex_body_statement(unsigned char *&cur) {
        int64_t l = eat_label(cur);
        if (lex_declaration(cur)) {
            return true;
        }
        if (lex_io(cur)) return true;
        if (next_is(cur, "if(")) {
            lex_cond(cur);
            return true;
        }
        unsigned char *nline = cur; next_line(nline);
        if (is_do_loop(cur)) {
            lex_do(cur);
            return true;
        }

        if (is_function_call(cur)) {
            push_token3(cur, "call");
            tokenize_line(cur);
            return true;
        }

        // assignment
        // TODO: this is fragile
        if (contains(cur, nline, '=')) {
            tokenize_line(cur);
            return true;
        }

        // `GOTO (X,Z,Y) M` translates to (roughly)
        // `IF (M .EQ. 1) THEN;  GOTO X; ELSE IF (M .EQ. 2) GOTO Z; IF (M .EQ. 3) GOTO Y; ENDIF`
        if (next_is(cur, "goto(")) {
            // lex_goto_select(cur);
            tokenize_line(cur);
            return true;
        }

        // careful addition -- `IF` and `DO` terminals are `CONTINUE`, too
        if (next_is(cur, "continue")) {
            push_token3(cur, "continue");
            tokenize_line(cur);
            return true;
        }

        if (next_is(cur, "goto")) {
            push_token3(cur, "goto");
            tokenize_line(cur);
            return true;
        }

        if (next_is(cur, "return")) {
            push_token3(cur, "return");
            tokenize_line(cur);
            return true;
        }

        if (next_is(cur, "common")) {
            push_token3(cur, "common");
            tokenize_line(cur);
            return true;
        }

        if (next_is(cur, "save")) {
            push_token3(cur, "save");
            tokenize_line(cur);
            return true;
        }

        if (next_is(cur, "entry")) {
            push_token3(cur, "entry");
            tokenize_line(cur);
            return true;
        }

        if (next_is(cur, "intrinsic")) {
            push_token3(cur, "intrinsic");
            tokenize_line(cur);
            return true;
        }

        if (next_is(cur, "equivalence")) {
            push_token3(cur, "equivalence");
            tokenize_line(cur);
            return true;
        }

        if (next_is(cur, "implicit")) {
            lex_implicit(cur);
            return true;
        }

        if (next_is(cur, "stop")) {
            push_token3(cur, "stop");
            tokenize_line(cur);
            return true;
        }

        if (next_is(cur, "assign")) {
            lex_assign(cur);
            return true;
        }

        if (l != -1) {
            // Undo the label, as it will be handled later
            undo_label(cur);
        }

        return false;
    }

    void lex_assign(unsigned char *&cur) {
        push_token2(cur, "assign", KW_ASSIGN);
        if (try_integer(cur)) {
            tokenize_until(cur);
            if (next_is(cur, "to")) {
                push_token3(cur, "to");
                tokenize_line(cur);
            } else {
                Location loc;
                loc.first = cur-string_start;
                loc.last = cur-string_start;
                throw parser_local::TokenizerError("Expected 'to' here", loc);
            }
        } else {
            Location loc;
            loc.first = cur-string_start;
            loc.last = cur-string_start;
            throw parser_local::TokenizerError("Expected integer after `assign`", loc);
        }
    }

    void insert_enddo() {
        // return an explicit "end do" token here
        push_token_no_advance(t.cur, "enddo", KW_END_DO);
        // And a new line
        push_TK_NEWLINE(t.cur);
    }

    bool all_labels_match(int64_t label) {
        return std::all_of(do_labels.begin(), do_labels.end(), [&label](const auto & x){
                return x == label; 
                });
    }

    bool lex_do_terminal(unsigned char *&cur, int64_t do_label) {
        if (*cur == '\0') {
            Location loc;
            loc.first = 1;
            loc.last = 1;
            throw parser_local::TokenizerError("End of file inside a do loop", loc);
        }
        int64_t label = eat_label(cur);
        bool label_match = false;
        if (label != -1) {
            if (label == do_label) {
                label_match = true;
            } else {
                label_match = false;
            }
        }
        if (next_is(cur, "enddo")) {
            // end one nesting of loop
            insert_enddo();
            next_line(cur);
            do_levels--;
            return true;
        } else if (label_match && all_labels_match(label)) {
            // end entire loop nesting with single `CONTINUE`
            // the usual terminal statement for do loops
            if (next_is(cur, "continue")) {
                push_token3(cur, "continue");
                tokenize_line(cur);
            } else {
                lex_body_statement(cur);
            }
            for (int i=0;i<do_levels;++i)
                insert_enddo();
            if (label_match && do_levels > 1) abort_loop = true;
            do_levels = 0;
            do_labels.clear();
            return true;
        } else if (label_match) {
            // end one nesting of loop 
            if (next_is(cur, "continue")) {
                push_token3(cur, "continue");
                tokenize_line(cur);
            } else {
                lex_body_statement(cur);
            }
            insert_enddo();
            do_levels--;
            do_labels.pop_back();
            return true;
        } else {
            return false;
        }
    }

    /*
    If the line contains any of these forms, then it is a do loop:

    do [LABEL] [,] x = EXPR, EXPR [, EXPR]
    do
    */
    bool is_do_loop(unsigned char *cur) {
        if (try_next(cur, "do")) {
            // do
            try_integer(cur); // Optional: do 5
            if (try_end_of_stmt(cur)) {
                // do
                // do 5
                return true;
            }
            try_next(cur, ","); // Optional comma
            // do
            // do 5
            // do ,
            // do 5,
            if (try_name(cur)) {
                // do x
                // do 5 x
                // do , x
                // do 5, x
                if (try_next(cur, "=")) {
                    // do x =
                    // do 5 x =
                    if (try_expr(cur, false)) {
                        // do x = 1
                        // do 5 x = 1
                        if (try_next(cur, ",")) {
                            // do x = 1,
                            // do 5 x = 1,
                            // It must be a do loop at this point, as
                            // it cannot be an assignment "dox=1" or "do5x=1"
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }


    void lex_do(unsigned char *&cur) {
        auto end = cur; next_line(end);
        do_levels++;
        Location loc; loc.first = cur-string_start; loc.last = cur-string_start;
        std::string l = "do";
        push_token3(cur, l);
        int64_t do_label = eat_label_inline(cur);
        if (do_label != -1) {
            do_labels.push_back(do_label);
            cur--; // un-advance as eat_label_inline moves 1 char too far when making checks
        }
        tokenize_line(cur); // tokenize rest of line where `do` starts
        while (!lex_do_terminal(cur, do_label)) {
            if (!lex_body_statement(cur)) {
                throw parser_local::TokenizerError("End of file inside a do loop 2", loc);
            };

            if (abort_loop) {
                if (do_levels == 0) abort_loop = false;
                break;
            }
        }
    }

    bool if_advance_or_terminate(unsigned char *&cur) {
        eat_label(cur);
        if (next_is(cur, "elseif")) {
            push_token3(cur, "elseif");
            tokenize_line(cur);
            return true;
        }
        if (next_is(cur, "else")) {
            push_token3(cur, "else");
            tokenize_line(cur);
            return true;
        }
        // TODO: check for other if terminals
        if (next_is(cur, "endif")) {
            push_token3(cur, "endif");
            tokenize_line(cur);
            return false;
        }
        if (next_is(cur, "continue")) {
            push_token3(cur, "continue");
            tokenize_line(cur);
            return true;
        }
        lex_body_statement(cur);
        return true;
    }

    void lex_cond(unsigned char *&cur) {
        if (lex_if_statement(cur)) while (if_advance_or_terminate(cur));
    }

    void lex_subroutine(unsigned char *&cur) {
        while(lex_body_statement(cur));
        int64_t l = eat_label(cur);
        if (next_is(cur, "endsubroutine")) {
            if (l != -1) {
                push_token_no_advance(cur, "continue", KW_CONTINUE);
                push_token_no_advance(cur, "\n", TK_NEWLINE);
            }
            push_token3(cur, "endsubroutine");
            tokenize_line(cur);
        } else if (next_is(cur, "end")) {
            if (l != -1) {
                push_token_no_advance(cur, "continue", KW_CONTINUE);
                push_token_no_advance(cur, "\n", TK_NEWLINE);
            }
            push_token3(cur, "end");
            tokenize_line(cur);
        } else {
            error(cur, "Expecting terminating symbol for subroutine");
        }
    }

    void lex_program(unsigned char *&cur, bool explicit_program) {
        if (explicit_program) {
            push_token3(cur, "program");
            tokenize_line(cur);
        }
        while(lex_body_statement(cur));
        eat_label(cur);
        if (next_is(cur, "endprogram")) {
            push_token3(cur, "endprogram");
            tokenize_line(cur);
        } else if (next_is(cur, "end")) {
            push_token3(cur, "end");
            tokenize_line(cur);
        } else {
            error(cur, "Expecting terminating symbol for program");
        }
    }

    void lex_function(unsigned char *&cur) {
        while(lex_body_statement(cur));
        eat_label(cur);

        if (next_is(cur, "endfunction")) {
            push_token3(cur, "endfunction");
            tokenize_line(cur);
        } else if (next_is(cur, "end")) {
            push_token3(cur, "end");
            tokenize_line(cur);
        } else {
            error(cur, "Expecting terminating symbol for function");
        }
    }

    void lex_block_data(unsigned char *&cur) {
        push_token2(cur, "block", KW_BLOCK);
        push_token2(cur, "data", KW_DATA);
        tokenize_line(cur);
        while(lex_body_statement(cur));
        if (next_is(cur, "endblockdata")) {
            push_token3(cur, "endblockdata");
            tokenize_line(cur);
        } else if (next_is(cur, "end")) {
            push_token3(cur, "end");
            tokenize_line(cur);
        } else {
            error(cur, "Expecting terminating symbol for block data");
        }
    }

    bool is_declaration(unsigned char *&cur, std::string declaration_type /*function, subroutine, program*/, const std::vector<std::string>& keywords) {
        unsigned char *cpy = cur;
        unsigned char *nextline = cur; next_line(nextline);
        std::string line{tostr(cur, nextline-1)};
        // current line does not contain type -> we abort
        if (!(line.find(std::string(declaration_type)) != std::string::npos)) return false;

        std::vector<std::string> kw_found;
        std::vector<std::string> decls{keywords.begin(), keywords.end()};
        while(decls.size() != 0) {
            for (unsigned int i=0;i<decls.size();++i) {
                if (next_is(cpy, decls[i])) {
                    kw_found.push_back(decls[i]);
                    cpy += decls[i].size();
                    decls.erase(decls.begin() + i);
                    break;
                }
            }
            if (next_is(cpy, declaration_type))
                break;
            // catching syntax errors like `recursive double precision recursive function f(...`
            for (auto kw = kw_found.begin(); kw != kw_found.end(); ++kw) {
                if (next_is(cpy, *kw)) {
                    error(cpy, "Syntax error: keyword " + *kw + "cannot occur multiple times in " + declaration_type + "declaration");
                }
            }
        }

        if (kw_found.size() == 0 && !next_is(cpy, declaration_type))
            return false;
     
        // tokenize all keywords
        for(auto iter = kw_found.begin(); iter != kw_found.end(); ++iter) {
            push_token3(cur, *iter);
        }

        cur = cpy;
        push_token3(cur, declaration_type);
        tokenize_line(cur);
        return true;
    }

    bool is_implicit_program(unsigned char *cur) {
        auto cpy = cur;
        auto prev = cpy;
        for (;;) {
            next_line(cpy);
            if (*cpy == '\0') break;
            prev = cpy;
        }
        if (next_is(prev, "endprogram\n") || next_is(prev, "end\n")) {
            return true;
        }
        return false;
    }

    bool is_program(unsigned char *cur) {
        return next_is(cur, "program");
    }


    void lex_global_scope_item(unsigned char *&cur) {
        // we can define a global assignment
        unsigned char *nline = cur; next_line(nline);
        // eat_label(cur);
        std::vector<std::string> program_keywords{};
        std::vector<std::string> subroutine_keywords{"recursive", "pure",
            "elemental"};
        std::vector<std::string> function_keywords{"recursive", "pure",
            "elemental",
            "real", "character", "complex", "integer", "logical",
            "doubleprecision", "doublecomplex"};

        if (next_is(cur, "include")) {
            push_token3(cur, "include");
            tokenize_line(cur);
        }
        if (is_program(cur)) {
            lex_program(cur, true);
        } else if (is_declaration(cur, "subroutine", subroutine_keywords)) {
            lex_subroutine(cur);
        } else if (is_declaration(cur, "function", function_keywords)) {
            lex_function(cur);
        } else if (next_is(cur, "blockdata")) {
            lex_block_data(cur);
        } else if (is_implicit_program(cur)) {
            std::string prog{"program"};
            std::string name{"implicit_program_lfortran"};
            // add implicit global program at the line `cur` is currently at
            YYSTYPE y;
            y.string.from_str(m_a, prog);
            stypes.push_back(y);
            tokens.push_back(yytokentype::KW_PROGRAM);
            Location loc;
            loc.first = cur - string_start;
            loc.last = cur - string_start + prog.size();
            locations.push_back(loc);
            y.string.from_str(m_a, name);
            stypes.push_back(y);
            tokens.push_back(yytokentype::TK_NAME);
            loc.first = cur - string_start + prog.size();
            loc.last = cur - string_start + prog.size() + name.size();
            locations.push_back(loc);
            push_TK_NEWLINE(cur + prog.size() + name.size());
            lex_program(cur, false);
        } else {
            error(cur, "ICE: Cannot recognize global scope entity");
        }


    }

    void lex_global_scope(unsigned char *&cur) {
        auto next = cur;
        while (*cur != '\0') {
            // eat_label(cur);
            next_line(next);
            lex_global_scope_item(cur);
            next = cur;
        }
        YYSTYPE y2;
        y2.string.from_str(m_a, "EOF");
        stypes.push_back(y2);
        tokens.push_back(yytokentype::END_OF_FILE);
        Location loc;
        loc.first = cur - string_start;
        loc.last = cur - string_start + 1;
        locations.push_back(loc);
    }

};

bool FixedFormTokenizer::tokenize_input(diag::Diagnostics &diagnostics, Allocator &al) {
    // We use a recursive descent parser.  We are starting at the global scope
    try {
        FixedFormRecursiveDescent f(diagnostics, al);
        f.string_start = string_start;
        //f.t.set_string(lines[lines.size()-1]);
        f.t.cur = string_start;
        f.t.string_start = string_start;
        f.t.cur_line = string_start;
        f.t.line_num = 1;

        f.lex_global_scope(cur);
        tokens = std::move(f.tokens);
        stypes = std::move(f.stypes);
        locations = std::move(f.locations);
        LFORTRAN_ASSERT(tokens.size() == stypes.size())
        LFORTRAN_ASSERT(tokens.size() == locations.size())
        tokenized = true;
    } catch (const parser_local::TokenizerError &e) {
        diagnostics.diagnostics.push_back(e.d);
        return false;
    }
    return true;
}

int FixedFormTokenizer::lex(Allocator &/*al*/, YYSTYPE &yylval,
        Location &loc, diag::Diagnostics &/*diagnostics*/)
{
    if (!tokens.empty()) {
        auto tok = tokens[token_pos];
        yylval = stypes.at(token_pos);
        loc = locations.at(token_pos);
        token_pos++;
        return tok;
    } else {
        return yytokentype::END_OF_FILE;
    }
}


} // namespace LFortran
