/*
This is a fixed-form tokenizer. It accepts a prescanned source code that removes
all whitespace and splits it into tokens one statement at a time; see
FixedFormStatements below. The tokens go to the parser that is shared with
free form.

Note: The prescanner removes CR from regular code, but not from string literals.
*/
#include <algorithm>
#include <cstring>
#include <unordered_map>
#include <utility>

#include <lfortran/parser/parser_exception.h>
#include <lfortran/parser/fixedform_tokenizer.h>
#include <lfortran/parser/tokens.h>
#include <lfortran/parser/tokenizer.h>
#include <libasr/bigint.h>
#include <libasr/string_utils.h>




namespace LCompilers::LFortran {

static const std::unordered_map<std::string, yytokentype> &identifier_token_map() {
    static const std::unordered_map<std::string, yytokentype> map = {
    {"EOF", END_OF_FILE},
    {"\n", TK_NEWLINE},
    {"name", TK_NAME},
    {"def_op", TK_DEF_OP},
    {"tk_integer", TK_INTEGER},
    {"tk_label", TK_LABEL},
    {"tk_real", TK_REAL},
    {"boz_constant", TK_BOZ_CONSTANT},
    {"plus", TK_PLUS},
    {"minus", TK_MINUS},
    {"*", TK_STAR},
    {"slash", TK_SLASH},
    {"colon", TK_COLON},
    {"semicolon", TK_SEMICOLON},
    {"comma", TK_COMMA},
    {"equal", TK_EQUAL},
    {"lparen", TK_LPAREN},
    {"rparen", TK_RPAREN},
    {"lbracket", TK_LBRACKET},
    {"rbracket", TK_RBRACKET},
    {"lbrace", TK_LBRACE},
    {"rbace", TK_RBRACE},
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
    {"byte", KW_BYTE},
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
    {"extensible", KW_EXTENSIBLE},
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
    {"pause", KW_PAUSE},
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
    };
    return map;
}

void FixedFormTokenizer::set_string(const std::string &str)
{
    // The input string must be NULL terminated, otherwise the tokenizertostr will
    // not detect the end of string. After C++11, the std::string is guaranteed
    // to end with \0, but we check this here just in case.
    LCOMPILERS_ASSERT(str[str.size()] == '\0');
    cur = (unsigned char *)(&str[0]);
    // Skip UTF-8 BOM (EF BB BF) if present at start of file
    if (str.size() >= 3 && cur[0] == 0xEF && cur[1] == 0xBB && cur[2] == 0xBF) {
        cur += 3;
    }
    string_start = cur;
    cur_line = cur;
    line_num = 1;
}


/*
The statements of a fixed-form source, one at a time.

After the prescanner removed the blanks, the tokens of a statement such as
`do10i=1,5` cannot be found by the longest match: this one is the DO loop
`do 10 i = 1, 5`, while `do10i=1` is an assignment to the variable `do10i`.
The parser is the same as for free form and resolves the grammar; the only
job here is to split each statement into tokens, and that needs just the
statement itself:

  * An assignment (`name [(...) | %name]... = ...` without a comma outside of
    parentheses after the `=`) is tokenized by the longest match.

  * Any other statement starts with keywords. `statement_keywords` lists the
    ones that can be glued to what follows (`callf(x)`, `endsubroutinef`,
    `goto10`); the rest of the statement is tokenized by the longest match.
    A few keywords need more than that, see `Mode`.

One thing depends on the previous statements: a function or a subroutine
statement is only recognized where a program unit can start (`integer
function f(n)` and the declaration of the array `functionf` are otherwise
the same characters).
*/
struct FixedFormStatements {
    Tokenizer t;
    diag::Diagnostics &diag;
    Allocator &m_a;
    unsigned char *string_start;
    std::vector<YYSTYPE> stypes;
    std::vector<int> tokens;
    std::vector<Location> locations;
    // The program units that are open: 'p'rogram, 'm'odule, 's'ubroutine,
    // 'f'unction, 'b'lock data
    std::vector<char> units;
    // A program unit can start with the next statement
    bool unit_start = true;

    enum Mode {
        Plain,       // keywords, then the rest by the longest match
        Type,        // `integer*4x`: only digits follow the `*`
        Do,          // `do10i=1,5`, `dowhile(x)`, `doconcurrent(i=1:n)`
        Condition,   // `if(x)y=1`: a statement follows the `(...)`
        Format,      // the whole statement is one token
        Assign,      // `assign10toi`
        EndUnit,     // the end of a program unit
        EndIf,       // `10 endif`
        Contains,    // internal procedures follow
        Interface,   // interface bodies follow
    };

    struct Keywords {
        // The keywords, separated by a space
        const char *words;
        Mode mode = Plain;
        // If not 0, the character that must follow: '(' or '$' for the end
        // of the statement
        char next = 0;
    };

    struct Entry {
        std::string text;
        std::vector<std::pair<yytokentype, size_t>> words;
        Mode mode;
        char next;
    };

    static const std::vector<Entry> &statement_keywords(unsigned char first) {
        static const std::vector<Keywords> keywords = {
            {"abstract interface", Interface, '$'},
            {"allocatable"}, {"assign", Assign}, {"asynchronous"},
            {"backspace"}, {"byte", Type},
            {"call"}, {"case default"}, {"character", Type},
            {"class default"}, {"class is", Plain, '('},
            {"codimension"}, {"common"}, {"complex", Type},
            {"contains", Contains, '$'}, {"contiguous"}, {"cycle"},
            {"data"}, {"dimension"}, {"do", Do},
            {"doublecomplex", Type}, {"doubleprecision", Type},
            {"else"}, {"elseif", Condition, '('}, {"elsewhere"},
            {"end", EndUnit, '$'}, {"endassociate"}, {"endblock"},
            {"endblockdata", EndUnit}, {"endcritical"}, {"enddo"},
            {"endenum"}, {"endfile"}, {"endforall"},
            {"endfunction", EndUnit}, {"endif", EndIf}, {"endinterface"},
            {"endmodule", EndUnit}, {"endprocedure"},
            {"endprogram", EndUnit}, {"endselect"},
            {"endsubmodule", EndUnit}, {"endsubroutine", EndUnit},
            {"endtype"}, {"endwhere"}, {"entry"}, {"enumerator"},
            {"equivalence"}, {"error stop"}, {"exit"}, {"external"},
            {"flush"}, {"forall", Condition, '('}, {"format", Format, '('},
            {"goto"},
            {"if", Condition, '('}, {"implicit"}, {"import"},
            {"include"}, {"integer", Type}, {"interface", Interface},
            {"intrinsic"},
            {"logical", Type},
            {"module procedure"},
            {"namelist"},
            {"optional"},
            {"parameter"}, {"pause"}, {"pointer"}, {"print"}, {"private"},
            {"procedure"}, {"protected"}, {"public"},
            {"rank default"}, {"read"}, {"real", Type},
            {"return"}, {"rewind"},
            {"save"}, {"select case", Plain, '('},
            {"select rank", Plain, '('}, {"select type", Plain, '('},
            {"stop"},
            {"target"}, {"then"}, {"type is", Plain, '('}, {"type"},
            {"use"},
            {"value"}, {"volatile"},
            {"where", Condition, '('},
        };
        // By the first letter, the longest first
        static const std::vector<std::vector<Entry>> by_letter = [] {
            std::vector<std::vector<Entry>> table(26);
            for (const Keywords &k : keywords) {
                Entry e;
                e.mode = k.mode;
                e.next = k.next;
                std::string words = k.words;
                size_t start = 0;
                while (start < words.size()) {
                    size_t end = words.find(' ', start);
                    if (end == std::string::npos) end = words.size();
                    std::string word = words.substr(start, end - start);
                    e.words.push_back({identifier_token_map().at(word),
                        word.size()});
                    e.text += word;
                    start = end + 1;
                }
                table[e.text[0] - 'a'].push_back(e);
            }
            for (auto &entries : table) {
                std::stable_sort(entries.begin(), entries.end(),
                    [](const Entry &a, const Entry &b) {
                        return a.text.size() > b.text.size();
                    });
            }
            return table;
        }();
        static const std::vector<Entry> none;
        if (first < 'a' || first > 'z') return none;
        return by_letter[first - 'a'];
    }

    FixedFormStatements(diag::Diagnostics &diag,
        Allocator &m_a) : diag{diag}, m_a{m_a} {
            t.fixed_form = true;
        };

    // Are the next characters equal to `str`?
    static bool next_is(const unsigned char *cur, const char *str) {
        while (*str) {
            if (*cur++ != *str++) return false;
        }
        return true;
    }

    // A statement ends at the end of the line or at a `;` separator
    static bool is_eos(const unsigned char *cur) {
        return *cur == '\n' || *cur == ';' || *cur == '\0';
    }

    static bool is_digit(unsigned char ch) {
        return ch >= '0' && ch <= '9';
    }

    static bool is_name_start(unsigned char ch) {
        return (ch >= 'a' && ch <= 'z') || ch == '_';
    }

    static bool is_name_char(unsigned char ch) {
        return is_name_start(ch) || is_digit(ch);
    }

    static const unsigned char *skip_name(const unsigned char *cur) {
        while (is_name_char(*cur)) cur++;
        return cur;
    }

    // `cur` points to `'` or `"`; returns the position after the string, or
    // the end of the input if the string is not terminated
    static const unsigned char *skip_string(const unsigned char *cur) {
        unsigned char delim = *cur++;
        while (*cur != '\0') {
            if (*cur == delim) {
                cur++;
                if (*cur != delim) return cur;
            }
            cur++;
        }
        return cur;
    }

    // `cur` points to `(`; returns the position after the matching `)`, or
    // nullptr if the statement ends before it
    static const unsigned char *skip_parentheses(const unsigned char *cur) {
        int depth = 0;
        while (!is_eos(cur)) {
            if (*cur == '\'' || *cur == '"') {
                cur = skip_string(cur);
                continue;
            }
            if (*cur == '(') {
                depth++;
            } else if (*cur == ')') {
                depth--;
                if (depth == 0) return cur + 1;
            }
            cur++;
        }
        return nullptr;
    }

    /*
    Is the statement an assignment?

        x = 1
        a(i, j)%b(k) = 1
        p => q
        do10i = 1

    A comma outside of parentheses after the `=` means it is not:

        do10i = 1, 5
    */
    static bool is_assignment(const unsigned char *cur) {
        if (!is_name_start(*cur)) return false;
        cur = skip_name(cur);
        while (true) {
            if (*cur == '(') {
                cur = skip_parentheses(cur);
                if (cur == nullptr) return false;
            } else if (*cur == '%') {
                cur++;
                if (!is_name_start(*cur)) return false;
                cur = skip_name(cur);
            } else {
                break;
            }
        }
        if (*cur != '=') return false;
        cur++;
        int depth = 0;
        while (!is_eos(cur)) {
            if (*cur == '\'' || *cur == '"') {
                cur = skip_string(cur);
                continue;
            }
            if (*cur == '(' || *cur == '[') {
                depth++;
            } else if (*cur == ')' || *cur == ']') {
                depth--;
            } else if (*cur == ',' && depth == 0) {
                return false;
            }
            cur++;
        }
        return true;
    }

    // The label in the first five characters of a line, if there is one
    static bool has_label(const unsigned char *cur) {
        bool digit = false;
        for (int i = 0; i < 5; i++) {
            if (is_digit(cur[i])) {
                digit = true;
            } else if (cur[i] != ' ') {
                return false;
            }
        }
        return digit;
    }

    void push(int token, const YYSTYPE &y, const unsigned char *first,
            const unsigned char *last) {
        tokens.push_back(token);
        stypes.push_back(y);
        Location loc;
        loc.first = source_offset(first - string_start);
        loc.last = source_offset(last - string_start);
        locations.push_back(loc);
    }

    // The token `text` that covers `len` characters at `cur`
    void push_token(const unsigned char *cur, int token, const char *text,
            size_t len) {
        YYSTYPE y;
        y.string.from_str(m_a, text);
        push(token, y, cur, cur + len - 1);
    }

    void push_newline(const unsigned char *cur) {
        push_token(cur, TK_NEWLINE, "\n", 1);
    }

    // The keyword of `len` characters at `cur`
    void push_keyword(unsigned char *&cur, yytokentype token, size_t len) {
        YYSTYPE y;
        y.string.p = m_a.allocate<char>(len);
        y.string.n = len;
        std::memcpy(y.string.p, cur, len);
        push(token, y, cur, cur + len - 1);
        cur += len;
    }

    // The integer made of the digits [cur, end)
    void push_integer(int token, const unsigned char *cur,
            const unsigned char *end, const unsigned char *last) {
        YYSTYPE y;
        lex_int_large(m_a, cur, end, y.int_suffix.int_n, y.int_suffix.int_kind);
        push(token, y, cur, last);
    }

    // Consumes the label of the line, if there is one, otherwise returns -1
    int64_t label(unsigned char *&cur) {
        if (!has_label(cur)) return -1;
        unsigned char digits[5];
        size_t n = 0;
        for (int i = 0; i < 5; i++) {
            if (cur[i] != ' ') digits[n++] = cur[i];
        }
        YYSTYPE y;
        lex_int_large(m_a, digits, digits + n, y.int_suffix.int_n,
            y.int_suffix.int_kind);
        push(TK_LABEL, y, cur, cur + n);
        cur += 5;
        return y.int_suffix.int_n.n;
    }

    // Lex one token at `t.cur` by the longest match and push it
    int lex_token() {
        YYSTYPE y;
        Location loc;
        int token = t.lex(m_a, y, loc, diag, false);
        size_t len = t.cur - t.tok;
        if (token == TK_INTEGER) {
            lex_int_large(m_a, t.tok, t.cur, y.int_suffix.int_n,
                y.int_suffix.int_kind);
        } else if (token == TK_STRING) {
            std::string s((char *)t.tok + 1, len - 2);
            s.erase(std::remove(s.begin(), s.end(), '\r'), s.end());
            Str str;
            str.from_str(m_a, s);
            char *unescaped = str_unescape_fortran(m_a, str, t.tok[0]);
            y.string.p = unescaped;
            y.string.n = strlen(unescaped);
        } else if (token != TK_TRUE && token != TK_FALSE) {
            // (The kind of a logical constant is already in `y.string`)
            y.string.p = m_a.allocate<char>(len + 1);
            y.string.n = len;
            std::memcpy(y.string.p, t.tok, len);
            y.string.p[len] = '\0';
        }
        tokens.push_back(token);
        stypes.push_back(y);
        locations.push_back(loc);
        return token;
    }

    // Tokenizes the rest of the statement by the longest match, including
    // the new line or the `;` that ends it (and the empty statements that
    // follow a `;` up to the end of the line)
    void rest(unsigned char *&cur) {
        t.cur = cur;
        while (*t.cur != '\0') {
            int token = lex_token();
            if (*(t.cur-1) == '\n') break;
            if (token == TK_SEMICOLON && *t.cur != ';'
                    && *t.cur != '\n' && *t.cur != '\r') break;
        }
        cur = t.cur;
    }

    // Tokenizes [cur, end) by the longest match
    void until(unsigned char *&cur, const unsigned char *end) {
        t.cur = cur;
        while (t.cur < end) lex_token();
        cur = t.cur;
    }

    // Tokenizes the `(...)` at `cur`; returns false if the statement ended
    // before the matching `)`
    bool parentheses(unsigned char *&cur) {
        t.cur = cur;
        int depth = 0;
        do {
            if (*t.cur == '\0') break;
            int token = lex_token();
            if (token == TK_LPAREN) {
                depth++;
            } else if (token == TK_RPAREN) {
                depth--;
            } else if (*(t.cur-1) == '\n' || token == TK_SEMICOLON) {
                cur = t.cur;
                return false;
            }
        } while (depth > 0);
        cur = t.cur;
        return true;
    }

    // `*8` and `*(*)` of `real*8` and `character*(*)`: only digits follow
    // the star, so that `character*2d3v` is `character*2 d3v`
    void star_length(unsigned char *&cur) {
        if (*cur != '*') return;
        push_keyword(cur, TK_STAR, 1);
        unsigned char *end = cur;
        while (is_digit(*end)) end++;
        if (end > cur) {
            push_integer(TK_INTEGER, cur, end, end - 1);
            cur = end;
        }
    }

    /*
    The statement that starts a program unit:

        program p
        module m
        submodule (m) s
        block data b
        [recursive | pure | ...] subroutine s(x)
        [recursive | pure | ... | integer | real*8 | ...] function f(x)

    Returns the kind of the unit and tokenizes the statement (if `emit`),
    or returns 0.
    */
    char unit_statement(unsigned char *&cur, bool emit) {
        if (is_assignment(cur)) return 0;
        if (next_is(cur, "module") && is_name_start(cur[6])) {
            // `module m`; inside of a program unit it is `module procedure`
            // or the prefix of a separate module procedure
            if (units.empty()) {
                if (emit) {
                    push_keyword(cur, KW_MODULE, 6);
                    rest(cur);
                }
                return 'm';
            }
            if (next_is(cur, "moduleprocedure")) return 0;
        }
        static const char *const prefixes[] = {"recursive", "non_recursive",
            "pure", "impure", "elemental", "module"};
        static const char *const types[] = {"doubleprecision",
            "doublecomplex", "character", "complex", "integer", "logical",
            "real"};
        // Each span is a keyword ('k'), a `*` length ('*') or a `(...)`
        // selector ('(')
        struct Span { char kind; const unsigned char *start, *end; };
        std::vector<Span> spans;
        const unsigned char *p = cur;
        bool type_seen = false;
        while (true) {
            bool matched = false;
            for (const char *prefix : prefixes) {
                if (next_is(p, prefix)) {
                    size_t n = strlen(prefix);
                    spans.push_back({'k', p, p + n});
                    p += n;
                    matched = true;
                    break;
                }
            }
            if (matched) continue;
            if (type_seen) break;
            for (const char *type : types) {
                if (next_is(p, type)) {
                    size_t n = strlen(type);
                    spans.push_back({'k', p, p + n});
                    p += n;
                    if (*p == '*') {
                        const unsigned char *end = p + 1;
                        if (*end == '(') {
                            end = skip_parentheses(end);
                            if (end == nullptr) return 0;
                        } else {
                            while (is_digit(*end)) end++;
                        }
                        spans.push_back({'*', p, end});
                        p = end;
                    } else if (*p == '(') {
                        const unsigned char *end = skip_parentheses(p);
                        if (end == nullptr) return 0;
                        spans.push_back({'(', p, end});
                        p = end;
                    }
                    type_seen = true;
                    matched = true;
                    break;
                }
            }
            if (!matched) break;
        }
        char kind = 0;
        size_t keyword = 0;
        if (next_is(p, "subroutine") && is_name_start(p[10])) {
            kind = 's';
            keyword = 10;
        } else if (next_is(p, "function") && is_name_start(p[8])
                && *skip_name(p + 8) == '(') {
            kind = 'f';
            keyword = 8;
        } else if (!spans.empty()) {
            return 0;
        } else if (next_is(p, "program") && is_name_start(p[7])) {
            kind = 'p';
            keyword = 7;
        } else if (next_is(p, "submodule(")) {
            kind = 'm';
            keyword = 9;
        } else if (next_is(p, "blockdata")) {
            kind = 'b';
        } else {
            return 0;
        }
        if (!emit) return kind;
        {
            for (const Span &span : spans) {
                unsigned char *start = const_cast<unsigned char*>(span.start);
                if (span.kind == 'k') {
                    std::string word((const char*)span.start,
                        span.end - span.start);
                    push_keyword(cur, identifier_token_map().at(word),
                        word.size());
                } else if (span.kind == '*' && span.start[1] != '(') {
                    cur = start;
                    star_length(cur);
                } else {
                    cur = start;
                    until(cur, span.end);
                }
            }
        }
        if (kind == 'b') {
            push_keyword(cur, KW_BLOCK, 5);
            push_keyword(cur, KW_DATA, 4);
        } else {
            std::string word((const char*)cur, keyword);
            push_keyword(cur, identifier_token_map().at(word), keyword);
        }
        rest(cur);
        return kind;
    }

    // The statement at `cur` (after its label, if it has one). On return,
    // `cur` points to the next statement.
    void statement(unsigned char *&cur, int64_t stmt_label) {
        if (is_eos(cur) || is_assignment(cur)) {
            if (!is_eos(cur)) unit_start = false;
            rest(cur);
            return;
        }
        // The name of a construct: `name: do`
        {
            const unsigned char *p = skip_name(cur);
            if (p > cur && is_name_start(*cur) && *p == ':' && p[1] != ':') {
                until(cur, p + 1);
            }
        }
        if (unit_start) {
            char kind = unit_statement(cur, true);
            if (kind != 0) {
                units.push_back(kind);
                unit_start = false;
                return;
            }
        }
        unit_start = false;
        for (const Entry &e : statement_keywords(*cur)) {
            if (!next_is(cur, e.text.c_str())) continue;
            unsigned char next = cur[e.text.size()];
            if (e.next == '$' ? !is_eos(cur + e.text.size())
                    : (e.next != 0 && next != e.next)) {
                continue;
            }
            if (e.mode == Do && !(is_eos(cur + 2) || is_digit(next)
                    || next == ',' || next_is(cur + 2, "while(")
                    || next_is(cur + 2, "concurrent(")
                    || *skip_name(cur + 2) == '=')) {
                continue;
            }
            if (e.mode == Format) {
                unsigned char *first = cur;
                unsigned char *start;
                Location content;
                cur += 6;
                lex_format(cur, content, start, diag, false, string_start);
                YYSTYPE y;
                y.string.p = (char*) start;
                y.string.n = cur - start - 1;
                push(TK_FORMAT, y, first, cur - 1);
                rest(cur);
                return;
            }
            if (e.mode == EndIf && stmt_label != -1) {
                // The label is the one of a `continue` statement
                push_token(cur, KW_CONTINUE, "continue", 8);
                push_newline(cur);
            } else if (e.mode == EndUnit) {
                if (stmt_label != -1 && !units.empty()
                        && (units.back() == 's' || units.back() == 'f')) {
                    push_token(cur, KW_CONTINUE, "continue", 8);
                    push_newline(cur);
                }
                if (!units.empty()) units.pop_back();
                unit_start = true;
            }
            for (const auto &word : e.words) {
                push_keyword(cur, word.first, word.second);
            }
            switch (e.mode) {
                case Type: {
                    star_length(cur);
                    break;
                }
                case Do: {
                    unsigned char *end = cur;
                    while (is_digit(*end)) end++;
                    if (end > cur) {
                        // The label: only digits, `do10e1=1,5` is not a real
                        push_integer(TK_INTEGER, cur, end, end);
                        cur = end;
                    }
                    if (next_is(cur, "while(")) {
                        push_keyword(cur, KW_WHILE, 5);
                    } else if (next_is(cur, "concurrent(")) {
                        push_keyword(cur, KW_CONCURRENT, 10);
                    }
                    break;
                }
                case Condition: {
                    if (!parentheses(cur)) return;
                    if (is_name_start(*cur)) {
                        // `then`, or the statement that the condition
                        // controls
                        statement(cur, -1);
                        return;
                    }
                    break;
                }
                case Assign: {
                    unsigned char *end = cur;
                    while (is_digit(*end)) end++;
                    if (end > cur && next_is(end, "to")) {
                        until(cur, end);
                        push_keyword(cur, KW_TO, 2);
                    }
                    break;
                }
                case Contains:
                case Interface: {
                    unit_start = true;
                    break;
                }
                default: {
                    break;
                }
            }
            break;
        }
        rest(cur);
    }

    // The next statement, with its label
    void labeled_statement(unsigned char *&cur) {
        int64_t l = label(cur);
        statement(cur, l);
    }

    void tokenize(unsigned char *&cur) {
        while (*cur != '\0') {
            unsigned char *start = cur;
            labeled_statement(cur);
            if (cur == start) rest(cur);
        }
        push_token(cur, END_OF_FILE, "EOF", 3);
    }

};

bool FixedFormTokenizer::tokenize_input(diag::Diagnostics &diagnostics, Allocator &al, bool /*continue_compilation*/) {
    try {
        FixedFormStatements f(diagnostics, al);
        f.string_start = string_start;
        f.t.cur = string_start;
        f.t.string_start = string_start;
        f.t.cur_line = string_start;
        f.t.line_num = 1;
        f.tokenize(cur);
        tokens = std::move(f.tokens);
        stypes = std::move(f.stypes);
        locations = std::move(f.locations);
        LCOMPILERS_ASSERT(tokens.size() == stypes.size())
        LCOMPILERS_ASSERT(tokens.size() == locations.size())
        tokenized = true;
    } catch (const parser_local::TokenizerAbort &) {
        return false;
    }
    return true;
}

int FixedFormTokenizer::lex(Allocator &/*al*/, YYSTYPE &yylval,
        Location &loc, diag::Diagnostics &/*diagnostics*/, bool /*continue_compilation*/)
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


} // namespace LCompilers::LFortran
