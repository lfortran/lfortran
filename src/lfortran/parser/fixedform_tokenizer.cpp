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

std::map<std::string, int> identifiers_map = {
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
        auto next=cur; next_line(next);
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

    bool is_integer(const std::string &s) const {
        return !s.empty() && std::all_of(s.begin(), s.end(), [](char c) {
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
            // y.n = std::stoi(label);
            tokens.push_back(yytokentype::TK_LABEL);
            stypes.push_back(y);
            Location loc;
            loc.first = cur - string_start;
            loc.last = cur - string_start + label.size();
            locations.push_back(loc);


            // int token = yytokentype::TK_LABEL;
            // tokens.push_back(token);
            cur+=reserved_cols;
            return y.int_suffix.int_n.n;
        }
        return -1;
    }

    int64_t eat_label_inline(unsigned char *&cur) {
        // consume label if it is available
        auto start = cur;
        auto cur2 = cur;
        std::string label;
        unsigned long long count = 0;
        if (::isdigit(*cur2)) {
            while(*cur2 == ' ' || ::isdigit(*(cur2++))) count++;
            cur2--;//count--; // we advance one too much
        }
        label.assign((char*)start, count);
        if (is_integer(label) && count > 0) {
            YYSTYPE yy;
            lex_int_large(m_a, cur, cur2,
                    yy.int_suffix.int_n,
                    yy.int_suffix.int_kind);
            tokens.push_back(yytokentype::TK_INTEGER);
            
            // yy.n = std::stoi(tostr(cur, cur2));
            // tokens.push_back(yytokentype::TK_LABEL);
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

    // Recursive descent parser with backtracking
    //
    // If a function returns void, then it will always parse the given grammar
    // rule (and `cur` progressed), or give a compiler error (via the `error`
    // function).  If a function returns bool, then it does not raise any
    // errors, and returns true (parsed, `cur` progressed) or false (not-parsed,
    // `cur` unchanged).


    /*
     * The line must start with `chop` which is returned as a keyword
     * (not an identifier).
     * Then the rest of the line is tokenized, including the new line.
     */
    void tokenize_line(const std::string &chop, unsigned char *&cur) {
        YYSTYPE y1;
        if (chop != "") {
            std::string l(chop); 
            y1.string.from_str(m_a, l);
            stypes.push_back(y1);
            if (chop == "enddo") {
                tokens.push_back(KW_END_DO);
            } else {
                tokens.push_back(identifiers_map[chop]);
            }
            Location loc;
            loc.first = cur - string_start;
            loc.last = cur - string_start + chop.size();
            locations.push_back(loc);
        }
        unsigned char *start = cur + chop.size();
        // move the cur pointer to the next line after
        next_line(cur);
        if (start >= cur) {
            Location loc;
            token_loc(cur, cur, loc);
            throw LFortran::parser_local::TokenizerError("ICE: chop longer than a line: '" + chop + "'",
                loc);
        }

        std::string line{tostr(start, cur)};
        lines.push_back(line);
        t.cur = start;


        Location loc;
        ptrdiff_t len = 1;
        for(;;) {
            YYSTYPE y2;
            if(*t.cur == '\n') {
                y2.string.from_str(m_a, "\n");
                stypes.push_back(y2);
                tokens.push_back(yytokentype::TK_NEWLINE);
                Location loc;
                loc.first = t.cur - t.string_start;
                loc.last = t.cur - t.string_start + 1;
                locations.push_back(loc);
                break;
            }
            auto token = t.lex(m_a, y2, loc, diag);
            // we need to disentangle "goto999" as the tokenizer cannot do it
            // on its own
            if (next_is(t.tok, "goto") && token != yytokentype::KW_GOTO) {

                std::string l("goto");
                y2.string.from_str(m_a, l);
                stypes.push_back(y2);
                tokens.push_back(yytokentype::KW_GOTO);
                Location loc;
                loc.first = t.tok - string_start;
                loc.last = t.tok - string_start + l.size();
                locations.push_back(loc);

                YYSTYPE y3;
                // TODO check if we actually just didn't get the arguments right (t.tok+4, t.cur)
                lex_int_large(m_a, t.tok + 4,t.cur,
                    y3.int_suffix.int_n,
                    y3.int_suffix.int_kind);
                tokens.push_back(yytokentype::TK_INTEGER);
                
                // y3.n = std::stoi(tostr(t.tok+4,t.cur));
                // tokens.push_back(yytokentype::TK_LABEL);
                
                stypes.push_back(y3);
                loc.first = t.tok+4 - t.string_start;
                loc.last = t.cur - t.string_start-1;
                locations.push_back(loc);

                
                continue;
            }
            // TODO: handle
            // We should have most types right by now; we have to check if
            // the types in the parser match and then adapt to that.


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
    }

    bool lex_declaration(unsigned char *&cur) {
        unsigned char *start = cur;
        next_line(cur);
        if (lex_declarator(start)) {
            tokenize_line("", start);
            return true;
        }
        cur = start;
        return false;
    }

    bool lex_declarator(unsigned char *&cur) {
        for(const auto& declarator : declarators) {
            if(next_is(cur, declarator)) {
                tokens.push_back(identifiers_map[declarator]);
                YYSTYPE y;
                std::string decl(declarator);
                y.string.from_str(m_a, decl);
                stypes.push_back(y);
                Location loc;
                loc.first = t.cur - string_start;
                loc.last = t.cur - string_start + declarator.size();
                locations.push_back(loc);
                cur += declarator.size();
                return true;
            }
        }
        return false;
    }

    bool lex_io(unsigned char *&cur) {
        for(const auto &io_str: io_names) {
            if (next_is(cur, io_str)) {
                tokenize_line(io_str, cur);
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

    bool lex_body_statement(unsigned char *&cur) {
        eat_label(cur);
        // if (has_terminal(cur)) return false;     
        if (lex_declaration(cur)) {
            return true;
        }
        if (lex_io(cur)) return true;
        if (next_is(cur, "if(")) {
            lex_if(cur);
            return true;
        }
        unsigned char *nline = cur; next_line(nline);
        if (next_is(cur, "do") && contains(cur, nline, '=') && contains(cur, nline, '=')) {
            lex_do(cur);
            return true;
        }
        // assignment
        if (contains(cur, nline, '=')) {
            tokenize_line("", cur);
            return true;
        }

        if (next_is(cur, "goto")) {
            tokenize_line("", cur);
            return true;
        }

        /*
         * explicitly DO NOT tokenize `CONTINUE`, `GO TO`
         */

        if (next_is(cur, "call") && !contains(cur, nline, '=')) {
            tokenize_line("call", cur);
            return true;
        }

        if (next_is(cur, "return")) {
            tokenize_line("return", cur);
            return true;
        }

        return false;
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
            }
        }
        if (next_is(cur, "enddo")) {
            tokenize_line("enddo", cur);
            return true;
        } else if (next_is(cur, "continue")) {
            // the usual terminal statement for do loops
            tokenize_line("continue", cur);
            //only append iff (tokens[tokens.size()-2] == yytokentype::TK_LABEL && tokens[tokens.size()-1 == yytokentype::KW_CONTINUE])

            // return an explicit "end do" token here
            std::string l("enddo");
            YYSTYPE y2;
            y2.string.from_str(m_a, l);
            stypes.push_back(y2);
            tokens.push_back(yytokentype::KW_END_DO);
            Location loc;
            loc.first = t.cur - string_start;
            loc.last = t.cur - string_start + l.size();
            locations.push_back(loc);
            // And a new line
            l = "\n";
            y2.string.from_str(m_a, l);
            stypes.push_back(y2);
            tokens.push_back(yytokentype::TK_NEWLINE);
            loc.first = t.cur - string_start;
            loc.last = t.cur - string_start + 1;
            locations.push_back(loc);
            return true;
        } else if (label_match) {
            lex_body_statement(cur);

            // return an explicit "end do" token here
            std::string l("enddo");
            YYSTYPE y2;
            y2.string.from_str(m_a, l);
            stypes.push_back(y2);
            tokens.push_back(yytokentype::KW_END_DO);
            Location loc;
            loc.first = t.cur - string_start;
            loc.last = t.cur - string_start + l.size();
            locations.push_back(loc);
            // And a new line
            l = "\n";
            y2.string.from_str(m_a, l);
            stypes.push_back(y2);
            tokens.push_back(yytokentype::TK_NEWLINE);
            loc.first = t.cur - string_start;
            loc.last = t.cur - string_start + 1;
            locations.push_back(loc);
            return true;
        } else {
            return false;
        }
    }


    void lex_do(unsigned char *&cur) {
        auto end = cur; next_line(end);
        YYSTYPE yy;
        std::string l{"do"};
        lines.push_back(l);
        yy.string.from_str(m_a, l);
        stypes.push_back(yy);
        tokens.push_back(identifiers_map[l]);
        Location loc;
        loc.first = cur - string_start;
        loc.last = cur - string_start + l.size();
        locations.push_back(loc);
        cur += l.size();
        int64_t do_label = eat_label_inline(cur);
        if (do_label != -1) {
            cur--; // un-advance as eat_label_inline moves 1 char too far when making checks
        }
        tokenize_line("", cur); // tokenize rest of line where `do` starts
        while (!lex_do_terminal(cur, do_label)) {
            if (!lex_body_statement(cur)) {
                Location loc;
                loc.first = 1;
                loc.last = 1;
                throw parser_local::TokenizerError("End of file inside a do loop 2", loc);
            };
        }
    }

    bool if_advance_or_terminate(unsigned char *&cur) {
        eat_label(cur);
        if (next_is(cur, "elseif")) {
            tokenize_line("elseif", cur);
            return true;
        }
        if (next_is(cur, "else")) {
            tokenize_line("else", cur);
            return true;
        }
        // TODO: check for other if terminals
        if (next_is(cur, "endif")) {
            tokenize_line("endif", cur);
            return false;
        }
        if (next_is(cur, "continue")) {
            tokenize_line("continue", cur);
            return false;
        }
        lex_body_statement(cur);
        return true;
    }

    void lex_if(unsigned char *&cur) {
        tokenize_line("if", cur);
        // check if it's a single line if statement
        // take the second-to-last as we MAYBE tokens = {..., "THEN", "newline"}
        if (tokens[tokens.size()-2] != yytokentype::KW_THEN) return;
        while(if_advance_or_terminate(cur));
    }

    void lex_subroutine(unsigned char *&cur) {
        while(lex_body_statement(cur));
        eat_label(cur);
        if (next_is(cur, "endsubroutine")) {
            tokenize_line("endsubroutine", cur);
        } else if (next_is(cur, "end")) {
            tokenize_line("end", cur);
        } else {
            error(cur, "Expecting terminating symbol for subroutine");
        }
    }

    void lex_program(unsigned char *&cur, bool explicit_program) {
        if (explicit_program) tokenize_line("program", cur);
        while(lex_body_statement(cur));
        eat_label(cur);
        if (next_is(cur, "endprogram")) {
            tokenize_line("endprogram", cur);
        } else if (next_is(cur, "end")) {
            tokenize_line("end", cur);
        } else {
            error(cur, "Expecting terminating symbol for program");
        }
    }

    void lex_function(unsigned char *&cur) {
        while(lex_body_statement(cur));
        eat_label(cur);

        if (next_is(cur, "endfunction")) {
            tokenize_line("endfunction", cur);
        } else if (next_is(cur, "end")) {
            tokenize_line("end", cur);
        } else {
            error(cur, "Expecting terminating symbol for function");
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
            tokens.push_back(identifiers_map[*iter]);
            YYSTYPE y;
            std::string decl(*iter);
            y.string.from_str(m_a, decl);
            stypes.push_back(y);
            Location loc;
            // TODO: refine the location here
            loc.first = cur - string_start;
            loc.last = cur - string_start + decl.size();
            locations.push_back(loc);
        }

        cur = cpy;
        tokenize_line(declaration_type, cur);
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
            "doubleprecision"};

        if (next_is(cur, "include")) tokenize_line("include", cur);
        if (is_program(cur)) {
            lex_program(cur, true);
        } else if (is_declaration(cur, "subroutine", subroutine_keywords)) {
            lex_subroutine(cur);
        } else if (is_declaration(cur, "function", function_keywords)) {
            lex_function(cur);
        /* TODO
        }  else if (is_declaration(cur, "blockdata", blockdata_keywords)) {
            lex_block_data(cur);
        } 
        */
        } else if (is_implicit_program(cur)) {
            std::string prog{"program"};
            // TODO -- mangling
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
            y.string.from_str(m_a, "\n");
            stypes.push_back(y);
            tokens.push_back(yytokentype::TK_NEWLINE);
            loc.first = cur - string_start + prog.size() + name.size();
            loc.last = cur - string_start + prog.size() + name.size() + 1;
            locations.push_back(loc);
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
