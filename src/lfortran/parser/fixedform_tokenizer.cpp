#include <limits>
#include <utility>

#include <lfortran/parser/parser_exception.h>
#include <lfortran/parser/fixedform_tokenizer.h>
#include <lfortran/parser/parser.tab.hh>
#include <lfortran/parser/tokenizer.h>
#include <libasr/bigint.h>

int position = 0;
#define TOK(pos, arg) std::cout << std::string(pos, ' ') << "TOKEN: " << arg << std::endl;

namespace LFortran
{

std::map<std::string, int> identifiers_map = {
    {"end of file", 0},
    {"newline", 258},
    {"\n" ,258},
    {"name",259},
{"def_op",260},
{"tk_integer",261},
{"tk_label",262},
{"tk_real",263},
{"boz_constant",264},
{"plus",265},
{"minus",266},
{"star",267},
{"slash",268},
{"colon",269},
{"semicolon",270},
{"comma",271},
{"equal",272},
{"lparen",273},
{"rparen",274},
{"lbracket",275},
{"rbracket",276},
{"rbracket_old",277},
{"percent",278},
{"vbar",279},
{"string",280},
{"comment",281},
{"eolcomment",282},
{":",283},
{";;",284},
{"pow",285},
{"concat",286},
{"arrow",287},
{"eq",288},
{"ne",289},
{"lt",290},
{"le",291},
{"gt",292},
{"ge",293},
{"not",294},
{"and",295},
{"or",296},
{"xor",297},
{"eqv",298},
{"neqv",299},
{"true",300},
{"false",301},
{"format",302},
{"abstract",303},
{"all",304},
{"allocatable",305},
{"allocate",306},
{"assign",307},
{"assignment",308},
{"associate",309},
{"asynchronous",310},
{"backspace",311},
{"bind",312},
{"block",313},
{"call",314},
{"case",315},
{"change",316},
{"change_team",317},
{"character",318},
{"class",319},
{"close",320},
{"codimension",321},
{"common",322},
{"complex",323},
{"concurrent",324},
{"contains",325},
{"contiguous",326},
{"continue",327},
{"critical",328},
{"cycle",329},
{"data",330},
{"deallocate",331},
{"default",332},
{"deferred",333},
{"dimension",334},
{"do",335},
{"dowhile",336},
{"double",337},
{"doubleprecision",338},
{"elemental",339},
{"else",340},
{"elseif",341},
{"elsewhere",342},
{"end",343},
{"end_program",344},
{"endprogram",345},
{"end_module",346},
{"endmodule",347},
{"end_submodule",348},
{"endsubmodule",349},
{"end_block",350},
{"endblock",351},
{"end_block_data",352},
{"endblockdata",353},
{"end_subroutine",354},
{"endsubroutine",355},
{"end_function",356},
{"endfunction",357},
{"end_procedure",358},
{"endprocedure",359},
{"end_enum",360},
{"endenum",361},
{"end_select",362},
{"endselect",363},
{"end_if",364},
{"endif",365},
{"end_interface",366},
{"endinterface",367},
{"end_type",368},
{"endtype",369},
{"end_associate",370},
{"endassociate",371},
{"end_forall",372},
{"endforall",373},
{"end_do",374},
{"enddo",375},
{"end_where",376},
{"endwhere",377},
{"end_critical",378},
{"endcritical",379},
{"end_file",380},
{"endfile",381},
{"end_team",382},
{"endteam",383},
{"entry",384},
{"enum",385},
{"enumerator",386},
{"equivalence",387},
{"errmsg",388},
{"error",389},
{"event",390},
{"exit",391},
{"extends",392},
{"external",393},
{"file",394},
{"final",395},
{"flush",396},
{"forall",397},
{"formatted",398},
{"form",399},
{"form_team",400},
{"function",401},
{"generic",402},
{"go",403},
{"goto",404},
{"if",405},
{"images",406},
{"implicit",407},
{"import",408},
{"impure",409},
{"in",410},
{"include",411},
{"inout",412},
{"in_out",413},
{"inquire",414},
{"integer",415},  // TODO we fixed form tokenizer to get this to do KW_INTEGER
{"intent",416},
{"interface",417},
{"intrinsic",418},
{"is",419},
{"kind",420},
{"len",421},
{"local",422},
{"local_init",423},
{"logical",424},
{"memory",425},
{"module",426},
{"mold",427},
{"name",428},
{"namelist",429},
{"new_index",430},
{"nopass",431},
{"non_intrinsic",432},
{"non_overridable",433},
{"non_recursive",434},
{"none",435},
{"nullify",436},
{"only",437},
{"open",438},
{"operator",439},
{"optional",440},
{"out",441},
{"parameter",442},
{"pass",443},
{"pointer",444},
{"post",445},
{"precision",446},
{"print",447},
{"private",448},
{"procedure",449},
{"program",450},
{"protected",451},
{"public",452},
{"pure",453},
{"quiet",454},
{"rank",455},
{"read",456},
{"real",457},
{"recursive",458},
{"reduce",459},
{"result",460},
{"return",461},
{"rewind",462},
{"save",463},
{"select",464},
{"select_case",465},
{"select_rank",466},
{"select_type",467},
{"sequence",468},
{"shared",469},
{"source",470},
{"stat",471},
{"stop",472},
{"submodule",473},
{"subroutine",474},
{"sync",475},
{"sync_all",476},
{"sync_images",477},
{"sync_memory",478},
{"sync_team",479},
{"target",480},
{"team",481},
{"team_number",482},
{"then",483},
{"to",484},
{"type",485},
{"unformatted",486},
{"use",487},
{"value",488},
{"volatile",489},
{"wait",490},
{"where",491},
{"while",492},
{"write",493},
{"uminus",494}
};

std::vector<std::string> declarators{
            "integerfunction",
            "realfunction",
            "complexfunction",
            "doubleprecisionfunction",
            "externalfunction",
            "bytefunction",
            "logicalfunction",
            "integer",
            "real",
            "complex",
            "doubleprecision",
            "external",
            "dimension",
            "character",
            "logical",
            "bytes"
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

    FixedFormRecursiveDescent(diag::Diagnostics &diag,
        Allocator &m_a) : diag{diag}, m_a{m_a} {};

    // Auxiliary functions:

    void error(unsigned char *cur, const std::string &text) {
        uint32_t loc_first = cur-string_start;
        Location loc;
        loc.first = loc_first;
        loc.last = loc_first;
        std::cout << cur << std::endl;
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
        // std::cout << "next is " << next_str << std::endl;
        return next_str == str;
    }

    bool is_integer(const std::string &s) const {
        return !s.empty() && std::all_of(s.begin(), s.end(), [](char c) {
            return ::isdigit(c) || c == ' ';
        });
    }

    bool eat_label(unsigned char *&cur) {
        // consume label if it is available
        // for line beginnings
        const int reserved_cols = 6;
        std::string label;
        label.assign((char*)cur, reserved_cols);
        if (is_integer(label)) {
            lines.push_back(label);
            YYSTYPE y;
            int token = 262; // TK_LABEL
            tokens.push_back(token);
            std::string::iterator end = std::remove(label.begin(), label.end(), ' ');
            label.erase(end, label.end());
            y.string.from_str(m_a, label);
            stypes.push_back(y);
            cur+=reserved_cols;
            return true;
        }
        return false;
    }

    bool eat_label_inline(unsigned char *&cur) {
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
            yy.string.from_str(m_a, label);
            stypes.push_back(yy);
            tokens.push_back(262);
            cur+=count+1; // TODO revisit
            return true;
        }
        return false;
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


    void tokenize_line(const std::string &chop, unsigned char *&cur) {
        YYSTYPE y1;
        if (chop != "") {
            std::string l(chop); // copy
            // lines.push_back(l);
            // yy.string.from_str_view(l);
            // y1.string.n = l.size();
            // y1.string.p = &l[0];
            y1.string.from_str(m_a, l);
            stypes.push_back(y1);
            tokens.push_back(identifiers_map[chop]);
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
        t.set_string(lines[lines.size()-1]);

        Location l;
        ptrdiff_t len = 1;
        for(;;) {
            YYSTYPE y2;
            if(*t.cur == '\n') {
                y2.string.from_str(m_a, "\n");
                stypes.push_back(y2);
                tokens.push_back(yytokentype::TK_NEWLINE);
                break;
            }
            auto token = t.lex(m_a, y2, l, diag);
            // we need to disentangle "goto999" as the tokenizer cannot do it
            // on its own
            if (next_is(t.tok, "goto") && token != 404) {
                int token_label = 262;
                int token_goto = 404;
                std::string l("goto"); // copy
                // lines.push_back(l);
                // y2.string.n = l.size();
                // y2.string.p = &l[0];
                y2.string.from_str(m_a, l);
                stypes.push_back(y2);
                tokens.push_back(token_goto);

                YYSTYPE y3;
                std::string ll{tostr(t.tok+4, t.cur)};
                // lines.push_back(ll);
                // y3.string.n = ll.size();
                // y3.string.p = &ll[0];
                y3.string.from_str(m_a, ll);
                stypes.push_back(y3);
                tokens.push_back(token_label);
                continue;
            }
            // TODO: handle
            //        - integer
            //        - bigint
            //        - float / decimal
            //        - other types (bytes?)

            len = t.cur - t.tok;

            tokens.push_back(token);
            // lines.push_back(tk);
            // y2.string.n = tk.size();
            // y2.string.p = &tk[0];
            if (token == yytokentype::TK_INTEGER) {
                lex_int_large(m_a, t.tok, t.cur,
                    y2.int_suffix.int_n,
                    y2.int_suffix.int_kind);
            } else {
                std::string tk{tostr(t.tok, t.tok + len)};
                y2.string.from_str(m_a, tk);
            }

            stypes.push_back(y2);
            // for now, this double check is needed as the usual tokenizer does not
            // like newlines '\n'
            // we have the check for ';' to be able to have multiple expressions per line
            /*
            if(*(t.cur+1) == '\n' || *(t.cur + 1) == ';') {
                token = t.lex(m_a, y2, l, diag);
                len = t.cur - t.tok;
                tokens.push_back(token);
                std::string tkk{tostr(t.tok, t.tok + len)};
                // lines.push_back(tkk);
                // y2.string.n = tkk.size();
                // y2.string.p = &tkk[0];
                y2.string.from_str(m_a, tkk);
                stypes.push_back(y2);
                break;
            }
            */
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

    bool lex_body_statement(unsigned char *&cur) {
        eat_label(cur);
        if (lex_declaration(cur)) return true;
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
        // assignment or some such
        if (contains(cur, nline, '=')) {
            tokenize_line("", cur);
            return true;
        }

        if (next_is(cur, "goto")) {
            tokenize_line("", cur);
            return true;
        }

        /*
         * explicitly DO NOT tokenize `CONTINUE`
         */

        if (next_is(cur, "call")) {
            tokenize_line("call", cur);
            return true;
        }

        return false;
    }

    bool find_terminal(unsigned char *&cur) {
        eat_label(cur);
        if (next_is(cur, "enddo")) {
            tokenize_line("enddo", cur);
            return true;
        } else if (next_is(cur, "continue")) {
            tokenize_line("continue", cur);
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
        cur += l.size();
        // bool has_label = false; -- do loops can have labels, we check below
        if (eat_label_inline(cur)) {
            // has_label = true;
            cur--; // un-advance
            // eat_label_inline pushes label to internal data structures on success
        }
        tokenize_line("", cur); // tokenize rest of line where `do` starts
        while (lex_body_statement(cur));
        if (!find_terminal(cur)) {
            error(cur, "Expecting termination symbol for do loop");
        }
    }

    bool if_advance_or_terminate(unsigned char *&cur) {
        eat_label(cur);
        if (next_is(cur, "elseif")) {
            tokenize_line("elseif", cur);
            return true;
        }
        if (next_is(cur, "else")) {
            tokenize_line("elseif", cur);
            return true;
        }
        // do we actually need a terminal symbol for if statements?
        // in F77, this is fuzzily defined
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
        if (tokens[tokens.size()-1] != 483) return; // single line if statement
        while(if_advance_or_terminate(cur));
    }

    void lex_subroutine(unsigned char *&cur) {
        tokenize_line("subroutine", cur);
        while(lex_body_statement(cur));
        eat_label(cur);
        if (next_is(cur, "return")) {
            tokenize_line("", cur);
            return;
        }
        if (next_is(cur, "endsubroutine")) {
            tokenize_line("endsubroutine", cur);
        } else if (next_is(cur, "end")) {
            tokenize_line("end", cur);
        } else {
            error(cur, "Expecting terminating symbol for subroutine");
        }
    }

    void lex_program(unsigned char *&cur) {
        tokenize_line("program", cur);
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
        tokenize_line("function", cur);
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

    void lex_global_scope_item(unsigned char *&cur) {
        // we can define a global assignment
        unsigned char *nline = cur; next_line(nline);
        if (lex_declaration(cur)) return;
        if (next_is(cur, "subroutine")) {
            lex_subroutine(cur);
        } else if (next_is(cur, "program")) {
            lex_program(cur);
        } else if (
                next_is(cur, "function") ||
                next_is(cur, "integerfunction") ||
                next_is(cur, "realfunction") ||
                next_is(cur, "complexfunction") ||
                next_is(cur, "doubleprecisionfunction") ||
                next_is(cur, "externalfunction")) {
            lex_function(cur);
        }
        // check for condition "do VAR=start,end"
        else if (next_is(cur, "do") && contains(cur, nline, '=') && contains(cur, nline, ',')) {
            lex_do(cur);
        } else if (!next_is(cur, "if") && contains(cur, nline, '=')) {
            tokenize_line("", cur);
        } else if (next_is(cur, "if(")) {
            lex_if(cur);
        } else if (lex_io(cur)) {

        } else if (next_is(cur, "call")) {
            tokenize_line("call", cur);
        } else if (next_is(cur, "include")) {
            tokenize_line("", cur);
        } else if (next_is(cur, "end")) {
            tokenize_line("end", cur);
            return;
        }

          // these are making it difficult to maintain context within the specific lex_* functions
          else if (next_is(cur, "goto")) {tokenize_line("", cur);}
          else if (next_is(cur, "continue")) {tokenize_line("", cur);}


        else {
            error(cur, "Cannot recognize the global scope entity");
        }
    }

    void lex_global_scope(unsigned char *&cur) {
        while (*cur != '\0') {
            eat_label(cur);
            lex_global_scope_item(cur);
        }
        YYSTYPE y2;
        y2.string.from_str(m_a, "EOF");
        stypes.push_back(y2);
        tokens.push_back(yytokentype::END_OF_FILE);
    }

};

bool FixedFormTokenizer::tokenize_input(diag::Diagnostics &diagnostics, Allocator &al) {
    // We use a recursive descent parser.  We are starting at the global scope
    try {
        FixedFormRecursiveDescent f(diagnostics, al);
        f.string_start = string_start;
        f.lex_global_scope(cur);
        tokens = std::move(f.tokens);
        stypes = std::move(f.stypes);
        tokenized = true;
    } catch (const parser_local::TokenizerError &e) {
        diagnostics.diagnostics.push_back(e.d);
        return false;
    }
    return true;
}

int FixedFormTokenizer::lex(Allocator &/*al*/, YYSTYPE &yylval,
        Location &/*loc*/, diag::Diagnostics &/*diagnostics*/)
{
    if (!tokens.empty()) {
        auto tok = tokens[0];
        tokens.erase(tokens.begin());
        yylval = stypes.at(0);
        stypes.erase(stypes.begin());
        return tok;
    }
    return yytokentype::END_OF_FILE;
    for (;;) {
        tok = cur;

        /*
        Re2c has excellent documentation at:

        https://re2c.org/manual/manual_c.html

        The first paragraph there explains the basics:

        * If multiple rules match, the longest match takes precedence
        * If multiple rules match the same string, the earlier rule takes
          precedence
        * Default rule `*` should always be defined, it has the lowest priority
          regardless of its place and matches any code unit
        * We use the "Sentinel character" method for end of input:
            * The end of the input text is denoted with a null character \x00
            * Thus the null character cannot be part of the input otherwise
            * There is one rule to match \x00 to end the parser
            * No other rule is allowed to match \x00, otherwise the re2c block
              would parse past the end of the string and segfaults
            * A special case of the previous point are negated character
              ranges, such as [^"\x00], where one must include \x00 in it to
              ensure this rule does not match \x00 (all other rules simply do
              not mention \x00)
            * See the "Handling the end of input" section in the re2c
              documentation for more info

        The re2c block interacts with the rest of the code via just one pointer
        variable `cur`. On entering the re2c block, the `cur` variable must
        point to the first character of the token to be tokenized by the block.
        The re2c block below then executes on its own until a rule is matched:
        the action in {} is then executed. In that action `cur` points to the
        first character of the next token.

        Before the re2c block we save the current `cur` into `tok`, so that we
        can use `tok` and `cur` in the action in {} to extract the token that
        corresponds to the rule that got matched:

        * `tok` points to the first character of the token
        * `cur-1` points to the last character of the token
        * `cur` points to the first character of the next token
        * `cur-tok` is the length of the token

        In the action, we do one of:

        * call `continue` which executes another cycle in the for loop (which
          will parse the next token); we use this to skip a token
        * call `return` which returns from this function; we return a token
        * throw an exception (terminates the tokenizer)

        In the first two cases, `cur` points to first character of the next
        token, which becomes `tok` at the next iteration of the loop (either
        right away after `continue` or after the `lex` function is called again
        after `return`).

        See the manual for more details.
        */


        // These two variables are needed by the re2c block below internally,
        // initialization is not needed. One can think of them as local
        // variables of the re2c block.
//        unsigned char *mar, *ctxmar;
        /*!re2c
            re2c:define:YYCURSOR = cur;
            re2c:define:YYMARKER = mar;
            re2c:define:YYCTXMARKER = ctxmar;
            re2c:yyfill:enable = 0;
            re2c:define:YYCTYPE = "unsigned char";

            end = "\x00";
            whitespace = [ \t\v\r]+;
            newline = "\n";
            digit = [0-9];
            char =  [a-zA-Z_];
            name = char (char | digit)*;
            defop = "."[a-zA-Z]+".";
            kind = digit+ | name;
            significand = (digit+"."digit*) | ("."digit+);
            exp = [edED][-+]? digit+;
            integer = digit+ ("_" kind)?;
            real = ((significand exp?) | (digit+ exp)) ("_" kind)?;
            string1 = (kind "_")? '"' ('""'|[^"\x00])* '"';
            string2 = (kind "_")? "'" ("''"|[^'\x00])* "'";
            comment = "!" [^\n\x00]*;
            ws_comment = whitespace? comment? newline;

            * { token_loc(loc);
                std::string t = token();
                throw parser_local::TokenizerError(diag::Diagnostic(
                    "Token '" + t + "' is not recognized",
                    diag::Level::Error, diag::Stage::Tokenizer, {
                        diag::Label("token not recognized", {loc})
                    })
                );
            }
            end { RET(END_OF_FILE); }
            whitespace { continue; }

            // Keywords
            'abstract' { KW(ABSTRACT) }
            'all' { KW(ALL) }
            'allocatable' { KW(ALLOCATABLE) }
            'allocate' { KW(ALLOCATE) }
            'assign' { KW(ASSIGN) }
            'assignment' { KW(ASSIGNMENT) }
            'associate' { KW(ASSOCIATE) }
            'asynchronous' { KW(ASYNCHRONOUS) }
            'backspace' { KW(BACKSPACE) }
            'bind' { KW(BIND) }
            'block' { KW(BLOCK) }
            'call' { KW(CALL) }
            'case' { KW(CASE) }
            'change' { KW(CHANGE) }
            'changeteam' { KW(CHANGE_TEAM) }
            'character' { KW(CHARACTER) }
            'class' { KW(CLASS) }
            'close' { KW(CLOSE) }
            'codimension' { KW(CODIMENSION) }
            'common' { KW(COMMON) }
            'complex' { KW(COMPLEX) }
            'concurrent' { KW(CONCURRENT) }
            'contains' { KW(CONTAINS) }
            'contiguous' { KW(CONTIGUOUS) }
            'continue' { KW(CONTINUE) }
            'critical' { KW(CRITICAL) }
            'cycle' { KW(CYCLE) }
            'data' { KW(DATA) }
            'deallocate' { KW(DEALLOCATE) }
            'default' { KW(DEFAULT) }
            'deferred' { KW(DEFERRED) }
            'dimension' { KW(DIMENSION) }
            'do' / (whitespace digit+) {
                // This is a label do statement, we have to match the
                // corresponding continue base "end do".
                uint64_t n = parse_int(cur);
                enddo_label_stack.push_back(n);
                KW(DO);
            }
            'do' { KW(DO) }
            'dowhile' { KW(DOWHILE) }
            'double' { KW(DOUBLE) }
            'doubleprecision' { KW(DOUBLE_PRECISION) }
            'elemental' { KW(ELEMENTAL) }
            'else' { KW(ELSE) }
            'elseif' { KW(ELSEIF) }
            'elsewhere' { KW(ELSEWHERE) }

            'end' { KW(END) }

            'end' whitespace 'program' { KW(END_PROGRAM) }
            'endprogram' { KW(ENDPROGRAM) }

            'end' whitespace 'module' { KW(END_MODULE) }
            'endmodule' { KW(ENDMODULE) }

            'end' whitespace 'submodule' { KW(END_SUBMODULE) }
            'endsubmodule' { KW(ENDSUBMODULE) }

            'end' whitespace 'block' { KW(END_BLOCK) }
            'endblock' { KW(ENDBLOCK) }

            'end' whitespace 'block' whitespace 'data' { KW(END_BLOCK_DATA) }
            'endblock' whitespace 'data' { KW(END_BLOCK_DATA) }
            'end' whitespace 'blockdata' { KW(END_BLOCK_DATA) }
            'endblockdata' { KW(ENDBLOCKDATA) }

            'end' whitespace 'subroutine' { KW(END_SUBROUTINE) }
            'endsubroutine' { KW(ENDSUBROUTINE) }

            'end' whitespace 'function' { KW(END_FUNCTION) }
            'endfunction' { KW(ENDFUNCTION) }

            'end' whitespace 'procedure' { KW(END_PROCEDURE) }
            'endprocedure' { KW(ENDPROCEDURE) }

            'end' whitespace 'enum' { KW(END_ENUM) }
            'endenum' { KW(ENDENUM) }

            'end' whitespace 'select' { KW(END_SELECT) }
            'endselect' { KW(ENDSELECT) }

            'end' whitespace 'associate' { KW(END_ASSOCIATE) }
            'endassociate' { KW(ENDASSOCIATE) }

            'end' whitespace 'critical' { KW(END_CRITICAL) }
            'endcritical' { KW(ENDCRITICAL) }

            'end' whitespace 'team' { KW(END_TEAM) }
            'endteam' { KW(ENDTEAM) }

            'end' whitespace 'forall' { KW(END_FORALL) }
            'endforall' { KW(ENDFORALL) }

            'end' whitespace 'if' { KW(END_IF) }
            'endif' { KW(ENDIF) }

            'end' whitespace 'interface' { KW(END_INTERFACE) }
            'endinterface' { KW(ENDINTERFACE) }

            'end' whitespace 'type' { KW(END_TYPE) }
            'endtype' { KW(ENDTYPE) }

            'end' whitespace 'do' {
                if (enddo_newline_process) {
                    KW(CONTINUE)
                } else {
                    KW(END_DO)
                }
            }
            'enddo' {
                if (enddo_newline_process) {
                    KW(CONTINUE)
                } else {
                    KW(ENDDO)
                }
            }

            'end' whitespace 'where' { KW(END_WHERE) }
            'endwhere' { KW(ENDWHERE) }

            'end file' { KW(END_FILE) }
            'endfile' { KW(ENDFILE) }

            'entry' { KW(ENTRY) }
            'enum' { KW(ENUM) }
            'enumerator' { KW(ENUMERATOR) }
            'equivalence' { KW(EQUIVALENCE) }
            'errmsg' { KW(ERRMSG) }
            'error' { KW(ERROR) }
            'event' { KW(EVENT) }
            'exit' { KW(EXIT) }
            'extends' { KW(EXTENDS) }
            'external' { KW(EXTERNAL) }
            'file' { KW(FILE) }
            'final' { KW(FINAL) }
            'flush' { KW(FLUSH) }
            'forall' { KW(FORALL) }
            'format' {
                if (last_token == yytokentype::TK_LABEL) {
                    unsigned char *start;
                    lex_format(cur, loc, start);
                    yylval.string.p = (char*) start;
                    yylval.string.n = cur-start-1;
                    RET(TK_FORMAT)
                } else {
                    token(yylval.string);
                    RET(TK_NAME)
                }
            }
            'formatted' { KW(FORMATTED) }
            'form' { KW(FORM) }
            'formteam' { KW(FORM_TEAM) }
            'function' { KW(FUNCTION) }
            'generic' { KW(GENERIC) }
            'go' { KW(GO) }
            'goto' { KW(GOTO) }
            'if' { KW(IF) }
            'images' { KW(IMAGES) }
            'implicit' { KW(IMPLICIT) }
            'import' { KW(IMPORT) }
            'impure' { KW(IMPURE) }
            'in' { KW(IN) }
            'include' { KW(INCLUDE) }
            'inout' { KW(INOUT) }
            'in' whitespace 'out' { KW(IN_OUT) }
            'inquire' { KW(INQUIRE) }
            'integer' { KW(INTEGER) }
            'intent' { KW(INTENT) }
            'interface' { KW(INTERFACE) }
            'intrinsic' { KW(INTRINSIC) }
            'is' { KW(IS) }
            'kind' { KW(KIND) }
            'len' { KW(LEN) }
            'local' { KW(LOCAL) }
            'local_init' { KW(LOCAL_INIT) }
            'logical' { KW(LOGICAL) }
            'memory' { KW(MEMORY) }
            'module' { KW(MODULE) }
            'mold' { KW(MOLD) }
            'name' { KW(NAME) }
            'namelist' { KW(NAMELIST) }
            'new_index' { KW(NEW_INDEX) }
            'nopass' { KW(NOPASS) }
            'non_intrinsic' { KW(NON_INTRINSIC) }
            'non_overridable' { KW(NON_OVERRIDABLE) }
            'non_recursive' { KW(NON_RECURSIVE) }
            'none' { KW(NONE) }
            'nullify' { KW(NULLIFY) }
            'only' { KW(ONLY) }
            'open' { KW(OPEN) }
            'operator' { KW(OPERATOR) }
            'optional' { KW(OPTIONAL) }
            'out' { KW(OUT) }
            'parameter' { KW(PARAMETER) }
            'pass' { KW(PASS) }
            'pointer' { KW(POINTER) }
            'post' { KW(POST) }
            'precision' { KW(PRECISION) }
            'print' { KW(PRINT) }
            'private' { KW(PRIVATE) }
            'procedure' { KW(PROCEDURE) }
            'program' { KW(PROGRAM) }
            'protected' { KW(PROTECTED) }
            'public' { KW(PUBLIC) }
            'pure' { KW(PURE) }
            'quiet' { KW(QUIET) }
            'rank' { KW(RANK) }
            'read' { KW(READ) }
            'real' {KW(REAL) }
            'recursive' { KW(RECURSIVE) }
            'reduce' { KW(REDUCE) }
            'result' { KW(RESULT) }
            'return' { KW(RETURN) }
            'rewind' { KW(REWIND) }
            'save' { KW(SAVE) }
            'select' { KW(SELECT) }
            'selectcase' { KW(SELECT_CASE) }
            'selectrank' { KW(SELECT_RANK) }
            'selecttype' { KW(SELECT_TYPE) }
            'sequence' { KW(SEQUENCE) }
            'shared' { KW(SHARED) }
            'source' { KW(SOURCE) }
            'stat' { KW(STAT) }
            'stop' { KW(STOP) }
            'submodule' { KW(SUBMODULE) }
            'subroutine' { KW(SUBROUTINE) }
            'sync' { KW(SYNC) }
            'syncall' { KW(SYNC_ALL) }
            'syncimages' { KW(SYNC_IMAGES) }
            'syncmemory' { KW(SYNC_MEMORY) }
            'syncteam' { KW(SYNC_TEAM) }
            'target' { KW(TARGET) }
            'team' { KW(TEAM) }
            'team_number' { KW(TEAM_NUMBER) }
            'then' { KW(THEN) }
            'to' { KW(TO) }
            'type' { KW(TYPE) }
            'unformatted' { KW(UNFORMATTED) }
            'use' { KW(USE) }
            'value' { KW(VALUE) }
            'volatile' { KW(VOLATILE) }
            'wait' { KW(WAIT) }
            'where' { KW(WHERE) }
            'while' { KW(WHILE) }
            'write' { KW(WRITE) }

            // Tokens
            newline {
                if (enddo_newline_process) {
                    enddo_newline_process = false;
                    enddo_state = 1;
                    return yytokentype::TK_NEWLINE;
                } else {
                    enddo_newline_process = false;
                    enddo_insert_count = 0;
                    token_loc(loc); line_num++; cur_line=cur;
                    last_token = yytokentype::TK_NEWLINE;
                    return yytokentype::TK_NEWLINE;
                }
            }

            // Single character symbols
            "(" { RET(TK_LPAREN) }
            "(" / "/=" { RET(TK_LPAREN) } // To parse "operator(/=)" correctly
            "(" / "/)" { RET(TK_LPAREN) } // To parse "operator(/)" correctly
            // To parse "operator(/ )" correctly
            "(" / "/" whitespace ")" { RET(TK_LPAREN) }
            // To parse "operator(// )" correctly
            "(" / "//" whitespace ")" { RET(TK_LPAREN) }
            "(" / "//)" { RET(TK_LPAREN) } // To parse "operator(//)" correctly
            ")" { RET(TK_RPAREN) }
            "[" | "(/" { RET(TK_LBRACKET) }
            "]" { RET(TK_RBRACKET) }
            "/)" { RET(TK_RBRACKET_OLD) }
            "+" { RET(TK_PLUS) }
            "-" { RET(TK_MINUS) }
            "=" { RET(TK_EQUAL) }
            ":" { RET(TK_COLON) }
            ";" { RET(TK_SEMICOLON) }
            "/" { RET(TK_SLASH) }
            "%" { RET(TK_PERCENT) }
            "," { RET(TK_COMMA) }
            "*" { RET(TK_STAR) }
            "|" { RET(TK_VBAR) }

            // Multiple character symbols
            ".." { RET(TK_DBL_DOT) }
            "::" { RET(TK_DBL_COLON) }
            "**" { RET(TK_POW) }
            "//" { RET(TK_CONCAT) }
            "=>" { RET(TK_ARROW) }

            // Relational operators
            "=="   { RET(TK_EQ) }
            '.eq.' { WARN_REL(EQ) RET(TK_EQ) }

            "/="   { RET(TK_NE) }
            '.ne.' { WARN_REL(NE) RET(TK_NE) }

            "<"    { RET(TK_LT) }
            '.lt.' { WARN_REL(LT) RET(TK_LT) }

            "<="   { RET(TK_LE) }
            '.le.' { WARN_REL(LE) RET(TK_LE) }

            ">"    { RET(TK_GT) }
            '.gt.' { WARN_REL(GT) RET(TK_GT) }

            ">="   { RET(TK_GE) }
            '.ge.' { WARN_REL(GE) RET(TK_GE) }


            // Logical operators
            '.not.'  { RET(TK_NOT) }
            '.and.'  { RET(TK_AND) }
            '.or.'   { RET(TK_OR) }
            '.xor.'  { RET(TK_XOR) }
            '.eqv.'  { RET(TK_EQV) }
            '.neqv.' { RET(TK_NEQV) }

            // True/False

            '.true.' ("_" kind)? { RET(TK_TRUE) }
            '.false.' ("_" kind)? { RET(TK_FALSE) }

            // This is needed to ensure that 2.op.3 gets tokenized as
            // TK_INTEGER(2), TK_DEFOP(.op.), TK_INTEGER(3), and not
            // TK_REAL(2.), TK_NAME(op), TK_REAL(.3). The `.op.` can be a
            // built-in or custom defined operator, such as: `.eq.`, `.not.`,
            // or `.custom.`.
            integer / defop {
                lex_int_large(al, tok, cur,
                    yylval.int_suffix.int_n,
                    yylval.int_suffix.int_kind);
                RET(TK_INTEGER)
            }


            real { token(yylval.string); RET(TK_REAL) }
            integer / (whitespace name) {
                if (last_token == yytokentype::TK_NEWLINE) {
                    uint64_t u;
                    if (lex_int(tok, cur, u, yylval.int_suffix.int_kind)) {
                            yylval.n = u;
                            if (enddo_label_stack[enddo_label_stack.size()-1] == u) {
                                while (enddo_label_stack[enddo_label_stack.size()-1] == u) {
                                    enddo_label_stack.pop_back();
                                    enddo_insert_count++;
                                }
                                enddo_newline_process = true;
                            } else {
                                enddo_newline_process = false;
                            }
                            RET(TK_LABEL)
                    } else {
                        token_loc(loc);
                        std::string t = token();
                        throw LFortran::parser_local::TokenizerError("Integer '" + t + "' too large",
                            loc);
                    }
                } else {
                    lex_int_large(al, tok, cur,
                        yylval.int_suffix.int_n,
                        yylval.int_suffix.int_kind);
                    RET(TK_INTEGER)
                }
            }
            integer {
                lex_int_large(al, tok, cur,
                    yylval.int_suffix.int_n,
                    yylval.int_suffix.int_kind);
                RET(TK_INTEGER)
            }

            [bB] '"' [01]+ '"' { token(yylval.string); RET(TK_BOZ_CONSTANT) }
            [bB] "'" [01]+ "'" { token(yylval.string); RET(TK_BOZ_CONSTANT) }
            [oO] '"' [0-7]+ '"' { token(yylval.string); RET(TK_BOZ_CONSTANT) }
            [oO] "'" [0-7]+ "'" { token(yylval.string); RET(TK_BOZ_CONSTANT) }
            [zZ] '"' [0-9a-fA-F]+ '"' { token(yylval.string); RET(TK_BOZ_CONSTANT) }
            [zZ] "'" [0-9a-fA-F]+ "'" { token(yylval.string); RET(TK_BOZ_CONSTANT) }

            "&" ws_comment+ whitespace? "&"? {
                line_num++; cur_line=cur; continue;
            }

            comment newline {
                line_num++; cur_line=cur;
                token(yylval.string);
                yylval.string.n--;
                token_loc(loc);
                if (last_token == yytokentype::TK_NEWLINE) {
                    return yytokentype::TK_COMMENT;
                } else {
                    last_token=yytokentype::TK_NEWLINE;
                    return yytokentype::TK_EOLCOMMENT;
                }
            }

            // Macros are ignored for now:
            "#" [^\n\x00]* newline { line_num++; cur_line=cur; continue; }

            // Include statements are ignored for now
            'include' whitespace string1 { continue; }
            'include' whitespace string2 { continue; }

            string1 { token_str(yylval.string); RET(TK_STRING) }
            string2 { token_str(yylval.string); RET(TK_STRING) }

            defop { token(yylval.string); RET(TK_DEF_OP) }
            name { token(yylval.string); RET(TK_NAME) }
        */
    }
}


} // namespace LFortran
