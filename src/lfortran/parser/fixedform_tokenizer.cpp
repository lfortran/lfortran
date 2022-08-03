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
        auto next=cur; next_line(next); std::cout << "error line " << tostr(cur,next-1) << std::endl;
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

    bool eat_label(unsigned char *&cur) {
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


            // int token = yytokentype::TK_LABEL;
            // tokens.push_back(token);
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
            lex_int_large(m_a, cur, cur2,
                    yy.int_suffix.int_n,
                    yy.int_suffix.int_kind);
            tokens.push_back(yytokentype::TK_INTEGER);
            
            // yy.n = std::stoi(tostr(cur, cur2));
            // tokens.push_back(yytokentype::TK_LABEL);
            stypes.push_back(yy);
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
            std::string l(chop); 
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
            if (next_is(t.tok, "goto") && token != yytokentype::KW_GOTO) {

                std::string l("goto");
                y2.string.from_str(m_a, l);
                stypes.push_back(y2);
                tokens.push_back(yytokentype::KW_GOTO);

                YYSTYPE y3;
                // TODO check if we actually just didn't get the arguments right (t.tok+4, t.cur)
                lex_int_large(m_a, t.tok + 4,t.cur,
                    y3.int_suffix.int_n,
                    y3.int_suffix.int_kind);
                tokens.push_back(yytokentype::TK_INTEGER);
                
                // y3.n = std::stoi(tostr(t.tok+4,t.cur));
                // tokens.push_back(yytokentype::TK_LABEL);
                
                stypes.push_back(y3);

                
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

        return false;
    }

    bool find_terminal(unsigned char *&cur) {
        // TODO: check that this label is the same label as the do loop label
        eat_label(cur);
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
            tokens.push_back(yytokentype::KW_ENDDO);
            // And a new line
            l = "\n";
            y2.string.from_str(m_a, l);
            stypes.push_back(y2);
            tokens.push_back(yytokentype::TK_NEWLINE);
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
        if (eat_label_inline(cur)) {
            cur--; // un-advance as eat_label_inline moves 1 char too far when making checks
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
        if (next_is(cur, "return")) {
            tokenize_line("", cur);
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
        if (next_is(cur, "return")) {
            tokenize_line("return", cur);
        }

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
        }

        cur = cpy;
        tokenize_line(declaration_type, cur);
        return true;
    }

    bool add_implicit_program(unsigned char *&cur) {
        auto cpy = cur;
        auto prev = cpy;
        for (;;) {
            next_line(cpy);
            if (*cpy == '\0') break;
            prev = cpy;
        }
        if (next_is(prev, "endprogram\n") || next_is(prev, "end\n")) {
            std::string prog{"program"};
            // TODO -- mangling
            std::string name{"implicit_program_lfortran"};
            // add implicit global program at the line `cur` is currently at
            YYSTYPE y;
            y.string.from_str(m_a, prog);
            stypes.push_back(y);
            tokens.push_back(yytokentype::KW_PROGRAM);
            y.string.from_str(m_a, name);
            stypes.push_back(y);
            tokens.push_back(yytokentype::TK_NAME);
            y.string.from_str(m_a, "\n");
            stypes.push_back(y);
            tokens.push_back(yytokentype::TK_NEWLINE);
            lex_program(cur);
            return true;
        }
        return false;
    }


    void lex_global_scope_item(unsigned char *&cur) {
        // we can define a global assignment
        unsigned char *nline = cur; next_line(nline);
        // eat_label(cur);
        std::vector<std::string> program_keywords{};
        std::vector<std::string> subroutine_keywords{"recursive"};
        std::vector<std::string> function_keywords{"recursive", "result", "character", "complex", "integer", "doubleprecision", "external"};

        if (next_is(cur, "include")) tokenize_line("include", cur);
        if (is_declaration(cur, "program", program_keywords)) {
            lex_program(cur);
        } else if (is_declaration(cur, "subroutine", subroutine_keywords)) {
            lex_subroutine(cur);
        } else if (is_declaration(cur, "function", function_keywords)) {
            lex_function(cur);
        }
        /* TODO
          else if (is_declaration(cur, "blockdata", blockdata_keywords)) {
            lex_block_data(cur);
        } 
        */ else if (add_implicit_program(cur)) {
            // give compiler a chance for implicitly defined programs
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
    }

};

bool FixedFormTokenizer::tokenize_input(diag::Diagnostics &diagnostics, Allocator &al) {
    // We use a recursive descent parser.  We are starting at the global scope
    try {
        FixedFormRecursiveDescent f(diagnostics, al);
        f.string_start = string_start;
        // std::cout << "fixed form tokenizer sees \n" << f.string_start;
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
        auto tok = tokens[token_pos];
        // tokens.erase(tokens.begin());
        yylval = stypes.at(token_pos++);
        // stypes.erase(stypes.begin());
        return tok;
    }
    return yytokentype::END_OF_FILE;
}


} // namespace LFortran
