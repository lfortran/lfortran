#include <limits>

#include <lfortran/parser/parser_exception.h>
#include <lfortran/parser/fixedform_tokenizer.h>
#include <lfortran/parser/parser.tab.hh>
#include <lfortran/parser/tokenizer.h>
#include <libasr/bigint.h>

int position = 0;
#define TOK(pos, arg) std::cout << std::string(pos, ' ') << "TOKEN: " << arg << std::endl;

namespace LFortran
{

void FixedFormTokenizer::set_string(const std::string &str)
{
    // The input string must be NULL terminated, otherwise the tokenizer will
    // not detect the end of string. After C++11, the std::string is guaranteed
    // to end with \0, but we check this here just in case.
    LFORTRAN_ASSERT(str[str.size()] == '\0');
    cur = (unsigned char *)(&str[0]);
    string_start = cur;
    cur_line = cur;
    line_num = 1;
}

#define KW(x) RET(KW_##x);
//#define KW(x) token(yylval.string); RET(KW_##x);
#define RET(x) token_loc(loc); last_token=yytokentype::x; return yytokentype::x;
#define WARN_REL(x) add_rel_warning(diagnostics, yytokentype::TK_##x);

struct FixedFormRecursiveDescent {
    Tokenizer t;
    diag::Diagnostics &diag;
    Allocator &m_a;
    unsigned char *string_start;

    FixedFormRecursiveDescent(diag::Diagnostics &diag,
        Allocator &m_a) : diag{diag}, m_a{m_a} {};

    // Auxiliary functions:

    void error(unsigned char *cur, const std::string &text) {
        uint32_t loc_first = cur-string_start;
        Location loc;
        loc.first = loc_first;
        loc.last = loc_first;
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

    bool next_is_advance(unsigned char *cur, const std::string &str) {
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
        if (next_str == str) cur = cur2;
        return next_str == str;
    }

    

    bool is_integer(const std::string &s) const {
        return !s.empty() && std::all_of(s.begin(), s.end(), [](char c) {
            return ::isdigit(c) || c == ' ';
        });
    }

    // TODO figure out if we can reuse existing infrastructure for numbers
    // from free-form tokenizer
    // bool is_decimal(const std::string &s) const {
    // }

    // placeholder for the occasions:
    // - multiline if statement
    // - multiline argument list for subroutine / function / function call
    void parentheses_wrap_line(unsigned char *cur) {

    }

    bool eat_label(unsigned char *&cur) {
        // consume label if it is available
        // for line beginnings
        std::string label;
        label.assign((char*)cur, 6);
        if (is_integer(label)) {
            // TOK(position, label);
            cur+=6;
            return true;
        }
        return false;
    }

    bool eat_label_inline(unsigned char *&cur) {
        // consume label if it is available
        // for labels
        std::string label;
        unsigned long long count = 0;
        while(*(cur++) != '\n') count++;
        label.assign((char*)cur, count);
        if (is_integer(label)) {
            // TOK(position, label);
            cur+=count;
            return true;
        }
        return false;
    }

    void next_line(unsigned char *&cur) {
        eat_label(cur);
        // TODO check if right time for this call, as next_line is called several times
        // sometimes even if the line is NOT advanced
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

    // extract and return substring that is between cur and c, advance cur by substring.size() characters
    std::string scan_next(unsigned char *&cur, unsigned char c) {
        unsigned char *start = cur;
        long long unsigned count = 0;
        while (*(cur++) != c) count++;
        // TOK(position, c);
        return tostr(start, cur-1);
    }
   
    bool lex_arg(unsigned char *&cur) {
        unsigned char *start = cur;
        long long unsigned count = 0;
        if (*cur == '(') {
            // TOK(position, '(');
            return true;
        }
        // we can easily extract all ',' and ')' and tokenize
        while (*(cur++) != ',') {
            // we have found the end of the argument list
            if (*cur == ')') {
                // TOK(position, ')');
                return false;
            }
            if (*cur == '\0') {
                return false;
            }
            count++;
        }

        // TOK(position, ',');
        
        if (count > 0) {
            // TOK(position, tostr(start, start + count));
        }
        return true;
    }
    // Recursive descent parser with backtracking
    //
    // If a function returns void, then it will always parse the given grammar
    // rule (and `cur` progressed), or give a compiler error (via the `error`
    // function).  If a function returns bool, then it does not raise any
    // errors, and returns true (parsed, `cur` progressed) or false (not-parsed,
    // `cur` unchanged).

    void tokenize_line(const std::string &chop, unsigned char *&cur) {
        // TODO keep a list of weird assignments that duplicate / substring with keywords
        // examples: 'result' used as a variable, 'doubleprecisioninteger' used as a variable
        // GOAL: double check to get the KW_XXX right (ie. the defining category)
        // Q: would we need to this for every line we land upon?

        TOK(position, chop);
        unsigned char *start = cur + std::string(chop).size();
        // move the cur pointer to the next line after
        next_line(cur);     
        // need to have the line string stay alive for function scope
        // adding the newline to make it easy for us to walk through the line
        std::string line{tostr(start, cur)};
        t.set_string(line);
        std::cout << line;
        YYSTYPE y;
        Location l;
        ptrdiff_t len = 1;
        for(;;) {
            if(*t.cur == '\n') break;
            TOK(position,t.lex(m_a, y, l, diag));
            len = t.cur - t.tok;
            TOK(position, tostr(t.tok, t.tok + len));
            // for now, this double check is needed as the usual tokenizer does not
            // like newlines '\n'
            // we have the check for ';' to be able to have multiple expressions per line
            if(*(t.cur+1) == '\n' || *(t.cur + 1) == ';') {
                TOK(position,t.lex(m_a, y, l, diag));
                len = t.cur - t.tok;
                TOK(position, tostr(t.tok, t.tok + len));
                break;
            }
        }
    }

    void lex_subroutine(unsigned char *&cur) {
        tokenize_line("subroutine", cur);
        // TODO check if this is sufficient or if we need an eternal loop to get
        // all possible combinations right
        while (lex_declaration(cur));
        while (lex_body_statement(cur));
        if (next_is(cur, "end")) {
            next_line(cur);
            position -= 2;
            return;
        }
    }

    bool lex_declaration(unsigned char *&cur) {
        unsigned char *start = cur;
        next_line(cur);
        if (
                next_is(start, "integer") ||
                next_is(start, "real") ||
                next_is(start, "complex") ||
                next_is(start, "doubleprecision") ||
                next_is(start, "external") ||
                next_is(start, "dimension") || // problematic case? dimension rg(25) has tokens:
                                               // ...
                                               // TOKEN: rg
                                               // TOKEN: 273
                                               // TOKEN: (
                                               // TOKEN: 261
                                               // TOKEN: 25
                                               // TOKEN: 274
                                               // TOKEN: )

                next_is(start, "character") ||
                next_is(start, "logical")
            ) {
            unsigned char *start2= start;
            lex_declarator(start2);
            // call with empty string as we found out the declarator
            tokenize_line("", start2);
            
            std::cout << "declaration: " << tostr(start, cur-1) << std::endl;
            return true;
        }
        cur = start;
        return false;
    }

    // TODO: put declaration lexers together
    bool lex_declarator(unsigned char *&cur) {
        std::vector<std::string> declarators{
            "integer",
            "real",
            "complex",
            "doubleprecision",
            "external",
            "dimension",
            "character",
            "logical"
        };
        // TODO rewrite
        for(const auto& declarator : declarators) {
            if(next_is(cur, declarator)) {
                cur += declarator.size();
                // TODO find out id of declarator
                TOK(position, declarator);
                return true;
            }
        }
        return false;
    }

    bool lex_declarator_list(unsigned char *&cur) {
        unsigned char *start = cur;
        long long unsigned count = 0;
        for(;;) {
            switch (*cur) {
                case ',':
                    // TOK(position, tostr(start, start + count));
                    // TOK(position, ',')
                    cur++;
                    return true;
                case '\n':
                    // TOK(position, tostr(start, start + count));
                    return false;
            }
            count++;
            cur++;
        }
        return false;
    }


    // TODO: Blocked on these or now. Need better strategy to make progress here.

    // bool is_comp_single_binary(unsigned char c) {
    //     std::vector<unsigned char> comparator_list {
    //         '<', '>'
    //     };
    // }

    // bool is_comp_double_binary(std::string c) {
    //     std::vector<std::string> comparator_list {
    //         "<=", ">=", "=>", "=<" 
    //     };
    // }

    // bool is_comp_quadruple_binary(std::string c) {
    //     std::vector<std::string> comparator_list {
    //         ".le.", ".ge."
    //     };
    // }

    // // helper function for '<=' etc
    // void peek_two(unsigned char *&cur, unsigned char *write) {
    //     write = cur;
    // }

    // // helper function for '.le.'
    // void peek_four(unsigned char *&cur, unsigned char *write) {
    //     write = cur;
    // }

    // void lex_math(unsigned char *&cur) {
    //     unsigned char *start = cur;
    //     unsigned char *cpy = cur;
    //     for(;;) {
    //         if ::isdigit(*cpy) {}
    //         if 


    //         cpy++;
    //         if(*cpy == '\n') break;
    //     }
    // }

    void lex_math_or_expression(unsigned char *&cur) {
        unsigned char *cpy = cur;
        while(*cpy != '\n') {
           cpy++;
        }

    }

    bool lex_body_statement(unsigned char *&cur) {
        unsigned char *start = cur;
        unsigned char *cpy = cur;
        next_line(cur);
        eat_label(start);
        // Handle assignment first, and return TK_NAME if matched,
        // as this identifier can be composed of keywords, so we do not
        // want to return them as keywords.
        // TODO: this must be made more robust:
        // we parse an "id", then optional "(...)", then there must be "="
        // the current implementation parses if(..) a=5 as assignment
        if (contains(start, cur, '=')) {
            // TOK(position, scan_next(cpy, '='));
            std::cout << "body assignment statement: " << tostr(start, cur-1) << std::endl;
            tokenize_line("", start);
            return true;
        }
        cur = start;

        // Now the first word cannot be an identifier, and must be a keyword,
        // so we can use it to figure out what kind of statement we have

        // Next we have to handle multiline statements (and consume their "end")
        // Tokenization: the first word as KW_*, then specific to each case
        if (next_is(cur, "if(")) {
            // lex_if_statement(cur);
            tokenize_line("if", cur);
            return true;
        }
        if (next_is(cur, "call")) {
            tokenize_line("call", cur);
            return true;
        }
        if (next_is(cur, "open") ||
            next_is(cur, "read") ||
            next_is(cur, "write") ||
            next_is(cur, "format") ||
            next_is(cur, "close")) {
            // CHECK: tokenizer _should_ be able to handle it
            tokenize_line("", cur);
            return true;
        }

       if (next_is(cur, "goto")) {
           // TOK(position, "GOTO");
           tokenize_line("goto", cur);
            return true;
       }


        // TODO: add `do`, `where`, etc.
        // TODO: print / read statements
        // Now an "end" must be the end statement for the program/function/etc
        if (next_is(cur, "end")) {
            // not a body statement, return false
            return false;
        }

        // Otherwise it must be a single line statement
        // Tokenization: We use the longest match, either TK_NAME or KW_*,
        // whichever is longer.
        next_line(cur);
        std::cout << "body statement: " << tostr(start, cur-1) << std::endl;

        return true;
    }

    void lex_if_statement(unsigned char *&cur) {
        unsigned char *start = cur;
        // Assume single line if for now
        // TODO: Implement multiline if
        position += 2;
        if (!eat_label(cur)) {
            // TOK(position, "NO LABEL GIVEN IN IF STATEMENT");
        }
        scan_next(cur, '(');
        lex_cond(cur);
        scan_next(cur, ')');
        if (next_is(cur, "goto")) lex_goto(cur);
        
        // lex_condition(cur);        eat_label(cur);
        next_line(cur);
        // // TOK(position, tostr(start, cur-1));
        // std::cout << "body if statement: " << tostr(start, cur-1) << std::endl;
        position -= 2;
    }

    void lex_goto(unsigned char *&cur, bool newline = true) {
        // flag for in-line statements ie. for IF  statements
        cur += std::string("goto").size();
        // TOK(position, "GOTO");
        eat_label(cur);
        if (newline)
            next_line(cur);
    }

    void lex_cond(unsigned char *&cur) {
        // recursive -- split along precedence: 1) logical 2) mathematical 3) symbolical
        // lex_math(cur);
        // lex_logic_or_none(cur);
        std::cout << "NOT IMPLEMENTED \n";

    }

    void lex_condition(unsigned char *&cur) {
        scan_next(cur, '(');
        while (*cur != ')')
        {
            // needs LOGICAL expression (not C-like values)
            cur++;
        }
    }

    void lex_call_statement(unsigned char *&cur) {
        unsigned char *start = cur;
        // TOK(position, "call");
        start += std::string("call").size();
        // TOK(position, scan_next(start, '('));
        // TOK(position, '(');
        while(lex_arg(start));
        next_line(cur);
    }

    void lex_data(unsigned char *&cur) {
        std::cout << "data\n";
        next_line(cur);
    }

    void lex_io(unsigned char *&cur) {
        std::cout << "io\n";
        next_line(cur);
    }

    void lex_function(unsigned char *&cur) {
        unsigned char *start=cur;
        scan_next(cur, '(');
        while(lex_arg(cur));
        next_line(cur);
        std::cout << "function: " << tostr(start, cur-1) << std::endl;
        while (lex_declaration(cur));
        while (lex_body_statement(cur));
        if (next_is(cur, "end")) {
            next_line(cur);
        } else {
            //std::cout << "?: " << tostr(cur, cur+5) << std::endl;
            error(cur, "end of function expected");
        }
    }

    void lex_program(unsigned char *&cur) {
        // TOK(position, "program");

        next_line(cur);
        while (lex_declaration(cur));
        while (lex_body_statement(cur));
        if (next_is(cur, "end")) {
            next_line(cur);
        } else {
            //std::cout << "?: " << tostr(cur, cur+5) << std::endl;
            error(cur, "end of program expected");
        }
    }

    void lex_global_scope_item(unsigned char *&cur) {
        if (next_is(cur, "subroutine")) {
            lex_subroutine(cur);
        } else if (next_is(cur, "program")) {
            lex_program(cur);
        } else if (
                next_is(cur, "function") ||
                next_is(cur, "integer") ||
                next_is(cur, "real") ||
                next_is(cur, "complex")) {
            lex_function(cur);
        } else if (next_is(cur, "blockdata")) {
            lex_data(cur);
        } else if (next_is(cur, "print")) {
            lex_io(cur);
        } else if (next_is(cur, "if")){
            lex_if_statement(cur);
        } else if (next_is(cur, "goto")) {
            lex_goto(cur);
        } else {
            error(cur, "Cannot recognize the global scope entity");
        }
    }

    void lex_global_scope(unsigned char *&cur) {
        while (*cur != '\0') {
            eat_label(cur);
            lex_global_scope_item(cur);
        }
    }

};

bool FixedFormTokenizer::tokenize_input(diag::Diagnostics &diagnostics) {
    // We use a recursive descent parser.  We are starting at the global scope
    try {
        Allocator al(1024);
        FixedFormRecursiveDescent f(diagnostics, al);
        f.string_start = string_start;
        f.lex_global_scope(cur);
    } catch (const parser_local::TokenizerError &e) {
        diagnostics.diagnostics.push_back(e.d);
        return false;
    }
    return true;
}

int FixedFormTokenizer::lex(Allocator &/*al*/, YYSTYPE &/*yylval*/,
        Location &/*loc*/, diag::Diagnostics &/*diagnostics*/)
{
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
