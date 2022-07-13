#include <limits>

#include <lfortran/parser/parser_exception.h>
#include <lfortran/parser/fixedform_tokenizer.h>
#include <lfortran/parser/parser.tab.hh>
#include <libasr/bigint.h>

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

#define KW(x) token(yylval.string); RET(KW_##x);
#define RET(x) token_loc(loc); last_token=yytokentype::x; return yytokentype::x;
#define WARN_REL(x) add_rel_warning(diagnostics, yytokentype::TK_##x);

struct FixedFormRecursiveDescent {

    unsigned char *string_start;

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
        return next_str == str;
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


    // Recursive descent parser with backtracking
    //
    // If a function returns void, then it will always parse the given grammar
    // rule (and `cur` progressed), or give a compiler error (via the `error`
    // function).  If a function returns bool, then it does not raise any
    // errors, and returns true (parsed, `cur` progressed) or false (not-parsed,
    // `cur` unchanged).

    void lex_subroutine(unsigned char *&cur) {
        unsigned char *start=cur;
        next_line(cur);
        std::cout << "subroutine: " << tostr(start, cur-1) << std::endl;
        while (lex_declaration(cur));
        while (lex_body_statement(cur));
        if (next_is(cur, "end")) {
            next_line(cur);
        } else {
            //std::cout << "?: " << tostr(cur, cur+5) << std::endl;
            error(cur, "end of subroutine expected");
        }
    }

    bool lex_declaration(unsigned char *&cur) {
        unsigned char *start = cur;
        next_line(cur);
        if (contains(start, cur, '=')) {
            cur = start;
            return false;
        }
        if (
                next_is(start, "integer") ||
                next_is(start, "real") ||
                next_is(start, "complex") ||
                next_is(start, "doubleprecision") ||
                next_is(start, "external") ||
                next_is(start, "dimension")
            ) {
            std::cout << "declaration: " << tostr(start, cur-1) << std::endl;
            return true;
        }
        cur = start;
        return false;
    }

    bool lex_body_statement(unsigned char *&cur) {
        unsigned char *start = cur;
        next_line(cur);

        // Handle assignment first, and return TK_NAME if matched,
        // as this identifier can be composed of keywords, so we do not
        // want to return them as keywords.
        // TODO: this must be made more robust:
        // we parse an "id", then optional "(...)", then there must be "="
        // the current implementation parses if(..) a=5 as assignment
        if (contains(start, cur, '=')) {
            std::cout << "body assignment statement: " << tostr(start, cur-1) << std::endl;
            return true;
        }
        cur = start;

        // Now the first word cannot be an identifier, and must be a keyword,
        // so we can use it to figure out what kind of statement we have

        // Next we have to handle multiline statements (and consume their "end")
        // Tokenization: the first word as KW_*, then specific to each case
        if (next_is(cur, "if(")) {
            lex_if_statement(cur);
            return true;
        }
        // TODO: add `do`, `where`, etc.

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
        next_line(cur);
        std::cout << "body if statement: " << tostr(start, cur-1) << std::endl;
    }

    void lex_function(unsigned char *&cur) {
        unsigned char *start=cur;
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
        std::cout << "program" << std::endl;
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
        } else {
            error(cur, "Cannot recognize the global scope entity");
        }
    }

    void lex_global_scope(unsigned char *&cur) {
        while (*cur != '\0') {
            lex_global_scope_item(cur);
        }
    }

};

bool FixedFormTokenizer::tokenize_input(diag::Diagnostics &diagnostics) {
    // We use a recursive descent parser.  We are starting at the global scope
    try {
        FixedFormRecursiveDescent f;
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
