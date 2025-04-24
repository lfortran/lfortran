#include <limits>

#include <lfortran/parser/parser_exception.h>
#include <lfortran/parser/tokenizer.h>
#include <lfortran/parser/parser.tab.hh>
#include <libasr/bigint.h>

using LCompilers::diag::Level;
using LCompilers::diag::Stage;
using LCompilers::diag::Label;
using LCompilers::diag::Diagnostic;

namespace LCompilers::LFortran {
unsigned char *string_start_pointer = nullptr;

void Tokenizer::set_string(const std::string &str)
{
    // The input string must be NULL terminated, otherwise the tokenizer will
    // not detect the end of string. After C++11, the std::string is guaranteed
    // to end with \0, but we check this here just in case.
    LCOMPILERS_ASSERT(str[str.size()] == '\0');
    cur = (unsigned char *)(&str[0]);
    string_start = cur;
    cur_line = cur;
    line_num = 1;
}

template<int base>
bool adddgt(uint64_t &u, uint64_t d)
{
    if (u > (std::numeric_limits<uint64_t>::max() - d) / base) {
        return false;
    }
    u = u * base + d;
    return true;
}

bool lex_dec(const unsigned char *s, const unsigned char *e, uint64_t &u)
{
    for (u = 0; s < e; ++s) {
        if (!adddgt<10>(u, *s - 0x30u)) {
            return false;
        }
    }
    return true;
}

// Tokenizes integer of the kind 1234_ikind into `u` and `suffix`
// s ... the start of the integer
// e ... the character after the end
bool lex_int(const unsigned char *s, const unsigned char *e, uint64_t &u,
    Str &suffix)
{
    for (u = 0; s < e; ++s) {
        if (*s == '_') {
            s++;
            suffix.p = (char*) s;
            suffix.n = e-s;
            return true;
        } else if (!adddgt<10>(u, *s - 0x30u)) {
            return false;
        }
    }
    suffix.p = nullptr;
    suffix.n = 0;
    return true;
}

void lex_int_large(Allocator &al, const unsigned char *s,
    const unsigned char *e, BigInt::BigInt &u, Str &suffix)
{
    uint64_t ui;
    if (lex_int(s, e, ui, suffix)) {
        if (ui <= BigInt::MAX_SMALL_INT) {
            u.from_smallint(ui);
            return;
        }
    }
    const unsigned char *start = s;
    for (; s < e; ++s) {
        if (*s == '_') {
            s++;
            suffix.p = (char*) s;
            suffix.n = e-s;

            Str num;
            num.p = (char*)start;
            num.n = s-start-1;
            u.from_largeint(al, num);
            return;
        }
    }
    suffix.p = nullptr;
    suffix.n = 0;
    Str num;
    num.p = (char*)start;
    num.n = e-start;
    u.from_largeint(al, num);
}

uint64_t parse_int(const unsigned char *s)
{
    while (*s == ' ') s++;
    uint64_t u;
    for (u = 0; ; ++s) {
        if (*s >= '0' && *s <= '9') {
            if (!adddgt<10>(u, *s - 0x30u)) {
                return false;
            }
        } else {
            return u;
        }
    }
}

#define KW(x) token(yylval.string); RET(KW_##x);
#define RET(x) token_loc(loc); last_token=yytokentype::x; return yytokentype::x;
#define WARN_REL(x) add_rel_warning(diagnostics, fixed_form, yytokentype::TK_##x);

#define TK_TRIVIA(X) {                              \
    line_num++; cur_line=cur;                       \
    token(yylval.string);                           \
    token_loc(loc);                                 \
    if (last_token == yytokentype::TK_NEWLINE) {    \
        return yytokentype::X;                      \
    } else {                                        \
        last_token=yytokentype::TK_NEWLINE;         \
        return yytokentype::TK_EOLCOMMENT;          \
    }                                               \
}

void Tokenizer::add_rel_warning(diag::Diagnostics &diagnostics, bool fixed_form, int rel_token) const {
    if (!fixed_form) {
        static const std::map<int, std::pair<std::string, std::string>> m = {
            {yytokentype::TK_EQ, {"==", ".eq."}},
            {yytokentype::TK_NE, {"/=", ".ne."}},
            {yytokentype::TK_LT, {"<",  ".lt."}},
            {yytokentype::TK_GT, {">",  ".gt."}},
            {yytokentype::TK_LE, {"<=", ".le."}},
            {yytokentype::TK_GE, {">=", ".ge."}},
        };
        const std::string rel_new = m.at(rel_token).first;
        const std::string rel_old = m.at(rel_token).second;
        Location loc;
        token_loc(loc);
        diagnostics.tokenizer_style_label(
            "Use '" + rel_new + "' instead of '" + rel_old + "'",
            {loc},
            "help: write this as '" + rel_new + "'");
    }
}

int Tokenizer::lex(Allocator &al, YYSTYPE &yylval, Location &loc, diag::Diagnostics &diagnostics, bool continue_compilation)
{
    if (string_start_pointer == nullptr) {
        string_start_pointer = cur;
    }
    if (enddo_state == 1) {
        enddo_state = 2;
        KW(END_DO)
    } else if (enddo_state == 2) {
        enddo_insert_count--;
        if (enddo_insert_count > 0) {
            enddo_state = 1;
        } else {
            enddo_state = 0;
            token_loc(loc); line_num++; cur_line=cur;
            last_token = yytokentype::TK_NEWLINE;
        }
        return yytokentype::TK_NEWLINE;
    }
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
        unsigned char *mar, *ctxmar;
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
            omp_kw = "!$" [oO][mM][pP];
            omp = omp_kw [^\n\x00]*;
            omp_end = omp_kw whitespace+ [eE][nN][dD] [^\n\x00]*;
            pragma_decl = "!LF$" [^\n\x00]*;
            comment = "!" [^\n\x00]*;
            ws_comment = whitespace? comment? newline;
            ignore_till_newline = [^\n\x00]* newline;

            * { token_loc(loc);
                std::string t = token();
                diagnostics.add(diag::Diagnostic(
                        "Token '" + t + "' is not recognized",
                        diag::Level::Error, diag::Stage::Tokenizer, {
                        diag::Label("token not recognized", {loc})
                    }));

                if(!continue_compilation) {
                    throw parser_local::TokenizerAbort();
                } else {
                    continue;
                }
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
            'doublecomplex' { KW(DOUBLE_COMPLEX) }
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
                    lex_format(cur, loc, start, diagnostics, continue_compilation);
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
            'instantiate' { KW(INSTANTIATE) }
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
            'requirement' { KW(REQUIREMENT) }
            'require' { KW(REQUIRE) }
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
            'template' { KW(TEMPLATE) }
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
            "{" { RET(TK_LBRACE) }
            "}" { RET(TK_RBRACE) }
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
                        diagnostics.add(diag::Diagnostic(
                            "Integer '" + t + "' too large",
                            diag::Level::Error, diag::Stage::Tokenizer, {
                            diag::Label("", {loc})}
                        ));
                        throw parser_local::TokenizerAbort();
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

            omp_end / newline { TK_TRIVIA(TK_OMP_END) }
            omp / newline { TK_TRIVIA(TK_OMP) }
            pragma_decl / newline { TK_TRIVIA(TK_PRAGMA_DECL) }

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

            "#" whitespace? "line" whitespace ignore_till_newline { line_num++; cur_line=cur; continue; }
            "#" whitespace? "ifdef" whitespace ignore_till_newline {
                Location loc; token_loc(loc);
                diagnostics.tokenizer_warning_label(
                    "#ifdef ignored", {loc},
                    "help: use the '--cpp' command line option to preprocess it");
                line_num++; cur_line=cur; continue;
            }
            "#" whitespace? "elif" whitespace ignore_till_newline {
                Location loc; token_loc(loc);
                diagnostics.tokenizer_warning_label(
                    "#elif ignored", {loc},
                    "help: use the '--cpp' command line option to preprocess it");
                line_num++; cur_line=cur; continue;
            }
            "#" whitespace? "else" whitespace? newline {
                Location loc; token_loc(loc);
                diagnostics.tokenizer_warning_label(
                    "#else ignored", {loc},
                    "help: use the '--cpp' command line option to preprocess it");
                line_num++; cur_line=cur; continue;
            }
            "#" whitespace? "endif" whitespace? newline {
                Location loc; token_loc(loc);
                diagnostics.tokenizer_warning_label(
                    "#endif ignored", {loc},
                    "help: use the '--cpp' command line option to preprocess it");
                line_num++; cur_line=cur; continue;
            }
            "#" whitespace? "define" whitespace ignore_till_newline {
                Location loc; token_loc(loc);
                diagnostics.tokenizer_warning_label(
                    "#define ignored", {loc},
                    "help: use the '--cpp' command line option to preprocess it");
                line_num++; cur_line=cur; continue;
            }
            "#" whitespace? "include" whitespace ignore_till_newline {
                Location loc; token_loc(loc);
                diagnostics.tokenizer_warning_label(
                    "#include ignored", {loc},
                    "help: use the '--cpp' command line option to preprocess it");
                line_num++; cur_line=cur; continue;
            }
            // Rest of the Macros are ignored with warning:
            "#" ignore_till_newline {
                Location loc; token_loc(loc);
                diagnostics.tokenizer_warning_label(
                    "Unsupported macro", {loc},
                    "Ignored");
                line_num++; cur_line=cur; continue;
            }

            string1 { token_str(al, yylval.string, '"'); RET(TK_STRING) }
            string2 { token_str(al, yylval.string, '\''); RET(TK_STRING) }

            defop { token(yylval.string); RET(TK_DEF_OP) }
            name { token(yylval.string); RET(TK_NAME) }
        */
    }
}

std::string token(unsigned char *tok, unsigned char* cur)
{
    return std::string((char *)tok, cur - tok);
}

void token_loc(Location &loc)
{
    loc.first = 1;
    loc.last = 1;
}

void lex_format(unsigned char *&cur, Location &loc,
        unsigned char *&start, diag::Diagnostics &diagnostics, bool continue_compilation) {
    int num_paren = 0;
    for (;;) {
        unsigned char *tok = cur;
        unsigned char *mar;
        /*!re2c
            re2c:define:YYCURSOR = cur;
            re2c:define:YYMARKER = mar;
            re2c:yyfill:enable = 0;
            re2c:define:YYCTYPE = "unsigned char";

            int = digit (whitespace? digit)*;
            dot_int = '.' whitespace? int;
            E_int = 'E' whitespace? int;
            data_edit_desc
                = 'I' whitespace? int whitespace? dot_int?
                | 'B' whitespace? int whitespace? dot_int?
                | 'O' whitespace? int whitespace? dot_int?
                | 'Z' whitespace? int whitespace? dot_int?
                | 'F' whitespace? int whitespace? dot_int
                | 'E' whitespace? int whitespace? dot_int whitespace? E_int?
                | 'E' whitespace? 'N' whitespace? int whitespace? dot_int whitespace? E_int?
                | 'E' whitespace? 'S' whitespace? int whitespace? dot_int whitespace? E_int?
                | 'E' whitespace? 'X' whitespace? int whitespace? dot_int whitespace? E_int?
                | 'G' whitespace? int whitespace? (dot_int whitespace? E_int?)?
                | 'L' whitespace? int
                | 'A' whitespace? (int)?
                | 'D' whitespace? int whitespace? dot_int
                | 'P' whitespace? 'E' whitespace? int whitespace? dot_int
                | 'P' whitespace? 'F' whitespace? int whitespace? dot_int
                | 'P'
                | 'X'
                ;

            position_edit_desc
                = 'T' whitespace? ('L' | 'R')? whitespace? int
                | int whitespace? 'X'
                ;
            control_edit_desc
                = position_edit_desc
                | (int)? '/'
                | ':'
                ;

            * {
                token_loc(loc);
                std::string t = token(tok, cur);
                loc.first = cur-string_start_pointer;
                loc.last = cur-string_start_pointer;
                diagnostics.add(diag::Diagnostic(
                    "Token '" + t + "' is not recognized in `format` statement",
                    diag::Level::Error, diag::Stage::Tokenizer, {
                    diag::Label("", {loc})}
                ));
                if(!continue_compilation) {
                    throw parser_local::TokenizerAbort();
                } else {
                    continue;
                }
            }
            '(' {
                if (num_paren == 0) {
                    num_paren++;
                    start = cur;
                    continue;
                } else {
                    cur--;
                    unsigned char *tmp;
                    lex_format(cur, loc, tmp, diagnostics, continue_compilation);
                    continue;
                }
            }
            int whitespace? '(' {
                cur--;
                unsigned char *tmp;
                lex_format(cur, loc, tmp, diagnostics, continue_compilation);
                continue;
            }
            '*' whitespace? '(' {
                cur--;
                unsigned char *tmp;
                lex_format(cur, loc, tmp, diagnostics, continue_compilation);
                continue;
            }
            ')' {
                LCOMPILERS_ASSERT(num_paren == 1);
                return;
            }
            end {
                token_loc(loc);
                std::string t = token(tok, cur);
                diagnostics.add(diag::Diagnostic(
                    "End of file not expected in `format` statement '" + t + "'",
                    diag::Level::Error, diag::Stage::Tokenizer, {
                    diag::Label("", {loc})}
                ));
                if(!continue_compilation) {
                    throw parser_local::TokenizerAbort();
                } else {
                    continue;
                }
            }
            whitespace { continue; }
            ',' { continue; }
            "&" ws_comment+ whitespace? "&"? { continue; }
            '"' ('""'|[^"\x00])* '"' { continue; }
            "'" ("''"|[^'\x00])* "'" { continue; }
            '-' { continue; }
            (int)? whitespace? data_edit_desc { continue; }
            control_edit_desc { continue; }
        */
    }
}

} // namespace LCompilers::LFortran
