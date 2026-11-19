// The LL grammar of Fortran.
//
// llgen.py generates the parser from this file (see llgen.py for the format);
// ll_parser.cpp has the rest of it: the tokens, the predicates, the functions
// declared here as %extern and the functions that the actions call. The
// actions build the AST with the macros of semantics.h.
//
// A keyword is not reserved: every keyword token is also a name (NAME). The
// generator tells the alternatives of a rule apart by the next few tokens;
// where a keyword could also be a name, the alternative with the keyword is
// written first, and a statement is checked for being an assignment
// (&is_assignment) before its first word is taken as a keyword.

// ----------------------------------------------------------------------------
// Lookaheads

%nested "(" "[" : ")" "]" "/)" until TK_NEWLINE END_OF_FILE

// The statement that starts here is an assignment: a name, any number of
// parenthesized lists and `% name` after it, and then `=`, `=>` or `:=`
%lookahead is_assignment
    : NAME ( NESTED | "%" NAME )* ( "=" | "=>" | ":=" )
    ;

// A declaration type starts here: an intrinsic type, or `type(`, `class(`
// or `procedure(`
%lookahead is_var_type_start
    : KW_INTEGER | KW_REAL | KW_COMPLEX | KW_LOGICAL | KW_CHARACTER | KW_BYTE
    | KW_DOUBLE_PRECISION | KW_DOUBLE_COMPLEX
    | KW_DOUBLE ( KW_PRECISION | KW_COMPLEX )
    | ( KW_LIST | KW_SET | KW_DICT | KW_TUPLE | KW_TYPE | KW_CLASS
        | KW_PROCEDURE ) "("
    ;

// After the `[` of an array constructor: `::` comes before the bracket that
// closes it, as in `[integer :: 1, 2]`
%lookahead array_constructor_has_type
    : ( NESTED | ~[ "::" | ")" | "]" | "/)" | TK_NEWLINE ] )* "::"
    ;

// `::` comes before the next `,` or `)` that is not nested, as in the
// `integer :: i = 1:n` of a FORALL header
%lookahead has_dbl_colon
    : ( NESTED | ~[ "::" | "," | ")" | "]" | TK_NEWLINE ] )* "::"
    ;

// ----------------------------------------------------------------------------
// Functions written by hand (ll_parser.cpp)

// A name: TK_NAME or any keyword
%extern id -> ast_t* token: NAME
// One or more separators (new lines, comments, `;`); the list is the trivia
%extern sep -> Vec<ast_t*> first: SEP
// A variable, an array element or section, a component, a function call
%extern designator -> ast_t* first: NAME
// "(" expr ")", a complex constant, an implied do loop or a conditional
// expression
%extern parenthesized -> ast_t* first: "("
// A statement with its label and the separator after it; null if no
// statement starts here
%extern statement -> ast_t*
// CALL statement
%extern call_statement -> ast_t* first: KW_CALL
// The data_set_list of a DATA statement; an object of one set, which can be
// an implied do loop; and the values of one set, which can be repeated
%extern data_sets -> Vec<ast_t*>
%extern data_object -> ast_t*
%extern data_stmt_value_list -> Vec<ast_t*>
// "(" concurrent_control_list [ "," mask ] ")" of FORALL and DO CONCURRENT
%extern concurrent_header -> ConcurrentHeader first: "("
// "," and the specifiers of EVENT WAIT
%extern event_wait_specs -> Vec<ast_t*> first: ","
// The statements of the DO loop with the label of the token `lab`, up to and
// including the statement that has the label
%extern labeled_do_body -> Vec<ast_t*>
// The declarations and statements of a program unit or a BLOCK. With
// --continue-compilation, after a syntax error in the rest of the unit the
// parser goes on with more declarations and statements.
%extern decl_statements -> Vec<ast_t*> resume: more_decl_statements
// A declaration or a statement that has a label
%extern labeled_decl_or_statement -> ast_t* first: TK_LABEL
// A declaration, or else a statement; null if neither starts here
%extern var_decl_or_statement -> ast_t*
// DEFERRED declarations of templates, other than DEFERRED INTERFACE
%extern deferred_decl -> ast_t* first: KW_DEFERRED
// "{" instantiate_symbol_list_opt "}"
%extern instantiate_symbols -> Vec<ast_t*> first: "{"
// The groups of a NAMELIST and the blocks of a COMMON statement
%extern namelist_group_list -> Vec<namelist_group_t>
%extern common_block_list -> Vec<common_block_t>
// CONTAINS and the subprograms after it
%extern contains_block -> Vec<ast_t*> first: KW_CONTAINS
// A subroutine, a function or a module procedure with its prefixes, or else a
// main program without the PROGRAM statement; in interactive mode, a
// declaration, a statement or an expression instead of the main program
%extern subprogram_or_main -> ast_t*

// ----------------------------------------------------------------------------
// Expressions

// The operand of a unary minus or plus is everything that binds tighter than
// they do (just `**`); the operand of `.not.` is a comparison or tighter
%operators expr primary
    left 1 TK_DEF_OP => DEFOP(x, $op, y, @$)
    left 2 ".eqv." => EQV(x, y, @$)
    left 2 ".neqv." => NEQV(x, y, @$)
    left 3 ".or." => OR(x, y, @$)
    left 3 ".xor." => XOR(x, y, @$)
    left 4 ".and." => AND(x, y, @$)
    prefix 6 ".not." => NOT(x, @$)
    left 6 "==" => EQ(x, y, @$)
    left 6 "/=" => NE(x, y, @$)
    left 6 "<" => LT(x, y, @$)
    left 6 "<=" => LE(x, y, @$)
    left 6 ">" => GT(x, y, @$)
    left 6 ">=" => GE(x, y, @$)
    left 7 "//" => STRCONCAT(x, y, @$)
    left 8 "+" => ADD(x, y, @$)
    left 8 "-" => SUB(x, y, @$)
    left 9 "*" => MUL(x, y, @$)
    left 9 "/" => DIV(x, y, @$)
    prefix 11 "-" => UNARY_MINUS(x, @$)
    prefix 11 "+" => UNARY_PLUS(x, @$)
    right 11 "**" => POW(x, y, @$)
    prefix 12 TK_DEF_OP => UNARY_DEFOP($op, x, @$)
%end

rule primary -> ast_t*
    : designator
    | v=TK_INTEGER                  => INTEGER($v, @$)
    | v=TK_REAL                     => REAL($v, @$)
    | v=TK_STRING "(" args=fnarray_arg_list_opt ")"
                                    => SUBSTRING($v, args, @$)
    | v=TK_STRING                   => STRING($v, @$)
    | v=TK_BOZ_CONSTANT             => BOZ($v, @$)
    | v=".true."                    => TRUE($v, @$)
    | v=".false."                   => FALSE($v, @$)
    | parenthesized
    | "["
        ( &array_constructor_has_type
            ( &is_var_type_start vt=var_type "::" list=bracket_list
                                    => ARRAY_IN2(vt, list, @$)
            | n=id "::" list=bracket_list
                                    => ARRAY_IN3(n, list, @$)
            )
        )?
        list=bracket_list           => ARRAY_IN1(list, @$)
    ;

// The values of an array constructor and the bracket that closes it
rule bracket_list -> Vec<ast_t*>
    : ( "]" | "/)" )                => empty_list()
    | list=expr % "," ( "]" | "/)" ) => list
    ;

rule expr_list -> Vec<ast_t*>
    : list=expr % ","               => list
    ;

rule fnarray_arg_list_opt -> Vec<FnArg>
    : &[ ")" ]                      => empty_fnargs()
    | list=fnarray_arg % ","        => list
    ;

// An argument of a function call or an array reference: an expression, a
// section `a:b:c` with any part left out, `name = value` or an alternate
// return `*label`
rule fnarray_arg -> FnArg*
    : n=id "=" e=expr               => ARRAY_COMP_DECL1k(n, e, @$)
    | "*" v=TK_INTEGER              => ARRAY_COMP_DECL_label($v, @$)
    | "::" c=expr                   => ARRAY_COMP_DECL_00c(c, @$)
    | ":" ":" c=expr                => ARRAY_COMP_DECL_00c(c, @$)
    | ":" &[ "," | ")" | "]" ]      => ARRAY_COMP_DECL_001(@$)
    | ":" b=expr
        ( ":" c=expr                => ARRAY_COMP_DECL_0bc(b, c, @$) )?
                                    => ARRAY_COMP_DECL_0b1(b, @$)
    | a=expr
        ( "::" c=expr               => ARRAY_COMP_DECL_a0c(a, c, @$)
        | ":" ":" c=expr            => ARRAY_COMP_DECL_a0c(a, c, @$)
        | ":" &[ "," | ")" | "]" ]  => ARRAY_COMP_DECL_a01(a, @$)
        | ":" b=expr
            ( ":" c=expr            => ARRAY_COMP_DECL_abc(a, b, c, @$) )?
                                    => ARRAY_COMP_DECL_ab1(a, b, @$)
        )?
                                    => ARRAY_COMP_DECL_0i0(a, @$)
    ;

rule coarray_arg_list -> Vec<CoarrayArg>
    : list=coarray_arg % ","        => list
    ;

rule coarray_arg -> CoarrayArg*
    : n=id "=" e=expr               => COARRAY_COMP_DECL1k(n, e, @$)
    | "*"                           => COARRAY_COMP_DECL_star(@$)
    | ":" &[ "," | ")" | "]" ]      => COARRAY_COMP_DECL_001(@$)
    | ":" b=expr                    => COARRAY_COMP_DECL_0b1(b, @$)
    | a=expr
        ( ":" &[ "," | ")" | "]" ]  => COARRAY_COMP_DECL_a01(a, @$)
        | ":" "*"                   => COARRAY_COMP_DECL_astar(a, @$)
        | ":" b=expr                => COARRAY_COMP_DECL_ab1(a, b, @$)
        )?
                                    => COARRAY_COMP_DECL_0i0(a, @$)
    ;

// ----------------------------------------------------------------------------
// Statements

rule assignment_statement -> ast_t*
    : x=designator
        ( "=" y=expr                => ASSIGNMENT(x, y, @$)
        | "=>" y=expr               => ASSOCIATE(x, y, @$)
        | ":=" y=expr               => INFER_ASSIGNMENT(x, y, @$)
        )
    ;

// A statement without its label and the separator after it
rule statement1? -> ast_t*
    : &is_assignment assignment_statement
    | n1=id ":" st=statement1 n2=id => stmt_name(n1, n2, st)
    | if_statement
    | do_statement
    | where_statement
    | forall_statement
    | select_statement
    | KW_PRINT fmt=format
        ( "," &[ SEP ]              => PRINT0(fmt, @$)
        | "," args=expr % ","       => PRINT(fmt, args, @$)
        )?
                                    => PRINT0(fmt, @$)
    | call_statement
    | KW_EXIT n=id                  => EXIT2(n, @$)
    | KW_EXIT                       => EXIT(@$)
    | KW_CYCLE n=id                 => CYCLE2(n, @$)
    | KW_CYCLE                      => CYCLE(@$)
    | KW_CONTINUE                   => CONTINUE(@$)
    | KW_RETURN &[ SEP ]            => RETURN(@$)
    | KW_RETURN e=expr              => RETURN1(e, @$)
    | KW_STOP c=stop_code           => stop_statement(c, @$)
    | KW_ERROR KW_STOP c=stop_code  => error_stop_statement(c, @$)
    | KW_PAUSE &[ SEP ]             => make_Pause_t(m_a, @$, 0, nullptr, nullptr)
    | KW_PAUSE e=expr               => make_Pause_t(m_a, @$, 0, EXPR(e), nullptr)
    | KW_ALLOCATE "(" args=fnarray_arg_list_opt ")"
                                    => ALLOCATE_STMT(args, @$)
    | KW_DEALLOCATE "(" args=fnarray_arg_list_opt ")"
                                    => DEALLOCATE_STMT(args, @$)
    | KW_NULLIFY a=write_args       => NULLIFY(a, @$)
    | KW_OPEN a=write_args          => OPEN(a, @$)
    | KW_CLOSE a=write_args         => CLOSE(a, @$)
    | KW_WRITE a=write_args
        ( &[ SEP ]                  => WRITE0(a, @$)
        | ","? args=expr % ","      => WRITE(a, args, @$)
        )
    | KW_READ a=write_args
        ( &[ SEP ]                  => READ0(a, @$)
        | ","? args=expr % ","      => READ(a, args, @$)
        )
    | KW_READ v=TK_INTEGER "," args=expr % ","
                                    => READ2($v, args, @$)
    | KW_READ v=TK_INTEGER          => READ4($v, @$)
    | KW_READ "*" "," &[ SEP ]      => READ3(empty_list(), @$)
    | KW_READ "*" "," args=expr % "," => READ3(args, @$)
    | KW_READ "*"                   => READ6(@$)
    | KW_READ v=TK_STRING "," args=expr % ","
                                    => READ5($v, args, @$)
    | KW_INQUIRE a=write_args
        ( &[ SEP ]                  => INQUIRE0(a, @$)
        | args=expr % ","           => INQUIRE(a, args, @$)
        )
    | KW_REWIND a=write_args        => REWIND(a, @$)
    | KW_REWIND u=unit_number(@)    => REWIND2(u, @$)
    | KW_BACKSPACE a=write_args     => BACKSPACE(a, @$)
    | KW_BACKSPACE u=unit_number(@) => BACKSPACE2(u, @$)
    | KW_FLUSH a=write_args         => FLUSH(a, @$)
    | KW_FLUSH u=unit_number(@)     => FLUSH2(u, @$)
    | ( KW_END_FILE | KW_ENDFILE ) a=write_args
                                    => ENDFILE(a, @$)
    | ( KW_END_FILE | KW_ENDFILE ) u=unit_number(@)
                                    => ENDFILE2(u, @$)
    | KW_INCLUDE v=TK_STRING        => INCLUDE($v, @$)
    | KW_ASSIGN v=TK_INTEGER KW_TO n=id
                                    => ASSIGN(INTEGER3($v), n, @$)
    | ( KW_GO KW_TO | KW_GOTO )
        ( v=TK_INTEGER              => GOTO($v, @$)
        | "(" labels=expr % "," ")" ","? e=expr
                                    => GOTO1(labels, e, @$)
        | n=id &[ SEP ]             => GOTO2(n, @$)
        | n=id ","? "(" labels=expr % "," ")"
                                    => GOTO3(n, labels, @$)
        )
    | KW_DATA sets=data_sets        => DATASTMT(sets, @$)
    | v=TK_FORMAT                   => FORMAT($v, @$)
    | v=TK_OMP                      => OMP_PRAGMA($v, @$)
    | v=TK_OMP_END                  => OMP_PRAGMA($v, @$)
    | KW_ENTRY n=id args=sub_args
        ( b=bind r=result_clause?   => ENTRY2(n, args, b, r, @$)
        | r=result_clause b=bind?   => ENTRY3(n, args, r, b, @$)
        )?
                                    => ENTRY1(n, args, @$)
    | KW_ASSOCIATE "(" syms=var_sym_decl % "," ")" sp=sep body=statement*
        ( KW_END_ASSOCIATE | KW_ENDASSOCIATE )
        => ASSOCIATE_BLOCK(syms, TRIVIA_AFTER(sp, @$), body, @$)
    | KW_BLOCK sp=sep decls=decl_statements ( KW_END_BLOCK | KW_ENDBLOCK )
        => BLOCK(TRIVIA_AFTER(sp, @$), decls, @$)
    | KW_CRITICAL "(" ")" sp=sep body=statement* end_critical
        => CRITICAL1(TRIVIA_AFTER(sp, @$), body, @$)
    | KW_CRITICAL "(" stats=sync_stat_list ")" sp=sep body=statement*
        end_critical
        => CRITICAL2(stats, TRIVIA_AFTER(sp, @$), body, @$)
    | KW_CRITICAL sp=sep body=statement* end_critical
        => CRITICAL(TRIVIA_AFTER(sp, @$), body, @$)
    | sync_statement
    | ( KW_FORM KW_TEAM | KW_FORM_TEAM ) "(" e=expr "," n=id
        ( ")"                       => FORMTEAM1(e, n, @$) )?
        stats=sync_stat_list ")"    => FORMTEAM2(e, n, stats, @$)
    | change_team_statement
    | KW_EVENT KW_POST "(" e=expr
        ( ")"                       => EVENT_POST(e, @$) )?
        "," specs=sync_stat_list ")" => EVENT_POST1(e, specs, @$)
    | KW_EVENT KW_WAIT "(" e=expr
        ( ")"                       => EVENT_WAIT(e, @$) )?
        specs=event_wait_specs ")"  => EVENT_WAIT1(e, specs, @$)
    ;

// The format of a PRINT statement; `*` is null
rule format -> ast_t*
    : "*"                           => nullptr
    | expr
    ;

// The stop code and the QUIET specifier of STOP and ERROR STOP
rule stop_code -> StopCode
    : &[ SEP ]                      => StopCode{nullptr, nullptr}
    | "," KW_QUIET "=" q=expr       => StopCode{nullptr, q}
    | c=expr ( "," KW_QUIET "=" q=expr => StopCode{c, q} )?
                                    => StopCode{c, nullptr}
    ;

// "(" write_arg_list ")"
rule write_args -> Vec<ArgStarKw>
    : "(" list=write_arg % "," ")"  => list
    ;

rule write_arg -> ArgStarKw*
    : n=id "=" "*"                  => write_arg_keyword(n, nullptr)
    | n=id "=" e=expr               => write_arg_keyword(n, e)
    | "*"                           => write_arg_value(nullptr)
    | e=expr                        => write_arg_value(e)
    ;

// The unit of REWIND, BACKSPACE, FLUSH and ENDFILE without parentheses; the
// location of an integer or a function reference is the whole statement `s`
rule unit_number(size_t s) -> ast_t*
    : v=TK_INTEGER                  => INTEGER($v, @$)
    | n=id "(" args=fnarray_arg_list_opt ")"
                                    => FUNCCALLORARRAY(n, args, @$)
    | id
    ;

// [","] sync_stat ["," sync_stat]...
rule sync_stat_list -> Vec<ast_t*>
    : ","? list=sync_stat % ","     => list
    ;

rule sync_stat -> ast_t*
    : KW_STAT "=" n=id              => STAT(n, @$)
    | KW_ERRMSG "=" n=id            => ERRMSG(n, @$)
    | KW_NEW_INDEX "=" e=expr       => NEW_INDEX(e, @$)
    ;

rule end_critical -> void
    : KW_END_CRITICAL
    | KW_ENDCRITICAL
    ;

rule sync_statement -> ast_t*
    : ( KW_SYNC KW_ALL | KW_SYNC_ALL )
        ( "(" ")"                   => SYNC_ALL1(@$)
        | "(" stats=sync_stat_list ")" => SYNC_ALL2(stats, @$)
        )?
                                    => SYNC_ALL(@$)
    | ( KW_SYNC KW_MEMORY | KW_SYNC_MEMORY )
        ( "(" ")"                   => SYNC_MEMORY1(@$)
        | "(" stats=sync_stat_list ")" => SYNC_MEMORY2(stats, @$)
        )?
                                    => SYNC_MEMORY(@$)
    | ( KW_SYNC KW_IMAGES | KW_SYNC_IMAGES ) "("
        ( "*" ")"                   => SYNC_IMAGE1(Asterisk, @$)
        | "*" stats=sync_stat_list ")" => SYNC_IMAGE3(Asterisk, stats, @$)
        | e=expr
            ( ")"                   => SYNC_IMAGE2(e, @$) )?
            stats=sync_stat_list ")" => SYNC_IMAGE4(e, stats, @$)
        )
    | ( KW_SYNC KW_TEAM | KW_SYNC_TEAM ) "(" e=expr
        ( ")"                       => SYNCTEAM1(e, @$) )?
        stats=sync_stat_list ")"    => SYNCTEAM2(e, stats, @$)
    ;

rule change_team_statement -> ast_t*
    : ( KW_CHANGE KW_TEAM | KW_CHANGE_TEAM ) "(" e=expr
        assoc=coarray_association*
        ( ")" sp=sep body=statement* ( KW_END_TEAM | KW_ENDTEAM )
            ( "(" stats2=sync_stat_list ")"
                => CHANGETEAM2(e, assoc, TRIVIA_AFTER(sp, @$), body, stats2, @$)
            )?
            => CHANGETEAM1(e, assoc, TRIVIA_AFTER(sp, @$), body, @$)
        | stats=sync_stat_list ")" sp=sep body=statement*
            ( KW_END_TEAM | KW_ENDTEAM )
            ( "(" stats2=sync_stat_list ")"
                => CHANGETEAM4(e, assoc, stats, TRIVIA_AFTER(sp, @$), body, stats2, @$)
            )?
            => CHANGETEAM3(e, assoc, stats, TRIVIA_AFTER(sp, @$), body, @$)
        )
    ;

// "," name "[" ... "]" "=>" expr; the location starts at the name
rule coarray_association? -> ast_t*
    : &[ "," NAME "[" ] "," a=coarray_association1 => a
    ;

rule coarray_association1 -> ast_t*
    : n=id "[" co=coarray_arg_list "]" "=>" e=expr
                                    => COARRAY_ASSOC(n, co, e, @$)
    ;

// DATA statement

rule data_set -> ast_t*
    : objects=data_object % "," "/" values=data_stmt_value_list "/"
                                    => DATA(objects, values, @$)
    ;

rule signed_numeric_constant -> ast_t*
    : v=TK_INTEGER                  => INTEGER($v, @$)
    | v=TK_REAL                     => REAL($v, @$)
    | "-" x=signed_numeric_constant => UNARY_MINUS(x, @$)
    | "+" x=signed_numeric_constant => UNARY_PLUS(x, @$)
    ;

rule data_stmt_constant -> ast_t*
    : designator
    | v=TK_STRING                   => STRING($v, @$)
    | v=TK_BOZ_CONSTANT             => BOZ($v, @$)
    | v=".true."                    => TRUE($v, @$)
    | v=".false."                   => FALSE($v, @$)
    | "(" x=signed_numeric_constant "," y=signed_numeric_constant ")"
                                    => COMPLEX(x, y, @$)
    | signed_numeric_constant
    ;

// IF

rule endif -> void
    : KW_END_IF
    | v=KW_ENDIF                    => warn_endif(@v)
    ;

// An IF construct, an IF statement or an arithmetic IF. `then` starts a
// construct only at the end of the line (or before a construct name):
// `if (c) then = 1` is an IF statement.
rule if_statement -> ast_t*
    : KW_IF "(" cond=expr ")"
        ( &[ KW_THEN SEP | KW_THEN NAME SEP ] if_block(@, cond)
        | a=TK_INTEGER "," b=TK_INTEGER "," c=TK_INTEGER
            => IFARITHMETIC(cond, INTEGER3($a), INTEGER3($b), INTEGER3($c), @$)
        | st=statement1?            => IFSINGLE(cond, required(st), @$)
        )
    ;

// The rest of an IF construct that starts at token `s`, from `then` on
rule if_block(size_t s, ast_t *cond) -> ast_t*
    : KW_THEN id? sp=sep body=statement*
        ( KW_ELSE blk=if_construct  => IF3(cond, TRIVIA_AFTER(sp, @$), body, blk, @$)
        | blk=elseif_construct      => IF3(cond, TRIVIA_AFTER(sp, @$), body, blk, @$)
        | KW_ELSE id? sp2=sep orelse=statement* endif
                                    => IF2(cond, TRIVIA(sp, sp2, @$), body, orelse, @$)
        | endif                     => IF1(cond, TRIVIA_AFTER(sp, @$), body, @$)
        )
    ;

// The IF construct after `else`: its location starts at the `if`
rule if_construct -> ast_t*
    : KW_IF "(" cond=expr ")" if_block(@, cond)
    ;

rule elseif_construct -> ast_t*
    : KW_ELSEIF "(" cond=expr ")" if_block(@, cond)
    ;

// DO

// The value is the label of a labeled END DO, or 0
rule enddo -> int64_t
    : lab=TK_LABEL ( KW_END_DO | KW_ENDDO ) => $lab
    | KW_END_DO                     => 0
    | v=KW_ENDDO                    => warn_enddo(@v)
    ;

rule do_statement -> ast_t*
    : KW_DOWHILE "(" cond=expr ")" sp=sep body=statement* enddo
        => WHILE(cond, TRIVIA_AFTER(sp, @$), body, @$)
    | KW_DO sp=sep body=statement* end_label=enddo
        => DO1(TRIVIA_AFTER(sp, @$), body, end_label, @$)
    | KW_DO ","? KW_WHILE "(" cond=expr ")" sp=sep body=statement* enddo
        => WHILE(cond, TRIVIA_AFTER(sp, @$), body, @$)
    | KW_DO ","? KW_CONCURRENT h=concurrent_header loc=concurrent_locality*
        sp=sep body=statement* enddo
        => do_concurrent(h, loc, TRIVIA_AFTER(sp, @$), body, @$)
    | KW_DO lab=TK_INTEGER ","? var=id "=" a=expr "," b=expr ( "," c=expr )?
        sp=sep body=labeled_do_body(lab)
        => do_loop_label(INTEGER3($lab), var, a, b, c, TRIVIA_AFTER(sp, @$), body, @$)
    | KW_DO ","? var=id "=" a=expr "," b=expr ( "," c=expr )?
        sp=sep body=statement* end_label=enddo
        => do_loop(var, a, b, c, TRIVIA_AFTER(sp, @$), body, end_label, @$)
    ;

rule concurrent_locality? -> ast_t*
    : KW_LOCAL "(" ids=id % "," ")"         => CONCURRENT_LOCAL(ids, @$)
    | KW_LOCAL_INIT "(" ids=id % "," ")"    => CONCURRENT_LOCAL_INIT(ids, @$)
    | KW_SHARED "(" ids=id % "," ")"        => CONCURRENT_SHARED(ids, @$)
    | KW_DEFAULT "(" KW_NONE ")"            => CONCURRENT_DEFAULT(@$)
    | KW_REDUCE "(" op=reduce_op ":" ids=id % "," ")"
                                            => CONCURRENT_REDUCE(op, ids, @$)
    ;

rule reduce_op -> reduce_opType
    : "+"                           => REDUCE_OP_TYPE_ADD(@$)
    | "*"                           => REDUCE_OP_TYPE_MUL(@$)
    | ".and."                       => reduce_opType::ReduceAND
    | ".or."                        => reduce_opType::ReduceOR
    | ".eqv."                       => reduce_opType::ReduceEQV
    | ".neqv."                      => reduce_opType::ReduceNEQV
    | n=id                          => REDUCE_OP_TYPE_ID(n, @$)
    ;

// WHERE

rule endwhere -> void
    : KW_END_WHERE
    | KW_ENDWHERE
    ;

rule where_statement -> ast_t*
    : KW_WHERE "(" cond=expr ")"
        ( where_block(@, cond)
        | &is_assignment st=assignment_statement => WHERESINGLE(cond, st, @$)
        )
    ;

// The rest of a WHERE construct (or of an ELSEWHERE block) that starts at
// token `s`, after the mask
rule where_block(size_t s, ast_t *cond) -> ast_t*
    : sp=sep body=statement*
        ( blk=elsewhere_construct   => WHERE3(cond, TRIVIA_AFTER(sp, @$), body, blk, @$)
        | ( KW_ELSEWHERE | KW_ELSE KW_WHERE ) sp2=sep orelse=statement* endwhere
                                    => WHERE2(cond, TRIVIA(sp, sp2, @$), body, orelse, @$)
        | endwhere                  => WHERE1(cond, TRIVIA_AFTER(sp, @$), body, @$)
        )
    ;

// ELSEWHERE with a mask: its location starts at the `elsewhere` or the `else`
rule elsewhere_construct -> ast_t*
    : ( KW_ELSEWHERE | KW_ELSE KW_WHERE ) "(" cond=expr ")" where_block(@, cond)
    ;

// FORALL

rule forall_statement -> ast_t*
    : KW_FORALL h=concurrent_header loc=concurrent_locality*
        ( sp=sep body=statement* ( KW_END_FORALL | KW_ENDFORALL )
            => forall_construct(h, loc, TRIVIA_AFTER(sp, @$), body, @$)
        | &is_assignment st=assignment_statement => forall_single(h, st, @$)
        )
    ;

// SELECT CASE, SELECT TYPE, SELECT RANK

rule end_select -> void
    : KW_END_SELECT
    | KW_ENDSELECT
    ;

rule select_statement -> ast_t*
    : ( KW_SELECT KW_CASE | KW_SELECT_CASE ) "(" e=expr ")" sp=sep
        cases=case_statement* end_select
        => SELECT(e, TRIVIA_AFTER(sp, @$), cases, @$)
    | ( KW_SELECT KW_TYPE | KW_SELECT_TYPE ) "(" n=id "=>" e=expr ")" sp=sep
        guards=type_guard* end_select
        => SELECT_TYPE2(n, e, TRIVIA_AFTER(sp, @$), guards, @$)
    | ( KW_SELECT KW_TYPE | KW_SELECT_TYPE ) "(" e=expr ")" sp=sep
        guards=type_guard* end_select
        => SELECT_TYPE1(e, TRIVIA_AFTER(sp, @$), guards, @$)
    | ( KW_SELECT KW_RANK | KW_SELECT_RANK ) "(" n=id "=>" e=expr ")" sp=sep
        ranks=rank_case* end_select
        => SELECT_RANK2(n, e, TRIVIA_AFTER(sp, @$), ranks, @$)
    | ( KW_SELECT KW_RANK | KW_SELECT_RANK ) "(" e=expr ")" sp=sep
        ranks=rank_case* end_select
        => SELECT_RANK1(e, TRIVIA_AFTER(sp, @$), ranks, @$)
    ;

rule case_statement? -> ast_t*
    : KW_CASE KW_DEFAULT id? sp=sep body=statement*
        => CASE_STMT_DEFAULT(TRIVIA_AFTER(sp, @$), body, @$)
    | KW_CASE "(" conds=case_condition % "," ")" id? sp=sep body=statement*
        => CASE_STMT(conds, TRIVIA_AFTER(sp, @$), body, @$)
    ;

rule case_condition -> ast_t*
    : ":" b=expr                    => CASE_RANGE2(b, @$)
    | a=expr
        ( ":" &[ "," | ")" ]        => CASE_RANGE1(a, @$)
        | ":" b=expr                => CASE_RANGE3(a, b, @$)
        )?
                                    => CASE_EXPR(a, @$)
    ;

// `type is (name)` takes a plain name only; a keyword there is an intrinsic
// type
rule type_guard? -> ast_t*
    : KW_TYPE KW_IS "(" v=TK_NAME ")" sp=sep body=statement*
        => TYPE_STMTNAME($v, TRIVIA_AFTER(sp, @$), body, @$)
    | KW_TYPE KW_IS "(" v=TK_NAME kind=paren_kind_args ")" sp=sep body=statement*
        => TYPE_STMTVAR(ATTR_TYPE_NAME_KIND(Type, SYMBOL($v, @v), kind, @$), TRIVIA_AFTER(sp, @$), body, @$)
    | KW_TYPE KW_IS "(" vt=var_type ")" sp=sep body=statement*
        => TYPE_STMTVAR(vt, TRIVIA_AFTER(sp, @$), body, @$)
    | KW_CLASS KW_IS "(" n=id ")" sp=sep body=statement*
        => CLASS_STMT(n, TRIVIA_AFTER(sp, @$), body, @$)
    | KW_CLASS KW_DEFAULT sp=sep body=statement*
        => CLASS_DEFAULT(TRIVIA_AFTER(sp, @$), body, @$)
    ;

rule rank_case? -> ast_t*
    : KW_RANK KW_DEFAULT id? sp=sep body=statement*
        => RANK_DEFAULT(TRIVIA_AFTER(sp, @$), body, @$)
    | KW_RANK "(" "*" ")" id? sp=sep body=statement*
        => RANK_STAR(TRIVIA_AFTER(sp, @$), body, @$)
    | KW_RANK "(" r=expr ")" id? sp=sep body=statement*
        => RANK_EXPR(r, TRIVIA_AFTER(sp, @$), body, @$)
    ;

// ----------------------------------------------------------------------------
// Declarations

// One declaration or statement of the specification and execution parts,
// which the grammar does not keep apart. Only a variable
// declaration and a statement can have a label.
rule decl_statement? -> ast_t*
    : labeled_decl_or_statement
    | &is_assignment statement
    | implicit_statement
    | use_statement
    | import_statement
    | interface_decl
    | deferred_decl
    | &[ KW_TEMPLATE NAME ] ![ _ ":" ] template_decl
    | &[ KW_REQUIREMENT ] ![ _ ":" ] requirement_decl
    | &[ KW_REQUIRE ] ![ _ ":" ] require_decl
    | union_type_decl
    | &[ KW_TYPE "," | KW_TYPE "::" | KW_TYPE NAME ] ![ KW_TYPE KW_IS ]
        derived_type_decl
    | enum_decl
    | var_decl_or_statement
    ;

rule template_decl -> ast_t*
    : KW_TEMPLATE name=id "{" params=id_list_opt "}" sep decls=decl_statements
        contains=contains_block_opt KW_END KW_TEMPLATE end_name=id? sep
        => TEMPLATE(name, params, decls, contains, end_name, @$)
    ;

rule requirement_decl -> ast_t*
    : KW_REQUIREMENT name=id "{" params=id_list_opt "}" sep
        decls=decl_statements KW_END KW_REQUIREMENT end_name=id? sep
        => REQUIREMENT(name, params, decls, end_name, @$)
    ;

rule require_decl -> ast_t*
    : KW_REQUIRE "::"? req=unit_require sep => REQUIRE(req, @$)
    ;

rule unit_require -> ast_t*
    : name=id args=instantiate_symbols => UNIT_REQUIRE(name, args, @$)
    ;

rule union_type_decl -> ast_t*
    : KW_UNION_TYPE mods=var_modifiers name=id sp=sep decls=var_decl*
        KW_END_UNION_TYPE id? sp2=sep
        => UNION_TYPE(mods, name, TRIVIA(sp, sp2, @$), decls, @$)
    ;

// An id_list or nothing, before the token that closes it
rule id_list_opt -> Vec<ast_t*>
    : &[ ")" | "}" ]                => empty_list()
    | list=id % ","                 => list
    ;

rule id_list -> Vec<ast_t*>
    : list=id % ","                 => list
    ;

rule implicit_statement -> ast_t*
    : KW_IMPLICIT KW_NONE "(" ")" sp=sep
        => IMPLICIT_NONE2(empty_list(), TRIVIA_AFTER(sp, @$), @$)
    | KW_IMPLICIT KW_NONE "(" specs=implicit_none_spec % "," ")" sp=sep
        => IMPLICIT_NONE2(specs, TRIVIA_AFTER(sp, @$), @$)
    | KW_IMPLICIT KW_NONE sp=sep    => IMPLICIT_NONE(TRIVIA_AFTER(sp, @$), @$)
    | KW_IMPLICIT specs=implicit_spec % "," sp=sep
        => IMPLICIT(specs, TRIVIA_AFTER(sp, @$), @$)
    ;

rule implicit_none_spec -> ast_t*
    : KW_EXTERNAL                   => IMPLICIT_NONE_EXTERNAL(@$)
    | KW_TYPE                       => IMPLICIT_NONE_TYPE(@$)
    ;

// "(" kind_arg_list ")"
rule paren_kind_args -> Vec<kind_item_t>
    : "(" list=kind_arg % "," ")"   => list
    ;

// A type and the letters it applies to: `integer (a-h)`. With two
// parenthesized lists the first one is the kind: `integer(8) (a-h)`. The
// warning for the old `*n` form is at the `*`.
rule implicit_spec -> ast_t*
    : KW_INTEGER star="*" v=TK_INTEGER letters=paren_kind_args
        => IMPLICIT_SPEC(warn_star(KW_INTEGER, $v, @star, ATTR_TYPE_INT(Integer, $v, @$)), letters, @$)
    | KW_CHARACTER star="*" v=TK_INTEGER letters=paren_kind_args
        => IMPLICIT_SPEC(warn_star(KW_CHARACTER, $v, @star, ATTR_TYPE_INT(Character, $v, @$)), letters, @$)
    | KW_REAL star="*" v=TK_INTEGER letters=paren_kind_args
        => IMPLICIT_SPEC(warn_star(KW_REAL, $v, @star, ATTR_TYPE_INT(Real, $v, @$)), letters, @$)
    | KW_LOGICAL star="*" v=TK_INTEGER letters=paren_kind_args
        => IMPLICIT_SPEC(warn_star(KW_LOGICAL, $v, @star, ATTR_TYPE_INT(Logical, $v, @$)), letters, @$)
    | KW_COMPLEX star="*" v=TK_INTEGER letters=paren_kind_args
        => IMPLICIT_SPEC(warn_star(KW_COMPLEX, $v, @star, ATTR_TYPE_INT(Complex, DIV2($v), @$)), letters, @$)
    | KW_CHARACTER star="*" "(" e=expr ")" letters=paren_kind_args
        => IMPLICIT_SPEC(warn_star_expr(@star, ATTR_TYPE_EXPR(Character, e, @$)), letters, @$)
    | KW_INTEGER implicit_spec_kind(@, decl_typeType::TypeInteger)
    | KW_CHARACTER implicit_spec_kind(@, decl_typeType::TypeCharacter)
    | KW_REAL implicit_spec_kind(@, decl_typeType::TypeReal)
    | KW_LOGICAL implicit_spec_kind(@, decl_typeType::TypeLogical)
    | KW_COMPLEX implicit_spec_kind(@, decl_typeType::TypeComplex)
    | ( KW_DOUBLE KW_PRECISION | KW_DOUBLE_PRECISION ) letters=paren_kind_args
        => IMPLICIT_SPEC(ATTR_TYPE(DoublePrecision, @$), letters, @$)
    | ( KW_DOUBLE KW_COMPLEX | KW_DOUBLE_COMPLEX ) letters=paren_kind_args
        => IMPLICIT_SPEC(ATTR_TYPE(DoubleComplex, @$), letters, @$)
    | KW_TYPE "(" it=intrinsic_type_spec ")"
        letters=paren_kind_args
        => IMPLICIT_SPEC(ATTR_TYPE_ATTR(Type, it, @$), letters, @$)
    | KW_TYPE "(" n=id ")" letters=paren_kind_args
        => IMPLICIT_SPEC(ATTR_TYPE_NAME(Type, n, @$), letters, @$)
    | KW_CLASS "(" n=id ")" letters=paren_kind_args
        => IMPLICIT_SPEC(ATTR_TYPE_NAME(Class, n, @$), letters, @$)
    ;

rule implicit_spec_kind(size_t s, decl_typeType type) -> ast_t*
    : first=paren_kind_args
        ( letters=paren_kind_args
            => IMPLICIT_SPEC(attr_type(type, first, @$), letters, @$) )?
        => IMPLICIT_SPEC(attr_type(type, @$), first, @$)
    ;

rule import_statement -> ast_t*
    : KW_IMPORT sp=sep              => IMPORT0(Default, TRIVIA_AFTER(sp, @$), @$)
    | KW_IMPORT "," KW_ONLY ":" ids=id % "," sp=sep
                                    => IMPORT1(ids, Only, TRIVIA_AFTER(sp, @$), @$)
    | KW_IMPORT "," KW_NONE sp=sep  => IMPORT0(None, TRIVIA_AFTER(sp, @$), @$)
    | KW_IMPORT "," KW_ALL sp=sep   => IMPORT0(All, TRIVIA_AFTER(sp, @$), @$)
    | KW_IMPORT "::"? ids=id % "," sp=sep
                                    => IMPORT1(ids, Default, TRIVIA_AFTER(sp, @$), @$)
    ;

rule operator_type -> intrinsicopType
    : "+"                           => intrinsicopType::PLUS
    | "-"                           => intrinsicopType::MINUS
    | "*"                           => intrinsicopType::STAR
    | "/"                           => intrinsicopType::DIV
    | "**"                          => intrinsicopType::POW
    | "=="                          => intrinsicopType::EQ
    | "/="                          => intrinsicopType::NOTEQ
    | ">"                           => intrinsicopType::GT
    | ">="                          => intrinsicopType::GTE
    | "<"                           => intrinsicopType::LT
    | "<="                          => intrinsicopType::LTE
    | "//"                          => intrinsicopType::CONCAT
    | ".not."                       => intrinsicopType::NOT
    | ".and."                       => intrinsicopType::AND
    | ".or."                        => intrinsicopType::OR
    | ".xor."                       => intrinsicopType::XOR
    | ".eqv."                       => intrinsicopType::EQV
    | ".neqv."                      => intrinsicopType::NEQV
    ;

// `operator(/)` is tokenized as `operator`, `(` and `/)`
rule use_symbol -> ast_t*
    : KW_OPERATOR "(" "/)"          => INTRINSIC_OPERATOR(intrinsicopType::DIV, @$)
    | KW_OPERATOR "(" op=operator_type ")" => INTRINSIC_OPERATOR(op, @$)
    | KW_OPERATOR "(" d=TK_DEF_OP ")" "=>" KW_OPERATOR "(" d2=TK_DEF_OP ")"
                                    => RENAME_OPERATOR($d, $d2, @$)
    | KW_OPERATOR "(" d=TK_DEF_OP ")" => DEFINED_OPERATOR($d, @$)
    | KW_ASSIGNMENT "(" "=" ")"     => USE_ASSIGNMENT(@$)
    | KW_WRITE "(" n=id ")"         => USE_WRITE(n, @$)
    | KW_READ "(" n=id ")"          => USE_READ(n, @$)
    | a=id "=>" b=id                => USE_SYMBOL2(a, b, @$)
    | a=id                          => USE_SYMBOL1(a, @$)
    ;

// `operator(+)` or `operator(.op.)` as an argument of a template
rule decl_op -> ast_t*
    : KW_OPERATOR "(" "/)"          => DECL_OP(intrinsicopType::DIV, @$)
    | KW_OPERATOR "(" op=operator_type ")" => DECL_OP(op, @$)
    | KW_OPERATOR "(" o=TK_DEF_OP ")" => DECL_DEFOP($o, @$)
    ;

rule use_symbol_list -> Vec<ast_t*>
    : list=use_symbol % ","         => list
    ;

rule use_statement -> ast_t*
    : u=use_statement1 sp=sep       => use_trivia(u, TRIVIA_AFTER(sp, @$))
    ;

rule use_statement1 -> ast_t*
    : KW_USE mods=use_modifiers mod=id
        ( "," KW_ONLY ":" &[ SEP ]  => USE3(mods, mod, nullptr, @$)
        | "," KW_ONLY ":" syms=use_symbol % ","
                                    => USE2(mods, mod, syms, nullptr, @$)
        | "," syms=use_symbol % "," => USE4(mods, mod, syms, nullptr, @$)
        )?
                                    => USE1(mods, mod, nullptr, @$)
    ;

rule use_modifiers -> Vec<ast_t*>
    : "," list=use_modifier % "," "::" => list
    | "::"                          => empty_list()
    |                               => empty_list()
    ;

rule use_modifier -> ast_t*
    : KW_INTRINSIC                  => SIMPLE_ATTR(Intrinsic, @$)
    | KW_NON_INTRINSIC              => SIMPLE_ATTR(Non_Intrinsic, @$)
    ;

// var_decl: a declaration without a label; null if none starts here
rule var_decl? -> ast_t*
    : typed_decl(@)
    | m=a_var_modifier
        ( ^ sp=sep                  => VAR_DECL2(m, TRIVIA_AFTER(sp, @^), @^)
        | "::"? syms=var_sym_decl % "," ^ sp=sep
                                    => VAR_DECL3(m, syms, TRIVIA_AFTER(sp, @^), @^)
        )
    ;

// The declarations that can have a label; `s` is the token of the label, if
// there is one. The location of a declaration does not include the separator
// after it.
rule typed_decl?(size_t s) -> ast_t*
    : KW_PARAMETER "(" list=named_constant_def % "," ")" ^ sp=sep
        => VAR_DECL_PARAMETER(list, TRIVIA_AFTER(sp, @^), @^)
    | KW_NAMELIST groups=namelist_group_list ^ sp=sep
        => VAR_DECL_NAMELIST(groups, TRIVIA_AFTER(sp, @^), @^)
    | KW_COMMON blocks=common_block_list ^ sp=sep
        => VAR_DECL_COMMON(blocks, TRIVIA_AFTER(sp, @^), @^)
    | KW_EQUIVALENCE sets=equivalence_set % "," ^ sp=sep
        => VAR_DECL_EQUIVALENCE(sets, TRIVIA_AFTER(sp, @^), @^)
    | v=TK_PRAGMA_DECL ^ sp=sep
        => VAR_DECL_PRAGMA($v, TRIVIA_AFTER(sp, @^), @^)
    | &is_var_type_start vt=var_type
        ( "," mods=a_var_modifier % "," "::" syms=var_sym_decl % "," ^ sp=sep
            => VAR_DECL1a(vt, mods, syms, TRIVIA_AFTER(sp, @^), @^)
        | "::" syms=var_sym_decl % "," ^ sp=sep
            => VAR_DECL1b(vt, syms, TRIVIA_AFTER(sp, @^), @^)
        | syms=var_sym_decl % "," ^ sp=sep
            => VAR_DECL1c(vt, syms, TRIVIA_AFTER(sp, @^), @^)
        )
    ;

rule pragma_decl -> ast_t*
    : v=TK_PRAGMA_DECL ^ sp=sep     => VAR_DECL_PRAGMA($v, TRIVIA_AFTER(sp, @^), @^)
    ;

rule named_constant_def -> var_sym_t*
    : n=id "=" e=expr               => VAR_SYM_DIM_INIT(n, nullptr, 0, e, Equal, @$)
    ;

rule equivalence_set -> equi_t*
    : "(" list=expr % "," ")"       => EQUIVALENCE_SET(list, @$)
    ;

// intrinsic_type_spec. `real*8` is the old form of `real(8)`; a `*` that is
// not followed by a number is not part of the type.
rule intrinsic_type_spec -> ast_t*
    : KW_INTEGER "*" v=TK_INTEGER
        => warn_star(KW_INTEGER, $v, @$, ATTR_TYPE_INT(Integer, $v, @$))
    | KW_REAL "*" v=TK_INTEGER
        => warn_star(KW_REAL, $v, @$, ATTR_TYPE_INT(Real, $v, @$))
    | KW_COMPLEX "*" v=TK_INTEGER
        => warn_star(KW_COMPLEX, $v, @$, ATTR_TYPE_INT(Complex, DIV2($v), @$))
    | KW_LOGICAL "*" v=TK_INTEGER
        => warn_star(KW_LOGICAL, $v, @$, ATTR_TYPE_INT(Logical, $v, @$))
    | KW_CHARACTER "*" v=TK_INTEGER
        => warn_star(KW_CHARACTER, $v, @$, ATTR_TYPE_INT(Character, $v, @$))
    | KW_CHARACTER "*" "(" "*" ")"
        => ATTR_TYPE_STAR(Character, DoubleAsterisk, @$)
    | KW_CHARACTER "*" "(" e=expr ")"
        => warn_star_expr(@$, ATTR_TYPE_EXPR(Character, e, @$))
    | KW_INTEGER type_with_kind(@, decl_typeType::TypeInteger)
    | KW_REAL type_with_kind(@, decl_typeType::TypeReal)
    | KW_COMPLEX type_with_kind(@, decl_typeType::TypeComplex)
    | KW_LOGICAL type_with_kind(@, decl_typeType::TypeLogical)
    | KW_CHARACTER type_with_kind(@, decl_typeType::TypeCharacter)
    | KW_BYTE                       => byte_type(@$)
    | ( KW_DOUBLE KW_PRECISION | KW_DOUBLE_PRECISION )
                                    => ATTR_TYPE(DoublePrecision, @$)
    | ( KW_DOUBLE KW_COMPLEX | KW_DOUBLE_COMPLEX )
                                    => ATTR_TYPE(DoubleComplex, @$)
    | KW_LIST "(" it=intrinsic_type_spec ")" => ATTR_TYPE_ATTR(List, it, @$)
    | KW_SET "(" it=intrinsic_type_spec ")"  => ATTR_TYPE_ATTR(Set, it, @$)
    | KW_DICT "(" list=intrinsic_type_spec % "," ")"
                                    => ATTR_TYPE_LIST(Dict, list, @$)
    | KW_TUPLE "(" list=intrinsic_type_spec % "," ")"
                                    => ATTR_TYPE_LIST(Tuple, list, @$)
    ;

// The type that starts at token `s`, with its kind if there is one
rule type_with_kind(size_t s, decl_typeType type) -> ast_t*
    : kind=paren_kind_args          => attr_type(type, kind, @$)
    |                               => attr_type(type, @$)
    ;

// `type(integer)` is the intrinsic type, not a type named `integer`. The
// location of the type inside `procedure(real(8))` is the one of the whole
// var_type.
rule var_type -> ast_t*
    : intrinsic_type_spec
    | KW_PROCEDURE "(" ")"          => ATTR_TYPE(Procedure, @$)
    | KW_PROCEDURE "(" t=procedure_type_kw kind=paren_kind_args ")"
        => ATTR_TYPE_ATTR(Procedure, attr_type(t, kind, @$), @$)
    | KW_PROCEDURE "(" KW_DOUBLE KW_PRECISION ")"
        => ATTR_TYPE_ATTR(Procedure, ATTR_TYPE(DoublePrecision, @$), @$)
    | KW_PROCEDURE "(" n=id ")"     => ATTR_TYPE_NAME(Procedure, n, @$)
    | KW_TYPE "(" "*" ")"           => ATTR_TYPE_STAR(Type, Asterisk, @$)
    | KW_TYPE "(" it=intrinsic_type_spec ")"
                                    => ATTR_TYPE_ATTR(Type, it, @$)
    | KW_TYPE "(" n=id kind=paren_kind_args ")"
                                    => ATTR_TYPE_NAME_KIND(Type, n, kind, @$)
    | KW_TYPE "(" n=id ")"          => ATTR_TYPE_NAME(Type, n, @$)
    | KW_CLASS "(" "*" ")"          => ATTR_TYPE_STAR(Class, Asterisk, @$)
    | KW_CLASS "(" n=id kind=paren_kind_args ")"
                                    => ATTR_TYPE_NAME_KIND(Class, n, kind, @$)
    | KW_CLASS "(" n=id ")"         => ATTR_TYPE_NAME(Class, n, @$)
    ;

rule procedure_type_kw -> decl_typeType
    : KW_INTEGER                    => decl_typeType::TypeInteger
    | KW_REAL                       => decl_typeType::TypeReal
    | KW_COMPLEX                    => decl_typeType::TypeComplex
    | KW_LOGICAL                    => decl_typeType::TypeLogical
    | KW_CHARACTER                  => decl_typeType::TypeCharacter
    ;

rule kind_arg -> kind_item_t*
    : n=id "=" "*"                  => KIND_ARG2S(n, @$)
    | n=id "=" ":"                  => KIND_ARG2C(n, @$)
    | n=id "=" e=expr               => KIND_ARG2(n, e, @$)
    | "*"                           => KIND_ARG1S(@$)
    | ":"                           => KIND_ARG1C(@$)
    | e=expr                        => KIND_ARG1(e, @$)
    ;

rule bind? -> ast_t*
    : KW_BIND a=write_args          => BIND2(a, @$)
    ;

rule result_clause? -> ast_t*
    : KW_RESULT "(" r=id ")"        => r
    ;

// An attribute of a declaration
rule var_modifier? -> ast_t*
    : b=bind                        => BIND(b, @$)
    | KW_PARAMETER                  => SIMPLE_ATTR(Parameter, @$)
    | KW_ALLOCATABLE                => SIMPLE_ATTR(Allocatable, @$)
    | KW_ASYNCHRONOUS               => SIMPLE_ATTR(Asynchronous, @$)
    | KW_POINTER                    => SIMPLE_ATTR(Pointer, @$)
    | KW_TARGET                     => SIMPLE_ATTR(Target, @$)
    | KW_OPTIONAL                   => SIMPLE_ATTR(Optional, @$)
    | KW_PROTECTED                  => SIMPLE_ATTR(Protected, @$)
    | KW_SAVE                       => SIMPLE_ATTR(Save, @$)
    | KW_SEQUENCE                   => SIMPLE_ATTR(Sequence, @$)
    | KW_CONTIGUOUS                 => SIMPLE_ATTR(Contiguous, @$)
    | KW_NOPASS                     => SIMPLE_ATTR(NoPass, @$)
    | KW_PRIVATE                    => SIMPLE_ATTR(Private, @$)
    | KW_PUBLIC                     => SIMPLE_ATTR(Public, @$)
    | KW_ABSTRACT                   => SIMPLE_ATTR(Abstract, @$)
    | KW_ENUMERATOR                 => SIMPLE_ATTR(Enumerator, @$)
    | KW_EXTERNAL                   => SIMPLE_ATTR(External, @$)
    | KW_INTRINSIC                  => SIMPLE_ATTR(Intrinsic, @$)
    | KW_VALUE                      => SIMPLE_ATTR(Value, @$)
    | KW_VOLATILE                   => SIMPLE_ATTR(Volatile, @$)
    | KW_KIND                       => SIMPLE_ATTR(Kind, @$)
    | KW_LEN                        => SIMPLE_ATTR(Len, @$)
    | KW_PASS "(" n=id ")"          => PASS(n, @$)
    | KW_PASS                       => PASS(nullptr, @$)
    | KW_EXTENDS "(" n=id ")"       => EXTENDS(n, @$)
    | KW_DIMENSION "(" d=array_comp_decl % "," ")" => DIMENSION(d, @$)
    | KW_DIMENSION                  => DIMENSION0(@$)
    | KW_CODIMENSION "[" d=coarray_comp_decl % "," "]" => CODIMENSION(d, @$)
    | KW_INTENT "(" KW_IN ")"       => INTENT(In, @$)
    | KW_INTENT "(" KW_OUT ")"      => INTENT(Out, @$)
    | KW_INTENT "(" ( KW_INOUT | KW_IN_OUT ) ")" => INTENT(InOut, @$)
    ;

// An attribute that has to be there
rule a_var_modifier -> ast_t*
    : m=var_modifier                => required(m)
    ;

rule array_comp_decl -> dimension_t*
    : ":" &[ "," | ")" ]            => ARRAY_COMP_DECL5d(@$)
    | ":" b=expr                    => ARRAY_COMP_DECL4d(b, @$)
    | "*"                           => ARRAY_COMP_DECL6d(@$)
    | ".."                          => ARRAY_COMP_DECL8d(@$)
    | a=expr
        ( ":" &[ "," | ")" ]        => ARRAY_COMP_DECL3d(a, @$)
        | ":" "*"                   => ARRAY_COMP_DECL7d(a, @$)
        | ":" b=expr                => ARRAY_COMP_DECL2d(a, b, @$)
        )?
                                    => ARRAY_COMP_DECL1d(a, @$)
    ;

rule coarray_comp_decl -> codimension_t*
    : ":" &[ "," | "]" ]            => COARRAY_COMP_DECL5d(@$)
    | ":" b=expr                    => COARRAY_COMP_DECL4d(b, @$)
    | "*"                           => COARRAY_COMP_DECL6d(@$)
    | a=expr
        ( ":" &[ "," | "]" ]        => COARRAY_COMP_DECL3d(a, @$)
        | ":" "*"                   => COARRAY_COMP_DECL7d(a, @$)
        | ":" b=expr                => COARRAY_COMP_DECL2d(a, b, @$)
        )?
                                    => COARRAY_COMP_DECL1d(a, @$)
    ;

rule array_comp_decl_list -> Vec<dimension_t>
    : list=array_comp_decl % ","    => list
    ;

rule var_sym_decl_list -> Vec<var_sym_t>
    : list=var_sym_decl % ","       => list
    ;

// One entity of a declaration: a name with its array bounds, codimensions,
// character length and initialization. `operator(*)` is the operator, not an
// array named `operator`.
rule var_sym_decl -> var_sym_t*
    : "/" n=id "/"                  => VAR_SYM_NAME(n, Slash, @$)
    | KW_OPERATOR "(" "/)"
        => VAR_SYM_SPEC(DECL_OP(intrinsicopType::DIV, @$), None, @$)
    | KW_OPERATOR "(" op=operator_type ")"
        => VAR_SYM_SPEC(DECL_OP(op, @$), None, @$)
    | KW_OPERATOR "(" o=TK_DEF_OP ")"
        => VAR_SYM_SPEC(DECL_DEFOP($o, @$), None, @$)
    | KW_ASSIGNMENT "(" "=" ")"
        => VAR_SYM_SPEC(DECL_ASSIGNMENT(@$), None, @$)
    | n=id ( "(" d=array_comp_decl % "," ")" )?
        ( "[" c=coarray_comp_decl % "," "]"
            ( "=" e=expr
                => VAR_SYM_DIM_CODIM_INIT(n, d.p, d.n, c.p, c.n, e, Equal, @$) )?
            => VAR_SYM_DIM_CODIM(n, d.p, d.n, c.p, c.n, None, @$)
        | "*" "(" "*" ")"
            ( "=" e=expr
                => VAR_SYM_DIM_INIT(n, d.p, d.n, e, DoubleAsterisk, @$) )?
            => VAR_SYM_DIM(n, d.p, d.n, DoubleAsterisk, @$)
        | "*" len=expr
            ( "=" e=expr
                => VAR_SYM_DIM_LEN_INIT(n, d.p, d.n, len, e, Equal, @$) )?
            => VAR_SYM_DIM_LEN(n, d.p, d.n, len, Asterisk, @$)
        | "=" e=expr                => VAR_SYM_DIM_INIT(n, d.p, d.n, e, Equal, @$)
        | "=>" e=expr               => VAR_SYM_DIM_INIT(n, d.p, d.n, e, Arrow, @$)
        | "/" values=data_stmt_constant % "," "/"
            => VAR_SYM_DIM_INIT(n, d.p, d.n, SLASH_INIT_EXPR(values, @$), SlashInit, @$)
        )?
        => VAR_SYM_DIM(n, d.p, d.n, None, @$)
    ;

// var_modifiers: [ [ "," var_modifier ]... "::" ]
rule var_modifiers -> Vec<ast_t*>
    : &[ "," ] "," list=a_var_modifier % "," "::" => list
    | "::"                          => empty_list()
    |                               => empty_list()
    ;

rule derived_type_decl -> ast_t*
    : KW_TYPE mods=var_modifiers name=id
        ( "(" params=id % "," ")" sp=sep decls=var_decl* contains=type_contains
            ( KW_END_TYPE | KW_ENDTYPE ) id? sp2=sep
            => DERIVED_TYPE1(mods, name, params, TRIVIA(sp, sp2, @$), decls, contains, @$)
        | sp=sep decls=var_decl* contains=type_contains
            ( KW_END_TYPE | KW_ENDTYPE ) id? sp2=sep
            => DERIVED_TYPE(mods, name, TRIVIA(sp, sp2, @$), decls, contains, @$)
        )
    ;

rule type_contains -> Vec<ast_t*>
    : KW_CONTAINS sep list=procedure_decl* => list
    |                               => empty_list()
    ;

// proc_modifiers: [ [ "," proc_modifier ]... "::" ]
rule proc_modifiers -> Vec<ast_t*>
    : "," list=proc_modifier % "," "::" => list
    | "::"                          => empty_list()
    |                               => empty_list()
    ;

rule proc_modifier -> ast_t*
    : KW_PRIVATE                    => SIMPLE_ATTR(Private, @$)
    | KW_PUBLIC                     => SIMPLE_ATTR(Public, @$)
    | KW_NOPASS                     => SIMPLE_ATTR(NoPass, @$)
    | KW_DEFERRED                   => SIMPLE_ATTR(Deferred, @$)
    | KW_NON_OVERRIDABLE            => SIMPLE_ATTR(NonDeferred, @$)
    | KW_PASS "(" n=id ")"          => PASS(n, @$)
    | KW_PASS                       => PASS(nullptr, @$)
    ;

// A type-bound procedure declaration
rule procedure_decl? -> ast_t*
    : KW_PROCEDURE "(" iface=id ")" mods=proc_modifiers
        syms=use_symbol % "," sp=sep
        => DERIVED_TYPE_PROC1(iface, mods, syms, TRIVIA_AFTER(sp, @$), @$)
    | KW_PROCEDURE mods=proc_modifiers syms=use_symbol % "," sp=sep
        => DERIVED_TYPE_PROC(mods, syms, TRIVIA_AFTER(sp, @$), @$)
    | KW_GENERIC access=access_spec_list
        ( KW_OPERATOR "(" "/)" "=>" ids=id % "," sp=sep
            => GENERIC_OPERATOR(access, intrinsicopType::DIV, ids, TRIVIA_AFTER(sp, @$), @$)
        | KW_OPERATOR "(" op=operator_type ")" "=>" ids=id % "," sp=sep
            => GENERIC_OPERATOR(access, op, ids, TRIVIA_AFTER(sp, @$), @$)
        | KW_OPERATOR "(" o=TK_DEF_OP ")" "=>" ids=id % "," sp=sep
            => GENERIC_DEFOP(access, $o, ids, TRIVIA_AFTER(sp, @$), @$)
        | KW_ASSIGNMENT "(" "=" ")" "=>" ids=id % "," sp=sep
            => GENERIC_ASSIGNMENT(access, ids, TRIVIA_AFTER(sp, @$), @$)
        | KW_WRITE "(" n=id ")" "=>" ids=id % "," sp=sep
            => GENERIC_WRITE(access, n, ids, TRIVIA_AFTER(sp, @$), @$)
        | KW_READ "(" n=id ")" "=>" ids=id % "," sp=sep
            => GENERIC_READ(access, n, ids, TRIVIA_AFTER(sp, @$), @$)
        | n=id "=>" ids=id % "," sp=sep
            => GENERIC_NAME(access, n, ids, TRIVIA_AFTER(sp, @$), @$)
        )
    | KW_FINAL "::" n=id sp=sep     => FINAL_NAME(n, TRIVIA_AFTER(sp, @$), @$)
    | KW_PRIVATE sp=sep             => PRIVATE(Private, TRIVIA_AFTER(sp, @$), @$)
    ;

rule access_spec_list -> Vec<ast_t*>
    : a=access_spec "::"            => one_item(a)
    | "::"                          => empty_list()
    ;

rule access_spec -> ast_t*
    : "," KW_PRIVATE                => SIMPLE_ATTR(Private, @$)
    | "," KW_PUBLIC                 => SIMPLE_ATTR(Public, @$)
    ;

rule enum_decl -> ast_t*
    : KW_ENUM "," mods=a_var_modifier % "," sp=sep decls=var_decl*
        ( KW_END_ENUM | KW_ENDENUM ) sp2=sep
        => ENUM(mods, TRIVIA(sp, sp2, @$), decls, @$)
    | KW_ENUM sp=sep decls=var_decl* ( KW_END_ENUM | KW_ENDENUM ) sp2=sep
        => ENUM(empty_list(), TRIVIA(sp, sp2, @$), decls, @$)
    ;

rule interface_decl -> ast_t*
    : header=interface_header sp=sep items=interface_item*
        ( KW_END_INTERFACE | KW_ENDINTERFACE ) end_interface_spec sp2=sep
        => INTERFACE(header, TRIVIA(sp, sp2, @$), items, @$)
    ;

rule interface_header -> ast_t*
    : KW_ABSTRACT KW_INTERFACE      => ABSTRACT_INTERFACE_HEADER(@$)
    | KW_DEFERRED KW_INTERFACE      => DEFERRED_INTERFACE_HEADER(@$)
    | KW_INTERFACE KW_OPERATOR "(" "/)"
        => INTERFACE_HEADER_OPERATOR(intrinsicopType::DIV, @$)
    | KW_INTERFACE KW_OPERATOR "(" op=operator_type ")"
        => INTERFACE_HEADER_OPERATOR(op, @$)
    | KW_INTERFACE KW_OPERATOR "(" o=TK_DEF_OP ")"
        => INTERFACE_HEADER_DEFOP($o, @$)
    | KW_INTERFACE KW_ASSIGNMENT "(" "=" ")"
        => INTERFACE_HEADER_ASSIGNMENT(@$)
    | KW_INTERFACE KW_WRITE "(" n=id ")" => INTERFACE_HEADER_WRITE(n, @$)
    | KW_INTERFACE KW_READ "(" n=id ")"  => INTERFACE_HEADER_READ(n, @$)
    | KW_INTERFACE n=id             => INTERFACE_HEADER_NAME(n, @$)
    | KW_INTERFACE                  => INTERFACE_HEADER(@$)
    ;

// What follows `end interface` is not checked against the header
rule end_interface_spec -> void
    : KW_OPERATOR "(" "/)"
    | KW_OPERATOR "(" operator_type ")"
    | KW_OPERATOR "(" TK_DEF_OP ")"
    | KW_ASSIGNMENT "(" "=" ")"
    | KW_WRITE "(" id ")"
    | KW_READ "(" id ")"
    | id
    |
    ;

// `[module] procedure a, b` or the interface of a subroutine or a function
rule interface_item? -> ast_t*
    : ![ KW_END_INTERFACE | KW_ENDINTERFACE ] mods=fn_mod*
        ( KW_PROCEDURE "::"? ids=id % "," sp=sep
            => interface_module_proc(mods, ids, TRIVIA_AFTER(sp, @$), @$)
        | u=subprogram(@, mods)     => interface_proc(u)
        )
    ;

// ----------------------------------------------------------------------------
// Program units

// A prefix of a subroutine or a function
rule fn_mod? -> ast_t*
    : KW_ELEMENTAL                  => SIMPLE_ATTR(Elemental, @$)
    | KW_IMPURE                     => SIMPLE_ATTR(Impure, @$)
    | KW_MODULE                     => SIMPLE_ATTR(Module, @$)
    | KW_PURE                       => SIMPLE_ATTR(Pure, @$)
    | KW_RECURSIVE                  => SIMPLE_ATTR(Recursive, @$)
    | KW_NON_RECURSIVE              => SIMPLE_ATTR(NonRecursive, @$)
    | &is_var_type_start var_type
    ;

rule fn_mods -> Vec<ast_t*>
    : list=fn_mod*                  => list
    ;

// The dummy arguments of a subroutine; `*` is an alternate return
rule sub_args -> Vec<ast_t*>
    : "(" ")"                       => empty_list()
    | "(" list=id_or_star % "," ")" => list
    |                               => empty_list()
    ;

rule id_or_star -> ast_t*
    : "*"                           => nullptr
    | id
    ;

// The END statements of the program units, optionally with a label; the
// value of the `_name` rules is the name after it, if any

rule end_program -> EndStmt
    : lab=TK_LABEL n=end_program_name   => END_STMT(n, $lab, @$)
    | n=end_program_name                => END_STMT(n, 0, @$)
    ;

rule end_program_name -> ast_t*
    : KW_END                        => nullptr
    | ( KW_END_PROGRAM | KW_ENDPROGRAM ) n=id? => n
    ;

rule end_subroutine -> EndStmt
    : lab=TK_LABEL n=end_subroutine_name => END_STMT(n, $lab, @$)
    | n=end_subroutine_name             => END_STMT(n, 0, @$)
    ;

rule end_subroutine_name -> ast_t*
    : KW_END                        => nullptr
    | ( KW_END_SUBROUTINE | KW_ENDSUBROUTINE ) n=id? => n
    ;

rule end_function -> EndStmt
    : lab=TK_LABEL n=end_function_name  => END_STMT(n, $lab, @$)
    | n=end_function_name               => END_STMT(n, 0, @$)
    ;

rule end_function_name -> ast_t*
    : KW_END                        => nullptr
    | ( KW_END_FUNCTION | KW_ENDFUNCTION ) n=id? => n
    ;

rule end_procedure -> EndStmt
    : lab=TK_LABEL n=end_procedure_name => END_STMT(n, $lab, @$)
    | n=end_procedure_name              => END_STMT(n, 0, @$)
    ;

rule end_procedure_name -> ast_t*
    : KW_END                        => nullptr
    | ( KW_END_PROCEDURE | KW_ENDPROCEDURE ) n=id? => n
    ;

rule end_module -> ast_t*
    : KW_END                        => nullptr
    | ( KW_END_MODULE | KW_ENDMODULE ) n=id? => n
    ;

rule end_submodule -> ast_t*
    : KW_END                        => nullptr
    | ( KW_END_SUBMODULE | KW_ENDSUBMODULE ) n=id? => n
    ;

rule end_blockdata -> void
    : KW_END
    | ( KW_END_BLOCK_DATA | KW_ENDBLOCKDATA ) id?
    ;

rule contains_block_opt -> Vec<ast_t*>
    : contains_block
    |                               => empty_list()
    ;

// The location of a unit ends at its END statement (the mark `^`); the
// separator after it only gives the trivia

rule program -> ast_t*
    : KW_PROGRAM name=id sp=sep decls=decl_statements
        ( contains=contains_block end=end_program ^ sp2=sep
            => PROGRAM(name, TRIVIA(sp, sp2, @^), decls, CONTAINS_END2(m_a, contains, end), @^)
        | end=end_program ^ sp2=sep
            => PROGRAM(name, TRIVIA(sp, sp2, @^), decls, CONTAINS_END1(m_a, end), @^)
        )
    ;

// The subroutine or function that starts at token `s` with the prefixes
// `mods`; null if none starts here
rule subprogram?(size_t s, Vec<ast_t*> &mods) -> ast_t*
    : KW_SUBROUTINE name=id args=sub_args b=bind? sp=sep decls=decl_statements
        ( contains=contains_block end=end_subroutine ^ sp2=sep
            => subroutine(mods, name, args, b, TRIVIA(sp, sp2, @^), decls, CONTAINS_END2(m_a, contains, end), @^)
        | end=end_subroutine ^ sp2=sep
            => subroutine(mods, name, args, b, TRIVIA(sp, sp2, @^), decls, CONTAINS_END1(m_a, end), @^)
        )
    | KW_FUNCTION name=id "(" args=id_list_opt ")" c=function_clauses sp=sep
        decls=decl_statements
        ( contains=contains_block end=end_function ^ sp2=sep
            => function(mods, name, args, c, TRIVIA(sp, sp2, @^), decls, CONTAINS_END2(m_a, contains, end), @^)
        | end=end_function ^ sp2=sep
            => function(mods, name, args, c, TRIVIA(sp, sp2, @^), decls, CONTAINS_END1(m_a, end), @^)
        )
    ;

// RESULT and BIND of a function, in any order
rule function_clauses -> FunctionClauses
    : r=result_clause b=bind?       => FunctionClauses{r, b}
    | b=bind r=result_clause?       => FunctionClauses{r, b}
    |                               => FunctionClauses{nullptr, nullptr}
    ;

// A module procedure, `module procedure name ... end procedure`, that starts
// at token `s`. Its location includes the separator after it.
rule module_procedure(size_t s, Vec<ast_t*> &mods) -> ast_t*
    : KW_PROCEDURE name=id args=sub_args sp=sep decls=decl_statements
        ( contains=contains_block end=end_procedure sp2=sep
            => PROCEDURE(mods, name, args, TRIVIA(sp, sp2, @$), decls, CONTAINS_END2(m_a, contains, end), @$)
        | end=end_procedure sp2=sep
            => PROCEDURE(mods, name, args, TRIVIA(sp, sp2, @$), decls, CONTAINS_END1(m_a, end), @$)
        )
    ;

// A templated subroutine or function, after `template` and its prefixes
rule templated_unit(size_t s, Vec<ast_t*> &mods) -> ast_t*
    : KW_SUBROUTINE name=id "{" temp_args=id % "," "}" args=sub_args b=bind?
        sp=sep decls=decl_statements end=end_subroutine ^ sp2=sep
        => TEMPLATED_SUBROUTINE1(mods, name, temp_args, args, b, TRIVIA(sp, sp2, @^), decls, end, @^)
    | KW_FUNCTION name=id "{" temp_args=id % "," "}" "(" args=id_list_opt ")"
        r=result_clause? b=bind? sp=sep decls=decl_statements end=end_function
        ^ sp2=sep
        => TEMPLATED_FUNCTION(mods, name, temp_args, args, r, b, TRIVIA(sp, sp2, @^), decls, end, @^)
    ;

rule module -> ast_t*
    : KW_MODULE name=id sp=sep decls=decl_statements
        contains=contains_block_opt end_name=end_module ^ sp2=sep
        => MODULE(name, TRIVIA(sp, sp2, @^), decls, contains, end_name, @^)
    ;

rule submodule -> ast_t*
    : KW_SUBMODULE "(" id1=id ":" id2=id ")" name=id sp=sep
        decls=decl_statements contains=contains_block_opt
        end_name=end_submodule ^ sp2=sep
        => SUBMODULE1(id1, id2, name, TRIVIA(sp, sp2, @^), decls, contains, end_name, @^)
    | KW_SUBMODULE "(" id1=id ")" name=id sp=sep
        decls=decl_statements contains=contains_block_opt
        end_name=end_submodule ^ sp2=sep
        => SUBMODULE(id1, name, TRIVIA(sp, sp2, @^), decls, contains, end_name, @^)
    ;

// The location of BLOCK DATA includes the separator after it
rule block_data -> ast_t*
    : KW_BLOCK KW_DATA name=id sp=sep decls=decl_statements end_blockdata
        sp2=sep                     => BLOCKDATA1(name, TRIVIA(sp, sp2, @$), decls, @$)
    | KW_BLOCK KW_DATA sp=sep decls=decl_statements end_blockdata sp2=sep
                                    => BLOCKDATA(TRIVIA(sp, sp2, @$), decls, @$)
    ;

// One item at the top level of a file
// A main program without the PROGRAM statement
rule main_program -> ast_t*
    : decls=decl_statements
        ( contains=contains_block end=end_program ^ sp=sep
            => implicit_program(decls, TRIVIA_AFTER(sp, @^), CONTAINS_END2(m_a, contains, end), @^)
        | end=end_program ^ sp=sep
            => implicit_program(decls, TRIVIA_AFTER(sp, @^), CONTAINS_END1(m_a, end), @^)
        )
    ;

rule unit -> ast_t*
    : program
    | submodule
    | module
    | block_data
    | subprogram_or_main
    ;

// ----------------------------------------------------------------------------
// Tokens
//
// Every token of the tokenizers is declared here, in the order of `enum
// yytokentype` (tokens.h, generated). `<member>` is the member of YYSTYPE
// that has the value of the token; a string such as "+" is another name of
// the token for the rules above. A %keyword is a token that is also a name
// (NAME).
//
// If this list is updated, update also token2text() in parser.cpp.


%token END_OF_FILE 0
%token TK_NEWLINE
%token <string> TK_NAME
%token <string> TK_DEF_OP
%token <int_suffix> TK_INTEGER
%token <n> TK_LABEL
%token <string> TK_REAL
%token <string> TK_BOZ_CONSTANT

%token TK_PLUS "+"
%token TK_MINUS "-"
%token TK_STAR "*"
%token TK_SLASH "/"
%token TK_COLON ":"
%token TK_SEMICOLON ";"
%token TK_COMMA ","
%token TK_EQUAL "="
%token TK_LPAREN "("
%token TK_RPAREN ")"
%token TK_LBRACKET "["
%token TK_RBRACKET "]"
%token TK_LBRACE "{"
%token TK_RBRACE "}"
%token TK_RBRACKET_OLD "/)"
%token TK_PERCENT "%"
%token TK_VBAR "|"
%token TK_QUESTION "?"

%token <str_prefix> TK_STRING
%token <string> TK_COMMENT
%token <string> TK_EOLCOMMENT
%token <string> TK_PRAGMA_DECL
%token <string> TK_OMP
%token <string> TK_OMP_END

%token TK_DBL_DOT ".."
%token TK_DBL_COLON "::"
%token TK_POW "**"
%token TK_CONCAT "//"
%token TK_ARROW "=>"
%token TK_COLON_EQUAL ":="

%token TK_EQ "=="
%token TK_NE "/="
%token TK_LT "<"
%token TK_LE "<="
%token TK_GT ">"
%token TK_GE ">="

%token TK_NOT ".not."
%token TK_AND ".and."
%token TK_OR ".or."
%token TK_XOR ".xor."
%token TK_EQV ".eqv."
%token TK_NEQV ".neqv."
%token TK_NIL ".nil."

%token <string> TK_TRUE ".true."
%token <string> TK_FALSE ".false."

%token <string> TK_FORMAT

// Keywords

%keyword <string> KW_ABSTRACT
%keyword <string> KW_ALL
%keyword <string> KW_ALLOCATABLE
%keyword <string> KW_ALLOCATE
%keyword <string> KW_ASSIGN
%keyword <string> KW_ASSIGNMENT
%keyword <string> KW_ASSOCIATE
%keyword <string> KW_ASYNCHRONOUS
%keyword <string> KW_BACKSPACE
%keyword <string> KW_BIND
%keyword <string> KW_BLOCK
%keyword <string> KW_BYTE
%keyword <string> KW_CALL
%keyword <string> KW_CASE
%keyword <string> KW_CHANGE
%token <string> KW_CHANGE_TEAM
%keyword <string> KW_CHARACTER
%keyword <string> KW_CLASS
%keyword <string> KW_CLOSE
%keyword <string> KW_CODIMENSION
%keyword <string> KW_COMMON
%keyword <string> KW_COMPLEX
%keyword <string> KW_CONCURRENT
%keyword <string> KW_CONTAINS
%keyword <string> KW_CONTIGUOUS
%keyword <string> KW_CONTINUE
%keyword <string> KW_CRITICAL
%keyword <string> KW_CYCLE
%keyword <string> KW_DATA
%keyword <string> KW_DEALLOCATE
%keyword <string> KW_DEFAULT
%keyword <string> KW_DEFERRED
%keyword <string> KW_DIMENSION
%keyword <string> KW_DO
%keyword <string> KW_DOWHILE
%keyword <string> KW_DOUBLE
%keyword <string> KW_DOUBLE_PRECISION
%keyword <string> KW_DOUBLE_COMPLEX
%keyword <string> KW_ELEMENTAL
%keyword <string> KW_ELSE
%keyword <string> KW_ELSEIF
%keyword <string> KW_ELSEWHERE

%keyword <string> KW_END

%token <string> KW_END_PROGRAM
%keyword <string> KW_ENDPROGRAM

%token <string> KW_END_MODULE
%keyword <string> KW_ENDMODULE

%token <string> KW_END_SUBMODULE
%keyword <string> KW_ENDSUBMODULE

%token <string> KW_END_BLOCK
%keyword <string> KW_ENDBLOCK

%token <string> KW_END_BLOCK_DATA
%keyword <string> KW_ENDBLOCKDATA

%token <string> KW_END_SUBROUTINE
%keyword <string> KW_ENDSUBROUTINE

%token <string> KW_END_FUNCTION
%keyword <string> KW_ENDFUNCTION

%token <string> KW_END_PROCEDURE
%keyword <string> KW_ENDPROCEDURE

%token <string> KW_END_ENUM
%keyword <string> KW_ENDENUM

%token <string> KW_END_SELECT
%keyword <string> KW_ENDSELECT

%token <string> KW_END_IF
%keyword <string> KW_ENDIF

%token <string> KW_END_INTERFACE
%keyword <string> KW_ENDINTERFACE

%token <string> KW_END_TYPE
%keyword <string> KW_ENDTYPE

%token <string> KW_END_ASSOCIATE
%keyword <string> KW_ENDASSOCIATE

%token <string> KW_END_FORALL
%keyword <string> KW_ENDFORALL

%token <string> KW_END_DO
%keyword <string> KW_ENDDO

%token <string> KW_END_WHERE
%keyword <string> KW_ENDWHERE

%token <string> KW_END_CRITICAL
%keyword <string> KW_ENDCRITICAL

%token <string> KW_END_FILE
%keyword <string> KW_ENDFILE

%token <string> KW_END_TEAM
%token <string> KW_ENDTEAM

%keyword <string> KW_ENTRY
%keyword <string> KW_ENUM
%keyword <string> KW_ENUMERATOR
%keyword <string> KW_EQUIVALENCE
%keyword <string> KW_ERRMSG
%keyword <string> KW_ERROR
%keyword <string> KW_EVENT
%keyword <string> KW_EXIT
%keyword <string> KW_EXTENDS
%keyword <string> KW_EXTENSIBLE
%keyword <string> KW_EXTERNAL
%keyword <string> KW_FILE
%keyword <string> KW_FINAL
%keyword <string> KW_FLUSH
%keyword <string> KW_FORALL
%keyword <string> KW_FORMATTED
%keyword <string> KW_FORM
%keyword <string> KW_FORM_TEAM
%keyword <string> KW_FUNCTION
%keyword <string> KW_GENERIC
%keyword <string> KW_GO
%keyword <string> KW_GOTO
%keyword <string> KW_IF
%keyword <string> KW_IMAGES
%keyword <string> KW_IMPLICIT
%keyword <string> KW_IMPORT
%keyword <string> KW_IMPURE
%keyword <string> KW_IN
%keyword <string> KW_INCLUDE
%keyword <string> KW_INOUT
%token <string> KW_IN_OUT
%keyword <string> KW_INQUIRE
%keyword <string> KW_INSTANTIATE
%keyword <string> KW_INTEGER
%keyword <string> KW_INTENT
%keyword <string> KW_INTERFACE
%keyword <string> KW_INTRINSIC
%keyword <string> KW_IS
%keyword <string> KW_KIND
%keyword <string> KW_LEN
%keyword <string> KW_LOCAL
%keyword <string> KW_LOCAL_INIT
%keyword <string> KW_LOGICAL
%keyword <string> KW_MEMORY
%keyword <string> KW_MODULE
%keyword <string> KW_MOLD
%keyword <string> KW_NAME
%keyword <string> KW_NAMELIST
%keyword <string> KW_NEW_INDEX
%keyword <string> KW_NOPASS
%keyword <string> KW_NON_INTRINSIC
%keyword <string> KW_NON_OVERRIDABLE
%keyword <string> KW_NON_RECURSIVE
%keyword <string> KW_NONE
%keyword <string> KW_NULLIFY
%keyword <string> KW_ONLY
%keyword <string> KW_OPEN
%keyword <string> KW_OPERATOR
%keyword <string> KW_OPTIONAL
%keyword <string> KW_OUT
%keyword <string> KW_PARAMETER
%keyword <string> KW_PASS
%keyword <string> KW_PAUSE
%keyword <string> KW_POINTER
%keyword <string> KW_POST
%keyword <string> KW_PRECISION
%keyword <string> KW_PRINT
%keyword <string> KW_PRIVATE
%keyword <string> KW_PROCEDURE
%keyword <string> KW_PROGRAM
%keyword <string> KW_PROTECTED
%keyword <string> KW_PUBLIC
%keyword <string> KW_PURE
%keyword <string> KW_QUIET
%keyword <string> KW_RANK
%keyword <string> KW_READ
%keyword <string> KW_REAL
%keyword <string> KW_RECURSIVE
%keyword <string> KW_REDUCE
%keyword <string> KW_RESULT
%keyword <string> KW_RETURN
%keyword <string> KW_REWIND
%keyword <string> KW_SAVE
%keyword <string> KW_SELECT
%keyword <string> KW_SELECT_CASE
%keyword <string> KW_SELECT_RANK
%keyword <string> KW_SELECT_TYPE
%keyword <string> KW_SEQUENCE
%keyword <string> KW_SHARED
%keyword <string> KW_SOURCE
%keyword <string> KW_STAT
%keyword <string> KW_STOP
%keyword <string> KW_SUBMODULE
%keyword <string> KW_SUBROUTINE
%keyword <string> KW_SYNC
%token <string> KW_SYNC_ALL
%token <string> KW_SYNC_IMAGES
%token <string> KW_SYNC_MEMORY
%token <string> KW_SYNC_TEAM
%keyword <string> KW_TARGET
%keyword <string> KW_TEAM
%keyword <string> KW_TEAM_NUMBER
%keyword <string> KW_REQUIREMENT
%keyword <string> KW_REQUIRE
%keyword <string> KW_TEMPLATE
%keyword <string> KW_THEN
%keyword <string> KW_TO
%keyword <string> KW_TYPE
%keyword <string> KW_UNFORMATTED
%keyword <string> KW_USE
%keyword <string> KW_VALUE
%keyword <string> KW_VOLATILE
%keyword <string> KW_WAIT
%keyword <string> KW_WHERE
%keyword <string> KW_WHILE
%keyword <string> KW_WRITE

// LFortran specific
%keyword <string> KW_LIST
%keyword <string> KW_SET
%keyword <string> KW_DICT
%keyword <string> KW_TUPLE
%token <string> KW_UNION_TYPE
%token <string> KW_END_UNION_TYPE
