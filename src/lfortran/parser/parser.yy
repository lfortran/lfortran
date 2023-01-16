%require "3.0"
%define api.pure
%define api.value.type {LCompilers::LFortran::YYSTYPE}
%param {LCompilers::LFortran::Parser &p}
%locations
%glr-parser
%expect    210 // shift/reduce conflicts
%expect-rr 175 // reduce/reduce conflicts

// Uncomment this to get verbose error messages
//%define parse.error verbose

/*
// Uncomment this to enable parser tracing. Then in the main code, set
// extern int yydebug;
// yydebug=1;
%define parse.trace
%printer { fprintf(yyo, "%s", $$.str().c_str()); } <string>
%printer { fprintf(yyo, "%d", $$); } <n>
%printer { std::cerr << "AST TYPE: " << $$->type; } <ast>
*/


%code requires // *.h
{
#include <lfortran/parser/parser.h>
}

%code // *.cpp
{

#include <lfortran/parser/parser.h>
#include <lfortran/parser/tokenizer.h>
#include <lfortran/parser/semantics.h>

int yylex(LCompilers::LFortran::YYSTYPE *yylval, YYLTYPE *yyloc,
    LCompilers::LFortran::Parser &p)
{
    if (p.fixed_form) {
        return p.f_tokenizer.lex(p.m_a, *yylval, *yyloc, p.diag);
    } else {
        return p.m_tokenizer.lex(p.m_a, *yylval, *yyloc, p.diag);
    }
} // ylex

void yyerror(YYLTYPE *yyloc, LCompilers::LFortran::Parser &p,
    const std::string &msg)
{
    p.handle_yyerror(*yyloc, msg);
}

#define YYLLOC_DEFAULT(Current, Rhs, N)                                 \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).first   = YYRHSLOC (Rhs, 1).first;                  \
          (Current).last    = YYRHSLOC (Rhs, N).last;                   \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first   = (Current).last   =                        \
            YYRHSLOC (Rhs, 0).last;                                     \
        }                                                               \
    while (0)

} // code


// -----------------------------------------------------------------------------
// List of tokens
// All tokens that we use (including "+" and such) are declared here first
// using the %token line. Each token will end up a member of the "enum
// yytokentype" in parser.tab.hh. Tokens can have a string equivalent (such as
// "+" for TK_PLUS) that is used later in the file to simplify reading it, but
// it is equivalent to TK_PLUS. Bison also allows so called "character token
// type" which are specified using single quotes (and that bypass the %token
// definitions), and those are not used here, and we test that the whole file
// does not contain any single quotes to ensure that.
//
// If this list is updated, update also token2text() in parser.cpp.

// Terminal tokens

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
%token TK_RBRACKET_OLD "/)"
%token TK_PERCENT "%"
%token TK_VBAR "|"

%token <string> TK_STRING
%token <string> TK_COMMENT
%token <string> TK_EOLCOMMENT

%token TK_DBL_DOT ".."
%token TK_DBL_COLON "::"
%token TK_POW "**"
%token TK_CONCAT "//"
%token TK_ARROW "=>"

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

%token TK_TRUE ".true."
%token TK_FALSE ".false."

%token <string> TK_FORMAT

// Terminal tokens: semi-reserved keywords

%token <string> KW_ABSTRACT
%token <string> KW_ALL
%token <string> KW_ALLOCATABLE
%token <string> KW_ALLOCATE
%token <string> KW_ASSIGN
%token <string> KW_ASSIGNMENT
%token <string> KW_ASSOCIATE
%token <string> KW_ASYNCHRONOUS
%token <string> KW_BACKSPACE
%token <string> KW_BIND
%token <string> KW_BLOCK
%token <string> KW_CALL
%token <string> KW_CASE
%token <string> KW_CHANGE
%token <string> KW_CHANGE_TEAM
%token <string> KW_CHARACTER
%token <string> KW_CLASS
%token <string> KW_CLOSE
%token <string> KW_CODIMENSION
%token <string> KW_COMMON
%token <string> KW_COMPLEX
%token <string> KW_CONCURRENT
%token <string> KW_CONTAINS
%token <string> KW_CONTIGUOUS
%token <string> KW_CONTINUE
%token <string> KW_CRITICAL
%token <string> KW_CYCLE
%token <string> KW_DATA
%token <string> KW_DEALLOCATE
%token <string> KW_DEFAULT
%token <string> KW_DEFERRED
%token <string> KW_DIMENSION
%token <string> KW_DO
%token <string> KW_DOWHILE
%token <string> KW_DOUBLE
%token <string> KW_DOUBLE_PRECISION
%token <string> KW_DOUBLE_COMPLEX
%token <string> KW_ELEMENTAL
%token <string> KW_ELSE
%token <string> KW_ELSEIF
%token <string> KW_ELSEWHERE

%token <string> KW_END

%token <string> KW_END_PROGRAM
%token <string> KW_ENDPROGRAM

%token <string> KW_END_MODULE
%token <string> KW_ENDMODULE

%token <string> KW_END_SUBMODULE
%token <string> KW_ENDSUBMODULE

%token <string> KW_END_BLOCK
%token <string> KW_ENDBLOCK

%token <string> KW_END_BLOCK_DATA
%token <string> KW_ENDBLOCKDATA

%token <string> KW_END_SUBROUTINE
%token <string> KW_ENDSUBROUTINE

%token <string> KW_END_FUNCTION
%token <string> KW_ENDFUNCTION

%token <string> KW_END_PROCEDURE
%token <string> KW_ENDPROCEDURE

%token <string> KW_END_ENUM
%token <string> KW_ENDENUM

%token <string> KW_END_SELECT
%token <string> KW_ENDSELECT

%token <string> KW_END_IF
%token <string> KW_ENDIF

%token <string> KW_END_INTERFACE
%token <string> KW_ENDINTERFACE

%token <string> KW_END_TYPE
%token <string> KW_ENDTYPE

%token <string> KW_END_ASSOCIATE
%token <string> KW_ENDASSOCIATE

%token <string> KW_END_FORALL
%token <string> KW_ENDFORALL

%token <string> KW_END_DO
%token <string> KW_ENDDO

%token <string> KW_END_WHERE
%token <string> KW_ENDWHERE

%token <string> KW_END_CRITICAL
%token <string> KW_ENDCRITICAL

%token <string> KW_END_FILE
%token <string> KW_ENDFILE

%token <string> KW_END_TEAM
%token <string> KW_ENDTEAM

%token <string> KW_ENTRY
%token <string> KW_ENUM
%token <string> KW_ENUMERATOR
%token <string> KW_EQUIVALENCE
%token <string> KW_ERRMSG
%token <string> KW_ERROR
%token <string> KW_EVENT
%token <string> KW_EXIT
%token <string> KW_EXTENDS
%token <string> KW_EXTERNAL
%token <string> KW_FILE
%token <string> KW_FINAL
%token <string> KW_FLUSH
%token <string> KW_FORALL
%token <string> KW_FORMATTED
%token <string> KW_FORM
%token <string> KW_FORM_TEAM
%token <string> KW_FUNCTION
%token <string> KW_GENERIC
%token <string> KW_GO
%token <string> KW_GOTO
%token <string> KW_IF
%token <string> KW_IMAGES
%token <string> KW_IMPLICIT
%token <string> KW_IMPORT
%token <string> KW_IMPURE
%token <string> KW_IN
%token <string> KW_INCLUDE
%token <string> KW_INOUT
%token <string> KW_IN_OUT
%token <string> KW_INQUIRE
%token <string> KW_INSTANTIATE
%token <string> KW_INTEGER
%token <string> KW_INTENT
%token <string> KW_INTERFACE
%token <string> KW_INTRINSIC
%token <string> KW_IS
%token <string> KW_KIND
%token <string> KW_LEN
%token <string> KW_LOCAL
%token <string> KW_LOCAL_INIT
%token <string> KW_LOGICAL
%token <string> KW_MEMORY
%token <string> KW_MODULE
%token <string> KW_MOLD
%token <string> KW_NAME
%token <string> KW_NAMELIST
%token <string> KW_NEW_INDEX
%token <string> KW_NOPASS
%token <string> KW_NON_INTRINSIC
%token <string> KW_NON_OVERRIDABLE
%token <string> KW_NON_RECURSIVE
%token <string> KW_NONE
%token <string> KW_NULLIFY
%token <string> KW_ONLY
%token <string> KW_OPEN
%token <string> KW_OPERATOR
%token <string> KW_OPTIONAL
%token <string> KW_OUT
%token <string> KW_PARAMETER
%token <string> KW_PASS
%token <string> KW_POINTER
%token <string> KW_POST
%token <string> KW_PRECISION
%token <string> KW_PRINT
%token <string> KW_PRIVATE
%token <string> KW_PROCEDURE
%token <string> KW_PROGRAM
%token <string> KW_PROTECTED
%token <string> KW_PUBLIC
%token <string> KW_PURE
%token <string> KW_QUIET
%token <string> KW_RANK
%token <string> KW_READ
%token <string> KW_REAL
%token <string> KW_RECURSIVE
%token <string> KW_REDUCE
%token <string> KW_RESULT
%token <string> KW_RETURN
%token <string> KW_REWIND
%token <string> KW_SAVE
%token <string> KW_SELECT
%token <string> KW_SELECT_CASE
%token <string> KW_SELECT_RANK
%token <string> KW_SELECT_TYPE
%token <string> KW_SEQUENCE
%token <string> KW_SHARED
%token <string> KW_SOURCE
%token <string> KW_STAT
%token <string> KW_STOP
%token <string> KW_SUBMODULE
%token <string> KW_SUBROUTINE
%token <string> KW_SYNC
%token <string> KW_SYNC_ALL
%token <string> KW_SYNC_IMAGES
%token <string> KW_SYNC_MEMORY
%token <string> KW_SYNC_TEAM
%token <string> KW_TARGET
%token <string> KW_TEAM
%token <string> KW_TEAM_NUMBER
%token <string> KW_REQUIREMENT
%token <string> KW_REQUIRES
%token <string> KW_TEMPLATE
%token <string> KW_THEN
%token <string> KW_TO
%token <string> KW_TYPE
%token <string> KW_UNFORMATTED
%token <string> KW_USE
%token <string> KW_VALUE
%token <string> KW_VOLATILE
%token <string> KW_WAIT
%token <string> KW_WHERE
%token <string> KW_WHILE
%token <string> KW_WRITE

// Nonterminal tokens

%type <ast> expr
%type <vec_ast> expr_list
%type <vec_ast> expr_list_opt
%type <ast> id
%type <vec_ast> id_list
%type <vec_ast> id_list_opt
%type <ast> script_unit
%type <ast> module
%type <ast> submodule
%type <ast> block_data
%type <ast> decl
%type <vec_ast> decl_star
%type <ast> instantiate
%type <ast> interface_decl
%type <ast> interface_stmt
%type <ast> derived_type_decl
%type <ast> template_decl
%type <ast> requirement_decl
%type <ast> requires_decl
%type <ast> enum_decl
%type <ast> program
%type <ast> subroutine
%type <ast> procedure
%type <ast> sub_or_func
%type <vec_ast> sub_args
%type <ast> function
%type <ast> use_statement
%type <ast> use_statement1
%type <vec_ast> use_statement_star
%type <ast> use_symbol
%type <vec_ast> use_symbol_list
%type <ast> use_modifier
%type <vec_ast> use_modifiers
%type <vec_ast> use_modifier_list
%type <vec_ast> var_decl_star
%type <vec_var_sym> var_sym_decl_list
%type <ast> var_decl
%type <ast> decl_spec
%type <var_sym> var_sym_decl
%type <vec_dim> array_comp_decl_list
%type <vec_codim> coarray_comp_decl_list
%type <fnarg> fnarray_arg
%type <vec_fnarg> fnarray_arg_list_opt
%type <coarrayarg> coarray_arg
%type <vec_coarrayarg> coarray_arg_list
%type <dim> array_comp_decl
%type <codim> coarray_comp_decl
%type <ast> var_type
%type <ast> fn_mod
%type <vec_ast> fn_mod_plus
%type <vec_ast> var_modifiers
%type <vec_ast> enum_var_modifiers
%type <vec_ast> var_modifier_list
%type <ast> var_modifier
%type <ast> statement
%type <ast> statement1
%type <ast> single_line_statement
%type <ast> multi_line_statement
%type <ast> multi_line_statement0
%type <ast> assign_statement
%type <ast> assignment_statement
%type <ast> goto_statement
%type <ast> associate_statement
%type <ast> associate_block
%type <ast> block_statement
%type <ast> subroutine_call
%type <ast> allocate_statement
%type <ast> deallocate_statement
%type <ast> nullify_statement
%type <ast> print_statement
%type <ast> format
%type <ast> open_statement
%type <ast> flush_statement
%type <ast> close_statement
%type <ast> write_statement
%type <ast> read_statement
%type <ast> include_statement
%type <ast> inquire_statement
%type <ast> rewind_statement
%type <ast> backspace_statement
%type <ast> endfile_statement
%type <ast> if_statement
%type <ast> if_statement_single
%type <ast> if_block
%type <ast> elseif_block
%type <ast> where_statement
%type <ast> where_statement_single
%type <ast> where_block
%type <ast> elsewhere_block
%type <ast> select_statement
%type <ast> select_type_statement
%type <vec_ast> select_type_body_statements
%type <ast> select_type_body_statement
%type <ast> select_rank_statement
%type <vec_ast> select_rank_case_stmts
%type <ast> select_rank_case_stmt
%type <vec_ast> case_statements
%type <ast> case_statement
%type <vec_ast> case_conditions
%type <ast> case_condition
%type <ast> while_statement
%type <ast> critical_statement
%type <ast> change_team_statement
%type <vec_ast> coarray_association_list
%type <ast> coarray_association
%type <ast> do_statement
%type <ast> forall_statement
%type <ast> forall_statement_single
%type <vec_ast> concurrent_locality_star
%type <ast> concurrent_locality
%type <reduce_op_type> reduce_op
%type <ast> exit_statement
%type <ast> return_statement
%type <ast> cycle_statement
%type <ast> continue_statement
%type <ast> stop_statement
%type <ast> entry_statement
%type <ast> error_stop_statement
%type <ast> event_post_statement
%type <ast> event_wait_statement
%type <ast> sync_all_statement
%type <ast> sync_images_statement
%type <ast> sync_memory_statement
%type <ast> sync_team_statement
%type <vec_ast> event_wait_spec_list
%type <ast> event_wait_spec
%type <vec_ast> sync_stat_list
%type <vec_ast> event_post_stat_list
%type <ast> sync_stat
%type <ast> format_statement
%type <ast> data_statement
%type <ast> form_team_statement
%type <ast> decl_statement
%type <vec_ast> statements
%type <vec_ast> decl_statements
%type <vec_ast> contains_block_opt
%type <vec_ast> sub_or_func_plus
%type <ast> result_opt
%type <ast> result
%type <string> inout
%type <vec_ast> concurrent_control_list
%type <ast> concurrent_control
%type <vec_var_sym> named_constant_def_list
%type <var_sym> named_constant_def
%type <vec_var_sym> common_block_list
%type <var_sym> common_block
%type <vec_ast> data_set_list
%type <ast> data_set
%type <vec_ast> data_object_list
%type <vec_ast> data_stmt_value_list
%type <ast> data_stmt_repeat
%type <ast> data_stmt_constant
%type <ast> data_object
%type <ast> integer_type
%type <vec_kind_arg> kind_arg_list
%type <kind_arg> kind_arg2
%type <vec_ast> interface_body
%type <ast> interface_item
%type <interface_op_type> operator_type
%type <ast> write_arg
%type <argstarkw> write_arg2
%type <vec_argstarkw> write_arg_list
%type <struct_member> struct_member
%type <vec_struct_member> struct_member_star
%type <ast> bind
%type <ast> bind_opt
%type <vec_ast> import_statement_star
%type <ast> import_statement
%type <ast> implicit_statement
%type <vec_ast> implicit_statement_star
%type <ast> implicit_none_spec
%type <vec_ast> implicit_none_spec_list
%type <ast> letter_spec
%type <vec_ast> letter_spec_list
%type <ast> procedure_decl
%type <vec_ast> access_spec_list
%type <ast> access_spec
%type <ast> proc_modifier
%type <vec_ast> procedure_list
%type <vec_ast> derived_type_contains_opt
%type <vec_ast> proc_modifiers
%type <vec_ast> proc_modifier_list
%type <equi> equivalence_set
%type <vec_equi> equivalence_set_list
%type <ast> sep_one
%type <vec_ast> sep

// Precedence

%left TK_DEF_OP
%left ".eqv." ".neqv."
%left ".or." ".xor."
%left ".and."
%precedence ".not."
%left "==" "/=" "<" "<=" ">" ">="
%left "//"
%left "-" "+"
%left "*" "/"
%precedence UMINUS
%right "**"

%start units

%%

// The order of rules does not matter in Bison (unlike in ANTLR). The
// precedence is specified not by the order but by %left and %right directives
// as well as with %dprec.

// ----------------------------------------------------------------------------
// Top level rules to be used for parsing.

// Higher %dprec means higher precedence

units
    : units script_unit  %dprec 9  { RESULT($2); }
    | script_unit        %dprec 10 { RESULT($1); }
    | sep
    ;

script_unit
    : module
    | submodule
    | block_data
    | program
    | subroutine
    | procedure
    | function
    | use_statement
    | implicit_statement
    | var_decl
    | statement          %dprec 7
    | expr sep           %dprec 8
    ;

// ----------------------------------------------------------------------------
// Module definitions
//
// * private/public blocks
// * interface blocks
//

module
    : KW_MODULE id sep use_statement_star implicit_statement_star
        decl_star contains_block_opt end_module sep {
            $$ = MODULE($2, TRIVIA($3, $9, @$), $4, $5, $6, $7, @$); }
    ;

submodule
    : KW_SUBMODULE "(" id ")" id sep use_statement_star implicit_statement_star
        decl_star contains_block_opt end_submodule sep {
            $$ = SUBMODULE($3, $5, TRIVIA($6, $12, @$), $7, $8, $9, $10, @$); }
    | KW_SUBMODULE "(" id ":" id ")" id sep use_statement_star
        implicit_statement_star decl_star
        contains_block_opt end_submodule sep {
            $$ = SUBMODULE1($3, $5, $7, TRIVIA($8, $14, @$), $9, $10, $11, $12, @$); }
    ;

block_data
    : KW_BLOCK KW_DATA sep use_statement_star implicit_statement_star
        decl_statements end_blockdata sep {
            $$ = BLOCKDATA(TRIVIA($3, $8, @$), $4, $5, SPLIT_DECL(p.m_a, $6),
                SPLIT_STMT(p.m_a, $6), @$); }
    | KW_BLOCK KW_DATA id sep use_statement_star implicit_statement_star
        decl_statements end_blockdata sep {
            $$ = BLOCKDATA1($3, TRIVIA($4, $9, @$), $5, $6, SPLIT_DECL(p.m_a, $7),
                SPLIT_STMT(p.m_a, $7), @$); }
    ;

interface_decl
    : interface_stmt sep interface_body endinterface sep {
            $$ = INTERFACE($1, TRIVIA($2, $5, @$), $3, @$); }
    ;

interface_stmt
    : KW_INTERFACE { $$ = INTERFACE_HEADER(@$); }
    | KW_INTERFACE id { $$ = INTERFACE_HEADER_NAME($2, @$); }
    | KW_INTERFACE KW_ASSIGNMENT "(" "=" ")" {
        $$ = INTERFACE_HEADER_ASSIGNMENT(@$); }
    | KW_INTERFACE KW_OPERATOR "(" operator_type ")" {
        $$ = INTERFACE_HEADER_OPERATOR($4, @$); }
    | KW_INTERFACE KW_OPERATOR "(" "/)" {
        $$ = INTERFACE_HEADER_OPERATOR(OPERATOR(DIV, @$), @$); }
    | KW_INTERFACE KW_OPERATOR "(" TK_DEF_OP ")" {
        $$ = INTERFACE_HEADER_DEFOP($4, @$); }
    | KW_ABSTRACT KW_INTERFACE { $$ = ABSTRACT_INTERFACE_HEADER(@$); }
    | KW_INTERFACE KW_WRITE "(" id ")" { $$ = INTERFACE_HEADER_WRITE($4, @$); }
    | KW_INTERFACE KW_READ "(" id ")" { $$ = INTERFACE_HEADER_READ($4, @$); }
    ;

endinterface
    : endinterface0
    | endinterface0 id
    | endinterface0 KW_ASSIGNMENT "(" "=" ")"
    | endinterface0 KW_OPERATOR "(" operator_type ")"
    | endinterface0 KW_OPERATOR "(" "/)"
    | endinterface0 KW_OPERATOR "(" TK_DEF_OP ")"
    ;

endinterface0
    : KW_END_INTERFACE
    | KW_ENDINTERFACE
    ;


interface_body
    : interface_body interface_item { $$ = $1; LIST_ADD($$, $2); }
    | %empty { LIST_NEW($$); }
    ;

interface_item
    : fn_mod_plus KW_PROCEDURE id_list sep {
        $$ = INTERFACE_MODULE_PROC1($1, $3, TRIVIA_AFTER($4, @$), @$); }
    | fn_mod_plus KW_PROCEDURE "::" id_list sep {
        $$ = INTERFACE_MODULE_PROC1($1, $4, TRIVIA_AFTER($5, @$), @$); }
    | KW_PROCEDURE id_list sep {
        $$ = INTERFACE_MODULE_PROC($2, TRIVIA_AFTER($3, @$), @$); }
    | KW_PROCEDURE "::" id_list sep {
        $$ = INTERFACE_MODULE_PROC($3, TRIVIA_AFTER($4, @$), @$); }
    | subroutine {
        $$ = INTERFACE_PROC($1, @$); }
    | function {
        $$ = INTERFACE_PROC($1, @$); }
    ;

enum_decl
    : KW_ENUM enum_var_modifiers sep var_decl_star endenum sep {
        $$ = ENUM($2, TRIVIA($3, $6, @$), $4, @$); }
    ;

endenum
    : KW_END_ENUM
    | KW_ENDENUM
    ;

enum_var_modifiers
    : %empty { LIST_NEW($$); }
    | var_modifier_list { $$ = $1; }
    ;

derived_type_decl
    : KW_TYPE var_modifiers id sep var_decl_star
        derived_type_contains_opt end_type sep {
            $$ = DERIVED_TYPE($2, $3, TRIVIA($4, $8, @$), $5, $6, @$); }
    | KW_TYPE var_modifiers id "(" id_list ")" sep var_decl_star
        derived_type_contains_opt end_type sep {
            $$ = DERIVED_TYPE1($2, $3, $5, TRIVIA($7, $11, @$), $8, $9, @$); }
    ;

template_decl
    : KW_TEMPLATE id "(" id_list ")" sep decl_star
        contains_block_opt KW_END KW_TEMPLATE sep {
            $$ = TEMPLATE($2, $4, $7, $8, @$); }
    ;

requirement_decl
    : KW_REQUIREMENT id "(" id_list ")" sep decl_star
        sub_or_func_plus KW_END KW_REQUIREMENT sep {
            $$ = REQUIREMENT($2, $4, $7, $8, @$); }
    ;

requires_decl
    : KW_REQUIRES id "(" id_list ")" sep {
        $$ = REQUIRES($2, $4, @$); }
    ;

instantiate
    : KW_INSTANTIATE id "(" id_list ")" "," KW_ONLY ":" use_symbol_list sep {
        $$ = INSTANTIATE($2, $4, $9, @$); }
    ;

end_type
    : KW_END_TYPE id_opt
    | KW_ENDTYPE id_opt
    ;

derived_type_contains_opt
    : KW_CONTAINS sep procedure_list { $$ = $3; }
    | %empty { LIST_NEW($$); }
    ;

procedure_list
    : procedure_list procedure_decl { $$ = $1; LIST_ADD($$, $2); }
    | procedure_decl { LIST_NEW($$); LIST_ADD($$, $1); }
    ;

procedure_decl
    : KW_PROCEDURE proc_modifiers use_symbol_list sep {
            $$ = DERIVED_TYPE_PROC($2, $3, TRIVIA_AFTER($4, @$), @$); }
    | KW_PROCEDURE "(" id ")" proc_modifiers use_symbol_list sep {
            $$ = DERIVED_TYPE_PROC1($3, $5, $6, TRIVIA_AFTER($7, @$), @$); }
    | KW_GENERIC access_spec_list KW_OPERATOR "(" operator_type ")" "=>" id_list sep {
            $$ = GENERIC_OPERATOR($2, $5, $8, TRIVIA_AFTER($9, @$), @$); }
    | KW_GENERIC access_spec_list KW_OPERATOR "(" "/)" "=>" id_list sep {
            $$ = GENERIC_OPERATOR($2, OPERATOR(DIV, @$), $7, TRIVIA_AFTER($8, @$), @$); }
    | KW_GENERIC access_spec_list KW_OPERATOR "(" TK_DEF_OP ")" "=>" id_list sep {
            $$ = GENERIC_DEFOP($2, $5, $8, TRIVIA_AFTER($9, @$), @$); }
    | KW_GENERIC access_spec_list KW_ASSIGNMENT "(" "=" ")" "=>" id_list sep {
            $$ = GENERIC_ASSIGNMENT($2, $8, TRIVIA_AFTER($9, @$), @$); }
    | KW_GENERIC access_spec_list id "=>" id_list sep {
            $$ = GENERIC_NAME($2, $3, $5, TRIVIA_AFTER($6, @$), @$); }
    | KW_GENERIC access_spec_list KW_WRITE "(" id ")" "=>" id_list sep {
            $$ = GENERIC_WRITE($2, $5, $8, TRIVIA_AFTER($9, @$), @$); }
    | KW_GENERIC access_spec_list KW_READ "(" id ")" "=>" id_list sep {
            $$ = GENERIC_READ($2, $5, $8, TRIVIA_AFTER($9, @$), @$); }
    | KW_FINAL "::" id sep { $$ = FINAL_NAME($3, TRIVIA_AFTER($4, @$), @$); }
    | KW_PRIVATE sep { $$ = PRIVATE(Private, TRIVIA_AFTER($2, @$), @$); }
    ;

access_spec_list
    : "::" { LIST_NEW($$); }
    | access_spec "::" { LIST_NEW($$); LIST_ADD($$, $1); }
    ;

access_spec
    : "," KW_PRIVATE  { $$ = SIMPLE_ATTR(Private, @$); }
    | "," KW_PUBLIC { $$ = SIMPLE_ATTR(Public, @$); }
    ;

operator_type
    : "+"      { $$ = OPERATOR(PLUS, @$); }
    | "-"      { $$ = OPERATOR(MINUS, @$); }
    | "*"      { $$ = OPERATOR(STAR, @$); }
    | "/"      { $$ = OPERATOR(DIV, @$); }
    | "**"     { $$ = OPERATOR(POW, @$); }
    | "=="     { $$ = OPERATOR(EQ, @$); }
    | "/="     { $$ = OPERATOR(NOTEQ, @$); }
    | ">"      { $$ = OPERATOR(GT, @$); }
    | ">="     { $$ = OPERATOR(GTE, @$); }
    | "<"      { $$ = OPERATOR(LT, @$); }
    | "<="     { $$ = OPERATOR(LTE, @$); }
    | "//"     { $$ = OPERATOR(CONCAT, @$); }
    | ".not."  { $$ = OPERATOR(NOT, @$); }
    | ".and."  { $$ = OPERATOR(AND, @$); }
    | ".or."   { $$ = OPERATOR(OR, @$); }
    | ".xor."  { $$ = OPERATOR(XOR, @$); }
    | ".eqv."  { $$ = OPERATOR(EQV, @$); }
    | ".neqv." { $$ = OPERATOR(NEQV, @$); }
    ;

proc_modifiers
    : %empty { LIST_NEW($$); }
    | "::" { LIST_NEW($$); }
    | proc_modifier_list "::" { $$ = $1; }
    ;

proc_modifier_list
    : proc_modifier_list "," proc_modifier { $$ = $1; LIST_ADD($$, $3); }
    | "," proc_modifier { LIST_NEW($$); LIST_ADD($$, $2); }
    ;

proc_modifier
    : KW_PRIVATE  { $$ = SIMPLE_ATTR(Private, @$); }
    | KW_PUBLIC { $$ = SIMPLE_ATTR(Public, @$); }
    | KW_PASS { $$ = PASS(nullptr, @$); }
    | KW_PASS "(" id ")" { $$ = PASS($3, @$); }
    | KW_NOPASS { $$ = SIMPLE_ATTR(NoPass, @$); }
    | KW_DEFERRED { $$ = SIMPLE_ATTR(Deferred, @$); }
    | KW_NON_OVERRIDABLE { $$ = SIMPLE_ATTR(NonDeferred, @$); }
    ;


// ----------------------------------------------------------------------------
// Subroutine/Procedure/functions/program definitions


program
    : KW_PROGRAM id sep use_statement_star implicit_statement_star decl_statements
        contains_block_opt end_program sep {
      LLOC(@$, @9); $$ = PROGRAM($2, TRIVIA($3, $9, @$), $4, $5, $6, $7, @$); }
    ;

end_program
    : KW_END_PROGRAM id_opt
    | KW_ENDPROGRAM id_opt
    | KW_END
    ;

end_module
    : KW_END_MODULE id_opt
    | KW_ENDMODULE id_opt
    | KW_END
    ;

end_submodule
    : KW_END_SUBMODULE id_opt
    | KW_ENDSUBMODULE id_opt
    | KW_END
    ;

end_blockdata
    : KW_END_BLOCK_DATA id_opt
    | KW_ENDBLOCKDATA id_opt
    | KW_END
    ;

end_subroutine
    : KW_END_SUBROUTINE id_opt
    | KW_ENDSUBROUTINE id_opt
    | KW_END
    ;

end_procedure
    : KW_END_PROCEDURE id_opt
    | KW_ENDPROCEDURE id_opt
    | KW_END
    ;

end_function
    : KW_END_FUNCTION id_opt
    | KW_ENDFUNCTION id_opt
    | KW_END
    ;

end_associate
    : KW_END_ASSOCIATE
    | KW_ENDASSOCIATE
    ;

end_block
    : KW_END_BLOCK
    | KW_ENDBLOCK
    ;

end_select
    : KW_END_SELECT
    | KW_ENDSELECT
    ;

end_critical
    : KW_END_CRITICAL
    | KW_ENDCRITICAL
    ;

end_team
    : KW_END_TEAM
    | KW_ENDTEAM
    ;

subroutine
    : KW_SUBROUTINE id sub_args bind_opt sep use_statement_star
    import_statement_star implicit_statement_star decl_statements
        contains_block_opt
        end_subroutine sep {
            LLOC(@$, @12); $$ = SUBROUTINE($2, $3, $4, TRIVIA($5, $12, @$), $6,
                $7, $8, SPLIT_DECL(p.m_a, $9), SPLIT_STMT(p.m_a, $9), $10, @$); }
    | fn_mod_plus KW_SUBROUTINE id sub_args bind_opt sep use_statement_star
    import_statement_star implicit_statement_star decl_statements
        contains_block_opt
        end_subroutine sep {
            LLOC(@$, @13); $$ = SUBROUTINE1($1, $3, $4, $5, TRIVIA($6, $13, @$),
                $7, $8, $9, SPLIT_DECL(p.m_a, $10), SPLIT_STMT(p.m_a, $10), $11, @$); }
    ;

procedure
    : fn_mod_plus KW_PROCEDURE id sub_args sep use_statement_star
    import_statement_star implicit_statement_star decl_statements
        contains_block_opt
        end_procedure sep {
            LLOC(@$, @12); $$ = PROCEDURE($1, $3, $4, TRIVIA($5, $12, @$), $6,
                $7, $8, SPLIT_DECL(p.m_a, $9), SPLIT_STMT(p.m_a, $9), $10, @$); }
    ;

function
    : KW_FUNCTION id "(" id_list_opt ")"
        sep use_statement_star import_statement_star implicit_statement_star decl_statements
        contains_block_opt
        end_function sep {
            LLOC(@$, @13); $$ = FUNCTION0($2, $4, nullptr, nullptr,
                TRIVIA($6, $13, @$), $7, $8, $9, SPLIT_DECL(p.m_a, $10),
                SPLIT_STMT(p.m_a, $10), $11, @$); }
    | KW_FUNCTION id "(" id_list_opt ")"
        bind
        result_opt
        sep use_statement_star import_statement_star implicit_statement_star decl_statements
        contains_block_opt
        end_function sep {
            LLOC(@$, @15); $$ = FUNCTION0($2, $4, $7, $6, TRIVIA($8, $15, @$),
                $9, $10, $11, SPLIT_DECL(p.m_a, $12), SPLIT_STMT(p.m_a, $12), $13, @$); }
    | KW_FUNCTION id "(" id_list_opt ")"
        result
        bind_opt
        sep use_statement_star import_statement_star implicit_statement_star decl_statements
        contains_block_opt
        end_function sep {
            LLOC(@$, @15); $$ = FUNCTION0($2, $4, $6, $7, TRIVIA($8, $15, @$),
                $9, $10, $11, SPLIT_DECL(p.m_a, $12), SPLIT_STMT(p.m_a, $12), $13, @$); }
    | fn_mod_plus KW_FUNCTION id "(" id_list_opt ")"
        sep use_statement_star import_statement_star implicit_statement_star decl_statements
        contains_block_opt
        end_function sep {
            LLOC(@$, @14); $$ = FUNCTION($1, $3, $5, nullptr, nullptr,
                TRIVIA($7, $14, @$), $8, $9, $10, SPLIT_DECL(p.m_a, $11),
                SPLIT_STMT(p.m_a, $11), $12, @$); }
    | fn_mod_plus KW_FUNCTION id "(" id_list_opt ")"
        bind
        result_opt
        sep use_statement_star import_statement_star implicit_statement_star decl_statements
        contains_block_opt
        end_function sep {
            LLOC(@$, @16); $$ = FUNCTION($1, $3, $5, $8, $7, TRIVIA($9, $16, @$),
                $10, $11, $12, SPLIT_DECL(p.m_a, $13), SPLIT_STMT(p.m_a, $13), $14, @$); }
    | fn_mod_plus KW_FUNCTION id "(" id_list_opt ")"
        result
        bind_opt
        sep use_statement_star import_statement_star implicit_statement_star decl_statements
        contains_block_opt
        end_function sep {
            LLOC(@$, @16); $$ = FUNCTION($1, $3, $5, $7, $8, TRIVIA($9, $16, @$),
                $10, $11, $12, SPLIT_DECL(p.m_a, $13), SPLIT_STMT(p.m_a, $13), $14, @$); }
    ;

fn_mod_plus
    : fn_mod_plus fn_mod { $$ = $1; LIST_ADD($$, $2); }
    | fn_mod { LIST_NEW($$); LIST_ADD($$, $1); }
    ;

fn_mod
    : var_type { $$ = $1; }
    | KW_ELEMENTAL { $$ = SIMPLE_ATTR(Elemental, @$); }
    | KW_IMPURE { $$ = SIMPLE_ATTR(Impure, @$); }
    | KW_MODULE { $$ = SIMPLE_ATTR(Module, @$); }
    | KW_PURE { $$ = SIMPLE_ATTR(Pure, @$); }
    | KW_RECURSIVE {  $$ = SIMPLE_ATTR(Recursive, @$); }
    ;

decl_star
    : decl_star decl { $$ = $1; LIST_ADD($$, $2); }
    | %empty { LIST_NEW($$); }

decl
    : var_decl
    | interface_decl
    | derived_type_decl
    | template_decl
    | requirement_decl
    | requires_decl
    | enum_decl
    ;

contains_block_opt
    : KW_CONTAINS sep sub_or_func_plus { $$ = $3; }
    | KW_CONTAINS sep { LIST_NEW($$); }
    | %empty { LIST_NEW($$); }
    ;

sub_or_func_plus
    : sub_or_func_plus sub_or_func { LIST_ADD($$, $2); }
    | sub_or_func { LIST_NEW($$); LIST_ADD($$, $1); }
    ;

sub_or_func
    : subroutine
    | function
    | procedure
    ;

sub_args
    : "(" id_list_opt ")" { $$ = $2; }
    | %empty { LIST_NEW($$); }
    ;

bind_opt
    : bind { $$ = $1; }
    | %empty { $$ = nullptr; }
    ;

bind
    : KW_BIND "(" write_arg_list ")" { $$ = BIND2($3, @$); }
    ;

result_opt
    : result { $$ = $1; }
    | %empty { $$ = nullptr; }
    ;

result
    : KW_RESULT "(" id ")" { $$ = $3; }
    ;

implicit_statement_star
    : implicit_statement_star implicit_statement { $$ = $1; LIST_ADD($$, $2); }
    | %empty { LIST_NEW($$); }
    ;

implicit_statement
    : KW_IMPLICIT KW_NONE sep { $$ = IMPLICIT_NONE(TRIVIA_AFTER($3, @$), @$); }
    | KW_IMPLICIT KW_NONE "(" implicit_none_spec_list ")" sep {
            $$ = IMPLICIT_NONE2($4, TRIVIA_AFTER($6, @$), @$); }
    | KW_IMPLICIT KW_INTEGER "(" letter_spec_list ")" sep {
            $$ = IMPLICIT(ATTR_TYPE(Integer, @$), $4, TRIVIA_AFTER($6, @$), @$); }
    | KW_IMPLICIT KW_INTEGER "*" TK_INTEGER "(" letter_spec_list ")" sep {
            $$ = IMPLICIT(ATTR_TYPE_INT(Integer, $4, @$), $6, TRIVIA_AFTER($8, @$), @$); }
    | KW_IMPLICIT KW_INTEGER "(" TK_INTEGER ")" "(" letter_spec_list ")" sep {
            $$ = IMPLICIT(ATTR_TYPE_INT(Integer, $4, @$), $7, TRIVIA_AFTER($9, @$), @$); }
    | KW_IMPLICIT KW_INTEGER "(" letter_spec_list ")"
        "(" letter_spec_list ")" sep {
            $$ = IMPLICIT1(ATTR_TYPE(Integer, @$), $4, $7, TRIVIA_AFTER($9, @$), @$); }
    | KW_IMPLICIT KW_CHARACTER "(" letter_spec_list ")" sep {
            $$ = IMPLICIT(ATTR_TYPE(Character, @$), $4, TRIVIA_AFTER($6, @$), @$); }
    | KW_IMPLICIT KW_CHARACTER "*" TK_INTEGER "(" letter_spec_list ")" sep {
            $$ = IMPLICIT(ATTR_TYPE_INT(Character, $4, @$), $6, TRIVIA_AFTER($8, @$), @$); }
    | KW_IMPLICIT KW_CHARACTER "(" TK_INTEGER ")" "(" letter_spec_list ")" sep {
        $$ = IMPLICIT(ATTR_TYPE_INT(Character, $4, @$), $7, TRIVIA_AFTER($9, @$), @$); }
    | KW_IMPLICIT KW_CHARACTER "(" letter_spec_list ")"
        "(" letter_spec_list ")" sep {
            $$ = IMPLICIT1(ATTR_TYPE(Character, @$), $4, $7, TRIVIA_AFTER($9, @$), @$); }
    | KW_IMPLICIT KW_REAL "(" letter_spec_list ")" sep {
            $$ = IMPLICIT(ATTR_TYPE(Real, @$), $4, TRIVIA_AFTER($6, @$), @$); }
    | KW_IMPLICIT KW_REAL "*" TK_INTEGER "(" letter_spec_list ")" sep {
            $$ = IMPLICIT(ATTR_TYPE_INT(Real, $4, @$), $6, TRIVIA_AFTER($8, @$), @$); }
    | KW_IMPLICIT KW_REAL "(" TK_INTEGER ")" "(" letter_spec_list ")" sep {
            $$ = IMPLICIT(ATTR_TYPE_INT(Real, $4, @$), $7, TRIVIA_AFTER($9, @$), @$); }
    | KW_IMPLICIT KW_REAL "(" letter_spec_list ")"
        "(" letter_spec_list ")" sep {
            $$ = IMPLICIT1(ATTR_TYPE(Real, @$), $4, $7, TRIVIA_AFTER($9, @$), @$); }
    | KW_IMPLICIT KW_COMPLEX "(" letter_spec_list ")" sep {
            $$ = IMPLICIT(ATTR_TYPE(Complex, @$), $4, TRIVIA_AFTER($6, @$), @$); }
    | KW_IMPLICIT KW_COMPLEX "*" TK_INTEGER "(" letter_spec_list ")" sep {
            $$ = IMPLICIT(ATTR_TYPE_INT(Complex, DIV2($4), @$), $6, TRIVIA_AFTER($8, @$), @$); }
    | KW_IMPLICIT KW_COMPLEX "(" TK_INTEGER ")" "(" letter_spec_list ")" sep {
            $$ = IMPLICIT(ATTR_TYPE_INT(Complex, $4, @$), $7, TRIVIA_AFTER($9, @$), @$); }
    | KW_IMPLICIT KW_COMPLEX "(" letter_spec_list ")"
        "(" letter_spec_list ")" sep {
            $$ = IMPLICIT1(ATTR_TYPE(Complex, @$), $4, $7, TRIVIA_AFTER($9, @$), @$); }
    | KW_IMPLICIT KW_LOGICAL "(" letter_spec_list ")" sep {
            $$ = IMPLICIT(ATTR_TYPE(Logical, @$), $4, TRIVIA_AFTER($6, @$), @$); }
    | KW_IMPLICIT KW_LOGICAL "*" TK_INTEGER "(" letter_spec_list ")" sep {
            $$ = IMPLICIT(ATTR_TYPE_INT(Logical, $4, @$), $6, TRIVIA_AFTER($8, @$), @$); }
    | KW_IMPLICIT KW_LOGICAL "(" TK_INTEGER ")" "(" letter_spec_list ")" sep {
            $$ = IMPLICIT(ATTR_TYPE_INT(Logical, $4, @$), $7, TRIVIA_AFTER($9, @$), @$); }
    | KW_IMPLICIT KW_LOGICAL "(" letter_spec_list ")"
        "(" letter_spec_list ")" sep {
            $$ = IMPLICIT1(ATTR_TYPE(Logical, @$), $4, $7, TRIVIA_AFTER($9, @$), @$); }
    | KW_IMPLICIT KW_DOUBLE KW_PRECISION "(" letter_spec_list ")" sep {
            $$ = IMPLICIT(ATTR_TYPE(DoublePrecision, @$), $5, TRIVIA_AFTER($7, @$), @$); }
    | KW_IMPLICIT KW_TYPE "(" id ")" "(" letter_spec_list ")" sep {
            $$ = IMPLICIT(ATTR_TYPE_NAME(Type, $4, @$), $7, TRIVIA_AFTER($9, @$), @$); }
    | KW_IMPLICIT KW_PROCEDURE "(" id ")" "(" letter_spec_list ")" sep {
            $$ = IMPLICIT(ATTR_TYPE_NAME(Procedure, $4, @$), $7, TRIVIA_AFTER($9, @$), @$); }
    | KW_IMPLICIT KW_CLASS "(" id ")" "(" letter_spec_list ")" sep {
            $$ = IMPLICIT(ATTR_TYPE_NAME(Class, $4, @$), $7, TRIVIA_AFTER($9, @$), @$); }
    ;

implicit_none_spec_list
    : implicit_none_spec_list "," implicit_none_spec { $$ = $1; LIST_ADD($$, $3); }
    | implicit_none_spec { LIST_NEW($$); LIST_ADD($$, $1); }
    ;

implicit_none_spec
    : KW_EXTERNAL { $$ = IMPLICIT_NONE_EXTERNAL(@$); }
    | KW_TYPE { $$ = IMPLICIT_NONE_TYPE(@$); }
    ;

letter_spec_list
    : letter_spec_list "," letter_spec { $$ = $1; LIST_ADD($$, $3); }
    | letter_spec { LIST_NEW($$); LIST_ADD($$, $1); }
    ;

letter_spec
    : id           { $$ = LETTER_SPEC1($1, @$); }
    | id "-" id    { $$ = LETTER_SPEC2($1, $3, @$); }
    ;

use_statement_star
    : use_statement_star use_statement { $$ = $1; LIST_ADD($$, $2); }
    | %empty { LIST_NEW($$); }
    ;

use_statement
    : use_statement1 sep { $$ = $1; TRIVIA2_($$, TRIVIA_AFTER($2, @$)); }
    ;

use_statement1
    : KW_USE use_modifiers id { $$ = USE1($2, $3, nullptr, @$); }
    | KW_USE use_modifiers id "," KW_ONLY ":" use_symbol_list {
            $$ = USE2($2, $3, $7, nullptr, @$); }
    | KW_USE use_modifiers id "," KW_ONLY ":" {
            $$ = USE3($2, $3, nullptr, @$); }
    | KW_USE use_modifiers id "," use_symbol_list {
            $$ = USE4($2, $3, $5, nullptr, @$); }
    ;

import_statement_star
    : import_statement_star import_statement { $$ = $1; LIST_ADD($$, $2); }
    | %empty { LIST_NEW($$); }
    ;

import_statement
    : KW_IMPORT sep { $$ = IMPORT0(Default, TRIVIA_AFTER($2, @$), @$); }
    | KW_IMPORT id_list sep { $$ = IMPORT1($2, Default, TRIVIA_AFTER($3, @$), @$); }
    | KW_IMPORT "::" id_list sep { $$ = IMPORT1($3, Default, TRIVIA_AFTER($4, @$), @$); }
    | KW_IMPORT "," KW_ONLY ":" id_list sep { $$ = IMPORT1($5, Only, TRIVIA_AFTER($6, @$), @$); }
    | KW_IMPORT "," KW_NONE sep { $$ = IMPORT0(None, TRIVIA_AFTER($4, @$), @$); }
    | KW_IMPORT "," KW_ALL sep { $$ = IMPORT0(All, TRIVIA_AFTER($4, @$), @$); }
    ;

use_symbol_list
    : use_symbol_list "," use_symbol { $$ = $1; LIST_ADD($$, $3); }
    | use_symbol { LIST_NEW($$); LIST_ADD($$, $1); }
    ;

use_symbol
    : id          { $$ = USE_SYMBOL1($1, @$); }
    | id "=>" id  { $$ = USE_SYMBOL2($1, $3, @$); }
    | KW_ASSIGNMENT "(" "=" ")"  { $$ = USE_ASSIGNMENT(@$); }
    | KW_OPERATOR "(" operator_type ")"  { $$ = INTRINSIC_OPERATOR($3, @$); }
    | KW_OPERATOR "(" "/)"  { $$ = INTRINSIC_OPERATOR(OPERATOR(DIV, @$), @$); }
    | KW_OPERATOR "(" TK_DEF_OP ")"  { $$ = DEFINED_OPERATOR($3, @$); }
    | KW_OPERATOR "(" TK_DEF_OP ")" "=>" KW_OPERATOR "(" TK_DEF_OP ")" {
        $$ = RENAME_OPERATOR($3, $8, @$); }
    | KW_WRITE "(" id ")" { $$ = USE_WRITE($3, @$); }
    | KW_READ "(" id ")" { $$ = USE_READ($3, @$); }
    ;

use_modifiers
    : %empty { LIST_NEW($$); }
    | "::" { LIST_NEW($$); }
    | use_modifier_list "::" { $$ = $1; }
    ;

use_modifier_list
    : use_modifier_list "," use_modifier { $$=$1; LIST_ADD($$, $3); }
    | "," use_modifier { LIST_NEW($$); LIST_ADD($$, $2); }
    ;

use_modifier
    : KW_INTRINSIC { $$ = SIMPLE_ATTR(Intrinsic, @$); }
    | KW_NON_INTRINSIC { $$ = SIMPLE_ATTR(Non_Intrinsic, @$); }
    ;

// var_decl*
var_decl_star
    : var_decl_star var_decl { $$ = $1; LIST_ADD($$, $2); }
    | %empty { LIST_NEW($$); }
    ;

var_decl
    : var_type var_modifiers var_sym_decl_list sep {
        LLOC(@$, @3); $$ = VAR_DECL1($1, $2, $3, TRIVIA_AFTER($4, @$), @$); }
    | var_modifier sep {
        LLOC(@$, @1); $$ = VAR_DECL2($1, TRIVIA_AFTER($2, @$), @$); }
    | var_modifier var_sym_decl_list sep {
        LLOC(@$, @2); $$ = VAR_DECL3($1, $2, TRIVIA_AFTER($3, @$), @$); }
    | var_modifier "::" var_sym_decl_list sep {
        LLOC(@$, @3); $$ = VAR_DECL3($1, $3, TRIVIA_AFTER($4, @$), @$); }
    | KW_PARAMETER "(" named_constant_def_list ")" sep {
        LLOC(@$, @4); $$ = VAR_DECL_PARAMETER($3, TRIVIA_AFTER($5, @$), @$); }
    | KW_NAMELIST "/" id "/" id_list sep {
        LLOC(@$, @5); $$ = VAR_DECL_NAMELIST($3, $5, TRIVIA_AFTER($6, @$), @$);}
    | KW_COMMON common_block_list sep {
        LLOC(@$, @2); $$ = VAR_DECL_COMMON($2, TRIVIA_AFTER($3, @$), @$); }
    | KW_EQUIVALENCE equivalence_set_list sep {
        LLOC(@$, @2); $$ = VAR_DECL_EQUIVALENCE($2, TRIVIA_AFTER($3, @$), @$);}
    ;

equivalence_set_list
    : equivalence_set_list "," equivalence_set { $$ = $1; PLIST_ADD($$, $3); }
    | equivalence_set { LIST_NEW($$); PLIST_ADD($$, $1); }
    ;

equivalence_set
    : "(" expr_list ")" { $$ = EQUIVALENCE_SET($2, @$); }
    ;

named_constant_def_list
    : named_constant_def_list "," named_constant_def {
            $$ = $1; PLIST_ADD($$, $3); }
    | named_constant_def { LIST_NEW($$); PLIST_ADD($$, $1); }
    ;

named_constant_def
    : id "=" expr { $$ = VAR_SYM_DIM_INIT($1, nullptr, 0, $3, Equal, @$); }
    ;

common_block_list
    : common_block_list "," common_block { $$ = $1; PLIST_ADD($$, $3); }
    | common_block { LIST_NEW($$); PLIST_ADD($$, $1); }
    ;

common_block
    : "/" id "/" expr {  $$ = VAR_SYM_DIM_INIT($2, nullptr, 0, $4, Equal, @$); }
    | expr { $$ = VAR_SYM_DIM_EXPR($1, None, @$); }
    ;

data_set_list
    : data_set_list "," data_set { $$ = $1; LIST_ADD($$, $3); }
    | data_set { LIST_NEW($$); LIST_ADD($$, $1); }
    ;

data_set
    : data_object_list "/" data_stmt_value_list "/" { $$ = DATA($1, $3, @$); }
    ;

data_object_list
    : data_object_list "," data_object { $$ = $1; LIST_ADD($$, $3); }
    | data_object { LIST_NEW($$); LIST_ADD($$, $1); }
    ;

data_object
    : id { $$ = $1; }
    | struct_member_star id { NAME1($$, $2, $1, @$); }
    | id "(" fnarray_arg_list_opt ")" { $$ = FUNCCALLORARRAY($1, $3, @$); }
    | "(" data_object_list "," id "=" expr "," expr ")" {
            $$ = DATA_IMPLIED_DO1($2, nullptr, $4, $6, $8, @$); }
    | "(" data_object_list "," integer_type id "=" expr "," expr ")" {
            $$ = DATA_IMPLIED_DO1($2, $4, $5, $7, $9, @$); }
    | "(" data_object_list "," id "=" expr "," expr "," expr ")" {
            $$ = DATA_IMPLIED_DO2($2, nullptr, $4, $6, $8, $10, @$); }
    | "(" data_object_list "," integer_type id "=" expr "," expr "," expr ")" {
            $$ = DATA_IMPLIED_DO2($2, $4, $5, $7, $9, $11, @$); }
    ;

data_stmt_value_list
    : data_stmt_value_list "," data_stmt_constant { $$ = $1; LIST_ADD($$, $3); }
    | data_stmt_value_list "," data_stmt_repeat "*" data_stmt_constant {
            $$ = $1; REPEAT_LIST_ADD($$, $3, $5); }
    | data_stmt_constant { LIST_NEW($$); LIST_ADD($$, $1); }
    | data_stmt_repeat "*" data_stmt_constant { LIST_NEW($$); REPEAT_LIST_ADD($$, $1, $3); }
    ;

data_stmt_repeat
    : id { $$ = $1; }
    | TK_INTEGER { $$ = INTEGER($1, @$); }
    | TK_REAL { $$ = REAL($1, @$); }
    | TK_STRING { $$ = STRING($1, @$); }
    | TK_BOZ_CONSTANT { $$ = BOZ($1, @$); }
    | ".true."  { $$ = TRUE(@$); }
    | ".false." { $$ = FALSE(@$); }
    ;

data_stmt_constant
    : id { $$ = $1; }
    | TK_INTEGER { $$ = INTEGER($1, @$); }
    | TK_REAL { $$ = REAL($1, @$); }
    | TK_STRING { $$ = STRING($1, @$); }
    | TK_BOZ_CONSTANT { $$ = BOZ($1, @$); }
    | ".true."  { $$ = TRUE(@$); }
    | ".false." { $$ = FALSE(@$); }
    | "-" expr %prec UMINUS { $$ = UNARY_MINUS($2, @$); }
    ;

integer_type
    : KW_INTEGER "(" kind_arg_list ")" "::" {
            $$ = ATTR_TYPE_KIND(Integer, $3, @$); }
    ;

kind_arg_list
    : kind_arg_list "," kind_arg2 { $$ = $1; LIST_ADD($$, *$3); }
    | kind_arg2 { LIST_NEW($$); LIST_ADD($$, *$1); }
    ;

kind_arg2
    : expr { $$ = KIND_ARG1($1, @$); }
    | "*" { $$ = KIND_ARG1S(@$); }
    | ":" { $$ = KIND_ARG1C(@$); }
    | id "=" expr { $$ = KIND_ARG2($1, $3, @$); }
    | id "=" "*" { $$ = KIND_ARG2S($1, @$); }
    | id "=" ":" { $$ = KIND_ARG2C($1, @$); }
    ;

var_modifiers
    : %empty { LIST_NEW($$); }
    | "::" { LIST_NEW($$); }
    | var_modifier_list "::" { $$ = $1; }
    ;

var_modifier_list
    : var_modifier_list "," var_modifier { $$=$1; LIST_ADD($$, $3); }
    | "," var_modifier { LIST_NEW($$); LIST_ADD($$, $2); }
    ;

var_modifier
    : KW_PARAMETER { $$ = SIMPLE_ATTR(Parameter, @$); }
    | KW_DIMENSION "(" array_comp_decl_list ")" { $$ = DIMENSION($3, @$); }
    | KW_DIMENSION { $$ = DIMENSION0(@$); }
    | KW_CODIMENSION "[" coarray_comp_decl_list "]" { $$ = CODIMENSION($3, @$); }
    | KW_ALLOCATABLE { $$ = SIMPLE_ATTR(Allocatable, @$); }
    | KW_ASYNCHRONOUS { $$ = SIMPLE_ATTR(Asynchronous, @$); }
    | KW_POINTER { $$ = SIMPLE_ATTR(Pointer, @$); }
    | KW_TARGET { $$ = SIMPLE_ATTR(Target, @$); }
    | KW_OPTIONAL { $$ = SIMPLE_ATTR(Optional, @$); }
    | KW_PROTECTED { $$ = SIMPLE_ATTR(Protected, @$); }
    | KW_SAVE { $$ = SIMPLE_ATTR(Save, @$); }
    | KW_SEQUENCE { $$ = SIMPLE_ATTR(Sequence, @$); }
    | KW_CONTIGUOUS { $$ = SIMPLE_ATTR(Contiguous, @$); }
    | KW_NOPASS { $$ = SIMPLE_ATTR(NoPass, @$); }
    | KW_PRIVATE { $$ = SIMPLE_ATTR(Private, @$); }
    | KW_PUBLIC { $$ = SIMPLE_ATTR(Public, @$); }
    | KW_ABSTRACT { $$ = SIMPLE_ATTR(Abstract, @$); }
    | KW_ENUMERATOR { $$ = SIMPLE_ATTR(Enumerator, @$); }
    | KW_EXTERNAL { $$ = SIMPLE_ATTR(External, @$); }
    | KW_INTENT "(" KW_IN ")" { $$ = INTENT(In, @$); }
    | KW_INTENT "(" KW_OUT ")" { $$ = INTENT(Out, @$); }
    | KW_INTENT "(" inout ")" { $$ = INTENT(InOut, @$); }
    | KW_INTRINSIC { $$ = SIMPLE_ATTR(Intrinsic, @$); }
    | KW_VALUE { $$ = SIMPLE_ATTR(Value, @$); }
    | KW_VOLATILE { $$ = SIMPLE_ATTR(Volatile, @$); }
    | KW_EXTENDS "(" id ")" { $$ = EXTENDS($3, @$); }
    | bind { $$ = BIND($1, @$); }
    | KW_KIND { $$ = SIMPLE_ATTR(Kind, @$); }
    | KW_LEN { $$ = SIMPLE_ATTR(Len, @$); }
    ;

var_type
    : KW_INTEGER { $$ = ATTR_TYPE(Integer, @$); }
    | KW_INTEGER "(" kind_arg_list ")" { $$ = ATTR_TYPE_KIND(Integer, $3, @$); }
    | KW_INTEGER "*" TK_INTEGER { $$ = ATTR_TYPE_INT(Integer, $3, @$); }
    | KW_CHARACTER { $$ = ATTR_TYPE(Character, @$); }
    | KW_CHARACTER "(" kind_arg_list ")" { $$ = ATTR_TYPE_KIND(Character, $3, @$); }
    | KW_CHARACTER "*" TK_INTEGER { $$ = ATTR_TYPE_INT(Character, $3, @$); }
    | KW_CHARACTER "*" "(" "*" ")" {
            $$ = ATTR_TYPE_STAR(Character, DoubleAsterisk, @$); }
    | KW_REAL { $$ = ATTR_TYPE(Real, @$); }
    | KW_REAL "(" kind_arg_list ")" { $$ = ATTR_TYPE_KIND(Real, $3, @$); }
    | KW_REAL "*" TK_INTEGER { $$ = ATTR_TYPE_INT(Real, $3, @$); }
    | KW_COMPLEX { $$ = ATTR_TYPE(Complex, @$); }
    | KW_COMPLEX "(" kind_arg_list ")" { $$ = ATTR_TYPE_KIND(Complex, $3, @$); }
    | KW_COMPLEX "*" TK_INTEGER { $$ = ATTR_TYPE_INT(Complex, DIV2($3), @$); }
    | KW_LOGICAL { $$ = ATTR_TYPE(Logical, @$); }
    | KW_LOGICAL "(" kind_arg_list ")" { $$ = ATTR_TYPE_KIND(Logical, $3, @$); }
    | KW_LOGICAL "*" TK_INTEGER { $$ = ATTR_TYPE_INT(Logical, $3, @$); }
    | KW_DOUBLE KW_PRECISION { $$ = ATTR_TYPE(DoublePrecision, @$); }
    | KW_DOUBLE_PRECISION { $$ = ATTR_TYPE(DoublePrecision, @$); }
    | KW_DOUBLE KW_COMPLEX { $$ = ATTR_TYPE(DoubleComplex, @$); }
    | KW_DOUBLE_COMPLEX { $$ = ATTR_TYPE(DoubleComplex, @$); }
    | KW_TYPE "(" id ")" { $$ = ATTR_TYPE_NAME(Type, $3, @$); }
    | KW_TYPE "(" "*" ")" { $$ = ATTR_TYPE_STAR(Type, Asterisk, @$); }
    | KW_PROCEDURE "(" id ")" { $$ = ATTR_TYPE_NAME(Procedure, $3, @$); }
    | KW_CLASS "(" id ")" { $$ = ATTR_TYPE_NAME(Class, $3, @$); }
    | KW_CLASS "(" "*" ")" { $$ = ATTR_TYPE_STAR(Class, Asterisk, @$); }
    ;

var_sym_decl_list
    : var_sym_decl_list "," var_sym_decl { $$=$1; PLIST_ADD($$, $3); }
    | var_sym_decl { LIST_NEW($$); PLIST_ADD($$, $1); }
    ;

var_sym_decl
    : id { $$ = VAR_SYM_NAME($1, None, @$); }
    | "/" id "/" { $$ = VAR_SYM_NAME($2, Slash, @$); }
    | id "=" expr { $$ = VAR_SYM_DIM_INIT($1, nullptr, 0, $3, Equal, @$); }
    | id "=>" expr { $$ = VAR_SYM_DIM_INIT($1, nullptr, 0, $3, Arrow, @$); }
    | id "*" expr { $$ = VAR_SYM_DIM_INIT($1, nullptr, 0, $3, Asterisk, @$); }
    | id "*" "(" "*" ")" { $$ = VAR_SYM_NAME($1, DoubleAsterisk, @$); }
    | id "(" array_comp_decl_list ")" %dprec 1 { $$ = VAR_SYM_DIM($1, $3.p, $3.n, None, @$); }
    | id "(" array_comp_decl_list ")" "*" TK_INTEGER %dprec 1 {
            $$ = VAR_SYM_DIM_INIT($1, $3.p, $3.n, INTEGER($6, @$), Asterisk, @$); }
    | id "(" array_comp_decl_list ")" "=" expr {
            $$ = VAR_SYM_DIM_INIT($1, $3.p, $3.n, $6, Equal, @$); }
    | id "(" array_comp_decl_list ")" "=>" expr {
            $$ = VAR_SYM_DIM_INIT($1, $3.p, $3.n, $6, Arrow, @$); }
    | id "[" coarray_comp_decl_list "]" {
            $$ = VAR_SYM_CODIM($1, $3.p, $3.n, None, @$); }
    | id "(" array_comp_decl_list ")" "[" coarray_comp_decl_list "]" {
            $$ = VAR_SYM_DIM_CODIM($1, $3.p, $3.n, $6.p, $6.n, None, @$); }
    | decl_spec %dprec 2 { $$ = VAR_SYM_SPEC($1, None, @$); }
    ;

decl_spec
    : KW_OPERATOR "(" operator_type ")" { $$ = DECL_OP($3, @$); }
    | KW_OPERATOR "(" "/)" { $$ = DECL_OP(OPERATOR(DIV, @$), @$); }
    | KW_OPERATOR "(" TK_DEF_OP ")" { $$ = DECL_DEFOP($3, @$); }
    | KW_ASSIGNMENT "(" "=" ")" { $$ = DECL_ASSIGNMENT(@$); }
    ;

array_comp_decl_list
    : array_comp_decl_list "," array_comp_decl { $$ = $1; PLIST_ADD($$, $3); }
    | array_comp_decl { LIST_NEW($$); PLIST_ADD($$, $1); }
    ;

array_comp_decl
    : expr           { $$ = ARRAY_COMP_DECL1d($1, @$); }
    | expr ":" expr  { $$ = ARRAY_COMP_DECL2d($1, $3, @$); }
    | expr ":"       { $$ = ARRAY_COMP_DECL3d($1, @$); }
    | ":" expr       { $$ = ARRAY_COMP_DECL4d($2, @$); }
    | ":"            { $$ = ARRAY_COMP_DECL5d(@$); }
    | "*"            { $$ = ARRAY_COMP_DECL6d(@$); }
    | expr ":" "*"   { $$ = ARRAY_COMP_DECL7d($1, @$); }
    | TK_DBL_DOT     { $$ = ARRAY_COMP_DECL8d(@$); }
    ;

coarray_comp_decl_list
    : coarray_comp_decl_list "," coarray_comp_decl { $$ = $1; PLIST_ADD($$, $3); }
    | coarray_comp_decl { LIST_NEW($$); PLIST_ADD($$, $1); }
    ;

coarray_comp_decl
    : expr           { $$ = COARRAY_COMP_DECL1d($1, @$); }
    | expr ":" expr  { $$ = COARRAY_COMP_DECL2d($1, $3, @$); }
    | expr ":"       { $$ = COARRAY_COMP_DECL3d($1, @$); }
    | ":" expr       { $$ = COARRAY_COMP_DECL4d($2, @$); }
    | ":"            { $$ = COARRAY_COMP_DECL5d(@$); }
    | "*"            { $$ = COARRAY_COMP_DECL6d(@$); }
    | expr ":" "*"   { $$ = COARRAY_COMP_DECL7d($1, @$); }
    ;


// -----------------------------------------------------------------------------
// Control flow

statements
    : statements statement { $$ = $1; LIST_ADD($$, $2); }
    | %empty { LIST_NEW($$); }
    ;

sep
    : sep sep_one { $$ = $1; LIST_ADD($$, $2); }
    | sep_one { LIST_NEW($$); LIST_ADD($$, $1); }
    ;

sep_one
    : TK_NEWLINE { $$ = NEWLINE(@$); }
    | TK_COMMENT { $$ = COMMENT($1, @$); }
    | TK_EOLCOMMENT { $$ = EOLCOMMENT($1, @$); }
    | ";" { $$ = SEMICOLON(@$); }
    ;

decl_statements
    : decl_statements decl_statement { $$ = $1; LIST_ADD($$, $2); }
    | %empty { LIST_NEW($$); }
    ;

decl_statement
    : var_decl
    | interface_decl
    | derived_type_decl
    | template_decl
    | enum_decl
    | statement
    | instantiate
    ;

statement
    : statement1 sep { $$ = $1; TRIVIA_($$, TRIVIA_AFTER($2, @$)); }
    | TK_LABEL statement1 sep { $$ = $2;
            LABEL($$, $1); TRIVIA_($$, TRIVIA_AFTER($3, @$)); }
    ;

statement1
    : single_line_statement
    | multi_line_statement
    ;

single_line_statement
    : allocate_statement
    | assign_statement
    | assignment_statement
    | associate_statement
    | close_statement
    | continue_statement
    | cycle_statement
    | deallocate_statement
    | entry_statement
    | error_stop_statement
    | event_post_statement
    | event_wait_statement
    | exit_statement
    | flush_statement
    | forall_statement_single
    | format_statement
    | data_statement
    | form_team_statement
    | goto_statement
    | if_statement_single
    | include_statement
    | inquire_statement
    | nullify_statement
    | open_statement
    | print_statement
    | read_statement
    | return_statement
    | rewind_statement
    | backspace_statement
    | endfile_statement
    | stop_statement
    | subroutine_call
    | sync_all_statement
    | sync_images_statement
    | sync_memory_statement
    | sync_team_statement
    | where_statement_single
    | write_statement
    ;

multi_line_statement
    : multi_line_statement0 { $$ = $1; }
    | id ":" multi_line_statement0 id { $$ = STMT_NAME($1, $4, $3); }
    ;

multi_line_statement0
    : associate_block
    | block_statement
    | change_team_statement
    | critical_statement
    | do_statement
    | forall_statement
    | if_statement
    | select_statement
    | select_type_statement
    | select_rank_statement
    | where_statement
    | while_statement
    ;

assign_statement
    : KW_ASSIGN TK_INTEGER KW_TO id { $$ = ASSIGN(INTEGER3($2), $4, @$); }
    ;

assignment_statement
    : expr "=" expr { $$ = ASSIGNMENT($1, $3, @$); }
    ;

goto_statement
    : goto TK_INTEGER { $$ = GOTO($2, @$); }
    | goto "(" expr_list ")" expr { $$ = GOTO1($3, $5, @$); }
    | goto "(" expr_list ")" "," expr { $$ = GOTO1($3, $6, @$); }
    | goto id { $$ = GOTO2($2, @$); }
    | goto id "(" expr_list ")" { $$ = GOTO3($2, $4, @$); }
    | goto id "," "(" expr_list ")" { $$ = GOTO3($2, $5, @$); }
    ;

goto
    : KW_GO KW_TO
    | KW_GOTO
    ;

associate_statement
    : expr "=>" expr { $$ = ASSOCIATE($1, $3, @$); }
    ;

associate_block
    : KW_ASSOCIATE "(" var_sym_decl_list ")" sep statements end_associate {
        $$ = ASSOCIATE_BLOCK($3, TRIVIA_AFTER($5, @$), $6, @$); }
    ;

block_statement
    : KW_BLOCK sep use_statement_star import_statement_star decl_statements
        end_block { $$ = BLOCK(TRIVIA_AFTER($2, @$), $3, $4, SPLIT_DECL(p.m_a, $5), SPLIT_STMT(p.m_a, $5), @$); }
    ;

allocate_statement
    : KW_ALLOCATE "(" fnarray_arg_list_opt ")" {
            $$ = ALLOCATE_STMT($3, @$); }

deallocate_statement
    : KW_DEALLOCATE "(" fnarray_arg_list_opt ")" {
            $$ = DEALLOCATE_STMT($3, @$); }

subroutine_call
    : KW_CALL id "(" fnarray_arg_list_opt ")" {
            $$ = SUBROUTINE_CALL($2, $4, @$); }
    | KW_CALL struct_member_star id "(" fnarray_arg_list_opt ")" {
            $$ = SUBROUTINE_CALL1($2, $3, $5, @$); }
    | KW_CALL id {
            $$ = SUBROUTINE_CALL2($2, @$); }
    | KW_CALL struct_member_star id {
            $$ = SUBROUTINE_CALL3($2, $3, @$); }
    ;

print_statement
    : KW_PRINT format               { $$ = PRINT0($2,    @$); }
    | KW_PRINT format ","           { $$ = PRINT0($2,    @$); }
    | KW_PRINT format "," expr_list { $$ = PRINT($2, $4, @$); }
    ;

format
    : expr { $$ = $1; }
    | "*" { $$ = nullptr; }
    ;

open_statement
    : KW_OPEN "(" write_arg_list ")" { $$ = OPEN($3, @$); }

close_statement
    : KW_CLOSE "(" write_arg_list ")" { $$ = CLOSE($3, @$); }

write_arg_list
    : write_arg_list "," write_arg2 { $$ = $1; PLIST_ADD($$, $3); }
    | write_arg2 { LIST_NEW($$); PLIST_ADD($$, $1); }
    ;

write_arg2
    : write_arg { WRITE_ARG1($$, $1); }
    | id "=" write_arg { WRITE_ARG2($$, $1, $3); }
    ;

write_arg
    : expr { $$ = $1; }
    | "*" { $$ = nullptr; }
    ;

write_statement
    : KW_WRITE "(" write_arg_list ")" expr_list { $$ = WRITE($3, $5, @$); }
    | KW_WRITE "(" write_arg_list ")" "," expr_list { $$ = WRITE($3, $6, @$); }
    | KW_WRITE "(" write_arg_list ")" { $$ = WRITE0($3, @$); }
    ;

read_statement
    : KW_READ "(" write_arg_list ")" expr_list { $$ = READ($3, $5, @$); }
    | KW_READ "(" write_arg_list ")" "," expr_list { $$ = READ($3, $6, @$); }
    | KW_READ "(" write_arg_list ")" { $$ = READ0($3, @$); }
    | KW_READ TK_INTEGER "," expr_list { $$ = READ2($2, $4, @$); }
    | KW_READ "*" "," expr_list { $$ = READ3($4, @$); }
    | KW_READ TK_INTEGER { $$ = READ4($2, @$); }
    ;

nullify_statement
    : KW_NULLIFY "(" write_arg_list ")" {
            $$ = NULLIFY($3, @$); }

include_statement
    : KW_INCLUDE TK_STRING { $$ = INCLUDE($2, @$); }

inquire_statement
    : KW_INQUIRE "(" write_arg_list ")" expr_list { $$ = INQUIRE($3, $5, @$); }
    | KW_INQUIRE "(" write_arg_list ")" { $$ = INQUIRE0($3, @$); }
    ;

rewind_statement
    : KW_REWIND "(" write_arg_list ")" { $$ = REWIND($3, @$); }
    | KW_REWIND id { $$ = REWIND2($2, @$); }
    | KW_REWIND TK_INTEGER { $$ = REWIND2(INTEGER($2, @$), @$); }
    | KW_REWIND id "(" fnarray_arg_list_opt ")" {
            $$ =  REWIND2(FUNCCALLORARRAY($2, $4, @$), @$); }
    ;

backspace_statement
    : KW_BACKSPACE "(" write_arg_list ")" { $$ = BACKSPACE($3, @$); }
    | KW_BACKSPACE id { $$ = BACKSPACE2($2, @$); }
    | KW_BACKSPACE TK_INTEGER { $$ = BACKSPACE2(INTEGER($2, @$), @$); }
    | KW_BACKSPACE id "(" fnarray_arg_list_opt ")" {
            $$ =  BACKSPACE2(FUNCCALLORARRAY($2, $4, @$), @$); }
    ;

flush_statement
    : KW_FLUSH "(" write_arg_list ")" { $$ = FLUSH($3, @$); }
    | KW_FLUSH TK_INTEGER { $$ = FLUSH1($2, @$); }
    ;

endfile_statement
    : end_file "(" write_arg_list ")" { $$ = ENDFILE($3, @$); }
    | end_file id { $$ = ENDFILE2($2, @$); }
    | end_file TK_INTEGER { $$ = ENDFILE2(INTEGER($2, @$), @$); }
    ;

end_file
    : KW_END_FILE
    | KW_ENDFILE
    ;

// sr-conflict (2x): KW_ENDIF can be an "id" or end of "if_statement"
if_statement
    : if_block endif {}
    ;

if_statement_single
    : KW_IF "(" expr ")" single_line_statement {
            $$ = IFSINGLE($3, $5, @$); }
    | KW_IF "(" expr ")" TK_INTEGER "," TK_INTEGER "," TK_INTEGER {
            $$ = IFARITHMETIC($3, INTEGER3($5), INTEGER3($7), INTEGER3($9), @$); }
    ;

if_block
    : KW_IF "(" expr ")" KW_THEN id_opt sep statements {
            $$ = IF1($3, TRIVIA_AFTER($7, @$), $8, @$); }
    | KW_IF "(" expr ")" KW_THEN id_opt sep statements
        KW_ELSE id_opt sep statements {
            $$ = IF2($3, TRIVIA($7, $11, @$), $8, $12, @$); }
    | KW_IF "(" expr ")" KW_THEN id_opt sep statements KW_ELSE if_block {
            $$ = IF3($3, TRIVIA_AFTER($7, @$), $8, $10, @$); }
    | KW_IF "(" expr ")" KW_THEN id_opt sep statements elseif_block {
            $$ = IF3($3, TRIVIA_AFTER($7, @$), $8, $9, @$); }
    ;

elseif_block
    : KW_ELSEIF "(" expr ")" KW_THEN id_opt sep statements {
            $$ = IF1($3, TRIVIA_AFTER($7, @$), $8, @$); }
    | KW_ELSEIF "(" expr ")" KW_THEN id_opt sep statements
        KW_ELSE id_opt sep statements {
            $$ = IF2($3, TRIVIA($7, $11, @$), $8, $12, @$); }
    | KW_ELSEIF "(" expr ")" KW_THEN id_opt sep statements KW_ELSE if_block {
            $$ = IF3($3, TRIVIA_AFTER($7, @$), $8, $10, @$); }
    | KW_ELSEIF "(" expr ")" KW_THEN id_opt sep statements elseif_block {
            $$ = IF3($3, TRIVIA_AFTER($7, @$), $8, $9, @$); }
    ;

where_statement
    : where_block endwhere {}
    ;

where_statement_single
    : KW_WHERE "(" expr ")" assignment_statement { $$ = WHERESINGLE($3, $5, @$); }
    ;

where_block
    : KW_WHERE "(" expr ")" sep statements elsewhere_block {
            $$ = WHERE3($3, TRIVIA_AFTER($5, @$), $6, $7, @$); }
    | KW_WHERE "(" expr ")" sep statements KW_ELSEWHERE sep statements {
            $$ = WHERE2($3, TRIVIA($5, $8, @$), $6, $9, @$); }
    | KW_WHERE "(" expr ")" sep statements KW_ELSE KW_WHERE sep statements {
            $$ = WHERE2($3, TRIVIA($5, $9, @$), $6, $10, @$); }
    | KW_WHERE "(" expr ")" sep statements {
            $$ = WHERE1($3, TRIVIA_AFTER($5, @$), $6, @$); }
    ;

elsewhere_block
    : KW_ELSEWHERE "(" expr ")" sep statements elsewhere_block {
            $$ = WHERE3($3, TRIVIA_AFTER($5, @$), $6, $7, @$); }
    | KW_ELSE KW_WHERE "(" expr ")" sep statements elsewhere_block {
            $$ = WHERE3($4, TRIVIA_AFTER($6, @$), $7, $8, @$); }
    | KW_ELSEWHERE "(" expr ")" sep statements KW_ELSEWHERE sep statements {
            $$ = WHERE2($3, TRIVIA($5, $8, @$), $6, $9, @$); }
    | KW_ELSEWHERE "(" expr ")" sep statements KW_ELSE KW_WHERE sep statements {
            $$ = WHERE2($3, TRIVIA($5, $9, @$), $6, $10, @$); }
    | KW_ELSE KW_WHERE "(" expr ")" sep statements KW_ELSEWHERE sep statements {
            $$ = WHERE2($4, TRIVIA($6, $9, @$), $7, $10, @$); }
    | KW_ELSE KW_WHERE "(" expr ")" sep statements KW_ELSE KW_WHERE sep statements {
            $$ = WHERE2($4, TRIVIA($6, $10, @$), $7, $11, @$); }
    | KW_ELSEWHERE "(" expr ")" sep statements {
            $$ = WHERE1($3, TRIVIA_AFTER($5, @$), $6, @$); }
    | KW_ELSE KW_WHERE "(" expr ")" sep statements {
            $$ = WHERE1($4, TRIVIA_AFTER($6, @$), $7, @$); }
    ;

select_statement
    : KW_SELECT KW_CASE "(" expr ")" sep case_statements end_select {
            $$ = SELECT($4, TRIVIA_AFTER($6, @$), $7, @$); }
    | KW_SELECT_CASE "(" expr ")" sep case_statements end_select {
                $$ = SELECT($3, TRIVIA_AFTER($5, @$), $6, @$); }
    ;

case_statements
    : case_statements case_statement { $$ = $1; LIST_ADD($$, $2); }
    | %empty { LIST_NEW($$); }
    ;

case_statement
    : KW_CASE "(" case_conditions ")" sep statements {
            $$ = CASE_STMT($3, TRIVIA_AFTER($5, @$), $6, @$); }
    | KW_CASE KW_DEFAULT sep statements { $$ = CASE_STMT_DEFAULT(TRIVIA_AFTER($3, @$), $4, @$); }
    ;

case_conditions
    : case_conditions "," case_condition { $$ = $1; LIST_ADD($$, $3); }
    | case_condition { LIST_NEW($$); LIST_ADD($$, $1); }
    ;

case_condition
    : expr { $$ = CASE_EXPR($1, @$); }
    | expr ":" { $$ = CASE_RANGE1($1, @$); }
    | ":" expr { $$ = CASE_RANGE2($2, @$); }
    | expr ":" expr { $$ = CASE_RANGE3($1, $3, @$); }
    ;

select_rank_statement
    : select_rank "(" expr ")" sep select_rank_case_stmts
        end_select { $$ = SELECT_RANK1($3, TRIVIA_AFTER($5, @$), $6, @$); }
    | select_rank "(" id "=>" expr ")" sep select_rank_case_stmts
        end_select { $$ = SELECT_RANK2($3, $5, TRIVIA_AFTER($7, @$), $8, @$); }
    ;

select_rank
    : KW_SELECT KW_RANK
    | KW_SELECT_RANK
    ;

select_rank_case_stmts
    : select_rank_case_stmts select_rank_case_stmt { $$ = $1; LIST_ADD($$, $2); }
    | %empty { LIST_NEW($$); }
    ;

select_rank_case_stmt
    : KW_RANK "(" expr ")" id_opt sep statements { $$ = RANK_EXPR($3, TRIVIA_AFTER($6, @$), $7, @$); }
    | KW_RANK "(" "*" ")" id_opt sep statements { $$ = RANK_STAR(TRIVIA_AFTER($6, @$), $7, @$); }
    | KW_RANK KW_DEFAULT id_opt sep statements { $$ = RANK_DEFAULT(TRIVIA_AFTER($4, @$), $5, @$); }
    ;

select_type_statement
    : select_type "(" expr ")" sep select_type_body_statements
        end_select {
                $$ = SELECT_TYPE1($3, TRIVIA_AFTER($5, @$), $6, @$); }
    | select_type "(" id "=>" expr ")" sep select_type_body_statements
        end_select {
                $$ = SELECT_TYPE2($3, $5, TRIVIA_AFTER($7, @$), $8, @$); }
    ;

select_type
    : KW_SELECT KW_TYPE
    | KW_SELECT_TYPE
    ;

select_type_body_statements
    : select_type_body_statements select_type_body_statement {
                        $$ = $1; LIST_ADD($$, $2); }
    | %empty { LIST_NEW($$); }
    ;

select_type_body_statement
    : KW_TYPE KW_IS "(" TK_NAME ")" sep statements { $$ = TYPE_STMTNAME($4, TRIVIA_AFTER($6, @$), $7, @$); }
    | KW_TYPE KW_IS "(" var_type ")" sep statements { $$ = TYPE_STMTVAR($4, TRIVIA_AFTER($6, @$), $7, @$); }
    | KW_CLASS KW_IS "(" id ")" sep statements { $$ = CLASS_STMT($4, TRIVIA_AFTER($6, @$), $7, @$); }
    | KW_CLASS KW_DEFAULT sep statements { $$ = CLASS_DEFAULT(TRIVIA_AFTER($3, @$), $4, @$); }
    ;

while_statement
    : KW_DO comma_opt KW_WHILE "(" expr ")" sep statements enddo {
            $$ = WHILE($5, TRIVIA_AFTER($7, @$), $8, @$); }
    | KW_DOWHILE "(" expr ")" sep statements enddo {
                $$ = WHILE($3, TRIVIA_AFTER($5, @$), $6, @$); }
    ;

// sr-conflict (2x): "KW_DO sep" being either a do_statement or an expr
do_statement
    : KW_DO sep statements enddo {
            $$ = DO1(TRIVIA_AFTER($2, @$), $3, @$); }
    | KW_DO comma_opt id "=" expr "," expr sep statements enddo {
            $$ = DO2($3, $5, $7, TRIVIA_AFTER($8, @$), $9, @$); }
    | KW_DO comma_opt id "=" expr "," expr "," expr sep statements enddo {
            $$ = DO3($3, $5, $7, $9, TRIVIA_AFTER($10, @$), $11, @$); }
    | KW_DO TK_INTEGER comma_opt id "=" expr "," expr sep statements enddo {
            $$ = DO2_LABEL(INTEGER3($2), $4, $6, $8, TRIVIA_AFTER($9, @$), $10, @$); }
    | KW_DO TK_INTEGER comma_opt id "=" expr "," expr "," expr sep statements enddo {
            $$ = DO3_LABEL(INTEGER3($2), $4, $6, $8, $10, TRIVIA_AFTER($11, @$), $12, @$); }
    | KW_DO comma_opt KW_CONCURRENT "(" concurrent_control_list ")"
        concurrent_locality_star sep statements enddo {
            $$ = DO_CONCURRENT1($5, $7, TRIVIA_AFTER($8, @$), $9, @$); }
    | KW_DO comma_opt KW_CONCURRENT "(" concurrent_control_list "," expr ")"
        concurrent_locality_star sep statements enddo {
            $$ = DO_CONCURRENT2($5, $7, $9, TRIVIA_AFTER($10, @$), $11, @$); }
    ;

concurrent_control_list
    : concurrent_control_list "," concurrent_control {
        $$ = $1; LIST_ADD($$, $3); }
    | concurrent_control { LIST_NEW($$); LIST_ADD($$, $1); }
    ;

concurrent_control
    : id "=" expr ":" expr {
            $$ = CONCURRENT_CONTROL1($1, $3, $5,     @$); }
    | id "=" expr ":" expr ":" expr {
            $$ = CONCURRENT_CONTROL2($1, $3, $5, $7, @$); }
    ;

concurrent_locality_star
    : concurrent_locality_star concurrent_locality {
        $$ = $1; LIST_ADD($$, $2); }
    | %empty { LIST_NEW($$); }
    ;

concurrent_locality
    : KW_LOCAL "(" id_list ")" { $$ = CONCURRENT_LOCAL($3, @$); }
    | KW_LOCAL_INIT "(" id_list ")" { $$ = CONCURRENT_LOCAL_INIT($3, @$); }
    | KW_SHARED "(" id_list ")" { $$ = CONCURRENT_SHARED($3, @$); }
    | KW_DEFAULT "(" KW_NONE ")" { $$ = CONCURRENT_DEFAULT(@$); }
    | KW_REDUCE "(" reduce_op ":" id_list ")" {
        $$ = CONCURRENT_REDUCE($3, $5, @$); }
    ;

comma_opt
    : ","
    | %empty
    ;

forall_statement
    : KW_FORALL "(" concurrent_control_list ")"
        concurrent_locality_star sep statements endforall {
            $$ = FORALL1($3, $5, TRIVIA_AFTER($6, @$), $7, @$); }
    | KW_FORALL "(" concurrent_control_list "," expr ")"
        concurrent_locality_star sep statements endforall {
            $$ = FORALL2($3, $5, $7, TRIVIA_AFTER($8, @$), $9, @$); }
    ;

forall_statement_single
    : KW_FORALL "(" concurrent_control_list ")"
        assignment_statement { $$ = FORALLSINGLE1($3, $5, @$); }
    | KW_FORALL "(" concurrent_control_list "," expr ")"
        assignment_statement { $$ = FORALLSINGLE2($3, $5, $7, @$); }
    ;

format_statement
    : TK_FORMAT { $$ = FORMAT($1, @$); }
    ;

data_statement
    : KW_DATA data_set_list { $$ = DATASTMT($2, @$); }
    ;

form_team_statement
    : form_team "(" expr "," id ")" { $$ = FORMTEAM1($3, $5, @$); }
    | form_team "(" expr "," id sync_stat_list ")" {
            $$ = FORMTEAM2($3, $5, $6, @$); }
    ;

form_team
    : KW_FORM KW_TEAM
    | KW_FORM_TEAM
    ;

reduce_op
    : "+" { $$ = REDUCE_OP_TYPE_ADD(@$); }
    | "*" { $$ = REDUCE_OP_TYPE_MUL(@$); }
    | id  { $$ = REDUCE_OP_TYPE_ID($1, @$); }
    ;

inout
    : KW_IN_OUT
    | KW_INOUT
    ;

enddo
    : KW_END_DO
    | TK_LABEL KW_END_DO
    | KW_ENDDO { WARN_ENDDO(@$); }
    | TK_LABEL KW_ENDDO {}
    ;

endforall
    : KW_END_FORALL
    | KW_ENDFORALL
    ;

endif
    : KW_END_IF
    | KW_ENDIF { WARN_ENDIF(@$); }
    ;

endwhere
    : KW_END_WHERE
    | KW_ENDWHERE
    ;

exit_statement
    : KW_EXIT { $$ = EXIT(@$); }
    | KW_EXIT id { $$ = EXIT2($2, @$); }
    ;

return_statement
    : KW_RETURN { $$ = RETURN(@$); }
    | KW_RETURN expr { $$ = RETURN1($2, @$); }
    ;

cycle_statement
    : KW_CYCLE { $$ = CYCLE(@$); }
    | KW_CYCLE id { $$ = CYCLE2($2, @$); }
    ;

continue_statement
    : KW_CONTINUE { $$ = CONTINUE(@$); }
    ;

entry_statement
    : KW_ENTRY id sub_args { $$ = ENTRY1($2, $3, @$); }
    | KW_ENTRY id sub_args bind result_opt { $$ = ENTRY2($2, $3, $4, $5, @$); }
    | KW_ENTRY id sub_args result bind_opt { $$ = ENTRY3($2, $3, $4, $5, @$); }
    ;

stop_statement
    : KW_STOP { $$ = STOP(@$); }
    | KW_STOP expr { $$ = STOP1($2, @$); }
    | KW_STOP "," KW_QUIET "=" expr { $$ = STOP2($5, @$); }
    | KW_STOP expr "," KW_QUIET "=" expr { $$ = STOP3($2, $6, @$); }
    ;

error_stop_statement
    : KW_ERROR KW_STOP { $$ = ERROR_STOP(@$); }
    | KW_ERROR KW_STOP expr { $$ = ERROR_STOP1($3, @$); }
    | KW_ERROR KW_STOP "," KW_QUIET "=" expr { $$ = ERROR_STOP2($6, @$); }
    | KW_ERROR KW_STOP expr "," KW_QUIET "=" expr {
            $$ = ERROR_STOP3($3, $7, @$); }
    ;

event_post_statement
    : KW_EVENT KW_POST "(" expr ")" { $$ = EVENT_POST($4, @$); }
    | KW_EVENT KW_POST "(" expr "," event_post_stat_list ")" {
            $$ = EVENT_POST1($4, $6, @$); }
    ;

event_wait_statement
    : KW_EVENT KW_WAIT "(" expr ")" {
            $$ = EVENT_WAIT($4, @$); }
    | KW_EVENT KW_WAIT "(" expr "," event_wait_spec_list ")" {
            $$ = EVENT_WAIT1($4, $6, @$); }
    ;

sync_all_statement
    : sync_all { $$ = SYNC_ALL(@$); }
    | sync_all "(" ")" { $$ = SYNC_ALL1(@$); }
    | sync_all "(" sync_stat_list ")" { $$ = SYNC_ALL2($3, @$); }
    ;
sync_all
    : KW_SYNC KW_ALL
    | KW_SYNC_ALL
    ;

sync_images_statement
    : sync_images "(" "*" ")" { $$ = SYNC_IMAGE1(Asterisk, @$); }
    | sync_images "(" expr ")" { $$ = SYNC_IMAGE2($3, @$); }
    | sync_images "(" "*" sync_stat_list ")" {
            $$ = SYNC_IMAGE3(Asterisk, $4, @$); }
    | sync_images "(" expr sync_stat_list ")" { $$ = SYNC_IMAGE4($3, $4, @$); }
    ;
sync_images
    : KW_SYNC KW_IMAGES
    | KW_SYNC_IMAGES
    ;

sync_memory_statement
    : sync_memory { $$ = SYNC_MEMORY(@$); }
    | sync_memory "(" ")" { $$ = SYNC_MEMORY1(@$); }
    | sync_memory "(" sync_stat_list ")" { $$ = SYNC_MEMORY2($3, @$); }
    ;
sync_memory
    : KW_SYNC KW_MEMORY
    | KW_SYNC_MEMORY
    ;

sync_team_statement
    : sync_team "(" expr ")" { $$ = SYNCTEAM1($3, @$); }
    | sync_team "(" expr sync_stat_list ")" { $$ = SYNCTEAM2($3, $4, @$); }
    ;

sync_team
    : KW_SYNC KW_TEAM
    | KW_SYNC_TEAM
    ;

event_wait_spec_list
    : event_wait_spec_list "," sync_stat { $$ = $1; LIST_ADD($$, $3); }
    | event_wait_spec { LIST_NEW($$); LIST_ADD($$, $1); }
    | %empty { LIST_NEW($$); }
    ;

event_wait_spec
    : id "=" expr { $$ = EVENT_WAIT_KW_ARG($1, $3, @$); }
    ;

event_post_stat_list
    : sync_stat { LIST_NEW($$); LIST_ADD($$, $1); }
    ;

sync_stat_list
    : sync_stat_list "," sync_stat { $$ = $1; LIST_ADD($$, $3); }
    | "," sync_stat { LIST_NEW($$); LIST_ADD($$, $2); }
    | sync_stat { LIST_NEW($$); LIST_ADD($$, $1); }
    ;

sync_stat
    : KW_STAT "=" id { $$ = STAT($3, @$); }
    | KW_ERRMSG "=" id { $$ = ERRMSG($3, @$); }
    | KW_NEW_INDEX "=" expr { $$ = NEW_INDEX($3, @$); }
    ;

critical_statement
    : KW_CRITICAL sep statements end_critical {
            $$ = CRITICAL(TRIVIA_AFTER($2, @$), $3, @$); }
    | KW_CRITICAL "(" ")" sep statements end_critical {
            $$ = CRITICAL1(TRIVIA_AFTER($4, @$), $5, @$); }
    | KW_CRITICAL "(" sync_stat_list ")" sep statements end_critical {
            $$ = CRITICAL2($3, TRIVIA_AFTER($5, @$), $6, @$); }
    ;

change_team_statement
    : change_team "(" expr coarray_association_list ")"
        sep statements end_team {
            $$ = CHANGETEAM1($3, $4, TRIVIA_AFTER($6, @$), $7, @$); }
    | change_team "(" expr coarray_association_list ")"
        sep statements end_team "(" sync_stat_list ")" {
            $$ = CHANGETEAM2($3, $4, TRIVIA_AFTER($6, @$), $7, $10, @$); }
    | change_team "(" expr coarray_association_list sync_stat_list ")"
        sep statements end_team {
            $$ = CHANGETEAM3($3, $4, $5, TRIVIA_AFTER($7, @$), $8, @$); }
    | change_team "(" expr coarray_association_list sync_stat_list ")"
        sep statements end_team "(" sync_stat_list ")" {
            $$ = CHANGETEAM4($3, $4, $5, TRIVIA_AFTER($7, @$), $8, $11, @$); }
    ;

coarray_association_list
    : coarray_association_list "," coarray_association { $$ = $1; LIST_ADD($$, $3); }
    | coarray_association { LIST_NEW($$); LIST_ADD($$, $1); }
    | %empty { LIST_NEW($$); }
    ;

coarray_association
    : id "[" coarray_arg_list "]" "=>" expr { $$ = COARRAY_ASSOC($1, $3, $6, @$); }
    ;

change_team
    : KW_CHANGE KW_TEAM
    | KW_CHANGE_TEAM
    ;

// -----------------------------------------------------------------------------
// Fortran expression

expr_list_opt
    : expr_list { $$ = $1; }
    | %empty { LIST_NEW($$); }
    ;

expr_list
    : expr_list "," expr { $$ = $1; LIST_ADD($$, $3); }
    | expr { LIST_NEW($$); LIST_ADD($$, $1); }
    ;

rbracket
    : "]"
    | "/)"
    ;

expr
// ### primary
    : id { $$ = $1; }
    | struct_member_star id { NAME1($$, $2, $1, @$); }
    | id "(" fnarray_arg_list_opt ")" { $$ = FUNCCALLORARRAY($1, $3, @$); }
    | TK_STRING "(" fnarray_arg_list_opt ")" { $$ = SUBSTRING($1, $3, @$);}
    | struct_member_star id "(" fnarray_arg_list_opt ")" {
            $$ = FUNCCALLORARRAY2($1, $2, $4, @$); }
    | id "(" fnarray_arg_list_opt ")" "(" fnarray_arg_list_opt ")" {
            $$ = FUNCCALLORARRAY3($1, $3, $6, @$); }
    | struct_member_star id "(" fnarray_arg_list_opt ")" "(" fnarray_arg_list_opt ")" {
            $$ = FUNCCALLORARRAY4($1, $2, $4, $7, @$); }
    | id "[" coarray_arg_list "]" {
            $$ = COARRAY1($1, $3, @$); }
    | struct_member_star id "[" coarray_arg_list "]" {
            $$ = COARRAY3($1, $2, $4, @$); }
    | id "(" fnarray_arg_list_opt ")" "[" coarray_arg_list "]" {
            $$ = COARRAY2($1, $3, $6, @$); }
    | struct_member_star id "(" fnarray_arg_list_opt ")" "[" coarray_arg_list "]" {
            $$ = COARRAY4($1, $2, $4, $7, @$); }
    | "[" expr_list_opt rbracket { $$ = ARRAY_IN1($2, @$); }
    | "[" var_type "::" expr_list_opt rbracket %dprec 2 { $$ = ARRAY_IN2($2, $4, @$); }
    | "[" id "::" expr_list_opt rbracket %dprec 1 { $$ = ARRAY_IN3($2, $4, @$); }
    | TK_INTEGER { $$ = INTEGER($1, @$); }
    | TK_REAL { $$ = REAL($1, @$); }
    | TK_STRING { $$ = STRING($1, @$); }
    | TK_BOZ_CONSTANT { $$ = BOZ($1, @$); }
    | ".true."  { $$ = TRUE(@$); }
    | ".false." { $$ = FALSE(@$); }
    | "(" expr ")" { $$ = PAREN($2, @$); }
    | "(" expr "," expr ")" { $$ = COMPLEX($2, $4, @$); }
    | "(" expr "," id "=" expr "," expr ")" {
            $$ = IMPLIED_DO_LOOP1($2, $4, $6, $8, @$); }
    | "(" expr "," expr "," id "=" expr "," expr ")" {
            $$ = IMPLIED_DO_LOOP2($2, $4, $6, $8, $10, @$); }
    | "(" expr "," expr "," expr_list "," id "=" expr "," expr ")" {
            $$ = IMPLIED_DO_LOOP3($2, $4, $6, $8, $10, $12, @$); }
    | "(" expr "," id "=" expr "," expr "," expr ")" {
            $$ = IMPLIED_DO_LOOP4($2, $4, $6, $8, $10, @$); }
    | "(" expr "," expr "," id "=" expr "," expr "," expr ")" {
            $$ = IMPLIED_DO_LOOP5($2, $4, $6, $8, $10, $12, @$); }
    | "(" expr "," expr "," expr_list "," id "=" expr "," expr "," expr ")" {
            $$ = IMPLIED_DO_LOOP6($2, $4, $6, $8, $10, $12, $14, @$); }

// ### level-1
    | TK_DEF_OP expr { $$ = UNARY_DEFOP($1, $2, @$); }

// ### level-2
    | expr "+" expr { $$ = ADD($1, $3, @$); }
    | expr "-" expr { $$ = SUB($1, $3, @$); }
    | expr "*" expr { $$ = MUL($1, $3, @$); }
    | expr "/" expr { $$ = DIV($1, $3, @$); }
    | "-" expr %prec UMINUS { $$ = UNARY_MINUS($2, @$); }
    | "+" expr %prec UMINUS { $$ = UNARY_PLUS ($2, @$); }
    | expr "**" expr { $$ = POW($1, $3, @$); }

// ### level-3
    | expr "//" expr { $$ = STRCONCAT($1, $3, @$); }

// ### level-4
    | expr "==" expr { $$ = EQ($1, $3, @$); }
    | expr "/=" expr { $$ = NE($1, $3, @$); }
    | expr "<" expr { $$ = LT($1, $3, @$); }
    | expr "<=" expr { $$ = LE($1, $3, @$); }
    | expr ">" expr { $$ = GT($1, $3, @$); }
    | expr ">=" expr { $$ = GE($1, $3, @$); }

// ### level-5
    | ".not." expr { $$ = NOT($2, @$); }
    | expr ".and." expr { $$ = AND($1, $3, @$); }
    | expr ".or." expr { $$ = OR($1, $3, @$); }
    | expr ".xor." expr { $$ = XOR($1, $3, @$); }
    | expr ".eqv." expr { $$ = EQV($1, $3, @$); }
    | expr ".neqv." expr { $$ = NEQV($1, $3, @$); }
    | expr TK_DEF_OP expr { $$ = DEFOP($1, $2, $3, @$); }
    ;

struct_member_star
    : struct_member_star struct_member { $$ = $1; PLIST_ADD($$, $2); }
    | struct_member { LIST_NEW($$); PLIST_ADD($$, $1); }
    ;

struct_member
    : id "%" { STRUCT_MEMBER1($$, $1); }
    | id "(" fnarray_arg_list_opt ")" "%" { STRUCT_MEMBER2($$, $1, $3); }
    ;

fnarray_arg_list_opt
    : fnarray_arg_list_opt "," fnarray_arg { $$ = $1; PLIST_ADD($$, $3); }
    | fnarray_arg { LIST_NEW($$); PLIST_ADD($$, $1); }
    | %empty { LIST_NEW($$); }
    ;

fnarray_arg
// array element / function argument
    : expr                   { $$ = ARRAY_COMP_DECL_0i0($1, @$); }
// array section
    | ":"                    { $$ = ARRAY_COMP_DECL_001(@$); }
    | expr ":"               { $$ = ARRAY_COMP_DECL_a01($1, @$); }
    | ":" expr               { $$ = ARRAY_COMP_DECL_0b1($2, @$); }
    | expr ":" expr          { $$ = ARRAY_COMP_DECL_ab1($1, $3, @$); }
    | "::" expr              { $$ = ARRAY_COMP_DECL_00c($2, @$); }
    | ":" ":" expr           { $$ = ARRAY_COMP_DECL_00c($3, @$); }
    | expr "::" expr         { $$ = ARRAY_COMP_DECL_a0c($1, $3, @$); }
    | expr ":" ":" expr      { $$ = ARRAY_COMP_DECL_a0c($1, $4, @$); }
    | ":" expr ":" expr      { $$ = ARRAY_COMP_DECL_0bc($2, $4, @$); }
    | expr ":" expr ":" expr { $$ = ARRAY_COMP_DECL_abc($1, $3, $5, @$); }
// keyword function argument
    | id "=" expr            { $$ = ARRAY_COMP_DECL1k($1, $3, @$); }
    ;

coarray_arg_list
    : coarray_arg_list "," coarray_arg { $$ = $1; PLIST_ADD($$, $3); }
    | coarray_arg { LIST_NEW($$); PLIST_ADD($$, $1); }
    ;

coarray_arg
// array element / function argument
    : expr                   { $$ = COARRAY_COMP_DECL_0i0($1, @$); }
// array section
    | ":"                    { $$ = COARRAY_COMP_DECL_001(@$); }
    | expr ":"               { $$ = COARRAY_COMP_DECL_a01($1, @$); }
    | ":" expr               { $$ = COARRAY_COMP_DECL_0b1($2, @$); }
    | expr ":" expr          { $$ = COARRAY_COMP_DECL_ab1($1, $3, @$); }
    | "::" expr              { $$ = COARRAY_COMP_DECL_00c($2, @$); }
    | ":" ":" expr           { $$ = COARRAY_COMP_DECL_00c($3, @$); }
    | expr "::" expr         { $$ = COARRAY_COMP_DECL_a0c($1, $3, @$); }
    | expr ":" ":" expr      { $$ = COARRAY_COMP_DECL_a0c($1, $4, @$); }
    | ":" expr ":" expr      { $$ = COARRAY_COMP_DECL_0bc($2, $4, @$); }
    | expr ":" expr ":" expr { $$ = COARRAY_COMP_DECL_abc($1, $3, $5, @$); }
// keyword function argument
    | id "=" expr            { $$ = COARRAY_COMP_DECL1k($1, $3, @$); }
// star
    | "*"                    { $$ = COARRAY_COMP_DECL_star(@$); }
    ;

id_list_opt
    : id_list
    | %empty { LIST_NEW($$); }
    ;

id_list
    : id_list "," id { $$ = $1; LIST_ADD($$, $3); }
    | id { LIST_NEW($$); LIST_ADD($$, $1); }
    ;

// id?
id_opt
    : id
    | %empty
    ;


id
    : TK_NAME { $$ = SYMBOL($1, @$); }
    | KW_ABSTRACT { $$ = SYMBOL($1, @$); }
    | KW_ALL { $$ = SYMBOL($1, @$); }
    | KW_ALLOCATABLE { $$ = SYMBOL($1, @$); }
    | KW_ALLOCATE { $$ = SYMBOL($1, @$); }
    | KW_ASSIGN { $$ = SYMBOL($1, @$); }
    | KW_ASSIGNMENT { $$ = SYMBOL($1, @$); }
    | KW_ASSOCIATE { $$ = SYMBOL($1, @$); }
    | KW_ASYNCHRONOUS { $$ = SYMBOL($1, @$); }
    | KW_BACKSPACE { $$ = SYMBOL($1, @$); }
    | KW_BIND { $$ = SYMBOL($1, @$); }
    | KW_BLOCK { $$ = SYMBOL($1, @$); }
    | KW_CALL { $$ = SYMBOL($1, @$); }
    | KW_CASE { $$ = SYMBOL($1, @$); }
    | KW_CHANGE { $$ = SYMBOL($1, @$); }
    | KW_CHARACTER { $$ = SYMBOL($1, @$); }
    | KW_CLASS { $$ = SYMBOL($1, @$); }
    | KW_CLOSE { $$ = SYMBOL($1, @$); }
    | KW_CODIMENSION { $$ = SYMBOL($1, @$); }
    | KW_COMMON { $$ = SYMBOL($1, @$); }
    | KW_COMPLEX { $$ = SYMBOL($1, @$); }
    | KW_CONCURRENT { $$ = SYMBOL($1, @$); }
    | KW_CONTAINS { $$ = SYMBOL($1, @$); }
    | KW_CONTIGUOUS { $$ = SYMBOL($1, @$); }
    | KW_CONTINUE { $$ = SYMBOL($1, @$); }
    | KW_CRITICAL { $$ = SYMBOL($1, @$); }
    | KW_CYCLE { $$ = SYMBOL($1, @$); }
    | KW_DATA { $$ = SYMBOL($1, @$); }
    | KW_DEALLOCATE { $$ = SYMBOL($1, @$); }
    | KW_DEFAULT { $$ = SYMBOL($1, @$); }
    | KW_DEFERRED { $$ = SYMBOL($1, @$); }
    | KW_DIMENSION { $$ = SYMBOL($1, @$); }
    | KW_DO { $$ = SYMBOL($1, @$); }
    | KW_DOWHILE { $$ = SYMBOL($1, @$); }
    | KW_DOUBLE { $$ = SYMBOL($1, @$); }
    | KW_DOUBLE_PRECISION { $$ = SYMBOL($1, @$); }
    | KW_DOUBLE_COMPLEX { $$ = SYMBOL($1, @$); }
    | KW_ELEMENTAL { $$ = SYMBOL($1, @$); }
    | KW_ELSE { $$ = SYMBOL($1, @$); }
    | KW_ELSEIF { $$ = SYMBOL($1, @$); }
    | KW_ELSEWHERE { $$ = SYMBOL($1, @$); }
    | KW_END { $$ = SYMBOL($1, @$); }
    | KW_ENDDO { $$ = SYMBOL($1, @$); }
    | KW_ENDIF { $$ = SYMBOL($1, @$); }
    | KW_ENDINTERFACE { $$ = SYMBOL($1, @$); }
    | KW_ENDTYPE { $$ = SYMBOL($1, @$); }
    | KW_ENDPROGRAM { $$ = SYMBOL($1, @$); }
    | KW_ENDMODULE { $$ = SYMBOL($1, @$); }
    | KW_ENDSUBMODULE { $$ = SYMBOL($1, @$); }
    | KW_ENDBLOCK { $$ = SYMBOL($1, @$); }
    | KW_ENDBLOCKDATA { $$ = SYMBOL($1, @$); }
    | KW_ENDSUBROUTINE { $$ = SYMBOL($1, @$); }
    | KW_ENDFUNCTION { $$ = SYMBOL($1, @$); }
    | KW_ENDPROCEDURE { $$ = SYMBOL($1, @$); }
    | KW_ENDENUM { $$ = SYMBOL($1, @$); }
    | KW_ENDSELECT { $$ = SYMBOL($1, @$); }
    | KW_ENDASSOCIATE { $$ = SYMBOL($1, @$); }
    | KW_ENDFORALL { $$ = SYMBOL($1, @$); }
    | KW_ENDWHERE { $$ = SYMBOL($1, @$); }
    | KW_ENDCRITICAL { $$ = SYMBOL($1, @$); }
    | KW_ENDFILE { $$ = SYMBOL($1, @$); }
    | KW_ENTRY { $$ = SYMBOL($1, @$); }
    | KW_ENUM { $$ = SYMBOL($1, @$); }
    | KW_ENUMERATOR { $$ = SYMBOL($1, @$); }
    | KW_EQUIVALENCE { $$ = SYMBOL($1, @$); }
    | KW_ERRMSG { $$ = SYMBOL($1, @$); }
    | KW_ERROR { $$ = SYMBOL($1, @$); }
    | KW_EVENT { $$ = SYMBOL($1, @$); }
    | KW_EXIT { $$ = SYMBOL($1, @$); }
    | KW_EXTENDS { $$ = SYMBOL($1, @$); }
    | KW_EXTERNAL { $$ = SYMBOL($1, @$); }
    | KW_FILE { $$ = SYMBOL($1, @$); }
    | KW_FINAL { $$ = SYMBOL($1, @$); }
    | KW_FLUSH { $$ = SYMBOL($1, @$); }
    | KW_FORALL { $$ = SYMBOL($1, @$); }
    | KW_FORMATTED { $$ = SYMBOL($1, @$); }
    | KW_FORM { $$ = SYMBOL($1, @$); }
    | KW_FORM_TEAM { $$ = SYMBOL($1, @$); }
    | KW_FUNCTION { $$ = SYMBOL($1, @$); }
    | KW_GENERIC { $$ = SYMBOL($1, @$); }
    | KW_GO { $$ = SYMBOL($1, @$); }
    | KW_GOTO { $$ = SYMBOL($1, @$); }
    | KW_IF { $$ = SYMBOL($1, @$); }
    | KW_IMAGES { $$ = SYMBOL($1, @$); }
    | KW_IMPLICIT { $$ = SYMBOL($1, @$); }
    | KW_IMPORT { $$ = SYMBOL($1, @$); }
    | KW_IMPURE { $$ = SYMBOL($1, @$); }
    | KW_IN { $$ = SYMBOL($1, @$); }
    | KW_INCLUDE { $$ = SYMBOL($1, @$); }
    | KW_INOUT { $$ = SYMBOL($1, @$); }
    | KW_INQUIRE { $$ = SYMBOL($1, @$); }
    | KW_INSTANTIATE { $$ = SYMBOL($1, @$); }
    | KW_INTEGER { $$ = SYMBOL($1, @$); }
    | KW_INTENT { $$ = SYMBOL($1, @$); }
    | KW_INTERFACE { $$ = SYMBOL($1, @$); }
    | KW_INTRINSIC { $$ = SYMBOL($1, @$); }
    | KW_IS { $$ = SYMBOL($1, @$); }
    | KW_KIND { $$ = SYMBOL($1, @$); }
    | KW_LEN { $$ = SYMBOL($1, @$); }
    | KW_LOCAL { $$ = SYMBOL($1, @$); }
    | KW_LOCAL_INIT { $$ = SYMBOL($1, @$); }
    | KW_LOGICAL { $$ = SYMBOL($1, @$); }
    | KW_MEMORY { $$ = SYMBOL($1, @$); }
    | KW_MODULE { $$ = SYMBOL($1, @$); }
    | KW_MOLD { $$ = SYMBOL($1, @$); }
    | KW_NAME { $$ = SYMBOL($1, @$); }
    | KW_NAMELIST { $$ = SYMBOL($1, @$); }
    | KW_NEW_INDEX { $$ = SYMBOL($1, @$); }
    | KW_NOPASS { $$ = SYMBOL($1, @$); }
    | KW_NON_INTRINSIC { $$ = SYMBOL($1, @$); }
    | KW_NON_OVERRIDABLE { $$ = SYMBOL($1, @$); }
    | KW_NON_RECURSIVE { $$ = SYMBOL($1, @$); }
    | KW_NONE { $$ = SYMBOL($1, @$); }
    | KW_NULLIFY { $$ = SYMBOL($1, @$); }
    | KW_ONLY { $$ = SYMBOL($1, @$); }
    | KW_OPEN { $$ = SYMBOL($1, @$); }
    | KW_OPERATOR { $$ = SYMBOL($1, @$); }
    | KW_OPTIONAL { $$ = SYMBOL($1, @$); }
    | KW_OUT { $$ = SYMBOL($1, @$); }
    | KW_PARAMETER { $$ = SYMBOL($1, @$); }
    | KW_PASS { $$ = SYMBOL($1, @$); }
    | KW_POINTER { $$ = SYMBOL($1, @$); }
    | KW_POST { $$ = SYMBOL($1, @$); }
    | KW_PRECISION { $$ = SYMBOL($1, @$); }
    | KW_PRINT { $$ = SYMBOL($1, @$); }
    | KW_PRIVATE { $$ = SYMBOL($1, @$); }
    | KW_PROCEDURE { $$ = SYMBOL($1, @$); }
    | KW_PROGRAM { $$ = SYMBOL($1, @$); }
    | KW_PROTECTED { $$ = SYMBOL($1, @$); }
    | KW_PUBLIC { $$ = SYMBOL($1, @$); }
    | KW_PURE { $$ = SYMBOL($1, @$); }
    | KW_QUIET { $$ = SYMBOL($1, @$); }
    | KW_RANK { $$ = SYMBOL($1, @$); }
    | KW_READ { $$ = SYMBOL($1, @$); }
    | KW_REAL { $$ = SYMBOL($1, @$); }
    | KW_RECURSIVE { $$ = SYMBOL($1, @$); }
    | KW_REDUCE { $$ = SYMBOL($1, @$); }
    | KW_REQUIREMENT { $$ = SYMBOL($1, @$); }
    | KW_REQUIRES { $$ = SYMBOL($1, @$); }
    | KW_RESULT { $$ = SYMBOL($1, @$); }
    | KW_RETURN { $$ = SYMBOL($1, @$); }
    | KW_REWIND { $$ = SYMBOL($1, @$); }
    | KW_SAVE { $$ = SYMBOL($1, @$); }
    | KW_SELECT { $$ = SYMBOL($1, @$); }
    | KW_SELECT_CASE { $$ = SYMBOL($1, @$); }
    | KW_SELECT_RANK { $$ = SYMBOL($1, @$); }
    | KW_SELECT_TYPE { $$ = SYMBOL($1, @$); }
    | KW_SEQUENCE { $$ = SYMBOL($1, @$); }
    | KW_SHARED { $$ = SYMBOL($1, @$); }
    | KW_SOURCE { $$ = SYMBOL($1, @$); }
    | KW_STAT { $$ = SYMBOL($1, @$); }
    | KW_STOP { $$ = SYMBOL($1, @$); }
    | KW_SUBMODULE { $$ = SYMBOL($1, @$); }
    | KW_SUBROUTINE { $$ = SYMBOL($1, @$); }
    | KW_SYNC { $$ = SYMBOL($1, @$); }
    | KW_TARGET { $$ = SYMBOL($1, @$); }
    | KW_TEAM { $$ = SYMBOL($1, @$); }
    | KW_TEAM_NUMBER { $$ = SYMBOL($1, @$); }
    | KW_TEMPLATE { $$ = SYMBOL($1, @$); }
    | KW_THEN { $$ = SYMBOL($1, @$); }
    | KW_TO { $$ = SYMBOL($1, @$); }
    | KW_TYPE { $$ = SYMBOL($1, @$); }
    | KW_UNFORMATTED { $$ = SYMBOL($1, @$); }
    | KW_USE { $$ = SYMBOL($1, @$); }
    | KW_VALUE { $$ = SYMBOL($1, @$); }
    | KW_VOLATILE { $$ = SYMBOL($1, @$); }
    | KW_WAIT { $$ = SYMBOL($1, @$); }
    | KW_WHERE { $$ = SYMBOL($1, @$); }
    | KW_WHILE { $$ = SYMBOL($1, @$); }
    | KW_WRITE { $$ = SYMBOL($1, @$); }
    ;
