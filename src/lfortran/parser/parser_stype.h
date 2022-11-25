#ifndef LFORTRAN_PARSER_STYPE_H
#define LFORTRAN_PARSER_STYPE_H

#include <cstring>
#include <lfortran/ast.h>
#include <libasr/location.h>
#include <libasr/containers.h>
#include <libasr/bigint.h>

namespace LCompilers::LFortran {

struct VarType {
    Location loc;
    Str string;
    Vec<AST::kind_item_t> kind;
    char *identifier;
};

struct FnArg {
    bool keyword;
    union {
        AST::fnarg_t arg;
        AST::keyword_t kw;
    };
};

struct CoarrayArg {
    bool keyword;
    union {
        AST::coarrayarg_t arg;
        AST::keyword_t kw;
    };
};

struct ArgStarKw {
    bool keyword;
    union {
        AST::argstar_t arg;
        AST::kw_argstar_t kw;
    };
};

struct IntSuffix {
    BigInt::BigInt int_n;
    Str int_kind;
};

union YYSTYPE {
    int64_t n;
    Str string;

    IntSuffix int_suffix;

    AST::ast_t* ast;
    Vec<AST::ast_t*> vec_ast;

    AST::var_sym_t *var_sym;
    Vec<AST::var_sym_t> vec_var_sym;

    AST::dimension_t *dim;
    Vec<AST::dimension_t> vec_dim;

    AST::codimension_t *codim;
    Vec<AST::codimension_t> vec_codim;

    AST::reduce_opType reduce_op_type;

    VarType *var_type;

    AST::kind_item_t *kind_arg;
    Vec<AST::kind_item_t> vec_kind_arg;

    FnArg *fnarg;
    Vec<FnArg> vec_fnarg;

    CoarrayArg *coarrayarg;
    Vec<CoarrayArg> vec_coarrayarg;

    ArgStarKw *argstarkw;
    Vec<ArgStarKw> vec_argstarkw;

    AST::struct_member_t *struct_member;
    Vec<AST::struct_member_t> vec_struct_member;

    AST::intrinsicopType interface_op_type;

    AST::equi_t *equi;
    Vec<AST::equi_t> vec_equi;
};

static_assert(std::is_standard_layout<YYSTYPE>::value);
static_assert(std::is_trivial<YYSTYPE>::value);
// Ensure the YYSTYPE size is equal to Vec<AST::ast_t*>, which is a required member, so
// YYSTYPE has to be at least as big, but it should not be bigger, otherwise it
// would reduce performance.
static_assert(sizeof(YYSTYPE) == sizeof(Vec<AST::ast_t*>));

} // namespace LCompilers::LFortran


typedef struct LCompilers::Location YYLTYPE;
#define YYLTYPE_IS_DECLARED 1
#define YYLTYPE_IS_TRIVIAL 0


#endif // LFORTRAN_PARSER_STYPE_H
