#ifndef LIBASR_PASS_INTRINSIC_FUNCTIONS_H
#define LIBASR_PASS_INTRINSIC_FUNCTIONS_H


#include <libasr/asr_builder.h>
#include <libasr/casting_utils.h>

namespace LCompilers::ASRUtils {

/*
To add a new function implementation,

1. Create a new namespace like, `Sin`, `LogGamma` in this file.
2. In the above created namespace add `eval_*`, `instantiate_*`, and `create_*`.
3. Then register in the maps present in `IntrinsicScalarFunctionRegistry`.

You can use helper macros and define your own helper macros to reduce
the code size.
*/

enum class IntrinsicScalarFunctions : int64_t {
    Kind, // if kind is reordered, update `extract_kind` in `asr_utils.h`
    Rank,
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
    Sinh,
    Cosh,
    Tanh,
    Atan2,
    Asinh,
    Acosh,
    Atanh,
    Erf,
    Erfc,
    Gamma,
    Log,
    Log10,
    LogGamma,
    Trunc,
    Fix,
    Abs,
    Aimag,
    Exp,
    Exp2,
    Expm1,
    FMA,
    FlipSign,
    Mod,
    Trailz,
    Shiftr,
    Shiftl,
    Ishft,
    Leadz,
    Digits,
    Repeat,
    Hypot,
    MinExponent,
    MaxExponent,
    FloorDiv,
    ListIndex,
    Partition,
    ListReverse,
    ListPop,
    Reserve,
    DictKeys,
    DictValues,
    SetAdd,
    SetRemove,
    Max,
    Min,
    Radix,
    Sign,
    SignFromValue,
    Nint,
    Aint,
    Anint,
    Sqrt,
    Sngl,
    Ifix,
    Idint,
    Floor,
    Ceiling,
    SymbolicSymbol,
    SymbolicAdd,
    SymbolicSub,
    SymbolicMul,
    SymbolicDiv,
    SymbolicPow,
    SymbolicPi,
    SymbolicE,
    SymbolicInteger,
    SymbolicDiff,
    SymbolicExpand,
    SymbolicSin,
    SymbolicCos,
    SymbolicLog,
    SymbolicExp,
    SymbolicAbs,
    SymbolicHasSymbolQ,
    SymbolicAddQ,
    SymbolicMulQ,
    SymbolicPowQ,
    SymbolicLogQ,
    SymbolicSinQ,
    SymbolicGetArgument,
    // ...
};

typedef ASR::expr_t* (*impl_function)(
    Allocator&, const Location &,
    SymbolTable*, Vec<ASR::ttype_t*>&, ASR::ttype_t *,
    Vec<ASR::call_arg_t>&, int64_t);

typedef ASR::expr_t* (*eval_intrinsic_function)(
    Allocator&, const Location &, ASR::ttype_t *,
    Vec<ASR::expr_t*>&);

typedef ASR::asr_t* (*create_intrinsic_function)(
    Allocator&, const Location&,
    Vec<ASR::expr_t*>&,
    diag::Diagnostics&);

typedef void (*verify_function)(
    const ASR::IntrinsicScalarFunction_t&,
    diag::Diagnostics&);

typedef ASR::expr_t* (*get_initial_value_func)(Allocator&, ASR::ttype_t*);

namespace UnaryIntrinsicFunction {

static inline ASR::expr_t* instantiate_functions(Allocator &al,
        const Location &loc, SymbolTable *scope, std::string new_name,
        ASR::ttype_t *arg_type, ASR::ttype_t *return_type,
        Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
    std::string c_func_name;
    switch (arg_type->type) {
        case ASR::ttypeType::Complex : {
            if (ASRUtils::extract_kind_from_ttype_t(arg_type) == 4) {
                c_func_name = "_lfortran_c" + new_name;
            } else {
                c_func_name = "_lfortran_z" + new_name;
            }
            break;
        }
        default : {
            if (ASRUtils::extract_kind_from_ttype_t(arg_type) == 4) {
                c_func_name = "_lfortran_s" + new_name;
            } else {
                c_func_name = "_lfortran_d" + new_name;
            }
        }
    }
    new_name = "_lcompilers_" + new_name + "_" + type_to_str_python(arg_type);

    declare_basic_variables(new_name);
    if (scope->get_symbol(new_name)) {
        ASR::symbol_t *s = scope->get_symbol(new_name);
        ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(s);
        return b.Call(s, new_args, expr_type(f->m_return_var));
    }
    fill_func_arg("x", arg_type);
    auto result = declare(new_name, ASRUtils::extract_type(return_type), ReturnVar);

    {
        SymbolTable *fn_symtab_1 = al.make_new<SymbolTable>(fn_symtab);
        Vec<ASR::expr_t*> args_1;
        {
            args_1.reserve(al, 1);
            ASR::expr_t *arg = b.Variable(fn_symtab_1, "x", arg_type,
                ASR::intentType::In, ASR::abiType::BindC, true);
            args_1.push_back(al, arg);
        }

        ASR::expr_t *return_var_1 = b.Variable(fn_symtab_1, c_func_name,
            return_type, ASRUtils::intent_return_var, ASR::abiType::BindC, false);

        SetChar dep_1; dep_1.reserve(al, 1);
        Vec<ASR::stmt_t*> body_1; body_1.reserve(al, 1);
        ASR::symbol_t *s = make_ASR_Function_t(c_func_name, fn_symtab_1, dep_1, args_1,
            body_1, return_var_1, ASR::abiType::BindC, ASR::deftypeType::Interface, s2c(al, c_func_name));
        fn_symtab->add_symbol(c_func_name, s);
        dep.push_back(al, s2c(al, c_func_name));
        body.push_back(al, b.Assignment(result, b.Call(s, args, return_type)));
    }

    ASR::symbol_t *new_symbol = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
        body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
    scope->add_symbol(fn_name, new_symbol);
    return b.Call(new_symbol, new_args, return_type);
}

static inline ASR::asr_t* create_UnaryFunction(Allocator& al, const Location& loc,
    Vec<ASR::expr_t*>& args, eval_intrinsic_function eval_function,
    int64_t intrinsic_id, int64_t overload_id, ASR::ttype_t* type) {
    ASR::expr_t *value = nullptr;
    if (ASRUtils::all_args_evaluated(args)) {
        Vec<ASR::expr_t*> arg_values; arg_values.reserve(al, 1);
        arg_values.push_back(al, ASRUtils::expr_value(args[0]));
        value = eval_function(al, loc, type, arg_values);
    }

    return ASRUtils::make_IntrinsicScalarFunction_t_util(al, loc, intrinsic_id,
        args.p, args.n, overload_id, type, value);
}

static inline ASR::symbol_t *create_KMP_function(Allocator &al,
        const Location &loc, SymbolTable *scope)
{
    /*
     * Knuth-Morris-Pratt (KMP) string-matching
     * This function takes two parameters:
     *     the sub-string or pattern string and the target string,
     * then returns the position of the first occurrence of the
     * string in the pattern.
     */
    declare_basic_variables("KMP_string_matching");
    fill_func_arg("target_string", character(-2));
    fill_func_arg("pattern", character(-2));

    auto result = declare("result", int32, ReturnVar);
    auto pi_len = declare("pi_len", int32, Local);
    auto i = declare("i", int32, Local);
    auto j = declare("j", int32, Local);
    auto s_len = declare("s_len", int32, Local);
    auto pat_len = declare("pat_len", int32, Local);
    auto flag = declare("flag", logical, Local);
    auto lps = declare("lps", List(int32), Local);

    body.push_back(al, b.Assignment(s_len, StringLen(args[0])));
    body.push_back(al, b.Assignment(pat_len, StringLen(args[1])));
    body.push_back(al, b.Assignment(result, i32_n(-1)));
    body.push_back(al, b.If(iEq(pat_len, i32(0)), {
            b.Assignment(result, i32(0)), Return()
        }, {
            b.If(iEq(s_len, i32(0)), { Return() }, {})
        }));
    body.push_back(al, b.Assignment(lps,
        EXPR(ASR::make_ListConstant_t(al, loc, nullptr, 0, List(int32)))));
    body.push_back(al, b.Assignment(i, i32(0)));
    body.push_back(al, b.While(iLtE(i, iSub(pat_len, i32(1))), {
        b.Assignment(i, iAdd(i, i32(1))),
        ListAppend(lps, i32(0))
    }));
    body.push_back(al, b.Assignment(flag, bool32(false)));
    body.push_back(al, b.Assignment(i, i32(1)));
    body.push_back(al, b.Assignment(pi_len, i32(0)));
    body.push_back(al, b.While(iLt(i, pat_len), {
        b.If(sEq(StringItem(args[1], iAdd(i, i32(1))),
                 StringItem(args[1], iAdd(pi_len, i32(1)))), {
            b.Assignment(pi_len, iAdd(pi_len, i32(1))),
            b.Assignment(ListItem(lps, i, int32), pi_len),
            b.Assignment(i, iAdd(i, i32(1)))
        }, {
            b.If(iNotEq(pi_len, i32(0)), {
                b.Assignment(pi_len, ListItem(lps, iSub(pi_len, i32(1)), int32))
            }, {
                b.Assignment(i, iAdd(i, i32(1)))
            })
        })
    }));
    body.push_back(al, b.Assignment(j, i32(0)));
    body.push_back(al, b.Assignment(i, i32(0)));
    body.push_back(al, b.While(And(iGtE(iSub(s_len, i),
            iSub(pat_len, j)), Not(flag)), {
        b.If(sEq(StringItem(args[1], iAdd(j, i32(1))),
                StringItem(args[0], iAdd(i, i32(1)))), {
            b.Assignment(i, iAdd(i, i32(1))),
            b.Assignment(j, iAdd(j, i32(1)))
        }, {}),
        b.If(iEq(j, pat_len), {
            b.Assignment(result, iSub(i, j)),
            b.Assignment(flag, bool32(true)),
            b.Assignment(j, ListItem(lps, iSub(j, i32(1)), int32))
        }, {
            b.If(And(iLt(i, s_len), sNotEq(StringItem(args[1], iAdd(j, i32(1))),
                    StringItem(args[0], iAdd(i, i32(1))))), {
                b.If(iNotEq(j, i32(0)), {
                    b.Assignment(j, ListItem(lps, iSub(j, i32(1)), int32))
                }, {
                    b.Assignment(i, iAdd(i, i32(1)))
                })
            }, {})
        })
    }));
    body.push_back(al, Return());
    ASR::symbol_t *fn_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
        body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
    scope->add_symbol(fn_name, fn_sym);
    return fn_sym;
}

static inline void verify_args(const ASR::IntrinsicScalarFunction_t& x,
        diag::Diagnostics& diagnostics) {
    const Location& loc = x.base.base.loc;
    ASRUtils::require_impl(x.n_args == 1,
        "Elemental intrinsics must have only 1 input argument",
        loc, diagnostics);

    ASR::ttype_t* input_type = ASRUtils::expr_type(x.m_args[0]);
    ASR::ttype_t* output_type = x.m_type;
    ASRUtils::require_impl(ASRUtils::check_equal_type(input_type, output_type, true),
        "The input and output type of elemental intrinsics must exactly match, input type: " +
        ASRUtils::get_type_code(input_type) + " output type: " + ASRUtils::get_type_code(output_type),
        loc, diagnostics);
}

} // namespace UnaryIntrinsicFunction

namespace BinaryIntrinsicFunction {

static inline ASR::expr_t* instantiate_functions(Allocator &al,
        const Location &loc, SymbolTable *scope, std::string new_name,
        ASR::ttype_t *arg_type, ASR::ttype_t *return_type,
        Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
    std::string c_func_name;
    switch (arg_type->type) {
        case ASR::ttypeType::Complex : {
            if (ASRUtils::extract_kind_from_ttype_t(arg_type) == 4) {
                c_func_name = "_lfortran_c" + new_name;
            } else {
                c_func_name = "_lfortran_z" + new_name;
            }
            break;
        }
        default : {
            if (ASRUtils::extract_kind_from_ttype_t(arg_type) == 4) {
                c_func_name = "_lfortran_s" + new_name;
            } else {
                c_func_name = "_lfortran_d" + new_name;
            }
        }
    }
    new_name = "_lcompilers_" + new_name + "_" + type_to_str_python(arg_type);

    declare_basic_variables(new_name);
    if (scope->get_symbol(new_name)) {
        ASR::symbol_t *s = scope->get_symbol(new_name);
        ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(s);
        return b.Call(s, new_args, expr_type(f->m_return_var));
    }
    fill_func_arg("x", arg_type);
    fill_func_arg("y", arg_type)
    auto result = declare(new_name, return_type, ReturnVar);

    {
        SymbolTable *fn_symtab_1 = al.make_new<SymbolTable>(fn_symtab);
        Vec<ASR::expr_t*> args_1;
        {
            args_1.reserve(al, 2);
            ASR::expr_t *arg_1 = b.Variable(fn_symtab_1, "x", arg_type,
                ASR::intentType::In, ASR::abiType::BindC, true);
            ASR::expr_t *arg_2 = b.Variable(fn_symtab_1, "y", arg_type,
                ASR::intentType::In, ASR::abiType::BindC, true);
            args_1.push_back(al, arg_1);
            args_1.push_back(al, arg_2);
        }

        ASR::expr_t *return_var_1 = b.Variable(fn_symtab_1, c_func_name,
            arg_type, ASRUtils::intent_return_var, ASR::abiType::BindC, false);

        SetChar dep_1; dep_1.reserve(al, 1);
        Vec<ASR::stmt_t*> body_1; body_1.reserve(al, 1);
        ASR::symbol_t *s = make_ASR_Function_t(c_func_name, fn_symtab_1, dep_1, args_1,
            body_1, return_var_1, ASR::abiType::BindC, ASR::deftypeType::Interface, s2c(al, c_func_name));
        fn_symtab->add_symbol(c_func_name, s);
        dep.push_back(al, s2c(al, c_func_name));
        body.push_back(al, b.Assignment(result, b.Call(s, args, arg_type)));
    }

    ASR::symbol_t *new_symbol = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
        body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
    scope->add_symbol(fn_name, new_symbol);
    return b.Call(new_symbol, new_args, return_type);
}

static inline ASR::asr_t* create_BinaryFunction(Allocator& al, const Location& loc,
    Vec<ASR::expr_t*>& args, eval_intrinsic_function eval_function,
    int64_t intrinsic_id, int64_t overload_id, ASR::ttype_t* type) {
    ASR::expr_t *value = nullptr;
    ASR::expr_t *arg_value_1 = ASRUtils::expr_value(args[0]);
    ASR::expr_t *arg_value_2 = ASRUtils::expr_value(args[1]);
    if (arg_value_1 && arg_value_2) {
        Vec<ASR::expr_t*> arg_values;
        arg_values.reserve(al, 2);
        arg_values.push_back(al, arg_value_1);
        arg_values.push_back(al, arg_value_2);
        value = eval_function(al, loc, type, arg_values);
    }

    return ASRUtils::make_IntrinsicScalarFunction_t_util(al, loc, intrinsic_id,
        args.p, args.n, overload_id, type, value);
}

static inline void verify_args(const ASR::IntrinsicScalarFunction_t& x,
        diag::Diagnostics& diagnostics) {
    const Location& loc = x.base.base.loc;
    ASRUtils::require_impl(x.n_args == 2,
        "Binary intrinsics must have only 2 input arguments",
        loc, diagnostics);

    ASR::ttype_t* input_type = ASRUtils::expr_type(x.m_args[0]);
    ASR::ttype_t* input_type_2 = ASRUtils::expr_type(x.m_args[1]);
    ASR::ttype_t* output_type = x.m_type;
    ASRUtils::require_impl(ASRUtils::check_equal_type(input_type, input_type_2, true),
        "The types of both the arguments of binary intrinsics must exactly match, argument 1 type: " +
        ASRUtils::get_type_code(input_type) + " argument 2 type: " + ASRUtils::get_type_code(input_type_2),
        loc, diagnostics);
    ASRUtils::require_impl(ASRUtils::check_equal_type(input_type, output_type, true),
        "The input and output type of elemental intrinsics must exactly match, input type: " +
        ASRUtils::get_type_code(input_type) + " output type: " + ASRUtils::get_type_code(output_type),
        loc, diagnostics);
}

} // namespace BinaryIntrinsicFunction

// `X` is the name of the function in the IntrinsicScalarFunctions enum and
// we use the same name for `create_X` and other places
// `eval_X` is the name of the function in the `std` namespace for compile
//  numerical time evaluation
// `lc_rt_name` is the name that we use in the C runtime library
#define create_unary_function(X, eval_X, lc_rt_name)                            \
namespace X {                                                                   \
    static inline ASR::expr_t *eval_##X(Allocator &al, const Location &loc,     \
            ASR::ttype_t *t, Vec<ASR::expr_t*> &args) {                         \
        double rv = ASR::down_cast<ASR::RealConstant_t>(args[0])->m_r;          \
        return f(std::eval_X(rv), t);                                           \
    }                                                                           \
    static inline ASR::asr_t* create_##X(Allocator &al, const Location &loc,    \
        Vec<ASR::expr_t*> &args,                                                \
        diag::Diagnostics& diag) {                                              \
        ASR::ttype_t *type = ASRUtils::expr_type(args[0]);                      \
        if (args.n != 1) {                                                      \
            append_error(diag, "Intrinsic `"#X"` accepts exactly one argument", \
                loc);                                                           \
            return nullptr;                                                     \
        } else if (!ASRUtils::is_real(*type)) {                                 \
            append_error(diag, "`x` argument of `"#X"` must be real",           \
                args[0]->base.loc);                                             \
            return nullptr;                                                     \
        }                                                                       \
        return UnaryIntrinsicFunction::create_UnaryFunction(al, loc, args,      \
                eval_##X, static_cast<int64_t>(IntrinsicScalarFunctions::X),    \
                0, type);                                                       \
    }                                                                           \
    static inline ASR::expr_t* instantiate_##X (Allocator &al,                  \
            const Location &loc, SymbolTable *scope,                            \
            Vec<ASR::ttype_t*> &arg_types, ASR::ttype_t *return_type,           \
            Vec<ASR::call_arg_t> &new_args, int64_t overload_id) {              \
        return UnaryIntrinsicFunction::instantiate_functions(al, loc, scope,    \
            #lc_rt_name, arg_types[0], return_type, new_args, overload_id);     \
    }                                                                           \
} // namespace X

create_unary_function(Trunc, trunc, trunc)
create_unary_function(Gamma, tgamma, gamma)
create_unary_function(LogGamma, lgamma, log_gamma)
create_unary_function(Log10, log10, log10)
create_unary_function(Erf, erf, erf)
create_unary_function(Erfc, erfc, erfc)

namespace Fix {
    static inline ASR::expr_t *eval_Fix(Allocator &al, const Location &loc,
            ASR::ttype_t *t, Vec<ASR::expr_t*>& args) {
        LCOMPILERS_ASSERT(args.size() == 1);
        double rv = ASR::down_cast<ASR::RealConstant_t>(args[0])->m_r;
        double val;
        if (rv > 0.0) {
            val = floor(rv);
        } else {
            val = ceil(rv);
        }
        return make_ConstantWithType(make_RealConstant_t, val, t, loc);
    }

    static inline ASR::asr_t* create_Fix(Allocator& al, const Location& loc,
        Vec<ASR::expr_t*>& args,
        diag::Diagnostics& diag) {
        ASR::ttype_t *type = ASRUtils::expr_type(args[0]);
        if (args.n != 1) {
            append_error(diag, "Intrinsic `fix` accepts exactly one argument", loc);
            return nullptr;
        } else if (!ASRUtils::is_real(*type)) {
            append_error(diag, "`fix` argument of `fix` must be real",
                args[0]->base.loc);
            return nullptr;
        }
        return UnaryIntrinsicFunction::create_UnaryFunction(al, loc, args,
                eval_Fix, static_cast<int64_t>(IntrinsicScalarFunctions::Fix),
                0, type);
    }

    static inline ASR::expr_t* instantiate_Fix (Allocator &al,
            const Location &loc, SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types,
            ASR::ttype_t *return_type, Vec<ASR::call_arg_t>& new_args,
            int64_t overload_id) {
        ASR::ttype_t* arg_type = arg_types[0];
        return UnaryIntrinsicFunction::instantiate_functions(al, loc, scope,
            "fix", arg_type, return_type, new_args, overload_id);
    }

} // namespace Fix

// `X` is the name of the function in the IntrinsicScalarFunctions enum and
// we use the same name for `create_X` and other places
// `stdeval` is the name of the function in the `std` namespace for compile
//  numerical time evaluation
// `lcompilers_name` is the name that we use in the C runtime library
#define create_trig(X, stdeval, lcompilers_name)                                \
namespace X {                                                                   \
    static inline ASR::expr_t *eval_##X(Allocator &al, const Location &loc,     \
            ASR::ttype_t *t, Vec<ASR::expr_t*>& args) {                         \
        LCOMPILERS_ASSERT(args.size() == 1);                                    \
        double rv = -1;                                                         \
        if( ASRUtils::extract_value(args[0], rv) ) {                            \
            double val = std::stdeval(rv);                                      \
            return make_ConstantWithType(make_RealConstant_t, val, t, loc);     \
        } else {                                                                \
            std::complex<double> crv;                                           \
            if( ASRUtils::extract_value(args[0], crv) ) {                       \
                std::complex<double> val = std::stdeval(crv);                   \
                return ASRUtils::EXPR(ASR::make_ComplexConstant_t(              \
                    al, loc, val.real(), val.imag(), t));                       \
            }                                                                   \
        }                                                                       \
        return nullptr;                                                         \
    }                                                                           \
    static inline ASR::asr_t* create_##X(Allocator& al, const Location& loc,    \
        Vec<ASR::expr_t*>& args,                                                \
        diag::Diagnostics& diag)                                                \
    {                                                                           \
        ASR::ttype_t *type = ASRUtils::expr_type(args[0]);                      \
        if (args.n != 1) {                                                      \
            append_error(diag, "Intrinsic `"#X"` accepts exactly one argument", \
                loc);                                                           \
            return nullptr;                                                     \
        } else if (!ASRUtils::is_real(*type) && !ASRUtils::is_complex(*type)) { \
            append_error(diag, "`x` argument of `"#X"` must be real or complex",\
                args[0]->base.loc);                                             \
            return nullptr;                                                     \
        }                                                                       \
        return UnaryIntrinsicFunction::create_UnaryFunction(al, loc, args,      \
                eval_##X, static_cast<int64_t>(IntrinsicScalarFunctions::X),    \
                0, type);                                                       \
    }                                                                           \
    static inline ASR::expr_t* instantiate_##X (Allocator &al,                  \
            const Location &loc, SymbolTable *scope,                            \
            Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,           \
            Vec<ASR::call_arg_t>& new_args,int64_t overload_id)  {              \
        ASR::ttype_t* arg_type = arg_types[0];                                  \
        return UnaryIntrinsicFunction::instantiate_functions(al, loc, scope,    \
            #lcompilers_name, arg_type, return_type, new_args, overload_id);    \
    }                                                                           \
} // namespace X

create_trig(Sin, sin, sin)
create_trig(Cos, cos, cos)
create_trig(Tan, tan, tan)
create_trig(Asin, asin, asin)
create_trig(Acos, acos, acos)
create_trig(Atan, atan, atan)
create_trig(Sinh, sinh, sinh)
create_trig(Cosh, cosh, cosh)
create_trig(Tanh, tanh, tanh)
create_trig(Asinh, asinh, asinh)
create_trig(Acosh, acosh, acosh)
create_trig(Atanh, atanh, atanh)
create_trig(Log, log, log)

namespace Aimag {

    static inline ASR::expr_t *eval_Aimag(Allocator &al, const Location &loc,
            ASR::ttype_t *t, Vec<ASR::expr_t*>& args) {
        std::complex<double> crv;
        if( ASRUtils::extract_value(args[0], crv) ) {
            return f(crv.imag(), t);
        } else {
            return nullptr;
        }
    }

    static inline ASR::asr_t* create_Aimag(Allocator& al, const Location& loc,
        Vec<ASR::expr_t*>& args,
        diag::Diagnostics& diag) {
        ASR::ttype_t *type = ASRUtils::expr_type(args[0]);
        if (args.n != 1) {
            append_error(diag, "Intrinsic `aimag` accepts exactly one argument", loc);
            return nullptr;
        } else if (!ASRUtils::is_complex(*type)) {
            append_error(diag, "`x` argument of `aimag` must be complex", args[0]->base.loc);
            return nullptr;
        }
        type = TYPE(ASR::make_Real_t(al, type->base.loc, extract_kind_from_ttype_t(type)));
        return UnaryIntrinsicFunction::create_UnaryFunction(al, loc, args, eval_Aimag,
            static_cast<int64_t>(IntrinsicScalarFunctions::Aimag), 0, type);
    }

    static inline ASR::expr_t* instantiate_Aimag (Allocator &al,
            const Location &loc, SymbolTable *scope,
            Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args,int64_t overload_id)  {
        return UnaryIntrinsicFunction::instantiate_functions(al, loc, scope,
            "aimag", arg_types[0], return_type, new_args, overload_id);
    }

} // namespace Aimag

namespace Atan2 {
    static inline ASR::expr_t *eval_Atan2(Allocator &al, const Location &loc,
            ASR::ttype_t *t, Vec<ASR::expr_t*>& args) {
        LCOMPILERS_ASSERT(args.size() == 2);
        double rv = -1, rv2 = -1;
        if( ASRUtils::extract_value(args[0], rv) && ASRUtils::extract_value(args[1], rv2) ) {
            double val = std::atan2(rv,rv2);
            return make_ConstantWithType(make_RealConstant_t, val, t, loc);
        }
        return nullptr;
    }
    static inline ASR::asr_t* create_Atan2(Allocator& al, const Location& loc,
        Vec<ASR::expr_t*>& args,
        diag::Diagnostics& diag)
    {
        ASR::ttype_t *type_1 = ASRUtils::expr_type(args[0]);
        ASR::ttype_t *type_2 = ASRUtils::expr_type(args[1]);
        if (!ASRUtils::is_real(*type_1)) {
            append_error(diag, "`x` argument of \"atan2\" must be real",args[0]->base.loc);
            return nullptr;
        } else if (!ASRUtils::is_real(*type_2)) {
            append_error(diag, "`y` argument of \"atan2\" must be real",args[1]->base.loc);
            return nullptr;
        }
        return BinaryIntrinsicFunction::create_BinaryFunction(al, loc, args,
                eval_Atan2, static_cast<int64_t>(IntrinsicScalarFunctions::Atan2),
                0, type_1);
    }
    static inline ASR::expr_t* instantiate_Atan2 (Allocator &al,
            const Location &loc, SymbolTable *scope,
            Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args,int64_t overload_id) {
        ASR::ttype_t* arg_type = arg_types[0];
        return BinaryIntrinsicFunction::instantiate_functions(al, loc, scope,
            "atan2", arg_type, return_type, new_args, overload_id);
    }
}

namespace Abs {

    static inline void verify_args(const ASR::IntrinsicScalarFunction_t& x, diag::Diagnostics& diagnostics) {
        const Location& loc = x.base.base.loc;
        ASRUtils::require_impl(x.n_args == 1,
            "Elemental intrinsics must have only 1 input argument",
            loc, diagnostics);

        ASR::ttype_t* input_type = ASRUtils::expr_type(x.m_args[0]);
        ASR::ttype_t* output_type = x.m_type;
        std::string input_type_str = ASRUtils::get_type_code(input_type);
        std::string output_type_str = ASRUtils::get_type_code(output_type);
        if( ASR::is_a<ASR::Complex_t>(*ASRUtils::type_get_past_pointer(ASRUtils::type_get_past_array(input_type))) ) {
            ASRUtils::require_impl(ASR::is_a<ASR::Real_t>(*output_type),
                "Abs intrinsic must return output of real for complex input, found: " + output_type_str,
                loc, diagnostics);
            int input_kind = ASRUtils::extract_kind_from_ttype_t(input_type);
            int output_kind = ASRUtils::extract_kind_from_ttype_t(output_type);
            ASRUtils::require_impl(input_kind == output_kind,
                "The input and output type of Abs intrinsic must be of same kind, input kind: " +
                std::to_string(input_kind) + " output kind: " + std::to_string(output_kind),
                loc, diagnostics);
        } else {
            ASRUtils::require_impl(ASRUtils::check_equal_type(input_type, output_type, true),
                "The input and output type of elemental intrinsics must exactly match, input type: " +
                input_type_str + " output type: " + output_type_str, loc, diagnostics);
        }
    }

    static ASR::expr_t *eval_Abs(Allocator &al, const Location &loc,
            ASR::ttype_t *t, Vec<ASR::expr_t*> &args) {
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));
        ASR::expr_t* arg = args[0];
        if (ASRUtils::is_real(*expr_type(arg))) {
            double rv = ASR::down_cast<ASR::RealConstant_t>(arg)->m_r;
            double val = std::abs(rv);
            return make_ConstantWithType(make_RealConstant_t, val, t, loc);
        } else if (ASRUtils::is_integer(*expr_type(arg))) {
            int64_t rv = ASR::down_cast<ASR::IntegerConstant_t>(arg)->m_n;
            int64_t val = std::abs(rv);
            return make_ConstantWithType(make_IntegerConstant_t, val, t, loc);
        } else if (ASRUtils::is_complex(*expr_type(arg))) {
            double re = ASR::down_cast<ASR::ComplexConstant_t>(arg)->m_re;
            double im = ASR::down_cast<ASR::ComplexConstant_t>(arg)->m_im;
            std::complex<double> x(re, im);
            double result = std::abs(x);
            return make_ConstantWithType(make_RealConstant_t, result, t, loc);
        } else {
            return nullptr;
        }
    }

    static inline ASR::asr_t* create_Abs(Allocator& al, const Location& loc,
            Vec<ASR::expr_t*>& args,
            diag::Diagnostics& diag) {
        if (args.size() != 1) {
            append_error(diag, "Intrinsic abs function accepts exactly 1 argument", loc);
            return nullptr;
        }
        ASR::ttype_t *type = ASRUtils::expr_type(args[0]);
        if (!ASRUtils::is_integer(*type) && !ASRUtils::is_real(*type)
                && !ASRUtils::is_complex(*type)) {
            append_error(diag, "Argument of the abs function must be Integer, Real or Complex",
                args[0]->base.loc);
            return nullptr;
        }
        if (is_complex(*type)) {
            type = TYPE(ASR::make_Real_t(al, type->base.loc,
                ASRUtils::extract_kind_from_ttype_t(type)));
        }
        return UnaryIntrinsicFunction::create_UnaryFunction(al, loc, args, eval_Abs,
            static_cast<int64_t>(IntrinsicScalarFunctions::Abs), 0, ASRUtils::type_get_past_allocatable(type));
    }

    static inline ASR::expr_t* instantiate_Abs(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        std::string func_name = "_lcompilers_abs_" + type_to_str_python(arg_types[0]);
        declare_basic_variables(func_name);
        if (scope->get_symbol(func_name)) {
            ASR::symbol_t *s = scope->get_symbol(func_name);
            ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(s);
            return b.Call(s, new_args, expr_type(f->m_return_var), nullptr);
        }
        fill_func_arg("x", arg_types[0]);

        auto result = declare(func_name, return_type, ReturnVar);

        if (is_integer(*arg_types[0]) || is_real(*arg_types[0])) {
            /*
             * if (x >= 0) then
             *     r = x
             * else
             *     r = -x
             * end if
             */
            ASR::expr_t *test;
            ASR::expr_t *negative_x;
            if (is_integer(*arg_types[0])) {
                ASR::expr_t* zero = make_ConstantWithType(make_IntegerConstant_t, 0, arg_types[0], loc);
                test = make_Compare(make_IntegerCompare_t, args[0], GtE, zero);
                negative_x = EXPR(ASR::make_IntegerUnaryMinus_t(al, loc, args[0],
                    arg_types[0], nullptr));
            } else {
                ASR::expr_t* zero = make_ConstantWithType(make_RealConstant_t, 0.0, arg_types[0], loc);
                test = make_Compare(make_RealCompare_t, args[0], GtE, zero);
                negative_x = EXPR(ASR::make_RealUnaryMinus_t(al, loc, args[0],
                    arg_types[0], nullptr));
            }

            Vec<ASR::stmt_t *> if_body; if_body.reserve(al, 1);
            if_body.push_back(al, b.Assignment(result, args[0]));
            Vec<ASR::stmt_t *> else_body; else_body.reserve(al, 1);
            else_body.push_back(al, b.Assignment(result, negative_x));
            body.push_back(al, STMT(ASR::make_If_t(al, loc, test,
                if_body.p, if_body.n, else_body.p, else_body.n)));
        } else {
            // * Complex type: `r = (real(x)**2 + aimag(x)**2)**0.5`
            ASR::ttype_t *real_type = TYPE(ASR::make_Real_t(al, loc,
                                        ASRUtils::extract_kind_from_ttype_t(arg_types[0])));
            ASR::symbol_t *sym_result = ASR::down_cast<ASR::Var_t>(result)->m_v;
            ASR::Variable_t *r_var = ASR::down_cast<ASR::Variable_t>(sym_result);
            r_var->m_type = return_type = real_type;
            ASR::expr_t *aimag_of_x;
            {
                std::string c_func_name;
                if (ASRUtils::extract_kind_from_ttype_t(arg_types[0]) == 4) {
                    c_func_name = "_lfortran_caimag";
                } else {
                    c_func_name = "_lfortran_zaimag";
                }
                SymbolTable *fn_symtab_1 = al.make_new<SymbolTable>(fn_symtab);
                Vec<ASR::expr_t*> args_1;
                {
                    args_1.reserve(al, 1);
                    auto arg = b.Variable(fn_symtab_1, "x", arg_types[0],
                        ASR::intentType::In, ASR::abiType::BindC, true);
                    args_1.push_back(al, arg);
                }

                auto return_var_1 = b.Variable(fn_symtab_1, c_func_name, real_type,
                    ASR::intentType::ReturnVar, ASR::abiType::BindC, false);

                SetChar dep_1; dep_1.reserve(al, 1);
                Vec<ASR::stmt_t*> body_1; body_1.reserve(al, 1);
                ASR::symbol_t *s = make_ASR_Function_t(c_func_name, fn_symtab_1, dep_1, args_1,
                    body_1, return_var_1, ASR::abiType::BindC, ASR::deftypeType::Interface, s2c(al, c_func_name));
                fn_symtab->add_symbol(c_func_name, s);
                dep.push_back(al, s2c(al, c_func_name));
                Vec<ASR::call_arg_t> call_args;
                {
                    call_args.reserve(al, 1);
                    ASR::call_arg_t arg;
                    arg.loc = args[0]->base.loc;
                    arg.m_value = args[0];
                    call_args.push_back(al, arg);
                }
                aimag_of_x = b.Call(s, call_args, real_type);
            }
            ASR::expr_t *constant_two = make_ConstantWithType(make_RealConstant_t, 2.0, real_type, loc);
            ASR::expr_t *constant_point_five = make_ConstantWithType(make_RealConstant_t, 0.5, real_type, loc);
            ASR::expr_t *real_of_x = EXPR(ASR::make_Cast_t(al, loc, args[0],
                ASR::cast_kindType::ComplexToReal, real_type, nullptr));

            ASR::expr_t *bin_op_1 = b.ElementalPow(real_of_x, constant_two, loc);
            ASR::expr_t *bin_op_2 = b.ElementalPow(aimag_of_x, constant_two, loc);

            bin_op_1 = b.ElementalAdd(bin_op_1, bin_op_2, loc);

            body.push_back(al, b.Assignment(result,
                b.ElementalPow(bin_op_1, constant_point_five, loc)));
        }

        ASR::symbol_t *f_sym = make_ASR_Function_t(func_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(func_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);
    }

} // namespace Abs

namespace Radix {
    static ASR::expr_t *eval_Radix(Allocator &al, const Location &loc,
            ASR::ttype_t* /*t1*/, Vec<ASR::expr_t*> &/*args*/) {
        return i32(2);
    }

    static inline ASR::expr_t* instantiate_Radix(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        std::string func_name = "_lcompilers_radix_" + type_to_str_python(arg_types[0]);
        declare_basic_variables(func_name);
        if (scope->get_symbol(func_name)) {
            ASR::symbol_t *s = scope->get_symbol(func_name);
            ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(s);
            return b.Call(s, new_args, expr_type(f->m_return_var), nullptr);
        }
        fill_func_arg("x", arg_types[0]);
        auto result = declare(fn_name, int32, ReturnVar);
        body.push_back(al, b.Assignment(result, i32(2)));
        ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
                body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);
    }

}  // namespace Radix

namespace Sign {

    static ASR::expr_t *eval_Sign(Allocator &al, const Location &loc,
            ASR::ttype_t* t1, Vec<ASR::expr_t*> &args) {
        if (ASRUtils::is_real(*t1)) {
            double rv1 = std::abs(ASR::down_cast<ASR::RealConstant_t>(args[0])->m_r);
            double rv2 = ASR::down_cast<ASR::RealConstant_t>(args[1])->m_r;
            rv1 = copysign(rv1, rv2);
            return make_ConstantWithType(make_RealConstant_t, rv1, t1, loc);
        } else {
            int64_t iv1 = std::abs(ASR::down_cast<ASR::IntegerConstant_t>(args[0])->m_n);
            int64_t iv2 = ASR::down_cast<ASR::IntegerConstant_t>(args[1])->m_n;
            if (iv2 < 0) iv1 = -iv1;
            return make_ConstantWithType(make_IntegerConstant_t, iv1, t1, loc);
        }
    }

    static inline ASR::expr_t* instantiate_Sign(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        declare_basic_variables("_lcompilers_sign_" + type_to_str_python(arg_types[0]));
        fill_func_arg("x", arg_types[0]);
        fill_func_arg("y", arg_types[0]);
        auto result = declare(fn_name, return_type, ReturnVar);
        if (is_real(*arg_types[0])) {
            Vec<ASR::expr_t*> args; args.reserve(al, 2);
            visit_expr_list(al, new_args, args);
            ASR::expr_t* real_copy_sign = ASRUtils::EXPR(ASR::make_RealCopySign_t(al, loc, args[0], args[1], arg_types[0], nullptr));
            return real_copy_sign;
        } else {
            /*
            * r = abs(x)
            * if (y < 0) then
            *     r = -r
            * end if
            */
            ASR::expr_t *zero = i(0, arg_types[0]);
            body.push_back(al, b.If(iGtE(args[0], zero), {
                b.Assignment(result, args[0])
            }, /* else */  {
                b.Assignment(result, i32_neg(args[0], arg_types[0]))
            }));
            body.push_back(al, b.If(iLt(args[1], zero), {
                b.Assignment(result, i32_neg(result, arg_types[0]))
            }, {}));

            ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
                body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
            scope->add_symbol(fn_name, f_sym);
            return b.Call(f_sym, new_args, return_type, nullptr);
        }
    }

} // namespace Sign

namespace Shiftr {

    static inline void verify_args(const ASR::IntrinsicScalarFunction_t& x, diag::Diagnostics& diagnostics) {
        ASRUtils::require_impl(x.n_args == 2,
            "Call to `shiftr` must have exactly two arguments",
            x.base.base.loc, diagnostics);
        ASR::ttype_t *type1 = ASRUtils::expr_type(x.m_args[0]);
        ASR::ttype_t *type2 = ASRUtils::expr_type(x.m_args[1]);
        ASRUtils::require_impl((is_integer(*type1) && is_integer(*type2)),
            "Arguments to `shiftr` must be of integer type",
            x.base.base.loc, diagnostics);
    }

    static ASR::expr_t *eval_Shiftr(Allocator &al, const Location &loc,
            ASR::ttype_t* t1, Vec<ASR::expr_t*> &args) {
        int64_t val1 = ASR::down_cast<ASR::IntegerConstant_t>(args[0])->m_n;
        int64_t val2 = ASR::down_cast<ASR::IntegerConstant_t>(args[1])->m_n;
        int64_t val = val1 >> val2;
        return make_ConstantWithType(make_IntegerConstant_t, val, t1, loc);
    }

    static inline ASR::asr_t* create_Shiftr(Allocator& al, const Location& loc,
            Vec<ASR::expr_t*>& args,
            diag::Diagnostics& diag) {
        if (args.size() != 2) {
            append_error(diag, "Intrinsic `shiftr` function accepts exactly 2 arguments", loc);
            return nullptr;
        }
        ASR::ttype_t *type1 = ASRUtils::expr_type(args[0]);
        ASR::ttype_t *type2 = ASRUtils::expr_type(args[1]);
        if (!ASRUtils::is_integer(*type1) || !ASRUtils::is_integer(*type2)) {
            append_error(diag, "Arguments of the `shiftr` function must be Integer",
                args[0]->base.loc);
            return nullptr;
        }

        ASR::expr_t *m_value = nullptr;
        if (all_args_evaluated(args)) {
            Vec<ASR::expr_t*> arg_values; arg_values.reserve(al, 2);
            arg_values.push_back(al, expr_value(args[0]));
            arg_values.push_back(al, expr_value(args[1]));
            m_value = eval_Shiftr(al, loc, expr_type(args[0]), arg_values);
        }
        return ASR::make_IntrinsicScalarFunction_t(al, loc,
            static_cast<int64_t>(IntrinsicScalarFunctions::Shiftr),
            args.p, args.n, 0, ASRUtils::expr_type(args[0]), m_value);
    }

    static inline ASR::expr_t* instantiate_Shiftr(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        declare_basic_variables("_lcompilers_sign_" + type_to_str_python(arg_types[0]));
        fill_func_arg("x", arg_types[0]);
        fill_func_arg("y", arg_types[1]);
        auto result = declare(fn_name, return_type, ReturnVar);
        /*
        * r = shiftr(x, y)
        * r = x / 2**y
        */
        ASR::expr_t *two = i(2, arg_types[0]);
        body.push_back(al, b.Assignment(result, i_tDiv(args[0], iPow(two, args[1], arg_types[0]), arg_types[0])));

        ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);

    }

} // namespace Shiftr

namespace Shiftl {

    static inline void verify_args(const ASR::IntrinsicScalarFunction_t& x, diag::Diagnostics& diagnostics) {
        ASRUtils::require_impl(x.n_args == 2,
            "Call to `shiftl` must have exactly two arguments",
            x.base.base.loc, diagnostics);
        ASR::ttype_t *type1 = ASRUtils::expr_type(x.m_args[0]);
        ASR::ttype_t *type2 = ASRUtils::expr_type(x.m_args[1]);
        ASRUtils::require_impl((is_integer(*type1) && is_integer(*type2)),
            "Arguments to `shiftl` must be of integer type",
            x.base.base.loc, diagnostics);
    }

    static ASR::expr_t *eval_Shiftl(Allocator &al, const Location &loc,
            ASR::ttype_t* t1, Vec<ASR::expr_t*> &args) {
        int64_t val1 = ASR::down_cast<ASR::IntegerConstant_t>(args[0])->m_n;
        int64_t val2 = ASR::down_cast<ASR::IntegerConstant_t>(args[1])->m_n;
        int64_t val = val1 << val2;
        return make_ConstantWithType(make_IntegerConstant_t, val, t1, loc);
    }

    static inline ASR::asr_t* create_Shiftl(Allocator& al, const Location& loc,
            Vec<ASR::expr_t*>& args,
            diag::Diagnostics& diag) {
        if (args.size() != 2) {
            append_error(diag, "Intrinsic `shiftl` function accepts exactly 2 arguments", loc);
            return nullptr;
        }
        ASR::ttype_t *type1 = ASRUtils::expr_type(args[0]);
        ASR::ttype_t *type2 = ASRUtils::expr_type(args[1]);
        if (!ASRUtils::is_integer(*type1) || !ASRUtils::is_integer(*type2)) {
            append_error(diag, "Arguments of the `shiftl` function must be Integer",
                args[0]->base.loc);
            return nullptr;
        }

        ASR::expr_t *m_value = nullptr;
        if (all_args_evaluated(args)) {
            Vec<ASR::expr_t*> arg_values; arg_values.reserve(al, 2);
            arg_values.push_back(al, expr_value(args[0]));
            arg_values.push_back(al, expr_value(args[1]));
            m_value = eval_Shiftl(al, loc, expr_type(args[0]), arg_values);
        }
        return ASR::make_IntrinsicScalarFunction_t(al, loc,
            static_cast<int64_t>(IntrinsicScalarFunctions::Shiftl),
            args.p, args.n, 0, ASRUtils::expr_type(args[0]), m_value);
    }

    static inline ASR::expr_t* instantiate_Shiftl(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        declare_basic_variables("_lcompilers_shiftl_" + type_to_str_python(arg_types[0]));
        fill_func_arg("x", arg_types[0]);
        fill_func_arg("y", arg_types[1]);
        auto result = declare(fn_name, return_type, ReturnVar);
        /*
        * r = shiftl(x, y)
        * r = x * 2**y
        */
        ASR::expr_t *two = i(2, arg_types[0]);
        body.push_back(al, b.Assignment(result, i_tMul(args[0], iPow(two, args[1], arg_types[0]), arg_types[0])));

        ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);

    }

} // namespace Shiftl

namespace Ishft {

    static ASR::expr_t *eval_Ishft(Allocator &al, const Location &loc,
            ASR::ttype_t* t1, Vec<ASR::expr_t*> &args) {
        int64_t val1 = ASR::down_cast<ASR::IntegerConstant_t>(args[0])->m_n;
        int64_t val2 = ASR::down_cast<ASR::IntegerConstant_t>(args[1])->m_n;
        int64_t val;
        if(val2<=0){
            val2 = val2 * -1;
            val = val1 >> val2;
        } else {
            val = val1 << val2;
        }
        return make_ConstantWithType(make_IntegerConstant_t, val, t1, loc);
    }

    static inline ASR::expr_t* instantiate_Ishft(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        declare_basic_variables("_lcompilers_ishft_" + type_to_str_python(arg_types[0]));
        fill_func_arg("x", arg_types[0]);
        fill_func_arg("y", arg_types[1]);
        auto result = declare(fn_name, return_type, ReturnVar);
        /*
        * r = ishft(x, y)
        * if ( y <= 0) {
        *   r = x / 2 ** ( -1 * y )
        * } else {
        *   r = x * 2 ** y
        * }
        */
        ASR::expr_t *two = i(2, arg_types[0]);
        ASR::expr_t *m_one = i(-1, arg_types[0]);
        body.push_back(al, b.If(iLtE(args[1], i(0, arg_types[0])), {
            b.Assignment(result, i_tDiv(args[0], iPow(two, iMul(m_one, args[1]), arg_types[0]), arg_types[0]))
        }, {
            b.Assignment(result, i_tMul(args[0], iPow(two, args[1], arg_types[0]), arg_types[0]))
        }));

        ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);
    }

} // namespace Ishft

namespace Aint {

    static ASR::expr_t *eval_Aint(Allocator &al, const Location &loc,
            ASR::ttype_t* arg_type, Vec<ASR::expr_t*> &args) {
        double rv = ASR::down_cast<ASR::RealConstant_t>(expr_value(args[0]))->m_r;
        return f(std::trunc(rv), arg_type);
    }

    static inline ASR::asr_t* create_Aint(
            Allocator& al, const Location& loc, Vec<ASR::expr_t*>& args,
            diag::Diagnostics& diag) {
        ASR::ttype_t* return_type = expr_type(args[0]);
        if (!(args.size() == 1 || args.size() == 2)) {
            append_error(diag, "Intrinsic `aint` function accepts exactly 1 or 2 arguments", loc);
            return nullptr;
        } else if (!ASRUtils::is_real(*return_type)) {
            append_error(diag, "Argument of the `aint` function must be Real", args[0]->base.loc);
            return nullptr;
        }
        Vec<ASR::expr_t *> m_args; m_args.reserve(al, 1);
        m_args.push_back(al, args[0]);
        if ( args[1] ) {
            int kind = -1;
            if (!ASR::is_a<ASR::Integer_t>(*expr_type(args[1])) ||
                    !extract_value(args[1], kind)) {
                append_error(diag, "`kind` argument of the `aint` function must be a "
                    "scalar Integer constant", args[1]->base.loc);
                return nullptr;
            }
            return_type = TYPE(ASR::make_Real_t(al, return_type->base.loc, kind));
        }
        ASR::expr_t *m_value = nullptr;
        if (all_args_evaluated(m_args)) {
            m_value = eval_Aint(al, loc, return_type, m_args);
        }
        return ASR::make_IntrinsicScalarFunction_t(al, loc,
            static_cast<int64_t>(IntrinsicScalarFunctions::Aint),
            m_args.p, m_args.n, 0, return_type, m_value);
    }

    static inline ASR::expr_t* instantiate_Aint(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        std::string func_name = "_lcompilers_aint_" + type_to_str_python(arg_types[0]);
        std::string fn_name = scope->get_unique_name(func_name);
        SymbolTable *fn_symtab = al.make_new<SymbolTable>(scope);
        Vec<ASR::expr_t*> args;
        args.reserve(al, new_args.size());
        ASRBuilder b(al, loc);
        Vec<ASR::stmt_t*> body; body.reserve(al, 1);
        SetChar dep; dep.reserve(al, 1);
        if (scope->get_symbol(fn_name)) {
            ASR::symbol_t *s = scope->get_symbol(fn_name);
            ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(s);
            return b.Call(s, new_args, expr_type(f->m_return_var), nullptr);
        }
        fill_func_arg("a", arg_types[0]);
        auto result = declare(fn_name, return_type, ReturnVar);

        // Cast: Real -> Integer -> Real
        // TODO: this approach doesn't work for numbers > i64_max
        body.push_back(al, b.Assignment(result, i2r(r2i64(args[0]), return_type)));

        ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);
    }

}  // namespace Aint

namespace Anint {

    static ASR::expr_t *eval_Anint(Allocator &al, const Location &loc,
            ASR::ttype_t* arg_type, Vec<ASR::expr_t*> &args) {
        double rv = ASR::down_cast<ASR::RealConstant_t>(expr_value(args[0]))->m_r;
        return f(std::round(rv), arg_type);
    }

    static inline ASR::asr_t* create_Anint(
            Allocator& al, const Location& loc, Vec<ASR::expr_t*>& args,
            diag::Diagnostics& diag) {
        ASR::ttype_t* return_type = expr_type(args[0]);
        if (!(args.size() == 1 || args.size() == 2)) {
            append_error(diag, "Intrinsic `anint` function accepts exactly 1 or 2 arguments", loc);
            return nullptr;
        } else if (!ASRUtils::is_real(*return_type)) {
            append_error(diag, "Argument of the `anint` function must be Real", args[0]->base.loc);
            return nullptr;
        }
        Vec<ASR::expr_t *> m_args; m_args.reserve(al, 1);
        m_args.push_back(al, args[0]);
        if ( args[1] ) {
            int kind = -1;
            if (!ASR::is_a<ASR::Integer_t>(*expr_type(args[1])) ||
                    !extract_value(args[1], kind)) {
                append_error(diag, "`kind` argument of the `anint` function must be a "
                    "scalar Integer constant", args[1]->base.loc);
                return nullptr;
            }
            return_type = TYPE(ASR::make_Real_t(al, return_type->base.loc, kind));
        }
        ASR::expr_t *m_value = nullptr;
        if (all_args_evaluated(m_args)) {
            m_value = eval_Anint(al, loc, return_type, m_args);
        }
        return ASR::make_IntrinsicScalarFunction_t(al, loc,
            static_cast<int64_t>(IntrinsicScalarFunctions::Anint),
            m_args.p, m_args.n, 0, return_type, m_value);
    }

    static inline ASR::expr_t* instantiate_Anint(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        std::string func_name = "_lcompilers_anint_" + type_to_str_python(arg_types[0]);
        std::string fn_name = scope->get_unique_name(func_name);
        SymbolTable *fn_symtab = al.make_new<SymbolTable>(scope);
        Vec<ASR::expr_t*> args;
        args.reserve(al, new_args.size());
        ASRBuilder b(al, loc);
        Vec<ASR::stmt_t*> body; body.reserve(al, 1);
        SetChar dep; dep.reserve(al, 1);
        if (scope->get_symbol(fn_name)) {
            ASR::symbol_t *s = scope->get_symbol(fn_name);
            ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(s);
            return b.Call(s, new_args, expr_type(f->m_return_var), nullptr);
        }
        fill_func_arg("a", arg_types[0]);
        auto result = declare(fn_name, return_type, ReturnVar);

        /*
        * if (x > 0) then
        *     r = aint(x+0.5)
        * else
        *     r = aint(x-0.5)
        * end if
        */

        ASR::expr_t *test;
        ASR::expr_t* zero = make_ConstantWithType(make_RealConstant_t, 0.0, arg_types[0], loc);
        test = make_Compare(make_RealCompare_t, args[0], Gt, zero);

        Vec<ASR::ttype_t*> arg_types_aint; arg_types_aint.reserve(al, 1);
        arg_types_aint.push_back(al, arg_types[0]);

        Vec<ASR::call_arg_t> new_args_aint1; new_args_aint1.reserve(al, 1);
        ASR::call_arg_t arg1; arg1.loc = loc; arg1.m_value = rAdd(args[0], f(0.5, arg_types_aint[0]), arg_types_aint[0]);
        new_args_aint1.push_back(al, arg1);

        Vec<ASR::call_arg_t> new_args_aint2; new_args_aint2.reserve(al, 1);
        ASR::call_arg_t arg2; arg2.loc = loc; arg2.m_value = rSub(args[0], f(0.5, arg_types_aint[0]), arg_types_aint[0]);
        new_args_aint2.push_back(al, arg2);

        ASR::expr_t* func_call_aint_pos = Aint::instantiate_Aint(al, loc, scope, arg_types_aint, return_type, new_args_aint1, 0);
        ASR::expr_t* func_call_aint_neg = Aint::instantiate_Aint(al, loc, scope, arg_types_aint, return_type, new_args_aint2, 0);

        Vec<ASR::stmt_t *> if_body; if_body.reserve(al, 1);
        if_body.push_back(al, b.Assignment(result, func_call_aint_pos));
        Vec<ASR::stmt_t *> else_body; else_body.reserve(al, 1);
        else_body.push_back(al, b.Assignment(result, func_call_aint_neg));
        body.push_back(al, STMT(ASR::make_If_t(al, loc, test,
            if_body.p, if_body.n, else_body.p, else_body.n)));

        ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);
    }

}  // namespace Anint

namespace Nint {

    static ASR::expr_t *eval_Nint(Allocator &al, const Location &loc,
            ASR::ttype_t* arg_type, Vec<ASR::expr_t*> &args) {
        double rv = ASR::down_cast<ASR::RealConstant_t>(expr_value(args[0]))->m_r;
        double near_integer = std::round(rv);
        int result = int(near_integer);
        return make_ConstantWithType(make_IntegerConstant_t, result, arg_type, loc);
    }

    static inline ASR::asr_t* create_Nint(Allocator& al, const Location& loc,
            Vec<ASR::expr_t*>& args, diag::Diagnostics& diag) {
        ASR::ttype_t* return_type = TYPE(ASR::make_Integer_t(al, loc, 4));
        if (!(args.size() == 1 || args.size() == 2)) {
            append_error(diag, "Intrinsic `Nint` function accepts exactly 1 or 2 arguments", loc);
            return nullptr;
        } else if (!ASRUtils::is_real(*ASRUtils::expr_type(args[0]))) {
            append_error(diag, "Argument of the `Nint` function must be Real", args[0]->base.loc);
            return nullptr;
        }
        Vec<ASR::expr_t *> m_args; m_args.reserve(al, 1);
        m_args.push_back(al, args[0]);
        if ( args[1] != nullptr ) {
            int kind = -1;
            if (!ASR::is_a<ASR::Integer_t>(*expr_type(args[1])) ||
                    !extract_value(args[1], kind)) {
                append_error(diag, "`kind` argument of the `Nint` function must be a "
                    "scalar Integer constant", args[1]->base.loc);
                return nullptr;
            }
            return_type = TYPE(ASR::make_Integer_t(al, return_type->base.loc, kind));
        }
        ASR::expr_t *m_value = nullptr;
        if (all_args_evaluated(m_args)) {
            Vec<ASR::expr_t*> arg_values; arg_values.reserve(al, 1);
            arg_values.push_back(al, expr_value(args[0]));
            m_value = eval_Nint(al, loc, return_type, arg_values);
        }
        return ASR::make_IntrinsicScalarFunction_t(al, loc,
            static_cast<int64_t>(IntrinsicScalarFunctions::Nint),
            m_args.p, m_args.n, 0, return_type, m_value);
    }

    static inline ASR::expr_t* instantiate_Nint(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        declare_basic_variables("_lcompilers_nint_" + type_to_str_python(arg_types[0]));
        fill_func_arg("x", arg_types[0]);
        auto result = declare(fn_name, return_type, ReturnVar);
        /*
        * r = nint(x)
        * r = int(anint(x))
        */

        ASR::ttype_t* return_type_real = expr_type(args[0]);

        Vec<ASR::ttype_t*> arg_types_mod; arg_types_mod.reserve(al, 1);
        arg_types_mod.push_back(al, arg_types[0]);

        Vec<ASR::call_arg_t> new_args_mod; new_args_mod.reserve(al, 1);
        ASR::call_arg_t arg1; arg1.loc = loc; arg1.m_value = args[0];
        new_args_mod.push_back(al, arg1);

        ASR::expr_t* func_call_anint = Anint::instantiate_Anint(al, loc, scope, arg_types_mod, return_type_real, new_args_mod, 0);
        ASR::expr_t *cast = ASRUtils::EXPR(ASR::make_Cast_t(al, loc, func_call_anint, ASR::cast_kindType::RealToInteger, return_type, nullptr));

        body.push_back(al,b.Assignment(result,cast));

        ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);
    }
} // namespace Nint


namespace Floor {

    static ASR::expr_t *eval_Floor(Allocator &al, const Location &loc,
            ASR::ttype_t* t1, Vec<ASR::expr_t*> &args) {
        float val = ASR::down_cast<ASR::RealConstant_t>(args[0])->m_r;
        int result;
        if (val < 0.0) {
            result = int(val)-1;
        } else {
            result = int(val);
        }
        return make_ConstantWithType(make_IntegerConstant_t, result, t1, loc);
    }

    static inline ASR::asr_t* create_Floor(Allocator& al, const Location& loc,
            Vec<ASR::expr_t*>& args,
            diag::Diagnostics& diag) {
        ASR::ttype_t* return_type = TYPE(ASR::make_Integer_t(al, loc, 4));
        if (!(args.size() == 1 || args.size() == 2)) {
            append_error(diag, "Intrinsic `Floor` function accepts exactly 1 or 2 arguments", loc);
            return nullptr;
        } else if (!ASRUtils::is_real(*ASRUtils::expr_type(args[0]))) {
            append_error(diag, "Argument of the `Floor` function must be Real", args[0]->base.loc);
            return nullptr;
        }
        Vec<ASR::expr_t *> m_args; m_args.reserve(al, 1);
        m_args.push_back(al, args[0]);
        if ( args[1] != nullptr ) {
            int kind = -1;
            if (!ASR::is_a<ASR::Integer_t>(*expr_type(args[1])) ||
                    !extract_value(args[1], kind)) {
                append_error(diag, "`kind` argument of the `Floor` function must be a "
                    "scalar Integer constant", args[1]->base.loc);
                return nullptr;
            }
            return_type = TYPE(ASR::make_Integer_t(al, return_type->base.loc, kind));
        }
        ASR::expr_t *m_value = nullptr;

        if (all_args_evaluated(m_args)) {
            Vec<ASR::expr_t*> arg_values; arg_values.reserve(al, 1);
            arg_values.push_back(al, expr_value(args[0]));
            m_value = eval_Floor(al, loc, return_type, arg_values);

        }
        return ASR::make_IntrinsicScalarFunction_t(al, loc,
            static_cast<int64_t>(IntrinsicScalarFunctions::Floor),
            m_args.p, m_args.n, 0, return_type, m_value);
    }

    static inline ASR::expr_t* instantiate_Floor(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        declare_basic_variables("_lcompilers_floor_" + type_to_str_python(arg_types[0]));
        fill_func_arg("x", arg_types[0]);
        auto result = declare(fn_name, return_type, ReturnVar);
        /*
        * r = floor(x)
        * if(x < 0.00){
        *   r = int(x) - 1
        * } else {
        *   r = int(x)
        * }
        */
        ASR::expr_t *one = i(1, return_type);
        ASR::expr_t *cast = ASRUtils::EXPR(ASR::make_Cast_t(al, loc, args[0], ASR::cast_kindType::RealToInteger, return_type, nullptr));
        body.push_back(al, b.If(fLt(args[0], f(0, arg_types[0])), {
            b.Assignment(result,i_tSub(cast,one,return_type))}, {b.Assignment(result,cast)
        }));
        ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);

    }

} // namespace Floor

namespace Ceiling {

    static ASR::expr_t *eval_Ceiling(Allocator &al, const Location &loc,
            ASR::ttype_t* t1, Vec<ASR::expr_t*> &args) {
        double val = ASR::down_cast<ASR::RealConstant_t>(args[0])->m_r;
        double difference = val - double(int(val));
        int64_t result;
        if (difference == 0.0) {
            result = int(val);
        } else if(val <= 0.0){
            result = int(val);
        } else{
            result = int(val) + 1;
        }
        return make_ConstantWithType(make_IntegerConstant_t, result, t1, loc);
    }

    static inline ASR::asr_t* create_Ceiling(Allocator& al, const Location& loc,
            Vec<ASR::expr_t*>& args,
            diag::Diagnostics& diag) {
        ASR::ttype_t* return_type = TYPE(ASR::make_Integer_t(al, loc, 4));
        if (!(args.size() == 1 || args.size() == 2)) {
            append_error(diag, "Intrinsic `Ceiling` function accepts exactly 1 or 2 arguments", loc);
            return nullptr;
        } else if (!ASRUtils::is_real(*ASRUtils::expr_type(args[0]))) {
            append_error(diag, "Argument of the `Ceiling` function must be Real", args[0]->base.loc);
            return nullptr;
        }
        Vec<ASR::expr_t *> m_args; m_args.reserve(al, 1);
        m_args.push_back(al, args[0]);
        if ( args[1] != nullptr ) {
            int kind = -1;
            if (!ASR::is_a<ASR::Integer_t>(*expr_type(args[1])) ||
                    !extract_value(args[1], kind)) {
                append_error(diag, "`kind` argument of the `Ceiling` function must be a "
                    "scalar Integer constant", args[1]->base.loc);
                return nullptr;
            }
            return_type = TYPE(ASR::make_Integer_t(al, return_type->base.loc, kind));
        }
        ASR::expr_t *m_value = nullptr;

        if (all_args_evaluated(m_args)) {
            Vec<ASR::expr_t*> arg_values; arg_values.reserve(al, 1);
            arg_values.push_back(al, expr_value(args[0]));
            m_value = eval_Ceiling(al, loc, return_type, arg_values);

        }
        return ASR::make_IntrinsicScalarFunction_t(al, loc,
            static_cast<int64_t>(IntrinsicScalarFunctions::Ceiling),
            m_args.p, m_args.n, 0, return_type, m_value);
    }

    static inline ASR::expr_t* instantiate_Ceiling(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        declare_basic_variables("_lcompilers_Ceiling_" + type_to_str_python(arg_types[0]));
        fill_func_arg("x", arg_types[0]);
        auto result = declare(fn_name, return_type, ReturnVar);
        /*
        * r = Ceiling(x)
        * if(x >= 0.00){
        *   if(x == int(x)){
        *       r = int(x)
        *   } else {
        *       r = int(x) + 1
        *   }
        * } else {
        *   r = int(x)
        * }
        */
        ASR::expr_t *one = i(1, return_type);
        ASR::expr_t *cast = ASRUtils::EXPR(ASR::make_Cast_t(al, loc, args[0], ASR::cast_kindType::RealToInteger, return_type, nullptr));
        ASR::expr_t *cast1 = ASRUtils::EXPR(ASR::make_Cast_t(al, loc, cast, ASR::cast_kindType::IntegerToReal, return_type, nullptr));
        ASR::expr_t *cast2 = ASRUtils::EXPR(ASR::make_Cast_t(al, loc, args[0], ASR::cast_kindType::RealToReal, return_type, nullptr));

        body.push_back(al, b.If(fGtE(args[0], f(0, arg_types[0])), {
            b.If(fEq(cast2, cast1),
            {b.Assignment(result,cast)}, {b.Assignment(result,i_tAdd(cast,one,return_type))})},
            {b.Assignment(result,cast)
        }));
        ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);

    }

} // namespace Ceiling

namespace Sqrt {

    static ASR::expr_t *eval_Sqrt(Allocator &al, const Location &loc,
            ASR::ttype_t* arg_type, Vec<ASR::expr_t*> &args) {
        if (is_real(*arg_type)) {
            double val = ASR::down_cast<ASR::RealConstant_t>(expr_value(args[0]))->m_r;
            return f(std::sqrt(val), arg_type);
        } else {
            std::complex<double> crv;
            if( ASRUtils::extract_value(args[0], crv) ) {
                std::complex<double> val = std::sqrt(crv);
                return ASRUtils::EXPR(ASR::make_ComplexConstant_t(
                    al, loc, val.real(), val.imag(), arg_type));
            } else {
                return nullptr;
            }
        }
    }

    static inline ASR::expr_t* instantiate_Sqrt(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t overload_id) {
        ASR::ttype_t* arg_type = arg_types[0];
        if (is_real(*arg_type)) {
            return EXPR(ASR::make_RealSqrt_t(al, loc,
                new_args[0].m_value, return_type, nullptr));
        } else {
            return UnaryIntrinsicFunction::instantiate_functions(al, loc, scope,
                "sqrt", arg_type, return_type, new_args, overload_id);
        }
    }

}  // namespace Sqrt

namespace Sngl {

    static ASR::expr_t *eval_Sngl(Allocator &al, const Location &loc,
            ASR::ttype_t* arg_type, Vec<ASR::expr_t*> &args) {
        double val = ASR::down_cast<ASR::RealConstant_t>(expr_value(args[0]))->m_r;
        return f(val, arg_type);
    }

    static inline ASR::expr_t* instantiate_Sngl(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        std::string func_name = "_lcompilers_sngl_" + type_to_str_python(arg_types[0]);
        std::string fn_name = scope->get_unique_name(func_name);
        SymbolTable *fn_symtab = al.make_new<SymbolTable>(scope);
        Vec<ASR::expr_t*> args;
        args.reserve(al, new_args.size());
        ASRBuilder b(al, loc);
        Vec<ASR::stmt_t*> body; body.reserve(al, 1);
        SetChar dep; dep.reserve(al, 1);
        if (scope->get_symbol(fn_name)) {
            ASR::symbol_t *s = scope->get_symbol(fn_name);
            ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(s);
            return b.Call(s, new_args, expr_type(f->m_return_var), nullptr);
        }
        fill_func_arg("a", arg_types[0]);
        auto result = declare(fn_name, return_type, ReturnVar);
        body.push_back(al, b.Assignment(result, r2r32(args[0])));

        ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);
    }

}  // namespace Sngl

namespace Ifix {

    static inline void verify_args(const ASR::IntrinsicScalarFunction_t& x,
            diag::Diagnostics& diagnostics) {
        ASRUtils::require_impl(x.n_args == 1,
            "ASR Verify: Call `ifix` must have exactly one argument",
            x.base.base.loc, diagnostics);
        ASR::ttype_t *type = ASRUtils::expr_type(x.m_args[0]);
        int kind = ASRUtils::extract_kind_from_ttype_t(type);
        ASRUtils::require_impl(ASRUtils::is_real(*type) && kind == 4,
            "ASR Verify: Arguments to `ifix` must be of real type",
            x.base.base.loc, diagnostics);
    }

    static ASR::expr_t *eval_Ifix(Allocator &al, const Location &loc,
            ASR::ttype_t* /*arg_type*/, Vec<ASR::expr_t*> &args) {
        int val = ASR::down_cast<ASR::RealConstant_t>(expr_value(args[0]))->m_r;
        return make_ConstantWithType(make_IntegerConstant_t, val, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)), loc);
    }

    static inline ASR::asr_t* create_Ifix(
            Allocator& al, const Location& loc, Vec<ASR::expr_t*>& args,
            diag::Diagnostics& diag) {
        ASR::ttype_t* return_type = int32;
        if ( args.n != 1 ) {
            append_error(diag, "Intrinsic `ifix` accepts exactly one argument", loc);
            return nullptr;
        } else if ( !is_real(*expr_type(args[0])) ) {
            append_error(diag, "Argument of the `ifix` must be Real", loc);
            return nullptr;
        }
        Vec<ASR::expr_t *> m_args; m_args.reserve(al, 1);
        m_args.push_back(al, args[0]);
        ASR::expr_t *m_value = nullptr;
        if (all_args_evaluated(m_args)) {
            m_value = eval_Ifix(al, loc, return_type, m_args);
        }
        return ASR::make_IntrinsicScalarFunction_t(al, loc,
            static_cast<int64_t>(IntrinsicScalarFunctions::Ifix),
            m_args.p, m_args.n, 0, return_type, m_value);
    }

    static inline ASR::expr_t* instantiate_Ifix(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        std::string func_name = "_lcompilers_ifix_" + type_to_str_python(arg_types[0]);
        std::string fn_name = scope->get_unique_name(func_name);
        SymbolTable *fn_symtab = al.make_new<SymbolTable>(scope);
        Vec<ASR::expr_t*> args;
        args.reserve(al, new_args.size());
        ASRBuilder b(al, loc);
        Vec<ASR::stmt_t*> body; body.reserve(al, 1);
        SetChar dep; dep.reserve(al, 1);
        if (scope->get_symbol(fn_name)) {
            ASR::symbol_t *s = scope->get_symbol(fn_name);
            ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(s);
            return b.Call(s, new_args, expr_type(f->m_return_var), nullptr);
        }
        fill_func_arg("a", arg_types[0]);
        auto result = declare(fn_name, return_type, ReturnVar);
        body.push_back(al, b.Assignment(result, r2i32(args[0])));

        ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);
    }

}  // namespace Ifix

namespace Idint {

    static inline void verify_args(const ASR::IntrinsicScalarFunction_t& x,
            diag::Diagnostics& diagnostics) {
        ASRUtils::require_impl(x.n_args == 1,
            "ASR Verify: Call `idint` must have exactly one argument",
            x.base.base.loc, diagnostics);
        ASR::ttype_t *type = ASRUtils::expr_type(x.m_args[0]);
        int kind = ASRUtils::extract_kind_from_ttype_t(type);
        ASRUtils::require_impl(ASRUtils::is_real(*type) && kind == 8,
            "ASR Verify: Arguments to `idint` must be of double precision type",
            x.base.base.loc, diagnostics);
    }

    static ASR::expr_t *eval_Idint(Allocator &al, const Location &loc,
            ASR::ttype_t* /*arg_type*/, Vec<ASR::expr_t*> &args) {
        int val = ASR::down_cast<ASR::RealConstant_t>(expr_value(args[0]))->m_r;
        return make_ConstantWithType(make_IntegerConstant_t, val, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)), loc);
    }

    static inline ASR::asr_t* create_Idint(
            Allocator& al, const Location& loc, Vec<ASR::expr_t*>& args,
            diag::Diagnostics& diag) {
        ASR::ttype_t* return_type = int32;
        if ( args.n != 1 ) {
            append_error(diag, "Intrinsic `idint` accepts exactly one argument", loc);
            return nullptr;
        } else if ( !is_real(*expr_type(args[0])) ) {
            append_error(diag, "Argument of the `idint` must be Double Precision", loc);
            return nullptr;
        }
        Vec<ASR::expr_t *> m_args; m_args.reserve(al, 1);
        m_args.push_back(al, args[0]);
        ASR::expr_t *m_value = nullptr;
        if (all_args_evaluated(m_args)) {
            m_value = eval_Idint(al, loc, return_type, m_args);
        }
        return ASR::make_IntrinsicScalarFunction_t(al, loc,
            static_cast<int64_t>(IntrinsicScalarFunctions::Idint),
            m_args.p, m_args.n, 0, return_type, m_value);
    }

    static inline ASR::expr_t* instantiate_Idint(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        std::string func_name = "_lcompilers_idint_" + type_to_str_python(arg_types[0]);
        std::string fn_name = scope->get_unique_name(func_name);
        SymbolTable *fn_symtab = al.make_new<SymbolTable>(scope);
        Vec<ASR::expr_t*> args;
        args.reserve(al, new_args.size());
        ASRBuilder b(al, loc);
        Vec<ASR::stmt_t*> body; body.reserve(al, 1);
        SetChar dep; dep.reserve(al, 1);
        if (scope->get_symbol(fn_name)) {
            ASR::symbol_t *s = scope->get_symbol(fn_name);
            ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(s);
            return b.Call(s, new_args, expr_type(f->m_return_var), nullptr);
        }
        fill_func_arg("a", arg_types[0]);
        auto result = declare(fn_name, return_type, ReturnVar);
        body.push_back(al, b.Assignment(result, r2i32(args[0])));

        ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);
    }

}

namespace FMA {

     static inline void verify_args(const ASR::IntrinsicScalarFunction_t& x, diag::Diagnostics& diagnostics) {
        ASRUtils::require_impl(x.n_args == 3,
            "ASR Verify: Call to FMA must have exactly 3 arguments",
            x.base.base.loc, diagnostics);
        ASR::ttype_t *type1 = ASRUtils::expr_type(x.m_args[0]);
        ASR::ttype_t *type2 = ASRUtils::expr_type(x.m_args[1]);
        ASR::ttype_t *type3 = ASRUtils::expr_type(x.m_args[2]);
        ASRUtils::require_impl((is_real(*type1) && is_real(*type2) && is_real(*type3)),
            "ASR Verify: Arguments to FMA must be of real type",
            x.base.base.loc, diagnostics);
    }

    static ASR::expr_t *eval_FMA(Allocator &al, const Location &loc,
            ASR::ttype_t* t1, Vec<ASR::expr_t*> &args) {
        double a = ASR::down_cast<ASR::RealConstant_t>(args[0])->m_r;
        double b = ASR::down_cast<ASR::RealConstant_t>(args[1])->m_r;
        double c = ASR::down_cast<ASR::RealConstant_t>(args[2])->m_r;
        return make_ConstantWithType(make_RealConstant_t, a + b*c, t1, loc);
    }

    static inline ASR::asr_t* create_FMA(Allocator& al, const Location& loc,
            Vec<ASR::expr_t*>& args,
            diag::Diagnostics& diag) {
        if (args.size() != 3) {
            append_error(diag, "Intrinsic FMA function accepts exactly 3 arguments", loc);
            return nullptr;
        }
        ASR::ttype_t *type1 = ASRUtils::expr_type(args[0]);
        ASR::ttype_t *type2 = ASRUtils::expr_type(args[1]);
        ASR::ttype_t *type3 = ASRUtils::expr_type(args[2]);
        if (!ASRUtils::is_real(*type1) || !ASRUtils::is_real(*type2) || !ASRUtils::is_real(*type3)) {
            append_error(diag, "Argument of the FMA function must be Real",
                args[0]->base.loc);
            return nullptr;
        }
        ASR::expr_t *m_value = nullptr;
        if (all_args_evaluated(args)) {
            Vec<ASR::expr_t*> arg_values; arg_values.reserve(al, 3);
            arg_values.push_back(al, expr_value(args[0]));
            arg_values.push_back(al, expr_value(args[1]));
            arg_values.push_back(al, expr_value(args[2]));
            m_value = eval_FMA(al, loc, expr_type(args[0]), arg_values);
        }
        return ASR::make_IntrinsicScalarFunction_t(al, loc,
            static_cast<int64_t>(IntrinsicScalarFunctions::FMA),
            args.p, args.n, 0, ASRUtils::expr_type(args[0]), m_value);
    }

    static inline ASR::expr_t* instantiate_FMA(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        declare_basic_variables("_lcompilers_optimization_fma_" + type_to_str_python(arg_types[0]));
        fill_func_arg("a", arg_types[0]);
        fill_func_arg("b", arg_types[0]);
        fill_func_arg("c", arg_types[0]);
        auto result = declare(fn_name, return_type, ReturnVar);
        /*
         * result = a + b*c
        */

        ASR::expr_t *op1 = b.ElementalMul(args[1], args[2], loc);
        body.push_back(al, b.Assignment(result,
        b.ElementalAdd(args[0], op1, loc)));

        ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);
    }

} // namespace FMA


namespace SignFromValue {

    static ASR::expr_t *eval_SignFromValue(Allocator &al, const Location &loc,
            ASR::ttype_t* t1, Vec<ASR::expr_t*> &args) {
        if (is_real(*t1)) {
            double a = ASR::down_cast<ASR::RealConstant_t>(args[0])->m_r;
            double b = ASR::down_cast<ASR::RealConstant_t>(args[1])->m_r;
            a = (b < 0 ? -a : a);
            return make_ConstantWithType(make_RealConstant_t, a, t1, loc);
        }
        int64_t a = ASR::down_cast<ASR::IntegerConstant_t>(args[0])->m_n;
        int64_t b = ASR::down_cast<ASR::IntegerConstant_t>(args[1])->m_n;
        a = (b < 0 ? -a : a);
        return make_ConstantWithType(make_IntegerConstant_t, a, t1, loc);

    }

    static inline ASR::expr_t* instantiate_SignFromValue(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        declare_basic_variables("_lcompilers_optimization_signfromvalue_" + type_to_str_python(arg_types[0]));
        fill_func_arg("a", arg_types[0]);
        fill_func_arg("b", arg_types[1]);
        auto result = declare(fn_name, return_type, ReturnVar);
        /*
         elemental real(real32) function signfromvaluer32r32(a, b) result(d)
            real(real32), intent(in) :: a, b
            d = a * asignr32(1.0_real32, b)
         end function
        */
        if (is_real(*arg_types[0])) {
            ASR::expr_t *zero = f(0.0, arg_types[1]);
            body.push_back(al, b.If(fLt(args[1], zero), {
                b.Assignment(result, f32_neg(args[0], arg_types[0]))
            }, {
                b.Assignment(result, args[0])
            }));
        } else {
            ASR::expr_t *zero = i(0, arg_types[1]);
            body.push_back(al, b.If(iLt(args[1], zero), {
                b.Assignment(result, i32_neg(args[0], arg_types[0]))
            }, {
                b.Assignment(result, args[0])
            }));
        }
        ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);
    }

} // namespace SignFromValue


namespace FlipSign {

    static ASR::expr_t *eval_FlipSign(Allocator &al, const Location &loc,
            ASR::ttype_t* t1, Vec<ASR::expr_t*> &args) {
        int a = ASR::down_cast<ASR::IntegerConstant_t>(args[0])->m_n;
        double b = ASR::down_cast<ASR::RealConstant_t>(args[1])->m_r;
        if (a % 2 == 1) b = -b;
        return make_ConstantWithType(make_RealConstant_t, b, t1, loc);
    }

    static inline ASR::expr_t* instantiate_FlipSign(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        declare_basic_variables("_lcompilers_optimization_flipsign_" + type_to_str_python(arg_types[1]));
        fill_func_arg("signal", arg_types[0]);
        fill_func_arg("variable", arg_types[1]);
        auto result = declare(fn_name, return_type, ReturnVar);
        /*
        real(real32) function flipsigni32r32(signal, variable)
            integer(int32), intent(in) :: signal
            real(real32), intent(out) :: variable
            integer(int32) :: q
            q = signal/2
            flipsigni32r32 = variable
            if (signal - 2*q == 1 ) flipsigni32r32 = -variable
        end subroutine
        */

        ASR::expr_t *two = i(2, arg_types[0]);
        ASR::expr_t *q = iDiv(args[0], two);
        ASR::expr_t *cond = iSub(args[0], iMul(two, q));
        body.push_back(al, b.If(iEq(cond, i(1, arg_types[0])), {
            b.Assignment(result, f32_neg(args[1], arg_types[1]))
        }, {
            b.Assignment(result, args[1])
        }));

        ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);
    }

} // namespace FlipSign

namespace FloorDiv {

    static ASR::expr_t *eval_FloorDiv(Allocator &al, const Location &loc,
            ASR::ttype_t* t1, Vec<ASR::expr_t*> &args) {
        ASR::ttype_t *type1 = ASRUtils::expr_type(args[0]);
        ASR::ttype_t *type2 = ASRUtils::expr_type(args[1]);
        type1 = ASRUtils::type_get_past_const(type1);
        type2 = ASRUtils::type_get_past_const(type2);
        bool is_real1 = is_real(*type1);
        bool is_real2 = is_real(*type2);
        bool is_int1 = is_integer(*type1);
        bool is_int2 = is_integer(*type2);
        bool is_unsigned_int1 = is_unsigned_integer(*type1);
        bool is_unsigned_int2 = is_unsigned_integer(*type2);
        bool is_logical1 = is_logical(*type1);
        bool is_logical2 = is_logical(*type2);


        if (is_int1 && is_int2) {
            int64_t a = ASR::down_cast<ASR::IntegerConstant_t>(args[0])->m_n;
            int64_t b = ASR::down_cast<ASR::IntegerConstant_t>(args[1])->m_n;
            return make_ConstantWithType(make_IntegerConstant_t, a / b, t1, loc);
        } else if (is_unsigned_int1 && is_unsigned_int2) {
            int64_t a = ASR::down_cast<ASR::UnsignedIntegerConstant_t>(args[0])->m_n;
            int64_t b = ASR::down_cast<ASR::UnsignedIntegerConstant_t>(args[1])->m_n;
            return make_ConstantWithType(make_UnsignedIntegerConstant_t, a / b, t1, loc);
        } else if (is_logical1 && is_logical2) {
            bool a = ASR::down_cast<ASR::LogicalConstant_t>(args[0])->m_value;
            bool b = ASR::down_cast<ASR::LogicalConstant_t>(args[1])->m_value;
            return make_ConstantWithType(make_LogicalConstant_t, a / b, t1, loc);
        } else if (is_real1 && is_real2) {
            double a = ASR::down_cast<ASR::RealConstant_t>(args[0])->m_r;
            double b = ASR::down_cast<ASR::RealConstant_t>(args[1])->m_r;
            double r = a / b;
            int64_t result = (int64_t)r;
            if ( r >= 0.0 || (double)result == r) {
                return make_ConstantWithType(make_RealConstant_t, (double)result, t1, loc);
            }
            return make_ConstantWithType(make_RealConstant_t, (double)(result - 1), t1, loc);
        }
        return nullptr;
    }

    static inline ASR::expr_t* instantiate_FloorDiv(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        declare_basic_variables("_lcompilers_optimization_floordiv_" + type_to_str_python(arg_types[1]));
        fill_func_arg("a", arg_types[0]);
        fill_func_arg("b", arg_types[1]);
        auto r = declare("r", real64, Local);
        auto tmp = declare("tmp", int64, Local);
        auto result = declare("result", return_type, ReturnVar);
        /*
        @overload
        def _lpython_floordiv(a: i32, b: i32) -> i32:
            r: f64 # f32 rounds things up and gives incorrect tmps
            tmp: i64
            result: i32
            r = float(a)/float(b)
            tmp = i64(r)
            if r < 0.0 and f64(tmp) != r:
                tmp = tmp - 1
            result = i32(tmp)
            return result
        */


        ASR::expr_t *op1 = r64Div(CastingUtil::perform_casting(args[0], arg_types[0], real64, al, loc),
            CastingUtil::perform_casting(args[1], arg_types[1], real64, al, loc));
        body.push_back(al, b.Assignment(r, op1));
        body.push_back(al, b.Assignment(tmp, r2i64(r)));
        body.push_back(al, b.If(And(fLt(r, f(0.0, real64)), fNotEq(i2r64(tmp), r)), {
                b.Assignment(tmp, i64Sub(tmp, i(1, int64)))
            }, {}));
        body.push_back(al, b.Assignment(result, CastingUtil::perform_casting(tmp, int64, return_type, al, loc)));
        ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);
    }

} // namespace FloorDiv

namespace Mod {

    static ASR::expr_t *eval_Mod(Allocator &al, const Location &loc,
            ASR::ttype_t* t1, Vec<ASR::expr_t*> &args) {
        bool is_real1 = is_real(*ASRUtils::expr_type(args[0]));
        bool is_real2 = is_real(*ASRUtils::expr_type(args[1]));
        bool is_int1 = is_integer(*ASRUtils::expr_type(args[0]));
        bool is_int2 = is_integer(*ASRUtils::expr_type(args[1]));

        if (is_int1 && is_int2) {
            int64_t a = ASR::down_cast<ASR::IntegerConstant_t>(args[0])->m_n;
            int64_t b = ASR::down_cast<ASR::IntegerConstant_t>(args[1])->m_n;
            return make_ConstantWithType(make_IntegerConstant_t, a % b, t1, loc);
        } else if (is_real1 && is_real2) {
            double a = ASR::down_cast<ASR::RealConstant_t>(args[0])->m_r;
            double b = ASR::down_cast<ASR::RealConstant_t>(args[1])->m_r;
            return make_ConstantWithType(make_RealConstant_t, std::fmod(a, b), t1, loc);
        }
        return nullptr;
    }

    static inline ASR::expr_t* instantiate_Mod(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        declare_basic_variables("_lcompilers_optimization_mod_" + type_to_str_python(arg_types[1]));
        fill_func_arg("a", arg_types[0]);
        fill_func_arg("p", arg_types[1]);
        auto result = declare(fn_name, return_type, ReturnVar);
        /*
        function modi32i32(a, p) result(d)
            integer(int32), intent(in) :: a, p
            integer(int32) :: q
            q = a/p
            d = a - p*q
        end function
        */

        ASR::expr_t *q = nullptr, *op1 = nullptr, *op2 = nullptr;
        int kind = ASRUtils::extract_kind_from_ttype_t(arg_types[1]);
        if (is_real(*arg_types[1])) {
            if (kind == 4) {
                q = r2i32(r32Div(args[0], args[1]));
                op1 = r32Mul(args[1], i2r32(q));
                op2 = r32Sub(args[0], op1);
            } else {
                q = r2i64(r64Div(args[0], args[1]));
                op1 = r64Mul(args[1], i2r64(q));
                op2 = r64Sub(args[0], op1);
            }
        } else {
            if (kind == 1) {
                q = i8Div(args[0], args[1]);
                op1 = i8Mul(args[1], q);
                op2 = i8Sub(args[0], op1);
            } else if (kind == 2) {
                q = i16Div(args[0], args[1]);
                op1 = i16Mul(args[1], q);
                op2 = i16Sub(args[0], op1);
            } else if (kind == 4) {
                q = iDiv(args[0], args[1]);
                op1 = iMul(args[1], q);
                op2 = iSub(args[0], op1);
            } else if (kind == 8) {
                q = i64Div(args[0], args[1]);
                op1 = i64Mul(args[1], q);
                op2 = i64Sub(args[0], op1);
            } else {
                LCOMPILERS_ASSERT(false);
            }
        }
        body.push_back(al, b.Assignment(result, op2));

        ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);
    }

} // namespace Mod

namespace Trailz {

    static ASR::expr_t *eval_Trailz(Allocator &al, const Location &loc,
            ASR::ttype_t* t1, Vec<ASR::expr_t*> &args) {
        int64_t a = ASR::down_cast<ASR::IntegerConstant_t>(args[0])->m_n;
        int64_t kind = ASRUtils::extract_kind_from_ttype_t(t1);
        int64_t trailing_zeros = ASRUtils::compute_trailing_zeros(a, kind);
        return make_ConstantWithType(make_IntegerConstant_t, trailing_zeros, t1, loc);
    }

    static inline ASR::expr_t* instantiate_Trailz(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        declare_basic_variables("_lcompilers_optimization_trailz_" + type_to_str_python(arg_types[0]));
        fill_func_arg("n", arg_types[0]);
        auto result = declare(fn_name, arg_types[0], ReturnVar);
        // This is not the most efficient way to do this, but it works for now.
        /*
        function trailz(n) result(result)
            integer :: n
            integer :: result
            integer :: k
            k = kind(n)
            result = 0
            if (n == 0) then
                if (k == 4) then
                    result = 32
                else
                    result = 64
                end if
            else
                do while (mod(n,2) == 0)
                    n = n/2
                    result = result + 1
                end do
            end if
        end function
        */

        body.push_back(al, b.Assignment(result, i(0, arg_types[0])));
        ASR::expr_t *two = i(2, arg_types[0]);
        int arg_0_kind = ASRUtils::extract_kind_from_ttype_t(arg_types[0]);

        Vec<ASR::ttype_t*> arg_types_mod; arg_types_mod.reserve(al, 2);
        arg_types_mod.push_back(al, arg_types[0]); arg_types_mod.push_back(al, ASRUtils::expr_type(two));

        Vec<ASR::call_arg_t> new_args_mod; new_args_mod.reserve(al, 2);
        ASR::call_arg_t arg1; arg1.loc = loc; arg1.m_value = args[0];
        ASR::call_arg_t arg2; arg2.loc = loc; arg2.m_value = two;
        new_args_mod.push_back(al, arg1); new_args_mod.push_back(al, arg2);

        ASR::expr_t* func_call_mod = Mod::instantiate_Mod(al, loc, scope, arg_types_mod, return_type, new_args_mod, 0);
        ASR::expr_t *cond = iEq(func_call_mod, i(0, arg_types[0]));

        int64_t base = 32;
        std::vector<ASR::stmt_t*> while_loop_body;
        if (arg_0_kind == 4) {
            while_loop_body.push_back(b.Assignment(args[0], iDiv(args[0], two)));
            while_loop_body.push_back(b.Assignment(result, iAdd(result, i(1, arg_types[0]))));
        } else {
            while_loop_body.push_back(b.Assignment(args[0], i64Div(args[0], two)));
            while_loop_body.push_back(b.Assignment(result, i64Add(result, i(1, arg_types[0]))));
            base = 64;
        }

        ASR::expr_t* check_zero = iEq(args[0], i(0, arg_types[0]));
        std::vector<ASR::stmt_t*> if_body; if_body.push_back(b.Assignment(result, i(base, arg_types[0])));
        std::vector<ASR::stmt_t*> else_body; else_body.push_back(b.While(cond, while_loop_body));
        body.push_back(al, b.If(check_zero, if_body, else_body));

        ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);
    }

} // namespace Trailz

namespace Leadz {

     static inline void verify_args(const ASR::IntrinsicScalarFunction_t& x, diag::Diagnostics& diagnostics) {
        ASRUtils::require_impl(x.n_args == 1,
            "Call to `leadz` must have exactly 1 argument",
            x.base.base.loc, diagnostics);
        ASR::ttype_t *type1 = ASRUtils::expr_type(x.m_args[0]);
        ASRUtils::require_impl(is_integer(*type1),
            "Arguments to `leadz` must be of integer type",
            x.base.base.loc, diagnostics);
    }

    static ASR::expr_t *eval_Leadz(Allocator &al, const Location &loc,
            ASR::ttype_t* t1, Vec<ASR::expr_t*> &args) {
        int64_t a = ASR::down_cast<ASR::IntegerConstant_t>(args[0])->m_n;
        int64_t kind = ASRUtils::extract_kind_from_ttype_t(t1);
        int64_t leading_zeros = ASRUtils::compute_leading_zeros(a, kind);
        return make_ConstantWithType(make_IntegerConstant_t, leading_zeros, t1, loc);
    }

    static inline ASR::asr_t* create_Leadz(Allocator& al, const Location& loc,
            Vec<ASR::expr_t*>& args,
            diag::Diagnostics& diag) {
        if (args.size() != 1) {
            append_error(diag, "Intrinsic `leadz` accepts exactly 1 argument", loc);
            return nullptr;
        }
        ASR::ttype_t *type1 = ASRUtils::expr_type(args[0]);
        if (!(ASRUtils::is_integer(*type1))) {
            append_error(diag, "Argument of the `leadz` must be Integer",
                args[0]->base.loc);
            return nullptr;
        }
        ASR::expr_t *m_value = nullptr;
        if (all_args_evaluated(args)) {
            Vec<ASR::expr_t*> arg_values; arg_values.reserve(al, 1);
            arg_values.push_back(al, expr_value(args[0]));
            m_value = eval_Leadz(al, loc, expr_type(args[0]), arg_values);
        }
        return ASR::make_IntrinsicScalarFunction_t(al, loc,
            static_cast<int64_t>(IntrinsicScalarFunctions::Leadz),
            args.p, args.n, 0, ASRUtils::expr_type(args[0]), m_value);
    }

    static inline ASR::expr_t* instantiate_Leadz(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        declare_basic_variables("_lcompilers_optimization_leadz_" + type_to_str_python(arg_types[0]));
        fill_func_arg("n", arg_types[0]);
        auto result = declare(fn_name, arg_types[0], ReturnVar);
        auto total_bits = declare("r", arg_types[0], Local);
        auto number = declare("num", arg_types[0], Local);
        /*
        function leadz(n) result(result)
            integer :: n, k, total_bits
            integer :: result
            k = kind(n)
            total_bits = 32
            if (k == 8) total_bits = 64
            if (n<0) then
                result = 0
            else
                do while (total_bits > 0)
                    if (mod(n,2) == 0) then
                        result = result + 1
                    else
                        result = 0
                    end if
                    n = n/2
                    total_bits = total_bits - 1
                end do
            end if
        end function
        */
        body.push_back(al, b.Assignment(result, i(0, arg_types[0])));
        body.push_back(al, b.Assignment(number, args[0]));
        int arg_0_kind = ASRUtils::extract_kind_from_ttype_t(arg_types[0]);
        if ( arg_0_kind == 4 ) body.push_back(al, b.Assignment(total_bits, i(32, arg_types[0])));
        else body.push_back(al, b.Assignment(total_bits, i(64, arg_types[0])));

        ASR::expr_t *two = i(2, arg_types[0]);
        Vec<ASR::ttype_t*> arg_types_mod; arg_types_mod.reserve(al, 2);
        arg_types_mod.push_back(al, arg_types[0]); arg_types_mod.push_back(al, ASRUtils::expr_type(two));

        Vec<ASR::call_arg_t> new_args_mod; new_args_mod.reserve(al, 2);
        ASR::call_arg_t arg1; arg1.loc = loc; arg1.m_value = number;
        ASR::call_arg_t arg2; arg2.loc = loc; arg2.m_value = two;
        new_args_mod.push_back(al, arg1); new_args_mod.push_back(al, arg2);

        ASR::expr_t* func_call_mod = Mod::instantiate_Mod(al, loc, scope, arg_types_mod, return_type, new_args_mod, 0);
        ASR::expr_t *if_cond = iLt(number, i(0, arg_types[0]));
        ASR::expr_t *loop_cond = iGt(total_bits, i(0, arg_types[0]));

        std::vector<ASR::stmt_t*> while_loop_body;
        while_loop_body.push_back(b.If(iEq(func_call_mod, i(0, arg_types[0])), {
            b.Assignment(result, i_tAdd(result, i(1, arg_types[0]), arg_types[0]))
        }, {
            b.Assignment(result, i(0, arg_types[0]))
        }));
        while_loop_body.push_back(b.Assignment(number, i_tDiv(number, two, arg_types[0])));
        while_loop_body.push_back(b.Assignment(total_bits, i_tSub(total_bits, i(1, arg_types[0]), arg_types[0])));

        std::vector<ASR::stmt_t*> if_body; if_body.push_back(b.Assignment(result, i(0, arg_types[0])));
        std::vector<ASR::stmt_t*> else_body; else_body.push_back(b.While(loop_cond, while_loop_body));
        body.push_back(al, b.If(if_cond, if_body, else_body));

        ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);
    }

} // namespace Leadz

namespace Hypot {

    static ASR::expr_t *eval_Hypot(Allocator &al, const Location &loc,
            ASR::ttype_t* t1, Vec<ASR::expr_t*> &args) {
        int kind = ASRUtils::extract_kind_from_ttype_t(t1);
        if (kind == 4) {
            float a = ASR::down_cast<ASR::RealConstant_t>(args[0])->m_r;
            float b = ASR::down_cast<ASR::RealConstant_t>(args[1])->m_r;
            return make_ConstantWithType(make_RealConstant_t, std::hypot(a, b), t1, loc);
        } else {
            double a = ASR::down_cast<ASR::RealConstant_t>(args[0])->m_r;
            double b = ASR::down_cast<ASR::RealConstant_t>(args[1])->m_r;
            return make_ConstantWithType(make_RealConstant_t, std::hypot(a, b), t1, loc);
        }
    }

    static inline ASR::expr_t* instantiate_Hypot(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        declare_basic_variables("_lcompilers_optimization_hypot_" + type_to_str_python(arg_types[0]));
        fill_func_arg("x", arg_types[0]);
        fill_func_arg("y", arg_types[1]);
        auto result = declare(fn_name, arg_types[0], ReturnVar);
        /*
            real function hypot_(x,y) result(hypot)
            real :: x,y
            hypot = sqrt(x*x + y*y)
            end function
        */
        ASR::expr_t *op1, *op2, *op3, *func_call_sqrt;
        int kind = ASRUtils::extract_kind_from_ttype_t(arg_types[0]);
        if (kind == 4) {
            op1 = r32Mul(args[0], args[0]); op2 = r32Mul(args[1], args[1]); op3 = r32Add(op1, op2);
        } else {
            op1 = r64Mul(args[0], args[0]); op2 = r64Mul(args[1], args[1]); op3 = r64Add(op1, op2);
        }
        Vec<ASR::ttype_t*> sqrt_arg_types; sqrt_arg_types.reserve(al, 1); sqrt_arg_types.push_back(al, ASRUtils::expr_type(op3));
        Vec<ASR::call_arg_t> sqrt_args; sqrt_args.reserve(al, 1);
        ASR::call_arg_t sqrt_arg; sqrt_arg.loc = loc; sqrt_arg.m_value = op3;
        sqrt_args.push_back(al, sqrt_arg);
        func_call_sqrt = Sqrt::instantiate_Sqrt(al, loc,scope, sqrt_arg_types, return_type, sqrt_args, 0);
        body.push_back(al, b.Assignment(result, func_call_sqrt));

        ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);
    }

} // namespace Hypot

namespace Kind {

    static ASR::expr_t *eval_Kind(Allocator &al, const Location &loc,
            ASR::ttype_t* /*t1*/, Vec<ASR::expr_t*> &args) {
        int result = ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(args[0]));
        return make_ConstantWithType(make_IntegerConstant_t, result, int32, loc);
    }

    static inline ASR::expr_t* instantiate_Kind(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        declare_basic_variables("_lcompilers_optimization_kind_" + type_to_str_python(arg_types[0]));
        fill_func_arg("x", arg_types[0]);
        auto result = declare(fn_name, int32, ReturnVar);
        body.push_back(al, b.Assignment(result, i32(ASRUtils::extract_kind_from_ttype_t(arg_types[0]))));

        ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);
    }

} // namespace Kind

namespace Rank {

     static inline void verify_args(const ASR::IntrinsicScalarFunction_t& x, diag::Diagnostics& diagnostics) {
        ASRUtils::require_impl(x.n_args == 1,
            "Call to `rank` must have exactly 1 argument",
            x.base.base.loc, diagnostics);
    }

    static ASR::expr_t *eval_Rank(Allocator &al, const Location &loc,
            ASR::ttype_t* /*t1*/, Vec<ASR::expr_t*> &args) {
        int result = ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(args[0]));
        return make_ConstantWithType(make_IntegerConstant_t, result, int32, loc);
    }

    static inline ASR::asr_t* create_Rank(Allocator& al, const Location& loc,
            Vec<ASR::expr_t*>& args,
            diag::Diagnostics& diag) {
        if (args.size() != 1) {
            append_error(diag, "Intrinsic `rank` function accepts exactly 1 argument", loc);
            return nullptr;
        }
        ASR::expr_t *m_value = nullptr;
        if (all_args_evaluated(args)) {
            Vec<ASR::expr_t*> arg_values; arg_values.reserve(al, 1);
            arg_values.push_back(al, expr_value(args[0]));
            m_value = eval_Rank(al, loc, expr_type(args[0]), arg_values);
        }
        return ASR::make_IntrinsicScalarFunction_t(al, loc,
            static_cast<int64_t>(IntrinsicScalarFunctions::Rank),
            args.p, args.n, 0, int32, m_value);
    }

    static inline ASR::expr_t* instantiate_Rank(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        declare_basic_variables("_lcompilers_optimization_rank_" + type_to_str_python(arg_types[0]));
        fill_func_arg("x", arg_types[0]);
        auto result = declare(fn_name, int32, ReturnVar);
        body.push_back(al, b.Assignment(result, i32(ASRUtils::extract_n_dims_from_ttype(arg_types[0]))));

        ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);
    }

} // namespace Rank

namespace Digits {

    static ASR::expr_t *eval_Digits(Allocator &al, const Location &loc,
            ASR::ttype_t* /*t1*/, Vec<ASR::expr_t*> &args) {
        ASR::ttype_t *type1 = ASRUtils::expr_type(args[0]);
        int kind = ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(args[0]));
        if (is_integer(*type1)) {
            if (kind == 4) {
                return make_ConstantWithType(make_IntegerConstant_t, 31, int32, loc);
            } else if (kind == 8) {
                return make_ConstantWithType(make_IntegerConstant_t, 63, int32, loc);
            }
        } else if (is_real(*type1)) {
            if (kind == 4) {
                return make_ConstantWithType(make_IntegerConstant_t, 24, int32, loc);
            } else if (kind == 8) {
                return make_ConstantWithType(make_IntegerConstant_t, 53, int32, loc);
            }
        }
        return nullptr;
    }

    static inline ASR::expr_t* instantiate_Digits(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        declare_basic_variables("_lcompilers_optimization_digits_" + type_to_str_python(arg_types[0]));
        fill_func_arg("x", arg_types[0]);
        auto result = declare(fn_name, int32, ReturnVar);
        int kind = ASRUtils::extract_kind_from_ttype_t(arg_types[0]);
        if (is_integer(*arg_types[0])) {
            if (kind == 4) {
                body.push_back(al, b.Assignment(result, i32(31)));
            } else if (kind == 8) {
                body.push_back(al, b.Assignment(result, i32(63)));
            }
        } else if (is_real(*arg_types[0])) {
            if (kind == 4) {
                body.push_back(al, b.Assignment(result, i32(24)));
            } else if (kind == 8) {
                body.push_back(al, b.Assignment(result, i32(53)));
            }
        }
        ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);
    }

} // namespace Digits

namespace Repeat {

    static ASR::expr_t *eval_Repeat(Allocator &al, const Location &loc,
            ASR::ttype_t* t1, Vec<ASR::expr_t*> &args) {
        char* str = ASR::down_cast<ASR::StringConstant_t>(args[0])->m_s;
        int64_t n = ASR::down_cast<ASR::IntegerConstant_t>(args[1])->m_n;
        size_t len = std::strlen(str);
        size_t new_len = len*n;
        char* result = new char[new_len+1];
        for (size_t i=0; i<new_len; i++) {
            result[i] = str[i%len];
        }
        result[new_len] = '\0';
        return make_ConstantWithType(make_StringConstant_t, result, t1, loc);
    }

    static inline ASR::expr_t* instantiate_Repeat(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        declare_basic_variables("_lcompilers_optimization_repeat_" + type_to_str_python(arg_types[0]));
        fill_func_arg("x", ASRUtils::TYPE(ASR::make_Character_t(al, loc, 1, -10, nullptr)));
        fill_func_arg("y", arg_types[1]);
        auto result = declare(fn_name, ASRUtils::TYPE(ASR::make_Character_t(al, loc, 1, -3,
            ASRUtils::EXPR(ASR::make_StringLen_t(al, loc, args[0], ASRUtils::TYPE(
                ASR::make_Integer_t(al, loc, 4)), nullptr)))), ReturnVar);
        auto itr = declare("r", arg_types[1], Local);
        /*
            function repeat_(s, n) result(r)
                character(len=*), intent(in) :: s
                integer, intent(in) :: n
                character(len=n*len(s)) :: r
                integer :: i
                i = n
                do while (i > 0)
                    r = s // r
                    i = i - 1
                end do
            end function
        */

        ASR::expr_t* empty_str =  StringConstant("", ASRUtils::TYPE(
            ASR::make_Character_t(al, loc, 1, 0, nullptr)));
        body.push_back(al, b.Assignment(result, empty_str));
        body.push_back(al, b.Assignment(itr, args[1]));
        int arg_1_kind = ASRUtils::extract_kind_from_ttype_t(arg_types[1]);
        ASR::expr_t *cond = iGt(itr, i(0, arg_types[1]));
        std::vector<ASR::stmt_t*> while_loop_body;
        if (arg_1_kind == 4) {
            while_loop_body.push_back(b.Assignment(itr, iSub(itr, i(1, arg_types[1]))));
            while_loop_body.push_back(b.Assignment(result, b.Add(result, args[0])));
        } else {
            while_loop_body.push_back(b.Assignment(itr, i64Sub(itr, i(1, arg_types[1]))));
            while_loop_body.push_back(b.Assignment(result, b.Add(result, args[0])));
        }
        body.push_back(al, b.While(cond, while_loop_body));

        ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);
    }

} // namespace Repeat

namespace MinExponent {

    static ASR::expr_t *eval_MinExponent(Allocator &al, const Location &loc,
            ASR::ttype_t* /*t1*/, Vec<ASR::expr_t*> &args) {
        ASR::RealConstant_t* a = ASR::down_cast<ASR::RealConstant_t>(args[0]);
        int m_kind = ASRUtils::extract_kind_from_ttype_t(a->m_type);
        int result;
        if (m_kind == 4) {
            result = std::numeric_limits<float>::min_exponent;
        } else {
            result = std::numeric_limits<double>::min_exponent;
        }
        return make_ConstantWithType(make_IntegerConstant_t, result, int32, loc);

    }

    static inline ASR::expr_t* instantiate_MinExponent(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        declare_basic_variables("_lcompilers_optimization_minexponent_" + type_to_str_python(arg_types[0]));
        fill_func_arg("x", arg_types[0]);
        auto result = declare(fn_name, int32, ReturnVar);

        int m_kind = ASRUtils::extract_kind_from_ttype_t(arg_types[0]);
        if (m_kind == 4) {
            body.push_back(al, b.Assignment(result, i32(-125)));
        } else {
            body.push_back(al, b.Assignment(result, i32(-1021)));
        }

        ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);
    }

} // namespace MinExponent

namespace MaxExponent {

    static ASR::expr_t *eval_MaxExponent(Allocator &al, const Location &loc,
            ASR::ttype_t* /*t1*/, Vec<ASR::expr_t*> &args) {
        ASR::RealConstant_t* a = ASR::down_cast<ASR::RealConstant_t>(args[0]);
        int m_kind = ASRUtils::extract_kind_from_ttype_t(a->m_type);
        int result;
        if (m_kind == 4) {
            result = std::numeric_limits<float>::max_exponent;
        } else {
            result = std::numeric_limits<double>::max_exponent;
        }
        return make_ConstantWithType(make_IntegerConstant_t, result, int32, loc);

    }

    static inline ASR::expr_t* instantiate_MaxExponent(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        declare_basic_variables("_lcompilers_optimization_maxexponent_" + type_to_str_python(arg_types[0]));
        fill_func_arg("x", arg_types[0]);
        auto result = declare(fn_name, int32, ReturnVar);

        int m_kind = ASRUtils::extract_kind_from_ttype_t(arg_types[0]);
        if (m_kind == 4) {
            body.push_back(al, b.Assignment(result, i32(128)));
        } else {
            body.push_back(al, b.Assignment(result, i32(1024)));
        }

        ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);
    }

} // namespace MaxExponent

#define create_exp_macro(X, stdeval)                                                      \
namespace X {                                                                             \
    static inline ASR::expr_t* eval_##X(Allocator &al, const Location &loc,               \
            ASR::ttype_t *t, Vec<ASR::expr_t*> &args) {                                   \
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));                            \
        double rv = -1;                                                                    \
        if( ASRUtils::extract_value(args[0], rv) ) {                                      \
            double val = std::stdeval(rv);                                                \
            return ASRUtils::EXPR(ASR::make_RealConstant_t(al, loc, val, t));             \
        }                                                                                 \
        return nullptr;                                                                   \
    }                                                                                     \
    static inline ASR::asr_t* create_##X(Allocator& al, const Location& loc,              \
            Vec<ASR::expr_t*>& args,                                                      \
            diag::Diagnostics& diag) {                                                    \
        if (args.size() != 1) {                                                           \
            append_error(diag, "Intrinsic function `"#X"` accepts exactly 1 argument",    \
                loc);                                                                     \
            return nullptr;                                                               \
        }                                                                                 \
        ASR::ttype_t *type = ASRUtils::expr_type(args[0]);                                \
        if (!ASRUtils::is_real(*type)) {                                                  \
            append_error(diag, "Argument of the `"#X"` function must be either Real",     \
                args[0]->base.loc);                                                       \
            return nullptr;                                                               \
        }                                                                                 \
        return UnaryIntrinsicFunction::create_UnaryFunction(al, loc, args, eval_##X,      \
            static_cast<int64_t>(IntrinsicScalarFunctions::X), 0, type);                  \
    }                                                                                     \
} // namespace X

create_exp_macro(Exp2, exp2)
create_exp_macro(Expm1, expm1)

namespace Exp {

    static inline ASR::expr_t* eval_Exp(Allocator &al, const Location &loc,
            ASR::ttype_t *t, Vec<ASR::expr_t*> &args) {
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));
        double rv = -1;
        if( ASRUtils::extract_value(args[0], rv) ) {
            double val = std::exp(rv);
            return ASRUtils::EXPR(ASR::make_RealConstant_t(al, loc, val, t));
        } else {
            std::complex<double> crv;
            if( ASRUtils::extract_value(args[0], crv) ) {
                std::complex<double> val = std::exp(crv);
                return ASRUtils::EXPR(ASR::make_ComplexConstant_t(
                    al, loc, val.real(), val.imag(), t));
            }
        }
        return nullptr;
    }

    static inline ASR::asr_t* create_Exp(Allocator& al, const Location& loc,
        Vec<ASR::expr_t*>& args,
        diag::Diagnostics& diag) {
        if (args.size() != 1) {
            append_error(diag, "Intrinsic function `exp` accepts exactly 1 argument", loc);
            return nullptr;
        }
        ASR::ttype_t *type = ASRUtils::expr_type(args[0]);
        if (!ASRUtils::is_real(*type) && !is_complex(*type)) {
            append_error(diag, "Argument of the `exp` function must be either Real or Complex",
                args[0]->base.loc);
            return nullptr;
        }
        return UnaryIntrinsicFunction::create_UnaryFunction(al, loc, args,
            eval_Exp, static_cast<int64_t>(IntrinsicScalarFunctions::Exp),
            0, type);
    }

    static inline ASR::expr_t* instantiate_Exp(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t overload_id) {
        if (is_real(*arg_types[0])) {
            Vec<ASR::expr_t *> args; args.reserve(al, 1);
            args.push_back(al, new_args[0].m_value);
            return EXPR(ASR::make_IntrinsicScalarFunction_t(al, loc,
                static_cast<int64_t>(IntrinsicScalarFunctions::Exp),
                args.p, 1, overload_id, return_type, nullptr));
        } else {
            return UnaryIntrinsicFunction::instantiate_functions(al, loc, scope,
                "exp", arg_types[0], return_type, new_args, overload_id);
        }
    }

} // namespace Exp

namespace ListIndex {

static inline void verify_args(const ASR::IntrinsicScalarFunction_t& x, diag::Diagnostics& diagnostics) {
    ASRUtils::require_impl(x.n_args <= 4, "Call to list.index must have at most four arguments",
        x.base.base.loc, diagnostics);
    ASRUtils::require_impl(ASR::is_a<ASR::List_t>(*ASRUtils::expr_type(x.m_args[0])) &&
        ASRUtils::check_equal_type(ASRUtils::expr_type(x.m_args[1]),
            ASRUtils::get_contained_type(ASRUtils::expr_type(x.m_args[0]))),
        "First argument to list.index must be of list type and "
        "second argument must be of same type as list elemental type",
        x.base.base.loc, diagnostics);
    if(x.n_args >= 3) {
        ASRUtils::require_impl(
            ASR::is_a<ASR::Integer_t>(*ASRUtils::expr_type(x.m_args[2])),
            "Third argument to list.index must be an integer",
            x.base.base.loc, diagnostics);
    }
    if(x.n_args == 4) {
        ASRUtils::require_impl(
            ASR::is_a<ASR::Integer_t>(*ASRUtils::expr_type(x.m_args[3])),
            "Fourth argument to list.index must be an integer",
            x.base.base.loc, diagnostics);
    }
    ASRUtils::require_impl(ASR::is_a<ASR::Integer_t>(*x.m_type),
        "Return type of list.index must be an integer",
        x.base.base.loc, diagnostics);
}

static inline ASR::expr_t *eval_list_index(Allocator &/*al*/,
    const Location &/*loc*/, ASR::ttype_t */*t*/, Vec<ASR::expr_t*>& /*args*/) {
    // TODO: To be implemented for ListConstant expression
    return nullptr;
}


static inline ASR::asr_t* create_ListIndex(Allocator& al, const Location& loc,
    Vec<ASR::expr_t*>& args,
    diag::Diagnostics& diag) {
    int64_t overload_id = 0;
    ASR::expr_t* list_expr = args[0];
    ASR::ttype_t *type = ASRUtils::expr_type(list_expr);
    ASR::ttype_t *list_type = ASR::down_cast<ASR::List_t>(type)->m_type;
    ASR::ttype_t *ele_type = ASRUtils::expr_type(args[1]);
    if (!ASRUtils::check_equal_type(ele_type, list_type)) {
        std::string fnd = ASRUtils::get_type_code(ele_type);
        std::string org = ASRUtils::get_type_code(list_type);
        append_error(diag,
            "Type mismatch in 'index', the types must be compatible "
            "(found: '" + fnd + "', expected: '" + org + "')", loc);
        return nullptr;
    }
    if (args.size() >= 3) {
        overload_id = 1;
        if(!ASR::is_a<ASR::Integer_t>(*ASRUtils::expr_type(args[2]))) {
            append_error(diag, "Third argument to list.index must be an integer", loc);
            return nullptr;
        }
    }
    if (args.size() == 4) {
        overload_id = 2;
        if(!ASR::is_a<ASR::Integer_t>(*ASRUtils::expr_type(args[3]))) {
            append_error(diag, "Fourth argument to list.index must be an integer", loc);
            return nullptr;
        }
    }
    Vec<ASR::expr_t*> arg_values;
    arg_values.reserve(al, args.size());
    for( size_t i = 0; i < args.size(); i++ ) {
        arg_values.push_back(al, ASRUtils::expr_value(args[i]));
    }
    ASR::ttype_t *to_type = int32;
    ASR::expr_t* compile_time_value = eval_list_index(al, loc, to_type, arg_values);
    return ASR::make_IntrinsicScalarFunction_t(al, loc,
            static_cast<int64_t>(IntrinsicScalarFunctions::ListIndex),
            args.p, args.size(), overload_id, to_type, compile_time_value);
}

} // namespace ListIndex

namespace ListReverse {

static inline ASR::expr_t *eval_ListReverse(Allocator &/*al*/,
    const Location &/*loc*/, ASR::ttype_t */*t*/, Vec<ASR::expr_t*>& /*args*/) {
    // TODO: To be implemented for ListConstant expression
    return nullptr;
}

} // namespace ListReverse

namespace ListPop {

static inline void verify_args(const ASR::IntrinsicScalarFunction_t& x, diag::Diagnostics& diagnostics) {
    ASRUtils::require_impl(x.n_args <= 2, "Call to list.pop must have at most one argument",
        x.base.base.loc, diagnostics);
    ASRUtils::require_impl(ASR::is_a<ASR::List_t>(*ASRUtils::expr_type(x.m_args[0])),
        "Argument to list.pop must be of list type",
        x.base.base.loc, diagnostics);
    switch(x.m_overload_id) {
        case 0:
            break;
        case 1:
            ASRUtils::require_impl(ASR::is_a<ASR::Integer_t>(*ASRUtils::expr_type(x.m_args[1])),
            "Argument to list.pop must be an integer",
            x.base.base.loc, diagnostics);
            break;
    }
    ASRUtils::require_impl(ASRUtils::check_equal_type(x.m_type,
            ASRUtils::get_contained_type(ASRUtils::expr_type(x.m_args[0]))),
        "Return type of list.pop must be of same type as list's element type",
        x.base.base.loc, diagnostics);
}

static inline ASR::expr_t *eval_list_pop(Allocator &/*al*/,
    const Location &/*loc*/, ASR::ttype_t */*t*/, Vec<ASR::expr_t*>& /*args*/) {
    // TODO: To be implemented for ListConstant expression
    return nullptr;
}

static inline ASR::asr_t* create_ListPop(Allocator& al, const Location& loc,
    Vec<ASR::expr_t*>& args,
    diag::Diagnostics& diag) {
    if (args.size() > 2) {
        append_error(diag, "Call to list.pop must have at most one argument", loc);
        return nullptr;
    }
    if (args.size() == 2 &&
        !ASR::is_a<ASR::Integer_t>(*ASRUtils::expr_type(args[1]))) {
        append_error(diag, "Argument to list.pop must be an integer", loc);
        return nullptr;
    }

    ASR::expr_t* list_expr = args[0];
    ASR::ttype_t *type = ASRUtils::expr_type(list_expr);
    ASR::ttype_t *list_type = ASR::down_cast<ASR::List_t>(type)->m_type;

    Vec<ASR::expr_t*> arg_values;
    arg_values.reserve(al, args.size());
    for( size_t i = 0; i < args.size(); i++ ) {
        arg_values.push_back(al, ASRUtils::expr_value(args[i]));
    }
    ASR::ttype_t *to_type = list_type;
    ASR::expr_t* compile_time_value = eval_list_pop(al, loc, to_type, arg_values);
    int64_t overload_id = (args.size() == 2);
    return ASR::make_IntrinsicScalarFunction_t(al, loc,
            static_cast<int64_t>(IntrinsicScalarFunctions::ListPop),
            args.p, args.size(), overload_id, to_type, compile_time_value);
}

} // namespace ListPop

namespace Reserve {

static inline ASR::expr_t *eval_Reserve(Allocator &/*al*/,
    const Location &/*loc*/, ASR::ttype_t *, Vec<ASR::expr_t*>& /*args*/) {
    // TODO: To be implemented for ListConstant expression
    return nullptr;
}

} // namespace Reserve

namespace DictKeys {

static inline void verify_args(const ASR::IntrinsicScalarFunction_t& x, diag::Diagnostics& diagnostics) {
    ASRUtils::require_impl(x.n_args == 1, "Call to dict.keys must have no argument",
        x.base.base.loc, diagnostics);
    ASRUtils::require_impl(ASR::is_a<ASR::Dict_t>(*ASRUtils::expr_type(x.m_args[0])),
        "Argument to dict.keys must be of dict type",
        x.base.base.loc, diagnostics);
    ASRUtils::require_impl(ASR::is_a<ASR::List_t>(*x.m_type) &&
        ASRUtils::check_equal_type(ASRUtils::get_contained_type(x.m_type),
        ASRUtils::get_contained_type(ASRUtils::expr_type(x.m_args[0]), 0)),
        "Return type of dict.keys must be of list of dict key element type",
        x.base.base.loc, diagnostics);
}

static inline ASR::expr_t *eval_dict_keys(Allocator &/*al*/,
    const Location &/*loc*/, ASR::ttype_t *, Vec<ASR::expr_t*>& /*args*/) {
    // TODO: To be implemented for DictConstant expression
    return nullptr;
}

static inline ASR::asr_t* create_DictKeys(Allocator& al, const Location& loc,
    Vec<ASR::expr_t*>& args,
    diag::Diagnostics& diag) {
    if (args.size() != 1) {
        append_error(diag, "Call to dict.keys must have no argument", loc);
        return nullptr;
    }

    ASR::expr_t* dict_expr = args[0];
    ASR::ttype_t *type = ASRUtils::expr_type(dict_expr);
    ASR::ttype_t *dict_keys_type = ASR::down_cast<ASR::Dict_t>(type)->m_key_type;

    Vec<ASR::expr_t*> arg_values;
    arg_values.reserve(al, args.size());
    for( size_t i = 0; i < args.size(); i++ ) {
        arg_values.push_back(al, ASRUtils::expr_value(args[i]));
    }
    ASR::ttype_t *to_type = List(dict_keys_type);
    ASR::expr_t* compile_time_value = eval_dict_keys(al, loc, to_type, arg_values);
    return ASR::make_IntrinsicScalarFunction_t(al, loc,
            static_cast<int64_t>(IntrinsicScalarFunctions::DictKeys),
            args.p, args.size(), 0, to_type, compile_time_value);
}

} // namespace DictKeys

namespace DictValues {

static inline void verify_args(const ASR::IntrinsicScalarFunction_t& x, diag::Diagnostics& diagnostics) {
    ASRUtils::require_impl(x.n_args == 1, "Call to dict.values must have no argument",
        x.base.base.loc, diagnostics);
    ASRUtils::require_impl(ASR::is_a<ASR::Dict_t>(*ASRUtils::expr_type(x.m_args[0])),
        "Argument to dict.values must be of dict type",
        x.base.base.loc, diagnostics);
    ASRUtils::require_impl(ASR::is_a<ASR::List_t>(*x.m_type) &&
        ASRUtils::check_equal_type(ASRUtils::get_contained_type(x.m_type),
        ASRUtils::get_contained_type(ASRUtils::expr_type(x.m_args[0]), 1)),
        "Return type of dict.values must be of list of dict value element type",
        x.base.base.loc, diagnostics);
}

static inline ASR::expr_t *eval_dict_values(Allocator &/*al*/,
    const Location &/*loc*/, ASR::ttype_t *, Vec<ASR::expr_t*>& /*args*/) {
    // TODO: To be implemented for DictConstant expression
    return nullptr;
}

static inline ASR::asr_t* create_DictValues(Allocator& al, const Location& loc,
    Vec<ASR::expr_t*>& args,
    diag::Diagnostics& diag) {
    if (args.size() != 1) {
        append_error(diag, "Call to dict.values must have no argument", loc);
        return nullptr;
    }

    ASR::expr_t* dict_expr = args[0];
    ASR::ttype_t *type = ASRUtils::expr_type(dict_expr);
    ASR::ttype_t *dict_values_type = ASR::down_cast<ASR::Dict_t>(type)->m_value_type;

    Vec<ASR::expr_t*> arg_values;
    arg_values.reserve(al, args.size());
    for( size_t i = 0; i < args.size(); i++ ) {
        arg_values.push_back(al, ASRUtils::expr_value(args[i]));
    }
    ASR::ttype_t *to_type = List(dict_values_type);
    ASR::expr_t* compile_time_value = eval_dict_values(al, loc, to_type, arg_values);
    return ASR::make_IntrinsicScalarFunction_t(al, loc,
            static_cast<int64_t>(IntrinsicScalarFunctions::DictValues),
            args.p, args.size(), 0, to_type, compile_time_value);
}

} // namespace DictValues

namespace SetAdd {

static inline void verify_args(const ASR::IntrinsicScalarFunction_t& x, diag::Diagnostics& diagnostics) {
    ASRUtils::require_impl(x.n_args == 2, "Call to set.add must have exactly one argument",
        x.base.base.loc, diagnostics);
    ASRUtils::require_impl(ASR::is_a<ASR::Set_t>(*ASRUtils::expr_type(x.m_args[0])),
        "First argument to set.add must be of set type",
        x.base.base.loc, diagnostics);
    ASRUtils::require_impl(ASRUtils::check_equal_type(ASRUtils::expr_type(x.m_args[1]),
            ASRUtils::get_contained_type(ASRUtils::expr_type(x.m_args[0]))),
        "Second argument to set.add must be of same type as set's element type",
        x.base.base.loc, diagnostics);
    ASRUtils::require_impl(x.m_type == nullptr,
        "Return type of set.add must be empty",
        x.base.base.loc, diagnostics);
}

static inline ASR::expr_t *eval_set_add(Allocator &/*al*/,
    const Location &/*loc*/, ASR::ttype_t *, Vec<ASR::expr_t*>& /*args*/) {
    // TODO: To be implemented for SetConstant expression
    return nullptr;
}

static inline ASR::asr_t* create_SetAdd(Allocator& al, const Location& loc,
    Vec<ASR::expr_t*>& args,
    diag::Diagnostics& diag) {
    if (args.size() != 2) {
        append_error(diag, "Call to set.add must have exactly one argument", loc);
        return nullptr;
    }
    if (!ASRUtils::check_equal_type(ASRUtils::expr_type(args[1]),
        ASRUtils::get_contained_type(ASRUtils::expr_type(args[0])))) {
        append_error(diag, "Argument to set.add must be of same type as set's "
            "element type", loc);
        return nullptr;
    }

    Vec<ASR::expr_t*> arg_values;
    arg_values.reserve(al, args.size());
    for( size_t i = 0; i < args.size(); i++ ) {
        arg_values.push_back(al, ASRUtils::expr_value(args[i]));
    }
    ASR::expr_t* compile_time_value = eval_set_add(al, loc, nullptr, arg_values);
    return ASR::make_Expr_t(al, loc,
            ASRUtils::EXPR(ASR::make_IntrinsicScalarFunction_t(al, loc,
            static_cast<int64_t>(IntrinsicScalarFunctions::SetAdd),
            args.p, args.size(), 0, nullptr, compile_time_value)));
}

} // namespace SetAdd

namespace SetRemove {

static inline void verify_args(const ASR::IntrinsicScalarFunction_t& x, diag::Diagnostics& diagnostics) {
    ASRUtils::require_impl(x.n_args == 2, "Call to set.remove must have exactly one argument",
        x.base.base.loc, diagnostics);
    ASRUtils::require_impl(ASR::is_a<ASR::Set_t>(*ASRUtils::expr_type(x.m_args[0])),
        "First argument to set.remove must be of set type",
        x.base.base.loc, diagnostics);
    ASRUtils::require_impl(ASRUtils::check_equal_type(ASRUtils::expr_type(x.m_args[1]),
            ASRUtils::get_contained_type(ASRUtils::expr_type(x.m_args[0]))),
        "Second argument to set.remove must be of same type as set's element type",
        x.base.base.loc, diagnostics);
    ASRUtils::require_impl(x.m_type == nullptr,
        "Return type of set.remove must be empty",
        x.base.base.loc, diagnostics);
}

static inline ASR::expr_t *eval_set_remove(Allocator &/*al*/,
    const Location &/*loc*/, ASR::ttype_t *, Vec<ASR::expr_t*>& /*args*/) {
    // TODO: To be implemented for SetConstant expression
    return nullptr;
}

static inline ASR::asr_t* create_SetRemove(Allocator& al, const Location& loc,
    Vec<ASR::expr_t*>& args,
    diag::Diagnostics& diag) {
    if (args.size() != 2) {
        append_error(diag, "Call to set.remove must have exactly one argument", loc);
        return nullptr;
    }
    if (!ASRUtils::check_equal_type(ASRUtils::expr_type(args[1]),
        ASRUtils::get_contained_type(ASRUtils::expr_type(args[0])))) {
        append_error(diag, "Argument to set.remove must be of same type as set's "
            "element type", loc);
        return nullptr;
    }

    Vec<ASR::expr_t*> arg_values;
    arg_values.reserve(al, args.size());
    for( size_t i = 0; i < args.size(); i++ ) {
        arg_values.push_back(al, ASRUtils::expr_value(args[i]));
    }
    ASR::expr_t* compile_time_value = eval_set_remove(al, loc, nullptr, arg_values);
    return ASR::make_Expr_t(al, loc,
            ASRUtils::EXPR(ASR::make_IntrinsicScalarFunction_t(al, loc,
            static_cast<int64_t>(IntrinsicScalarFunctions::SetRemove),
            args.p, args.size(), 0, nullptr, compile_time_value)));
}

} // namespace SetRemove

namespace Max {

    static inline void verify_args(const ASR::IntrinsicScalarFunction_t& x, diag::Diagnostics& diagnostics) {
        ASRUtils::require_impl(x.n_args > 1, "Call to max0 must have at least two arguments",
            x.base.base.loc, diagnostics);
        ASRUtils::require_impl(ASR::is_a<ASR::Real_t>(*ASRUtils::expr_type(x.m_args[0])) ||
            ASR::is_a<ASR::Integer_t>(*ASRUtils::expr_type(x.m_args[0])) ||
            ASR::is_a<ASR::Character_t>(*ASRUtils::expr_type(x.m_args[0])),
             "Arguments to max0 must be of real, integer or character type",
            x.base.base.loc, diagnostics);
        for(size_t i=0;i<x.n_args;i++){
            ASRUtils::require_impl((ASR::is_a<ASR::Real_t>(*ASRUtils::expr_type(x.m_args[i])) &&
                                            ASR::is_a<ASR::Real_t>(*ASRUtils::expr_type(x.m_args[0]))) ||
                                        (ASR::is_a<ASR::Integer_t>(*ASRUtils::expr_type(x.m_args[i])) &&
                                         ASR::is_a<ASR::Integer_t>(*ASRUtils::expr_type(x.m_args[0]))) ||
                                         (ASR::is_a<ASR::Character_t>(*ASRUtils::expr_type(x.m_args[i])) &&
                                         ASR::is_a<ASR::Character_t>(*ASRUtils::expr_type(x.m_args[0]))),
            "All arguments must be of the same type",
            x.base.base.loc, diagnostics);
        }
    }

    static ASR::expr_t *eval_Max(Allocator &al, const Location &loc,
            ASR::ttype_t* arg_type, Vec<ASR::expr_t*> &args) {
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));
        if (ASR::is_a<ASR::Real_t>(*arg_type)) {
            double max_val = ASR::down_cast<ASR::RealConstant_t>(args[0])->m_r;
            for (size_t i = 1; i < args.size(); i++) {
                double val = ASR::down_cast<ASR::RealConstant_t>(args[i])->m_r;
                max_val = std::fmax(max_val, val);
            }
            return ASR::down_cast<ASR::expr_t>(ASR::make_RealConstant_t(al, loc, max_val, arg_type));
        } else if (ASR::is_a<ASR::Integer_t>(*arg_type)) {
            int64_t max_val = ASR::down_cast<ASR::IntegerConstant_t>(args[0])->m_n;
            for (size_t i = 1; i < args.size(); i++) {
                int64_t val = ASR::down_cast<ASR::IntegerConstant_t>(args[i])->m_n;
                max_val = std::fmax(max_val, val);
            }
            return ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(al, loc, max_val, arg_type));
        } else if (ASR::is_a<ASR::Character_t>(*arg_type)) {
            char* max_val = ASR::down_cast<ASR::StringConstant_t>(args[0])->m_s;
            for (size_t i = 1; i < args.size(); i++) {
                char* val = ASR::down_cast<ASR::StringConstant_t>(args[i])->m_s;
                if (strcmp(val, max_val) > 0) {
                    max_val = val;
                }
            }
            return ASR::down_cast<ASR::expr_t>(ASR::make_StringConstant_t(al, loc, max_val, arg_type));
        } else {
            return nullptr;
        }
    }

    static inline ASR::asr_t* create_Max(
        Allocator& al, const Location& loc, Vec<ASR::expr_t*>& args,
        diag::Diagnostics& diag) {
        bool is_compile_time = true;
        for(size_t i=0; i<100;i++){
            args.erase(nullptr);
        }
        if (args.size() < 2) {
            append_error(diag, "Intrinsic max0 must have 2 arguments", loc);
            return nullptr;
        }
        ASR::ttype_t *arg_type = ASRUtils::expr_type(args[0]);
        for(size_t i=0;i<args.size();i++){
            if (!ASRUtils::check_equal_type(arg_type, ASRUtils::expr_type(args[i]))) {
                append_error(diag, "All arguments to max0 must be of the same type and kind", loc);
            return nullptr;
            }
        }
        Vec<ASR::expr_t*> arg_values;
        arg_values.reserve(al, args.size());
        ASR::expr_t *arg_value;
        for(size_t i=0;i<args.size();i++){
            arg_value = ASRUtils::expr_value(args[i]);
            if (!arg_value) {
                is_compile_time = false;
            }
            arg_values.push_back(al, arg_value);
        }
        if (is_compile_time) {
            ASR::expr_t *value = eval_Max(al, loc, expr_type(args[0]), arg_values);
            return ASR::make_IntrinsicScalarFunction_t(al, loc,
                static_cast<int64_t>(IntrinsicScalarFunctions::Max),
                args.p, args.n, 0, ASRUtils::expr_type(args[0]), value);
        } else {
            return ASR::make_IntrinsicScalarFunction_t(al, loc,
                static_cast<int64_t>(IntrinsicScalarFunctions::Max),
                args.p, args.n, 0, ASRUtils::expr_type(args[0]), nullptr);
        }
    }

    static inline ASR::expr_t* instantiate_Max(Allocator &al, const Location &loc,
        SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
        Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        std::string func_name = "_lcompilers_max0_" + type_to_str_python(arg_types[0])
            + "_" + std::to_string(new_args.n);
        std::string fn_name = scope->get_unique_name(func_name);
        SymbolTable *fn_symtab = al.make_new<SymbolTable>(scope);
        Vec<ASR::expr_t*> args;
        args.reserve(al, new_args.size());
        ASRBuilder b(al, loc);
        Vec<ASR::stmt_t*> body; body.reserve(al, args.size());
        SetChar dep; dep.reserve(al, 1);
        if (scope->get_symbol(func_name)) {
            ASR::symbol_t *s = scope->get_symbol(func_name);
            ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(s);
            return b.Call(s, new_args, expr_type(f->m_return_var), nullptr);
        }
        for (size_t i = 0; i < new_args.size(); i++) {
            fill_func_arg("x" + std::to_string(i), arg_types[0]);
        }

        auto result = declare(fn_name, return_type, ReturnVar);

        ASR::expr_t* test;
        body.push_back(al, b.Assignment(result, args[0]));
        if (ASR::is_a<ASR::Integer_t>(*return_type)) {
            for (size_t i = 1; i < args.size(); i++) {
                test = make_Compare(make_IntegerCompare_t, args[i], Gt, result);
                Vec<ASR::stmt_t *> if_body; if_body.reserve(al, 1);
                if_body.push_back(al, b.Assignment(result, args[i]));
                body.push_back(al, STMT(ASR::make_If_t(al, loc, test,
                    if_body.p, if_body.n, nullptr, 0)));
            }
        } else if (ASR::is_a<ASR::Real_t>(*return_type)) {
            for (size_t i = 1; i < args.size(); i++) {
                test = make_Compare(make_RealCompare_t, args[i], Gt, result);
                Vec<ASR::stmt_t *> if_body; if_body.reserve(al, 1);
                if_body.push_back(al, b.Assignment(result, args[i]));
                body.push_back(al, STMT(ASR::make_If_t(al, loc, test,
                    if_body.p, if_body.n, nullptr, 0)));
            }
        } else if (ASR::is_a<ASR::Character_t>(*return_type)) {
            for (size_t i = 1; i < args.size(); i++) {
                test = make_Compare(make_StringCompare_t, args[i], Gt, result);
                Vec<ASR::stmt_t *> if_body; if_body.reserve(al, 1);
                if_body.push_back(al, b.Assignment(result, args[i]));
                body.push_back(al, STMT(ASR::make_If_t(al, loc, test,
                    if_body.p, if_body.n, nullptr, 0)));
            }
        } else {
            throw LCompilersException("Arguments to max0 must be of real, integer or character type");
        }
        ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);
    }

}  // namespace Max

namespace Min {

    static inline void verify_args(const ASR::IntrinsicScalarFunction_t& x, diag::Diagnostics& diagnostics) {
        ASRUtils::require_impl(x.n_args > 1, "Call to min0 must have at least two arguments",
            x.base.base.loc, diagnostics);
        ASR::ttype_t* arg0_type = ASRUtils::type_get_past_array(ASRUtils::expr_type(x.m_args[0]));
        ASRUtils::require_impl(ASR::is_a<ASR::Real_t>(*arg0_type) ||
            ASR::is_a<ASR::Integer_t>(*arg0_type) || ASR::is_a<ASR::Character_t>(*arg0_type),
             "Arguments to min0 must be of real, integer or character type",
            x.base.base.loc, diagnostics);
        for(size_t i=0;i<x.n_args;i++){
            ASR::ttype_t* arg_type = ASRUtils::type_get_past_array(ASRUtils::expr_type(x.m_args[i]));
            ASRUtils::require_impl((ASR::is_a<ASR::Real_t>(*arg_type) && ASR::is_a<ASR::Real_t>(*arg0_type)) ||
                                    (ASR::is_a<ASR::Integer_t>(*arg_type) && ASR::is_a<ASR::Integer_t>(*arg0_type)) ||
                                    (ASR::is_a<ASR::Character_t>(*arg_type) && ASR::is_a<ASR::Character_t>(*arg0_type) ),
            "All arguments must be of the same type",
            x.base.base.loc, diagnostics);
        }
    }

    static ASR::expr_t *eval_Min(Allocator &al, const Location &loc,
            ASR::ttype_t *arg_type, Vec<ASR::expr_t*> &args) {
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));
        if (ASR::is_a<ASR::Real_t>(*arg_type)) {
            double min_val = ASR::down_cast<ASR::RealConstant_t>(args[0])->m_r;
            for (size_t i = 1; i < args.size(); i++) {
                double val = ASR::down_cast<ASR::RealConstant_t>(args[i])->m_r;
                min_val = std::fmin(min_val, val);
            }
            return ASR::down_cast<ASR::expr_t>(ASR::make_RealConstant_t(al, loc, min_val, arg_type));
        } else if (ASR::is_a<ASR::Integer_t>(*arg_type)) {
            int64_t min_val = ASR::down_cast<ASR::IntegerConstant_t>(args[0])->m_n;
            for (size_t i = 1; i < args.size(); i++) {
                int64_t val = ASR::down_cast<ASR::IntegerConstant_t>(args[i])->m_n;
                min_val = std::fmin(min_val, val);
            }
            return ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(al, loc, min_val, arg_type));
        } else if (ASR::is_a<ASR::Character_t>(*arg_type)) {
            char* min_val = ASR::down_cast<ASR::StringConstant_t>(args[0])->m_s;
            for (size_t i = 1; i < args.size(); i++) {
                char* val = ASR::down_cast<ASR::StringConstant_t>(args[i])->m_s;
                if (strcmp(val, min_val) < 0) {
                    min_val = val;
                }
            }
            return ASR::down_cast<ASR::expr_t>(ASR::make_StringConstant_t(al, loc, min_val, arg_type));
        } else {
            return nullptr;
        }
    }

    static inline ASR::asr_t* create_Min(
        Allocator& al, const Location& loc, Vec<ASR::expr_t*>& args,
        diag::Diagnostics& diag) {
        bool is_compile_time = true;
        for(size_t i=0; i<100;i++){
            args.erase(nullptr);
        }
        if (args.size() < 2) {
            append_error(diag, "Intrinsic min0 must have 2 arguments", loc);
            return nullptr;
        }
        ASR::ttype_t *arg_type = ASRUtils::expr_type(args[0]);
        for(size_t i=0;i<args.size();i++){
            if (!ASRUtils::check_equal_type(arg_type, ASRUtils::expr_type(args[i]))) {
                append_error(diag, "All arguments to min0 must be of the same type and kind", loc);
                return nullptr;
            }
        }
        Vec<ASR::expr_t*> arg_values;
        arg_values.reserve(al, args.size());
        ASR::expr_t *arg_value;
        for(size_t i=0;i<args.size();i++){
            arg_value = ASRUtils::expr_value(args[i]);
            if (!arg_value) {
                is_compile_time = false;
            }
            arg_values.push_back(al, arg_value);
        }
        if (is_compile_time) {
            ASR::expr_t *value = eval_Min(al, loc, expr_type(args[0]), arg_values);
            return ASR::make_IntrinsicScalarFunction_t(al, loc,
                static_cast<int64_t>(IntrinsicScalarFunctions::Min),
                args.p, args.n, 0, ASRUtils::expr_type(args[0]), value);
        } else {
            return ASR::make_IntrinsicScalarFunction_t(al, loc,
                static_cast<int64_t>(IntrinsicScalarFunctions::Min),
                args.p, args.n, 0, ASRUtils::expr_type(args[0]), nullptr);
        }
    }

    static inline ASR::expr_t* instantiate_Min(Allocator &al, const Location &loc,
        SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types, ASR::ttype_t *return_type,
        Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        std::string func_name = "_lcompilers_min0_" + type_to_str_python(arg_types[0]);
        std::string fn_name = scope->get_unique_name(func_name);
        SymbolTable *fn_symtab = al.make_new<SymbolTable>(scope);
        Vec<ASR::expr_t*> args;
        args.reserve(al, new_args.size());
        ASRBuilder b(al, loc);
        Vec<ASR::stmt_t*> body; body.reserve(al, args.size());
        SetChar dep; dep.reserve(al, 1);
        if (scope->get_symbol(fn_name)) {
            ASR::symbol_t *s = scope->get_symbol(fn_name);
            ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(s);
            return b.Call(s, new_args, expr_type(f->m_return_var), nullptr);
        }
        for (size_t i = 0; i < new_args.size(); i++) {
            fill_func_arg("x" + std::to_string(i), arg_types[0]);
        }

        auto result = declare(fn_name, return_type, ReturnVar);

        ASR::expr_t* test;
        body.push_back(al, b.Assignment(result, args[0]));
        if (ASR::is_a<ASR::Integer_t>(*return_type)) {
            for (size_t i = 1; i < args.size(); i++) {
                test = make_Compare(make_IntegerCompare_t, args[i], Lt, result);
                Vec<ASR::stmt_t *> if_body; if_body.reserve(al, 1);
                if_body.push_back(al, b.Assignment(result, args[i]));
                body.push_back(al, STMT(ASR::make_If_t(al, loc, test,
                    if_body.p, if_body.n, nullptr, 0)));
            }
        } else if (ASR::is_a<ASR::Real_t>(*return_type)) {
            for (size_t i = 1; i < args.size(); i++) {
                test = make_Compare(make_RealCompare_t, args[i], Lt, result);
                Vec<ASR::stmt_t *> if_body; if_body.reserve(al, 1);
                if_body.push_back(al, b.Assignment(result, args[i]));
                body.push_back(al, STMT(ASR::make_If_t(al, loc, test,
                    if_body.p, if_body.n, nullptr, 0)));
            }
        } else if (ASR::is_a<ASR::Character_t>(*return_type)) {
            for (size_t i = 1; i < args.size(); i++) {
                test = make_Compare(make_StringCompare_t, args[i], Lt, result);
                Vec<ASR::stmt_t *> if_body; if_body.reserve(al, 1);
                if_body.push_back(al, b.Assignment(result, args[i]));
                body.push_back(al, STMT(ASR::make_If_t(al, loc, test,
                    if_body.p, if_body.n, nullptr, 0)));
            }
        } else {
            throw LCompilersException("Arguments to min0 must be of real, integer or character type");
        }
        ASR::symbol_t *f_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, f_sym);
        return b.Call(f_sym, new_args, return_type, nullptr);
    }

} // namespace Min

namespace Partition {

    static inline ASR::expr_t* eval_Partition(Allocator &al, const Location &loc,
            std::string &s_var, std::string &sep) {
        /*
            using KMP algorithm to find separator inside string
            res_tuple: stores the resulting 3-tuple expression --->
            (if separator exist)           tuple:   (left of separator, separator, right of separator)
            (if separator does not exist)  tuple:   (string, "", "")
            res_tuple_type: stores the type of each expression present in resulting 3-tuple
        */
        ASRBuilder b(al, loc);
        int sep_pos = ASRUtils::KMP_string_match(s_var, sep);
        std::string first_res, second_res, third_res;
        if(sep_pos == -1) {
            /* seperator does not exist */
            first_res = s_var;
            second_res = "";
            third_res = "";
        } else {
            first_res = s_var.substr(0, sep_pos);
            second_res = sep;
            third_res = s_var.substr(sep_pos + sep.size());
        }

        Vec<ASR::expr_t *> res_tuple; res_tuple.reserve(al, 3);
        ASR::ttype_t *first_res_type = character(first_res.size());
        ASR::ttype_t *second_res_type = character(second_res.size());
        ASR::ttype_t *third_res_type = character(third_res.size());
        return b.TupleConstant({ StringConstant(first_res, first_res_type),
            StringConstant(second_res, second_res_type),
            StringConstant(third_res, third_res_type) },
            b.Tuple({first_res_type, second_res_type, third_res_type}));
    }

    static inline ASR::asr_t *create_partition(Allocator &al, const Location &loc,
            Vec<ASR::expr_t*> &args, ASR::expr_t *s_var,
            diag::Diagnostics& diag) {
        ASRBuilder b(al, loc);
        if (args.size() != 1) {
            append_error(diag, "str.partition() takes exactly one argument", loc);
            return nullptr;
        }
        ASR::expr_t *arg = args[0];
        if (!ASRUtils::is_character(*expr_type(arg))) {
            append_error(diag, "str.partition() takes one arguments of type: str", arg->base.loc);
            return nullptr;
        }

        Vec<ASR::expr_t *> e_args; e_args.reserve(al, 2);
        e_args.push_back(al, s_var);
        e_args.push_back(al, arg);

        ASR::ttype_t *return_type = b.Tuple({character(-2), character(-2), character(-2)});
        ASR::expr_t *value = nullptr;
        if (ASR::is_a<ASR::StringConstant_t>(*s_var)
         && ASR::is_a<ASR::StringConstant_t>(*arg)) {
            std::string s_sep = ASR::down_cast<ASR::StringConstant_t>(arg)->m_s;
            std::string s_str = ASR::down_cast<ASR::StringConstant_t>(s_var)->m_s;
            if (s_sep.size() == 0) {
                append_error(diag, "Separator cannot be an empty string", arg->base.loc);
                return nullptr;
            }
            value = eval_Partition(al, loc, s_str, s_sep);
        }

        return ASR::make_IntrinsicScalarFunction_t(al, loc,
            static_cast<int64_t>(IntrinsicScalarFunctions::Partition),
            e_args.p, e_args.n, 0, return_type, value);
    }

    static inline ASR::expr_t *instantiate_Partition(Allocator &al,
            const Location &loc, SymbolTable *scope,
            Vec<ASR::ttype_t*>& /*arg_types*/, ASR::ttype_t *return_type,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        // TODO: show runtime error for empty separator or pattern
        declare_basic_variables("_lpython_str_partition");
        fill_func_arg("target_string", character(-2));
        fill_func_arg("pattern", character(-2));

        auto result = declare("result", return_type, ReturnVar);
        auto index = declare("index", int32, Local);
        body.push_back(al, b.Assignment(index, b.Call(UnaryIntrinsicFunction::
            create_KMP_function(al, loc, scope), args, int32)));
        body.push_back(al, b.If(iEq(index, i32_n(-1)), {
                b.Assignment(result, b.TupleConstant({ args[0],
                    StringConstant("", character(0)),
                    StringConstant("", character(0)) },
                b.Tuple({character(-2), character(0), character(0)})))
            }, {
                b.Assignment(result, b.TupleConstant({
                    StringSection(args[0], i32(0), index), args[1],
                    StringSection(args[0], iAdd(index, StringLen(args[1])),
                        StringLen(args[0]))}, return_type))
            }));
        body.push_back(al, Return());
        ASR::symbol_t *fn_sym = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, fn_sym);
        return b.Call(fn_sym, new_args, return_type, nullptr);
    }

} // namespace Partition

namespace SymbolicSymbol {

    static inline void verify_args(const ASR::IntrinsicScalarFunction_t& x, diag::Diagnostics& diagnostics) {
        const Location& loc = x.base.base.loc;
        ASRUtils::require_impl(x.n_args == 1,
            "SymbolicSymbol intrinsic must have exactly 1 input argument",
            loc, diagnostics);

        ASR::ttype_t* input_type = ASRUtils::expr_type(x.m_args[0]);
        ASRUtils::require_impl(ASR::is_a<ASR::Character_t>(*input_type),
            "SymbolicSymbol intrinsic expects a character input argument",
            loc, diagnostics);
    }

    static inline ASR::expr_t *eval_SymbolicSymbol(Allocator &/*al*/,
    const Location &/*loc*/, ASR::ttype_t *, Vec<ASR::expr_t*>& /*args*/) {
        // TODO
        return nullptr;
    }

    static inline ASR::asr_t* create_SymbolicSymbol(Allocator& al, const Location& loc,
            Vec<ASR::expr_t*>& args,
            diag::Diagnostics& diag) {
        if (args.size() != 1) {
            append_error(diag, "Intrinsic Symbol function accepts exactly 1 argument", loc);
            return nullptr;
        }

        ASR::ttype_t *type = ASRUtils::expr_type(args[0]);
        if (!ASRUtils::is_character(*type)) {
            append_error(diag, "Argument of the Symbol function must be a Character",
                args[0]->base.loc);
            return nullptr;
        }

        ASR::ttype_t *to_type = ASRUtils::TYPE(ASR::make_SymbolicExpression_t(al, loc));
        return UnaryIntrinsicFunction::create_UnaryFunction(al, loc, args, eval_SymbolicSymbol,
            static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicSymbol), 0, to_type);
    }

} // namespace SymbolicSymbol

#define create_symbolic_binary_macro(X)                                                    \
namespace X{                                                                               \
    static inline void verify_args(const ASR::IntrinsicScalarFunction_t& x,                \
            diag::Diagnostics& diagnostics) {                                              \
        ASRUtils::require_impl(x.n_args == 2, "Intrinsic function `"#X"` accepts"          \
            "exactly 2 arguments", x.base.base.loc, diagnostics);                          \
                                                                                           \
        ASR::ttype_t* left_type = ASRUtils::expr_type(x.m_args[0]);                        \
        ASR::ttype_t* right_type = ASRUtils::expr_type(x.m_args[1]);                       \
                                                                                           \
        ASRUtils::require_impl(ASR::is_a<ASR::SymbolicExpression_t>(*left_type) &&         \
            ASR::is_a<ASR::SymbolicExpression_t>(*right_type),                             \
            "Both arguments of `"#X"` must be of type SymbolicExpression",                 \
            x.base.base.loc, diagnostics);                                                 \
    }                                                                                      \
                                                                                           \
    static inline ASR::expr_t* eval_##X(Allocator &/*al*/, const Location &/*loc*/,        \
            ASR::ttype_t *, Vec<ASR::expr_t*> &/*args*/) {                                 \
        /*TODO*/                                                                           \
        return nullptr;                                                                    \
    }                                                                                      \
                                                                                           \
    static inline ASR::asr_t* create_##X(Allocator& al, const Location& loc,               \
            Vec<ASR::expr_t*>& args,                                                       \
            diag::Diagnostics& diag) {       \
        if (args.size() != 2) {                                                            \
            append_error(diag, "Intrinsic function `"#X"` accepts exactly 2 arguments",    \
                loc);                                                                      \
            return nullptr;                                                                \
        }                                                                                  \
                                                                                           \
        for (size_t i = 0; i < args.size(); i++) {                                         \
            ASR::ttype_t* argtype = ASRUtils::expr_type(args[i]);                          \
            if(!ASR::is_a<ASR::SymbolicExpression_t>(*argtype)) {                          \
                append_error(diag,                                                         \
                    "Arguments of `"#X"` function must be of type SymbolicExpression",     \
                    args[i]->base.loc);                                                    \
                return nullptr;                                                            \
            }                                                                              \
        }                                                                                  \
                                                                                           \
        Vec<ASR::expr_t*> arg_values;                                                      \
        arg_values.reserve(al, args.size());                                               \
        for( size_t i = 0; i < args.size(); i++ ) {                                        \
            arg_values.push_back(al, ASRUtils::expr_value(args[i]));                       \
        }                                                                                  \
        ASR::ttype_t *to_type = ASRUtils::TYPE(ASR::make_SymbolicExpression_t(al, loc));   \
        ASR::expr_t* compile_time_value = eval_##X(al, loc, to_type, arg_values);          \
        return ASR::make_IntrinsicScalarFunction_t(al, loc,                                \
                static_cast<int64_t>(IntrinsicScalarFunctions::X),                         \
                args.p, args.size(), 0, to_type, compile_time_value);                      \
    }                                                                                      \
} // namespace X

create_symbolic_binary_macro(SymbolicAdd)
create_symbolic_binary_macro(SymbolicSub)
create_symbolic_binary_macro(SymbolicMul)
create_symbolic_binary_macro(SymbolicDiv)
create_symbolic_binary_macro(SymbolicPow)
create_symbolic_binary_macro(SymbolicDiff)

#define create_symbolic_constants_macro(X)                                                \
namespace X {                                                                             \
    static inline void verify_args(const ASR::IntrinsicScalarFunction_t& x,               \
            diag::Diagnostics& diagnostics) {                                             \
        const Location& loc = x.base.base.loc;                                            \
        ASRUtils::require_impl(x.n_args == 0,                                             \
            #X " does not take arguments", loc, diagnostics);                             \
    }                                                                                     \
                                                                                          \
    static inline ASR::expr_t* eval_##X(Allocator &/*al*/, const Location &/*loc*/,       \
            ASR::ttype_t *, Vec<ASR::expr_t*> &/*args*/) {                                \
        /*TODO*/                                                                          \
        return nullptr;                                                                   \
    }                                                                                     \
                                                                                          \
    static inline ASR::asr_t* create_##X(Allocator& al, const Location& loc,              \
            Vec<ASR::expr_t*>& args,                                                      \
            diag::Diagnostics& /*diag*/) {  \
        ASR::ttype_t *to_type = ASRUtils::TYPE(ASR::make_SymbolicExpression_t(al, loc));  \
        ASR::expr_t* compile_time_value = eval_##X(al, loc, to_type, args);               \
        return ASR::make_IntrinsicScalarFunction_t(al, loc,                               \
                static_cast<int64_t>(IntrinsicScalarFunctions::X),                        \
                nullptr, 0, 0, to_type, compile_time_value);                              \
    }                                                                                     \
} // namespace X

create_symbolic_constants_macro(SymbolicPi)
create_symbolic_constants_macro(SymbolicE)

namespace SymbolicInteger {

    static inline void verify_args(const ASR::IntrinsicScalarFunction_t& x, diag::Diagnostics& diagnostics) {
        ASRUtils::require_impl(x.n_args == 1,
            "SymbolicInteger intrinsic must have exactly 1 input argument",
            x.base.base.loc, diagnostics);

        ASR::ttype_t* input_type = ASRUtils::expr_type(x.m_args[0]);
        ASRUtils::require_impl(ASR::is_a<ASR::Integer_t>(*input_type),
            "SymbolicInteger intrinsic expects an integer input argument",
            x.base.base.loc, diagnostics);
    }

    static inline ASR::expr_t* eval_SymbolicInteger(Allocator &/*al*/,
    const Location &/*loc*/, ASR::ttype_t *, Vec<ASR::expr_t*>& /*args*/) {
        // TODO
        return nullptr;
    }

    static inline ASR::asr_t* create_SymbolicInteger(Allocator& al, const Location& loc,
            Vec<ASR::expr_t*>& args,
            diag::Diagnostics& /*diag*/) {
        ASR::ttype_t *to_type = ASRUtils::TYPE(ASR::make_SymbolicExpression_t(al, loc));
        return UnaryIntrinsicFunction::create_UnaryFunction(al, loc, args, eval_SymbolicInteger,
            static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicInteger), 0, to_type);
    }

} // namespace SymbolicInteger

namespace SymbolicHasSymbolQ {
    static inline void verify_args(const ASR::IntrinsicScalarFunction_t& x,
        diag::Diagnostics& diagnostics) {
        ASRUtils::require_impl(x.n_args == 2, "Intrinsic function SymbolicHasSymbolQ"
            "accepts exactly 2 arguments", x.base.base.loc, diagnostics);

        ASR::ttype_t* left_type = ASRUtils::expr_type(x.m_args[0]);
        ASR::ttype_t* right_type = ASRUtils::expr_type(x.m_args[1]);

        ASRUtils::require_impl(ASR::is_a<ASR::SymbolicExpression_t>(*left_type) &&
            ASR::is_a<ASR::SymbolicExpression_t>(*right_type),
            "Both arguments of SymbolicHasSymbolQ must be of type SymbolicExpression",
                x.base.base.loc, diagnostics);
    }

    static inline ASR::expr_t* eval_SymbolicHasSymbolQ(Allocator &/*al*/,
        const Location &/*loc*/, ASR::ttype_t *, Vec<ASR::expr_t*> &/*args*/) {
        /*TODO*/
        return nullptr;
    }

    static inline ASR::asr_t* create_SymbolicHasSymbolQ(Allocator& al,
        const Location& loc, Vec<ASR::expr_t*>& args,
        diag::Diagnostics& diag) {

        if (args.size() != 2) {
            append_error(diag, "Intrinsic function SymbolicHasSymbolQ accepts exactly 2 arguments", loc);
            return nullptr;
        }

        for (size_t i = 0; i < args.size(); i++) {
            ASR::ttype_t* argtype = ASRUtils::expr_type(args[i]);
            if(!ASR::is_a<ASR::SymbolicExpression_t>(*argtype)) {
                append_error(diag, "Arguments of SymbolicHasSymbolQ function must be of type SymbolicExpression",
                    args[i]->base.loc);
                return nullptr;
            }
        }

        Vec<ASR::expr_t*> arg_values;
        arg_values.reserve(al, args.size());
        for( size_t i = 0; i < args.size(); i++ ) {
            arg_values.push_back(al, ASRUtils::expr_value(args[i]));
        }

        ASR::expr_t* compile_time_value = eval_SymbolicHasSymbolQ(al, loc, logical, arg_values);
        return ASR::make_IntrinsicScalarFunction_t(al, loc,
            static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicHasSymbolQ),
            args.p, args.size(), 0, logical, compile_time_value);
    }
} // namespace SymbolicHasSymbolQ

namespace SymbolicGetArgument {
    static inline void verify_args(const ASR::IntrinsicScalarFunction_t& x,
        diag::Diagnostics& diagnostics) {
        ASRUtils::require_impl(x.n_args == 2, "Intrinsic function SymbolicGetArgument"
            "accepts exactly 2 argument", x.base.base.loc, diagnostics);

        ASR::ttype_t* arg1_type = ASRUtils::expr_type(x.m_args[0]);
        ASR::ttype_t* arg2_type = ASRUtils::expr_type(x.m_args[1]);
        ASRUtils::require_impl(ASR::is_a<ASR::SymbolicExpression_t>(*arg1_type),
            "SymbolicGetArgument expects the first argument to be of type SymbolicExpression",
                x.base.base.loc, diagnostics);
        ASRUtils::require_impl(ASR::is_a<ASR::Integer_t>(*arg2_type),
            "SymbolicGetArgument expects the second argument to be of type Integer",
                x.base.base.loc, diagnostics);
    }

    static inline ASR::expr_t* eval_SymbolicGetArgument(Allocator &/*al*/,
        const Location &/*loc*/, ASR::ttype_t *, Vec<ASR::expr_t*> &/*args*/) {
        /*TODO*/
        return nullptr;
    }

    static inline ASR::asr_t* create_SymbolicGetArgument(Allocator& al,
        const Location& loc, Vec<ASR::expr_t*>& args,
        diag::Diagnostics& diag) {

        if (args.size() != 2) {
            append_error(diag, "Intrinsic function SymbolicGetArguments accepts exactly 2 argument", loc);
            return nullptr;
        }

        ASR::ttype_t* arg1_type = ASRUtils::expr_type(args[0]);
        ASR::ttype_t* arg2_type = ASRUtils::expr_type(args[1]);
        if (!ASR::is_a<ASR::SymbolicExpression_t>(*arg1_type)) {
            append_error(diag, "The first argument of SymbolicGetArgument function must be of type SymbolicExpression",
                    args[0]->base.loc);
                return nullptr;
        }
        if (!ASR::is_a<ASR::Integer_t>(*arg2_type)) {
            append_error(diag, "The second argument of SymbolicGetArgument function must be of type Integer",
                    args[1]->base.loc);
                return nullptr;
        }

        ASR::ttype_t *to_type = ASRUtils::TYPE(ASR::make_SymbolicExpression_t(al, loc));
        return UnaryIntrinsicFunction::create_UnaryFunction(al, loc, args, eval_SymbolicGetArgument,
            static_cast<int64_t>(IntrinsicScalarFunctions::SymbolicGetArgument),
            0, to_type);
    }
} // namespace SymbolicGetArgument

#define create_symbolic_query_macro(X)                                                    \
namespace X {                                                                             \
    static inline void verify_args(const ASR::IntrinsicScalarFunction_t& x,               \
            diag::Diagnostics& diagnostics) {                                             \
        const Location& loc = x.base.base.loc;                                            \
        ASRUtils::require_impl(x.n_args == 1,                                             \
            #X " must have exactly 1 input argument", loc, diagnostics);                  \
                                                                                          \
        ASR::ttype_t* input_type = ASRUtils::expr_type(x.m_args[0]);                      \
        ASRUtils::require_impl(ASR::is_a<ASR::SymbolicExpression_t>(*input_type),         \
            #X " expects an argument of type SymbolicExpression", loc, diagnostics);      \
    }                                                                                     \
                                                                                          \
    static inline ASR::expr_t* eval_##X(Allocator &/*al*/, const Location &/*loc*/,       \
            ASR::ttype_t *, Vec<ASR::expr_t*> &/*args*/) {                                \
        /*TODO*/                                                                          \
        return nullptr;                                                                   \
    }                                                                                     \
                                                                                          \
    static inline ASR::asr_t* create_##X(Allocator& al, const Location& loc,              \
            Vec<ASR::expr_t*>& args,                                                      \
            diag::Diagnostics& diag) {                                                    \
        if (args.size() != 1) {                                                           \
            append_error(diag, "Intrinsic " #X " function accepts exactly 1 argument",    \
                loc);                                                                     \
            return nullptr;                                                               \
        }                                                                                 \
                                                                                          \
        ASR::ttype_t* argtype = ASRUtils::expr_type(args[0]);                             \
        if (!ASR::is_a<ASR::SymbolicExpression_t>(*argtype)) {                            \
            append_error(diag,                                                            \
                "Argument of " #X " function must be of type SymbolicExpression",         \
                args[0]->base.loc);                                                       \
            return nullptr;                                                               \
        }                                                                                 \
                                                                                          \
        return UnaryIntrinsicFunction::create_UnaryFunction(al, loc, args, eval_##X,      \
            static_cast<int64_t>(IntrinsicScalarFunctions::X), 0, logical);               \
    }                                                                                     \
} // namespace X

create_symbolic_query_macro(SymbolicAddQ)
create_symbolic_query_macro(SymbolicMulQ)
create_symbolic_query_macro(SymbolicPowQ)
create_symbolic_query_macro(SymbolicLogQ)
create_symbolic_query_macro(SymbolicSinQ)

#define create_symbolic_unary_macro(X)                                                    \
namespace X {                                                                             \
    static inline void verify_args(const ASR::IntrinsicScalarFunction_t& x,               \
            diag::Diagnostics& diagnostics) {                                             \
        const Location& loc = x.base.base.loc;                                            \
        ASRUtils::require_impl(x.n_args == 1,                                             \
            #X " must have exactly 1 input argument", loc, diagnostics);                  \
                                                                                          \
        ASR::ttype_t* input_type = ASRUtils::expr_type(x.m_args[0]);                      \
        ASRUtils::require_impl(ASR::is_a<ASR::SymbolicExpression_t>(*input_type),         \
            #X " expects an argument of type SymbolicExpression", loc, diagnostics);      \
    }                                                                                     \
                                                                                          \
    static inline ASR::expr_t* eval_##X(Allocator &/*al*/, const Location &/*loc*/,       \
            ASR::ttype_t *, Vec<ASR::expr_t*> &/*args*/) {                                \
        /*TODO*/                                                                          \
        return nullptr;                                                                   \
    }                                                                                     \
                                                                                          \
    static inline ASR::asr_t* create_##X(Allocator& al, const Location& loc,              \
            Vec<ASR::expr_t*>& args,                                                      \
            diag::Diagnostics& diag) {                                                    \
        if (args.size() != 1) {                                                           \
            append_error(diag, "Intrinsic " #X " function accepts exactly 1 argument",    \
                loc);                                                                     \
            return nullptr;                                                               \
        }                                                                                 \
                                                                                          \
        ASR::ttype_t* argtype = ASRUtils::expr_type(args[0]);                             \
        if (!ASR::is_a<ASR::SymbolicExpression_t>(*argtype)) {                            \
            append_error(diag,                                                            \
                "Argument of " #X " function must be of type SymbolicExpression",         \
                args[0]->base.loc);                                                       \
            return nullptr;                                                               \
        }                                                                                 \
                                                                                          \
        ASR::ttype_t *to_type = ASRUtils::TYPE(ASR::make_SymbolicExpression_t(al, loc));  \
        return UnaryIntrinsicFunction::create_UnaryFunction(al, loc, args, eval_##X,      \
            static_cast<int64_t>(IntrinsicScalarFunctions::X), 0, to_type);               \
    }                                                                                     \
} // namespace X

create_symbolic_unary_macro(SymbolicSin)
create_symbolic_unary_macro(SymbolicCos)
create_symbolic_unary_macro(SymbolicLog)
create_symbolic_unary_macro(SymbolicExp)
create_symbolic_unary_macro(SymbolicAbs)
create_symbolic_unary_macro(SymbolicExpand)

} // namespace LCompilers::ASRUtils

#endif // LIBASR_PASS_INTRINSIC_FUNCTIONS_H
