#ifndef LFORTRAN_SEMANTICS_AST_COMPTIME_EVAL_H
#define LFORTRAN_SEMANTICS_AST_COMPTIME_EVAL_H

#include <complex>

#include <libasr/asr.h>
#include <lfortran/ast.h>
#include <libasr/bigint.h>
#include <libasr/string_utils.h>
#include <lfortran/utils.h>
#include <lfortran/semantics/semantic_exception.h>

#include <set>

namespace LCompilers::LFortran {

struct IntrinsicProceduresAsASRNodes {

    private:

        std::set<std::string> intrinsics_present_in_ASR;
        std::set<std::string> kind_based_intrinsics;

    public:

        IntrinsicProceduresAsASRNodes() {
            intrinsics_present_in_ASR = {"size", "lbound", "ubound",
                "transpose", "matmul", "pack", "transfer", "cmplx",
                "dcmplx", "reshape", "ichar", "iachar", "maxloc",
                "null", "associated", "all"};

            kind_based_intrinsics = {"scan", "verify"};
        }

        bool is_intrinsic_present_in_ASR(std::string& name) {
            return intrinsics_present_in_ASR.find(name) != intrinsics_present_in_ASR.end();
        }

        bool is_kind_based_selection_required(std::string& name) {
            return kind_based_intrinsics.find(name) != kind_based_intrinsics.end();
        }

};

struct IntrinsicProcedures {

    const std::string m_kind = "lfortran_intrinsic_kind";
    const std::string m_builtin = "lfortran_intrinsic_builtin";
    const std::string m_trig = "lfortran_intrinsic_trig";
    const std::string m_math = "lfortran_intrinsic_math";
    const std::string m_math2 = "lfortran_intrinsic_math2";
    const std::string m_math3 = "lfortran_intrinsic_math3";
    const std::string m_string = "lfortran_intrinsic_string";
    const std::string m_bit = "lfortran_intrinsic_bit";
    const std::string m_ieee_arithmetic = "lfortran_intrinsic_ieee_arithmetic";
    const std::string m_iso_c_binding = "lfortran_intrinsic_iso_c_binding";

    /*
        The last parameter is true if the callback accepts evaluated arguments.

        If true, the arguments are first converted to their compile time
        "values". If not possible, nullptr is returned; otherwise the
        callback is called and it always succeeds to evaluate the result at
        compile time.

        If false, the arguments might not be compile time values. The
        callback can return nullptr if it cannot evaluate itself.
    */

    typedef ASR::expr_t* (*comptime_eval_callback)(Allocator &, const Location &, Vec<ASR::expr_t*> &);
    std::map<std::string, std::tuple<std::string, comptime_eval_callback, bool>> comptime_eval_map;

    IntrinsicProcedures() {
        comptime_eval_map = {
            // Arguments can be evaluated or not
            {"kind", {m_kind, &eval_kind, false}},
            {"tiny", {m_builtin, &eval_tiny, false}},
            // real and int get transformed into ExplicitCast
            // in intrinsic_function_transformation()
            // So we shouldn't even encounter them here
            {"int", {m_builtin, &eval_int, false}},
            {"real", {m_builtin, &not_implemented, false}},
            {"any", {m_builtin, &not_implemented, false}},
            {"is_iostat_eor", {m_builtin, &not_implemented, false}},
            {"is_iostat_end", {m_builtin, &not_implemented, false}},
            {"get_command_argument", {m_builtin, &not_implemented, false}},
            {"command_argument_count", {m_builtin, &not_implemented, false}},
            {"execute_command_line", {m_builtin, &not_implemented, false}},
            {"get_environment_variable", {m_builtin, &not_implemented, false}},

            // Require evaluated arguments
            {"aimag", {m_math, &eval_aimag, true}},
            {"imag", {m_math, &eval_aimag, true}},
            {"dimag", {m_math, &eval_aimag, true}},
            {"char", {m_builtin, &eval_char, true}},
            {"floor", {m_math3, &eval_floor, true}},
            {"ceiling", {m_math2, &eval_ceiling, true}},
            {"nint", {m_math2, &eval_nint, true}},
            {"mod", {m_math2, &eval_mod, true}},
            {"modulo", {m_math2, &eval_modulo, true}},
            {"min", {m_math2, &eval_min, true}},
            {"max", {m_math2, &eval_max, true}},
            {"min0", {m_math2, &eval_min0, true}},
            {"dmin1", {m_math2, &eval_dmin1, true}},
            {"max0", {m_math2, &eval_max0, true}},
            {"dmax1", {m_math2, &eval_dmax1, true}},
            {"merge", {m_math2, &not_implemented, false}},
            {"selected_int_kind", {m_kind, &eval_selected_int_kind, true}},
            {"selected_real_kind", {m_kind, &eval_selected_real_kind, true}},
            {"selected_char_kind", {m_kind, &eval_selected_char_kind, true}},
            {"exp", {m_math, &eval_exp, true}},
            {"dexp", {m_math, &eval_dexp, true}},
            {"sexp", {m_math, &eval_sexp, true}},
            {"cexp", {m_math, &eval_cexp, true}},
            {"zexp", {m_math, &eval_zexp, true}},
            {"range", {m_math, &eval_range, false}},
            {"epsilon", {m_math, &eval_epsilon, false}},
            {"log", {m_math, &eval_log, true}},
            {"alog", {m_math, &eval_alog, true}},
            {"slog", {m_math, &eval_slog, true}},
            {"dlog", {m_math, &eval_dlog, true}},
            {"clog", {m_math, &eval_clog, true}},
            {"zlog", {m_math, &eval_zlog, true}},
            {"erf", {m_math, &eval_erf, true}},
            {"erfc", {m_math, &eval_erfc, true}},
            {"abs", {m_math, &eval_abs, true}},
            {"iabs", {m_math, &eval_abs, true}},
            {"sqrt", {m_math, &eval_sqrt, true}},
            {"dsqrt", {m_math, &eval_dsqrt, true}},
            {"datan", {m_math, &eval_datan, true}},
            {"dabs", {m_math2, &eval_dabs, true}},
            {"dcos", {m_math, &eval_dcos, true}},
            {"dsin", {m_math, &eval_dsin, true}},
            {"gamma", {m_math, &eval_gamma, true}},
            {"log_gamma", {m_math, &eval_log_gamma, true}},
            {"log10", {m_math, &eval_log10, true}},
            {"dlog10", {m_math, &eval_dlog10, true}},

            //{"sin", {m_trig, &eval_sin, true}},
            {"sin", {m_math, &eval_sin, true}},
            {"cos", {m_math, &eval_cos, true}},
            {"tan", {m_math, &eval_tan, true}},

            {"asin", {m_math, &eval_asin, true}},
            {"acos", {m_math, &eval_acos, true}},
            {"atan", {m_math, &eval_atan, true}},

            {"sinh", {m_math, &eval_sinh, true}},
            {"cosh", {m_math, &eval_cosh, true}},
            {"tanh", {m_math, &eval_tanh, true}},

            {"asinh", {m_math, &eval_asinh, true}},
            {"acosh", {m_math, &eval_acosh, true}},
            {"atanh", {m_math, &eval_atanh, true}},

            {"atan2", {m_math, &eval_atan2, true}},
            {"sign", {m_math, &not_implemented, false}},

            {"dot_product", {m_math, &not_implemented, false}},
            {"conjg", {m_math, &not_implemented, false}},

            {"iand", {m_bit, &not_implemented, false}},
            {"ior", {m_bit, &not_implemented, false}},
            {"ieor", {m_bit, &eval_ieor, true}},
            {"ibclr", {m_bit, &eval_ibclr, true}},
            {"ibset", {m_bit, &eval_ibset, true}},
            {"btest", {m_bit, &not_implemented, false}},
            // Elemental function
            {"ishft", {m_bit, &eval_ishft, true}},
            {"shiftr", {m_bit, &not_implemented, true}},
            {"shiftl", {m_bit, &not_implemented, true}},

            // These will fail if used in symbol table visitor, but will be
            // left unevaluated in body visitor
            {"trim", {m_string, &not_implemented, false}},
            {"len_trim", {m_string, &not_implemented, false}},
            {"adjustl", {m_string, &not_implemented, false}},
            {"adjustr", {m_string, &not_implemented, false}},
            {"lgt", {m_string, &not_implemented, false}},
            {"llt", {m_string, &not_implemented, false}},
            {"lge", {m_string, &not_implemented, false}},
            {"lle", {m_string, &not_implemented, false}},
            {"len_adjustl", {m_string, &not_implemented, false}},
            {"repeat", {m_string, &not_implemented, false}},
            {"new_line", {m_string, &eval_new_line, false}},
            {"scan_kind4", {m_string, &not_implemented, false}},
            {"scan_kind8", {m_string, &not_implemented, false}},
            {"verify_kind4", {m_string, &not_implemented, false}},
            {"verify_kind8", {m_string, &not_implemented, false}},

            // Subroutines
            {"cpu_time", {m_math, &not_implemented, false}},
            {"bit_size", {m_builtin, &eval_bit_size, false}},
            {"not", {m_bit, &eval_not, true}},
            {"mvbits", {m_bit, &not_implemented, true}},
            {"bge", {m_bit, &not_implemented, true}},
            {"bgt", {m_bit, &not_implemented, true}},
            {"ble", {m_bit, &not_implemented, true}},
            {"blt", {m_bit, &not_implemented, true}},
            {"ibits", {m_bit, &not_implemented, true}},
            {"count", {m_bit, &not_implemented, false}},
            {"iachar",  {m_builtin, &eval_iachar, true}},
            {"achar", {m_builtin, &eval_achar, false}},
            {"len", {m_builtin, &eval_len, false}},
            {"move_alloc", {m_builtin, &not_implemented, false}},
            {"shape", {m_builtin, &not_implemented, false}},
            {"reshape", {m_builtin, &not_implemented, false}},
            {"present", {m_builtin, &not_implemented, false}},
            {"lbound", {m_builtin, &not_implemented, false}},
            {"ubound", {m_builtin, &not_implemented, false}},
            {"allocated", {m_builtin, &not_implemented, false}},
            {"minval", {m_builtin, &not_implemented, false}},
            {"maxval", {m_builtin, &not_implemented, false}},
            {"sum", {m_builtin, &not_implemented, false}},
            {"index", {m_string, &not_implemented, false}},
            {"system_clock", {m_math, &not_implemented, false}},
            {"random_number", {m_math, &not_implemented, false}},
            {"date_and_time", {m_string, &not_implemented, false}},

            // Inquiry function
            {"huge", {m_math2, &eval_huge, false}},

            // Transformational function
            {"c_associated", {m_iso_c_binding, &not_implemented, false}},
            {"all",          {m_builtin, &not_implemented, false}},

            // IEEE Arithmetic
            {"ieee_value", {m_ieee_arithmetic, &not_implemented, false}},
            {"ieee_is_nan", {m_ieee_arithmetic, &not_implemented, false}},

            // ISO C Binding
            {"c_associated", {m_iso_c_binding, &not_implemented, false}},
        };
    }

    bool is_intrinsic(std::string name) const {
        auto search = comptime_eval_map.find(name);
        if (search != comptime_eval_map.end()) {
            return true;
        } else {
            return false;
        }
    }

    std::string get_module(std::string name, const Location &loc) const {
        auto search = comptime_eval_map.find(name);
        if (search != comptime_eval_map.end()) {
            std::string module_name = std::get<0>(search->second);
            return module_name;
        } else {
            throw SemanticError("Function '" + name
                + "' not found among intrinsic procedures",
                loc);
        }
    }

    ASR::expr_t *comptime_eval(std::string name, Allocator &al, const Location &loc, Vec<ASR::call_arg_t>& args) const {
        auto search = comptime_eval_map.find(name);
        if (search != comptime_eval_map.end()) {
            comptime_eval_callback cb = std::get<1>(search->second);
            bool eval_args = std::get<2>(search->second);
            if (eval_args) {
                Vec<ASR::call_arg_t> arg_values = ASRUtils::get_arg_values(al, args);
                if (arg_values.size() != args.size()) return nullptr;
                Vec<ASR::expr_t*> expr_args;
                expr_args.reserve(al, arg_values.size());
                for( auto& a: arg_values ) {
                    expr_args.push_back(al, a.m_value);
                }
                return cb(al, loc, expr_args);
            } else {
                Vec<ASR::expr_t*> expr_args;
                expr_args.reserve(al, args.size());
                for( auto& a: args ) {
                    expr_args.push_back(al, a.m_value);
                }
                return cb(al, loc, expr_args);
            }
        } else {
            throw SemanticError("Intrinsic function '" + name
                + "' compile time evaluation is not implemented yet",
                loc);
        }
    }

    static ASR::expr_t *eval_kind(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        // TODO: Refactor to allow early return
        // kind_num --> value {4, 8, etc.}
        int64_t kind_num = 4; // Default
        ASR::expr_t* kind_expr = args[0];
        // TODO: Check that the expression reduces to a valid constant expression (10.1.12)
        switch( kind_expr->type ) {
            case ASR::exprType::IntegerConstant: {
                kind_num = ASR::down_cast<ASR::Integer_t>(ASR::down_cast<ASR::IntegerConstant_t>(kind_expr)->m_type)->m_kind;
                break;
            }
            case ASR::exprType::RealConstant:{
                kind_num = ASR::down_cast<ASR::Real_t>(ASR::down_cast<ASR::RealConstant_t>(kind_expr)->m_type)->m_kind;
                break;
            }
            case ASR::exprType::LogicalConstant:{
                kind_num = ASR::down_cast<ASR::Logical_t>(ASR::down_cast<ASR::LogicalConstant_t>(kind_expr)->m_type)->m_kind;
                break;
            }
            case ASR::exprType::Var : {
                kind_num = ASRUtils::extract_kind<SemanticError>(kind_expr, loc);
                break;
            }
            default: {
                std::string msg = R"""(Only Integer literals or expressions which reduce to constant Integer are accepted as kind parameters.)""";
                throw SemanticError(msg, loc);
                break;
            }
        }
        ASR::ttype_t *type = ASRUtils::TYPE(
                ASR::make_Integer_t(al, loc,
                    4, nullptr, 0));
        return ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(al, loc,
                kind_num, type));
    }

    static ASR::expr_t *eval_bit_size(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        LCOMPILERS_ASSERT(args.size() >= 1);
        ASR::expr_t* arg = args[0];
        ASR::ttype_t* arg_type = ASRUtils::expr_type(arg);
        int64_t bit_size_val = 0;
        switch( arg_type->type ) {
            case ASR::ttypeType::Integer: {
                ASR::Integer_t* arg_int = ASR::down_cast<ASR::Integer_t>(arg_type);
                bit_size_val = arg_int->m_kind * 8;
                break;
            }
            default: {
                LCOMPILERS_ASSERT(false);
                break;
            }
        }
        ASR::ttype_t* int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4, nullptr, 0));
        return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, bit_size_val, int32_type));
    }

    static ASR::expr_t *eval_not(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        LCOMPILERS_ASSERT(args.size() >= 1);
        ASR::expr_t* arg = ASRUtils::expr_value(args[0]);
        LCOMPILERS_ASSERT(arg);
        ASR::ttype_t* arg_type = ASRUtils::expr_type(arg);
        LCOMPILERS_ASSERT(arg_type->type == ASR::ttypeType::Integer);
        ASR::Integer_t* arg_int_type = ASR::down_cast<ASR::Integer_t>(arg_type);
        ASR::IntegerConstant_t* arg_int = ASR::down_cast<ASR::IntegerConstant_t>(arg);
        int64_t not_arg = ~(arg_int->m_n);
        ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, arg_int_type->m_kind, nullptr, 0));
        return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, not_arg, int_type));
    }

    static ASR::expr_t *eval_tiny(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        // We assume the input is valid
        // ASR::expr_t* tiny_expr = args[0];
        ASR::ttype_t* tiny_type = ASRUtils::expr_type(args[0]);
        // TODO: Arrays of reals are a valid argument for tiny
        if (ASRUtils::is_array(tiny_type)){
            throw SemanticError("Array values not implemented yet",
                                loc);
        }
        // TODO: Figure out how to deal with higher precision later
        if (ASR::is_a<ASR::Real_t>(*tiny_type)) {
            // We don't actually need the value yet, it is enough to know it is a double
            // but it might provide further information later (precision)
            // double tiny_val = ASR::down_cast<ASR::RealConstant_t>(ASRUtils::expr_value(tiny_expr))->m_r;
            int tiny_kind = ASRUtils::extract_kind_from_ttype_t(tiny_type);
            if (tiny_kind == 4){
                float low_val = std::numeric_limits<float>::min();
                return ASR::down_cast<ASR::expr_t>(ASR::make_RealConstant_t(al, loc,
                                                                                low_val, // value
                                                                                tiny_type));
            } else {
                double low_val = std::numeric_limits<double>::min();
                return ASR::down_cast<ASR::expr_t>(ASR::make_RealConstant_t(al, loc,
                                                                                low_val, // value
                                                                                tiny_type));
                    }
        }
        else {
            throw SemanticError("Argument for tiny must be Real",
                                loc);
        }
    }

    static ASR::expr_t *eval_floor(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));
        // TODO: Implement optional kind; J3/18-007r1 --> FLOOR(A, [KIND])
        ASR::expr_t* func_expr = args[0];
        ASR::ttype_t* func_type = ASRUtils::expr_type(func_expr);
        if (ASR::is_a<ASR::Real_t>(*func_type)) {
            double rv = ASR::down_cast<ASR::RealConstant_t>(func_expr)->m_r;
            int64_t ival = floor(rv);
            ASR::ttype_t *type = ASRUtils::TYPE(
                    ASR::make_Integer_t(al, loc, 4, nullptr, 0));
            return ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(al, loc, ival, type));
        } else {
            throw SemanticError("floor must have one real argument", loc);
        }
    }

    static ASR::expr_t *eval_ceiling(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));
        // TODO: Implement optional kind; J3/18-007r1 --> CEILING(A, [KIND])
        ASR::expr_t* func_expr = args[0];
        ASR::ttype_t* func_type = ASRUtils::expr_type(func_expr);
        if (ASR::is_a<ASR::Real_t>(*func_type)) {
            double rv = ASR::down_cast<ASR::RealConstant_t>(func_expr)->m_r;
            int64_t ival = ceil(rv);
            ASR::ttype_t *type = ASRUtils::TYPE(
                    ASR::make_Integer_t(al, loc, 4, nullptr, 0));
            return ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(al, loc, ival, type));
        } else {
            throw SemanticError("floor must have one real argument", loc);
        }
    }

    static ASR::expr_t *eval_nint(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));
        ASR::expr_t* func_expr = args[0];
        ASR::ttype_t* func_type = ASRUtils::expr_type(func_expr);
        if (ASR::is_a<ASR::Real_t>(*func_type)) {
            double rv = ASR::down_cast<ASR::RealConstant_t>(func_expr)->m_r;
            int64_t ival = round(rv);
            ASR::ttype_t *type = ASRUtils::TYPE(
                    ASR::make_Integer_t(al, loc, 4, nullptr, 0));
            return ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(al, loc, ival, type));
        } else {
            throw SemanticError("nint must have one real argument", loc);
        }
    }

    typedef double (*trig_eval_callback_double)(double);
    typedef std::complex<double> (*trig_eval_callback_complex_double)(std::complex<double>);
    static ASR::expr_t *eval_trig(Allocator &al, const Location &loc,
            Vec<ASR::expr_t*> &args,
            trig_eval_callback_double trig_double,
            trig_eval_callback_complex_double trig_complex_double
            ) {
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));
        if (args.size() != 1) {
            throw SemanticError("Intrinsic trig function accepts exactly 1 argument", loc);
        }
        ASR::expr_t* trig_arg = args[0];
        ASR::ttype_t* t = ASRUtils::expr_type(args[0]);
        if (ASR::is_a<ASR::Real_t>(*t)) {
            double rv = ASR::down_cast<ASR::RealConstant_t>(trig_arg)->m_r;
            double val = trig_double(rv);
            return ASR::down_cast<ASR::expr_t>(ASR::make_RealConstant_t(al, loc, val, t));
        } else if (ASR::is_a<ASR::Complex_t>(*t)) {
            if (trig_complex_double) {
                double re = ASR::down_cast<ASR::ComplexConstant_t>(trig_arg)->m_re;
                double im = ASR::down_cast<ASR::ComplexConstant_t>(trig_arg)->m_im;
                std::complex<double> x(re, im);
                std::complex<double> result = trig_complex_double(x);
                re = std::real(result);
                im = std::imag(result);
                return ASR::down_cast<ASR::expr_t>(ASR::make_ComplexConstant_t(al, loc, re, im, t));
            } else {
                return nullptr;
            }
        } else {
            throw SemanticError("Argument for trig function must be Real or Complex", loc);
        }
    }

    typedef double (*eval2_callback_double)(double, double);
    static ASR::expr_t *eval_2args(Allocator &al, const Location &loc,
            Vec<ASR::expr_t*> &args,
            eval2_callback_double eval2_double
            ) {
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));
        if (args.size() != 2) {
            throw SemanticError("This intrinsic function accepts exactly 2 arguments", loc);
        }
        ASR::expr_t* trig_arg1 = args[0];
        ASR::ttype_t* t1 = ASRUtils::expr_type(args[0]);
        ASR::expr_t* trig_arg2 = args[1];
        ASR::ttype_t* t2 = ASRUtils::expr_type(args[1]);
        if (ASR::is_a<ASR::Real_t>(*t1) && ASR::is_a<ASR::Real_t>(*t2)) {
            double rv1 = ASR::down_cast<ASR::RealConstant_t>(trig_arg1)->m_r;
            double rv2 = ASR::down_cast<ASR::RealConstant_t>(trig_arg2)->m_r;
            double val = eval2_double(rv1, rv2);
            return ASR::down_cast<ASR::expr_t>(ASR::make_RealConstant_t(al, loc, val, t1));
        } else {
            throw SemanticError("Arguments for this intrinsic function must be Real", loc);
        }
    }

    typedef int64_t (*eval2_callback_int)(int64_t, int64_t);
    static ASR::expr_t *eval_2args_ri(Allocator &al, const Location &loc,
            Vec<ASR::expr_t*> &args,
            eval2_callback_double eval2_double,
            eval2_callback_int eval2_int) {
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));
        if (args.size() != 2) {
            throw SemanticError("This intrinsic function accepts exactly 2 arguments", loc);
        }
        ASR::expr_t* trig_arg1 = args[0];
        ASR::ttype_t* t1 = ASRUtils::expr_type(args[0]);
        ASR::expr_t* trig_arg2 = args[1];
        ASR::ttype_t* t2 = ASRUtils::expr_type(args[1]);
        if (ASR::is_a<ASR::Real_t>(*t1) && ASR::is_a<ASR::Real_t>(*t2)) {
            double rv1 = ASR::down_cast<ASR::RealConstant_t>(trig_arg1)->m_r;
            double rv2 = ASR::down_cast<ASR::RealConstant_t>(trig_arg2)->m_r;
            double val = eval2_double(rv1, rv2);
            return ASR::down_cast<ASR::expr_t>(ASR::make_RealConstant_t(al, loc, val, t1));
        } else if (ASR::is_a<ASR::Integer_t>(*t1) && ASR::is_a<ASR::Integer_t>(*t2)) {
            int64_t rv1 = ASR::down_cast<ASR::IntegerConstant_t>(trig_arg1)->m_n;
            int64_t rv2 = ASR::down_cast<ASR::IntegerConstant_t>(trig_arg2)->m_n;
            int64_t val = eval2_int(rv1, rv2);
            ASR::ttype_t *type = ASRUtils::TYPE(
                    ASR::make_Integer_t(al, loc, 4, nullptr, 0));
            return ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(al, loc, val, type));
        } else {
            throw SemanticError("Arguments for this intrinsic function must be Real or Integer", loc);
        }
    }

#define TRIG_CB(X) static std::complex<double> lfortran_z##X(std::complex<double> x) { return std::X(x); }
#define TRIG_CB2(X) static ASR::expr_t *eval_##X(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) { \
        return eval_trig(al, loc, args, &X, &lfortran_z##X); \
    }
#define TRIG2_CB(X, Y) static std::complex<double> lfortran_z##Y(std::complex<double> x) { return std::X(x); }
#define TRIG2_CB2(X, Y) static ASR::expr_t *eval_##Y(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) { \
        return eval_trig(al, loc, args, &X, &lfortran_z##Y); \
    }
#define TRIG(X) TRIG_CB(X) \
    TRIG_CB2(X)
#define TRIG2(X, Y) TRIG2_CB(X, Y) \
    TRIG2_CB2(X, Y)

TRIG(sin)
TRIG(cos)
TRIG(tan)
TRIG(asin)
TRIG(acos)
TRIG(atan)
TRIG(sinh)
TRIG(cosh)
TRIG(tanh)
TRIG(asinh)
TRIG(acosh)
TRIG(atanh)

TRIG(exp)
TRIG(log)
TRIG(sqrt)

TRIG2(exp, dexp)
TRIG2(exp, sexp)
TRIG2(exp, cexp)
TRIG2(exp, zexp)

TRIG2(log, alog)
TRIG2(log, dlog)
TRIG2(log10, dlog10)
TRIG2(log, slog)
TRIG2(log, clog)
TRIG2(log, zlog)

TRIG2(sin, dsin)
TRIG2(cos, dcos)
TRIG2(atan, datan)
TRIG2(sqrt, dsqrt)


    static ASR::expr_t *eval_erf(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        return eval_trig(al, loc, args, &erf, nullptr);
    }
    static ASR::expr_t *eval_erfc(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        return eval_trig(al, loc, args, &erfc, nullptr);
    }
    static ASR::expr_t *eval_gamma(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        return eval_trig(al, loc, args, &tgamma, nullptr);
    }
    static ASR::expr_t *eval_log_gamma(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        return eval_trig(al, loc, args, &lgamma, nullptr);
    }
    static ASR::expr_t *eval_log10(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        return eval_trig(al, loc, args, &log10, nullptr);
    }
    static ASR::expr_t *eval_atan2(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        return eval_2args(al, loc, args, &atan2);
    }

    static ASR::expr_t *eval_len(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        if( !ASRUtils::all_args_evaluated(args) ) {
            return nullptr;
        }
        LCOMPILERS_ASSERT(args.size() == 1 || args.size() == 2);
        ASR::expr_t *arg_value = ASRUtils::expr_value(args[0]);
        LCOMPILERS_ASSERT(arg_value->type == ASR::exprType::StringConstant);
        ASR::StringConstant_t *value_str = ASR::down_cast<ASR::StringConstant_t>(arg_value);
        int64_t len_str = to_lower(value_str->m_s).length();
        ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc,
                                                        4, nullptr, 0));
        return ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(al, loc, len_str, type));
    }

    static double lfortran_modulo(double x, double y) {
        if (x > 0 && y > 0) {
            return std::fmod(x, y);
        } else if (x < 0 && y < 0) {
            return -std::fmod(-x, -y);
        } else {
            return std::remainder(x, y);
        }
    }

    static int64_t lfortran_modulo_i(int64_t x, int64_t y) {
        if (x > 0 && y > 0) {
            return std::fmod(x, y);
        } else if (x < 0 && y < 0) {
            return -std::fmod(-x, -y);
        } else {
            return std::remainder(x, y);
        }
    }

    static double lfortran_mod(double x, double y) {
        return std::fmod(x, y);
    }

    static int64_t lfortran_mod_i(int64_t x, int64_t y) {
        return std::fmod(x, y);
    }

    static ASR::expr_t *eval_modulo(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        return eval_2args_ri(al, loc, args,
            &IntrinsicProcedures::lfortran_modulo,
            &IntrinsicProcedures::lfortran_modulo_i);
    }

    static ASR::expr_t *eval_mod(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        return eval_2args_ri(al, loc, args,
            &IntrinsicProcedures::lfortran_mod,
            &IntrinsicProcedures::lfortran_mod_i);
    }

    static double lfortran_min(double x, double y) {
        return std::fmin(x, y);
    }

    static int64_t lfortran_min_i(int64_t x, int64_t y) {
        return std::fmin(x, y);
    }

    static ASR::expr_t *eval_min(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        return eval_2args_ri(al, loc, args,
            &IntrinsicProcedures::lfortran_min,
            &IntrinsicProcedures::lfortran_min_i);
    }

    static ASR::expr_t *eval_dmin1(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        return eval_2args_ri(al, loc, args,
            &IntrinsicProcedures::lfortran_min,
            &IntrinsicProcedures::lfortran_min_i);
    }

    static ASR::expr_t *eval_min0(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        return eval_2args_ri(al, loc, args,
            &IntrinsicProcedures::lfortran_min,
            &IntrinsicProcedures::lfortran_min_i);
    }

    static double lfortran_max(double x, double y) {
        return std::fmax(x, y);
    }

    static int64_t lfortran_max_i(int64_t x, int64_t y) {
        return std::fmax(x, y);
    }

    static ASR::expr_t *eval_max(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        return eval_2args_ri(al, loc, args,
            &IntrinsicProcedures::lfortran_max,
            &IntrinsicProcedures::lfortran_max_i);
    }

    static ASR::expr_t *eval_dmax1(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        return eval_2args_ri(al, loc, args,
            &IntrinsicProcedures::lfortran_max,
            &IntrinsicProcedures::lfortran_max_i);
    }

    static ASR::expr_t *eval_max0(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        return eval_2args_ri(al, loc, args,
            &IntrinsicProcedures::lfortran_max,
            &IntrinsicProcedures::lfortran_max_i);
    }

    static ASR::expr_t *eval_abs(Allocator &al, const Location &loc,
            Vec<ASR::expr_t*> &args
            ) {
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));
        if (args.size() != 1) {
            throw SemanticError("Intrinsic abs function accepts exactly 1 argument", loc);
        }
        ASR::expr_t* trig_arg = args[0];
        ASR::ttype_t* t = ASRUtils::expr_type(args[0]);
        if (ASR::is_a<ASR::Real_t>(*t)) {
            double rv = ASR::down_cast<ASR::RealConstant_t>(trig_arg)->m_r;
            double val = std::abs(rv);
            return ASR::down_cast<ASR::expr_t>(ASR::make_RealConstant_t(al, loc, val, t));
        } else if (ASR::is_a<ASR::Integer_t>(*t)) {
            int64_t rv = ASR::down_cast<ASR::IntegerConstant_t>(trig_arg)->m_n;
            int64_t val = std::abs(rv);
            return ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(al, loc, val, t));
        } else if (ASR::is_a<ASR::Complex_t>(*t)) {
            double re = ASR::down_cast<ASR::ComplexConstant_t>(trig_arg)->m_re;
            double im = ASR::down_cast<ASR::ComplexConstant_t>(trig_arg)->m_im;
            std::complex<double> x(re, im);
            double result = std::abs(x);
            return ASR::down_cast<ASR::expr_t>(ASR::make_RealConstant_t(al, loc, result, t));
        } else {
            throw SemanticError("Argument of the abs function must be Integer, Real or Complex", loc);
        }
    }

    static ASR::expr_t *eval_dabs(Allocator &al, const Location &loc,
            Vec<ASR::expr_t*> &args
            ) {
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));
        if (args.size() != 1) {
            throw SemanticError("Intrinsic abs function accepts exactly 1 argument", loc);
        }
        ASR::expr_t* trig_arg = args[0];
        ASR::ttype_t* t = ASRUtils::expr_type(args[0]);
        if (ASR::is_a<ASR::Real_t>(*t)) {
            double rv = ASR::down_cast<ASR::RealConstant_t>(trig_arg)->m_r;
            double val = std::abs(rv);
            return ASR::down_cast<ASR::expr_t>(ASR::make_RealConstant_t(al, loc, val, t));
        } else {
            throw SemanticError("Argument of the dabs function must be Real", loc);
        }
    }

    static ASR::expr_t *eval_range(Allocator &al, const Location &loc,
            Vec<ASR::expr_t*> &args
            ) {
        if (args.size() != 1) {
            throw SemanticError("Intrinsic range function accepts exactly 1 argument", loc);
        }
        ASR::ttype_t* t = ASRUtils::expr_type(args[0]);
        int64_t range_val = -1;
        if (ASR::is_a<ASR::Real_t>(*t)) {
            ASR::Real_t* t_real = ASR::down_cast<ASR::Real_t>(t);
            if( t_real->m_kind == 4 ) {
                range_val = 37;
            } else if( t_real->m_kind == 8 ) {
                range_val = 307;
            } else {
                throw SemanticError("Only 32 and 64 bit kinds are supported in range intrinsic.", loc);
            }
        } else if (ASR::is_a<ASR::Integer_t>(*t)) {
            ASR::Integer_t* t_int = ASR::down_cast<ASR::Integer_t>(t);
            if( t_int->m_kind == 4 ) {
                range_val = 9;
            } else if( t_int->m_kind == 8 ) {
                range_val = 18;
            } else if( t_int->m_kind == 1 ) {
                range_val = 2;
            } else if( t_int->m_kind == 2 ) {
                range_val = 4;
            } else {
                throw SemanticError("Only 32, 64, 8 and 16 bit kinds are supported in range intrinsic.", loc);
            }
        } else if (ASR::is_a<ASR::Complex_t>(*t)) {
            ASR::Complex_t* t_complex = ASR::down_cast<ASR::Complex_t>(t);
            if( t_complex->m_kind == 4 ) {
                range_val = 37;
            } else if( t_complex->m_kind == 8 ) {
                range_val = 307;
            } else {
                throw SemanticError("Only 32 and 64 bit kinds are supported in range intrinsic.", loc);
            }
        } else {
            throw SemanticError("Argument of the range function must be Integer, Real or Complex", loc);
        }
        ASR::ttype_t* tmp_int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4, nullptr, 0));
        return ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(al, loc, range_val, tmp_int_type));;
    }

    static ASR::expr_t *eval_aimag(Allocator &al, const Location &loc,
            Vec<ASR::expr_t*> &args
            ) {
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));
        if (args.size() != 1) {
            throw SemanticError("Intrinsic trig function accepts exactly 1 argument", loc);
        }
        ASR::expr_t* trig_arg = args[0];
        ASR::ttype_t* t = ASRUtils::expr_type(args[0]);
        if (ASR::is_a<ASR::Complex_t>(*t)) {
            double im = ASR::down_cast<ASR::ComplexConstant_t>(trig_arg)->m_im;
            double result = im;
            return ASR::down_cast<ASR::expr_t>(ASR::make_RealConstant_t(al, loc, result, t));
        } else {
            throw SemanticError("Argument of the aimag() function must be Complex", loc);
        }
    }

    static ASR::expr_t *eval_int(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        ASR::expr_t* int_expr = args[0];
        if( int_expr->type == ASR::exprType::IntegerBOZ ) {
            ASR::IntegerBOZ_t *boz_expr = ASR::down_cast<ASR::IntegerBOZ_t>(int_expr);
            ASR::ttype_t* tmp_int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4, nullptr, 0));
            return ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(al, loc, boz_expr->m_v, tmp_int_type));;
        }
        ASR::ttype_t* int_type = ASRUtils::expr_type(int_expr);
        int int_kind = ASRUtils::extract_kind_from_ttype_t(int_type);
        if (ASR::is_a<ASR::Integer_t>(*int_type)) {
            if (int_kind == 4){
                int64_t ival = ASR::down_cast<ASR::IntegerConstant_t>(ASRUtils::expr_value(int_expr))->m_n;
                return ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(al, loc, ival, int_type));
            } else {
                int64_t ival = ASR::down_cast<ASR::IntegerConstant_t>(ASRUtils::expr_value(int_expr))->m_n;
                return ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(al, loc, ival, int_type));
            }
        } else if (ASR::is_a<ASR::Real_t>(*int_type)) {
            if (int_kind == 4){
                float rv = ASR::down_cast<ASR::RealConstant_t>(
                    ASRUtils::expr_value(int_expr))->m_r;
                int64_t ival = static_cast<int64_t>(rv);
                return ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(al, loc, ival, int_type));
            } else {
                double rv = ASR::down_cast<ASR::RealConstant_t>(ASRUtils::expr_value(int_expr))->m_r;
                int64_t ival = static_cast<int64_t>(rv);
                return ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(al, loc, ival, int_type));
            }
        } else {
            throw SemanticError("int must have only one argument", loc);
        }
    }

    static ASR::expr_t *not_implemented(Allocator &/*al*/, const Location &/*loc*/, Vec<ASR::expr_t*> &/*args*/) {
        // This intrinsic is not evaluated at compile time yet
        return nullptr;
    }

    static ASR::expr_t *eval_char(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));
        ASR::expr_t* real_expr = args[0];
        ASR::ttype_t* real_type = ASRUtils::expr_type(real_expr);
        if (ASR::is_a<ASR::Integer_t>(*real_type)) {
            int64_t c = ASR::down_cast<ASR::IntegerConstant_t>(real_expr)->m_n;
            if (! (c >= 0 && c <= 127) ) {
                throw SemanticError("The argument 'x' in char(x) must be in the range 0 <= x <= 127.", loc);
            }
            char cc = c;
            std::string svalue;
            svalue += cc;
            Str s;
            s.from_str_view(svalue);
            char *str_val = s.c_str(al);
            // TODO: Should be 0 for char(0) but we store it as 1
            ASR::ttype_t* str_type = ASRUtils::TYPE(ASR::make_Character_t(al,
                loc, 1, 1, nullptr, nullptr, 0));
            return ASR::down_cast<ASR::expr_t>(
                ASR::make_StringConstant_t(al, loc,
                str_val, str_type));
        } else {
            throw SemanticError("char() must have one integer argument", loc);
        }
    }

    static ASR::expr_t *eval_achar(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));
        ASR::expr_t* int_expr = args[0];
        ASR::ttype_t* int_type = ASRUtils::expr_type(int_expr);
        if (ASR::is_a<ASR::Integer_t>(*int_type)) {
            int64_t c = ASR::down_cast<ASR::IntegerConstant_t>(ASRUtils::expr_value(int_expr))->m_n;
            ASR::ttype_t* str_type =
                ASRUtils::TYPE(ASR::make_Character_t(al,
                loc, 1, 1, nullptr, nullptr, 0));
            char cc = c;
            std::string svalue;
            svalue += cc;
            Str s;
            s.from_str_view(svalue);
            char *str_val = s.c_str(al);
            return ASR::down_cast<ASR::expr_t>(
                ASR::make_StringConstant_t(al, loc,
                str_val, str_type));
        } else {
            throw SemanticError("achar() must have one integer argument", loc);
        }
    }

    static ASR::expr_t *eval_iachar(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));
        ASR::expr_t* char_expr = args[0];
        ASR::ttype_t* char_type = ASRUtils::expr_type(char_expr);
        if (ASR::is_a<ASR::Character_t>(*char_type)) {
            char* c = ASR::down_cast<ASR::StringConstant_t>(ASRUtils::expr_value(char_expr))->m_s;
            ASR::ttype_t* int_type =
                ASRUtils::TYPE(ASR::make_Integer_t(al,
                loc, 4, nullptr, 0));
            return ASR::down_cast<ASR::expr_t>(
                ASR::make_IntegerConstant_t(al, loc,
                c[0], int_type));
        } else {
            throw SemanticError("iachar() must have one character argument", loc);
        }
    }

    static ASR::expr_t *eval_epsilon(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        LCOMPILERS_ASSERT(args.size() == 1);
        ASR::ttype_t* t = ASRUtils::expr_type(args[0]);
        if (!ASR::is_a<ASR::Real_t>(*t)) {
            throw SemanticError("Only inputs of real type are accepted in epsilon intrinsic.", loc);
        }
        ASR::Real_t* t_real = ASR::down_cast<ASR::Real_t>(t);
        if( t_real->m_kind == 4 ) {
            float epsilon_val = std::numeric_limits<float>::epsilon();
            return ASR::down_cast<ASR::expr_t>(
                    ASR::make_RealConstant_t(al, loc, epsilon_val, t));
        } else if( t_real->m_kind == 8 ) {
            double epsilon_val = std::numeric_limits<double>::epsilon();
            return ASR::down_cast<ASR::expr_t>(
                    ASR::make_RealConstant_t(al, loc, epsilon_val, t));
        } else {
            throw SemanticError("Only 32 and 64 bit kinds are supported in epsilon intrinsic.", loc);
        }
        return nullptr;
    }

    static ASR::expr_t *eval_new_line(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        LCOMPILERS_ASSERT(args.size() == 1);
        char* new_line_str = (char*)"\n";
        return ASR::down_cast<ASR::expr_t>(ASR::make_StringConstant_t(
                    al, loc, new_line_str,
                    ASRUtils::expr_type(args[0])));
    }

    static ASR::expr_t *eval_selected_int_kind(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));
        ASR::expr_t* real_expr = args[0];
        ASR::ttype_t* real_type = ASRUtils::expr_type(real_expr);
        if (ASR::is_a<ASR::Integer_t>(*real_type)) {
            int64_t R = ASR::down_cast<ASR::IntegerConstant_t>(
                ASRUtils::expr_value(real_expr))->m_n;
            int a_kind = 4;
            if (R < 10) {
                a_kind = 4;
            } else {
                a_kind = 8;
            }
            ASR::ttype_t *type = ASRUtils::TYPE(
                    ASR::make_Integer_t(al, loc,
                        4, nullptr, 0));
            return ASR::down_cast<ASR::expr_t>(
                ASR::make_IntegerConstant_t(al, loc,
                a_kind, type));
        } else {
            throw SemanticError("integer_int_kind() must have one integer argument", loc);
        }
    }
    static ASR::expr_t *eval_selected_real_kind(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));
        // TODO: Be more standards compliant 16.9.170
        // e.g. selected_real_kind(6, 70)
        ASR::expr_t* real_expr = args[0];
        ASR::ttype_t* real_type = ASRUtils::expr_type(real_expr);
        if (ASR::is_a<ASR::Integer_t>(*real_type)) {
            int64_t R = ASR::down_cast<ASR::IntegerConstant_t>(
                ASRUtils::expr_value(real_expr))->m_n;
            int a_kind = 4;
            if (R < 7) {
                a_kind = 4;
            } else {
                a_kind = 8;
            }
            ASR::ttype_t *type = ASRUtils::TYPE(
                    ASR::make_Integer_t(al, loc,
                        4, nullptr, 0));
            return ASR::down_cast<ASR::expr_t>(
                ASR::make_IntegerConstant_t(al, loc,
                a_kind, type));
        } else {
            throw SemanticError("integer_real_kind() must have one integer argument", loc);
        }
    }
    static ASR::expr_t *eval_selected_char_kind(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));
        ASR::expr_t* real_expr = args[0];
        ASR::ttype_t* real_type = ASRUtils::expr_type(real_expr);
        if (ASR::is_a<ASR::Character_t>(*real_type)) {
            ASR::ttype_t *type = ASRUtils::TYPE(
                    ASR::make_Integer_t(al, loc,
                        4, nullptr, 0));
            return ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(al, loc,
                    1, type));
        } else {
            throw SemanticError("integer_char_kind() must have one character argument", loc);
        }
    }

    static ASR::expr_t *eval_ibclr(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));
        if (args.size() != 2) {
            throw SemanticError("The ibclr intrinsic function accepts exactly 2 arguments", loc);
        }
        ASR::expr_t* arg1 = args[0];
        ASR::expr_t* arg2 = args[1];
        ASR::ttype_t* t1 = ASRUtils::expr_type(arg1);
        ASR::ttype_t* t2 = ASRUtils::expr_type(arg2);
        if (ASR::is_a<ASR::Integer_t>(*t1) && ASR::is_a<ASR::Integer_t>(*t2)) {
            int pos = ASR::down_cast<ASR::IntegerConstant_t>(arg2)->m_n;
            int t1_kind = ASRUtils::extract_kind_from_ttype_t(t1);
            if (t1_kind == 4 && pos >= 0 && pos < 32) {
                int32_t i = ASR::down_cast<ASR::IntegerConstant_t>(arg1)->m_n;
                int32_t val = IntrinsicProcedures::lfortran_ibclr32(i, pos);
                ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, t1_kind, nullptr, 0));
                return ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(al, loc, val, type));
            } else if (t1_kind == 8 && pos >= 0 && pos < 64) {
                int64_t i = ASR::down_cast<ASR::IntegerConstant_t>(arg1)->m_n;
                int64_t val = IntrinsicProcedures::lfortran_ibclr64(i, pos);
                ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, t1_kind, nullptr, 0));
                return ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(al, loc, val, type));
            } else {
                throw SemanticError("ibclr(i, pos) for pos < 0 or pos >= bit_size(i) is not allowed", loc);
            }
        } else {
            throw SemanticError("Arguments for this intrinsic function must be Integer", loc);
        }
    }

    static ASR::expr_t *eval_ibset(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));
        if (args.size() != 2) {
            throw SemanticError("The ibset intrinsic function accepts exactly 2 arguments", loc);
        }
        ASR::expr_t* arg1 = args[0];
        ASR::expr_t* arg2 = args[1];
        ASR::ttype_t* t1 = ASRUtils::expr_type(arg1);
        ASR::ttype_t* t2 = ASRUtils::expr_type(arg2);
        if (ASR::is_a<ASR::Integer_t>(*t1) && ASR::is_a<ASR::Integer_t>(*t2)) {
            int pos = ASR::down_cast<ASR::IntegerConstant_t>(arg2)->m_n;
            int t1_kind = ASRUtils::extract_kind_from_ttype_t(t1);
            if (t1_kind == 4 && pos >= 0 && pos < 32) {
                int32_t i = ASR::down_cast<ASR::IntegerConstant_t>(arg1)->m_n;
                int32_t val = IntrinsicProcedures::lfortran_ibset32(i, pos);
                ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, t1_kind, nullptr, 0));
                return ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(al, loc, val, type));
            } else if (t1_kind == 8 && pos >= 0 && pos < 64) {
                int64_t i = ASR::down_cast<ASR::IntegerConstant_t>(arg1)->m_n;
                int64_t val = IntrinsicProcedures::lfortran_ibset64(i, pos);
                ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, t1_kind, nullptr, 0));
                return ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(al, loc, val, type));
            } else {
                throw SemanticError("ibset(i, pos) for pos < 0 or pos >= bit_size(i) is not allowed", loc);
            }
        } else {
            throw SemanticError("Arguments for this intrinsic function must be Integer", loc);
        }
    }

    static ASR::expr_t *eval_ieor(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));
        if (args.size() != 2) {
            throw SemanticError("The ieor intrinsic function accepts exactly 2 arguments", loc);
        }
        ASR::expr_t* arg1 = args[0];
        ASR::expr_t* arg2 = args[1];
        ASR::ttype_t* t1 = ASRUtils::expr_type(arg1);
        ASR::ttype_t* t2 = ASRUtils::expr_type(arg2);
        if (ASR::is_a<ASR::Integer_t>(*t1) && ASR::is_a<ASR::Integer_t>(*t2)) {
            int t1_kind = ASRUtils::extract_kind_from_ttype_t(t1);
            int t2_kind = ASRUtils::extract_kind_from_ttype_t(t2);
            if (t1_kind == 4 && t2_kind == 4) {
                int32_t x = ASR::down_cast<ASR::IntegerConstant_t>(arg1)->m_n;
                int32_t y = ASR::down_cast<ASR::IntegerConstant_t>(arg2)->m_n;
                int32_t val = IntrinsicProcedures::lfortran_ieor32(x, y);
                ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, t1_kind, nullptr, 0));
                return ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(al, loc, val, type));
            } else if (t1_kind == 8 && t2_kind == 8) {
                int64_t x = ASR::down_cast<ASR::IntegerConstant_t>(arg1)->m_n;
                int64_t y = ASR::down_cast<ASR::IntegerConstant_t>(arg2)->m_n;
                int64_t val = IntrinsicProcedures::lfortran_ieor64(x, y);
                ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, t1_kind, nullptr, 0));
                return ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(al, loc, val, type));
            } else {
                throw SemanticError("ieor(x, y): x and y should have the same kind type", loc);
            }
        } else {
            throw SemanticError("Arguments for this intrinsic function must be Integer", loc);
        }
    }

    static int32_t lfortran_ibclr32(int32_t i, int pos) {
        return i & ~(1 << pos);
    }

    static int64_t lfortran_ibclr64(int64_t i, int pos) {
        return i & ~(1LL << pos);
    }

    static int32_t lfortran_ibset32(int32_t i, int pos) {
        return i | (1 << pos);
    }

    static int64_t lfortran_ibset64(int64_t i, int pos) {
        return i | (1LL << pos);
    }

    static int32_t lfortran_ieor32(int32_t x, int32_t y) {
        return x ^ y;
    }

    static int64_t lfortran_ieor64(int64_t x, int64_t y) {
        return x ^ y;
    }

    static ASR::expr_t *eval_huge(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args) {
        ASR::ttype_t* huge_type = ASRUtils::expr_type(args[0]);
        // TODO: Arrays are a valid argument for huge
        if (ASRUtils::is_array(huge_type)) {
            throw SemanticError("Array values not implemented yet", loc);
        }
        if (ASR::is_a<ASR::Integer_t>(*huge_type)) {
            int kind = ASRUtils::extract_kind_from_ttype_t(huge_type);
            if (kind == 4) {
                int32_t max_val = std::numeric_limits<int>::max();
                return ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(al, loc, max_val, huge_type));
            } else if (kind == 8) {
                int64_t max_val = std::numeric_limits<int64_t>::max();
                return ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(al, loc, max_val, huge_type));
            } else {
                throw SemanticError("Only int32, int64 kind is supported", loc);
            }
        } else if (ASR::is_a<ASR::Real_t>(*huge_type)) {
            // TODO: Figure out how to deal with higher precision later
            int kind = ASRUtils::extract_kind_from_ttype_t(huge_type);
            if (kind == 4) {
                float max_val = std::numeric_limits<float>::max();
                return ASR::down_cast<ASR::expr_t>(
                    ASR::make_RealConstant_t(al, loc, max_val, huge_type)
                );
            } else {
                double max_val = std::numeric_limits<double>::max();
                return ASR::down_cast<ASR::expr_t>(
                    ASR::make_RealConstant_t(al, loc, max_val, huge_type)
                );
            }
        } else {
            throw SemanticError("Argument for huge() must be Real or Integer", loc);
        }
    }

    static ASR::expr_t *eval_ishft(Allocator &al,
            const Location &loc, Vec<ASR::expr_t*> &args) {
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));
        if (args.size() != 2) {
            throw SemanticError(
                "The ishft intrinsic function accepts exactly 2 arguments", loc
            );
        }
        ASR::expr_t* i = args[0];
        ASR::expr_t* shift = args[1];
        ASR::ttype_t* t1 = ASRUtils::expr_type(i);
        ASR::ttype_t* t2 = ASRUtils::expr_type(shift);
        if (ASR::is_a<ASR::Integer_t>(*t1) &&
                ASR::is_a<ASR::Integer_t>(*t2)) {
            int t1_kind = ASRUtils::extract_kind_from_ttype_t(t1);
            int t2_kind = ASRUtils::extract_kind_from_ttype_t(t2);
            if (t1_kind == 4 && t2_kind == 4) {
                int32_t x = ASR::down_cast<ASR::IntegerConstant_t>(i)->m_n;
                int32_t y = 0;
                if (shift->type == ASR::exprType::IntegerConstant) {
                    y = ASR::down_cast<ASR::IntegerConstant_t>(shift)->m_n;
                } else if(shift->type == ASR::exprType::IntegerUnaryMinus) {
                    ASR::IntegerUnaryMinus_t *u = ASR::down_cast<ASR::IntegerUnaryMinus_t>(shift);
                    y = - ASR::down_cast<ASR::IntegerConstant_t>(u->m_arg)->m_n;
                }
                if(abs(y) <= 31) {
                    int32_t val;
                    if(y > 0) {
                        val = x << y;
                    } else if(y < 0) {
                        val = x >> abs(y);
                    } else {
                        val = x;
                    }
                    ASR::ttype_t *type = ASRUtils::TYPE(
                        ASR::make_Integer_t(al, loc, t1_kind, nullptr, 0)
                    );
                    return ASR::down_cast<ASR::expr_t>(
                        ASR::make_IntegerConstant_t(al, loc, val, type)
                    );
                } else {
                    throw SemanticError(
                        "shift must be less than 32", loc
                    );
                }
            } else if (t1_kind == 8 && t2_kind == 8) {
                int64_t x = ASR::down_cast<ASR::IntegerConstant_t>(i)->m_n;
                int64_t y = 0;
                if (shift->type == ASR::exprType::IntegerConstant) {
                    y = ASR::down_cast<ASR::IntegerConstant_t>(shift)->m_n;
                } else if(shift->type == ASR::exprType::IntegerUnaryMinus) {
                    ASR::IntegerUnaryMinus_t *u = ASR::down_cast<ASR::IntegerUnaryMinus_t>(shift);
                    y = - ASR::down_cast<ASR::IntegerConstant_t>(u->m_arg)->m_n;
                }
                if(abs(y) <= 63) {
                    int64_t val;
                    if(y > 0) {
                        val = x << y;
                    } else if(y < 0) {
                        val = x >> abs(y);
                    } else {
                        val = x;
                    }
                    ASR::ttype_t *type = ASRUtils::TYPE(
                        ASR::make_Integer_t(al, loc, t1_kind, nullptr, 0)
                    );
                    return ASR::down_cast<ASR::expr_t>(
                        ASR::make_IntegerConstant_t(al, loc, val, type)
                    );
                } else {
                    throw SemanticError(
                        "shift must be less than 64", loc
                    );
                }
            } else {
                throw SemanticError(
                    "ishft(x, y): x and y should have the same kind type", loc
                );
            }
        } else {
            throw SemanticError(
                "Arguments for this intrinsic function must be an Integer", loc
            );
        }
    }

}; // ComptimeEval

} // namespace LCompilers::LFortran

#endif /* LFORTRAN_SEMANTICS_AST_COMPTIME_EVAL_H */
