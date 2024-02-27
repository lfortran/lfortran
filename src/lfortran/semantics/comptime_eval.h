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
                "dcmplx", "reshape", "ichar", "iachar", "char", "maxloc",
                "null", "associated", "all", "len", "complex"};

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
    const std::string m_math = "lfortran_intrinsic_math";
    const std::string m_math2 = "lfortran_intrinsic_math2";
    const std::string m_math3 = "lfortran_intrinsic_math3";
    const std::string m_string = "lfortran_intrinsic_string";
    const std::string m_bit = "lfortran_intrinsic_bit";
    const std::string m_ieee_arithmetic = "lfortran_intrinsic_ieee_arithmetic";
    const std::string m_iso_c_binding = "lfortran_intrinsic_iso_c_binding";
    const std::string m_custom = "lfortran_intrinsic_custom";

    /*
        The last parameter is true if the callback accepts evaluated arguments.

        If true, the arguments are first converted to their compile time
        "values". If not possible, nullptr is returned; otherwise the
        callback is called and it always succeeds to evaluate the result at
        compile time.

        If false, the arguments might not be compile time values. The
        callback can return nullptr if it cannot evaluate itself.
    */

    typedef ASR::expr_t* (*comptime_eval_callback)(Allocator &, const Location &, Vec<ASR::expr_t*> &, const CompilerOptions &);
    std::map<std::string, std::tuple<std::string, comptime_eval_callback, bool>> comptime_eval_map;

    IntrinsicProcedures() {
        comptime_eval_map = {
            // Arguments can be evaluated or not
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
            {"newunit", {m_custom, &not_implemented, false}},

            // Require evaluated arguments
            {"modulo", {m_math2, &eval_modulo, true}},
            {"selected_char_kind", {m_kind, &eval_selected_char_kind, true}},

            {"btest", {m_bit, &not_implemented, false}},

            // These will fail if used in symbol table visitor, but will be
            // left unevaluated in body visitor
            {"trim", {m_string, &not_implemented, false}},
            {"len_trim", {m_string, &not_implemented, false}},
            {"adjustl", {m_string, &eval_adjustl, false}},
            {"adjustr", {m_string, &not_implemented, false}},
            {"lgt", {m_string, &not_implemented, false}},
            {"llt", {m_string, &not_implemented, false}},
            {"lge", {m_string, &not_implemented, false}},
            {"lle", {m_string, &not_implemented, false}},
            {"len_adjustl", {m_string, &not_implemented, false}},
            {"new_line", {m_string, &eval_new_line, false}},
            {"scan_kind4", {m_string, &not_implemented, false}},
            {"scan_kind8", {m_string, &not_implemented, false}},
            {"verify_kind4", {m_string, &not_implemented, false}},
            {"verify_kind8", {m_string, &not_implemented, false}},

            // Subroutines
            {"cpu_time", {m_math, &not_implemented, false}},
            {"bit_size", {m_builtin, &eval_bit_size, false}},
            {"mvbits", {m_bit, &not_implemented, true}},
            {"ibits", {m_bit, &not_implemented, true}},
            {"count", {m_bit, &not_implemented, false}},
            {"achar", {m_builtin, &eval_achar, true}},
            {"move_alloc", {m_builtin, &not_implemented, false}},
            {"shape", {m_builtin, &not_implemented, false}},
            {"reshape", {m_builtin, &not_implemented, false}},
            {"present", {m_builtin, &not_implemented, false}},
            {"index", {m_string, &not_implemented, false}},
            {"system_clock", {m_math, &not_implemented, false}},
            {"random_number", {m_math, &not_implemented, false}},
            {"srand", {m_math, &not_implemented, false}},
            {"date_and_time", {m_string, &not_implemented, false}},

            // Transformational function
            {"all", {m_builtin, &not_implemented, false}},

            // IEEE Arithmetic
            {"ieee_value", {m_ieee_arithmetic, &not_implemented, false}},
            {"ieee_is_nan", {m_ieee_arithmetic, &not_implemented, false}},
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

    ASR::expr_t *comptime_eval(std::string name, Allocator &al, const Location &loc, Vec<ASR::call_arg_t>& args, const CompilerOptions &compiler_options) const {
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
                return cb(al, loc, expr_args, compiler_options);
            } else {
                Vec<ASR::expr_t*> expr_args;
                expr_args.reserve(al, args.size());
                for( auto& a: args ) {
                    expr_args.push_back(al, a.m_value);
                }
                return cb(al, loc, expr_args, compiler_options);
            }
        } else {
            throw SemanticError("Intrinsic function '" + name
                + "' compile time evaluation is not implemented yet",
                loc);
        }
    }

    static ASR::expr_t *eval_bit_size(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args, const CompilerOptions &compiler_options) {
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
        ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, compiler_options.po.default_integer_kind));
        return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, bit_size_val, int_type));
    }

    typedef double (*trig_eval_callback_double)(double);
    typedef std::complex<double> (*trig_eval_callback_complex_double)(std::complex<double>);
    static ASR::expr_t *eval_trig(Allocator &al, const Location &loc,
            Vec<ASR::expr_t*> &args, const CompilerOptions &,
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
            Vec<ASR::expr_t*> &args, const CompilerOptions &,
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
            Vec<ASR::expr_t*> &args, const CompilerOptions &compiler_options,
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
                    ASR::make_Integer_t(al, loc, compiler_options.po.default_integer_kind));
            return ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(al, loc, val, type));
        } else {
            throw SemanticError("Arguments for this intrinsic function must be Real or Integer", loc);
        }
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

    static ASR::expr_t *eval_modulo(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args, const CompilerOptions &compiler_options) {
        return eval_2args_ri(al, loc, args, compiler_options,
            &IntrinsicProcedures::lfortran_modulo,
            &IntrinsicProcedures::lfortran_modulo_i);
    }

    static ASR::expr_t *eval_int(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args, const CompilerOptions &compiler_options) {
        ASR::expr_t* int_expr = args[0];
        if( int_expr->type == ASR::exprType::IntegerBOZ ) {
            ASR::IntegerBOZ_t *boz_expr = ASR::down_cast<ASR::IntegerBOZ_t>(int_expr);
            ASR::ttype_t* tmp_int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, compiler_options.po.default_integer_kind));
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

    static ASR::expr_t *not_implemented(Allocator &/*al*/, const Location &/*loc*/, Vec<ASR::expr_t*> &/*args*/, const CompilerOptions &/*compiler_options*/) {
        // This intrinsic is not evaluated at compile time yet
        return nullptr;
    }

    static ASR::expr_t *eval_achar(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args, const CompilerOptions &) {
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));
        ASR::expr_t* int_expr = args[0];
        ASR::ttype_t* int_type = ASRUtils::expr_type(int_expr);
        if (ASR::is_a<ASR::Integer_t>(*int_type)) {
            int64_t c = ASR::down_cast<ASR::IntegerConstant_t>(ASRUtils::expr_value(int_expr))->m_n;
            ASR::ttype_t* str_type =
                ASRUtils::TYPE(ASR::make_Character_t(al, loc, 1, 1, nullptr));
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

    static ASR::expr_t *eval_adjustl(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args, const CompilerOptions &) {
        LCOMPILERS_ASSERT(args.size() == 1);
        ASR::expr_t* args0 = ASRUtils::expr_value(args[0]);
        if( !ASRUtils::is_value_constant(args0) ) {
            return nullptr;
        }
        ASR::StringConstant_t* string_constant = ASR::down_cast<ASR::StringConstant_t>(args0);
        std::string str = string_constant->m_s;
        std::string adjusted_str = "";
        std::string spaces = "";
        size_t i = 0;
        for( ; i < str.size() && str[i] == ' '; i++ ) {
            spaces.push_back(' ');
        }
        for( ; i < str.size(); i++ ) {
            adjusted_str.push_back(str[i]);
        }
        adjusted_str += spaces;

        return ASR::down_cast<ASR::expr_t>(ASR::make_StringConstant_t(
                    al, loc, s2c(al, adjusted_str),
                    ASRUtils::expr_type(args[0])));
    }

    static ASR::expr_t *eval_new_line(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args, const CompilerOptions &) {
        LCOMPILERS_ASSERT(args.size() == 1);
        char* new_line_str = (char*)"\n";
        return ASR::down_cast<ASR::expr_t>(ASR::make_StringConstant_t(
                    al, loc, new_line_str,
                    ASRUtils::expr_type(args[0])));
    }

    static ASR::expr_t *eval_selected_char_kind(Allocator &al, const Location &loc, Vec<ASR::expr_t*> &args, const CompilerOptions &compiler_options) {
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));
        ASR::expr_t* real_expr = args[0];
        ASR::ttype_t* real_type = ASRUtils::expr_type(real_expr);
        if (ASR::is_a<ASR::Character_t>(*real_type)) {
            ASR::ttype_t *type = ASRUtils::TYPE(
                    ASR::make_Integer_t(al, loc, compiler_options.po.default_integer_kind));
            return ASR::down_cast<ASR::expr_t>(ASR::make_IntegerConstant_t(al, loc,
                    1, type));
        } else {
            throw SemanticError("integer_char_kind() must have one character argument", loc);
        }
    }

}; // ComptimeEval

} // namespace LCompilers::LFortran

#endif /* LFORTRAN_SEMANTICS_AST_COMPTIME_EVAL_H */
