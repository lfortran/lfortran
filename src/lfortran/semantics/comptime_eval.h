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
                "transpose", "transfer", "cmplx", "dcmplx", "reshape",
                "iachar", "null", "associated", "len", "complex", "is_contiguous",

                // LF specific
                "_lfortran_len", "_lfortran_get_item", "_lfortran_concat",
                "_lfortran_list_constant", "_lfortran_list_count",
                "_lfortran_set_constant",
                "_lfortran_dict_constant",
                "_lfortran_tuple_constant"};

            kind_based_intrinsics = {};
        }

        bool is_intrinsic_present_in_ASR(std::string& name) {
            return intrinsics_present_in_ASR.find(name) != intrinsics_present_in_ASR.end();
        }

        bool is_kind_based_selection_required(std::string& name) {
            return kind_based_intrinsics.find(name) != kind_based_intrinsics.end();
        }

};

struct IntrinsicProcedures {
    const std::string m_builtin = "lfortran_intrinsic_builtin";
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
            {"newunit", {m_custom, &not_implemented, false}},

            // IEEE Arithmetic
            {"ieee_value", {m_ieee_arithmetic, &not_implemented, false}},
            {"ieee_is_nan", {m_ieee_arithmetic, &not_implemented, false}},
            {"ieee_is_finite", {m_ieee_arithmetic, &not_implemented, false}},
            {"ieee_is_negative", {m_ieee_arithmetic, &not_implemented, false}},
            {"ieee_copy_sign", {m_ieee_arithmetic, &not_implemented, false}},
            {"ieee_support_datatype", {m_ieee_arithmetic, &not_implemented, false}},
            {"ieee_is_normal", {m_ieee_arithmetic, &not_implemented, false}},
            {"ieee_unordered", {m_ieee_arithmetic, &not_implemented, false}},
            {"ieee_logb", {m_ieee_arithmetic, &not_implemented, false}},
            {"ieee_rem", {m_ieee_arithmetic, &not_implemented, false}},
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

    std::string get_module(std::string name, const Location &loc, diag::Diagnostics &diag) const {
        auto search = comptime_eval_map.find(name);
        if (search != comptime_eval_map.end()) {
            std::string module_name = std::get<0>(search->second);
            return module_name;
        } else {
            diag.add(diag::Diagnostic(
                "Function '" + name
                + "' not found among intrinsic procedures",
                diag::Level::Error, diag::Stage::Semantic, {
                    diag::Label("", {loc})}));
            throw SemanticAbort();
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
            return nullptr;
        }
    }

    typedef double (*trig_eval_callback_double)(double);
    typedef std::complex<double> (*trig_eval_callback_complex_double)(std::complex<double>);
    static ASR::expr_t *eval_trig(Allocator &al, const Location &loc,
            Vec<ASR::expr_t*> &args, const CompilerOptions &,
            trig_eval_callback_double trig_double,
            trig_eval_callback_complex_double trig_complex_double,
            diag::Diagnostics &diag) {
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));
        if (args.size() != 1) {
            diag.add(diag::Diagnostic(
                "Intrinsic trig function accepts exactly 1 argument",
                diag::Level::Error, diag::Stage::Semantic, {
                    diag::Label("", {loc})}));
            throw SemanticAbort();
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
            diag.add(diag::Diagnostic(
                "Argument for trig function must be Real or Complex",
                diag::Level::Error, diag::Stage::Semantic, {
                    diag::Label("", {loc})}));
            throw SemanticAbort();
        }
    }

    typedef double (*eval2_callback_double)(double, double);
    static ASR::expr_t *eval_2args(Allocator &al, const Location &loc,
            Vec<ASR::expr_t*> &args, const CompilerOptions &,
            eval2_callback_double eval2_double, diag::Diagnostics &diag) {
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));
        if (args.size() != 2) {
            diag.add(diag::Diagnostic(
                "This intrinsic function accepts exactly 2 arguments",
                diag::Level::Error, diag::Stage::Semantic, {
                    diag::Label("", {loc})}));
            throw SemanticAbort();
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
            diag.add(diag::Diagnostic(
                "Arguments for this intrinsic function must be Real",
                diag::Level::Error, diag::Stage::Semantic, {
                    diag::Label("", {loc})}));
            throw SemanticAbort();
        }
    }

    typedef int64_t (*eval2_callback_int)(int64_t, int64_t);
    static ASR::expr_t *eval_2args_ri(Allocator &al, const Location &loc,
            Vec<ASR::expr_t*> &args, const CompilerOptions &compiler_options,
            eval2_callback_double eval2_double,
            eval2_callback_int eval2_int, diag::Diagnostics &diag) {
        LCOMPILERS_ASSERT(ASRUtils::all_args_evaluated(args));
        if (args.size() != 2) {
            diag.add(diag::Diagnostic(
                "This intrinsic function accepts exactly 2 arguments",
                diag::Level::Error, diag::Stage::Semantic, {
                    diag::Label("", {loc})}));
            throw SemanticAbort();
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
            diag.add(diag::Diagnostic(
                "Arguments for this intrinsic function must be Real or Integer",
                diag::Level::Error, diag::Stage::Semantic, {
                    diag::Label("", {loc})}));
            throw SemanticAbort();
        }
    }

    static ASR::expr_t *not_implemented(Allocator &/*al*/, const Location &/*loc*/, Vec<ASR::expr_t*> &/*args*/, const CompilerOptions &/*compiler_options*/) {
        // This intrinsic is not evaluated at compile time yet
        return nullptr;
    }

}; // ComptimeEval

} // namespace LCompilers::LFortran

#endif /* LFORTRAN_SEMANTICS_AST_COMPTIME_EVAL_H */
