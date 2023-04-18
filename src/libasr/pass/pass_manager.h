#ifndef LCOMPILERS_PASS_MANAGER_H
#define LCOMPILERS_PASS_MANAGER_H

#include <libasr/asr.h>
#include <libasr/string_utils.h>
#include <libasr/alloc.h>

// TODO: Remove lpython/lfortran includes, make it compiler agnostic
#if __has_include(<lfortran/utils.h>)
    #include <lfortran/utils.h>
#endif

#if __has_include(<lpython/utils.h>)
    #include <lpython/utils.h>
#endif

#include <libasr/pass/do_loops.h>
#include <libasr/pass/for_all.h>
#include <libasr/pass/init_expr.h>
#include <libasr/pass/implied_do_loops.h>
#include <libasr/pass/array_op.h>
#include <libasr/pass/select_case.h>
#include <libasr/pass/global_stmts.h>
#include <libasr/pass/param_to_const.h>
#include <libasr/pass/print_arr.h>
#include <libasr/pass/print_list_tuple.h>
#include <libasr/pass/arr_slice.h>
#include <libasr/pass/flip_sign.h>
#include <libasr/pass/div_to_mul.h>
#include <libasr/pass/intrinsic_function.h>
#include <libasr/pass/fma.h>
#include <libasr/pass/loop_unroll.h>
#include <libasr/pass/sign_from_value.h>
#include <libasr/pass/class_constructor.h>
#include <libasr/pass/unused_functions.h>
#include <libasr/pass/inline_function_calls.h>
#include <libasr/pass/dead_code_removal.h>
#include <libasr/pass/for_all.h>
#include <libasr/pass/init_expr.h>
#include <libasr/pass/select_case.h>
#include <libasr/pass/loop_vectorise.h>
#include <libasr/pass/update_array_dim_intrinsic_calls.h>
#include <libasr/pass/pass_array_by_data.h>
#include <libasr/pass/pass_list_expr.h>
#include <libasr/pass/subroutine_from_function.h>
#include <libasr/pass/transform_optional_argument_functions.h>
#include <libasr/pass/nested_vars.h>
#include <libasr/asr_verify.h>

#include <map>
#include <vector>
#include <algorithm>

namespace LCompilers {

    typedef void (*pass_function)(Allocator&, ASR::TranslationUnit_t&,
                                  const LCompilers::PassOptions&);

    enum PASS {
        do_loops = 0,
        global_stmts = 1,
        implied_do_loops = 2,
        array_op = 3,
        intrinsic_function = 4,
        arr_slice = 5,
        print_arr = 6,
        print_list_tuple = 7,
        class_constructor = 8,
        unused_functions = 9,
        flip_sign = 10,
        div_to_mul = 11,
        fma = 12,
        sign_from_value = 13,
        inline_function_calls = 14,
        loop_unroll = 15,
        dead_code_removal = 16,
        forall = 17,
        select_case = 18,
        loop_vectorise = 19,
        array_dim_intrinsics_update = 20,
        list_expr = 21,
        array_by_data = 22,
        subroutine_from_function = 23,
        transform_optional_argument_functions = 24,
        init_expr = 25,
        nested_vars = 26,
        NO_OF_PASSES = 27
    };

    class PassManager {
        private:

        std::vector<PASS> _passes;
        std::vector<PASS> _with_optimization_passes;
        std::vector<PASS> _user_defined_passes;
        std::vector<PASS> _skip_passes, _c_skip_passes;
        std::map<PASS, pass_function> _passes_db = {
            {PASS::do_loops, &pass_replace_do_loops},
            {PASS::global_stmts, &pass_wrap_global_stmts_into_function},
            {PASS::implied_do_loops, &pass_replace_implied_do_loops},
            {PASS::array_op, &pass_replace_array_op},
            {PASS::intrinsic_function, &pass_replace_intrinsic_function},
            {PASS::arr_slice, &pass_replace_arr_slice},
            {PASS::print_arr, &pass_replace_print_arr},
            {PASS::print_list_tuple, &pass_replace_print_list_tuple},
            {PASS::class_constructor, &pass_replace_class_constructor},
            {PASS::unused_functions, &pass_unused_functions},
            {PASS::flip_sign, &pass_replace_flip_sign},
            {PASS::div_to_mul, &pass_replace_div_to_mul},
            {PASS::fma, &pass_replace_fma},
            {PASS::sign_from_value, &pass_replace_sign_from_value},
            {PASS::inline_function_calls, &pass_inline_function_calls},
            {PASS::loop_unroll, &pass_loop_unroll},
            {PASS::dead_code_removal, &pass_dead_code_removal},
            {PASS::forall, &pass_replace_forall},
            {PASS::select_case, &pass_replace_select_case},
            {PASS::loop_vectorise, &pass_loop_vectorise},
            {PASS::array_dim_intrinsics_update, &pass_update_array_dim_intrinsic_calls},
            {PASS::list_expr, &pass_list_expr},
            {PASS::array_by_data, &pass_array_by_data},
            {PASS::subroutine_from_function, &pass_create_subroutine_from_function},
            {PASS::transform_optional_argument_functions, &pass_transform_optional_argument_functions},
            {PASS::init_expr, &pass_replace_init_expr},
            {PASS::nested_vars, &pass_nested_vars}
        };

        bool is_fast;
        bool apply_default_passes;
        bool c_skip_pass; // This will contain the passes that are to be skipped in C

        void _apply_passes(Allocator& al, ASR::TranslationUnit_t* asr,
                           std::vector<PASS>& passes, PassOptions &pass_options,
                           diag::Diagnostics &diagnostics) {
            if (pass_options.pass_cumulative) {
                int _pass_max_idx = -1, _opt_max_idx = -1;
                for (PASS current_pass: passes) {
                    auto it1 = std::find(_passes.begin(), _passes.end(), current_pass);
                    if (it1 != _passes.end()) {
                        _pass_max_idx = std::max(_pass_max_idx,
                                            (int)(it1 - _passes.begin()));
                    }
                    auto it2 = std::find(_with_optimization_passes.begin(),
                                    _with_optimization_passes.end(), current_pass);
                    if (it2 != _with_optimization_passes.end()) {
                        _opt_max_idx = std::max(_opt_max_idx,
                                            (int)(it2 - _with_optimization_passes.begin()));
                    }
                }
                passes.clear();
                if (_pass_max_idx != -1) {
                    for (int i=0; i<=_pass_max_idx; i++)
                        passes.push_back(_passes[i]);
                }
                if (_opt_max_idx != -1) {
                    for (int i=0; i<=_opt_max_idx; i++)
                        passes.push_back(_with_optimization_passes[i]);
                }
            }
            for (size_t i = 0; i < passes.size(); i++) {
                // TODO: rework the whole pass manager: construct the passes
                // ahead of time (not at the last minute), and remove this much
                // earlier
                // Note: this is not enough for rtlib, we also need to include
                // it

                if (rtlib && passes[i] == PASS::unused_functions) continue;
                if( std::find(_skip_passes.begin(), _skip_passes.end(), passes[i]) != _skip_passes.end())
                    continue;
                if (c_skip_pass && std::find(_c_skip_passes.begin(),
                        _c_skip_passes.end(), passes[i]) != _c_skip_passes.end())
                    continue;
                if (pass_options.verbose) {
                    std::cerr << "ASR Pass starts: '" << p2s(passes[i]) << "'\n";
                }
                _passes_db[passes[i]](al, *asr, pass_options);
            #if defined(WITH_LFORTRAN_ASSERT)
                if (!asr_verify(*asr, true, diagnostics)) {
                    std::cerr << diagnostics.render2();
                    throw LCompilersException("Verify failed");
                };
            #endif
                if (pass_options.verbose) {
                    std::cerr << "ASR Pass ends: '" << p2s(passes[i]) << "'\n";
                }
            }
        }

        public:

        bool rtlib=false;

        std::map<std::string, PASS> s2p = {
            {"do_loops", PASS::do_loops},
            {"global_stmts", PASS::global_stmts},
            {"implied_do_loops", PASS::implied_do_loops},
            {"array_op", PASS::array_op},
            {"intrinsic_function", PASS::intrinsic_function},
            {"arr_slice", PASS::arr_slice},
            {"print_arr", PASS::print_arr},
            {"print_list_tuple", PASS::print_list_tuple},
            {"class_constructor", PASS::class_constructor},
            {"unused_functions", PASS::unused_functions},
            {"flip_sign", PASS::flip_sign},
            {"div_to_mul", PASS::div_to_mul},
            {"fma", PASS::fma},
            {"sign_from_value", PASS::sign_from_value},
            {"inline_function_calls", PASS::inline_function_calls},
            {"loop_unroll", PASS::loop_unroll},
            {"dead_code_removal", PASS::dead_code_removal},
            {"forall", PASS::forall},
            {"select_case", PASS::select_case},
            {"loop_vectorise", PASS::loop_vectorise},
            {"array_dim_intrinsics_update", PASS::array_dim_intrinsics_update},
            {"list_expr", PASS::list_expr},
            {"array_by_data", PASS::array_by_data},
            {"subroutine_from_function", PASS::subroutine_from_function},
            {"transform_optional_argument_functions", PASS::transform_optional_argument_functions},
            {"init_expr", PASS::init_expr},
            {"nested_vars", PASS::nested_vars}
        };

        std::string p2s(PASS p) {
            switch (p) {
                case PASS::do_loops: return "do_loops";
                case PASS::global_stmts: return "global_stmts";
                case PASS::implied_do_loops: return "implied_do_loops";
                case PASS::array_op: return "array_op";
                case PASS::intrinsic_function: return "intrinsic_function";
                case PASS::arr_slice: return "arr_slice";
                case PASS::print_arr: return "print_arr";
                case PASS::print_list_tuple: return "print_list_tuple";
                case PASS::class_constructor: return "class_constructor";
                case PASS::unused_functions: return "unused_functions";
                case PASS::flip_sign: return "flip_sign";
                case PASS::div_to_mul: return "div_to_mul";
                case PASS::fma: return "fma";
                case PASS::sign_from_value: return "sign_from_value";
                case PASS::inline_function_calls: return "inline_function_calls";
                case PASS::loop_unroll: return "loop_unroll";
                case PASS::dead_code_removal: return "dead_code_removal";
                case PASS::forall: return "forall";
                case PASS::select_case: return "select_case";
                case PASS::loop_vectorise: return "loop_vectorise";
                case PASS::array_dim_intrinsics_update: return "array_dim_intrinsics_update";
                case PASS::list_expr: return "list_expr";
                case PASS::array_by_data: return "array_by_data";
                case PASS::subroutine_from_function: return "subroutine_from_function";
                case PASS::transform_optional_argument_functions: return "transform_optional_argument_functions";
                case PASS::init_expr: return "init_expr";
                case PASS::nested_vars: return "nested_vars";
                case PASS::NO_OF_PASSES: return std::to_string(NO_OF_PASSES);
            }
            return "";
        }

        void _parse_pass_arg(std::string& arg, std::vector<PASS>& passes) {
            if (arg == "") return;

            std::string current_pass = "";
            for( size_t i = 0; i < arg.size(); i++ ) {
                char ch = arg[i];
                if (ch != ' ' && ch != ',') {
                    current_pass.push_back(ch);
                }
                if (ch == ',' || i == arg.size() - 1) {
                    current_pass = to_lower(current_pass);
                    if( s2p.find(current_pass) == s2p.end() ||
                        _passes_db.find(s2p[current_pass]) == _passes_db.end() ) {
                        std::cerr << current_pass << " isn't supported yet.";
                        std::cerr << " Only the following passes are supported:- "<<std::endl;
                        for( auto it: _passes_db ) {
                            std::cerr << p2s(it.first) << std::endl;
                        }
                        exit(1);
                    }
                    passes.push_back(s2p[current_pass]);
                    current_pass.clear();
                }
            }
        }

        PassManager(): is_fast{false}, apply_default_passes{false},
            c_skip_pass{false} {
            _passes = {
                PASS::nested_vars,
                PASS::global_stmts,
                PASS::init_expr,
                PASS::class_constructor,
                PASS::implied_do_loops,
                PASS::list_expr,
                PASS::arr_slice,
                PASS::subroutine_from_function,
                PASS::array_op,
                PASS::intrinsic_function,
                PASS::array_by_data,
                PASS::print_arr,
                PASS::print_list_tuple,
                PASS::array_dim_intrinsics_update,
                PASS::do_loops,
                PASS::forall,
                PASS::select_case,
                PASS::inline_function_calls,
                PASS::unused_functions,
                PASS::transform_optional_argument_functions
            };

            _with_optimization_passes = {
                PASS::global_stmts,
                PASS::init_expr,
                PASS::class_constructor,
                PASS::implied_do_loops,
                PASS::array_by_data,
                PASS::arr_slice,
                PASS::subroutine_from_function,
                PASS::array_op,
                PASS::intrinsic_function,
                PASS::print_arr,
                PASS::print_list_tuple,
                PASS::loop_vectorise,
                PASS::loop_unroll,
                PASS::array_dim_intrinsics_update,
                PASS::do_loops,
                PASS::forall,
                PASS::dead_code_removal,
                PASS::select_case,
                PASS::unused_functions,
                PASS::flip_sign,
                PASS::sign_from_value,
                PASS::div_to_mul,
                PASS::fma,
                PASS::transform_optional_argument_functions,
                PASS::inline_function_calls
            };

            // These are re-write passes which are already handled
            // appropriately in C backend.
            _c_skip_passes = {
                PASS::list_expr,
                PASS::print_list_tuple,
                PASS::do_loops,
                PASS::inline_function_calls
            };
            _user_defined_passes.clear();
        }

        void parse_pass_arg(std::string& arg_pass, std::string& skip_pass) {
            _user_defined_passes.clear();
            _skip_passes.clear();
            _parse_pass_arg(arg_pass, _user_defined_passes);
            _parse_pass_arg(skip_pass, _skip_passes);
        }

        void apply_passes(Allocator& al, ASR::TranslationUnit_t* asr,
                          PassOptions& pass_options,
                          diag::Diagnostics &diagnostics) {
            if( !_user_defined_passes.empty() ) {
                pass_options.fast = true;
                _apply_passes(al, asr, _user_defined_passes, pass_options,
                    diagnostics);
            } else if( apply_default_passes ) {
                pass_options.fast = is_fast;
                if( is_fast ) {
                    _apply_passes(al, asr, _with_optimization_passes, pass_options,
                        diagnostics);
                } else {
                    _apply_passes(al, asr, _passes, pass_options, diagnostics);
                }
            }
        }

        void use_optimization_passes() {
            is_fast = true;
        }

        void do_not_use_optimization_passes() {
            is_fast = false;
        }

        void use_default_passes(bool _c_skip_pass=false) {
            apply_default_passes = true;
            c_skip_pass = _c_skip_pass;
        }

        void do_not_use_default_passes() {
            apply_default_passes = false;
        }

    };

}
#endif
