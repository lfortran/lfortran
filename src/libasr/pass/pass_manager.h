#ifndef LCOMPILERS_PASS_MANAGER_H
#define LCOMPILERS_PASS_MANAGER_H

#include <libasr/asr.h>
#include <libasr/string_utils.h>
#include <libasr/alloc.h>
#include <lfortran/utils.h>
#include <libasr/pass/do_loops.h>
#include <libasr/pass/for_all.h>
#include <libasr/pass/implied_do_loops.h>
#include <libasr/pass/array_op.h>
#include <libasr/pass/select_case.h>
#include <libasr/pass/global_stmts.h>
#include <libasr/pass/param_to_const.h>
#include <libasr/pass/nested_vars.h>
#include <libasr/pass/print_arr.h>
#include <libasr/pass/arr_slice.h>
#include <libasr/pass/flip_sign.h>
#include <libasr/pass/div_to_mul.h>
#include <libasr/pass/fma.h>
#include <libasr/pass/loop_unroll.h>
#include <libasr/pass/sign_from_value.h>
#include <libasr/pass/class_constructor.h>
#include <libasr/pass/unused_functions.h>
#include <libasr/pass/inline_function_calls.h>
#include <libasr/pass/dead_code_removal.h>
#include <libasr/pass/for_all.h>
#include <libasr/pass/select_case.h>

#include <map>
#include <vector>

namespace LCompilers {

    enum ASRPass {
        do_loops, global_stmts, implied_do_loops, array_op,
        arr_slice, print_arr, class_constructor, unused_functions,
        flip_sign, div_to_mul, fma, sign_from_value,
        inline_function_calls, loop_unroll, dead_code_removal,
        forall, select_case
    };

    class PassManager {
        private:

        std::vector<ASRPass> _passes;
        std::vector<ASRPass> _with_optimization_passes;
        std::vector<ASRPass> _user_defined_passes;
        std::map<std::string, ASRPass> _passes_db = {
            {"do_loops", ASRPass::do_loops},
            {"global_stmts", ASRPass::global_stmts},
            {"implied_do_loops", ASRPass::implied_do_loops},
            {"array_op", ASRPass::array_op},
            {"arr_slice", ASRPass::arr_slice},
            {"print_arr", ASRPass::print_arr},
            {"class_constructor", ASRPass::class_constructor},
            {"unused_functions", ASRPass::unused_functions},
            {"flip_sign", ASRPass::flip_sign},
            {"div_to_mul", ASRPass::div_to_mul},
            {"fma", ASRPass::fma},
            {"sign_from_value", ASRPass::sign_from_value},
            {"inline_function_calls", ASRPass::inline_function_calls},
            {"loop_unroll", ASRPass::loop_unroll},
            {"dead_code_removal", ASRPass::dead_code_removal},
            {"forall", ASRPass::forall},
            {"select_case", ASRPass::select_case}
        };

        bool is_fast;

        void _apply_passes(Allocator& al, LFortran::ASR::TranslationUnit_t* asr,
                           std::vector<ASRPass>& passes) {
            for (size_t i = 0; i < passes.size(); i++) {
                switch (passes[i]) {
                    case (ASRPass::do_loops) : {
                        LFortran::pass_replace_do_loops(al, *asr);
                        break;
                    }
                    case (ASRPass::global_stmts) : {
                        LFortran::pass_wrap_global_stmts_into_function(al, *asr, "f");
                        break;
                    }
                    case (ASRPass::implied_do_loops) : {
                        LFortran::pass_replace_implied_do_loops(al, *asr, LFortran::get_runtime_library_dir());
                        break;
                    }
                    case (ASRPass::array_op) : {
                        LFortran::pass_replace_array_op(al, *asr, LFortran::get_runtime_library_dir());
                        break;
                    }
                    case (ASRPass::flip_sign) : {
                        LFortran::pass_replace_flip_sign(al, *asr, LFortran::get_runtime_library_dir());
                        break;
                    }
                    case (ASRPass::fma) : {
                        LFortran::pass_replace_fma(al, *asr, LFortran::get_runtime_library_dir());
                        break;
                    }
                    case (ASRPass::loop_unroll) : {
                        LFortran::pass_loop_unroll(al, *asr, LFortran::get_runtime_library_dir());
                        break;
                    }
                    case (ASRPass::inline_function_calls) : {
                        LFortran::pass_inline_function_calls(al, *asr, LFortran::get_runtime_library_dir());
                        break;
                    }
                    case (ASRPass::dead_code_removal) : {
                        LFortran::pass_dead_code_removal(al, *asr, LFortran::get_runtime_library_dir());
                        break;
                    }
                    case (ASRPass::sign_from_value) : {
                        LFortran::pass_replace_sign_from_value(al, *asr, LFortran::get_runtime_library_dir());
                        break;
                    }
                    case (ASRPass::div_to_mul) : {
                        LFortran::pass_replace_div_to_mul(al, *asr, LFortran::get_runtime_library_dir());
                        break;
                    }
                    case (ASRPass::class_constructor) : {
                        LFortran::pass_replace_class_constructor(al, *asr);
                        break;
                    }
                    case (ASRPass::arr_slice) : {
                        LFortran::pass_replace_arr_slice(al, *asr, LFortran::get_runtime_library_dir());
                        break;
                    }
                    case (ASRPass::print_arr) : {
                        LFortran::pass_replace_print_arr(al, *asr, LFortran::get_runtime_library_dir());
                        break;
                    }
                    case (ASRPass::unused_functions) : {
                        LFortran::pass_unused_functions(al, *asr);
                        break;
                    }
                    case (ASRPass::forall) : {
                        LFortran::pass_replace_forall(al, *asr);
                        break ;
                    }
                    case (ASRPass::select_case) : {
                        LFortran::pass_replace_select_case(al, *asr);
                        break;
                    }
                }
            }
        }

        public:

        PassManager(): is_fast{false} {
            _passes = {
                ASRPass::global_stmts,
                ASRPass::class_constructor,
                ASRPass::implied_do_loops,
                ASRPass::arr_slice,
                ASRPass::array_op,
                ASRPass::print_arr,
                ASRPass::do_loops,
                ASRPass::forall,
                ASRPass::select_case,
                ASRPass::unused_functions
            };

            _with_optimization_passes = {
                ASRPass::global_stmts,
                ASRPass::class_constructor,
                ASRPass::implied_do_loops,
                ASRPass::arr_slice,
                ASRPass::array_op,
                ASRPass::print_arr,
                ASRPass::loop_unroll,
                ASRPass::do_loops,
                ASRPass::forall,
                ASRPass::dead_code_removal,
                ASRPass::select_case,
                ASRPass::unused_functions,
                ASRPass::flip_sign,
                ASRPass::sign_from_value,
                ASRPass::div_to_mul,
                ASRPass::fma,
                ASRPass::inline_function_calls
            };

            _user_defined_passes.clear();
        }

        void parse_pass_arg(std::string& arg_pass, bool is_fast_) {
            _user_defined_passes.clear();
            is_fast = is_fast_;
            if (arg_pass != "") {
                return ;
            }

            std::string current_pass = "";
            for( auto ch: arg_pass ) {
                if( ch != ' ' ) {
                    current_pass.push_back(ch);
                    continue ;
                }
                if( ch == ',' ) {
                    current_pass = LFortran::to_lower(current_pass);
                    if( _passes_db.find(current_pass) == _passes_db.end() ) {
                        std::cerr << current_pass << " isn't supported yet.";
                        std::cerr << " Only the following passes are supported:- "<<std::endl;
                        for( auto it: _passes_db ) {
                            std::cerr << it.first << std::endl;
                        }
                        return ;
                    }
                    _user_defined_passes.push_back(_passes_db[current_pass]);
                    current_pass.clear();
                }
            }
        }

        void apply_passes(Allocator& al, LFortran::ASR::TranslationUnit_t* asr) {
            if( !_user_defined_passes.empty() ) {
                _apply_passes(al, asr, _user_defined_passes);
            } else {
                if( is_fast ) {
                    _apply_passes(al, asr, _with_optimization_passes);
                } else {
                    _apply_passes(al, asr, _passes);
                }
            }
        }
    };

}
#endif
