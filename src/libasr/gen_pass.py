passes = [
        # name, optional prefix (pass_PREFIX_name())
        ("arr_slice", "replace"),
        ("array_op", "replace"),
        ("class_constructor", "replace"),
        ("dead_code_removal", ""),
        ("div_to_mul", "replace"),
        ("do_loops", "replace"),
        ("flip_sign", "replace"),
        ("fma", "replace"),
        ("for_all", "replace"),
        ("global_stmts", "wrap"),
        ("global_stmts_program", "wrap"),
        ("global_symbols", "wrap"),
        ("implied_do_loops", "replace"),
        ("init_expr", "replace"),
        ("inline_function_calls", ""),
        ("intrinsic_function", "replace"),
        ("loop_unroll", ""),
        ("loop_vectorise", ""),
        ("nested_vars", ""),
        ("param_to_const", "replace"),
        ("pass_array_by_data", ""),
        ("pass_compare", ""),
        ("pass_list_expr", ""),
        ("print_arr", "replace"),
        ("print_list_tuple", "replace"),
        ("print_struct_type", "replace"),
        ("select_case", "replace"),
        ("sign_from_value", "replace"),
        ("subroutine_from_function", "create"),
        ("transform_optional_argument_functions", ""),
        ("unused_functions", ""),
        ("update_array_dim_intrinsic_calls", ""),
        ("where", "replace"),
]



for pass_name, prefix in passes:
    print(f"Processing: {pass_name}")
    name = pass_name
    if name.startswith("pass"):
        name = name[5:]
    name_up = name.upper()
    if prefix != "":
        prefix += "_"
    header = rf"""#ifndef LIBASR_PASS_{name_up}_H
#define LIBASR_PASS_{name_up}_H

#include <libasr/asr.h>
#include <libasr/utils.h>

namespace LCompilers {{

    void pass_{prefix}{name}(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &pass_options);

}} // namespace LCompilers

#endif // LIBASR_PASS_{name_up}_H
"""
    header_filename = f"pass/{pass_name}.h"
    f = open(header_filename, "w")
    f.write(header)
