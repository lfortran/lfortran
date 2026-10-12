# Stable feature coverage for secondary PR configurations. Keep broad suites
# on the primary Linux lane and on every main push; do not sample randomly.
set(LFORTRAN_SMOKE_TESTS
    # Expressions, control flow, kinds, intrinsics and expected failures.
    program_cmake_01 program_cmake_02
    error_stop_01 error_stop_02 stop_01 stop_02
    expr_02 expr_03 expr_04 expr_05 expr_06 expr_07 expr_08 expr_21
    doloop_01 doloop_02 doloop_03 doloop_09 doloop_11
    while_01 while_02 goto_01 select_case_01 select_case_02
    intrinsics_01 intrinsics_02 intrinsics_03 intrinsics_04 intrinsics_04s
    abs_01 complex_01 complex_02 complex_03 complex_07
    enum_01 enum_02 enum_07

    # Arrays: descriptors, sections, bounds, constructors and reductions.
    arrays_01 arrays_01_size arrays_02_size arrays_03_size
    arrays_01_real arrays_01_complex arrays_01_logical arrays_01_multi_dim
    arrays_op_1 arrays_op_2 arrays_op_3 arrays_op_4
    arrays_op_20
    arrays_28 arrays_30
    array_01_pack array_02_pack array_01_transfer array_02_transfer
    array_bound_1 array_bound_2 array_bound_3
    array_constructor_02 array_constructor_03
    array_section_27 matmul_01 matmul_02
    sum_01 any_01 where_01 where_02
    assumed_rank_01 assumed_rank_02 select_rank_01 select_rank_02

    # Allocation, pointers, derived types, polymorphism and finalization.
    allocate_01 allocate_02 allocate_03 allocate_04 allocate_05
    deallocate_01 deallocate_02 pointer_01 pointer_02 pointer_03 pointer_13
    allocatable_polymorphic_mold_01 allocatable_polymorphic_assign_01
    allocatable_component_struct_array_01 allocatable_lhs_finalization_01
    allocatable_dummy_descriptor_01
    derived_types_01 derived_types_02 derived_types_03 derived_types_04
    derived_types_05 derived_types_06 derived_types_07 derived_types_08
    class_01 class_02 class_03 class_04
    select_type_01 select_type_02
    polymorphic_arguments_01 type_bound_generic_member_access_01
    finalization_01 finalization_02 finalization_03 finalization_04
    associate_01 associate_02 associate_03

    # Calls, modules, C ABI, descriptors, callbacks and separate compilation.
    subroutines_01 subroutines_02 subroutines_03 subroutines_04
    functions_01 functions_02 functions_03 functions_04 functions_07
    modules_01 modules_02 modules_03 modules_04 modules_05 modules_15 modules_51
    interface_01 interface_02 interface_03
    procedure_pointer_array_01 procedure_pointer_12
    bindc_01 bindc_02 bindc_03 bindc_04 bindc_05 bindc_08
    bindc_06 bindc_07 complex_cloc_01
    bindc_iso_fb_01 bindc_iso_fb_02 bindc_iso_fb_03
    c_f_pointer_01 c_f_pointer_02 c_f_proc_ptr_01
    separate_compilation_07 separate_compilation_09
    multi_file_member_access_01 multi_file_member_access_02
    submodule_18a submodule_19a submodule_20a submodule_21a
    submodule_23a submodule_25a submodule_27a submodule_29a
    submodule_30a submodule_34a submodule_37a submodule_38a
    submodule_39a submodule_40a submodule_44a submodule_50a
    submodule_51a submodule_53a operator_overloading_36
    separate_compilation_16 separate_compilation_17 separate_compilation_18
    separate_compilation_19 separate_compilation_20 separate_compilation_21
    separate_compilation_22 separate_compilation_23 separate_compilation_24
    separate_compilation_25 separate_compilation_26 separate_compilation_27

    # Character handling, formatted/unformatted I/O and source forms.
    string_01 string_02 string_03 string_04 string_05 string_06
    string_concat_deferred_len
    character_01 character_02 character_03 character_04
    file_02 file_03 file_04 file_05 file_06
    read_01 read_02 read_03 read_04 write_01 write_02 write_03 write_04
    read_94
    print_01 print_02 print_03 print_04 print_05 print_07 print_09
    print_10 print_11 print_12 print_arr_01 print_arr_02 print_arr_03 print_arr_07
    format_01 format_02 format_56 get_environment_variable_01
    cpp_pre_01 cpp_pre_02 include_01 include_04
    preprocessor_define_equals
    fixed_form_select_case_01 fixed_form_comment_01 fixed_form_module_01

    # Small option-specific backends, legacy interfaces and runtime library.
    minpack_01 simd_01 simd_02 do_concurrent_01
    implicit_interface_01 implicit_interface_02 implicit_interface_03
    implicit_interface_04 implicit_interface_05 implicit_interface_06
    implicit_interface_07 implicit_interface_08 implicit_interface_09
    implicit_interface_10 implicit_interface_11 implicit_interface_12
    implicit_interface_13 implicit_interface_14 implicit_interface_16
    implicit_interface_17 implicit_interface_18
    implicit_typing_01 implicit_typing_02 implicit_typing_03 implicit_typing_04
    data_07 data_11 data_16 block_data_parameter_01 attr_dim_04
    external_01 external_02 external_03 external_04 external_05
    external_06 external_07 external_08 procedure_05
    common_06 common_07 common_08 common_12 common_24
    statement_01 statement_02 statement_03 statement_04 statement_05 statement_07
    save_12 character_17 character_18 dabs_01 capital_01
    legacy_array_sections_04 legacy_array_sections_10
    character_len_of_function_12614
)
