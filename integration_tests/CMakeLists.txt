cmake_minimum_required(VERSION 3.10 FATAL_ERROR)

project(lfort C Fortran)

if (NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug
        CACHE STRING "Build type (Debug, Release)" FORCE)
endif ()
if (NOT (CMAKE_BUILD_TYPE STREQUAL "Debug" OR
        CMAKE_BUILD_TYPE STREQUAL "Release"))
    message("${CMAKE_BUILD_TYPE}")
    message(FATAL_ERROR "CMAKE_BUILD_TYPE must be one of: Debug, Release (current value: '${CMAKE_BUILD_TYPE}')")
endif ()

set(LFORTRAN_BACKEND no CACHE STRING "Only compile the LFortran subset for the given backend")

enable_testing()

message("\n")
message("Configuration results")
message("---------------------")
message("Fortran compiler: ${CMAKE_Fortran_COMPILER}")
message("C compiler      : ${CMAKE_C_COMPILER}")
message("Build type: ${CMAKE_BUILD_TYPE}")
if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    message("Fortran compiler flags: ${CMAKE_Fortran_FLAGS_DEBUG}")
    message("C compiler flags      : ${CMAKE_C_FLAGS_DEBUG}")
else ()
    message("Fortran compiler flags: ${CMAKE_Fortran_FLAGS_RELEASE}")
    message("C compiler flags      : ${CMAKE_C_FLAGS_RELEASE}")
endif ()
message("Installation prefix: ${CMAKE_INSTALL_PREFIX}")
message("LFORTRAN_BACKEND: ${LFORTRAN_BACKEND}")



macro(RUN)
    set(options FAIL)
    set(oneValueArgs NAME INCLUDE_PATH)
    set(multiValueArgs LABELS EXTRAFILES)
    cmake_parse_arguments(RUN "${options}" "${oneValueArgs}"
                          "${multiValueArgs}" ${ARGN} )
    set(name ${RUN_NAME})
    set(include_path ${RUN_INCLUDE_PATH})
    if (NOT name)
        message(FATAL_ERROR "Must specify the NAME argument")
    endif()
    if (LFORTRAN_BACKEND)
        if (${LFORTRAN_BACKEND} IN_LIST RUN_LABELS)
            # Test is supported by the given LFortran backend
            set(ADD_TEST ON)
        else()
            # Test is not supported by the given LFortran backend
            set(ADD_TEST OFF)
        endif()
    else()
        # GFortran
        if ("gfortran" IN_LIST RUN_LABELS)
            set(ADD_TEST ON)
        else()
            set(ADD_TEST OFF)
        endif()
    endif()

    if (ADD_TEST)
		if (include_path)
			# Only relative include path supported currently for testing
			set(include_path_with_flag -I${CMAKE_CURRENT_SOURCE_DIR}/${include_path})
		else ()
			set(include_path_with_flag "")
		endif()

        if ((LFORTRAN_BACKEND STREQUAL "cpp") OR (LFORTRAN_BACKEND STREQUAL "x86"))
            add_executable(${name} ${name}.f90 ${RUN_EXTRAFILES})
            target_compile_options(${name} PUBLIC ${include_path_with_flag} --backend=${LFORTRAN_BACKEND})
            target_link_options(${name} PUBLIC --backend=${LFORTRAN_BACKEND})
            add_test(${name} ${CURRENT_BINARY_DIR}/${name})
        elseif ((LFORTRAN_BACKEND STREQUAL "wasm"))
            # wasm test
            execute_process(COMMAND lfortran ${include_path_with_flag} --backend=wasm ${CMAKE_CURRENT_SOURCE_DIR}/${name}.f90 -o ${name})

            find_program(WASM_EXEC_RUNTIME node)
            execute_process(COMMAND "${WASM_EXEC_RUNTIME}" --version
                            OUTPUT_VARIABLE WASM_EXEC_VERSION
                            OUTPUT_STRIP_TRAILING_WHITESPACE)
            string(COMPARE GREATER_EQUAL "${WASM_EXEC_VERSION}"
                                    "v16.0.0" IS_NODE_ABOVE_16)

            if (NOT IS_NODE_ABOVE_16)
                message(STATUS "${WASM_EXEC_RUNTIME} version: ${WASM_EXEC_VERSION}")
                set(WASM_EXEC_FLAGS "--experimental-wasm-bigint")
            endif()
            set(WASM_EXEC_FLAGS ${WASM_EXEC_FLAGS} "--experimental-wasi-unstable-preview1")
            add_test(${name} ${WASM_EXEC_RUNTIME} ${WASM_EXEC_FLAGS} ${CURRENT_BINARY_DIR}/${name}.js)
        elseif (LFORTRAN_BACKEND STREQUAL "llvm2")
            add_executable(${name} ${name}.f90 ${RUN_EXTRAFILES})
            target_compile_options(${name} PUBLIC --generate-object-code ${include_path_with_flag})
            target_link_options(${name} PUBLIC  --generate-object-code ${include_path_with_flag})
            add_test(${name} ${CURRENT_BINARY_DIR}/${name})
        elseif (LFORTRAN_BACKEND STREQUAL "llvm_rtlib")
            add_executable(${name} ${name}.f90 ${RUN_EXTRAFILES})
            target_compile_options(${name} PUBLIC --generate-object-code ${include_path_with_flag})
            set_source_files_properties(${name}.f90 PROPERTIES COMPILE_OPTIONS "--rtlib")
            add_test(${name} ${CURRENT_BINARY_DIR}/${name})
        elseif (LFORTRAN_BACKEND STREQUAL "llvmImplicit")
            execute_process(COMMAND lfortran ${include_path_with_flag} --implicit-typing
				--implicit-interface ${CMAKE_CURRENT_SOURCE_DIR}/${name}.f90 -o ${name})
            add_test(${name} ${CURRENT_BINARY_DIR}/${name})
        else ()
            add_executable(${name} ${name}.f90 ${RUN_EXTRAFILES})
            target_compile_options(${name} PUBLIC ${include_path_with_flag})
            add_test(${name} ${CURRENT_BINARY_DIR}/${name})
        endif()

        if (RUN_LABELS)
            set_tests_properties(${name} PROPERTIES LABELS "${RUN_LABELS}")
        endif()

        if (${RUN_FAIL})
            set_tests_properties(${name} PROPERTIES WILL_FAIL TRUE)
        endif()
    endif()
endmacro(RUN)

# The supported test labels are:
#
# gfortran --- compile with GFortran
# llvm     --- compile with LFortran, only generate binary code at the very end
# llvm2    --- compile with LFortran, generate object files
# llvm_rtlib    --- compile with LFortran loading ASR runtime library, generate object files
# cpp      --- compile to C++, compile C++ to binary
# x86      --- compile to x86 binary directly
# wasm     --- compile to WASM binary directly


# GFortran + LFortran LLVM + LFortran C++

RUN(NAME program_cmake_01 LABELS gfortran llvm cpp x86 wasm)
RUN(NAME program_cmake_02 LABELS gfortran llvm cpp x86 wasm)

RUN(NAME error_stop_01 FAIL LABELS gfortran llvm cpp x86 wasm llvm2)
RUN(NAME error_stop_02 FAIL LABELS llvm wasm llvm2)
RUN(NAME stop_01 LABELS llvm wasm llvm2)

RUN(NAME print_01 LABELS gfortran llvm cpp llvm2 wasm)
RUN(NAME print_02 LABELS gfortran llvm llvm2 wasm)
RUN(NAME print_03 LABELS gfortran llvm llvm2 wasm)
RUN(NAME print_arr_01 LABELS gfortran llvm llvm2 wasm)
RUN(NAME print_arr_02 LABELS gfortran llvm llvm2 wasm)

RUN(NAME include_01 LABELS gfortran llvm wasm)
RUN(NAME include_02 LABELS gfortran llvm wasm)
RUN(NAME include_03 LABELS gfortran llvm wasm INCLUDE_PATH include_03)

RUN(NAME cond_01 LABELS gfortran llvm cpp x86 wasm)
RUN(NAME cond_02 LABELS gfortran llvm wasm)

RUN(NAME expr_01 FAIL LABELS gfortran llvm cpp x86 wasm)
RUN(NAME expr_02 LABELS gfortran llvm cpp x86 wasm)
RUN(NAME expr_03 LABELS gfortran llvm cpp x86 wasm)
RUN(NAME expr_04 LABELS gfortran llvm cpp wasm)
RUN(NAME expr_05 LABELS gfortran llvm cpp) # it contains pow, wasm supports only x**2
RUN(NAME expr_06 LABELS gfortran llvm wasm)
RUN(NAME expr_07 LABELS gfortran llvm wasm)
RUN(NAME expr_08 LABELS gfortran llvm wasm)
RUN(NAME expr_09 LABELS gfortran llvm wasm)
RUN(NAME expr_10 LABELS gfortran llvm wasm)
RUN(NAME expr_12 LABELS gfortran llvm) # it uses string compare
RUN(NAME expr_13 LABELS gfortran llvm wasm)

RUN(NAME data_01 LABELS gfortran llvm)
RUN(NAME data_02 LABELS gfortran)
RUN(NAME data_03 LABELS gfortran llvm)
RUN(NAME data_04 LABELS gfortran llvm)
RUN(NAME data_05 LABELS gfortran llvmImplicit)
RUN(NAME minmax_01 LABELS gfortran llvm wasm)
RUN(NAME arithmetic_if_01 LABELS gfortran llvm) # arithmetic tests use goto
RUN(NAME arithmetic_if_02 LABELS gfortran llvm)
RUN(NAME arithmetic_if_03 LABELS gfortran llvm)
RUN(NAME arithmetic_if_04 LABELS gfortran llvm)

RUN(NAME variables_01 LABELS gfortran llvm cpp x86 wasm)
RUN(NAME variables_02 LABELS gfortran llvm cpp x86 wasm)
RUN(NAME variables_03 LABELS gfortran llvm cpp wasm)

RUN(NAME if_01 LABELS gfortran llvm cpp x86 wasm)
RUN(NAME if_02 FAIL LABELS gfortran llvm cpp x86 wasm)
RUN(NAME if_03 FAIL LABELS gfortran llvm cpp x86 wasm)
RUN(NAME if_04 LABELS gfortran llvm cpp x86 wasm)
RUN(NAME if_05 LABELS gfortran llvm cpp wasm)

RUN(NAME while_01 LABELS gfortran llvm cpp x86 wasm)
RUN(NAME while_02 LABELS gfortran llvm cpp wasm)

RUN(NAME doloop_01 LABELS gfortran llvm cpp x86 wasm)
RUN(NAME doloop_02 LABELS gfortran llvm cpp x86 wasm)
RUN(NAME doloop_03 LABELS gfortran llvm cpp wasm)
RUN(NAME doloop_04 LABELS gfortran llvm) # uses goto target
RUN(NAME doloop_05 LABELS gfortran llvm cpp) # uses goto
RUN(NAME doloop_06 LABELS gfortran llvm cpp wasm)
RUN(NAME doloop_07 LABELS gfortran llvm cpp wasm)
RUN(NAME doloop_08 LABELS gfortran llvm cpp wasm)
RUN(NAME doloop_09 LABELS gfortran llvm) # nested subroutine

RUN(NAME cycle_and_exit1 LABELS gfortran llvm)
RUN(NAME cycle_and_exit2 LABELS gfortran llvm)
RUN(NAME cycle_and_exit3 LABELS gfortran llvm)
RUN(NAME cycle_and_exit4 LABELS gfortran llvm)

RUN(NAME goto_01 LABELS gfortran llvm)
RUN(NAME goto_02 LABELS gfortran llvm)
RUN(NAME goto_03 LABELS gfortran llvm)
RUN(NAME goto_04 LABELS gfortran llvm)
RUN(NAME goto_05 LABELS gfortran llvm)

RUN(NAME subroutines_01 LABELS gfortran llvm cpp x86 wasm)
RUN(NAME subroutines_02 LABELS gfortran llvm cpp x86 wasm)
RUN(NAME subroutines_03 LABELS gfortran llvm wasm)
RUN(NAME subroutines_04 LABELS gfortran llvm wasm)
RUN(NAME subroutines_06 LABELS gfortran)
RUN(NAME subroutines_07 LABELS gfortran)
RUN(NAME subroutines_08 LABELS gfortran llvm wasm)
RUN(NAME subroutines_09 LABELS gfortran)

RUN(NAME functions_01 LABELS gfortran llvm cpp x86 wasm)
RUN(NAME functions_02 LABELS gfortran llvm wasm)
RUN(NAME functions_03 LABELS gfortran llvm)
RUN(NAME functions_04 LABELS gfortran llvm)
RUN(NAME functions_05 LABELS gfortran llvm wasm)
RUN(NAME functions_06 LABELS gfortran)
RUN(NAME functions_07 LABELS gfortran llvm)
RUN(NAME functions_08 LABELS gfortran llvm)
RUN(NAME functions_09 LABELS gfortran)
RUN(NAME functions_10 LABELS gfortran)
RUN(NAME functions_11 LABELS gfortran llvm wasm)
RUN(NAME functions_13 LABELS gfortran)
RUN(NAME functions_15 LABELS gfortran llvm)
RUN(NAME functions_16 LABELS gfortran)
RUN(NAME functions_17 LABELS gfortran llvm)


RUN(NAME types_01 LABELS gfortran llvm cpp wasm)
RUN(NAME types_02 LABELS gfortran llvm cpp wasm)
RUN(NAME types_03 LABELS gfortran llvm cpp wasm)
RUN(NAME types_04 LABELS gfortran llvm cpp wasm)
RUN(NAME types_05 LABELS gfortran llvm cpp wasm)
RUN(NAME types_06 LABELS gfortran llvm cpp wasm)

# GFortran + LFortran C++
RUN(NAME doconcurrentloop_01 LABELS gfortran cpp)

RUN(NAME arrays_01 LABELS gfortran cpp llvm wasm)
RUN(NAME arrays_01_size LABELS gfortran llvm wasm)
RUN(NAME arrays_02_size LABELS gfortran llvm wasm)
RUN(NAME arrays_03_size LABELS gfortran llvm)
RUN(NAME matrix_01_transpose LABELS gfortran)
RUN(NAME matrix_02_matmul LABELS gfortran)
RUN(NAME array_01_pack LABELS gfortran)
RUN(NAME array_01_transfer LABELS gfortran)
RUN(NAME array_02_pack LABELS gfortran)
RUN(NAME array_02_transfer LABELS gfortran)
RUN(NAME array_03_transfer LABELS gfortran)
RUN(NAME arrays_01_real LABELS gfortran llvm wasm)
RUN(NAME arrays_01_complex LABELS gfortran llvm wasm)
RUN(NAME arrays_01_logical LABELS gfortran llvm wasm)
RUN(NAME arrays_01_multi_dim LABELS gfortran llvm wasm)
RUN(NAME array_bound_1 LABELS gfortran llvm wasm)
RUN(NAME arrays_op_1 LABELS gfortran llvm wasm)
RUN(NAME arrays_op_2 LABELS gfortran llvm wasm)
RUN(NAME arrays_op_3 LABELS gfortran llvm wasm)
RUN(NAME arrays_op_4 LABELS gfortran llvm)
RUN(NAME arrays_op_5 LABELS gfortran llvm wasm)
RUN(NAME arrays_op_6 LABELS gfortran llvm)
RUN(NAME arrays_op_7 LABELS gfortran llvm wasm)
RUN(NAME arrays_op_8 LABELS gfortran llvm)
RUN(NAME arrays_op_9 LABELS gfortran llvm)
RUN(NAME arrays_op_10 LABELS gfortran llvm wasm)
RUN(NAME arrays_op_11 LABELS gfortran llvm)
RUN(NAME arrays_op_13 LABELS gfortran llvm wasm)
RUN(NAME arrays_op_14 LABELS gfortran llvm wasm)
RUN(NAME arrays_op_15 LABELS gfortran llvm)
RUN(NAME arrays_reshape_14 LABELS gfortran llvm)
RUN(NAME arrays_elemental_15 LABELS gfortran llvm)
RUN(NAME arrays_03_func LABELS gfortran cpp llvm wasm)
RUN(NAME arrays_03_func_pass_arr_dims LABELS gfortran cpp llvm wasm)
RUN(NAME arrays_04_func LABELS gfortran cpp llvm wasm)
RUN(NAME arrays_04_func_pass_arr_dims LABELS gfortran cpp llvm wasm)
RUN(NAME arrays_05 LABELS gfortran llvm cpp wasm)
RUN(NAME arrays_17 LABELS gfortran llvm wasm)
RUN(NAME arrays_18_func LABELS gfortran llvm)
RUN(NAME arrays_19 LABELS gfortran llvm)
RUN(NAME arrays_20 LABELS gfortran llvm)
RUN(NAME arrays_22 LABELS gfortran llvm)
RUN(NAME arrays_23 LABELS gfortran llvm wasm)

# GFortran
RUN(NAME arrays_02 LABELS gfortran)
RUN(NAME arrays_06 LABELS gfortran llvm wasm)
RUN(NAME arrays_07 LABELS gfortran llvm)
RUN(NAME arrays_08_func LABELS gfortran llvm wasm)
RUN(NAME arrays_08_func_pass_arr_dims LABELS gfortran llvm wasm)
RUN(NAME arrays_09 LABELS gfortran)
RUN(NAME arrays_10 LABELS gfortran)
RUN(NAME arrays_11 LABELS gfortran llvm cpp)
RUN(NAME arrays_12 LABELS gfortran) # constant arrays
RUN(NAME arrays_13 LABELS gfortran llvm) # constant arrays
RUN(NAME arrays_14 LABELS gfortran llvm)
RUN(NAME arrays_15 LABELS gfortran llvm)
RUN(NAME arrays_inputs_15 LABELS gfortran llvm) # constant arrays
RUN(NAME arrays_16_func LABELS gfortran llvm)

RUN(NAME arrays_intrin_01 LABELS gfortran) # minval, maxval
RUN(NAME arrays_intrin_02 LABELS gfortran) # all, any
RUN(NAME any_01 LABELS gfortran)
RUN(NAME any_02 LABELS gfortran llvm)
RUN(NAME sum_01 LABELS gfortran llvm)
RUN(NAME sum_02 LABELS gfortran llvm)

RUN(NAME reserved_01 LABELS gfortran llvm wasm)
RUN(NAME reserved_02 LABELS gfortran llvm)
RUN(NAME reserved_03 LABELS gfortran)

RUN(NAME namelist_01 LABELS gfortran)

RUN(NAME format_01 LABELS gfortran)
RUN(NAME format_02 LABELS gfortran)

RUN(NAME submodule_01 LABELS gfortran)
RUN(NAME submodule_02 LABELS gfortran)
RUN(NAME submodule_03 LABELS gfortran)

RUN(NAME floor_01 LABELS gfortran llvm wasm) # floor body
RUN(NAME floor_02 LABELS gfortran llvm wasm) # floor symboltable
RUN(NAME floor_03 LABELS gfortran wasm)

RUN(NAME modulo_01 LABELS gfortran llvm)

RUN(NAME int_01 LABELS gfortran llvm wasm) # int body
RUN(NAME int_02 LABELS gfortran wasm) # int symboltable
RUN(NAME int_03 LABELS gfortran llvm) # large int and prints array

RUN(NAME intrinsics_01 LABELS gfortran) # sqrt, abs, log
RUN(NAME intrinsics_02 LABELS gfortran llvm) # sin
RUN(NAME intrinsics_03 LABELS gfortran llvm) # cos
RUN(NAME intrinsics_04 LABELS gfortran llvm) # tan
RUN(NAME intrinsics_04s LABELS gfortran llvm) # ctan
RUN(NAME intrinsics_05 LABELS gfortran llvm) # hyperbolics
RUN(NAME intrinsics_06 LABELS gfortran llvm) # inverse trignometric
RUN(NAME intrinsics_07 LABELS gfortran wasm) # kind
RUN(NAME intrinsics_08 LABELS gfortran wasm) # tiny symboltable
RUN(NAME intrinsics_09 LABELS gfortran wasm) # tiny body
RUN(NAME intrinsics_10 LABELS gfortran llvm wasm) # real body
RUN(NAME intrinsics_11 LABELS gfortran llvm wasm) # real symboltable
RUN(NAME intrinsics_12 LABELS gfortran llvm wasm) # kind body
RUN(NAME intrinsics_13 LABELS gfortran wasm) # kind symboltable
RUN(NAME intrinsics_14 LABELS gfortran llvm) # selected_real,int_kind body
RUN(NAME intrinsics_15 LABELS gfortran llvm) # real
RUN(NAME intrinsics_16 LABELS gfortran llvm) # aimag
RUN(NAME intrinsics_17 LABELS gfortran llvm) # exp, log, erf
RUN(NAME intrinsics_18 LABELS gfortran llvm)
RUN(NAME intrinsics_18c LABELS gfortran llvm)
RUN(NAME intrinsics_19 LABELS gfortran llvm)
RUN(NAME intrinsics_19c LABELS gfortran llvm)
RUN(NAME intrinsics_20 LABELS gfortran llvm)
RUN(NAME intrinsics_21 LABELS gfortran llvm wasm) # modulo and mod
RUN(NAME intrinsics_22 LABELS gfortran llvm wasm)
RUN(NAME intrinsics_23 LABELS gfortran llvm) # huge
RUN(NAME intrinsics_24 LABELS gfortran llvm) # System_clock
RUN(NAME intrinsics_25 LABELS gfortran llvm) # ishft
RUN(NAME intrinsics_26 LABELS gfortran llvm)
RUN(NAME intrinsics_27 LABELS gfortran)
RUN(NAME intrinsics_28 LABELS gfortran llvm wasm)
RUN(NAME intrinsics_29 LABELS gfortran llvm) # random_number
RUN(NAME intrinsics_30 LABELS gfortran llvm)
RUN(NAME intrinsics_31 LABELS gfortran llvm wasm)
RUN(NAME intrinsics_32 LABELS gfortran)
RUN(NAME intrinsics_33 LABELS gfortran llvm)
RUN(NAME intrinsics_34 LABELS gfortran llvm wasm)
RUN(NAME intrinsics_35 LABELS gfortran)
RUN(NAME intrinsics_36 LABELS gfortran llvm) # adjustl
RUN(NAME intrinsics_37 LABELS gfortran)
RUN(NAME intrinsics_38 LABELS gfortran) # adjustr
RUN(NAME intrinsics_39 LABELS gfortran)
RUN(NAME intrinsics_40 LABELS gfortran)
RUN(NAME intrinsics_41 LABELS gfortran) # command_argument_count
RUN(NAME intrinsics_42 LABELS gfortran) # hypot
RUN(NAME intrinsics_43 LABELS gfortran) # dim
RUN(NAME intrinsics_44 LABELS gfortran) # cshift
RUN(NAME intrinsics_45 LABELS gfortran llvm wasm) # iso_fortran_env
RUN(NAME intrinsics_46 LABELS gfortran llvm) # ichar & iachar
RUN(NAME intrinsics_47 LABELS gfortran llvm) # all
RUN(NAME intrinsics_48 LABELS gfortran llvm)
RUN(NAME intrinsics_49 LABELS gfortran llvm)
RUN(NAME intrinsics_open_close_read_write LABELS gfortran)

RUN(NAME parameter_01 LABELS gfortran)
RUN(NAME parameter_02 LABELS gfortran)
RUN(NAME parameter_03 LABELS gfortran llvm wasm)
RUN(NAME parameter_04 LABELS gfortran llvm wasm) # selected_real,int_kind symboltable
RUN(NAME parameter_05 LABELS gfortran llvm wasm) # Implicit IntegerToReal

RUN(NAME modules_01 LABELS gfortran llvm wasm)
RUN(NAME modules_02 LABELS gfortran llvm wasm)
RUN(NAME modules_03 LABELS gfortran)
RUN(NAME modules_04 LABELS gfortran)
RUN(NAME modules_05 LABELS gfortran)
RUN(NAME modules_06 LABELS gfortran llvm wasm)
RUN(NAME modules_07 LABELS gfortran llvm EXTRAFILES modules_07_module.f90)
RUN(NAME modules_22 LABELS gfortran llvm EXTRAFILES modules_22_module.f90)
RUN(NAME modules_23 LABELS gfortran llvm EXTRAFILES modules_23_module.f90)
RUN(NAME modules_08 LABELS gfortran llvm EXTRAFILES
        modules_08_a.f90 modules_08_b.f90)
RUN(NAME modules_09 LABELS gfortran llvm EXTRAFILES
        modules_09_a.f90 modules_09_b.f90)
RUN(NAME modules_10 LABELS gfortran llvm wasm)
RUN(NAME modules_11 LABELS gfortran llvm)
RUN(NAME modules_12 LABELS gfortran)
RUN(NAME modules_13 LABELS gfortran llvm wasm)
RUN(NAME modules_14 LABELS gfortran llvm EXTRAFILES modules_14_a.f90)
RUN(NAME modules_15 LABELS gfortran llvm EXTRAFILES
        modules_15b.f90 modules_15c.c)
RUN(NAME modules_16 LABELS gfortran llvm EXTRAFILES modules_16b.f90)
RUN(NAME modules_18 LABELS gfortran llvm EXTRAFILES
        modules_18b.f90 modules_15c.c)
RUN(NAME modules_19 LABELS gfortran llvm EXTRAFILES
        modules_19b.f90)
RUN(NAME modules_20 LABELS gfortran llvm EXTRAFILES
        modules_20b.f90)
RUN(NAME modules_21 LABELS gfortran llvm EXTRAFILES
        modules_21b.f90)
RUN(NAME modules_24 LABELS gfortran)
RUN(NAME modules_25 LABELS gfortran EXTRAFILES
        modules_25_module.f90 modules_25_module1.f90)
RUN(NAME modules_27 LABELS gfortran EXTRAFILES
        modules_27_module1.f90 modules_27_module2.f90)
RUN(NAME modules_28 LABELS gfortran EXTRAFILES
        modules_28_module1.f90 modules_28_module2.f90)
RUN(NAME modules_29 LABELS gfortran EXTRAFILES
        modules_29_module1.f90 modules_29_module2.f90 modules_29_module3.f90)
RUN(NAME modules_30 LABELS gfortran EXTRAFILES
        modules_30_module1.f90 modules_30_module2.f90
        modules_30_module3.f90 modules_30_module4.f90)
RUN(NAME modules_31 LABELS gfortran EXTRAFILES
        modules_31_module1.f90 modules_31_module2.f90)
RUN(NAME modules_32 LABELS gfortran)
RUN(NAME modules_33 LABELS gfortran EXTRAFILES
        modules_33_module1.f90 modules_33_module2.f90
        modules_33_module3.f90)
RUN(NAME modules_34 LABELS gfortran EXTRAFILES
        modules_34_module1a.f90 modules_34_module1.f90
        modules_34_module2.f90 modules_34_module3.f90)
RUN(NAME modules_35 LABELS gfortran)
RUN(NAME modules_36 LABELS gfortran)
RUN(NAME modules_37 LABELS gfortran)
RUN(NAME modules_38 LABELS gfortran)
RUN(NAME modules_39 LABELS gfortran EXTRAFILES
        modules_39_module.f90)
RUN(NAME modules_40 LABELS gfortran)
RUN(NAME operator_overloading_05_module3 LABELS gfortran llvm EXTRAFILES
        operator_overloading_05_module1.f90 operator_overloading_05_module2.f90)
RUN(NAME associate_06 LABELS gfortran EXTRAFILES
        associate_06_module.f90)

RUN(NAME bindc1 LABELS gfortran llvm)
RUN(NAME bindc2 LABELS gfortran llvm)
RUN(NAME bindc3 LABELS gfortran llvm)
RUN(NAME bindc4 LABELS gfortran llvm)
RUN(NAME bindc_01 LABELS gfortran llvm EXTRAFILES bindc_01b.f90 bindc_01c.c)
RUN(NAME bindc_02 LABELS gfortran llvm EXTRAFILES bindc_02b.f90 bindc_02c.c)

RUN(NAME case_01 LABELS gfortran llvm cpp)
RUN(NAME case_02 LABELS gfortran llvm cpp)
RUN(NAME case_03 LABELS gfortran llvm cpp)
RUN(NAME case_04 LABELS gfortran)
RUN(NAME case_05 LABELS gfortran llvm cpp)
RUN(NAME case_06 LABELS gfortran llvm cpp)
RUN(NAME case_07 LABELS gfortran llvm)

RUN(NAME select_type_01 LABELS gfortran llvm)
RUN(NAME select_type_02 LABELS gfortran llvm)
RUN(NAME select_type_04 LABELS gfortran
        EXTRAFILES select_type_03_module.f90 select_type_03.f90)
RUN(NAME select_type_05 LABELS gfortran llvm)

RUN(NAME program_02 LABELS gfortran)
RUN(NAME program_03 LABELS gfortran llvm)
RUN(NAME program_04 LABELS gfortran)

RUN(NAME where_01 LABELS gfortran llvm)
RUN(NAME where_02 LABELS gfortran llvm)
RUN(NAME where_03 LABELS gfortran llvm)
RUN(NAME where_04 LABELS gfortran) # TODO: Fix this test #1631
RUN(NAME where_05 LABELS gfortran llvm)

RUN(NAME forallloop_01 LABELS gfortran)

RUN(NAME parsing_01 LABELS gfortran)
RUN(NAME parsing_02 LABELS gfortran)
RUN(NAME parsing_03 LABELS gfortran llvm wasm)

RUN(NAME interface_01 LABELS gfortran llvm)
RUN(NAME interface_02 LABELS gfortran llvm)
RUN(NAME interface_03 LABELS gfortran)
RUN(NAME interface_04 LABELS gfortran llvm)
RUN(NAME interface_05 LABELS gfortran llvm)
RUN(NAME interface_06 LABELS gfortran)
RUN(NAME interface_07 LABELS gfortran llvm)
RUN(NAME interface_09 LABELS gfortran)
RUN(NAME interface_10 LABELS gfortran llvm)
RUN(NAME interface_12a LABELS gfortran llvm EXTRAFILES interface_12b.f90)
RUN(NAME interface_13 LABELS gfortran llvm)

RUN(NAME implicit_interface_01 LABELS gfortran llvmImplicit)
RUN(NAME implicit_interface_02 LABELS gfortran llvmImplicit)
RUN(NAME implicit_interface_03 LABELS gfortran llvmImplicit)
RUN(NAME implicit_interface_04 LABELS gfortran llvmImplicit)
RUN(NAME implicit_interface_05 LABELS gfortran llvmImplicit)
RUN(NAME implicit_interface_06 LABELS gfortran llvmImplicit)
RUN(NAME implicit_interface_07 LABELS gfortran llvmImplicit)
RUN(NAME implicit_interface_08 LABELS gfortran llvmImplicit)
RUN(NAME implicit_interface_09 LABELS gfortran llvmImplicit)
RUN(NAME implicit_interface_10 LABELS gfortran llvmImplicit)
RUN(NAME implicit_interface_11 LABELS gfortran llvmImplicit)
RUN(NAME implicit_interface_12 LABELS gfortran llvmImplicit)
RUN(NAME implicit_interface_13 LABELS gfortran llvmImplicit)
RUN(NAME implicit_interface_14 LABELS gfortran) # ! TODO: fix this test


RUN(NAME generic_name_01 LABELS gfortran llvm)

RUN(NAME operator_overloading_01 LABELS gfortran llvm)
RUN(NAME operator_overloading_02 LABELS gfortran llvm)
RUN(NAME operator_overloading_03 LABELS gfortran llvm)
RUN(NAME operator_overloading_04 LABELS gfortran)

RUN(NAME types_07 LABELS gfortran)
RUN(NAME types_08 LABELS gfortran)
RUN(NAME types_09 LABELS gfortran)
RUN(NAME types_10 LABELS gfortran)
RUN(NAME types_11 LABELS gfortran)
RUN(NAME types_12 LABELS gfortran llvm wasm)
RUN(NAME types_13 LABELS gfortran)
RUN(NAME types_14 LABELS gfortran llvm wasm)
RUN(NAME types_16 LABELS gfortran llvm wasm)
RUN(NAME types_17 LABELS gfortran llvm)
RUN(NAME types_18 LABELS gfortran llvm cpp wasm)
RUN(NAME types_19 LABELS gfortran llvm cpp wasm)

RUN(NAME complex_01 LABELS gfortran llvm wasm)
RUN(NAME complex_02 LABELS gfortran llvm wasm)
RUN(NAME complex_03 LABELS llvm wasm)
RUN(NAME complex_04 LABELS gfortran llvm wasm)
RUN(NAME complex_05 LABELS gfortran llvm wasm)
RUN(NAME complex_06 LABELS gfortran llvm wasm)
RUN(NAME complex_07 LABELS gfortran) # cmplx
RUN(NAME complex_08 LABELS gfortran) # conjg
RUN(NAME complex_09 LABELS gfortran)
RUN(NAME complex_10 LABELS gfortran llvm) # conjg
RUN(NAME complex_sub_test LABELS gfortran llvm wasm)
RUN(NAME complex_mul_test LABELS gfortran llvm wasm)
RUN(NAME complex_div_test LABELS gfortran llvm)
RUN(NAME complex_pow_test LABELS gfortran llvm)

RUN(NAME logical1 LABELS gfortran llvm wasm)
RUN(NAME logical2 LABELS gfortran llvm wasm)
RUN(NAME logical3 LABELS gfortran llvm wasm)
RUN(NAME logical4 LABELS gfortran llvm wasm)

# `reduce` is not supported by GFortran yet:
# RUN(NAME doconcurrentloop_02 LABELS gfortran)

RUN(NAME derived_types_01 LABELS gfortran llvm)
RUN(NAME derived_types_02 LABELS gfortran)
RUN(NAME derived_types_03 LABELS gfortran)
RUN(NAME derived_types_04 LABELS gfortran)
RUN(NAME derived_types_06 LABELS gfortran llvm)
RUN(NAME derived_types_05 LABELS gfortran)
RUN(NAME derived_types_07 LABELS gfortran)
RUN(NAME derived_types_08 LABELS gfortran)
RUN(NAME derived_types_09 LABELS gfortran EXTRAFILES
        derived_types_09b.f90 derived_types_09c.f90)
RUN(NAME derived_types_10 LABELS gfortran llvm)
RUN(NAME derived_types_11 LABELS gfortran llvm)
RUN(NAME derived_types_12 LABELS gfortran llvm)
RUN(NAME derived_types_13 LABELS gfortran llvm)
RUN(NAME derived_types_15 LABELS gfortran)
RUN(NAME derived_types_14 LABELS gfortran
        EXTRAFILES derived_types_14_module.f90)
RUN(NAME derived_types_16_module LABELS gfortran)
RUN(NAME derived_types_18 LABELS gfortran)
RUN(NAME derived_types_19 LABELS gfortran
        EXTRAFILES derived_types_19_module.f90)
RUN(NAME derived_types_20 LABELS gfortran
        EXTRAFILES derived_types_20_module2.f90
                   derived_types_20_module1.f90)

RUN(NAME line_continuation_01 LABELS gfortran llvm wasm)
RUN(NAME line_continuation_02 LABELS gfortran llvm wasm)
RUN(NAME line_continuation_03 LABELS gfortran llvm wasm)

RUN(NAME program_01 LABELS gfortran)
RUN(NAME init_values LABELS gfortran llvm)
RUN(NAME param_pass_01 LABELS gfortran llvm wasm)

RUN(NAME allocate_01 LABELS gfortran llvm)
RUN(NAME allocate_02 LABELS gfortran llvm)
RUN(NAME allocate_03 LABELS gfortran llvm)
RUN(NAME allocate_04 LABELS gfortran llvm)
RUN(NAME allocate_05 LABELS gfortran llvm)
RUN(NAME block_01 LABELS gfortran llvm)
RUN(NAME block_02 LABELS gfortran)
RUN(NAME block_03 LABELS gfortran llvm)
RUN(NAME block_04 LABELS gfortran llvm)
RUN(NAME block_05 LABELS gfortran llvm)
RUN(NAME block_06 LABELS gfortran llvm wasm)
RUN(NAME associate_01 LABELS gfortran llvm)
RUN(NAME associate_02 LABELS gfortran llvm)
RUN(NAME associate_03 LABELS gfortran llvm)
RUN(NAME associate_04 LABELS gfortran llvm)
RUN(NAME associate_05 LABELS gfortran llvm)
RUN(NAME associate_07 LABELS gfortran llvm)
RUN(NAME associate_08 LABELS gfortran)
RUN(NAME associate_09 LABELS gfortran llvm)
RUN(NAME associate_10 LABELS gfortran llvm)

RUN(NAME real_dp LABELS gfortran llvm wasm)
RUN(NAME bin_op_real_dp LABELS gfortran llvm)
RUN(NAME const_real_dp LABELS gfortran llvm wasm)
RUN(NAME real_dp_param LABELS gfortran llvm wasm)
RUN(NAME int_dp LABELS gfortran llvm wasm)
RUN(NAME int_dp_param LABELS gfortran llvm wasm)
RUN(NAME complex_dp LABELS gfortran llvm wasm)
RUN(NAME bin_op_complex_dp LABELS gfortran llvm wasm)
RUN(NAME complex_dp_param LABELS gfortran llvm wasm)
RUN(NAME const_kind_01 LABELS gfortran llvm wasm)
RUN(NAME const_kind_02 LABELS gfortran llvm wasm)
RUN(NAME const_array_01 LABELS gfortran llvm)
RUN(NAME const_array_02 LABELS gfortran llvm)

RUN(NAME string_01 LABELS gfortran llvm cpp wasm)
RUN(NAME string_02 LABELS gfortran llvm wasm)
RUN(NAME string_03 LABELS gfortran llvm)
RUN(NAME string_04 LABELS gfortran llvm)
RUN(NAME string_05 LABELS gfortran llvm)
RUN(NAME string_06 LABELS gfortran llvm)
RUN(NAME string_07 LABELS gfortran llvm)
RUN(NAME string_08 LABELS gfortran llvm)
RUN(NAME string_09 LABELS gfortran llvm)
RUN(NAME string_10 LABELS gfortran llvm)
RUN(NAME string_11 LABELS gfortran llvm)
RUN(NAME string_12 LABELS gfortran)
RUN(NAME string_13 LABELS gfortran llvm)
RUN(NAME string_14 LABELS gfortran)
RUN(NAME string_15 LABELS gfortran) # lge, lgt lle, llt
RUN(NAME string_16 LABELS gfortran) # achar
RUN(NAME string_19 LABELS gfortran) # achar
RUN(NAME string_20 LABELS gfortran llvm)

RUN(NAME nested_01 LABELS gfortran llvm)
RUN(NAME nested_02 LABELS gfortran llvm)
RUN(NAME nested_03 LABELS gfortran llvm)
RUN(NAME nested_04 LABELS gfortran llvm)
RUN(NAME nested_05 LABELS gfortran llvm)
RUN(NAME nested_06 LABELS gfortran llvm)
RUN(NAME nested_07 LABELS gfortran llvm)
RUN(NAME nested_08 LABELS gfortran llvm)
RUN(NAME nested_09 LABELS gfortran llvm)
RUN(NAME nested_10 LABELS gfortran llvm)
RUN(NAME nested_11 LABELS gfortran llvm)

RUN(NAME nested_vars1 LABELS gfortran llvm)
RUN(NAME nested_vars2 LABELS gfortran llvm)

RUN(NAME nbody LABELS gfortran llvm)

RUN(NAME intent_01 LABELS gfortran llvm)

RUN(NAME callback_01 LABELS gfortran llvm)
RUN(NAME callback_02 LABELS gfortran llvm)
RUN(NAME callback_03 LABELS gfortran llvm)
RUN(NAME callback_04 LABELS gfortran llvm)
RUN(NAME callback_05 LABELS gfortran llvm)

RUN(NAME recursion_01 LABELS gfortran llvm)
RUN(NAME recursion_02 LABELS gfortran llvm)
RUN(NAME recursion_03 LABELS gfortran llvm)

RUN(NAME return_01 LABELS gfortran llvm wasm)
RUN(NAME return_02 LABELS gfortran llvm) # contains module
RUN(NAME return_03 LABELS gfortran llvm wasm)
RUN(NAME return_04 LABELS gfortran llvm wasm)

RUN(NAME file_01 LABELS gfortran llvm)

RUN(NAME class_01 LABELS gfortran)
RUN(NAME class_02 LABELS gfortran llvm)
RUN(NAME class_03 LABELS gfortran)
RUN(NAME class_04 LABELS gfortran)
RUN(NAME class_05 LABELS gfortran llvm)

RUN(NAME kwargs_01 LABELS gfortran llvm wasm)
RUN(NAME kwargs_02 LABELS gfortran)

RUN(NAME test_iso_c_binding LABELS gfortran llvm wasm)
RUN(NAME test_iso_fortran_env LABELS gfortran llvm wasm)

RUN(NAME abs_01 LABELS gfortran llvm wasm)
RUN(NAME abs_02 LABELS gfortran llvm wasm)
RUN(NAME abs_03 LABELS gfortran llvm wasm)
RUN(NAME sqrt_01 LABELS gfortran llvm)
RUN(NAME sqrt_02 LABELS gfortran llvm)
RUN(NAME sin_01 LABELS gfortran llvm)
RUN(NAME sin_02 LABELS gfortran llvm)
RUN(NAME sin_03 LABELS gfortran llvm)
RUN(NAME sin_04 LABELS gfortran llvm)
RUN(NAME bits_01 LABELS gfortran llvm)
RUN(NAME bits_02 LABELS gfortran llvm)
RUN(NAME bits_03 LABELS gfortran llvm)
RUN(NAME bits_04 LABELS gfortran llvm)
RUN(NAME bits_05 LABELS gfortran llvm)
RUN(NAME cpu_time_01 LABELS gfortran llvm)
RUN(NAME cpu_time_02_wasm LABELS wasm)
RUN(NAME boz_01 LABELS gfortran llvm)
RUN(NAME cmd_01 LABELS gfortran llvm)
RUN(NAME cmd_02 LABELS gfortran llvm)

RUN(NAME flip_sign LABELS gfortran llvm wasm)
RUN(NAME div_to_mul LABELS gfortran llvm wasm)
RUN(NAME fma LABELS gfortran llvm wasm)
RUN(NAME loop_unroll_small LABELS gfortran llvm wasm)
RUN(NAME loop_unroll_large LABELS gfortran llvm)
RUN(NAME sign_from_value LABELS gfortran llvm wasm)

RUN(NAME rewind_inquire_flush LABELS gfortran)

RUN(NAME assign_to1 LABELS gfortran llvm)
RUN(NAME assign_to2 LABELS gfortran llvm)

RUN(NAME conv_complex2real LABELS gfortran llvm wasm)

RUN(NAME template_add_01 LABELS llvm wasm)
RUN(NAME template_add_02 LABELS llvm wasm)
RUN(NAME template_add_03 LABELS llvm wasm)
RUN(NAME template_nested LABELS llvm wasm)
RUN(NAME template_travel LABELS llvm wasm)
RUN(NAME template_triple LABELS llvm wasm)
RUN(NAME template_array_01 LABELS llvm wasm)
RUN(NAME template_array_01b LABELS llvm wasm)
RUN(NAME template_array_02 LABELS llvm wasm)
RUN(NAME template_array_02b LABELS llvm wasm)
RUN(NAME template_array_03 LABELS llvm wasm)
RUN(NAME template_array_04 LABELS llvm wasm)

RUN(NAME statement1 LABELS gfortran llvm)

RUN(NAME implied_do_loops1 LABELS gfortran)
RUN(NAME implied_do_loops2 LABELS gfortran)
RUN(NAME implied_do_loops3 LABELS gfortran)

RUN(NAME minpack_01 LABELS gfortran llvm_rtlib EXTRAFILES
        minpack_01_module.f90 minpack_01_func.f90)
RUN(NAME minpack_02 LABELS gfortran llvm)

RUN(NAME external_01 LABELS gfortran llvm)

RUN(NAME interface_12 LABELS gfortran llvm)

RUN(NAME private1 LABELS gfortran llvm)

RUN(NAME modules_26 LABELS gfortran llvm)

RUN(NAME enum_01 LABELS gfortran llvm)
RUN(NAME enum_02 LABELS gfortran llvm EXTRAFILES
         enum_02_module.f90)

RUN(NAME array_section_01 LABELS gfortran llvm)

RUN(NAME pass_array_by_data_01 LABELS gfortran llvm wasm)
RUN(NAME pass_array_by_data_02 LABELS gfortran llvm)
RUN(NAME pass_array_by_data_03 LABELS gfortran llvm)
RUN(NAME pass_array_by_data_04 LABELS gfortran llvm)
RUN(NAME pass_array_by_data_05 LABELS gfortran llvm)
RUN(NAME pass_array_by_data_06 LABELS gfortran llvm wasm)

RUN(NAME implicit_deallocate_01 LABELS gfortran llvm)

RUN(NAME write_01 LABELS gfortran llvm)

RUN(NAME do_loop_01 LABELS gfortran llvm)

RUN(NAME array_01 LABELS gfortran llvm)
RUN(NAME array_02 LABELS gfortran llvm wasm)

RUN(NAME do_loop_02 LABELS gfortran llvm)

RUN(NAME block_07 LABELS gfortran llvm)

RUN(NAME complex_11 LABELS gfortran llvmImplicit wasm)
RUN(NAME complex_12 LABELS gfortran llvm wasm)
RUN(NAME complex_13 LABELS gfortran llvm wasm)
RUN(NAME complex_14 LABELS gfortran llvm wasm)

RUN(NAME array_op_01 LABELS gfortran llvm wasm)
RUN(NAME array_op_02 LABELS gfortran llvm wasm)
RUN(NAME array_op_03 LABELS gfortran llvm wasm)
RUN(NAME array_op_04 LABELS gfortran llvm)
RUN(NAME array_slice_01 LABELS gfortran llvm)

RUN(NAME declaration_01 LABELS gfortran llvm)

RUN(NAME procedure_01 LABELS gfortran llvm)
RUN(NAME procedure_02 LABELS gfortran llvm)
RUN(NAME procedure_03 LABELS gfortran llvm)
RUN(NAME procedure_04 LABELS gfortran llvm)

RUN(NAME allocated_01 LABELS gfortran llvm)
RUN(NAME allocated_02 LABELS gfortran llvm)
RUN(NAME allocated_03 LABELS gfortran llvm)
