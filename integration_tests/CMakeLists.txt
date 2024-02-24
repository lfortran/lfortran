cmake_minimum_required(VERSION 3.10 FATAL_ERROR)

project(lfort C Fortran)

if (NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug
        CACHE STRING "Build type (Debug, Release)" FORCE)
endif ()

set(LFORTRAN_BACKEND no CACHE STRING "Only compile the LFortran subset for the given backend")
set(FAST no CACHE BOOL "Run supported tests with --fast")

enable_testing()

message("\n")
message("Configuration results")
message("---------------------")
message("Fortran compiler: ${CMAKE_Fortran_COMPILER}")
message("C compiler      : ${CMAKE_C_COMPILER}")
message("Build type: ${CMAKE_BUILD_TYPE}")
string(TOUPPER "${CMAKE_BUILD_TYPE}" BTYPE)
message("Fortran compiler flags: ${CMAKE_Fortran_FLAGS_${BTYPE}}")
message("C compiler flags      : ${CMAKE_C_FLAGS_${BTYPE}}")
message("Installation prefix: ${CMAKE_INSTALL_PREFIX}")
message("LFORTRAN_BACKEND: ${LFORTRAN_BACKEND}")
message("FAST: ${FAST}")


macro(RUN_UTIL RUN_FAIL RUN_NAME RUN_FILE_NAME RUN_LABELS RUN_EXTRAFILES RUN_EXTRA_ARGS RUN_COPY_TO_BIN RUN_GFORTRAN_ARGS)
    set(fail ${${RUN_FAIL}})
    set(name ${${RUN_NAME}})
    set(file_name ${${RUN_FILE_NAME}})
    set(labels ${${RUN_LABELS}})
    set(extra_files ${${RUN_EXTRAFILES}})
    set(extra_args ${${RUN_EXTRA_ARGS}})
    set(copy_to_bin ${${RUN_COPY_TO_BIN}})
    set(gfortran_args ${${RUN_GFORTRAN_ARGS}})

    if (NOT name)
        message(FATAL_ERROR "Must specify the NAME argument")
    endif()

    if (LFORTRAN_BACKEND)
        if (${LFORTRAN_BACKEND} IN_LIST labels)
            # Test is supported by the given LFortran backend
            set(ADD_TEST ON)
        else()
            # Test is not supported by the given LFortran backend
            set(ADD_TEST OFF)
        endif()
    else()
        # GFortran
        if ("gfortran" IN_LIST labels)
            set(ADD_TEST ON)
        elseif ("gfortranImplicitArgument" IN_LIST labels)
            set(ADD_TEST ON)
        else()
            set(ADD_TEST OFF)
        endif()
    endif()

    if (ADD_TEST)
        if ((LFORTRAN_BACKEND STREQUAL "cpp") OR (LFORTRAN_BACKEND STREQUAL "x86")
                OR (LFORTRAN_BACKEND STREQUAL "c") OR (LFORTRAN_BACKEND STREQUAL "fortran"))
            add_executable(${name} ${file_name}.f90 ${extra_files})
            target_compile_options(${name} PUBLIC ${extra_args} --backend=${LFORTRAN_BACKEND})
            target_link_options(${name} PUBLIC --backend=${LFORTRAN_BACKEND})
            add_test(${name} ${CURRENT_BINARY_DIR}/${name})
        elseif (LFORTRAN_BACKEND STREQUAL "wasm")
            # wasm test
            execute_process(COMMAND lfortran ${extra_args} --backend=wasm ${CMAKE_CURRENT_SOURCE_DIR}/${file_name}.f90 -o ${name})

            find_program(WASM_EXEC_RUNTIME node)
            execute_process(COMMAND "${WASM_EXEC_RUNTIME}" --version
                            OUTPUT_VARIABLE WASM_EXEC_VERSION
                            OUTPUT_STRIP_TRAILING_WHITESPACE)

            string(REGEX REPLACE "v([0-9]+)\\.([0-9]+)\\.([0-9]+)" "\\1" NODE_MAJOR_VERSION "${WASM_EXEC_VERSION}")

            if (NODE_MAJOR_VERSION LESS 16)
                message(STATUS "${WASM_EXEC_RUNTIME} version: ${WASM_EXEC_VERSION}")
                set(WASM_EXEC_FLAGS "--experimental-wasm-bigint")
            endif()
            set(WASM_EXEC_FLAGS ${WASM_EXEC_FLAGS} "--experimental-wasi-unstable-preview1")
            add_test(${name} ${WASM_EXEC_RUNTIME} ${WASM_EXEC_FLAGS} ${CURRENT_BINARY_DIR}/${name}.js)
        elseif (LFORTRAN_BACKEND STREQUAL "llvm2")
            add_executable(${name} ${file_name}.f90 ${extra_files})
            target_compile_options(${name} PUBLIC --generate-object-code ${extra_args})
            target_link_options(${name} PUBLIC  --generate-object-code ${extra_args})
            add_test(${name} ${CURRENT_BINARY_DIR}/${name})
        elseif (LFORTRAN_BACKEND STREQUAL "llvm_rtlib")
            add_executable(${name} ${file_name}.f90 ${extra_files})
            target_compile_options(${name} PUBLIC --generate-object-code ${extra_args})
            set_source_files_properties(${file_name}.f90 PROPERTIES COMPILE_OPTIONS "--rtlib")
            add_test(${name} ${CURRENT_BINARY_DIR}/${name})
        elseif (LFORTRAN_BACKEND STREQUAL "llvmImplicit")
            execute_process(COMMAND lfortran ${CMAKE_CURRENT_SOURCE_DIR}/${file_name}.f90
                ${extra_args} --implicit-typing --implicit-interface -o ${name})
            add_test(${name} ${CURRENT_BINARY_DIR}/${name})
        elseif (LFORTRAN_BACKEND STREQUAL "llvmStackArray")
            execute_process(COMMAND lfortran ${CMAKE_CURRENT_SOURCE_DIR}/${file_name}.f90
                ${extra_args} --stack-arrays=true -o ${name})
            add_test(${name} ${CURRENT_BINARY_DIR}/${name})
        elseif (LFORTRAN_BACKEND STREQUAL "llvm_nopragma")
            add_executable(${name} ${file_name}.f90 ${extra_files})
            target_compile_options(${name} PUBLIC --ignore-pragma ${extra_args})
            add_test(${name} ${CURRENT_BINARY_DIR}/${name})
        elseif (LFORTRAN_BACKEND STREQUAL "c_nopragma")
            add_executable(${name} ${file_name}.f90 ${extra_files})
            target_compile_options(${name} PUBLIC --backend=c --ignore-pragma ${extra_args})
            add_test(${name} ${CURRENT_BINARY_DIR}/${name})
        elseif (LFORTRAN_BACKEND STREQUAL "llvm")
            add_executable(${name} ${file_name}.f90 ${extra_files})
            target_compile_options(${name} PUBLIC ${extra_args})
            add_test(${name} ${CURRENT_BINARY_DIR}/${name})
        elseif (LFORTRAN_BACKEND STREQUAL "gfortranModfileCompilation")
            execute_process(COMMAND gfortran ${CMAKE_CURRENT_SOURCE_DIR}/${file_name}.f90 -c)
            add_test(${name} ${CURRENT_BINARY_DIR}/${name})
        else ()
            add_executable(${name} ${file_name}.f90 ${extra_files})
            target_compile_options(${name} PUBLIC ${gfortran_args})
            add_test(${name} ${CURRENT_BINARY_DIR}/${name})
        endif()

        if (labels)
            set_tests_properties(${name} PROPERTIES LABELS "${labels}")
        endif()

        if (fail)
            set_tests_properties(${name} PROPERTIES WILL_FAIL TRUE)
        endif()

        if (copy_to_bin)
            file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/${copy_to_bin} DESTINATION ${CURRENT_BINARY_DIR})
        endif()
    endif()
endmacro(RUN_UTIL)

macro(RUN)
    set(options FAIL NOFAST)
    set(oneValueArgs NAME INCLUDE_PATH  COPY_TO_BIN)
    set(multiValueArgs LABELS EXTRAFILES EXTRA_ARGS GFORTRAN_ARGS)
    cmake_parse_arguments(RUN "${options}" "${oneValueArgs}"
                        "${multiValueArgs}" ${ARGN} )

    foreach(b ${RUN_LABELS})
        if (NOT (b MATCHES "^(llvm|llvm2|llvm_rtlib|c|cpp|x86|wasm|gfortran|llvmImplicit|llvmStackArray|fortran|c_nopragma|llvm_nopragma)$"))
            message(FATAL_ERROR "Unsupported backend: ${b}")
        endif()
    endforeach()

    set(RUN_FILE_NAME ${RUN_NAME})

    if (RUN_INCLUDE_PATH)
        # Only one include path supported for now
        # Later add support for multiple include paths by looping over and appending to extra args
        set(RUN_EXTRA_ARGS ${RUN_EXTRA_ARGS} -I${CMAKE_CURRENT_SOURCE_DIR}/${RUN_INCLUDE_PATH})
        set(RUN_GFORTRAN_ARGS ${RUN_GFORTRAN_ARGS} -I${CMAKE_CURRENT_SOURCE_DIR}/${RUN_INCLUDE_PATH})
    endif()

    if (NOT FAST)
        RUN_UTIL(RUN_FAIL RUN_NAME RUN_FILE_NAME RUN_LABELS RUN_EXTRAFILES RUN_EXTRA_ARGS RUN_COPY_TO_BIN RUN_GFORTRAN_ARGS)
    endif()

    if ((FAST) AND (NOT RUN_NOFAST))
        set(RUN_EXTRA_ARGS ${RUN_EXTRA_ARGS} --fast)
        set(RUN_NAME "${RUN_NAME}_FAST")
        list(REMOVE_ITEM RUN_LABELS gfortran) # remove gfortran from --fast test
        RUN_UTIL(RUN_FAIL RUN_NAME RUN_FILE_NAME RUN_LABELS RUN_EXTRAFILES RUN_EXTRA_ARGS RUN_COPY_TO_BIN RUN_GFORTRAN_ARGS)
    endif()
endmacro(RUN)

# only compiles till object file
# to make sure that the ASR generation is successful
macro(COMPILE)
    set(oneValueArgs NAME)
    set(multiValueArgs COMPILERS)
    cmake_parse_arguments(COMPILE "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
    set(name ${COMPILE_NAME})
    set(compilers ${COMPILE_COMPILERS})
    if (NOT name)
        message(FATAL_ERROR "Must specify the NAME argument")
    endif()

    if (NOT compilers)
        message(FATAL_ERROR "Must specify the COMPILER argument")
    endif()

    foreach(compiler ${compilers})
        if (compiler STREQUAL "lfortran" AND ${LFORTRAN_BACKEND} STREQUAL "llvm")
            add_custom_command(
                OUTPUT ${name}_lfortran.mod
                COMMAND lfortran -c ${CMAKE_CURRENT_SOURCE_DIR}/${name}.f90)
            add_library(${name}_lfortran OBJECT ${name}_lfortran.mod)
        elseif(compiler STREQUAL "gfortran" AND ${LFORTRAN_BACKEND} STREQUAL "no")
            add_custom_command(
                OUTPUT ${name}_gfortran.mod
                COMMAND gfortran -c ${CMAKE_CURRENT_SOURCE_DIR}/${name}.f90)
            add_library(${name}_gfortran OBJECT ${name}_gfortran.mod)
        endif()
    endforeach()
endmacro(COMPILE)

# The supported test labels are:
#
# gfortran      --- compile with GFortran
# fortran       --- convert to Fortran code and compile with GFortran
# llvm          --- compile with LFortran, only generate binary code at the very end
# llvm2         --- compile with LFortran, generate object files
# llvm_rtlib    --- compile with LFortran loading ASR runtime library, generate object files
# cpp           --- compile to C++, compile C++ to binary
# x86           --- compile to x86 binary directly
# wasm          --- compile to WASM binary directly

# `reduce` is not supported by GFortran yet:
# RUN(NAME doconcurrentloop_02 LABELS gfortran)

# GFortran + LFortran LLVM + LFortran C++

RUN(NAME program_cmake_01 LABELS gfortran llvm cpp x86 wasm c fortran)
RUN(NAME program_cmake_02 LABELS gfortran llvm cpp x86 wasm c fortran)

RUN(NAME error_stop_01 FAIL LABELS gfortran llvm cpp x86 wasm llvm2 fortran)
RUN(NAME error_stop_02 FAIL LABELS llvm wasm llvm2 fortran)
RUN(NAME error_stop_03 LABELS gfortran llvm)
RUN(NAME error_stop_04 FAIL LABELS gfortran llvm)
RUN(NAME stop_01 LABELS llvm wasm llvm2 c fortran)
RUN(NAME stop_02 FAIL LABELS gfortran llvm)
RUN(NAME stop_03 LABELS gfortran llvm)

RUN(NAME print_01 LABELS gfortran llvm cpp llvm2 wasm fortran)
RUN(NAME print_02 LABELS gfortran llvm llvm2 wasm fortran)
RUN(NAME print_03 LABELS gfortran llvm llvm2 wasm c fortran)
RUN(NAME print_04 LABELS gfortran llvm llvm2 fortran)
RUN(NAME print_05 LABELS gfortran llvm llvm2 fortran)
RUN(NAME print_arr_01 LABELS gfortran llvm llvm2 wasm c fortran)
RUN(NAME print_arr_02 LABELS gfortran llvm llvm2 wasm c fortran)
RUN(NAME print_arr_03 LABELS gfortran llvm llvm2 fortran)
RUN(NAME print_arr_04 LABELS llvm fortran EXTRA_ARGS --apply-fortran-mangling)

RUN(NAME include_01 LABELS gfortran llvm wasm c fortran)
RUN(NAME include_02 LABELS gfortran llvm wasm c fortran)
RUN(NAME include_03 LABELS gfortran llvm wasm c INCLUDE_PATH include_03 fortran)

RUN(NAME cond_01 LABELS gfortran llvm cpp x86 wasm c fortran)
RUN(NAME cond_02 LABELS gfortran llvm wasm c fortran)

RUN(NAME expr_01 FAIL LABELS gfortran llvm cpp x86 wasm c fortran)
RUN(NAME expr_02 LABELS gfortran llvm cpp x86 wasm c fortran)
RUN(NAME expr_03 LABELS gfortran llvm cpp x86 wasm c fortran)
RUN(NAME expr_04 LABELS gfortran llvm cpp wasm c fortran)
RUN(NAME expr_05 LABELS gfortran llvm cpp c fortran) # it contains pow, wasm supports only x**2
RUN(NAME expr_06 LABELS gfortran llvm wasm c fortran)
RUN(NAME expr_07 LABELS gfortran llvm wasm c fortran)
RUN(NAME expr_08 LABELS gfortran llvm wasm c fortran)
RUN(NAME expr_09 LABELS gfortran llvm wasm c fortran)
RUN(NAME expr_10 LABELS gfortran llvm wasm c fortran)
RUN(NAME expr_12 LABELS gfortran llvm c fortran) # it uses string compare
RUN(NAME expr_13 LABELS gfortran llvm wasm c)
RUN(NAME expr_14 LABELS gfortran llvm wasm c)
RUN(NAME expr_15 LABELS gfortran llvm wasm c fortran)
RUN(NAME expr_16 LABELS gfortran llvm wasm c)
RUN(NAME expr_17 LABELS gfortran llvm)
RUN(NAME expr_18 LABELS gfortran llvm wasm c)
RUN(NAME expr_19 LABELS gfortran llvm wasm c)

RUN(NAME data_01 LABELS gfortran llvm c fortran)
RUN(NAME data_02 LABELS gfortran)
RUN(NAME data_03 LABELS gfortran llvm c fortran)
RUN(NAME data_04 LABELS gfortran llvm c)
RUN(NAME data_05 LABELS gfortran) # llvmImplicit removed
RUN(NAME data_06 LABELS gfortran llvm c)
RUN(NAME data_07 LABELS gfortran) # TODO: Fix this test
RUN(NAME data_08 LABELS gfortran llvmImplicit)
RUN(NAME data_09 LABELS gfortran llvm c)
RUN(NAME data_10 LABELS gfortran llvm)
RUN(NAME data_11 LABELS gfortran llvmImplicit)

RUN(NAME minmax_01 LABELS gfortran llvm wasm c)
RUN(NAME arithmetic_if_01 LABELS gfortran llvm c) # arithmetic tests use goto
RUN(NAME arithmetic_if_02 LABELS gfortran llvm c)
RUN(NAME arithmetic_if_03 LABELS gfortran llvm c)
RUN(NAME arithmetic_if_04 LABELS gfortran llvm c)

RUN(NAME variables_01 LABELS gfortran llvm cpp x86 wasm c)
RUN(NAME variables_02 LABELS gfortran llvm cpp x86 wasm c)
RUN(NAME variables_03 LABELS gfortran llvm cpp wasm c)

RUN(NAME if_01 LABELS gfortran llvm cpp x86 wasm c)
RUN(NAME if_02 FAIL LABELS gfortran llvm cpp x86 wasm c)
RUN(NAME if_03 FAIL LABELS gfortran llvm cpp x86 wasm c)
RUN(NAME if_04 LABELS gfortran llvm cpp x86 wasm c)
RUN(NAME if_05 LABELS gfortran llvm cpp wasm c)

RUN(NAME while_01 LABELS gfortran llvm cpp x86 wasm c)
RUN(NAME while_02 LABELS gfortran llvm cpp wasm c)

RUN(NAME doloop_01 LABELS gfortran llvm c cpp x86 wasm)
RUN(NAME doloop_02 LABELS gfortran llvm c cpp x86 wasm)
RUN(NAME doloop_03 LABELS gfortran llvm c cpp wasm)
RUN(NAME doloop_04 LABELS gfortran llvm) # uses goto target
RUN(NAME doloop_05 LABELS gfortran llvm) # uses goto
RUN(NAME doloop_06 LABELS gfortran llvm c cpp wasm)
RUN(NAME doloop_07 LABELS gfortran llvm c cpp wasm)
RUN(NAME doloop_08 LABELS gfortran llvm c cpp wasm)
RUN(NAME doloop_09 LABELS gfortran llvm) # nested subroutine
RUN(NAME doloop_10 LABELS gfortran llvm wasm) # implicit cast loop variable
RUN(NAME doloop_11 LABELS gfortran llvm wasm EXTRA_ARGS --use-loop-variable-after-loop)

RUN(NAME cycle_and_exit1 LABELS gfortran llvm fortran)
RUN(NAME cycle_and_exit2 LABELS gfortran llvm fortran)
RUN(NAME cycle_and_exit3 LABELS gfortran llvm)
RUN(NAME cycle_and_exit4 LABELS gfortran llvm)
RUN(NAME cycle_and_exit5 LABELS gfortran llvm)

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
RUN(NAME subroutines_10 LABELS gfortran)

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
RUN(NAME functions_18 LABELS gfortran llvm)
RUN(NAME functions_19 LABELS gfortran llvm)
RUN(NAME functions_20 LABELS gfortran llvm)

RUN(NAME common_01 LABELS gfortran)


RUN(NAME types_01 LABELS gfortran llvm cpp wasm)
RUN(NAME types_02 LABELS gfortran llvm cpp wasm fortran)
RUN(NAME types_03 LABELS gfortran llvm cpp wasm fortran)
RUN(NAME types_04 LABELS gfortran llvm cpp wasm)
RUN(NAME types_05 LABELS gfortran llvm cpp wasm)
RUN(NAME types_06 LABELS gfortran llvm cpp wasm)

# GFortran + LFortran C++
RUN(NAME doconcurrentloop_01 LABELS gfortran cpp)

RUN(NAME arrays_01 LABELS gfortran cpp llvm llvmStackArray wasm)
RUN(NAME arrays_01_size LABELS gfortran llvm llvmStackArray wasm fortran)
RUN(NAME arrays_02_size LABELS gfortran llvm llvmStackArray wasm fortran)
RUN(NAME arrays_03_size LABELS gfortran llvm llvmStackArray fortran)
RUN(NAME matrix_01_transpose LABELS gfortran llvm)
RUN(NAME matrix_02_matmul LABELS gfortran)
RUN(NAME array_01_pack LABELS gfortran llvm fortran)
RUN(NAME array_01_transfer LABELS gfortran)
RUN(NAME array_02_pack LABELS gfortran llvm fortran)
RUN(NAME array_02_transfer LABELS gfortran)
RUN(NAME array_03_transfer LABELS gfortran)
RUN(NAME array_04_all LABELS gfortran fortran)
RUN(NAME arrays_01_real LABELS gfortran llvm llvmStackArray wasm)
RUN(NAME arrays_01_complex LABELS gfortran llvm llvmStackArray wasm)
RUN(NAME arrays_01_logical LABELS gfortran llvm llvmStackArray wasm)
RUN(NAME arrays_01_multi_dim LABELS gfortran llvm llvmStackArray wasm)
RUN(NAME array_bound_1 LABELS gfortran llvm llvmStackArray wasm)
RUN(NAME array_bound_2 LABELS gfortran llvm llvmStackArray wasm)
RUN(NAME array_bound_3 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_op_1 LABELS gfortran llvm llvmStackArray wasm)
RUN(NAME arrays_op_2 LABELS gfortran llvm llvmStackArray wasm)
RUN(NAME arrays_op_3 LABELS gfortran llvm llvmStackArray wasm)
RUN(NAME arrays_op_4 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_op_5 LABELS gfortran llvm llvmStackArray wasm)
RUN(NAME arrays_op_6 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_op_7 LABELS gfortran llvm llvmStackArray wasm)
RUN(NAME arrays_op_8 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_op_9 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_op_10 LABELS gfortran llvm llvmStackArray wasm)
RUN(NAME arrays_op_11 LABELS gfortran llvm llvmStackArray
    EXTRA_ARGS --realloc-lhs)
RUN(NAME arrays_op_13 LABELS gfortran llvm llvmStackArray wasm)
RUN(NAME arrays_op_14 LABELS gfortran llvm llvmStackArray wasm)
RUN(NAME arrays_op_15 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_op_16 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_op_17 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_op_18 LABELS gfortran llvm llvmStackArray
    EXTRA_ARGS --realloc-lhs)
RUN(NAME arrays_op_19 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_op_20 LABELS gfortran llvm llvmStackArray
    EXTRA_ARGS --realloc-lhs)
RUN(NAME arrays_op_21 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_op_22 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_op_23 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_op_24 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_op_26 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_op_27 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_op_28 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_op_29 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_reshape_14 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_reshape_15 LABELS gfortran llvm fortran)
RUN(NAME arrays_reshape_16 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_elemental_15 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_03_func LABELS gfortran cpp llvm llvmStackArray wasm)
RUN(NAME arrays_03_func_pass_arr_dims LABELS gfortran llvm llvmStackArray wasm)
RUN(NAME arrays_04_func LABELS gfortran cpp llvm llvmStackArray wasm)
RUN(NAME arrays_04_func_pass_arr_dims LABELS gfortran llvm llvmStackArray wasm)
RUN(NAME arrays_05 LABELS gfortran llvm llvmStackArray wasm)
RUN(NAME arrays_17 LABELS gfortran llvm llvmStackArray wasm)
RUN(NAME arrays_18_func LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_19 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_20 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_22 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_23 LABELS gfortran llvm llvmStackArray wasm)
RUN(NAME arrays_24 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_25 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_26 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_27 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_28 LABELS gfortran llvm llvmStackArray) # maxloc, minloc
RUN(NAME arrays_29 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_30 LABELS gfortran llvm llvmStackArray) # maxloc
RUN(NAME arrays_31 LABELS gfortran llvm llvmStackArray wasm) # init expr with fixed size arr as dependency
RUN(NAME arrays_32 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_33 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_34 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_35 LABELS gfortran llvm)
RUN(NAME arrays_36 LABELS gfortran llvm)
RUN(NAME arrays_37 LABELS gfortran llvm)
RUN(NAME arrays_38 LABELS gfortran llvm)
RUN(NAME arrays_39 LABELS gfortran llvm)
RUN(NAME arrays_40 LABELS gfortran llvm)


# GFortran
RUN(NAME arrays_02 LABELS gfortran)
RUN(NAME arrays_06 LABELS gfortran llvm llvmStackArray wasm)
RUN(NAME arrays_07 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_08_func LABELS gfortran llvm llvmStackArray wasm)
RUN(NAME arrays_08_func_pass_arr_dims LABELS gfortran llvm llvmStackArray wasm)
RUN(NAME arrays_09 LABELS gfortran)
RUN(NAME arrays_10 LABELS gfortran)
RUN(NAME arrays_11 LABELS gfortran llvm llvmStackArray cpp)
RUN(NAME arrays_12 LABELS gfortran) # constant arrays
RUN(NAME arrays_13 LABELS gfortran llvm llvmStackArray) # constant arrays
RUN(NAME arrays_14 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_15 LABELS gfortran llvm llvmStackArray)
RUN(NAME arrays_inputs_15 LABELS gfortran llvm llvmStackArray) # constant arrays
RUN(NAME arrays_16_func LABELS gfortran llvm llvmStackArray)

RUN(NAME arrays_intrin_01 LABELS gfortran fortran) # minval, maxval
RUN(NAME arrays_intrin_02 LABELS gfortran) # all, any
RUN(NAME arrays_intrin_03 LABELS gfortran llvm llvmStackArray) # maxval
RUN(NAME arrays_intrin_04 LABELS gfortran llvm llvmStackArray fortran) # maxval
RUN(NAME arrays_intrin_05 LABELS gfortran llvm llvmStackArray) # minval
RUN(NAME arrays_intrin_06 LABELS gfortran llvm llvmStackArray fortran) # minval
RUN(NAME arrays_intrin_07 LABELS gfortran llvm llvmStackArray) # min
RUN(NAME any_01 LABELS gfortran)
RUN(NAME any_02 LABELS gfortran llvm)
RUN(NAME sum_01 LABELS gfortran llvm)
RUN(NAME sum_02 LABELS gfortran llvm fortran)
RUN(NAME product_01 LABELS gfortran llvm)
RUN(NAME product_02 LABELS gfortran llvm)

RUN(NAME reserved_01 LABELS gfortran llvm wasm)
RUN(NAME reserved_02 LABELS gfortran llvm)
RUN(NAME reserved_03 LABELS gfortran)

RUN(NAME namelist_01 LABELS gfortran)

RUN(NAME format_01 LABELS gfortran)
RUN(NAME format_02 LABELS gfortran)
RUN(NAME format_04 LABELS gfortran llvm)
RUN(NAME format_05 LABELS gfortran)
RUN(NAME format_06 LABELS gfortran llvm)
RUN(NAME format_07 LABELS gfortran llvm)
RUN(NAME format_08 LABELS gfortran llvm)
RUN(NAME format_09 LABELS gfortran llvm)
RUN(NAME format_10 LABELS gfortran llvm)
RUN(NAME format_11 LABELS gfortran llvm)
RUN(NAME format_12 LABELS gfortran llvm)

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
RUN(NAME intrinsics_07 LABELS gfortran llvm wasm) # kind
RUN(NAME intrinsics_08 LABELS gfortran llvm wasm fortran) # tiny symboltable
RUN(NAME intrinsics_09 LABELS gfortran llvm wasm fortran) # tiny body
RUN(NAME intrinsics_10 LABELS gfortran llvm wasm) # real body
RUN(NAME intrinsics_11 LABELS gfortran llvm wasm) # real symboltable
RUN(NAME intrinsics_12 LABELS gfortran llvm wasm) # kind body
RUN(NAME intrinsics_13 LABELS gfortran llvm wasm) # kind symboltable
RUN(NAME intrinsics_14 LABELS gfortran llvm) # selected_real,int_kind body
RUN(NAME intrinsics_15 LABELS gfortran llvm) # real
RUN(NAME intrinsics_16 LABELS gfortran llvm) # aimag
RUN(NAME intrinsics_17 LABELS gfortran llvm) # exp, log, erf
RUN(NAME intrinsics_17b LABELS gfortran llvm c) # log
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
RUN(NAME intrinsics_30 LABELS gfortran llvm fortran) # range
RUN(NAME intrinsics_31 LABELS gfortran llvm wasm)
RUN(NAME intrinsics_32 LABELS gfortran)
RUN(NAME intrinsics_33 LABELS gfortran llvm)
RUN(NAME intrinsics_34 LABELS gfortran llvm wasm) # epsilon
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
RUN(NAME intrinsics_50 LABELS gfortran llvm)
RUN(NAME intrinsics_51 LABELS gfortran llvm)
RUN(NAME intrinsics_52 LABELS gfortran llvm) # max0, min0
RUN(NAME intrinsics_53 LABELS gfortran llvm) # repeat
RUN(NAME intrinsics_54 LABELS gfortran llvm) # max0
RUN(NAME intrinsics_open_close_read_write LABELS gfortran)
RUN(NAME intrinsics_55 LABELS gfortran llvm) #min
RUN(NAME intrinsics_56 LABELS gfortran llvm)
RUN(NAME intrinsics_57 LABELS gfortran llvm)
RUN(NAME intrinsics_58 LABELS gfortran llvm) # sign
RUN(NAME intrinsics_59 LABELS gfortran llvm) # sign
RUN(NAME intrinsics_60 LABELS gfortran llvm COPY_TO_BIN intrinsics_60_data.txt) # iachar for extended ascii
RUN(NAME intrinsics_61 LABELS gfortran llvm fortran) # char
RUN(NAME intrinsics_62 LABELS gfortran llvm) # shape
RUN(NAME intrinsics_63 LABELS gfortran llvm)
RUN(NAME intrinsics_64 LABELS gfortran llvm wasm) # sign
RUN(NAME intrinsics_65 LABELS gfortran llvm) # matmul
RUN(NAME intrinsics_66 LABELS gfortran llvm) # sign
RUN(NAME intrinsics_67 LABELS gfortran llvm) # dcosh, dsinh, dtanh, dtan
RUN(NAME intrinsics_68 LABELS gfortran llvm) # dsign
RUN(NAME intrinsics_69 LABELS gfortran llvm fortran) # radix
RUN(NAME intrinsics_70 LABELS gfortran llvm) # aint
RUN(NAME intrinsics_71 LABELS gfortran llvm) # matmul
RUN(NAME intrinsics_72 LABELS gfortran llvmImplicit) # datan2
RUN(NAME intrinsics_73 LABELS gfortran llvm) # sngl
RUN(NAME intrinsics_74 LABELS gfortran llvm c) # random_number
RUN(NAME intrinsics_75 LABELS gfortran llvm)
RUN(NAME intrinsics_76 LABELS gfortran llvm)
RUN(NAME intrinsics_77 LABELS gfortran llvm)
RUN(NAME intrinsics_78 LABELS gfortran llvm wasm) # mod
RUN(NAME intrinsics_79 LABELS gfortran llvm wasm) # Trailz
RUN(NAME intrinsics_80 LABELS gfortran llvm) # sum
RUN(NAME intrinsics_81 LABELS gfortran llvm) # maxval
RUN(NAME intrinsics_82 LABELS gfortran llvm) # array support tan, cosh, sinh, tanh
RUN(NAME intrinsics_83 LABELS gfortran llvm) # kind
RUN(NAME intrinsics_84 LABELS gfortran llvm) # minexponent
RUN(NAME intrinsics_85 LABELS gfortran llvm) # maxexponent
RUN(NAME intrinsics_86 LABELS gfortran llvm) # kind
RUN(NAME intrinsics_87 LABELS gfortran llvm) # abs
RUN(NAME intrinsics_88 LABELS gfortran llvm) # min for strings
RUN(NAME intrinsics_89 LABELS gfortran llvm) # sin
RUN(NAME intrinsics_90 LABELS gfortran llvm) # min
RUN(NAME intrinsics_91 LABELS gfortran llvm) # index
RUN(NAME intrinsics_92 LABELS gfortran llvm) # anint
RUN(NAME intrinsics_93 LABELS gfortran llvm) # digits
RUN(NAME intrinsics_94 LABELS gfortran llvm NOFAST) # hypot
RUN(NAME intrinsics_95 LABELS gfortran llvm)
RUN(NAME intrinsics_96 LABELS gfortran llvm)
RUN(NAME intrinsics_97 LABELS gfortran llvm) # tiny
RUN(NAME intrinsics_98 LABELS gfortran llvm) # shape
RUN(NAME intrinsics_99 LABELS gfortran llvm)
RUN(NAME intrinsics_100 LABELS gfortran llvm) # mod
RUN(NAME intrinsics_101 LABELS gfortran llvm) # repeat
RUN(NAME intrinsics_102 LABELS gfortran llvm)
RUN(NAME intrinsics_103 LABELS gfortran llvm)
RUN(NAME intrinsics_104 LABELS gfortran llvm c wasm)
RUN(NAME intrinsics_105 LABELS gfortran llvm) # shape
RUN(NAME intrinsics_106 LABELS gfortran llvm) # merge
RUN(NAME intrinsics_107 LABELS gfortran llvm) # trailz
RUN(NAME intrinsics_108 LABELS gfortran llvm) # shiftr
RUN(NAME intrinsics_109 LABELS gfortran llvm)
RUN(NAME intrinsics_110 LABELS gfortran llvm)
RUN(NAME intrinsics_111 LABELS gfortran llvm)
RUN(NAME intrinsics_112 LABELS gfortran llvm)
RUN(NAME intrinsics_113 LABELS gfortran llvm)
RUN(NAME intrinsics_114 LABELS gfortran llvm)
RUN(NAME intrinsics_115 LABELS gfortran llvm)
RUN(NAME intrinsics_116 LABELS gfortran llvm)
RUN(NAME intrinsics_117 LABELS gfortran llvm)
RUN(NAME intrinsics_118 LABELS gfortran llvm)
RUN(NAME intrinsics_119 LABELS gfortran llvm)
RUN(NAME intrinsics_120 LABELS gfortran llvm)
RUN(NAME intrinsics_121 LABELS gfortran llvm) # leadz
RUN(NAME intrinsics_122 LABELS gfortran llvm) # rank
RUN(NAME intrinsics_123 LABELS gfortran llvm) # shiftl
RUN(NAME intrinsics_124 LABELS gfortran llvm) # lshift
RUN(NAME intrinsics_125 LABELS gfortran llvm) # random_number
RUN(NAME intrinsics_126 LABELS gfortran llvm) # random_number
RUN(NAME intrinsics_127 LABELS gfortran llvm) # ifix
RUN(NAME intrinsics_128 LABELS gfortran llvm) # idint
RUN(NAME intrinsics_129 LABELS gfortran llvm) # abs
RUN(NAME intrinsics_130 LABELS gfortran llvm) # ishft
RUN(NAME intrinsics_131 LABELS gfortran llvm) # max for strings
RUN(NAME intrinsics_132 LABELS gfortran llvm) # Floor
RUN(NAME intrinsics_133 LABELS gfortran llvm) # Ceiling
RUN(NAME intrinsics_135 LABELS gfortran llvm) # nint
RUN(NAME intrinsics_136 LABELS gfortran llvm) # rshift
RUN(NAME intrinsics_137 LABELS gfortran llvm) # huge
RUN(NAME intrinsics_138 LABELS gfortran llvm fortran) # precision
RUN(NAME intrinsics_140 LABELS gfortran llvm) # selected_int_kind
RUN(NAME intrinsics_141 LABELS gfortran llvm) # scale
RUN(NAME intrinsics_142 LABELS gfortran llvm) # dot_product
RUN(NAME intrinsics_143 LABELS gfortran llvm) # dot_product
RUN(NAME intrinsics_144 LABELS gfortran llvm) # dot_product
RUN(NAME intrinsics_145 LABELS gfortran llvm) # dot_product
RUN(NAME intrinsics_146 LABELS gfortran llvm) # dprod
RUN(NAME intrinsics_147 LABELS gfortran llvm) # pack
RUN(NAME intrinsics_148 LABELS gfortran llvm) # pack
RUN(NAME intrinsics_149 LABELS gfortran llvm NOFAST) # unpack
RUN(NAME intrinsics_150 LABELS gfortran llvm) # maskr
RUN(NAME intrinsics_151 LABELS gfortran llvm NOFAST) # maskl

RUN(NAME parameter_01 LABELS gfortran)
RUN(NAME parameter_02 LABELS gfortran)
RUN(NAME parameter_03 LABELS gfortran llvm wasm)
RUN(NAME parameter_04 LABELS gfortran llvm wasm) # selected_real,int_kind symboltable
RUN(NAME parameter_05 LABELS gfortran llvm wasm) # Implicit IntegerToReal
RUN(NAME parameter_06 LABELS gfortran llvm)
RUN(NAME parameter_07 LABELS gfortran llvm)

RUN(NAME modules_01 LABELS gfortran llvm wasm fortran)
RUN(NAME modules_02 LABELS gfortran llvm wasm)
RUN(NAME modules_03 LABELS gfortran)
RUN(NAME modules_04 LABELS gfortran)
RUN(NAME modules_05 LABELS gfortran)
RUN(NAME modules_06 LABELS gfortran llvm wasm fortran)
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
RUN(NAME modules_13 LABELS gfortran llvm wasm fortran)
RUN(NAME modules_14 LABELS gfortran llvm EXTRAFILES modules_14_a.f90)
RUN(NAME modules_15 LABELS gfortran llvm EXTRAFILES
        modules_15b.f90 modules_15c.c NOFAST)
RUN(NAME modules_16 LABELS gfortran llvm EXTRAFILES modules_16b.f90)
RUN(NAME modules_18 LABELS gfortran llvm EXTRAFILES
        modules_18b.f90 modules_15c.c NOFAST)
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
RUN(NAME modules_44 LABELS gfortran EXTRAFILES
        modules_44_module.f90)
RUN(NAME modules_40 LABELS gfortran)
RUN(NAME modules_41 LABELS gfortran llvm)
RUN(NAME modules_42 LABELS gfortran llvm)
RUN(NAME modules_43 LABELS gfortran)
RUN(NAME modules_45 LABELS gfortran)
RUN(NAME modules_47 LABELS gfortran llvm)
RUN(NAME modules_48 LABELS gfortran llvm)
RUN(NAME modules_49 LABELS gfortran llvm)
RUN(NAME modules_50 LABELS gfortran)
RUN(NAME modules_51 LABELS gfortran llvmImplicit)
RUN(NAME modules_53 LABELS gfortran llvm)
RUN(NAME modules_54 LABELS gfortran llvm)
RUN(NAME modules_55 LABELS gfortran llvm)
RUN(NAME modules_56 LABELS gfortran llvm NOFAST)
RUN(NAME operator_overloading_05_module3 LABELS gfortran llvm EXTRAFILES
        operator_overloading_05_module1.f90 operator_overloading_05_module2.f90)
RUN(NAME associate_06 LABELS gfortran EXTRAFILES
        associate_06_module.f90)

RUN(NAME bindc1 LABELS gfortran llvm)
RUN(NAME bindc2 LABELS gfortran llvm)
RUN(NAME bindc3 LABELS gfortran llvm)
RUN(NAME bindc4 LABELS gfortran llvm)
RUN(NAME bindc_01 LABELS gfortran llvm EXTRAFILES bindc_01b.f90 bindc_01c.c NOFAST)
RUN(NAME bindc_02 LABELS gfortran llvm EXTRAFILES bindc_02b.f90 bindc_02c.c NOFAST)

RUN(NAME case_01 LABELS gfortran llvm cpp)
RUN(NAME case_02 LABELS gfortran llvm)
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

RUN(NAME where_01 LABELS gfortran llvm fortran)
RUN(NAME where_02 LABELS gfortran llvm fortran)
RUN(NAME where_03 LABELS gfortran llvm)
RUN(NAME where_04 LABELS gfortran) # TODO: Fix this test #1631
RUN(NAME where_05 LABELS gfortran llvm)
RUN(NAME where_06 LABELS gfortran llvm)

RUN(NAME forallloop_01 LABELS gfortran)

RUN(NAME parsing_01 LABELS gfortran)
RUN(NAME parsing_02 LABELS gfortran)
RUN(NAME parsing_03 LABELS gfortran llvm wasm)

RUN(NAME interface_01 LABELS gfortran llvm)
RUN(NAME interface_02 LABELS gfortran llvm fortran)
RUN(NAME interface_03 LABELS gfortran)
RUN(NAME interface_04 LABELS gfortran llvm)
RUN(NAME interface_05 LABELS gfortran llvm fortran)
RUN(NAME interface_06 LABELS gfortran)
RUN(NAME interface_07 LABELS gfortran llvm fortran)
RUN(NAME interface_09 LABELS gfortran)
RUN(NAME interface_10 LABELS gfortran llvm)
RUN(NAME interface_12a LABELS gfortran llvm EXTRAFILES interface_12b.f90)
RUN(NAME interface_13 LABELS gfortran llvm)
RUN(NAME interface_14 LABELS gfortran llvm)
RUN(NAME interface_15 LABELS gfortran llvm fortran)

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

RUN(NAME implicit_typing_01 LABELS gfortran llvmImplicit)
RUN(NAME implicit_typing_02 LABELS gfortran llvmImplicit)

RUN(NAME generic_name_01 LABELS gfortran llvm)

RUN(NAME operator_overloading_01 LABELS gfortran llvm)
RUN(NAME operator_overloading_02 LABELS gfortran llvm)
RUN(NAME operator_overloading_03 LABELS gfortran llvm)
RUN(NAME operator_overloading_04 LABELS gfortran)
RUN(NAME operator_overloading_06 LABELS gfortran llvm)

RUN(NAME types_07 LABELS gfortran)
RUN(NAME types_08 LABELS gfortran)
RUN(NAME types_09 LABELS gfortran)
RUN(NAME types_10 LABELS gfortran)
RUN(NAME types_11 LABELS gfortran)
RUN(NAME types_12 LABELS gfortran llvm wasm)
RUN(NAME types_13 LABELS gfortran)
RUN(NAME types_14 LABELS gfortran llvm wasm fortran)
RUN(NAME types_16 LABELS gfortran llvm wasm fortran)
RUN(NAME types_17 LABELS gfortran llvm)
RUN(NAME types_18 LABELS gfortran llvm wasm fortran)
RUN(NAME types_19 LABELS gfortran llvm wasm fortran)

RUN(NAME complex_01 LABELS gfortran llvm wasm fortran)
RUN(NAME complex_02 LABELS gfortran llvm wasm fortran)
RUN(NAME complex_03 LABELS llvm wasm)
RUN(NAME complex_04 LABELS gfortran llvm wasm)
RUN(NAME complex_05 LABELS gfortran llvm wasm fortran)
RUN(NAME complex_06 LABELS gfortran llvm wasm fortran)
RUN(NAME complex_07 LABELS gfortran) # cmplx
RUN(NAME complex_08 LABELS gfortran) # conjg
RUN(NAME complex_09 LABELS gfortran)
RUN(NAME complex_10 LABELS gfortran llvm) # conjg
RUN(NAME complex_sub_test LABELS gfortran llvm wasm)
RUN(NAME complex_mul_test LABELS gfortran llvm wasm)
RUN(NAME complex_div_test LABELS gfortran llvm)
RUN(NAME complex_pow_test LABELS gfortran llvm)

RUN(NAME logical1 LABELS gfortran llvm wasm fortran)
RUN(NAME logical2 LABELS gfortran llvm wasm fortran)
RUN(NAME logical3 LABELS gfortran llvm wasm)
RUN(NAME logical4 LABELS gfortran llvm wasm fortran)

RUN(NAME tsunami LABELS gfortran llvm)

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
RUN(NAME derived_types_21 LABELS gfortran llvm)
RUN(NAME derived_types_22 LABELS gfortran llvm)
RUN(NAME derived_types_23 LABELS gfortran llvm)
RUN(NAME derived_types_24 LABELS gfortran llvm)
RUN(NAME derived_types_25 LABELS gfortran llvm)
RUN(NAME derived_types_26 LABELS gfortran llvm)
RUN(NAME derived_types_27 LABELS gfortran llvm fortran)
RUN(NAME derived_types_28 LABELS gfortran llvm)
RUN(NAME derived_types_29 LABELS gfortran llvm fortran)
RUN(NAME derived_types_30 LABELS gfortran llvm)
RUN(NAME derived_types_31 LABELS gfortran llvm)
RUN(NAME derived_types_32 LABELS gfortran)
RUN(NAME derived_types_33 LABELS gfortran llvm)

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
RUN(NAME allocate_06 LABELS gfortran llvm)
RUN(NAME allocate_10 LABELS gfortran llvm)
RUN(NAME allocate_11 LABELS gfortran llvm
    EXTRA_ARGS --realloc-lhs)
RUN(NAME allocate_12 LABELS gfortran llvm
    EXTRA_ARGS --realloc-lhs)
RUN(NAME allocate_13 LABELS gfortran llvm)
RUN(NAME allocate_14 LABELS gfortran llvm
    EXTRA_ARGS --realloc-lhs)
RUN(NAME allocate_15 LABELS gfortran llvm)
RUN(NAME allocate_16 LABELS gfortran llvm)
RUN(NAME block_01 LABELS gfortran llvm)
RUN(NAME block_02 LABELS gfortran)
RUN(NAME block_03 LABELS gfortran llvm)
RUN(NAME block_04 LABELS gfortran llvm)
RUN(NAME block_05 LABELS gfortran llvm)
RUN(NAME block_06 LABELS gfortran llvm wasm)
RUN(NAME block_07 LABELS gfortran llvm)
RUN(NAME block_08 LABELS gfortran llvm)
RUN(NAME associate_01 LABELS gfortran llvm)
RUN(NAME associate_02 LABELS gfortran llvm)
RUN(NAME associate_03 LABELS gfortran llvm)
RUN(NAME associate_04 LABELS gfortran llvm)
RUN(NAME associate_05 LABELS gfortran llvm)
RUN(NAME associate_07 LABELS gfortran llvm)
RUN(NAME associate_08 LABELS gfortran)
RUN(NAME associate_09 LABELS gfortran llvm)
RUN(NAME associate_10 LABELS gfortran llvm)
RUN(NAME associate_11 LABELS gfortran llvm)
RUN(NAME associate_12 LABELS gfortran llvm)
RUN(NAME associate_13 LABELS gfortran llvm)

RUN(NAME real_dp_01 LABELS gfortran llvm wasm)
RUN(NAME real_dp_02 LABELS gfortran llvm wasm)
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
RUN(NAME const_array_01 LABELS gfortran llvm llvmStackArray)
RUN(NAME const_array_02 LABELS gfortran llvm llvmStackArray)

RUN(NAME string_01 LABELS gfortran llvm cpp wasm fortran)
RUN(NAME string_02 LABELS gfortran llvm wasm fortran)
RUN(NAME string_03 LABELS gfortran llvm)
RUN(NAME string_04 LABELS gfortran llvm)
RUN(NAME string_05 LABELS gfortran llvm fortran)
RUN(NAME string_06 LABELS gfortran llvm)
RUN(NAME string_07 LABELS gfortran llvm)
RUN(NAME string_08 LABELS gfortran llvm)
RUN(NAME string_09 LABELS gfortran llvm)
RUN(NAME string_10 LABELS gfortran llvm)
RUN(NAME string_11 LABELS gfortran llvm)
RUN(NAME string_12 LABELS gfortran fortran)
RUN(NAME string_13 LABELS gfortran llvm fortran)
RUN(NAME string_14 LABELS gfortran)
RUN(NAME string_15 LABELS gfortran) # lge, lgt lle, llt
RUN(NAME string_16 LABELS gfortran fortran) # achar
RUN(NAME string_18 LABELS gfortran llvm)
RUN(NAME string_19 LABELS gfortran) # achar
RUN(NAME string_20 LABELS gfortran llvm)
RUN(NAME string_21 LABELS gfortran llvm)
RUN(NAME string_22 LABELS gfortran llvm)
RUN(NAME string_23 LABELS gfortran llvm)
RUN(NAME string_24 LABELS gfortran llvm)
RUN(NAME string_25 LABELS gfortran llvm fortran)
RUN(NAME string_26 LABELS gfortran llvm fortran)
RUN(NAME string_27 LABELS gfortran llvm)
RUN(NAME string_28 LABELS gfortran llvm)
RUN(NAME string_29 LABELS gfortran llvm)
COMPILE(NAME string_30 COMPILERS gfortran lfortran)
RUN(NAME string_31 LABELS gfortran llvm)
RUN(NAME string_32 LABELS gfortran llvm)
RUN(NAME string_33 LABELS gfortran llvm)

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

RUN(NAME nested_vars1 LABELS gfortran llvm wasm c)
RUN(NAME nested_vars2 LABELS gfortran llvm wasm c)
RUN(NAME nested_vars3 LABELS gfortran llvm wasm c)

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

RUN(NAME file_01       LABELS gfortran llvm COPY_TO_BIN file_01_data.txt)
RUN(NAME file_02       LABELS gfortran llvm COPY_TO_BIN file_01_data.txt)
RUN(NAME file_03       LABELS gfortran llvm fortran COPY_TO_BIN file_03_data.txt)
RUN(NAME file_04       LABELS gfortran llvm COPY_TO_BIN file_04_data.dat)
RUN(NAME file_05       LABELS gfortran llvm COPY_TO_BIN file_05_data.dat)
RUN(NAME file_06       LABELS gfortran llvm COPY_TO_BIN file_06_data.dat)
RUN(NAME file_07       LABELS gfortran llvm COPY_TO_BIN file_07_data.dat)
RUN(NAME file_08       LABELS gfortran llvm fortran COPY_TO_BIN file_08.txt)
RUN(NAME file_09       LABELS gfortran llvm COPY_TO_BIN file_01_data.txt)
RUN(NAME file_10       LABELS gfortran llvm COPY_TO_BIN file_10_data.txt)
RUN(NAME file_11       LABELS gfortran llvm COPY_TO_BIN file_01_data.txt)
RUN(NAME file_12       LABELS gfortran llvm COPY_TO_BIN file_12_data.txt)
RUN(NAME file_13       LABELS gfortran llvm COPY_TO_BIN file_01_data.txt)
RUN(NAME file_14       LABELS gfortran llvm COPY_TO_BIN file_08.txt)
RUN(NAME file_15       LABELS gfortran llvm fortran COPY_TO_BIN file_03_data.txt)
RUN(NAME file_16       LABELS gfortran llvm)
RUN(NAME file_17       LABELS gfortran llvm)
RUN(NAME file_18       LABELS gfortran llvm COPY_TO_BIN file_18_data.txt)

RUN(NAME inquire_01    LABELS gfortran llvm)
RUN(NAME test_backspace_01 LABELS gfortran llvm COPY_TO_BIN file_01_data.txt)

RUN(NAME class_01 LABELS gfortran)
RUN(NAME class_02 LABELS gfortran llvm)
RUN(NAME class_03 LABELS gfortran)
RUN(NAME class_04 LABELS gfortran)
RUN(NAME class_05 LABELS gfortran llvm)
RUN(NAME class_06 LABELS gfortran llvm)
RUN(NAME class_07 LABELS gfortran llvm)
RUN(NAME class_08 LABELS gfortran llvm)
RUN(NAME class_09 LABELS gfortran llvm)
RUN(NAME class_10 LABELS gfortran llvm)
RUN(NAME class_11 LABELS gfortran llvm)
RUN(NAME class_12 LABELS gfortran llvm)
RUN(NAME class_13 LABELS gfortran llvm)

RUN(NAME kwargs_01 LABELS gfortran llvm wasm)
RUN(NAME kwargs_02 LABELS gfortran)

RUN(NAME test_iso_c_binding LABELS gfortran llvm wasm)
RUN(NAME test_iso_fortran_env LABELS gfortran llvm wasm)

RUN(NAME abs_01 LABELS gfortran llvm wasm c fortran)
RUN(NAME abs_02 LABELS gfortran llvm wasm fortran)
RUN(NAME abs_03 LABELS gfortran llvm wasm c fortran)
RUN(NAME sqrt_01 LABELS gfortran llvm)
RUN(NAME sqrt_02 LABELS gfortran llvm)
RUN(NAME sin_01 LABELS gfortran llvm)
RUN(NAME sin_02 LABELS gfortran llvm)
RUN(NAME sin_03 LABELS gfortran llvm)
RUN(NAME sin_04 LABELS gfortran llvm)
RUN(NAME bits_01 LABELS gfortran llvm)
RUN(NAME bits_02 LABELS gfortran llvm fortran)
RUN(NAME bits_03 LABELS gfortran llvm)
RUN(NAME bits_04 LABELS gfortran llvm)
RUN(NAME bits_05 LABELS gfortran llvm)
RUN(NAME bits_06 LABELS gfortran llvm fortran)
RUN(NAME cpu_time_01 LABELS gfortran llvm)
RUN(NAME cpu_time_02_wasm LABELS wasm)
RUN(NAME system_clock_01 LABELS gfortran llvm)
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
RUN(NAME flush_01 LABELS gfortran llvm COPY_TO_BIN file_01_data.txt)

RUN(NAME assign_to1 LABELS gfortran llvm)
RUN(NAME assign_to2 LABELS gfortran llvm)
RUN(NAME assign_to3 LABELS gfortran llvm fortran)

RUN(NAME conv_complex2real LABELS gfortran llvm wasm)

RUN(NAME template_02 LABELS llvm)
RUN(NAME template_03 LABELS llvm wasm)
RUN(NAME template_04 LABELS llvm)
RUN(NAME template_05 LABELS llvm wasm)
RUN(NAME template_add_01 LABELS llvm wasm)
RUN(NAME template_add_01b LABELS llvm wasm)
RUN(NAME template_add_01c LABELS llvm wasm)
RUN(NAME template_add_02 LABELS llvm wasm)
RUN(NAME template_add_03 LABELS llvm wasm)
RUN(NAME template_add_04 LABELS llvm wasm)
RUN(NAME template_nested LABELS llvm wasm)
RUN(NAME template_travel_01 LABELS llvm wasm)
RUN(NAME template_travel_01b LABELS llvm wasm)
RUN(NAME template_travel_02 LABELS llvm wasm)
RUN(NAME template_triple LABELS llvm wasm)
RUN(NAME template_array_01 LABELS llvm llvmStackArray wasm)
RUN(NAME template_array_02 LABELS llvm llvmStackArray wasm)
RUN(NAME template_array_03 LABELS llvm llvmStackArray wasm)
RUN(NAME template_array_04 LABELS llvm llvmStackArray wasm)
RUN(NAME template_array_04b LABELS llvm llvmStackArray wasm)
RUN(NAME template_matrix_01 LABELS llvm)
RUN(NAME template_matrix_test LABELS llvm EXTRAFILES
    template_semigroup.f90
    template_semiring.f90
    template_monoid.f90
    template_unitring.f90
    template_field.f90
    template_matrix.f90
    )
RUN(NAME template_struct_01 LABELS llvm)
RUN(NAME template_vector LABELS llvm)
RUN(NAME template_simple_01 LABELS llvm wasm)
RUN(NAME template_simple_02 LABELS llvm wasm)
RUN(NAME template_simple_03 LABELS llvm wasm)
RUN(NAME template_simple_04 LABELS llvm wasm)
RUN(NAME template_sort_01 LABELS llvm)
RUN(NAME template_sort_02 LABELS llvm)
RUN(NAME template_lapack_01 LABELS llvm wasm)
RUN(NAME template_interface_01 LABELS llvm wasm)

RUN(NAME statement1 LABELS gfortran llvm)

RUN(NAME implied_do_loops1 LABELS gfortran llvm)
RUN(NAME implied_do_loops2 LABELS gfortran)
RUN(NAME implied_do_loops3 LABELS gfortran)
RUN(NAME implied_do_loops4 LABELS gfortran llvm)


RUN(NAME minpack_01 LABELS gfortran llvm_rtlib EXTRAFILES
        minpack_01_module.f90 minpack_01_func.f90 NOFAST)
RUN(NAME minpack_02 LABELS gfortran llvm)

RUN(NAME external_01 LABELS gfortran llvmImplicit)
RUN(NAME external_02 LABELS gfortran llvmImplicit)
RUN(NAME external_03 LABELS gfortran llvmImplicit)
RUN(NAME external_04 LABELS gfortran llvmImplicit)
RUN(NAME external_05 LABELS gfortran llvmImplicit)
RUN(NAME external_06 LABELS gfortran llvmImplicit)
RUN(NAME external_07 LABELS gfortran llvmImplicit)
RUN(NAME external_08 LABELS gfortran llvmImplicit)


RUN(NAME interface_12 LABELS gfortran llvm)

RUN(NAME private1 LABELS gfortran llvm)

RUN(NAME modules_26 LABELS gfortran llvm)

RUN(NAME enum_01 LABELS gfortran llvm)
RUN(NAME enum_02 LABELS gfortran llvm EXTRAFILES
         enum_02_module.f90)

RUN(NAME array_section_01 LABELS gfortran llvm llvmStackArray fortran)
RUN(NAME array_section_02 LABELS gfortran llvm llvmStackArray fortran)

RUN(NAME pass_array_by_data_01 LABELS gfortran llvm llvmStackArray wasm)
RUN(NAME pass_array_by_data_02 LABELS gfortran llvm llvmStackArray)
RUN(NAME pass_array_by_data_03 LABELS gfortran llvm llvmStackArray)
RUN(NAME pass_array_by_data_04 LABELS gfortran llvm llvmStackArray)
RUN(NAME pass_array_by_data_05 LABELS gfortran llvm llvmStackArray)
RUN(NAME pass_array_by_data_06 LABELS gfortran llvm llvmStackArray wasm)
RUN(NAME pass_array_by_data_07 LABELS gfortran llvm llvmStackArray)
RUN(NAME pass_array_by_data_08 LABELS gfortran llvm llvmStackArray)

RUN(NAME implicit_deallocate_01 LABELS gfortran llvm)

RUN(NAME write_01 LABELS gfortran llvm)
RUN(NAME write_02 LABELS gfortran llvm)
RUN(NAME write_03 LABELS gfortran llvm)
RUN(NAME write_04 LABELS gfortran llvm)
RUN(NAME write_05 LABELS gfortran llvm)
RUN(NAME write_06 LABELS gfortran llvm)

RUN(NAME do_loop_01 LABELS gfortran llvm)

RUN(NAME array_01 LABELS gfortran llvm llvmStackArray)
RUN(NAME array_02 LABELS gfortran llvm llvmStackArray wasm)

RUN(NAME do_loop_02 LABELS gfortran llvm)
RUN(NAME do_loop_03 LABELS gfortran llvm
    EXTRA_ARGS --use-loop-variable-after-loop)
RUN(NAME do_loop_04 LABELS llvm) # This test is not supported by gfortran, as it uses a loop variable after the loop ( bad code practice )

RUN(NAME complex_11 LABELS gfortran llvmImplicit wasm)
RUN(NAME complex_12 LABELS gfortran llvm wasm)
RUN(NAME complex_13 LABELS gfortran llvm wasm)
RUN(NAME complex_14 LABELS gfortran llvm wasm)
RUN(NAME complex_15 LABELS gfortran llvm fortran)
RUN(NAME complex_16 LABELS gfortran llvm)
RUN(NAME complex_17 LABELS gfortran llvm)
RUN(NAME complex_18 LABELS gfortran llvm)
RUN(NAME complex_19 LABELS gfortran llvm)

RUN(NAME array_op_01 LABELS gfortran llvm llvmStackArray wasm)
RUN(NAME array_op_02 LABELS gfortran llvm llvmStackArray wasm)
RUN(NAME array_op_03 LABELS gfortran llvm llvmStackArray wasm)
RUN(NAME array_op_04 LABELS gfortran llvm llvmStackArray)
RUN(NAME array_slice_01 LABELS gfortran llvm llvmStackArray c)
RUN(NAME array_slice_02 LABELS gfortran llvm llvmStackArray)
RUN(NAME array_slice_03 LABELS gfortran llvm llvmStackArray c)
RUN(NAME array_slice_04 LABELS gfortran llvm llvmStackArray)

RUN(NAME declaration_01 LABELS gfortran llvm)

RUN(NAME procedure_01 LABELS gfortran llvm)
RUN(NAME procedure_02 LABELS gfortran llvm)
RUN(NAME procedure_03 LABELS gfortran llvm)
RUN(NAME procedure_04 LABELS gfortran llvm)
RUN(NAME procedure_05 LABELS gfortran llvmImplicit)

RUN(NAME allocated_01 LABELS gfortran llvm)
RUN(NAME allocated_02 LABELS gfortran llvm)
RUN(NAME allocated_03 LABELS gfortran llvm)
RUN(NAME allocate_07 LABELS gfortran llvm
    EXTRA_ARGS --realloc-lhs)
RUN(NAME allocate_08 LABELS gfortran llvm)
RUN(NAME allocate_09 LABELS gfortran llvm)

RUN(NAME double_complex_01 LABELS gfortran llvm)

RUN(NAME common_02 LABELS gfortran llvm)
RUN(NAME common_03 LABELS gfortran llvm)
RUN(NAME common_04 LABELS gfortran) # TODO: Fix these tests
RUN(NAME common_05 LABELS gfortran)
RUN(NAME common_06 LABELS gfortran llvm llvmImplicit)
RUN(NAME common_07 LABELS gfortran llvmImplicit)
RUN(NAME common_08 LABELS gfortran llvmImplicit)
RUN(NAME common_09 LABELS gfortran llvm)
RUN(NAME common_10 LABELS gfortran llvm)
RUN(NAME common_11 LABELS gfortran llvm)
RUN(NAME common_12 LABELS gfortran llvm llvmImplicit)

RUN(NAME statement_01 LABELS gfortran llvmImplicit)
RUN(NAME statement_02 LABELS gfortran llvmImplicit)
RUN(NAME statement_03 LABELS gfortran llvmImplicit)

RUN(NAME data_implied_do_01 LABELS gfortran llvm)
RUN(NAME data_implied_do_02 LABELS gfortran llvm)
RUN(NAME data_implied_do_03 LABELS gfortran llvm)
RUN(NAME data_implied_do_04 LABELS gfortran llvm)

RUN(NAME save_01 LABELS gfortran llvm)
RUN(NAME save_02 LABELS gfortran llvm)
RUN(NAME save_03 LABELS gfortran llvm)
RUN(NAME save_04 LABELS gfortran llvm fortran)
RUN(NAME save_05 LABELS gfortran llvm)
RUN(NAME save_06 LABELS gfortran llvm)
RUN(NAME save_07 LABELS gfortran llvm)
RUN(NAME save_08 LABELS gfortran llvm)


RUN(NAME entry_01 LABELS gfortran llvm)
RUN(NAME entry_02 LABELS gfortran llvm)
RUN(NAME entry_03 LABELS gfortran llvm)
RUN(NAME entry_04 LABELS gfortran llvm NOFAST)
RUN(NAME entry_05 LABELS gfortran llvm)
RUN(NAME entry_06 LABELS gfortran)
RUN(NAME entry_07 LABELS gfortran llvm)
RUN(NAME entry_08 LABELS gfortran llvm)
RUN(NAME entry_09 LABELS gfortran llvm)
RUN(NAME entry_10 LABELS gfortran llvm)
RUN(NAME entry_11 LABELS gfortran llvm)
RUN(NAME entry_12 LABELS gfortran llvm)
RUN(NAME entry_13 LABELS gfortran llvm)

RUN(NAME character_01 LABELS gfortran llvm)
RUN(NAME character_02 LABELS gfortran llvm)
RUN(NAME character_03 LABELS gfortran llvm)
RUN(NAME character_04 LABELS gfortran llvm)

RUN(NAME c_ptr_01 LABELS gfortran llvm)
RUN(NAME c_ptr_02 LABELS gfortran llvm)

RUN(NAME arrayitem_01 LABELS gfortran llvm llvmStackArray)

RUN(NAME implicit_argument_casting_01 LABELS gfortran llvm
    EXTRA_ARGS --implicit-argument-casting
    GFORTRAN_ARGS -fallow-argument-mismatch)

RUN(NAME implicit_argument_casting_02 LABELS gfortran llvm
    EXTRA_ARGS --implicit-argument-casting
    GFORTRAN_ARGS -fallow-argument-mismatch)

RUN(NAME specfun_01 LABELS gfortran llvm
    EXTRA_ARGS --use-loop-variable-after-loop --implicit-typing)

RUN(NAME cpp_pre_01 LABELS gfortran llvm c
    EXTRA_ARGS --cpp
    GFORTRAN_ARGS -cpp)
RUN(NAME cpp_pre_02 LABELS gfortran llvm c wasm
    EXTRA_ARGS --cpp
    GFORTRAN_ARGS -cpp)
RUN(NAME cpp_pre_03 LABELS gfortran llvm c wasm
    EXTRA_ARGS --cpp
    GFORTRAN_ARGS -cpp)
RUN(NAME cpp_pre_04 LABELS gfortran llvm c wasm
    EXTRA_ARGS --cpp
    GFORTRAN_ARGS -cpp)

RUN(NAME dabs_01 LABELS gfortran llvmImplicit)

RUN(NAME minpack_03 LABELS gfortran llvm)

RUN(NAME optional_01 LABELS gfortran llvm)
RUN(NAME optional_02 LABELS gfortran llvm)
RUN(NAME optional_03 LABELS gfortran llvm)
RUN(NAME optional_04 LABELS gfortran llvm)

RUN(NAME end_name_match LABELS gfortran llvm)

RUN(NAME mangle_underscore_01 LABELS gfortran llvm EXTRA_ARGS --mangle-underscore --all-mangling)

RUN(NAME nan_handling_01 LABELS gfortran llvm)

RUN(NAME precision_01 LABELS gfortran llvm)
RUN(NAME precision_02 LABELS gfortran) # TODO fix this test

RUN(NAME array_unbounded_01 LABELS gfortran llvm llvmStackArray)
RUN(NAME array_unbounded_02 LABELS gfortran llvm llvmStackArray)

RUN(NAME matmul_01 LABELS gfortran llvm c llvm_nopragma c_nopragma)
RUN(NAME matmul_02 LABELS gfortran)
RUN(NAME simd_01 LABELS gfortran c llvm llvm_nopragma c_nopragma)
RUN(NAME simd_02 LABELS gfortran c llvm llvm_nopragma c_nopragma)
RUN(NAME legacy_array_sections_01 LABELS gfortran llvm llvmStackArray EXTRA_ARGS --legacy-array-sections)
RUN(NAME legacy_array_sections_02 LABELS gfortran llvm llvmStackArray EXTRA_ARGS --legacy-array-sections)
RUN(NAME legacy_array_sections_03 LABELS gfortran llvm EXTRA_ARGS --legacy-array-sections --implicit-interface)
RUN(NAME cmake_minimal_test_01 LABELS gfortran llvm)

RUN(NAME ip_01 LABELS gfortran llvm)

RUN(NAME shifta_01 LABELS gfortran llvm)
RUN(NAME shifta_02 LABELS gfortran llvm)

RUN(NAME equivalence_01 LABELS gfortran llvm)
RUN(NAME equivalence_02 LABELS gfortran llvm)
RUN(NAME equivalence_03 LABELS gfortran llvm)
RUN(NAME equivalence_04 LABELS gfortran llvm)
RUN(NAME equivalence_05 LABELS gfortran llvm)
RUN(NAME equivalence_06 LABELS gfortran llvm)
RUN(NAME equivalence_07 LABELS gfortran llvm)
RUN(NAME equivalence_08 LABELS gfortran llvm)

RUN(NAME fortran_primes_01 LABELS gfortran llvm)

RUN(NAME min_01 LABELS gfortran)

RUN(NAME goto_gototarget_return_01 LABELS gfortran llvm fortran)

RUN(NAME selected_int_kind_01 LABELS gfortran llvm)

RUN(NAME capital_01 LABELS gfortran llvmImplicit)

RUN(NAME do_concurrent_01 LABELS gfortran llvm)

RUN(NAME transfer_01 LABELS gfortran llvm)
RUN(NAME transfer_02 LABELS gfortran)

RUN(NAME present_01 LABELS gfortran llvm)
RUN(NAME present_02 LABELS gfortran llvm)
RUN(NAME types_20 LABELS gfortran llvm)
RUN(NAME arrayprint_01 LABELS gfortran llvm)
RUN(NAME print_arr_05 LABELS gfortran llvm)

RUN(NAME binop_01 LABELS gfortran llvm)

RUN(NAME dfloat_01 LABELS gfortran llvm)

RUN(NAME elemental_01 LABELS gfortran llvm)
RUN(NAME elemental_02 LABELS gfortran llvm)
RUN(NAME elemental_03 LABELS gfortran llvm)
RUN(NAME elemental_04 LABELS gfortran llvm)
RUN(NAME elemental_05 LABELS gfortran llvm)
RUN(NAME elemental_06 LABELS gfortran llvm)
RUN(NAME elemental_07 LABELS gfortran llvm)
RUN(NAME elemental_08 LABELS gfortran llvm)

RUN(NAME types_21 LABELS gfortran llvm)
RUN(NAME types_22 LABELS gfortran llvm)
RUN(NAME types_23 LABELS gfortran llvm)
RUN(NAME types_real_to_complex_cast LABELS gfortran llvm)
RUN(NAME types_real_array_to_complex_array_cast LABELS gfortran llvm)

RUN(NAME lbound_01 LABELS gfortran llvm)
