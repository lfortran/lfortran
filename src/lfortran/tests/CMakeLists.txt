project(tests)

macro(ADDTEST name)
    add_executable(${name} ${name}.cpp)
    target_link_libraries(${name} lfortran_lib  ${ARGN})
    add_test(${name} ${PROJECT_BINARY_DIR}/${name})
endmacro(ADDTEST)

macro(ADDTESTC name)
    add_executable(${name} ${name}.c)
    # Link only to specified libraries, not lfortran_lib which brings in C++
    # flags from xeus-zmq that are invalid for C compilation (-Wreorder)
    target_link_libraries(${name} ${ARGN})
    add_test(${name} ${PROJECT_BINARY_DIR}/${name})
endmacro(ADDTESTC)

if (NOT XEUS_LFORTRAN_WASM_BUILD)
    if (NOT EMSCRIPTEN)
        add_executable(test_jupyter_stubs test_jupyter_stubs.cpp ../jupyter.cpp)
        target_compile_definitions(test_jupyter_stubs PRIVATE __EMSCRIPTEN__)
        target_include_directories(test_jupyter_stubs PRIVATE
            ${CMAKE_SOURCE_DIR}/src ${CMAKE_BINARY_DIR}/src)
        add_test(NAME test_jupyter_stubs COMMAND test_jupyter_stubs)
    endif()
    ADDTESTC(test_cwrapper lfortran_c)
    ADDTESTC(test_hashtable)
    if (NOT EMSCRIPTEN)
        ADDTESTC(test_runtime_input lfortran_runtime_static)
        ADDTESTC(test_runtime_dtio lfortran_runtime_static)
    endif()
    if (NOT WIN32 AND NOT EMSCRIPTEN)
        ADDTESTC(test_system_clock lfortran_runtime_static)
    endif()
    # Needs a 64-bit file offset type; seeks past 2**31 without writing data.
    if (NOT EMSCRIPTEN AND (WIN32 OR CMAKE_SIZEOF_VOID_P EQUAL 8))
        ADDTESTC(test_file_seek lfortran_runtime_static)
    endif()
    ADDTEST(test_stacktrace)
endif()

add_library(cuda_link_helper_plain_fixture STATIC plain.c)
set_target_properties(cuda_link_helper_plain_fixture PROPERTIES OUTPUT_NAME plain)
set(cuda_link_helpers_fixture_header_content [=[
#pragma once
namespace cuda_link_helpers_fixture {
inline constexpr const char plain_archive[] = R"PATH($<TARGET_FILE:cuda_link_helper_plain_fixture>)PATH";
inline constexpr const char plain_member[] = "plain.c@CMAKE_C_OUTPUT_EXTENSION@";
}
]=])
string(CONFIGURE "${cuda_link_helpers_fixture_header_content}" cuda_link_helpers_fixture_header_content @ONLY)
file(GENERATE OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/$<CONFIG>/test_cuda_link_helpers_fixture.hpp" CONTENT "${cuda_link_helpers_fixture_header_content}")

set(SRC
    test_parse.cpp
    test_ast.cpp
    test_stacktrace2.cpp
    test_asm.cpp
    test_serialization.cpp
    test_pickle.cpp
    test_error_rendering.cpp
    test_asr_text.cpp
    test_asr_text_parser.cpp
    test_gpu.cpp
    ../../bin/cuda_link_helpers.cpp
    ../../tests/test_cuda_link_helpers.cpp
    ../../bin/lfortran_command_line_parser.cpp
    ../../tests/test_emscripten_options.cpp
)

if (WITH_JSON)
    set(SRC ${SRC}
        test_ast_to_json.cpp
    )
endif()
if (WITH_LLVM)
    set(SRC ${SRC}
        test_llvm.cpp
    )
endif()

if (WITH_LSP)
    set(SRC_AST ${SRC_AST}
        test_ast.cpp
    )
endif()

# For the JupyterLite kernel build we only need to exercise the evaluator path
# (FortranEvaluator -> WasmLFortranExecutor). The other tests are already
# covered by the HAVE_BUILD_TO_WASM CI job.
if (XEUS_LFORTRAN_WASM_BUILD)
    set(SRC test_llvm.cpp)
endif()

# Add one main test suite for LFortran, composed of many individual cpp files:
add_executable(test_lfortran ${SRC})
target_link_libraries(test_lfortran lfortran_lib p::doctest)
target_compile_definitions(test_lfortran PRIVATE LFORTRAN_PROJECT_SOURCE_DIR="${CMAKE_SOURCE_DIR}")
target_include_directories(test_lfortran PRIVATE "${CMAKE_CURRENT_BINARY_DIR}/$<CONFIG>")
add_dependencies(test_lfortran cuda_link_helper_plain_fixture)
# The test binary never calls set_exec_path_and_mode(), so it cannot work out
# where the runtime modfiles are; a test that uses an intrinsic module would
# fail with "modfile was not found". Under Emscripten they are preloaded into
# the virtual filesystem at /lib, the same place xlfortran reads them from.
if (EMSCRIPTEN)
    set(LFORTRAN_TEST_RUNTIME_DIR "/lib")
else()
    set(LFORTRAN_TEST_RUNTIME_DIR "${CMAKE_BINARY_DIR}/src/runtime")
endif()
target_compile_definitions(test_lfortran PRIVATE
    LFORTRAN_BUILD_RUNTIME_DIR="${LFORTRAN_TEST_RUNTIME_DIR}")
# The JIT load_library test needs a shared library whose symbol the test binary
# itself does not define, so that only the load can resolve the call. The
# archive covers the second path, where members become JIT definitions.
if (WITH_LLVM AND NOT EMSCRIPTEN)
    add_library(jit_load_plain_fixture SHARED plain.c)
    # MSVC exports nothing by default; the JIT looks the symbol up by name.
    set_target_properties(jit_load_plain_fixture PROPERTIES OUTPUT_NAME plain-jit
        WINDOWS_EXPORT_ALL_SYMBOLS ON)
    add_library(jit_load_plain_archive_fixture STATIC plain.c)
    set_target_properties(jit_load_plain_archive_fixture PROPERTIES OUTPUT_NAME plain-jit-static)
    target_compile_definitions(test_lfortran PRIVATE
        LFORTRAN_TEST_SHARED_PLAIN="$<TARGET_FILE:jit_load_plain_fixture>"
        LFORTRAN_TEST_STATIC_PLAIN="$<TARGET_FILE:jit_load_plain_archive_fixture>")
    add_dependencies(test_lfortran jit_load_plain_fixture jit_load_plain_archive_fixture)
endif()
if (HAVE_BUILD_TO_WASM)
    set(WASM_COMPILE_FLAGS "-g0 -fexceptions")
    # The default Emscripten stack is 64kb, which is far too small for the
    # recursive descent parsers and ASR visitors; use a native-sized stack.
    set(WASM_LINK_FLAGS
      "-Oz -g0 -fexceptions -Wall -Wextra -s ASSERTIONS -s ALLOW_MEMORY_GROWTH=1 -s WASM_BIGINT -s STACK_SIZE=8mb"
    )
    set_target_properties(test_lfortran PROPERTIES COMPILE_FLAGS ${WASM_COMPILE_FLAGS})
    set_target_properties(test_lfortran PROPERTIES LINK_FLAGS ${WASM_LINK_FLAGS})
endif()

if (XEUS_LFORTRAN_WASM_BUILD)
    # WasmLFortranExecutor uses dlopen — test binary must be a MAIN_MODULE.
    target_link_options(test_lfortran PRIVATE
        "SHELL:-fwasm-exceptions"
        "SHELL:-s MAIN_MODULE=1"
        "SHELL:-s ALLOW_MEMORY_GROWTH=1"
        "SHELL:-s STACK_SIZE=32mb"
        "SHELL:-s INITIAL_MEMORY=128mb"
    )
    # Preload the same .mod files that xlfortran embeds.
    file(GLOB LFORTRAN_TEST_MOD_FILES "${CMAKE_INSTALL_PREFIX}/lib/*.mod")
    foreach(MOD_FILE ${LFORTRAN_TEST_MOD_FILES})
        get_filename_component(MOD_NAME ${MOD_FILE} NAME)
        target_link_options(test_lfortran PRIVATE
            "SHELL:--preload-file \"${MOD_FILE}@/lib/${MOD_NAME}\""
        )
    endforeach()
    if(LFORTRAN_TEST_MOD_FILES)
        set_property(TARGET test_lfortran APPEND PROPERTY LINK_DEPENDS ${LFORTRAN_TEST_MOD_FILES})
    endif()
endif()
add_test(test_lfortran ${PROJECT_BINARY_DIR}/test_lfortran)

# These tests drive the lfortran executable. This directory is configured
# before src/bin, so `if (TARGET lfortran)` is not usable yet; mirror the
# condition under which src/bin creates the target instead.
if (WITH_LLVM AND (NOT EMSCRIPTEN OR LFORTRAN_BUILD_TO_WASM))
    # Every fixture under tests/asr/compile/ must link an executable. That is
    # the second half of the contract the fuzzer enforces, and it is what
    # proves the ASR was well formed enough to reach a binary. The programs
    # are deliberately not run: a generated program may fault at runtime for
    # reasons no ASR verifier could predict, so only linking is a property of
    # the ASR itself.
    file(GLOB LFORTRAN_ASR_COMPILE_FIXTURES CONFIGURE_DEPENDS
        ${CMAKE_SOURCE_DIR}/tests/asr/compile/*.asr)
    foreach(fixture ${LFORTRAN_ASR_COMPILE_FIXTURES})
        get_filename_component(fixture_name ${fixture} NAME_WE)
        # Emitting an executable needs a working end-to-end link, which the
        # Windows CI job does not provide: it builds and unit tests the
        # compiler but never runs the integration suite. Object emission is
        # still covered there by the test below.
        # `--verify-all-passes` so a pass that corrupts a graph the initial
        # verifier accepted is reported against that pass here too, and not
        # only in a build with assertions enabled.
        if (NOT WIN32)
            add_test(
                NAME test_asr_compile_${fixture_name}
                COMMAND $<TARGET_FILE:lfortran>
                    ${fixture}
                    --verify-all-passes
                    --no-color
                    -o ${PROJECT_BINARY_DIR}/${fixture_name}
            )
        endif()
        add_test(
            NAME test_asr_object_${fixture_name}
            COMMAND $<TARGET_FILE:lfortran>
                ${fixture}
                --verify-all-passes
                --no-color
                -c
                -o ${PROJECT_BINARY_DIR}/${fixture_name}.o
        )
    endforeach()
    # `--show-asm` must lower the ASR after the same passes as `-S`. Assembly
    # differs between targets, so only success is checked. The program lives
    # here because CI builds from the source tarball, which ships `src` but not
    # `integration_tests`.
    add_test(
        NAME test_show_asm_01
        COMMAND $<TARGET_FILE:lfortran>
            ${CMAKE_CURRENT_SOURCE_DIR}/show_asm_01.f90
            --show-asm
            --no-color
    )
    # The format claims to be an EDN data subset; enforce that with a reader
    # that knows nothing about LFortran.
    find_program(LFORTRAN_TEST_PYTHON NAMES python python3)
    if (LFORTRAN_TEST_PYTHON)
        if (NOT WIN32 AND NOT EMSCRIPTEN)
            add_test(
                NAME test_scoped_attribute_roundtrip
                COMMAND ${LFORTRAN_TEST_PYTHON}
                    ${CMAKE_CURRENT_SOURCE_DIR}/test_scoped_attribute_roundtrip.py
                    --lfortran $<TARGET_FILE:lfortran>
                    --workdir ${PROJECT_BINARY_DIR}/scoped-attribute-roundtrip
            )
            set_tests_properties(test_scoped_attribute_roundtrip PROPERTIES TIMEOUT 180)
            add_test(
                NAME test_gpu_launch_geometry_cpu
                COMMAND ${LFORTRAN_TEST_PYTHON}
                    ${CMAKE_CURRENT_SOURCE_DIR}/test_gpu_launch_geometry.py
                    --backend cpu --cc ${CMAKE_C_COMPILER}
                    "--sysroot=${CMAKE_OSX_SYSROOT}"
                    --workdir ${PROJECT_BINARY_DIR}/gpu-launch-geometry-cpu
            )
            set_tests_properties(test_gpu_launch_geometry_cpu PROPERTIES TIMEOUT 180)
            if (APPLE)
                add_test(
                    NAME test_gpu_launch_geometry_metal
                    COMMAND ${LFORTRAN_TEST_PYTHON}
                        ${CMAKE_CURRENT_SOURCE_DIR}/test_gpu_launch_geometry.py
                        --backend metal --cc ${CMAKE_C_COMPILER}
                        "--sysroot=${CMAKE_OSX_SYSROOT}"
                        --workdir ${PROJECT_BINARY_DIR}/gpu-launch-geometry-metal
                )
                set_tests_properties(test_gpu_launch_geometry_metal PROPERTIES TIMEOUT 300)
            endif()
        endif()
        add_test(
            NAME test_dwarf_convert
            COMMAND ${LFORTRAN_TEST_PYTHON}
                ${CMAKE_CURRENT_SOURCE_DIR}/test_dwarf_convert.py
        )
        add_test(
            NAME test_asr_text_edn_conformance
            COMMAND ${LFORTRAN_TEST_PYTHON}
                ${CMAKE_SOURCE_DIR}/tests/asr/check_edn.py
                --lfortran $<TARGET_FILE:lfortran>
                --input ${CMAKE_SOURCE_DIR}/tests/asr/compile/asr_text_compile_01.asr
                --input ${CMAKE_SOURCE_DIR}/tests/asr/edn_strings_01.f90
                --input ${CMAKE_SOURCE_DIR}/examples/expr2.f90
        )
        # The ASR node documentation goes stale silently unless something
        # reads it: this round-trips every documented example and compares
        # every documented declaration with ASR.asdl.
        add_test(
            NAME test_asr_documentation
            COMMAND ${LFORTRAN_TEST_PYTHON}
                ${CMAKE_SOURCE_DIR}/tests/asr/check_docs.py
                --lfortran $<TARGET_FILE:lfortran>
        )
        # Producing an executable is required by the fuzzer's oracle, so the
        # smoke case is skipped where the toolchain cannot link one.
        if (NOT WIN32)
            add_test(
                NAME test_asr_fuzzer_smoke
                COMMAND ${LFORTRAN_TEST_PYTHON}
                    ${CMAKE_SOURCE_DIR}/tests/asr/fuzz.py
                    --lfortran $<TARGET_FILE:lfortran>
                    --source ${CMAKE_SOURCE_DIR}/examples/expr2.f90
                    --seed 1
                    --cases 4
                    --generator mutation
                    --strategy valid
                    --artifacts ${PROJECT_BINARY_DIR}/asr-fuzz-artifacts
            )
            add_test(
                NAME test_asr_schema_generator_smoke
                COMMAND ${LFORTRAN_TEST_PYTHON}
                    ${CMAKE_SOURCE_DIR}/tests/asr/fuzz.py
                    --lfortran $<TARGET_FILE:lfortran>
                    --seed 2
                    --cases 4
                    --generator schema-valid
                    --artifacts ${PROJECT_BINARY_DIR}/asr-fuzz-artifacts
            )
        endif()
        # The invalid generators build graphs no verifier rule may accept.
        # Only initial verification runs here, so this needs no linker.
        add_test(
            NAME test_asr_schema_invalid_rejected
            COMMAND ${LFORTRAN_TEST_PYTHON}
                ${CMAKE_SOURCE_DIR}/tests/asr/fuzz.py
                --lfortran $<TARGET_FILE:lfortran>
                --seed 3
                --cases 12
                --generator schema-invalid
                --expect verify
                --artifacts ${PROJECT_BINARY_DIR}/asr-fuzz-artifacts
        )
        add_test(
            NAME test_asr_reducer_unit
            COMMAND ${LFORTRAN_TEST_PYTHON}
                ${CMAKE_SOURCE_DIR}/tests/asr/test_edn.py
        )
        # Invalid code compiled with `-c --continue-compilation` (or wrapped
        # with `pywrap`) must fail without writing an object, .mod or wrapper
        # file.
        set(CONTINUE_COMPILATION_OUTPUT_ARGS)
        if (HAVE_LFORTRAN_MLIR)
            set(CONTINUE_COMPILATION_OUTPUT_ARGS --mlir)
        endif()
        add_test(
            NAME test_continue_compilation_output
            COMMAND ${LFORTRAN_TEST_PYTHON}
                ${CMAKE_CURRENT_SOURCE_DIR}/test_continue_compilation_output.py
                --lfortran $<TARGET_FILE:lfortran>
                --workdir ${PROJECT_BINARY_DIR}/continue-compilation-output
                ${CONTINUE_COMPILATION_OUTPUT_ARGS}
        )
        add_test(
            NAME test_asr_llvm_constructor_coverage
            COMMAND ${LFORTRAN_TEST_PYTHON}
                ${CMAKE_SOURCE_DIR}/tests/asr/check_llvm_coverage.py
                --asdl ${CMAKE_SOURCE_DIR}/src/libasr/ASR.asdl
                --llvm ${CMAKE_SOURCE_DIR}/src/libasr/codegen/asr_to_llvm.cpp
                --manifest ${CMAKE_SOURCE_DIR}/tests/asr/llvm_constructor_coverage.toml
        )
        # The prompt is driven through a pipe, and on a pseudo-terminal
        # where there is one (not on Windows).
        add_test(
            NAME test_repl
            COMMAND ${LFORTRAN_TEST_PYTHON}
                ${CMAKE_SOURCE_DIR}/src/lfortran/tests/test_repl.py
                --lfortran $<TARGET_FILE:lfortran>
        )
        add_executable(prompt_events_fixture prompt_events_fixture.cpp)
        target_include_directories(prompt_events_fixture PRIVATE
            ${CMAKE_SOURCE_DIR}/src/bin/tpl)
        target_link_libraries(prompt_events_fixture lfortran_lib)
        # The prompt starts the kernel that is built: the built-in one here.
        # These cases assume that kernel: a READ whose client leaves fails,
        # and a failed cell is aborted with an idle status.
        if (NOT WITH_XEUS)
            add_test(
                NAME test_session
                COMMAND ${LFORTRAN_TEST_PYTHON}
                    ${CMAKE_SOURCE_DIR}/src/lfortran/tests/test_session.py
                    --lfortran $<TARGET_FILE:lfortran> --builtin
            )
            set_tests_properties(test_session PROPERTIES TIMEOUT 180
                SKIP_RETURN_CODE 77)
            add_test(
                NAME test_session_regressions
                COMMAND ${LFORTRAN_TEST_PYTHON}
                    ${CMAKE_SOURCE_DIR}/src/lfortran/tests/test_session_regressions.py
                    --lfortran $<TARGET_FILE:lfortran> --builtin
                    --prompt-events-fixture $<TARGET_FILE:prompt_events_fixture>
            )
            set_tests_properties(test_session_regressions PROPERTIES TIMEOUT 420
                SKIP_RETURN_CODE 77)
        else()
            # The Xeus kernel, on Xeus's own channels, which the prompt starts
            # here. The cases that assume the built-in kernel stay out.
            add_test(
                NAME test_session
                COMMAND ${LFORTRAN_TEST_PYTHON}
                    ${CMAKE_SOURCE_DIR}/src/lfortran/tests/test_session.py
                    --lfortran $<TARGET_FILE:lfortran>
            )
            set_tests_properties(test_session PROPERTIES TIMEOUT 180
                SKIP_RETURN_CODE 77)
            add_test(
                NAME test_session_regressions
                COMMAND ${LFORTRAN_TEST_PYTHON}
                    ${CMAKE_SOURCE_DIR}/src/lfortran/tests/test_session_regressions.py
                    --lfortran $<TARGET_FILE:lfortran>
                    --case xeus_connection_files --case implicit_typing
                    --case prompt_departure --case prompt_killed_in_cell
                    --case kernel_parent --case stop_ends_session --case verbose
                    --case test_idle_updates --case test_edited_updates
                    --case test_fragmented_updates --case test_idle_disconnect
                    --case test_unicode_updates --case test_output_error_cleanup
                    --prompt-events-fixture $<TARGET_FILE:prompt_events_fixture>
            )
            set_tests_properties(test_session_regressions PROPERTIES TIMEOUT 180
                SKIP_RETURN_CODE 77)
        endif()
        # The Xeus kernel, or else the built-in one
        if (NOT EMSCRIPTEN)
            add_test(
                NAME test_kernel_evaluation
                COMMAND ${LFORTRAN_TEST_PYTHON}
                    ${CMAKE_CURRENT_SOURCE_DIR}/test_kernel_evaluation.py
                    --lfortran $<TARGET_FILE:lfortran>
            )
            set_tests_properties(test_kernel_evaluation PROPERTIES TIMEOUT 180
                SKIP_RETURN_CODE 77)
        endif()
    endif()
    if (CMAKE_SYSTEM_NAME STREQUAL "Linux"
        OR CMAKE_SYSTEM_NAME STREQUAL "FreeBSD"
        OR CMAKE_SYSTEM_NAME STREQUAL "OpenBSD")

        target_link_options(test_lfortran PRIVATE "LINKER:--export-dynamic")
    endif()
endif()

if (WITH_LSP)
    add_executable(test_asr ${SRC_AST})
    target_link_libraries(test_asr lfortran_lib p::doctest)
    add_test(test_asr ${PROJECT_BINARY_DIR}/test_asr)
    if (CMAKE_SYSTEM_NAME STREQUAL "Linux"
        OR CMAKE_SYSTEM_NAME STREQUAL "FreeBSD"
        OR CMAKE_SYSTEM_NAME STREQUAL "OpenBSD")
        target_link_options(test_asr PRIVATE "LINKER:--export-dynamic")
    endif()
endif()

if (APPLE)
    add_test(NAME test_gpu_metal_runtime
        COMMAND bash ${CMAKE_CURRENT_SOURCE_DIR}/test_gpu_metal_runtime.sh)
    set_tests_properties(test_gpu_metal_runtime PROPERTIES TIMEOUT 180)
endif()
