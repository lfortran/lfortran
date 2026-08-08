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
    ADDTESTC(test_cwrapper lfortran_c)
    ADDTEST(test_stacktrace)
endif()

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
    find_program(LFORTRAN_TEST_PYTHON NAMES python python3 REQUIRED)
    # Producing an executable needs a working end-to-end link, which the
    # Windows CI job does not provide: it builds and unit tests the compiler
    # but never runs the integration suite. Parsing and verification are still
    # covered there; only the phases past object emission are skipped.
    if (WIN32)
        set(LFORTRAN_ASR_CORPUS_ARGS --skip-compile)
    else()
        set(LFORTRAN_ASR_CORPUS_ARGS "")
    endif()
    add_test(
        NAME test_asr_corpus
        COMMAND ${LFORTRAN_TEST_PYTHON}
            ${CMAKE_SOURCE_DIR}/tests/asr/run_tests.py
            --lfortran $<TARGET_FILE:lfortran>
            --manifest ${CMAKE_SOURCE_DIR}/tests/asr/tests.toml
            ${LFORTRAN_ASR_CORPUS_ARGS}
    )
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
                --artifacts ${CMAKE_BINARY_DIR}/asr-fuzz-artifacts
        )
        add_test(
            NAME test_asr_schema_generator_smoke
            COMMAND ${LFORTRAN_TEST_PYTHON}
                ${CMAKE_SOURCE_DIR}/tests/asr/fuzz.py
                --lfortran $<TARGET_FILE:lfortran>
                --seed 2
                --cases 4
                --generator schema-valid
                --artifacts ${CMAKE_BINARY_DIR}/asr-fuzz-artifacts
        )
    endif()
    add_test(
        NAME test_asr_reducer_unit
        COMMAND ${LFORTRAN_TEST_PYTHON}
            ${CMAKE_SOURCE_DIR}/tests/asr/test_edn.py
    )
    add_test(
        NAME test_asr_llvm_constructor_coverage
        COMMAND ${LFORTRAN_TEST_PYTHON}
            ${CMAKE_SOURCE_DIR}/tests/asr/check_llvm_coverage.py
            --asdl ${CMAKE_SOURCE_DIR}/src/libasr/ASR.asdl
            --llvm ${CMAKE_SOURCE_DIR}/src/libasr/codegen/asr_to_llvm.cpp
            --manifest ${CMAKE_SOURCE_DIR}/tests/asr/llvm_constructor_coverage.toml
    )
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