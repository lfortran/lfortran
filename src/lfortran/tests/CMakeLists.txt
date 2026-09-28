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

# The startup initialization engine of the runtime, one process per scenario;
# see test_init_engine.c.
if (NOT XEUS_LFORTRAN_WASM_BUILD AND NOT EMSCRIPTEN)
    add_executable(test_init_engine test_init_engine.c)
    target_link_libraries(test_init_engine lfortran_runtime_static)
    set(init_engine_scenarios order_forward order_reverse republish host_startup
        program_start collective
        publish_during_init publish_twice remove_during_init remove_in_teardown
        unload_in_teardown bootstrap_incarnation)
    if (NOT WIN32)
        find_package(Threads REQUIRED)
        target_link_libraries(test_init_engine Threads::Threads)
        list(APPEND init_engine_scenarios concurrent remove_concurrent
            withdraw_unrelated bootstrap bootstrap_local bootstrap_shared)
    endif()
    foreach(s ${init_engine_scenarios})
        add_test(NAME test_init_engine_${s} COMMAND test_init_engine ${s})
        # A lock-order bug hangs rather than fails.
        set_tests_properties(test_init_engine_${s} PROPERTIES TIMEOUT 60)
    endforeach()
    # These end the process, which has to exit with a nonzero status and the
    # engine's diagnostic; see run_fatal_test.cmake.
    add_test(NAME test_init_engine_cycle COMMAND ${CMAKE_COMMAND}
        -DPROGRAM=$<TARGET_FILE:test_init_engine> -DSCENARIO=cycle
        "-DEXPECT=startup initialization: .*depends on itself"
        -P ${CMAKE_CURRENT_SOURCE_DIR}/run_fatal_test.cmake)
    add_test(NAME test_init_engine_collective_outside COMMAND ${CMAKE_COMMAND}
        -DPROGRAM=$<TARGET_FILE:test_init_engine> -DSCENARIO=collective_outside
        "-DEXPECT=startup initialization: .*collective startup boundary"
        -P ${CMAKE_CURRENT_SOURCE_DIR}/run_fatal_test.cmake)
    add_test(NAME test_init_engine_malformed_record COMMAND ${CMAKE_COMMAND}
        -DPROGRAM=$<TARGET_FILE:test_init_engine> -DSCENARIO=malformed_record
        "-DEXPECT=startup initialization: .*malformed"
        -P ${CMAKE_CURRENT_SOURCE_DIR}/run_fatal_test.cmake)
    add_test(NAME test_init_engine_malformed_bootstrap COMMAND ${CMAKE_COMMAND}
        -DPROGRAM=$<TARGET_FILE:test_init_engine> -DSCENARIO=malformed_bootstrap
        "-DEXPECT=startup initialization: .*malformed"
        -P ${CMAKE_CURRENT_SOURCE_DIR}/run_fatal_test.cmake)
    add_test(NAME test_init_engine_both_flags COMMAND ${CMAKE_COMMAND}
        -DPROGRAM=$<TARGET_FILE:test_init_engine> -DSCENARIO=both_flags
        "-DEXPECT=startup initialization: .*malformed"
        -P ${CMAKE_CURRENT_SOURCE_DIR}/run_fatal_test.cmake)
    add_test(NAME test_init_engine_old_abi COMMAND ${CMAKE_COMMAND}
        -DPROGRAM=$<TARGET_FILE:test_init_engine> -DSCENARIO=old_abi
        "-DEXPECT=startup initialization: a batch was compiled for another version"
        -P ${CMAKE_CURRENT_SOURCE_DIR}/run_fatal_test.cmake)
    add_test(NAME test_init_engine_unknown_phase COMMAND ${CMAKE_COMMAND}
        -DPROGRAM=$<TARGET_FILE:test_init_engine> -DSCENARIO=unknown_phase
        "-DEXPECT=startup initialization: unknown dispatch phase"
        -P ${CMAKE_CURRENT_SOURCE_DIR}/run_fatal_test.cmake)
    add_test(NAME test_init_engine_duplicate_collective COMMAND ${CMAKE_COMMAND}
        -DPROGRAM=$<TARGET_FILE:test_init_engine> -DSCENARIO=duplicate_collective
        "-DEXPECT=startup initialization: two collective initializers have the stable id 'm:dup'"
        -P ${CMAKE_CURRENT_SOURCE_DIR}/run_fatal_test.cmake)
    if (NOT WIN32)
        add_test(NAME test_init_engine_collective_twice COMMAND ${CMAKE_COMMAND}
            -DPROGRAM=$<TARGET_FILE:test_init_engine> -DSCENARIO=collective_twice
            "-DEXPECT=startup initialization: two threads entered the collective startup boundary at once"
            -P ${CMAKE_CURRENT_SOURCE_DIR}/run_fatal_test.cmake)
        add_test(NAME test_init_engine_bootstrap_collective_work COMMAND ${CMAKE_COMMAND}
            -DPROGRAM=$<TARGET_FILE:test_init_engine> -DSCENARIO=bootstrap_collective_work
            "-DEXPECT=startup initialization: .*collective startup boundary"
            -P ${CMAKE_CURRENT_SOURCE_DIR}/run_fatal_test.cmake)
        add_test(NAME test_init_engine_remove_inside_while_pinned COMMAND ${CMAKE_COMMAND}
            -DPROGRAM=$<TARGET_FILE:test_init_engine> -DSCENARIO=remove_inside_while_pinned
            "-DEXPECT=startup initialization: records were withdrawn from inside an initializer"
            -P ${CMAKE_CURRENT_SOURCE_DIR}/run_fatal_test.cmake)
    endif()
    # Discovery of the records of every loaded image; see test_init_native.c.
    if (NOT WIN32)
        if (APPLE)
            set(init_native_strip -Wl,-dead_strip)
        else()
            set(init_native_strip -Wl,--gc-sections)
        endif()
        add_executable(test_init_native_ab test_init_native.c
            test_init_native_a.c test_init_native_b.c)
        add_executable(test_init_native_ba test_init_native.c
            test_init_native_b.c test_init_native_a.c)
        foreach(t test_init_native_ab test_init_native_ba)
            target_link_libraries(${t} lfortran_runtime_static ${init_native_strip})
            add_test(NAME ${t} COMMAND ${t})
        endforeach()
        # ELF linkers lay notes out differently; the tests that read them are
        # linked with lld too, if the C compiler can use it.
        set(init_linkers "")
        if (NOT APPLE)
            include(CheckCSourceCompiles)
            set(CMAKE_REQUIRED_FLAGS -fuse-ld=lld)
            check_c_source_compiles("int main(void) { return 0; }" HAVE_FUSE_LD_LLD)
            unset(CMAKE_REQUIRED_FLAGS)
            if (HAVE_FUSE_LD_LLD)
                list(APPEND init_linkers lld)
            endif()
        endif()
        foreach(l ${init_linkers})
            add_executable(test_init_native_ab_${l} test_init_native.c
                test_init_native_a.c test_init_native_b.c)
            add_executable(test_init_native_ba_${l} test_init_native.c
                test_init_native_b.c test_init_native_a.c)
            foreach(t test_init_native_ab_${l} test_init_native_ba_${l})
                target_link_libraries(${t} lfortran_runtime_static ${init_native_strip}
                    -fuse-ld=${l})
                add_test(NAME ${t} COMMAND ${t})
            endforeach()
        endforeach()
        # The program and the library it loads share the engine, so both use
        # the shared runtime; the library calls back into the program.
        add_library(test_init_native_c SHARED test_init_native_c.c)
        target_link_libraries(test_init_native_c lfortran_runtime)
        add_executable(test_init_native_dlopen test_init_native_dlopen.c
            test_init_native_a.c)
        set_target_properties(test_init_native_dlopen PROPERTIES ENABLE_EXPORTS ON)
        target_link_libraries(test_init_native_dlopen lfortran_runtime ${CMAKE_DL_LIBS})
        if (APPLE)
            target_link_libraries(test_init_native_c -Wl,-undefined,dynamic_lookup)
        endif()
        add_dependencies(test_init_native_dlopen test_init_native_c)
        add_test(NAME test_init_native_dlopen COMMAND test_init_native_dlopen
            $<TARGET_FILE:test_init_native_c>)
        foreach(l ${init_linkers})
            add_executable(test_init_native_dlopen_${l} test_init_native_dlopen.c
                test_init_native_a.c)
            set_target_properties(test_init_native_dlopen_${l} PROPERTIES
                ENABLE_EXPORTS ON)
            target_link_libraries(test_init_native_dlopen_${l} lfortran_runtime
                ${CMAKE_DL_LIBS} -fuse-ld=${l})
            add_dependencies(test_init_native_dlopen_${l} test_init_native_c)
            add_test(NAME test_init_native_dlopen_${l} COMMAND test_init_native_dlopen_${l}
                $<TARGET_FILE:test_init_native_c>)
        endforeach()
        # Two shared libraries the program is linked with, the second
        # needing the first; see test_init_native_order.c.
        add_library(test_init_native_la SHARED test_init_native_la.c)
        target_link_libraries(test_init_native_la lfortran_runtime)
        add_library(test_init_native_lb SHARED test_init_native_lb.c)
        target_link_libraries(test_init_native_lb test_init_native_la lfortran_runtime)
        add_executable(test_init_native_order test_init_native_order.c)
        target_link_libraries(test_init_native_order test_init_native_lb
            test_init_native_la)
        add_test(NAME test_init_native_order COMMAND test_init_native_order)
        # Loader events racing the engine; see test_init_native_race.c.
        foreach(l test_init_native_d test_init_native_bad)
            add_library(${l} SHARED ${l}.c)
            target_link_libraries(${l} lfortran_runtime)
            if (APPLE)
                target_link_libraries(${l} -Wl,-undefined,dynamic_lookup)
            endif()
        endforeach()
        add_executable(test_init_native_race test_init_native_race.c)
        set_target_properties(test_init_native_race PROPERTIES ENABLE_EXPORTS ON)
        target_link_libraries(test_init_native_race lfortran_runtime
            Threads::Threads ${CMAKE_DL_LIBS})
        add_dependencies(test_init_native_race test_init_native_d test_init_native_bad)
        # A load at the address of an unloaded one, initialized before its
        # own constructor runs; see test_init_native_reload.c.
        add_library(test_init_native_f SHARED test_init_native_f.c)
        add_library(test_init_native_e SHARED test_init_native_e.c)
        target_link_libraries(test_init_native_f lfortran_runtime)
        target_link_libraries(test_init_native_e test_init_native_f lfortran_runtime)
        if (APPLE)
            target_link_libraries(test_init_native_f -Wl,-undefined,dynamic_lookup)
            target_link_libraries(test_init_native_e -Wl,-undefined,dynamic_lookup)
        endif()
        add_executable(test_init_native_reload test_init_native_reload.c)
        set_target_properties(test_init_native_reload PROPERTIES ENABLE_EXPORTS ON)
        target_link_libraries(test_init_native_reload lfortran_runtime ${CMAKE_DL_LIBS})
        add_dependencies(test_init_native_reload test_init_native_e)
        add_test(NAME test_init_native_reload COMMAND test_init_native_reload
            $<TARGET_FILE:test_init_native_e>)
        set_tests_properties(test_init_native_reload PROPERTIES SKIP_RETURN_CODE 77)
        # A malformed table found by the scan ends the process; see
        # test_init_native_malformed.c.
        add_library(test_init_native_malformed SHARED test_init_native_malformed.c)
        target_link_libraries(test_init_native_malformed lfortran_runtime)
        add_executable(test_init_native_malformed_load test_init_native_malformed_load.c)
        target_link_libraries(test_init_native_malformed_load lfortran_runtime
            ${CMAKE_DL_LIBS})
        add_dependencies(test_init_native_malformed_load test_init_native_malformed)
        add_test(NAME test_init_native_malformed COMMAND ${CMAKE_COMMAND}
            -DPROGRAM=$<TARGET_FILE:test_init_native_malformed_load>
            -DSCENARIO=$<TARGET_FILE:test_init_native_malformed>
            "-DEXPECT=startup initialization: an initialization table of an image is malformed"
            -P ${CMAKE_CURRENT_SOURCE_DIR}/run_fatal_test.cmake)
        set_tests_properties(test_init_native_malformed PROPERTIES TIMEOUT 60)
        # glibc lists images of loads in progress; see test_init_native_barrier.c.
        # Other C libraries (musl) do not, which the test cannot observe.
        include(CheckCSourceCompiles)
        check_c_source_compiles("#include <features.h>
#ifndef __GLIBC__
#error not glibc
#endif
int main(void) { return 0; }" HAVE_GLIBC)
        if (HAVE_GLIBC)
            add_executable(test_init_native_barrier test_init_native_barrier.c)
            target_link_libraries(test_init_native_barrier lfortran_runtime
                Threads::Threads ${CMAKE_DL_LIBS})
            add_dependencies(test_init_native_barrier test_init_native_bad)
            add_test(NAME test_init_native_barrier COMMAND test_init_native_barrier
                $<TARGET_FILE:test_init_native_bad>)
            set_tests_properties(test_init_native_barrier PROPERTIES TIMEOUT 60)
        endif()
        # What --show-llvm prints, built into programs by other tools and by
        # LFortran from IR; see run_show_llvm_test.cmake.
        set(init_llc ${LLVM_TOOLS_BINARY_DIR}/llc${CMAKE_EXECUTABLE_SUFFIX})
        set(init_llvm_link ${LLVM_TOOLS_BINARY_DIR}/llvm-link${CMAKE_EXECUTABLE_SUFFIX})
        if (WITH_LLVM AND EXISTS ${init_llc})
            if (NOT EXISTS ${init_llvm_link})
                set(init_llvm_link "")
            endif()
            # The script runs the C compiler itself, outside of the rules
            # through which CMake passes it the macOS SDK, which a compiler
            # of an Xcode toolchain does not find on its own.
            set(init_cc_flags "")
            if (APPLE)
                set(init_sysroot "${CMAKE_OSX_SYSROOT}")
                if (NOT IS_ABSOLUTE "${init_sysroot}")
                    set(init_sdk "")
                    if (init_sysroot)
                        set(init_sdk --sdk ${init_sysroot})
                    endif()
                    execute_process(COMMAND xcrun ${init_sdk} --show-sdk-path
                        OUTPUT_VARIABLE init_sysroot
                        OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_QUIET)
                endif()
                if (init_sysroot)
                    list(APPEND init_cc_flags -isysroot ${init_sysroot})
                endif()
            endif()
            add_test(NAME test_init_show_llvm COMMAND ${CMAKE_COMMAND}
                -DLFORTRAN=$<TARGET_FILE:lfortran> -DLLC=${init_llc}
                -DLLVM_LINK=${init_llvm_link} -DCC=${CMAKE_C_COMPILER}
                "-DCC_FLAGS=${init_cc_flags}"
                "-DLINKERS=${init_linkers}"
                -DRUNTIME=$<TARGET_FILE_DIR:lfortran_runtime>
                -DSRC=${CMAKE_CURRENT_SOURCE_DIR}
                -DCTOR_MAIN=${CMAKE_CURRENT_SOURCE_DIR}/test_init_ctor.c
                -DWORK=${CMAKE_CURRENT_BINARY_DIR}/test_init_show_llvm
                -P ${CMAKE_CURRENT_SOURCE_DIR}/run_show_llvm_test.cmake)
            set_tests_properties(test_init_show_llvm PROPERTIES TIMEOUT 300)
        endif()
        # The notes the C backend emits, compiled by GCC and by Clang where
        # they are there; see run_c_note_test.cmake. `lfortran -c` needs the
        # LLVM backend.
        if (NOT APPLE AND WITH_LLVM)
            set(init_c_compilers ${CMAKE_C_COMPILER})
            foreach(c gcc clang)
                find_program(init_${c} ${c})
                if (init_${c})
                    list(APPEND init_c_compilers ${init_${c}})
                endif()
            endforeach()
            list(REMOVE_DUPLICATES init_c_compilers)
            add_test(NAME test_init_c_note COMMAND ${CMAKE_COMMAND}
                -DLFORTRAN=$<TARGET_FILE:lfortran>
                "-DCOMPILERS=${init_c_compilers}" "-DLINKERS=${init_linkers}"
                -DINCLUDE=${CMAKE_SOURCE_DIR}/src/libasr/runtime
                -DRUNTIME=$<TARGET_FILE_DIR:lfortran_runtime>
                -DSRC=${CMAKE_CURRENT_SOURCE_DIR}
                -DWORK=${CMAKE_CURRENT_BINARY_DIR}/test_init_c_note
                -P ${CMAKE_CURRENT_SOURCE_DIR}/run_c_note_test.cmake)
            set_tests_properties(test_init_c_note PROPERTIES TIMEOUT 300)
        endif()
        # The host startup of a C program with a Fortran library it loads,
        # called from the library's C constructor too; see
        # run_host_load_test.cmake.
        if (NOT APPLE AND WITH_LLVM)
            add_test(NAME test_init_host_load COMMAND ${CMAKE_COMMAND}
                -DLFORTRAN=$<TARGET_FILE:lfortran> -DCC=${CMAKE_C_COMPILER}
                "-DLINKERS=${init_linkers}"
                -DINCLUDE=${CMAKE_SOURCE_DIR}/src/libasr/runtime
                -DRUNTIME=$<TARGET_FILE_DIR:lfortran_runtime>
                -DSRC=${CMAKE_CURRENT_SOURCE_DIR}
                -DWORK=${CMAKE_CURRENT_BINARY_DIR}/test_init_host_load
                -P ${CMAKE_CURRENT_SOURCE_DIR}/run_host_load_test.cmake)
            set_tests_properties(test_init_host_load PROPERTIES TIMEOUT 300)
        endif()
        foreach(s failed_load teardown_race reload_during_walk first_registration)
            add_test(NAME test_init_native_race_${s} COMMAND test_init_native_race
                ${s} $<TARGET_FILE:test_init_native_d>
                $<TARGET_FILE:test_init_native_bad>)
            set_tests_properties(test_init_native_race_${s} PROPERTIES TIMEOUT 120)
        endforeach()
    endif()
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
    test_gpu.cpp
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
    # The format claims to be an EDN data subset; enforce that with a reader
    # that knows nothing about LFortran.
    find_program(LFORTRAN_TEST_PYTHON NAMES python python3)
    if (LFORTRAN_TEST_PYTHON)
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
        add_test(
            NAME test_asr_llvm_constructor_coverage
            COMMAND ${LFORTRAN_TEST_PYTHON}
                ${CMAKE_SOURCE_DIR}/tests/asr/check_llvm_coverage.py
                --asdl ${CMAKE_SOURCE_DIR}/src/libasr/ASR.asdl
                --llvm ${CMAKE_SOURCE_DIR}/src/libasr/codegen/asr_to_llvm.cpp
                --manifest ${CMAKE_SOURCE_DIR}/tests/asr/llvm_constructor_coverage.toml
        )
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