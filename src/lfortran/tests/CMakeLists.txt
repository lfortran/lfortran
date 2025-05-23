project(tests)

macro(ADDTEST name)
    add_executable(${name} ${name}.cpp)
    target_link_libraries(${name} lfortran_lib  ${ARGN})
    add_test(${name} ${PROJECT_BINARY_DIR}/${name})
endmacro(ADDTEST)

macro(ADDTESTC name)
    add_executable(${name} ${name}.c)
    target_link_libraries(${name} lfortran_lib  ${ARGN})
    add_test(${name} ${PROJECT_BINARY_DIR}/${name})
endmacro(ADDTESTC)

ADDTESTC(test_cwrapper lfortran_c)
ADDTEST(test_stacktrace)

set(SRC
    test_parse.cpp
    test_ast.cpp
    test_stacktrace2.cpp
    test_asm.cpp
    test_serialization.cpp
    test_pickle.cpp
    test_error_rendering.cpp
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


# Add one main test suite for LFortran, composed of many individual cpp files:
add_executable(test_lfortran ${SRC})
target_link_libraries(test_lfortran lfortran_lib p::doctest)
target_compile_definitions(test_lfortran PRIVATE LFORTRAN_PROJECT_SOURCE_DIR="${CMAKE_SOURCE_DIR}")
if (HAVE_BUILD_TO_WASM)
    set(WASM_COMPILE_FLAGS "-g0 -fexceptions")
    set(WASM_LINK_FLAGS
      "-Oz -g0 -fexceptions -Wall -Wextra -s ASSERTIONS -s ALLOW_MEMORY_GROWTH=1 -s WASM_BIGINT"
    )
    set_target_properties(test_lfortran PROPERTIES COMPILE_FLAGS ${WASM_COMPILE_FLAGS})
    set_target_properties(test_lfortran PROPERTIES LINK_FLAGS ${WASM_LINK_FLAGS})
endif()
add_test(test_lfortran ${PROJECT_BINARY_DIR}/test_lfortran)

if (WITH_LLVM)
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
