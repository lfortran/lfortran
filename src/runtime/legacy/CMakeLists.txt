set(SRC
    ${CMAKE_CURRENT_SOURCE_DIR}/../../../src/libasr/runtime/lfortran_intrinsics.c
)

if(WIN32)
  set(MATH_LIBRARIES "" CACHE STRING "math library")
else()
  set(MATH_LIBRARIES "m" CACHE STRING "math library")
endif()
mark_as_advanced( MATH_LIBRARIES )

add_library(lfortran_runtime SHARED ${SRC})
target_include_directories(lfortran_runtime BEFORE PUBLIC ${libasr_SOURCE_DIR}/..)
target_include_directories(lfortran_runtime BEFORE PUBLIC ${libasr_BINARY_DIR}/..)
target_link_libraries(lfortran_runtime PRIVATE ${MATH_LIBRARIES})
if (HAVE_INTERNAL_ALLOC_CHECK)
    target_compile_definitions(lfortran_runtime PRIVATE LFORTRAN_INTERNAL_ALLOC_CHECK)
endif()
set_target_properties(lfortran_runtime PROPERTIES
  VERSION ${PROJECT_VERSION}
  SOVERSION ${PROJECT_VERSION_MAJOR})
set_target_properties(lfortran_runtime PROPERTIES
  LIBRARY_OUTPUT_DIRECTORY ..)

add_library(lfortran_runtime_static STATIC ${SRC})
target_include_directories(lfortran_runtime_static BEFORE PUBLIC ${libasr_SOURCE_DIR}/..)
target_include_directories(lfortran_runtime_static BEFORE PUBLIC ${libasr_BINARY_DIR}/..)
target_link_libraries(lfortran_runtime_static PRIVATE ${MATH_LIBRARIES})
if (HAVE_INTERNAL_ALLOC_CHECK)
    target_compile_definitions(lfortran_runtime_static PRIVATE LFORTRAN_INTERNAL_ALLOC_CHECK)
endif()
set_target_properties(lfortran_runtime_static PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY ..)

if(WITH_TARGET_WASM)

    # Locate the emcc driver for the emscripten runtime object, using the
    # same precedence as the lfortran link step: an explicit LFORTRAN_EMCC,
    # then the emsdk checkout layout from EMSDK_PATH, then plain `emcc`
    # from $PATH (Homebrew, apt, pip installs).
    set(LFORTRAN_EMCC_CANDIDATE "")
    if (DEFINED ENV{LFORTRAN_EMCC} AND NOT "$ENV{LFORTRAN_EMCC}" STREQUAL "")
        set(LFORTRAN_EMCC_CANDIDATE "$ENV{LFORTRAN_EMCC}")
    elseif (DEFINED ENV{EMSDK_PATH} AND NOT "$ENV{EMSDK_PATH}" STREQUAL "")
        set(LFORTRAN_EMCC_CANDIDATE "$ENV{EMSDK_PATH}/upstream/emscripten/emcc")
    else()
        find_program(EMCC_FROM_PATH emcc)
        if (EMCC_FROM_PATH)
            set(LFORTRAN_EMCC_CANDIDATE "${EMCC_FROM_PATH}")
        endif()
    endif()

    if ((NOT LFORTRAN_EMCC_CANDIDATE) AND (NOT (DEFINED ENV{WASI_SDK_PATH})))
        message(FATAL_ERROR "At least one of `emcc` (via LFORTRAN_EMCC, EMSDK_PATH or PATH) or `WASI_SDK_PATH` must be available to target `WASM`")
    endif()

    if (LFORTRAN_EMCC_CANDIDATE)
        add_custom_command(OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/lfortran_runtime_wasm_emcc.o
            COMMAND ${LFORTRAN_EMCC_CANDIDATE} -I${libasr_SOURCE_DIR}/.. -I${libasr_BINARY_DIR}/.. -DCOMPILE_TO_WASM
                -c --target=wasm32-unknown-emscripten ${SRC} -o ${CMAKE_CURRENT_SOURCE_DIR}/lfortran_runtime_wasm_emcc.o
            COMMENT "Cross compiling lfortran_intrinscs.c to ${CMAKE_CURRENT_SOURCE_DIR}/lfortran_runtime_wasm_emcc.o"
            DEPENDS ${SRC} ${libasr_BINARY_DIR}/config.h)
        add_custom_target(lfortran_runtime_wasm_emcc ALL DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/lfortran_runtime_wasm_emcc.o)
        install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/lfortran_runtime_wasm_emcc.o
            DESTINATION ${CMAKE_INSTALL_LIBDIR})
        install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/lfortran_runtime_wasm_emcc.o
            DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/..)
    endif()

    if (DEFINED ENV{WASI_SDK_PATH})
        add_custom_command(OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/lfortran_runtime_wasm_wasi.o
            COMMAND $ENV{WASI_SDK_PATH}/bin/clang -I${libasr_SOURCE_DIR}/.. -I${libasr_BINARY_DIR}/.. -DCOMPILE_TO_WASM
                -D_WASI_EMULATED_PROCESS_CLOCKS -c --target=wasm32-wasi ${SRC} -o ${CMAKE_CURRENT_SOURCE_DIR}/lfortran_runtime_wasm_wasi.o
            COMMENT "Cross compiling lfortran_intrinscs.c to ${CMAKE_CURRENT_SOURCE_DIR}/lfortran_runtime_wasm_wasi.o"
            DEPENDS ${SRC} ${libasr_BINARY_DIR}/config.h)
        add_custom_target(lfortran_runtime_wasm_wasi ALL DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/lfortran_runtime_wasm_wasi.o)
        install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/lfortran_runtime_wasm_wasi.o
            DESTINATION ${CMAKE_INSTALL_LIBDIR})
        install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/lfortran_runtime_wasm_wasi.o
            DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/..)
    endif()
endif()

install(TARGETS lfortran_runtime_static
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
)

install(TARGETS lfortran_runtime
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
)

# Install the header
install(
    FILES ../../../src/libasr/runtime/lfortran_intrinsics.h
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/lfortran/impure
)

install(
    FILES ../../../src/libasr/runtime/ISO_Fortran_binding.h
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/lfortran
)
