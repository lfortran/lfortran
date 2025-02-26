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
target_link_libraries(lfortran_runtime PUBLIC lf::base PRIVATE ${MATH_LIBRARIES})
set_target_properties(lfortran_runtime PROPERTIES
  VERSION ${PROJECT_VERSION}
  SOVERSION ${PROJECT_VERSION_MAJOR})
set_target_properties(lfortran_runtime PROPERTIES
  LIBRARY_OUTPUT_DIRECTORY ..)

add_library(lfortran_runtime_static STATIC ${SRC})
target_include_directories(lfortran_runtime_static BEFORE PUBLIC ${libasr_SOURCE_DIR}/..)
target_include_directories(lfortran_runtime_static BEFORE PUBLIC ${libasr_BINARY_DIR}/..)
target_link_libraries(lfortran_runtime_static PUBLIC lf::base PRIVATE ${MATH_LIBRARIES})
set_target_properties(lfortran_runtime_static PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY ..)

if(WITH_TARGET_WASM)

    if ((NOT (DEFINED ENV{EMSDK_PATH})) AND (NOT (DEFINED ENV{WASI_SDK_PATH})))
        message(FATAL_ERROR "Atleast one of `EMSDK_PATH` or `WASI_SDK_PATH` must be available to target `WASM`")
    endif()

    if (DEFINED ENV{EMSDK_PATH})
        add_custom_command(OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/lfortran_runtime_wasm_emcc.o
            COMMAND $ENV{EMSDK_PATH}/upstream/emscripten/emcc -I${libasr_SOURCE_DIR}/.. -DCOMPILE_TO_WASM
                -c --target=wasm32-unknown-emscripten ${SRC} -o ${CMAKE_CURRENT_SOURCE_DIR}/lfortran_runtime_wasm_emcc.o
            COMMENT "Cross compiling lfortran_intrinscs.c to ${CMAKE_CURRENT_SOURCE_DIR}/lfortran_runtime_wasm_emcc.o"
            DEPENDS ${SRC})
        add_custom_target(lfortran_runtime_wasm_emcc ALL DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/lfortran_runtime_wasm_emcc.o)
        install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/lfortran_runtime_wasm_emcc.o
            DESTINATION ${CMAKE_INSTALL_LIBDIR})
        install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/lfortran_runtime_wasm_emcc.o
            DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/..)
    endif()

    if (DEFINED ENV{WASI_SDK_PATH})
        add_custom_command(OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/lfortran_runtime_wasm_wasi.o
            COMMAND $ENV{WASI_SDK_PATH}/bin/clang -I${libasr_SOURCE_DIR}/.. -DCOMPILE_TO_WASM
                -D_WASI_EMULATED_PROCESS_CLOCKS -c --target=wasm32-wasi ${SRC} -o ${CMAKE_CURRENT_SOURCE_DIR}/lfortran_runtime_wasm_wasi.o
            COMMENT "Cross compiling lfortran_intrinscs.c to ${CMAKE_CURRENT_SOURCE_DIR}/lfortran_runtime_wasm_wasi.o"
            DEPENDS ${SRC})
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
