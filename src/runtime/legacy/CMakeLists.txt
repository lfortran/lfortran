set(SRC
    ../../../src/libasr/runtime/lfortran_intrinsics.c
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
set_target_properties(lfortran_runtime PROPERTIES
  VERSION ${PROJECT_VERSION}
  SOVERSION ${PROJECT_VERSION_MAJOR})
set_target_properties(lfortran_runtime PROPERTIES
  LIBRARY_OUTPUT_DIRECTORY ..)

add_library(lfortran_runtime_static STATIC ${SRC})
target_include_directories(lfortran_runtime_static BEFORE PUBLIC ${libasr_SOURCE_DIR}/..)
target_include_directories(lfortran_runtime_static BEFORE PUBLIC ${libasr_BINARY_DIR}/..)
target_link_libraries(lfortran_runtime PRIVATE ${MATH_LIBRARIES})
set_target_properties(lfortran_runtime_static PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY ..)

if(WITH_TARGET_WASM)
    if (NOT (DEFINED ENV{WASI_SDK_PATH}))
        message(FATAL_ERROR "'WASI_SDK_PATH' must be set when building with 'WITH_TARGET_WASM'")
    endif()

    add_custom_command(OUTPUT lfortran_runtime_wasm.o
        COMMAND $ENV{WASI_SDK_PATH}/bin/clang -I${libasr_SOURCE_DIR}/.. -DCOMPILE_TO_WASM -D_WASI_EMULATED_PROCESS_CLOCKS -c --target=wasm32-wasi ${SRC} -o lfortran_runtime_wasm.o
        COMMENT "Cross compiling lfortran_intrinscs.c to lfortran_runtime_wasm.o"
        DEPENDS ${SRC})
    add_custom_target(lfortran_runtime_wasm ALL DEPENDS lfortran_runtime_wasm.o)
    install(FILES lfortran_runtime_wasm.o
        DESTINATION ${CMAKE_INSTALL_LIBDIR})
    install(FILES lfortran_runtime_wasm.o
        DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/..)
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
