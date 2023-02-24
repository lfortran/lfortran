set(SRC
    ../../../src/libasr/runtime/lfortran_intrinsics.c
)

if (WITH_RUNTIME_LIBRARY)
    add_library(lfortran_runtime SHARED ${SRC})
    target_include_directories(lfortran_runtime BEFORE PUBLIC ${libasr_SOURCE_DIR}/..)
    target_include_directories(lfortran_runtime BEFORE PUBLIC ${libasr_BINARY_DIR}/..)
    set_target_properties(lfortran_runtime PROPERTIES
        LIBRARY_OUTPUT_DIRECTORY ..)
endif()

add_library(lfortran_runtime_static STATIC ${SRC})
target_include_directories(lfortran_runtime_static BEFORE PUBLIC ${libasr_SOURCE_DIR}/..)
target_include_directories(lfortran_runtime_static BEFORE PUBLIC ${libasr_BINARY_DIR}/..)
set_target_properties(lfortran_runtime_static PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY ..)

install(TARGETS lfortran_runtime_static
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    ARCHIVE DESTINATION share/lfortran/lib
    LIBRARY DESTINATION share/lfortran/lib
)

if(WITH_RUNTIME_LIBRARY)
    install(TARGETS lfortran_runtime
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
        ARCHIVE DESTINATION share/lfortran/lib
        LIBRARY DESTINATION share/lfortran/lib
    )
endif()

# Install the header
install(
    FILES ../../../src/libasr/runtime/lfortran_intrinsics.h
    DESTINATION share/lfortran/lib/impure
)
