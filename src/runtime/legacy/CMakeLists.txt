set(SRC
    ../../../src/libasr/runtime/lfortran_intrinsics.c
)

if (WITH_RUNTIME_LIBRARY OR WITH_RUNTIME_LIBRARY_ONLY)
    add_library(lfortran_runtime SHARED ${SRC})
    set_target_properties(lfortran_runtime PROPERTIES
        LIBRARY_OUTPUT_DIRECTORY ..)
endif()

add_library(lfortran_runtime_static STATIC ${SRC})
set_target_properties(lfortran_runtime_static PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY ..)

install(TARGETS lfortran_runtime_static
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    ARCHIVE DESTINATION share/lfortran/lib
    LIBRARY DESTINATION share/lfortran/lib
)

if(WITH_RUNTIME_LIBRARY OR WITH_RUNTIME_LIBRARY_ONLY)
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
