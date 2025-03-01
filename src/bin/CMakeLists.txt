set(LFORTRAN_SRC
    lfortran_command_line_parser.cpp
    lfortran.cpp
)
set(LFORTRAN_LINK_LIBRARIES
    lfortran_lib
)

if (WITH_LSP)
    add_compile_definitions(WITH_LSP)
    set(LFORTRAN_SRC
        lfortran_lsp_config.cpp
        lfortran_lsp_language_server.cpp
        language_server_interface.cpp
        ${LFORTRAN_SRC}
    )
    set(LFORTRAN_LINK_LIBRARIES
        ${LFORTRAN_LINK_LIBRARIES}
        llanguage_server
    )
endif()

set(LFORTRAN_SRC
    lfortran_accessor.cpp
    lsp_cli.cpp
    ${LFORTRAN_SRC}
)

add_executable(lfortran ${LFORTRAN_SRC})
target_include_directories(lfortran PRIVATE "tpl")
target_link_libraries(lfortran ${LFORTRAN_LINK_LIBRARIES})
if (LFORTRAN_STATIC_BIN)
    if (CMAKE_SYSTEM_NAME STREQUAL "Linux"
        OR CMAKE_SYSTEM_NAME STREQUAL "FreeBSD"
        OR CMAKE_SYSTEM_NAME STREQUAL "OpenBSD")

        # Link statically on Linux with gcc or clang
        if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU" OR
                CMAKE_CXX_COMPILER_ID MATCHES Clang)
            target_link_options(lfortran PRIVATE -static)
        endif()
    endif()
endif()

if (CMAKE_SYSTEM_NAME STREQUAL "Linux"
    OR CMAKE_SYSTEM_NAME STREQUAL "FreeBSD"
    OR CMAKE_SYSTEM_NAME STREQUAL "OpenBSD")

    target_link_options(lfortran PRIVATE "LINKER:--export-dynamic")
endif()

if (WITH_STACKTRACE AND APPLE AND CMAKE_CXX_COMPILER_ID MATCHES Clang)
    # On macOS we have to call dsymutil to create the dSYM bundle so that the
    # stacktrace can find debugging information corresponding to the lfortran
    # binary
    find_program(DSYMUTIL NAMES dsymutil PATHS /usr/bin NO_DEFAULT_PATH)
    if(NOT DSYMUTIL)
        find_program(DSYMUTIL NAMES dsymutil)
    endif()
    message("DSYMUTIL: ${DSYMUTIL}")
    add_custom_command(
        TARGET lfortran
        POST_BUILD
        COMMAND ${DSYMUTIL} lfortran
    )
    if (WITH_DWARFDUMP)
        add_custom_command(
            TARGET lfortran
            POST_BUILD
            COMMAND llvm-dwarfdump --debug-line lfortran.dSYM > lfortran.dSYM/raw.txt
        )
        add_custom_command(
            TARGET lfortran
            POST_BUILD
            COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/../libasr/dwarf_convert.py lfortran.dSYM/raw.txt lfortran.dSYM/lines.txt lfortran.dSYM/lines.dat
        )
    endif()
endif()

# Ensure "Release" is not appended to the path on Windows:
# https://stackoverflow.com/a/56201564/479532
set_target_properties(lfortran PROPERTIES RUNTIME_OUTPUT_DIRECTORY $<0:>)

set_target_properties(lfortran PROPERTIES
    INSTALL_RPATH_USE_LINK_PATH TRUE
)

if (HAVE_BUILD_TO_WASM)
    # "-g0": Store no debugging information in the generated wasm file. This helps reduce generated file size
    # "-Oz": Optimize for size. With this code size ~ 2.4mb. Without this code size ~49mb
    # "-fexceptions": Enable Cpp exception support
    # "--no-entry": No start function to execute
    # "-s ASSERTIONS": Compile with Assertions which (as per docs) are helpful to debug compilation process
    # "-s ALLOW_MEMORY_GROWTH": Allow dynamic memory growth upto the maximum page size limit
    # "-s WASM_BIGINT": Allow use of i64 integers. ASR is needing this option to be enabled.
    # "-s EXPORTED_RUNTIME_METHODS=['cwrap']": Export cwarp. cwarp helps us to call our EMSCRIPTEN_KEEPALIVE functions

    # Notes:
    # STANDALONE_WASM is disabling support for exceptions, so it is currently omitted
    # In build_to_wasm.sh, we need CMAKE_CXX_FLAGS_DEBUG="-Wall -Wextra -fexceptions" flags for exception support
    set(WASM_COMPILE_FLAGS "-g0 -fexceptions")
    set(WASM_LINK_FLAGS
      "-g0 -Oz -fexceptions --preload-file asset_dir -Wall -Wextra --no-entry -s ASSERTIONS -s ALLOW_MEMORY_GROWTH=1 -s WASM_BIGINT -s \"EXPORTED_RUNTIME_METHODS=['cwrap']\""
    )
    set_target_properties(lfortran PROPERTIES COMPILE_FLAGS ${WASM_COMPILE_FLAGS})
    set_target_properties(lfortran PROPERTIES LINK_FLAGS ${WASM_LINK_FLAGS})
endif()

install(TARGETS lfortran
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
)

if (WITH_BENCHMARKS)
    add_executable(parse parse.cpp)
    target_link_libraries(parse lfortran_lib)

    add_executable(parse2 parse2.cpp)
    target_link_libraries(parse2 lfortran_lib)

    if (WITH_FMT)
        add_executable(parse3 parse3.cpp)
        target_link_libraries(parse3 lfortran_lib fmt::fmt)
    endif()
endif()
