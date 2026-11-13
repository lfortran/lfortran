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
        semantic_highlighter.cpp
        lfortran_lsp_language_server.cpp
        concurrent_lfortran_lsp_language_server.cpp
        parallel_lfortran_lsp_language_server.cpp
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
    cuda_link_helpers.cpp
    ${LFORTRAN_SRC}
)

if (HAVE_LFORTRAN_LLD)
    list(APPEND LFORTRAN_SRC lld_linker.cpp)
endif()

if (NOT EMSCRIPTEN OR LFORTRAN_BUILD_TO_WASM)

add_executable(lfortran ${LFORTRAN_SRC})
# tpl/ contains only vendored third-party code (cpp-terminal, whereami).
target_include_directories(lfortran SYSTEM PRIVATE "tpl")
target_link_libraries(lfortran ${LFORTRAN_LINK_LIBRARIES})
if (HAVE_LFORTRAN_LLD)
    target_include_directories(lfortran PRIVATE ${LLD_INCLUDE_DIRS})
    # The imported LLD targets (lldELF, lldMachO) link the shared libLLVM.
    # This binary already links the static component archives, and both
    # register the same cl::opt options. Link the static LLD archives and
    # every static LLVM archive instead, so LLD's references resolve without
    # loading the shared libLLVM. On Linux they form one group, so the
    # single-pass linker resolves them; one -Wl item keeps CMake from dropping
    # the second copy of an archive. Packages without a CMake config (LLVM
    # 7–9) contribute the ELF archive and the common archives next to it.
    # liblldDriver.a defines main, so it stays out.
    set(_lld_archives "")
    if (TARGET ${LFORTRAN_LLD_TARGET})
        foreach(_lld_target ${LFORTRAN_LLD_TARGET} lldCommon)
            set(_loc "")
            get_target_property(_configs ${_lld_target} IMPORTED_CONFIGURATIONS)
            foreach(_cfg IN LISTS _configs)
                get_target_property(_loc ${_lld_target} IMPORTED_LOCATION_${_cfg})
                if(_loc)
                    break()
                endif()
            endforeach()
            if(NOT _loc)
                message(FATAL_ERROR "LLD target ${_lld_target} has no imported archive")
            endif()
            list(APPEND _lld_archives "${_loc}")
        endforeach()
    else()
        list(APPEND _lld_archives "${LFORTRAN_LLD_ELF}")
        get_filename_component(_lld_dir "${LFORTRAN_LLD_ELF}" DIRECTORY)
        foreach(_extra Common Core YAML ReaderWriter)
            if (EXISTS "${_lld_dir}/liblld${_extra}.a")
                list(APPEND _lld_archives "${_lld_dir}/liblld${_extra}.a")
            endif()
        endforeach()
    endif()
    file(GLOB _llvm_static_libs "${LLVM_LIBRARY_DIR}/libLLVM*.a")
    if(NOT _llvm_static_libs)
        message(FATAL_ERROR
            "No static LLVM archives in ${LLVM_LIBRARY_DIR}. "
            "LFortran links LLVM statically.")
    endif()
    list(SORT _llvm_static_libs)
    if (APPLE)
        # Apple's linker searches every archive on the command line for each
        # undefined symbol, so the archives need no group. LLVM Support's
        # imported target already brings zlib and zstd. LLD 15 and 16 write
        # bitcode bundles with the system libxar.
        target_link_libraries(lfortran ${_lld_archives} ${_llvm_static_libs})
        if (LLVM_HAVE_LIBXAR)
            target_link_libraries(lfortran xar)
        endif()
        # CMake lists the LLVM archives again after libasr, which needs them
        # too, and Apple's linker (Xcode 15 and newer) warns about each
        # repeated archive. Older linkers neither warn nor know the flag.
        include(CheckLinkerFlag OPTIONAL)
        if (COMMAND check_linker_flag)
            check_linker_flag(CXX "-Wl,-no_warn_duplicate_libraries"
                LFORTRAN_LINKER_NO_WARN_DUPLICATE_LIBRARIES)
            if (LFORTRAN_LINKER_NO_WARN_DUPLICATE_LIBRARIES)
                target_link_options(lfortran PRIVATE
                    "-Wl,-no_warn_duplicate_libraries")
            endif()
        endif()
    else()
        set(_lld_group "")
        foreach(_loc IN LISTS _lld_archives)
            if(_lld_group STREQUAL "")
                set(_lld_group "-Wl,--start-group,${_loc}")
            else()
                string(APPEND _lld_group ",${_loc}")
            endif()
        endforeach()
        foreach(_llvm_archive IN LISTS _llvm_static_libs)
            string(APPEND _lld_group ",${_llvm_archive}")
        endforeach()
        # LLD and LLVM Support need these. They are not part of libLLVM.
        foreach(_syslib z zstd pthread dl m rt)
            string(APPEND _lld_group ",-l${_syslib}")
        endforeach()
        string(APPEND _lld_group ",--end-group")
        target_link_libraries(lfortran "${_lld_group}")
    endif()
    if (LFORTRAN_CXX_NO_RTTI_FLAG)
        set_source_files_properties(lld_linker.cpp PROPERTIES
            COMPILE_FLAGS "${LFORTRAN_CXX_NO_RTTI_FLAG}")
    endif()
endif()
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

if (APPLE)
    # At every launch dyld coalesces each exported weak symbol with the loaded
    # dylibs. A Debug build exports ~66,000 weak C++ symbols (inline functions
    # and template instantiations), so each lfortran invocation spent ~150 ms
    # in dyld before main(). The JIT only needs the C symbols (runtime and
    # Jupyter display functions), so do not export C++ symbols.
    target_link_options(lfortran PRIVATE
        "LINKER:-unexported_symbols_list,${CMAKE_CURRENT_SOURCE_DIR}/unexported_symbols.txt")
    set_property(TARGET lfortran APPEND PROPERTY LINK_DEPENDS
        "${CMAKE_CURRENT_SOURCE_DIR}/unexported_symbols.txt")
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
    # "-s STACK_SIZE": the 64kb Emscripten default is far too small for the
    # recursive descent parsers and ASR visitors; use a native-sized stack.
    set(WASM_LINK_FLAGS
      "-g0 -Oz -fexceptions --preload-file asset_dir -Wall -Wextra --no-entry -s ASSERTIONS -s ALLOW_MEMORY_GROWTH=1 -s WASM_BIGINT -s STACK_SIZE=8mb -s \"EXPORTED_RUNTIME_METHODS=['cwrap']\""
    )
    set_target_properties(lfortran PROPERTIES COMPILE_FLAGS ${WASM_COMPILE_FLAGS})
    set_target_properties(lfortran PROPERTIES LINK_FLAGS ${WASM_LINK_FLAGS})
endif()

install(TARGETS lfortran
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
)

endif() # NOT EMSCRIPTEN OR LFORTRAN_BUILD_TO_WASM

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
