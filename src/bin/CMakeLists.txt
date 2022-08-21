add_executable(lfortran lfortran.cpp)
target_include_directories(lfortran PRIVATE "tpl")
target_link_libraries(lfortran lfortran_lib)
if (LFORTRAN_STATIC_BIN)
    if (CMAKE_SYSTEM_NAME STREQUAL "Linux")
        # Link statically on Linux with gcc or clang
        if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU" OR
                CMAKE_CXX_COMPILER_ID MATCHES Clang)
            target_link_options(lfortran PRIVATE -static)
        endif()
    endif()
endif()

if (CMAKE_SYSTEM_NAME STREQUAL "Linux")
    target_link_options(lfortran PRIVATE "LINKER:--export-dynamic")
endif()

if (WITH_STACKTRACE AND APPLE AND CMAKE_CXX_COMPILER_ID MATCHES Clang)
    # On macOS we have to call dsymutil to create the dSYM bundle so that the
    # stacktrace can find debugging information corresponding to the lfortran
    # binary
    add_custom_command(
        TARGET lfortran
        POST_BUILD
        COMMAND dsymutil lfortran
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
            COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/dwarf_convert.py lfortran.dSYM/raw.txt lfortran.dSYM/lines.txt lfortran.dSYM/lines.dat
        )
    endif()
endif()


if (WITH_RUNTIME_LIBRARY)
    macro(LFORTRAN_COMPILE_RUNTIME name dir)
        add_custom_command(
            OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/../runtime/${name}.mod
            COMMAND ${CMAKE_CURRENT_BINARY_DIR}/lfortran
            ARGS --backend=cpp -c ${CMAKE_CURRENT_SOURCE_DIR}/../runtime/${dir}/${name}.f90 -o ${name}.o
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../runtime
            DEPENDS lfortran ${CMAKE_CURRENT_SOURCE_DIR}/../runtime/${dir}/${name}.f90 ${ARGN}
            COMMENT "LFortran Compiling ${dir}/${name}.f90")
    endmacro(LFORTRAN_COMPILE_RUNTIME)

    LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_builtin builtin)
    LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_kind pure)
    LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_iso_fortran_env pure)
    LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_optimization builtin
      ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_iso_fortran_env.mod
    )
    LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_math2 pure
      ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_iso_fortran_env.mod
      ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_math3.mod
      ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_builtin.mod
    )
    LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_math3 pure
      ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_iso_fortran_env.mod
      ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_builtin.mod
    )
    LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_ieee_arithmetic pure
      ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_iso_fortran_env.mod
    )
    LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_iso_c_binding pure
      ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_builtin.mod
    )
    LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_string pure
      ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_builtin.mod
      ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_iso_fortran_env.mod
    )
    LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_trig pure
      ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_iso_fortran_env.mod
    )
    LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_math impure
      ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_iso_c_binding.mod
      ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_builtin.mod
      ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_iso_fortran_env.mod
      ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_math2.mod
    )
    LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_sin impure
      ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_iso_c_binding.mod
      ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_builtin.mod
      ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_iso_fortran_env.mod
      ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_math.mod
    )
    LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_bit impure
      ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_iso_fortran_env.mod
    )

    add_custom_target(lfortran2
        ALL
        DEPENDS
            ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_builtin.mod
            ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_optimization.mod
            ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_kind.mod
            ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_iso_fortran_env.mod
            ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_ieee_arithmetic.mod
            ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_iso_c_binding.mod
            ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_math2.mod
            ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_math3.mod
            ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_math.mod
            ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_trig.mod
            ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_sin.mod
            ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_string.mod
            ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_bit.mod
    )

    install(
        FILES ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_builtin.mod
              ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_optimization.mod
              ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_kind.mod
              ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_iso_fortran_env.mod
              ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_ieee_arithmetic.mod
              ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_iso_c_binding.mod
              ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_math2.mod
              ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_math3.mod
              ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_math.mod
              ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_trig.mod
              ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_sin.mod
              ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_string.mod
              ${CMAKE_CURRENT_BINARY_DIR}/../runtime/lfortran_intrinsic_bit.mod
        DESTINATION share/lfortran/lib
    )
endif()

# Ensure "Release" is not appended to the path on Windows:
# https://stackoverflow.com/a/56201564/479532
set_target_properties(lfortran PROPERTIES RUNTIME_OUTPUT_DIRECTORY $<0:>)

set_target_properties(lfortran PROPERTIES
    INSTALL_RPATH_USE_LINK_PATH TRUE
)

if (HAVE_BUILD_TO_WASM)
    # set(WASM_LINK_FLAGS
    #     "-g0" # Store no debugging information in the generated wasm file. This helps reduce generated file size
    #     "-Oz" # Optimize for size. With this code size ~ 2.4mb. Without this code size ~49mb
    #     "-fexceptions" # Enable Cpp exception support
    #     "--no-entry" # No start function to execute
    #     "-s ASSERTIONS" # Compile with Assertions which (as per docs) are helpful to debug compilation process
    #     "-s ALLOW_MEMORY_GROWTH" # Allow dynamic memory growth upto the maximum page size limit
    #     "-s WASM_BIGINT" # Allow use of i64 integers. ASR is needing this option to be enabled.
    #     "-s EXPORTED_RUNTIME_METHODS=['cwrap']" # Export cwarp. cwarp helps us to call our EMSCRIPTEN_KEEPALIVE functions
    # )

    # Some extra flags below that we may need in future. But these may/might increase the code size
    # "--preload-file ./asset_dir"
    # "-s SAFE_HEAP=1"
    # "-s \"EXPORTED_RUNTIME_METHODS=['ccall']\""
    # "-s EXPORTED_FUNCTIONS=\"['_free', '_malloc']\""

    # Notes:
    # STANDALONE_WASM is disabling support for exceptions, so it is currently omitted
    # In build_to_wasm.sh, we need CMAKE_CXX_FLAGS_DEBUG="-Wall -Wextra -fexceptions" flags for exception support
    set(WASM_COMPILE_FLAGS "-g0 -fexceptions")
    set(WASM_LINK_FLAGS
      "-g0 -Oz -fexceptions --embed-file asset_dir -Wall -Wextra --no-entry -s ASSERTIONS -s ALLOW_MEMORY_GROWTH=1 -s WASM_BIGINT -s \"EXPORTED_RUNTIME_METHODS=['cwrap']\""
    )
    set_target_properties(lfortran PROPERTIES COMPILE_FLAGS ${WASM_COMPILE_FLAGS})
    set_target_properties(lfortran PROPERTIES LINK_FLAGS ${WASM_LINK_FLAGS})
endif()

install(TARGETS lfortran
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    ARCHIVE DESTINATION share/lfortran/lib
    LIBRARY DESTINATION share/lfortran/lib
)


add_executable(parse parse.cpp)
target_link_libraries(parse lfortran_lib)

add_executable(parse2 parse2.cpp)
target_link_libraries(parse2 lfortran_lib)

if (WITH_FMT)
    add_executable(parse3 parse3.cpp)
    target_link_libraries(parse3 lfortran_lib fmt::fmt)
endif()

add_executable(cpptranslate cpptranslate.cpp)
target_link_libraries(cpptranslate lfortran_lib)
