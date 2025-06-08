cmake_minimum_required(VERSION 3.10 FATAL_ERROR)

set(CMAKE_USER_MAKE_RULES_OVERRIDE ${CMAKE_SOURCE_DIR}/cmake/UserOverride.cmake)

# We don't execute this if we have a tarball
if (LFORTRAN_BUILD_ALL)
    find_program(RE2C re2c REQUIRED)
    find_program(BISON bison REQUIRED)
    find_program(BASH_BIN bash REQUIRED)
    execute_process(COMMAND "env" "RE2C=${RE2C}" "BISON=${BISON}" "${BASH_BIN}" "-e" "build0.sh" WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} RESULT_VARIABLE _BUILD0_EXIT)
    if (NOT _BUILD0_EXIT EQUAL 0)
        message(FATAL_ERROR "Running build0.sh failed (see error above)")
    endif ()
endif()

file(STRINGS "${CMAKE_CURRENT_SOURCE_DIR}/version" LFORTRAN_VERSION)
string(REGEX MATCH "^[^-]*" LFORTRAN_NO_TAG_VERSION ${LFORTRAN_VERSION})

project(lfortran
        VERSION ${LFORTRAN_NO_TAG_VERSION}
        DESCRIPTION "A modern open-source interactive Fortran compiler"
        HOMEPAGE_URL "https://lfortran.org/"
        LANGUAGES C CXX)

set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)

if (NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release
        CACHE STRING "Build type (Debug, Release)" FORCE)
endif ()

# xeus does use `dynamic_cast` and thus needs rtti (otherwise it segfaults)
if (NOT WITH_XEUS)
    if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
        set(LFORTRAN_CXX_NO_RTTI_FLAG "-fno-rtti")
    elseif (CMAKE_CXX_COMPILER_ID MATCHES Clang)
        set(LFORTRAN_CXX_NO_RTTI_FLAG "-fno-rtti")
    elseif (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
        set(LFORTRAN_CXX_NO_RTTI_FLAG "/GR-")
    endif ()
endif(NOT WITH_XEUS)

if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /bigobj")
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /bigobj")
endif ()

# For CMAKE_INSTALL_BINDIR:
include(GNUInstallDirs)

# create CMAKE_INSTALL_LIBDIR_RELATIVE, LIBDIR relative to BINDIR
file(RELATIVE_PATH CMAKE_INSTALL_LIBDIR_RELATIVE ${CMAKE_INSTALL_FULL_BINDIR} ${CMAKE_INSTALL_FULL_LIBDIR})
# create CMAKE_INSTALL_INCLUDEDIR_RELATIVE, INCLUDEDIR relative to BINDIR
file(RELATIVE_PATH CMAKE_INSTALL_INCLUDEDIR_RELATIVE ${CMAKE_INSTALL_FULL_BINDIR} ${CMAKE_INSTALL_FULL_INCLUDEDIR})

# Build a CPack driven installer package
#
# To build a binary package:
#
# cpack
#
if(NOT WIN32)
    include(InstallRequiredSystemLibraries)
endif()
set(CPACK_GENERATOR "TBZ2")
set(CPACK_STRIP_FILES YES)
set(CPACK_PACKAGE_FILE_NAME lfortran-${LFORTRAN_VERSION}-${CMAKE_SYSTEM_NAME})
include(CPack)
# So that CPACK_PACKAGE_FILE_NAME prints the correct value below
set(CPACK_PACKAGE_FILE_NAME lfortran-${LFORTRAN_VERSION}-${CMAKE_SYSTEM_NAME})


if (NOT CMAKE_CXX_STANDARD)
    set(CMAKE_CXX_STANDARD 17
        CACHE STRING "C++ standard" FORCE)
endif ()

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    # In Debug mode we enable assertions
    set(WITH_LFORTRAN_ASSERT_DEFAULT yes)
else()
    set(WITH_LFORTRAN_ASSERT_DEFAULT no)
endif()


# LFORTRAN_ASSERT
set(WITH_LFORTRAN_ASSERT ${WITH_LFORTRAN_ASSERT_DEFAULT}
    CACHE BOOL "Enable LFORTRAN_ASSERT macro")

# LFORTRAN_STATIC_BIN
set(LFORTRAN_STATIC_BIN no CACHE BOOL "Build LFortran as a static binary")

# WITH_LFORTRAN_BINARY_MODFILES
set(WITH_LFORTRAN_BINARY_MODFILES YES
    CACHE BOOL "Use binary modfiles")

set(WITH_RUNTIME_LIBRARY YES
    CACHE BOOL "Compile and install the runtime library")

set(WITH_WHEREAMI yes
    CACHE BOOL "Include whereami.cpp")

set(WITH_ZLIB yes
    CACHE BOOL "Compile with ZLIB Library")

set(WITH_LCOMPILERS_FAST_ALLOC yes
    CACHE BOOL "Compile with fast allocator")

set(WITH_CCACHE no CACHE BOOL "Build with ccache support")

# Build to wasm
set(LFORTRAN_BUILD_TO_WASM no
    CACHE BOOL "Compile LFortran To WASM")

if (LFORTRAN_BUILD_TO_WASM)
    set(HAVE_BUILD_TO_WASM yes)
    SET(WITH_WHEREAMI no)
    SET(WITH_ZLIB no)
    SET(WITH_RUNTIME_LIBRARY no)
    SET(WITH_LCOMPILERS_FAST_ALLOC no)
    add_definitions("-DHAVE_BUILD_TO_WASM=1")
endif()

if (WITH_WHEREAMI)
    add_definitions("-DHAVE_WHEREAMI=1")
endif()

if (WITH_ZLIB)
    add_definitions("-DHAVE_ZLIB=1")

    # Find ZLIB with our custom finder before including LLVM since the finder for LLVM
    # might search for ZLIB again and find the shared libraries instead of the static ones
    find_package(StaticZLIB REQUIRED)
endif()

if (WITH_LCOMPILERS_FAST_ALLOC)
    add_definitions("-DLCOMPILERS_FAST_ALLOC=1")
endif()


if (WITH_CCACHE)
    find_program(CCACHE_PROGRAM ccache REQUIRED)
    set(CMAKE_C_COMPILER_LAUNCHER ccache)
    set(CMAKE_CXX_COMPILER_LAUNCHER ccache)
endif()

# LLVM
set(WITH_LLVM no CACHE BOOL "Build with LLVM support")
set(WITH_TARGET_AARCH64 no CACHE BOOL "Enable target AARCH64")
set(WITH_TARGET_X86 no CACHE BOOL "Enable target X86")
set(WITH_TARGET_WASM no CACHE BOOL "Enable target WebAssembly")
set(WITH_MLIR no CACHE BOOL "Build with MLIR support")

# Stacktrace
set(WITH_UNWIND no
    CACHE BOOL "Build with unwind support")
set(WITH_BFD no
    CACHE BOOL "Build with BFD support")
set(WITH_DWARFDUMP no
    CACHE BOOL "Build with dwarfdump support")
set(WITH_LINKH no
    CACHE BOOL "Build with link.h support")
set(WITH_MACHO no
    CACHE BOOL "Build with mach-o support")
set(WITH_STACKTRACE no
    CACHE BOOL "Build with stacktrace support (requires binutils-dev)")
set(WITH_LLVM_STACKTRACE no
    CACHE BOOL "Build with LLVM stacktrace support")
if (WITH_STACKTRACE)
    set(WITH_UNWIND yes)
    if (APPLE)
        set(WITH_MACHO yes)
        if (NOT WITH_BFD)
            set(WITH_DWARFDUMP yes)
        endif()
    else()
        set(WITH_LINKH yes)
        if (NOT WITH_BFD)
            set(WITH_LLVM_STACKTRACE yes)
        endif()
    endif()
    set(HAVE_LFORTRAN_STACKTRACE yes)
endif()
if (WITH_RUNTIME_STACKTRACE)
    set(WITH_UNWIND yes)
    if (APPLE)
        set(WITH_MACHO yes)
    else()
        set(WITH_LINKH yes)
    endif()
    set(HAVE_RUNTIME_STACKTRACE yes)
endif()
if (WITH_BFD)
    find_package(BFD REQUIRED)
    set(HAVE_LFORTRAN_BFD yes)
endif()
if (WITH_LLVM_STACKTRACE)
    set(HAVE_LFORTRAN_LLVM_STACKTRACE yes)
endif()
if (WITH_DWARFDUMP)
    set(HAVE_LFORTRAN_DWARFDUMP yes)
endif()
if (WITH_LINKH)
    find_package(LINKH REQUIRED)
    set(HAVE_LFORTRAN_LINK yes)
endif()
if (WITH_MACHO)
    find_package(MACHO REQUIRED)
    set(HAVE_LFORTRAN_MACHO yes)
endif()
if (WITH_UNWIND)
    set(HAVE_LFORTRAN_UNWIND yes)
endif()

if (WITH_LLVM)
    set(WITH_ZSTD yes CACHE BOOL "Detect (and require) static libzstd for llvm")
    if(WITH_ZSTD)
        find_package(StaticZSTD REQUIRED)
    endif()

    set(LFORTRAN_LLVM_COMPONENTS core support mcjit orcjit native asmparser asmprinter)
    if (WITH_LLVM_STACKTRACE)
        list(APPEND LFORTRAN_LLVM_COMPONENTS symbolize object)
    endif()
    find_package(LLVM REQUIRED)
    message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
    message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

    # Always enable the native target
    if ("${LLVM_NATIVE_ARCH}" STREQUAL "AArch64")
        set(WITH_TARGET_AARCH64 yes)
    endif()

    if ("${LLVM_NATIVE_ARCH}" STREQUAL "X86")
        set(WITH_TARGET_X86 yes)
    endif()

    if (WITH_TARGET_AARCH64)
        if (NOT ("${LLVM_TARGETS_TO_BUILD}" MATCHES "AArch64"))
            message(FATAL_ERROR "The selected LLVM library doesn't have support for AArch64 targets")
        endif()

        list(APPEND LFORTRAN_LLVM_COMPONENTS aarch64info aarch64utils aarch64desc aarch64asmparser aarch64codegen aarch64disassembler)
        add_definitions("-DHAVE_TARGET_AARCH64=1")
    endif()

    if (WITH_TARGET_X86)
        if (NOT ("${LLVM_TARGETS_TO_BUILD}" MATCHES "X86"))
            message(FATAL_ERROR "The selected LLVM library doesn't have support for X86 targets")
        endif()

        list(APPEND LFORTRAN_LLVM_COMPONENTS x86info x86desc x86codegen x86asmparser x86disassembler)
        add_definitions("-DHAVE_TARGET_X86=1")
    endif()

    if (WITH_TARGET_WASM)
        if (NOT ("${LLVM_TARGETS_TO_BUILD}" MATCHES "WebAssembly"))
            message(FATAL_ERROR "The selected LLVM library doesn't have support for WebAssembly targets")
        endif()

        list(APPEND LFORTRAN_LLVM_COMPONENTS webassemblyasmparser webassemblycodegen webassemblydesc webassemblydisassembler webassemblyinfo)
        add_definitions("-DHAVE_TARGET_WASM=1")
    endif()

    if (TARGET LLVMCore)
        # If `LLVMCore` target is present, then LLVM is distributed as separate
        # libraries and llvm_map_components_to_libnames() should work:
        llvm_map_components_to_libnames(llvm_libs ${LFORTRAN_LLVM_COMPONENTS})
    else()
        # Workaround for https://github.com/llvm/llvm-project/issues/34593
        # If LLVM is distributed as a single library (the LLVMCore target is
        # missing), we set `llvm_libs` to "LLVM" which links against the single
        # `libLLVM.so` shared library.
        set(llvm_libs "LLVM")
    endif()
    unset(LFORTRAN_LLVM_COMPONENTS)

    if (WITH_MLIR)
        add_library(p::mlir INTERFACE IMPORTED)
        find_package(MLIR REQUIRED CONFIG)
        set(mlir_libs
                MLIRIR
                MLIRLLVMToLLVMIRTranslation
                MLIRBuiltinToLLVMIRTranslation
                MLIRLLVMDialect
                MLIROpenMPToLLVMIRTranslation
                MLIROpenMPDialect
            )
        set_property(TARGET p::mlir PROPERTY INTERFACE_LINK_LIBRARIES ${mlir_libs})
        set(HAVE_LFORTRAN_MLIR yes)
    endif()

    add_library(p::llvm INTERFACE IMPORTED)
    set_property(TARGET p::llvm PROPERTY INTERFACE_INCLUDE_DIRECTORIES
        ${LLVM_INCLUDE_DIRS})
    #set_property(TARGET p::llvm PROPERTY INTERFACE_COMPILE_DEFINITIONS
    #    ${LLVM_DEFINITIONS})
    #set_property(TARGET p::llvm PROPERTY INTERFACE_COMPILE_OPTIONS
    #    ${LLVM_DEFINITIONS})
    set_property(TARGET p::llvm PROPERTY INTERFACE_COMPILE_OPTIONS
        $<$<COMPILE_LANGUAGE:CXX>:${LFORTRAN_CXX_NO_RTTI_FLAG}>)
    set_property(TARGET p::llvm PROPERTY INTERFACE_LINK_LIBRARIES
        ${llvm_libs})
    if (MSVC)
        # LLVM on Windows appends zlib shared library and we must provide
        # a path to find it:
        get_filename_component(mypath ${ZLIB_LIBRARY} DIRECTORY)
        target_link_directories(p::llvm BEFORE INTERFACE ${mypath})
        message(STATUS "ZLIB LIBRARY PATH: ${mypath}")
    endif()
    set(HAVE_LFORTRAN_LLVM yes)
    if(CMAKE_SYSTEM_NAME MATCHES "Linux" AND LLVM_VERSION_MAJOR GREATER_EQUAL 12)
        set(WITH_LIBUNWIND_FORCE yes)
    endif()
endif()

# XEUS (Fortran kernel)
set(WITH_XEUS no CACHE BOOL "Build with XEUS support")
if (WITH_XEUS)
    find_package(xeus 5.1.0 REQUIRED)
    find_package(xeus-zmq 3.0.0 REQUIRED)
    set(HAVE_LFORTRAN_XEUS yes)

    # Generate kernel.json with correct paths
    configure_file (
        "${CMAKE_CURRENT_SOURCE_DIR}/share/jupyter/kernels/fortran/kernel.json.in"
        "${CMAKE_CURRENT_BINARY_DIR}/share/jupyter/kernels/fortran/kernel.json"
    )

    # Generate logo-svg.svg with correct paths
    configure_file (
        "${CMAKE_CURRENT_SOURCE_DIR}/share/jupyter/kernels/fortran/lfortran-logo.svg"
        "${CMAKE_CURRENT_BINARY_DIR}/share/jupyter/kernels/fortran/logo-svg.svg"
    )

    # Configuration and data directories for Jupyter and LFortran
    set(XJUPYTER_DATA_DIR "share/jupyter" CACHE STRING "Jupyter data directory")

    # Install Jupyter kernelspecs
    set(XEUS_CALCSPEC_DIR ${CMAKE_CURRENT_BINARY_DIR}/share/jupyter/kernels)
    install(DIRECTORY ${XEUS_CALCSPEC_DIR}
            DESTINATION ${XJUPYTER_DATA_DIR}
            PATTERN "*.in" EXCLUDE)
endif()

# JSON OR LSP (`conda install rapidjson`)
set(WITH_JSON no CACHE BOOL "Build with JSON support")
set(WITH_LSP no CACHE BOOL "Build with LSP support")
if (WITH_JSON)
    find_package(RapidJSON REQUIRED)
    set(HAVE_LFORTRAN_RAPIDJSON yes)
endif()

set(HAVE_LFORTRAN_DEMANGLE yes
    CACHE BOOL "Build with C++ name demangling support (cxxabi.h)")

if (MSVC)
    # MSVC doesn't have cxxabi.h
    set(HAVE_LFORTRAN_DEMANGLE no)
endif()

# fmt (`conda install fmt`)
set(WITH_FMT no CACHE BOOL "Build with `fmt` support")
if (WITH_FMT)
    find_package(fmt)
endif()

set(WITH_BENCHMARKS no
    CACHE BOOL "Build benchmarks")

set(WITH_LIBUNWIND no
    CACHE BOOL "Build with libunwind (required libunwind-dev)")
if (WITH_LIBUNWIND_FORCE)
    set(WITH_LIBUNWIND yes)
endif()
if (WITH_LIBUNWIND)
    find_package(libunwind REQUIRED)
endif()

set(WITH_KOKKOS no CACHE BOOL "Detect Kokkos location for cpp backend")
if (WITH_KOKKOS)
   find_package(Kokkos)
   if(TARGET Kokkos::kokkoscore)
     get_target_property(KOKKOS_INCLUDEDIR Kokkos::kokkoscore INTERFACE_INCLUDE_DIRECTORIES)
     get_target_property(KOKKOS_LIBRARY Kokkos::kokkoscore LOCATION)
     get_filename_component(KOKKOS_LIBDIR "${KOKKOS_LIBRARY}" DIRECTORY)
   endif()
endif()

enable_testing()

message("\n")
message("Configuration results")
message("---------------------")
message("LFORTRAN_VERSION: ${LFORTRAN_VERSION}")
message("CPACK_PACKAGE_FILE_NAME: ${CPACK_PACKAGE_FILE_NAME}")
message("C compiler      : ${CMAKE_C_COMPILER}")
message("C++ compiler    : ${CMAKE_CXX_COMPILER}")
message("Build type: ${CMAKE_BUILD_TYPE}")
string(TOUPPER "${CMAKE_BUILD_TYPE}" BTYPE)
message("C compiler flags      : ${CMAKE_C_FLAGS_${BTYPE}}")
message("C++ compiler flags    : ${CMAKE_CXX_FLAGS_${BTYPE}}")
message("Installation prefix: ${CMAKE_INSTALL_PREFIX}")
message("WITH_LFORTRAN_ASSERT: ${WITH_LFORTRAN_ASSERT}")
message("LFORTRAN_STATIC_BIN: ${LFORTRAN_STATIC_BIN}")
message("LFORTRAN_BUILD_TO_WASM: ${LFORTRAN_BUILD_TO_WASM}")
message("WITH_STACKTRACE: ${WITH_STACKTRACE}")
message("WITH_RUNTIME_STACKTRACE: ${WITH_RUNTIME_STACKTRACE}")
message("WITH_UNWIND: ${WITH_UNWIND}")
message("WITH_LIBUNWIND: ${WITH_LIBUNWIND}")
message("WITH_BFD: ${WITH_BFD}")
message("WITH_LLVM_STACKTRACE: ${WITH_LLVM_STACKTRACE}")
message("WITH_DWARFDUMP: ${WITH_DWARFDUMP}")
message("WITH_LINKH: ${WITH_LINKH}")
message("WITH_MACHO: ${WITH_MACHO}")
message("HAVE_LFORTRAN_DEMANGLE: ${HAVE_LFORTRAN_DEMANGLE}")
message("WITH_LLVM: ${WITH_LLVM}")
message("WITH_MLIR: ${WITH_MLIR}")
message("WITH_XEUS: ${WITH_XEUS}")
message("WITH_JSON: ${WITH_JSON}")
message("WITH_LSP: ${WITH_LSP}")
message("WITH_FMT: ${WITH_FMT}")
message("WITH_BENCHMARKS: ${WITH_BENCHMARKS}")
message("WITH_LFORTRAN_BINARY_MODFILES: ${WITH_LFORTRAN_BINARY_MODFILES}")
message("WITH_RUNTIME_LIBRARY: ${WITH_RUNTIME_LIBRARY}")
message("WITH_WHEREAMI: ${WITH_WHEREAMI}")
message("WITH_ZLIB: ${WITH_ZLIB}")
message("WITH_TARGET_AARCH64: ${WITH_TARGET_AARCH64}")
message("WITH_TARGET_X86: ${WITH_TARGET_X86}")
message("WITH_TARGET_WASM: ${WITH_TARGET_WASM}")
message("WITH_KOKKOS: ${WITH_KOKKOS}")
message("WITH_CCACHE: ${WITH_CCACHE}")
message("CXXFLAGS: ${CMAKE_CXX_FLAGS}")
message("CFLAGS: ${CMAKE_C_FLAGS}")

add_subdirectory(src)
add_subdirectory(doc/man)

if(LFORTRAN_BUILD_TO_WASM)
  set(WITH_RUNTIME_LIBRARY No)
endif()

if (WITH_RUNTIME_LIBRARY)
  if(WIN32)
    set(LFORTRAN_PATH "${CMAKE_BINARY_DIR}/src/bin/lfortran.exe")
  else()
    set(LFORTRAN_PATH "${CMAKE_BINARY_DIR}/src/bin/lfortran")
  endif()


  add_custom_target(configure_runtime
    ALL
    COMMAND "${CMAKE_COMMAND}"
    "-G" "${CMAKE_GENERATOR}"
    "-DCMAKE_TOOLCHAIN_FILE=${CMAKE_TOOLCHAIN_FILE}"
    "-DCMAKE_INSTALL_PREFIX=${CMAKE_INSTALL_PREFIX}"
    "-DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}"
    "-DCMAKE_PREFIX_PATH=\"${CMAKE_PREFIX_PATH}\""
    "-DCMAKE_INSTALL_LIBDIR=${CMAKE_INSTALL_LIBDIR}"
#    "-DCMAKE_VERBOSE_MAKEFILE:BOOL=${CMAKE_VERBOSE_MAKEFILE}"
    "-DCMAKE_Fortran_COMPILER=${LFORTRAN_PATH}"
    "-DLFORTRAN_NESTED_BUILD=yes"
    "${CMAKE_SOURCE_DIR}/src/runtime/"
    WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/src/runtime")

  add_dependencies(configure_runtime lfortran)

  add_custom_target(build_runtime
    ALL
    COMMAND "${CMAKE_COMMAND}" --build  "${CMAKE_BINARY_DIR}/src/runtime" -j1
    WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/src/runtime")

  add_dependencies(build_runtime configure_runtime)

  # This is called after lfortran has been installed
  # For more info https://stackoverflow.com/a/29979349/16568788
  add_subdirectory(cmake/postinstall)

  install(CODE "set(CMAKE_COMMAND \"${CMAKE_COMMAND}\")")
#  install(CODE "set(CMAKE_VERBOSE_MAKEFILE \"${CMAKE_VERBOSE_MAKEFILE}\")")
  install(CODE "set(CMAKE_SOURCE_DIR \"${CMAKE_SOURCE_DIR}\")")
  install(CODE "set(CMAKE_BINARY_DIR \"${CMAKE_BINARY_DIR}\")")
endif()
