cmake_minimum_required(VERSION 3.10 FATAL_ERROR)

set(CMAKE_USER_MAKE_RULES_OVERRIDE ${CMAKE_SOURCE_DIR}/cmake/UserOverride.cmake)

file(STRINGS "version" LFORTRAN_VERSION)

project(lfortran LANGUAGES C CXX)

set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)

if (NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release
        CACHE STRING "Build type (Debug, Release)" FORCE)
endif ()
if (NOT (CMAKE_BUILD_TYPE STREQUAL "Debug" OR
        CMAKE_BUILD_TYPE STREQUAL "Release"))
    message("${CMAKE_BUILD_TYPE}")
    message(FATAL_ERROR "CMAKE_BUILD_TYPE must be one of: Debug, Release (current value: '${CMAKE_BUILD_TYPE}')")
endif ()

if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    set(LFORTRAN_CXX_NO_RTTI_FLAG "-fno-rtti")
elseif (CMAKE_CXX_COMPILER_ID MATCHES Clang)
    set(LFORTRAN_CXX_NO_RTTI_FLAG "-fno-rtti")
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    set(LFORTRAN_CXX_NO_RTTI_FLAG "/GR-")
endif ()


# For CMAKE_INSTALL_BINDIR:
include(GNUInstallDirs)


# Build a CPack driven installer package
#
# To build a binary package:
#
# cpack
#
include(InstallRequiredSystemLibraries)
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

#ZLIB
find_package(ZLIB REQUIRED)

# LLVM
set(WITH_LLVM no CACHE BOOL "Build with LLVM support")
if (WITH_LLVM)
    set(LFORTRAN_LLVM_COMPONENTS core support mcjit orcjit native asmparser asmprinter)
    find_package(LLVM REQUIRED)
    message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
    message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
    llvm_map_components_to_libnames(llvm_libs ${LFORTRAN_LLVM_COMPONENTS})
    unset(LFORTRAN_LLVM_COMPONENTS)

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
    set(HAVE_LFORTRAN_LLVM yes)
endif()

# XEUS (Fortran kernel)
set(WITH_XEUS no CACHE BOOL "Build with XEUS support")
if (WITH_XEUS)
    find_package(xeus 0.24.1 REQUIRED)
    set(HAVE_LFORTRAN_XEUS yes)

    # Generate kernel.json with correct paths
    configure_file (
        "${CMAKE_CURRENT_SOURCE_DIR}/share/jupyter/kernels/fortran/kernel.json.in"
        "${CMAKE_CURRENT_BINARY_DIR}/share/jupyter/kernels/fortran/kernel.json"
    )

    # Configuration and data directories for Jupyter and LFortran
    set(XJUPYTER_DATA_DIR "share/jupyter" CACHE STRING "Jupyter data directory")

    # Install Jupyter kernelspecs
    set(XEUS_CALCSPEC_DIR ${CMAKE_CURRENT_BINARY_DIR}/share/jupyter/kernels)
    install(DIRECTORY ${XEUS_CALCSPEC_DIR}
            DESTINATION ${XJUPYTER_DATA_DIR}
            PATTERN "*.in" EXCLUDE)
endif()

# JSON
set(WITH_JSON no CACHE BOOL "Build with JSON support")
if (WITH_JSON)
    find_package(RapidJSON REQUIRED)
endif()

set(HAVE_LFORTRAN_DEMANGLE yes
    CACHE BOOL "Build with C++ name demangling support (cxxabi.h)")

if (MSVC)
    # MSVC doesn't have cxxabi.h
    set(HAVE_LFORTRAN_DEMANGLE no)
endif()

# fmt
set(WITH_FMT no CACHE BOOL "Build with `fmt` support")
if (WITH_FMT)
    find_package(fmt)
endif()

# Stacktrace
set(WITH_UNWIND no
    CACHE BOOL "Build with unwind support")
set(WITH_BFD no
    CACHE BOOL "Build with BFD support")
set(WITH_LINKH no
    CACHE BOOL "Build with link.h support")
set(WITH_MACHO no
    CACHE BOOL "Build with mach-o support")
set(WITH_STACKTRACE no
    CACHE BOOL "Build with stacktrace support (requires binutils-dev)")
if (WITH_STACKTRACE)
    set(WITH_UNWIND yes)
    set(WITH_BFD yes)
    if (APPLE)
        set(WITH_MACHO yes)
    else()
        set(WITH_LINKH yes)
    endif()
    set(HAVE_LFORTRAN_STACKTRACE yes)
endif()
if (WITH_BFD)
    find_package(BFD REQUIRED)
    set(HAVE_LFORTRAN_BFD yes)
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

enable_testing()

message("\n")
message("Configuration results")
message("---------------------")
message("LFORTRAN_VERSION: ${LFORTRAN_VERSION}")
message("CPACK_PACKAGE_FILE_NAME: ${CPACK_PACKAGE_FILE_NAME}")
message("C compiler      : ${CMAKE_C_COMPILER}")
message("C++ compiler    : ${CMAKE_CXX_COMPILER}")
message("Build type: ${CMAKE_BUILD_TYPE}")
if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    message("C compiler flags      : ${CMAKE_C_FLAGS_DEBUG}")
    message("C++ compiler flags    : ${CMAKE_CXX_FLAGS_DEBUG}")
else ()
    message("C compiler flags      : ${CMAKE_C_FLAGS_RELEASE}")
    message("C++ compiler flags    : ${CMAKE_CXX_FLAGS_RELEASE}")
endif ()
message("Installation prefix: ${CMAKE_INSTALL_PREFIX}")
message("WITH_LFORTRAN_ASSERT: ${WITH_LFORTRAN_ASSERT}")
message("LFORTRAN_STATIC_BIN: ${LFORTRAN_STATIC_BIN}")
message("WITH_STACKTRACE: ${WITH_STACKTRACE}")
message("WITH_UNWIND: ${WITH_UNWIND}")
message("WITH_BFD: ${WITH_BFD}")
message("WITH_LINKH: ${WITH_LINKH}")
message("WITH_MACHO: ${WITH_MACHO}")
message("HAVE_LFORTRAN_DEMANGLE: ${HAVE_LFORTRAN_DEMANGLE}")
message("WITH_LLVM: ${WITH_LLVM}")
message("WITH_XEUS: ${WITH_XEUS}")
message("WITH_JSON: ${WITH_JSON}")
message("WITH_FMT: ${WITH_FMT}")


add_subdirectory(src)
