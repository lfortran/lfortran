cmake_minimum_required(VERSION 3.5 FATAL_ERROR)

project(interop C CXX Fortran)

if (NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug
        CACHE STRING "Build type (Debug, Release)" FORCE)
endif ()

enable_testing()

add_library(mod1 mod1.f90)

add_executable(test_mod1 test_mod1.f90)
target_link_libraries(test_mod1 mod1)
add_test(test_mod1 ${CURRENT_BINARY_DIR}/test_mod1)

add_executable(test_cpp test_cpp.cpp)
target_link_libraries(test_cpp mod1)
target_include_directories(test_cpp BEFORE PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/../../src)
add_test(test_cpp ${CURRENT_BINARY_DIR}/test_cpp)

add_executable(test_fortran test_fortran.f90 gfort_interop.f90)
target_link_libraries(test_fortran mod1)
add_test(test_fortran ${CURRENT_BINARY_DIR}/test_fortran)

message("\n")
message("Configuration results")
message("---------------------")
message("Fortran compiler: ${CMAKE_Fortran_COMPILER}")
message("C++ compiler    : ${CMAKE_CXX_COMPILER}")
message("C compiler      : ${CMAKE_C_COMPILER}")
message("Build type: ${CMAKE_BUILD_TYPE}")
string(TOUPPER "${CMAKE_BUILD_TYPE}" BTYPE)
message("Fortran compiler flags: ${CMAKE_Fortran_FLAGS_${BTYPE}}")
message("C++ compiler flags    : ${CMAKE_CXX_FLAGS_${BTYPE}}")
message("C compiler flags      : ${CMAKE_C_FLAGS_${BTYPE}}")
message("Installation prefix: ${CMAKE_INSTALL_PREFIX}")


