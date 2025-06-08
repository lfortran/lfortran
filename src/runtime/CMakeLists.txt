cmake_minimum_required(VERSION 3.10 FATAL_ERROR)

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

project(lfortran_unif_runtime C Fortran)

set(LFORTRAN_NESTED_BUILD no
    CACHE BOOL "Whether we are being called from the LFortran build system")

if(NOT LFORTRAN_NESTED_BUILD)
    add_subdirectory(legacy)
endif()

# See the TODO in the installation step below
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR})
include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})

macro(lfortran_compile_runtime name dir)
  add_library(${name} OBJECT ${dir}/${name}.f90)
  target_compile_options(${name} PUBLIC "--backend=cpp")
  set_target_properties(${name} PROPERTIES
    POSITION_INDEPENDENT_CODE False
    Fortran_PREPROCESS False)

  if(ARGN)
    add_dependencies(${name} ${ARGN})
  endif()
endmacro(lfortran_compile_runtime)

lfortran_compile_runtime(lfortran_intrinsic_iso_fortran_env pure)

lfortran_compile_runtime(lfortran_intrinsic_custom custom)

lfortran_compile_runtime(lfortran_intrinsic_ieee_arithmetic pure)
add_dependencies(lfortran_intrinsic_ieee_arithmetic lfortran_intrinsic_iso_fortran_env)

lfortran_compile_runtime(lfortran_intrinsic_iso_c_binding pure)

lfortran_compile_runtime(omp_lib openmp)
add_dependencies(omp_lib lfortran_intrinsic_iso_c_binding)

# Ideally this should be a target based install, not file based:
# install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION ${CMAKE_INSTALL_LIBDIR})
# but for historical reasons get_runtime_library_dir() looks under "/../../runtime"
# during tests.
# TODO: Change get_runtime_library_dir() to "/../../modfiles" for tests
install(
  FILES
  ${CMAKE_Fortran_MODULE_DIRECTORY}/lfortran_intrinsic_custom.mod
  ${CMAKE_Fortran_MODULE_DIRECTORY}/lfortran_intrinsic_iso_fortran_env.mod
  ${CMAKE_Fortran_MODULE_DIRECTORY}/lfortran_intrinsic_ieee_arithmetic.mod
  ${CMAKE_Fortran_MODULE_DIRECTORY}/lfortran_intrinsic_iso_c_binding.mod
  ${CMAKE_Fortran_MODULE_DIRECTORY}/omp_lib.mod
  DESTINATION ${CMAKE_INSTALL_LIBDIR}
  )

