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

macro(lfrotran_compile_runtime name dir)
  add_library(${name} OBJECT ${dir}/${name}.f90)
  target_compile_options(${name} PUBLIC "--backend=cpp")
  set_target_properties(${name} PROPERTIES
    POSITION_INDEPENDENT_CODE False
    Fortran_PREPROCESS False)

  if(ARGN)
    add_dependencies(${name} ${ARGN})
  endif()
endmacro(lfrotran_compile_runtime)

lfrotran_compile_runtime(lfortran_intrinsic_builtin builtin)
lfrotran_compile_runtime(lfortran_intrinsic_kind pure)
lfrotran_compile_runtime(lfortran_intrinsic_iso_fortran_env pure)

lfrotran_compile_runtime(lfortran_intrinsic_optimization builtin)
add_dependencies(lfortran_intrinsic_optimization lfortran_intrinsic_iso_fortran_env)

lfrotran_compile_runtime(lfortran_intrinsic_math2 pure)
add_dependencies(lfortran_intrinsic_math2 lfortran_intrinsic_iso_fortran_env)
add_dependencies(lfortran_intrinsic_math2 lfortran_intrinsic_math3)
add_dependencies(lfortran_intrinsic_math2 lfortran_intrinsic_builtin)

lfrotran_compile_runtime(lfortran_intrinsic_math3 pure)
add_dependencies(lfortran_intrinsic_math3 lfortran_intrinsic_iso_fortran_env)
add_dependencies(lfortran_intrinsic_math3 lfortran_intrinsic_builtin)

lfrotran_compile_runtime(lfortran_intrinsic_ieee_arithmetic pure)
add_dependencies(lfortran_intrinsic_ieee_arithmetic lfortran_intrinsic_iso_fortran_env)

lfrotran_compile_runtime(lfortran_intrinsic_iso_c_binding pure)
add_dependencies(lfortran_intrinsic_iso_c_binding lfortran_intrinsic_builtin)

lfrotran_compile_runtime(lfortran_intrinsic_string pure)
add_dependencies(lfortran_intrinsic_string lfortran_intrinsic_builtin)
add_dependencies(lfortran_intrinsic_string lfortran_intrinsic_iso_fortran_env)

lfrotran_compile_runtime(lfortran_intrinsic_trig pure)
add_dependencies(lfortran_intrinsic_trig lfortran_intrinsic_iso_fortran_env)

lfrotran_compile_runtime(lfortran_intrinsic_math impure)
add_dependencies(lfortran_intrinsic_math lfortran_intrinsic_iso_c_binding)
add_dependencies(lfortran_intrinsic_math lfortran_intrinsic_builtin)
add_dependencies(lfortran_intrinsic_math lfortran_intrinsic_iso_fortran_env)
add_dependencies(lfortran_intrinsic_math lfortran_intrinsic_math2)

lfrotran_compile_runtime(lfortran_intrinsic_sin impure)
add_dependencies(lfortran_intrinsic_sin lfortran_intrinsic_iso_c_binding)
add_dependencies(lfortran_intrinsic_sin lfortran_intrinsic_builtin)
add_dependencies(lfortran_intrinsic_sin lfortran_intrinsic_iso_fortran_env)
add_dependencies(lfortran_intrinsic_sin lfortran_intrinsic_math)

lfrotran_compile_runtime(lfortran_intrinsic_bit impure)
add_dependencies(lfortran_intrinsic_bit lfortran_intrinsic_iso_fortran_env)

# Ideally this should be a target based install, not file based:
# install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION share/lfortran/lib)
# but for historical reasons get_runtime_library_dir() looks under "/../../runtime"
# during tests.
# TODO: Change get_runtime_library_dir() to "/../../modfiles" for tests
install(
  FILES
  ${CMAKE_Fortran_MODULE_DIRECTORY}/lfortran_intrinsic_builtin.mod
  ${CMAKE_Fortran_MODULE_DIRECTORY}/lfortran_intrinsic_optimization.mod
  ${CMAKE_Fortran_MODULE_DIRECTORY}/lfortran_intrinsic_kind.mod
  ${CMAKE_Fortran_MODULE_DIRECTORY}/lfortran_intrinsic_iso_fortran_env.mod
  ${CMAKE_Fortran_MODULE_DIRECTORY}/lfortran_intrinsic_ieee_arithmetic.mod
  ${CMAKE_Fortran_MODULE_DIRECTORY}/lfortran_intrinsic_iso_c_binding.mod
  ${CMAKE_Fortran_MODULE_DIRECTORY}/lfortran_intrinsic_math2.mod
  ${CMAKE_Fortran_MODULE_DIRECTORY}/lfortran_intrinsic_math3.mod
  ${CMAKE_Fortran_MODULE_DIRECTORY}/lfortran_intrinsic_math.mod
  ${CMAKE_Fortran_MODULE_DIRECTORY}/lfortran_intrinsic_trig.mod
  ${CMAKE_Fortran_MODULE_DIRECTORY}/lfortran_intrinsic_sin.mod
  ${CMAKE_Fortran_MODULE_DIRECTORY}/lfortran_intrinsic_string.mod
  ${CMAKE_Fortran_MODULE_DIRECTORY}/lfortran_intrinsic_bit.mod
  DESTINATION share/lfortran/lib
  )

