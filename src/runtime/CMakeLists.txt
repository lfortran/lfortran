cmake_minimum_required(VERSION 3.5 FATAL_ERROR)

project(lfortran_unif_runtime C Fortran)

# lfortran needs this to find the .mod files created during the build
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modfiles)

macro(lfrotran_compile_runtime name dir)
  add_library(${name} OBJECT ${dir}/${name}.f90)
  target_compile_options(${name} PUBLIC "--backend=cpp")
  set_target_properties(${name} PROPERTIES
    POSITION_INDEPENDENT_CODE False
    Fortran_PREPROCESS False)

  # if(ARGN)
  #   add_dependencies(${name} ${ARGN})
  # endif()
endmacro(lfrotran_compile_runtime)

# macro(lfortran_install_runtime)
#   foreach(arg IN ITEMS ${ARGN})p
#     install(FILES
#       ${CMAKE_CURRENT_BINARY_DIR}/${arg}.mod
#       DESTINATION share/lfortran/lib)
#     endforeach()
# endmacro(lfortran_install_runtime)

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

install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION share/lfortran/lib)

# lfortran_install_runtime(
#   lfortran_intrinsic_builtin
#   lfortran_intrinsic_optimization
#   lfortran_intrinsic_kind
#   lfortran_intrinsic_iso_fortran_env
#   lfortran_intrinsic_ieee_arithmetic
#   lfortran_intrinsic_iso_c_binding
#   lfortran_intrinsic_math2
#   lfortran_intrinsic_math3
#   lfortran_intrinsic_math
#   lfortran_intrinsic_trig
#   lfortran_intrinsic_sin
#   lfortran_intrinsic_string
#   lfortran_intrinsic_bit)

