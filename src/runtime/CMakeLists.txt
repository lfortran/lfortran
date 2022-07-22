cmake_minimum_required(VERSION 3.5 FATAL_ERROR)

project(lfortran_unif_runtime C Fortran)

set(CMAKE_Fortran_COMPILER ${CMAKE_CURRENT_BINARY_DIR}/../bin/lfortran)

macro(lfrotran_compile_runtime name dir)
  add_library(${name} OBJECT ${CMAKE_CURRENT_SOURCE_DIR}/${dir}/${name}.f90)
  target_compile_options(${name} PUBLIC "--backend=cpp")
  set_target_properties(${name} PROPERTIES
    POSITION_INDEPENDENT_CODE False)
  add_dependencies(${name} lfortran ${ARGN})
endmacro(lfrotran_compile_runtime)

lfrotran_compile_runtime(lfortran_intrinsic_builtin builtin)
lfrotran_compile_runtime(lfortran_intrinsic_kind pure)
lfrotran_compile_runtime(lfortran_intrinsic_iso_fortran_env pure)
lfrotran_compile_runtime(lfortran_intrinsic_optimization builtin
  lfortran_intrinsic_iso_fortran_env
  )
lfrotran_compile_runtime(lfortran_intrinsic_math2 pure
  lfortran_intrinsic_iso_fortran_env
  lfortran_intrinsic_math3
  lfortran_intrinsic_builtin
  )
lfrotran_compile_runtime(lfortran_intrinsic_math3 pure
  lfortran_intrinsic_iso_fortran_env
  lfortran_intrinsic_builtin
  )
lfrotran_compile_runtime(lfortran_intrinsic_ieee_arithmetic pure
  lfortran_intrinsic_iso_fortran_env
  )
lfrotran_compile_runtime(lfortran_intrinsic_iso_c_binding pure
  lfortran_intrinsic_builtin
  )
lfrotran_compile_runtime(lfortran_intrinsic_string pure
  lfortran_intrinsic_builtin
  lfortran_intrinsic_iso_fortran_env
  )
lfrotran_compile_runtime(lfortran_intrinsic_trig pure
  lfortran_intrinsic_iso_fortran_env
  )
lfrotran_compile_runtime(lfortran_intrinsic_math impure
  lfortran_intrinsic_iso_c_binding
  lfortran_intrinsic_builtin
  lfortran_intrinsic_iso_fortran_env
  lfortran_intrinsic_math2
  )
lfrotran_compile_runtime(lfortran_intrinsic_sin impure
  lfortran_intrinsic_iso_c_binding
  lfortran_intrinsic_builtin
  lfortran_intrinsic_iso_fortran_env
  lfortran_intrinsic_math
  )
lfrotran_compile_runtime(lfortran_intrinsic_bit impure
  lfortran_intrinsic_iso_fortran_env
  )

install(TARGETS
  lfortran_intrinsic_builtin
  lfortran_intrinsic_optimization
  lfortran_intrinsic_kind
  lfortran_intrinsic_iso_fortran_env
  lfortran_intrinsic_ieee_arithmetic
  lfortran_intrinsic_iso_c_binding
  lfortran_intrinsic_math2
  lfortran_intrinsic_math3
  lfortran_intrinsic_math
  lfortran_intrinsic_trig
  lfortran_intrinsic_sin
  lfortran_intrinsic_string
  lfortran_intrinsic_bit
  DESTINATION share/lfortran/lib
  )


# LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_builtin builtin)
# LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_kind pure)
# LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_iso_fortran_env pure)
# LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_optimization builtin
#   ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_iso_fortran_env.mod
#   )
# LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_math2 pure
#   ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_iso_fortran_env.mod
#   ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_math3.mod
#   ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_builtin.mod
#   )
# LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_math3 pure
#   ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_iso_fortran_env.mod
#   ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_builtin.mod
#   )
# LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_ieee_arithmetic pure
#   ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_iso_fortran_env.mod
#   )
# LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_iso_c_binding pure
#   ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_builtin.mod
#   )
# LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_string pure
#   ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_builtin.mod
#   ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_iso_fortran_env.mod
#   )
# LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_trig pure
#   ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_iso_fortran_env.mod
#   )
# LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_math impure
#   ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_iso_c_binding.mod
#   ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_builtin.mod
#   ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_iso_fortran_env.mod
#   ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_math2.mod
#   )
# LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_sin impure
#   ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_iso_c_binding.mod
#   ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_builtin.mod
#   ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_iso_fortran_env.mod
#   ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_math.mod
#   )
# LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_bit impure
#   ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_iso_fortran_env.mod
#   )

install(
  FILES ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_builtin.mod
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_optimization.mod
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_kind.mod
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_iso_fortran_env.mod
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_ieee_arithmetic.mod
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_iso_c_binding.mod
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_math2.mod
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_math3.mod
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_math.mod
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_trig.mod
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_sin.mod
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_string.mod
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_bit.mod
  DESTINATION share/lfortran/lib
  )

# add_library(lfortran_unif_runtime INTERFACE)
# target_link_libraries(lfortran_unif_runtime INTERFACE ${runtime_components})

# # Install
# install(TARGETS lfortran_unif_runtime)
