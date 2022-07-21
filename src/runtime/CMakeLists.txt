cmake_minimum_required(VERSION 3.5 FATAL_ERROR)

project(lfortran_unif_runtime C Fortran)

macro(LFORTRAN_COMPILE_RUNTIME name dir)
  add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${name}.mod
    COMMAND ${CMAKE_CURRENT_BINARY_DIR}/../bin/lfortran
    ARGS --backend=cpp -c ${CMAKE_CURRENT_SOURCE_DIR}/${dir}/${name}.f90 -o ${name}.o
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS lfortran ${CMAKE_CURRENT_SOURCE_DIR}/${dir}/${name}.f90 ${ARGN}
    COMMENT "LFortran Compiling ${dir}/${name}.f90")
endmacro(LFORTRAN_COMPILE_RUNTIME)

LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_builtin builtin)
LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_kind pure)
LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_iso_fortran_env pure)
LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_optimization builtin
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_iso_fortran_env.mod
  )
LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_math2 pure
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_iso_fortran_env.mod
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_math3.mod
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_builtin.mod
  )
LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_math3 pure
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_iso_fortran_env.mod
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_builtin.mod
  )
LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_ieee_arithmetic pure
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_iso_fortran_env.mod
  )
LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_iso_c_binding pure
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_builtin.mod
  )
LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_string pure
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_builtin.mod
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_iso_fortran_env.mod
  )
LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_trig pure
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_iso_fortran_env.mod
  )
LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_math impure
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_iso_c_binding.mod
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_builtin.mod
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_iso_fortran_env.mod
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_math2.mod
  )
LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_sin impure
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_iso_c_binding.mod
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_builtin.mod
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_iso_fortran_env.mod
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_math.mod
  )
LFORTRAN_COMPILE_RUNTIME(lfortran_intrinsic_bit impure
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_iso_fortran_env.mod
  )

add_custom_target(lfortran2
  ALL
  DEPENDS
  ${CMAKE_CURRENT_BINARY_DIR}/lfortran_intrinsic_builtin.mod
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
  )

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

#if (WITH_RUNTIME_LIBRARY)
# macro(LFORTRAN_COMPILE_RUNTIME name dir)
#   add_custom_target(${name}.mod
#     ALL
#     DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${name}.mod)

#   add_custom_command(
#     OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${name}.mod
#     COMMAND ${CMAKE_CURRENT_BINARY_DIR}/../bin/lfortran
#     ARGS --backend=cpp -c ${dir}/${name}.f90 -o ${name}.o
    

#     WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
#     DEPENDS lfortran ${CMAKE_CURRENT_SOURCE_DIR}/${dir}/${name}.f90 ${ARGN}
#     COMMENT "LFortran Compiling ${dir}/${name}.f90")

#   install(TARGETS ${name}.mod
#     DESTINATION share/lfortran/lib)
# endmacro(LFORTRAN_COMPILE_RUNTIME)

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

# # endif()

# add_subdirectory(builtin)
# add_subdirectory(pure)
# add_subdirectory(impure)

# set(runtime_components
#     lfortran_runtime_pure
#     lfortran_runtime_impure
#     lfortran_runtime_builtin
#     )

# add_library(lfortran_unif_runtime INTERFACE)
# target_link_libraries(lfortran_unif_runtime INTERFACE ${runtime_components})

# # Install
# install(TARGETS lfortran_unif_runtime)
