cmake_minimum_required(VERSION 3.5 FATAL_ERROR)

project(lfortran_unif_runtime C Fortran)

# if(CMAKE_C_COMPILER_ID MATCHES GNU)
#   set(CMAKE_Fortran_FLAGS "--link-with-gcc -I/home/tuco/miniconda3/envs/lfortran-dev/include -L/home/tuco/miniconda3/envs/lfortran-dev/lib -llibkokkoscore.so")
# endif()

# Normally this would be set to "-nocpp" but lfortran doesn't support this option
# set(CMAKE_Fortran_COMPILE_OPTIONS_PREPROCESS_OFF "")

# If we are only building the runtime library the user
# should provide a lfortran binary.
if(NOT WITH_RUNTIME_LIBRARY_ONLY)
  # set(CMAKE_Fortran_COMPILER_WORKS TRUE)
  set(CMAKE_Fortran_COMPILER ${CMAKE_CURRENT_BINARY_DIR}/../bin/lfortran)
endif()

# lfortran needs this to find the .mod files created during the build
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

macro(lfrotran_compile_runtime name dir)
  add_library(${name} OBJECT ${dir}/${name}.f90)
  target_compile_options(${name} PUBLIC "--backend=cpp")
  set_target_properties(${name} PROPERTIES
    POSITION_INDEPENDENT_CODE False)
    # Fortran_PREPROCESS False)

  # If we are only building the runtime libraries assume
  # the user already built lfortran and don't require
  # it as a dependency.
  if(NOT WITH_RUNTIME_LIBRARY_ONLY)
    add_dependencies(${name} lfortran ${ARGN})
  endif()
  
  if(ARGN)
    add_dependencies(${name} ${ARGN})
  endif()
endmacro(lfrotran_compile_runtime)

macro(lfortran_install_runtime)
  foreach(arg IN ITEMS ${ARGN})
    install(FILES
      ${CMAKE_CURRENT_BINARY_DIR}/${arg}.mod
      DESTINATION share/lfortran/lib)
    endforeach()
endmacro(lfortran_install_runtime)

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

lfortran_install_runtime(
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
  lfortran_intrinsic_bit)

# add_library(lfortran_unif_runtime INTERFACE)
# target_link_libraries(lfortran_unif_runtime INTERFACE ${runtime_components})

# # Install
# install(TARGETS lfortran_unif_runtime)
