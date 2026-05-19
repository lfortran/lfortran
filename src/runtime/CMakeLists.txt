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

set(LFORTRAN_COMPILER_STAMP "${CMAKE_BINARY_DIR}/lfortran_compiler.stamp")
add_custom_command(
  OUTPUT "${LFORTRAN_COMPILER_STAMP}"
  COMMAND "${CMAKE_COMMAND}" -E touch "${LFORTRAN_COMPILER_STAMP}"
  DEPENDS "${CMAKE_Fortran_COMPILER}"
  VERBATIM
  )

macro(lfortran_compile_runtime name dir)
  set_source_files_properties(
    ${dir}/${name}.f90
    PROPERTIES
    OBJECT_DEPENDS "${LFORTRAN_COMPILER_STAMP}"
    )
  add_library(${name} OBJECT ${dir}/${name}.f90)
  if(LIRIC_RUNTIME)
    target_compile_options(${name} PUBLIC "--backend=liric")
  else()
    target_compile_options(${name} PUBLIC "--backend=cpp")
  endif()
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

# When building with --backend=liric the runtime intrinsic modules emit
# real object code with the function bodies that the liric backend
# expects to find at link time (newunit_int_4, c_logb, compute_lps,
# etc.).  Bundle them into a static archive that the lfortran CLI's
# link path picks up alongside liblfortran_runtime.
if(LIRIC_RUNTIME)
  set(LIRIC_FORTRAN_RT_EMPTY "${CMAKE_BINARY_DIR}/liric_runtime_fortran_empty.c")
  file(WRITE "${LIRIC_FORTRAN_RT_EMPTY}" "/* placeholder for object-only archive */\n")
  add_library(lfortran_runtime_fortran STATIC
    "${LIRIC_FORTRAN_RT_EMPTY}"
    $<TARGET_OBJECTS:lfortran_intrinsic_iso_fortran_env>
    $<TARGET_OBJECTS:lfortran_intrinsic_custom>
    $<TARGET_OBJECTS:lfortran_intrinsic_ieee_arithmetic>
    $<TARGET_OBJECTS:lfortran_intrinsic_iso_c_binding>
    $<TARGET_OBJECTS:omp_lib>)
  # lfortran's link path searches build/src/runtime (../runtime relative
  # to the bin/lfortran).  Place the archive there.
  set_target_properties(lfortran_runtime_fortran PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}")
endif()

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
