# Find the compiler
find_program(
    CMAKE_Fortran_COMPILER 
        NAMES "lfortran" 
        HINTS "${CMAKE_SOURCE_DIR}"
        DOC "LFortran compiler"
)
mark_as_advanced(CMAKE_Fortran_COMPILER)

set(CMAKE_Fortran_SOURCE_FILE_EXTENSIONS f;F;f90;F90)
set(CMAKE_Fortran_OUTPUT_EXTENSION .o)
set(CMAKE_Fortran_COMPILER_ENV_VAR "LFortran")

# Configure variables set in this file for fast reload later on
configure_file(${CMAKE_CURRENT_LIST_DIR}/CMakeFortranCompiler.cmake.in
               ${CMAKE_PLATFORM_INFO_DIR}/CMakeFortranCompiler.cmake)
