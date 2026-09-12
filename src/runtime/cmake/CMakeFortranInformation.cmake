include(Compiler/Clang)

# For more info see:
# https://github.com/Kitware/CMake/blob/master/Modules/CMakeAddNewLanguage.txt
# https://stackoverflow.com/questions/38293535/generic-rule-from-makefile-to-cmake

set(CMAKE_Fortran_COMPILE_OBJECT "<CMAKE_Fortran_COMPILER> <DEFINES> -I <INCLUDES> <FLAGS> -c <SOURCE> -o <OBJECT>")
# set(CMAKE_Fortran_CREATE_SHARED_LIBRARY )
# set(CMAKE_Fortran_CREATE_SHARED_MODULE )
# set(CMAKE_Fortran_CREATE_STATIC_LIBRARY )
# set(CMAKE_Fortran_LINK_EXECUTABLE )

# When creating a shared library on macOS, CMake sets the dylib install name
# via this flag. It defaults to a bare `-install_name`, which lfortran's driver
# does not accept. lfortran forwards `-Wl,` flags to the linker, so emit the
# linker-prefixed form instead (as CMake does for e.g. NAG). This lets any CMake
# project (e.g. netcdf-fortran) link a Fortran shared library with lfortran on
# Darwin without special-casing the flag on the command line.
set(CMAKE_SHARED_LIBRARY_SONAME_Fortran_FLAG "-Wl,-install_name,")

set(CMAKE_Fortran_PREPROCESS_SOURCE "<CMAKE_Fortran_COMPILER> --cpp <DEFINES> -I <INCLUDES> <FLAGS> -E <SOURCE> > <PREPROCESSED_SOURCE>")
set(CMAKE_Fortran_INFORMATION_LOADED 1)
