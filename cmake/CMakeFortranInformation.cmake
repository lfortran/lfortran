# This file sets the basic flags for the Fortran compiler
if(NOT CMAKE_Fortran_COMPILE_OBJECT)
    set(CMAKE_Fortran_COMPILE_OBJECT "<CMAKE_Fortran_COMPILER> <INCLUDES> <FLAGS> <SOURCE> -o <OBJECT>")
endif()
set(CMAKE_Fortran_INFORMATION_LOADED 1)
