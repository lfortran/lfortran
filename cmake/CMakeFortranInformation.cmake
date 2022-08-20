include(Compiler/Clang)

# For more info see https://github.com/Kitware/CMake/blob/master/Modules/CMakeAddNewLanguage.txt

# if(NOT CMAKE_Fortran_COMPILE_OBJECT)
# set(CMAKE_Fortran_COMPILE_OBJECT "<CMAKE_Fortran_COMPILER> <DEFINES> <INCLUDES> <FLAGS> -c <SOURCE> -o <OBJECT>")
# else()
#   set(CMAKE_Fortran_COMPILE_OBJECT "<CMAKE_Fortran_COMPILER> <DEFINES> <INCLUDES> <FLAGS> <SOURCE> -o <OBJECT>")
# endif()

set(CMAKE_Fortran_COMPILE_OBJECT "<CMAKE_Fortran_COMPILER> <DEFINES> <INCLUDES> <FLAGS> -c <SOURCE> -o <OBJECT>")
# set(CMAKE_Fortran_CREATE_SHARED_LIBRARY )
# set(CMAKE_Fortran_CREATE_SHARED_MODULE )
# set(CMAKE_Fortran_CREATE_STATIC_LIBRARY )
# set(CMAKE_Fortran_COMPILE_OBJECT )
# set(CMAKE_Fortran_LINK_EXECUTABLE )

set(CMAKE_Fortran_PREPROCESS_SOURCE "<CMAKE_Fortran_COMPILER> <DEFINES> <INCLUDES> <FLAGS> <SOURCE> -o <OBJECT>")
set(CMAKE_Fortran_INFORMATION_LOADED 1)
