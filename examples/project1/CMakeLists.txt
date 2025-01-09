cmake_minimum_required(VERSION 3.5 FATAL_ERROR)

project(project1 C Fortran)

string(REPLACE "--generate-object-code" "" CMAKE_Fortran_COMPILE_OBJECT "${CMAKE_Fortran_COMPILE_OBJECT}")

add_executable(project1
    project1.f90
    a.f90
    b.f90
)
