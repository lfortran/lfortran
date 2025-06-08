cmake_minimum_required(VERSION 3.5)

project(test_project VERSION 0.1
        DESCRIPTION "hello"
        LANGUAGES Fortran)
add_executable(hello hello.f90)
