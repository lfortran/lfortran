cmake_minimum_required(VERSION 3.5 FATAL_ERROR)

project(lfortran_runtime_openmp C Fortran)

add_library(lfortran_runtime_openmp
    STATIC
    omp_lib.f90
)
