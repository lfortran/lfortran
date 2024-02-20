module lfortran_intrinsic_iso_fortran_env
implicit none

integer, parameter :: int8 = 1
integer, parameter :: int16 = 2
integer, parameter :: int32 = 4
integer, parameter :: int64 = 8
integer, parameter :: real32 = 4
integer, parameter :: real64 = 8
integer, parameter :: real128 = -1

integer, parameter :: input_unit = 5
integer, parameter :: output_unit = 6
integer, parameter :: error_unit = 0

integer, parameter :: integer_kinds(2) = [4, 8]
integer, parameter :: real_kinds(2) = [4, 8]
integer, parameter :: character_kinds(1) = [1]
integer, parameter :: logical_kinds(1) = [4]

integer, parameter :: iostat_end = -1

end module
