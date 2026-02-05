! Test inquire iolength with different type kinds (issue #4640)
program inquire_09
    use iso_fortran_env, only: int8, int16, int32, int64, real32, real64
    implicit none
    integer :: len1, len2, len3, len4, len5, len6

    inquire(iolength=len1) 42_int8
    inquire(iolength=len2) 42_int16
    inquire(iolength=len3) 42_int32
    inquire(iolength=len4) 42_int64
    inquire(iolength=len5) 4.2_real32
    inquire(iolength=len6) 4.2_real64

    if (len1 /= 1) error stop
    if (len2 /= 2) error stop
    if (len3 /= 4) error stop
    if (len4 /= 8) error stop
    if (len5 /= 4) error stop
    if (len6 /= 8) error stop
end program inquire_09
