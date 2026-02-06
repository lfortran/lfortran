! Test for https://github.com/lfortran/lfortran/issues/4640
! Inquire iolength with different type kinds
! Exact MRE from issue body
program inquire_09
    use iso_fortran_env, only: int8, int16, int32, int64, real32, real64
    implicit none
    integer :: i, len(6) = -huge(1)
    inquire(iolength=len(1)) 42_int8
    inquire(iolength=len(2)) 42_int16
    inquire(iolength=len(3)) 42_int32
    inquire(iolength=len(4)) 42_int64
    inquire(iolength=len(5)) 4.2_real32
    inquire(iolength=len(6)) 4.2_real64
    print "(6(I0,1X))", len
    if (len(1) /= 1) error stop
    if (len(2) /= 2) error stop
    if (len(3) /= 4) error stop
    if (len(4) /= 8) error stop
    if (len(5) /= 4) error stop
    if (len(6) /= 8) error stop
end program inquire_09
