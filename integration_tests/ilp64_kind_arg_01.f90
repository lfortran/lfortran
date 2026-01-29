program ilp64_kind_arg_01
    use, intrinsic :: iso_fortran_env, only: real32
    implicit none
    integer, parameter :: WP = real32
    complex(kind=WP) :: c
    real(kind=WP) :: r
    integer(4) :: exit_code
    r = 1.0_WP
    c = cmplx(r, 0.0_WP, kind=WP)
    exit_code = 0
    if (abs(real(c) - 1.0_WP) > 0.001_WP) exit_code = 1
    if (abs(aimag(c)) > 0.001_WP) exit_code = 2
    if (exit_code /= 0) stop exit_code
    print *, "PASS"
end program
