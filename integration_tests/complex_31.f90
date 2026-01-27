program complex_31
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    implicit none

    complex(dp) :: a, b, c
    real(dp) :: tol

    tol = 1.0d-12

    a = cmplx(1.0d200, 1.0d200, kind=dp)
    b = cmplx(1.0d200, 1.0d200, kind=dp)
    c = a / b
    if (ieee_is_nan(real(c)) .or. ieee_is_nan(aimag(c))) &
        error stop "nan in div case 1"
    if (abs(real(c) - 1.0d0) > tol) error stop "div case 1 real"
    if (abs(aimag(c)) > tol) error stop "div case 1 imag"

    a = cmplx(1.0d-200, 1.0d200, kind=dp)
    b = cmplx(1.0d-200, 1.0d200, kind=dp)
    c = a / b
    if (ieee_is_nan(real(c)) .or. ieee_is_nan(aimag(c))) &
        error stop "nan in div case 2"
    if (abs(real(c) - 1.0d0) > tol) error stop "div case 2 real"
    if (abs(aimag(c)) > tol) error stop "div case 2 imag"

    print *, "PASS"
end program
