program dcmplx_01
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    real(dp) :: x, y
    complex(dp) :: z

    x = 1.5d0
    y = 2.5d0

    ! Test dcmplx with single real argument
    z = dcmplx(x)
    if (abs(real(z, dp) - 1.5d0) > 1d-15) error stop "dcmplx(x) real part failed"
    if (abs(aimag(z)) > 1d-15) error stop "dcmplx(x) imag part failed"

    ! Test dcmplx with two real arguments
    z = dcmplx(x, y)
    if (abs(real(z, dp) - 1.5d0) > 1d-15) error stop "dcmplx(x,y) real part failed"
    if (abs(aimag(z) - 2.5d0) > 1d-15) error stop "dcmplx(x,y) imag part failed"

    ! Test dconjg
    z = (1.5d0, 2.5d0)
    z = dconjg(z)
    if (abs(real(z, dp) - 1.5d0) > 1d-15) error stop "dconjg real part failed"
    if (abs(aimag(z) + 2.5d0) > 1d-15) error stop "dconjg imag part failed"

    print *, "All tests passed"
end program dcmplx_01
