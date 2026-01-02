program dcmplx_01
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    real(dp) :: x, y
    complex(dp) :: z

    ! Use values that require double precision for accurate representation
    ! These values would lose precision if converted to single precision
    x = 1.123456789012345d0
    y = 2.987654321098765d0

    ! Test dcmplx with single real argument
    z = dcmplx(x)
    if (abs(real(z, dp) - 1.123456789012345d0) > 1d-15) error stop
    if (abs(aimag(z)) > 1d-15) error stop

    ! Test dcmplx with two real arguments
    z = dcmplx(x, y)
    if (abs(real(z, dp) - 1.123456789012345d0) > 1d-15) error stop
    if (abs(aimag(z) - 2.987654321098765d0) > 1d-15) error stop

    ! Test dconjg
    z = (1.123456789012345d0, 2.987654321098765d0)
    z = dconjg(z)
    if (abs(real(z, dp) - 1.123456789012345d0) > 1d-15) error stop
    if (abs(aimag(z) + 2.987654321098765d0) > 1d-15) error stop

end program dcmplx_01
