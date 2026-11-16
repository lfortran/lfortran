program real16_literal_exponent_01
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(16) :: p, x, ten, tol
    real(real128) :: y
    integer :: i

    ten = 10.0_16
    tol = 1.0e-32_16

    ! 10**k is exactly representable in binary128 for k <= 48.
    p = 1.0_16
    do i = 1, 48
        p = p * ten
        if (i == 38) then
            if (p /= 1.0e38_16) error stop "1e38"
        end if
        if (i == 39) then
            if (p /= 1.0e39_16) error stop "1e39"
        end if
        if (i == 43) then
            if (p /= 1.0e43_16) error stop "1e43"
            if (p /= 1.0e43_real128) error stop "1e43 real128"
            if (p /= 1.0e+43_16) error stop "1e+43"
            if (-p /= -1.0e43_16) error stop "-1e43"
        end if
        if (i == 45) then
            if (p /= 1.0e45_16) error stop "1e45"
        end if
        if (i == 48) then
            if (p /= 1.0e48_16) error stop "1e48"
        end if
    end do

    x = 1.0e50_16
    if (abs(x * x - 1.0e100_16) > tol * 1.0e100_16) error stop "1e100"
    x = 1.0e150_16
    if (abs(x * x - 1.0e300_16) > tol * 1.0e300_16) error stop "1e300"
    x = 1.0e2000_16
    if (abs(x * x - 1.0e4000_16) > tol * 1.0e4000_16) error stop "1e4000"

    x = 1.0e-43_16
    if (abs(x * 1.0e43_16 - 1.0_16) > tol) error stop "1e-43"
    x = 1.0e-300_16
    if (abs(x * 1.0e300_16 - 1.0_16) > tol) error stop "1e-300"
    x = 1.0e-2000_16
    if (abs(x * 1.0e2000_16 - 1.0_16) > tol) error stop "1e-2000"

    y = 1.5e43_real128
    if (abs(y - 1.5_16 * 1.0e43_16) > tol * 1.5e43_16) error stop "1.5e43"
    y = 1.0_16 / 3.0e45_16
    if (abs(y * 3.0e45_16 - 1.0_16) > tol) error stop "3e45"
end program
