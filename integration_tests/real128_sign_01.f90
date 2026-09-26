program real128_sign_01
    ! SIGN with real(16) arguments
    implicit none
    real(16) :: x, y, m
    real(16), parameter :: p = sign(2.0_16, 0.0_16 - 1.0_16)
    x = 2.75_16
    y = 0.0_16 - 0.5_16
    if (sign(x, y) /= 0.0_16 - 2.75_16) error stop 1
    if (sign(y, x) /= 0.5_16) error stop 2
    if (sign(x, x) /= x) error stop 3
    if (sign(1.0_16, y) /= 0.0_16 - 1.0_16) error stop 4
    if (p /= 0.0_16 - 2.0_16) error stop 5
    m = sign(1.0_16, x)
    if (m /= 1.0_16) error stop 6
    print *, "ok"
end program
