program real128_unary_minus_01
    ! unary minus on real(16) constants must fold on the binary128 value
    implicit none
    real(16) :: x, y = -0.5_16
    real(16), parameter :: m = -0.25_16
    real(16), parameter :: a(2) = [-1.0_16, -2.0_16]
    x = -0.5_16
    if (x /= 0.0_16 - 0.5_16) error stop 1
    if (y /= 0.0_16 - 0.5_16) error stop 2
    if (m /= 0.0_16 - 0.25_16) error stop 3
    if (-(0.5_16) >= 0.0_16) error stop 4
    if (-1.0_16 * 0.5_16 /= x) error stop 5
    if (-x /= 0.5_16) error stop 6
    if (a(2) /= 0.0_16 - 2.0_16) error stop 7
    if (-huge(x) /= 0.0_16 - huge(x)) error stop 8
    if (-1.0e-300_16 >= 0.0_16) error stop 9
    print *, "ok"
end program
