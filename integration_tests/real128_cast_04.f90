program real128_cast_04
    ! A negated real128 literal is folded at compile time and must keep its
    ! sign and value, including when it is converted to another kind.
    use, intrinsic :: iso_fortran_env, only: int64, real64, real128
    implicit none
    real(real128), parameter :: q = -1.5_real128
    real(real64), parameter :: d = -1.5_real128
    integer, parameter :: i = -2.5_real128
    integer(int64), parameter :: j = -3.75_real128
    real(real128) :: x
    real(real64) :: y
    integer :: k
    x = -2.25_real128
    y = -0.5_real128
    k = -4.5_real128
    print *, q, d, i, j, x, y, k
    if (q /= -1.5_real128) error stop 1
    if (.not. (q < 0.0_real128)) error stop 2
    if (d /= -1.5_real64) error stop 3
    if (i /= -2) error stop 4
    if (j /= -3_int64) error stop 5
    if (x /= -2.25_real128) error stop 6
    if (x + 2.25_real128 /= 0.0_real128) error stop 7
    if (y /= -0.5_real64) error stop 8
    if (k /= -4) error stop 9
end program real128_cast_04
