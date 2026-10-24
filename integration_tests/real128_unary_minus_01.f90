program real128_unary_minus_01
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128), parameter :: a = -4.0_real128
    real(real128), parameter :: b = 2.5_real128
    real(real128), parameter :: c = -b
    real(real128) :: x

    if (a /= 0.0_real128 - 4.0_real128) error stop 1
    if (c /= 0.0_real128 - 2.5_real128) error stop 2
    if (-a /= 4.0_real128) error stop 3
    if (a + 4.0_real128 /= 0.0_real128) error stop 4
    x = -1.5_real128
    if (x + 1.5_real128 /= 0.0_real128) error stop 5
    x = -c
    if (x /= b) error stop 6
    print *, a, c
end program
