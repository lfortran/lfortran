program real128_int_to_real_02
    use, intrinsic :: iso_fortran_env, only: int64, real128
    implicit none

    real(real128), parameter :: threeqp = 3, thirdqp = 1/threeqp
    real(real128), parameter :: six = threeqp*2
    real(real128), parameter :: neg = -7 + threeqp
    real(real128), parameter :: big = 9007199254740993_int64
    real(real128), parameter :: q = real(1, real128)/3
    real(real128) :: x

    if (thirdqp /= 1.0_real128/3.0_real128) error stop 1
    if (six /= 6.0_real128) error stop 2
    if (neg /= -4.0_real128) error stop 3
    if (big /= 9007199254740993.0_real128) error stop 4
    if (q /= thirdqp) error stop 5
    x = 2
    if (x /= 2.0_real128) error stop 6
    x = threeqp*2
    if (x /= six) error stop 7
    x = -threeqp
    if (x /= -3.0_real128) error stop 8
    print *, thirdqp
end program
