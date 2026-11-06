program real128_cast_02
    ! real128 constants converted to integer and complex are folded at
    ! compile time and must keep their value.
    use, intrinsic :: iso_fortran_env, only: int64, real64, real128
    implicit none
    integer, parameter :: i = 2.5_real128
    integer(int64), parameter :: j = 0.25_real128 - 8.0_real128
    complex(real64), parameter :: z = 1.5_real128
    integer :: k
    complex(real64) :: w
    print *, i, j, z
    if (i /= 2) error stop 1
    if (j /= -7_int64) error stop 2
    if (z /= (1.5_real64, 0.0_real64)) error stop 3
    k = 3.75_real128
    if (k /= 3) error stop 4
    w = 0.5_real128 - 1.0_real128
    if (w /= (-0.5_real64, 0.0_real64)) error stop 5
end program real128_cast_02
