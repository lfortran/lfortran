program real128_int_to_real_01
    use, intrinsic :: iso_fortran_env, only: int64, real128
    implicit none

    integer(int64), parameter :: i = 9007199254740993_int64
    real(real128), parameter :: x = real(i, kind=real128)
    real(real128), parameter :: expected = 9007199254740993.0_real128

    if (x /= expected) error stop
end program
