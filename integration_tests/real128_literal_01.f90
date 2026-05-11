program real128_literal_01
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none

    real(real128) :: x, y

    x = 1.234567890123456789012345678901234_real128
    y = 2.0_real128 + x

    if (y <= 3.23_real128 .or. y >= 3.24_real128) error stop
end program
