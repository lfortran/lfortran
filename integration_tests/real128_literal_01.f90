program real128_literal_01
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) :: x, y
    x = 1.234567890123456789012345678901234_real128
    y = 2.0_real128 + x
    print   *, "x = ", x
    print   *, "y = ", y
end program
