program real128_arith_06
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) :: a, b, c
    a = 1.234567890123456789_real128
    b = 9.876543210987654321_real128
    c = a + b
    print *, c
end program
