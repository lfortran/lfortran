program real128_arith_10
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) :: a, b, c
    a = 3.0_real128
    b = 4.0_real128
    c = (a + b) * (a - b)
    print *, c
end program
