program real128_arith_04
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) :: a, b, c
    a = 22.0_real128
    b = 7.0_real128
    c = a / b
    print *, c
end program
