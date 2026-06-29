program real128_arith_01
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) :: a, b, c
    a = 1.5_real128
    print *, a
    b = 2.5_real128
    c = a + b
    c = 2*c
    print *, c
end program
