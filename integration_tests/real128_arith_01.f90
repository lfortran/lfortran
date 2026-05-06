program real128_arith_01
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) :: a, b, c
    a = 1.5_real128
    b = 2.5_real128
    c = a + b
    if (c /= 4.0_real128) error stop "real128 add failed"
    print *, c
end program
