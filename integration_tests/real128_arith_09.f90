program real128_arith_09
    !support needed to add
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) :: a, b, c
    a = 5.0_real128
    b = 2.0_real128
    c = mod(a, b)
    print *, c
end program
