program real128_arith_07
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) :: a, b
    a = -5.5_real128
    b = abs(a)
    print *, b
end program
