program real128_arith_05
    !! support needed to add
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) :: a, b
    a = 2.0_real128
    b = a ** 10
    print *, b
end program
