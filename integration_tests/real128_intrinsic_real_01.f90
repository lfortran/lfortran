program real128_intrinsic_real_01
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none

    real(real128), parameter :: a = 1.25_real128
    real(8), parameter :: b = real(a, kind=8)
    real(real128), parameter :: c = real(a, kind=real128)

    if (abs(b - 1.25d0) > 1d-12) error stop
    if (c /= 1.25_real128) error stop
end program
