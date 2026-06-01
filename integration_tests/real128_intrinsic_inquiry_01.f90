program real128_intrinsic_inquiry_01
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none

    integer, parameter :: wp = selected_real_kind(33)
    integer, parameter :: real128_significant_digits = &
        int(log10(2.0_wp**digits(0.0_wp)))
    real(real128) :: x
    complex(real128) :: z

    if (wp /= real128) error stop 1
    if (real128_significant_digits /= 34) error stop 2
    if (storage_size(x) /= 128) error stop 3
    if (storage_size(z) /= 256) error stop 4
    if (radix(x) /= 2) error stop 5
    if (digits(x) /= 113) error stop 6
    if (precision(x) /= 33) error stop 7
    if (range(x) /= 4931) error stop 8
    if (minexponent(x) /= -16381) error stop 9
    if (maxexponent(x) /= 16384) error stop 10
    if (epsilon(x) <= 0.0_real128) error stop 11
    if (tiny(x) <= 0.0_real128) error stop 12

    x = huge(0.0_real128)

    if (kind(x) /= real128) error stop 13
    if (x <= 0.0_real128) error stop 14
    if (x /= huge(x)) error stop 15
end program
