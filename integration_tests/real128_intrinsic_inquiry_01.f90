program real128_intrinsic_inquiry_01
    use, intrinsic :: iso_fortran_env, only: real128

    real(real128), parameter :: x = 0.0_real128
    real(real128), parameter :: h = huge(x)
    real(real128), parameter :: expected_huge = &
        1.18973149535723176508575932662800702e4932_real128
    real(real128), parameter :: t = tiny(x)
    real(real128), parameter :: expected_tiny = &
        3.36210314311209350626267781732175260e-4932_real128
    integer, parameter :: d = digits(x)
    integer, parameter :: real128_significant_digits = &
        int(log10(2.0_real128**digits(0.0_real128)))

    real(real128) :: y
    real(real128) :: z
    integer :: e

    if (kind(h) /= real128) error stop 10
    if (abs((h - expected_huge) / expected_huge) > 1.0e-30_real128) error stop 1

    y = huge(1.0_real128)
    if (kind(y) /= real128) error stop 11

    if (kind(t) /= real128) error stop 12
    if (abs((t - expected_tiny) / expected_tiny) > 1.0e-30_real128) error stop 3

    z = tiny(1.0_real128)
    if (kind(z) /= real128) error stop 13

    if (d /= 113) error stop 5

    e = digits(1.0_real128)
    if (e /= 113) error stop 6

end program
