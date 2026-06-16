program real128_intrinsic_inquiry_01
    use, intrinsic :: iso_fortran_env, only: real128

    real(real128), parameter :: x = 0.0_real128
    real(real128), parameter :: h = huge(x)
    real(real128), parameter :: expected_huge = &
        1.18973149535723176508575932662800702e4932_real128
    real(real128), parameter :: t = tiny(x)
    real(real128), parameter :: expected_tiny = &
        3.36210314311209350626267781732175260e-4932_real128
    real(real128), parameter :: eps = epsilon(x)
    real(real128), parameter :: expected_epsilon = &
        1.92592994438723585305597794258492732e-34_real128
    integer, parameter :: d = digits(x)
    integer, parameter :: p = precision(x)
    integer, parameter :: r = range(x)
    integer, parameter :: min_exp = minexponent(x)
    integer, parameter :: max_exp = maxexponent(x)
    integer, parameter :: bits = storage_size(x)
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

    if (p /= 33) error stop 7
    if (precision(1.0_real128) /= 33) error stop 8

    if (r /= 4931) error stop 9
    if (range(1.0_real128) /= 4931) error stop 14

    if (min_exp /= -16381) error stop 15
    if (minexponent(1.0_real128) /= -16381) error stop 16

    if (max_exp /= 16384) error stop 17
    if (maxexponent(1.0_real128) /= 16384) error stop 18

    if (kind(eps) /= real128) error stop 19
    if (abs((eps - expected_epsilon) / expected_epsilon) > 1.0e-30_real128) error stop 20

    z = epsilon(1.0_real128)
    if (kind(z) /= real128) error stop 21

    if (bits /= 128) error stop 22
    if (storage_size(1.0_real128) /= 128) error stop 23
    if (storage_size(cmplx(1.0_real128, 0.0_real128, kind=real128)) /= 256) error stop 24
  print *, "All tests passed."
end program
