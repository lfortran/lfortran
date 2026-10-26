program real128_cast_01
    use real128_cast_01_mod, only: half, quarter, one, three_halves, &
        nine_quarters, scale_half
    use, intrinsic :: iso_fortran_env, only: real32, real64, real128
    implicit none
    real(real128) :: x
    real(real128) :: q = 1.0_real128
    real(real64) :: d
    call scale_half(3.0_real128, x)
    print *, x
    if (x /= 1.5_real128) error stop 1
    if (half /= 0.5_real128) error stop 2
    if (quarter /= 0.25_real128) error stop 3
    if (one /= 1.0_real128) error stop 4
    if (three_halves /= 1.5_real64) error stop 5
    if (nine_quarters /= 2.25_real32) error stop 6
    x = 0.75
    if (x /= 0.75_real128) error stop 7
    d = 1.25_real128
    if (d /= 1.25_real64) error stop 8
    d = 2.0_real64 * 1.5_real128
    if (d /= 3.0_real64) error stop 9
    ! Comparisons with a double constant fold the converted constant too.
    if (.not. (q == 1.0_real64)) error stop 10
    if (.not. (huge(d) < huge(q))) error stop 11
end program real128_cast_01
