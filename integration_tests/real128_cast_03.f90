program real128_cast_03
    use real128_cast_03_mod, only: half, quarter, two, three_halves, n
    use, intrinsic :: iso_fortran_env, only: int64, real64, real128
    implicit none
    real(real128) :: h
    parameter (h = 0.75)
    real(real64) :: d
    parameter (d = 8.0_real128 - 0.25_real128)
    integer(int64) :: k
    parameter (k = 3.75_real128)
    print *, half, quarter, two, three_halves, n
    if (half /= 0.5_real128) error stop 1
    if (quarter /= 0.25_real128) error stop 2
    if (two /= 2.0_real128) error stop 3
    if (three_halves /= 1.5_real64) error stop 4
    if (n /= 2) error stop 5
    if (h /= 0.75_real128) error stop 6
    if (d /= 7.75_real64) error stop 7
    if (k /= 3_int64) error stop 8
end program real128_cast_03
