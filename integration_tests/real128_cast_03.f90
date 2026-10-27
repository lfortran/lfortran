program real128_cast_03
    use real128_cast_03_mod, only: half, quarter, two, three_halves, n, zh
    use, intrinsic :: iso_fortran_env, only: int64, real32, real64, real128
    implicit none
    real(real128) :: h
    parameter (h = 0.75)
    real(real64) :: d
    parameter (d = 8.0_real128 - 0.25_real128)
    integer(int64) :: k
    parameter (k = 3.75_real128)
    real(real128), parameter :: q = 4.5_real128
    complex(real32) :: zs
    parameter (zs = 2.5_real128)
    complex(real64) :: zn
    parameter (zn = q)
    real(real64), parameter :: zhr = real(zh)
    real(real32), parameter :: zsr = real(zs)
    real(real64), parameter :: znr = real(zn)
    print *, half, quarter, two, three_halves, n
    if (half /= 0.5_real128) error stop 1
    if (quarter /= 0.25_real128) error stop 2
    if (two /= 2.0_real128) error stop 3
    if (three_halves /= 1.5_real64) error stop 4
    if (n /= 2) error stop 5
    if (h /= 0.75_real128) error stop 6
    if (d /= 7.75_real64) error stop 7
    if (k /= 3_int64) error stop 8
    print *, zh, zs, zn
    if (zhr /= 1.5_real64) error stop 9
    if (zsr /= 2.5_real32) error stop 10
    if (znr /= 4.5_real64) error stop 11
    if (zh /= (1.5_real64, 0.0_real64)) error stop 12
    if (zs /= (2.5_real32, 0.0_real32)) error stop 13
    if (zn /= (4.5_real64, 0.0_real64)) error stop 14
end program real128_cast_03
