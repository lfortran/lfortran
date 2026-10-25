program real128_intrinsic_elemental_01
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128), parameter :: t = 3.0_real128, s = 2.0_real128
    real(real128) :: x
    complex(8) :: c

    x = abs(t)
    if (x /= 3.0_real128) error stop 1
    x = max(t, s)
    if (x /= 3.0_real128) error stop 2
    x = min(t, s)
    if (x /= 2.0_real128) error stop 3
    x = mod(t, s)
    if (x /= 1.0_real128) error stop 4
    x = modulo(t, s)
    if (x /= 1.0_real128) error stop 5
    if (nint(t) /= 3) error stop 6
    if (floor(t) /= 3) error stop 7
    if (ceiling(t) /= 3) error stop 8
    x = aint(1.5_real128)
    if (x /= 1.0_real128) error stop 9
    x = anint(1.5_real128)
    if (x /= 2.0_real128) error stop 10
    x = dim(t, s)
    if (x /= 1.0_real128) error stop 11
    x = sqrt(4.0_real128)
    if (x /= 2.0_real128) error stop 12
    c = cmplx(1.5_real128, t, kind=8)
    if (c /= (1.5d0, 3.0d0)) error stop 13
    print *, abs(t), max(t, s), min(t, s)
end program
