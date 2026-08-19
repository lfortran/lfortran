program real128_intrinsic_math_01
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) :: v
    real :: r
    integer :: n

    ! Runtime-only values so the intrinsics below go through the runtime
    ! library instead of compile-time folding.
    n = command_argument_count()

    r = 100.0 + real(n)
    v = r
    if (abs(log10(v) - 2.0_real128) > 1.0e-30_real128) error stop 1

    r = 2.0 + real(n)
    v = r
    if (abs(log10(v) - 0.301029995663981195213738894724493027_real128) &
        > 1.0e-30_real128) error stop 2

    r = 10.0 + real(n)
    v = r
    if (abs(log10(v) - 1.0_real128) > 1.0e-30_real128) error stop 3

    r = 1.23 + real(n)
    v = r
    if (abs(log10(v) - 8.9905111439979327e-2_real128) > 1.0e-7_real128) &
        error stop 4

    r = 2.0 + real(n)
    v = r
    if (abs(sqrt(v) - 1.41421356237309504880168872420969808_real128) &
        > 1.0e-30_real128) error stop 5

    print *, "ok"
end program real128_intrinsic_math_01
