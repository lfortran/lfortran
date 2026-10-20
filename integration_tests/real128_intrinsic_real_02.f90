program real128_intrinsic_real_02
    ! Converting a real(128) value known only at run time to real(8) must
    ! round correctly also when rounding up carries into the exponent.
    use, intrinsic :: iso_fortran_env, only: real128, real64, int64
    implicit none
    real(real128) :: x, zero
    real(real64) :: d
    integer :: n

    n = command_argument_count()
    zero = n

    ! 1 - 2**-113 rounds up to 1.
    x = transfer([-1_int64, 4611404543450677247_int64 + n], x)
    d = real(x, real64)
    print *, d
    if (d /= 1) error stop 1

    ! -(4 - 2**-70) rounds to -4.
    x = 4 + zero
    x = -(x - scale(x, -72))
    d = real(x, real64)
    print *, d
    if (d /= -4) error stop 2

    ! The largest real(128) below huge(1._real64) * (1 + 2**-53) rounds to
    ! huge(1._real64); huge(1._real64) * (1 + 2**-53) itself overflows.
    x = real(huge(d), real128) + zero
    x = x + scale(x, -54)
    d = real(x, real64)
    print *, d
    if (d /= huge(d)) error stop 3

    ! A value below 1.5 must not become 1.5.
    x = 1.5_real128 - scale(1 + zero, -60)
    d = real(x, real64)
    print *, d
    if (d /= 1.5_real64) error stop 4

    x = 2 - scale(1 + zero, -60)
    d = real(x, real64)
    print *, d
    if (d /= 2) error stop 5
end program
