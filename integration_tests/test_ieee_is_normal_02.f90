! ieee_is_normal must return .true. for positive and negative zero
program test_ieee_is_normal_02
    use, intrinsic :: ieee_arithmetic, only: ieee_is_normal, ieee_value, &
        ieee_positive_inf, ieee_quiet_nan, ieee_positive_denormal
    use, intrinsic :: iso_fortran_env, only: real32, real64
    implicit none

    real(real32) :: xs, zs(3)
    real(real64) :: xd, zd(3)
    logical :: ls(3), ld(3)

    if (.not. ieee_is_normal(0.0)) error stop 1
    if (.not. ieee_is_normal(-0.0)) error stop 2
    if (.not. ieee_is_normal(0.0_real64)) error stop 3
    if (.not. ieee_is_normal(-0.0_real64)) error stop 4

    xs = 0.0_real32
    if (.not. ieee_is_normal(xs)) error stop 5
    xs = -xs
    if (.not. ieee_is_normal(xs)) error stop 6
    xd = 0.0_real64
    if (.not. ieee_is_normal(xd)) error stop 7
    xd = -xd
    if (.not. ieee_is_normal(xd)) error stop 8

    zs = [0.0_real32, 1.5_real32, 0.0_real32]
    zs(1) = ieee_value(zs(1), ieee_positive_inf)
    ls = ieee_is_normal(zs)
    if (ls(1) .or. .not. ls(2) .or. .not. ls(3)) error stop 9

    zd = [0.0_real64, -2.5_real64, 0.0_real64]
    zd(1) = ieee_value(zd(1), ieee_quiet_nan)
    zd(3) = -zd(3)
    ld = ieee_is_normal(zd)
    if (ld(1) .or. .not. ld(2) .or. .not. ld(3)) error stop 10

    xs = ieee_value(xs, ieee_positive_denormal)
    if (ieee_is_normal(xs)) error stop 11
    xd = ieee_value(xd, ieee_positive_denormal)
    if (ieee_is_normal(xd)) error stop 12

    print *, "ok"
end program
