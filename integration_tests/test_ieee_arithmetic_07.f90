program test_ieee_arithmetic_07
    ! ieee_arithmetic inquiry functions and ieee_copy_sign with real(10)
    use, intrinsic :: ieee_arithmetic
    implicit none
    real(10) :: x, y, zero, pinf, ninf, qnan, den, nzero
    real(10) :: a(3)
    integer(8) :: bits(2)

    zero = 0
    pinf = 1 / zero
    ninf = -1 / zero
    qnan = zero / zero
    nzero = -zero
    bits(1) = 1
    bits(2) = 0
    den = transfer(bits, den)
    x = 1.0_10
    y = 2.5_10

    if (.not. ieee_is_normal(x)) error stop 1
    if (.not. ieee_is_normal(-x)) error stop 2
    if (.not. ieee_is_normal(zero)) error stop 3
    if (ieee_is_normal(pinf)) error stop 4
    if (ieee_is_normal(qnan)) error stop 5
    if (ieee_is_normal(den)) error stop 6

    if (ieee_is_nan(x)) error stop 7
    if (ieee_is_nan(pinf)) error stop 8
    if (.not. ieee_is_nan(qnan)) error stop 9

    if (.not. ieee_is_finite(x)) error stop 10
    if (.not. ieee_is_finite(den)) error stop 11
    if (ieee_is_finite(pinf)) error stop 12
    if (ieee_is_finite(ninf)) error stop 13
    if (ieee_is_finite(qnan)) error stop 14

    if (ieee_is_negative(x)) error stop 15
    if (.not. ieee_is_negative(-x)) error stop 16
    if (.not. ieee_is_negative(nzero)) error stop 17
    if (ieee_is_negative(zero)) error stop 18
    if (.not. ieee_is_negative(ninf)) error stop 19
    if (ieee_is_negative(qnan)) error stop 20

    if (ieee_copy_sign(y, -x) /= -y) error stop 21
    if (ieee_copy_sign(-y, x) /= y) error stop 22
    if (.not. ieee_is_negative(ieee_copy_sign(zero, -x))) error stop 23
    if (ieee_copy_sign(pinf, nzero) /= ninf) error stop 24

    if (ieee_class(x) /= ieee_positive_normal) error stop 25
    if (ieee_class(-x) /= ieee_negative_normal) error stop 26
    if (ieee_class(zero) /= ieee_positive_zero) error stop 27
    if (ieee_class(nzero) /= ieee_negative_zero) error stop 28
    if (ieee_class(den) /= ieee_positive_denormal) error stop 29
    if (ieee_class(-den) /= ieee_negative_denormal) error stop 30
    if (ieee_class(pinf) /= ieee_positive_inf) error stop 31
    if (ieee_class(ninf) /= ieee_negative_inf) error stop 32
    if (.not. (ieee_class(qnan) == ieee_quiet_nan)) error stop 33

    a = [x, pinf, qnan]
    if (any(ieee_is_finite(a) .neqv. [.true., .false., .false.])) error stop 34
    if (any(ieee_is_nan(a) .neqv. [.false., .false., .true.])) error stop 35

    print *, ieee_is_normal(x), ieee_is_nan(qnan), ieee_is_finite(pinf)
end program
