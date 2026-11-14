program real_pow_fast_01
    real :: y, e(1), r(1)
    real(8) :: z, f(1), s(1)
    e = 2
    f = 2
    ! The exponent is an array element, so this is not the constant
    ! fold of 10**2.
    y = 10.0 ** e(1)
    z = 10.0d0 ** f(1)
    r = 10.0 ** e
    s = 10.0d0 ** f
    if (y /= 100.0) error stop 1
    if (z /= 100.0d0) error stop 2
    if (r(1) /= 100.0) error stop 3
    if (s(1) /= 100.0d0) error stop 4
end program
