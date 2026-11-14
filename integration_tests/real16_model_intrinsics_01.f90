program real16_model_intrinsics_01
    implicit none
    integer, parameter :: qp = 16
    real(qp) :: x, y, z
    real(qp) :: a(4), r(4)
    integer :: e(4)
    integer :: n
    integer(8) :: n8
    integer(2) :: n2

    x = 2.75_qp
    if (exponent(x) /= 2) error stop 1
    if (fraction(x) /= 0.6875_qp) error stop 2
    y = 2
    if (set_exponent(y, 3) /= 4.0_qp) error stop 3
    if (set_exponent(x, 3) /= 5.5_qp) error stop 4
    if (rrspacing(y) /= 2.0_qp**112) error stop 5
    if (rrspacing(-x) /= x / spacing(x)) error stop 6

    x = 1
    if (spacing(x) /= epsilon(x)) error stop 7
    if (nearest(x, 1.0_qp) /= 1 + epsilon(x)) error stop 8
    if (nearest(x, -1.0_qp) /= 1 - epsilon(x) / 2) error stop 9
    if (nearest(-x, -1.0_qp) /= -1 - epsilon(x)) error stop 10

    x = 8
    if (exponent(x) /= 4) error stop 11
    if (fraction(x) /= 0.5_qp) error stop 12
    if (set_exponent(x, 2) /= 2.0_qp) error stop 13
    if (rrspacing(x) /= 2.0_qp**112) error stop 14

    ! zero, tiny, a subnormal and huge
    x = 0
    if (exponent(x) /= 0) error stop 20
    if (fraction(x) /= 0) error stop 21
    if (set_exponent(x, 5) /= 0) error stop 22
    if (rrspacing(x) /= 0) error stop 23
    if (spacing(x) /= tiny(x)) error stop 24
    if (nearest(x, 1.0_qp) /= tiny(x) * epsilon(x)) error stop 25
    if (nearest(x, -1.0_qp) /= -tiny(x) * epsilon(x)) error stop 26
    x = tiny(x)
    if (exponent(x) /= -16381) error stop 27
    if (spacing(x) /= tiny(x)) error stop 28
    y = x / 2
    if (exponent(y) /= -16382) error stop 29
    if (fraction(y) /= 0.5_qp) error stop 30
    if (spacing(y) /= tiny(x)) error stop 31
    x = huge(x)
    if (exponent(x) /= 16384) error stop 32
    if (spacing(x) /= 2.0_qp**16271) error stop 33
    z = nearest(x, -1.0_qp)
    if (z >= x) error stop 34
    if (x - z /= spacing(x)) error stop 35

    ! array arguments
    a = [1.0_qp, 2.75_qp, -8.0_qp, 0.0_qp]
    e = exponent(a)
    if (any(e /= [1, 2, 4, 0])) error stop 40
    r = fraction(a)
    if (any(r /= [0.5_qp, 0.6875_qp, -0.5_qp, 0.0_qp])) error stop 41
    n = 3
    r = set_exponent(a, n)
    if (any(r /= [4.0_qp, 5.5_qp, -4.0_qp, 0.0_qp])) error stop 42
    r = spacing(a)
    if (r(1) /= epsilon(x)) error stop 43
    if (r(4) /= tiny(x)) error stop 44
    r = rrspacing(a)
    if (r(1) /= 2.0_qp**112) error stop 45
    if (r(3) /= 2.0_qp**112) error stop 46
    r = nearest(a, 1.0_qp)
    if (r(1) /= 1 + epsilon(x)) error stop 47
    r = nearest(a, -a)
    if (r(1) >= a(1)) error stop 48
    if (r(3) <= a(3)) error stop 49
    n8 = 3_8
    if (set_exponent(a(2), n8) /= 5.5_qp) error stop 50
    n2 = 3_2
    if (set_exponent(a(2), n2) /= 5.5_qp) error stop 52
    x = huge(x)
    y = x * 2
    if (exponent(y) /= huge(n)) error stop 53
    print *, "ok"
end program
