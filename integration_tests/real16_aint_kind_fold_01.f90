program real16_aint_kind_fold_01
    ! aint(x, kind) and anint(x, kind) of real(16) constants are rounded in
    ! real(16) and then converted to the result kind, for array and scalar
    ! named constants, initializers and expressions
    implicit none
    real(16), parameter :: q(3) = [1.5_16, -2.75_16, 2.0_16**62 + 3.5_16]
    real(8), parameter :: d(3) = [1.5_8, -2.75_8, 2.0_8**50 + 3.5_8]
    real(4), parameter :: s(3) = [1.5_4, -2.75_4, 2.0_4**20 + 0.5_4]

    real(4), parameter :: a4(3) = aint(q, 4), n4(3) = anint(q, 4)
    real(8), parameter :: a8(3) = aint(q, 8), n8(3) = anint(q, 8)
    real(16), parameter :: ad(3) = aint(d, 16), nd(3) = anint(d, 16)
    real(16), parameter :: as(3) = aint(s, 16), ns(3) = anint(s, 16)
    real(8), parameter :: a8s = aint(q(3), 8), n8s = anint(q(2), 8)
    real(4), parameter :: a4s = aint(-2.75_16, 4)

    real(4) :: va4(3) = aint(q, 4), vn4(3) = anint(q, 4)
    real(8) :: va8(3) = aint(q, 8), vn8(3) = anint(q, 8)
    real(8) :: va8s = aint(1.5_16, 8), vn8s = anint(-2.75_16, 8)

    real(4) :: f4(3)
    real(8) :: f8(3), f8s

    if (any(a4 /= [1.0_4, -2.0_4, 2.0_4**62]) .or. &
        any(n4 /= [2.0_4, -3.0_4, 2.0_4**62])) error stop
    if (any(a8 /= [1.0_8, -2.0_8, 2.0_8**62]) .or. &
        any(n8 /= [2.0_8, -3.0_8, 2.0_8**62])) error stop
    if (any(ad /= [1.0_16, -2.0_16, 2.0_16**50 + 3]) .or. &
        any(nd /= [2.0_16, -3.0_16, 2.0_16**50 + 4])) error stop
    if (any(as /= [1.0_16, -2.0_16, 2.0_16**20]) .or. &
        any(ns /= [2.0_16, -3.0_16, 2.0_16**20 + 1])) error stop
    if (a8s /= 2.0_8**62 .or. n8s /= -3 .or. a4s /= -2) error stop

    if (any(va4 /= a4) .or. any(vn4 /= n4)) error stop
    if (any(va8 /= a8) .or. any(vn8 /= n8)) error stop
    if (va8s /= 1 .or. vn8s /= -3) error stop

    f4 = aint(q, 4)
    if (any(f4 /= a4)) error stop
    f4 = anint(q, 4)
    if (any(f4 /= n4)) error stop
    f8 = aint(q, 8)
    if (any(f8 /= a8)) error stop
    f8 = anint(q, 8)
    if (any(f8 /= n8)) error stop
    f8s = aint(q(1), 8)
    if (f8s /= 1) error stop
    print *, a8, n8
end program
