program real10_aint_kind_fold_01
    ! aint(x, kind) and anint(x, kind) of real(10) constants are rounded in
    ! real(10) and then converted to the result kind, and those of real(4),
    ! real(8) and real(16) constants to real(10), for array and scalar named
    ! constants, initializers and expressions
    implicit none
    real(10), parameter :: c(3) = [1.5_10, -2.75_10, 2.0_10**62 + 3.5_10]
    real(16), parameter :: q(3) = [1.5_16, -2.75_16, 2.0_16**62 + 3.5_16]
    real(8), parameter :: d(3) = [1.5_8, -2.75_8, 2.0_8**50 + 3.5_8]
    real(4), parameter :: s(3) = [1.5_4, -2.75_4, 2.0_4**20 + 0.5_4]
    ! Truncated and rounded, in the precision of real(10) and real(16)
    real(16), parameter :: t16(3) = [1.0_16, -2.0_16, 2.0_16**62 + 3]
    real(16), parameter :: r16(3) = [2.0_16, -3.0_16, 2.0_16**62 + 4]

    real(4), parameter :: a4(3) = aint(c, 4), n4(3) = anint(c, 4)
    real(8), parameter :: a8(3) = aint(c, 8), n8(3) = anint(c, 8)
    real(16), parameter :: a16(3) = aint(c, 16), n16(3) = anint(c, 16)
    real(10), parameter :: aq(3) = aint(q, 10), nq(3) = anint(q, 10)
    real(10), parameter :: ad(3) = aint(d, 10), nd(3) = anint(d, 10)
    real(10), parameter :: as(3) = aint(s, 10), ns(3) = anint(s, 10)
    real(8), parameter :: a8s = aint(c(3), 8), n8s = anint(c(2), 8)
    real(16), parameter :: a16s = aint(c(3), 16), n16s = anint(c(3), 16)
    real(10), parameter :: aqs = aint(q(3), 10), nqs = anint(q(3), 10)

    real(4) :: va4(3) = aint(c, 4), vn4(3) = anint(c, 4)
    real(8) :: va8(3) = aint(c, 8), vn8(3) = anint(c, 8)
    real(16) :: va16(3) = aint(c, 16), vn16(3) = anint(c, 16)
    real(10) :: vaq(3) = aint(q, 10), vnq(3) = anint(q, 10)
    real(8) :: va8s = aint(1.5_10, 8), vn8s = anint(-2.75_10, 8)

    real(4) :: f4(3)
    real(8) :: f8(3), f8s
    real(16) :: f16(3)
    real(10) :: f10(3), f10s

    if (any(a4 /= real(t16, 4)) .or. any(n4 /= real(r16, 4))) error stop
    if (any(a8 /= real(t16, 8)) .or. any(n8 /= real(r16, 8))) error stop
    if (any(a16 /= t16) .or. any(n16 /= r16)) error stop
    if (any(aq /= real(t16, 10)) .or. any(nq /= real(r16, 10))) error stop
    if (any(ad /= [1.0_10, -2.0_10, 2.0_10**50 + 3]) .or. &
        any(nd /= [2.0_10, -3.0_10, 2.0_10**50 + 4])) error stop
    if (any(as /= [1.0_10, -2.0_10, 2.0_10**20]) .or. &
        any(ns /= [2.0_10, -3.0_10, 2.0_10**20 + 1])) error stop
    if (a8s /= 2.0_8**62 .or. n8s /= -3) error stop
    if (a16s /= t16(3) .or. n16s /= r16(3)) error stop
    if (aqs /= real(t16(3), 10) .or. nqs /= real(r16(3), 10)) error stop

    if (any(va4 /= a4) .or. any(vn4 /= n4)) error stop
    if (any(va8 /= a8) .or. any(vn8 /= n8)) error stop
    if (any(va16 /= a16) .or. any(vn16 /= n16)) error stop
    if (any(vaq /= aq) .or. any(vnq /= nq)) error stop
    if (va8s /= 1 .or. vn8s /= -3) error stop

    f4 = aint(c, 4)
    if (any(f4 /= a4)) error stop
    f4 = anint(c, 4)
    if (any(f4 /= n4)) error stop
    f8 = aint(c, 8)
    if (any(f8 /= a8)) error stop
    f8 = anint(c, 8)
    if (any(f8 /= n8)) error stop
    f16 = aint(c, 16)
    if (any(f16 /= t16)) error stop
    f16 = anint(c, 16)
    if (any(f16 /= r16)) error stop
    f10 = aint(q, 10)
    if (any(f10 /= aq)) error stop
    f10 = anint(q, 10)
    if (any(f10 /= nq)) error stop
    f8s = aint(c(1), 8)
    if (f8s /= 1) error stop
    f10s = anint(q(2), 10)
    if (f10s /= -3) error stop
    print *, a8, n16, aq
end program
