program real10_aint_kind_scalar_fold_01
    ! aint(x, kind) and anint(x, kind) of real(10) scalar constants are
    ! rounded in real(10) and then converted to the result kind, and those of
    ! real(4), real(8) and real(16) constants to real(10), for named
    ! constants, initializers and expressions
    implicit none
    ! 2**62 + 3.5 is not representable in double
    real(10), parameter :: c1 = 1.5_10, c2 = -2.75_10, c3 = 2.0_10**62 + 3.5_10
    real(16), parameter :: q3 = 2.0_16**62 + 3.5_16
    ! Truncated and rounded, in the precision of real(10) and real(16)
    real(16), parameter :: t16 = 2.0_16**62 + 3, r16 = 2.0_16**62 + 4

    real(4), parameter :: a4 = aint(c2, 4), n4 = anint(c2, 4)
    real(8), parameter :: a8 = aint(c3, 8), n8 = anint(c2, 8)
    real(16), parameter :: a16 = aint(c3, 16), n16 = anint(c3, 16)
    real(10), parameter :: aq = aint(q3, 10), nq = anint(q3, 10)
    real(10), parameter :: ad = aint(2.0_8**50 + 3.5_8, 10)
    real(10), parameter :: nd = anint(2.0_8**50 + 3.5_8, 10)
    real(10), parameter :: as = aint(-2.75_4, 10), ns = anint(-2.75_4, 10)
    real(8) :: va8 = aint(1.5_10, 8), vn8 = anint(-2.75_10, 8)
    real(16) :: va16 = aint(c3, 16)
    real(10) :: vnq = anint(q3, 10)
    real(8) :: f8
    real(10) :: f10

    if (a4 /= -2 .or. n4 /= -3) error stop
    if (a8 /= 2.0_8**62 .or. n8 /= -3) error stop
    if (a16 /= t16 .or. n16 /= r16) error stop
    if (aq /= real(t16, 10) .or. nq /= real(r16, 10)) error stop
    if (ad /= 2.0_10**50 + 3 .or. nd /= 2.0_10**50 + 4) error stop
    if (as /= -2 .or. ns /= -3) error stop
    if (va8 /= 1 .or. vn8 /= -3) error stop
    if (va16 /= t16 .or. vnq /= nq) error stop

    f8 = aint(c1, 8)
    if (f8 /= 1) error stop
    f8 = anint(c1, 8)
    if (f8 /= 2) error stop
    f10 = anint(q3, 10)
    if (f10 /= nq) error stop
    f10 = aint(q3, 10)
    if (f10 /= aq) error stop
    print *, a8, n16, aq
end program
