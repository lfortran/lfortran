program real10_modulo_scalar_fold_01
    ! modulo of real(10) scalar constants is folded exactly: 3**30 is large
    ! compared with 0.7, so `a - b*floor(a/b)` cancels
    implicit none
    real(10), parameter :: m1 = 3.0_10**30, m2 = -7.75_10
    real(10), parameter :: m3 = 2.0_10 - 2.0_10**(-62)
    real(10), parameter :: p1 = modulo(m1, 0.7_10), p2 = modulo(m2, 0.7_10)
    real(10), parameter :: p3 = modulo(m3, 0.7_10), pn1 = modulo(m1, -0.7_10)
    real(10), parameter :: pr1 = modulo(-1.0e25_10, m1)
    real(10), parameter :: pr2 = modulo(-1.0e25_10, m2)
    real(10) :: v1 = modulo(3.0_10**30, 0.7_10)
    real(10) :: a, b

    ! The run time computes modulo exactly
    a = m1
    b = 0.7_10
    if (p1 /= modulo(a, b) .or. v1 /= p1) error stop "p1"
    if (pn1 /= modulo(a, -b)) error stop "pn1"
    a = m2
    if (p2 /= modulo(a, b)) error stop "p2"
    a = m3
    if (p3 /= modulo(a, b)) error stop "p3"
    a = -1.0e25_10
    b = m1
    if (pr1 /= modulo(a, b)) error stop "pr1"
    b = m2
    if (pr2 /= modulo(a, b)) error stop "pr2"
    if (p1 < 0 .or. p1 >= 0.7_10 .or. pn1 > 0 .or. pn1 <= -0.7_10) error stop "sign"
    print *, p1, pn1
end program
