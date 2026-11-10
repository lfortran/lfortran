program complex128_module_01
    use complex128_module_01_mod
    implicit none
    complex(qp) :: z, a(2)
    real(qp), parameter :: tol = 1.0e-30_qp

    if (zero_cqp /= 0) error stop "zero"
    if (i_cqp * i_cqp /= -1) error stop "i*i"
    if (abs(real(c1) - 0.1_qp) > tol .or. abs(aimag(c1) + 0.3_qp) > tol) error stop "c1"
    if (abs(real(c2) - 0.1_qp) > tol .or. aimag(c2) /= 2) error stop "c2"
    if (c3 /= c1) error stop "c3"
    if (c4 /= (1, 2)) error stop "c4"
    if (g /= (1._qp, 2._qp)) error stop "g"
    g = g + c1
    if (abs(real(g) - 1.1_qp) > tol) error stop "g update"
    if (abs(sqrt_2_pi - 2.50662827463100050241576528481104525_qp) > tol) error stop "sqrt_2_pi"
    if (abs(e_qp - 2.71828182845904523536028747135266250_qp) > tol) error stop "e"
    if (abs(rarr(1) - 100) > tol .or. abs(rarr(3) - 1) > tol) error stop "rarr"
    if (carr(2) /= (3, 4) .or. carr(1) /= (1, 2)) error stop "carr"
    if (rcat(1) /= 100 .or. rcat(3) /= 1 .or. rcat(5) /= 8) error stop "rcat"
    if (ccat(1) /= (1, 2) .or. ccat(2) /= (3, 4) .or. ccat(3) /= (5, 6)) error stop "ccat"
    z = twice(c1)
    if (abs(real(z) - 0.2_qp) > tol) error stop "twice"
    a = twice(carr)
    if (a(1) /= (2, 4) .or. a(2) /= (6, 8)) error stop "twice array"
    z = make(0.25_qp, -0.5_qp)
    if (z /= (0.25_qp, -0.5_qp)) error stop "make"
    if (abs(pi - 4 * atan(1.0_qp)) > tol) error stop "pi"
    print *, "ok"
end program
