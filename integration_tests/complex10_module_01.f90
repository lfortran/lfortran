program complex10_module_01
    use complex10_module_01_mod
    implicit none
    complex(xp) :: z, a(2)
    real(xp), parameter :: tol = 1.0e-17_xp

    if (zero_cxp /= 0) error stop "zero"
    if (i_cxp * i_cxp /= -1) error stop "i*i"
    if (abs(real(c1) - 0.1_xp) > tol .or. abs(aimag(c1) + 0.3_xp) > tol) error stop "c1"
    if (abs(real(c2) - 0.1_xp) > tol .or. aimag(c2) /= 2) error stop "c2"
    if (c3 /= c1) error stop "c3"
    if (c4 /= (1, 2)) error stop "c4"
    if (g /= (1._xp, 2._xp)) error stop "g"
    g = g + c1
    if (abs(real(g) - 1.1_xp) > tol) error stop "g update"
    if (abs(rarr(1) - 100) > tol .or. abs(rarr(3) - 1) > tol) error stop "rarr"
    if (carr(2) /= (3, 4) .or. carr(1) /= (1, 2)) error stop "carr"
    if (rcat(1) /= 100 .or. rcat(3) /= 1 .or. rcat(5) /= 8) error stop "rcat"
    if (ccat(1) /= (1, 2) .or. ccat(2) /= (3, 4) .or. ccat(3) /= (5, 6)) error stop "ccat"
    z = twice(c1)
    if (abs(real(z) - 0.2_xp) > tol) error stop "twice"
    a = twice(carr)
    if (a(1) /= (2, 4) .or. a(2) /= (6, 8)) error stop "twice array"
    z = make(0.25_xp, -0.5_xp)
    if (z /= (0.25_xp, -0.5_xp)) error stop "make"
    print *, "ok"
end program
