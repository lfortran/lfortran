program template_operator_mod_01
    use template_operator_mod_01_user
    implicit none
    type(v_t) :: v

    v = square(v_t(3))
    if (v%i /= 9) error stop 1
    v = negate(v_t(4))
    if (v%i /= -4) error stop 2
    v = v_t(5) * v_t(6)
    if (v%i /= 30) error stop 3
    v = .neg. v_t(7)
    if (v%i /= -7) error stop 4
    if (isquare(3) /= 10) error stop 5
    if (inegate(4) /= -5) error stop 6
    if (bump(4) /= 5) error stop 7
    print *, "ok"
end program
