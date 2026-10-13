program template_operator_mod_02
    use template_operator_mod_02_user
    implicit none
    type(a_t) :: a
    type(b_t) :: b

    call set(a, 4)
    if (a%i /= 8) error stop 1
    a = 5
    if (a%i /= 10) error stop 2
    call set_b(b, a_t(3))
    if (abs(b%r - 3.5) > 1e-6) error stop 3
    print *, "ok"
end program
