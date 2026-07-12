program separate_compilation_50
    use separate_compilation_50a_mod
    use separate_compilation_50b_mod
    implicit none
    real(8) :: x
    integer :: r1, r2
    x = 3.14d0
    call get_exp_a(x, r1)
    call get_exp_b(x, r2)
    print *, r1, r2
    if (r1 /= r2) error stop
end program separate_compilation_50
