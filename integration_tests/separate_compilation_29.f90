program separate_compilation_29
    use separate_compilation_29a_module, only: sa, get_a
    use separate_compilation_29b_module, only: sb, get_b
    implicit none

    call sa()
    call sb()

    if (get_a() /= 1) error stop 1
    if (get_b() /= 2) error stop 2
end program separate_compilation_29
