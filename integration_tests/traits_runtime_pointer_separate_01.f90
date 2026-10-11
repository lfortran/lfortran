program traits_runtime_pointer_separate_01
    use traits_runtime_pointer_separate_01_contracts
    use traits_runtime_pointer_separate_01_consumer
    use traits_runtime_pointer_separate_01_a, only: install_a
    use traits_runtime_pointer_separate_01_b, only: install_b
    implicit none
    class(IValue), pointer :: first, second, alias
    integer :: round, expected
    do round = 1, 2
        call install_a(first, 17)
        call install_b(second, 14)
        if (mod(command_argument_count() + round, 2) == 0) then
            call forward(first, alias)
            expected = 17
        else
            call forward(second, alias)
            expected = 29
        end if
        call observe(alias, expected)
        nullify(first, second)
        call observe(alias, expected)
        call install_a(first, 31)
        call observe(first, 31)
        if (expected == 17) call observe(alias, 31)
        if (expected == 29) call observe(alias, 29)
        nullify(first, alias)
    end do
end program
