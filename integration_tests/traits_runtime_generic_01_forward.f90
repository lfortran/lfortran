! Uses the existing frozen provider and unchanged late layout/identity matrix.
program traits_runtime_generic_forwarding
    use traits_runtime_generic_01_contracts_m, only: IAlgorithm, make_algorithm
    use traits_runtime_generic_01_matrix_client_m, only: LateValue, PaddedValue, AlternateValue, argument_finals
    use traits_runtime_generic_forwarding_m, only: forward, forward_twice
    implicit none
    type(LateValue) :: value
    type(PaddedValue) :: padded
    type(AlternateValue) :: alternate
    class(IAlgorithm), allocatable :: implementation
    integer :: i, choice, ordinary, doubled
    value%payload = 37
    padded%prefix = [1000.0_8, -999.0_8, 42.0_8]
    padded%payload = 37
    alternate%payload = 37
    do i = 0, 1
        choice = mod(command_argument_count() + i, 2)
        call make_algorithm(choice, implementation)
        if (choice == 0) then
            ordinary = 47
            doubled = 84
        else
            ordinary = 174
            doubled = 248
        end if
        if (forward(implementation, value) /= ordinary) error stop 1
        if (forward{PaddedValue}(implementation, padded) /= ordinary) error stop 2
        if (forward(implementation, alternate) /= doubled) error stop 3
        if (forward_twice(implementation, value) /= 2 * ordinary) error stop 4
        if (forward_twice{PaddedValue}(implementation, padded) /= 2 * ordinary) error stop 5
        if (forward_twice(implementation, alternate) /= 2 * doubled) error stop 6
        if (argument_finals /= 0) error stop 7
        if (any(padded%prefix /= [1000.0_8, -999.0_8, 42.0_8])) error stop 8
        if (value%payload /= 37 .or. padded%payload /= 37 .or. alternate%payload /= 37) error stop 9
        deallocate(implementation)
        if (argument_finals /= 0) error stop 10
    end do
end program
