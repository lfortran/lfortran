program traits_runtime_generic_01
    use traits_runtime_generic_01_contracts_m, only: IAlgorithm, make_algorithm
    use traits_runtime_generic_01_late_client_m, only: LateValue
    use traits_runtime_generic_01_consumer_m, only: invoke
    implicit none
    type(LateValue) :: value
    class(IAlgorithm), allocatable :: implementation
    integer :: i, choice
    value%payload = 37
    do i = 0, 1
        choice = mod(command_argument_count() + i, 2)
        call make_algorithm(choice, implementation)
        if (choice == 0) then
            if (invoke(implementation, value) /= 47) error stop 1101
        else
            if (invoke(implementation, value) /= 174) error stop 1102
        end if
        deallocate(implementation)
    end do
end program traits_runtime_generic_01
