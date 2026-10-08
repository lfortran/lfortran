program traits_runtime_generic_matrix
    use traits_runtime_generic_01_contracts_m, only: IAlgorithm, make_algorithm
    use traits_runtime_generic_01_late_client_m, only: LateValue, PaddedValue, AlternateValue, argument_finals
    use traits_runtime_generic_01_consumer_m, only: invoke, invoke_padded, invoke_alternate
    implicit none
    type(LateValue) :: value
    type(PaddedValue) :: padded
    type(AlternateValue) :: alternate
    class(IAlgorithm), allocatable :: implementation
    integer :: i, choice

    value%payload = 37
    padded%prefix = [1000.0_8, -999.0_8, 42.0_8]
    padded%payload = 37
    alternate%payload = 37
    do i = 0, 1
        choice = mod(command_argument_count() + i, 2)
        call make_algorithm(choice, implementation)
        if (choice == 0) then
            if (invoke(implementation, value) /= 47) error stop 1201
            if (invoke_padded(implementation, padded) /= 47) error stop 1202
            if (invoke_alternate(implementation, alternate) /= 84) error stop 1203
        else
            if (invoke(implementation, value) /= 174) error stop 1204
            if (invoke_padded(implementation, padded) /= 174) error stop 1205
            if (invoke_alternate(implementation, alternate) /= 248) error stop 1206
        end if
        if (argument_finals /= 0) error stop 1207
        if (value%payload /= 37 .or. padded%payload /= 37 .or. alternate%payload /= 37) error stop 1208
        if (any(padded%prefix /= [1000.0_8, -999.0_8, 42.0_8])) error stop 1210
        deallocate(implementation)
        if (argument_finals /= 0) error stop 1209
    end do
end program
