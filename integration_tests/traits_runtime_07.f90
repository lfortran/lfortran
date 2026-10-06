program traits_runtime_07
    use traits_runtime_07_contracts_m, only: IValue
    use traits_runtime_07_impl_a_m, only: make_a
    use traits_runtime_07_impl_b_m, only: make_b
    use traits_runtime_07_consumer_m, only: observe
    implicit none
    class(IValue), allocatable :: object
    integer :: i, choice
    do i = 0, 1
        choice = mod(command_argument_count() + i, 2)
        if (choice == 0) then
            call make_a(object)
            if (observe(object) /= 17) error stop 701
        else
            call make_b(object)
            if (observe(object) /= 29) error stop 702
        end if
        deallocate(object)
    end do
end program traits_runtime_07
