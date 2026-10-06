program traits_runtime_factory_01
    use traits_runtime_factory_01_contracts_m, only: finals, sums, requests
    use traits_runtime_factory_01_consumer_m, only: exercise, replace_from_factory
    implicit none
    integer :: i, choice, expected(0:1)
    expected = [17, 29]
    do i = 0, 1
        choice = mod(command_argument_count() + i, 2)
        call exercise(choice, expected(choice))
    end do
    if (any(finals /= 13)) error stop 201
    if (sums(0) /= 221 .or. sums(1) /= 377) error stop 202
    if (requests /= 8) error stop 203
    call replace_from_factory(mod(command_argument_count(), 2))
    if (any(finals /= 16)) error stop 204
    if (sums(0) /= 272 .or. sums(1) /= 464) error stop 205
    if (requests /= 10) error stop 206
    print *, "factory results:", finals, sums, requests
end program
