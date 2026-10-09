program traits_runtime_inspection_separate_01
    use traits_runtime_inspection_contracts_m
    use traits_runtime_inspection_consumer_m, only: exercise
    implicit none
    integer :: i, choice
    do i = 0, 1
        choice = mod(command_argument_count() + i, 2) + 1
        call exercise(choice)
    end do
    if (finals /= 8 .or. total /= 608) error stop 41
    print *, "concrete inspection: frozen providers, selected methods, identity and finalization"
end program
