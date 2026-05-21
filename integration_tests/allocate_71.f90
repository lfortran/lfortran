program allocate_71
    logical, allocatable :: tf
    integer :: counter
    counter = 0
    allocate(tf)
    tf = .true.
    if (tf) counter = counter + 1
    tf = .false.
    if (tf) counter = counter + 10
    if (.not. tf) counter = counter + 100
    if (counter /= 101) error stop
    deallocate(tf)
end program
