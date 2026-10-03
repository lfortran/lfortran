program free_all_memory_01
    implicit none

    character(len=:), allocatable :: s

    s = "hello"
    if (.not. allocated(s)) error stop
    if (len(s) /= 5) error stop
end program free_all_memory_01
