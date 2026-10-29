program allocatable_07
    implicit none
    integer, allocatable :: n
    character(:), allocatable :: s
    allocate(n)
    n = 7
    if (n /= 7) error stop 1
    deallocate(n)
    allocate(character(5) :: s)
    if (len(s) /= 5) error stop 2
    s = "hi"
    if (s /= "hi") error stop 3
    if (len(s) /= 2) error stop 4
    deallocate(s)
end program
