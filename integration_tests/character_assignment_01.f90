program string_assignment_test
    implicit none
    character(len=1,kind=1) :: x
    character(len=1,kind=4) :: y
    x = 'A'
    y = x
    if (y /= 4_'A') error stop

    ! Test the other direction
    y = 4_'B'
    x = y
    if (x /= 'B') error stop
end program
