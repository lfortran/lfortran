program pthread_flag_01
    implicit none
    integer :: i
    i = 2 + 3
    if (i /= 5) error stop
    print *, i
end program
