program string_22
    implicit none
    character(len=4) :: str
    str = 'abcd'
    if (len(str(2:)) /= 3) error stop
end program string_22

