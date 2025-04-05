program string_51
    implicit none
    character(len=4) :: array(2)
    print*, len(array)
    if (len(array) /= 4) error stop
    print*, len(array(1))
    if (len(array(1)) /= 4) error stop
end program