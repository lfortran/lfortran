program transfer_20
    implicit none
    integer :: i

    i = transfer("", 1)
    if (i /= 0) error stop

    i = transfer("A", 1)
    if (i /= 65) error stop

end program transfer_20
