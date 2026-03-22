program arrays_120
    implicit none
    integer(8) :: ilist(1, 1)
    ilist(1, 1:1) = 1
    if (ilist(1, 1) /= 1) error stop
    print *, ilist
end program arrays_120
