program arrays_119
    implicit none
    integer(8) :: ilist(1, 1)
    integer, parameter :: x = 1
    ilist(1, 1:1) = [x]
    if (ilist(1, 1) /= 1) error stop
    print *, ilist
end program arrays_119
