program allocate_29
    implicit none
    integer, allocatable:: arr(:)
    arr = [5, 6]
    if (arr(1) /= 5) error stop
    if (arr(2) /= 6) error stop
    print *, arr
end program allocate_29