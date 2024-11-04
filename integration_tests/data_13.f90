program data_13
    implicit none
    integer, parameter :: k = 3
    integer :: i, iarx(k), iary(2*k)
    data(iarx(i), i=1, k) / 1, 2, 3 /
    print *, iarx(1)
    if (iarx(1) /= 1) error stop
    print *, iarx(2)
    if (iarx(2) /= 2) error stop
    print *, iarx(3)
    if (iarx(3) /= 3) error stop

    data(iary(i), i=1, 2*k, 2) / 1, 3, 5 /
    data(iary(i), i=2, 2*k, 2) / 2, 4, 6 /
    print *, iary
    if (any(iary /= [1, 2, 3, 4, 5, 6])) error stop
end program data_13
