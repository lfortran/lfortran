program automatic_allocation_03
    implicit none
    integer, allocatable :: i

    i = 4
    i = abs(i - 4);

    if (i /= 0) error stop
end program automatic_allocation_03
