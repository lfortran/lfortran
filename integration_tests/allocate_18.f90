program allocate_18
    implicit none

    integer, allocatable :: i
    real, allocatable :: r
    complex, allocatable :: c
    logical, allocatable :: l

    allocate(i)
    allocate(r)
    allocate(c)
    allocate(l)

    i = 10
    r = 4.4
    c = (1, 2)
    l = .true.

    if (i /= 10) error stop
    if (r /= 4.4) error stop
    if (c /= (1, 2)) error stop
    if (l .neqv. .true.) error stop

    deallocate(i)
    deallocate(r)
    deallocate(c)
    deallocate(l)
end program allocate_18
