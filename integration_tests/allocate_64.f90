program allocate_64
    integer, allocatable :: a(:)
    allocate(a(10))
    a = 1
    if (any(a /= 1)) error stop
    if (size(a) /= 10) error stop
    deallocate(a)
    if (allocated(a)) error stop
end program allocate_64