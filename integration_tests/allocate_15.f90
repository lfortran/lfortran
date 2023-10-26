program allocate_15
    implicit none
    integer, allocatable :: x(:), y(:), z(:)
    allocate(x(3), y(3))
    allocate(z(size(x+y) - 1))
    if (size(z) /= 2) error stop
    deallocate(z)
    allocate(z(1 + size(x+y)))
    if (size(z) /= 4) error stop
    deallocate(z)
end program
