program array_shape_11
    ! The section target of `x(:) = y + 1` is not allocated: it is not
    ! allocated by the assignment
    implicit none
    real, allocatable :: x(:), y(:)
    allocate(x(3), y(3))
    y = 1
    x(:) = y + 1
    deallocate(x)
    x(:) = y + 1
    print *, x
end program array_shape_11
