program array_bounds_check_01
    integer, allocatable :: x(:)
    allocate(x(10))

    x = [1, 2, 3]
end program
