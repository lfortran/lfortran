program array_bounds_check_07
    integer, allocatable :: x(:)
    integer, allocatable :: y(:)
    integer, allocatable :: z(:)
    allocate(x(3))
    allocate(y(4))
    allocate(z(5))

    x = 1
    y = 2

    z = x + y
end program
