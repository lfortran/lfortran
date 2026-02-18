integer, allocatable :: x(:)
allocate(x(3))
x(1) = 1
x(2) = 2
x(3) = 3
x(1) + x(2) + x(3)
