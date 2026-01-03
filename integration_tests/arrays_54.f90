program arrays_54
    implicit none
    integer :: res(4)
    real :: x(1, 4), center, order
    integer, parameter :: dim = 1
    order = 2
    x = 1
    center = 0
    res = 1 + sum((x - center), dim) / size(x, dim)
    if( any(res /= 2) ) error stop
    print *, res
end program
