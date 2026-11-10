program vector_subscript_01
    implicit none
    integer :: x(6), idx(0:1)
    x = [1, 2, 3, 4, 5, 6]
    idx = [3, 4]
    print *, lbound(x(idx)), ubound(x(idx)), size(x(idx))
    if (lbound(x(idx), 1) /= 1) error stop
    if (ubound(x(idx), 1) /= 2) error stop
    if (size(x(idx)) /= 2) error stop
    if (any(x(idx) /= [3, 4])) error stop
end program
