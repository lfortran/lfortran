program array_bound_4
    integer :: x(-10:-5)

    if (lbound(x, 1) /= -10) error stop
    if (ubound(x, 1) /= -5) error stop
    if (size(x) /= 6) error stop
end program
