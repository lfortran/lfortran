program array_bound_2
    implicit none

    integer(8) :: x(4)

    print *, lbound(x, dim=1), ubound(x, dim=1)

    if (lbound(x, dim=1) /= 1) error stop
    if (ubound(x, dim=1) /= 4) error stop

end program
