program array_bound_3
    implicit none

    integer :: x(4, 3)
    integer :: lbound_x(2)
    integer :: ubound_x(2)

    lbound_x = lbound(x)
    ubound_x = ubound(x)

    print *, lbound_x
    print *, ubound_x

    if (sum(lbound_x) /= 2) error stop
    if (sum(ubound_x) /= 7) error stop

end program
