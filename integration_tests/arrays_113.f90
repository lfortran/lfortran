program arrays_113
    implicit none
    ! Test implied-shape parameter arrays with explicit lower bounds
    integer, parameter :: n(0:*) = [2, 4, 4, 1]
    integer, parameter :: m(-2:*) = [10, 20, 30]
    integer, parameter :: p(*) = [5, 6, 7]
    integer :: i

    ! Check bounds and values for 0-based array
    if (lbound(n, 1) /= 0) error stop
    if (ubound(n, 1) /= 3) error stop
    if (size(n) /= 4) error stop
    if (n(0) /= 2) error stop
    if (n(1) /= 4) error stop
    if (n(2) /= 4) error stop
    if (n(3) /= 1) error stop

    ! Check bounds and values for negative-lower-bound array
    if (lbound(m, 1) /= -2) error stop
    if (ubound(m, 1) /= 0) error stop
    if (size(m) /= 3) error stop
    if (m(-2) /= 10) error stop
    if (m(-1) /= 20) error stop
    if (m(0) /= 30) error stop

    ! Check default lower bound (1) still works
    if (lbound(p, 1) /= 1) error stop
    if (ubound(p, 1) /= 3) error stop
    if (p(1) /= 5) error stop
    if (p(2) /= 6) error stop
    if (p(3) /= 7) error stop

    ! Access via variable index
    i = 1
    if (n(i - 1) /= 2) error stop
    if (m(i - 3) /= 10) error stop

    print *, "All tests passed"
end program
