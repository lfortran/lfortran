program data_implied_do_13
    ! DATA implied-do with a named-constant bound and a named-constant repeat
    ! count of a negative value (#4680)
    implicit none
    integer, parameter :: k = 3
    integer :: i, iarx(k)
    data(iarx(i), i=1,k) / k*-1/
    print *, iarx
    if (any(iarx /= -1)) error stop
    if (size(iarx) /= 3) error stop
end program data_implied_do_13
