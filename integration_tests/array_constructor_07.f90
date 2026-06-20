program array_constructor_07
    implicit none

    integer, parameter :: x(1,1) = 1
    logical, parameter :: m(1,1) = x > 1

    print *, m
    if (any(m .neqv. reshape([.false.], [1,1]))) error stop
end program