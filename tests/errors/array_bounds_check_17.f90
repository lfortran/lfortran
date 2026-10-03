program array_bounds_check_17
    implicit none

    type :: container
        real :: v(2)
    end type container

    type(container) :: t
    real, allocatable :: y(:)

    allocate(y(3))
    y = 1.0
    t%v = y ! t%v cannot be reallocated, so do not suggest the option

end program
