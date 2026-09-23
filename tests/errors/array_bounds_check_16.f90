program array_bounds_check_16
    implicit none

    type :: container
        real, allocatable :: v(:)
    end type container

    type(container) :: t

    allocate(t%v(2))
    t%v = [9.0, 9.0]
    t%v = [1.0, 2.0, 3.0] ! t%v can be reallocated, so suggest the option

end program
