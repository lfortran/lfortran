program pointer_04
    implicit none

    type :: base
        integer :: v
    end type

    class(base), allocatable :: x
    class(base), pointer :: y
    type(base), target :: t

    t%v = 42
    y => t

    allocate(x, source=y)
    if (.not. allocated(x)) error stop
    if (x%v /= 42) error stop

    t%v = 100
    if (y%v /= 100) error stop
    if (x%v /= 42) error stop
end program pointer_04
