program derived_types_148
    implicit none

    type :: point
        integer :: value
    end type

    type(point), parameter :: defaults(*) = [point(10), point(20)]
    type(point) :: data(2) = defaults

    if (data(1)%value /= 10) error stop
    if (data(2)%value /= 20) error stop
    print *, data(1)%value, data(2)%value
end program derived_types_148
