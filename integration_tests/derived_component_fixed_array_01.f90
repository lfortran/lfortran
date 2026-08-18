program derived_component_fixed_array_01
    implicit none
    type :: point
        real :: x(2)
    end type
    type(point), allocatable :: points(:)

    allocate(points(2))
    points(1)%x = [1.0, 3.0]
    points(2)%x = [2.0, 4.0]
    points(:)%x(1) = 2.0 * points(:)%x(1)
    if (any(points(:)%x(1) /= [2.0, 4.0])) error stop
end program
