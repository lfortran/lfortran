program derived_component_elemental_01
    implicit none
    type :: point
        real :: x(1)
    end type
    type(point), allocatable :: points(:,:)

    allocate(points(2, 1))
    points(1, 1)%x = 1.0
    points(2, 1)%x = 2.0
    if (any(isnan(points(:, 1)%x(1)))) error stop
end program
