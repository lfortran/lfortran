program derived_component_section_03
    implicit none
    type :: point
        real :: x(1)
    end type
    type(point), allocatable :: points(:,:)
    integer :: location(1)

    allocate(points(3, 1))
    points(1, 1)%x = 1.0
    points(2, 1)%x = 3.0
    points(3, 1)%x = 2.0
    location = maxloc(points(:, 1)%x(1), points(:, 1)%x(1) < 3.0)
    if (location(1) /= 3) error stop
end program
