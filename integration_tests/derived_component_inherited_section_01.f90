program derived_component_inherited_section_01
    implicit none

    type :: parent_type
        real :: values(2)
        integer :: score
    end type
    type, extends(parent_type) :: child_type
        integer :: marker
    end type

    type(child_type), allocatable, target :: children(:)
    real, pointer :: selected(:)

    allocate(children(2))
    children(1)%values = [1.0, 2.0]
    children(2)%values = [3.0, 4.0]
    children%score = [5, 6]
    children%marker = [10, 20]

    associate(scores => children%score)
        if (size(scores) /= 2) error stop
        if (any(scores /= [5, 6])) error stop
        scores(1) = 9
    end associate
    if (children(1)%score /= 9) error stop

    selected => children(:)%values(1)
    if (size(selected) /= 2) error stop
    if (any(selected /= [1.0, 3.0])) error stop
    selected(2) = 7.0
    if (children(2)%values(1) /= 7.0) error stop
    if (any(children%marker /= [10, 20])) error stop
end program
