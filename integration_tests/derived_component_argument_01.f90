module derived_component_argument_m
    implicit none
    type :: point
        real :: x(1)
    end type
contains
    subroutine check(values, n)
        integer, intent(in) :: n
        real, intent(in) :: values(n)
        if (sum(values) /= 3.0) error stop
    end subroutine
    subroutine increment(values)
        real, intent(inout) :: values(:)
        values = values + 1.0
    end subroutine
end module

program derived_component_argument_01
    use derived_component_argument_m
    implicit none
    type(point), allocatable :: points(:)

    allocate(points(2))
    points(1)%x = 1.0
    points(2)%x = 2.0
    call check(points(:)%x(1), 2)
    call increment(points(:)%x(1))
    if (any(points(:)%x(1) /= [2.0, 3.0])) error stop
end program
