module allocate_on_assignment_01_m
    implicit none
    type :: point
        integer :: x = 0
    end type

    type :: item
        integer :: v = 0
    contains
        final :: fin
    end type
contains
    subroutine fin(x)
        type(item), intent(inout) :: x
        x%v = -1
    end subroutine
end module

program allocate_on_assignment_01
    use allocate_on_assignment_01_m
    implicit none
    type(point), allocatable :: a
    type(item), allocatable :: c
    a = point(1)
    c = item(2)
    print *, a%x, c%v
end program
