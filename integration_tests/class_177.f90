module class_177_m
    implicit none
    type :: inner
        integer, allocatable :: values(:)
    end type
    type :: base
        type(inner) :: inherited
    end type
    type, extends(base) :: child
        type(inner) :: part
    end type
contains
    subroutine reset(x)
        class(base), intent(out) :: x
    end subroutine
end module

program class_177
    use class_177_m
    implicit none
    type(child) :: x
    allocate(x%inherited%values(1), x%part%values(2))
    call reset(x)
    if (allocated(x%inherited%values)) error stop 1
    if (allocated(x%part%values)) error stop 2
    allocate(x%part%values(3))
    x%part%values = 7
    if (sum(x%part%values) /= 21) error stop 3
end program
