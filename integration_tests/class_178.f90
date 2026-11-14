module class_178_m
    implicit none
    type :: inner
        integer :: mark = 42
    end type
    type :: base
        integer :: root = -1
    end type
    type, extends(base) :: child
        type(inner) :: part(2)
    end type
contains
    subroutine reset(x)
        class(base), intent(out) :: x
    end subroutine
end module

program class_178
    use class_178_m
    implicit none
    type(child) :: x
    x%root = 9
    x%part(1)%mark = 9
    x%part(2)%mark = 17
    call reset(x)
    if (x%root /= -1) error stop 1
    if (any(x%part%mark /= 42)) error stop 2
end program
