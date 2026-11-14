module class_180_m
    implicit none
    integer :: calls = 0
    type :: base
        integer :: root = 1
    contains
        final :: finish
    end type
    type :: inner
        integer :: mark = 42
    end type
    type, extends(base) :: child
        type(inner) :: part
    end type
contains
    subroutine finish(x)
        type(base), intent(inout) :: x
        if (x%root /= 9) error stop 1
        calls = calls + 1
    end subroutine
    subroutine reset(x)
        class(base), intent(out) :: x
    end subroutine
end module

program class_180
    use class_180_m
    implicit none
    type(child) :: x
    x%root = 9
    x%part%mark = 17
    call reset(x)
    if (calls /= 1) error stop 2
    if (x%root /= 1) error stop 3
    if (x%part%mark /= 42) error stop 4
end program
