module class_intent_out_reset_02_base_m
    implicit none
    type :: base
        integer :: root = -1
        integer, allocatable :: values(:)
    contains
        procedure :: name
        procedure :: scale
    end type
contains
    integer function name(self)
        class(base), intent(in) :: self
        name = 1
    end function
    integer function scale(self)
        class(base), intent(in) :: self
        scale = 10
    end function
    subroutine reset(x)
        class(base), intent(out) :: x
    end subroutine
end module
