module class_179_base_m
    implicit none
    type :: base
        integer :: root = -1
    end type
contains
    subroutine reset(x)
        class(base), intent(out) :: x
    end subroutine
end module
