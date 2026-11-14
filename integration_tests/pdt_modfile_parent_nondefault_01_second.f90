module pdt_nondefault_second_m
    use pdt_nondefault_base_m, only: second_base => base_t
    implicit none
    private :: second_base
    type, extends(second_base) :: second_child
        integer :: child_default = 73
    end type
contains
    subroutine reset_second(x)
        class(second_child(8)), intent(out) :: x
    end subroutine
end module
