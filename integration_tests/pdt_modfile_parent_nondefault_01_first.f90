module pdt_nondefault_first_m
    use pdt_nondefault_base_m, only: first_base => base_t
    implicit none
    private :: first_base
    type, extends(first_base) :: first_child
        integer :: child_default = 42
    end type
contains
    subroutine reset_first(x)
        class(first_child(8)), intent(out) :: x
    end subroutine
end module
