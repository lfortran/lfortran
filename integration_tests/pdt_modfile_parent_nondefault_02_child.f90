module pdt_nondefault_nested_child_m
    use pdt_nondefault_nested_base_m, only: renamed_root => root_t
    implicit none
    private :: renamed_root
    type, extends(renamed_root) :: child_t
    end type
contains
    subroutine reset(x)
        class(child_t(8)), intent(out) :: x
    end subroutine
end module
