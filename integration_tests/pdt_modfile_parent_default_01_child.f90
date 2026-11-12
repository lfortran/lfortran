module pdt_modfile_child_m
    use pdt_modfile_base_m, only: renamed_base => base_t
    implicit none
    private :: renamed_base
    type, extends(renamed_base) :: child_t
        integer :: child_default = 42
    end type
contains
    subroutine reset(x)
        class(child_t), intent(out) :: x
        x%parent_default = 23
        x%child_default = 47
    end subroutine
end module
