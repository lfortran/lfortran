module submodule_35_util_mod
    implicit none
    type :: point_t
        real :: x, y
    end type point_t
end module submodule_35_util_mod

module submodule_35_parent_mod
    use submodule_35_util_mod, only: point_t
    implicit none
    interface
        module subroutine scale(p, factor)
            type(point_t), intent(inout) :: p
            real, intent(in) :: factor
        end subroutine scale
    end interface
end module submodule_35_parent_mod
