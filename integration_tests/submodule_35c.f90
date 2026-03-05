submodule(submodule_35_parent_mod) submodule_35_impl
    use submodule_35_util_mod, only: point_t
    implicit none
contains
    module subroutine scale(p, factor)
        type(point_t), intent(inout) :: p
        real, intent(in) :: factor
        p%x = p%x * factor
        p%y = p%y * factor
    end subroutine scale
end submodule submodule_35_impl
