program pdt_modfile_parent_default_02
    use pdt_modfile_child_m, only: child_t, reset
    use pdt_modfile_base_m, only: base_t
    implicit none
    type(child_t) :: child_default
    type(child_t(4)) :: child_explicit

    if (child_default%parent_default /= 17) error stop 1
    call reset(child_default)
    call reset(child_explicit)
    call check_child(child_default)
    call check_child(child_explicit)
contains
    subroutine check_child(x)
        class(base_t), intent(in) :: x
        select type (x)
        type is (child_t(4))
            if (x%child_default /= 47) error stop 2
        class default
            error stop 3
        end select
    end subroutine
end program
