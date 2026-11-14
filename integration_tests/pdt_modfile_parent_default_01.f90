program pdt_modfile_parent_default_01
    use pdt_modfile_child_m, only: child_t, reset
    use pdt_modfile_base_m, only: base_t, base_t_4
    implicit none
    type(base_t) :: parent_default
    type(base_t(4)) :: parent_explicit
    type(base_t_4) :: unrelated
    type(child_t) :: child_default
    type(child_t(4)) :: child_explicit

    if (parent_default%k /= 4 .or. parent_explicit%k /= 4) error stop 1
    if (child_default%k /= 4 .or. child_explicit%k /= 4) error stop 2
    if (child_default%parent_default /= 17) error stop 3
    if (.not. same_type_as(parent_default, parent_explicit)) error stop 4
    if (.not. same_type_as(child_default, child_explicit)) error stop 5
    if (.not. extends_type_of(child_default, parent_default)) error stop 6
    if (extends_type_of(parent_default, child_default)) error stop 7
    if (unrelated%user_value /= 99 .or. &
        same_type_as(parent_default, unrelated) .or. &
        extends_type_of(child_default, unrelated)) error stop 12

    call reset(child_default)
    call reset(child_explicit)
    if (child_default%parent_default /= 23 .or. &
        child_explicit%parent_default /= 23) error stop 8
    if (child_default%child_default /= 47 .or. &
        child_explicit%child_default /= 47) error stop 9

    call check_child(child_default)
contains
    subroutine check_child(x)
        class(base_t), intent(in) :: x
        select type (x)
        type is (child_t(4))
            if (x%child_default /= 47) error stop 10
        class default
            error stop 11
        end select
    end subroutine
end program
