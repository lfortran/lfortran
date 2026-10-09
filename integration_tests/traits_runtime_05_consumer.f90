subroutine r3_check_parent(object, n)
    use traits_runtime_05_contracts_m, only: IValue
    implicit none
    class(IValue), target, intent(in) :: object
    integer, intent(in) :: n
    class(IValue), pointer :: alias
    alias => object
    if (object%value() /= n .or. alias%value() /= n) error stop 503
    if (.not. associated(alias, object)) error stop 504
    nullify(alias)
end subroutine

subroutine r3_check_readonly(object, n)
    use traits_runtime_05_contracts_m, only: IValue
    implicit none
    class(IValue), pointer, intent(in) :: object
    integer, intent(in) :: n
    if (object%value() /= n) error stop 505
end subroutine

subroutine r3_check_combined(object, n)
    use traits_runtime_05_contracts_m, only: IValue, IChild, ILabel, ICombined, &
        r3_check_parent, r3_check_readonly
    implicit none
    class(ICombined), pointer, intent(in) :: object
    integer, intent(in) :: n
    class(IChild), pointer :: child
    class(IValue), pointer :: parent, direct_parent
    class(ILabel), pointer :: label
    child => object
    parent => child
    direct_parent => object
    label => object
    if (.not. associated(parent, direct_parent)) error stop 507
    if (.not. associated(parent, object)) error stop 506
    if (child%double_value() /= 2 * n) error stop 508
    if (label%label() /= 101) error stop 509
    call r3_check_parent(parent, n)
    call r3_check_parent(child, n)
    call r3_check_parent(object, n)
    call r3_check_readonly(object, n)
    nullify(child, parent, direct_parent, label)
    if (object%value() /= n) error stop 510
end subroutine
