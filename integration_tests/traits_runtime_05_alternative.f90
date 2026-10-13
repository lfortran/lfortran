subroutine r3_check_alternative_scope(object, n)
    use traits_runtime_05_contracts_m, only: IValue, IChild, ILabel, ICombined, Box, r3_check_parent
    use traits_runtime_05_alternative_m
    implicit none
    class(ICombined), pointer, intent(in) :: object
    integer, intent(in) :: n
    type(Box), target :: fresh
    class(ICombined), pointer :: locally_selected
    class(IChild), pointer :: child
    class(IValue), pointer :: parent
    class(ILabel), pointer :: label
    class(ILabel), allocatable, target :: owned_label

    ! Only fresh erasure may select the alternative conformance.
    fresh%payload = 3
    locally_selected => fresh
    if (locally_selected%value() /= 1003) error stop 511
    if (locally_selected%double_value() /= 2006) error stop 512
    if (locally_selected%label() /= 909) error stop 513
    nullify(locally_selected)
    child => object
    parent => child
    label => object
    if (object%value() /= n .or. parent%value() /= n) error stop 514
    if (child%double_value() /= 2 * n .or. label%label() /= 101) error stop 515
    call r3_check_parent(object, n)
    call r3_check_parent(parent, n)
    owned_label = object
    if (owned_label%label() /= 101) error stop 520
    if (associated(label, owned_label)) error stop 521
    deallocate(owned_label)
    nullify(child, parent, label)
end subroutine
