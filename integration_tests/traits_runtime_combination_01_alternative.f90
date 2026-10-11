subroutine combination_alternative(object, n, tag)
    use traits_runtime_combination_01_contracts_m, only: IValue, ILabel, IScale, &
        IRich, PublicBox, finals, final_sum
    use traits_runtime_combination_01_alternative_m
    implicit none
    class(IRich), pointer, intent(in) :: object
    integer, intent(in) :: n, tag
    type(PublicBox), target, save :: fresh
    class(IValue + ILabel), pointer :: selected, local
    class(ILabel + IScale + IValue), pointer :: richer
    class(ILabel + IValue), allocatable, target :: owner
    integer :: before, before_sum

    fresh%payload = 3
    local => fresh
    richer => fresh
    if (local%value() /= 1003 .or. local%label() /= 909) error stop 1340
    if (richer%scaled(2) /= 2006) error stop 1341
    selected => object
    if (selected%value() /= n .or. selected%label() /= tag) error stop 1342
    before = finals
    before_sum = final_sum
    owner = fresh
    if (owner%value() /= 1003 .or. owner%label() /= 909) error stop 1347
    owner = selected
    if (finals /= before + 1 .or. final_sum /= before_sum + 3) error stop 1348
    if (owner%value() /= n .or. owner%label() /= tag) error stop 1343
    if (associated(selected, owner)) error stop 1344
    deallocate(owner)
    if (finals /= before + 2 .or. final_sum /= before_sum + 3 + n) error stop 1345
    local => selected
    nullify(selected)
    if (local%value() /= n .or. local%label() /= tag) error stop 1346
    nullify(local, richer)
end subroutine
