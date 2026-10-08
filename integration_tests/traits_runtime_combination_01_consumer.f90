module traits_runtime_combination_01_consumer_m
    use iso_c_binding, only: c_ptr, c_associated
    use traits_runtime_combination_01_contracts_m, only: A => IValue, B => ILabel, &
        IScale, ILeft, IRight, IAlias, IRich, observed_address, finals, final_sum
    use traits_runtime_combination_01_reordered_m
    implicit none
    private
    public :: exercise_combinations, check_address
    class(A + B), pointer, save :: saved => null()
contains
    subroutine check_address(object, n, tag, address)
        class(A + B), target, intent(in) :: object
        integer, intent(in) :: n, tag
        type(c_ptr), intent(in) :: address
        class(B + A), pointer :: local
        local => object
        if (local%value() /= n .or. object%label() /= tag) error stop 1320
        if (.not. c_associated(observed_address, address)) error stop 1321
        if (.not. associated(local, object)) error stop 1322
        nullify(local)
    end subroutine
    subroutine check_readonly(object, live, n, tag, address)
        class(B + A), pointer, intent(in) :: object
        logical, intent(in) :: live
        integer, intent(in) :: n, tag
        type(c_ptr), intent(in) :: address
        if (associated(object) .neqv. live) error stop 1323
        if (live) call check_address(object, n, tag, address)
    end subroutine
    subroutine remember(object)
        class(A + B), pointer, intent(in) :: object
        saved => object
    end subroutine
    subroutine recall(object)
        class(B + A), pointer, intent(out) :: object
        object => saved
        nullify(saved)
    end subroutine
    subroutine exercise_combinations(object, richer, n, tag, address, survivor)
        class(IRich), pointer, intent(in) :: object
        class(A + IScale + B), pointer, intent(in) :: richer
        integer, intent(in) :: n, tag
        type(c_ptr), intent(in) :: address
        class(B + A), pointer, intent(out) :: survivor
        class(A + B), pointer :: late, alias
        class(B + IScale + A), pointer :: shuffled
        class(ILeft + IRight + IAlias + B), pointer :: diamond
        class(IRich + A + B), pointer :: redundant
        class(IRich), pointer :: empty => null()
        class(A + B), allocatable, target :: owner, copy, defaulted
        class(A), allocatable :: member
        type(c_ptr) :: owned_address
        integer :: before, before_sum

        before = finals
        before_sum = final_sum
        late => empty
        call check_readonly(late, .false., n, tag, address)
        call check_readonly(empty, .false., n, tag, address)
        call check_readonly(null(), .false., n, tag, address)
        redundant => object
        shuffled => richer
        if (shuffled%scaled(factor=3) /= 3*n) error stop 1324
        if (.not. c_associated(observed_address, address)) error stop 1325
        late => shuffled
        call remember(late)
        alias => late
        nullify(late, shuffled)
        call recall(survivor)
        call check_address(survivor, n, tag, address)
        call check_address(richer, n, tag, address)
        call check_readonly(object, .true., n, tag, address)
        diamond => redundant
        call check_address(diamond, n, tag, address)
        if (.not. associated(alias, survivor)) error stop 1326
        call pointer_out(late, object)
        call pointer_inout(late, empty)
        if (associated(late)) error stop 1327
        call pointer_unspecified(late, object)
        nullify(late, diamond, redundant)
        if (finals /= before .or. final_sum /= before_sum) error stop 1328

        call slot_in(owner, .false., 0)
        allocate(owner, source=survivor)
        call slot_in(owner, .true., n)
        owned_address = observed_address
        if (c_associated(owned_address, address)) error stop 1329
        copy = owner
        member = copy
        allocate(defaulted, mold=owner)
        if (defaulted%value() /= 0 .or. defaulted%label() /= tag) error stop 1330
        if (member%value() /= n .or. copy%label() /= tag) error stop 1331
        alias => owner
        call check_address(alias, n, tag, owned_address)
        nullify(alias)
        call slot_inout(owner, object)
        call slot_out(owner, object)
        call slot_unspecified(owner, object)
        if (finals /= before + 3 .or. final_sum /= before_sum + 3*n) error stop 1332
        owner = owner
        if (finals /= before + 4 .or. final_sum /= before_sum + 4*n) error stop 1333
        deallocate(owner, copy, member, defaulted)
        if (finals /= before + 8 .or. final_sum /= before_sum + 7*n) error stop 1334
        call check_address(survivor, n, tag, address)
    end subroutine
end module
