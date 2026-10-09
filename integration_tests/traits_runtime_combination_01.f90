program traits_runtime_combination_01
    use iso_c_binding, only: c_ptr
    use traits_runtime_combination_01_contracts_m, only: IValue, ILabel, IScale, IRich, &
        combination_acquire, combination_acquire_anonymous, combination_update, &
        combination_alternative, combination_release, expected_address, finals, final_sum
    use traits_runtime_combination_01_consumer_m, only: exercise_combinations, check_address
    implicit none
    class(IRich), pointer :: selected => null(), other => null()
    class(ILabel + IValue + IScale), pointer :: richer => null()
    class(IValue + ILabel), pointer :: survivor => null(), alias => null()
    type(c_ptr) :: address
    integer :: i, choice, n, tag, before, before_sum, expected_final

    do i = 0, 1
        choice = mod(command_argument_count() + i, 2) + 1
        n = 7 + 12*i
        tag = 101*choice
        call combination_acquire(choice, n, selected)
        address = expected_address
        call combination_acquire_anonymous(choice, n, richer)
        call exercise_combinations(selected, richer, n, tag, address, survivor)
        call combination_alternative(selected, n, tag)
        alias => survivor
        call combination_acquire(3-choice, 89, other)
        survivor => other
        if (survivor%value() /= 89 .or. survivor%label() /= 101*(3-choice)) error stop 1350
        nullify(survivor, selected, richer, other)
        call combination_update(choice, n + 2)
        call check_address(alias, n + 2, tag, address)
        nullify(alias)
    end do
    before = finals
    before_sum = final_sum
    expected_final = 89
    if (choice == 2) expected_final = n + 2
    call combination_release()
    if (finals /= before + 1 .or. final_sum /= before_sum + expected_final) error stop 1351
    print *, "late combinations: selected procedures, addresses, slots and finalization"
end program
