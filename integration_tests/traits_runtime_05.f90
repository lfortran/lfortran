program traits_runtime_05
    use traits_runtime_05_contracts_m, only: ICombined, r3_acquire, r3_update, &
        r3_check_combined, r3_check_alternative_scope
    implicit none
    class(ICombined), pointer :: selected => null(), alias => null()
    integer :: i, n
    call r3_acquire(7, selected)
    alias => selected
    do i = 0, 1
        if (mod(command_argument_count() + i, 2) == 0) then
            n = 7
        else
            n = 19
        end if
        call r3_update(n)
        call r3_check_combined(selected, n)
        call r3_check_alternative_scope(selected, n)
        if (alias%value() /= n .or. alias%label() /= 101) error stop 516
        if (.not. associated(selected, alias)) error stop 517
    end do
    nullify(selected)
    if (.not. associated(alias)) error stop 518
    if (alias%value() /= n) error stop 519
    nullify(alias)
end program
