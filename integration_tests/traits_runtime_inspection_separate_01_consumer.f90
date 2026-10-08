module traits_runtime_inspection_consumer_m
    use iso_c_binding, only: c_ptr, c_loc, c_associated
    use traits_runtime_inspection_contracts_m
    implicit none
contains
    subroutine check_borrow(view, choice, n)
        class(ILabel + IValue), intent(in) :: view
        integer, intent(in) :: choice, n
        select type (concrete => view)
        class is (Root)
            error stop 1
        class is (Branch)
            if (choice /= 2 .or. concrete%n /= n .or. concrete%extra /= 7) error stop 2
            select type (nested => concrete)
            type is (PublicCell)
                error stop 3
            class default
                if (nested%n /= n) error stop 4
            end select
        type is (PublicCell)
            if (choice /= 1 .or. concrete%n /= n .or. concrete%last /= 11) error stop 5
        class default
            error stop 6
        end select
        if (view%value() /= n .or. view%label() /= 101 * choice) error stop 7
    end subroutine
    subroutine exercise(choice)
        integer, intent(in) :: choice
        class(IRich), pointer :: selected, other_view
        class(IValue + ILabel), pointer :: combination
        class(ILabel + IValue), pointer :: reverse
        class(IValue), pointer :: parent
        class(IRich), allocatable, target :: owner, copy
        type(PublicCell), pointer :: expected
        type(c_ptr) :: address, actual_address
        integer :: before, sum_before
        before = finals
        sum_before = total
        call inspection_acquire(choice, 41, selected)
        address = expected_address
        call inspection_acquire(3 - choice, 59, other_view)
        combination => selected
        reverse => combination
        parent => selected
        call check_borrow(combination, choice, 41)
        select type (concrete => combination)
        class is (Root)
            error stop 8
        type is (PublicCell)
            if (choice /= 1) error stop 9
            expected => concrete
            actual_address = c_loc(concrete%n)
            if (.not. c_associated(address, actual_address)) error stop 10
            combination => other_view
            if (.not. associated(expected, concrete)) error stop 11
            concrete%n = 43
            nullify(expected)
        class is (Branch)
            if (choice /= 2 .or. concrete%n /= 41) error stop 12
            actual_address = c_loc(concrete%n)
            if (.not. c_associated(address, actual_address)) error stop 13
            combination => other_view
            concrete%n = 43
        class default
            error stop 14
        end select
        if (combination%value() /= 59 .or. parent%value() /= 43) error stop 15
        call check_borrow(reverse, choice, 43)
        select type (concrete => combination)
        type is (Other)
            error stop 16
        class default
            combination => selected
            if (concrete%value() /= 59 .or. concrete%label() /= 101 * (3 - choice)) error stop 17
        end select
        if (selected%value() /= 43 .or. selected%label() /= 101 * choice) error stop 18
        if (choice == 1) call inspection_alternative(selected, 43)
        nullify(selected, other_view, combination, reverse, parent)
        if (finals /= before) error stop 19

        owner = inspection_make(choice, 71)
        if (finals /= before + 1 .or. total /= sum_before + 71) error stop 20
        select type (concrete => owner)
        class is (Root)
            error stop 21
        class is (Branch)
            concrete%n = 72
        end select
        call check_borrow(owner, choice, 72)
        allocate(copy, source=owner)
        if (copy%value() /= 72 .or. copy%label() /= 101 * choice) error stop 22
        deallocate(owner, copy)
        if (finals /= before + 3 .or. total /= sum_before + 215) error stop 23
        select type (concrete => inspection_make(choice, 89))
        class is (Root)
            error stop 24
        class is (Branch)
            if (concrete%n /= 89 .or. finals /= before + 3) error stop 25
        end select
        if (finals /= before + 4 .or. total /= sum_before + 304) error stop 26
    end subroutine
end module
