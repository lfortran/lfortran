module traits_runtime_separate_01_consumer_m
    use traits_runtime_separate_01_contracts_m, only: IValue
    implicit none
contains
    function observe(object) result(r)
        class(IValue), intent(in) :: object
        integer :: r
        r = object%value()
    end function observe

    subroutine check(object, value_expected, affine_expected, tag_expected)
        class(IValue), intent(in) :: object
        integer, intent(in) :: value_expected, affine_expected, tag_expected
        integer :: result
        if (observe(object) /= value_expected) error stop 701
        if (object%affine(right=3, left=2) /= affine_expected) error stop 702
        if (object%tag(5) /= tag_expected) error stop 703
        call object%message(right=3, result=result, left=2)
        if (result /= affine_expected) error stop 704
    end subroutine check

    subroutine choose(choice, first, second)
        integer, intent(in) :: choice
        class(IValue), intent(in) :: first, second
        if (choice == 0) then
            call check(first, 11, 34, 405)
        else
            call check(second, 29, 232, 905)
        end if
    end subroutine choose
end module traits_runtime_separate_01_consumer_m
