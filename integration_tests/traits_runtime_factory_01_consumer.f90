module traits_runtime_factory_01_consumer_m
    use traits_runtime_factory_01_contracts_m, only: View => IValue, make_value, finals, sums, requests
    implicit none
    private
    public :: exercise, replace_from_factory
contains
    subroutine observe(object, expected)
        class(View), intent(in) :: object
        integer, intent(in) :: expected
        if (object%value() /= expected) error stop 101
    end subroutine
    subroutine check(choice, expected, before_f, before_s, delta)
        integer, intent(in) :: choice, expected, before_f, before_s, delta
        if (finals(choice) /= before_f + delta) error stop 102
        if (sums(choice) /= before_s + delta * expected) error stop 103
    end subroutine
    function relay(choice) result(object)
        integer, intent(in) :: choice
        class(View), allocatable :: object
        object = make_value(choice)
    end function
    subroutine exercise(choice, expected)
        integer, intent(in) :: choice, expected
        class(View), allocatable :: owner, copy
        integer :: before_f, before_s, other_f, before_requests
        before_f = finals(choice)
        before_s = sums(choice)
        other_f = finals(1 - choice)
        before_requests = requests

        ! Each factory call also finalizes its private construction seed once.
        call observe(make_value(choice), expected)
        call check(choice, expected, before_f, before_s, 2)
        owner = make_value(choice)
        call observe(owner, expected)
        call check(choice, expected, before_f, before_s, 4)
        copy = owner
        deallocate(owner)
        call observe(copy, expected)
        call check(choice, expected, before_f, before_s, 5)
        deallocate(copy)
        call check(choice, expected, before_f, before_s, 6)

        allocate(owner, source=make_value(choice))
        call observe(owner, expected)
        call check(choice, expected, before_f, before_s, 8)
        deallocate(owner)
        call check(choice, expected, before_f, before_s, 9)
        owner = relay(choice)
        call observe(owner, expected)
        call check(choice, expected, before_f, before_s, 12)
        deallocate(owner)
        call check(choice, expected, before_f, before_s, 13)
        if (finals(1 - choice) /= other_f) error stop 104
        if (requests /= before_requests + 4) error stop 105
    end subroutine
    subroutine replace_from_factory(choice)
        integer, intent(in) :: choice
        class(View), allocatable :: owner
        integer :: before_f(0:1), before_s(0:1), expected(0:1), before_requests
        before_f = finals
        before_s = sums
        expected = [17, 29]
        before_requests = requests
        owner = make_value(choice)
        call observe(owner, expected(choice))
        call check(choice, expected(choice), before_f(choice), before_s(choice), 2)
        owner = make_value(1 - choice)
        call observe(owner, expected(1 - choice))
        call check(choice, expected(choice), before_f(choice), before_s(choice), 3)
        call check(1 - choice, expected(1 - choice), before_f(1 - choice), before_s(1 - choice), 2)
        deallocate(owner)
        call check(1 - choice, expected(1 - choice), before_f(1 - choice), before_s(1 - choice), 3)
        if (requests /= before_requests + 2) error stop 106
    end subroutine
end module
