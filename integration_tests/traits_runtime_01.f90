! Runtime traits are an LFortran extension.
module traits_runtime_01_m
    implicit none

    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function value
    end interface IValue

    type :: DirectValue
        integer :: payload
    end type DirectValue

    type :: ScaledValue
        integer :: factor, payload
    end type ScaledValue

    implements IValue :: DirectValue
        procedure, pass :: value => direct_value
    end implements DirectValue

    implements IValue :: ScaledValue
        procedure, pass :: value => scaled_value
    end implements ScaledValue
contains
    function direct_value(self) result(r)
        class(DirectValue), intent(in) :: self
        integer :: r
        r = self%payload
    end function direct_value

    function scaled_value(self) result(r)
        class(ScaledValue), intent(in) :: self
        integer :: r
        r = self%factor * self%payload + 1
    end function scaled_value

    function observe(object) result(r)
        class(IValue), intent(in) :: object
        integer :: r
        r = object%value()
    end function observe

    function choose_value(choice, first, second) result(r)
        integer, intent(in) :: choice
        class(IValue), intent(in) :: first, second
        integer :: r
        if (choice == 0) then
            r = observe(first)
        else
            r = observe(second)
        end if
    end function choose_value
end module traits_runtime_01_m

program traits_runtime_01
    use traits_runtime_01_m
    implicit none
    type(DirectValue) :: a
    type(ScaledValue) :: b
    integer :: i, choice, actual

    a%payload = 11
    b%factor = 4
    b%payload = 7
    do i = 0, 1
        choice = mod(command_argument_count() + i, 2)
        actual = choose_value(choice, a, b)
        if (choice == 0) then
            if (actual /= 11) error stop 101
        else
            if (actual /= 29) error stop 102
        end if
    end do
end program traits_runtime_01
