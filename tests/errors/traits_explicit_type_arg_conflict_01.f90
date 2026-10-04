module traits_explicit_type_arg_conflict_01_m
    implicit none

    abstract interface :: IValue
        function get_value() result(res)
            integer :: res
        end function get_value
    end interface IValue

    type :: SmallBox
        integer :: value
    end type SmallBox

    type :: LargeBox
        integer :: value
    end type LargeBox

    implements IValue :: SmallBox
        procedure, pass :: get_value => small_value
    end implements SmallBox

    implements IValue :: LargeBox
        procedure, pass :: get_value => large_value
    end implements LargeBox

contains

    function small_value(self) result(res)
        class(SmallBox), intent(in) :: self
        integer :: res
        res = self%value
    end function small_value

    function large_value(self) result(res)
        class(LargeBox), intent(in) :: self
        integer :: res
        res = 2 * self%value
    end function large_value

    function read_value{IValue :: T}(x) result(res)
        type(T), intent(in) :: x
        integer :: res
        res = x%get_value()
    end function read_value

    subroutine drive()
        type(SmallBox) :: left
        type(LargeBox) :: right
        left = SmallBox(7)
        right = LargeBox(11)
        if (read_value{SmallBox}(right) /= 22) error stop
        if (read_value{LargeBox}(left) /= 7) error stop
    end subroutine drive
end module traits_explicit_type_arg_conflict_01_m
