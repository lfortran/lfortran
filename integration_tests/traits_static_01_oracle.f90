module traits_static_01_oracle_m
    implicit none

    type :: SmallBox
        integer :: value
    contains
        procedure :: get_value => small_value
    end type SmallBox

    type :: LargeBox
        integer :: value
    contains
        procedure :: get_value => large_value
    end type LargeBox

    interface read_value
        module procedure read_small_value
        module procedure read_large_value
    end interface read_value

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

    function read_small_value(x) result(res)
        type(SmallBox), intent(in) :: x
        integer :: res
        res = x%get_value()
    end function read_small_value

    function read_large_value(x) result(res)
        type(LargeBox), intent(in) :: x
        integer :: res
        res = x%get_value()
    end function read_large_value
end module traits_static_01_oracle_m

program traits_static_01_oracle
    use traits_static_01_oracle_m
    implicit none
    type(SmallBox) :: left
    type(LargeBox) :: right

    left = SmallBox(7)
    right = LargeBox(11)

    if (read_value(left) /= 7) error stop
    if (read_value(right) /= 22) error stop
end program traits_static_01_oracle
