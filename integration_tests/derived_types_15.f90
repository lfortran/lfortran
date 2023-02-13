module module_1
    implicit none
    private
    public :: t_1
    type t_1
        real :: i
    contains
        generic :: compute => multiply, sqrt_subtract
        procedure :: multiply
        procedure :: sqrt_subtract
    end type t_1
contains
    function multiply(self, j) result(res)
        class(t_1), intent(in) :: self
        real, intent(in) :: j
        real :: res
        res = self%i * j
    end function multiply

    function sqrt_subtract(self, j) result(res)
        class(t_1), intent(in) :: self
        integer, intent(in) :: j
        real :: res
        res = self%i - (j ** 0.5)
    end function sqrt_subtract
end module module_1

program name
    use module_1
    implicit none
    type(t_1) :: type_1
    type_1%i = 100.0

    if (type_1%compute(5.0) /= 500) error stop
    if (type_1%compute(2500) /= 50) error stop
end program name
