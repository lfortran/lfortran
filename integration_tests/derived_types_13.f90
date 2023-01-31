module debug_1
    implicit none
    ! private
    ! public :: compute
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

    subroutine sqrt_subtract(self, j)
        class(t_1), intent(inout) :: self
        integer, intent(in) :: j
        self%i = self%i - j ** 0.5
    end subroutine sqrt_subtract
end module debug_1

program name
    use debug_1
    implicit none

    type(t_1) :: type_1

    type_1%i = 100.0

    print *, type_1%compute(5.0)
    print *, type_1%i

    call type_1%compute(2500)
    print *, type_1%i
end program name

