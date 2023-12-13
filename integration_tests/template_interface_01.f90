module template_interface_01_m
    implicit none
    public :: test_template

    requirement operator_r(T, U, V, binary_func)
        type, deferred :: T
        type, deferred :: U
        type, deferred :: V
        pure elemental function binary_func(lhs, rhs) result(res)
            type(T), intent(in) :: lhs
            type(U), intent(in) :: rhs
            type(V) :: res
        end function
    end requirement

    requirement cast_r(T, cast)
        type, deferred :: T
        pure elemental function cast(arg) result(res)
            integer, intent(in) :: arg
            type(T) :: res
        end function
    end requirement

contains

    pure elemental function cast_integer(arg) result(res)
        integer, intent(in) :: arg
        integer :: res
        res = 0
    end function

    pure elemental function cast_real(arg) result(res)
        integer, intent(in) :: arg
        real :: res
        res = 0.0
    end function

    subroutine test_template()

    end subroutine

end module

program template_interface_01
use template_interface_01_m
implicit none

end program