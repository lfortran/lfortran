module operator_overloading_33_mod
    implicit none

    type :: array_type
        real :: val
    contains
        procedure :: negate_array
        procedure :: sub_arrays
        generic :: operator(-) => negate_array, sub_arrays
    end type

    interface operator(-)
        module procedure scalar_sub
    end interface

contains

    function negate_array(a) result(c)
        class(array_type), intent(in) :: a
        type(array_type) :: c
        c%val = -a%val
    end function

    function sub_arrays(a, b) result(c)
        class(array_type), intent(in) :: a, b
        type(array_type) :: c
        c%val = a%val - b%val
    end function

    function scalar_sub(a, b) result(c)
        real, intent(in) :: a
        type(array_type), intent(in) :: b
        type(array_type) :: c
        c%val = a - b%val
    end function

end module operator_overloading_33_mod

module operator_overloading_33_ops_mod
    use operator_overloading_33_mod, only: array_type, operator(-)
    implicit none

contains

    subroutine compute(u)
        type(array_type), intent(in) :: u
        type(array_type) :: y
        y = -u
        if (abs(y%val - (-3.0)) > 1.0e-6) error stop
    end subroutine

end module operator_overloading_33_ops_mod

program operator_overloading_33
    use operator_overloading_33_mod
    use operator_overloading_33_ops_mod
    implicit none
    type(array_type) :: x
    x%val = 3.0
    call compute(x)
end program operator_overloading_33
