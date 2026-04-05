module operator_overloading_36_mod_base
    implicit none
    type :: array_type
        real :: val
    contains
        procedure :: negate_array
        procedure :: add_arrays
        procedure :: subtract_arrays
        generic, public :: operator(-) => negate_array, subtract_arrays
        generic, public :: operator(+) => add_arrays
    end type
    interface
        module function negate_array(a) result(c)
            class(array_type), intent(in) :: a
            type(array_type) :: c
        end function
        module function add_arrays(a, b) result(c)
            class(array_type), intent(in) :: a, b
            type(array_type) :: c
        end function
        module function subtract_arrays(a, b) result(c)
            class(array_type), intent(in) :: a, b
            type(array_type) :: c
        end function
    end interface
end module
