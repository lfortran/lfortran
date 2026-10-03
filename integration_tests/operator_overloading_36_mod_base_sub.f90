submodule(operator_overloading_36_mod_base) operator_overloading_36_mod_base_sub
    implicit none
contains
    module function negate_array(a) result(c)
        class(array_type), intent(in) :: a
        type(array_type) :: c
        c%val = -a%val
    end function
    module function add_arrays(a, b) result(c)
        class(array_type), intent(in) :: a, b
        type(array_type) :: c
        c%val = a%val + b%val
    end function
    module function subtract_arrays(a, b) result(c)
        class(array_type), intent(in) :: a, b
        type(array_type) :: c
        c = a + (-b)
    end function
end submodule
