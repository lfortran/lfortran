module derived_types_13_module_01
    implicit none
    type t
        integer :: i
    end type t
contains
    function new_value() result(p)
        type(t) :: p
        p%i = 123
    end function new_value
end module derived_types_13_module_01

program derived_types_13
    use derived_types_13_module_01
    implicit none

    type(t) :: x
    x = new_value()
    if (x%i /= 123) error stop
end program derived_types_13
