! Pure rules for elemental and pure procedures
module continue_compilation_elemental_pure_m
implicit none

type :: ccep_t
    integer :: h = 0
end type

contains

elemental subroutine ccep_elem_sub(x)
    integer :: x  ! {Error} dummy argument 'x' of elemental procedure 'ccep_elem_sub' must have an explicit intent or the value attribute
    x = 1
end subroutine

impure elemental subroutine ccep_impure_elem_sub(x)
    integer :: x  ! {Error} dummy argument 'x' of elemental procedure 'ccep_impure_elem_sub' must have an explicit intent or the value attribute
    x = 1
end subroutine

elemental integer function ccep_elem_fn(x, y)
    integer :: x  ! {Error} dummy argument 'x' of elemental procedure 'ccep_elem_fn' must have an explicit intent or the value attribute
    integer, value :: y
    ccep_elem_fn = x + y
end function

elemental integer function ccep_elem_fn_out(x)
    integer, intent(out) :: x  ! {Error} dummy argument 'x' of pure function 'ccep_elem_fn_out' must be intent(in) or have the value attribute
    x = 1
    ccep_elem_fn_out = 1
end function

pure integer function ccep_pure_fn(a, b, p, v, g)
    integer, intent(out) :: a  ! {Error} dummy argument 'a' of pure function 'ccep_pure_fn' must be intent(in) or have the value attribute
    integer, intent(inout) :: b  ! {Error} dummy argument 'b' of pure function 'ccep_pure_fn' must be intent(in) or have the value attribute
    integer, pointer :: p
    integer, value :: v
    interface
        pure integer function g(y)
            integer, intent(in) :: y
        end function
    end interface
    a = 1
    ccep_pure_fn = b + p + v + g(1)
end function

pure subroutine ccep_pure_sub(x, p)
    integer :: x  ! {Error} dummy argument 'x' of pure subroutine 'ccep_pure_sub' must have an explicit intent or the value attribute
    integer, pointer :: p
    x = p
end subroutine

pure integer function ccep_init() result(r)
    integer, parameter :: k = 3
    integer :: s = 9  ! {Error} local variable 's' of pure procedure 'ccep_init' cannot have an initial value, because it implies the save attribute
    r = s + k
end function

elemental integer function ccep_init_type(x) result(r)
    integer, intent(in) :: x
    type(ccep_t) :: s = ccep_t(9)  ! {Error} local variable 's' of pure procedure 'ccep_init_type' cannot have an initial value, because it implies the save attribute
    type(ccep_t) :: u
    r = s%h + u%h + x
end function

pure integer function ccep_save() result(r)
    integer, save :: s  ! {Error} local variable 's' of pure procedure 'ccep_save' cannot have the save attribute
    s = 1
    r = s
end function

end module
