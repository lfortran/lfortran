module template_operator_mod_01_ops
    implicit none

    interface operator(.plus.)
        module procedure iplus
    end interface

contains

    pure function iplus(x, y) result(r)
        integer, intent(in) :: x, y
        integer :: r
        r = x + y
    end function
end module

module template_operator_mod_01_tmpl
    use template_operator_mod_01_ops
    implicit none

    template ops_t{T, mul, neg}
        deferred type :: T
        deferred interface
            pure function mul(x, y) result(r)
                type(T), intent(in) :: x, y
                type(T) :: r
            end function
            pure function neg(x) result(r)
                type(T), intent(in) :: x
                type(T) :: r
            end function
        end interface
        interface operator(*)
            module procedure mul
        end interface
        interface operator(.neg.)
            module procedure neg
        end interface
    contains
        pure function square(x) result(r)
            type(T), intent(in) :: x
            type(T) :: r
            r = x*x
        end function
        pure function negate(x) result(r)
            type(T), intent(in) :: x
            type(T) :: r
            r = .neg. x
        end function
        pure function bump(n) result(r)
            integer, intent(in) :: n
            integer :: r
            r = n .plus. 1
        end function
    end template
end module

module template_operator_mod_01_user
    use template_operator_mod_01_tmpl
    implicit none

    type :: v_t
        integer :: i
    end type

    instantiate ops_t{v_t, vmul, vneg}
    instantiate ops_t{integer, imul, ineg}, only: isquare => square, &
        inegate => negate

contains

    pure function vmul(x, y) result(r)
        type(v_t), intent(in) :: x, y
        type(v_t) :: r
        r%i = x%i*y%i
    end function

    pure function vneg(x) result(r)
        type(v_t), intent(in) :: x
        type(v_t) :: r
        r%i = -x%i
    end function

    pure function imul(x, y) result(r)
        integer, intent(in) :: x, y
        integer :: r
        r = x*y + 1
    end function

    pure function ineg(x) result(r)
        integer, intent(in) :: x
        integer :: r
        r = -x - 1
    end function
end module
