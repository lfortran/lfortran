module nested_vars_34_mod
implicit none

type :: t
    procedure(norm_iface), pointer :: norm => null()
contains
    procedure :: interp
end type

abstract interface
    function norm_iface(me, fvec) result(r)
        import :: t
        class(t), intent(in) :: me
        real(8), intent(in) :: fvec(2)
        real(8) :: r
    end function
end interface

contains

    subroutine callee(a)
        real(8), intent(in) :: a(2)
        if (abs(a(1) - 1.0d0) > 1.0d-12) error stop 1
        if (abs(a(2) - 2.0d0) > 1.0d-12) error stop 2
    end subroutine

    subroutine host(a)
        real(8), intent(in) :: a(2)
        call inner()
    contains
        subroutine inner()
            call callee(a)
        end subroutine
    end subroutine

    subroutine interp(me, y)
        class(t), intent(inout) :: me
        real(8), intent(out) :: y(2)
        y(1) = 3.0d0
        y(2) = 4.0d0
    end subroutine

    subroutine wrapper(me, y)
        class(t), intent(inout) :: me
        real(8), intent(inout) :: y(2)
        call locate()
    contains
        subroutine locate()
            call me%interp(y)
        end subroutine
    end subroutine

    function mynorm(me, fvec) result(r)
        class(t), intent(in) :: me
        real(8), intent(in) :: fvec(2)
        real(8) :: r
        r = fvec(1) + fvec(2)
    end function

    subroutine host_norm(me, fvec, got)
        class(t), intent(inout) :: me
        real(8), intent(inout) :: fvec(2)
        real(8), intent(out) :: got
        got = inner()
    contains
        real(8) function inner()
            inner = me%norm(fvec)
        end function
    end subroutine

end module

program nested_vars_34
use nested_vars_34_mod
implicit none
type(t) :: obj
real(8) :: a(2), y(2), f(2), got

a = [1.0d0, 2.0d0]
call host(a)

y = 0.0d0
call wrapper(obj, y)
if (abs(y(1) - 3.0d0) > 1.0d-12) error stop 3
if (abs(y(2) - 4.0d0) > 1.0d-12) error stop 4

obj%norm => mynorm
f = [3.0d0, 4.0d0]
call host_norm(obj, f, got)
if (abs(got - 7.0d0) > 1.0d-12) error stop 5
end program
