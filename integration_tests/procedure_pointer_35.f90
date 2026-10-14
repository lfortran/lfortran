module procedure_pointer_35_funcs
implicit none
contains
    integer function add_one(x)
        integer, intent(in) :: x
        add_one = x + 1
    end function
end module

! A template declared after a structure constructor that gives an already
! accessible function to a procedure pointer component, whose interface is a
! module function defined only later, does not affect that constructor.
! Inside a template, such a component, whether declared by the host or by the
! template, and that function itself refer to the module function.
module procedure_pointer_35_mod
use procedure_pointer_35_funcs, only: add_one
implicit none
type :: pp
    procedure(ifunc), pointer, nopass :: pf => null()
end type
type(pp) :: s = pp(add_one)
template unused {t}
    deferred type :: t
contains
    subroutine noop(x)
        type(t), intent(in) :: x
    end subroutine
end template
template add_two_tmpl {t}
    deferred type :: t
    type(pp), parameter :: c = pp(add_one)
    type :: tt
        procedure(ifunc), pointer, nopass :: pf => null()
    end type
    type(tt), parameter :: d = tt(add_one)
contains
    integer function add_two(x)
        integer, intent(in) :: x
        type(pp) :: a
        type(tt) :: b
        a = c
        b = d
        add_two = a%pf(b%pf(x)) + ifunc(x)
    end function
end template
contains
    integer function ifunc(x)
        integer, intent(in) :: x
        ifunc = 0
    end function
end module

program procedure_pointer_35
use procedure_pointer_35_mod
implicit none
instantiate add_two_tmpl {integer}, only: add_two => add_two
if (s%pf(41) /= 42) error stop
if (add_two(40) /= 42) error stop
print *, s%pf(41), add_two(40)
end program
