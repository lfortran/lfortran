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
contains
    integer function ifunc(x)
        integer, intent(in) :: x
        ifunc = 0
    end function
end module

program procedure_pointer_35
use procedure_pointer_35_mod
implicit none
if (s%pf(41) /= 42) error stop
print *, s%pf(41)
end program
