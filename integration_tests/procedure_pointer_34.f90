module procedure_pointer_34_funcs
implicit none
contains
    integer function add_one(x)
        integer, intent(in) :: x
        add_one = x + 1
    end function
    integer function twice(x)
        integer, intent(in) :: x
        twice = 2*x
    end function
end module

! A structure constructor in the specification part of a module, or of one of
! its procedures, gives an already accessible function to a procedure pointer
! component whose interface is a module function defined only later, after
! `contains`.
module procedure_pointer_34_mod
use procedure_pointer_34_funcs, only: add_one, twice
implicit none
type :: pp
    procedure(ifunc), pointer, nopass :: pf => null()
end type
type(pp) :: s1 = pp(add_one)
type(pp) :: s2 = pp(pf=twice)
type(pp), parameter :: c = pp(twice)
contains
    integer function local_value(x)
        integer, intent(in) :: x
        type(pp) :: s4 = pp(add_one)
        local_value = s4%pf(x)
    end function
    integer function ifunc(x)
        integer, intent(in) :: x
        ifunc = 0
    end function
end module

program procedure_pointer_34
use procedure_pointer_34_mod
implicit none
type(pp) :: s3
if (s1%pf(41) /= 42) error stop
if (s2%pf(21) /= 42) error stop
s3 = c
if (s3%pf(5) /= 10) error stop
if (local_value(9) /= 10) error stop
print *, s1%pf(41), s2%pf(21), s3%pf(5), local_value(9)
end program
