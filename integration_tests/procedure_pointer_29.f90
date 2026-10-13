! A structure constructor in the specification part of a module gives a
! procedure pointer component a module procedure that is defined only later,
! after `contains`.
module procedure_pointer_29_mod
implicit none
abstract interface
    subroutine void()
    end subroutine
    integer function ifunc(x)
        integer, intent(in) :: x
    end function
end interface
type :: pp
    procedure(void), pointer, nopass :: pf => null()
end type
type :: pf_t
    procedure(ifunc), pointer, nopass :: f => null()
end type
integer :: cnt = 0
type(pp) :: s1 = pp(add_one)
type(pp) :: s2 = pp(pf=add_ten)
type(pf_t) :: s3 = pf_t(twice)
contains
    subroutine add_one()
        cnt = cnt + 1
    end subroutine
    subroutine add_ten()
        cnt = cnt + 10
    end subroutine
    integer function twice(x)
        integer, intent(in) :: x
        twice = 2*x
    end function
end module

program procedure_pointer_29
use procedure_pointer_29_mod
implicit none
call s1%pf()
call s2%pf()
if (cnt /= 11) error stop
if (s3%f(21) /= 42) error stop
print *, cnt, s3%f(21)
end program
