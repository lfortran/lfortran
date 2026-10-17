! A named constant of derived type whose procedure pointer component is
! associated with a module procedure, called through the component of a dummy
! argument associated with the constant, since a procedure pointer component
! of a constant cannot be called directly (R1040, C901).
module procedure_pointer_31_mod
implicit none
abstract interface
    subroutine void()
    end subroutine
    integer function ifunc(x)
        integer, intent(in) :: x
    end function
end interface
integer :: cnt = 0
contains
    subroutine add_one()
        cnt = cnt + 1
    end subroutine
    integer function twice(x)
        integer, intent(in) :: x
        twice = 2*x
    end function
end module

module procedure_pointer_31_types
use procedure_pointer_31_mod
implicit none
type :: pp
    procedure(void), pointer, nopass :: pf => null()
end type
type :: pf_t
    integer :: n = 5
    procedure(ifunc), pointer, nopass :: f => null()
end type
type(pp), parameter :: s = pp(add_one)
type(pf_t), parameter :: t = pf_t(7, twice)
end module

program procedure_pointer_31
use procedure_pointer_31_types
implicit none
call run(s)
call run(s)
if (cnt /= 2) error stop
if (t%n /= 7) error stop
if (apply(t) /= 14) error stop
print *, cnt, apply(t)
contains
    subroutine run(x)
        type(pp), intent(in) :: x
        call x%pf()
    end subroutine
    integer function apply(x)
        type(pf_t), intent(in) :: x
        apply = x%f(x%n)
    end function
end program
