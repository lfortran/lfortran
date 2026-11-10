! A procedure pointer component of a derived type declared in a module
! procedure takes its interface from that procedure's internal procedure,
! which hides the module procedure of the same name defined before it.
module procedure_pointer_52_mod
implicit none
contains
    integer function later()
        later = 7
    end function
    subroutine shadowed(res)
        integer, intent(out) :: res
        type :: tt
            procedure(later), pointer, nopass :: pf => null()
        end type
        type(tt) :: a
        a%pf => later
        call a%pf(res)
    contains
        subroutine later(r)
            integer, intent(out) :: r
            r = 5
        end subroutine
    end subroutine
end module
program procedure_pointer_52
use procedure_pointer_52_mod
integer :: r
call shadowed(r)
if (r /= 5) error stop
if (later() /= 7) error stop
print *, r, later()
end program procedure_pointer_52
