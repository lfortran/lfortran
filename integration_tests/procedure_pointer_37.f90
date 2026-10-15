module procedure_pointer_37_funcs
implicit none
integer :: hits = 0
contains
    integer function forty_two()
        forty_two = 42
    end function
    subroutine bump()
        hits = hits + 1
    end subroutine
end module

! The interface of a procedure pointer component of a derived type declared
! in a procedure is the procedure of that name accessible in the procedure:
! its own internal procedure rather than the host's procedure of the same
! name, both defined after the type.
module procedure_pointer_37_mod
use procedure_pointer_37_funcs, only: forty_two, bump
implicit none
template local_type_tmpl {t}
    deferred type :: t
contains
    ! A derived type declared in a procedure contained in a procedure of a
    ! template can take the interface from a procedure of the template's
    ! host defined later.
    integer function outer()
        outer = inner()
    contains
        integer function inner()
            type :: tt
                procedure(later), pointer, nopass :: pf => null()
            end type
            type(tt), parameter :: c = tt(forty_two)
            type(tt) :: a
            a = c
            inner = a%pf()
        end function
    end function
end template
contains
    subroutine shadowed()
        type :: tt
            procedure(later), pointer, nopass :: pf => null()
        end type
        type(tt), parameter :: c = tt(bump)
        type(tt) :: a
        a = c
        call a%pf()
    contains
        subroutine later()
        end subroutine
    end subroutine
    integer function host()
        type :: tt
            procedure(later), pointer, nopass :: pf => null()
        end type
        type(tt), parameter :: c = tt(forty_two)
        type(tt) :: a
        a = c
        host = a%pf()
    end function
    integer function later()
        later = 0
    end function
end module

program procedure_pointer_37
use procedure_pointer_37_funcs, only: hits
use procedure_pointer_37_mod
implicit none
instantiate local_type_tmpl {integer}, only: outer => outer
call shadowed()
if (hits /= 1) error stop
if (host() /= 42) error stop
if (outer() /= 42) error stop
print *, hits, host(), outer()
end program
