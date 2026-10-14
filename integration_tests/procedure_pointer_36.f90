module procedure_pointer_36_funcs
implicit none
contains
    integer function forty_two()
        forty_two = 42
    end function
    integer function seven()
        seven = 7
    end function
end module

! A derived type declared in a procedure can take the interface of a
! procedure pointer component from a procedure defined later in its host, as
! can another derived type of the same name declared in another procedure,
! and a structure constructor can give such a component a function.
module procedure_pointer_36_mod
use procedure_pointer_36_funcs, only: forty_two, seven
implicit none
contains
    integer function invoke_direct()
        type :: tt
            procedure(later), pointer, nopass :: pf => null()
        end type
        type(tt) :: a
        a = tt(forty_two)
        invoke_direct = a%pf()
    end function
    integer function invoke_nested()
        type :: tt
            procedure(later), pointer, nopass :: pf => null()
        end type
        type :: holder
            type(tt) :: item
        end type
        type(holder) :: a
        a = holder(tt(seven))
        invoke_nested = a%item%pf()
    end function
    integer function later()
        later = 0
    end function
end module

! A derived type declared in an external procedure can take the interface of
! a procedure pointer component from an internal procedure of it.
integer function procedure_pointer_36_external()
use procedure_pointer_36_funcs, only: forty_two
implicit none
type :: tt
    procedure(later), pointer, nopass :: pf => null()
end type
type(tt) :: a
a = tt(forty_two)
procedure_pointer_36_external = a%pf()
contains
    integer function later()
        later = 0
    end function
end function

program procedure_pointer_36
use procedure_pointer_36_mod, only: invoke_direct, invoke_nested
implicit none
interface
    integer function procedure_pointer_36_external()
    end function
end interface
if (invoke_direct() /= 42) error stop
if (invoke_nested() /= 7) error stop
if (procedure_pointer_36_external() /= 42) error stop
print *, invoke_direct(), invoke_nested(), procedure_pointer_36_external()
end program
