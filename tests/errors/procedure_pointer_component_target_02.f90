! A procedure pointer component value may name a module procedure defined
! later in the module, but one that does not match the component's interface
! stops compilation.
module procedure_pointer_component_target_02_m
    implicit none
    abstract interface
        subroutine void()
        end subroutine
    end interface
    type :: pp
        procedure(void), pointer, nopass :: pf => null()
    end type
    type(pp) :: s1 = pp(f)
contains
    integer function f()
        f = 3
    end function
end module

program procedure_pointer_component_target_02
    use procedure_pointer_component_target_02_m
    call s1%pf()
end program
