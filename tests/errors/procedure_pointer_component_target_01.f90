! A procedure pointer component value may name a module procedure defined
! later in the module, but a name the module never defines stops compilation.
module procedure_pointer_component_target_01_m
    implicit none
    abstract interface
        subroutine void()
        end subroutine
    end interface
    type :: pp
        procedure(void), pointer, nopass :: pf => null()
    end type
    type(pp) :: s1 = pp(nosuch)
contains
    subroutine add_one()
    end subroutine
end module

program procedure_pointer_component_target_01
    use procedure_pointer_component_target_01_m
    call s1%pf()
end program
