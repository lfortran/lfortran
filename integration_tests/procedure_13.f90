module pintrf_mod_procedure_13
implicit none
private
public :: OBJCON

interface
    subroutine OBJCON(x)
        implicit none
        real, intent(in) :: x
    end subroutine OBJCON
end interface
end module pintrf_mod_procedure_13

module cobylb_mod_procedure_13
contains
subroutine cobylb(calcfc)
use, non_intrinsic :: pintrf_mod_procedure_13, only : OBJCON

implicit none

procedure(OBJCON) :: calcfc

call evaluate(calcfc_internal)
contains

    subroutine calcfc_internal(x_internal)
        implicit none
        real, intent(in) :: x_internal
        call calcfc(x_internal)
    end subroutine calcfc_internal

    subroutine evaluate(calcfc)
        use, non_intrinsic :: pintrf_mod_procedure_13, only : OBJCON
        implicit none
        procedure(OBJCON) :: calcfc
        call calcfc(12.91)
    end subroutine evaluate

end subroutine cobylb
end module

program procedure_13
use cobylb_mod_procedure_13

call cobylb(calcfc)

contains
    subroutine calcfc(x)
        implicit none
        real, intent(in) :: x
        print *, "inside calcfc: ", x
        if ( abs(x - 12.91) > 1e-8 ) error stop
    end subroutine calcfc
end program
