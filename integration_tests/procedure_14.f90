module cobylb_mod_procedure_14
contains
subroutine cobylb(calcfc)

implicit none

interface
    subroutine OBJCON(x)
        implicit none
        real, intent(in) :: x(:)
    end subroutine OBJCON
end interface

procedure(OBJCON) :: calcfc

call calcfc_internal([1.0, 2.0])
contains

    subroutine calcfc_internal(x_internal)
        implicit none
        real, intent(in) :: x_internal(:)
        call calcfc(x_internal)
    end subroutine calcfc_internal

end subroutine cobylb
end module

program procedure_14
use cobylb_mod_procedure_14
implicit none
call cobylb(calcfc)
contains
subroutine calcfc(x)
    implicit none
    real, intent(in) :: x(:)
    print *, x
    if ( abs(sum(x) - 3.0) > 1e-8 ) error stop
end subroutine
end program
