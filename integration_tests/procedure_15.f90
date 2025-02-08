module nested_context
interface
subroutine lcompilers_OBJCON(x)
    implicit none
    real, intent(in) :: x(:)
end subroutine lcompilers_OBJCON
end interface
procedure(lcompilers_OBJCON), pointer :: lcompilers_calcfc
end module nested_context

module cobylb_mod_procedure_15
use nested_context, only: lcompilers_calcfc
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
lcompilers_calcfc => calcfc
call calcfc_internal([1.0, 2.0])
contains

    subroutine calcfc_internal(x_internal)
        use nested_context, only: lcompilers_calcfc
        implicit none
        real, intent(in) :: x_internal(:)
        call lcompilers_calcfc(x_internal)
    end subroutine calcfc_internal

end subroutine cobylb
end module cobylb_mod_procedure_15

program procedure_15
use cobylb_mod_procedure_15
implicit none
call cobylb(calcfc)
contains
subroutine calcfc(x)
    implicit none
    real, intent(in) :: x(:)
    print *, x
    if ( abs(sum(x) - 3.0) > 1e-8 ) error stop
end subroutine
end program procedure_15
