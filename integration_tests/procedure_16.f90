module pintrf_mod
implicit none
private
public :: OBJCON

contains
    subroutine OBJCON(x)
        implicit none
        real, intent(in) :: x(:)
    end subroutine OBJCON
end module pintrf_mod

module cobylb_mod_procedure_16
contains
subroutine cobylb(calcfc, amat)
use, non_intrinsic :: pintrf_mod, only : OBJCON

implicit none

procedure(OBJCON) :: calcfc
real, intent(in) :: amat(:,:)

call evaluate(calcfc_internal)
contains

    subroutine calcfc_internal(x_internal)
        implicit none
        real, intent(in) :: x_internal(:)
        call calcfc(x_internal)
    end subroutine calcfc_internal

    subroutine evaluate(calcfc)
        use, non_intrinsic :: pintrf_mod, only : OBJCON
        implicit none
        procedure(OBJCON) :: calcfc
        call calcfc([1.0, 2.0])
    end subroutine evaluate

end subroutine cobylb
end module

program procedure_16
use cobylb_mod_procedure_16

real :: amat(5, 5)

call cobylb(calcfc, amat)

contains
    subroutine calcfc(x)
        implicit none
        real, intent(in) :: x(:)
        print *, x
        if (abs(sum(x) - 3.0) > 1e-8) error stop
    end subroutine calcfc
end program

