module pass_array_by_data_10_pintrf_module
public :: OBJCON

contains

    subroutine OBJCON(x, out)
        implicit none
        real, intent(in) :: x(:)
        real, intent(out) :: out
    end subroutine OBJCON

end module pass_array_by_data_10_pintrf_module

subroutine cobyla(x, out)
    implicit none

    real, intent(inout) :: x(:)
    real, intent(out) :: out
    call evaluate(calcfc_internal, out)
    contains

    subroutine calcfc_internal(x_internal, out)
        implicit none
        real, intent(in) :: x_internal(:)
        real, intent(out) :: out

        out = sum(x_internal**2)
    end subroutine calcfc_internal

    subroutine evaluate(calcfc, out)
        use, non_intrinsic :: pass_array_by_data_10_pintrf_module, only : OBJCON
        implicit none
        procedure(OBJCON) :: calcfc
        real, intent(out) :: out
        call calcfc(x, out)
    end subroutine evaluate

end subroutine

program pass_array_by_data_10
    implicit none
    interface
        subroutine cobyla(x, out)
            real, intent(inout) :: x(:)
            real, intent(out) :: out
        end subroutine cobyla
    end interface

    real :: x(2) = [1.0, 2.0]
    real :: out
    call cobyla(x, out)
    print *, out
    if ( abs(out - 5.0 ) > 1e-8 ) error stop
end program

