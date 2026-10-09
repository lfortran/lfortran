module traits_static_04_m
    implicit none

    abstract interface :: IVaLuE
        subroutine gEt_VaLuE(oUt)
            integer, intent(out) :: oUt
        end subroutine gEt_VaLuE
    end interface IVaLuE

    type :: MiXeDBox
        integer :: VaLuE
    end type MiXeDBox

    implements IVaLuE :: MiXeDBox
        procedure, pass(self) :: gEt_VaLuE => MiXeDBox_get_value
    end implements MiXeDBox

contains

    subroutine MiXeDBox_get_value(self, oUt)
        class(MiXeDBox), intent(in) :: self
        integer, intent(out) :: oUt
        oUt = self%VaLuE
    end subroutine MiXeDBox_get_value

    subroutine query{IVaLuE :: T}(x, oUt)
        type(T), intent(in) :: x
        integer, intent(out) :: oUt
        call x%gEt_VaLuE(oUt)
    end subroutine query
end module traits_static_04_m

program traits_static_04
    use traits_static_04_m
    implicit none
    type(MiXeDBox) :: box
    integer :: oUt
    integer :: implements, endimplements

    implements = 2
    endimplements = 3
    if (implements + endimplements /= 5) error stop
    box = MiXeDBox(9)
    call query(box, oUt)
    if (oUt /= 9) error stop
end program traits_static_04
