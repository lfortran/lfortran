module derived_types_55_m
    implicit none

    type :: Base
    contains
        procedure :: get_out
    end type Base

    type, extends(Base) :: Derived
    contains
        procedure :: get_out => derived_get_out
    end type Derived

contains

    subroutine get_out(self, outi)
        class(Base), intent(in) :: self
        integer, intent(out) :: outi

        outi = 1
    end subroutine get_out

    subroutine derived_get_out(self, outi)
        class(Derived), intent(in) :: self
        integer, intent(out) :: outi

        outi = 2
    end subroutine derived_get_out

    subroutine helper(self)
        class(Base), intent(in) :: self
        integer :: outi

        call self%get_out(outi)
        if (outi /= 2) error stop
    end subroutine helper
end module derived_types_55_m


program derived_types_55
    use derived_types_55_m
    implicit none

    type(Derived) :: c
    call helper(c)
end program derived_types_55
