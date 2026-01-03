module derived_types_54_m
    implicit none
    type :: Base 
    contains
        procedure :: get_out => Base_get_out
    end type

    type, extends(Base) :: Derived
    contains
        procedure :: get_out => Derived_get_out
    end type

    contains
        subroutine Base_get_out(self, outi)
            class(Base) :: self
            integer :: outi

            outi = 1
        end subroutine

        subroutine Derived_get_out(self, outi)
            class(Derived) :: self
            integer :: outi

            outi = 2
        end subroutine
end module

program derived_types_54
    use derived_types_54_m
    implicit none
    type(Base) :: b
    type(Derived) :: d

    integer :: outi

    call d%get_out(outi)
    if (outi /= 2) error stop
    call b%get_out(outi)
    if (outi /= 1) error stop
end program
