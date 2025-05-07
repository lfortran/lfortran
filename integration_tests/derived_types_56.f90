module derived_types_56_m
    implicit none

    type, abstract :: base
    contains
        procedure(base_m), deferred, pass(self) :: m
        procedure(base_m2), deferred, pass(self) :: m2
        generic :: m_gen => m2, m
    end type

    type, extends(base) :: derived
    contains
        procedure, pass(self) :: m => derived_m
        procedure, pass(self) :: m2 => derived_m2
    end type

    abstract interface
        subroutine base_m(self, outi)
            import :: base
            class(base) :: self
            integer :: outi
        end subroutine

        subroutine base_m2(self, outi, my)
            import :: base
            class(base) :: self
            integer :: outi
            integer :: my
        end subroutine
    end interface

    contains
        subroutine derived_m(self, outi)
            class(derived) :: self
            integer :: outi
            outi = 2
        end subroutine

        subroutine derived_m2(self, outi, my)
            class(derived) :: self
            integer :: outi, my
            outi = 3
        end subroutine
end module

program derived_types_56
    use derived_types_56_m
    implicit none

    type(derived) :: d

    integer :: outi
    call d%m_gen(outi, 10)
    if (outi /= 3) error stop
    call d%m_gen(outi)
    if (outi /= 2) error stop
end program
