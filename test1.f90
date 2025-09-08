program main
    implicit none

    type :: Base
        integer :: x
    end type

    type, extends(Base) :: Derived
        real :: r
    end type

    type(Base) :: base_var
    type(Derived) :: derived_var

    base_var = derived_var
end program main