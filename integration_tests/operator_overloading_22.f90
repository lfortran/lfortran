! Regression test for unary defined operator with elemental function
! Covers:
!  - OPERATOR(.rw.) defined via MODULE PROCEDURE
!  - ELEMENTAL function used in expression context
!  - Use inside another elemental function
!  - Use from main program

module operator_overloading_22_mod
    implicit none
    private

    public :: myt, afunc, operator(.rw.)

    type myt
        sequence
        integer :: x
        integer :: y
        integer :: z
    end type myt

    interface operator(.rw.)
        module procedure rw
    end interface

contains

    integer elemental function rw(t)
        implicit none
        type(myt), intent(in) :: t
        rw = 1
    end function rw

    logical elemental function afunc(t)
        implicit none
        type(myt), intent(in) :: t

        if (.rw. t <= 5) then
            afunc = .true.
        else
            afunc = .false.
        end if
    end function afunc

end module operator_overloading_22_mod


program operator_overloading_22
    use operator_overloading_22_mod
    implicit none

    type(myt) :: t
    logical :: z
    integer :: x

    t = myt(1, 2, 3)

    x = .rw. t
    z = afunc(t)

    print *, x
    print *, z
end program operator_overloading_22
