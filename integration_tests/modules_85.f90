! assignment(=) from two modules whose private specifics have the same
! name: each type uses its own.
module modules_85_ma
    implicit none
    private
    public :: ta, assignment(=)

    type :: ta
        integer :: i = 0
    end type

    interface assignment(=)
        module procedure assign
    end interface

contains

    subroutine assign(lhs, rhs)
        type(ta), intent(inout) :: lhs
        integer, intent(in) :: rhs
        lhs%i = rhs
    end subroutine

end module

module modules_85_mb
    implicit none
    private
    public :: tb, assignment(=)

    type :: tb
        integer :: i = 0
    end type

    interface assignment(=)
        module procedure assign
    end interface

contains

    subroutine assign(lhs, rhs)
        type(tb), intent(inout) :: lhs
        integer, intent(in) :: rhs
        lhs%i = rhs + 100
    end subroutine

end module

program modules_85
    use modules_85_ma
    use modules_85_mb
    implicit none
    type(ta) :: a
    type(tb) :: b

    a = 1
    b = 2
    if (a%i /= 1) error stop
    if (b%i /= 102) error stop
    print *, a%i, b%i
end program
