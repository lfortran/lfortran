! A defined assignment with an elemental specific listed before a
! non-elemental one: the specific whose dummy ranks match is called.
! See https://github.com/lfortran/lfortran/issues/14756

module defined_assignment_elemental_01_mod
    implicit none

    type :: a_t
        integer :: i
    end type

    interface assignment(=)
        module procedure set_elemental, set_array
    end interface

contains

    elemental subroutine set_elemental(lhs, rhs)
        type(a_t), intent(out) :: lhs
        integer, intent(in) :: rhs
        lhs%i = rhs
    end subroutine

    subroutine set_array(lhs, rhs)
        type(a_t), intent(out) :: lhs
        integer, intent(in) :: rhs(:)
        lhs%i = sum(rhs)
    end subroutine

end module

program defined_assignment_elemental_01
    use defined_assignment_elemental_01_mod
    implicit none
    type(a_t) :: a(3), s

    s = [1, 2, 3]
    if (s%i /= 6) error stop
    a = 4
    if (any(a%i /= 4)) error stop
    print *, s%i, a%i
end program
