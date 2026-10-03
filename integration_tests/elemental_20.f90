! Test elemental defined assignment(=) with array arguments.
! The elemental subroutine must be called element-by-element.
module elemental_20_mod
    implicit none
    type :: t
        integer :: val
    end type t
    interface assignment(=)
        module procedure :: assign_t_char
    end interface
contains
    elemental subroutine assign_t_char(lhs, rhs)
        type(t), intent(inout) :: lhs
        character(len=*), intent(in) :: rhs
        lhs%val = len(rhs)
    end subroutine
end module elemental_20_mod

program elemental_20
    use elemental_20_mod
    implicit none
    type(t) :: arr(3)

    arr = "hello"
    if (arr(1)%val /= 5) error stop
    if (arr(2)%val /= 5) error stop
    if (arr(3)%val /= 5) error stop

    arr = "ab"
    if (arr(1)%val /= 2) error stop
    if (arr(2)%val /= 2) error stop
    if (arr(3)%val /= 2) error stop

    print *, "PASS"
end program elemental_20
