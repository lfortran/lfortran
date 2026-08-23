module m_custom_operator_20
    implicit none

    type :: t
        integer :: x
    end type

    interface assignment(=)
        module procedure defAsst2
    end interface

contains

    subroutine defAsst2(lhs, rhs)
        type(t), intent(out) :: lhs
        integer, intent(in)  :: rhs

        lhs%x = rhs
    end subroutine
end module

program custom_operator_20
    use m_custom_operator_20
    implicit none

    type(t) :: a
    a = 5

    print *, a%x
    if (a%x /= 5) error stop
end program