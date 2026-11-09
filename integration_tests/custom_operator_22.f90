module m_custom_operator_22_tb
    implicit none
    type :: t
    contains
        procedure :: f
        procedure :: f_int
        generic :: operator(.f.) => f, f_int
        procedure :: dot
        procedure :: dot_int
        generic :: operator(.dot.) => dot, dot_int
    end type
contains
    integer function f(x, y)
        class(t), intent(in) :: x, y
        f = 1
    end function

    integer function f_int(x, n)
        class(t), intent(in) :: x
        integer, intent(in) :: n
        f_int = 2
    end function

    integer function dot(x, y)
        class(t), intent(in) :: x, y
        dot = 3
    end function

    integer function dot_int(x, n)
        class(t), intent(in) :: x
        integer, intent(in) :: n
        dot_int = 4
    end function

    subroutine check_tb()
        type(t) :: a, b
        if ((a .f. b) /= 1) error stop 1
        if ((a .f. 2) /= 2) error stop 2
        if ((a .dot. b) /= 3) error stop 3
        if ((a .dot. 2) /= 4) error stop 4
    end subroutine
end module

module m_custom_operator_22_iface
    implicit none
    type :: t
    end type
    interface operator(.f.)
        module procedure f, f_int
    end interface
contains
    integer function f(x, y)
        type(t), intent(in) :: x, y
        f = 1
    end function

    integer function f_int(x, n)
        type(t), intent(in) :: x
        integer, intent(in) :: n
        f_int = 2
    end function
end module

program custom_operator_22
    use m_custom_operator_22_tb, only: check_tb
    use m_custom_operator_22_iface
    implicit none
    type(t) :: a, b

    call check_tb()
    if ((a .f. b) /= 1) error stop 5
    if ((a .f. 2) /= 2) error stop 6
end program
