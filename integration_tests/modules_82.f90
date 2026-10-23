! The private specifics of the operators of a module, used through a
! module that passes them on, are not the procedures of the program
! that have their names.
module modules_82_a
    implicit none
    private
    public :: t, operator(.plus.), operator(.and.), operator(==), &
        operator(-), operator(//), operator(+)

    type :: t
        integer :: x
    end type

    interface operator(.plus.)
        module procedure plus_i
    end interface

    interface operator(.and.)
        module procedure and_t
    end interface

    interface operator(==)
        module procedure eq_t
    end interface

    interface operator(-)
        module procedure neg_t
    end interface

    interface operator(//)
        module procedure cat_t
    end interface

    interface operator(+)
        module procedure add_li
    end interface

contains

    integer function plus_i(a, b)
        integer, intent(in) :: a, b
        plus_i = a + 2*b
    end function

    logical function and_t(a, b)
        type(t), intent(in) :: a, b
        and_t = a%x > 0 .and. b%x > 0
    end function

    logical function eq_t(a, b)
        type(t), intent(in) :: a, b
        eq_t = a%x == b%x
    end function

    function neg_t(a) result(c)
        type(t), intent(in) :: a
        type(t) :: c
        c%x = -2*a%x
    end function

    function cat_t(a, b) result(c)
        type(t), intent(in) :: a, b
        type(t) :: c
        c%x = 10*a%x + b%x
    end function

    integer function add_li(a, b)
        logical, intent(in) :: a
        integer, intent(in) :: b
        add_li = merge(1, 0, a) + b
    end function

end module

module modules_82_b
    use modules_82_a
end module

program modules_82
    use modules_82_b
    implicit none
    type(t) :: a

    if ((1 .plus. 2) /= 5) error stop
    if (.not. (t(1) .and. t(2))) error stop
    if (.not. (t(3) == t(3))) error stop
    a = -t(4)
    if (a%x /= -8) error stop
    a = t(1) // t(2)
    if (a%x /= 12) error stop
    if ((.true. + 2) /= 3) error stop

    if (plus_i(1) /= 100) error stop
    if (and_t(1) /= 200) error stop
    if (eq_t(1) /= 300) error stop
    if (neg_t(1) /= 400) error stop
    if (cat_t(1) /= 500) error stop
    if (add_li(1) /= 600) error stop
    print *, 1 .plus. 2, a%x, .true. + 2, plus_i(1)

contains

    integer function plus_i(i)
        integer, intent(in) :: i
        plus_i = 100*i
    end function

    integer function and_t(i)
        integer, intent(in) :: i
        and_t = 200*i
    end function

    integer function eq_t(i)
        integer, intent(in) :: i
        eq_t = 300*i
    end function

    integer function neg_t(i)
        integer, intent(in) :: i
        neg_t = 400*i
    end function

    integer function cat_t(i)
        integer, intent(in) :: i
        cat_t = 500*i
    end function

    integer function add_li(i)
        integer, intent(in) :: i
        add_li = 600*i
    end function

end program
