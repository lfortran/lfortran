! A use-associated operator calls its own specific, not a host procedure
! that has the name of the specific.
module modules_81_ops
    implicit none
    private
    public :: t, operator(+), operator(-), operator(==), operator(.and.)

    type :: t
        integer :: x
    end type

    interface operator(+)
        module procedure add_t
    end interface

    interface operator(-)
        module procedure neg_t
    end interface

    interface operator(==)
        module procedure eq_t
    end interface

    interface operator(.and.)
        module procedure and_t
    end interface

contains

    function add_t(a, b) result(c)
        type(t), intent(in) :: a, b
        type(t) :: c
        c%x = a%x + b%x
    end function

    function neg_t(a) result(c)
        type(t), intent(in) :: a
        type(t) :: c
        c%x = -a%x
    end function

    logical function eq_t(a, b)
        type(t), intent(in) :: a, b
        eq_t = a%x == b%x
    end function

    logical function and_t(a, b)
        type(t), intent(in) :: a, b
        and_t = a%x > 0 .and. b%x > 0
    end function

end module

module modules_81_host
    use modules_81_ops, only: t
    implicit none

contains

    function add_t(a, b) result(c)
        type(t), intent(in) :: a, b
        type(t) :: c
        c%x = 100
    end function

    function neg_t(a) result(c)
        type(t), intent(in) :: a
        type(t) :: c
        c%x = 200
    end function

    logical function eq_t(a, b)
        type(t), intent(in) :: a, b
        eq_t = .true.
    end function

    logical function and_t(a, b)
        type(t), intent(in) :: a, b
        and_t = .false.
    end function

    subroutine run()
        use modules_81_ops, only: t, operator(+), operator(-), operator(==), &
            operator(.and.)
        type(t) :: r, s
        r = t(1) + t(2)
        s = -t(5)
        if (r%x /= 3) error stop
        if (s%x /= -5) error stop
        if (t(1) == t(2)) error stop
        if (.not. (t(1) .and. t(2))) error stop
        print *, r%x, s%x, t(1) == t(2), t(1) .and. t(2)
    end subroutine

end module

program modules_81
    use modules_81_host
    implicit none
    call run()
end program
