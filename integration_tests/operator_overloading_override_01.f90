! A parent type declares a type-bound unary `-` and a defined operator
! `.plus.`, and an extended type overrides their specific bindings. The
! operators call the bindings of the object's own type.
! See https://github.com/lfortran/lfortran/issues/14907
! and https://github.com/lfortran/lfortran/issues/14908

module operator_overloading_override_01_mod
    implicit none

    type :: base_t
        integer :: i = 0
    contains
        procedure :: neg
        procedure :: add
        generic :: operator(-) => neg
        generic :: operator(.plus.) => add
    end type

    type, extends(base_t) :: child_t
    contains
        procedure :: neg => neg_child
        procedure :: add => add_child
    end type

contains

    integer function neg(a)
        class(base_t), intent(in) :: a
        neg = -a%i
    end function

    integer function add(a, b)
        class(base_t), intent(in) :: a
        integer, intent(in) :: b
        add = a%i + b
    end function

    integer function neg_child(a)
        class(child_t), intent(in) :: a
        neg_child = -a%i - 100
    end function

    integer function add_child(a, b)
        class(child_t), intent(in) :: a
        integer, intent(in) :: b
        add_child = a%i + b + 100
    end function

end module

program operator_overloading_override_01
    use operator_overloading_override_01_mod
    implicit none
    type(child_t) :: c

    c%i = 1
    if (-c /= -101) error stop
    if ((c .plus. 5) /= 106) error stop
    print *, -c, c .plus. 5
end program
