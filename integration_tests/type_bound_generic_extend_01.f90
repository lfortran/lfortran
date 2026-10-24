! An extended type declares a generic binding with the same generic-spec as
! an inherited one. The specific bindings of both types stay available.

module type_bound_generic_extend_01_mod
    implicit none

    type :: base_t
        integer :: i = 0
    contains
        procedure :: set_i
        procedure :: add_i
        generic :: assignment(=) => set_i
        generic :: operator(+) => add_i
        generic :: put => set_i
    end type

    type, extends(base_t) :: child_t
        integer :: j = 0
    contains
        procedure :: set_r
        procedure :: add_r
        generic :: assignment(=) => set_r
        generic :: operator(+) => add_r
        generic :: put => set_r
    end type

contains

    subroutine set_i(lhs, rhs)
        class(base_t), intent(inout) :: lhs
        integer, intent(in) :: rhs
        lhs%i = rhs
    end subroutine

    integer function add_i(a, b)
        class(base_t), intent(in) :: a
        integer, intent(in) :: b
        add_i = a%i + b
    end function

    subroutine set_r(lhs, rhs)
        class(child_t), intent(inout) :: lhs
        real, intent(in) :: rhs
        lhs%j = int(rhs)
    end subroutine

    integer function add_r(a, b)
        class(child_t), intent(in) :: a
        real, intent(in) :: b
        add_r = a%j + int(b)
    end function

end module

program type_bound_generic_extend_01
    use type_bound_generic_extend_01_mod
    implicit none
    type(child_t) :: c

    c = 1
    c = 2.0
    if (c%i /= 1 .or. c%j /= 2) error stop
    call c%put(3)
    call c%put(4.0)
    if (c%i /= 3 .or. c%j /= 4) error stop
    if (c + 10 /= 13) error stop
    if (c + 20.0 /= 24) error stop
    print *, c%i, c%j
end program
