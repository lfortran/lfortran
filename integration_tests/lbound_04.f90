module lbound_04_m
    implicit none
    type :: lbound_04_t
        integer :: v
    end type
    interface operator(+)
        module procedure lbound_04_add, lbound_04_pos
    end interface
    interface operator(==)
        module procedure lbound_04_eq
    end interface
    interface operator(.and.)
        module procedure lbound_04_and
    end interface
contains
    function lbound_04_add(a, b) result(r)
        type(lbound_04_t), intent(in) :: a, b
        integer :: r(0:2)
        r = a%v + b%v
    end function
    function lbound_04_pos(a) result(r)
        type(lbound_04_t), intent(in) :: a
        integer :: r(-3:-1)
        r = a%v
    end function
    function lbound_04_eq(a, b) result(r)
        type(lbound_04_t), intent(in) :: a, b
        logical :: r(5:7)
        r = a%v == b%v
    end function
    function lbound_04_and(a, b) result(r)
        type(lbound_04_t), intent(in) :: a, b
        integer :: r(2:3, -1:1)
        r = a%v * b%v
    end function
end module

program lbound_04
    ! An operator expression is not a whole array, so its lower bounds are 1.
    use lbound_04_m
    implicit none
    type(lbound_04_t) :: x, y
    x%v = 1
    y%v = 2
    if (any(lbound(x + y) /= [1])) error stop 1
    if (any(ubound(x + y) /= [3])) error stop 2
    if (sum(x + y) /= 9) error stop 3
    if (any(lbound(+x) /= [1])) error stop 4
    if (any(ubound(+x) /= [3])) error stop 5
    if (any(lbound(x == y) /= [1])) error stop 6
    if (any(ubound(x == y) /= [3])) error stop 7
    if (any(lbound(x .and. y) /= [1, 1])) error stop 8
    if (any(ubound(x .and. y) /= [2, 3])) error stop 9
    print *, "PASS"
end program
