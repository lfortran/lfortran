module operator_overloading_48_m
    implicit none
    integer, target :: operator_overloading_48_tgt(3) = [1, 2, 3]
    type :: operator_overloading_48_t
        integer :: v
    end type
    interface operator(+)
        module procedure operator_overloading_48_add
    end interface
    interface operator(-)
        module procedure operator_overloading_48_neg
    end interface
    interface operator(.and.)
        module procedure operator_overloading_48_and
    end interface
    interface operator(==)
        module procedure operator_overloading_48_eq
    end interface
    interface operator(//)
        module procedure operator_overloading_48_cat
    end interface
contains
    function operator_overloading_48_add(a, b) result(r)
        type(operator_overloading_48_t), intent(in) :: a, b
        integer :: r(a%v:b%v + 3)
        r = a%v + b%v
    end function
    function operator_overloading_48_neg(a) result(r)
        type(operator_overloading_48_t), intent(in) :: a
        integer, pointer :: r(:)
        r => operator_overloading_48_tgt(:a%v - 1)
    end function
    function operator_overloading_48_and(a, b) result(r)
        type(operator_overloading_48_t), intent(in) :: a, b
        integer, allocatable :: r(:)
        allocate(r(0:2))
        r = a%v * b%v
    end function
    function operator_overloading_48_eq(a, b) result(r)
        type(operator_overloading_48_t), intent(in) :: a, b
        logical, allocatable :: r(:)
        allocate(r(0:3))
        r = a%v == b%v
    end function
    function operator_overloading_48_cat(a, b) result(r)
        type(operator_overloading_48_t), intent(in) :: a, b
        integer :: r(b%v:a%v)
        r = a%v - b%v
    end function
end module

program operator_overloading_48
    ! Defined operations whose array result has a size known only at run time.
    use operator_overloading_48_m
    implicit none
    type(operator_overloading_48_t) :: x, y
    x%v = 4
    y%v = 2
    print *, x + y
    if (size(x + y) /= 2) error stop 1
    if (any(x + y /= 6)) error stop 2
    print *, -x
    if (any(-x /= [1, 2, 3])) error stop 3
    print *, lbound(x .and. y), ubound(x .and. y)
    if (any(lbound(x .and. y) /= [1])) error stop 4
    if (any(ubound(x .and. y) /= [3])) error stop 5
    if (size(x == y) /= 4) error stop 6
    if (size(x // y) /= 3) error stop 7
end program
