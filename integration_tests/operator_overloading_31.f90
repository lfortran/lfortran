module operator_overloading_31_m1
    implicit none
    private
    public :: t, operator(==)
    type :: t
        integer :: v
    end type
    interface operator(==)
        module procedure t_eq
    end interface
contains
    pure logical function t_eq(a, b)
        type(t), intent(in) :: a(:), b(:)
        t_eq = all(a%v == b%v)
    end function
end module

module operator_overloading_31_m2
    use operator_overloading_31_m1, only: t, operator(==)
    implicit none
    private
    public :: t, operator(==)
end module

module operator_overloading_31_m3
    use operator_overloading_31_m2
    implicit none
contains
    logical function compare(a, b)
        type(t), intent(in) :: a(:), b(:)
        compare = (a == b)
    end function
end module

program operator_overloading_31
    use operator_overloading_31_m1
    use operator_overloading_31_m3
    implicit none
    type(t) :: x(2), y(2), z(2)
    x = [t(1), t(2)]
    y = [t(1), t(2)]
    z = [t(3), t(4)]
    if (.not. compare(x, y)) error stop
    if (compare(x, z)) error stop
    print *, compare(x, y)
end program
