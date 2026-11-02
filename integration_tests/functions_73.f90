module functions_73_m
    implicit none
    type :: operation
    contains
        procedure :: real_values
        procedure :: integer_values
        procedure :: complex_values
        generic :: evaluate => real_values, integer_values, complex_values
    end type
contains
    pure function real_values(self, x) result(res)
        class(operation), intent(in) :: self
        real, intent(in) :: x(0:)
        real :: res(size(x))
        res = 2*x(size(x)-1:0:-1)
    end function

    pure function integer_values(self, x) result(res)
        class(operation), intent(in) :: self
        integer, intent(in) :: x(:)
        integer :: res(size(x))
        res = 2*x(size(x):1:-1)
    end function

    pure function complex_values(self, x) result(res)
        class(operation), intent(in) :: self
        complex, intent(in) :: x(:)
        complex :: res(size(x))
        res = 2*x(size(x):1:-1)
    end function
end module

program functions_73
    use functions_73_m
    implicit none
    type(operation) :: object
    real :: x(-3:4), actual(8), expected(8), section(4), empty(0)
    integer :: i, integers(4), actual_integers(4)
    complex :: complexes(4), actual_complexes(4)

    do i = -3, 4
        x(i) = real(i)/16
    end do
    expected = 2*x(4:-3:-1)*(1 - 2*x(4:-3:-1))
    actual = object%evaluate(x)*(1 - object%evaluate(x))
    if (any(actual /= expected)) error stop 1

    expected = 2*x*(1 - 2*x)
    actual = object%evaluate(x(4:-3:-1))*(1 - object%evaluate(x(4:-3:-1)))
    if (any(actual /= expected)) error stop 2

    section = object%evaluate(x(-3:3:2)) + object%evaluate(x(-3:3:2))
    if (any(section /= 4*x(3:-3:-2))) error stop 3

    expected = 2*actual(8:1:-1)
    actual = object%evaluate(actual)
    if (any(actual /= expected)) error stop 4

    empty = object%evaluate(x(1:0)) + object%evaluate(x(1:0))
    if (size(empty) /= 0) error stop 5

    integers = [1, -2, 3, -4]
    actual_integers = object%evaluate(integers) + object%evaluate(integers)
    if (any(actual_integers /= 4*integers(4:1:-1))) error stop 6

    complexes = [(1., 2.), (-2., 1.), (3., -2.), (-4., -1.)]
    actual_complexes = object%evaluate(complexes) + object%evaluate(complexes)
    if (any(actual_complexes /= 4*complexes(4:1:-1))) error stop 7
end program
