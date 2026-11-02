module pure_array_repeated_calls
    implicit none
    type :: activation
        real :: shift = 0.0
    contains
        procedure :: evaluate
        procedure :: derivative
    end type
    type, extends(activation) :: scaled_activation
    contains
        procedure :: evaluate => evaluate_scaled
    end type
contains
    pure function evaluate(self, x) result(y)
        class(activation), intent(in) :: self
        real, intent(in) :: x(:)
        real :: y(size(x))
        y = x + self%shift
    end function

    pure function evaluate_scaled(self, x) result(y)
        class(scaled_activation), intent(in) :: self
        real, intent(in) :: x(:)
        real :: y(size(x))
        y = 2.0 * x + self%shift
    end function

    pure function derivative(self, x) result(y)
        class(activation), intent(in) :: self
        real, intent(in) :: x(:)
        real :: y(size(x))
        y = self%evaluate(x) * (1.0 - self%evaluate(x))
    end function

    pure function combine(first, second, x) result(y)
        class(activation), intent(in) :: first, second
        real, intent(in) :: x(:)
        real :: y(size(x))
        y = first%evaluate(x) + second%evaluate(x)
    end function
end module

program check_repeated_calls
    use pure_array_repeated_calls
    implicit none
    type(activation) :: first, second
    type(scaled_activation) :: scaled
    real :: x(5), y(5), z(5), expected(5), empty(0)
    integer :: i
    first%shift = 0.125
    second%shift = 0.25
    scaled%shift = -0.5
    do i = 1, 4
        x = real(i) * [0.125, 0.25, 0.5, 0.75, 1.0]
        z = x + first%shift
        expected = z * (1.0 - z)
        y = first%derivative(x)
        if (any(abs(y - expected) > 1.e-6)) error stop 1
        z = 2.0 * x + scaled%shift
        y = scaled%derivative(x)
        if (any(abs(y - z * (1.0 - z)) > 1.e-6)) error stop 2
        y = combine(first, second, x)
        if (any(abs(y - (2.0 * x + 0.375)) > 1.e-6)) error stop 3
    end do
    empty = first%derivative(empty)
    if (size(empty) /= 0) error stop 4
end program
