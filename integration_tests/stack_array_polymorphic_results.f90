module stack_array_polymorphic_results
    implicit none
    type :: activation
        real :: shift
    contains
        procedure :: evaluate
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

    pure function combine(first, second, x) result(y)
        class(activation), intent(in) :: first, second
        real, intent(in) :: x(:)
        real :: y(size(x))
        y = first%evaluate(x) + second%evaluate(x)
    end function
end module

program check_stack_array_polymorphic_results
    use stack_array_polymorphic_results
    implicit none
    type(activation) :: first, second
    type(scaled_activation) :: scaled
    real :: x(5), y(5)
    integer :: n, i
    first%shift = 0.125
    second%shift = 0.25
    scaled%shift = -0.5
    do i = 1, 3
        x = real(i) * [0.125, 0.25, 0.5, 0.75, 1.0]
        y = combine(first, second, x)
        if (any(abs(y - (2.0*x + 0.375)) > 1.e-6)) error stop 4
        y = combine(first, scaled, x)
        if (any(abs(y - (3.0*x - 0.375)) > 1.e-6)) error stop 5
        y = combine(scaled, second, x)
        if (any(abs(y - (3.0*x - 0.25)) > 1.e-6)) error stop 6
        do n = 0, size(x)
            y(1:n) = combine(first, second, x(1:n))
            if (any(abs(y(1:n) - (2.0*x(1:n) + 0.375)) > 1.e-6)) error stop 1
            y(1:n) = combine(first, scaled, x(1:n))
            if (any(abs(y(1:n) - (3.0*x(1:n) - 0.375)) > 1.e-6)) error stop 2
            y(1:n) = combine(scaled, second, x(1:n))
            if (any(abs(y(1:n) - (3.0*x(1:n) - 0.25)) > 1.e-6)) error stop 3
        end do
    end do
end program
