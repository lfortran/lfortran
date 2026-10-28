! Intrinsic array-expression arguments are scalarized in the result loop.
module elemental_28_m
implicit none
integer :: scalar_calls = 0, array_calls = 0, element_calls = 0
contains
    subroutine e_exp(x, y)
        real, intent(in) :: x(:)
        real, intent(out) :: y(:)
        y = exp(-x)
    end subroutine

    subroutine e_max(x, y, alpha)
        real, intent(in) :: x(:), alpha
        real, intent(out) :: y(:)
        y = max(alpha*x, x)
    end subroutine

    subroutine e_sqrt(x, y, eps)
        real, intent(in) :: x(:), eps
        real, intent(out) :: y(:)
        y = sqrt(x + eps)
    end subroutine

    subroutine e_shift(x, y)
        real, intent(in) :: x(:)
        real, intent(out) :: y(:)
        y = exp(x - maxval(x))
    end subroutine

    subroutine e_nested(x, y)
        real, intent(in) :: x(:)
        real, intent(out) :: y(:)
        y = log(exp(x) + 1)
    end subroutine

    subroutine e_sin(x, y)
        real, intent(in) :: x(:)
        real, intent(out) :: y(:)
        y = exp(sin(x))
    end subroutine

    subroutine e_rank2(x, y)
        integer, intent(in) :: x(:,:)
        real, intent(out) :: y(:,:)
        y = sqrt(real(x) + 1.)
    end subroutine

    real function scalar_value()
        scalar_calls = scalar_calls + 1
        scalar_value = 1.
    end function

    function array_value() result(r)
        real :: r(4)
        array_calls = array_calls + 1
        r = [1., 2., 3., 4.]
    end function

    impure elemental real function element_value(x) result(r)
        real, intent(in) :: x
        element_calls = element_calls + 1
        r = x + 1.
    end function
end module

program elemental_28
use elemental_28_m
implicit none
real :: x(4), y(4), a(6), b(2,2)
real, allocatable :: empty(:), result(:)
integer :: i
x = [0., 1., 2., 3.]
call e_exp(x, y)
do i = 1, 4
    if (abs(y(i) - exp(-x(i))) > 1e-6) error stop 1
end do
call e_max(x - 1., y, 0.1)
if (any(abs(y - [-0.1, 0., 1., 2.]) > 1e-6)) error stop 2
call e_sqrt(x, y, 1.)
if (any(abs(y - [1., sqrt(2.), sqrt(3.), 2.]) > 1e-6)) error stop 3
call e_shift(x, y)
do i = 1, 4
    if (abs(y(i) - exp(x(i) - 3.)) > 1e-6) error stop 4
end do
call e_nested(x, y)
do i = 1, 4
    if (abs(y(i) - log(exp(x(i)) + 1.)) > 1e-6) error stop 5
end do
call e_sin(x, y)
do i = 1, 4
    if (abs(y(i) - exp(sin(x(i)))) > 1e-6) error stop 17
end do
y = sqrt(x + scalar_value())
if (scalar_calls /= 1) error stop 6
if (any(abs(y - [1., sqrt(2.), sqrt(3.), 2.]) > 1e-6)) error stop 7
y = sqrt(array_value() + 3.)
if (array_calls /= 1) error stop 8
if (any(abs(y - [2., sqrt(5.), sqrt(6.), sqrt(7.)]) > 1e-6)) error stop 9
y = sqrt(element_value(x) + 3.)
if (element_calls /= 4) error stop 10
if (any(abs(y - [2., sqrt(5.), sqrt(6.), sqrt(7.)]) > 1e-6)) error stop 11
a = [0., 1., 2., 3., 4., 5.]
a(2:6) = sqrt(a(1:5) + 1.)
if (any(abs(a - [0., 1., sqrt(2.), sqrt(3.), 2., sqrt(5.)]) > 1e-6)) error stop 12
call e_sqrt(x(4:1:-1), y, 1.)
if (any(abs(y - [2., sqrt(3.), sqrt(2.), 1.]) > 1e-6)) error stop 13
call e_rank2(reshape([0, 3, 8, 15], [2,2]), b)
if (any(abs(b - reshape([1., 2., 3., 4.], [2,2])) > 1e-6)) error stop 14
y = -1.
where (x > 0.)
    y = sqrt(x - 1.)
end where
if (any(abs(y - [-1., 0., 1., sqrt(2.)]) > 1e-6)) error stop 15
allocate(empty(0), result(0))
call e_exp(empty, result)
if (size(result) /= 0) error stop 16
end program
