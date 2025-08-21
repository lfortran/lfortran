module pass_array_by_data_05_find_fit_module

implicit none

contains

subroutine find_fit(data_x, expr)
real, intent(in) :: data_x(:)
real :: y
interface
    function expr(x) result(z)
        real, intent(in) :: x(:)
        real :: z
    end function expr
end interface
print *, size(data_x)
y = expr(data_x)
print *, y
if (abs(y - 6.000) > 1e7 ) error stop

end subroutine find_fit

end module

program example_primes

use pass_array_by_data_05_find_fit_module, only: find_fit

implicit none

real :: data_x(3)
integer :: i
data_x = [1.0, 2.0, 3.0]
call find_fit(data_x, expression)

contains

function expression(x) result(y)
real, intent(in) :: x(:)
real :: y
integer :: i
y = 0.0
do i = 1, size(x)
    y = y + x(i)
end do
end function expression

end program example_primes
