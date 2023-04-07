program main
implicit none
call find_fit(expression)

contains
real function expression(x) result(y)
    real, intent(in) :: x(3)
    y = x(1) + x(2) + x(3)
end function expression

subroutine find_fit(expr)
real :: x(3)
real :: y

interface
    real function expr(x) result(y)
        real, intent(in) :: x(3)
    end function expr
end interface

x(1) = 1.0
x(2) = 2.0
x(3) = 3.0

y = expr(x)
if (abs(y - 6.0) > 1e-16) error stop
print *, y
end subroutine find_fit

end program main
