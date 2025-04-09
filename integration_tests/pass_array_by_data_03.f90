program main
implicit none
integer :: x1(20)
call find_fit(expression)
call ub_proc(x1)

contains
real function expression(x) result(y)
    real, intent(in) :: x(3)
    y = x(1) + x(2) + x(3)
end function expression

subroutine ub_proc (x)
    integer, intent(in) :: x(0:)
    integer :: temp (0:ubound (x, 1))

    if (lbound(x,1) /= 0 .or. ubound(x,1) /= 19 ) error stop 
    if (lbound(temp,1) /= 0 .or. ubound(temp, 1) /= 19) error stop 
end subroutine

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
if (abs(y - 6.0) > 1e-7) error stop
print *, y
end subroutine find_fit

end program main
