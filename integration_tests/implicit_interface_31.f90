real function myfun(x)
real :: x
myfun = x * 2.0
end function

subroutine extfun(fun, x1, res)
real x1, res
real g8, fun, x, h
g8(x, h) = fun(x - x1 * h)
res = g8(10.0, 1.0)
end subroutine

program implicit_interface_31
implicit none
real :: res
real, external :: myfun
call extfun(myfun, 2.0, res)
if (abs(res - 16.0) > 1e-6) error stop
end program
