function func() result(y)
real :: y(1)
y(1) = 1.0
end function func

program main
real :: x(1)
interface 
function func() result(y)
real :: y(1)
end function func
end interface
x = func()
if (abs(x(1) - 1.0) > 1e-6)  error stop
print *, x
end program main
