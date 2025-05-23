program functions_39

integer :: y = 30
print *, f(y)
print *, y
if( y /= 60 ) error stop
if( f(y) /= 360 ) error stop

contains

integer function f(x) result(r)
integer, intent(out) :: x

x = 2 * x
r = 3 * x

end function

end program
