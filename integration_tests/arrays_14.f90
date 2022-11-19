program array_14

integer :: a = 10
integer :: b(2)
b = [14, 12]

call foo()

if( b(1) /= 243 ) error stop
if( b(2) /= 930 ) error stop

contains

subroutine foo()
if( a /= 10 ) error stop
if( b(1) /= 14 ) error stop
if( b(2) /= 12 ) error stop
b(1) = 243
b(2) = 930
end subroutine

end program
