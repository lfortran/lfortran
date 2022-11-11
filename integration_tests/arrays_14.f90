program array_14

integer :: a = 10
integer :: b(2)
b = [14, 12]

call foo()

contains

subroutine foo()
if( a /= 10 ) error stop
if( b(1) /= 14 ) error stop
if( b(2) /= 12 ) error stop
end subroutine

end program
