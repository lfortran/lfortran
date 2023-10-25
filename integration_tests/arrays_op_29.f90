program arrays_op_29
implicit none

real(8) :: x(2) = (/ 0.5_8, 0.5_8 /)
print *, R3(x)
if( any(R3(x) /= -3.5) ) error stop

contains

function R3(x)
real(8), intent(in) :: x(:)
real(8) :: R3(size(x))

real(8) :: a, b
real(8), parameter :: c = 2.0
a = x(1)
b = x(2)
R3 = a**2.0 + b**2.0 - c**2.0
end function

end program arrays_op_29
