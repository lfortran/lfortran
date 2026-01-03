program intrinsics_75
implicit none

real(8) :: lambda = 1.123
real(8) :: x(10)
integer :: i_max = 2
x = 23.
print *, exp(-lambda * (x(i_max:i_max+4)-x(1)))
if( any(exp(-lambda * (x(i_max:i_max+4)-x(1))) /= 1.0) ) error stop

end program
