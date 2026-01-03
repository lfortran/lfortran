program arrays_op_20
implicit none

real, allocatable :: array(:, :), arrayoutput(:, :)
integer :: i, j

allocate(array(2:5, 5:10))
allocate(arrayoutput(2:5, 5:10))
array = 3.0

arrayoutput = f(5, array)
print *, size(arrayoutput)
print *, arrayoutput
if( size(arrayoutput) /= 24 ) error stop
if( any(arrayoutput /= 3.0) ) error stop

contains

function f(m, input) result(output)
integer :: m
real :: input(2:m, m:2*m)
real :: output(2:m, m:2*m)
output = input
end function

end program
