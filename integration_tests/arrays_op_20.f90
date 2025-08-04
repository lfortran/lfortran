program arrays_op_20
implicit none

real, allocatable :: array(:, :), arrayoutput(:, :)

allocate(array(5, 5))
arrayoutput = f(5, array)
print *, size(arrayoutput)
if( size(arrayoutput) /= 24 ) error stop

contains

function f(m, input) result(output)
integer :: m
real :: input(m, m)
real :: output(2:m, m:2*m)
end function

end program
