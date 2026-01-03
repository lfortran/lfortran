program arrays_op_22
implicit none

real :: array(10, 10), output(10, 10)
array = 3.0

output = prg(array)
print *, output
if( any(abs(output - 6.0) > 1e-8) ) error stop

contains

function prg(arr) result(otpt)
real, intent(in) :: arr(:, :)
real :: otpt(size(arr, 1), size(arr, 2))
otpt = copy_array(twice(copy_array(arr)))
end function

elemental real function twice(x) result(y)
real, intent(in) :: x
y = 2.0*x
end function

function copy_array(input) result(output)
real, intent(in) :: input(:, :)
real :: output(size(input, 1), size(input, 2))
output = input
end function

end program
