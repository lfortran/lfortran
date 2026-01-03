program arrays_op_23
implicit none

real :: array(10, 10), output(10, 10)
array = 3.0

output = attention(10, 10, 10, array)
print *, output
if( any(abs(output - 8.52982235) > 1e-6) ) error stop

contains

function attention(l, n, m, mask) result(y)
integer, intent(in) :: l, n, m
real, intent(in) :: mask(n, m)
real :: y(l, m)
real :: tmp(n, m)
tmp = 4.0
call copy_array(twice(tmp / sqrt(real(l, 4)) + mask), y)
end function

function twice(x) result(y)
real, intent(in) :: x(:, :)
real :: y(size(x, 1), size(x, 2))
y = 2*x
end function

subroutine copy_array(src, dest)
real, intent(in) :: src(:, :)
real, intent(out) :: dest(:, :)
dest = src
end subroutine

end program
