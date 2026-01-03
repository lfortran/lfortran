program const_array_01
implicit none
real, parameter :: A(5) = [1.1, 3.0, 10.0, 2.1, 5.5]
integer, parameter :: x(*) = [4, 9, 6, 12, 13]
print *, A
if (x(1) /= 4) error stop
if (x(5) /= 13) error stop
end

