program separate_compilation_63
use separate_compilation_63a
implicit none
integer :: y
y = t%f(7)
print *, y
if (y /= 14) error stop
end program
