program separate_compilation_60
use separate_compilation_60a, only: s
use separate_compilation_60b, only: sb
implicit none
real :: x(2)
call s(x)
print *, x
if (any(x /= 6.0)) error stop
call sb()
end program
