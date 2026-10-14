program separate_compilation_63
use separate_compilation_63a
implicit none
type(pf_t) :: v
integer :: y
v = t
y = v%f(7)
print *, y
if (y /= 14) error stop
end program
