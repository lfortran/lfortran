program separate_compilation_69
use separate_compilation_69a
implicit none
type(child_t) :: c
if (c%value() /= 42) error stop
print *, "ok"
end program
