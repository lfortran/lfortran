program separate_compilation_70
use separate_compilation_70a
implicit none
type(t) :: x
if (x%value() /= 7) error stop
print *, "ok"
end program
