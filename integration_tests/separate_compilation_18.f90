program separate_compilation_18
use mod_separate_compilation_18, only: f
implicit none
integer :: i
i = f(5)
print *, i
if (i /= 6) error stop
end program