program separate_compilation_22
use mod_separate_compilation_22, only: f_sc_22
implicit none
integer :: i
i = f_sc_22(5)
print *, i
if (i /= 6) error stop
end program