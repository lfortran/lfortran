program real_kinds_01
use iso_fortran_env
integer :: i
i = 1
print *, real_kinds(i)
if (real_kinds(i) /= 4) error stop
i = 2
print *, real_kinds(i)
if (real_kinds(i) /= 8) error stop
end program
