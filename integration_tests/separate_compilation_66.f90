program separate_compilation_66
use separate_compilation_66a
implicit none
if (call_local(1) /= 2) error stop
if (call_saved(41) /= 42) error stop
print *, "ok"
end program
