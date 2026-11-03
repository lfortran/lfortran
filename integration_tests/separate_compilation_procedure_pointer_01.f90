program separate_compilation_procedure_pointer_01
use separate_compilation_procedure_pointer_01a_mod, only: holder, set, call_f, call_s
implicit none
type(holder) :: o
call set(o)
if (call_f(o, 5) /= 6) error stop 1
if (call_s(o, 5) /= 10) error stop 2
print *, call_f(o, 1), call_s(o, 3)
end program
