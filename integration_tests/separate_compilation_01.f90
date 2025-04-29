program separate_compilation_01
use separate_compilation_01_module
implicit none
real :: x = 9124.19

call set_val(x)
call get_val(x)
call test_val(x)
print *, "val = ", val
print *, "x = ", x
if (abs(val - x) > 1e-8) error stop
end program separate_compilation_01
