program separate_compilation_13
use ieee_arithmetic, only: ieee_quiet_nan
use separate_compilation_13a_module
real :: x
call check(x)
print *, x
if (abs(x - 151.15981) > 1e-8) error stop 
end program
