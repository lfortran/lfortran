module real16_module_var_01_m
implicit none
real(16) :: x
end module

program real16_module_var_01
use real16_module_var_01_m
implicit none
x = 3
print *, x
if (x /= 3) error stop
end program
