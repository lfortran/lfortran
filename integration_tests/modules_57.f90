module module_modules_57
complex :: c = (3.0, 4.0)
end module module_modules_57

program modules_57
use module_modules_57
print *, abs(c)
if (abs(abs(c) - 5.0) > 1e-8) error stop
end program modules_57
