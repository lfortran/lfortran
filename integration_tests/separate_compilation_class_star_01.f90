program separate_compilation_class_star_01
use separate_compilation_class_star_01_mod, only: get_value
implicit none
integer :: x

call get_value(x)
if (x /= 42) error stop

end program separate_compilation_class_star_01
