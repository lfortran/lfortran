module attr_intrinsic_02_m
! A local variable or dummy argument of a module procedure hides an
! intrinsic procedure of the same name declared INTRINSIC in the module.
intrinsic sin, size
contains
real function local_array()
real :: sin(3)
sin = [1.0, 2.0, 3.0]
local_array = sin(2)
end function

integer function dummy_array(size)
integer, intent(in) :: size(3)
dummy_array = size(2)
end function

real function intrinsic_sin(x)
real, intent(in) :: x
intrinsic_sin = sin(x)
end function
end module

program attr_intrinsic_02
use attr_intrinsic_02_m
if (abs(local_array() - 2.0) > 1e-6) error stop 1
if (dummy_array([1, 5, 9]) /= 5) error stop 2
if (abs(intrinsic_sin(0.5) - 0.4794255) > 1e-6) error stop 3
print *, "ok"
end program
