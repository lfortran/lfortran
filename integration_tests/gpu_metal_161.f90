program gpu_metal_161
! Test: separate compilation with multiple files where the module file
! has no do concurrent and the main program has a GPU kernel.
! Verifies that the Metal shader source is regenerated per translation
! unit so the kernel name matches at runtime.
use gpu_metal_161_m
implicit none
real :: a(4)
integer :: i

a = 1.0
do concurrent (i = 1:4)
  a(i) = a(i) + 1.0
end do

print *, a(1)
if (abs(a(1) - 2.0) > 1e-6) error stop
if (abs(a(2) - 2.0) > 1e-6) error stop
if (abs(a(3) - 2.0) > 1e-6) error stop
if (abs(a(4) - 2.0) > 1e-6) error stop
end program
