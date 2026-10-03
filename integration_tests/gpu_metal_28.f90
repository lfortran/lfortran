program gpu_metal_28
! Test parameter variables with dependencies in do concurrent
implicit none
integer :: l
real :: x(4)
real, parameter :: a = 0.9
real, parameter :: b = 1.0 - a

do concurrent(l = 1:4)
  x(l) = b
end do

if (any(abs(x - 0.1) > 1e-5)) error stop

print *, "PASS"
end program
