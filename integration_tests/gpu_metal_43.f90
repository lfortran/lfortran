! Test: do concurrent inside associate block that also contains a
! whole-array assignment through the associate alias before the loop.
! The gpu_offload pass must not confuse `v = 0.` (regular assignment)
! with an associate binding, which would cause infinite recursion.
program gpu_metal_43
implicit none
real :: x(3)
integer :: l

associate(v => x)
  v = 0.
  do concurrent(l = 1:3)
    v(l) = real(l)
  end do
end associate

if (x(1) /= 1.0) error stop
if (x(2) /= 2.0) error stop
if (x(3) /= 3.0) error stop
print *, "PASSED"
end program
