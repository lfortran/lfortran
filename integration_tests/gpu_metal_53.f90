! Test: do concurrent with array section inside associate block.
! The gpu_offload pass must place the __gpu_sec_i temp variable
! in the containing function scope, not in the AssociateBlock scope.
program gpu_metal_53
  implicit none
  real :: b(4)
  integer :: l
  b = 0.0
  associate(x => 2)
    do concurrent(l = 1:3)
      b(1:l) = 1.0
    end do
  end associate
  if (abs(b(1) - 1.0) > 1e-6) error stop
  if (abs(b(2) - 1.0) > 1e-6) error stop
  if (abs(b(3) - 1.0) > 1e-6) error stop
  print *, "ok"
end program
