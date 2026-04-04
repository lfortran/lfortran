program gpu_metal_70
  ! Test: elemental intrinsic (abs) on array slice inside do concurrent
  ! Ensures the GPU offload pass inlines elemental operations on
  ! ArraySection into element-wise loops before Metal codegen.
  implicit none
  real :: a(3, 2), b(3)
  integer :: l

  ! Both columns have the same absolute values so the result is
  ! deterministic regardless of iteration order (avoids data race).
  a(:,1) = [-1.0, 2.0, -3.0]
  a(:,2) = [1.0, -2.0, 3.0]

  do concurrent(l = 1:2)
    b = abs(a(:,l))
  end do

  ! Both iterations produce [1.0, 2.0, 3.0]
  if (abs(b(1) - 1.0) > 1.0e-6) error stop
  if (abs(b(2) - 2.0) > 1.0e-6) error stop
  if (abs(b(3) - 3.0) > 1.0e-6) error stop

  print *, "PASS"
end program
