! Test: sum() intrinsic inside do concurrent with GPU offloading.
! Ensures the sum() call is inlined for the Metal backend.
program gpu_metal_93
  implicit none
  real :: a(3), results(2)
  integer :: b(4), ires(3)
  integer :: i
  a = [1.0, 2.0, 3.0]
  b = [10, 20, 30, 40]
  do concurrent (i = 1:2)
    results(i) = sum(a)
  end do
  if (abs(results(1) - 6.0) > 1e-6) error stop
  if (abs(results(2) - 6.0) > 1e-6) error stop
  do concurrent (i = 1:3)
    ires(i) = sum(b)
  end do
  if (ires(1) /= 100) error stop
  if (ires(2) /= 100) error stop
  if (ires(3) /= 100) error stop
  print *, "PASS"
end program
