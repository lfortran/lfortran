! Test: sum() of binary operation on array sections inside do concurrent.
! Ensures sum(a(1:n) + b(1:n)) is properly inlined for GPU offloading,
! with correct loop bounds and element-wise access.
program gpu_metal_111
  implicit none
  integer :: i
  real :: a(3), b(3), results(2)
  integer :: c(4), d(4), ires(2)
  a = [1.0, 2.0, 3.0]
  b = [10.0, 20.0, 30.0]
  do concurrent (i = 1:2)
    results(i) = sum(a(1:3) + b(1:3))
  end do
  if (abs(results(1) - 66.0) > 1e-6) error stop
  if (abs(results(2) - 66.0) > 1e-6) error stop
  c = [1, 2, 3, 4]
  d = [5, 6, 7, 8]
  do concurrent (i = 1:2)
    ires(i) = sum(c(1:4) + d(1:4))
  end do
  if (ires(1) /= 36) error stop
  if (ires(2) /= 36) error stop
  print *, "PASS"
end program
