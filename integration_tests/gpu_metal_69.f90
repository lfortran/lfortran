program gpu_metal_69
  ! Test: all() with array binary operation inside do concurrent
  ! Ensures the GPU offload pass inlines all() when the mask
  ! contains element-wise arithmetic (e.g. a - b < tol).
  implicit none
  integer :: n(3)
  real :: a(3,3), b(3,3)
  logical :: eq(3)
  integer :: l

  n = [2, 3, 1]
  a = 1.0
  b = 1.0

  do concurrent(l = 1:3)
    eq(l) = all(a(1:n(l),l) - b(1:n(l),l) < 1.0e-06)
  end do

  if (.not. all(eq)) error stop
  print *, "PASS"
end program
