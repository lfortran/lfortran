program gpu_metal_194
  ! Test: all() on a 2D array section inside do concurrent iterates
  ! over all elements, not just the diagonal.
  implicit none
  real :: a(2, 2), b(2, 2)
  logical :: res
  integer :: l

  a = 1.0
  b = 1.0
  a(1, 2) = 2.0

  do concurrent(l = 1:1)
    res = all(a(1:2, 1:2) == b(1:2, 1:2))
  end do

  if (res) error stop
  print *, "PASS"
end program
