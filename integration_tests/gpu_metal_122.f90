program gpu_metal_122
  implicit none
  integer :: i
  real :: a(4)
  do concurrent (i = 1:4)
    a(i) = merge(1.0, 0.0, i > 2)
  end do
  print *, a
  if (abs(a(1) - 0.0) > 1e-6) error stop
  if (abs(a(2) - 0.0) > 1e-6) error stop
  if (abs(a(3) - 1.0) > 1e-6) error stop
  if (abs(a(4) - 1.0) > 1e-6) error stop
end program
