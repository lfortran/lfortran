program gpu_metal_108
  implicit none
  real :: a(2,3), b(3,2)
  integer :: i, j
  a(1,1) = 1.0; a(2,1) = 2.0
  a(1,2) = 3.0; a(2,2) = 4.0
  a(1,3) = 5.0; a(2,3) = 6.0
  do concurrent (i = 1:1)
    b = transpose(a)
  end do
  if (abs(b(1,1) - 1.0) > 1e-6) error stop
  if (abs(b(2,1) - 3.0) > 1e-6) error stop
  if (abs(b(3,1) - 5.0) > 1e-6) error stop
  if (abs(b(1,2) - 2.0) > 1e-6) error stop
  if (abs(b(2,2) - 4.0) > 1e-6) error stop
  if (abs(b(3,2) - 6.0) > 1e-6) error stop
  print *, "ok"
end program
