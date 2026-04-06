program gpu_metal_116
  implicit none
  integer :: i
  real :: z(4), a(4)
  integer :: ix(3), iy(3)
  z = 1.0
  ix = 2
  do concurrent (i = 1:4)
    a(i) = -z(i)
  end do
  if (any(abs(a - (-1.0)) > 1e-6)) error stop
  do concurrent (i = 1:3)
    iy(i) = -ix(i)
  end do
  if (any(iy /= -2)) error stop
  print *, "PASS"
end program
