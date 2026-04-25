program gpu_metal_99
  implicit none
  real :: x(2, 2), res(2)
  integer :: i
  x = reshape([1.0, 2.0, 3.0, 4.0], [2, 2])
  do concurrent (i = 1:2)
    res(i) = sum(x(:, i))
  end do
  if (abs(res(1) - 3.0) > 1e-6) error stop
  if (abs(res(2) - 7.0) > 1e-6) error stop
  print *, "ALL PASSED"
end program
