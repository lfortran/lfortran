program gpu_metal_98
  implicit none
  integer, parameter :: n = 2, m = 2
  real :: v(m, n), w(m, m), r(m, n)
  integer :: i
  w = reshape([2.0, 0.0, 0.0, 2.0], [m, m])
  v = reshape([1.0, 2.0, 3.0, 4.0], [m, n])
  do concurrent (i = 1:n)
    r(:, i) = matmul(w, v(:, i))
  end do
  if (abs(r(1,1) - 2.0) > 1e-6) error stop
  if (abs(r(2,1) - 4.0) > 1e-6) error stop
  if (abs(r(1,2) - 6.0) > 1e-6) error stop
  if (abs(r(2,2) - 8.0) > 1e-6) error stop
  print *, "ALL PASSED"
end program
