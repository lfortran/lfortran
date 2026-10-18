program gpu_cuda_cpu_01
  use gpu_cuda_cpu_01_m, only: transform_t, shift_up
  implicit none
  type(transform_t) :: t
  real :: x(4), y(4)
  integer :: i
  x = [1.0, 2.0, 3.0, 4.0]
  call shift_up(x)
  do concurrent (i = 1:4)
    y(i) = t%apply(x(i))
  end do
  if (abs(y(1) - 4.0) > 1e-6) error stop 1
  if (abs(y(2) - 6.0) > 1e-6) error stop 2
  if (abs(y(3) - 8.0) > 1e-6) error stop 3
  if (abs(y(4) - 10.0) > 1e-6) error stop 4
  print *, "ok"
end program
