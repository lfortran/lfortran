program reserved_names_in_kernel
  implicit none
  real, parameter :: half = 0.5
  real :: x(4), float(4)
  integer :: i, int, device
  int = 3
  device = 2
  x = [1., 2., 3., 4.]
  do concurrent (i = 1:4)
    float(i) = x(i) * half + int * device
  end do
  print *, float
  if (any(float /= [6.5, 7., 7.5, 8.])) error stop
end program
