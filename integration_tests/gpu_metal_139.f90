program gpu_metal_139
  ! Test: allocatable array used in binary operation inside do concurrent
  implicit none
  real, allocatable :: x(:)
  real :: y(4)
  integer :: i
  allocate(x(4))
  x = [1.0, 2.0, 3.0, 4.0]
  do concurrent (i = 1:4)
    y = x * 2.0
  end do
  print *, y
  if (any(abs(y - [2.0, 4.0, 6.0, 8.0]) > 1.0e-6)) error stop
end program
