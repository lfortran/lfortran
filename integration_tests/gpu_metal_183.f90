program gpu_metal_183
  implicit none
  real :: x(2,1)
  logical :: mask(2,1)
  integer :: i
  x = reshape([1.0, 3.0], [2,1])
  do concurrent(i=1:1)
    mask(:,i) = x(:,i) < 2.5
  end do
  print *, mask(1,1), mask(2,1)
  if (.not. mask(1,1)) error stop
  if (mask(2,1)) error stop
end program
