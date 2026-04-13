program gpu_metal_172
  implicit none
  real :: x(2,2), res(2,2)
  integer :: i

  x(:,1) = [1.0, 2.0]
  x(:,2) = [3.0, 4.0]

  do concurrent (i = 1:2)
    res(:,i) = copy(x(:,i))
  end do

  if (abs(res(1,1) - 1.0) > 1e-6) error stop
  if (abs(res(2,1) - 2.0) > 1e-6) error stop
  if (abs(res(1,2) - 3.0) > 1e-6) error stop
  if (abs(res(2,2) - 4.0) > 1e-6) error stop
  print *, "PASS"

contains
  pure function copy(a) result(b)
    real, intent(in) :: a(:)
    real, allocatable :: b(:)
    allocate(b(size(a)))
    b = a
  end function
end program
