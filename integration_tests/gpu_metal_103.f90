program gpu_metal_103
  implicit none
  type :: t
    real :: v(2)
  end type
  integer :: i
  type(t) :: arr(2)
  real :: res(2, 2)
  arr(1)%v = [1.0, 2.0]
  arr(2)%v = [3.0, 4.0]
  do concurrent (i = 1:2)
    res(:,i) = get_v(arr(i))
  end do
  if (abs(res(1,1) - 1.0) > 1.0e-6) error stop
  if (abs(res(2,1) - 2.0) > 1.0e-6) error stop
  if (abs(res(1,2) - 3.0) > 1.0e-6) error stop
  if (abs(res(2,2) - 4.0) > 1.0e-6) error stop
  print *, "PASS"
contains
  pure function get_v(self) result(r)
    type(t), intent(in) :: self
    real :: r(2)
    r = self%v
  end function
end program
