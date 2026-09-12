! Test: a device function whose allocatable result is written by an array
! expression, called from a do concurrent loop.
program gpu_metal_200
  implicit none
  real :: v(3), res(3, 2)
  integer :: i
  v = [1.0, 2.0, 3.0]
  res = 0.0
  do concurrent (i = 1:2)
    res(:, i) = scaled(v, real(i))
  end do
  if (abs(res(3,1) - 3.0) > 1e-5) error stop
  if (abs(res(1,2) - 2.0) > 1e-5) error stop
  if (abs(res(3,2) - 6.0) > 1e-5) error stop
  print *, "ok"
contains
  pure function scaled(a, s) result(w)
    real, intent(in) :: a(:)
    real, intent(in) :: s
    real, allocatable :: w(:)
    allocate(w(size(a)))
    w = a * s
  end function
end program
