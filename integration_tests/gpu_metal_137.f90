! Test: associate with function result inside do concurrent (GPU offloading)
! Verifies that array section pointers to function call results use
! thread-space addressing in Metal shaders, not device-space.
program gpu_metal_137
  implicit none
  real :: x(4)
  integer :: i

  x = 0.0

  do concurrent (i = 1:4)
    associate(y => get_values())
      x(i) = y(i)
    end associate
  end do

  if (abs(x(1) - 10.0) > 1e-6) error stop
  if (abs(x(2) - 20.0) > 1e-6) error stop
  if (abs(x(3) - 30.0) > 1e-6) error stop
  if (abs(x(4) - 40.0) > 1e-6) error stop
  print *, "PASS"

contains
  pure function get_values() result(r)
    real :: r(4)
    r = [10.0, 20.0, 30.0, 40.0]
  end function
end program
