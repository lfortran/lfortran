program elemental_01
  real :: x(4)
  print *, arg([(0.0, 1.0), (1.0, 0.0), (0.0, -1.0), (-1.0, 0.0)])
  x = arg([(0.0, 1.0), (1.0, 0.0), (0.0, -1.0), (-1.0, 0.0)])
  if (abs(x(1) - 1.57079637) > 1e-6) error stop
  if (abs(x(2) - 0.0) > 1e-6) error stop
  if (abs(x(3) + 1.57079637) > 1e-6) error stop
  if (abs(x(4) - 3.14159274) > 1e-6) error stop

contains
  elemental function arg(z) result(result) 
  complex, intent(in) :: z
  real :: result
  result = atan2(aimag(z), real(z))
  end function arg
end program elemental_01
