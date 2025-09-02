program parameter_16
  implicit real (a-h,o-z)
  parameter (x = 42)
  real :: r
  r = x
  if (abs(r - 42.0) > 1e-8) error stop
end program parameter_16
