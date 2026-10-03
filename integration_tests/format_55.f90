program format_55
  implicit none
  real :: pi
  pi = 4*atan(1.0)
  print "(A,F9.6)", 'pi =', pi
  if (abs(pi - 3.141593) > 1e-5) error stop "incorrect value"
end program format_55