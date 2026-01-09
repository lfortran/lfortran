program ieee_value_test
  use, intrinsic :: ieee_arithmetic
  implicit none

  real(kind=8) :: x

  x = ieee_value(x, ieee_quiet_nan)
  print *, ieee_is_nan(x)

end program ieee_value_test
