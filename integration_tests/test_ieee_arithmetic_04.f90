program test_nan
  use, intrinsic :: ieee_arithmetic, only: ieee_quiet_nan, ieee_value, ieee_is_nan
  implicit none
  real(kind=8) :: x

  x = ieee_value(x, ieee_quiet_nan)

  if (.not. ieee_is_nan(x)) error stop
  print *, "test passed"

end program test_nan