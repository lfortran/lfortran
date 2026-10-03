program expr_21
  use ieee_arithmetic, only: ieee_is_nan
  implicit none
  real :: x = -8.0
  real :: res
  res = x**(1.0/3.0)
  if (.not. ieee_is_nan(res)) error stop "expected NaN for negative real ** non-integer"
  print "(F0.1)", res
end program expr_21
