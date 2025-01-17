program test_ieee_arithmetic_02
  use , intrinsic:: ieee_arithmetic
  integer , parameter:: dp = kind(1d0)
  real:: expected = 1/3.0
  if (abs(1/3.0_dp - expected) > 1e-6) error stop
end program test_ieee_arithmetic_02
