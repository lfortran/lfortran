program dpcheck
  use , intrinsic:: ieee_arithmetic
  integer , parameter:: dp = kind(1d0)
  real:: expected = 1/3.0
  if (abs(1/3.0_dp - expected) > 1e-6) error stop
end program dpcheck
