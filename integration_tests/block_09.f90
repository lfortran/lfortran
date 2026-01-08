block data init_data
  real :: xyzzy
  common xyzzy
  data xyzzy / 42.0 /
end block data init_data

program test_blank_common
  implicit none

  real :: xyzzy
  common xyzzy

  if (abs(xyzzy - 42.0) > 1.0e-8) error stop "wrong value"

  print *, "test passed"
end program test_blank_common
