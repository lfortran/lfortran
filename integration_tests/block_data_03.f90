! Test: DATA statement in BLOCK DATA with all-identical values
! When all values in a DATA initializer for an array are the same,
! they must be correctly initialized (not zeroed out).
program block_data_03
  integer :: x(2)
  common /b/ x
  if (x(1) /= 4) error stop
  if (x(2) /= 4) error stop
  print *, x(1), x(2)
end program

block data
  integer :: x(2)
  common /b/ x
  data x /4, 4/
end
