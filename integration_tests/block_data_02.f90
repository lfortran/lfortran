program block_data_02
  implicit none
  integer :: x(4)
  common /blk1/ x

  ! Verify element-wise DATA initialization in BLOCK DATA
  if (x(1) /= 0) error stop
  if (x(2) /= 8) error stop
  if (x(3) /= 0) error stop
  if (x(4) /= 42) error stop
  print *, x(1), x(2), x(3), x(4)
end program

block data
  implicit none
  integer :: x(4)
  common /blk1/ x
  data x(2) /8/
  data x(4) /42/
end block data
