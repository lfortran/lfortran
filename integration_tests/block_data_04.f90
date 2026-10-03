program block_data_04
  implicit none
  integer :: mx(4)
  common /blk1/ mx

  if (mx(1) /= 4) error stop
  if (mx(2) /= 4) error stop
  if (mx(3) /= 4) error stop
  if (mx(4) /= 4) error stop
  print *, mx(1), mx(2), mx(3), mx(4)
end program

block data init
  implicit none
  integer :: i, mx(4)
  common /blk1/ mx
  data (mx(i), i=1,4) /4*4/
end block data
