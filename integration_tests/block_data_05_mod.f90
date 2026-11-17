module block_data_05_m
  implicit none
  integer :: x = 1
end module block_data_05_m

block data block_data_05_init
  implicit none
  integer :: y
  common /block_data_05_c/ y
  data y /2/
end block data block_data_05_init
