program block_data_05
  use block_data_05_m, only: x
  implicit none
  integer :: y
  common /block_data_05_c/ y
  if (x /= 1) error stop 1
  if (y /= 2) error stop 2
end program block_data_05
