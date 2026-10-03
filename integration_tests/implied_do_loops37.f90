program implied_do_loops37
  implicit none
  character(len=256) :: buffer
  real, allocatable :: data_list(:)
  integer :: j, stat, c, k
  allocate(data_list(10), source=0.0)
  buffer = "1.0 2.0 3.0 4.0 5.0"
  c = 1
  k = 5
  read(buffer, *, iostat=stat) (data_list(j), j=c, c+k-1)
  if (stat /= 0) error stop
  if (abs(data_list(1) - 1.0) > 1e-5) error stop
  if (abs(data_list(2) - 2.0) > 1e-5) error stop
  if (abs(data_list(3) - 3.0) > 1e-5) error stop
  if (abs(data_list(4) - 4.0) > 1e-5) error stop
  if (abs(data_list(5) - 5.0) > 1e-5) error stop
  print *, "PASSED"
end program implied_do_loops37
