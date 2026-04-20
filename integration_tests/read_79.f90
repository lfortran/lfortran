program read_79
  logical :: x
  integer :: stat
  character(len=64) :: stored_data

  stored_data = ' F'

  stat = 0
  read(stored_data(2:2), *, iostat=stat) x

  if (stat /= 0) error stop
  
end program read_79
