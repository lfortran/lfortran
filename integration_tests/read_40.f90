program read_40
  implicit none
  real :: x
  integer :: io
  character(3) :: data = 'bad'
  do
     read(data, *, iostat=io) x
     if (io /= 0) exit
     print *, 'Loop should have exited!'
     exit
  end do
  if (io == 0) error stop "Test failed: iostat should be non-zero for bad input"
  print *, 'Program ending.'
end program read_40
