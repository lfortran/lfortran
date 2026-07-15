program read_98
  implicit none

  integer :: value
  character(len=4), allocatable :: lines(:)

  allocate(lines(1))
  lines = [" 42 "]
  read(lines, *) value

  if (value /= 42) error stop
end program read_98
