program read_98
  implicit none

  integer :: value, a, b
  character(len=4), allocatable :: lines(:)
  character(len=2) :: records(2)

  allocate(lines(1))
  lines = [" 42 "]
  read(lines, *) value

  if (value /= 42) error stop

  records = ["42", "43"]
  read(records, *) a, b

  if (a /= 42 .or. b /= 43) error stop
end program read_98
