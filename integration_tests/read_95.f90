program read_95
  implicit none

  integer :: a(2), b(2), i, n
  character(len=100) :: line

  n = 2
  ! simulate input stream
  line = "1 10 2 20"

  read(line, *) (a(i), b(i), i = 1, n)

  if (a(1) /= 1 .or. b(1) /= 10 .or. a(2) /= 2 .or. b(2) /= 20) error stop
  print *, "Test passed!"
end program read_95