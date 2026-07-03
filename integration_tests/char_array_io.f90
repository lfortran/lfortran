program test_char_array_io
  implicit none
  character(len=1), allocatable :: s(:)
  integer :: x

  allocate(s(1))
  s = ["1"]

  read(s, *) x

  if (x /= 1) error stop "read failed for allocatable character array"
  
  print *, "ok"
end program test_char_array_io
