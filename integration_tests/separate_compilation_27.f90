program separate_compilation_27
  use io_separate_compilation_27, only: open
  implicit none

  integer :: u = 1
  call open(u)

  print *, u
  if (u /= 3) error stop
end program separate_compilation_27