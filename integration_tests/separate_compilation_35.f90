program separate_compilation_35
  use separate_compilation_35a, only: get_version
  implicit none
  character(len=:), allocatable :: v
  v = get_version()
  print *, v
  if (len(v) == 0) error stop
end program
