program arrays_112
  implicit none
  real :: empty(0) = [real ::]
  character(80) :: line
  if (size(empty) /= 0) error stop
  write(line, "(2A)") ':', empty(:), ':'
  if (trim(line) /= '::') error stop
  print "(2A)", ':', empty(:), ':'
end program arrays_112
