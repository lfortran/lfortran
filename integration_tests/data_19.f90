program data_19
  implicit none
  character(6) :: s
  data s(1:6) / 'hello!'/
  if (s /= 'hello!') error stop
end program
