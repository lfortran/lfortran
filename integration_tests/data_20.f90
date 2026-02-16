program data_chars2
  implicit none

  character(2) :: s(3)
  data s / 'h', 'el', 'lo' /

  if (s(1) /= 'h ') error stop "s(1) incorrect"
  if (s(2) /= 'el') error stop "s(2) incorrect"
  if (s(3) /= 'lo') error stop "s(3) incorrect"
   print *, "Test passed"
end program