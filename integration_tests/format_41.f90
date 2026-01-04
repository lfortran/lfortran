program format_40
  ! Test sign control format descriptors: S, SP, SS
  implicit none
  character(len=20) :: output

  ! Test S (restore default sign printing)
  write(output, 100) -42.43, 42.43
  if (output /= "   -42.430    42.430 ") error stop "Test 1 failed"

  ! Test SP (always print sign)
  write(output, 101) -42.43, 42.43
  if (output /= "   -42.430   +42.430 ") error stop "Test 2 failed"

  ! Test SS (suppress optional plus sign)
  write(output, 102) -42.43, 42.43
  if (output /= "   -42.430    42.430 ") error stop "Test 3 failed"

  print *, "OK"

100 format (s, 2f10.3)
101 format (sp, 2f10.3)
102 format (ss, 2f10.3)

end program
