program format_68
  implicit none
  character(1) :: fmt*10, line*80, lstring*26, larray(26)
  integer :: i
  lstring = 'abcdefghijklmnopqrstuvwxyz'
  larray = transfer(lstring, larray)
  if (size(larray) /= 26) error stop
  if (larray(1) /= 'a') error stop
  if (larray(26) /= 'z') error stop
  fmt = '(99(1X,A))'
  write(line, fmt) (larray(i), i=1,26)
  if (line(2:2) /= 'a') error stop
  if (line(52:52) /= 'z') error stop
  if (len_trim(line) /= 52) error stop
  print "(A)", trim(line)
end program format_68
