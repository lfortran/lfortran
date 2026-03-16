program format_67
  implicit none
  character :: fmt*10, line*80, letters*26
  integer :: i
  letters = 'abcdefghijklmnopqrstuvwxyz'
  fmt = '(99(1X,A))'
  write(line, fmt) (letters(i:i), i=1,26)
  if (line(1:1) /= ' ') error stop
  if (line(2:2) /= 'a') error stop
  if (line(52:52) /= 'z') error stop
  if (len_trim(line) /= 52) error stop
  print "(A)", trim(line)
end program format_67
