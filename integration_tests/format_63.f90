program format_63
  implicit none

  character :: s(5)

  open (10, file='format_63_tmp.txt', status='unknown', form='formatted')

  write (10, '(a5)') '12345'
  rewind (10)
  s = '?'
  read (10, '(5a1)') s
  if (s(1) /= '1') error stop
  if (s(2) /= '2') error stop
  if (s(3) /= '3') error stop
  if (s(4) /= '4') error stop
  if (s(5) /= '5') error stop

  close(10, status='delete')
end program format_63
