program formatted_read_02
  implicit none

  character(4) :: s1, s2, s3

  open (10, file='formatted_read_02.txt', form='formatted', status='unknown')
  write (10, '(a)') 'abcdwxyz1234'

  rewind (10)
  s1 = '(no)'; s2 = '(no)'; s3 = '(no)'
  read  (10, '(a4, 1(a4), a4)') s1, s2, s3
  if (s1 /= 'abcd') error stop
  if (s2 /= 'wxyz') error stop
  if (s3 /= '1234') error stop

  rewind (10)
  s1 = '(no)'; s2 = '(no)'; s3 = '(no)'
  read  (10, '(a4, 2(a4))') s1, s2, s3
  if (s1 /= 'abcd') error stop
  if (s2 /= 'wxyz') error stop
  if (s3 /= '1234') error stop

  rewind (10)
  s1 = '(no)'; s2 = '(no)'; s3 = '(no)'
  read  (10, '((a4), a4, a4)') s1, s2, s3
  if (s1 /= 'abcd') error stop
  if (s2 /= 'wxyz') error stop
  if (s3 /= '1234') error stop

  close (10, status='delete')
  print *, "All tests passed."

end program formatted_read_02
