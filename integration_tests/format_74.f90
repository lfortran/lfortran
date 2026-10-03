program format_74
  implicit none

  integer :: u
  character(len=120) :: line1, line2

  open (newunit=u, file='format_74_data.txt', status='replace', form='formatted')
  write (u, 100) 1, 2, 3, 4
  close (u)

  open (newunit=u, file='format_74_data.txt', status='old', form='formatted')
  read (u, '(A)') line1
  read (u, '(A)') line2
  close (u)

  if (index(line1, '1 **') == 0) error stop "missing first item"
  if (index(line1, "2 ''") == 0) error stop "missing quoted literal"
  if (index(line1, '3 ((') == 0) error stop "missing literal parentheses"
  if (index(line2, "4 ''") == 0) error stop "missing reverted-format item"

100 format (27X,I4," **",1(27X,I4," ''",(I4," ((")))

end program format_74
