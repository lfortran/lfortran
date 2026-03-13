program test_trim_implied_do
  implicit none
  integer i
  character(8) :: strings(2) = ['Hello   ', 'World   ']
  character(256) :: output

  write(output, *) (trim(strings(i)), i=1, size(strings)), '(Implied-do I/O)'
  if (index(trim(output), 'Hello') == 0) error stop 'wrong output'
  if (index(trim(output), 'World') == 0) error stop 'wrong output'
  if (index(trim(output), '(Implied-do I/O)') == 0) error stop 'wrong output'
  print *, 'test passed'
end program test_trim_implied_do