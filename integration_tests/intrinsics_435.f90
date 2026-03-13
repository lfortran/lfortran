program test_trim_implied_do_fmt
  implicit none
  integer i
  character(8) :: strings(2) = ['Hello   ', 'World   ']
  character(256) :: output

  write(output, "(*(1X,A))") (trim(strings(i)), i=1, size(strings)), '(Implied-do I/O)'

  if (trim(output) /= ' Hello World (Implied-do I/O)') error stop 'wrong output'
  print *, 'test passed'
end program test_trim_implied_do_fmt