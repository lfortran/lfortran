program format_quotes_01
  implicit none

  character(len=32) :: s1
  character(len=32) :: s2

  s1 = ''
  s2 = ''

  write (s1, 100)
  write (s2, 101)

  if (trim(s1) /= "'Hello world!'") then
    error stop "format single-quote literal mismatch"
  end if

  if (trim(s2) /= '"Hello world!"') then
    error stop "format double-quote literal mismatch"
  end if

100 format ('''Hello world!''')
101 format ("""Hello world!""")

end program format_quotes_01
